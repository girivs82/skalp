//! Clock Domain Crossing (CDC) Analysis
//!
//! This module provides compile-time detection of potentially unsafe clock domain crossings.
//! CDC violations occur when signals from different clock domains are used together without
//! proper synchronization, which can lead to metastability and data corruption.

use crate::mir::{Module, Signal, Process, Statement, Expression, LValue, ClockDomainId};
use std::collections::{HashMap, HashSet};
use serde::{Serialize, Deserialize};

/// CDC violation severity levels
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum CdcSeverity {
    /// Critical violations that will cause design failures
    Critical,
    /// Warnings about potentially unsafe patterns
    Warning,
    /// Informational notices about clock domain usage
    Info,
}

/// Types of CDC violations
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum CdcViolationType {
    /// Direct assignment from one clock domain to another
    DirectCrossing,
    /// Combinational logic mixing multiple clock domains
    CombinationalMixing,
    /// Asynchronous reset crossing clock domains
    AsyncResetCrossing,
    /// Clock domain mismatch in arithmetic operations
    ArithmeticMixing,
}

/// A detected CDC violation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CdcViolation {
    /// Type of violation
    pub violation_type: CdcViolationType,
    /// Severity level
    pub severity: CdcSeverity,
    /// Human-readable description
    pub description: String,
    /// Source clock domain
    pub source_domain: Option<ClockDomainId>,
    /// Target clock domain
    pub target_domain: Option<ClockDomainId>,
    /// Location information (for future use)
    pub location: Option<String>,
}

/// CDC Analyzer for detecting clock domain crossing violations
pub struct CdcAnalyzer {
    /// Clock domain assignments for each signal
    signal_domains: HashMap<crate::mir::SignalId, ClockDomainId>,
    /// Clock domain assignments for each port
    port_domains: HashMap<crate::mir::PortId, ClockDomainId>,
}

impl CdcAnalyzer {
    /// Create a new CDC analyzer
    pub fn new() -> Self {
        Self {
            signal_domains: HashMap::new(),
            port_domains: HashMap::new(),
        }
    }

    /// Analyze a module for CDC violations
    pub fn analyze_module(&mut self, module: &Module) -> Vec<CdcViolation> {
        let mut violations = Vec::new();


        // First, collect all clock domain information from signals
        self.collect_clock_domains(module);


        // Analyze each process for CDC violations
        for (i, process) in module.processes.iter().enumerate() {
            let process_violations = self.analyze_process(process, module);
            violations.extend(process_violations);
        }

        // Analyze continuous assignments
        for (i, continuous_assign) in module.assignments.iter().enumerate() {
            let assign_violations = self.analyze_continuous_assign(continuous_assign, module);
            violations.extend(assign_violations);
        }

        violations
    }

    /// Collect clock domain information from signals and ports
    fn collect_clock_domains(&mut self, module: &Module) {
        // Collect signal clock domains
        for signal in &module.signals {
            if let Some(domain) = signal.clock_domain {
                self.signal_domains.insert(signal.id, domain);
            }
        }

        // Infer port clock domains from their types
        for port in &module.ports {
            if let Some(domain) = self.infer_port_clock_domain(port) {
                self.port_domains.insert(port.id, domain);
            }
        }
    }

    /// Infer clock domain for a port based on its type
    fn infer_port_clock_domain(&self, port: &crate::mir::Port) -> Option<ClockDomainId> {
        use crate::mir::DataType;
        match &port.port_type {
            DataType::Clock { domain } => *domain,
            DataType::Reset { domain, .. } => *domain,
            _ => None,
        }
    }

    /// Analyze a process for CDC violations
    fn analyze_process(&self, process: &Process, module: &Module) -> Vec<CdcViolation> {
        let mut violations = Vec::new();

        // Get the clock domain for this process based on its sensitivity list
        let process_domain = self.get_process_clock_domain(process);

        // Analyze all statements in the process
        violations.extend(self.analyze_statements(&process.body.statements, process_domain, module));

        violations
    }

    /// Get the clock domain for a process based on its sensitivity list
    fn get_process_clock_domain(&self, process: &Process) -> Option<ClockDomainId> {
        use crate::mir::SensitivityList;

        // Look at the sensitivity list to determine the clock domain
        match &process.sensitivity {
            SensitivityList::Edge(edge_sensitivities) => {
                // For edge-triggered processes, look at the clock signals
                for edge_sens in edge_sensitivities {
                    if let Some(domain) = self.get_lvalue_clock_domain(&edge_sens.signal) {
                        return Some(domain);
                    }
                }
                None
            }
            SensitivityList::Level(lvalues) => {
                // For level-sensitive processes, use the first signal's domain
                for lvalue in lvalues {
                    if let Some(domain) = self.get_lvalue_clock_domain(lvalue) {
                        return Some(domain);
                    }
                }
                None
            }
            SensitivityList::Always => {
                // Always processes don't have a specific clock domain
                None
            }
        }
    }

    /// Analyze a list of statements for CDC violations
    fn analyze_statements(
        &self,
        statements: &[Statement],
        process_domain: Option<ClockDomainId>,
        module: &Module,
    ) -> Vec<CdcViolation> {
        let mut violations = Vec::new();

        for statement in statements {
            violations.extend(self.analyze_statement(statement, process_domain, module));
        }

        violations
    }

    /// Analyze a single statement for CDC violations
    fn analyze_statement(
        &self,
        statement: &Statement,
        process_domain: Option<ClockDomainId>,
        module: &Module,
    ) -> Vec<CdcViolation> {
        let mut violations = Vec::new();

        match statement {
            Statement::Assignment(assignment) => {
                // Check if we're assigning across clock domains
                let target_domain = self.get_lvalue_clock_domain(&assignment.lhs);
                let source_domains = self.get_expression_clock_domains(&assignment.rhs);

                // Debug removed

                // Check for direct clock domain crossings
                if let Some(target_domain) = target_domain {
                    if let Some(process_domain) = process_domain {
                        if target_domain != process_domain {
                            violations.push(CdcViolation {
                                violation_type: CdcViolationType::DirectCrossing,
                                severity: CdcSeverity::Critical,
                                description: format!(
                                    "Assignment to signal in domain {:?} from process in domain {:?}",
                                    target_domain, process_domain
                                ),
                                source_domain: Some(process_domain),
                                target_domain: Some(target_domain),
                                location: None,
                            });
                        }
                    }

                    // Check for cross-domain reads in source expression
                    for source_domain in source_domains.iter() {
                        if let Some(process_domain) = process_domain {
                            if *source_domain != process_domain {
                                violations.push(CdcViolation {
                                    violation_type: CdcViolationType::DirectCrossing,
                                    severity: CdcSeverity::Critical,
                                    description: format!(
                                        "Reading signal from domain {:?} in process from domain {:?}",
                                        source_domain, process_domain
                                    ),
                                    source_domain: Some(*source_domain),
                                    target_domain: Some(process_domain),
                                    location: None,
                                });
                            }
                        }
                    }

                    // Check for mixing multiple source domains
                    if source_domains.len() > 1 {
                        violations.push(CdcViolation {
                            violation_type: CdcViolationType::CombinationalMixing,
                            severity: CdcSeverity::Warning,
                            description: format!(
                                "Expression mixes signals from {} different clock domains",
                                source_domains.len()
                            ),
                            source_domain: None,
                            target_domain: Some(target_domain),
                            location: None,
                        });
                    }
                }
            }

            Statement::If(if_stmt) => {
                violations.extend(self.analyze_statements(&if_stmt.then_block.statements, process_domain, module));
                if let Some(else_block) = &if_stmt.else_block {
                    violations.extend(self.analyze_statements(&else_block.statements, process_domain, module));
                }
            }

            Statement::Case(case_stmt) => {
                for item in &case_stmt.items {
                    violations.extend(self.analyze_statements(&item.block.statements, process_domain, module));
                }
                if let Some(default_block) = &case_stmt.default {
                    violations.extend(self.analyze_statements(&default_block.statements, process_domain, module));
                }
            }

            Statement::Block(block) => {
                violations.extend(self.analyze_statements(&block.statements, process_domain, module));
            }

            Statement::Loop(_) => {
                // Loop analysis would go here
            }
        }

        violations
    }

    /// Analyze a continuous assignment for CDC violations
    fn analyze_continuous_assign(
        &self,
        continuous_assign: &crate::mir::ContinuousAssign,
        _module: &Module,
    ) -> Vec<CdcViolation> {
        let mut violations = Vec::new();

        // Get the clock domains involved in the assignment
        let target_domain = self.get_lvalue_clock_domain(&continuous_assign.lhs);
        let source_domains = self.get_expression_clock_domains(&continuous_assign.rhs);

        // Continuous assignments should not cross clock domains
        if let Some(target_domain) = target_domain {
            for source_domain in source_domains {
                if source_domain != target_domain {
                    violations.push(CdcViolation {
                        violation_type: CdcViolationType::DirectCrossing,
                        severity: CdcSeverity::Critical,
                        description: format!(
                            "Continuous assignment crosses from domain {:?} to {:?}",
                            source_domain, target_domain
                        ),
                        source_domain: Some(source_domain),
                        target_domain: Some(target_domain),
                        location: None,
                    });
                }
            }
        }

        violations
    }

    /// Get the clock domain of an LValue
    fn get_lvalue_clock_domain(&self, lvalue: &LValue) -> Option<ClockDomainId> {
        match lvalue {
            LValue::Signal(signal_id) => self.signal_domains.get(signal_id).copied(),
            LValue::Port(port_id) => self.port_domains.get(port_id).copied(),
            LValue::Variable(_) => None, // Variables don't have clock domains
            LValue::BitSelect { base, .. } => self.get_lvalue_clock_domain(base),
            LValue::RangeSelect { base, .. } => self.get_lvalue_clock_domain(base),
            LValue::Concat(lvalues) => {
                // For concatenations, use the first non-None domain
                for lvalue in lvalues {
                    if let Some(domain) = self.get_lvalue_clock_domain(lvalue) {
                        return Some(domain);
                    }
                }
                None
            }
        }
    }

    /// Get all clock domains referenced by an expression
    fn get_expression_clock_domains(&self, expression: &Expression) -> HashSet<ClockDomainId> {
        let mut domains = HashSet::new();

        match expression {
            Expression::Literal(_) => {
                // Literals don't have clock domains
            }
            Expression::Ref(lvalue) => {
                if let Some(domain) = self.get_lvalue_clock_domain(lvalue) {
                    domains.insert(domain);
                }
            }
            Expression::Binary { left, right, .. } => {
                domains.extend(self.get_expression_clock_domains(left));
                domains.extend(self.get_expression_clock_domains(right));
            }
            Expression::Unary { operand, .. } => {
                domains.extend(self.get_expression_clock_domains(operand));
            }
            Expression::Conditional { cond, then_expr, else_expr } => {
                domains.extend(self.get_expression_clock_domains(cond));
                domains.extend(self.get_expression_clock_domains(then_expr));
                domains.extend(self.get_expression_clock_domains(else_expr));
            }
            Expression::Concat(expressions) => {
                for expr in expressions {
                    domains.extend(self.get_expression_clock_domains(expr));
                }
            }
            Expression::Replicate { count, value } => {
                domains.extend(self.get_expression_clock_domains(count));
                domains.extend(self.get_expression_clock_domains(value));
            }
            Expression::FunctionCall { args, .. } => {
                for arg in args {
                    domains.extend(self.get_expression_clock_domains(arg));
                }
            }
        }

        domains
    }
}

impl Default for CdcAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}