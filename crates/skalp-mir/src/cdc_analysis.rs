//! Clock Domain Crossing (CDC) Analysis
//!
//! This module provides compile-time detection of Clock Domain Crossing violations
//! which are a major source of metastability issues in digital designs.

use crate::mir::*;
use std::collections::{HashMap, HashSet};

/// CDC violation types
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CdcViolationType {
    /// Signal crosses from one clock domain to another without synchronization
    UnsynchronizedCrossing {
        source_domain: ClockDomainId,
        target_domain: ClockDomainId,
        signal_name: String,
    },
    /// Multiple signals from different domains converge
    MultiDomainConvergence {
        domains: Vec<ClockDomainId>,
        target_signal: String,
    },
    /// Reset crossing without proper handling
    ResetCrossing {
        reset_domain: ClockDomainId,
        target_domain: ClockDomainId,
    },
}

/// CDC violation report
#[derive(Debug, Clone)]
pub struct CdcViolation {
    /// Type of violation
    pub violation_type: CdcViolationType,
    /// Location in the design
    pub location: ViolationLocation,
    /// Severity level
    pub severity: CdcSeverity,
    /// Suggested fix
    pub suggestion: String,
}

/// Location where CDC violation occurs
#[derive(Debug, Clone)]
pub struct ViolationLocation {
    /// Module where violation occurs
    pub module_name: String,
    /// Process or assignment where violation is detected
    pub process_name: Option<String>,
    /// Line information (if available)
    pub line: Option<u32>,
}

/// CDC violation severity
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CdcSeverity {
    /// Information - potential issue but may be intentional
    Info,
    /// Warning - likely issue that should be reviewed
    Warning,
    /// Error - definite violation that must be fixed
    Error,
    /// Critical - violation that could cause system failure
    Critical,
}

/// CDC analyzer
pub struct CdcAnalyzer {
    /// Violations found during analysis
    violations: Vec<CdcViolation>,
    /// Signal to clock domain mapping
    signal_domains: HashMap<SignalId, ClockDomainId>,
    /// Process to clock domain mapping
    process_domains: HashMap<ProcessId, ClockDomainId>,
}

impl CdcAnalyzer {
    /// Create a new CDC analyzer
    pub fn new() -> Self {
        Self {
            violations: Vec::new(),
            signal_domains: HashMap::new(),
            process_domains: HashMap::new(),
        }
    }

    /// Analyze MIR for CDC violations
    pub fn analyze(&mut self, mir: &Mir) -> Vec<CdcViolation> {
        self.violations.clear();
        self.signal_domains.clear();
        self.process_domains.clear();

        // Analyze each module
        for module in &mir.modules {
            self.analyze_module(module);
        }

        self.violations.clone()
    }

    /// Analyze a single module for CDC violations
    fn analyze_module(&mut self, module: &Module) {
        // Build clock domain mapping for signals
        self.build_signal_domain_mapping(module);

        // Build clock domain mapping for processes
        self.build_process_domain_mapping(module);

        // Check for CDC violations in processes
        for process in &module.processes {
            self.analyze_process(module, process);
        }

        // Check for CDC violations in continuous assignments
        for assignment in &module.assignments {
            self.analyze_continuous_assignment(module, assignment);
        }
    }

    /// Build mapping of signals to their clock domains
    fn build_signal_domain_mapping(&mut self, module: &Module) {
        // Map clock and reset signals to their domains
        for signal in &module.signals {
            if let Some(domain) = self.extract_signal_domain(&signal.signal_type) {
                self.signal_domains.insert(signal.id, domain);
            }
        }

        // Map port signals to their domains
        for port in &module.ports {
            if let Some(domain) = self.extract_signal_domain(&port.port_type) {
                // Convert PortId to SignalId for domain tracking
                // This is a simplification - in real implementation we'd need proper mapping
                let signal_id = SignalId(port.id.0);
                self.signal_domains.insert(signal_id, domain);
            }
        }
    }

    /// Extract clock domain from a data type
    fn extract_signal_domain(&self, data_type: &DataType) -> Option<ClockDomainId> {
        match data_type {
            DataType::Clock { domain } => *domain,
            DataType::Reset { domain, .. } => *domain,
            _ => None,
        }
    }

    /// Build mapping of processes to their clock domains
    fn build_process_domain_mapping(&mut self, module: &Module) {
        for process in &module.processes {
            if let Some(domain) = self.infer_process_domain(process) {
                self.process_domains.insert(process.id, domain);
            }
        }
    }

    /// Infer the clock domain of a process from its sensitivity list
    fn infer_process_domain(&self, process: &Process) -> Option<ClockDomainId> {
        match &process.sensitivity {
            SensitivityList::Edge(edges) => {
                // Find clock signals in the sensitivity list
                for edge in edges {
                    if let LValue::Signal(signal_id) = &edge.signal {
                        if let Some(domain) = self.signal_domains.get(signal_id) {
                            return Some(*domain);
                        }
                    }
                }
                None
            }
            SensitivityList::Level(signals) => {
                // For combinational processes, check if all inputs are from same domain
                let mut domains = HashSet::new();
                for signal_lval in signals {
                    if let LValue::Signal(signal_id) = signal_lval {
                        if let Some(domain) = self.signal_domains.get(signal_id) {
                            domains.insert(*domain);
                        }
                    }
                }
                // Return domain only if all signals are from same domain
                if domains.len() == 1 {
                    domains.into_iter().next()
                } else {
                    None
                }
            }
            SensitivityList::Always => None, // Always_comb has no specific domain
        }
    }

    /// Analyze a process for CDC violations
    fn analyze_process(&mut self, module: &Module, process: &Process) {
        let process_domain = self.process_domains.get(&process.id).copied();

        // Analyze all statements in the process
        self.analyze_block(module, &process.body, process_domain, Some(process));
    }

    /// Analyze a block of statements
    fn analyze_block(
        &mut self,
        module: &Module,
        block: &Block,
        current_domain: Option<ClockDomainId>,
        process: Option<&Process>,
    ) {
        for statement in &block.statements {
            self.analyze_statement(module, statement, current_domain, process);
        }
    }

    /// Analyze a single statement for CDC violations
    fn analyze_statement(
        &mut self,
        module: &Module,
        statement: &Statement,
        current_domain: Option<ClockDomainId>,
        process: Option<&Process>,
    ) {
        match statement {
            Statement::Assignment(assign) => {
                self.analyze_assignment(module, assign, current_domain, process);
            }
            Statement::If(if_stmt) => {
                self.analyze_block(module, &if_stmt.then_block, current_domain, process);
                if let Some(else_block) = &if_stmt.else_block {
                    self.analyze_block(module, else_block, current_domain, process);
                }
            }
            Statement::Case(case_stmt) => {
                for item in &case_stmt.items {
                    self.analyze_block(module, &item.block, current_domain, process);
                }
                if let Some(default_block) = &case_stmt.default {
                    self.analyze_block(module, default_block, current_domain, process);
                }
            }
            Statement::Block(inner_block) => {
                self.analyze_block(module, inner_block, current_domain, process);
            }
            Statement::Loop(_) => {
                // TODO: Implement loop analysis
            }
        }
    }

    /// Analyze an assignment for CDC violations
    fn analyze_assignment(
        &mut self,
        module: &Module,
        assignment: &Assignment,
        current_domain: Option<ClockDomainId>,
        process: Option<&Process>,
    ) {
        // Get the domain of the target signal
        let target_domain = self.get_lvalue_domain(&assignment.lhs);

        // Get domains of all source signals
        let source_domains = self.get_expression_domains(&assignment.rhs);

        // Check for CDC violations
        self.check_assignment_cdc(
            module,
            &assignment.lhs,
            &assignment.rhs,
            target_domain,
            source_domains,
            current_domain,
            process,
        );
    }

    /// Get the clock domain of an LValue
    fn get_lvalue_domain(&self, lvalue: &LValue) -> Option<ClockDomainId> {
        match lvalue {
            LValue::Signal(signal_id) => self.signal_domains.get(signal_id).copied(),
            LValue::Variable(_) => None, // Variables don't have domains
            LValue::Port(port_id) => {
                // Convert PortId to SignalId for domain lookup
                let signal_id = SignalId(port_id.0);
                self.signal_domains.get(&signal_id).copied()
            }
            LValue::BitSelect { base, .. } => self.get_lvalue_domain(base),
            LValue::RangeSelect { base, .. } => self.get_lvalue_domain(base),
            LValue::Concat(lvals) => {
                // For concatenation, check if all parts have same domain
                let mut domains = HashSet::new();
                for lval in lvals {
                    if let Some(domain) = self.get_lvalue_domain(lval) {
                        domains.insert(domain);
                    }
                }
                if domains.len() == 1 {
                    domains.into_iter().next()
                } else {
                    None
                }
            }
        }
    }

    /// Get all clock domains referenced in an expression
    fn get_expression_domains(&self, expr: &Expression) -> HashSet<ClockDomainId> {
        let mut domains = HashSet::new();
        self.collect_expression_domains(expr, &mut domains);
        domains
    }

    /// Recursively collect clock domains from an expression
    fn collect_expression_domains(&self, expr: &Expression, domains: &mut HashSet<ClockDomainId>) {
        match expr {
            Expression::Literal(_) => {}, // Literals have no domain
            Expression::Ref(lvalue) => {
                if let Some(domain) = self.get_lvalue_domain(lvalue) {
                    domains.insert(domain);
                }
            }
            Expression::Binary { left, right, .. } => {
                self.collect_expression_domains(left, domains);
                self.collect_expression_domains(right, domains);
            }
            Expression::Unary { operand, .. } => {
                self.collect_expression_domains(operand, domains);
            }
            Expression::Conditional { cond, then_expr, else_expr } => {
                self.collect_expression_domains(cond, domains);
                self.collect_expression_domains(then_expr, domains);
                self.collect_expression_domains(else_expr, domains);
            }
            Expression::Concat(exprs) => {
                for expr in exprs {
                    self.collect_expression_domains(expr, domains);
                }
            }
            Expression::Replicate { count, value } => {
                self.collect_expression_domains(count, domains);
                self.collect_expression_domains(value, domains);
            }
            Expression::FunctionCall { args, .. } => {
                for arg in args {
                    self.collect_expression_domains(arg, domains);
                }
            }
        }
    }

    /// Check for CDC violations in an assignment
    fn check_assignment_cdc(
        &mut self,
        module: &Module,
        target: &LValue,
        source: &Expression,
        target_domain: Option<ClockDomainId>,
        source_domains: HashSet<ClockDomainId>,
        current_domain: Option<ClockDomainId>,
        process: Option<&Process>,
    ) {
        // Check for unsynchronized domain crossings
        if let Some(target_dom) = target_domain {
            for source_dom in &source_domains {
                if target_dom != *source_dom {
                    // Found a CDC violation
                    let violation = CdcViolation {
                        violation_type: CdcViolationType::UnsynchronizedCrossing {
                            source_domain: *source_dom,
                            target_domain: target_dom,
                            signal_name: self.get_lvalue_name(target),
                        },
                        location: ViolationLocation {
                            module_name: module.name.clone(),
                            process_name: process.map(|p| format!("process_{}", p.id.0)),
                            line: None,
                        },
                        severity: CdcSeverity::Error,
                        suggestion: "Add proper synchronization (e.g., flip-flop synchronizer) when crossing clock domains".to_string(),
                    };
                    self.violations.push(violation);
                }
            }
        }

        // Check for multi-domain convergence
        if source_domains.len() > 1 {
            let violation = CdcViolation {
                violation_type: CdcViolationType::MultiDomainConvergence {
                    domains: source_domains.into_iter().collect(),
                    target_signal: self.get_lvalue_name(target),
                },
                location: ViolationLocation {
                    module_name: module.name.clone(),
                    process_name: process.map(|p| format!("process_{}", p.id.0)),
                    line: None,
                },
                severity: CdcSeverity::Warning,
                suggestion: "Ensure all source signals are properly synchronized to the same domain before combining".to_string(),
            };
            self.violations.push(violation);
        }
    }

    /// Analyze continuous assignment for CDC violations
    fn analyze_continuous_assignment(&mut self, module: &Module, assignment: &ContinuousAssign) {
        let target_domain = self.get_lvalue_domain(&assignment.lhs);
        let source_domains = self.get_expression_domains(&assignment.rhs);

        self.check_assignment_cdc(
            module,
            &assignment.lhs,
            &assignment.rhs,
            target_domain,
            source_domains,
            None,
            None,
        );
    }

    /// Get a human-readable name for an LValue
    fn get_lvalue_name(&self, lvalue: &LValue) -> String {
        match lvalue {
            LValue::Signal(id) => format!("signal_{}", id.0),
            LValue::Variable(id) => format!("var_{}", id.0),
            LValue::Port(id) => format!("port_{}", id.0),
            LValue::BitSelect { base, index } => {
                format!("{}[{}]", self.get_lvalue_name(base), "index")
            }
            LValue::RangeSelect { base, .. } => {
                format!("{}[range]", self.get_lvalue_name(base))
            }
            LValue::Concat(lvals) => {
                let names: Vec<String> = lvals.iter()
                    .map(|l| self.get_lvalue_name(l))
                    .collect();
                format!("{{{}}}", names.join(", "))
            }
        }
    }
}

impl Default for CdcAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cdc_analyzer_creation() {
        let analyzer = CdcAnalyzer::new();
        assert_eq!(analyzer.violations.len(), 0);
    }

    #[test]
    fn test_extract_signal_domain() {
        let analyzer = CdcAnalyzer::new();

        let clock_type = DataType::Clock { domain: Some(ClockDomainId(1)) };
        assert_eq!(analyzer.extract_signal_domain(&clock_type), Some(ClockDomainId(1)));

        let reset_type = DataType::Reset { active_high: true, domain: Some(ClockDomainId(2)) };
        assert_eq!(analyzer.extract_signal_domain(&reset_type), Some(ClockDomainId(2)));

        let bit_type = DataType::Bit(8);
        assert_eq!(analyzer.extract_signal_domain(&bit_type), None);
    }
}