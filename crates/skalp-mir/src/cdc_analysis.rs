//! Clock Domain Crossing (CDC) Analysis
//!
//! This module provides compile-time detection of potentially unsafe clock domain crossings.
//! CDC violations occur when signals from different clock domains are used together without
//! proper synchronization, which can lead to metastability and data corruption.

use crate::mir::{
    ClockDomainId, Expression, ExpressionKind, LValue, Module, Process, Signal, Statement,
};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

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
    signal_domains: IndexMap<crate::mir::SignalId, ClockDomainId>,
    /// Clock domain assignments for each port
    port_domains: IndexMap<crate::mir::PortId, ClockDomainId>,
}

impl CdcAnalyzer {
    /// Create a new CDC analyzer
    pub fn new() -> Self {
        Self {
            signal_domains: IndexMap::new(),
            port_domains: IndexMap::new(),
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
        // TRIAGE #10: signal.clock_domain stamps from hir_builder are keyed
        // by PRE-monomorphization port IDs — specialization remaps port IDs,
        // so the stamps go stale and mismatch the analyzer's port-keyed
        // scheme (false criticals on every specialized multi-clock design,
        // e.g. the tutorial AsyncFIFO). The process-based inference below
        // recomputes the same information consistently, so the stale stamps
        // are intentionally NOT consulted.

        // Infer port clock domains from their types
        for port in &module.ports {
            if let Some(domain) = self.infer_port_clock_domain(port) {
                self.port_domains.insert(port.id, domain);
            }
        }

        // TRIAGE #10: clock-typed ports whose domain annotation never reached
        // MIR (the clock<'x> lifetimes currently lower with domain: None —
        // triage #13) each get an IMPLICIT domain keyed by the PORT ID —
        // the SAME scheme hir_builder's infer_clock_domains uses for signal
        // domains (`ClockDomainId(port_id.0)`), so process domains line up
        // with the signal domains already stamped in HIR. Physically,
        // distinct clock pins are distinct domains unless proven otherwise;
        // single-clock designs stay single-domain.
        for port in &module.ports {
            use crate::mir::DataType;
            if matches!(port.port_type, DataType::Clock { domain: None })
                && !self.port_domains.contains_key(&port.id)
            {
                self.port_domains
                    .insert(port.id, crate::mir::ClockDomainId(port.id.0));
            }
        }

        // TRIAGE #10 (propagation, applied after process inference below):
        // see the fixpoint loop at the end of this function.

        // TRIAGE #10: infer signal domains from the process that assigns
        // them. hir_to_mir never populates signal.clock_domain, so without
        // this the analysis was vacuous — no signal ever had a domain and no
        // crossing was ever reported. A signal written in a process clocked
        // by domain D belongs to D (the standard CDC inference).
        for process in &module.processes {
            let Some(process_domain) = self.get_process_clock_domain(process) else {
                continue;
            };
            let mut targets: Vec<crate::mir::SignalId> = Vec::new();
            Self::collect_assigned_signals(&process.body.statements, &mut targets);
            for sig in targets {
                self.signal_domains.entry(sig).or_insert(process_domain);
            }
        }

        // TRIAGE #10: propagate domains through combinational (continuous)
        // assignments to fixpoint. A comb-derived signal like
        // `wr_ptr_gray = wr_ptr ^ (wr_ptr >> 1)` belongs to its source's
        // domain; without this, synchronizer first stages sampling such
        // signals went completely unanalyzed. Only single-domain sources
        // propagate — mixed-domain expressions are themselves flagged by
        // the continuous-assign analysis.
        loop {
            let mut changed = false;
            for assign in &module.assignments {
                let LValue::Signal(target) = &assign.lhs else {
                    continue;
                };
                if self.signal_domains.contains_key(target) {
                    continue;
                }
                let src_domains = self.get_expression_clock_domains(&assign.rhs);
                if src_domains.len() == 1 {
                    let d = *src_domains.iter().next().unwrap();
                    self.signal_domains.insert(*target, d);
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
    }

    /// Collect every signal assigned (whole or partial) in a statement list.
    fn collect_assigned_signals(statements: &[Statement], out: &mut Vec<crate::mir::SignalId>) {
        fn lvalue_base_signal(lv: &LValue) -> Option<crate::mir::SignalId> {
            match lv {
                LValue::Signal(id) => Some(*id),
                LValue::BitSelect { base, .. } | LValue::RangeSelect { base, .. } => {
                    lvalue_base_signal(base)
                }
                _ => None,
            }
        }
        for stmt in statements {
            match stmt {
                Statement::Assignment(a) => {
                    if let Some(id) = lvalue_base_signal(&a.lhs) {
                        out.push(id);
                    }
                }
                Statement::If(i) => {
                    Self::collect_assigned_signals(&i.then_block.statements, out);
                    if let Some(e) = &i.else_block {
                        Self::collect_assigned_signals(&e.statements, out);
                    }
                }
                Statement::Case(c) => {
                    for item in &c.items {
                        Self::collect_assigned_signals(&item.block.statements, out);
                    }
                    if let Some(d) = &c.default {
                        Self::collect_assigned_signals(&d.statements, out);
                    }
                }
                Statement::Block(b) => Self::collect_assigned_signals(&b.statements, out),
                Statement::ResolvedConditional(rc) => {
                    if let Some(id) = lvalue_base_signal(&rc.target) {
                        out.push(id);
                    }
                }
                _ => {}
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
        violations.extend(self.analyze_statements(
            &process.body.statements,
            process_domain,
            module,
        ));

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
                                    "assignment to `{}` (domain {:?}) from a process clocked in domain {:?} — needs a synchronizer",
                                    Self::lvalue_name(module, &assignment.lhs),
                                    target_domain, process_domain
                                ),
                                source_domain: Some(process_domain),
                                target_domain: Some(target_domain),
                                location: None,
                            });
                        }
                    }

                    // Check for cross-domain reads in source expression.
                    //
                    // TRIAGE #10 severity policy: a synchronizer's FIRST stage
                    // legitimately samples a foreign-domain signal — a bare
                    // registered copy (`ff1 = foreign`) is the standard 2-flop
                    // pattern and must not fail the build. Bare samples are
                    // WARNING (single-flop chains still deserve eyes), and
                    // Info when the target signal carries a #[cdc] annotation
                    // (documented intent). Crossings through LOGIC (arith,
                    // comparisons, muxes on foreign signals) stay CRITICAL.
                    let is_bare_sample = matches!(
                        &assignment.rhs.kind,
                        crate::mir::ExpressionKind::Ref(LValue::Signal(_) | LValue::Port(_))
                    );
                    let target_has_cdc_annotation = match &assignment.lhs {
                        LValue::Signal(id) => module
                            .signals
                            .iter()
                            .find(|s| s.id == *id)
                            .is_some_and(|s| s.cdc_config.is_some()),
                        _ => false,
                    };
                    for source_domain in source_domains.iter() {
                        if let Some(process_domain) = process_domain {
                            if *source_domain != process_domain {
                                let (severity, hint) = if target_has_cdc_annotation {
                                    (CdcSeverity::Info, "#[cdc]-annotated synchronizer")
                                } else if is_bare_sample {
                                    (
                                        CdcSeverity::Warning,
                                        "registered sample — ensure a >=2-stage chain or add #[cdc]",
                                    )
                                } else {
                                    (CdcSeverity::Critical, "needs a synchronizer")
                                };
                                violations.push(CdcViolation {
                                    violation_type: CdcViolationType::DirectCrossing,
                                    severity,
                                    description: format!(
                                        "assignment to `{}` reads a signal from domain {:?} inside a process clocked in domain {:?} — {}",
                                        Self::lvalue_name(module, &assignment.lhs),
                                        source_domain, process_domain, hint
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
                                "expression assigned to `{}` mixes signals from {} different clock domains",
                                Self::lvalue_name(module, &assignment.lhs),
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
                violations.extend(self.analyze_statements(
                    &if_stmt.then_block.statements,
                    process_domain,
                    module,
                ));
                if let Some(else_block) = &if_stmt.else_block {
                    violations.extend(self.analyze_statements(
                        &else_block.statements,
                        process_domain,
                        module,
                    ));
                }
            }

            Statement::Case(case_stmt) => {
                for item in &case_stmt.items {
                    violations.extend(self.analyze_statements(
                        &item.block.statements,
                        process_domain,
                        module,
                    ));
                }
                if let Some(default_block) = &case_stmt.default {
                    violations.extend(self.analyze_statements(
                        &default_block.statements,
                        process_domain,
                        module,
                    ));
                }
            }

            Statement::Block(block) => {
                violations.extend(self.analyze_statements(
                    &block.statements,
                    process_domain,
                    module,
                ));
            }

            Statement::Loop(_) => {
                // Loop analysis would go here
            }

            Statement::ResolvedConditional(resolved) => {
                // Analyze the resolved priority mux for CDC violations
                for case in &resolved.resolved.cases {
                    let condition_domains = self.get_expression_clock_domains(&case.condition);
                    let value_domains = self.get_expression_clock_domains(&case.value);

                    // Check for domain crossings in conditions and values
                    for &source_domain in &condition_domains {
                        if let Some(process_domain) = process_domain {
                            if source_domain != process_domain {
                                violations.push(CdcViolation {
                                    violation_type: CdcViolationType::DirectCrossing,
                                    severity: CdcSeverity::Warning,
                                    description: format!(
                                        "condition driving `{}` uses a signal from domain {:?} in a process clocked in domain {:?}",
                                        Self::lvalue_name(module, &resolved.target),
                                        source_domain, process_domain
                                    ),
                                    source_domain: Some(source_domain),
                                    target_domain: Some(process_domain),
                                    location: None,
                                });
                            }
                        }
                    }
                }

                // Check default value
                let default_domains = self.get_expression_clock_domains(&resolved.resolved.default);
                for &source_domain in &default_domains {
                    if let Some(process_domain) = process_domain {
                        if source_domain != process_domain {
                            violations.push(CdcViolation {
                                violation_type: CdcViolationType::DirectCrossing,
                                severity: CdcSeverity::Warning,
                                description: format!(
                                    "default value driving `{}` uses a signal from domain {:?} in a process clocked in domain {:?}",
                                    Self::lvalue_name(module, &resolved.target),
                                    source_domain, process_domain
                                ),
                                source_domain: Some(source_domain),
                                target_domain: Some(process_domain),
                                location: None,
                            });
                        }
                    }
                }
            }

            // Formal verification statements don't involve clock domain crossings
            Statement::Assert(_) | Statement::Assume(_) | Statement::Cover(_) => {
                // Assertions and covers don't have CDC implications
            }
        }

        violations
    }

    /// Analyze a continuous assignment for CDC violations
    fn analyze_continuous_assign(
        &self,
        continuous_assign: &crate::mir::ContinuousAssign,
        module: &Module,
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
                            "continuous assignment to `{}` crosses from domain {:?} to {:?} — needs a synchronizer",
                            Self::lvalue_name(module, &continuous_assign.lhs),
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
    /// Resolve an lvalue to a human-readable name for diagnostics (TRIAGE #10).
    fn lvalue_name(module: &Module, lvalue: &LValue) -> String {
        match lvalue {
            LValue::Signal(id) => module
                .signals
                .iter()
                .find(|s| s.id == *id)
                .map(|s| s.name.clone())
                .unwrap_or_else(|| format!("signal#{}", id.0)),
            LValue::Port(id) => module
                .ports
                .iter()
                .find(|p| p.id == *id)
                .map(|p| p.name.clone())
                .unwrap_or_else(|| format!("port#{}", id.0)),
            LValue::Variable(id) => module
                .variables
                .iter()
                .find(|v| v.id == *id)
                .map(|v| v.name.clone())
                .unwrap_or_else(|| format!("var#{}", id.0)),
            LValue::BitSelect { base, .. } => format!("{}[...]", Self::lvalue_name(module, base)),
            LValue::RangeSelect { base, .. } => {
                format!("{}[..:..]", Self::lvalue_name(module, base))
            }
            LValue::Concat(parts) => format!(
                "{{{}}}",
                parts
                    .iter()
                    .map(|p| Self::lvalue_name(module, p))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }

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

        match &expression.kind {
            ExpressionKind::Literal(_) => {
                // Literals don't have clock domains
            }
            ExpressionKind::Ref(lvalue) => {
                if let Some(domain) = self.get_lvalue_clock_domain(lvalue) {
                    domains.insert(domain);
                }
            }
            ExpressionKind::Binary { left, right, .. } => {
                domains.extend(self.get_expression_clock_domains(left));
                domains.extend(self.get_expression_clock_domains(right));
            }
            ExpressionKind::Unary { operand, .. } => {
                domains.extend(self.get_expression_clock_domains(operand));
            }
            ExpressionKind::Conditional {
                cond,
                then_expr,
                else_expr,
            } => {
                domains.extend(self.get_expression_clock_domains(cond));
                domains.extend(self.get_expression_clock_domains(then_expr));
                domains.extend(self.get_expression_clock_domains(else_expr));
            }
            ExpressionKind::Concat(expressions) => {
                for expr in expressions {
                    domains.extend(self.get_expression_clock_domains(expr));
                }
            }
            ExpressionKind::Replicate { count, value } => {
                domains.extend(self.get_expression_clock_domains(count));
                domains.extend(self.get_expression_clock_domains(value));
            }
            ExpressionKind::FunctionCall { args, .. } => {
                for arg in args {
                    domains.extend(self.get_expression_clock_domains(arg));
                }
            }
            ExpressionKind::Cast { expr, .. } => {
                // Cast is a no-op, propagate domains from inner expression
                domains.extend(self.get_expression_clock_domains(expr));
            }
            // BUG FIX #85: Handle tuple/field access
            ExpressionKind::TupleFieldAccess { base, .. } => {
                domains.extend(self.get_expression_clock_domains(base));
            }
            ExpressionKind::FieldAccess { base, .. } => {
                domains.extend(self.get_expression_clock_domains(base));
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
