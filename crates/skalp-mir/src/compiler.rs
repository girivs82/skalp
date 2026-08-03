//! Main MIR compiler pipeline
//!
//! This module provides the main compilation pipeline from HIR to SystemVerilog

use crate::cdc_analysis::{CdcAnalyzer, CdcSeverity, CdcViolation};
use crate::hir_to_mir::HirToMir;
use crate::mir::Mir;
use crate::optimize::{ConstantFolding, DeadCodeElimination, OptimizationPass};
use crate::ssa_conversion::apply_ssa_conversion;
use anyhow::Result;
use indexmap::IndexMap;
use skalp_frontend::hir::Hir;
use std::path::PathBuf;

/// Optimization level
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptimizationLevel {
    /// No optimizations
    None,
    /// Basic optimizations (dead code elimination)
    Basic,
    /// Full optimizations (all available passes)
    Full,
}

/// MIR compiler
pub struct MirCompiler {
    /// Optimization level
    opt_level: OptimizationLevel,
    /// Enable verbose output
    verbose: bool,
}

impl MirCompiler {
    /// Create a new MIR compiler
    pub fn new() -> Self {
        Self {
            opt_level: OptimizationLevel::Basic,
            verbose: false,
        }
    }

    /// Set optimization level
    pub fn with_optimization_level(mut self, level: OptimizationLevel) -> Self {
        self.opt_level = level;
        self
    }

    /// Enable verbose output
    pub fn with_verbose(mut self, verbose: bool) -> Self {
        self.verbose = verbose;
        self
    }

    /// Compile HIR to MIR with CDC analysis
    ///
    /// If module_hirs is provided, the compiler can properly resolve function calls
    /// in their original module scope, enabling proper transitive imports.
    pub fn compile_to_mir(&self, hir: &Hir) -> Result<Mir, String> {
        self.compile_to_mir_with_modules(hir, &IndexMap::new())
    }

    /// Compile HIR to MIR with CDC analysis and module scope resolution
    ///
    /// The module_hirs parameter provides access to all loaded module HIRs, allowing
    /// the compiler to resolve function calls in their proper module scope.
    /// This fixes Bug #84: transitive imports now work correctly.
    pub fn compile_to_mir_with_modules(
        &self,
        hir: &Hir,
        module_hirs: &IndexMap<PathBuf, Hir>,
    ) -> Result<Mir, String> {
        // Step 1: Transform HIR to MIR
        let mut transformer = HirToMir::new_with_modules(module_hirs);
        let mut mir = transformer.transform(hir);

        // Modules reachable from the main design. Silently-wrong-hardware checks
        // are scoped to these — stdlib entities are monomorphized even when
        // unused, and a latent problem in an unused stdlib entity must not block
        // an unrelated design.
        let reachable_names = Self::reachable_module_names(&mir, hir);

        // Fail on conversion errors: these are cases where the converter would
        // otherwise emit silently wrong hardware (unresolved identifiers → 0,
        // silently dropped struct literals).
        let conversion_errors = transformer.conversion_errors();
        if !conversion_errors.is_empty() {
            use std::collections::HashSet;
            let mut seen = HashSet::new();
            let relevant: Vec<&str> = conversion_errors
                .iter()
                .filter(|(entity, _)| {
                    // Module names for monomorphized entities equal the entity name
                    reachable_names.contains(entity.as_str())
                })
                .map(|(_, msg)| msg.as_str())
                .filter(|m| seen.insert(*m))
                .collect();

            if !relevant.is_empty() {
                return Err(format!(
                    "MIR conversion failed with {} error(s):\n  {}",
                    relevant.len(),
                    relevant.join("\n  ")
                ));
            }
        }

        // Fail on undriven outputs: an output port nothing writes to is a
        // dropped-statement lowering bug or a design error — the emitted
        // netlist would drive the port from nothing.
        {
            let reachable_refs: std::collections::HashSet<&str> =
                reachable_names.iter().map(|s| s.as_str()).collect();
            let undriven = crate::undriven::check_undriven_outputs(&mir, &reachable_refs);
            if !undriven.is_empty() {
                return Err(format!(
                    "undriven output check failed with {} error(s):\n  {}",
                    undriven.len(),
                    undriven.join("\n  ")
                ));
            }
        }

        // Step 2: Perform CDC analysis
        let violations = self.perform_cdc_analysis(&mir);

        // Check for critical CDC violations and report them
        if !violations.is_empty() {
            self.report_cdc_violations(&violations);

            // Fail compilation if there are critical violations
            let critical_violations: Vec<_> = violations
                .iter()
                .filter(|v| v.severity == CdcSeverity::Critical)
                .collect();

            if !critical_violations.is_empty() {
                return Err(format!(
                    "Compilation failed due to {} critical CDC violations",
                    critical_violations.len()
                ));
            }
        }

        // Step 3: Apply SSA conversion
        // This eliminates combinational cycles from mutable variable reassignment (x = f(x))
        // by transforming to unique variables (x_0 = value, x_1 = f(x_0), etc.)
        apply_ssa_conversion(&mut mir);

        // Step 4: Apply optimizations
        self.apply_optimizations(&mut mir);

        Ok(mir)
    }

    /// Compile HIR to MIR (without codegen - that's handled by skalp-codegen crate)
    pub fn compile(&self, hir: &Hir) -> Result<Mir, String> {
        self.compile_to_mir(hir)
    }

    /// Names of modules reachable from the main design's entities.
    ///
    /// Roots are the modules for entities defined in the main source file
    /// (`hir.main_entity_names`); an empty list means provenance is unknown
    /// (e.g. direct-HIR callers like the VHDL frontend) — every module is
    /// treated as a root in that case. Reachability follows instance edges.
    fn reachable_module_names(mir: &Mir, hir: &Hir) -> std::collections::HashSet<String> {
        use std::collections::{HashSet, VecDeque};
        let mut reachable: HashSet<crate::mir::ModuleId> = if hir.main_entity_names.is_empty() {
            mir.modules.iter().map(|m| m.id).collect()
        } else {
            mir.modules
                .iter()
                .filter(|m| {
                    hir.main_entity_names.iter().any(|n| {
                        // Exact match, or a monomorphized specialization of a
                        // main entity (e.g. "TmrCounter_8" for generic entity
                        // "TmrCounter"). Specialized top modules are not
                        // instantiated by anything, so they must be roots.
                        n == &m.name
                            || (m.name.len() > n.len()
                                && m.name.starts_with(n.as_str())
                                && m.name.as_bytes()[n.len()] == b'_')
                    })
                })
                .map(|m| m.id)
                .collect()
        };
        let mut queue: VecDeque<crate::mir::ModuleId> = reachable.iter().copied().collect();
        while let Some(mid) = queue.pop_front() {
            if let Some(module) = mir.modules.iter().find(|m| m.id == mid) {
                for inst in &module.instances {
                    if reachable.insert(inst.module) {
                        queue.push_back(inst.module);
                    }
                }
            }
        }
        mir.modules
            .iter()
            .filter(|m| reachable.contains(&m.id))
            .map(|m| m.name.clone())
            .collect()
    }

    /// Apply optimization passes based on optimization level
    fn apply_optimizations(&self, mir: &mut Mir) {
        match self.opt_level {
            OptimizationLevel::None => {}
            OptimizationLevel::Basic => {
                // Apply dead code elimination
                self.apply_pass(mir, &mut DeadCodeElimination::new());
            }
            OptimizationLevel::Full => {
                // Apply all optimizations in order
                self.apply_pass(mir, &mut ConstantFolding::new());
                self.apply_pass(mir, &mut DeadCodeElimination::new());
            }
        }
    }

    /// Apply a single optimization pass
    fn apply_pass(&self, mir: &mut Mir, pass: &mut dyn OptimizationPass) {
        pass.apply(mir);
    }

    /// Perform CDC analysis on all modules in the MIR
    fn perform_cdc_analysis(&self, mir: &Mir) -> Vec<CdcViolation> {
        let mut all_violations = Vec::new();

        for module in &mir.modules {
            let mut analyzer = CdcAnalyzer::new();
            let violations = analyzer.analyze_module(module);
            all_violations.extend(violations);
        }

        all_violations
    }

    /// Report CDC violations to the user
    fn report_cdc_violations(&self, violations: &[CdcViolation]) {
        if violations.is_empty() {
            return;
        }

        for (i, violation) in violations.iter().enumerate() {
            let severity_str = match violation.severity {
                CdcSeverity::Critical => "CRITICAL",
                CdcSeverity::Warning => "WARNING",
                CdcSeverity::Info => "INFO",
            };

            let violation_type_str = match violation.violation_type {
                crate::cdc_analysis::CdcViolationType::DirectCrossing => {
                    "Direct Clock Domain Crossing"
                }
                crate::cdc_analysis::CdcViolationType::CombinationalMixing => {
                    "Combinational Logic Mixing"
                }
                crate::cdc_analysis::CdcViolationType::AsyncResetCrossing => "Async Reset Crossing",
                crate::cdc_analysis::CdcViolationType::ArithmeticMixing => "Arithmetic Mixing",
            };

            // Violation details removed
        }

        // Summary
        let critical_count = violations
            .iter()
            .filter(|v| v.severity == CdcSeverity::Critical)
            .count();
        let warning_count = violations
            .iter()
            .filter(|v| v.severity == CdcSeverity::Warning)
            .count();
        let info_count = violations
            .iter()
            .filter(|v| v.severity == CdcSeverity::Info)
            .count();

        // Summary removed
    }
}

impl Default for MirCompiler {
    fn default() -> Self {
        Self::new()
    }
}

// Old convenience functions removed - use skalp_codegen directly for code generation
