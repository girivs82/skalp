//! Main MIR compiler pipeline
//!
//! This module provides the main compilation pipeline from HIR to SystemVerilog

use crate::cdc_analysis::{CdcAnalyzer, CdcSeverity, CdcViolation};
use crate::hir_to_mir::HirToMir;
use crate::mir::Mir;
use crate::optimize::{ConstantFolding, DeadCodeElimination, OptimizationPass};
use crate::ssa_conversion::apply_ssa_conversion;
use anyhow::Result;
use skalp_frontend::hir::Hir;
use std::collections::HashMap;
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
        self.compile_to_mir_with_modules(hir, &HashMap::new())
    }

    /// Compile HIR to MIR with CDC analysis and module scope resolution
    ///
    /// The module_hirs parameter provides access to all loaded module HIRs, allowing
    /// the compiler to resolve function calls in their proper module scope.
    /// This fixes Bug #84: transitive imports now work correctly.
    pub fn compile_to_mir_with_modules(
        &self,
        hir: &Hir,
        module_hirs: &HashMap<PathBuf, Hir>,
    ) -> Result<Mir, String> {
        // Step 1: Transform HIR to MIR
        if self.verbose {
            println!("Phase 1: HIR to MIR transformation");
        }
        let mut transformer = HirToMir::new_with_modules(module_hirs);
        let mut mir = transformer.transform(hir);

        // Step 2: Perform CDC analysis
        if self.verbose {
            println!("Phase 2: Clock Domain Crossing (CDC) analysis");
        }
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
        if self.verbose {
            println!("Phase 3: SSA conversion");
        }
        apply_ssa_conversion(&mut mir);

        // Step 4: Apply optimizations
        if self.verbose {
            println!(
                "Phase 4: Applying optimizations (level: {:?})",
                self.opt_level
            );
        }
        self.apply_optimizations(&mut mir);

        Ok(mir)
    }

    /// Compile HIR to MIR (without codegen - that's handled by skalp-codegen crate)
    pub fn compile(&self, hir: &Hir) -> Result<Mir, String> {
        self.compile_to_mir(hir)
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
        if self.verbose {
            println!("  - Applying {}", pass.name());
        }
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
