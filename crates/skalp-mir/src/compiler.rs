//! Main MIR compiler pipeline
//!
//! This module provides the main compilation pipeline from HIR to SystemVerilog

use crate::hir_to_mir::HirToMir;
use crate::optimize::{OptimizationPass, DeadCodeElimination, ConstantFolding};
use crate::codegen::SystemVerilogGenerator;
use crate::cdc_analysis::{CdcAnalyzer, CdcViolation, CdcSeverity};
use crate::mir::Mir;
use skalp_frontend::hir::Hir;

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
    pub fn compile_to_mir(&self, hir: &Hir) -> Result<Mir, String> {
        // Step 1: Transform HIR to MIR
        if self.verbose {
            println!("Phase 1: HIR to MIR transformation");
        }
        let mut transformer = HirToMir::new();
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
            let critical_violations: Vec<_> = violations.iter()
                .filter(|v| v.severity == CdcSeverity::Critical)
                .collect();

            if !critical_violations.is_empty() {
                return Err(format!("Compilation failed due to {} critical CDC violations", critical_violations.len()));
            }
        }

        // Step 3: Apply optimizations
        if self.verbose {
            println!("Phase 3: Applying optimizations (level: {:?})", self.opt_level);
        }
        self.apply_optimizations(&mut mir);

        Ok(mir)
    }

    /// Compile HIR to SystemVerilog
    pub fn compile(&self, hir: &Hir) -> Result<String, String> {
        // Compile to MIR with CDC analysis
        let mir = self.compile_to_mir(hir)?;

        // Step 4: Generate SystemVerilog
        if self.verbose {
            println!("Phase 4: SystemVerilog code generation");
        }
        let mut generator = SystemVerilogGenerator::new();
        let verilog = generator.generate(&mir);

        Ok(verilog)
    }

    /// Apply optimization passes based on optimization level
    fn apply_optimizations(&self, mir: &mut Mir) {
        match self.opt_level {
            OptimizationLevel::None => {},
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
                crate::cdc_analysis::CdcViolationType::DirectCrossing => "Direct Clock Domain Crossing",
                crate::cdc_analysis::CdcViolationType::CombinationalMixing => "Combinational Logic Mixing",
                crate::cdc_analysis::CdcViolationType::AsyncResetCrossing => "Async Reset Crossing",
                crate::cdc_analysis::CdcViolationType::ArithmeticMixing => "Arithmetic Mixing",
            };

            // Violation details removed

        }

        // Summary
        let critical_count = violations.iter().filter(|v| v.severity == CdcSeverity::Critical).count();
        let warning_count = violations.iter().filter(|v| v.severity == CdcSeverity::Warning).count();
        let info_count = violations.iter().filter(|v| v.severity == CdcSeverity::Info).count();


        // Summary removed
    }
}

impl Default for MirCompiler {
    fn default() -> Self {
        Self::new()
    }
}

/// Compile HIR to SystemVerilog with default settings
pub fn compile_hir_to_verilog(hir: &Hir) -> Result<String, String> {
    let compiler = MirCompiler::new();
    compiler.compile(hir)
}

/// Compile HIR to SystemVerilog with full optimizations
pub fn compile_hir_to_verilog_optimized(hir: &Hir) -> Result<String, String> {
    let compiler = MirCompiler::new()
        .with_optimization_level(OptimizationLevel::Full);
    compiler.compile(hir)
}