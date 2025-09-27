//! Main MIR compiler pipeline
//!
//! This module provides the main compilation pipeline from HIR to SystemVerilog

use crate::hir_to_mir::HirToMir;
use crate::optimize::{OptimizationPass, DeadCodeElimination, ConstantFolding};
use crate::codegen::SystemVerilogGenerator;
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

    /// Compile HIR to SystemVerilog
    pub fn compile(&self, hir: &Hir) -> Result<String, String> {
        // Step 1: Transform HIR to MIR
        if self.verbose {
            println!("Phase 1: HIR to MIR transformation");
        }
        let mut transformer = HirToMir::new();
        let mut mir = transformer.transform(hir);

        // Step 2: Apply optimizations
        if self.opt_level != OptimizationLevel::None {
            if self.verbose {
                println!("Phase 2: Applying optimizations");
            }
            self.apply_optimizations(&mut mir);
        }

        // Step 3: Generate SystemVerilog
        if self.verbose {
            println!("Phase 3: SystemVerilog code generation");
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