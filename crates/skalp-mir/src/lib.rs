#![allow(dead_code, unused_variables, unused_imports)]

//! SKALP MIR - Mid-level Intermediate Representation
//!
//! This crate handles:
//! - HIR to MIR transformation
//! - Process generation (always blocks)
//! - Optimization passes
//! - Preparation for code generation

// GLOBAL: Disable all debug output for performance
macro_rules! eprintln {
    ($($arg:tt)*) => {{}};
}

pub mod cdc_analysis;
pub mod compiler;
pub mod hir_to_mir;
pub mod mir;
pub mod mir_validation;
pub mod monomorphize;
pub mod optimize;
pub mod timing;
pub mod transform;

// Shared utility modules for consistent transformations
pub mod signal_naming;
pub mod type_flattening;
pub mod type_width;

// Re-export main types
pub use cdc_analysis::{CdcAnalyzer, CdcSeverity, CdcViolation, CdcViolationType};
pub use compiler::{MirCompiler, OptimizationLevel};
pub use hir_to_mir::HirToMir;
pub use mir::{
    Assignment, AssignmentKind, BinaryOp, Block, CaseStatement, ContinuousAssign, DataType,
    EdgeSensitivity, EdgeType, EnumType, EnumVariant, Expression, ExpressionKind,
    GenericParameter, GenericParameterType, IfStatement, LValue, LoopStatement, Mir, Module,
    ModuleId, ModuleInstance, Port, PortDirection, PortId, Process, ProcessId, ProcessKind,
    ReduceOp, SensitivityList, Signal, SignalId, Statement, StructField, StructType, UnaryOp,
    UnionType, Value, Variable, VariableId,
};
pub use mir_validation::{validate_mir, ValidationError};
pub use monomorphize::Monomorphizer;
pub use optimize::{ConstantFolding, DeadCodeElimination, OptimizationPass};

use anyhow::Result;
use skalp_frontend::Hir;

// Re-export Type from frontend for use in MIR Expression typing
pub use skalp_frontend::types::Type;

/// Lower HIR to MIR
pub fn lower_to_mir(hir: &Hir) -> Result<Mir> {
    // Phase 1: Monomorphize generic functions
    // This replaces generic functions with specialized (monomorphic) versions
    let mut monomorphizer = Monomorphizer::new();
    let monomorphic_hir = monomorphizer.monomorphize(hir);

    // Phase 2: Transform HIR to MIR
    let mut transformer = HirToMir::new();
    let mir = transformer.transform(&monomorphic_hir);

    // Validate MIR invariants
    // This catches bugs early if type flattening isn't complete
    if let Err(e) = validate_mir(&mir) {
        anyhow::bail!("MIR validation failed: {}", e);
    }

    Ok(mir)
}
