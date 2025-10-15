#![allow(dead_code, unused_variables, unused_imports)]
//! SKALP MIR - Mid-level Intermediate Representation
//!
//! This crate handles:
//! - HIR to MIR transformation
//! - Process generation (always blocks)
//! - Optimization passes
//! - Preparation for code generation

pub mod cdc_analysis;
pub mod compiler;
pub mod hir_to_mir;
pub mod mir;
pub mod mir_validation;
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
    EdgeSensitivity, EdgeType, EnumType, EnumVariant, Expression, GenericParameter,
    GenericParameterType, IfStatement, LValue, LoopStatement, Mir, Module, ModuleId,
    ModuleInstance, Port, PortDirection, PortId, Process, ProcessId, ProcessKind, ReduceOp,
    SensitivityList, Signal, SignalId, Statement, StructField, StructType, UnaryOp, UnionType,
    Value, Variable, VariableId,
};
pub use mir_validation::{validate_mir, ValidationError};
pub use optimize::{ConstantFolding, DeadCodeElimination, OptimizationPass};

use anyhow::Result;
use skalp_frontend::Hir;

/// Lower HIR to MIR
pub fn lower_to_mir(hir: &Hir) -> Result<Mir> {
    // Monomorphization already happened in the frontend
    // Use the actual HIR to MIR transformer
    let mut transformer = HirToMir::new();
    let mir = transformer.transform(hir);

    // Validate MIR invariants
    // This catches bugs early if type flattening isn't complete
    if let Err(e) = validate_mir(&mir) {
        anyhow::bail!("MIR validation failed: {}", e);
    }

    Ok(mir)
}
