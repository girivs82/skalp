#![allow(dead_code, unused_variables, unused_imports)]
//! SKALP MIR - Mid-level Intermediate Representation
//!
//! This crate handles:
//! - HIR to MIR transformation
//! - Process generation (always blocks)
//! - Optimization passes
//! - Preparation for code generation

pub mod cdc_analysis;
pub mod codegen;
pub mod compiler;
pub mod hir_to_mir;
pub mod mir;
pub mod optimize;
pub mod timing;
pub mod transform;

// Re-export main types
pub use cdc_analysis::{CdcAnalyzer, CdcSeverity, CdcViolation, CdcViolationType};
pub use codegen::SystemVerilogGenerator;
pub use compiler::{
    compile_hir_to_verilog, compile_hir_to_verilog_optimized, MirCompiler, OptimizationLevel,
};
pub use hir_to_mir::HirToMir;
pub use mir::{
    Assignment, AssignmentKind, BinaryOp, Block, CaseStatement, ContinuousAssign, DataType,
    EdgeSensitivity, EdgeType, EnumType, EnumVariant, Expression, GenericParameter,
    GenericParameterType, IfStatement, LValue, LoopStatement, Mir, Module, ModuleId,
    ModuleInstance, Port, PortDirection, PortId, Process, ProcessId, ProcessKind, ReduceOp,
    SensitivityList, Signal, SignalId, Statement, StructField, StructType, UnaryOp, UnionType,
    Value, Variable, VariableId,
};
pub use optimize::{ConstantFolding, DeadCodeElimination, OptimizationPass};

use anyhow::Result;
use skalp_frontend::Hir;

/// Lower HIR to MIR
pub fn lower_to_mir(hir: &Hir) -> Result<Mir> {
    // Use the actual HIR to MIR transformer
    let mut transformer = HirToMir::new();
    let mir = transformer.transform(hir);

    // Debug info removed

    Ok(mir)
}
