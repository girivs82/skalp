#![allow(dead_code, unused_variables, unused_imports)]
//! SKALP MIR - Mid-level Intermediate Representation
//!
//! This crate handles:
//! - HIR to MIR transformation
//! - Process generation (always blocks)
//! - Optimization passes
//! - Preparation for code generation

pub mod mir;
pub mod transform;
pub mod optimize;
pub mod timing;
pub mod hir_to_mir;
pub mod codegen;
pub mod compiler;
pub mod cdc_analysis;

// Re-export main types
pub use mir::{
    Mir, Module, ModuleId,
    Port, PortId, PortDirection,
    Signal, SignalId, Variable, VariableId,
    DataType, Process, ProcessId, ProcessKind,
    SensitivityList, EdgeSensitivity, EdgeType,
    Block, Statement, Assignment, AssignmentKind,
    LValue, Expression, Value,
    BinaryOp, UnaryOp, ReduceOp,
    IfStatement, CaseStatement, LoopStatement,
    ContinuousAssign, ModuleInstance,
    GenericParameter, GenericParameterType,
    StructType, StructField, EnumType, EnumVariant, UnionType,
};
pub use hir_to_mir::HirToMir;
pub use compiler::{MirCompiler, OptimizationLevel, compile_hir_to_verilog, compile_hir_to_verilog_optimized};
pub use optimize::{OptimizationPass, DeadCodeElimination, ConstantFolding};
pub use codegen::SystemVerilogGenerator;
pub use cdc_analysis::{CdcAnalyzer, CdcViolation, CdcViolationType, CdcSeverity};

use skalp_frontend::Hir;
use anyhow::Result;

/// Lower HIR to MIR
pub fn lower_to_mir(hir: &Hir) -> Result<Mir> {
    // Simplified lowering - would be more complex in production
    let mut mir = Mir::new(hir.name.clone());

    // Create a basic module for now
    let module = Module {
        id: ModuleId(0),
        name: "main".to_string(),
        parameters: Vec::new(),
        ports: Vec::new(),
        signals: Vec::new(),
        variables: Vec::new(),
        processes: Vec::new(),
        assignments: Vec::new(),
        instances: Vec::new(),
        clock_domains: Vec::new(),
    };

    mir.add_module(module);
    Ok(mir)
}