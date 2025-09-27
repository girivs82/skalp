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