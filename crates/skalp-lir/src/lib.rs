//! SKALP LIR - Low-level Intermediate Representation
//!
//! This crate handles:
//! - Gate-level representation
//! - Technology mapping
//! - Netlist generation
//! - Physical primitives
//! - MIR to LIR transformation
//! - Optimization passes
//! - Timing analysis

pub mod lir;
pub mod mir_to_lir;
pub mod netlist;
pub mod optimization;
pub mod primitives;
pub mod tech_mapping;
pub mod technology;
pub mod timing;

pub use lir::{Gate, GateType, Lir, Net};
pub use mir_to_lir::transform_mir_to_lir;
pub use optimization::{OptimizationPipeline, OptimizationResult};
pub use netlist::Netlist;