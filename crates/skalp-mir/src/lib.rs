//! SKALP MIR - Mid-level Intermediate Representation
//!
//! This crate handles:
//! - Architecture-aware transformations
//! - Optimization passes
//! - Timing analysis and constraint propagation
//! - Resource allocation

pub mod mir;
pub mod optimize;
pub mod timing;
pub mod transform;

pub use mir::Mir;