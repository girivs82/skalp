//! SKALP LIR - Low-level Intermediate Representation
//!
//! This crate handles:
//! - Gate-level representation
//! - Technology mapping
//! - Netlist generation
//! - Physical primitives

pub mod lir;
pub mod netlist;
pub mod primitives;
pub mod technology;

pub use lir::Lir;
pub use netlist::Netlist;