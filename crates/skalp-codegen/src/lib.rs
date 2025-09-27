//! SKALP Code Generation
//!
//! This crate handles:
//! - SystemVerilog generation
//! - VHDL generation
//! - Verilog generation
//! - Timing constraint file generation

pub mod systemverilog;
pub mod vhdl;
pub mod verilog;
pub mod constraints;

pub trait CodeGen {
    fn generate(&self, target: Target) -> String;
}

pub enum Target {
    SystemVerilog,
    Vhdl,
    Verilog,
}