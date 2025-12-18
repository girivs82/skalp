//! FPGA synthesis backends - to be reimplemented for GateNetlist
//!
//! This module needs to be updated to work with GateNetlist from the
//! technology mapping flow instead of the legacy Lir type.

pub mod ice40;
pub mod intel;
pub mod xilinx;
