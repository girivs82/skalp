//! Physical constraint validation and device database
//!
//! This module provides device-specific constraint validation for FPGA targets.

pub mod devices;
pub mod validator;

pub use devices::{Bank, Device, DeviceDatabase, FpgaFamily, Pin};
pub use validator::{ConstraintError, ConstraintValidator};
