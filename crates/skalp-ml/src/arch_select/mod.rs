//! Architecture Selection for Datapath Operations
//!
//! This module provides ML-guided selection of datapath architectures
//! for operations like addition, multiplication, and shifting.
//!
//! # Architectures
//!
//! For adders:
//! - Ripple-carry: Simple, low area, high delay
//! - Carry-lookahead (CLA): Medium area, medium delay
//! - Kogge-Stone: Parallel prefix, low delay, high area
//! - Brent-Kung: Balanced parallel prefix
//!
//! For multipliers:
//! - Array: Simple, predictable
//! - Wallace tree: Fast, more complex
//! - Booth encoding: Efficient for signed
//!
//! # Decision Factors
//!
//! - Bitwidth of operands
//! - Timing constraints (available slack)
//! - Area budget
//! - Power constraints

mod classifier;

pub use classifier::{ArchAdvisor, ArchAdvisorConfig, DatapathArchitecture, OperationType};
