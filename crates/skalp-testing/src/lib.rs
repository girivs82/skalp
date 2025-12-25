//! SKALP Testing Framework
//!
//! High-level testing utilities for hardware verification.
//!
//! This module provides:
//! - Ergonomic testbench API for writing hardware tests
//! - Compilation cache for fast test iteration
//! - Test vector generation and golden reference comparison

pub mod cache;
pub mod golden;
pub mod testbench;

// Re-export common types for ergonomic use
pub use cache::CompilationCache;
pub use golden::GoldenTest;
pub use testbench::{
    FromSignalValue, GateLevelTestbench, IntoSignalValue, TestSequence, Testbench,
};
