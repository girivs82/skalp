#![allow(dead_code, unused_variables, unused_imports)]
//! SKALP Verification Framework
//!
//! This crate provides verification capabilities including:
//! - Immediate and concurrent assertions
//! - Property and sequence definitions
//! - Coverage collection and reporting
//! - Requirement tracking
//! - Formal verification support

pub mod assertions;
pub mod coverage;
pub mod properties;
pub mod requirements;
pub mod testbench;

#[cfg(all(feature = "formal", not(tarpaulin)))]
pub mod formal;

pub use assertions::{Assertion, AssertionKind, ConcurrentAssertion, ImmediateAssertion};
pub use coverage::{Coverage, CoverageMetrics, CoverageReport};
pub use properties::{Property, Sequence, TemporalOperator};
pub use requirements::{Requirement, RequirementStatus, RequirementTracker};
pub use testbench::{AsyncTask, TestbenchBuilder};

use thiserror::Error;

/// Verification errors
#[derive(Error, Debug)]
pub enum VerifyError {
    #[error("Assertion failed: {0}")]
    AssertionFailed(String),

    #[error("Property violation: {0}")]
    PropertyViolation(String),

    #[error("Coverage target not met: {0}")]
    CoverageInsufficient(String),

    #[error("Requirement not satisfied: {0}")]
    RequirementNotMet(String),

    #[error("Formal verification failed: {0}")]
    FormalVerificationFailed(String),

    #[error("Invalid property specification: {0}")]
    InvalidProperty(String),
}

/// Verification configuration
#[derive(Debug, Clone)]
pub struct VerifyConfig {
    /// Enable assertion checking
    pub enable_assertions: bool,

    /// Enable coverage collection
    pub enable_coverage: bool,

    /// Coverage target percentage
    pub coverage_target: f64,

    /// Enable formal verification
    pub enable_formal: bool,

    /// Maximum bound for bounded model checking
    pub bmc_bound: usize,

    /// Timeout for formal verification (seconds)
    pub formal_timeout: u64,
}

impl Default for VerifyConfig {
    fn default() -> Self {
        Self {
            enable_assertions: true,
            enable_coverage: true,
            coverage_target: 90.0,
            enable_formal: false,
            bmc_bound: 100,
            formal_timeout: 60,
        }
    }
}
