#![allow(dead_code, unused_variables, unused_imports)]
//! # SKALP Safety Framework
//!
//! ISO 26262 functional safety implementation for SKALP hardware synthesis.
//! Provides ASIL-compliant safety requirements, FMEA generation, and safety mechanisms.
//!
//! NOTE: Several modules have been stubbed out pending migration to GateNetlist.

use serde::{Deserialize, Serialize};
use thiserror::Error;

// Core modules (working)
pub mod analysis;
pub mod asil;
pub mod common_cause;
pub mod design_resolver;
pub mod design_rules;
pub mod diversity;
pub mod do254;
pub mod fmea;
pub mod fmeda;
pub mod fmeda_library;
pub mod fta;
pub mod hierarchy;
pub mod hsi;
pub mod iec61508;
pub mod mechanisms;
pub mod metrics;
pub mod pipeline;
pub mod power_domains;
pub mod requirements;
pub mod safety_case;
pub mod seooc;
pub mod sm_failure_analysis;
pub mod tool_qualification;
pub mod traits;
pub mod uncertainty;
pub mod workproducts;

// Modules pending migration to GateNetlist (stubbed)
pub mod bist_generation;
pub mod coverage_verification;
pub mod fault_diagnostics;
pub mod fault_simulation;
pub mod gate_netlist_integration;
pub mod power_infrastructure;
pub mod safety_driven_fmea;
pub mod safety_power_verification;

// Re-export SEooC analysis types
pub use seooc::{
    analyze_seooc, analyze_seooc_with_ccf, format_seooc_report, from_hir_seooc_config,
    AssumedMechanism, DcCategory, DerivedSafetyRequirement, FaultInjectionData, PowerCcfCoverage,
    PowerDomainCcfData, SeoocAnalysisConfig, SeoocAnalysisResult, UndetectedFault,
    UndetectedFaultCategories,
};

/// Safety-related errors
#[derive(Error, Debug)]
pub enum SafetyError {
    #[error("ASIL requirement violation: {0}")]
    AsilViolation(String),
    #[error("Safety mechanism failure: {0}")]
    MechanismFailure(String),
    #[error("FMEA generation failed: {0}")]
    FmeaError(String),
    #[error("Power domain isolation error: {0}")]
    PowerDomainError(String),
    #[error("Safety metrics calculation failed: {0}")]
    MetricsError(String),
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

pub type SafetyResult<T> = Result<T, SafetyError>;
