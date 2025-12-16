#![allow(dead_code, unused_variables, unused_imports)]
//! # SKALP Safety Framework
//!
//! ISO 26262 functional safety implementation for SKALP hardware synthesis.
//! Provides ASIL-compliant safety requirements, FMEA generation, and safety mechanisms.

use serde::{Deserialize, Serialize};
use thiserror::Error;

pub mod analysis;
pub mod asil;
pub mod common_cause;
pub mod coverage_verification;
pub mod design_resolver;
pub mod design_rules;
pub mod diversity;
pub mod do254;
pub mod fault_simulation;
pub mod fmea;
pub mod fmeda;
pub mod fmeda_library;
pub mod fta;
pub mod gate_netlist_integration;
pub mod hierarchy;
pub mod hsi;
pub mod iec61508;
pub mod mechanisms;
pub mod metrics;
pub mod pipeline;
pub mod power_domains;
pub mod requirements;
pub mod safety_case;
pub mod sm_failure_analysis;
pub mod tool_qualification;
pub mod traits;
pub mod workproducts;

// Re-export fault simulation integration functions when sim-integration feature is enabled
#[cfg(feature = "sim-integration")]
pub use fault_simulation::{build_primitive_path_map, fault_campaign_to_safety_results};

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
}

/// Configuration for safety analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyConfig {
    /// Target ASIL level for the design
    pub target_asil: asil::AsilLevel,
    /// Enable FMEA generation
    pub enable_fmea: bool,
    /// Enable safety metrics calculation
    pub enable_metrics: bool,
    /// Enable power domain analysis
    pub enable_power_domains: bool,
    /// Safety mechanism requirements
    pub mechanism_requirements: Vec<String>,
}

impl Default for SafetyConfig {
    fn default() -> Self {
        Self {
            target_asil: asil::AsilLevel::A,
            enable_fmea: true,
            enable_metrics: true,
            enable_power_domains: false,
            mechanism_requirements: vec![],
        }
    }
}

/// Result type for safety operations
pub type SafetyResult<T> = Result<T, SafetyError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_safety_config_default() {
        let config = SafetyConfig::default();
        assert_eq!(config.target_asil, asil::AsilLevel::A);
        assert!(config.enable_fmea);
        assert!(config.enable_metrics);
        assert!(!config.enable_power_domains);
    }
}
