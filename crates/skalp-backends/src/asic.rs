//! ASIC synthesis backends - to be reimplemented for GateNetlist
//!
//! This module needs to be updated to work with GateNetlist from the
//! technology mapping flow instead of the legacy Lir type.

use serde::{Deserialize, Serialize};

pub mod freepdk45;
pub mod generic;
pub mod sky130;

/// ASIC-specific synthesis configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AsicConfig {
    /// Standard cell library path
    pub liberty_file: Option<String>,
    /// Technology LEF file
    pub tech_lef: Option<String>,
    /// Standard cell LEF file
    pub cell_lef: Option<String>,
    /// Target utilization percentage
    pub target_utilization: f64,
    /// Enable clock gating
    pub clock_gating: bool,
    /// Power optimization level
    pub power_optimization: PowerOptLevel,
}

impl Default for AsicConfig {
    fn default() -> Self {
        Self {
            liberty_file: None,
            tech_lef: None,
            cell_lef: None,
            target_utilization: 0.7,
            clock_gating: true,
            power_optimization: PowerOptLevel::Medium,
        }
    }
}

/// Power optimization levels
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PowerOptLevel {
    None,
    Low,
    Medium,
    High,
    Aggressive,
}
