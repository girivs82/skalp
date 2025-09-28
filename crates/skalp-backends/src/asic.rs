//! ASIC synthesis backends
//!
//! Provides synthesis support for ASIC targets using standard cell libraries.

use crate::{
    Backend, BackendError, BackendResult, AsicTarget, LogLevel, LogMessage, SynthesisConfig,
    SynthesisResults, TargetPlatform, AreaMetrics, TimingResults, PowerResults, OutputFile,
    OutputFileType,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;
use tempfile::TempDir;

pub mod generic;
pub mod sky130;
pub mod freepdk45;

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

/// Generic ASIC backend
pub struct AsicBackend {
    target: AsicTarget,
    config: AsicConfig,
}

impl AsicBackend {
    pub fn new(target: AsicTarget, config: AsicConfig) -> Self {
        Self { target, config }
    }

    /// Convert LIR to structural Verilog for ASIC synthesis
    async fn lir_to_structural_verilog(&self, lir: &skalp_lir::LirDesign) -> BackendResult<String> {
        // Generate structural Verilog from LIR - more detailed than FPGA version
        // Use the Verilog generation utility
        crate::verilog::generate_verilog(&lir.modules[0])
    }

    /// Run ASIC synthesis flow
    async fn run_asic_synthesis(&self, verilog: &str, temp_dir: &Path) -> BackendResult<SynthesisResults> {
        match &self.target {
            AsicTarget::Generic { library_name, process_node } => {
                generic::synthesize_generic(verilog, library_name, process_node, temp_dir, &self.config).await
            }
            AsicTarget::FreePdk45 => {
                freepdk45::synthesize_freepdk45(verilog, temp_dir, &self.config).await
            }
            AsicTarget::Sky130 => {
                sky130::synthesize_sky130(verilog, temp_dir, &self.config).await
            }
        }
    }
}

#[async_trait::async_trait]
impl Backend for AsicBackend {
    async fn synthesize(
        &self,
        lir: &skalp_lir::LirDesign,
        config: &SynthesisConfig,
    ) -> BackendResult<SynthesisResults> {
        // Create temporary directory for synthesis
        let temp_dir = TempDir::new()?;
        let temp_path = temp_dir.path();

        // Convert LIR to structural Verilog
        let verilog = self.lir_to_structural_verilog(lir).await?;

        // Write Verilog to file
        let verilog_file = temp_path.join("design.v");
        tokio::fs::write(&verilog_file, &verilog).await?;

        // Run ASIC synthesis
        let mut results = self.run_asic_synthesis(&verilog, temp_path).await?;

        // Update results with target information
        results.target = TargetPlatform::Asic(self.target.clone());

        Ok(results)
    }

    fn supported_targets(&self) -> Vec<TargetPlatform> {
        vec![TargetPlatform::Asic(self.target.clone())]
    }

    fn validate_config(&self, config: &SynthesisConfig) -> BackendResult<()> {
        match &config.target {
            TargetPlatform::Asic(asic_target) => {
                if asic_target != &self.target {
                    return Err(BackendError::AsicError(
                        "Target mismatch".to_string()
                    ));
                }
                Ok(())
            }
            _ => Err(BackendError::AsicError(
                "Not an ASIC target".to_string()
            )),
        }
    }

    fn tool_version(&self) -> BackendResult<String> {
        match &self.target {
            AsicTarget::Generic { .. } => Ok("Generic ASIC flow".to_string()),
            AsicTarget::FreePdk45 => Ok("FreePDK45 + OpenROAD".to_string()),
            AsicTarget::Sky130 => Ok("SkyWater 130nm + OpenROAD".to_string()),
        }
    }

    fn name(&self) -> &str {
        "asic"
    }

    fn supported_devices(&self) -> Vec<String> {
        vec![
            "freepdk45".to_string(),
            "sky130".to_string(),
            "generic".to_string(),
        ]
    }

    fn validate_design(&self, _lir: &skalp_lir::LirDesign) -> BackendResult<()> {
        Ok(())
    }
}

/// Create ASIC backend for the specified target
pub fn create_asic_backend(target: &AsicTarget) -> BackendResult<Box<dyn Backend>> {
    let config = AsicConfig::default();
    let backend = AsicBackend::new(target.clone(), config);
    Ok(Box::new(backend))
}

/// Get available ASIC targets
pub fn available_asic_targets() -> Vec<TargetPlatform> {
    vec![
        TargetPlatform::Asic(AsicTarget::Generic {
            library_name: "generic_stdcells".to_string(),
            process_node: "28nm".to_string(),
        }),
        TargetPlatform::Asic(AsicTarget::FreePdk45),
        TargetPlatform::Asic(AsicTarget::Sky130),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{OptimizationGoals, OptimizationTarget};

    #[test]
    fn test_asic_config_default() {
        let config = AsicConfig::default();
        assert_eq!(config.target_utilization, 0.7);
        assert!(config.clock_gating);
        assert!(matches!(config.power_optimization, PowerOptLevel::Medium));
    }

    #[test]
    fn test_create_asic_backend() {
        let target = AsicTarget::FreePdk45;
        let backend = create_asic_backend(&target);
        assert!(backend.is_ok());
    }

    #[test]
    fn test_available_asic_targets() {
        let targets = available_asic_targets();
        assert!(!targets.is_empty());
        assert!(targets.iter().all(|t| matches!(t, TargetPlatform::Asic(_))));
    }

    #[tokio::test]
    async fn test_asic_backend_validation() {
        let target = AsicTarget::Sky130;
        let backend = AsicBackend::new(target.clone(), AsicConfig::default());

        let config = SynthesisConfig {
            target: TargetPlatform::Asic(target),
            optimization: OptimizationGoals {
                primary: OptimizationTarget::Area,
                max_area_utilization: Some(0.7),
                target_frequency: Some(500.0),
                max_power: Some(100.0),
            },
            timing_constraints: vec![],
            power_constraints: None,
            output_dir: "/tmp".to_string(),
            tool_options: HashMap::new(),
        };

        assert!(backend.validate_config(&config).is_ok());
    }
}