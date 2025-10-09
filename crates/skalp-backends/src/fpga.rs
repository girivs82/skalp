//! FPGA synthesis backends
//!
//! Provides synthesis support for various FPGA families including iCE40, Xilinx, and Intel.

use crate::{
    Backend, BackendError, BackendResult, FpgaTarget, SynthesisConfig, SynthesisResults,
    TargetPlatform,
};
use serde::{Deserialize, Serialize};
use std::path::Path;
use tempfile::TempDir;

pub mod ice40;
pub mod intel;
pub mod xilinx;

/// FPGA-specific synthesis configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FpgaConfig {
    /// Use DSP blocks for arithmetic
    pub use_dsp: bool,
    /// Use block RAM for memory
    pub use_bram: bool,
    /// Enable retiming optimization
    pub enable_retiming: bool,
    /// Frequency-driven place and route
    pub frequency_driven: bool,
    /// Pin assignment file
    pub pin_file: Option<String>,
}

impl Default for FpgaConfig {
    fn default() -> Self {
        Self {
            use_dsp: true,
            use_bram: true,
            enable_retiming: true,
            frequency_driven: true,
            pin_file: None,
        }
    }
}

/// Generic FPGA backend
pub struct FpgaBackend {
    target: FpgaTarget,
    config: FpgaConfig,
}

impl FpgaBackend {
    pub fn new(target: FpgaTarget, config: FpgaConfig) -> Self {
        Self { target, config }
    }

    /// Convert LIR to Verilog for FPGA synthesis
    async fn lir_to_verilog(&self, lir: &skalp_lir::LirDesign) -> BackendResult<String> {
        // Generate Verilog from LIR
        // For now, just generate from the first module
        if let Some(module) = lir.modules.first() {
            crate::verilog::generate_verilog(module)
        } else {
            // Generate a simple stub module
            Ok(format!("module {} ();\nendmodule\n", lir.name))
        }
    }

    async fn lir_to_verilog_old(&self, lir: &crate::mock_lir::Design) -> BackendResult<String> {
        let mut verilog = String::new();
        // Gate instantiations
        for gate in &lir.gates {
            match &gate.gate_type {
                crate::mock_lir::GateType::And => {
                    verilog.push_str(&format!("assign {} = ", gate.outputs[0]));
                    verilog.push_str(&gate.inputs.join(" & "));
                    verilog.push_str(";\n");
                }
                crate::mock_lir::GateType::Or => {
                    verilog.push_str(&format!("assign {} = ", gate.outputs[0]));
                    verilog.push_str(&gate.inputs.join(" | "));
                    verilog.push_str(";\n");
                }
                crate::mock_lir::GateType::Not => {
                    verilog.push_str(&format!(
                        "assign {} = ~{};\n",
                        gate.outputs[0], gate.inputs[0]
                    ));
                }
                crate::mock_lir::GateType::Xor => {
                    verilog.push_str(&format!("assign {} = ", gate.outputs[0]));
                    verilog.push_str(&gate.inputs.join(" ^ "));
                    verilog.push_str(";\n");
                }
                crate::mock_lir::GateType::Mux => {
                    if gate.inputs.len() >= 3 {
                        verilog.push_str(&format!(
                            "assign {} = {} ? {} : {};\n",
                            gate.outputs[0], gate.inputs[0], gate.inputs[1], gate.inputs[2]
                        ));
                    }
                }
                crate::mock_lir::GateType::FlipFlop => {
                    verilog.push_str(&format!("always @(posedge {}) begin\n", gate.inputs[1])); // clock
                    if gate.inputs.len() > 2 {
                        verilog.push_str(&format!("    if ({})\n", gate.inputs[2])); // reset
                        verilog.push_str(&format!("        {} <= 1'b0;\n", gate.outputs[0]));
                        verilog.push_str("    else\n");
                        verilog.push_str(&format!(
                            "        {} <= {};\n",
                            gate.outputs[0], gate.inputs[0]
                        ));
                    } else {
                        verilog
                            .push_str(&format!("    {} <= {};\n", gate.outputs[0], gate.inputs[0]));
                    }
                    verilog.push_str("end\n");
                }
                crate::mock_lir::GateType::Constant => {
                    if let Some(value) = gate.parameters.get("value") {
                        verilog.push_str(&format!("assign {} = {};\n", gate.outputs[0], value));
                    }
                }
                _ => {
                    // Handle other gate types
                    verilog.push_str(&format!("// Unsupported gate type: {:?}\n", gate.gate_type));
                }
            }
        }

        verilog.push_str("\nendmodule\n");
        Ok(verilog)
    }

    /// Run synthesis tool chain
    async fn run_synthesis(
        &self,
        verilog: &str,
        temp_dir: &Path,
        _lir: &skalp_lir::LirDesign,
    ) -> BackendResult<SynthesisResults> {
        // TODO: Convert LirDesign to Netlist for constraint generation
        // For now, pass None until full integration is complete
        let netlist = None;

        match &self.target {
            FpgaTarget::Ice40 { part, package } => {
                ice40::synthesize_ice40(verilog, part, package, temp_dir, &self.config, netlist)
                    .await
            }
            FpgaTarget::Xilinx7Series { part, package } => {
                xilinx::synthesize_xilinx(verilog, part, package, temp_dir, &self.config, netlist)
                    .await
            }
            FpgaTarget::CycloneV { part, package } => {
                intel::synthesize_intel(verilog, part, package, temp_dir, &self.config).await
            }
        }
    }
}

#[async_trait::async_trait]
impl Backend for FpgaBackend {
    async fn synthesize(
        &self,
        lir: &skalp_lir::LirDesign,
        config: &SynthesisConfig,
    ) -> BackendResult<SynthesisResults> {
        // Create temporary directory for synthesis
        let temp_dir = TempDir::new()?;
        let temp_path = temp_dir.path();

        // Convert LIR to Verilog
        let verilog = self.lir_to_verilog(lir).await?;

        // Write Verilog to file
        let verilog_file = temp_path.join("design.v");
        tokio::fs::write(&verilog_file, &verilog).await?;

        // Run synthesis
        let mut results = self.run_synthesis(&verilog, temp_path, lir).await?;

        // Update results with target information
        results.target = TargetPlatform::Fpga(self.target.clone());

        Ok(results)
    }

    fn supported_targets(&self) -> Vec<TargetPlatform> {
        vec![TargetPlatform::Fpga(self.target.clone())]
    }

    fn validate_config(&self, config: &SynthesisConfig) -> BackendResult<()> {
        match &config.target {
            TargetPlatform::Fpga(fpga_target) => {
                if fpga_target != &self.target {
                    return Err(BackendError::FpgaError("Target mismatch".to_string()));
                }
                Ok(())
            }
            _ => Err(BackendError::FpgaError("Not an FPGA target".to_string())),
        }
    }

    fn tool_version(&self) -> BackendResult<String> {
        match &self.target {
            FpgaTarget::Ice40 { .. } => Ok("Yosys + nextpnr".to_string()),
            FpgaTarget::Xilinx7Series { .. } => Ok("Vivado".to_string()),
            FpgaTarget::CycloneV { .. } => Ok("Quartus".to_string()),
        }
    }

    fn name(&self) -> &str {
        "fpga"
    }

    fn supported_devices(&self) -> Vec<String> {
        vec![
            "ice40hx1k".to_string(),
            "xc7a35t".to_string(),
            "cyclonev".to_string(),
        ]
    }

    fn validate_design(&self, _lir: &skalp_lir::LirDesign) -> BackendResult<()> {
        Ok(())
    }
}

/// Create FPGA backend for the specified target
pub fn create_fpga_backend(target: &FpgaTarget) -> BackendResult<Box<dyn Backend>> {
    let config = FpgaConfig::default();
    let backend = FpgaBackend::new(target.clone(), config);
    Ok(Box::new(backend))
}

/// Get available FPGA targets
pub fn available_fpga_targets() -> Vec<TargetPlatform> {
    vec![
        TargetPlatform::Fpga(FpgaTarget::Ice40 {
            part: "iCE40HX8K".to_string(),
            package: "CT256".to_string(),
        }),
        TargetPlatform::Fpga(FpgaTarget::Xilinx7Series {
            part: "xc7a35t".to_string(),
            package: "cpg236".to_string(),
        }),
        TargetPlatform::Fpga(FpgaTarget::CycloneV {
            part: "5CGXFC7C7F23C8".to_string(),
            package: "FBGA".to_string(),
        }),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{OptimizationGoals, OptimizationTarget};
    use std::collections::HashMap;

    #[test]
    fn test_fpga_config_default() {
        let config = FpgaConfig::default();
        assert!(config.use_dsp);
        assert!(config.use_bram);
        assert!(config.enable_retiming);
        assert!(config.frequency_driven);
        assert!(config.pin_file.is_none());
    }

    #[test]
    fn test_create_fpga_backend() {
        let target = FpgaTarget::Ice40 {
            part: "iCE40HX8K".to_string(),
            package: "CT256".to_string(),
        };

        let backend = create_fpga_backend(&target);
        assert!(backend.is_ok());
    }

    #[test]
    fn test_available_fpga_targets() {
        let targets = available_fpga_targets();
        assert!(!targets.is_empty());
        assert!(targets.iter().all(|t| matches!(t, TargetPlatform::Fpga(_))));
    }

    #[tokio::test]
    async fn test_fpga_backend_validation() {
        let target = FpgaTarget::Ice40 {
            part: "iCE40HX8K".to_string(),
            package: "CT256".to_string(),
        };

        let backend = FpgaBackend::new(target.clone(), FpgaConfig::default());

        let config = SynthesisConfig {
            target: TargetPlatform::Fpga(target),
            optimization: OptimizationGoals {
                primary: OptimizationTarget::Performance,
                max_area_utilization: Some(0.8),
                target_frequency: Some(100.0),
                max_power: Some(500.0),
            },
            timing_constraints: vec![],
            power_constraints: None,
            output_dir: "/tmp".to_string(),
            tool_options: HashMap::new(),
        };

        assert!(backend.validate_config(&config).is_ok());
    }
}
