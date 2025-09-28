//! Xilinx Vivado backend for SKALP

use crate::{Backend, BackendError, BackendResult, SynthesisConfig, SynthesisResults};
use skalp_lir::LirDesign;
use async_trait::async_trait;
use std::process::Command;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

/// Xilinx Vivado backend
pub struct XilinxBackend {
    /// Path to Vivado installation
    vivado_path: PathBuf,
    /// Target device family
    device_family: XilinxFamily,
    /// Synthesis strategy
    strategy: SynthesisStrategy,
}

#[derive(Debug, Clone)]
pub enum XilinxFamily {
    /// Virtex UltraScale+
    VirtexUltraScalePlus,
    /// Kintex UltraScale+
    KintexUltraScalePlus,
    /// Zynq UltraScale+ MPSoC
    ZynqUltraScalePlus,
    /// Artix-7
    Artix7,
    /// Spartan-7
    Spartan7,
    /// Versal ACAP
    Versal,
}

#[derive(Debug, Clone)]
pub enum SynthesisStrategy {
    /// Default balanced strategy
    Default,
    /// Optimize for performance
    Performance,
    /// Optimize for area
    Area,
    /// Optimize for power
    Power,
    /// High-effort optimization
    HighEffort,
}

impl XilinxBackend {
    pub fn new(device_family: XilinxFamily) -> Self {
        Self {
            vivado_path: Self::find_vivado().unwrap_or_else(|| PathBuf::from("vivado")),
            device_family,
            strategy: SynthesisStrategy::Default,
        }
    }

    pub fn with_strategy(mut self, strategy: SynthesisStrategy) -> Self {
        self.strategy = strategy;
        self
    }

    fn find_vivado() -> Option<PathBuf> {
        // Look for Vivado in standard locations
        let locations = [
            "/opt/Xilinx/Vivado",
            "/tools/Xilinx/Vivado",
            "C:/Xilinx/Vivado",
        ];

        for loc in &locations {
            let path = Path::new(loc);
            if path.exists() {
                // Find latest version
                if let Ok(entries) = fs::read_dir(path) {
                    for entry in entries.flatten() {
                        let version_path = entry.path();
                        if version_path.is_dir() {
                            let bin = version_path.join("bin").join("vivado");
                            if bin.exists() {
                                return Some(bin);
                            }
                        }
                    }
                }
            }
        }

        // Check if vivado is in PATH
        if Command::new("vivado").arg("-version").output().is_ok() {
            return Some(PathBuf::from("vivado"));
        }

        None
    }

    fn generate_tcl(&self, lir: &LirDesign, output_dir: &Path) -> Result<String, BackendError> {
        let mut tcl = String::new();

        // Project setup
        tcl.push_str(&format!(
            "# SKALP generated TCL script for Xilinx Vivado\n\
             # Design: {}\n\n",
            lir.name
        ));

        // Create project
        let device = self.get_device_part();
        tcl.push_str(&format!(
            "create_project {} {} -part {}\n\n",
            lir.name,
            output_dir.display(),
            device
        ));

        // Add source files
        tcl.push_str("# Add source files\n");
        for module in &lir.modules {
            let verilog_file = format!("{}.v", module.name);
            tcl.push_str(&format!("add_files {}\n", verilog_file));
        }
        tcl.push_str("\n");

        // Set top module
        if let Some(top) = lir.modules.first() {
            tcl.push_str(&format!("set_property top {} [current_fileset]\n\n", top.name));
        }

        // Synthesis settings based on strategy
        tcl.push_str("# Synthesis settings\n");
        match self.strategy {
            SynthesisStrategy::Performance => {
                tcl.push_str("set_property strategy Performance_ExploreWithRemap [get_runs synth_1]\n");
            }
            SynthesisStrategy::Area => {
                tcl.push_str("set_property strategy Area_OptimizedRegBalance [get_runs synth_1]\n");
            }
            SynthesisStrategy::Power => {
                tcl.push_str("set_property strategy Power_DefaultOpt [get_runs synth_1]\n");
            }
            SynthesisStrategy::HighEffort => {
                tcl.push_str("set_property strategy Performance_ExtraTimingOpt [get_runs synth_1]\n");
                tcl.push_str("set_property STEPS.SYNTH_DESIGN.ARGS.DIRECTIVE ExtraNetDelay [get_runs synth_1]\n");
            }
            _ => {}
        }

        // Run synthesis
        tcl.push_str("\n# Run synthesis\n");
        tcl.push_str("launch_runs synth_1 -jobs 8\n");
        tcl.push_str("wait_on_run synth_1\n\n");

        // Implementation settings
        tcl.push_str("# Implementation settings\n");
        match self.strategy {
            SynthesisStrategy::Performance => {
                tcl.push_str("set_property strategy Performance_ExtraTimingOpt [get_runs impl_1]\n");
            }
            SynthesisStrategy::Area => {
                tcl.push_str("set_property strategy Area_Explore [get_runs impl_1]\n");
            }
            SynthesisStrategy::Power => {
                tcl.push_str("set_property strategy Power_ExploreArea [get_runs impl_1]\n");
            }
            _ => {}
        }

        // Run implementation
        tcl.push_str("\n# Run implementation\n");
        tcl.push_str("launch_runs impl_1 -jobs 8\n");
        tcl.push_str("wait_on_run impl_1\n\n");

        // Generate bitstream
        tcl.push_str("# Generate bitstream\n");
        tcl.push_str("launch_runs impl_1 -to_step write_bitstream -jobs 8\n");
        tcl.push_str("wait_on_run impl_1\n\n");

        // Report generation
        tcl.push_str("# Generate reports\n");
        tcl.push_str("open_run synth_1 -name synth_1\n");
        tcl.push_str(&format!(
            "report_utilization -file {}/utilization.rpt\n",
            output_dir.display()
        ));
        tcl.push_str(&format!(
            "report_timing_summary -file {}/timing.rpt\n",
            output_dir.display()
        ));
        tcl.push_str(&format!(
            "report_power -file {}/power.rpt\n",
            output_dir.display()
        ));

        Ok(tcl)
    }

    fn get_device_part(&self) -> &str {
        match self.device_family {
            XilinxFamily::VirtexUltraScalePlus => "xcvu9p-flga2104-2L-e",
            XilinxFamily::KintexUltraScalePlus => "xcku15p-ffva1760-2-e",
            XilinxFamily::ZynqUltraScalePlus => "xczu9eg-ffvb1156-2-e",
            XilinxFamily::Artix7 => "xc7a100tcsg324-1",
            XilinxFamily::Spartan7 => "xc7s50csga324-1",
            XilinxFamily::Versal => "xcvc1902-vsva2197-2MP-e",
        }
    }

    async fn run_vivado(&self, tcl_file: &Path) -> Result<String, BackendError> {
        let output = Command::new(&self.vivado_path)
            .arg("-mode")
            .arg("batch")
            .arg("-source")
            .arg(tcl_file)
            .output()
            .map_err(|e| BackendError::ToolNotFound(format!("Vivado: {}", e)))?;

        if !output.status.success() {
            return Err(BackendError::ToolFailed(
                String::from_utf8_lossy(&output.stderr).to_string()
            ));
        }

        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }

    fn parse_reports(&self, output_dir: &Path) -> Result<SynthesisResults, BackendError> {
        let mut results = SynthesisResults::default();

        // Parse utilization report
        let util_report = output_dir.join("utilization.rpt");
        if util_report.exists() {
            let content = fs::read_to_string(&util_report)
                .map_err(|e| BackendError::Io(e))?;

            // Extract resource usage (simplified parsing)
            if let Some(lut_line) = content.lines().find(|l| l.contains("Slice LUTs")) {
                if let Some(num) = lut_line.split_whitespace().nth(3) {
                    results.luts = num.parse().unwrap_or(0);
                }
            }
            if let Some(ff_line) = content.lines().find(|l| l.contains("Slice Registers")) {
                if let Some(num) = ff_line.split_whitespace().nth(3) {
                    results.registers = num.parse().unwrap_or(0);
                }
            }
        }

        // Parse timing report
        let timing_report = output_dir.join("timing.rpt");
        if timing_report.exists() {
            let content = fs::read_to_string(&timing_report)
                .map_err(|e| BackendError::Io(e))?;

            // Extract max frequency (simplified parsing)
            if let Some(wns_line) = content.lines().find(|l| l.contains("WNS(ns)")) {
                if let Some(wns) = wns_line.split_whitespace().nth(1) {
                    if let Ok(wns_val) = wns.parse::<f64>() {
                        // Calculate frequency from worst negative slack
                        let period = 10.0; // Assume 10ns = 100MHz target
                        let actual_period = period - wns_val;
                        results.max_frequency = (1000.0 / actual_period) as u32;
                    }
                }
            }
        }

        // Parse power report
        let power_report = output_dir.join("power.rpt");
        if power_report.exists() {
            let content = fs::read_to_string(&power_report)
                .map_err(|e| BackendError::Io(e))?;

            // Extract total power (simplified parsing)
            if let Some(power_line) = content.lines().find(|l| l.contains("Total On-Chip Power")) {
                if let Some(power) = power_line.split_whitespace().nth(4) {
                    results.power_mw = power.parse().unwrap_or(0.0);
                }
            }
        }

        // Set bitstream path
        let bitstream = output_dir.join(&format!("{}.bit", results.top_module));
        if bitstream.exists() {
            results.bitstream = Some(bitstream);
        }

        Ok(results)
    }
}

#[async_trait]
impl Backend for XilinxBackend {
    async fn synthesize(
        &self,
        lir: &LirDesign,
        _config: &SynthesisConfig,
    ) -> BackendResult<SynthesisResults> {
        // Create temp directory for synthesis
        let temp_dir = TempDir::new()
            .map_err(|e| BackendError::Io(e))?;
        let work_dir = temp_dir.path();

        // Generate Verilog files
        for module in &lir.modules {
            let verilog = crate::verilog::generate_verilog(module)?;
            let file_path = work_dir.join(&format!("{}.v", module.name));
            fs::write(&file_path, verilog)
                .map_err(|e| BackendError::Io(e))?;
        }

        // Generate TCL script
        let tcl_content = self.generate_tcl(lir, work_dir)?;
        let tcl_file = work_dir.join("synthesis.tcl");
        fs::write(&tcl_file, tcl_content)
            .map_err(|e| BackendError::Io(e))?;

        // Run Vivado
        let output = self.run_vivado(&tcl_file).await?;
        log::info!("Vivado output: {}", output);

        // Parse results
        let mut results = self.parse_reports(work_dir)?;
        results.backend = "xilinx_vivado".to_string();
        results.device = self.get_device_part().to_string();
        results.top_module = lir.name.clone();

        Ok(results)
    }

    fn name(&self) -> &str {
        "xilinx_vivado"
    }

    fn supported_devices(&self) -> Vec<String> {
        vec![
            "xcvu9p".to_string(),
            "xcku15p".to_string(),
            "xczu9eg".to_string(),
            "xc7a100t".to_string(),
            "xc7s50".to_string(),
            "xcvc1902".to_string(),
        ]
    }

    fn validate_design(&self, _lir: &LirDesign) -> BackendResult<()> {
        // Check if Vivado is available
        if !self.vivado_path.exists() && self.vivado_path != PathBuf::from("vivado") {
            return Err(BackendError::ToolNotFound(
                "Vivado not found. Please install Xilinx Vivado.".to_string()
            ));
        }

        Ok(())
    }
}

/// XDC (Xilinx Design Constraints) generator
pub struct XdcGenerator {
    constraints: Vec<XdcConstraint>,
}

#[derive(Debug, Clone)]
pub enum XdcConstraint {
    /// Pin location constraint
    PinLocation { port: String, pin: String },
    /// I/O standard constraint
    IoStandard { port: String, standard: String },
    /// Clock constraint
    Clock { port: String, period_ns: f64 },
    /// False path
    FalsePath { from: String, to: String },
    /// Multi-cycle path
    MultiCyclePath { from: String, to: String, cycles: u32 },
}

impl XdcGenerator {
    pub fn new() -> Self {
        Self {
            constraints: Vec::new(),
        }
    }

    pub fn add_constraint(&mut self, constraint: XdcConstraint) {
        self.constraints.push(constraint);
    }

    pub fn generate(&self) -> String {
        let mut xdc = String::new();
        xdc.push_str("## SKALP Generated XDC Constraints\n\n");

        for constraint in &self.constraints {
            match constraint {
                XdcConstraint::PinLocation { port, pin } => {
                    xdc.push_str(&format!("set_property PACKAGE_PIN {} [get_ports {{{}}}]\n", pin, port));
                }
                XdcConstraint::IoStandard { port, standard } => {
                    xdc.push_str(&format!("set_property IOSTANDARD {} [get_ports {{{}}}]\n", standard, port));
                }
                XdcConstraint::Clock { port, period_ns } => {
                    xdc.push_str(&format!("create_clock -period {} [get_ports {{{}}}]\n", period_ns, port));
                }
                XdcConstraint::FalsePath { from, to } => {
                    xdc.push_str(&format!("set_false_path -from [get_pins {{{}}}] -to [get_pins {{{}}}]\n", from, to));
                }
                XdcConstraint::MultiCyclePath { from, to, cycles } => {
                    xdc.push_str(&format!(
                        "set_multicycle_path {} -from [get_pins {{{}}}] -to [get_pins {{{}}}]\n",
                        cycles, from, to
                    ));
                }
            }
        }

        xdc
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_xdc_generation() {
        let mut gen = XdcGenerator::new();

        gen.add_constraint(XdcConstraint::PinLocation {
            port: "clk".to_string(),
            pin: "E3".to_string(),
        });

        gen.add_constraint(XdcConstraint::IoStandard {
            port: "clk".to_string(),
            standard: "LVCMOS33".to_string(),
        });

        gen.add_constraint(XdcConstraint::Clock {
            port: "clk".to_string(),
            period_ns: 10.0,
        });

        let xdc = gen.generate();
        assert!(xdc.contains("PACKAGE_PIN E3"));
        assert!(xdc.contains("IOSTANDARD LVCMOS33"));
        assert!(xdc.contains("create_clock -period 10"));
    }

    #[test]
    fn test_device_part_selection() {
        let backend = XilinxBackend::new(XilinxFamily::Artix7);
        assert_eq!(backend.get_device_part(), "xc7a100tcsg324-1");

        let backend = XilinxBackend::new(XilinxFamily::VirtexUltraScalePlus);
        assert_eq!(backend.get_device_part(), "xcvu9p-flga2104-2L-e");
    }
}