//! Intel Quartus Prime backend for SKALP

use crate::{Backend, BackendError, BackendResult, SynthesisConfig, SynthesisResults};
use async_trait::async_trait;
use skalp_lir::Lir;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

/// Intel Quartus Prime backend
pub struct IntelBackend {
    /// Path to Quartus installation
    quartus_path: PathBuf,
    /// Target device family
    device_family: IntelFamily,
    /// Optimization goal
    optimization: OptimizationGoal,
}

#[derive(Debug, Clone)]
pub enum IntelFamily {
    /// Stratix 10
    Stratix10,
    /// Arria 10
    Arria10,
    /// Cyclone 10 GX
    Cyclone10GX,
    /// Cyclone V
    CycloneV,
    /// MAX 10
    Max10,
    /// Agilex
    Agilex,
    /// eASIC N5X
    EasicN5X,
}

#[derive(Debug, Clone)]
pub enum OptimizationGoal {
    /// Balanced optimization
    Balanced,
    /// Optimize for speed
    Speed,
    /// Optimize for area
    Area,
    /// Optimize for power
    Power,
    /// Aggressive performance
    AggressivePerformance,
}

impl IntelBackend {
    pub fn new(device_family: IntelFamily) -> Self {
        Self {
            quartus_path: Self::find_quartus().unwrap_or_else(|| PathBuf::from("quartus")),
            device_family,
            optimization: OptimizationGoal::Balanced,
        }
    }

    pub fn with_optimization(mut self, opt: OptimizationGoal) -> Self {
        self.optimization = opt;
        self
    }

    fn find_quartus() -> Option<PathBuf> {
        // Look for Quartus in standard locations
        let locations = [
            "/opt/intel/intelFPGA_pro",
            "/opt/intel/intelFPGA",
            "/opt/altera",
            "C:/intelFPGA_pro",
            "C:/intelFPGA",
        ];

        for loc in &locations {
            let path = Path::new(loc);
            if path.exists() {
                // Find latest version
                if let Ok(entries) = fs::read_dir(path) {
                    for entry in entries.flatten() {
                        let version_path = entry.path();
                        if version_path.is_dir() {
                            let bin = version_path.join("quartus").join("bin").join("quartus_sh");
                            if bin.exists() {
                                return Some(bin);
                            }
                        }
                    }
                }
            }
        }

        // Check if quartus is in PATH
        if Command::new("quartus_sh").arg("--version").output().is_ok() {
            return Some(PathBuf::from("quartus_sh"));
        }

        None
    }

    fn generate_qsf(&self, lir: &Lir, _project_dir: &Path) -> Result<String, BackendError> {
        let mut qsf = String::new();

        // Header
        qsf.push_str("# SKALP Generated Quartus Settings File\n");
        qsf.push_str(&format!("# Design: {}\n\n", lir.name));

        // Device settings
        let (family, device) = self.get_device_info();
        qsf.push_str(&format!(
            "set_global_assignment -name FAMILY \"{}\"\n",
            family
        ));
        qsf.push_str(&format!("set_global_assignment -name DEVICE {}\n", device));
        qsf.push_str("set_global_assignment -name DEVICE_FILTER_PACKAGE FBGA\n");
        qsf.push_str("set_global_assignment -name DEVICE_FILTER_SPEED_GRADE FASTEST\n\n");

        // Project settings
        qsf.push_str(&format!(
            "set_global_assignment -name TOP_LEVEL_ENTITY {}\n",
            lir.name
        ));
        qsf.push_str("set_global_assignment -name ORIGINAL_QUARTUS_VERSION 21.3.0\n");
        qsf.push_str("set_global_assignment -name PROJECT_CREATION_TIME_DATE \"00:00:00  JANUARY 01, 2024\"\n");
        qsf.push_str("set_global_assignment -name LAST_QUARTUS_VERSION \"21.3.0 Pro Edition\"\n\n");

        // Add source file (single LIR = single module)
        qsf.push_str(&format!(
            "set_global_assignment -name VERILOG_FILE {}.v\n",
            lir.name
        ));
        qsf.push('\n');

        // Optimization settings based on goal
        match self.optimization {
            OptimizationGoal::Speed => {
                qsf.push_str(
                    "set_global_assignment -name OPTIMIZATION_MODE \"AGGRESSIVE PERFORMANCE\"\n",
                );
                qsf.push_str("set_global_assignment -name OPTIMIZATION_TECHNIQUE SPEED\n");
                qsf.push_str("set_global_assignment -name PHYSICAL_SYNTHESIS_COMBO_LOGIC ON\n");
                qsf.push_str(
                    "set_global_assignment -name PHYSICAL_SYNTHESIS_REGISTER_DUPLICATION ON\n",
                );
                qsf.push_str(
                    "set_global_assignment -name PHYSICAL_SYNTHESIS_REGISTER_RETIMING ON\n",
                );
            }
            OptimizationGoal::Area => {
                qsf.push_str("set_global_assignment -name OPTIMIZATION_MODE \"AGGRESSIVE AREA\"\n");
                qsf.push_str("set_global_assignment -name OPTIMIZATION_TECHNIQUE AREA\n");
                qsf.push_str("set_global_assignment -name AUTO_RESOURCE_SHARING ON\n");
                qsf.push_str("set_global_assignment -name AUTO_RAM_RECOGNITION ON\n");
            }
            OptimizationGoal::Power => {
                qsf.push_str(
                    "set_global_assignment -name OPTIMIZATION_MODE \"AGGRESSIVE POWER\"\n",
                );
                qsf.push_str("set_global_assignment -name OPTIMIZE_POWER_DURING_SYNTHESIS \"EXTRA EFFORT\"\n");
                qsf.push_str(
                    "set_global_assignment -name OPTIMIZE_POWER_DURING_FITTING \"EXTRA EFFORT\"\n",
                );
            }
            OptimizationGoal::AggressivePerformance => {
                qsf.push_str(
                    "set_global_assignment -name OPTIMIZATION_MODE \"AGGRESSIVE PERFORMANCE\"\n",
                );
                qsf.push_str("set_global_assignment -name OPTIMIZATION_TECHNIQUE SPEED\n");
                qsf.push_str(
                    "set_global_assignment -name ROUTER_TIMING_OPTIMIZATION_LEVEL MAXIMUM\n",
                );
                qsf.push_str("set_global_assignment -name PLACEMENT_EFFORT_MULTIPLIER 4.0\n");
                qsf.push_str("set_global_assignment -name ROUTER_EFFORT_MULTIPLIER 4.0\n");
                qsf.push_str("set_global_assignment -name FITTER_AGGRESSIVE_ROUTABILITY_OPTIMIZATION ALWAYS\n");
            }
            _ => {
                qsf.push_str("set_global_assignment -name OPTIMIZATION_MODE BALANCED\n");
            }
        }
        qsf.push('\n');

        // Timing settings
        qsf.push_str("# Timing Settings\n");
        qsf.push_str("set_global_assignment -name ENABLE_ADVANCED_IO_TIMING ON\n");
        qsf.push_str("set_global_assignment -name USE_LOGICLOCK_CONSTRAINTS_IN_BALANCING ON\n");
        qsf.push_str("set_global_assignment -name ENABLE_SIGNALTAP OFF\n");
        qsf.push_str("set_global_assignment -name ENABLE_LOGIC_ANALYZER_INTERFACE OFF\n\n");

        // Synthesis settings
        qsf.push_str("# Synthesis Settings\n");
        qsf.push_str("set_global_assignment -name SYNTH_TIMING_DRIVEN_SYNTHESIS ON\n");
        qsf.push_str("set_global_assignment -name AUTO_ROM_RECOGNITION ON\n");
        qsf.push_str("set_global_assignment -name AUTO_RAM_RECOGNITION ON\n");
        qsf.push_str("set_global_assignment -name AUTO_DSP_RECOGNITION ON\n");
        qsf.push_str("set_global_assignment -name AUTO_SHIFT_REGISTER_RECOGNITION AUTO\n");

        Ok(qsf)
    }

    fn generate_tcl(&self, project_name: &str) -> String {
        let mut tcl = String::new();

        tcl.push_str("# SKALP Generated TCL Script for Intel Quartus\n\n");

        // Load project
        tcl.push_str(&format!("project_open {}\n\n", project_name));

        // Run compilation flow
        tcl.push_str("# Run full compilation flow\n");
        tcl.push_str("execute_flow -compile\n\n");

        // Generate reports
        tcl.push_str("# Generate reports\n");
        tcl.push_str("load_report\n");
        tcl.push_str(
            "report_timing -setup -npaths 10 -detail full_path -panel_name \"Setup Summary\"\n",
        );
        tcl.push_str(
            "report_timing -hold -npaths 10 -detail full_path -panel_name \"Hold Summary\"\n",
        );
        tcl.push_str("report_timing -recovery -npaths 10 -detail full_path -panel_name \"Recovery Summary\"\n");
        tcl.push_str(
            "report_timing -removal -npaths 10 -detail full_path -panel_name \"Removal Summary\"\n",
        );
        tcl.push_str("unload_report\n\n");

        // Export reports
        tcl.push_str("# Export reports\n");
        tcl.push_str("qsta_utility::generate_all_reports\n");

        tcl.push_str("project_close\n");

        tcl
    }

    fn get_device_info(&self) -> (&str, &str) {
        match self.device_family {
            IntelFamily::Stratix10 => ("Stratix 10", "1SG280HU2F50E2VG"),
            IntelFamily::Arria10 => ("Arria 10", "10AX115U4F45I3SG"),
            IntelFamily::Cyclone10GX => ("Cyclone 10 GX", "10CX220YF672I5G"),
            IntelFamily::CycloneV => ("Cyclone V", "5CGXFC7D7F31C8"),
            IntelFamily::Max10 => ("MAX 10", "10M50DAF484C7G"),
            IntelFamily::Agilex => ("Agilex", "AGFB014R24B2E2V"),
            IntelFamily::EasicN5X => ("eASIC N5X", "EN5X1EF780I7"),
        }
    }

    async fn run_quartus(
        &self,
        project_dir: &Path,
        tcl_file: &Path,
    ) -> Result<String, BackendError> {
        // Run Quartus compilation
        let output = Command::new(&self.quartus_path)
            .arg("-t")
            .arg(tcl_file)
            .current_dir(project_dir)
            .output()
            .map_err(|e| BackendError::ToolNotFound(format!("Quartus: {}", e)))?;

        if !output.status.success() {
            return Err(BackendError::ToolFailed(
                String::from_utf8_lossy(&output.stderr).to_string(),
            ));
        }

        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }

    fn parse_results(
        &self,
        project_dir: &Path,
        project_name: &str,
    ) -> Result<SynthesisResults, BackendError> {
        let mut results = SynthesisResults::default();

        // Parse fit report for utilization
        let fit_report = project_dir
            .join("output_files")
            .join(format!("{}.fit.summary", project_name));

        if fit_report.exists() {
            let content = fs::read_to_string(&fit_report).map_err(BackendError::IoError)?;

            // Extract resource usage
            if let Some(alm_line) = content.lines().find(|l| l.contains("Logic utilization")) {
                if let Some(usage) = alm_line.split('/').next() {
                    if let Some(num) = usage.split_whitespace().last() {
                        results.area_metrics.luts_used =
                            Some(num.replace(",", "").parse().unwrap_or(0));
                    }
                }
            }

            if let Some(reg_line) = content
                .lines()
                .find(|l| l.contains("Dedicated logic registers"))
            {
                if let Some(usage) = reg_line.split('/').next() {
                    if let Some(num) = usage.split_whitespace().last() {
                        results.area_metrics.flip_flops_used =
                            num.replace(",", "").parse().unwrap_or(0);
                    }
                }
            }

            if let Some(bram_line) = content.lines().find(|l| l.contains("Memory blocks")) {
                if let Some(usage) = bram_line.split('/').next() {
                    if let Some(num) = usage.split_whitespace().last() {
                        results.area_metrics.block_ram_used =
                            Some(num.replace(",", "").parse().unwrap_or(0));
                    }
                }
            }

            if let Some(dsp_line) = content.lines().find(|l| l.contains("DSP blocks")) {
                if let Some(usage) = dsp_line.split('/').next() {
                    if let Some(num) = usage.split_whitespace().last() {
                        results.area_metrics.dsp_slices_used =
                            Some(num.replace(",", "").parse().unwrap_or(0));
                    }
                }
            }
        }

        // Parse timing report for Fmax
        let sta_report = project_dir
            .join("output_files")
            .join(format!("{}.sta.summary", project_name));

        if sta_report.exists() {
            let content = fs::read_to_string(&sta_report).map_err(BackendError::IoError)?;

            // Extract Fmax
            if let Some(fmax_line) = content.lines().find(|l| l.contains("Fmax")) {
                if let Some(freq) = fmax_line
                    .split_whitespace()
                    .find(|s| s.contains("MHz"))
                    .and_then(|s| s.replace("MHz", "").parse::<f64>().ok())
                {
                    results.timing_results.max_frequency_mhz = freq;
                }
            }
        }

        // Parse power report
        let power_report = project_dir
            .join("output_files")
            .join(format!("{}.pow.summary", project_name));

        if power_report.exists() {
            let content = fs::read_to_string(&power_report).map_err(BackendError::IoError)?;

            // Extract total power
            if let Some(power_line) = content
                .lines()
                .find(|l| l.contains("Total Thermal Power Dissipation"))
            {
                if let Some(power) = power_line
                    .split_whitespace()
                    .find(|s| s.contains("mW"))
                    .and_then(|s| s.replace("mW", "").parse::<f64>().ok())
                {
                    results.power_results.total_power_mw = power;
                }
            }
        }

        // Set output files
        let sof_file = project_dir
            .join("output_files")
            .join(format!("{}.sof", project_name));

        if sof_file.exists() {
            results.output_files.push(crate::OutputFile {
                file_type: crate::OutputFileType::Bitstream,
                path: sof_file.to_string_lossy().to_string(),
                description: "FPGA bitstream".to_string(),
            });
        }

        // Backend and device info are already in target field

        Ok(results)
    }
}

#[async_trait]
impl Backend for IntelBackend {
    async fn synthesize(
        &self,
        lir: &Lir,
        _config: &SynthesisConfig,
    ) -> BackendResult<SynthesisResults> {
        // Create temp directory
        let temp_dir = TempDir::new().map_err(BackendError::IoError)?;
        let work_dir = temp_dir.path();

        // Create project directory structure
        let project_name = &lir.name;
        fs::create_dir_all(work_dir.join("output_files")).map_err(BackendError::IoError)?;

        // Generate Verilog file for the LIR
        let verilog = crate::verilog::generate_verilog(lir)?;
        let file_path = work_dir.join(format!("{}.v", lir.name));
        fs::write(&file_path, verilog).map_err(BackendError::IoError)?;

        // Generate QSF file
        let qsf_content = self.generate_qsf(lir, work_dir)?;
        let qsf_file = work_dir.join(format!("{}.qsf", project_name));
        fs::write(&qsf_file, qsf_content).map_err(BackendError::IoError)?;

        // Generate QPF file (project file)
        let qpf_content = format!(
            "QUARTUS_VERSION = \"21.3\"\n\
             PROJECT_REVISION = \"{}\"\n",
            project_name
        );
        let qpf_file = work_dir.join(format!("{}.qpf", project_name));
        fs::write(&qpf_file, qpf_content).map_err(BackendError::IoError)?;

        // Generate TCL script
        let tcl_content = self.generate_tcl(project_name);
        let tcl_file = work_dir.join("compile.tcl");
        fs::write(&tcl_file, tcl_content).map_err(BackendError::IoError)?;

        // Run Quartus
        let output = self.run_quartus(work_dir, &tcl_file).await?;
        log::info!("Quartus output: {}", output);

        // Parse results
        self.parse_results(work_dir, project_name)
    }

    fn name(&self) -> &str {
        "intel_quartus"
    }

    fn supported_devices(&self) -> Vec<String> {
        vec![
            "stratix10".to_string(),
            "arria10".to_string(),
            "cyclone10gx".to_string(),
            "cyclonev".to_string(),
            "max10".to_string(),
            "agilex".to_string(),
            "easic_n5x".to_string(),
        ]
    }

    fn validate_design(&self, _lir: &Lir) -> BackendResult<()> {
        // Check if Quartus is available
        if !self.quartus_path.exists() && self.quartus_path != Path::new("quartus_sh") {
            return Err(BackendError::ToolNotFound(
                "Quartus not found. Please install Intel Quartus Prime.".to_string(),
            ));
        }

        Ok(())
    }

    fn supported_targets(&self) -> Vec<crate::TargetPlatform> {
        vec![crate::TargetPlatform::Fpga(crate::FpgaTarget::CycloneV {
            part: self.get_device_info().1.to_string(),
            package: "F896C6".to_string(),
        })]
    }

    fn validate_config(&self, _config: &SynthesisConfig) -> BackendResult<()> {
        Ok(())
    }

    fn tool_version(&self) -> BackendResult<String> {
        Ok("Quartus Prime 23.1".to_string())
    }
}

/// SDC (Synopsys Design Constraints) generator for Intel
pub struct SdcGenerator {
    constraints: Vec<SdcConstraint>,
}

#[derive(Debug, Clone)]
pub enum SdcConstraint {
    /// Clock constraint
    CreateClock {
        name: String,
        period_ns: f64,
        port: String,
    },
    /// Generated clock
    CreateGeneratedClock {
        name: String,
        source: String,
        divide_by: u32,
    },
    /// Input delay
    SetInputDelay {
        clock: String,
        min: f64,
        max: f64,
        ports: Vec<String>,
    },
    /// Output delay
    SetOutputDelay {
        clock: String,
        min: f64,
        max: f64,
        ports: Vec<String>,
    },
    /// False path
    SetFalsePath {
        from: Option<String>,
        to: Option<String>,
    },
    /// Multicycle path
    SetMulticyclePath {
        from: String,
        to: String,
        setup: u32,
        hold: u32,
    },
}

impl Default for SdcGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl SdcGenerator {
    pub fn new() -> Self {
        Self {
            constraints: Vec::new(),
        }
    }

    pub fn add_constraint(&mut self, constraint: SdcConstraint) {
        self.constraints.push(constraint);
    }

    pub fn generate(&self) -> String {
        let mut sdc = String::new();
        sdc.push_str("# SKALP Generated SDC Constraints\n\n");

        for constraint in &self.constraints {
            match constraint {
                SdcConstraint::CreateClock {
                    name,
                    period_ns,
                    port,
                } => {
                    sdc.push_str(&format!(
                        "create_clock -name {} -period {} [get_ports {{{}}}]\n",
                        name, period_ns, port
                    ));
                }
                SdcConstraint::CreateGeneratedClock {
                    name,
                    source,
                    divide_by,
                } => {
                    sdc.push_str(&format!(
                        "create_generated_clock -name {} -source [get_pins {{{}}}] -divide_by {}\n",
                        name, source, divide_by
                    ));
                }
                SdcConstraint::SetInputDelay {
                    clock,
                    min,
                    max,
                    ports,
                } => {
                    for port in ports {
                        sdc.push_str(&format!(
                            "set_input_delay -clock {} -min {} [get_ports {{{}}}]\n",
                            clock, min, port
                        ));
                        sdc.push_str(&format!(
                            "set_input_delay -clock {} -max {} [get_ports {{{}}}]\n",
                            clock, max, port
                        ));
                    }
                }
                SdcConstraint::SetOutputDelay {
                    clock,
                    min,
                    max,
                    ports,
                } => {
                    for port in ports {
                        sdc.push_str(&format!(
                            "set_output_delay -clock {} -min {} [get_ports {{{}}}]\n",
                            clock, min, port
                        ));
                        sdc.push_str(&format!(
                            "set_output_delay -clock {} -max {} [get_ports {{{}}}]\n",
                            clock, max, port
                        ));
                    }
                }
                SdcConstraint::SetFalsePath { from, to } => {
                    let mut cmd = "set_false_path".to_string();
                    if let Some(f) = from {
                        cmd.push_str(&format!(" -from [get_clocks {{{}}}]", f));
                    }
                    if let Some(t) = to {
                        cmd.push_str(&format!(" -to [get_clocks {{{}}}]", t));
                    }
                    sdc.push_str(&format!("{}\n", cmd));
                }
                SdcConstraint::SetMulticyclePath {
                    from,
                    to,
                    setup,
                    hold,
                } => {
                    sdc.push_str(&format!(
                        "set_multicycle_path -from [get_clocks {{{}}}] -to [get_clocks {{{}}}] -setup {}\n",
                        from, to, setup
                    ));
                    sdc.push_str(&format!(
                        "set_multicycle_path -from [get_clocks {{{}}}] -to [get_clocks {{{}}}] -hold {}\n",
                        from, to, hold
                    ));
                }
            }
        }

        sdc
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_device_selection() {
        let backend = IntelBackend::new(IntelFamily::Stratix10);
        let (family, device) = backend.get_device_info();
        assert_eq!(family, "Stratix 10");
        assert_eq!(device, "1SG280HU2F50E2VG");
    }

    #[test]
    fn test_sdc_generation() {
        let mut gen = SdcGenerator::new();

        gen.add_constraint(SdcConstraint::CreateClock {
            name: "sys_clk".to_string(),
            period_ns: 10.0,
            port: "clk".to_string(),
        });

        gen.add_constraint(SdcConstraint::SetInputDelay {
            clock: "sys_clk".to_string(),
            min: 1.0,
            max: 3.0,
            ports: vec!["data_in[*]".to_string()],
        });

        let sdc = gen.generate();
        assert!(sdc.contains("create_clock -name sys_clk"));
        assert!(sdc.contains("set_input_delay -clock sys_clk"));
    }
}
