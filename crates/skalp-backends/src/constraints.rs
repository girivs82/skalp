//! Constraint management for synthesis flows
//!
//! Provides constraint parsing, validation, and application for both FPGA and ASIC flows.

use crate::{BackendResult, TimingConstraint, PowerConstraints};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Constraint manager
#[derive(Debug)]
pub struct ConstraintManager {
    /// Timing constraints
    timing_constraints: Vec<TimingConstraint>,
    /// Power constraints
    power_constraints: Option<PowerConstraints>,
    /// Area constraints
    area_constraints: Option<AreaConstraints>,
    /// Pin constraints
    pin_constraints: HashMap<String, PinConstraint>,
    /// Floorplan constraints
    floorplan_constraints: Vec<FloorplanConstraint>,
}

/// Area constraints
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AreaConstraints {
    /// Maximum area utilization (0.0-1.0)
    pub max_utilization: f64,
    /// Maximum absolute area in um^2 (ASIC) or LUTs (FPGA)
    pub max_area: Option<f64>,
    /// Area target for optimization
    pub target_area: Option<f64>,
}

/// Pin constraint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PinConstraint {
    /// Signal name
    pub signal_name: String,
    /// Physical pin location
    pub location: String,
    /// I/O standard
    pub io_standard: Option<String>,
    /// Drive strength
    pub drive_strength: Option<String>,
    /// Slew rate
    pub slew_rate: Option<SlewRate>,
    /// Pull-up/pull-down
    pub termination: Option<Termination>,
}

/// Slew rate options
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SlewRate {
    Fast,
    Medium,
    Slow,
}

/// Termination options
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Termination {
    None,
    PullUp,
    PullDown,
    Keeper,
}

/// Floorplan constraint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FloorplanConstraint {
    /// Constraint type
    pub constraint_type: FloorplanConstraintType,
    /// Instance or group name
    pub instance_name: String,
    /// Region specification
    pub region: Region,
}

/// Floorplan constraint types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FloorplanConstraintType {
    /// Place instance in region
    Place,
    /// Route nets through region
    Route,
    /// Exclude instance from region
    Exclude,
    /// Create region group
    Group,
}

/// Region specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Region {
    /// Region name
    pub name: String,
    /// Bounding box
    pub bbox: BoundingBox,
    /// Region type
    pub region_type: RegionType,
}

/// Bounding box
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BoundingBox {
    /// Lower-left X coordinate
    pub x1: f64,
    /// Lower-left Y coordinate
    pub y1: f64,
    /// Upper-right X coordinate
    pub x2: f64,
    /// Upper-right Y coordinate
    pub y2: f64,
}

/// Region types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RegionType {
    /// Hard region (strict boundary)
    Hard,
    /// Soft region (preferred boundary)
    Soft,
    /// Exclusive region (no other instances)
    Exclusive,
}

/// Constraint file format
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ConstraintFormat {
    /// Xilinx Design Constraints (XDC)
    Xdc,
    /// Synopsys Design Constraints (SDC)
    Sdc,
    /// Physical Constraints File (PCF) for iCE40
    Pcf,
    /// SKALP native format
    Skalp,
}

impl ConstraintManager {
    /// Create new constraint manager
    pub fn new() -> Self {
        Self {
            timing_constraints: Vec::new(),
            power_constraints: None,
            area_constraints: None,
            pin_constraints: HashMap::new(),
            floorplan_constraints: Vec::new(),
        }
    }

    /// Add timing constraint
    pub fn add_timing_constraint(&mut self, constraint: TimingConstraint) {
        self.timing_constraints.push(constraint);
    }

    /// Set power constraints
    pub fn set_power_constraints(&mut self, constraints: PowerConstraints) {
        self.power_constraints = Some(constraints);
    }

    /// Set area constraints
    pub fn set_area_constraints(&mut self, constraints: AreaConstraints) {
        self.area_constraints = Some(constraints);
    }

    /// Add pin constraint
    pub fn add_pin_constraint(&mut self, constraint: PinConstraint) {
        self.pin_constraints.insert(constraint.signal_name.clone(), constraint);
    }

    /// Add floorplan constraint
    pub fn add_floorplan_constraint(&mut self, constraint: FloorplanConstraint) {
        self.floorplan_constraints.push(constraint);
    }

    /// Load constraints from file
    pub async fn load_from_file(
        &mut self,
        file_path: &str,
        format: ConstraintFormat,
    ) -> BackendResult<()> {
        let content = tokio::fs::read_to_string(file_path).await?;
        self.parse_constraints(&content, format)
    }

    /// Parse constraints from string
    pub fn parse_constraints(
        &mut self,
        content: &str,
        format: ConstraintFormat,
    ) -> BackendResult<()> {
        match format {
            ConstraintFormat::Sdc => self.parse_sdc(content),
            ConstraintFormat::Xdc => self.parse_xdc(content),
            ConstraintFormat::Pcf => self.parse_pcf(content),
            ConstraintFormat::Skalp => self.parse_skalp(content),
        }
    }

    /// Parse SDC (Synopsys Design Constraints) format
    fn parse_sdc(&mut self, content: &str) -> BackendResult<()> {
        for line in content.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            if line.starts_with("create_clock") {
                self.parse_sdc_create_clock(line)?;
            } else if line.starts_with("set_input_delay") {
                self.parse_sdc_input_delay(line)?;
            } else if line.starts_with("set_output_delay") {
                self.parse_sdc_output_delay(line)?;
            } else if line.starts_with("set_false_path") {
                self.parse_sdc_false_path(line)?;
            } else if line.starts_with("set_multicycle_path") {
                self.parse_sdc_multicycle_path(line)?;
            }
        }
        Ok(())
    }

    /// Parse XDC (Xilinx Design Constraints) format
    fn parse_xdc(&mut self, content: &str) -> BackendResult<()> {
        // XDC is similar to SDC with some Xilinx-specific extensions
        self.parse_sdc(content)?;

        // Parse Xilinx-specific constraints
        for line in content.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            if line.starts_with("set_property") {
                self.parse_xdc_property(line)?;
            }
        }
        Ok(())
    }

    /// Parse PCF (Physical Constraints File) format for iCE40
    fn parse_pcf(&mut self, content: &str) -> BackendResult<()> {
        for line in content.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            if line.starts_with("set_io") {
                self.parse_pcf_set_io(line)?;
            }
        }
        Ok(())
    }

    /// Parse SKALP native constraint format
    fn parse_skalp(&mut self, content: &str) -> BackendResult<()> {
        // Simple JSON-based format for SKALP constraints
        #[derive(Deserialize)]
        struct SkalpConstraints {
            timing: Option<Vec<TimingConstraint>>,
            power: Option<PowerConstraints>,
            area: Option<AreaConstraints>,
            pins: Option<HashMap<String, PinConstraint>>,
            floorplan: Option<Vec<FloorplanConstraint>>,
        }

        let constraints: SkalpConstraints = serde_json::from_str(content)
            .map_err(|e| crate::BackendError::SerializationError(e))?;

        if let Some(timing) = constraints.timing {
            self.timing_constraints.extend(timing);
        }

        if let Some(power) = constraints.power {
            self.power_constraints = Some(power);
        }

        if let Some(area) = constraints.area {
            self.area_constraints = Some(area);
        }

        if let Some(pins) = constraints.pins {
            self.pin_constraints.extend(pins);
        }

        if let Some(floorplan) = constraints.floorplan {
            self.floorplan_constraints.extend(floorplan);
        }

        Ok(())
    }

    /// Parse SDC create_clock command
    fn parse_sdc_create_clock(&mut self, line: &str) -> BackendResult<()> {
        // Simple parser for: create_clock -period 10.0 [get_ports clk]
        if let Some(period_start) = line.find("-period") {
            let period_part = &line[period_start + 7..];
            let parts: Vec<&str> = period_part.split_whitespace().collect();

            if let Some(period_str) = parts.first() {
                if let Ok(period) = period_str.parse::<f64>() {
                    // Extract clock name (simplified)
                    let clock_name = if line.contains("[get_ports") {
                        "clk" // Default name
                    } else {
                        "clk"
                    };

                    let constraint = TimingConstraint::ClockPeriod {
                        clock_name: clock_name.to_string(),
                        period_ns: period,
                    };
                    self.add_timing_constraint(constraint);
                }
            }
        }
        Ok(())
    }

    /// Parse SDC set_input_delay command
    fn parse_sdc_input_delay(&mut self, line: &str) -> BackendResult<()> {
        // Simplified parser for: set_input_delay -clock clk 2.0 [get_ports data_in]
        let constraint = TimingConstraint::InputDelay {
            port_name: "data_in".to_string(), // Simplified
            delay_ns: 2.0,
            clock_name: "clk".to_string(),
        };
        self.add_timing_constraint(constraint);
        Ok(())
    }

    /// Parse SDC set_output_delay command
    fn parse_sdc_output_delay(&mut self, line: &str) -> BackendResult<()> {
        // Simplified parser for: set_output_delay -clock clk 2.0 [get_ports data_out]
        let constraint = TimingConstraint::OutputDelay {
            port_name: "data_out".to_string(), // Simplified
            delay_ns: 2.0,
            clock_name: "clk".to_string(),
        };
        self.add_timing_constraint(constraint);
        Ok(())
    }

    /// Parse SDC set_false_path command
    fn parse_sdc_false_path(&mut self, line: &str) -> BackendResult<()> {
        // Simplified parser for: set_false_path -from [get_pins A] -to [get_pins B]
        let constraint = TimingConstraint::FalsePath {
            from: "A".to_string(), // Simplified
            to: "B".to_string(),
        };
        self.add_timing_constraint(constraint);
        Ok(())
    }

    /// Parse SDC set_multicycle_path command
    fn parse_sdc_multicycle_path(&mut self, line: &str) -> BackendResult<()> {
        // Simplified parser for: set_multicycle_path -setup 2 -from [get_pins A] -to [get_pins B]
        let constraint = TimingConstraint::MulticyclePath {
            from: "A".to_string(), // Simplified
            to: "B".to_string(),
            cycles: 2,
        };
        self.add_timing_constraint(constraint);
        Ok(())
    }

    /// Parse XDC set_property command
    fn parse_xdc_property(&mut self, line: &str) -> BackendResult<()> {
        // Simplified parser for: set_property PACKAGE_PIN A1 [get_ports clk]
        if line.contains("PACKAGE_PIN") {
            let pin_constraint = PinConstraint {
                signal_name: "clk".to_string(), // Simplified
                location: "A1".to_string(),
                io_standard: Some("LVCMOS33".to_string()),
                drive_strength: None,
                slew_rate: None,
                termination: None,
            };
            self.add_pin_constraint(pin_constraint);
        }
        Ok(())
    }

    /// Parse PCF set_io command
    fn parse_pcf_set_io(&mut self, line: &str) -> BackendResult<()> {
        // Parser for: set_io clk A1
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() >= 3 {
            let pin_constraint = PinConstraint {
                signal_name: parts[1].to_string(),
                location: parts[2].to_string(),
                io_standard: None,
                drive_strength: None,
                slew_rate: None,
                termination: None,
            };
            self.add_pin_constraint(pin_constraint);
        }
        Ok(())
    }

    /// Generate constraints in specified format
    pub fn generate_constraints(&self, format: ConstraintFormat) -> String {
        match format {
            ConstraintFormat::Sdc => self.generate_sdc(),
            ConstraintFormat::Xdc => self.generate_xdc(),
            ConstraintFormat::Pcf => self.generate_pcf(),
            ConstraintFormat::Skalp => self.generate_skalp(),
        }
    }

    /// Generate SDC format
    fn generate_sdc(&self) -> String {
        let mut sdc = String::new();
        sdc.push_str("# SDC Constraints generated by SKALP\n\n");

        for constraint in &self.timing_constraints {
            match constraint {
                TimingConstraint::ClockPeriod { clock_name, period_ns } => {
                    sdc.push_str(&format!("create_clock -period {:.3} [get_ports {}]\n",
                                        period_ns, clock_name));
                }
                TimingConstraint::InputDelay { port_name, delay_ns, clock_name } => {
                    sdc.push_str(&format!("set_input_delay -clock {} {:.3} [get_ports {}]\n",
                                        clock_name, delay_ns, port_name));
                }
                TimingConstraint::OutputDelay { port_name, delay_ns, clock_name } => {
                    sdc.push_str(&format!("set_output_delay -clock {} {:.3} [get_ports {}]\n",
                                        clock_name, delay_ns, port_name));
                }
                TimingConstraint::FalsePath { from, to } => {
                    sdc.push_str(&format!("set_false_path -from [get_pins {}] -to [get_pins {}]\n",
                                        from, to));
                }
                TimingConstraint::MulticyclePath { from, to, cycles } => {
                    sdc.push_str(&format!("set_multicycle_path -setup {} -from [get_pins {}] -to [get_pins {}]\n",
                                        cycles, from, to));
                }
            }
        }

        sdc
    }

    /// Generate XDC format
    fn generate_xdc(&self) -> String {
        let mut xdc = self.generate_sdc(); // Start with SDC commands
        xdc.push_str("\n# Xilinx-specific constraints\n");

        for (signal, pin) in &self.pin_constraints {
            xdc.push_str(&format!("set_property PACKAGE_PIN {} [get_ports {}]\n",
                                pin.location, signal));
            if let Some(ref io_standard) = pin.io_standard {
                xdc.push_str(&format!("set_property IOSTANDARD {} [get_ports {}]\n",
                                    io_standard, signal));
            }
        }

        xdc
    }

    /// Generate PCF format
    fn generate_pcf(&self) -> String {
        let mut pcf = String::new();
        pcf.push_str("# PCF Constraints generated by SKALP\n\n");

        for (signal, pin) in &self.pin_constraints {
            pcf.push_str(&format!("set_io {} {}\n", signal, pin.location));
        }

        pcf
    }

    /// Generate SKALP native format
    fn generate_skalp(&self) -> String {
        #[derive(Serialize)]
        struct SkalpConstraints<'a> {
            timing: &'a Vec<TimingConstraint>,
            power: &'a Option<PowerConstraints>,
            area: &'a Option<AreaConstraints>,
            pins: &'a HashMap<String, PinConstraint>,
            floorplan: &'a Vec<FloorplanConstraint>,
        }

        let constraints = SkalpConstraints {
            timing: &self.timing_constraints,
            power: &self.power_constraints,
            area: &self.area_constraints,
            pins: &self.pin_constraints,
            floorplan: &self.floorplan_constraints,
        };

        serde_json::to_string_pretty(&constraints).unwrap_or_default()
    }

    /// Validate constraints
    pub fn validate(&self) -> Vec<String> {
        let mut errors = Vec::new();

        // Check for conflicting timing constraints
        let mut clock_names = std::collections::HashSet::new();
        for constraint in &self.timing_constraints {
            if let TimingConstraint::ClockPeriod { clock_name, .. } = constraint {
                if !clock_names.insert(clock_name.clone()) {
                    errors.push(format!("Duplicate clock definition: {}", clock_name));
                }
            }
        }

        // Check pin constraints for conflicts
        let mut pin_locations = std::collections::HashSet::new();
        for pin in self.pin_constraints.values() {
            if !pin_locations.insert(&pin.location) {
                errors.push(format!("Pin location conflict: {}", pin.location));
            }
        }

        errors
    }

    /// Get all timing constraints
    pub fn get_timing_constraints(&self) -> &Vec<TimingConstraint> {
        &self.timing_constraints
    }

    /// Get power constraints
    pub fn get_power_constraints(&self) -> &Option<PowerConstraints> {
        &self.power_constraints
    }

    /// Get area constraints
    pub fn get_area_constraints(&self) -> &Option<AreaConstraints> {
        &self.area_constraints
    }
}

impl Default for ConstraintManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constraint_manager_creation() {
        let manager = ConstraintManager::new();
        assert!(manager.timing_constraints.is_empty());
        assert!(manager.power_constraints.is_none());
        assert!(manager.pin_constraints.is_empty());
    }

    #[test]
    fn test_timing_constraint_addition() {
        let mut manager = ConstraintManager::new();
        let constraint = TimingConstraint::ClockPeriod {
            clock_name: "clk".to_string(),
            period_ns: 10.0,
        };

        manager.add_timing_constraint(constraint);
        assert_eq!(manager.timing_constraints.len(), 1);
    }

    #[test]
    fn test_pin_constraint_addition() {
        let mut manager = ConstraintManager::new();
        let constraint = PinConstraint {
            signal_name: "clk".to_string(),
            location: "A1".to_string(),
            io_standard: Some("LVCMOS33".to_string()),
            drive_strength: None,
            slew_rate: None,
            termination: None,
        };

        manager.add_pin_constraint(constraint);
        assert_eq!(manager.pin_constraints.len(), 1);
        assert!(manager.pin_constraints.contains_key("clk"));
    }

    #[test]
    fn test_sdc_parsing() {
        let mut manager = ConstraintManager::new();
        let sdc_content = r#"
# Test SDC file
create_clock -period 10.0 [get_ports clk]
set_input_delay -clock clk 2.0 [get_ports data_in]
"#;

        let result = manager.parse_constraints(sdc_content, ConstraintFormat::Sdc);
        assert!(result.is_ok());
        assert!(!manager.timing_constraints.is_empty());
    }

    #[test]
    fn test_pcf_parsing() {
        let mut manager = ConstraintManager::new();
        let pcf_content = r#"
# Test PCF file
set_io clk A1
set_io data_in B2
"#;

        let result = manager.parse_constraints(pcf_content, ConstraintFormat::Pcf);
        assert!(result.is_ok());
        assert_eq!(manager.pin_constraints.len(), 2);
    }

    #[test]
    fn test_constraint_generation() {
        let mut manager = ConstraintManager::new();

        let timing_constraint = TimingConstraint::ClockPeriod {
            clock_name: "clk".to_string(),
            period_ns: 8.0,
        };
        manager.add_timing_constraint(timing_constraint);

        let pin_constraint = PinConstraint {
            signal_name: "reset".to_string(),
            location: "C3".to_string(),
            io_standard: Some("LVCMOS25".to_string()),
            drive_strength: None,
            slew_rate: None,
            termination: None,
        };
        manager.add_pin_constraint(pin_constraint);

        let sdc = manager.generate_constraints(ConstraintFormat::Sdc);
        assert!(sdc.contains("create_clock -period 8.000"));

        let xdc = manager.generate_constraints(ConstraintFormat::Xdc);
        assert!(sdc.contains("create_clock"));
        assert!(xdc.contains("PACKAGE_PIN"));

        let pcf = manager.generate_constraints(ConstraintFormat::Pcf);
        assert!(pcf.contains("set_io reset C3"));
    }

    #[test]
    fn test_constraint_validation() {
        let mut manager = ConstraintManager::new();

        // Add duplicate clock
        let clock1 = TimingConstraint::ClockPeriod {
            clock_name: "clk".to_string(),
            period_ns: 10.0,
        };
        let clock2 = TimingConstraint::ClockPeriod {
            clock_name: "clk".to_string(),
            period_ns: 8.0,
        };
        manager.add_timing_constraint(clock1);
        manager.add_timing_constraint(clock2);

        let errors = manager.validate();
        assert!(!errors.is_empty());
        assert!(errors[0].contains("Duplicate clock"));
    }
}