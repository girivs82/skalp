//! I/O Pin Constraints
//!
//! Allows users to specify pin locations for I/O cells, which is essential
//! for real hardware where specific pins need to connect to board traces.

use crate::device::Device;
use crate::error::{PlaceRouteError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// I/O pin constraint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PinConstraint {
    /// Signal/port name in the design
    pub signal_name: String,
    /// Pin name on the package (e.g., "A1", "B2", "IOT_224")
    pub pin_name: String,
    /// Optional I/O standard (e.g., "SB_LVCMOS")
    pub io_standard: Option<String>,
    /// Optional drive strength (mA)
    pub drive_strength: Option<u8>,
    /// Optional pull-up/pull-down
    pub pull: Option<PullType>,
}

/// Pull-up/pull-down type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum PullType {
    /// No pull
    #[default]
    None,
    /// Pull-up resistor
    PullUp,
    /// Pull-down resistor
    PullDown,
    /// Keeper (hold last value)
    Keeper,
}

impl PinConstraint {
    /// Create a simple pin constraint (signal to pin mapping)
    pub fn new(signal_name: impl Into<String>, pin_name: impl Into<String>) -> Self {
        Self {
            signal_name: signal_name.into(),
            pin_name: pin_name.into(),
            io_standard: None,
            drive_strength: None,
            pull: None,
        }
    }

    /// Set I/O standard
    pub fn with_io_standard(mut self, standard: impl Into<String>) -> Self {
        self.io_standard = Some(standard.into());
        self
    }

    /// Set drive strength
    pub fn with_drive_strength(mut self, strength: u8) -> Self {
        self.drive_strength = Some(strength);
        self
    }

    /// Set pull type
    pub fn with_pull(mut self, pull: PullType) -> Self {
        self.pull = Some(pull);
        self
    }
}

/// Collection of I/O constraints for a design
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct IoConstraints {
    /// Package name (e.g., "ct256", "tq144", "sg48")
    pub package: Option<String>,
    /// Pin constraints: signal_name -> constraint
    constraints: HashMap<String, PinConstraint>,
}

impl IoConstraints {
    /// Create empty constraints
    pub fn new() -> Self {
        Self::default()
    }

    /// Create constraints for a specific package
    pub fn for_package(package: impl Into<String>) -> Self {
        Self {
            package: Some(package.into()),
            constraints: HashMap::new(),
        }
    }

    /// Add a pin constraint
    pub fn add(&mut self, constraint: PinConstraint) {
        self.constraints
            .insert(constraint.signal_name.clone(), constraint);
    }

    /// Add a simple signal-to-pin mapping
    pub fn set_pin(&mut self, signal: impl Into<String>, pin: impl Into<String>) {
        let constraint = PinConstraint::new(signal, pin);
        self.constraints
            .insert(constraint.signal_name.clone(), constraint);
    }

    /// Get constraint for a signal
    pub fn get(&self, signal_name: &str) -> Option<&PinConstraint> {
        self.constraints.get(signal_name)
    }

    /// Check if a signal has a constraint
    pub fn has_constraint(&self, signal_name: &str) -> bool {
        self.constraints.contains_key(signal_name)
    }

    /// Get all constraints
    pub fn all(&self) -> impl Iterator<Item = &PinConstraint> {
        self.constraints.values()
    }

    /// Number of constraints
    pub fn len(&self) -> usize {
        self.constraints.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.constraints.is_empty()
    }

    /// Validate constraints against a device's package
    pub fn validate<D: Device>(&self, device: &D) -> Result<()> {
        let packages = device.packages();

        // If no package specified, try to find one that contains all pins
        let package = if let Some(ref pkg_name) = self.package {
            packages.get(pkg_name).ok_or_else(|| {
                PlaceRouteError::InvalidConstraint(format!(
                    "Package '{}' not found. Available: {:?}",
                    pkg_name,
                    packages.keys().collect::<Vec<_>>()
                ))
            })?
        } else if packages.len() == 1 {
            // Only one package, use it
            packages.values().next().unwrap()
        } else {
            // No package specified and multiple available
            return Err(PlaceRouteError::InvalidConstraint(
                "No package specified and multiple packages available. Please specify a package."
                    .to_string(),
            ));
        };

        // Validate each constraint
        for constraint in self.constraints.values() {
            if !package.pins.contains_key(&constraint.pin_name) {
                return Err(PlaceRouteError::InvalidConstraint(format!(
                    "Pin '{}' not found in package '{}' for signal '{}'",
                    constraint.pin_name, package.name, constraint.signal_name
                )));
            }
        }

        // Check for duplicate pin assignments
        let mut used_pins: HashMap<&str, &str> = HashMap::new();
        for constraint in self.constraints.values() {
            if let Some(&existing_signal) = used_pins.get(constraint.pin_name.as_str()) {
                return Err(PlaceRouteError::InvalidConstraint(format!(
                    "Pin '{}' is assigned to both '{}' and '{}'",
                    constraint.pin_name, existing_signal, constraint.signal_name
                )));
            }
            used_pins.insert(&constraint.pin_name, &constraint.signal_name);
        }

        Ok(())
    }

    /// Resolve constraints to tile locations
    /// Returns: signal_name -> (tile_x, tile_y, bel_idx)
    pub fn resolve<D: Device>(&self, device: &D) -> Result<HashMap<String, (u32, u32, usize)>> {
        let packages = device.packages();

        let package = if let Some(ref pkg_name) = self.package {
            packages.get(pkg_name).ok_or_else(|| {
                PlaceRouteError::InvalidConstraint(format!("Package '{}' not found", pkg_name))
            })?
        } else if packages.len() == 1 {
            packages.values().next().unwrap()
        } else {
            return Err(PlaceRouteError::InvalidConstraint(
                "No package specified".to_string(),
            ));
        };

        let mut result = HashMap::new();

        for constraint in self.constraints.values() {
            if let Some(&(tile_x, tile_y, bel_idx)) = package.pins.get(&constraint.pin_name) {
                result.insert(
                    constraint.signal_name.clone(),
                    (tile_x, tile_y, bel_idx as usize),
                );
            }
        }

        Ok(result)
    }

    /// Parse constraints from PCF (Physical Constraints File) format
    /// Format: set_io signal_name pin_name
    pub fn from_pcf(content: &str) -> Result<Self> {
        let mut constraints = IoConstraints::new();

        for (line_num, line) in content.lines().enumerate() {
            let line = line.trim();

            // Skip empty lines and comments
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            // Parse set_io command
            if line.starts_with("set_io") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 3 {
                    let signal = parts[1];
                    let pin = parts[2];
                    constraints.set_pin(signal, pin);
                } else {
                    return Err(PlaceRouteError::InvalidConstraint(format!(
                        "Line {}: Invalid set_io format: {}",
                        line_num + 1,
                        line
                    )));
                }
            }
        }

        Ok(constraints)
    }

    /// Generate PCF content from constraints
    pub fn to_pcf(&self) -> String {
        let mut output = String::new();
        output.push_str("# Pin constraints generated by SKALP\n\n");

        for constraint in self.constraints.values() {
            output.push_str(&format!(
                "set_io {} {}\n",
                constraint.signal_name, constraint.pin_name
            ));
        }

        output
    }
}

/// Resolved I/O location from constraints
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct ResolvedIoLocation {
    /// Signal name
    pub signal_name: String,
    /// Tile X coordinate
    pub tile_x: u32,
    /// Tile Y coordinate
    pub tile_y: u32,
    /// BEL index within tile
    pub bel_index: usize,
    /// Original constraint
    pub constraint: PinConstraint,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pin_constraint_creation() {
        let constraint = PinConstraint::new("clk", "A1")
            .with_io_standard("LVCMOS33")
            .with_drive_strength(8);

        assert_eq!(constraint.signal_name, "clk");
        assert_eq!(constraint.pin_name, "A1");
        assert_eq!(constraint.io_standard, Some("LVCMOS33".to_string()));
        assert_eq!(constraint.drive_strength, Some(8));
    }

    #[test]
    fn test_io_constraints() {
        let mut constraints = IoConstraints::for_package("ct256");

        constraints.set_pin("clk", "J3");
        constraints.set_pin("led", "B5");
        constraints.set_pin("btn", "A6");

        assert_eq!(constraints.len(), 3);
        assert!(constraints.has_constraint("clk"));
        assert!(!constraints.has_constraint("unknown"));

        let clk = constraints.get("clk").unwrap();
        assert_eq!(clk.pin_name, "J3");
    }

    #[test]
    fn test_pcf_parsing() {
        let pcf = r#"
# Pin assignments for LED blinker
set_io clk J3
set_io led[0] B5
set_io led[1] B4
set_io btn A6
"#;

        let constraints = IoConstraints::from_pcf(pcf).unwrap();
        assert_eq!(constraints.len(), 4);
        assert_eq!(constraints.get("clk").unwrap().pin_name, "J3");
        assert_eq!(constraints.get("led[0]").unwrap().pin_name, "B5");
    }

    #[test]
    fn test_pcf_generation() {
        let mut constraints = IoConstraints::new();
        constraints.set_pin("clk", "J3");
        constraints.set_pin("led", "B5");

        let pcf = constraints.to_pcf();
        assert!(pcf.contains("set_io clk J3") || pcf.contains("set_io led B5"));
    }
}
