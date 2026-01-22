//! Design Constraints
//!
//! Comprehensive constraint system for FPGA designs, compatible with nextpnr:
//! - I/O pin constraints (PCF format)
//! - Frequency/timing constraints
//! - BEL placement constraints
//! - Region-based floorplanning
//! - Pull-up resistor configuration

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

/// Pull-up resistor strength (iCE40 UltraPlus only)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum PullResistor {
    /// 3.3k ohm
    R3P3K,
    /// 6.8k ohm
    R6P8K,
    /// 10k ohm (default)
    #[default]
    R10K,
    /// 100k ohm
    R100K,
}

/// Frequency constraint for timing analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FrequencyConstraint {
    /// Net/port name (clock signal)
    pub net_name: String,
    /// Target frequency in MHz
    pub frequency_mhz: f64,
}

impl FrequencyConstraint {
    /// Create a new frequency constraint
    pub fn new(net_name: impl Into<String>, frequency_mhz: f64) -> Self {
        Self {
            net_name: net_name.into(),
            frequency_mhz,
        }
    }

    /// Get period in nanoseconds
    pub fn period_ns(&self) -> f64 {
        1000.0 / self.frequency_mhz
    }
}

/// BEL placement constraint (direct BEL specification)
/// Compatible with nextpnr's (* BEL="X2/Y5/lc0" *) attribute
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BelConstraint {
    /// Cell name in the design
    pub cell_name: String,
    /// Target BEL location "X{x}/Y{y}/{bel_name}"
    pub bel_location: String,
    /// Parsed tile X coordinate
    pub tile_x: u32,
    /// Parsed tile Y coordinate
    pub tile_y: u32,
    /// BEL name within tile (e.g., "lc0", "lc7", "io0")
    pub bel_name: String,
}

impl BelConstraint {
    /// Create a BEL constraint from a location string
    /// Format: "X{x}/Y{y}/{bel}" e.g., "X2/Y5/lc0"
    pub fn parse(cell_name: impl Into<String>, location: &str) -> Result<Self> {
        let cell_name = cell_name.into();
        let parts: Vec<&str> = location.split('/').collect();

        if parts.len() != 3 {
            return Err(PlaceRouteError::InvalidConstraint(format!(
                "Invalid BEL location format '{}' for cell '{}'. Expected 'X{{x}}/Y{{y}}/{{bel}}'",
                location, cell_name
            )));
        }

        let tile_x = parts[0]
            .strip_prefix('X')
            .and_then(|s| s.parse::<u32>().ok())
            .ok_or_else(|| {
                PlaceRouteError::InvalidConstraint(format!(
                    "Invalid X coordinate in BEL location '{}'",
                    location
                ))
            })?;

        let tile_y = parts[1]
            .strip_prefix('Y')
            .and_then(|s| s.parse::<u32>().ok())
            .ok_or_else(|| {
                PlaceRouteError::InvalidConstraint(format!(
                    "Invalid Y coordinate in BEL location '{}'",
                    location
                ))
            })?;

        let bel_name = parts[2].to_string();

        Ok(Self {
            cell_name,
            bel_location: location.to_string(),
            tile_x,
            tile_y,
            bel_name,
        })
    }

    /// Create a BEL constraint directly
    pub fn new(
        cell_name: impl Into<String>,
        tile_x: u32,
        tile_y: u32,
        bel_name: impl Into<String>,
    ) -> Self {
        let bel_name = bel_name.into();
        Self {
            cell_name: cell_name.into(),
            bel_location: format!("X{}/Y{}/{}", tile_x, tile_y, bel_name),
            tile_x,
            tile_y,
            bel_name,
        }
    }

    /// Get BEL index from name (e.g., "lc0" -> 0, "lc7" -> 7)
    pub fn bel_index(&self) -> Option<usize> {
        if let Some(suffix) = self.bel_name.strip_prefix("lc") {
            suffix.parse().ok()
        } else if let Some(suffix) = self.bel_name.strip_prefix("io") {
            suffix.parse().ok()
        } else if let Some(suffix) = self.bel_name.strip_prefix("ram") {
            suffix.parse().ok()
        } else {
            None
        }
    }
}

/// Rectangular region for floorplanning
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlacementRegion {
    /// Region name
    pub name: String,
    /// Minimum X coordinate (inclusive)
    pub x_min: u32,
    /// Minimum Y coordinate (inclusive)
    pub y_min: u32,
    /// Maximum X coordinate (inclusive)
    pub x_max: u32,
    /// Maximum Y coordinate (inclusive)
    pub y_max: u32,
}

impl PlacementRegion {
    /// Create a rectangular region
    pub fn new(name: impl Into<String>, x_min: u32, y_min: u32, x_max: u32, y_max: u32) -> Self {
        Self {
            name: name.into(),
            x_min,
            y_min,
            x_max,
            y_max,
        }
    }

    /// Check if a location is within this region
    pub fn contains(&self, x: u32, y: u32) -> bool {
        x >= self.x_min && x <= self.x_max && y >= self.y_min && y <= self.y_max
    }

    /// Get the area of this region in tiles
    pub fn area(&self) -> u32 {
        (self.x_max - self.x_min + 1) * (self.y_max - self.y_min + 1)
    }
}

/// Region constraint - assign cells to specific regions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegionConstraint {
    /// Cell name pattern (can include wildcards)
    pub cell_pattern: String,
    /// Region name to constrain to
    pub region_name: String,
}

impl RegionConstraint {
    /// Create a region constraint
    pub fn new(cell_pattern: impl Into<String>, region_name: impl Into<String>) -> Self {
        Self {
            cell_pattern: cell_pattern.into(),
            region_name: region_name.into(),
        }
    }

    /// Check if a cell name matches this constraint's pattern
    pub fn matches(&self, cell_name: &str) -> bool {
        if self.cell_pattern.contains('*') {
            // Simple wildcard matching
            let pattern = &self.cell_pattern;
            if let Some(middle) = pattern.strip_prefix('*').and_then(|s| s.strip_suffix('*')) {
                // Pattern like "*foo*" - contains
                cell_name.contains(middle)
            } else if let Some(suffix) = pattern.strip_prefix('*') {
                // Pattern like "*foo" - ends with
                cell_name.ends_with(suffix)
            } else if let Some(prefix) = pattern.strip_suffix('*') {
                // Pattern like "foo*" - starts with
                cell_name.starts_with(prefix)
            } else {
                // Pattern like "foo*bar"
                let parts: Vec<&str> = pattern.split('*').collect();
                if parts.len() == 2 {
                    cell_name.starts_with(parts[0]) && cell_name.ends_with(parts[1])
                } else {
                    cell_name == pattern
                }
            }
        } else {
            cell_name == self.cell_pattern
        }
    }
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
    /// Frequency constraints: net_name -> constraint
    frequency_constraints: HashMap<String, FrequencyConstraint>,
    /// BEL placement constraints: cell_name -> constraint
    bel_constraints: HashMap<String, BelConstraint>,
    /// Named placement regions
    regions: HashMap<String, PlacementRegion>,
    /// Region constraints for cells
    region_constraints: Vec<RegionConstraint>,
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
            frequency_constraints: HashMap::new(),
            bel_constraints: HashMap::new(),
            regions: HashMap::new(),
            region_constraints: Vec::new(),
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

    /// Number of pin constraints
    pub fn len(&self) -> usize {
        self.constraints.len()
    }

    /// Check if empty (no pin constraints)
    pub fn is_empty(&self) -> bool {
        self.constraints.is_empty()
    }

    // === Frequency Constraints ===

    /// Add a frequency constraint for a clock net
    pub fn set_frequency(&mut self, net_name: impl Into<String>, frequency_mhz: f64) {
        let constraint = FrequencyConstraint::new(net_name, frequency_mhz);
        self.frequency_constraints
            .insert(constraint.net_name.clone(), constraint);
    }

    /// Get frequency constraint for a net
    pub fn get_frequency(&self, net_name: &str) -> Option<&FrequencyConstraint> {
        self.frequency_constraints.get(net_name)
    }

    /// Get all frequency constraints
    pub fn frequency_constraints(&self) -> impl Iterator<Item = &FrequencyConstraint> {
        self.frequency_constraints.values()
    }

    // === BEL Constraints ===

    /// Add a BEL placement constraint
    pub fn set_bel(&mut self, cell_name: impl Into<String>, location: &str) -> Result<()> {
        let constraint = BelConstraint::parse(cell_name, location)?;
        self.bel_constraints
            .insert(constraint.cell_name.clone(), constraint);
        Ok(())
    }

    /// Add a BEL constraint directly
    pub fn add_bel_constraint(&mut self, constraint: BelConstraint) {
        self.bel_constraints
            .insert(constraint.cell_name.clone(), constraint);
    }

    /// Get BEL constraint for a cell
    pub fn get_bel(&self, cell_name: &str) -> Option<&BelConstraint> {
        self.bel_constraints.get(cell_name)
    }

    /// Get all BEL constraints
    pub fn bel_constraints(&self) -> impl Iterator<Item = &BelConstraint> {
        self.bel_constraints.values()
    }

    // === Region Constraints ===

    /// Create a rectangular placement region
    pub fn create_region(
        &mut self,
        name: impl Into<String>,
        x_min: u32,
        y_min: u32,
        x_max: u32,
        y_max: u32,
    ) {
        let region = PlacementRegion::new(name, x_min, y_min, x_max, y_max);
        self.regions.insert(region.name.clone(), region);
    }

    /// Constrain cells matching a pattern to a region
    pub fn constrain_to_region(
        &mut self,
        cell_pattern: impl Into<String>,
        region_name: impl Into<String>,
    ) {
        self.region_constraints
            .push(RegionConstraint::new(cell_pattern, region_name));
    }

    /// Get a region by name
    pub fn get_region(&self, name: &str) -> Option<&PlacementRegion> {
        self.regions.get(name)
    }

    /// Find the region constraint for a cell
    pub fn find_region_for_cell(&self, cell_name: &str) -> Option<&PlacementRegion> {
        for constraint in &self.region_constraints {
            if constraint.matches(cell_name) {
                return self.regions.get(&constraint.region_name);
            }
        }
        None
    }

    /// Get all regions
    pub fn regions(&self) -> impl Iterator<Item = &PlacementRegion> {
        self.regions.values()
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
    ///
    /// Supported commands:
    /// - `set_io [-nowarn] [-pullup yes|no] [-pullup_resistor 3P3K|6P8K|10K|100K] signal pin`
    /// - `set_frequency net frequency_mhz`
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
                Self::parse_set_io(&mut constraints, line, line_num)?;
            }
            // Parse set_frequency command
            else if line.starts_with("set_frequency") {
                Self::parse_set_frequency(&mut constraints, line, line_num)?;
            }
            // Unknown command - warn but continue
            else {
                // Could add warning here if needed
            }
        }

        Ok(constraints)
    }

    /// Parse a set_io command line
    fn parse_set_io(constraints: &mut IoConstraints, line: &str, line_num: usize) -> Result<()> {
        let parts: Vec<&str> = line.split_whitespace().collect();

        if parts.len() < 3 {
            return Err(PlaceRouteError::InvalidConstraint(format!(
                "Line {}: Invalid set_io format: {}",
                line_num + 1,
                line
            )));
        }

        // Parse options and find signal/pin
        let mut signal: Option<&str> = None;
        let mut pin: Option<&str> = None;
        let mut pull = None;
        let mut _pull_resistor = PullResistor::R10K;
        let mut i = 1;

        while i < parts.len() {
            match parts[i] {
                "-nowarn" => {
                    // Ignore -nowarn flag
                    i += 1;
                }
                "-pullup" => {
                    if i + 1 < parts.len() {
                        match parts[i + 1].to_lowercase().as_str() {
                            "yes" => pull = Some(PullType::PullUp),
                            "no" => pull = Some(PullType::None),
                            _ => {}
                        }
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
                "-pullup_resistor" => {
                    if i + 1 < parts.len() {
                        _pull_resistor = match parts[i + 1].to_uppercase().as_str() {
                            "3P3K" => PullResistor::R3P3K,
                            "6P8K" => PullResistor::R6P8K,
                            "10K" => PullResistor::R10K,
                            "100K" => PullResistor::R100K,
                            _ => PullResistor::R10K,
                        };
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
                _ => {
                    // Non-option arguments are signal and pin
                    if signal.is_none() {
                        signal = Some(parts[i]);
                    } else if pin.is_none() {
                        pin = Some(parts[i]);
                    }
                    i += 1;
                }
            }
        }

        if let (Some(sig), Some(p)) = (signal, pin) {
            let mut constraint = PinConstraint::new(sig, p);
            if let Some(pull_type) = pull {
                constraint.pull = Some(pull_type);
            }
            constraints.add(constraint);
        } else {
            return Err(PlaceRouteError::InvalidConstraint(format!(
                "Line {}: Missing signal or pin in set_io: {}",
                line_num + 1,
                line
            )));
        }

        Ok(())
    }

    /// Parse a set_frequency command line
    fn parse_set_frequency(
        constraints: &mut IoConstraints,
        line: &str,
        line_num: usize,
    ) -> Result<()> {
        let parts: Vec<&str> = line.split_whitespace().collect();

        if parts.len() < 3 {
            return Err(PlaceRouteError::InvalidConstraint(format!(
                "Line {}: Invalid set_frequency format: {}",
                line_num + 1,
                line
            )));
        }

        let net = parts[1];
        let freq: f64 = parts[2].parse().map_err(|_| {
            PlaceRouteError::InvalidConstraint(format!(
                "Line {}: Invalid frequency value '{}' in: {}",
                line_num + 1,
                parts[2],
                line
            ))
        })?;

        constraints.set_frequency(net, freq);
        Ok(())
    }

    /// Generate PCF content from constraints
    pub fn to_pcf(&self) -> String {
        let mut output = String::new();
        output.push_str("# Pin constraints generated by SKALP\n\n");

        // Pin constraints
        for constraint in self.constraints.values() {
            let mut line = format!("set_io {} {}", constraint.signal_name, constraint.pin_name);

            // Add pullup option if specified
            if let Some(pull) = &constraint.pull {
                match pull {
                    PullType::PullUp => line.push_str(" -pullup yes"),
                    PullType::None => {}
                    _ => {}
                }
            }

            output.push_str(&line);
            output.push('\n');
        }

        // Frequency constraints
        if !self.frequency_constraints.is_empty() {
            output.push_str("\n# Frequency constraints\n");
            for constraint in self.frequency_constraints.values() {
                output.push_str(&format!(
                    "set_frequency {} {}\n",
                    constraint.net_name, constraint.frequency_mhz
                ));
            }
        }

        output
    }

    /// Total number of constraints (all types)
    pub fn total_constraints(&self) -> usize {
        self.constraints.len()
            + self.frequency_constraints.len()
            + self.bel_constraints.len()
            + self.region_constraints.len()
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

    #[test]
    fn test_frequency_constraints() {
        let mut constraints = IoConstraints::new();
        constraints.set_frequency("clk", 12.0);
        constraints.set_frequency("pll_out", 48.0);

        let clk_freq = constraints.get_frequency("clk").unwrap();
        assert_eq!(clk_freq.frequency_mhz, 12.0);
        assert!((clk_freq.period_ns() - 83.333).abs() < 0.01);

        let pll_freq = constraints.get_frequency("pll_out").unwrap();
        assert_eq!(pll_freq.frequency_mhz, 48.0);
    }

    #[test]
    fn test_pcf_with_frequency() {
        let pcf = r#"
# Clock constraints
set_frequency clk 12
set_frequency pll_out 48.5

# Pin assignments
set_io clk J3
set_io led B5
"#;

        let constraints = IoConstraints::from_pcf(pcf).unwrap();
        assert_eq!(constraints.len(), 2);
        assert_eq!(
            constraints.get_frequency("clk").unwrap().frequency_mhz,
            12.0
        );
        assert_eq!(
            constraints.get_frequency("pll_out").unwrap().frequency_mhz,
            48.5
        );
    }

    #[test]
    fn test_pcf_with_pullup() {
        let pcf = r#"
set_io -pullup yes btn A6
set_io -pullup no led B5
set_io -pullup_resistor 100K input C7
"#;

        let constraints = IoConstraints::from_pcf(pcf).unwrap();
        assert_eq!(constraints.get("btn").unwrap().pull, Some(PullType::PullUp));
        assert_eq!(constraints.get("led").unwrap().pull, Some(PullType::None));
    }

    #[test]
    fn test_bel_constraint_parsing() {
        let constraint = BelConstraint::parse("my_lut", "X2/Y5/lc0").unwrap();
        assert_eq!(constraint.tile_x, 2);
        assert_eq!(constraint.tile_y, 5);
        assert_eq!(constraint.bel_name, "lc0");
        assert_eq!(constraint.bel_index(), Some(0));

        let constraint2 = BelConstraint::parse("my_io", "X0/Y10/io1").unwrap();
        assert_eq!(constraint2.tile_x, 0);
        assert_eq!(constraint2.tile_y, 10);
        assert_eq!(constraint2.bel_index(), Some(1));
    }

    #[test]
    fn test_bel_constraint_invalid() {
        assert!(BelConstraint::parse("cell", "invalid").is_err());
        assert!(BelConstraint::parse("cell", "X/Y/lc0").is_err());
        assert!(BelConstraint::parse("cell", "2/5/lc0").is_err());
    }

    #[test]
    fn test_placement_region() {
        let region = PlacementRegion::new("core", 1, 1, 10, 10);
        assert!(region.contains(5, 5));
        assert!(region.contains(1, 1));
        assert!(region.contains(10, 10));
        assert!(!region.contains(0, 5));
        assert!(!region.contains(11, 5));
        assert_eq!(region.area(), 100);
    }

    #[test]
    fn test_region_constraint_matching() {
        // Exact match
        let constraint = RegionConstraint::new("my_cell", "region1");
        assert!(constraint.matches("my_cell"));
        assert!(!constraint.matches("other_cell"));

        // Prefix wildcard
        let constraint = RegionConstraint::new("*_ff", "region2");
        assert!(constraint.matches("counter_ff"));
        assert!(constraint.matches("data_ff"));
        assert!(!constraint.matches("ff_counter"));

        // Suffix wildcard
        let constraint = RegionConstraint::new("alu_*", "region3");
        assert!(constraint.matches("alu_add"));
        assert!(constraint.matches("alu_sub"));
        assert!(!constraint.matches("cpu_alu"));

        // Contains wildcard
        let constraint = RegionConstraint::new("*ring*", "osc_region");
        assert!(constraint.matches("ringosc"));
        assert!(constraint.matches("my_ring_buffer"));
        assert!(!constraint.matches("other"));
    }

    #[test]
    fn test_floorplanning() {
        let mut constraints = IoConstraints::new();

        // Create regions
        constraints.create_region("left_half", 0, 0, 5, 10);
        constraints.create_region("right_half", 6, 0, 12, 10);

        // Constrain cells
        constraints.constrain_to_region("alu_*", "left_half");
        constraints.constrain_to_region("*_mem", "right_half");

        // Check region lookup
        let region = constraints.find_region_for_cell("alu_add").unwrap();
        assert_eq!(region.name, "left_half");

        let region = constraints.find_region_for_cell("data_mem").unwrap();
        assert_eq!(region.name, "right_half");

        assert!(constraints.find_region_for_cell("other_cell").is_none());
    }
}
