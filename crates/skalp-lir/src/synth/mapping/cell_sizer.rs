//! Cell Sizing Pass
//!
//! This module implements cell sizing for drive strength selection.
//! It analyzes fanout and timing requirements to select appropriate
//! cell variants (X1, X2, X4, etc.).
//!
//! # Algorithm
//!
//! 1. Compute fanout for each cell output
//! 2. Estimate load capacitance based on fanout
//! 3. Select minimum drive strength that meets timing
//! 4. Optionally upsize cells on critical paths
//!
//! # Drive Strength Selection
//!
//! | Fanout | Recommended Drive |
//! |--------|-------------------|
//! | 1-2    | X1                |
//! | 3-4    | X2                |
//! | 5-8    | X4                |
//! | 9+     | X8 or buffer tree |

use crate::gate_netlist::{Cell, CellId, GateNetId, GateNetlist};
use std::collections::HashMap;

/// Cell sizing configuration
#[derive(Debug, Clone)]
pub struct CellSizingConfig {
    /// Maximum fanout before requiring larger drive
    pub fanout_thresholds: Vec<(usize, DriveStrength)>,
    /// Enable timing-driven sizing
    pub timing_driven: bool,
    /// Target slack (ps) - upsize if slack is less than this
    pub target_slack: f64,
    /// Maximum drive strength to use
    pub max_drive: DriveStrength,
    /// Preserve original sizing for cells matching these patterns
    pub preserve_patterns: Vec<String>,
}

impl Default for CellSizingConfig {
    fn default() -> Self {
        Self {
            fanout_thresholds: vec![
                (2, DriveStrength::X1),
                (4, DriveStrength::X2),
                (8, DriveStrength::X4),
                (16, DriveStrength::X8),
            ],
            timing_driven: false,
            target_slack: 0.0,
            max_drive: DriveStrength::X8,
            preserve_patterns: Vec::new(),
        }
    }
}

impl CellSizingConfig {
    /// Create config optimized for area
    pub fn area_focused() -> Self {
        Self {
            fanout_thresholds: vec![
                (3, DriveStrength::X1),
                (6, DriveStrength::X2),
                (12, DriveStrength::X4),
            ],
            timing_driven: false,
            max_drive: DriveStrength::X4,
            ..Default::default()
        }
    }

    /// Create config optimized for timing
    pub fn timing_focused() -> Self {
        Self {
            fanout_thresholds: vec![
                (2, DriveStrength::X1),
                (3, DriveStrength::X2),
                (5, DriveStrength::X4),
                (8, DriveStrength::X8),
            ],
            timing_driven: true,
            target_slack: 100.0, // 100ps slack target
            max_drive: DriveStrength::X8,
            ..Default::default()
        }
    }
}

/// Drive strength levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DriveStrength {
    X1,
    X2,
    X4,
    X8,
    X16,
}

impl DriveStrength {
    /// Get the multiplier for this drive strength
    pub fn multiplier(&self) -> usize {
        match self {
            Self::X1 => 1,
            Self::X2 => 2,
            Self::X4 => 4,
            Self::X8 => 8,
            Self::X16 => 16,
        }
    }

    /// Get the suffix for cell naming
    pub fn suffix(&self) -> &'static str {
        match self {
            Self::X1 => "X1",
            Self::X2 => "X2",
            Self::X4 => "X4",
            Self::X8 => "X8",
            Self::X16 => "X16",
        }
    }

    /// Parse drive strength from cell name suffix
    pub fn from_suffix(suffix: &str) -> Option<Self> {
        match suffix {
            "X1" => Some(Self::X1),
            "X2" => Some(Self::X2),
            "X4" => Some(Self::X4),
            "X8" => Some(Self::X8),
            "X16" => Some(Self::X16),
            _ => None,
        }
    }

    /// Get the next larger drive strength
    pub fn upsize(&self) -> Option<Self> {
        match self {
            Self::X1 => Some(Self::X2),
            Self::X2 => Some(Self::X4),
            Self::X4 => Some(Self::X8),
            Self::X8 => Some(Self::X16),
            Self::X16 => None,
        }
    }

    /// Get relative area cost
    pub fn area_cost(&self) -> f64 {
        self.multiplier() as f64
    }

    /// Get relative delay improvement (smaller is faster)
    pub fn delay_factor(&self) -> f64 {
        1.0 / (self.multiplier() as f64).sqrt()
    }
}

/// Statistics from cell sizing
#[derive(Debug, Clone, Default)]
pub struct CellSizingStats {
    /// Total cells analyzed
    pub cells_analyzed: usize,
    /// Cells that were resized
    pub cells_resized: usize,
    /// Cells upsized
    pub cells_upsized: usize,
    /// Cells downsized
    pub cells_downsized: usize,
    /// Area change (positive = increase)
    pub area_change: f64,
    /// Distribution of final drive strengths
    pub drive_distribution: HashMap<DriveStrength, usize>,
}

impl CellSizingStats {
    /// Get summary string
    pub fn summary(&self) -> String {
        format!(
            "Analyzed {} cells, resized {} (↑{} ↓{}), area change: {:.1}%",
            self.cells_analyzed,
            self.cells_resized,
            self.cells_upsized,
            self.cells_downsized,
            self.area_change * 100.0
        )
    }
}

/// Cell sizer for drive strength optimization
pub struct CellSizer {
    config: CellSizingConfig,
    stats: CellSizingStats,
}

impl Default for CellSizer {
    fn default() -> Self {
        Self::new()
    }
}

impl CellSizer {
    /// Create a new cell sizer with default config
    pub fn new() -> Self {
        Self {
            config: CellSizingConfig::default(),
            stats: CellSizingStats::default(),
        }
    }

    /// Create with specific config
    pub fn with_config(config: CellSizingConfig) -> Self {
        Self {
            config,
            stats: CellSizingStats::default(),
        }
    }

    /// Get statistics from the last run
    pub fn stats(&self) -> &CellSizingStats {
        &self.stats
    }

    /// Run cell sizing on a gate netlist
    pub fn size_cells(&mut self, netlist: &mut GateNetlist) -> CellSizingStats {
        self.stats = CellSizingStats::default();

        // Compute fanout for each net
        let fanout = self.compute_fanout(netlist);

        // Track original area
        let original_area: f64 = netlist.cells.iter().map(|c| self.cell_area(c)).sum();

        // Size each cell
        for cell in &mut netlist.cells {
            self.stats.cells_analyzed += 1;

            // Skip cells matching preserve patterns
            if self.should_preserve(&cell.cell_type) {
                continue;
            }

            // Get current drive strength
            let current_drive = self.parse_drive_strength(&cell.cell_type);

            // Compute required drive strength based on fanout
            let max_fanout = cell
                .outputs
                .iter()
                .map(|id| fanout.get(id).copied().unwrap_or(0))
                .max()
                .unwrap_or(0);

            let required_drive = self.select_drive_strength(max_fanout);

            // Apply sizing change if needed
            if let Some(current) = current_drive {
                if required_drive != current && required_drive <= self.config.max_drive {
                    self.resize_cell(cell, required_drive);
                    self.stats.cells_resized += 1;

                    if required_drive > current {
                        self.stats.cells_upsized += 1;
                    } else {
                        self.stats.cells_downsized += 1;
                    }

                    *self
                        .stats
                        .drive_distribution
                        .entry(required_drive)
                        .or_insert(0) += 1;
                } else {
                    *self.stats.drive_distribution.entry(current).or_insert(0) += 1;
                }
            } else {
                // Cell doesn't have drive suffix, add one if needed
                if max_fanout > 2 {
                    self.add_drive_suffix(cell, required_drive);
                    self.stats.cells_resized += 1;
                    self.stats.cells_upsized += 1;

                    *self
                        .stats
                        .drive_distribution
                        .entry(required_drive)
                        .or_insert(0) += 1;
                }
            }
        }

        // Compute area change
        let final_area: f64 = netlist.cells.iter().map(|c| self.cell_area(c)).sum();
        self.stats.area_change = if original_area > 0.0 {
            (final_area - original_area) / original_area
        } else {
            0.0
        };

        self.stats.clone()
    }

    /// Compute fanout for each net
    fn compute_fanout(&self, netlist: &GateNetlist) -> HashMap<GateNetId, usize> {
        let mut fanout: HashMap<GateNetId, usize> = HashMap::new();

        // Count cell input connections
        for cell in &netlist.cells {
            for input_id in &cell.inputs {
                *fanout.entry(*input_id).or_insert(0) += 1;
            }
        }

        // Count output port connections
        for output_id in &netlist.outputs {
            *fanout.entry(*output_id).or_insert(0) += 1;
        }

        fanout
    }

    /// Select drive strength based on fanout
    fn select_drive_strength(&self, fanout: usize) -> DriveStrength {
        for (threshold, drive) in &self.config.fanout_thresholds {
            if fanout <= *threshold {
                return *drive;
            }
        }
        self.config.max_drive
    }

    /// Parse drive strength from cell type name
    fn parse_drive_strength(&self, cell_type: &str) -> Option<DriveStrength> {
        // Look for _X1, _X2, etc. suffix
        if let Some(idx) = cell_type.rfind('_') {
            let suffix = &cell_type[idx + 1..];
            return DriveStrength::from_suffix(suffix);
        }
        None
    }

    /// Get base cell type (without drive suffix)
    fn base_cell_type(&self, cell_type: &str) -> String {
        if let Some(idx) = cell_type.rfind('_') {
            let suffix = &cell_type[idx + 1..];
            if DriveStrength::from_suffix(suffix).is_some() {
                return cell_type[..idx].to_string();
            }
        }
        cell_type.to_string()
    }

    /// Resize a cell to a new drive strength
    fn resize_cell(&self, cell: &mut Cell, new_drive: DriveStrength) {
        let base = self.base_cell_type(&cell.cell_type);
        cell.cell_type = format!("{}_{}", base, new_drive.suffix());
    }

    /// Add drive suffix to a cell that doesn't have one
    fn add_drive_suffix(&self, cell: &mut Cell, drive: DriveStrength) {
        cell.cell_type = format!("{}_{}", cell.cell_type, drive.suffix());
    }

    /// Check if cell should be preserved (not resized)
    fn should_preserve(&self, cell_type: &str) -> bool {
        for pattern in &self.config.preserve_patterns {
            if cell_type.contains(pattern) {
                return true;
            }
        }
        false
    }

    /// Estimate cell area based on type and drive
    fn cell_area(&self, cell: &Cell) -> f64 {
        let base_area = match self.base_cell_type(&cell.cell_type).as_str() {
            t if t.starts_with("INV") => 1.0,
            t if t.starts_with("BUF") => 1.0,
            t if t.starts_with("AND2") => 2.0,
            t if t.starts_with("OR2") => 2.0,
            t if t.starts_with("NAND2") => 1.5,
            t if t.starts_with("NOR2") => 1.5,
            t if t.starts_with("XOR2") => 3.0,
            t if t.starts_with("AND3") => 2.5,
            t if t.starts_with("OR3") => 2.5,
            t if t.starts_with("NAND3") => 2.0,
            t if t.starts_with("NOR3") => 2.0,
            t if t.starts_with("AND4") => 3.0,
            t if t.starts_with("OR4") => 3.0,
            t if t.starts_with("MUX2") => 3.0,
            t if t.starts_with("DFF") => 5.0,
            _ => 2.0,
        };

        let drive_mult = self
            .parse_drive_strength(&cell.cell_type)
            .map(|d| d.area_cost())
            .unwrap_or(1.0);

        base_area * drive_mult
    }
}

/// Convenience function to size cells with default config
pub fn size_cells(netlist: &mut GateNetlist) -> CellSizingStats {
    let mut sizer = CellSizer::new();
    sizer.size_cells(netlist)
}

/// Size cells with area focus
pub fn size_cells_for_area(netlist: &mut GateNetlist) -> CellSizingStats {
    let mut sizer = CellSizer::with_config(CellSizingConfig::area_focused());
    sizer.size_cells(netlist)
}

/// Size cells with timing focus
pub fn size_cells_for_timing(netlist: &mut GateNetlist) -> CellSizingStats {
    let mut sizer = CellSizer::with_config(CellSizingConfig::timing_focused());
    sizer.size_cells(netlist)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_drive_strength_ordering() {
        assert!(DriveStrength::X1 < DriveStrength::X2);
        assert!(DriveStrength::X2 < DriveStrength::X4);
        assert!(DriveStrength::X4 < DriveStrength::X8);
    }

    #[test]
    fn test_drive_strength_suffix() {
        assert_eq!(DriveStrength::X1.suffix(), "X1");
        assert_eq!(DriveStrength::X4.suffix(), "X4");
        assert_eq!(DriveStrength::from_suffix("X2"), Some(DriveStrength::X2));
        assert_eq!(DriveStrength::from_suffix("invalid"), None);
    }

    #[test]
    fn test_drive_strength_upsize() {
        assert_eq!(DriveStrength::X1.upsize(), Some(DriveStrength::X2));
        assert_eq!(DriveStrength::X8.upsize(), Some(DriveStrength::X16));
        assert_eq!(DriveStrength::X16.upsize(), None);
    }

    #[test]
    fn test_cell_sizer_parse_drive() {
        let sizer = CellSizer::new();

        assert_eq!(
            sizer.parse_drive_strength("AND2_X1"),
            Some(DriveStrength::X1)
        );
        assert_eq!(
            sizer.parse_drive_strength("NAND3_X4"),
            Some(DriveStrength::X4)
        );
        assert_eq!(sizer.parse_drive_strength("INV"), None);
    }

    #[test]
    fn test_cell_sizer_base_type() {
        let sizer = CellSizer::new();

        assert_eq!(sizer.base_cell_type("AND2_X1"), "AND2");
        assert_eq!(sizer.base_cell_type("NAND3_X4"), "NAND3");
        assert_eq!(sizer.base_cell_type("INV"), "INV");
    }

    #[test]
    fn test_drive_selection() {
        let sizer = CellSizer::new();

        assert_eq!(sizer.select_drive_strength(1), DriveStrength::X1);
        assert_eq!(sizer.select_drive_strength(2), DriveStrength::X1);
        assert_eq!(sizer.select_drive_strength(3), DriveStrength::X2);
        assert_eq!(sizer.select_drive_strength(5), DriveStrength::X4);
        assert_eq!(sizer.select_drive_strength(10), DriveStrength::X8);
    }

    #[test]
    fn test_config_presets() {
        let area = CellSizingConfig::area_focused();
        assert_eq!(area.max_drive, DriveStrength::X4);

        let timing = CellSizingConfig::timing_focused();
        assert!(timing.timing_driven);
        assert_eq!(timing.max_drive, DriveStrength::X8);
    }

    #[test]
    fn test_stats_summary() {
        let stats = CellSizingStats {
            cells_analyzed: 100,
            cells_resized: 20,
            cells_upsized: 15,
            cells_downsized: 5,
            area_change: 0.1,
            ..Default::default()
        };

        let summary = stats.summary();
        assert!(summary.contains("100 cells"));
        assert!(summary.contains("resized 20"));
    }
}
