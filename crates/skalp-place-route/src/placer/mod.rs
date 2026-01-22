//! FPGA Placement Engine
//!
//! Implements placement algorithms for FPGA designs:
//! - Analytical placement using quadratic wirelength minimization
//! - Legalization to valid BEL sites
//! - Simulated annealing refinement

mod analytical;
mod annealing;
mod legalization;

pub use analytical::AnalyticalPlacer;
pub use annealing::SimulatedAnnealing;
pub use legalization::Legalizer;

use crate::device::{BelType, Device, TileType};
use crate::error::{PlaceRouteError, Result};
use serde::{Deserialize, Serialize};
use skalp_lir::gate_netlist::{CellId, GateNetlist};
use std::collections::HashMap;

/// Placement algorithm selection
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum PlacementAlgorithm {
    /// Random placement (baseline)
    Random,
    /// Analytical placement using quadratic wirelength
    Analytical,
    /// Force-directed placement
    ForceDirected,
    /// Simulated annealing (highest quality)
    SimulatedAnnealing,
    /// Analytical with simulated annealing refinement
    #[default]
    AnalyticalWithRefinement,
}

/// Placer configuration
#[derive(Debug, Clone)]
pub struct PlacerConfig {
    /// Placement algorithm
    pub algorithm: PlacementAlgorithm,
    /// Maximum iterations for iterative algorithms
    pub max_iterations: usize,
    /// Initial temperature for simulated annealing
    pub initial_temperature: f64,
    /// Cooling rate for simulated annealing
    pub cooling_rate: f64,
    /// Weight for timing cost
    pub timing_weight: f64,
    /// Weight for wirelength cost
    pub wirelength_weight: f64,
    /// Weight for congestion cost
    pub congestion_weight: f64,
    /// Random seed for reproducibility
    pub seed: u64,
}

impl Default for PlacerConfig {
    fn default() -> Self {
        Self {
            algorithm: PlacementAlgorithm::AnalyticalWithRefinement,
            max_iterations: 5000,
            initial_temperature: 100.0,
            cooling_rate: 0.95,
            timing_weight: 0.5,
            wirelength_weight: 0.4,
            congestion_weight: 0.1,
            seed: 42,
        }
    }
}

/// Location of a placed cell
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PlacementLoc {
    /// Tile X coordinate
    pub tile_x: u32,
    /// Tile Y coordinate
    pub tile_y: u32,
    /// BEL index within the tile
    pub bel_index: usize,
    /// BEL type
    pub bel_type: BelType,
}

impl PlacementLoc {
    /// Create a new placement location
    pub fn new(tile_x: u32, tile_y: u32, bel_index: usize, bel_type: BelType) -> Self {
        Self {
            tile_x,
            tile_y,
            bel_index,
            bel_type,
        }
    }

    /// Get coordinates as (x, y)
    pub fn coords(&self) -> (u32, u32) {
        (self.tile_x, self.tile_y)
    }
}

/// Placement result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlacementResult {
    /// Cell placements: CellId -> PlacementLoc
    pub placements: HashMap<CellId, PlacementLoc>,
    /// Estimated wirelength
    pub wirelength: u64,
    /// Timing score (lower is better)
    pub timing_score: f64,
    /// Device utilization (0.0 - 1.0)
    pub utilization: f64,
    /// Placement cost (combined metric)
    pub cost: f64,
}

impl PlacementResult {
    /// Create an empty placement result
    pub fn new() -> Self {
        Self {
            placements: HashMap::new(),
            wirelength: 0,
            timing_score: 0.0,
            utilization: 0.0,
            cost: 0.0,
        }
    }

    /// Get placement for a cell
    pub fn get(&self, cell_id: CellId) -> Option<&PlacementLoc> {
        self.placements.get(&cell_id)
    }

    /// Check if a cell is placed
    pub fn is_placed(&self, cell_id: CellId) -> bool {
        self.placements.contains_key(&cell_id)
    }

    /// Get all placed cells
    pub fn cells(&self) -> impl Iterator<Item = (&CellId, &PlacementLoc)> {
        self.placements.iter()
    }

    /// Get number of placed cells
    pub fn cell_count(&self) -> usize {
        self.placements.len()
    }
}

impl Default for PlacementResult {
    fn default() -> Self {
        Self::new()
    }
}

/// Main placer struct
pub struct Placer<D: Device> {
    /// Configuration
    config: PlacerConfig,
    /// Target device
    device: D,
}

impl<D: Device + Clone> Placer<D> {
    /// Create a new placer
    pub fn new(config: PlacerConfig, device: D) -> Self {
        Self { config, device }
    }

    /// Place a design
    pub fn place(&mut self, netlist: &GateNetlist) -> Result<PlacementResult> {
        // Check device capacity
        self.check_capacity(netlist)?;

        // Run placement algorithm
        let mut result = match self.config.algorithm {
            PlacementAlgorithm::Random => self.random_placement(netlist)?,
            PlacementAlgorithm::Analytical => {
                let analytical = AnalyticalPlacer::new(&self.device);
                analytical.place(netlist)?
            }
            PlacementAlgorithm::ForceDirected => {
                // Use analytical as base for force-directed
                let analytical = AnalyticalPlacer::new(&self.device);
                analytical.place(netlist)?
            }
            PlacementAlgorithm::SimulatedAnnealing => {
                // Start with random, then anneal
                let initial = self.random_placement(netlist)?;
                let annealer = SimulatedAnnealing::new(
                    &self.device,
                    self.config.initial_temperature,
                    self.config.cooling_rate,
                    self.config.max_iterations,
                );
                annealer.optimize(initial, netlist)?
            }
            PlacementAlgorithm::AnalyticalWithRefinement => {
                // Analytical + legalization + SA refinement
                let analytical = AnalyticalPlacer::new(&self.device);
                let initial = analytical.place(netlist)?;

                // Legalize
                let legalizer = Legalizer::new(&self.device);
                let legalized = legalizer.legalize(initial, netlist)?;

                // SA refinement
                let annealer = SimulatedAnnealing::new(
                    &self.device,
                    self.config.initial_temperature * 0.5, // Lower temp for refinement
                    self.config.cooling_rate,
                    self.config.max_iterations / 2,
                );
                annealer.optimize(legalized, netlist)?
            }
        };

        // Calculate final metrics
        result.wirelength = self.calculate_wirelength(&result, netlist);
        result.utilization = self.calculate_utilization(&result);
        result.cost = self.calculate_cost(&result, netlist);

        Ok(result)
    }

    /// Check if design fits on device
    fn check_capacity(&self, netlist: &GateNetlist) -> Result<()> {
        let stats = self.device.stats();

        // Count required resources
        let mut required_luts = 0usize;
        let mut required_ffs = 0usize;
        let mut required_ios = 0usize;
        let mut required_brams = 0usize;

        for cell in &netlist.cells {
            let cell_type = &cell.cell_type;
            if cell_type.contains("LUT") || cell_type.starts_with("SB_LUT") {
                required_luts += 1;
            } else if cell_type.contains("DFF") || cell_type.starts_with("SB_DFF") {
                required_ffs += 1;
            } else if cell_type.contains("IO") || cell_type.starts_with("SB_IO") {
                required_ios += 1;
            } else if cell_type.contains("RAM") || cell_type.starts_with("SB_RAM") {
                required_brams += 1;
            }
        }

        // Check capacity
        if required_luts > stats.total_luts {
            return Err(PlaceRouteError::CapacityExceeded {
                resource: "LUTs".to_string(),
                required: required_luts,
                available: stats.total_luts,
            });
        }
        if required_ffs > stats.total_ffs {
            return Err(PlaceRouteError::CapacityExceeded {
                resource: "FFs".to_string(),
                required: required_ffs,
                available: stats.total_ffs,
            });
        }
        if required_ios > stats.total_ios {
            return Err(PlaceRouteError::CapacityExceeded {
                resource: "I/Os".to_string(),
                required: required_ios,
                available: stats.total_ios,
            });
        }
        if required_brams > stats.total_brams {
            return Err(PlaceRouteError::CapacityExceeded {
                resource: "BRAMs".to_string(),
                required: required_brams,
                available: stats.total_brams,
            });
        }

        Ok(())
    }

    /// Random placement (baseline)
    fn random_placement(&self, netlist: &GateNetlist) -> Result<PlacementResult> {
        use rand::{Rng, SeedableRng};
        let mut rng = rand::rngs::StdRng::seed_from_u64(self.config.seed);

        let mut result = PlacementResult::new();
        let (width, height) = self.device.grid_size();

        // Track used BELs
        let mut used_bels: HashMap<(u32, u32, usize), CellId> = HashMap::new();

        for cell in &netlist.cells {
            let cell_id = cell.id;
            // Determine required BEL type
            let bel_type = self.cell_to_bel_type(&cell.cell_type);
            let tile_type = self.bel_to_tile_type(bel_type);

            // Find a random valid location
            let mut attempts = 0;
            loop {
                let x = rng.gen_range(0..width);
                let y = rng.gen_range(0..height);

                if let Some(tile) = self.device.tile_at(x, y) {
                    if tile.tile_type() == tile_type {
                        // Find an unused BEL
                        for (bel_idx, bel) in tile.bels().iter().enumerate() {
                            if bel.bel_type == bel_type && !used_bels.contains_key(&(x, y, bel_idx))
                            {
                                used_bels.insert((x, y, bel_idx), cell_id);
                                result
                                    .placements
                                    .insert(cell_id, PlacementLoc::new(x, y, bel_idx, bel_type));
                                break;
                            }
                        }
                        if result.placements.contains_key(&cell_id) {
                            break;
                        }
                    }
                }

                attempts += 1;
                if attempts > 10000 {
                    return Err(PlaceRouteError::NoBelForCell {
                        cell_type: cell.cell_type.clone(),
                        path: cell.path.clone(),
                    });
                }
            }
        }

        Ok(result)
    }

    /// Convert cell type to BEL type
    fn cell_to_bel_type(&self, cell_type: &str) -> BelType {
        if cell_type.contains("LUT") || cell_type.starts_with("SB_LUT") {
            BelType::Lut4
        } else if cell_type.contains("DFFE")
            || cell_type.starts_with("SB_DFFE")
            || cell_type.starts_with("SB_SDFFE")
        {
            BelType::DffE
        } else if cell_type.contains("DFFSR") || cell_type.starts_with("SB_DFFSR") {
            BelType::DffSr
        } else if cell_type.contains("DFF") || cell_type.starts_with("SB_DFF") {
            BelType::Dff
        } else if cell_type.contains("CARRY") || cell_type.starts_with("SB_CARRY") {
            BelType::Carry
        } else if cell_type.contains("IO") || cell_type.starts_with("SB_IO") {
            BelType::IoCell
        } else if cell_type.contains("RAM") || cell_type.starts_with("SB_RAM") {
            BelType::RamSlice
        } else if cell_type.contains("GB") || cell_type.starts_with("SB_GB") {
            BelType::GlobalBuf
        } else {
            // Default to LUT for unknown cells
            BelType::Lut4
        }
    }

    /// Convert BEL type to tile type
    fn bel_to_tile_type(&self, bel_type: BelType) -> TileType {
        match bel_type {
            BelType::Lut4
            | BelType::Lut6
            | BelType::Dff
            | BelType::DffE
            | BelType::DffSr
            | BelType::DffSrE
            | BelType::Carry => TileType::Logic,
            BelType::IoCell => TileType::IoTop, // Will be refined during placement
            BelType::RamSlice => TileType::RamTop,
            BelType::GlobalBuf => TileType::GlobalBuf,
            BelType::Pll => TileType::Pll,
            BelType::DspSlice => TileType::Dsp,
        }
    }

    /// Calculate total wirelength (HPWL)
    fn calculate_wirelength(&self, result: &PlacementResult, netlist: &GateNetlist) -> u64 {
        let mut total_wl = 0u64;

        for net in &netlist.nets {
            // Get all connected cells
            let mut min_x = u32::MAX;
            let mut max_x = 0u32;
            let mut min_y = u32::MAX;
            let mut max_y = 0u32;

            // Driver
            if let Some(driver_id) = net.driver {
                if let Some(loc) = result.placements.get(&driver_id) {
                    min_x = min_x.min(loc.tile_x);
                    max_x = max_x.max(loc.tile_x);
                    min_y = min_y.min(loc.tile_y);
                    max_y = max_y.max(loc.tile_y);
                }
            }

            // Fanout
            for (cell_id, _pin) in &net.fanout {
                if let Some(loc) = result.placements.get(cell_id) {
                    min_x = min_x.min(loc.tile_x);
                    max_x = max_x.max(loc.tile_x);
                    min_y = min_y.min(loc.tile_y);
                    max_y = max_y.max(loc.tile_y);
                }
            }

            // HPWL (Half-Perimeter Wire Length)
            if min_x <= max_x && min_y <= max_y {
                total_wl += (max_x - min_x + max_y - min_y) as u64;
            }
        }

        total_wl
    }

    /// Calculate device utilization
    fn calculate_utilization(&self, result: &PlacementResult) -> f64 {
        let stats = self.device.stats();
        let total_resources = stats.total_luts + stats.total_ffs;
        if total_resources == 0 {
            return 0.0;
        }
        result.placements.len() as f64 / total_resources as f64
    }

    /// Calculate combined placement cost
    fn calculate_cost(&self, result: &PlacementResult, netlist: &GateNetlist) -> f64 {
        let wl = self.calculate_wirelength(result, netlist) as f64;
        let timing = result.timing_score;
        let congestion = 0.0; // TODO: implement congestion estimation

        self.config.wirelength_weight * wl
            + self.config.timing_weight * timing
            + self.config.congestion_weight * congestion
    }
}
