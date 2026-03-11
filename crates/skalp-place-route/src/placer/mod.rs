//! FPGA Placement Engine
//!
//! Implements placement algorithms for FPGA designs:
//! - Analytical placement using quadratic wirelength minimization
//! - Legalization to valid BEL sites
//! - Simulated annealing refinement
//! - I/O pin constraints

mod analytical;
mod annealing;
mod constraints;
mod legalization;

pub use analytical::AnalyticalPlacer;
pub use annealing::SimulatedAnnealing;
pub use constraints::{
    BelConstraint, FrequencyConstraint, IoConstraints, PinConstraint, PlacementRegion,
    PullResistor, PullType, RegionConstraint,
};
pub use legalization::Legalizer;

use crate::device::{BelType, Device, TileType};
use crate::error::{PlaceRouteError, Result};
use crate::packing::CarryChain;
use serde::{Deserialize, Serialize};
use skalp_lir::gate_netlist::{CellId, GateNetlist};
use skalp_lir::tech_library::CellFunction;
use std::collections::HashMap;

/// Check if a cell is a constant driver (TIE_HIGH/TIE_LOW/GND/VCC).
/// These don't need physical placement on FPGA — the fabric has hardwired VCC/GND.
pub(crate) fn is_constant_cell(cell_type: &str) -> bool {
    cell_type.starts_with("TIE_")
        || cell_type.starts_with("TIE0")
        || cell_type.starts_with("TIE1")
        || cell_type.starts_with("TIEH")
        || cell_type.starts_with("TIEL")
        || cell_type == "SB_GND"
        || cell_type == "SB_VCC"
        || cell_type == "GND"
        || cell_type == "VCC"
}

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
    /// Timing-driven simulated annealing
    TimingDriven,
    /// Analytical with timing-driven refinement
    AnalyticalTimingDriven,
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
    /// I/O pin constraints
    pub io_constraints: IoConstraints,
    /// Enable parallel move evaluation in simulated annealing
    pub parallel: bool,
    /// Batch size for parallel move evaluation
    pub parallel_batch_size: usize,
}

impl Default for PlacerConfig {
    fn default() -> Self {
        Self {
            algorithm: PlacementAlgorithm::AnalyticalTimingDriven,
            max_iterations: 5000,
            initial_temperature: 100.0,
            cooling_rate: 0.95,
            timing_weight: 0.5,
            wirelength_weight: 0.4,
            congestion_weight: 0.1,
            seed: 42,
            io_constraints: IoConstraints::new(),
            parallel: false, // Default to serial for determinism
            parallel_batch_size: 64,
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

    /// Place a design with carry chain constraints
    pub fn place_with_carry_chains(
        &mut self,
        netlist: &GateNetlist,
        carry_chains: &[CarryChain],
    ) -> Result<PlacementResult> {
        // Check device capacity
        self.check_capacity(netlist)?;

        // Extract inline constraints
        let inline_constraints = Self::extract_inline_constraints(netlist);
        if !inline_constraints.is_empty() {
            for constraint in inline_constraints.all() {
                if !self
                    .config
                    .io_constraints
                    .has_constraint(&constraint.signal_name)
                {
                    self.config.io_constraints.add(constraint.clone());
                }
            }
        }

        // Validate and resolve I/O constraints
        let resolved_constraints = if !self.config.io_constraints.is_empty() {
            self.config.io_constraints.validate(&self.device)?;
            self.config.io_constraints.resolve(&self.device)?
        } else {
            HashMap::new()
        };

        // Run placement with carry chain awareness
        let mut result = match self.config.algorithm {
            PlacementAlgorithm::AnalyticalWithRefinement
            | PlacementAlgorithm::AnalyticalTimingDriven => {
                // Analytical with carry chain column constraints
                let analytical = AnalyticalPlacer::new(&self.device);
                let initial = if carry_chains.is_empty() {
                    analytical.place(netlist)?
                } else {
                    analytical.place_with_carry_chains(netlist, carry_chains)?
                };

                // Legalize with carry chain consecutive-row constraints
                let legalizer = Legalizer::new(&self.device);
                let legalized =
                    legalizer.legalize_with_carry_chains(initial, netlist, carry_chains)?;

                // SA refinement with carry chain awareness
                let timing_weight = if matches!(
                    self.config.algorithm,
                    PlacementAlgorithm::AnalyticalTimingDriven
                ) {
                    self.config.timing_weight
                } else {
                    0.0
                };

                let annealer = if timing_weight > 0.0 {
                    SimulatedAnnealing::new_timing_driven(
                        &self.device,
                        self.config.initial_temperature * 0.5,
                        self.config.cooling_rate,
                        self.config.max_iterations / 2,
                        timing_weight,
                    )
                } else {
                    SimulatedAnnealing::new(
                        &self.device,
                        self.config.initial_temperature * 0.5,
                        self.config.cooling_rate,
                        self.config.max_iterations / 2,
                    )
                }
                .with_parallel(self.config.parallel)
                .with_batch_size(self.config.parallel_batch_size)
                .with_carry_chains(carry_chains.to_vec());

                annealer.optimize(legalized, netlist)?
            }
            _ => {
                // For other algorithms, fall back to regular placement
                return self.place(netlist);
            }
        };

        // Apply I/O constraints
        if !resolved_constraints.is_empty() {
            self.apply_io_constraints(&mut result, netlist, &resolved_constraints)?;
        }

        // Calculate final metrics
        result.wirelength = self.calculate_wirelength(&result, netlist);
        result.utilization = self.calculate_utilization(&result);
        result.cost = self.calculate_cost(&result, netlist);

        Ok(result)
    }

    /// Place a design
    pub fn place(&mut self, netlist: &GateNetlist) -> Result<PlacementResult> {
        // Check device capacity
        self.check_capacity(netlist)?;

        // Extract inline constraints from cell parameters (LOC, IO_STANDARD, etc.)
        let inline_constraints = Self::extract_inline_constraints(netlist);
        if !inline_constraints.is_empty() {
            for constraint in inline_constraints.all() {
                if !self
                    .config
                    .io_constraints
                    .has_constraint(&constraint.signal_name)
                {
                    self.config.io_constraints.add(constraint.clone());
                }
            }
        }

        // Validate and resolve I/O constraints
        let resolved_constraints = if !self.config.io_constraints.is_empty() {
            self.config.io_constraints.validate(&self.device)?;
            self.config.io_constraints.resolve(&self.device)?
        } else {
            HashMap::new()
        };

        // Run placement algorithm
        let mut result = match self.config.algorithm {
            PlacementAlgorithm::Random => self.random_placement(netlist)?,
            PlacementAlgorithm::Analytical => {
                let analytical = AnalyticalPlacer::new(&self.device)
                    .with_resolved_constraints(resolved_constraints.clone());
                analytical.place(netlist)?
            }
            PlacementAlgorithm::ForceDirected => {
                // Use analytical as base for force-directed
                let analytical = AnalyticalPlacer::new(&self.device)
                    .with_resolved_constraints(resolved_constraints.clone());
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
                )
                .with_parallel(self.config.parallel)
                .with_batch_size(self.config.parallel_batch_size);
                annealer.optimize(initial, netlist)?
            }
            PlacementAlgorithm::AnalyticalWithRefinement => {
                // Analytical + legalization + SA refinement
                let analytical = AnalyticalPlacer::new(&self.device)
                    .with_resolved_constraints(resolved_constraints.clone());
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
                )
                .with_parallel(self.config.parallel)
                .with_batch_size(self.config.parallel_batch_size);
                annealer.optimize(legalized, netlist)?
            }
            PlacementAlgorithm::TimingDriven => {
                // Timing-driven simulated annealing from random start
                let initial = self.random_placement(netlist)?;
                let annealer = SimulatedAnnealing::new_timing_driven(
                    &self.device,
                    self.config.initial_temperature,
                    self.config.cooling_rate,
                    self.config.max_iterations,
                    self.config.timing_weight,
                )
                .with_parallel(self.config.parallel)
                .with_batch_size(self.config.parallel_batch_size);
                annealer.optimize(initial, netlist)?
            }
            PlacementAlgorithm::AnalyticalTimingDriven => {
                // Analytical + legalization + timing-driven SA refinement
                let analytical = AnalyticalPlacer::new(&self.device)
                    .with_resolved_constraints(resolved_constraints.clone());
                let initial = analytical.place(netlist)?;

                // Legalize
                let legalizer = Legalizer::new(&self.device);
                let legalized = legalizer.legalize(initial, netlist)?;

                // Timing-driven SA refinement
                let annealer = SimulatedAnnealing::new_timing_driven(
                    &self.device,
                    self.config.initial_temperature * 0.5,
                    self.config.cooling_rate,
                    self.config.max_iterations / 2,
                    self.config.timing_weight,
                )
                .with_parallel(self.config.parallel)
                .with_batch_size(self.config.parallel_batch_size);
                annealer.optimize(legalized, netlist)?
            }
        };

        // Apply I/O constraints - override placements for constrained I/O cells
        if !resolved_constraints.is_empty() {
            self.apply_io_constraints(&mut result, netlist, &resolved_constraints)?;
        }

        // Calculate final metrics
        result.wirelength = self.calculate_wirelength(&result, netlist);
        result.utilization = self.calculate_utilization(&result);
        result.cost = self.calculate_cost(&result, netlist);

        Ok(result)
    }

    /// Extract inline IO constraints from cell parameters
    ///
    /// Reads LOC, IO_STANDARD, and DRIVE_STRENGTH from IO cell parameters
    /// (set by tech mapper from inline annotations like `@ { pin: "A1" }`)
    pub(crate) fn extract_inline_constraints(netlist: &GateNetlist) -> IoConstraints {
        let mut constraints = IoConstraints::new();

        for cell in &netlist.cells {
            let is_io = matches!(
                &cell.function,
                Some(CellFunction::InputPad)
                    | Some(CellFunction::OutputPad)
                    | Some(CellFunction::BidirPad)
                    | Some(CellFunction::ClockPad)
            );

            if !is_io {
                continue;
            }

            if let Some(loc) = cell.parameters.get("LOC") {
                // Extract signal name from path, stripping buffer suffixes
                let signal_name = cell.path.split('.').next_back().unwrap_or(&cell.path);
                let signal_name = signal_name
                    .strip_suffix("_ibuf")
                    .or_else(|| signal_name.strip_suffix("_obuf"))
                    .unwrap_or(signal_name);

                let mut constraint = PinConstraint::new(signal_name, loc.as_str());

                if let Some(standard) = cell.parameters.get("IO_STANDARD") {
                    constraint = constraint.with_io_standard(standard.as_str());
                }
                if let Some(drive) = cell.parameters.get("DRIVE_STRENGTH") {
                    if let Ok(strength) = drive.parse::<u8>() {
                        constraint = constraint.with_drive_strength(strength);
                    }
                }

                constraints.add(constraint);
            }
        }

        constraints
    }

    /// Check if design fits on device
    fn check_capacity(&self, netlist: &GateNetlist) -> Result<()> {
        let stats = self.device.stats();

        // Count required resources
        let mut required_luts = 0usize;
        let mut required_ffs = 0usize;
        let mut required_ios = 0usize;
        let mut required_brams = 0usize;
        let mut required_dsps = 0usize;

        for cell in &netlist.cells {
            if is_constant_cell(&cell.cell_type) {
                continue;
            }
            let cell_type = &cell.cell_type;
            if cell_type.contains("LUT") || cell_type.starts_with("SB_LUT") {
                required_luts += 1;
            } else if cell_type.contains("DFF") || cell_type.starts_with("SB_DFF") {
                required_ffs += 1;
            } else if cell_type.contains("IO") || cell_type.starts_with("SB_IO") {
                required_ios += 1;
            } else if cell_type.contains("RAM") || cell_type.starts_with("SB_RAM") {
                required_brams += 1;
            } else if cell_type.contains("MAC")
                || cell_type.starts_with("SB_MAC")
                || cell_type.contains("MULT")
            {
                required_dsps += 1;
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
        if required_dsps > stats.total_dsps {
            return Err(PlaceRouteError::CapacityExceeded {
                resource: "DSPs".to_string(),
                required: required_dsps,
                available: stats.total_dsps,
            });
        }

        Ok(())
    }

    /// Apply I/O constraints to placement result
    fn apply_io_constraints(
        &self,
        result: &mut PlacementResult,
        netlist: &GateNetlist,
        resolved_constraints: &HashMap<String, (u32, u32, usize)>,
    ) -> Result<()> {
        // Build a map from signal name to cell ID
        // For I/O cells, the path typically contains the port name
        let mut signal_to_cell: HashMap<&str, CellId> = HashMap::new();

        for cell in &netlist.cells {
            // Check if this is an I/O cell
            if cell.cell_type.contains("IO") || cell.cell_type.starts_with("SB_IO") {
                // Extract signal name from path (e.g., "top.clk" -> "clk")
                let signal_name = cell.path.split('.').next_back().unwrap_or(&cell.path);
                signal_to_cell.insert(signal_name, cell.id);

                // Also try the full path
                signal_to_cell.insert(&cell.path, cell.id);
            }
        }

        // Apply each constraint
        for (signal_name, &(tile_x, tile_y, bel_index)) in resolved_constraints {
            // Find the cell for this signal
            let cell_id = if let Some(&id) = signal_to_cell.get(signal_name.as_str()) {
                id
            } else {
                // Try matching by port name suffix
                let mut found = None;
                for (name, &id) in &signal_to_cell {
                    if name.ends_with(signal_name) || signal_name.ends_with(name) {
                        found = Some(id);
                        break;
                    }
                }
                match found {
                    Some(id) => id,
                    None => {
                        // Signal not found - this is a warning, not an error
                        // The signal might not exist in the design
                        continue;
                    }
                }
            };

            // Update the placement
            let bel_type = if let Some(existing) = result.placements.get(&cell_id) {
                existing.bel_type
            } else {
                BelType::IoCell
            };

            result.placements.insert(
                cell_id,
                PlacementLoc::new(tile_x, tile_y, bel_index, bel_type),
            );
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
            // Skip constant cells (TIE_HIGH/TIE_LOW/GND/VCC) — on iCE40 these
            // use hardwired fabric resources, not physical LUTs
            if is_constant_cell(&cell.cell_type) {
                continue;
            }

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
                    // Check tile type match, or fallback: check if tile has a compatible BEL
                    // (needed for BELs like PLL that live in IO tiles on HX/LP devices)
                    let tile_ok = tile.tile_type() == tile_type
                        || tile
                            .bels()
                            .iter()
                            .any(|b| Self::bel_types_compatible(b.bel_type, bel_type));
                    if tile_ok {
                        // Find an unused BEL
                        for (bel_idx, bel) in tile.bels().iter().enumerate() {
                            if Self::bel_types_compatible(bel.bel_type, bel_type)
                                && !used_bels.contains_key(&(x, y, bel_idx))
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
        } else if cell_type.contains("PLL") || cell_type.starts_with("SB_PLL") {
            BelType::Pll
        } else if cell_type.contains("MAC")
            || cell_type.starts_with("SB_MAC")
            || cell_type.contains("MULT")
        {
            BelType::DspSlice
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

    /// Check if two BEL types are compatible for placement
    /// In iCE40, all DFF variants use the same hardware with different config bits
    fn bel_types_compatible(available: BelType, required: BelType) -> bool {
        if available == required {
            return true;
        }
        matches!(
            (available, required),
            // A basic Dff BEL can implement any DFF variant
            (BelType::Dff, BelType::DffE)
                | (BelType::Dff, BelType::DffSr)
                | (BelType::Dff, BelType::DffSrE)
                // Any FF type can implement a basic DFF
                | (BelType::DffE, BelType::Dff)
                | (BelType::DffSr, BelType::Dff)
                | (BelType::DffSrE, BelType::Dff)
                | (BelType::DffSrE, BelType::DffE)
                | (BelType::DffSrE, BelType::DffSr)
        )
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

    /// Estimate routing congestion using RUDY (Rectangular Uniform wire DensitY).
    ///
    /// For each net: demand = HPWL * 2 / bbox_area
    /// This distributes wire demand uniformly across the bounding box,
    /// weighted by the expected wire density (proportional to HPWL).
    /// Track capacity comes from the iCE40 device model (~48 tracks/tile).
    fn estimate_congestion(&self, result: &PlacementResult, netlist: &GateNetlist) -> f64 {
        let (grid_x, grid_y) = self.device.grid_size();

        // iCE40 HX1K: 8 LCs per tile, each with local + span4 routing
        // Approximately 48 routing tracks per tile (8 local + 20 span4H + 20 span4V)
        let tracks_per_tile: f64 = 48.0;

        // Track estimated wire demand per tile
        let mut wire_demand: HashMap<(u32, u32), f64> = HashMap::new();

        // For each net, compute RUDY demand
        for net in &netlist.nets {
            let mut min_x = u32::MAX;
            let mut max_x = 0u32;
            let mut min_y = u32::MAX;
            let mut max_y = 0u32;
            let mut pin_count = 0u32;

            if let Some(driver_id) = net.driver {
                if let Some(loc) = result.placements.get(&driver_id) {
                    min_x = min_x.min(loc.tile_x);
                    max_x = max_x.max(loc.tile_x);
                    min_y = min_y.min(loc.tile_y);
                    max_y = max_y.max(loc.tile_y);
                    pin_count += 1;
                }
            }

            for (cell_id, _pin) in &net.fanout {
                if let Some(loc) = result.placements.get(cell_id) {
                    min_x = min_x.min(loc.tile_x);
                    max_x = max_x.max(loc.tile_x);
                    min_y = min_y.min(loc.tile_y);
                    max_y = max_y.max(loc.tile_y);
                    pin_count += 1;
                }
            }

            if pin_count < 2 || min_x > max_x || min_y > max_y {
                continue;
            }

            let dx = (max_x - min_x + 1) as f64;
            let dy = (max_y - min_y + 1) as f64;
            let hpwl = (max_x - min_x + max_y - min_y) as f64;
            let bbox_area = dx * dy;

            // RUDY: demand = HPWL * 2 / bbox_area
            // The factor of 2 accounts for both horizontal and vertical wires
            let demand_per_tile = if bbox_area > 0.0 {
                hpwl * 2.0 / bbox_area
            } else {
                0.0
            };

            for x in min_x..=max_x {
                for y in min_y..=max_y {
                    *wire_demand.entry((x, y)).or_insert(0.0) += demand_per_tile;
                }
            }
        }

        // Calculate congestion metric
        let mut max_congestion: f64 = 0.0;
        let mut total_congestion: f64 = 0.0;
        let mut congested_tiles: u32 = 0;

        for x in 0..grid_x {
            for y in 0..grid_y {
                let demand = wire_demand.get(&(x, y)).copied().unwrap_or(0.0);
                let tile_congestion = demand / tracks_per_tile;
                if tile_congestion > max_congestion {
                    max_congestion = tile_congestion;
                }
                total_congestion += tile_congestion;
                if tile_congestion > 1.0 {
                    congested_tiles += 1;
                }
            }
        }

        let total_tiles = (grid_x * grid_y) as f64;
        let avg_congestion = total_congestion / total_tiles;
        let congested_ratio = congested_tiles as f64 / total_tiles;

        // Combined metric: penalize both hotspots and widespread congestion
        0.5 * max_congestion + 0.3 * avg_congestion + 0.2 * congested_ratio * 10.0
    }

    /// Calculate combined placement cost
    fn calculate_cost(&self, result: &PlacementResult, netlist: &GateNetlist) -> f64 {
        let wl = self.calculate_wirelength(result, netlist) as f64;
        let timing = result.timing_score;
        let congestion = self.estimate_congestion(result, netlist);

        self.config.wirelength_weight * wl
            + self.config.timing_weight * timing
            + self.config.congestion_weight * congestion
    }
}
