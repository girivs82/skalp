//! Simulated Annealing Placement Optimization
//!
//! Refines placement using simulated annealing with HPWL cost function.
//! Supports timing-driven placement by incorporating estimated path delays.
//! Includes parallel move evaluation for improved performance on multi-core systems.

use super::{PlacementLoc, PlacementResult};
use crate::device::{BelType, Device};
use crate::error::Result;
use crate::timing::DelayModel;
use rand::{Rng, SeedableRng};
use rayon::prelude::*;
use skalp_lir::gate_netlist::{CellId, GateNetId, GateNetlist};
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicUsize, Ordering};

/// Move operation for simulated annealing
#[derive(Debug, Clone)]
enum Move {
    /// Swap two cells
    Swap(CellId, CellId),
    /// Move a cell to a new location
    Relocate(CellId, PlacementLoc),
}

/// Simulated annealing optimizer
pub struct SimulatedAnnealing<'a, D: Device> {
    device: &'a D,
    initial_temp: f64,
    cooling_rate: f64,
    max_iterations: usize,
    /// Timing weight (0.0 = pure wirelength, 1.0 = pure timing)
    timing_weight: f64,
    /// Delay model for timing estimation
    delay_model: DelayModel,
    /// Enable parallel move evaluation
    parallel: bool,
    /// Number of parallel moves to evaluate per batch
    parallel_batch_size: usize,
}

impl<'a, D: Device> SimulatedAnnealing<'a, D> {
    /// Create a new simulated annealing optimizer (wirelength-driven)
    pub fn new(device: &'a D, initial_temp: f64, cooling_rate: f64, max_iterations: usize) -> Self {
        Self {
            device,
            initial_temp,
            cooling_rate,
            max_iterations,
            timing_weight: 0.0, // Pure wirelength mode
            delay_model: DelayModel::ice40_default(),
            parallel: false,
            parallel_batch_size: 64,
        }
    }

    /// Create a timing-driven simulated annealing optimizer
    pub fn new_timing_driven(
        device: &'a D,
        initial_temp: f64,
        cooling_rate: f64,
        max_iterations: usize,
        timing_weight: f64,
    ) -> Self {
        Self {
            device,
            initial_temp,
            cooling_rate,
            max_iterations,
            timing_weight: timing_weight.clamp(0.0, 1.0),
            delay_model: DelayModel::ice40_default(),
            parallel: false,
            parallel_batch_size: 64,
        }
    }

    /// Create with a specific delay model
    pub fn with_delay_model(mut self, delay_model: DelayModel) -> Self {
        self.delay_model = delay_model;
        self
    }

    /// Enable parallel move evaluation
    pub fn with_parallel(mut self, parallel: bool) -> Self {
        self.parallel = parallel;
        self
    }

    /// Set parallel batch size (number of moves to evaluate in parallel)
    pub fn with_batch_size(mut self, batch_size: usize) -> Self {
        self.parallel_batch_size = batch_size.max(1);
        self
    }

    /// Optimize a placement using simulated annealing
    pub fn optimize(
        &self,
        initial: PlacementResult,
        netlist: &GateNetlist,
    ) -> Result<PlacementResult> {
        if self.parallel {
            self.optimize_parallel(initial, netlist)
        } else {
            self.optimize_serial(initial, netlist)
        }
    }

    /// Serial simulated annealing optimization
    fn optimize_serial(
        &self,
        initial: PlacementResult,
        netlist: &GateNetlist,
    ) -> Result<PlacementResult> {
        let mut rng = rand::rngs::StdRng::seed_from_u64(42);
        let mut current = initial;

        // Compute net criticalities for timing-driven mode
        let net_criticalities = if self.timing_weight > 0.0 {
            self.compute_net_criticalities(netlist, &current)
        } else {
            HashMap::new()
        };

        let mut current_cost = self.calculate_combined_cost(&current, netlist, &net_criticalities);
        let mut best = current.clone();
        let mut best_cost = current_cost;

        let mut temperature = self.initial_temp;

        // Build reverse mapping: location -> cell_id
        let mut location_to_cell: HashMap<(u32, u32, usize), CellId> = HashMap::new();
        for (&cell_id, loc) in &current.placements {
            location_to_cell.insert((loc.tile_x, loc.tile_y, loc.bel_index), cell_id);
        }

        let cells: Vec<CellId> = current.placements.keys().copied().collect();
        let moves_per_temp = cells.len().max(100);

        for _iteration in 0..self.max_iterations {
            // Perform several moves at each temperature
            for _ in 0..moves_per_temp {
                // Generate a random move (biased toward critical cells in timing mode)
                let move_op = if self.timing_weight > 0.0 && rng.gen::<f64>() < 0.3 {
                    self.generate_critical_path_move(
                        &current,
                        &cells,
                        netlist,
                        &net_criticalities,
                        &mut rng,
                    )
                } else {
                    self.generate_move(&current, &cells, &mut rng)
                };

                // Calculate cost delta
                let (new_placement, new_loc_map) =
                    self.apply_move(&current, &location_to_cell, &move_op);
                let new_cost =
                    self.calculate_combined_cost(&new_placement, netlist, &net_criticalities);
                let delta = new_cost - current_cost;

                // Acceptance criterion
                let accept = if delta <= 0.0 {
                    true
                } else {
                    let probability = (-delta / temperature).exp();
                    rng.gen::<f64>() < probability
                };

                if accept {
                    current = new_placement;
                    location_to_cell = new_loc_map;
                    current_cost = new_cost;

                    if current_cost < best_cost {
                        best = current.clone();
                        best_cost = current_cost;
                    }
                }
            }

            // Cool down
            temperature *= self.cooling_rate;

            // Early termination if temperature is too low
            if temperature < 0.01 {
                break;
            }
        }

        // Update metrics
        let mut result = best;
        result.cost = best_cost;

        // Calculate timing score for timing-driven mode
        if self.timing_weight > 0.0 {
            result.timing_score = self.estimate_timing_score(&result, netlist);
        }

        Ok(result)
    }

    /// Parallel simulated annealing optimization
    ///
    /// Evaluates multiple moves in parallel and selects the best improving moves.
    /// This provides speedup on multi-core systems while maintaining solution quality.
    fn optimize_parallel(
        &self,
        initial: PlacementResult,
        netlist: &GateNetlist,
    ) -> Result<PlacementResult> {
        let mut current = initial;

        // Compute net criticalities for timing-driven mode
        let net_criticalities = if self.timing_weight > 0.0 {
            self.compute_net_criticalities(netlist, &current)
        } else {
            HashMap::new()
        };

        let mut current_cost = self.calculate_combined_cost(&current, netlist, &net_criticalities);
        let mut best = current.clone();
        let mut best_cost = current_cost;

        let mut temperature = self.initial_temp;

        // Build reverse mapping: location -> cell_id
        let mut location_to_cell: HashMap<(u32, u32, usize), CellId> = HashMap::new();
        for (&cell_id, loc) in &current.placements {
            location_to_cell.insert((loc.tile_x, loc.tile_y, loc.bel_index), cell_id);
        }

        let cells: Vec<CellId> = current.placements.keys().copied().collect();
        let batch_size = self.parallel_batch_size;
        let batches_per_temp = (cells.len().max(100) / batch_size).max(1);

        // Thread-safe counter for accepted moves (for statistics)
        let accepted_count = AtomicUsize::new(0);

        for _iteration in 0..self.max_iterations {
            // Process batches at this temperature
            for _ in 0..batches_per_temp {
                // Generate a batch of candidate moves using different seeds
                let moves: Vec<Move> = (0..batch_size)
                    .into_par_iter()
                    .map(|i| {
                        let mut rng = rand::rngs::StdRng::seed_from_u64(
                            42 + _iteration as u64 * 1000 + i as u64,
                        );
                        if self.timing_weight > 0.0 && rng.gen::<f64>() < 0.3 {
                            self.generate_critical_path_move(
                                &current,
                                &cells,
                                netlist,
                                &net_criticalities,
                                &mut rng,
                            )
                        } else {
                            self.generate_move(&current, &cells, &mut rng)
                        }
                    })
                    .collect();

                // Evaluate all moves in parallel
                #[allow(clippy::type_complexity)]
                let evaluated: Vec<(
                    Move,
                    PlacementResult,
                    HashMap<(u32, u32, usize), CellId>,
                    f64,
                )> = moves
                    .into_par_iter()
                    .map(|move_op| {
                        let (new_placement, new_loc_map) =
                            self.apply_move(&current, &location_to_cell, &move_op);
                        let new_cost = self.calculate_combined_cost(
                            &new_placement,
                            netlist,
                            &net_criticalities,
                        );
                        (move_op, new_placement, new_loc_map, new_cost)
                    })
                    .collect();

                // Find the best improving move (or best move if temperature allows)
                let mut best_move_idx: Option<usize> = None;
                let mut best_delta = f64::MAX;
                let mut rng = rand::rngs::StdRng::seed_from_u64(42 + _iteration as u64);

                for (idx, (_, _, _, new_cost)) in evaluated.iter().enumerate() {
                    let delta = new_cost - current_cost;

                    // Check acceptance
                    let accept = if delta <= 0.0 {
                        true
                    } else {
                        let probability = (-delta / temperature).exp();
                        rng.gen::<f64>() < probability
                    };

                    if accept && delta < best_delta {
                        best_move_idx = Some(idx);
                        best_delta = delta;
                    }
                }

                // Apply the best accepted move
                if let Some(idx) = best_move_idx {
                    let (_, new_placement, new_loc_map, new_cost) =
                        evaluated.into_iter().nth(idx).unwrap();
                    current = new_placement;
                    location_to_cell = new_loc_map;
                    current_cost = new_cost;
                    accepted_count.fetch_add(1, Ordering::Relaxed);

                    if current_cost < best_cost {
                        best = current.clone();
                        best_cost = current_cost;
                    }
                }
            }

            // Cool down
            temperature *= self.cooling_rate;

            // Early termination if temperature is too low
            if temperature < 0.01 {
                break;
            }
        }

        // Update metrics
        let mut result = best;
        result.cost = best_cost;

        // Calculate timing score for timing-driven mode
        if self.timing_weight > 0.0 {
            result.timing_score = self.estimate_timing_score(&result, netlist);
        }

        Ok(result)
    }

    /// Generate a random move
    fn generate_move<R: Rng>(
        &self,
        placement: &PlacementResult,
        cells: &[CellId],
        rng: &mut R,
    ) -> Move {
        if cells.len() < 2 {
            // Can't swap with only one cell
            return Move::Relocate(cells[0], placement.placements[&cells[0]]);
        }

        // Choose move type: 70% swap, 30% relocate
        if rng.gen::<f64>() < 0.7 {
            // Swap two cells of the same BEL type
            let idx1 = rng.gen_range(0..cells.len());
            let cell1 = cells[idx1];
            let loc1 = &placement.placements[&cell1];

            // Find another cell with same BEL type
            let candidates: Vec<_> = cells
                .iter()
                .filter(|&&c| c != cell1 && placement.placements[&c].bel_type == loc1.bel_type)
                .collect();

            if let Some(&&cell2) = candidates.get(rng.gen_range(0..candidates.len().max(1))) {
                Move::Swap(cell1, cell2)
            } else {
                // No compatible cell found, do relocate instead
                self.generate_relocate_move(placement, cell1, rng)
            }
        } else {
            // Relocate a random cell
            let idx = rng.gen_range(0..cells.len());
            self.generate_relocate_move(placement, cells[idx], rng)
        }
    }

    /// Generate a relocate move
    fn generate_relocate_move<R: Rng>(
        &self,
        placement: &PlacementResult,
        cell_id: CellId,
        rng: &mut R,
    ) -> Move {
        let current_loc = &placement.placements[&cell_id];
        let (width, height) = self.device.grid_size();

        // Random displacement (nearby)
        let max_dist = (width.max(height) / 4).max(2);
        let dx = rng.gen_range(-(max_dist as i32)..=(max_dist as i32));
        let dy = rng.gen_range(-(max_dist as i32)..=(max_dist as i32));

        let new_x = (current_loc.tile_x as i32 + dx).clamp(0, (width - 1) as i32) as u32;
        let new_y = (current_loc.tile_y as i32 + dy).clamp(0, (height - 1) as i32) as u32;

        // Find a valid BEL at the new location
        if let Some(tile) = self.device.tile_at(new_x, new_y) {
            for (bel_idx, bel) in tile.bels().iter().enumerate() {
                if Self::bel_types_compatible(bel.bel_type, current_loc.bel_type) {
                    // Keep the cell's original bel_type (requirement) not the BEL's type
                    return Move::Relocate(
                        cell_id,
                        PlacementLoc::new(new_x, new_y, bel_idx, current_loc.bel_type),
                    );
                }
            }
        }

        // If no valid BEL found, stay in place
        Move::Relocate(cell_id, *current_loc)
    }

    /// Apply a move to create a new placement
    fn apply_move(
        &self,
        placement: &PlacementResult,
        loc_map: &HashMap<(u32, u32, usize), CellId>,
        move_op: &Move,
    ) -> (PlacementResult, HashMap<(u32, u32, usize), CellId>) {
        let mut new_placement = placement.clone();
        let mut new_loc_map = loc_map.clone();

        match move_op {
            Move::Swap(cell1, cell2) => {
                let loc1 = placement.placements[cell1];
                let loc2 = placement.placements[cell2];

                // Swap locations
                new_placement.placements.insert(*cell1, loc2);
                new_placement.placements.insert(*cell2, loc1);

                // Update location map
                new_loc_map.insert((loc1.tile_x, loc1.tile_y, loc1.bel_index), *cell2);
                new_loc_map.insert((loc2.tile_x, loc2.tile_y, loc2.bel_index), *cell1);
            }
            Move::Relocate(cell_id, new_loc) => {
                let old_loc = placement.placements[cell_id];

                // Check if new location is occupied
                let new_key = (new_loc.tile_x, new_loc.tile_y, new_loc.bel_index);
                if let Some(&occupant) = loc_map.get(&new_key) {
                    if occupant != *cell_id {
                        // Swap with occupant
                        new_placement.placements.insert(*cell_id, *new_loc);
                        new_placement.placements.insert(occupant, old_loc);

                        let old_key = (old_loc.tile_x, old_loc.tile_y, old_loc.bel_index);
                        new_loc_map.insert(new_key, *cell_id);
                        new_loc_map.insert(old_key, occupant);
                    }
                } else {
                    // Simple move to empty location
                    new_placement.placements.insert(*cell_id, *new_loc);

                    let old_key = (old_loc.tile_x, old_loc.tile_y, old_loc.bel_index);
                    new_loc_map.remove(&old_key);
                    new_loc_map.insert(new_key, *cell_id);
                }
            }
        }

        (new_placement, new_loc_map)
    }

    /// Calculate placement cost (HPWL-based)
    fn calculate_cost(&self, placement: &PlacementResult, netlist: &GateNetlist) -> f64 {
        let mut total_hpwl = 0.0;

        for net in &netlist.nets {
            let mut min_x = f64::MAX;
            let mut max_x = f64::MIN;
            let mut min_y = f64::MAX;
            let mut max_y = f64::MIN;
            let mut connected = 0;

            // Driver
            if let Some(driver) = net.driver {
                if let Some(loc) = placement.placements.get(&driver) {
                    min_x = min_x.min(loc.tile_x as f64);
                    max_x = max_x.max(loc.tile_x as f64);
                    min_y = min_y.min(loc.tile_y as f64);
                    max_y = max_y.max(loc.tile_y as f64);
                    connected += 1;
                }
            }

            // Fanout
            for (cell_id, _) in &net.fanout {
                if let Some(loc) = placement.placements.get(cell_id) {
                    min_x = min_x.min(loc.tile_x as f64);
                    max_x = max_x.max(loc.tile_x as f64);
                    min_y = min_y.min(loc.tile_y as f64);
                    max_y = max_y.max(loc.tile_y as f64);
                    connected += 1;
                }
            }

            // HPWL with net weighting
            if connected >= 2 {
                let hpwl = (max_x - min_x) + (max_y - min_y);
                // Weight by net fanout (high fanout nets matter more)
                let weight = (connected as f64).sqrt();
                total_hpwl += hpwl * weight;
            }
        }

        total_hpwl
    }

    /// Calculate combined cost with timing weight
    fn calculate_combined_cost(
        &self,
        placement: &PlacementResult,
        netlist: &GateNetlist,
        net_criticalities: &HashMap<GateNetId, f64>,
    ) -> f64 {
        // Wirelength cost (HPWL)
        let wl_cost = self.calculate_cost(placement, netlist);

        if self.timing_weight == 0.0 {
            return wl_cost;
        }

        // Timing cost based on critical path delays
        let timing_cost = self.calculate_timing_cost(placement, netlist, net_criticalities);

        // Combine costs
        (1.0 - self.timing_weight) * wl_cost + self.timing_weight * timing_cost
    }

    /// Calculate timing cost based on estimated path delays
    fn calculate_timing_cost(
        &self,
        placement: &PlacementResult,
        netlist: &GateNetlist,
        net_criticalities: &HashMap<GateNetId, f64>,
    ) -> f64 {
        let mut total_timing_cost = 0.0;

        for net in &netlist.nets {
            let criticality = net_criticalities.get(&net.id).copied().unwrap_or(0.0);

            // Skip non-critical nets (< 0.1 criticality)
            if criticality < 0.1 {
                continue;
            }

            // Calculate estimated delay for this net based on placement
            let net_delay = self.estimate_net_delay(net.id, placement, netlist);

            // Weight by criticality (critical paths contribute more to cost)
            total_timing_cost += net_delay * criticality * criticality; // Square for emphasis
        }

        total_timing_cost
    }

    /// Estimate delay for a single net based on current placement
    fn estimate_net_delay(
        &self,
        net_id: GateNetId,
        placement: &PlacementResult,
        netlist: &GateNetlist,
    ) -> f64 {
        let net = match netlist.nets.get(net_id.0 as usize) {
            Some(n) => n,
            None => return 0.0,
        };

        // Get driver location
        let driver_loc = match net.driver.and_then(|d| placement.placements.get(&d)) {
            Some(loc) => loc,
            None => return 0.0,
        };

        // Calculate worst-case delay to any sink
        let mut max_delay: f64 = 0.0;

        for (sink_id, _) in &net.fanout {
            if let Some(sink_loc) = placement.placements.get(sink_id) {
                // Manhattan distance
                let dx = (driver_loc.tile_x as i32 - sink_loc.tile_x as i32).unsigned_abs();
                let dy = (driver_loc.tile_y as i32 - sink_loc.tile_y as i32).unsigned_abs();
                let distance = dx + dy;

                // Estimate routing delay based on distance
                let routing_delay = self.delay_model.estimated_path_delay(distance);

                // Add cell delay for the driver
                let cell_delay = if let Some(driver_id) = net.driver {
                    if let Some(cell) = netlist.get_cell(driver_id) {
                        self.delay_model.cell_delay(&cell.cell_type)
                    } else {
                        self.delay_model.lut4_delay
                    }
                } else {
                    0.0
                };

                let total_delay = cell_delay + routing_delay;
                max_delay = max_delay.max(total_delay);
            }
        }

        max_delay
    }

    /// Compute net criticalities based on topology and path depth
    fn compute_net_criticalities(
        &self,
        netlist: &GateNetlist,
        placement: &PlacementResult,
    ) -> HashMap<GateNetId, f64> {
        let mut criticalities: HashMap<GateNetId, f64> = HashMap::new();
        let mut path_delays: HashMap<GateNetId, f64> = HashMap::new();

        // Forward pass: compute arrival times (estimated delays to each net)
        for net in &netlist.nets {
            let delay = self.estimate_arrival_time(
                net.id,
                netlist,
                placement,
                &path_delays,
                &mut HashSet::new(),
            );
            path_delays.insert(net.id, delay);
        }

        // Find maximum delay (worst slack point)
        let max_delay = path_delays.values().copied().fold(0.0f64, f64::max);

        // Compute criticalities: 1.0 for critical path, 0.0 for slack
        if max_delay > 0.0 {
            for (net_id, delay) in &path_delays {
                let criticality = delay / max_delay;
                criticalities.insert(*net_id, criticality);
            }
        }

        criticalities
    }

    /// Estimate arrival time for a net (forward propagation)
    fn estimate_arrival_time(
        &self,
        net_id: GateNetId,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        memo: &HashMap<GateNetId, f64>,
        visited: &mut HashSet<GateNetId>,
    ) -> f64 {
        // Check memo
        if let Some(&delay) = memo.get(&net_id) {
            return delay;
        }

        // Prevent cycles
        if visited.contains(&net_id) {
            return 0.0;
        }
        visited.insert(net_id);

        let net = match netlist.nets.get(net_id.0 as usize) {
            Some(n) => n,
            None => return 0.0,
        };

        // If this is a primary input or clock, arrival time is 0
        if net.is_input || net.is_clock {
            return 0.0;
        }

        // Get the driver cell
        let driver_id = match net.driver {
            Some(id) => id,
            None => return 0.0,
        };

        let driver = match netlist.get_cell(driver_id) {
            Some(c) => c,
            None => return 0.0,
        };

        // If driver is sequential, this starts a new timing path
        if driver.clock.is_some() {
            return self.delay_model.dff_clk_to_q;
        }

        // Driver is combinational - get max input arrival + cell delay
        let cell_delay = self.delay_model.cell_delay(&driver.cell_type);
        let mut max_input_arrival = 0.0f64;

        for &input_net in &driver.inputs {
            let input_arrival =
                self.estimate_arrival_time(input_net, netlist, placement, memo, visited);
            max_input_arrival = max_input_arrival.max(input_arrival);
        }

        // Add routing delay estimate based on placement
        let routing_delay = self.estimate_net_delay(net_id, placement, netlist);

        max_input_arrival + cell_delay + routing_delay * 0.5 // Partial routing delay
    }

    /// Generate a move biased toward cells on critical paths
    fn generate_critical_path_move<R: Rng>(
        &self,
        placement: &PlacementResult,
        cells: &[CellId],
        netlist: &GateNetlist,
        net_criticalities: &HashMap<GateNetId, f64>,
        rng: &mut R,
    ) -> Move {
        // Find cells on critical paths (connected to high-criticality nets)
        let mut critical_cells: Vec<(CellId, f64)> = Vec::new();

        for &cell_id in cells {
            if let Some(cell) = netlist.get_cell(cell_id) {
                let mut max_criticality = 0.0f64;

                // Check output criticality
                for &output in &cell.outputs {
                    if let Some(&crit) = net_criticalities.get(&output) {
                        max_criticality = max_criticality.max(crit);
                    }
                }

                // Check input criticalities
                for &input_net in &cell.inputs {
                    if let Some(&crit) = net_criticalities.get(&input_net) {
                        max_criticality = max_criticality.max(crit);
                    }
                }

                if max_criticality > 0.5 {
                    critical_cells.push((cell_id, max_criticality));
                }
            }
        }

        // If no critical cells, fall back to normal move
        if critical_cells.is_empty() {
            return self.generate_move(placement, cells, rng);
        }

        // Sort by criticality and pick one of the most critical
        critical_cells.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
        let top_n = (critical_cells.len() / 4).max(1);
        let idx = rng.gen_range(0..top_n);
        let (cell_id, _) = critical_cells[idx];

        // Generate a move for this critical cell
        self.generate_relocate_move(placement, cell_id, rng)
    }

    /// Estimate timing score for final placement (worst slack)
    fn estimate_timing_score(&self, placement: &PlacementResult, netlist: &GateNetlist) -> f64 {
        let criticalities = self.compute_net_criticalities(netlist, placement);

        // Sum of critical path delays
        let mut max_delay = 0.0f64;
        for net in &netlist.nets {
            let delay = self.estimate_net_delay(net.id, placement, netlist);
            let criticality = criticalities.get(&net.id).copied().unwrap_or(0.0);
            if criticality > 0.5 {
                max_delay = max_delay.max(delay);
            }
        }

        max_delay
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
}
