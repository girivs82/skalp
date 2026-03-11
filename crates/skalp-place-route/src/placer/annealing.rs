//! Simulated Annealing Placement Optimization
//!
//! Refines placement using simulated annealing with HPWL cost function.
//! Supports timing-driven placement by incorporating estimated path delays.
//! Includes parallel move evaluation for improved performance on multi-core systems.

use super::{PlacementLoc, PlacementResult};
use crate::device::{BelType, Device};
use crate::error::Result;
use crate::packing::CarryChain;
use crate::timing::DelayModel;
use rand::{Rng, SeedableRng};
use rayon::prelude::*;
use skalp_lir::gate_netlist::{CellId, GateNetId, GateNetlist};
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicUsize, Ordering};

/// Incremental cost cache for O(affected_nets) cost updates instead of O(all_nets).
///
/// Pre-computes per-net HPWL and timing costs, and maintains a cell→nets index
/// so that move evaluation only recomputes costs for nets connected to moved cells.
struct NetCostCache {
    /// Per-net HPWL cost (already weighted by sqrt(fanout))
    net_wl_cost: Vec<f64>,
    /// Per-net timing cost (delay * criticality²)
    net_timing_cost: Vec<f64>,
    /// Total wirelength cost (sum of net_wl_cost)
    total_wl_cost: f64,
    /// Total timing cost (sum of net_timing_cost)
    total_timing_cost: f64,
    /// Cell → list of net indices that the cell participates in
    cell_to_nets: HashMap<CellId, Vec<usize>>,
}

impl NetCostCache {
    /// Build the cache from scratch for a given placement.
    fn build(
        placement: &PlacementResult,
        netlist: &GateNetlist,
        net_criticalities: &HashMap<GateNetId, f64>,
        delay_model: &DelayModel,
        timing_weight: f64,
    ) -> Self {
        let num_nets = netlist.nets.len();
        let mut net_wl_cost = vec![0.0f64; num_nets];
        let mut net_timing_cost = vec![0.0f64; num_nets];
        let mut cell_to_nets: HashMap<CellId, Vec<usize>> = HashMap::new();

        for (i, net) in netlist.nets.iter().enumerate() {
            // Build cell_to_nets index
            if let Some(driver) = net.driver {
                cell_to_nets.entry(driver).or_default().push(i);
            }
            for (cell_id, _) in &net.fanout {
                cell_to_nets.entry(*cell_id).or_default().push(i);
            }

            // Compute HPWL cost
            net_wl_cost[i] = Self::compute_net_wl(net, placement);

            // Compute timing cost
            if timing_weight > 0.0 {
                let criticality = net_criticalities
                    .get(&net.id)
                    .copied()
                    .unwrap_or(0.0);
                if criticality >= 0.1 {
                    let delay = Self::compute_net_delay(net, placement, delay_model);
                    net_timing_cost[i] = delay * criticality * criticality;
                }
            }
        }

        // Deduplicate net indices per cell (a cell can appear as both driver and fanout)
        for nets in cell_to_nets.values_mut() {
            nets.sort_unstable();
            nets.dedup();
        }

        let total_wl_cost = net_wl_cost.iter().sum();
        let total_timing_cost = net_timing_cost.iter().sum();

        Self {
            net_wl_cost,
            net_timing_cost,
            total_wl_cost,
            total_timing_cost,
            cell_to_nets,
        }
    }

    /// Get the combined cost using the same formula as calculate_combined_cost.
    fn combined_cost(&self, timing_weight: f64) -> f64 {
        if timing_weight == 0.0 {
            self.total_wl_cost
        } else {
            (1.0 - timing_weight) * self.total_wl_cost
                + timing_weight * self.total_timing_cost
        }
    }

    /// Collect the unique set of net indices affected by moving the given cells.
    fn affected_nets(&self, cells: &[CellId]) -> Vec<usize> {
        let mut nets: HashSet<usize> = HashSet::new();
        for cell in cells {
            if let Some(cell_nets) = self.cell_to_nets.get(cell) {
                nets.extend(cell_nets);
            }
        }
        nets.into_iter().collect()
    }

    /// Compute the cost delta for a new placement, updating only affected nets.
    /// Returns (delta, affected net indices with their new costs) for later commit.
    fn compute_delta(
        &self,
        affected: &[usize],
        new_placement: &PlacementResult,
        netlist: &GateNetlist,
        net_criticalities: &HashMap<GateNetId, f64>,
        delay_model: &DelayModel,
        timing_weight: f64,
    ) -> (f64, Vec<(usize, f64, f64)>) {
        let mut wl_delta = 0.0;
        let mut timing_delta = 0.0;
        let mut updates = Vec::with_capacity(affected.len());

        for &net_idx in affected {
            let net = &netlist.nets[net_idx];
            let new_wl = Self::compute_net_wl(net, new_placement);
            let new_timing = if timing_weight > 0.0 {
                let criticality = net_criticalities
                    .get(&net.id)
                    .copied()
                    .unwrap_or(0.0);
                if criticality >= 0.1 {
                    let delay = Self::compute_net_delay(net, new_placement, delay_model);
                    delay * criticality * criticality
                } else {
                    0.0
                }
            } else {
                0.0
            };

            wl_delta += new_wl - self.net_wl_cost[net_idx];
            timing_delta += new_timing - self.net_timing_cost[net_idx];
            updates.push((net_idx, new_wl, new_timing));
        }

        let delta = if timing_weight == 0.0 {
            wl_delta
        } else {
            (1.0 - timing_weight) * wl_delta + timing_weight * timing_delta
        };

        (delta, updates)
    }

    /// Apply cached updates after a move is accepted.
    fn apply_updates(&mut self, updates: &[(usize, f64, f64)]) {
        for &(net_idx, new_wl, new_timing) in updates {
            self.total_wl_cost += new_wl - self.net_wl_cost[net_idx];
            self.total_timing_cost += new_timing - self.net_timing_cost[net_idx];
            self.net_wl_cost[net_idx] = new_wl;
            self.net_timing_cost[net_idx] = new_timing;
        }
    }

    /// Compute HPWL cost for a single net (weighted by sqrt(fanout)).
    fn compute_net_wl(
        net: &skalp_lir::gate_netlist::GateNet,
        placement: &PlacementResult,
    ) -> f64 {
        let mut min_x = f64::MAX;
        let mut max_x = f64::MIN;
        let mut min_y = f64::MAX;
        let mut max_y = f64::MIN;
        let mut connected = 0u32;

        if let Some(driver) = net.driver {
            if let Some(loc) = placement.placements.get(&driver) {
                let x = loc.tile_x as f64;
                let y = loc.tile_y as f64;
                min_x = min_x.min(x);
                max_x = max_x.max(x);
                min_y = min_y.min(y);
                max_y = max_y.max(y);
                connected += 1;
            }
        }

        for (cell_id, _) in &net.fanout {
            if let Some(loc) = placement.placements.get(cell_id) {
                let x = loc.tile_x as f64;
                let y = loc.tile_y as f64;
                min_x = min_x.min(x);
                max_x = max_x.max(x);
                min_y = min_y.min(y);
                max_y = max_y.max(y);
                connected += 1;
            }
        }

        if connected >= 2 {
            let hpwl = (max_x - min_x) + (max_y - min_y);
            hpwl * (connected as f64).sqrt()
        } else {
            0.0
        }
    }

    /// Compute timing delay for a single net (max sink delay).
    fn compute_net_delay(
        net: &skalp_lir::gate_netlist::GateNet,
        placement: &PlacementResult,
        delay_model: &DelayModel,
    ) -> f64 {
        let driver_loc = match net.driver.and_then(|d| placement.placements.get(&d)) {
            Some(loc) => loc,
            None => return 0.0,
        };

        let mut max_delay: f64 = 0.0;
        for (sink_id, _) in &net.fanout {
            if let Some(sink_loc) = placement.placements.get(sink_id) {
                let dx = (driver_loc.tile_x as i32 - sink_loc.tile_x as i32).unsigned_abs();
                let dy = (driver_loc.tile_y as i32 - sink_loc.tile_y as i32).unsigned_abs();
                let distance = dx + dy;
                let routing_delay = delay_model.estimated_path_delay(distance);
                max_delay = max_delay.max(routing_delay);
            }
        }
        max_delay
    }
}

/// Move operation for simulated annealing
#[derive(Debug, Clone)]
enum Move {
    /// Swap two cells
    Swap(CellId, CellId),
    /// Move a cell to a new location
    Relocate(CellId, PlacementLoc),
    /// Relocate an entire carry chain to a new column/start position
    RelocateCarryChain(usize, u32, u32), // chain_id, new_x, new_start_y
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
    /// Carry chain membership: cell_id → chain_id
    carry_chain_membership: HashMap<CellId, usize>,
    /// Carry chains for chain-aware moves
    carry_chains: Vec<CarryChain>,
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
            carry_chain_membership: HashMap::new(),
            carry_chains: Vec::new(),
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
            carry_chain_membership: HashMap::new(),
            carry_chains: Vec::new(),
        }
    }

    /// Set carry chains for chain-aware moves
    pub fn with_carry_chains(mut self, carry_chains: Vec<CarryChain>) -> Self {
        for chain in &carry_chains {
            for &cell_id in &chain.cells {
                self.carry_chain_membership.insert(cell_id, chain.chain_id);
            }
        }
        self.carry_chains = carry_chains;
        self
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

        // Build incremental cost cache: O(nets) once, then O(affected) per move
        let mut cost_cache = NetCostCache::build(
            &current,
            netlist,
            &net_criticalities,
            &self.delay_model,
            self.timing_weight,
        );
        let mut current_cost = cost_cache.combined_cost(self.timing_weight);
        let mut best = current.clone();
        let mut best_cost = current_cost;

        // VPR-style initial temperature calibration:
        // Perform random moves and set T so that ~95% are accepted.
        let mut temperature = {
            let mut delta_sum = 0.0f64;
            let mut delta_count = 0u32;
            let mut cal_rng = rand::rngs::StdRng::seed_from_u64(12345);
            let cal_cells: Vec<CellId> = current.placements.keys().copied().collect();
            let cal_loc_map: HashMap<(u32, u32, usize), CellId> = current
                .placements
                .iter()
                .map(|(&cid, loc)| ((loc.tile_x, loc.tile_y, loc.bel_index), cid))
                .collect();

            for _ in 0..100.min(cal_cells.len() * 2) {
                let move_op = self.generate_move(&current, &cal_cells, &mut cal_rng);
                let (new_pl, _) = self.apply_move(&current, &cal_loc_map, &move_op);
                let affected = self.moved_cells_with_displaced(&move_op, &current, &cal_loc_map);
                let affected_nets = cost_cache.affected_nets(&affected);
                let (delta, _) = cost_cache.compute_delta(
                    &affected_nets,
                    &new_pl,
                    netlist,
                    &net_criticalities,
                    &self.delay_model,
                    self.timing_weight,
                );
                let delta = delta.abs();
                if delta > 0.0 {
                    delta_sum += delta;
                    delta_count += 1;
                }
            }

            if delta_count > 0 {
                let avg_delta = delta_sum / delta_count as f64;
                // T = -avg_delta / ln(acceptance_rate)
                // For 95% acceptance: ln(0.95) ≈ -0.0513
                avg_delta / 0.0513
            } else {
                self.initial_temp
            }
        };

        // Build reverse mapping: location -> cell_id
        let mut location_to_cell: HashMap<(u32, u32, usize), CellId> = HashMap::new();
        for (&cell_id, loc) in &current.placements {
            location_to_cell.insert((loc.tile_x, loc.tile_y, loc.bel_index), cell_id);
        }

        let cells: Vec<CellId> = current.placements.keys().copied().collect();
        let moves_per_temp = cells.len().max(100);

        // Scale iterations to design size — tiny designs converge quickly
        let effective_iterations = self.max_iterations.min(cells.len() * 100 + 100);

        for _iteration in 0..effective_iterations {
            // Track acceptance rate for convergence detection
            let mut accepted_moves = 0u32;
            let mut total_moves = 0u32;

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

                // Incremental cost delta: only recompute nets connected to moved cells
                let (new_placement, new_loc_map) =
                    self.apply_move(&current, &location_to_cell, &move_op);
                let affected = self.moved_cells_with_displaced(
                    &move_op,
                    &current,
                    &location_to_cell,
                );
                let affected_nets = cost_cache.affected_nets(&affected);
                let (delta, updates) = cost_cache.compute_delta(
                    &affected_nets,
                    &new_placement,
                    netlist,
                    &net_criticalities,
                    &self.delay_model,
                    self.timing_weight,
                );

                // Acceptance criterion
                let accept = if delta <= 0.0 {
                    true
                } else {
                    let probability = (-delta / temperature).exp();
                    rng.gen::<f64>() < probability
                };

                total_moves += 1;
                if accept {
                    accepted_moves += 1;
                    current = new_placement;
                    location_to_cell = new_loc_map;
                    cost_cache.apply_updates(&updates);
                    current_cost = cost_cache.combined_cost(self.timing_weight);

                    if current_cost < best_cost {
                        best = current.clone();
                        best_cost = current_cost;
                    }
                }
            }

            // Cool down
            temperature *= self.cooling_rate;

            // Early termination based on acceptance rate
            // When acceptance rate drops below 0.1%, the solution has frozen
            let acceptance_rate = if total_moves > 0 {
                accepted_moves as f64 / total_moves as f64
            } else {
                0.0
            };
            if acceptance_rate < 0.001 {
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

        // Build incremental cost cache
        let mut cost_cache = NetCostCache::build(
            &current,
            netlist,
            &net_criticalities,
            &self.delay_model,
            self.timing_weight,
        );
        let mut current_cost = cost_cache.combined_cost(self.timing_weight);
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

        // Scale iterations to design size — tiny designs converge quickly
        let effective_iterations = self.max_iterations.min(cells.len() * 100 + 100);

        // Thread-safe counter for accepted moves (for statistics)
        let accepted_count = AtomicUsize::new(0);

        for _iteration in 0..effective_iterations {
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

                // Evaluate all moves in parallel using incremental cost
                #[allow(clippy::type_complexity)]
                let evaluated: Vec<(
                    Move,
                    PlacementResult,
                    HashMap<(u32, u32, usize), CellId>,
                    f64,
                    Vec<(usize, f64, f64)>,
                )> = moves
                    .into_par_iter()
                    .map(|move_op| {
                        let (new_placement, new_loc_map) =
                            self.apply_move(&current, &location_to_cell, &move_op);
                        let affected = self.moved_cells_with_displaced(
                            &move_op,
                            &current,
                            &location_to_cell,
                        );
                        let affected_nets = cost_cache.affected_nets(&affected);
                        let (delta, updates) = cost_cache.compute_delta(
                            &affected_nets,
                            &new_placement,
                            netlist,
                            &net_criticalities,
                            &self.delay_model,
                            self.timing_weight,
                        );
                        (move_op, new_placement, new_loc_map, delta, updates)
                    })
                    .collect();

                // Find the best improving move (or best move if temperature allows)
                let mut best_move_idx: Option<usize> = None;
                let mut best_delta = f64::MAX;
                let mut rng = rand::rngs::StdRng::seed_from_u64(42 + _iteration as u64);

                for (idx, (_, _, _, delta, _)) in evaluated.iter().enumerate() {
                    // Check acceptance
                    let accept = if *delta <= 0.0 {
                        true
                    } else {
                        let probability = (-*delta / temperature).exp();
                        rng.gen::<f64>() < probability
                    };

                    if accept && *delta < best_delta {
                        best_move_idx = Some(idx);
                        best_delta = *delta;
                    }
                }

                // Apply the best accepted move
                if let Some(idx) = best_move_idx {
                    let (_, new_placement, new_loc_map, _, updates) =
                        evaluated.into_iter().nth(idx).unwrap();
                    current = new_placement;
                    location_to_cell = new_loc_map;
                    cost_cache.apply_updates(&updates);
                    current_cost = cost_cache.combined_cost(self.timing_weight);
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
            return Move::Relocate(cells[0], placement.placements[&cells[0]]);
        }

        // Select a random cell
        let idx = rng.gen_range(0..cells.len());
        let cell = cells[idx];

        // If this cell is in a carry chain, generate a chain move instead
        if let Some(&chain_id) = self.carry_chain_membership.get(&cell) {
            if rng.gen::<f64>() < 0.5 {
                // Move entire chain
                let (width, height) = self.device.grid_size();
                let new_x = rng.gen_range(1..width.saturating_sub(1).max(2));
                let new_y = rng.gen_range(1..height.saturating_sub(1).max(2));
                return Move::RelocateCarryChain(chain_id, new_x, new_y);
            }
            // Otherwise fall through to normal move (for non-chain cells)
        }

        // Choose move type: 70% swap, 30% relocate
        if rng.gen::<f64>() < 0.7 {
            let cell1 = cell;
            let loc1 = &placement.placements[&cell1];

            // Don't swap carry chain cells individually
            if self.carry_chain_membership.contains_key(&cell1) {
                return self.generate_relocate_move(placement, cell1, rng);
            }

            let candidates: Vec<_> = cells
                .iter()
                .filter(|&&c| {
                    c != cell1
                        && placement.placements[&c].bel_type == loc1.bel_type
                        && !self.carry_chain_membership.contains_key(&c)
                })
                .collect();

            if let Some(&&cell2) = candidates.get(rng.gen_range(0..candidates.len().max(1))) {
                Move::Swap(cell1, cell2)
            } else {
                self.generate_relocate_move(placement, cell1, rng)
            }
        } else {
            self.generate_relocate_move(placement, cell, rng)
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

    /// Get all cells affected by a move, including displaced occupants.
    fn moved_cells_with_displaced(
        &self,
        move_op: &Move,
        placement: &PlacementResult,
        loc_map: &HashMap<(u32, u32, usize), CellId>,
    ) -> Vec<CellId> {
        match move_op {
            Move::Swap(c1, c2) => vec![*c1, *c2],
            Move::Relocate(cell_id, new_loc) => {
                let new_key = (new_loc.tile_x, new_loc.tile_y, new_loc.bel_index);
                if let Some(&occupant) = loc_map.get(&new_key) {
                    if occupant != *cell_id {
                        return vec![*cell_id, occupant];
                    }
                }
                vec![*cell_id]
            }
            Move::RelocateCarryChain(chain_id, new_x, new_start_y) => {
                let mut cells = Vec::new();
                if let Some(chain) = self.carry_chains.get(*chain_id) {
                    let (_, height) = self.device.grid_size();
                    let chain_cell_set: HashSet<CellId> = chain.cells.iter().copied().collect();
                    for (i, &cell_id) in chain.cells.iter().enumerate() {
                        cells.push(cell_id);
                        // Check for displaced occupants
                        let tile_y = (*new_start_y + (i as u32 / 8))
                            .min(height.saturating_sub(2).max(1));
                        let bel_idx = i % 8;
                        let key = (*new_x, tile_y, bel_idx);
                        if let Some(&occupant) = loc_map.get(&key) {
                            if !chain_cell_set.contains(&occupant)
                                && placement.placements.contains_key(&occupant)
                            {
                                cells.push(occupant);
                            }
                        }
                    }
                }
                cells
            }
        }
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
                        // Swap with occupant, preserving each cell's bel_type
                        new_placement.placements.insert(*cell_id, *new_loc);
                        let occupant_bel_type = placement.placements[&occupant].bel_type;
                        let displaced_loc = PlacementLoc::new(
                            old_loc.tile_x,
                            old_loc.tile_y,
                            old_loc.bel_index,
                            occupant_bel_type,
                        );
                        new_placement.placements.insert(occupant, displaced_loc);

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
            Move::RelocateCarryChain(chain_id, new_x, new_start_y) => {
                // Move entire carry chain to new column/start position
                if let Some(chain) = self.carry_chains.get(*chain_id) {
                    let (_, height) = self.device.grid_size();
                    for (i, &cell_id) in chain.cells.iter().enumerate() {
                        if let Some(&old_loc) = placement.placements.get(&cell_id) {
                            let old_key = (old_loc.tile_x, old_loc.tile_y, old_loc.bel_index);
                            new_loc_map.remove(&old_key);

                            // Place consecutively: tile_y advances every 8 LCs
                            let lc_offset = i;
                            let tile_y = (*new_start_y + (lc_offset as u32 / 8))
                                .min(height.saturating_sub(2).max(1));
                            let bel_idx = lc_offset % 8;

                            let new_loc =
                                PlacementLoc::new(*new_x, tile_y, bel_idx, BelType::Carry);

                            let new_key = (new_loc.tile_x, new_loc.tile_y, new_loc.bel_index);
                            // If occupied by non-chain cell, displace it to the carry's old position
                            // but preserve the displaced cell's original bel_type
                            if let Some(&occupant) = loc_map.get(&new_key) {
                                if !chain.cells.contains(&occupant) {
                                    let occupant_bel_type = placement
                                        .placements
                                        .get(&occupant)
                                        .map(|loc| loc.bel_type)
                                        .unwrap_or(old_loc.bel_type);
                                    let displaced_loc = PlacementLoc::new(
                                        old_loc.tile_x,
                                        old_loc.tile_y,
                                        old_loc.bel_index,
                                        occupant_bel_type,
                                    );
                                    new_placement.placements.insert(occupant, displaced_loc);
                                    new_loc_map.insert(old_key, occupant);
                                }
                            }
                            new_placement.placements.insert(cell_id, new_loc);
                            new_loc_map.insert(new_key, cell_id);
                        }
                    }
                }
            }
        }

        (new_placement, new_loc_map)
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
