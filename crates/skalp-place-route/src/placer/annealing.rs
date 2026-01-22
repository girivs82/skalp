//! Simulated Annealing Placement Optimization
//!
//! Refines placement using simulated annealing with HPWL cost function.

use super::{PlacementLoc, PlacementResult};
use crate::device::Device;
use crate::error::Result;
use rand::{Rng, SeedableRng};
use skalp_lir::gate_netlist::{CellId, GateNetlist};
use std::collections::HashMap;

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
}

impl<'a, D: Device> SimulatedAnnealing<'a, D> {
    /// Create a new simulated annealing optimizer
    pub fn new(device: &'a D, initial_temp: f64, cooling_rate: f64, max_iterations: usize) -> Self {
        Self {
            device,
            initial_temp,
            cooling_rate,
            max_iterations,
        }
    }

    /// Optimize a placement using simulated annealing
    pub fn optimize(
        &self,
        initial: PlacementResult,
        netlist: &GateNetlist,
    ) -> Result<PlacementResult> {
        let mut rng = rand::rngs::StdRng::seed_from_u64(42);
        let mut current = initial;
        let mut current_cost = self.calculate_cost(&current, netlist);
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
                // Generate a random move
                let move_op = self.generate_move(&current, &cells, &mut rng);

                // Calculate cost delta
                let (new_placement, new_loc_map) =
                    self.apply_move(&current, &location_to_cell, &move_op);
                let new_cost = self.calculate_cost(&new_placement, netlist);
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
                if bel.bel_type == current_loc.bel_type {
                    return Move::Relocate(
                        cell_id,
                        PlacementLoc::new(new_x, new_y, bel_idx, bel.bel_type),
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
}
