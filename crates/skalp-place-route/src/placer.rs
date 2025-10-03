//! Placement algorithms for FPGA designs

use crate::device::{Device, LogicTile};
use rayon::prelude::*;
use skalp_lir::{Gate, LirDesign};
use std::collections::HashMap;

/// Main placer struct with advanced algorithms
pub struct Placer {
    /// Placement configuration
    config: PlacerConfig,
    /// Target device
    device: Device,
    /// Current placement state
    placement: HashMap<String, (usize, usize)>,
    /// Random number generator seed
    rng_seed: u64,
}

/// Placement configuration
#[derive(Debug, Clone)]
pub struct PlacerConfig {
    /// Placement algorithm to use
    pub algorithm: PlacementAlgorithm,
    /// Maximum iterations for simulated annealing
    pub max_iterations: usize,
    /// Initial temperature
    pub initial_temperature: f64,
    /// Cooling rate
    pub cooling_rate: f64,
    /// Timing weight in cost function
    pub timing_weight: f64,
    /// Wirelength weight in cost function
    pub wirelength_weight: f64,
    /// Congestion weight in cost function
    pub congestion_weight: f64,
}

/// Available placement algorithms
#[derive(Debug, Clone)]
pub enum PlacementAlgorithm {
    /// Simple random placement
    Random,
    /// Simulated annealing
    SimulatedAnnealing,
    /// Analytical placement with quadratic optimization
    Analytical,
    /// Force-directed placement
    ForceDirected,
}

impl Default for PlacerConfig {
    fn default() -> Self {
        Self {
            algorithm: PlacementAlgorithm::SimulatedAnnealing,
            max_iterations: 10000,
            initial_temperature: 100.0,
            cooling_rate: 0.95,
            timing_weight: 0.6,
            wirelength_weight: 0.3,
            congestion_weight: 0.1,
        }
    }
}

impl Placer {
    /// Create a new placer
    pub fn new(config: PlacerConfig, device: Device) -> Self {
        Self {
            config,
            device,
            placement: HashMap::new(),
            rng_seed: 12345,
        }
    }

    /// Run placement algorithm
    pub fn place(&mut self, design: &LirDesign) -> Result<PlacementResult, PlacementError> {
        println!(
            "🔧 Running placement with algorithm: {:?}",
            self.config.algorithm
        );

        // Extract gates from all modules
        let mut all_gates = Vec::new();
        for module in &design.modules {
            all_gates.extend(module.gates.iter());
        }

        if all_gates.is_empty() {
            return Ok(PlacementResult {
                placements: HashMap::new(),
                cost: 0.0,
                wirelength: 0,
                timing_score: 0.0,
                utilization: 0.0,
            });
        }

        match self.config.algorithm {
            PlacementAlgorithm::Random => self.random_placement(&all_gates),
            PlacementAlgorithm::SimulatedAnnealing => {
                self.simulated_annealing_placement(&all_gates)
            }
            PlacementAlgorithm::Analytical => self.analytical_placement(&all_gates),
            PlacementAlgorithm::ForceDirected => self.force_directed_placement(&all_gates),
        }
    }

    /// Simple random placement
    fn random_placement(&mut self, gates: &[&Gate]) -> Result<PlacementResult, PlacementError> {
        let mut available_tiles: Vec<_> = self.device.logic_tiles.iter().collect();

        if gates.len() > available_tiles.len() {
            return Err(PlacementError::Failed(
                "Not enough logic tiles for design".to_string(),
            ));
        }

        // Simple random assignment
        for (i, gate) in gates.iter().enumerate() {
            if i < available_tiles.len() {
                let tile = available_tiles[i];
                self.placement.insert(gate.id.clone(), tile.position);
            }
        }

        Ok(self.create_placement_result(gates))
    }

    /// Simulated annealing placement
    fn simulated_annealing_placement(
        &mut self,
        gates: &[&Gate],
    ) -> Result<PlacementResult, PlacementError> {
        // Initial random placement
        self.random_placement(gates)?;

        let mut temperature = self.config.initial_temperature;
        let mut best_placement = self.placement.clone();
        let mut best_cost = self.calculate_cost(gates);

        println!("   Initial cost: {:.2}", best_cost);

        for iteration in 0..self.config.max_iterations {
            // Try a random move
            if let Some((gate_id, old_pos)) = self.select_random_gate_to_move(gates) {
                let new_pos = self.select_random_legal_position();

                // Make the move
                self.placement.insert(gate_id.clone(), new_pos);
                let new_cost = self.calculate_cost(gates);

                let delta_cost = new_cost - best_cost;

                // Accept or reject move based on simulated annealing criteria
                if delta_cost < 0.0 || self.accept_move(delta_cost, temperature) {
                    best_cost = new_cost;
                    best_placement = self.placement.clone();
                } else {
                    // Reject move - restore old position
                    self.placement.insert(gate_id, old_pos);
                }
            }

            // Cool down
            temperature *= self.config.cooling_rate;

            if iteration % 1000 == 0 {
                println!(
                    "   Iteration {}: cost = {:.2}, temp = {:.2}",
                    iteration, best_cost, temperature
                );
            }
        }

        self.placement = best_placement;
        println!("   Final cost: {:.2}", best_cost);

        Ok(self.create_placement_result(gates))
    }

    /// Analytical placement using quadratic optimization
    fn analytical_placement(&mut self, gates: &[&Gate]) -> Result<PlacementResult, PlacementError> {
        // Simplified analytical placement - would use actual quadratic solver
        println!("   Running analytical placement (simplified)");

        // For now, use a grid-based approach
        let grid_width = (gates.len() as f64).sqrt().ceil() as usize;

        for (i, gate) in gates.iter().enumerate() {
            let x = (i % grid_width) + 1;
            let y = (i / grid_width) + 1;

            // Ensure position is within device bounds
            let x = x.min(self.device.grid_size.0 - 2);
            let y = y.min(self.device.grid_size.1 - 2);

            self.placement.insert(gate.id.clone(), (x, y));
        }

        Ok(self.create_placement_result(gates))
    }

    /// Force-directed placement
    fn force_directed_placement(
        &mut self,
        gates: &[&Gate],
    ) -> Result<PlacementResult, PlacementError> {
        println!("   Running force-directed placement");

        // Initial grid placement
        self.analytical_placement(gates)?;

        // Apply force-directed iterations
        for iteration in 0..100 {
            let mut forces: HashMap<String, (f64, f64)> = HashMap::new();

            // Calculate forces between connected gates
            for gate in gates {
                let mut total_force = (0.0, 0.0);

                if let Some(&(x, y)) = self.placement.get(&gate.id) {
                    // Calculate attractive forces to connected gates
                    for input in &gate.inputs {
                        if let Some(&(ix, iy)) = self.placement.get(input) {
                            let dx = ix as f64 - x as f64;
                            let dy = iy as f64 - y as f64;
                            let distance = (dx * dx + dy * dy).sqrt().max(1.0);

                            // Attractive force proportional to distance
                            total_force.0 += dx / distance * 0.1;
                            total_force.1 += dy / distance * 0.1;
                        }
                    }
                }

                forces.insert(gate.id.clone(), total_force);
            }

            // Apply forces with damping
            for gate in gates {
                if let Some(&(fx, fy)) = forces.get(&gate.id) {
                    if let Some(&(x, y)) = self.placement.get(&gate.id) {
                        let new_x = ((x as f64 + fx * 0.5)
                            .max(1.0)
                            .min((self.device.grid_size.0 - 2) as f64))
                            as usize;
                        let new_y = ((y as f64 + fy * 0.5)
                            .max(1.0)
                            .min((self.device.grid_size.1 - 2) as f64))
                            as usize;

                        self.placement.insert(gate.id.clone(), (new_x, new_y));
                    }
                }
            }

            if iteration % 20 == 0 {
                let cost = self.calculate_cost(gates);
                println!("   Force iteration {}: cost = {:.2}", iteration, cost);
            }
        }

        Ok(self.create_placement_result(gates))
    }

    /// Calculate placement cost (wirelength + timing + congestion)
    fn calculate_cost(&self, gates: &[&Gate]) -> f64 {
        let wirelength = self.calculate_wirelength(gates) as f64;
        let timing_cost = self.calculate_timing_cost(gates);
        let congestion_cost = self.calculate_congestion_cost();

        self.config.wirelength_weight * wirelength
            + self.config.timing_weight * timing_cost
            + self.config.congestion_weight * congestion_cost
    }

    /// Calculate total wirelength
    fn calculate_wirelength(&self, gates: &[&Gate]) -> usize {
        let mut total_wirelength = 0;

        for gate in gates {
            if let Some(&(gx, gy)) = self.placement.get(&gate.id) {
                for input in &gate.inputs {
                    if let Some(&(ix, iy)) = self.placement.get(input) {
                        let dx = (gx as i32 - ix as i32).abs();
                        let dy = (gy as i32 - iy as i32).abs();
                        total_wirelength += (dx + dy) as usize; // Manhattan distance
                    }
                }
            }
        }

        total_wirelength
    }

    /// Calculate timing cost (simplified)
    fn calculate_timing_cost(&self, gates: &[&Gate]) -> f64 {
        // Simplified timing model - would use actual delay models
        let wirelength = self.calculate_wirelength(gates) as f64;
        wirelength * 0.1 // Assume 0.1ns per unit wirelength
    }

    /// Calculate congestion cost
    fn calculate_congestion_cost(&self) -> f64 {
        // Simplified congestion model
        let mut tile_usage: HashMap<(usize, usize), usize> = HashMap::new();

        for &position in self.placement.values() {
            *tile_usage.entry(position).or_insert(0) += 1;
        }

        // Penalize overused tiles
        tile_usage
            .values()
            .map(|&usage| {
                if usage > 1 {
                    (usage - 1) * (usage - 1)
                } else {
                    0
                }
            })
            .sum::<usize>() as f64
            * 10.0
    }

    /// Select a random gate to move
    fn select_random_gate_to_move(&self, gates: &[&Gate]) -> Option<(String, (usize, usize))> {
        if gates.is_empty() {
            return None;
        }

        let index = (self.rng_seed as usize) % gates.len();
        let gate_id = &gates[index].id;

        if let Some(&position) = self.placement.get(gate_id) {
            Some((gate_id.clone(), position))
        } else {
            None
        }
    }

    /// Select a random legal position
    fn select_random_legal_position(&mut self) -> (usize, usize) {
        // Simple random position within logic tile area
        self.rng_seed = self.rng_seed.wrapping_mul(1103515245).wrapping_add(12345);
        let x = (self.rng_seed as usize % (self.device.grid_size.0 - 2)) + 1;

        self.rng_seed = self.rng_seed.wrapping_mul(1103515245).wrapping_add(12345);
        let y = (self.rng_seed as usize % (self.device.grid_size.1 - 2)) + 1;

        (x, y)
    }

    /// Accept or reject a move based on simulated annealing
    fn accept_move(&mut self, delta_cost: f64, temperature: f64) -> bool {
        if temperature <= 0.0 {
            return false;
        }

        let probability = (-delta_cost / temperature).exp();

        self.rng_seed = self.rng_seed.wrapping_mul(1103515245).wrapping_add(12345);
        let random_value = (self.rng_seed as f64) / (u64::MAX as f64);

        random_value < probability
    }

    /// Create final placement result
    fn create_placement_result(&self, gates: &[&Gate]) -> PlacementResult {
        let wirelength = self.calculate_wirelength(gates);
        let timing_score = self.calculate_timing_cost(gates);
        let total_tiles_used = self.placement.len();
        let total_tiles_available = self.device.logic_tiles.len();
        let utilization = (total_tiles_used as f64) / (total_tiles_available as f64);

        PlacementResult {
            placements: self.placement.clone(),
            cost: self.calculate_cost(gates),
            wirelength,
            timing_score,
            utilization,
        }
    }
}

/// Placement result with detailed metrics
#[derive(Debug)]
pub struct PlacementResult {
    /// Cell placements (gate_id -> (x, y))
    pub placements: HashMap<String, (usize, usize)>,
    /// Total placement cost
    pub cost: f64,
    /// Total wirelength
    pub wirelength: usize,
    /// Timing score (critical path estimate)
    pub timing_score: f64,
    /// Device utilization (0.0 to 1.0)
    pub utilization: f64,
}

/// Placement errors
#[derive(Debug, thiserror::Error)]
pub enum PlacementError {
    #[error("Placement failed: {0}")]
    Failed(String),
    #[error("Not enough resources for design")]
    InsufficientResources,
    #[error("Invalid device configuration")]
    InvalidDevice,
}
