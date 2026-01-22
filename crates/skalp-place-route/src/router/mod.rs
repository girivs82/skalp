//! FPGA Routing Engine
//!
//! Implements routing algorithms for FPGA designs:
//! - A* shortest path routing
//! - PathFinder negotiated congestion routing
//! - Global clock/reset routing

mod astar;
mod carry_chain;
mod global_nets;
mod lut_permutation;
mod pathfinder;

pub use astar::AStarRouter;
pub use carry_chain::CarryChainRouter;
pub use global_nets::GlobalNetRouter;
pub use lut_permutation::{
    all_lut4_permutations, permute_lut4_init, LutPermutation, LutPermutationOptimizer,
};
pub use pathfinder::PathFinder;

use crate::device::{Device, PipId, WireId};
use crate::error::Result;
use crate::placer::PlacementResult;
use serde::{Deserialize, Serialize};
use skalp_lir::gate_netlist::{GateNetId, GateNetlist};
use std::collections::HashMap;

/// Routing algorithm selection
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum RoutingAlgorithm {
    /// PathFinder with A* (default, good quality)
    #[default]
    PathFinderAStar,
    /// Simple maze routing (fast but lower quality)
    MazeRouting,
    /// Timing-driven routing
    TimingDriven,
}

/// Router configuration
#[derive(Debug, Clone)]
pub struct RouterConfig {
    /// Routing algorithm
    pub algorithm: RoutingAlgorithm,
    /// Maximum routing iterations
    pub max_iterations: usize,
    /// Allow rip-up and reroute
    pub allow_ripup: bool,
    /// Maximum allowed congestion
    pub max_congestion: f64,
    /// History cost factor for PathFinder
    pub history_cost_factor: f64,
    /// Present congestion factor
    pub present_congestion_factor: f64,
    /// Timing weight for timing-driven routing
    pub timing_weight: f64,
}

impl Default for RouterConfig {
    fn default() -> Self {
        Self {
            algorithm: RoutingAlgorithm::PathFinderAStar,
            max_iterations: 100, // Increased for better convergence
            allow_ripup: true,
            max_congestion: 1.5,
            history_cost_factor: 1.0, // Increased for stronger congestion avoidance
            present_congestion_factor: 1.5, // Increased to penalize current congestion more
            timing_weight: 0.3,
        }
    }
}

/// A routed net
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Route {
    /// Net ID
    pub net_id: GateNetId,
    /// Source wire
    pub source: WireId,
    /// Sink wires
    pub sinks: Vec<WireId>,
    /// Wires used in the route
    pub wires: Vec<WireId>,
    /// PIPs (switches) enabled
    pub pips: Vec<PipId>,
    /// Total delay (ps)
    pub delay: u32,
}

impl Route {
    /// Create a new empty route
    pub fn new(net_id: GateNetId) -> Self {
        Self {
            net_id,
            source: WireId(0),
            sinks: Vec::new(),
            wires: Vec::new(),
            pips: Vec::new(),
            delay: 0,
        }
    }

    /// Get the wirelength of this route
    pub fn wirelength(&self) -> usize {
        self.wires.len()
    }
}

/// Routing result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RoutingResult {
    /// Routes for each net
    pub routes: HashMap<GateNetId, Route>,
    /// Maximum congestion
    pub congestion: f64,
    /// Total wirelength
    pub wirelength: u64,
    /// Number of routing iterations used
    pub iterations: usize,
    /// Whether all nets were successfully routed
    pub success: bool,
}

impl RoutingResult {
    /// Create a new empty routing result
    pub fn new() -> Self {
        Self {
            routes: HashMap::new(),
            congestion: 0.0,
            wirelength: 0,
            iterations: 0,
            success: true,
        }
    }
}

impl Default for RoutingResult {
    fn default() -> Self {
        Self::new()
    }
}

/// Main router struct
pub struct Router<D: Device> {
    /// Configuration
    config: RouterConfig,
    /// Target device
    device: D,
}

impl<D: Device + Clone> Router<D> {
    /// Create a new router
    pub fn new(config: RouterConfig, device: D) -> Self {
        Self { config, device }
    }

    /// Route a design using integrated specialized routers
    ///
    /// The routing flow:
    /// 1. Route global nets (clocks/resets) through dedicated GBUF network
    /// 2. Route carry chain nets through dedicated carry wires
    /// 3. Route remaining nets through PathFinder/A*
    pub fn route(
        &mut self,
        netlist: &GateNetlist,
        placement: &PlacementResult,
    ) -> Result<RoutingResult> {
        let mut result = RoutingResult::new();

        // Phase 1: Route global nets (clocks and resets) through dedicated resources
        let mut global_router = GlobalNetRouter::new(&self.device);
        let carry_router = CarryChainRouter::new(&self.device);

        let mut global_net_ids = Vec::new();
        let mut carry_net_ids = Vec::new();
        let mut regular_net_ids = Vec::new();

        // Classify nets
        for net in &netlist.nets {
            if net.driver.is_none() || net.fanout.is_empty() {
                continue;
            }

            if global_router.is_global_net(net) {
                global_net_ids.push(net.id);
            } else if carry_router.is_carry_net(net, netlist) {
                carry_net_ids.push(net.id);
            } else {
                regular_net_ids.push(net.id);
            }
        }

        // Route global nets through GBUF network
        for net_id in &global_net_ids {
            match global_router.route_clock(*net_id, netlist, placement) {
                Ok(route) => {
                    result.wirelength += route.wires.len() as u64;
                    result.routes.insert(*net_id, route);
                }
                Err(_) => {
                    // Fall back to regular routing if global routing fails
                    regular_net_ids.push(*net_id);
                }
            }
        }

        // Route carry chain nets through dedicated wires
        for net_id in &carry_net_ids {
            match carry_router.route_carry(*net_id, netlist, placement) {
                Ok(route) => {
                    result.wirelength += route.wires.len() as u64;
                    result.routes.insert(*net_id, route);
                }
                Err(_) => {
                    // Fall back to regular routing if carry routing fails
                    regular_net_ids.push(*net_id);
                }
            }
        }

        // Phase 2: Route remaining nets through PathFinder
        let pathfinder_result = match self.config.algorithm {
            RoutingAlgorithm::PathFinderAStar => {
                let pathfinder = PathFinder::new(
                    &self.device,
                    self.config.max_iterations,
                    self.config.history_cost_factor,
                    self.config.present_congestion_factor,
                );
                pathfinder.route(netlist, placement)
            }
            RoutingAlgorithm::MazeRouting => {
                let astar = AStarRouter::new(&self.device);
                self.simple_astar_route(&astar, netlist, placement)
            }
            RoutingAlgorithm::TimingDriven => {
                let pathfinder = PathFinder::new_timing_driven(
                    &self.device,
                    self.config.max_iterations,
                    self.config.history_cost_factor,
                    self.config.present_congestion_factor,
                    self.config.timing_weight,
                );
                pathfinder.route(netlist, placement)
            }
        }?;

        // Merge PathFinder results (only for non-global/carry nets)
        for (net_id, route) in pathfinder_result.routes {
            if let std::collections::hash_map::Entry::Vacant(e) = result.routes.entry(net_id) {
                result.wirelength += route.wires.len() as u64;
                e.insert(route);
            }
        }

        result.congestion = pathfinder_result.congestion;
        result.iterations = pathfinder_result.iterations;
        result.success = pathfinder_result.success;

        Ok(result)
    }

    /// Simple A* routing (no congestion negotiation)
    fn simple_astar_route(
        &self,
        astar: &AStarRouter<D>,
        netlist: &GateNetlist,
        placement: &PlacementResult,
    ) -> Result<RoutingResult> {
        let mut result = RoutingResult::new();

        // Route each net
        for net in &netlist.nets {
            let net_id = net.id;
            // Skip undriven nets
            if net.driver.is_none() || net.fanout.is_empty() {
                continue;
            }

            // Get source location
            let driver_id = net.driver.unwrap();
            let source_loc = match placement.get(driver_id) {
                Some(loc) => loc,
                None => continue,
            };

            // Get sink locations
            let sinks: Vec<_> = net
                .fanout
                .iter()
                .filter_map(|(cell_id, _)| placement.get(*cell_id))
                .collect();

            if sinks.is_empty() {
                continue;
            }

            // Find source wire
            let source_wires = self.device.tile_wires(source_loc.tile_x, source_loc.tile_y);
            let source_wire = source_wires.first().copied().unwrap_or(WireId(0));

            // Create route
            let mut route = Route::new(net_id);
            route.source = source_wire;

            // Route to each sink
            for sink_loc in sinks {
                let sink_wires = self.device.tile_wires(sink_loc.tile_x, sink_loc.tile_y);
                if let Some(&sink_wire) = sink_wires.first() {
                    route.sinks.push(sink_wire);

                    // Find path
                    if let Ok((path_wires, path_pips)) =
                        astar.find_path(source_wire, sink_wire, &[])
                    {
                        route.wires.extend(path_wires);
                        route.pips.extend(path_pips);
                    }
                }
            }

            result.wirelength += route.wires.len() as u64;
            result.routes.insert(net_id, route);
        }

        result.success = true;
        Ok(result)
    }
}
