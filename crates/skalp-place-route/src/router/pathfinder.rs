//! PathFinder Negotiated Congestion Router
//!
//! Implements the PathFinder algorithm for FPGA routing with congestion negotiation.
//! The algorithm:
//! 1. Route all nets (potentially with conflicts)
//! 2. Track congestion per wire
//! 3. Rip up congested nets and reroute with increased costs
//! 4. Iterate until no congestion

use super::astar::AStarRouter;
use super::{Route, RoutingResult};
use crate::device::{Device, WireId};
use crate::error::{PlaceRouteError, Result};
use crate::placer::PlacementResult;
use skalp_lir::gate_netlist::{GateNetId, GateNetlist};
use std::collections::{HashMap, HashSet};

/// Wire congestion tracking
#[allow(dead_code)]
struct CongestionTracker {
    /// Current usage count per wire
    usage: HashMap<WireId, usize>,
    /// Historical cost per wire (increased when congested)
    history_cost: HashMap<WireId, f64>,
    /// Wire capacity (typically 1 for most FPGA wires)
    capacity: HashMap<WireId, usize>,
}

#[allow(dead_code)]
impl CongestionTracker {
    fn new() -> Self {
        Self {
            usage: HashMap::new(),
            history_cost: HashMap::new(),
            capacity: HashMap::new(),
        }
    }

    /// Add usage for a wire
    fn add_usage(&mut self, wire: WireId) {
        *self.usage.entry(wire).or_insert(0) += 1;
        // Default capacity is 1
        self.capacity.entry(wire).or_insert(1);
    }

    /// Remove usage for a wire
    fn remove_usage(&mut self, wire: WireId) {
        if let Some(count) = self.usage.get_mut(&wire) {
            *count = count.saturating_sub(1);
        }
    }

    /// Get current congestion level for a wire
    fn congestion(&self, wire: WireId) -> f64 {
        let usage = self.usage.get(&wire).copied().unwrap_or(0);
        let capacity = self.capacity.get(&wire).copied().unwrap_or(1);
        usage as f64 / capacity as f64
    }

    /// Get cost factor for a wire (for routing)
    fn cost_factor(&self, wire: WireId, present_factor: f64, history_factor: f64) -> f64 {
        let usage = self.usage.get(&wire).copied().unwrap_or(0);
        let capacity = self.capacity.get(&wire).copied().unwrap_or(1);
        let history = self.history_cost.get(&wire).copied().unwrap_or(0.0);

        // PathFinder cost function
        let present_cost = if usage >= capacity {
            present_factor * (1.0 + usage as f64 - capacity as f64)
        } else {
            1.0
        };

        1.0 + present_cost + history_factor * history
    }

    /// Update history costs for congested wires
    fn update_history(&mut self) {
        for (&wire, &usage) in &self.usage {
            let capacity = self.capacity.get(&wire).copied().unwrap_or(1);
            if usage > capacity {
                // Increase history cost for overused wires
                *self.history_cost.entry(wire).or_insert(0.0) += (usage - capacity) as f64;
            }
        }
    }

    /// Get maximum congestion
    fn max_congestion(&self) -> f64 {
        self.usage
            .iter()
            .map(|(&wire, &usage)| {
                let capacity = self.capacity.get(&wire).copied().unwrap_or(1);
                usage as f64 / capacity as f64
            })
            .fold(0.0, f64::max)
    }

    /// Check if any wire is overused
    fn has_overuse(&self) -> bool {
        self.usage.iter().any(|(&wire, &usage)| {
            let capacity = self.capacity.get(&wire).copied().unwrap_or(1);
            usage > capacity
        })
    }

    /// Get overused wires
    fn overused_wires(&self) -> Vec<WireId> {
        self.usage
            .iter()
            .filter_map(|(&wire, &usage)| {
                let capacity = self.capacity.get(&wire).copied().unwrap_or(1);
                if usage > capacity {
                    Some(wire)
                } else {
                    None
                }
            })
            .collect()
    }

    /// Clear usage (keep history)
    fn clear_usage(&mut self) {
        self.usage.clear();
    }
}

/// PathFinder router
pub struct PathFinder<'a, D: Device> {
    device: &'a D,
    max_iterations: usize,
    history_factor: f64,
    present_factor: f64,
}

impl<'a, D: Device> PathFinder<'a, D> {
    /// Create a new PathFinder router
    pub fn new(
        device: &'a D,
        max_iterations: usize,
        history_factor: f64,
        present_factor: f64,
    ) -> Self {
        Self {
            device,
            max_iterations,
            history_factor,
            present_factor,
        }
    }

    /// Route all nets using PathFinder
    pub fn route(
        &self,
        netlist: &GateNetlist,
        placement: &PlacementResult,
    ) -> Result<RoutingResult> {
        let mut result = RoutingResult::new();
        let mut congestion = CongestionTracker::new();
        let astar = AStarRouter::new(self.device);

        // Collect nets to route
        let nets_to_route: Vec<_> = netlist
            .nets
            .iter()
            .filter(|net| net.driver.is_some() && !net.fanout.is_empty())
            .map(|net| net.id)
            .collect();

        // Initial routing
        for &net_id in &nets_to_route {
            let route = self.route_net(net_id, netlist, placement, &congestion, &astar)?;

            // Track wire usage
            for &wire in &route.wires {
                congestion.add_usage(wire);
            }

            result.routes.insert(net_id, route);
        }

        // Iterative rip-up and reroute
        for iteration in 0..self.max_iterations {
            if !congestion.has_overuse() {
                result.iterations = iteration;
                break;
            }

            // Find nets using congested wires
            let overused = congestion.overused_wires();
            let overused_set: HashSet<_> = overused.iter().copied().collect();

            let congested_nets: Vec<_> = result
                .routes
                .iter()
                .filter(|(_, route)| route.wires.iter().any(|w| overused_set.contains(w)))
                .map(|(&id, _)| id)
                .collect();

            // Rip up congested nets
            for &net_id in &congested_nets {
                if let Some(route) = result.routes.get(&net_id) {
                    for &wire in &route.wires {
                        congestion.remove_usage(wire);
                    }
                }
            }

            // Update history costs
            congestion.update_history();

            // Reroute with updated costs
            for &net_id in &congested_nets {
                let route =
                    self.route_net_with_costs(net_id, netlist, placement, &congestion, &astar)?;

                // Track wire usage
                for &wire in &route.wires {
                    congestion.add_usage(wire);
                }

                result.routes.insert(net_id, route);
            }

            result.iterations = iteration + 1;
        }

        // Calculate final metrics
        result.congestion = congestion.max_congestion();
        result.wirelength = result.routes.values().map(|r| r.wires.len() as u64).sum();
        result.success = !congestion.has_overuse();

        Ok(result)
    }

    /// Route a single net
    fn route_net(
        &self,
        net_id: GateNetId,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        _congestion: &CongestionTracker,
        astar: &AStarRouter<D>,
    ) -> Result<Route> {
        let net = &netlist.nets[net_id.0 as usize];
        let mut route = Route::new(net_id);

        // Get source location
        let driver_id = net.driver.ok_or_else(|| {
            PlaceRouteError::RoutingFailed(format!("Net {:?} has no driver", net_id))
        })?;

        let source_loc = placement.get(driver_id).ok_or_else(|| {
            PlaceRouteError::RoutingFailed(format!("Driver {:?} not placed", driver_id))
        })?;

        // Get source wire
        let source_wires = self.device.tile_wires(source_loc.tile_x, source_loc.tile_y);
        let source_wire = source_wires.first().copied().ok_or_else(|| {
            PlaceRouteError::RoutingFailed(format!(
                "No wires in tile ({}, {})",
                source_loc.tile_x, source_loc.tile_y
            ))
        })?;

        route.source = source_wire;

        // Route to each sink
        for (sink_id, _pin) in &net.fanout {
            let sink_loc = match placement.get(*sink_id) {
                Some(loc) => loc,
                None => continue,
            };

            let sink_wires = self.device.tile_wires(sink_loc.tile_x, sink_loc.tile_y);
            let sink_wire = match sink_wires.first().copied() {
                Some(w) => w,
                None => continue,
            };

            route.sinks.push(sink_wire);

            // Find path using A*
            match astar.find_path(source_wire, sink_wire, &[]) {
                Ok((wires, pips)) => {
                    // Add unique wires
                    for wire in wires {
                        if !route.wires.contains(&wire) {
                            route.wires.push(wire);
                        }
                    }
                    route.pips.extend(pips);

                    // Calculate delay
                    for &pip_id in &route.pips {
                        if let Some(pip) = self.device.pip(pip_id) {
                            route.delay += pip.delay;
                        }
                    }
                }
                Err(_) => {
                    // Failed to route this sink - continue trying others
                }
            }
        }

        Ok(route)
    }

    /// Route a net with congestion costs
    fn route_net_with_costs(
        &self,
        net_id: GateNetId,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        congestion: &CongestionTracker,
        astar: &AStarRouter<D>,
    ) -> Result<Route> {
        let net = &netlist.nets[net_id.0 as usize];
        let mut route = Route::new(net_id);

        // Get source location
        let driver_id = net.driver.ok_or_else(|| {
            PlaceRouteError::RoutingFailed(format!("Net {:?} has no driver", net_id))
        })?;

        let source_loc = placement.get(driver_id).ok_or_else(|| {
            PlaceRouteError::RoutingFailed(format!("Driver {:?} not placed", driver_id))
        })?;

        // Get source wire
        let source_wires = self.device.tile_wires(source_loc.tile_x, source_loc.tile_y);
        let source_wire = source_wires.first().copied().ok_or_else(|| {
            PlaceRouteError::RoutingFailed(format!(
                "No wires in tile ({}, {})",
                source_loc.tile_x, source_loc.tile_y
            ))
        })?;

        route.source = source_wire;

        // Build wire cost map from congestion
        let wire_costs: HashMap<WireId, f64> = congestion
            .usage
            .keys()
            .map(|&wire| {
                (
                    wire,
                    congestion.cost_factor(wire, self.present_factor, self.history_factor),
                )
            })
            .collect();

        // Route to each sink
        for (sink_id, _pin) in &net.fanout {
            let sink_loc = match placement.get(*sink_id) {
                Some(loc) => loc,
                None => continue,
            };

            let sink_wires = self.device.tile_wires(sink_loc.tile_x, sink_loc.tile_y);
            let sink_wire = match sink_wires.first().copied() {
                Some(w) => w,
                None => continue,
            };

            route.sinks.push(sink_wire);

            // Find path using A* with costs
            if let Ok((wires, pips)) =
                astar.find_path_with_costs(source_wire, sink_wire, &[], &wire_costs)
            {
                for wire in wires {
                    if !route.wires.contains(&wire) {
                        route.wires.push(wire);
                    }
                }
                route.pips.extend(pips);

                for &pip_id in &route.pips {
                    if let Some(pip) = self.device.pip(pip_id) {
                        route.delay += pip.delay;
                    }
                }
            }
        }

        Ok(route)
    }
}
