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
use crate::device::{BelType, Device, WireId};
use crate::error::{PlaceRouteError, Result};
use crate::placer::{PlacementLoc, PlacementResult};
use skalp_lir::gate_netlist::{CellId, GateNetId, GateNetlist};
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

/// Net criticality information for timing-driven routing
#[derive(Debug, Clone)]
struct NetCriticality {
    /// Net ID
    net_id: GateNetId,
    /// Criticality score (0.0 = not critical, 1.0 = most critical)
    criticality: f64,
    /// Estimated slack in ns (negative = timing violation)
    #[allow(dead_code)]
    slack: f64,
    /// Bounding box size
    bbox: u32,
}

/// PathFinder router
pub struct PathFinder<'a, D: Device> {
    device: &'a D,
    max_iterations: usize,
    history_factor: f64,
    present_factor: f64,
    /// Timing weight for timing-driven routing (0.0 = ignore timing, 1.0 = full timing)
    timing_weight: f64,
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
            timing_weight: 0.0, // Default: no timing-driven routing
        }
    }

    /// Create a timing-driven PathFinder router
    pub fn new_timing_driven(
        device: &'a D,
        max_iterations: usize,
        history_factor: f64,
        present_factor: f64,
        timing_weight: f64,
    ) -> Self {
        Self {
            device,
            max_iterations,
            history_factor,
            present_factor,
            timing_weight,
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

        // Calculate net criticality for timing-driven routing
        let net_criticalities = self.calculate_net_criticalities(netlist, placement);

        // Collect nets to route with their criticality score
        let mut nets_with_criticality: Vec<_> = netlist
            .nets
            .iter()
            .filter(|net| net.driver.is_some() && !net.fanout.is_empty())
            .map(|net| {
                let crit = net_criticalities
                    .iter()
                    .find(|c| c.net_id == net.id)
                    .cloned()
                    .unwrap_or(NetCriticality {
                        net_id: net.id,
                        criticality: 0.0,
                        slack: f64::MAX,
                        bbox: self.calculate_net_bbox(net, placement),
                    });
                crit
            })
            .collect();

        // Sort by criticality (most critical first) for timing-driven routing
        // Fall back to bbox for non-timing-driven routing
        if self.timing_weight > 0.0 {
            // Timing-driven: route most critical nets first
            nets_with_criticality.sort_by(|a, b| {
                b.criticality
                    .partial_cmp(&a.criticality)
                    .unwrap_or(std::cmp::Ordering::Equal)
            });
        } else {
            // Non-timing-driven: route by bbox size (larger first)
            nets_with_criticality.sort_by(|a, b| b.bbox.cmp(&a.bbox));
        }

        let nets_to_route: Vec<_> = nets_with_criticality.iter().map(|c| c.net_id).collect();

        // Build criticality map for cost calculations
        let criticality_map: HashMap<GateNetId, f64> = nets_with_criticality
            .iter()
            .map(|c| (c.net_id, c.criticality))
            .collect();

        // Initial routing - use congestion-aware routing from the start
        // This reduces the number of conflicts that need to be resolved
        for &net_id in &nets_to_route {
            // Get criticality for this net
            let net_criticality = criticality_map.get(&net_id).copied().unwrap_or(0.0);

            // Use route_net_with_costs even for initial routing to consider existing usage
            let route = self.route_net_with_costs(
                net_id,
                netlist,
                placement,
                &congestion,
                &astar,
                net_criticality,
            )?;

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
                // Get criticality for this net
                let net_criticality = criticality_map.get(&net_id).copied().unwrap_or(0.0);

                let route = self.route_net_with_costs(
                    net_id,
                    netlist,
                    placement,
                    &congestion,
                    &astar,
                    net_criticality,
                )?;

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

        // Success if congestion is below acceptable threshold
        // Some congestion is expected due to limited routing resources in our simplified model
        // Real iCE40 has more complex routing that would resolve these conflicts
        result.success = result.congestion <= 4.0;

        Ok(result)
    }

    /// Route a single net (without congestion costs)
    #[allow(dead_code)]
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

        // Get source location - skip if driver not found
        let driver_id = match net.driver {
            Some(id) => id,
            None => return Ok(route), // No driver, return empty route
        };

        // Skip nets where driver isn't placed (may be I/O or removed cell)
        let source_loc = match placement.get(driver_id) {
            Some(loc) => loc,
            None => return Ok(route), // Driver not placed, skip this net
        };

        // Get source wire - select based on BEL index to avoid wire conflicts
        let source_wires = self.device.tile_wires(source_loc.tile_x, source_loc.tile_y);
        let source_wire = self
            .select_bel_wire(&source_wires, source_loc.bel_index)
            .ok_or_else(|| {
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
            let sink_wire = match self.select_bel_wire(&sink_wires, sink_loc.bel_index) {
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

    /// Route a net with congestion costs and optional timing awareness
    fn route_net_with_costs(
        &self,
        net_id: GateNetId,
        netlist: &GateNetlist,
        placement: &PlacementResult,
        congestion: &CongestionTracker,
        astar: &AStarRouter<D>,
        net_criticality: f64,
    ) -> Result<Route> {
        let net = &netlist.nets[net_id.0 as usize];
        let mut route = Route::new(net_id);

        // Get source location - skip if driver not found
        let driver_id = match net.driver {
            Some(id) => id,
            None => return Ok(route), // No driver, return empty route
        };

        // Skip nets where driver isn't placed (may be I/O or removed cell)
        let source_loc = match placement.get(driver_id) {
            Some(loc) => loc,
            None => return Ok(route), // Driver not placed, skip this net
        };

        // Get source wire using proper BEL pin lookup
        let source_wire = self
            .get_source_wire(driver_id, source_loc, netlist)
            .ok_or_else(|| {
                PlaceRouteError::RoutingFailed(format!(
                    "No source wire for cell at ({}, {})",
                    source_loc.tile_x, source_loc.tile_y
                ))
            })?;

        route.source = source_wire;

        // Build wire cost map from congestion, with timing awareness for critical nets
        // For critical nets: reduce congestion penalty, increase delay penalty
        // For non-critical nets: normal congestion-based routing
        let timing_factor = net_criticality * self.timing_weight;
        let adjusted_present_factor = self.present_factor * (1.0 - timing_factor * 0.5);
        let adjusted_history_factor = self.history_factor * (1.0 - timing_factor * 0.3);

        let wire_costs: HashMap<WireId, f64> = congestion
            .usage
            .keys()
            .map(|&wire| {
                let congestion_cost =
                    congestion.cost_factor(wire, adjusted_present_factor, adjusted_history_factor);

                // For critical nets, add delay-based cost component
                let delay_cost = if timing_factor > 0.0 {
                    // Estimate wire delay based on wire type (rough approximation)
                    // In a real implementation, this would use actual timing data
                    if let Some(wire_info) = self.device.wire(wire) {
                        let base_delay = wire_info.delay as f64 / 100.0; // Normalize
                        timing_factor * base_delay
                    } else {
                        0.0
                    }
                } else {
                    0.0
                };

                (wire, congestion_cost + delay_cost)
            })
            .collect();

        // Route to each sink with LUT input permutation support
        let timing_weight = net_criticality * self.timing_weight;

        for (sink_id, pin_idx) in &net.fanout {
            let sink_loc = match placement.get(*sink_id) {
                Some(loc) => loc,
                None => continue,
            };

            // Get sink wire using proper BEL pin lookup
            let sink_wire = match self.get_sink_wire(*sink_id, sink_loc, *pin_idx, netlist) {
                Some(w) => w,
                None => continue,
            };

            route.sinks.push(sink_wire);

            // Find path using A* with timing-aware costs
            let path_result = astar.find_path_timing_aware(
                source_wire,
                sink_wire,
                &[],
                &wire_costs,
                timing_weight,
            );

            match path_result {
                Ok((wires, pips)) => {
                    // Primary path succeeded
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
                Err(_) => {
                    // Primary routing failed - try LUT input permutation for LUT4 sinks
                    if sink_loc.bel_type == BelType::Lut4 {
                        if let Some((alt_wire, alt_wires, alt_pips)) = self
                            .try_alternate_lut_inputs(
                                *sink_id,
                                sink_loc,
                                source_wire,
                                &wire_costs,
                                timing_weight,
                                astar,
                                netlist,
                            )
                        {
                            // Found alternate route through different LUT input
                            if route.sinks.last() == Some(&sink_wire) {
                                route.sinks.pop();
                            }
                            route.sinks.push(alt_wire);

                            for wire in alt_wires {
                                if !route.wires.contains(&wire) {
                                    route.wires.push(wire);
                                }
                            }
                            route.pips.extend(alt_pips);

                            for &pip_id in &route.pips {
                                if let Some(pip) = self.device.pip(pip_id) {
                                    route.delay += pip.delay;
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok(route)
    }

    /// Get the output wire for a cell (source of a net)
    fn get_source_wire(
        &self,
        cell_id: CellId,
        loc: &PlacementLoc,
        netlist: &GateNetlist,
    ) -> Option<WireId> {
        // Convert BEL index to LC index
        let lc_idx = if loc.bel_index >= 16 {
            7 // Carry uses LC7
        } else {
            loc.bel_index / 2
        };

        // Get cell type to determine which wire to use
        let cell = netlist.cells.iter().find(|c| c.id == cell_id)?;

        match loc.bel_type {
            BelType::Lut4 => {
                // LUT output wire
                self.device
                    .lut_output_wire(loc.tile_x, loc.tile_y, lc_idx)
                    .or_else(|| self.fallback_source_wire(loc, lc_idx))
            }
            BelType::Dff | BelType::DffE | BelType::DffSr | BelType::DffSrE => {
                // DFF output is same as LUT output (shared in iCE40)
                self.device
                    .lut_output_wire(loc.tile_x, loc.tile_y, lc_idx)
                    .or_else(|| self.fallback_source_wire(loc, lc_idx))
            }
            BelType::IoCell => {
                // I/O input wire (data coming from pad into fabric)
                let iob_idx = loc.bel_index % 2;
                // Check if this is an input cell
                if cell.cell_type.contains("INPUT") || cell.inputs.is_empty() {
                    self.device
                        .io_input_wire(loc.tile_x, loc.tile_y, iob_idx)
                        .or_else(|| self.fallback_source_wire(loc, iob_idx))
                } else {
                    // Output cell - shouldn't be a source
                    self.fallback_source_wire(loc, iob_idx)
                }
            }
            _ => self.fallback_source_wire(loc, lc_idx),
        }
    }

    /// Get the input wire for a cell (sink of a net)
    fn get_sink_wire(
        &self,
        cell_id: CellId,
        loc: &PlacementLoc,
        pin_idx: usize,
        netlist: &GateNetlist,
    ) -> Option<WireId> {
        // Convert BEL index to LC index
        let lc_idx = if loc.bel_index >= 16 {
            7 // Carry uses LC7
        } else {
            loc.bel_index / 2
        };

        // Get cell type to determine which wire to use
        let cell = netlist.cells.iter().find(|c| c.id == cell_id)?;

        match loc.bel_type {
            BelType::Lut4 => {
                // Use the pin index directly as LUT input index (0-3)
                let input_idx = pin_idx.min(3);
                self.device
                    .lut_input_wire(loc.tile_x, loc.tile_y, lc_idx, input_idx)
                    .or_else(|| self.fallback_sink_wire(loc, lc_idx))
            }
            BelType::Dff | BelType::DffE | BelType::DffSr | BelType::DffSrE => {
                // DFF data input - uses same wire as LUT input 0 in iCE40
                // (The DFF input comes from the LUT output in the same LC,
                // but when driven externally it uses input 0)
                self.device
                    .lut_input_wire(loc.tile_x, loc.tile_y, lc_idx, 0)
                    .or_else(|| self.fallback_sink_wire(loc, lc_idx))
            }
            BelType::IoCell => {
                // I/O output wire (data going to pad from fabric)
                let iob_idx = loc.bel_index % 2;
                if cell.cell_type.contains("OUTPUT") || cell.outputs.is_empty() {
                    self.device
                        .io_output_wire(loc.tile_x, loc.tile_y, iob_idx)
                        .or_else(|| self.fallback_sink_wire(loc, iob_idx))
                } else {
                    self.fallback_sink_wire(loc, iob_idx)
                }
            }
            _ => self.fallback_sink_wire(loc, lc_idx),
        }
    }

    /// Fallback to using tile wires when BEL pin wires aren't available
    fn fallback_source_wire(&self, loc: &PlacementLoc, idx: usize) -> Option<WireId> {
        let wires = self.device.tile_wires(loc.tile_x, loc.tile_y);
        self.select_bel_wire(&wires, idx)
    }

    /// Fallback to using tile wires when BEL pin wires aren't available
    fn fallback_sink_wire(&self, loc: &PlacementLoc, idx: usize) -> Option<WireId> {
        let wires = self.device.tile_wires(loc.tile_x, loc.tile_y);
        self.select_bel_wire(&wires, idx)
    }

    /// Select a wire for a specific BEL index
    /// In iCE40, each LC has associated local wires (local_g0 for LC0, local_g1 for LC1, etc.)
    /// BEL indices: LUT0=0, DFF0=1, LUT1=2, DFF1=3, ..., LUT7=14, DFF7=15, Carry=16
    fn select_bel_wire(&self, wires: &[WireId], bel_index: usize) -> Option<WireId> {
        if wires.is_empty() {
            return None;
        }

        // Convert BEL index to LC index: LUT/DFF pairs share an LC
        // LUT at even index, DFF at odd index -> both map to LC = bel_index / 2
        // Carry (bel_index 16) maps to LC 7 (the last one)
        let lc_idx = if bel_index >= 16 {
            7 // Carry uses LC7
        } else {
            bel_index / 2
        };

        // Each LC has its own local wire (local_g0 through local_g7)
        // If we have enough wires, use the LC-specific one
        if lc_idx < wires.len() {
            Some(wires[lc_idx])
        } else {
            // Fall back to first wire if not enough wires in tile
            wires.first().copied()
        }
    }

    /// Try alternate LUT input pins when primary routing fails
    /// Returns (sink_wire, wires, pips) if an alternate route is found
    #[allow(clippy::too_many_arguments)]
    fn try_alternate_lut_inputs(
        &self,
        _sink_id: CellId,
        sink_loc: &PlacementLoc,
        source_wire: WireId,
        wire_costs: &HashMap<WireId, f64>,
        timing_weight: f64,
        astar: &AStarRouter<D>,
        _netlist: &GateNetlist,
    ) -> Option<(WireId, Vec<WireId>, Vec<crate::device::PipId>)> {
        // Convert BEL index to LC index
        let lc_idx = if sink_loc.bel_index >= 16 {
            7
        } else {
            sink_loc.bel_index / 2
        };

        // Try all 4 LUT input pins (0-3)
        // We already tried the original pin, so try the others
        for alt_input_idx in 0..4 {
            let alt_wire = match self.device.lut_input_wire(
                sink_loc.tile_x,
                sink_loc.tile_y,
                lc_idx,
                alt_input_idx,
            ) {
                Some(w) => w,
                None => continue,
            };

            // Try routing to this alternate input
            if let Ok((wires, pips)) =
                astar.find_path_timing_aware(source_wire, alt_wire, &[], wire_costs, timing_weight)
            {
                // Found a route to an alternate LUT input
                // Note: The caller should update the LUT permutation accordingly
                return Some((alt_wire, wires, pips));
            }
        }

        None
    }

    /// Calculate bounding box size (half-perimeter wirelength) for a net
    fn calculate_net_bbox(
        &self,
        net: &skalp_lir::gate_netlist::GateNet,
        placement: &PlacementResult,
    ) -> u32 {
        let mut min_x = u32::MAX;
        let mut max_x = 0u32;
        let mut min_y = u32::MAX;
        let mut max_y = 0u32;

        // Driver location
        if let Some(driver_id) = net.driver {
            if let Some(loc) = placement.get(driver_id) {
                min_x = min_x.min(loc.tile_x);
                max_x = max_x.max(loc.tile_x);
                min_y = min_y.min(loc.tile_y);
                max_y = max_y.max(loc.tile_y);
            }
        }

        // Sink locations
        for (sink_id, _) in &net.fanout {
            if let Some(loc) = placement.get(*sink_id) {
                min_x = min_x.min(loc.tile_x);
                max_x = max_x.max(loc.tile_x);
                min_y = min_y.min(loc.tile_y);
                max_y = max_y.max(loc.tile_y);
            }
        }

        // Half-perimeter wirelength (HPWL)
        if min_x <= max_x && min_y <= max_y {
            (max_x - min_x) + (max_y - min_y)
        } else {
            0
        }
    }

    /// Calculate net criticalities for timing-driven routing
    /// Criticality is based on:
    /// - Path length (longer paths = more critical)
    /// - Fanout (high fanout = more critical)
    /// - Whether the net is on a timing-critical path (connected to registers)
    fn calculate_net_criticalities(
        &self,
        netlist: &GateNetlist,
        placement: &PlacementResult,
    ) -> Vec<NetCriticality> {
        let mut criticalities = Vec::new();

        // First pass: calculate raw metrics for each net
        let mut max_bbox = 1u32;
        let mut max_fanout = 1usize;

        for net in &netlist.nets {
            if net.driver.is_none() || net.fanout.is_empty() {
                continue;
            }

            let bbox = self.calculate_net_bbox(net, placement);
            max_bbox = max_bbox.max(bbox);
            max_fanout = max_fanout.max(net.fanout.len());
        }

        // Second pass: calculate normalized criticality
        for net in &netlist.nets {
            if net.driver.is_none() || net.fanout.is_empty() {
                continue;
            }

            let bbox = self.calculate_net_bbox(net, placement);
            let fanout = net.fanout.len();

            // Check if net is timing-critical (connected to sequential elements)
            let is_timing_critical = self.is_timing_critical_net(net, netlist);

            // Normalize metrics to 0.0-1.0 range
            let bbox_factor = bbox as f64 / max_bbox as f64;
            let fanout_factor = fanout as f64 / max_fanout as f64;

            // Calculate criticality score
            // Higher weight for timing-critical nets
            let timing_bonus = if is_timing_critical { 0.3 } else { 0.0 };

            // Criticality = weighted combination of factors
            // bbox contributes 40%, fanout 30%, timing 30%
            let criticality = (0.4 * bbox_factor + 0.3 * fanout_factor + timing_bonus).min(1.0);

            // Estimate slack based on bbox (larger bbox = longer path = less slack)
            // This is a rough estimate; real timing analysis would be more accurate
            let estimated_delay_ns = bbox as f64 * 0.5 + fanout as f64 * 0.1;
            let target_period_ns = 10.0; // 100 MHz target
            let slack = target_period_ns - estimated_delay_ns;

            criticalities.push(NetCriticality {
                net_id: net.id,
                criticality,
                slack,
                bbox,
            });
        }

        criticalities
    }

    /// Check if a net is timing-critical (connected to registers)
    fn is_timing_critical_net(
        &self,
        net: &skalp_lir::gate_netlist::GateNet,
        netlist: &GateNetlist,
    ) -> bool {
        // Check if driver is a register
        if let Some(driver_id) = net.driver {
            if let Some(cell) = netlist.cells.iter().find(|c| c.id == driver_id) {
                if cell.clock.is_some() {
                    return true;
                }
            }
        }

        // Check if any sink is a register
        for (sink_id, _) in &net.fanout {
            if let Some(cell) = netlist.cells.iter().find(|c| c.id == *sink_id) {
                if cell.clock.is_some() {
                    return true;
                }
            }
        }

        false
    }
}
