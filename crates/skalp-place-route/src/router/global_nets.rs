//! Global Net Routing
//!
//! Routes clock and reset signals through dedicated global routing resources.
//! In iCE40 FPGAs, global buffer (GBUF) primitives provide low-skew clock distribution
//! through 8 dedicated global clock networks.

use super::Route;
use crate::device::{Device, WireId, WireType};
use crate::error::Result;
use crate::placer::PlacementResult;
use skalp_lir::gate_netlist::{GateNetId, GateNetlist};

/// Tracks which global networks are in use
#[derive(Default)]
pub struct GlobalNetworkAllocator {
    /// Which global network indices are currently in use
    used_networks: [bool; 8],
    /// Mapping from net ID to allocated global network
    allocations: std::collections::HashMap<GateNetId, u8>,
}

impl GlobalNetworkAllocator {
    /// Create a new allocator
    pub fn new() -> Self {
        Self::default()
    }

    /// Allocate a global network for a net
    pub fn allocate(&mut self, net_id: GateNetId) -> Option<u8> {
        // Check if already allocated
        if let Some(&glb_num) = self.allocations.get(&net_id) {
            return Some(glb_num);
        }

        // Find first available network
        for (i, &used) in self.used_networks.iter().enumerate() {
            if !used {
                self.used_networks[i] = true;
                self.allocations.insert(net_id, i as u8);
                return Some(i as u8);
            }
        }

        None // All networks in use
    }

    /// Get the allocated network for a net
    pub fn get_allocation(&self, net_id: GateNetId) -> Option<u8> {
        self.allocations.get(&net_id).copied()
    }

    /// Get number of available networks
    pub fn available_count(&self) -> usize {
        self.used_networks.iter().filter(|&&x| !x).count()
    }
}

/// Global net router for clocks and resets
pub struct GlobalNetRouter<'a, D: Device> {
    device: &'a D,
    allocator: GlobalNetworkAllocator,
}

impl<'a, D: Device> GlobalNetRouter<'a, D> {
    /// Create a new global net router
    pub fn new(device: &'a D) -> Self {
        Self {
            device,
            allocator: GlobalNetworkAllocator::new(),
        }
    }

    /// Route a clock net through global GBUF resources
    ///
    /// The routing process:
    /// 1. Find the clock source location
    /// 2. Allocate a global network
    /// 3. Connect source to nearest GBUF input
    /// 4. Use global network to distribute to all sinks
    pub fn route_clock(
        &mut self,
        net_id: GateNetId,
        netlist: &GateNetlist,
        placement: &PlacementResult,
    ) -> Result<Route> {
        let net = &netlist.nets[net_id.0 as usize];
        let mut route = Route::new(net_id);

        // Find the clock source location
        let source_loc = if let Some(driver_id) = net.driver {
            placement.get(driver_id)
        } else {
            None
        };

        // Allocate a global network for this clock
        let glb_num = self.allocator.allocate(net_id).unwrap_or(0);

        // Find global wire at source or use center tile
        let (source_x, source_y) = source_loc
            .map(|loc| (loc.tile_x, loc.tile_y))
            .unwrap_or_else(|| {
                let (w, h) = self.device.grid_size();
                (w / 2, h / 2)
            });

        // Find the global clock wire
        let global_wire = self.find_global_wire_at(source_x, source_y, glb_num)?;
        route.source = global_wire;
        route.wires.push(global_wire);

        // Route to each sink through the global network
        for (sink_id, _) in &net.fanout {
            if let Some(loc) = placement.get(*sink_id) {
                // Find the global wire at the sink tile
                if let Ok(sink_global_wire) =
                    self.find_global_wire_at(loc.tile_x, loc.tile_y, glb_num)
                {
                    if !route.sinks.contains(&sink_global_wire) {
                        route.sinks.push(sink_global_wire);
                    }
                    if !route.wires.contains(&sink_global_wire) {
                        route.wires.push(sink_global_wire);
                    }
                } else {
                    // Fall back to clock wire method from Device trait
                    if let Some(clk_wire) = self.device.clock_wire(loc.tile_x, loc.tile_y) {
                        if !route.sinks.contains(&clk_wire) {
                            route.sinks.push(clk_wire);
                        }
                        if !route.wires.contains(&clk_wire) {
                            route.wires.push(clk_wire);
                        }
                    }
                }
            }
        }

        // Global clock routes have minimal delay due to dedicated routing
        route.delay = 50; // ~50ps for global network

        Ok(route)
    }

    /// Route a reset net through global resources
    pub fn route_reset(
        &mut self,
        net_id: GateNetId,
        netlist: &GateNetlist,
        placement: &PlacementResult,
    ) -> Result<Route> {
        // Reset can also use global network if available
        // Otherwise falls back to regular routing
        if self.allocator.available_count() > 0 {
            self.route_clock(net_id, netlist, placement)
        } else {
            // No global networks available, create basic route
            let net = &netlist.nets[net_id.0 as usize];
            let mut route = Route::new(net_id);

            // Find any available wire at source
            if let Some(driver_id) = net.driver {
                if let Some(loc) = placement.get(driver_id) {
                    let wires = self.device.tile_wires(loc.tile_x, loc.tile_y);
                    if let Some(&wire) = wires.first() {
                        route.source = wire;
                        route.wires.push(wire);
                    }
                }
            }

            Ok(route)
        }
    }

    /// Find a global wire at a specific tile location
    fn find_global_wire_at(&self, tile_x: u32, tile_y: u32, glb_num: u8) -> Result<WireId> {
        // First try to find a specific global wire for this network
        for wire_id in self.device.tile_wires(tile_x, tile_y) {
            if let Some(wire) = self.device.wire(wire_id) {
                if matches!(wire.wire_type, WireType::Global(n) if n == glb_num) {
                    return Ok(wire_id);
                }
            }
        }

        // Try any global wire at this tile
        for wire_id in self.device.tile_wires(tile_x, tile_y) {
            if let Some(wire) = self.device.wire(wire_id) {
                if matches!(wire.wire_type, WireType::Global(_)) {
                    return Ok(wire_id);
                }
            }
        }

        // Fall back to clock wire method
        if let Some(clk_wire) = self.device.clock_wire(tile_x, tile_y) {
            return Ok(clk_wire);
        }

        // Last resort: any wire at this tile
        let wires = self.device.tile_wires(tile_x, tile_y);
        wires.first().copied().ok_or_else(|| {
            crate::error::PlaceRouteError::RoutingFailed(format!(
                "No global wire found at tile ({}, {})",
                tile_x, tile_y
            ))
        })
    }

    /// Check if a net is a global net (clock/reset)
    pub fn is_global_net(&self, net: &skalp_lir::gate_netlist::GateNet) -> bool {
        net.is_clock || net.is_reset
    }

    /// Get the number of available global networks
    pub fn available_networks(&self) -> usize {
        self.allocator.available_count()
    }

    /// Get allocated network for a net
    pub fn get_network_allocation(&self, net_id: GateNetId) -> Option<u8> {
        self.allocator.get_allocation(net_id)
    }
}
