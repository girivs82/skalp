//! Global Net Routing
//!
//! Routes clock and reset signals through dedicated global routing resources.

use super::Route;
use crate::device::{Device, WireId, WireType};
use crate::error::Result;
use crate::placer::PlacementResult;
use skalp_lir::gate_netlist::{GateNetId, GateNetlist};

/// Global net router for clocks and resets
pub struct GlobalNetRouter<'a, D: Device> {
    device: &'a D,
}

impl<'a, D: Device> GlobalNetRouter<'a, D> {
    /// Create a new global net router
    pub fn new(device: &'a D) -> Self {
        Self { device }
    }

    /// Route a clock net through global resources
    pub fn route_clock(
        &self,
        net_id: GateNetId,
        netlist: &GateNetlist,
        placement: &PlacementResult,
    ) -> Result<Route> {
        let net = &netlist.nets[net_id.0 as usize];
        let mut route = Route::new(net_id);

        // Find a global clock wire
        let global_wire = self.find_global_wire(net, placement)?;
        route.source = global_wire;

        // Add all sinks
        for (sink_id, _) in &net.fanout {
            if let Some(loc) = placement.get(*sink_id) {
                // Get local global wire at sink tile
                let sink_wires = self.device.tile_wires(loc.tile_x, loc.tile_y);
                for &wire in &sink_wires {
                    if let Some(w) = self.device.wire(wire) {
                        if matches!(w.wire_type, WireType::Global(_)) {
                            route.sinks.push(wire);
                            route.wires.push(wire);
                            break;
                        }
                    }
                }
            }
        }

        Ok(route)
    }

    /// Route a reset net through global resources
    pub fn route_reset(
        &self,
        net_id: GateNetId,
        netlist: &GateNetlist,
        placement: &PlacementResult,
    ) -> Result<Route> {
        // Reset routing is similar to clock routing
        self.route_clock(net_id, netlist, placement)
    }

    /// Find a global wire for the net
    fn find_global_wire(
        &self,
        _net: &skalp_lir::gate_netlist::GateNet,
        _placement: &PlacementResult,
    ) -> Result<WireId> {
        // Find an available global wire
        // For simplicity, use the first global wire in the device
        let (width, height) = self.device.grid_size();

        // Check center tile for global wires
        let center_x = width / 2;
        let center_y = height / 2;

        for wire in self.device.tile_wires(center_x, center_y) {
            if let Some(w) = self.device.wire(wire) {
                if matches!(w.wire_type, WireType::Global(_)) {
                    return Ok(wire);
                }
            }
        }

        // Fall back to first wire
        Ok(WireId(0))
    }

    /// Check if a net is a global net (clock/reset)
    pub fn is_global_net(&self, net: &skalp_lir::gate_netlist::GateNet) -> bool {
        net.is_clock || net.is_reset
    }
}
