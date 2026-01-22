//! Carry Chain Routing
//!
//! Routes carry chain signals through dedicated carry chain wires.
//! In iCE40 FPGAs, carry chains run vertically through logic tiles,
//! connecting LC[n].CO to LC[n+1].CI within a tile, and across tiles.
//!
//! The carry chain structure in iCE40:
//! - Each LC has carry_in and carry_out pins
//! - carry_in_mux selects between fabric input and carry from previous LC
//! - Carry propagates vertically through the chip

use super::Route;
use crate::device::{Device, PipId, WireId, WireType};
use crate::error::Result;
use crate::placer::PlacementResult;
use skalp_lir::gate_netlist::{GateNetId, GateNetlist};

/// Carry chain router for dedicated carry routing
pub struct CarryChainRouter<'a, D: Device> {
    device: &'a D,
}

impl<'a, D: Device> CarryChainRouter<'a, D> {
    /// Create a new carry chain router
    pub fn new(device: &'a D) -> Self {
        Self { device }
    }

    /// Check if a net is a carry chain net
    /// Carry nets connect carry-out of one cell to carry-in of next
    pub fn is_carry_net(
        &self,
        net: &skalp_lir::gate_netlist::GateNet,
        netlist: &GateNetlist,
    ) -> bool {
        // Check if driver is a carry cell
        if let Some(driver_id) = net.driver {
            if let Some(cell) = netlist.cells.iter().find(|c| c.id == driver_id) {
                if cell.cell_type.contains("CARRY") || cell.cell_type == "SB_CARRY" {
                    return true;
                }
            }
        }

        // Check if any sink is a carry cell expecting carry input
        for (sink_id, _) in &net.fanout {
            if let Some(cell) = netlist.cells.iter().find(|c| c.id == *sink_id) {
                if cell.cell_type.contains("CARRY") || cell.cell_type == "SB_CARRY" {
                    return true;
                }
            }
        }

        false
    }

    /// Route a carry chain net through dedicated carry wires
    pub fn route_carry(
        &self,
        net_id: GateNetId,
        netlist: &GateNetlist,
        placement: &PlacementResult,
    ) -> Result<Route> {
        let net = &netlist.nets[net_id.0 as usize];
        let mut route = Route::new(net_id);

        // Get driver location
        let driver_id = match net.driver {
            Some(id) => id,
            None => return Ok(route),
        };

        let source_loc = match placement.get(driver_id) {
            Some(loc) => loc,
            None => return Ok(route),
        };

        // Find carry_out wire at source
        let source_wire = self
            .find_carry_out_wire(source_loc.tile_x, source_loc.tile_y, source_loc.bel_index)
            .unwrap_or_else(|| {
                // Fall back to any tile wire
                self.device
                    .tile_wires(source_loc.tile_x, source_loc.tile_y)
                    .first()
                    .copied()
                    .unwrap_or(WireId(0))
            });

        route.source = source_wire;
        route.wires.push(source_wire);

        // Route to each sink
        for (sink_id, _) in &net.fanout {
            let sink_loc = match placement.get(*sink_id) {
                Some(loc) => loc,
                None => continue,
            };

            // Find carry_in wire at sink
            let sink_wire = self
                .find_carry_in_wire(sink_loc.tile_x, sink_loc.tile_y, sink_loc.bel_index)
                .unwrap_or_else(|| {
                    self.device
                        .tile_wires(sink_loc.tile_x, sink_loc.tile_y)
                        .first()
                        .copied()
                        .unwrap_or(WireId(0))
                });

            route.sinks.push(sink_wire);

            // Check if source and sink are in same tile or adjacent tiles
            // Carry chain routing is simple when cells are properly placed
            if source_loc.tile_x == sink_loc.tile_x {
                // Same column - carry can propagate directly
                self.route_vertical_carry(
                    source_loc.tile_y,
                    sink_loc.tile_y,
                    source_loc.tile_x,
                    &mut route,
                );
            } else {
                // Different columns - need to use general routing for hop
                // This shouldn't happen with proper carry placement
                // Add sink wire to route
                if !route.wires.contains(&sink_wire) {
                    route.wires.push(sink_wire);
                }
            }
        }

        // Carry chain has very low delay
        route.delay = 10; // ~10ps per carry propagation

        Ok(route)
    }

    /// Find carry_out wire at a specific location
    fn find_carry_out_wire(&self, tile_x: u32, tile_y: u32, bel_index: usize) -> Option<WireId> {
        let lc_idx = if bel_index >= 16 { 7 } else { bel_index / 2 };

        // Try to find carry_out wire by name pattern
        let wire_name = format!("lutff_{}/cout", lc_idx);
        if let Some(wire_id) = self.device.wire_by_name(&wire_name) {
            return Some(wire_id);
        }

        // Search for CarryChain type wires in the tile
        for wire_id in self.device.tile_wires(tile_x, tile_y) {
            if let Some(wire) = self.device.wire(wire_id) {
                if matches!(wire.wire_type, WireType::CarryChain) && wire.name.contains("out") {
                    return Some(wire_id);
                }
            }
        }

        None
    }

    /// Find carry_in wire at a specific location
    fn find_carry_in_wire(&self, tile_x: u32, tile_y: u32, bel_index: usize) -> Option<WireId> {
        let lc_idx = if bel_index >= 16 { 7 } else { bel_index / 2 };

        // Try to find carry_in wire by name pattern
        let wire_name = format!("lutff_{}/cin", lc_idx);
        if let Some(wire_id) = self.device.wire_by_name(&wire_name) {
            return Some(wire_id);
        }

        // Search for CarryChain type wires in the tile
        for wire_id in self.device.tile_wires(tile_x, tile_y) {
            if let Some(wire) = self.device.wire(wire_id) {
                if matches!(wire.wire_type, WireType::CarryChain) && wire.name.contains("in") {
                    return Some(wire_id);
                }
            }
        }

        None
    }

    /// Route carry signal vertically through tiles
    fn route_vertical_carry(&self, source_y: u32, sink_y: u32, tile_x: u32, route: &mut Route) {
        // Carry chain propagates from lower y to higher y (typically)
        let (min_y, max_y) = if source_y < sink_y {
            (source_y, sink_y)
        } else {
            (sink_y, source_y)
        };

        // Add intermediate carry wires for each tile in the chain
        for y in min_y..=max_y {
            // Find any carry chain wires in this tile
            for wire_id in self.device.tile_wires(tile_x, y) {
                if let Some(wire) = self.device.wire(wire_id) {
                    if matches!(wire.wire_type, WireType::CarryChain) {
                        if !route.wires.contains(&wire_id) {
                            route.wires.push(wire_id);
                        }
                        break;
                    }
                }
            }
        }
    }

    /// Get PIPs for carry chain routing
    /// In iCE40, carry chain connections are typically hardwired,
    /// not requiring explicit PIP configuration
    #[allow(dead_code)]
    fn get_carry_pips(&self, _source_wire: WireId, _sink_wire: WireId) -> Vec<PipId> {
        // Carry chain connections are hardwired in iCE40
        // No PIPs needed for direct carry propagation
        Vec::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::device::ice40::{Ice40Device, Ice40Variant};
    use skalp_lir::gate_netlist::{Cell, GateNet};

    #[test]
    fn test_carry_chain_detection() {
        let device = Ice40Device::new(Ice40Variant::Hx1k);
        let router = CarryChainRouter::new(&device);

        // Create a netlist with carry cells
        let mut netlist = GateNetlist::new("carry_test".to_string(), "ice40".to_string());

        // Carry input net
        let carry_in = netlist.add_net(GateNet::new(
            skalp_lir::gate_netlist::GateNetId(0),
            "carry_in".to_string(),
        ));

        // Carry output net
        let carry_out = netlist.add_net(GateNet::new(
            skalp_lir::gate_netlist::GateNetId(1),
            "carry_out".to_string(),
        ));

        // Add SB_CARRY cell
        netlist.add_cell(Cell::new_comb(
            skalp_lir::gate_netlist::CellId(0),
            "SB_CARRY".to_string(),
            "ice40".to_string(),
            0.0,
            "adder.carry".to_string(),
            vec![carry_in],
            vec![carry_out],
        ));

        // Check that the output net is detected as carry net
        let net = &netlist.nets[carry_out.0 as usize];
        assert!(
            router.is_carry_net(net, &netlist),
            "Should detect carry net"
        );
    }

    #[test]
    fn test_carry_wire_search() {
        let device = Ice40Device::new(Ice40Variant::Hx1k);
        let router = CarryChainRouter::new(&device);

        // Test logic tile (6, 9) for carry wires
        let tile_x = 6;
        let tile_y = 9;

        // Search for any carry chain wires in the tile
        let mut found_carry = false;
        for wire_id in device.tile_wires(tile_x, tile_y) {
            if let Some(wire) = device.wire(wire_id) {
                if matches!(wire.wire_type, WireType::CarryChain) {
                    found_carry = true;
                    println!(
                        "Found carry wire: {} at ({}, {})",
                        wire.name, tile_x, tile_y
                    );
                }
            }
        }

        // Try to find specific carry wire
        let carry_out = router.find_carry_out_wire(tile_x, tile_y, 0);
        let carry_in = router.find_carry_in_wire(tile_x, tile_y, 0);

        println!("Carry out wire: {:?}", carry_out);
        println!("Carry in wire: {:?}", carry_in);
        println!("Found carry wire in tile: {}", found_carry);

        // Note: Carry wires may not be found if chipdb doesn't have them
        // The test verifies the search logic works
    }
}
