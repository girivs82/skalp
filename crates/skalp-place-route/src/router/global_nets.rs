//! Global Net Routing
//!
//! Routes clock and reset signals through dedicated global routing resources.
//! In iCE40 FPGAs, global buffer (GBUF) primitives provide low-skew clock distribution
//! through 8 dedicated global clock networks.
//!
//! The GBUF routing path is:
//!   IO pad → (hardwired) → padin_glb_netwk.N (extra bit) → global network → ColBufCtrl → LCs
//!
//! Each GBUF-capable pad (.gbufpin) is hardwired to exactly one global network.
//! The padin_glb_netwk.N extra bit enables this connection. If the clock source
//! is not at a GBUF-capable pad, routing falls back to fabric routing via PathFinder.

use super::{ExtraBitConfig, Route};
use crate::device::{Device, WireType};
use crate::error::Result;
use crate::placer::PlacementResult;
use crate::timing::DelayModel;
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

    /// Allocate a specific global network for a net.
    /// Each GBUF-capable pad is hardwired to one network, so the caller
    /// must request the specific network number.
    pub fn allocate_specific(&mut self, net_id: GateNetId, glb_num: u8) -> Option<u8> {
        if glb_num >= 8 {
            return None;
        }

        // Check if already allocated to this net
        if let Some(&existing) = self.allocations.get(&net_id) {
            if existing == glb_num {
                return Some(glb_num);
            }
            return None; // Already allocated to a different network
        }

        // Check if this specific network is available
        if self.used_networks[glb_num as usize] {
            return None; // Network already in use
        }

        self.used_networks[glb_num as usize] = true;
        self.allocations.insert(net_id, glb_num);
        Some(glb_num)
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
    delay_model: DelayModel,
    /// Extra bits to emit in the bitstream (padin_glb_netwk)
    extra_bits: Vec<ExtraBitConfig>,
}

impl<'a, D: Device> GlobalNetRouter<'a, D> {
    /// Create a new global net router
    pub fn new(device: &'a D) -> Self {
        Self {
            device,
            allocator: GlobalNetworkAllocator::new(),
            delay_model: DelayModel::ice40_default(),
            extra_bits: Vec::new(),
        }
    }

    /// Create a new global net router with a specific delay model
    pub fn with_delay_model(device: &'a D, delay_model: DelayModel) -> Self {
        Self {
            device,
            allocator: GlobalNetworkAllocator::new(),
            delay_model,
            extra_bits: Vec::new(),
        }
    }

    /// Route a clock net through global GBUF resources
    ///
    /// Returns Ok(Route) if the clock source is at a GBUF-capable pad and
    /// the corresponding global network is available. Returns Err otherwise,
    /// and the caller should fall back to fabric routing.
    pub fn route_clock(
        &mut self,
        net_id: GateNetId,
        netlist: &GateNetlist,
        placement: &PlacementResult,
    ) -> Result<Route> {
        let net = &netlist.nets[net_id.0 as usize];

        // 1. Find the clock source (driver) location
        let driver_id = net.driver.ok_or_else(|| {
            crate::error::PlaceRouteError::RoutingFailed(
                "Clock net has no driver".to_string(),
            )
        })?;

        let source_loc = placement.get(driver_id).ok_or_else(|| {
            crate::error::PlaceRouteError::RoutingFailed(
                "Clock driver not placed".to_string(),
            )
        })?;

        // 2. Check if the driver pad is GBUF-capable
        let pio_num = source_loc.bel_index as u8;
        let glb_num = self.device.gbufpin_for_pad(
            source_loc.tile_x,
            source_loc.tile_y,
            pio_num,
        ).ok_or_else(|| {
            crate::error::PlaceRouteError::RoutingFailed(format!(
                "Clock pad at ({},{}) IOB_{} is not GBUF-capable",
                source_loc.tile_x, source_loc.tile_y, pio_num
            ))
        })?;

        // 3. Allocate the specific global network (each pad is hardwired to one)
        self.allocator.allocate_specific(net_id, glb_num).ok_or_else(|| {
            crate::error::PlaceRouteError::RoutingFailed(format!(
                "Global network {} already in use", glb_num
            ))
        })?;

        // 4. Record the padin_glb_netwk extra bit
        if let Some((bank, addr_x, addr_y)) = self.device.padin_extra_bit(glb_num) {
            self.extra_bits.push(ExtraBitConfig { bank, addr_x, addr_y });
        }

        // 5. Build the route
        let mut route = Route::new(net_id);

        // Source: the IO input wire at the pad tile
        if let Some(src_wire) = self.device.io_input_wire(
            source_loc.tile_x,
            source_loc.tile_y,
            pio_num as usize,
        ) {
            route.source = src_wire;
            route.wires.push(src_wire);
        }

        // Sinks: global clock wire at each sink tile
        for (sink_id, _) in &net.fanout {
            if let Some(loc) = placement.get(*sink_id) {
                // Try to find the specific glb_netwk wire at the sink tile
                if let Ok(sink_wire) = self.find_global_wire_at(loc.tile_x, loc.tile_y, glb_num) {
                    if !route.sinks.contains(&sink_wire) {
                        route.sinks.push(sink_wire);
                    }
                    if !route.wires.contains(&sink_wire) {
                        route.wires.push(sink_wire);
                    }
                } else if let Some(clk_wire) = self.device.clock_wire(loc.tile_x, loc.tile_y) {
                    if !route.sinks.contains(&clk_wire) {
                        route.sinks.push(clk_wire);
                    }
                    if !route.wires.contains(&clk_wire) {
                        route.wires.push(clk_wire);
                    }
                }
            }
        }

        // No PIPs needed — the GBUF path is hardwired, enabled by the extra bit.
        // The global network distribution is handled by ColBufCtrl bits (set elsewhere).

        // Global clock delay
        route.delay = (self.delay_model.global_clock_delay * 1000.0) as u32;

        Ok(route)
    }

    /// Route a reset net through global resources
    pub fn route_reset(
        &mut self,
        net_id: GateNetId,
        netlist: &GateNetlist,
        placement: &PlacementResult,
    ) -> Result<Route> {
        self.route_clock(net_id, netlist, placement)
    }

    /// Find a global wire at a specific tile location
    fn find_global_wire_at(
        &self,
        tile_x: u32,
        tile_y: u32,
        glb_num: u8,
    ) -> Result<crate::device::WireId> {
        // Try to find the specific global wire for this network
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

        // Fall back to clock wire
        if let Some(clk_wire) = self.device.clock_wire(tile_x, tile_y) {
            return Ok(clk_wire);
        }

        Err(crate::error::PlaceRouteError::RoutingFailed(format!(
            "No global wire found at tile ({}, {})",
            tile_x, tile_y
        )))
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

    /// Consume the extra bits collected during routing
    pub fn take_extra_bits(&mut self) -> Vec<ExtraBitConfig> {
        std::mem::take(&mut self.extra_bits)
    }
}
