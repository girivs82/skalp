//! Delay Models for iCE40 FPGAs
//!
//! Provides timing delay models for cells and routing.

use crate::device::ecp5::data as ecp5_data;
use crate::device::ice40::data as ice40_data;
use crate::device::ice40::Ice40Variant;
use crate::device::nexus::data as nexus_data;
use crate::device::xc7::data as xc7_data;
use serde::{Deserialize, Serialize};

/// Delay model for timing analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DelayModel {
    /// LUT4 propagation delay (ns)
    pub lut4_delay: f64,
    /// DFF clock-to-Q delay (ns)
    pub dff_clk_to_q: f64,
    /// DFF setup time (ns)
    pub dff_setup: f64,
    /// DFF hold time (ns)
    pub dff_hold: f64,
    /// Carry chain delay per bit (ns)
    pub carry_delay: f64,
    /// I/O input delay (ns)
    pub io_input_delay: f64,
    /// I/O output delay (ns)
    pub io_output_delay: f64,
    /// Local wire delay (ns)
    pub local_wire_delay: f64,
    /// Span-4 wire delay (ns)
    pub span4_delay: f64,
    /// Span-12 wire delay (ns)
    pub span12_delay: f64,
    /// Global clock delay (ns)
    pub global_clock_delay: f64,
    /// PIP switch delay (ns) — flat fallback, prefer pip_delay_typed()
    pub pip_delay: f64,
    /// RAM read delay (ns)
    pub ram_read_delay: f64,
    /// RAM write delay (ns)
    pub ram_write_delay: f64,
    /// BelPin→Local PIP delay (ns)
    pub pip_belpin_to_local: f64,
    /// Local→Local PIP delay (ns)
    pub pip_local_to_local: f64,
    /// Local→Span4 PIP delay (ns)
    pub pip_local_to_span4: f64,
    /// Span4→Span4 PIP delay (ns)
    pub pip_span4_to_span4: f64,
    /// Span4→Local PIP delay (ns)
    pub pip_span4_to_local: f64,
    /// Span12→Span12 PIP delay (ns)
    pub pip_span12_to_span12: f64,
    /// Local→BelPin PIP delay (ns)
    pub pip_local_to_belpin: f64,
    /// Global→Local PIP delay (ns)
    pub pip_global_to_local: f64,
    /// Fanout delay per additional load (ns)
    pub fanout_delay_per_load: f64,
}

impl Default for DelayModel {
    fn default() -> Self {
        Self::ice40_default()
    }
}

impl DelayModel {
    /// Default delay model for iCE40 FPGAs
    pub fn ice40_default() -> Self {
        Self::from_ice40_timing(&ice40_data::TIMING_DEFAULT)
    }

    /// Select delay model for a specific iCE40 variant
    pub fn for_variant(variant: Ice40Variant) -> Self {
        use ice40_data::Ice40SpeedFamily;
        let timing = match variant.die_data().speed_family {
            Ice40SpeedFamily::Hx => &ice40_data::TIMING_HX,
            Ice40SpeedFamily::Lp => &ice40_data::TIMING_LP,
            Ice40SpeedFamily::Up => &ice40_data::TIMING_UP,
        };
        Self::from_ice40_timing(timing)
    }

    /// Delay model for iCE40 HX series (higher performance)
    pub fn ice40_hx() -> Self {
        Self::from_ice40_timing(&ice40_data::TIMING_HX)
    }

    /// Delay model for iCE40 LP series (lower power, ~15% slower than HX)
    pub fn ice40_lp() -> Self {
        Self::from_ice40_timing(&ice40_data::TIMING_LP)
    }

    /// Delay model for iCE40 UP series (ultra-low power)
    pub fn ice40_up() -> Self {
        Self::from_ice40_timing(&ice40_data::TIMING_UP)
    }

    fn from_ice40_timing(t: &ice40_data::Ice40TimingData) -> Self {
        Self {
            lut4_delay: t.lut4_delay,
            dff_clk_to_q: t.dff_clk_to_q,
            dff_setup: t.dff_setup,
            dff_hold: t.dff_hold,
            carry_delay: t.carry_delay,
            io_input_delay: t.io_input_delay,
            io_output_delay: t.io_output_delay,
            local_wire_delay: t.local_wire_delay,
            span4_delay: t.span4_delay,
            span12_delay: t.span12_delay,
            global_clock_delay: t.global_clock_delay,
            pip_delay: t.pip_delay,
            ram_read_delay: t.ram_read_delay,
            ram_write_delay: 0.0,
            pip_belpin_to_local: t.pip_belpin_to_local,
            pip_local_to_local: t.pip_local_to_local,
            pip_local_to_span4: t.pip_local_to_span4,
            pip_span4_to_span4: t.pip_span4_to_span4,
            pip_span4_to_local: t.pip_span4_to_local,
            pip_span12_to_span12: t.pip_span12_to_span12,
            pip_local_to_belpin: t.pip_local_to_belpin,
            pip_global_to_local: t.pip_global_to_local,
            fanout_delay_per_load: t.fanout_per_load,
        }
    }

    /// Delay model for Lattice Nexus (CertusPro-NX / CrossLink-NX) — 28nm
    /// Timing from prjoxide LIFCL speed grade 10 (fast corner).
    pub fn nexus() -> Self {
        let t = &nexus_data::TIMING_GRADE10;
        Self {
            lut4_delay: t.lut4_delay,
            dff_clk_to_q: t.dff_clk_to_q,
            dff_setup: t.dff_setup,
            dff_hold: t.dff_hold,
            carry_delay: t.carry_delay,
            io_input_delay: t.io_input_delay,
            io_output_delay: t.io_output_delay,
            local_wire_delay: t.cib_mux_delay,
            span4_delay: t.span0_delay,
            span12_delay: t.span6_delay,
            global_clock_delay: t.global_clock_delay,
            pip_delay: t.span2_delay,
            ram_read_delay: t.ram_read_delay,
            ram_write_delay: 0.0,
            pip_belpin_to_local: t.pip_lut_to_local,
            pip_local_to_local: t.pip_local_to_local,
            pip_local_to_span4: t.pip_local_to_span,
            pip_span4_to_span4: t.pip_span2_to_span2,
            pip_span4_to_local: t.pip_span2_to_local,
            pip_span12_to_span12: t.pip_span6_to_span6,
            pip_local_to_belpin: t.pip_local_to_bel,
            pip_global_to_local: t.pip_clock_to_local,
            fanout_delay_per_load: t.fanout_per_load,
        }
    }

    /// Delay model for Lattice ECP5 — 45nm
    /// Timing from prjtrellis-db, speed grade -8 (fastest).
    pub fn ecp5() -> Self {
        let t = &ecp5_data::TIMING_SPEED8;
        Self {
            lut4_delay: t.lut4_delay,
            dff_clk_to_q: t.dff_clk_to_q,
            dff_setup: t.dff_setup,
            dff_hold: t.dff_hold,
            carry_delay: t.carry_delay,
            io_input_delay: t.io_input_delay,
            io_output_delay: t.io_output_delay,
            local_wire_delay: t.local_wire_delay,
            span4_delay: t.span2_delay,
            span12_delay: t.span6_delay,
            global_clock_delay: t.global_clock_delay,
            pip_delay: t.cib_mux_delay,
            ram_read_delay: t.ram_read_delay,
            ram_write_delay: 0.0,
            pip_belpin_to_local: t.pip_f_to_local,
            pip_local_to_local: t.pip_local_to_local,
            pip_local_to_span4: t.pip_local_to_span2,
            pip_span4_to_span4: t.pip_span2_cascade,
            pip_span4_to_local: t.pip_span2_to_bel,
            pip_span12_to_span12: t.pip_span6_cascade,
            pip_local_to_belpin: t.pip_to_bel,
            pip_global_to_local: t.pip_clock_to_local,
            fanout_delay_per_load: t.fanout_per_load,
        }
    }

    /// Delay model for Xilinx 7-series — 28nm HPL
    /// Timing from prjxray-db SDF files and DS181/DS182.
    pub fn xc7() -> Self {
        Self::from_xc7_timing(&xc7_data::TIMING_ARTIX_1)
    }

    /// Select delay model for a specific 7-series variant
    pub fn for_xc7_variant(variant: crate::device::xc7::Xc7Variant) -> Self {
        let timing = match variant.speed_family() {
            xc7_data::Xc7SpeedFamily::Artix => &xc7_data::TIMING_ARTIX_1,
            xc7_data::Xc7SpeedFamily::Kintex => &xc7_data::TIMING_KINTEX_1,
            xc7_data::Xc7SpeedFamily::Spartan => &xc7_data::TIMING_SPARTAN_1,
        };
        Self::from_xc7_timing(timing)
    }

    fn from_xc7_timing(t: &xc7_data::Xc7TimingData) -> Self {
        Self {
            lut4_delay: t.lut6_delay,          // LUT6 used as generic LUT delay
            dff_clk_to_q: t.dff_clk_to_q,
            dff_setup: t.dff_setup,
            dff_hold: t.dff_hold,
            carry_delay: t.carry4_delay,
            io_input_delay: t.io_input_delay,
            io_output_delay: t.io_output_delay,
            local_wire_delay: t.local_wire_delay,
            span4_delay: t.double_delay,       // map double (span 2) to span4 field
            span12_delay: t.long_delay,        // map long (span 12) to span12 field
            global_clock_delay: t.global_clock_delay,
            pip_delay: t.pip_local_to_local,   // flat fallback
            ram_read_delay: t.bram_read_delay,
            ram_write_delay: 0.0,
            pip_belpin_to_local: t.pip_bel_to_local,
            pip_local_to_local: t.pip_local_to_local,
            pip_local_to_span4: t.pip_local_to_double,
            pip_span4_to_span4: t.pip_double_to_double,
            pip_span4_to_local: t.pip_span_to_local,
            pip_span12_to_span12: t.pip_long_to_long,
            pip_local_to_belpin: t.pip_bel_to_local,  // symmetric estimate
            pip_global_to_local: t.pip_clock_to_local,
            fanout_delay_per_load: t.fanout_per_load,
        }
    }

    /// Get cell delay for a given cell type
    pub fn cell_delay(&self, cell_type: &str) -> f64 {
        if cell_type.contains("LUT") || cell_type.starts_with("SB_LUT") {
            self.lut4_delay
        } else if cell_type.contains("DFF") || cell_type.starts_with("SB_DFF") {
            self.dff_clk_to_q
        } else if cell_type.contains("CARRY") || cell_type.starts_with("SB_CARRY") {
            self.carry_delay
        } else if cell_type.contains("IO") || cell_type.starts_with("SB_IO") {
            self.io_input_delay // Conservative estimate
        } else if cell_type.contains("RAM") || cell_type.starts_with("SB_RAM") {
            self.ram_read_delay
        } else {
            // Default to LUT delay for unknown cells
            self.lut4_delay
        }
    }

    /// Get register clock-to-Q delay
    pub fn register_clock_to_q(&self) -> f64 {
        self.dff_clk_to_q
    }

    /// Get register setup time
    pub fn register_setup(&self) -> f64 {
        self.dff_setup
    }

    /// Get register hold time
    pub fn register_hold(&self) -> f64 {
        self.dff_hold
    }

    /// Get wire-type-aware PIP delay based on source and destination wire types
    pub fn pip_delay_typed(
        &self,
        src_type: &crate::device::WireType,
        dst_type: &crate::device::WireType,
    ) -> f64 {
        use crate::device::WireType;
        match (src_type, dst_type) {
            (WireType::BelPin, WireType::Local(_)) => self.pip_belpin_to_local,
            (WireType::Local(_), WireType::Local(_)) => self.pip_local_to_local,
            (WireType::Local(_), WireType::Span4H(_) | WireType::Span4V(_)) => {
                self.pip_local_to_span4
            }
            (
                WireType::Span4H(_) | WireType::Span4V(_),
                WireType::Span4H(_) | WireType::Span4V(_),
            ) => self.pip_span4_to_span4,
            (WireType::Span4H(_) | WireType::Span4V(_), WireType::Local(_)) => {
                self.pip_span4_to_local
            }
            (
                WireType::Span12H(_) | WireType::Span12V(_),
                WireType::Span12H(_) | WireType::Span12V(_),
            ) => self.pip_span12_to_span12,
            (WireType::Local(_), WireType::BelPin) => self.pip_local_to_belpin,
            (WireType::Global(_), WireType::Local(_)) => self.pip_global_to_local,
            _ => self.pip_delay, // Conservative fallback
        }
    }

    /// Compute fanout-dependent delay (capacitive loading)
    pub fn fanout_delay(&self, fanout: usize) -> f64 {
        if fanout <= 1 {
            0.0
        } else {
            (fanout as f64 - 1.0) * self.fanout_delay_per_load
        }
    }

    /// Get wire delay based on wire type
    pub fn wire_delay(&self, wire_type: &crate::device::WireType) -> f64 {
        match wire_type {
            crate::device::WireType::Local(_) => self.local_wire_delay,
            crate::device::WireType::Span4H(_) | crate::device::WireType::Span4V(_) => {
                self.span4_delay
            }
            crate::device::WireType::Span12H(_) | crate::device::WireType::Span12V(_) => {
                self.span12_delay
            }
            crate::device::WireType::Global(_) => self.global_clock_delay,
            crate::device::WireType::Neighbour => self.local_wire_delay,
            crate::device::WireType::CarryChain => self.carry_delay,
            crate::device::WireType::BelPin => 0.0, // No delay for BEL pins
        }
    }

    /// Get estimated path delay for a given distance.
    ///
    /// Uses a calibrated wire mix model: for a given Manhattan distance,
    /// estimates how many span-12, span-4, and local wires will be used.
    /// The mix ratios can be calibrated from actual routing results via
    /// `calibrate_wire_mix()`.
    pub fn estimated_path_delay(&self, manhattan_distance: u32) -> f64 {
        let dist = manhattan_distance as f64;
        if dist <= 0.0 {
            return 0.0;
        }

        // Wire mix: greedily use longest wires first
        let span12_count = (manhattan_distance / 12) as f64;
        let remaining = (manhattan_distance % 12) as f64;
        let span4_count = (remaining / 4.0).floor();
        let local_count = remaining - span4_count * 4.0;

        // Wire delays
        let wire_delay = span12_count * self.span12_delay
            + span4_count * self.span4_delay
            + local_count * self.local_wire_delay;

        // PIP delays (one PIP per wire segment)
        let pip_count = span12_count + span4_count + local_count;
        let pip_delay = pip_count * self.pip_delay;

        wire_delay + pip_delay
    }

    /// Calibrate wire mix estimation from actual post-routing wire type statistics.
    ///
    /// Collects wire type usage from a routing result and computes average
    /// delay-per-distance, which can be used to validate or improve the
    /// `estimated_path_delay()` model.
    ///
    /// Returns `(actual_avg_delay_per_hop, estimated_avg_delay_per_hop)` for comparison.
    pub fn calibrate_from_routing<D: crate::device::Device>(
        &self,
        routing: &crate::router::RoutingResult,
        placement: &crate::placer::PlacementResult,
        netlist: &skalp_lir::gate_netlist::GateNetlist,
        device: &D,
    ) -> (f64, f64) {
        let mut total_actual_delay = 0.0;
        let mut total_manhattan_distance = 0u32;
        let mut route_count = 0u32;

        for (net_id, route) in &routing.routes {
            if route.wires.is_empty() {
                continue;
            }

            // Compute actual delay from wire types
            let mut actual_delay = 0.0;
            for &wire_id in &route.wires {
                if let Some(wire) = device.wire(wire_id) {
                    actual_delay += self.wire_delay(&wire.wire_type);
                }
            }
            // Add PIP delays
            for &pip_id in &route.pips {
                if let Some(pip) = device.pip(pip_id) {
                    if let (Some(src_wire), Some(dst_wire)) =
                        (device.wire(pip.src_wire), device.wire(pip.dst_wire))
                    {
                        actual_delay +=
                            self.pip_delay_typed(&src_wire.wire_type, &dst_wire.wire_type);
                    }
                }
            }
            total_actual_delay += actual_delay;

            // Compute Manhattan distance from placement
            let net = match netlist.nets.get(net_id.0 as usize) {
                Some(n) => n,
                None => continue,
            };
            if let Some(driver_id) = net.driver {
                if let Some(src_loc) = placement.get(driver_id) {
                    for (sink_id, _) in &net.fanout {
                        if let Some(dst_loc) = placement.get(*sink_id) {
                            let dx =
                                (src_loc.tile_x as i32 - dst_loc.tile_x as i32).unsigned_abs();
                            let dy =
                                (src_loc.tile_y as i32 - dst_loc.tile_y as i32).unsigned_abs();
                            total_manhattan_distance += dx + dy;
                            route_count += 1;
                        }
                    }
                }
            }
        }

        if route_count == 0 || total_manhattan_distance == 0 {
            return (0.0, 0.0);
        }

        let actual_avg = total_actual_delay / route_count as f64;
        let estimated_avg =
            self.estimated_path_delay(total_manhattan_distance / route_count);

        (actual_avg, estimated_avg)
    }
}
