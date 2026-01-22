//! Top-Level Place and Route Orchestration
//!
//! Provides the high-level API for running the complete P&R flow.

use crate::bitstream::{Bitstream, BitstreamConfig, BitstreamGenerator};
use crate::device::ice40::{Ice40Device, Ice40Variant};
use crate::error::Result;
use crate::placer::{PlacementResult, Placer, PlacerConfig};
use crate::router::{Router, RouterConfig, RoutingResult};
use crate::timing::{TimingAnalyzer, TimingConfig, TimingReport};
use skalp_lir::gate_netlist::GateNetlist;

/// P&R configuration
#[derive(Debug, Clone)]
pub struct PnrConfig {
    /// Placer configuration
    pub placer: PlacerConfig,
    /// Router configuration
    pub router: RouterConfig,
    /// Bitstream configuration
    pub bitstream: BitstreamConfig,
    /// Timing configuration (optional)
    pub timing: Option<TimingConfig>,
}

impl Default for PnrConfig {
    fn default() -> Self {
        Self {
            placer: PlacerConfig::default(),
            router: RouterConfig::default(),
            bitstream: BitstreamConfig::default(),
            timing: Some(TimingConfig::default()),
        }
    }
}

impl PnrConfig {
    /// Create config for fast compilation (lower quality)
    pub fn fast() -> Self {
        Self {
            placer: PlacerConfig {
                max_iterations: 1000,
                initial_temperature: 50.0,
                ..Default::default()
            },
            router: RouterConfig {
                max_iterations: 20,
                ..Default::default()
            },
            bitstream: BitstreamConfig::default(),
            timing: None,
        }
    }

    /// Create config for high quality (slower)
    pub fn high_quality() -> Self {
        Self {
            placer: PlacerConfig {
                max_iterations: 10000,
                initial_temperature: 200.0,
                cooling_rate: 0.99,
                ..Default::default()
            },
            router: RouterConfig {
                max_iterations: 100,
                history_cost_factor: 0.7,
                ..Default::default()
            },
            bitstream: BitstreamConfig::default(),
            timing: Some(TimingConfig {
                max_critical_paths: 20,
                ..Default::default()
            }),
        }
    }
}

/// P&R result
#[derive(Debug, Clone)]
pub struct PnrResult {
    /// Placement result
    pub placement: PlacementResult,
    /// Routing result
    pub routing: RoutingResult,
    /// Generated bitstream
    pub bitstream: Bitstream,
    /// Timing report (if enabled)
    pub timing: Option<TimingReport>,
    /// Device variant used
    pub variant: Ice40Variant,
}

impl PnrResult {
    /// Generate IceStorm ASCII format bitstream
    pub fn to_icestorm_ascii(&self) -> String {
        self.to_icestorm_ascii_with_netlist(None)
    }

    /// Generate IceStorm ASCII format bitstream with LUT init values from netlist
    pub fn to_icestorm_ascii_with_netlist(&self, netlist: Option<&GateNetlist>) -> String {
        let device = Ice40Device::new(self.variant);
        let ascii_gen = crate::bitstream::IceStormAscii::new(&device);
        ascii_gen
            .generate(&self.placement, &self.routing, netlist)
            .unwrap_or_else(|e| format!(".comment Error generating ASCII: {}\n", e))
    }
}

/// Run place and route on a GateNetlist for iCE40
pub fn place_and_route(
    netlist: &GateNetlist,
    variant: Ice40Variant,
    config: PnrConfig,
) -> Result<PnrResult> {
    // Create device
    let device = Ice40Device::new(variant);

    // Run placement
    let mut placer = Placer::new(config.placer.clone(), device.clone());
    let placement = placer.place(netlist)?;

    // Run routing
    let mut router = Router::new(config.router.clone(), device.clone());
    let routing = router.route(netlist, &placement)?;

    // Run timing analysis if configured
    let timing = if let Some(timing_config) = config.timing.clone() {
        let mut analyzer = TimingAnalyzer::new(timing_config, device.clone());
        Some(analyzer.analyze_timing(netlist, &placement, &routing)?)
    } else {
        None
    };

    // Generate bitstream (with netlist for LUT init values)
    let generator = BitstreamGenerator::with_config(device.clone(), config.bitstream.clone());
    let bitstream = generator.generate_with_netlist(&placement, &routing, Some(netlist))?;

    Ok(PnrResult {
        placement,
        routing,
        bitstream,
        timing,
        variant,
    })
}

/// Convenience function for HX1K
pub fn place_and_route_hx1k(netlist: &GateNetlist, config: PnrConfig) -> Result<PnrResult> {
    place_and_route(netlist, Ice40Variant::Hx1k, config)
}

/// Convenience function for HX8K
pub fn place_and_route_hx8k(netlist: &GateNetlist, config: PnrConfig) -> Result<PnrResult> {
    place_and_route(netlist, Ice40Variant::Hx8k, config)
}

/// Convenience function for UP5K
pub fn place_and_route_up5k(netlist: &GateNetlist, config: PnrConfig) -> Result<PnrResult> {
    place_and_route(netlist, Ice40Variant::Up5k, config)
}

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_lir::gate_netlist::{Cell, GateNet};

    fn create_simple_netlist() -> GateNetlist {
        let mut netlist = GateNetlist::new("test".to_string(), "ice40".to_string());

        // Create input net
        let input_net_id = netlist.add_net(GateNet::new_input(
            skalp_lir::gate_netlist::GateNetId(0),
            "in".to_string(),
        ));

        // Create output net
        let output_net_id = netlist.add_net(GateNet::new_output(
            skalp_lir::gate_netlist::GateNetId(1),
            "out".to_string(),
        ));

        // Create a simple LUT cell
        let cell = Cell::new_comb(
            skalp_lir::gate_netlist::CellId(0),
            "SB_LUT4".to_string(),
            "ice40".to_string(),
            0.0,
            "top.lut0".to_string(),
            vec![input_net_id],
            vec![output_net_id],
        );
        netlist.add_cell(cell);

        netlist
    }

    #[test]
    fn test_basic_pnr_flow() {
        let netlist = create_simple_netlist();
        let config = PnrConfig::fast();

        let result = place_and_route(&netlist, Ice40Variant::Hx1k, config);
        assert!(result.is_ok());

        let pnr_result = result.unwrap();
        assert!(!pnr_result.placement.placements.is_empty());
        assert!(!pnr_result.bitstream.data.is_empty());
    }

    #[test]
    fn test_routing_quality() {
        // Create a more complex netlist to test routing
        let mut netlist = GateNetlist::new("counter".to_string(), "ice40".to_string());

        // Clock net
        let clock_net = netlist.add_net({
            let mut net =
                GateNet::new_input(skalp_lir::gate_netlist::GateNetId(0), "clk".to_string());
            net.is_clock = true;
            net
        });

        // Create a chain of 4 LUT-DFF pairs
        let mut prev_net = clock_net; // Start signal
        for i in 0..4 {
            // Internal net from LUT to DFF
            let lut_out = netlist.add_net(GateNet::new(
                skalp_lir::gate_netlist::GateNetId(10 + i as u32),
                format!("lut{}_out", i),
            ));

            // Output from DFF (feedback or output)
            let dff_out = netlist.add_net(GateNet::new(
                skalp_lir::gate_netlist::GateNetId(20 + i as u32),
                format!("dff{}_out", i),
            ));

            // LUT
            netlist.add_cell(Cell::new_comb(
                skalp_lir::gate_netlist::CellId(i as u32),
                "SB_LUT4".to_string(),
                "ice40".to_string(),
                0.0,
                format!("chain.lut{}", i),
                vec![prev_net],
                vec![lut_out],
            ));

            // DFF
            let mut dff = Cell::new_seq(
                skalp_lir::gate_netlist::CellId(10 + i as u32),
                "SB_DFF".to_string(),
                "ice40".to_string(),
                0.0,
                format!("chain.dff{}", i),
                vec![lut_out],
                vec![dff_out],
                clock_net,
                None, // No reset
            );
            dff.clock = Some(clock_net);
            netlist.add_cell(dff);

            prev_net = dff_out;
        }

        let config = PnrConfig::default();
        let result = place_and_route(&netlist, Ice40Variant::Hx1k, config).unwrap();

        println!("\n=== Routing Quality Test ===");
        println!("Cells: {}", result.placement.placements.len());
        println!("Routing success: {}", result.routing.success);
        println!("Congestion: {:.2}", result.routing.congestion);
        println!("Iterations: {}", result.routing.iterations);

        let routed = result
            .routing
            .routes
            .values()
            .filter(|r| !r.wires.is_empty())
            .count();
        let total = result.routing.routes.len();
        println!("Routed nets: {}/{}", routed, total);

        // Print route details for all routes
        for (net_id, route) in result.routing.routes.iter() {
            if !route.wires.is_empty() || !route.pips.is_empty() {
                println!(
                    "  Net {:?}: {} wires, {} PIPs",
                    net_id,
                    route.wires.len(),
                    route.pips.len()
                );
            }
        }

        assert!(result.routing.success, "Routing should succeed");
        assert!(
            result.routing.congestion < 5.0,
            "Congestion should be acceptable"
        );
    }

    #[test]
    fn test_pip_connectivity() {
        use crate::device::ice40::Ice40Device;
        use crate::device::Device;

        let device = Ice40Device::new(Ice40Variant::Hx1k);

        println!("\n=== PIP Connectivity Test ===");
        println!("Total wires: {}", device.wire_count());
        println!("Total PIPs: {}", device.pip_count());

        // Check a logic tile
        let tile_wires = device.tile_wires(5, 5);
        println!("Wires at tile (5,5): {}", tile_wires.len());

        let mut wires_with_src_pips = 0;
        let mut wires_with_dst_pips = 0;

        for &wire_id in &tile_wires {
            let src_pips = device.wire_src_pips(wire_id);
            let dst_pips = device.wire_pips(wire_id);
            if !src_pips.is_empty() {
                wires_with_src_pips += 1;
            }
            if !dst_pips.is_empty() {
                wires_with_dst_pips += 1;
            }
        }

        println!("Wires with src PIPs (can drive): {}", wires_with_src_pips);
        println!(
            "Wires with dst PIPs (can be driven): {}",
            wires_with_dst_pips
        );

        // The chipdb should have PIPs for most wires
        assert!(
            device.pip_count() > 10000,
            "Should have many PIPs from chipdb"
        );
    }

    #[test]
    fn test_bitstream_routing_bits() {
        // Create a counter netlist that will have routing
        let mut netlist = GateNetlist::new("counter".to_string(), "ice40".to_string());

        // Clock net
        let clock_net = netlist.add_net({
            let mut net =
                GateNet::new_input(skalp_lir::gate_netlist::GateNetId(0), "clk".to_string());
            net.is_clock = true;
            net
        });

        // Create a chain of 4 LUT-DFF pairs
        let mut prev_net = clock_net;
        for i in 0..4 {
            let lut_out = netlist.add_net(GateNet::new(
                skalp_lir::gate_netlist::GateNetId(10 + i as u32),
                format!("lut{}_out", i),
            ));

            let dff_out = netlist.add_net(GateNet::new(
                skalp_lir::gate_netlist::GateNetId(20 + i as u32),
                format!("dff{}_out", i),
            ));

            netlist.add_cell(Cell::new_comb(
                skalp_lir::gate_netlist::CellId(i as u32),
                "SB_LUT4".to_string(),
                "ice40".to_string(),
                0.0,
                format!("chain.lut{}", i),
                vec![prev_net],
                vec![lut_out],
            ));

            let mut dff = Cell::new_seq(
                skalp_lir::gate_netlist::CellId(10 + i as u32),
                "SB_DFF".to_string(),
                "ice40".to_string(),
                0.0,
                format!("chain.dff{}", i),
                vec![lut_out],
                vec![dff_out],
                clock_net,
                None,
            );
            dff.clock = Some(clock_net);
            netlist.add_cell(dff);

            prev_net = dff_out;
        }

        let config = PnrConfig::default();
        let result = place_and_route(&netlist, Ice40Variant::Hx1k, config).unwrap();

        println!("\n=== Bitstream Routing Test ===");
        println!("Routing success: {}", result.routing.success);

        // Count PIPs used in routing
        let total_pips: usize = result.routing.routes.values().map(|r| r.pips.len()).sum();
        println!("Total PIPs used in routing: {}", total_pips);

        // Generate ASCII bitstream with netlist for LUT init values
        let asc = result.to_icestorm_ascii_with_netlist(Some(&netlist));

        // Check that the bitstream has content
        assert!(!asc.is_empty(), "Bitstream should not be empty");
        assert!(asc.contains(".device 1k"), "Should have device header");

        // Print first few logic tiles to see the bit patterns
        let lines: Vec<&str> = asc.lines().collect();
        let mut tile_count = 0;
        let mut in_tile = false;
        let mut tile_bits = Vec::new();

        for line in &lines {
            if line.starts_with(".logic_tile") {
                tile_count += 1;
                in_tile = true;
                tile_bits.clear();
                println!("\n{}", line);
            } else if in_tile {
                if line.is_empty() {
                    // End of tile
                    in_tile = false;
                    // Count non-zero bits
                    let total_bits: usize = tile_bits
                        .iter()
                        .map(|s: &&str| s.chars().filter(|&c| c == '1').count())
                        .sum();
                    if total_bits > 0 {
                        println!("  Active bits: {}", total_bits);
                    }
                } else {
                    tile_bits.push(*line);
                    // Only print rows with non-zero bits
                    if line.contains('1') {
                        println!(
                            "  Row: {} ({} bits set)",
                            tile_bits.len() - 1,
                            line.chars().filter(|&c| c == '1').count()
                        );
                    }
                }
            }

            // Only show first 5 logic tiles
            if tile_count > 5 && !in_tile {
                break;
            }
        }

        // Verify that we have at least some active bits (LUT init or routing)
        let total_ones: usize = asc
            .lines()
            .filter(|l| !l.starts_with('.') && !l.is_empty())
            .map(|l| l.chars().filter(|&c| c == '1').count())
            .sum();
        println!("\nTotal '1' bits in bitstream: {}", total_ones);

        assert!(
            total_ones > 0,
            "Bitstream should have some active configuration bits"
        );
    }
}
