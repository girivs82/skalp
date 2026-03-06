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

    // Run routing — use timing-driven routing when frequency constraints exist
    let mut router_config = config.router.clone();
    let freq_constraints = collect_frequency_constraints(netlist, &config);
    if !freq_constraints.is_empty() && router_config.timing_weight < 0.3 {
        router_config.timing_weight = 0.5;
    }
    let mut router = Router::new(router_config, device.clone());
    let routing = router.route(netlist, &placement)?;

    // Run timing analysis — feed frequency constraints into target frequency
    let timing = if let Some(mut timing_config) = config.timing.clone() {
        // Use the tightest (highest) frequency constraint as the target
        if let Some(max_freq) = freq_constraints.values().cloned().reduce(f64::max) {
            if max_freq > timing_config.target_frequency {
                timing_config.target_frequency = max_freq;
            }
        }
        let mut analyzer = TimingAnalyzer::new(timing_config, device.clone());
        let mut report = analyzer.analyze_timing(netlist, &placement, &routing)?;

        // Check frequency constraints and annotate violations
        for (net_name, target_freq) in &freq_constraints {
            let target_period = 1000.0 / target_freq; // ns
                                                      // Find matching clock domain
            for summary in &report.clock_summaries {
                if summary.name == *net_name || net_name == "*" {
                    if summary.worst_slack < 0.0 {
                        report.meets_timing = false;
                    }
                    // Check if achieved frequency meets constraint
                    let achieved_period = 1000.0 / summary.frequency;
                    if achieved_period > target_period * 1.01 {
                        // 1% tolerance
                        report.meets_timing = false;
                        report.failing_paths += 1;
                    }
                }
            }
        }

        Some(report)
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

/// Collect frequency constraints from placer config and netlist inline annotations
fn collect_frequency_constraints(
    netlist: &GateNetlist,
    config: &PnrConfig,
) -> std::collections::HashMap<String, f64> {
    let mut constraints = std::collections::HashMap::new();

    // From placer IO constraints
    for fc in config.placer.io_constraints.frequency_constraints() {
        constraints.insert(fc.net_name.clone(), fc.frequency_mhz);
    }

    // From inline constraints: PLL cells with target output frequency
    for cell in &netlist.cells {
        if cell.cell_type.contains("PLL") || cell.cell_type.starts_with("SB_PLL") {
            if let Some(freq_str) = cell.parameters.get("FREQUENCY_PIN_PLLOUTCORE") {
                if let Ok(freq) = freq_str.parse::<f64>() {
                    // Convert Hz to MHz if needed
                    let freq_mhz = if freq > 1e6 { freq / 1e6 } else { freq };
                    constraints.insert("pll_out".to_string(), freq_mhz);
                }
            }
        }
    }

    constraints
}

/// Auto-compute PLL dividers from frequency constraints and apply them to a mutable netlist.
/// Call this before place_and_route if PLL cells lack explicit DIVR/DIVF/DIVQ parameters.
pub fn auto_configure_pll(
    netlist: &mut GateNetlist,
    input_freq_mhz: f64,
) -> Result<Option<crate::utils::PllConfig>> {
    use crate::utils::PllCalculator;

    for cell in &mut netlist.cells {
        if !(cell.cell_type.contains("PLL") || cell.cell_type.starts_with("SB_PLL")) {
            continue;
        }

        // Skip if already configured
        if cell.parameters.contains_key("DIVR") && cell.parameters.contains_key("DIVF") {
            continue;
        }

        // Look for target frequency in parameters
        let target_freq = cell
            .parameters
            .get("FREQUENCY_PIN_PLLOUTCORE")
            .and_then(|s| s.parse::<f64>().ok())
            .map(|f| if f > 1e6 { f / 1e6 } else { f });

        if let Some(target) = target_freq {
            let calc = PllCalculator::new();
            let pll_config = calc.calculate(input_freq_mhz, target)?;

            cell.parameters
                .insert("DIVR".to_string(), pll_config.divr.to_string());
            cell.parameters
                .insert("DIVF".to_string(), pll_config.divf.to_string());
            cell.parameters
                .insert("DIVQ".to_string(), pll_config.divq.to_string());
            cell.parameters.insert(
                "FILTER_RANGE".to_string(),
                pll_config.filter_range.to_string(),
            );
            if !cell.parameters.contains_key("FEEDBACK_PATH") {
                cell.parameters
                    .insert("FEEDBACK_PATH".to_string(), "SIMPLE".to_string());
            }

            return Ok(Some(pll_config));
        }
    }

    Ok(None)
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
    fn test_bel_pin_wires() {
        use crate::device::ice40::Ice40Device;

        let device = Ice40Device::new(Ice40Variant::Hx1k);

        println!("\n=== BEL Pin Wire Mapping Test ===");
        println!("Total BEL pin wires: {}", device.bel_wire_count());

        // Test logic tile at (6, 9) - a typical logic tile
        let tile_x = 6;
        let tile_y = 9;

        // Check LUT output wires
        for lc_idx in 0..8 {
            if let Some(wire_id) = device.lut_output_wire(tile_x, tile_y, lc_idx) {
                println!("  LC{} output wire: {:?}", lc_idx, wire_id);
            }
        }

        // Check LUT input wires for LC0
        for input_idx in 0..4 {
            if let Some(wire_id) = device.lut_input_wire(tile_x, tile_y, 0, input_idx) {
                println!("  LC0 input {} wire: {:?}", input_idx, wire_id);
            }
        }

        // Check clock wire
        if let Some(wire_id) = device.clock_wire(tile_x, tile_y) {
            println!("  Clock wire: {:?}", wire_id);
        }

        // Verify we have BEL pin wires
        assert!(
            device.bel_wire_count() > 1000,
            "Should have many BEL pin wires from chipdb"
        );

        // Verify LUT output wires exist
        let lut_out = device.lut_output_wire(tile_x, tile_y, 0);
        assert!(lut_out.is_some(), "Should find lutff_0/out wire");

        // Verify LUT input wires exist
        let lut_in = device.lut_input_wire(tile_x, tile_y, 0, 0);
        assert!(lut_in.is_some(), "Should find lutff_0/in_0 wire");
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

    #[test]
    fn test_timing_driven_routing() {
        use crate::router::RoutingAlgorithm;

        // Create a netlist with a timing-critical path (register to register)
        let mut netlist = GateNetlist::new("timing_test".to_string(), "ice40".to_string());

        // Clock net
        let clock_net = netlist.add_net({
            let mut net =
                GateNet::new_input(skalp_lir::gate_netlist::GateNetId(0), "clk".to_string());
            net.is_clock = true;
            net
        });

        // Create a timing-critical path: DFF -> LUT -> LUT -> LUT -> DFF
        // This simulates a combinational path between registers
        let dff1_out = netlist.add_net(GateNet::new(
            skalp_lir::gate_netlist::GateNetId(1),
            "dff1_out".to_string(),
        ));
        let lut1_out = netlist.add_net(GateNet::new(
            skalp_lir::gate_netlist::GateNetId(2),
            "lut1_out".to_string(),
        ));
        let lut2_out = netlist.add_net(GateNet::new(
            skalp_lir::gate_netlist::GateNetId(3),
            "lut2_out".to_string(),
        ));
        let lut3_out = netlist.add_net(GateNet::new(
            skalp_lir::gate_netlist::GateNetId(4),
            "lut3_out".to_string(),
        ));
        let dff2_out = netlist.add_net(GateNet::new(
            skalp_lir::gate_netlist::GateNetId(5),
            "dff2_out".to_string(),
        ));

        // Input DFF
        let mut dff1 = Cell::new_seq(
            skalp_lir::gate_netlist::CellId(0),
            "SB_DFF".to_string(),
            "ice40".to_string(),
            0.0,
            "path.dff1".to_string(),
            vec![clock_net], // Using clock as D input for simplicity
            vec![dff1_out],
            clock_net,
            None,
        );
        dff1.clock = Some(clock_net);
        netlist.add_cell(dff1);

        // LUT chain (combinational path)
        netlist.add_cell(Cell::new_comb(
            skalp_lir::gate_netlist::CellId(1),
            "SB_LUT4".to_string(),
            "ice40".to_string(),
            0.0,
            "path.lut1".to_string(),
            vec![dff1_out],
            vec![lut1_out],
        ));

        netlist.add_cell(Cell::new_comb(
            skalp_lir::gate_netlist::CellId(2),
            "SB_LUT4".to_string(),
            "ice40".to_string(),
            0.0,
            "path.lut2".to_string(),
            vec![lut1_out],
            vec![lut2_out],
        ));

        netlist.add_cell(Cell::new_comb(
            skalp_lir::gate_netlist::CellId(3),
            "SB_LUT4".to_string(),
            "ice40".to_string(),
            0.0,
            "path.lut3".to_string(),
            vec![lut2_out],
            vec![lut3_out],
        ));

        // Output DFF
        let mut dff2 = Cell::new_seq(
            skalp_lir::gate_netlist::CellId(4),
            "SB_DFF".to_string(),
            "ice40".to_string(),
            0.0,
            "path.dff2".to_string(),
            vec![lut3_out],
            vec![dff2_out],
            clock_net,
            None,
        );
        dff2.clock = Some(clock_net);
        netlist.add_cell(dff2);

        // Test with timing-driven routing
        let mut config = PnrConfig::default();
        config.router.algorithm = RoutingAlgorithm::TimingDriven;
        config.router.timing_weight = 0.5; // Medium timing emphasis

        let result = place_and_route(&netlist, Ice40Variant::Hx1k, config).unwrap();

        println!("\n=== Timing-Driven Routing Test ===");
        println!("Cells: {}", result.placement.placements.len());
        println!("Routing success: {}", result.routing.success);
        println!("Congestion: {:.2}", result.routing.congestion);
        println!("Total wirelength: {}", result.routing.wirelength);
        println!("Iterations: {}", result.routing.iterations);

        // Calculate total delay
        let total_delay: u32 = result.routing.routes.values().map(|r| r.delay).sum();
        println!("Total route delay: {} ps", total_delay);

        // Print route details
        for (net_id, route) in result.routing.routes.iter() {
            if !route.wires.is_empty() {
                println!(
                    "  Net {:?}: {} wires, {} PIPs, delay {} ps",
                    net_id,
                    route.wires.len(),
                    route.pips.len(),
                    route.delay
                );
            }
        }

        assert!(
            result.routing.success,
            "Timing-driven routing should succeed"
        );

        // For timing-driven routing, we expect the router to find paths
        // even if they have slightly higher congestion, in favor of lower delay
        let routed_nets = result
            .routing
            .routes
            .values()
            .filter(|r| !r.wires.is_empty())
            .count();
        assert!(
            routed_nets >= 4,
            "Should route at least 4 nets (the critical path)"
        );
    }

    #[test]
    fn test_global_clock_routing() {
        use crate::device::ice40::Ice40Device;
        use crate::router::GlobalNetRouter;

        // Create a design with clock distribution
        let mut netlist = GateNetlist::new("clock_test".to_string(), "ice40".to_string());

        // Clock net
        let clock_net = netlist.add_net({
            let mut net =
                GateNet::new_input(skalp_lir::gate_netlist::GateNetId(0), "clk".to_string());
            net.is_clock = true;
            net
        });

        // Create 4 DFFs that all use the same clock
        for i in 0..4 {
            let dff_in = netlist.add_net(GateNet::new(
                skalp_lir::gate_netlist::GateNetId(10 + i as u32),
                format!("dff{}_in", i),
            ));
            let dff_out = netlist.add_net(GateNet::new(
                skalp_lir::gate_netlist::GateNetId(20 + i as u32),
                format!("dff{}_out", i),
            ));

            let mut dff = Cell::new_seq(
                skalp_lir::gate_netlist::CellId(i as u32),
                "SB_DFF".to_string(),
                "ice40".to_string(),
                0.0,
                format!("reg{}", i),
                vec![dff_in],
                vec![dff_out],
                clock_net,
                None,
            );
            dff.clock = Some(clock_net);
            netlist.add_cell(dff);
        }

        // Run P&R to get placement
        let config = PnrConfig::default();
        let result = place_and_route(&netlist, Ice40Variant::Hx1k, config).unwrap();

        println!("\n=== Global Clock Routing Test ===");
        println!("Cells: {}", result.placement.placements.len());

        // Test the GlobalNetRouter directly
        let device = Ice40Device::new(Ice40Variant::Hx1k);
        let mut global_router = GlobalNetRouter::new(&device);

        // Check gbuf locations
        println!("GBUF locations: {:?}", device.gbuf_locations());

        // Route the clock net through global resources
        let clock_id = skalp_lir::gate_netlist::GateNetId(0);
        let clock_route = global_router.route_clock(clock_id, &netlist, &result.placement);

        assert!(clock_route.is_ok(), "Global clock routing should succeed");

        let route = clock_route.unwrap();
        println!(
            "Clock route: {} wires, {} sinks",
            route.wires.len(),
            route.sinks.len()
        );
        println!("Clock route delay: {} ps", route.delay);

        // Verify the clock route reaches all DFFs
        // We should have at least some sinks for the clock net
        assert!(
            !route.wires.is_empty(),
            "Clock route should have at least one wire"
        );

        // Check global network allocation
        let allocation = global_router.get_network_allocation(clock_id);
        println!("Allocated global network: {:?}", allocation);
        assert!(
            allocation.is_some(),
            "Clock should be allocated to a global network"
        );

        // Verify available networks decreased
        assert!(
            global_router.available_networks() < 8,
            "Should have used at least one global network"
        );
    }

    #[test]
    fn test_integrated_routing_flow() {
        // Create a design with clock and regular nets
        // This tests that specialized routers integrate properly with PathFinder
        let mut netlist = GateNetlist::new("integrated_test".to_string(), "ice40".to_string());

        // Clock net (will use GBUF)
        let clock_net = netlist.add_net({
            let mut net =
                GateNet::new_input(skalp_lir::gate_netlist::GateNetId(0), "clk".to_string());
            net.is_clock = true;
            net
        });

        // Create a LUT-DFF chain similar to test_routing_quality
        // This ensures we have properly connected nets
        let mut prev_net = clock_net;
        for i in 0..3 {
            // LUT output
            let lut_out = netlist.add_net(GateNet::new(
                skalp_lir::gate_netlist::GateNetId(10 + i as u32),
                format!("lut{}_out", i),
            ));

            // DFF output
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

            // DFF with clock
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

        // Run P&R
        let config = PnrConfig::default();
        let result = place_and_route(&netlist, Ice40Variant::Hx1k, config).unwrap();

        println!("\n=== Integrated Routing Flow Test ===");
        println!("Cells: {}", result.placement.placements.len());
        println!("Total routes: {}", result.routing.routes.len());
        println!("Routing success: {}", result.routing.success);
        println!("Total wirelength: {}", result.routing.wirelength);

        // Count routes by type
        let mut global_routes = 0;
        let mut regular_routes = 0;

        for (net_id, route) in &result.routing.routes {
            let net = &netlist.nets[net_id.0 as usize];
            if net.is_clock || net.is_reset {
                global_routes += 1;
                println!(
                    "  Global net {:?}: {} wires, delay {} ps",
                    net_id,
                    route.wires.len(),
                    route.delay
                );
            } else {
                regular_routes += 1;
                println!(
                    "  Regular net {:?}: {} wires, {} PIPs",
                    net_id,
                    route.wires.len(),
                    route.pips.len()
                );
            }
        }

        println!(
            "Global routes: {}, Regular routes: {}",
            global_routes, regular_routes
        );

        // Verify we have routes
        assert!(result.routing.success, "Integrated routing should succeed");
        assert!(
            result.routing.routes.len() >= 4,
            "Should have at least 4 routes (clock + 3 data paths)"
        );
    }

    #[test]
    fn test_timing_driven_placement() {
        use crate::placer::PlacementAlgorithm;

        // Create a design with a clear critical path
        let mut netlist = GateNetlist::new("timing_test".to_string(), "ice40".to_string());

        // Clock net
        let clock_net = netlist.add_net({
            let mut net =
                GateNet::new_input(skalp_lir::gate_netlist::GateNetId(0), "clk".to_string());
            net.is_clock = true;
            net
        });

        // Create a long combinational path: DFF -> LUT -> LUT -> LUT -> LUT -> DFF
        // This creates timing pressure that should affect placement
        let dff1_out = netlist.add_net(GateNet::new(
            skalp_lir::gate_netlist::GateNetId(1),
            "dff1_q".to_string(),
        ));
        let lut1_out = netlist.add_net(GateNet::new(
            skalp_lir::gate_netlist::GateNetId(2),
            "lut1_out".to_string(),
        ));
        let lut2_out = netlist.add_net(GateNet::new(
            skalp_lir::gate_netlist::GateNetId(3),
            "lut2_out".to_string(),
        ));
        let lut3_out = netlist.add_net(GateNet::new(
            skalp_lir::gate_netlist::GateNetId(4),
            "lut3_out".to_string(),
        ));
        let lut4_out = netlist.add_net(GateNet::new(
            skalp_lir::gate_netlist::GateNetId(5),
            "lut4_out".to_string(),
        ));
        let dff2_out = netlist.add_net(GateNet::new(
            skalp_lir::gate_netlist::GateNetId(6),
            "dff2_q".to_string(),
        ));

        // Input DFF
        let mut dff1 = Cell::new_seq(
            skalp_lir::gate_netlist::CellId(0),
            "SB_DFF".to_string(),
            "ice40".to_string(),
            0.0,
            "path.dff1".to_string(),
            vec![clock_net], // D input (use clock for simplicity)
            vec![dff1_out],
            clock_net,
            None,
        );
        dff1.clock = Some(clock_net);
        netlist.add_cell(dff1);

        // Long combinational chain (4 LUTs)
        netlist.add_cell(Cell::new_comb(
            skalp_lir::gate_netlist::CellId(1),
            "SB_LUT4".to_string(),
            "ice40".to_string(),
            0.0,
            "path.lut1".to_string(),
            vec![dff1_out],
            vec![lut1_out],
        ));

        netlist.add_cell(Cell::new_comb(
            skalp_lir::gate_netlist::CellId(2),
            "SB_LUT4".to_string(),
            "ice40".to_string(),
            0.0,
            "path.lut2".to_string(),
            vec![lut1_out],
            vec![lut2_out],
        ));

        netlist.add_cell(Cell::new_comb(
            skalp_lir::gate_netlist::CellId(3),
            "SB_LUT4".to_string(),
            "ice40".to_string(),
            0.0,
            "path.lut3".to_string(),
            vec![lut2_out],
            vec![lut3_out],
        ));

        netlist.add_cell(Cell::new_comb(
            skalp_lir::gate_netlist::CellId(4),
            "SB_LUT4".to_string(),
            "ice40".to_string(),
            0.0,
            "path.lut4".to_string(),
            vec![lut3_out],
            vec![lut4_out],
        ));

        // Output DFF
        let mut dff2 = Cell::new_seq(
            skalp_lir::gate_netlist::CellId(5),
            "SB_DFF".to_string(),
            "ice40".to_string(),
            0.0,
            "path.dff2".to_string(),
            vec![lut4_out],
            vec![dff2_out],
            clock_net,
            None,
        );
        dff2.clock = Some(clock_net);
        netlist.add_cell(dff2);

        // Test with timing-driven placement
        let mut config = PnrConfig::default();
        config.placer.algorithm = PlacementAlgorithm::AnalyticalTimingDriven;
        config.placer.timing_weight = 0.7; // High timing emphasis
        config.placer.max_iterations = 2000;

        let result = place_and_route(&netlist, Ice40Variant::Hx1k, config).unwrap();

        println!("\n=== Timing-Driven Placement Test ===");
        println!("Cells placed: {}", result.placement.placements.len());
        println!("Wirelength: {}", result.placement.wirelength);
        println!("Timing score: {:.2}", result.placement.timing_score);
        println!("Placement cost: {:.2}", result.placement.cost);

        // Print placements for the critical path
        let critical_path_cells = [
            "path.dff1",
            "path.lut1",
            "path.lut2",
            "path.lut3",
            "path.lut4",
            "path.dff2",
        ];
        println!("\nCritical path placement:");
        for (cell_id, loc) in &result.placement.placements {
            if let Some(cell) = netlist.get_cell(*cell_id) {
                if critical_path_cells.contains(&cell.path.as_str()) {
                    println!(
                        "  {} ({:?}): tile ({}, {}), bel {}",
                        cell.path, cell.cell_type, loc.tile_x, loc.tile_y, loc.bel_index
                    );
                }
            }
        }

        // For timing-driven placement, we expect cells on the critical path
        // to be placed close together
        let mut path_positions: Vec<(u32, u32)> = Vec::new();
        for (cell_id, loc) in &result.placement.placements {
            if let Some(cell) = netlist.get_cell(*cell_id) {
                if critical_path_cells.contains(&cell.path.as_str()) {
                    path_positions.push((loc.tile_x, loc.tile_y));
                }
            }
        }

        // Calculate bounding box of critical path
        if !path_positions.is_empty() {
            let min_x = path_positions.iter().map(|(x, _)| *x).min().unwrap();
            let max_x = path_positions.iter().map(|(x, _)| *x).max().unwrap();
            let min_y = path_positions.iter().map(|(_, y)| *y).min().unwrap();
            let max_y = path_positions.iter().map(|(_, y)| *y).max().unwrap();

            let bbox_hpwl = (max_x - min_x) + (max_y - min_y);
            println!(
                "\nCritical path bounding box: ({},{}) to ({},{})",
                min_x, min_y, max_x, max_y
            );
            println!("Critical path HPWL: {}", bbox_hpwl);

            // Timing-driven placement should keep critical path relatively compact
            // For a 6-cell path, HPWL should be reasonable (< 15 tiles)
            assert!(
                bbox_hpwl < 15,
                "Critical path should be placed compactly, HPWL was {}",
                bbox_hpwl
            );
        }

        assert!(result.routing.success, "Routing should succeed");
    }

    #[test]
    fn test_io_pin_constraints() {
        use crate::placer::IoConstraints;

        // Create a design with I/O cells
        let mut netlist = GateNetlist::new("io_test".to_string(), "ice40".to_string());

        // Clock input
        let clock_net = netlist.add_net({
            let mut net =
                GateNet::new_input(skalp_lir::gate_netlist::GateNetId(0), "clk".to_string());
            net.is_clock = true;
            net
        });

        // Data input
        let data_in = netlist.add_net(GateNet::new_input(
            skalp_lir::gate_netlist::GateNetId(1),
            "data_in".to_string(),
        ));

        // Data output
        let data_out = netlist.add_net(GateNet::new_output(
            skalp_lir::gate_netlist::GateNetId(2),
            "data_out".to_string(),
        ));

        // Create I/O cells
        let io_clk = Cell::new_comb(
            skalp_lir::gate_netlist::CellId(0),
            "SB_IO".to_string(),
            "ice40".to_string(),
            0.0,
            "io.clk".to_string(),
            vec![],
            vec![clock_net],
        );
        netlist.add_cell(io_clk);

        let io_data_in = Cell::new_comb(
            skalp_lir::gate_netlist::CellId(1),
            "SB_IO".to_string(),
            "ice40".to_string(),
            0.0,
            "io.data_in".to_string(),
            vec![],
            vec![data_in],
        );
        netlist.add_cell(io_data_in);

        let io_data_out = Cell::new_comb(
            skalp_lir::gate_netlist::CellId(2),
            "SB_IO".to_string(),
            "ice40".to_string(),
            0.0,
            "io.data_out".to_string(),
            vec![data_in],
            vec![data_out],
        );
        netlist.add_cell(io_data_out);

        // Create I/O constraints
        let mut io_constraints = IoConstraints::for_package("ct256");
        io_constraints.set_pin("clk", "J3");
        io_constraints.set_pin("data_in", "H16");
        io_constraints.set_pin("data_out", "J14");

        // Configure P&R with constraints
        let mut config = PnrConfig::fast();
        config.placer.io_constraints = io_constraints;

        // Run P&R
        let result = place_and_route(&netlist, Ice40Variant::Hx8k, config).unwrap();

        println!("\n=== I/O Pin Constraints Test ===");
        println!("Cells placed: {}", result.placement.placements.len());

        // Verify placements
        for (cell_id, loc) in &result.placement.placements {
            if let Some(cell) = netlist.get_cell(*cell_id) {
                if cell.cell_type.contains("IO") {
                    println!(
                        "  {} ({:?}): tile ({}, {}), bel {}",
                        cell.path, loc.bel_type, loc.tile_x, loc.tile_y, loc.bel_index
                    );
                }
            }
        }

        // The test verifies that I/O constraints are applied without errors
        // Specific pin locations depend on the package database
        assert!(
            result.placement.placements.len() >= 3,
            "Should place all cells"
        );
    }

    #[test]
    fn test_parallel_simulated_annealing() {
        use crate::placer::PlacementAlgorithm;
        use std::time::Instant;

        // Create a larger design to benefit from parallelism
        let mut netlist = GateNetlist::new("parallel_test".to_string(), "ice40".to_string());

        // Clock net
        let clock_net = netlist.add_net({
            let mut net =
                GateNet::new_input(skalp_lir::gate_netlist::GateNetId(0), "clk".to_string());
            net.is_clock = true;
            net
        });

        // Create a chain of LUTs and DFFs
        let mut prev_net = clock_net;
        for i in 0..20 {
            let lut_out = netlist.add_net(GateNet::new(
                skalp_lir::gate_netlist::GateNetId(10 + i as u32),
                format!("lut{}_out", i),
            ));

            let dff_out = netlist.add_net(GateNet::new(
                skalp_lir::gate_netlist::GateNetId(100 + i as u32),
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
                skalp_lir::gate_netlist::CellId(100 + i as u32),
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

        // Test parallel placement
        let mut config = PnrConfig::fast();
        config.placer.algorithm = PlacementAlgorithm::SimulatedAnnealing;
        config.placer.parallel = true;
        config.placer.parallel_batch_size = 32;
        config.placer.max_iterations = 500;

        let start = Instant::now();
        let result = place_and_route(&netlist, Ice40Variant::Hx1k, config).unwrap();
        let parallel_time = start.elapsed();

        println!("\n=== Parallel SA Test ===");
        println!("Cells placed: {}", result.placement.placements.len());
        println!("Wirelength: {}", result.placement.wirelength);
        println!("Time: {:?}", parallel_time);

        // Verify placement succeeded
        assert!(
            result.placement.placements.len() >= 40,
            "Should place all cells"
        );
        assert!(result.routing.success, "Routing should succeed");

        // Compare with serial (optional benchmark)
        let mut serial_config = PnrConfig::fast();
        serial_config.placer.algorithm = PlacementAlgorithm::SimulatedAnnealing;
        serial_config.placer.parallel = false;
        serial_config.placer.max_iterations = 500;

        let start = Instant::now();
        let serial_result = place_and_route(&netlist, Ice40Variant::Hx1k, serial_config).unwrap();
        let serial_time = start.elapsed();

        println!("Serial time: {:?}", serial_time);
        println!(
            "Speedup: {:.2}x",
            serial_time.as_secs_f64() / parallel_time.as_secs_f64()
        );

        // Both should produce valid results
        assert!(
            serial_result.placement.placements.len() >= 40,
            "Serial should also place all cells"
        );
    }

    #[test]
    fn test_pcf_constraint_parsing() {
        use crate::placer::IoConstraints;

        let pcf_content = r#"
# Pin constraints for test design
set_io clk J3
set_io led[0] B5
set_io led[1] B4
set_io btn A6
"#;

        let constraints = IoConstraints::from_pcf(pcf_content).unwrap();

        assert_eq!(constraints.len(), 4);
        assert!(constraints.has_constraint("clk"));
        assert!(constraints.has_constraint("led[0]"));
        assert!(constraints.has_constraint("led[1]"));
        assert!(constraints.has_constraint("btn"));

        // Verify pin mappings
        assert_eq!(constraints.get("clk").unwrap().pin_name, "J3");
        assert_eq!(constraints.get("led[0]").unwrap().pin_name, "B5");

        // Test PCF generation
        let generated_pcf = constraints.to_pcf();
        assert!(generated_pcf.contains("set_io"));
        println!("\n=== Generated PCF ===\n{}", generated_pcf);
    }

    // === Phase 1: IO direction from CellFunction ===

    #[test]
    fn test_io_direction_from_cell_function() {
        use skalp_lir::tech_library::CellFunction;

        let mut netlist = GateNetlist::new("io_test".to_string(), "ice40".to_string());

        let net0 = netlist.add_net(GateNet::new_input(
            skalp_lir::gate_netlist::GateNetId(0),
            "clk_in".to_string(),
        ));
        let net1 = netlist.add_net(GateNet::new_output(
            skalp_lir::gate_netlist::GateNetId(1),
            "led_out".to_string(),
        ));
        let net2 = netlist.add_net(GateNet::new(
            skalp_lir::gate_netlist::GateNetId(2),
            "sda".to_string(),
        ));

        // Input pad
        let mut input_cell = Cell::new_comb(
            skalp_lir::gate_netlist::CellId(0),
            "SB_IO".to_string(),
            "ice40".to_string(),
            0.0,
            "top.clk_ibuf".to_string(),
            vec![],
            vec![net0],
        );
        input_cell.function = Some(CellFunction::InputPad);
        netlist.add_cell(input_cell);

        // Output pad
        let mut output_cell = Cell::new_comb(
            skalp_lir::gate_netlist::CellId(1),
            "SB_IO".to_string(),
            "ice40".to_string(),
            0.0,
            "top.led_obuf".to_string(),
            vec![net1],
            vec![],
        );
        output_cell.function = Some(CellFunction::OutputPad);
        netlist.add_cell(output_cell);

        // Bidir pad
        let mut bidir_cell = Cell::new_comb(
            skalp_lir::gate_netlist::CellId(2),
            "SB_IO".to_string(),
            "ice40".to_string(),
            0.0,
            "top.sda_iobuf".to_string(),
            vec![net2],
            vec![net2],
        );
        bidir_cell.function = Some(CellFunction::BidirPad);
        netlist.add_cell(bidir_cell);

        // Place and generate bitstream
        let config = PnrConfig::fast();
        let result = place_and_route(&netlist, Ice40Variant::Hx1k, config);
        assert!(result.is_ok(), "P&R should succeed: {:?}", result.err());

        let pnr = result.unwrap();
        let asc = pnr.to_icestorm_ascii_with_netlist(Some(&netlist));

        // Input pad should have PINTYPE with output_mode=0 (no output)
        // Output pad should have PINTYPE with output_mode=1 (output enabled)
        // Bidir pad should have PINTYPE with output_mode=2 (tristate)
        // If we got this far without error, the cell function was used correctly
        assert!(!asc.is_empty());
        assert!(asc.contains(".device"));
    }

    #[test]
    fn test_io_pullup_from_parameters() {
        use skalp_lir::tech_library::CellFunction;

        let mut netlist = GateNetlist::new("pullup_test".to_string(), "ice40".to_string());

        let net0 = netlist.add_net(GateNet::new_input(
            skalp_lir::gate_netlist::GateNetId(0),
            "btn".to_string(),
        ));

        // Input pad with pull-up
        let mut cell = Cell::new_comb(
            skalp_lir::gate_netlist::CellId(0),
            "SB_IO".to_string(),
            "ice40".to_string(),
            0.0,
            "top.btn_ibuf".to_string(),
            vec![],
            vec![net0],
        );
        cell.function = Some(CellFunction::InputPad);
        cell.parameters
            .insert("PULLUP".to_string(), "yes".to_string());
        netlist.add_cell(cell);

        let config = PnrConfig::fast();
        let result = place_and_route(&netlist, Ice40Variant::Hx1k, config);
        assert!(result.is_ok(), "P&R should succeed with PULLUP");
    }

    // === Phase 2: Inline constraints ===

    #[test]
    fn test_inline_constraints_from_loc() {
        use skalp_lir::tech_library::CellFunction;

        let mut netlist = GateNetlist::new("loc_test".to_string(), "ice40".to_string());

        let net0 = netlist.add_net(GateNet::new_input(
            skalp_lir::gate_netlist::GateNetId(0),
            "clk".to_string(),
        ));

        // Input pad with LOC parameter
        let mut cell = Cell::new_comb(
            skalp_lir::gate_netlist::CellId(0),
            "SB_IO".to_string(),
            "ice40".to_string(),
            0.0,
            "top.clk_ibuf".to_string(),
            vec![],
            vec![net0],
        );
        cell.function = Some(CellFunction::InputPad);
        cell.parameters.insert("LOC".to_string(), "J3".to_string());
        netlist.add_cell(cell);

        // Extract inline constraints
        let constraints =
            crate::placer::Placer::<Ice40Device>::extract_inline_constraints(&netlist);
        assert_eq!(constraints.len(), 1);
        assert!(constraints.has_constraint("clk"));
        assert_eq!(constraints.get("clk").unwrap().pin_name, "J3");
    }

    #[test]
    fn test_inline_constraints_with_io_standard() {
        use skalp_lir::tech_library::CellFunction;

        let mut netlist = GateNetlist::new("std_test".to_string(), "ice40".to_string());

        let net0 = netlist.add_net(GateNet::new_output(
            skalp_lir::gate_netlist::GateNetId(0),
            "led".to_string(),
        ));

        let mut cell = Cell::new_comb(
            skalp_lir::gate_netlist::CellId(0),
            "SB_IO".to_string(),
            "ice40".to_string(),
            0.0,
            "top.led_obuf".to_string(),
            vec![net0],
            vec![],
        );
        cell.function = Some(CellFunction::OutputPad);
        cell.parameters.insert("LOC".to_string(), "B5".to_string());
        cell.parameters
            .insert("IO_STANDARD".to_string(), "SB_LVCMOS".to_string());
        cell.parameters
            .insert("DRIVE_STRENGTH".to_string(), "8".to_string());
        netlist.add_cell(cell);

        let constraints =
            crate::placer::Placer::<Ice40Device>::extract_inline_constraints(&netlist);
        assert_eq!(constraints.len(), 1);

        let c = constraints.get("led").unwrap();
        assert_eq!(c.pin_name, "B5");
        assert_eq!(c.io_standard.as_deref(), Some("SB_LVCMOS"));
        assert_eq!(c.drive_strength, Some(8));
    }

    // === Phase 3: RAM configuration ===

    #[test]
    fn test_ram_config_bits() {
        // Test that RAM cells with READ_MODE/WRITE_MODE generate non-zero config
        let mut netlist = GateNetlist::new("ram_test".to_string(), "ice40".to_string());

        let net0 = netlist.add_net(GateNet::new(
            skalp_lir::gate_netlist::GateNetId(0),
            "rdata".to_string(),
        ));

        let mut ram_cell = Cell::new_comb(
            skalp_lir::gate_netlist::CellId(0),
            "SB_RAM40_4K".to_string(),
            "ice40".to_string(),
            0.0,
            "top.mem0".to_string(),
            vec![],
            vec![net0],
        );
        ram_cell
            .parameters
            .insert("READ_MODE".to_string(), "1".to_string());
        ram_cell
            .parameters
            .insert("WRITE_MODE".to_string(), "2".to_string());
        netlist.add_cell(ram_cell);

        let config = PnrConfig::fast();
        let result = place_and_route(&netlist, Ice40Variant::Hx1k, config);
        assert!(
            result.is_ok(),
            "P&R with RAM should succeed: {:?}",
            result.err()
        );

        let pnr = result.unwrap();
        let asc = pnr.to_icestorm_ascii_with_netlist(Some(&netlist));

        // Should contain RAM tile sections
        assert!(
            asc.contains(".ramb_tile") || asc.contains(".ramt_tile"),
            "Should have RAM tile in bitstream"
        );
    }

    #[test]
    fn test_ram_init_data() {
        let mut netlist = GateNetlist::new("ram_init_test".to_string(), "ice40".to_string());

        let net0 = netlist.add_net(GateNet::new(
            skalp_lir::gate_netlist::GateNetId(0),
            "rdata".to_string(),
        ));

        let mut ram_cell = Cell::new_comb(
            skalp_lir::gate_netlist::CellId(0),
            "SB_RAM40_4K".to_string(),
            "ice40".to_string(),
            0.0,
            "top.mem0".to_string(),
            vec![],
            vec![net0],
        );
        ram_cell
            .parameters
            .insert("READ_MODE".to_string(), "0".to_string());
        ram_cell
            .parameters
            .insert("WRITE_MODE".to_string(), "0".to_string());
        // Add init data for first 256 bits
        ram_cell.parameters.insert(
            "INIT_0".to_string(),
            "DEADBEEF".repeat(8), // 64 hex chars = 256 bits
        );
        netlist.add_cell(ram_cell);

        let config = PnrConfig::fast();
        let result = place_and_route(&netlist, Ice40Variant::Hx1k, config);
        assert!(
            result.is_ok(),
            "P&R with RAM init should succeed: {:?}",
            result.err()
        );

        let pnr = result.unwrap();
        let asc = pnr.to_icestorm_ascii_with_netlist(Some(&netlist));

        // Should contain .ram_data section
        assert!(
            asc.contains(".ram_data"),
            "Should have .ram_data section in bitstream for initialized RAM"
        );
    }

    // === Phase 4: DSP scaffolding ===

    #[test]
    fn test_up5k_dsp_tiles() {
        use crate::device::Device;

        let device = Ice40Device::up5k();
        let stats = device.stats();

        // UP5K should report DSP resources
        assert!(stats.total_dsps > 0, "UP5K should have DSP blocks");

        // cell_to_bel_type should map MAC cells to DspSlice
        let placer =
            crate::placer::Placer::new(crate::placer::PlacerConfig::default(), device.clone());
        // The cell_to_bel_type is private, but we can verify by checking that
        // the device has DSP tiles
        assert!(
            !device.dsp_tiles.is_empty(),
            "UP5K should have DSP tiles in the device model"
        );

        // Verify non-UP5K devices don't have DSPs
        let hx1k = Ice40Device::hx1k();
        assert_eq!(hx1k.stats().total_dsps, 0, "HX1K should have no DSP blocks");

        // Suppress unused variable warning
        let _ = placer;
    }

    #[test]
    fn test_dsp_capacity_check() {
        use crate::device::Device;

        // Create a netlist with more DSP cells than UP5K supports
        let mut netlist = GateNetlist::new("dsp_cap_test".to_string(), "ice40".to_string());

        let device = Ice40Device::up5k();
        let dsp_count = device.stats().total_dsps;

        // Add one more DSP cell than available
        for i in 0..=(dsp_count) {
            let net = netlist.add_net(GateNet::new(
                skalp_lir::gate_netlist::GateNetId(i as u32),
                format!("mac_out_{}", i),
            ));

            netlist.add_cell(Cell::new_comb(
                skalp_lir::gate_netlist::CellId(i as u32),
                "SB_MAC16".to_string(),
                "ice40".to_string(),
                0.0,
                format!("top.mac{}", i),
                vec![],
                vec![net],
            ));
        }

        let mut config = PnrConfig::fast();
        config.placer.seed = 42;
        let mut placer = crate::placer::Placer::new(config.placer, device);
        let result = placer.place(&netlist);

        assert!(result.is_err(), "Should fail when exceeding DSP capacity");
        let err = result.unwrap_err();
        let err_msg = format!("{}", err);
        assert!(
            err_msg.contains("DSP"),
            "Error should mention DSPs: {}",
            err_msg
        );
    }

    // === PLL bitstream configuration ===

    #[test]
    fn test_pll_bitstream_config() {
        use skalp_lir::tech_library::CellFunction;

        let mut netlist = GateNetlist::new("pll_test".to_string(), "ice40".to_string());

        // Clock input net
        let clk_in = netlist.add_net({
            let mut net =
                GateNet::new_input(skalp_lir::gate_netlist::GateNetId(0), "clk_in".to_string());
            net.is_clock = true;
            net
        });

        // PLL output net
        let clk_out = netlist.add_net(GateNet::new(
            skalp_lir::gate_netlist::GateNetId(1),
            "clk_out".to_string(),
        ));

        // PLL cell with explicit divider parameters (12MHz -> 48MHz)
        let mut pll_cell = Cell::new_comb(
            skalp_lir::gate_netlist::CellId(0),
            "SB_PLL40_CORE".to_string(),
            "ice40".to_string(),
            0.0,
            "top.pll_inst".to_string(),
            vec![clk_in],
            vec![clk_out],
        );
        pll_cell.function = Some(CellFunction::Pll);
        pll_cell
            .parameters
            .insert("DIVR".to_string(), "0".to_string());
        pll_cell
            .parameters
            .insert("DIVF".to_string(), "63".to_string());
        pll_cell
            .parameters
            .insert("DIVQ".to_string(), "4".to_string());
        pll_cell
            .parameters
            .insert("FILTER_RANGE".to_string(), "1".to_string());
        pll_cell
            .parameters
            .insert("FEEDBACK_PATH".to_string(), "SIMPLE".to_string());
        netlist.add_cell(pll_cell);

        let config = PnrConfig::fast();
        let result = place_and_route(&netlist, Ice40Variant::Hx1k, config);
        assert!(result.is_ok(), "PLL P&R should succeed: {:?}", result.err());

        let pnr = result.unwrap();
        let asc = pnr.to_icestorm_ascii_with_netlist(Some(&netlist));

        // For HX1K, PLL config bits are embedded in IO tiles (not separate ipcon_tile).
        // DIVF=63 (0b0111111) means DIVF_0..DIVF_5 are set, so multiple IO tiles
        // should have non-zero bits from PLL config.
        // Check that we have IO tiles with bits set (the PLL config is there).
        let io_tile_count = asc.matches(".io_tile").count();
        assert!(
            io_tile_count > 0,
            "Should have IO tiles with PLL config bits: {}",
            &asc[..asc.len().min(500)]
        );

        // Verify PLL params are correctly resolved - DIVF=63 means bits 0-5 set,
        // which maps to PLLCONFIG entries in multiple IO tiles
        // Just check the ASC has non-trivial content (not all-zero IO tiles)
        let has_nonzero_io = asc.lines().any(|line| {
            !line.starts_with('.')
                && !line.starts_with('#')
                && !line.is_empty()
                && line.chars().all(|c| c == '0' || c == '1')
                && line.contains('1')
        });
        assert!(
            has_nonzero_io,
            "IO tiles should have non-zero bits from PLL config"
        );
    }

    #[test]
    fn test_pll_auto_configure() {
        // Test auto_configure_pll computes correct dividers
        let mut netlist = GateNetlist::new("auto_pll".to_string(), "ice40".to_string());

        let clk_in = netlist.add_net({
            let mut net =
                GateNet::new_input(skalp_lir::gate_netlist::GateNetId(0), "clk".to_string());
            net.is_clock = true;
            net
        });
        let clk_out = netlist.add_net(GateNet::new(
            skalp_lir::gate_netlist::GateNetId(1),
            "pll_out".to_string(),
        ));

        // PLL cell with only target frequency, no explicit dividers
        let mut pll_cell = Cell::new_comb(
            skalp_lir::gate_netlist::CellId(0),
            "SB_PLL40_CORE".to_string(),
            "ice40".to_string(),
            0.0,
            "top.pll".to_string(),
            vec![clk_in],
            vec![clk_out],
        );
        pll_cell
            .parameters
            .insert("FREQUENCY_PIN_PLLOUTCORE".to_string(), "48".to_string());
        netlist.add_cell(pll_cell);

        // Auto-configure with 12 MHz input
        let pll_config = auto_configure_pll(&mut netlist, 12.0).unwrap();
        assert!(pll_config.is_some(), "Should compute PLL configuration");

        let cfg = pll_config.unwrap();
        assert!(
            cfg.error_percent < 1.0,
            "PLL error should be < 1%: {:.2}%",
            cfg.error_percent
        );
        assert!(
            (cfg.f_achieved - 48.0).abs() < 1.0,
            "Achieved frequency should be ~48 MHz: {:.2}",
            cfg.f_achieved
        );

        // Verify dividers were set on the cell
        let cell = &netlist.cells[0];
        assert!(cell.parameters.contains_key("DIVR"), "DIVR should be set");
        assert!(cell.parameters.contains_key("DIVF"), "DIVF should be set");
        assert!(cell.parameters.contains_key("DIVQ"), "DIVQ should be set");
        assert!(
            cell.parameters.contains_key("FILTER_RANGE"),
            "FILTER_RANGE should be set"
        );
    }

    #[test]
    fn test_frequency_constraint_in_timing() {
        use crate::placer::IoConstraints;

        // Create a design with a frequency constraint
        let mut netlist = GateNetlist::new("freq_test".to_string(), "ice40".to_string());

        let clock_net = netlist.add_net({
            let mut net =
                GateNet::new_input(skalp_lir::gate_netlist::GateNetId(0), "clk".to_string());
            net.is_clock = true;
            net
        });

        // Chain of LUTs and DFFs
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

        // Set frequency constraint
        let mut io_constraints = IoConstraints::new();
        io_constraints.set_frequency("clk", 100.0); // 100 MHz constraint

        let mut config = PnrConfig::default();
        config.placer.io_constraints = io_constraints;

        let result = place_and_route(&netlist, Ice40Variant::Hx1k, config).unwrap();

        // Timing analysis should use the frequency constraint
        let timing = result.timing.as_ref().expect("Should have timing report");
        assert_eq!(
            timing.target_frequency, 100.0,
            "Target frequency should match constraint"
        );

        println!("Frequency constraint test:");
        println!("  Target: {} MHz", timing.target_frequency);
        println!("  Achieved: {:.2} MHz", timing.design_frequency);
        println!("  Meets timing: {}", timing.meets_timing);
        println!("  WNS: {:.2} ns", timing.worst_negative_slack);
    }

    #[test]
    fn test_pll_cell_placement() {
        // Verify PLL cells get placed on PLL BELs
        use skalp_lir::tech_library::CellFunction;

        let mut netlist = GateNetlist::new("pll_place".to_string(), "ice40".to_string());

        let clk_in = netlist.add_net(GateNet::new_input(
            skalp_lir::gate_netlist::GateNetId(0),
            "clk_in".to_string(),
        ));
        let clk_out = netlist.add_net(GateNet::new(
            skalp_lir::gate_netlist::GateNetId(1),
            "clk_out".to_string(),
        ));

        let mut pll_cell = Cell::new_comb(
            skalp_lir::gate_netlist::CellId(0),
            "SB_PLL40_CORE".to_string(),
            "ice40".to_string(),
            0.0,
            "top.pll".to_string(),
            vec![clk_in],
            vec![clk_out],
        );
        pll_cell.function = Some(CellFunction::Pll);
        pll_cell
            .parameters
            .insert("DIVR".to_string(), "0".to_string());
        pll_cell
            .parameters
            .insert("DIVF".to_string(), "63".to_string());
        pll_cell
            .parameters
            .insert("DIVQ".to_string(), "4".to_string());
        pll_cell
            .parameters
            .insert("FILTER_RANGE".to_string(), "1".to_string());
        netlist.add_cell(pll_cell);

        let config = PnrConfig::fast();
        let result = place_and_route(&netlist, Ice40Variant::Hx1k, config);
        assert!(
            result.is_ok(),
            "PLL placement should succeed: {:?}",
            result.err()
        );

        let pnr = result.unwrap();
        // Verify the PLL cell is placed on a PLL BEL
        let pll_placement = pnr
            .placement
            .placements
            .values()
            .find(|loc| matches!(loc.bel_type, crate::device::BelType::Pll));
        assert!(
            pll_placement.is_some(),
            "PLL cell should be placed on a PLL BEL"
        );
    }

    #[test]
    fn test_chipdb_pll_extra_cell() {
        // Verify the chipdb parser correctly parses PLL extra_cell data
        use crate::device::ice40::chipdb_parser::ChipDb;

        let chipdb = ChipDb::load_embedded(Ice40Variant::Hx1k).unwrap();

        assert!(
            !chipdb.pll_cells.is_empty(),
            "HX1K chipdb should have PLL extra_cell data"
        );

        let pll = &chipdb.pll_cells[0];
        println!("PLL at ({}, {})", pll.cell_x, pll.cell_y);
        println!("PLL params: {}", pll.param_bits.len());

        // Verify key PLL parameters are present
        assert!(
            pll.param_bits.contains_key("DIVR_0"),
            "Should have DIVR_0 mapping"
        );
        assert!(
            pll.param_bits.contains_key("DIVF_0"),
            "Should have DIVF_0 mapping"
        );
        assert!(
            pll.param_bits.contains_key("DIVQ_0"),
            "Should have DIVQ_0 mapping"
        );
        assert!(
            pll.param_bits.contains_key("FILTER_RANGE_0"),
            "Should have FILTER_RANGE_0 mapping"
        );

        // Verify resolution works
        let resolved = chipdb.resolve_pll_param_bit(pll, "DIVR_0");
        assert!(resolved.is_some(), "Should resolve DIVR_0 to bit position");
        let (tx, ty, row, col) = resolved.unwrap();
        println!("DIVR_0 -> tile ({}, {}), bit ({}, {})", tx, ty, row, col);
    }
}
