//! The real-device Nexus flow: place, route, and bitstream emission against
//! prjoxide's device data. Needs a prjoxide checkout, so it skips without one.

#![cfg(feature = "nexus-real")]

use skalp_lir::GateNetlist;
use skalp_place_route::device::nexus::prjoxide_load::find_database;
use skalp_place_route::{NexusVariant, PnrConfig};

const LFCPNX_100_IDCODE: [u8; 4] = [0x01, 0x0F, 0x40, 0x43];

fn netlist_with_pads(fixture: &str, top_name: &str) -> GateNetlist {
    let path = std::path::PathBuf::from(format!(
        "{}/tests/fixtures/{fixture}",
        env!("CARGO_MANIFEST_DIR")
    ));
    let ctx = skalp_frontend::parse_and_build_compilation_context(&path).unwrap();
    let mir = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .unwrap();
    let (hier, _) = skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir);
    let library = skalp_lir::get_stdlib_library("nexus").unwrap();
    let mut netlist =
        skalp_lir::synthesize_hierarchical(&hier, &library, skalp_lir::synth::SynthPreset::Quick)
            .flatten();

    let top = mir.modules.iter().find(|m| m.name == top_name).unwrap();
    let constraints: indexmap::IndexMap<_, _> = top
        .ports
        .iter()
        .filter_map(|p| Some((format!("top.{}", p.name), p.physical_constraints.clone()?)))
        .collect();
    skalp_lir::insert_io_buffers(&mut netlist, &library, &constraints);
    netlist
}

#[test]
fn blinker_builds_a_bitstream_for_the_real_device() {
    let Some(db) = find_database() else {
        eprintln!("prjoxide database not found; skipping");
        return;
    };

    let netlist = netlist_with_pads("nexus_blinker.sk", "Blinker");
    let result = skalp_place_route::pnr::place_and_route_nexus_real(
        &netlist,
        NexusVariant::Lfcpnx100,
        PnrConfig::default(),
        &db,
    )
    .expect("the blinker should place and route");

    assert!(result.routing.success);
    assert_eq!(result.routing.unrouted_connections, 0);
    let verify_id = result
        .bitstream
        .data
        .windows(8)
        .any(|w| w[..4] == [0xE2, 0, 0, 0] && w[4..] == LFCPNX_100_IDCODE);
    assert!(verify_id, "bitstream should target the LFCPNX-100");

    // A 25-bit counter comfortably exceeds the 18 MHz oscillator it runs on.
    let timing = result.timing.expect("the real flow reports timing");
    assert!(timing.meets_timing);
    assert_eq!(timing.target_frequency, 18.0);
    assert!(timing.design_frequency > 100.0);
    assert!(!timing.critical_paths.is_empty());

    // The counter's registers sit on one low-skew clock tree, so nothing
    // races through in the fast corner either.
    let hold = timing.hold.expect("hold is checked");
    assert_eq!(hold.violations, 0, "{:?}", hold.paths.first());
    assert!(hold.worst_slack > 0.0);
    assert!(!hold.paths.is_empty());
}

#[test]
fn distributed_ram_places_and_routes_on_the_real_device() {
    let Some(db) = find_database() else {
        eprintln!("prjoxide database not found; skipping");
        return;
    };

    let netlist = netlist_with_pads("nexus_distram.sk", "RamBlink");
    assert!(
        netlist
            .cells
            .iter()
            .any(|c| c.function == Some(skalp_lir::CellFunction::DistRam16x4)),
        "the design should use distributed RAM"
    );
    let result = skalp_place_route::pnr::place_and_route_nexus_real(
        &netlist,
        NexusVariant::Lfcpnx100,
        PnrConfig::default(),
        &db,
    )
    .expect("the RAM design should place and route");
    assert!(result.routing.success);
    assert_eq!(result.routing.unrouted_connections, 0);
}

#[test]
fn block_ram_builds_a_bitstream_for_the_real_device() {
    let Some(db) = find_database() else {
        eprintln!("PRJOXIDE_DB not found — skipping");
        return;
    };
    let netlist = netlist_with_pads("nexus_blockram.sk", "BlockRamBlink");
    assert!(
        netlist.cells.iter().any(|c| c.ram_ports().is_some()),
        "the memory should map to block RAM"
    );
    let result = skalp_place_route::pnr::place_and_route_nexus_real(
        &netlist,
        NexusVariant::Lfcpnx100,
        PnrConfig::default(),
        &db,
    )
    .unwrap();
    assert!(result.routing.success);
    assert!(result.timing.is_some());
}

#[test]
fn multiplier_builds_a_bitstream_for_the_real_device() {
    let Some(db) = find_database() else {
        eprintln!("PRJOXIDE_DB not found — skipping");
        return;
    };
    let netlist = netlist_with_pads("nexus_multiplier.sk", "MulBlink");
    assert!(
        netlist.cells.iter().any(|c| c.cell_type == "MULT18X18"),
        "the product should map to a hard multiplier"
    );
    let result = skalp_place_route::pnr::place_and_route_nexus_real(
        &netlist,
        NexusVariant::Lfcpnx100,
        PnrConfig::default(),
        &db,
    )
    .unwrap();
    assert!(result.routing.success);
}

#[test]
fn bus_port_pads_land_on_their_own_pins() {
    let Some(db) = find_database() else {
        eprintln!("PRJOXIDE_DB not found — skipping");
        return;
    };
    let netlist = netlist_with_pads("nexus_bus_pins.sk", "Bus");
    let result = skalp_place_route::pnr::place_and_route_nexus_real(
        &netlist,
        NexusVariant::Lfcpnx100,
        PnrConfig::default(),
        &db,
    )
    .expect("the LED counter should place and route");
    assert!(result.routing.success);

    // Each of the eight LED pads sits on a different I/O site
    let sites: std::collections::HashSet<_> = netlist
        .cells
        .iter()
        .filter(|c| c.path.starts_with("top.led[") && c.path.ends_with("_obuf"))
        .map(|c| {
            let loc = result.placement.placements[&c.id];
            (loc.tile_x, loc.tile_y, loc.bel_index)
        })
        .collect();
    assert_eq!(sites.len(), 8);
}

/// A ROM preloaded from a hex file reaches the bitstream: the configuration
/// bus writes the block RAM's contents, one set bit per one-bit of the words.
#[test]
fn preloaded_block_ram_contents_reach_the_bitstream() {
    let Some(db) = find_database() else {
        eprintln!("PRJOXIDE_DB not found — skipping");
        return;
    };
    let netlist = netlist_with_pads("nexus_rom_leds.sk", "NexusRomLeds");
    let rams: Vec<_> = netlist
        .cells
        .iter()
        .filter(|c| c.ram_ports().is_some())
        .collect();
    assert_eq!(rams.len(), 1, "the 1024 x 8 ROM takes one block RAM");
    let words = rams[0].memory_init().expect("the block RAM has contents");
    let expected: Vec<u64> = (0..1024u64).map(|a| (a ^ 0xA5) & 0xFF).collect();
    assert_eq!(words, expected);

    let result = skalp_place_route::pnr::place_and_route_nexus_real(
        &netlist,
        NexusVariant::Lfcpnx100,
        PnrConfig::default(),
        &db,
    )
    .unwrap();
    assert!(result.routing.success);

    let bus = skalp_place_route::device::nexus::prjoxide_pack::read_bus_config(
        &db,
        &result.bitstream.data,
    )
    .expect("the bitstream parses");
    let written: u32 = bus.iter().map(|&(_, byte)| byte.count_ones()).sum();
    let ones: u32 = expected.iter().map(|w| w.count_ones()).sum();
    assert_eq!(written, ones, "every one-bit of the contents is written");
}

/// Registers with and without a reset never share a slice: its two
/// flip-flops share one clock, enable and set/reset input, and the bitstream
/// holds a slice without a reset user's set/reset off by inverting it.
#[test]
fn registers_with_and_without_reset_get_their_own_slices() {
    let Some(db) = find_database() else {
        eprintln!("PRJOXIDE_DB not found — skipping");
        return;
    };
    let netlist = netlist_with_pads("nexus_mixed_resets.sk", "MixedResets");
    let sets = skalp_place_route::placer::control_sets::control_sets(&netlist);
    let resets = sets.values().filter(|s| s.reset.is_some()).count();
    assert!(resets > 0, "the counter has a reset");
    assert!(resets < sets.len(), "the shift register has none");

    let result = skalp_place_route::pnr::place_and_route_nexus_real(
        &netlist,
        NexusVariant::Lfcpnx100,
        PnrConfig::default(),
        &db,
    )
    .expect("mixed reset registers should place, route and emit");
    // The placer refuses a slice of mixed control sets, and so does emit
    assert!(result.routing.success);
}

/// A PLL instance: the blackbox `PLL_CORE` cell goes to a corner PLL site,
/// its clock output drives a counter on the clock network, and its
/// registers are written over the configuration bus at the corner's base.
#[test]
fn pll_builds_a_bitstream_for_the_real_device() {
    let Some(db) = find_database() else {
        eprintln!("PRJOXIDE_DB not found — skipping");
        return;
    };
    let mut netlist = netlist_with_pads("nexus_pll.sk", "NexusPll");
    // The CLI drops ports hierarchical synthesis promotes for child instances
    let names: Vec<String> = netlist.nets.iter().map(|n| n.name.clone()).collect();
    let real_port = |net: &skalp_lir::GateNetId| {
        let name = &names[net.0 as usize];
        name.starts_with("top.clk") || name.starts_with("top.led")
    };
    netlist.inputs.retain(real_port);
    netlist.outputs.retain(real_port);
    netlist.cells.retain(|c| {
        !(c.cell_type.ends_with("_OB") || c.cell_type.ends_with("_IB"))
            || c.parameters.contains_key("LOC")
    });
    let pll = netlist
        .cells
        .iter()
        .find(|c| c.cell_type == "BLACKBOX_PLL_CORE")
        .expect("the PLL instance is a blackbox cell");
    assert_eq!(pll.parameters.get("DIVA").map(String::as_str), Some("15"));
    let clkop = pll.outputs[0];
    assert!(
        netlist.cells.iter().any(|c| c.clock == Some(clkop)),
        "the counter is clocked by the PLL output"
    );

    let result = skalp_place_route::pnr::place_and_route_nexus_real(
        &netlist,
        NexusVariant::Lfcpnx100,
        PnrConfig::default(),
        &db,
    )
    .expect("the PLL design should place, route and emit");
    assert!(result.routing.success);

    // Each clock is checked at the frequency its source gives it: 125 MHz
    // from the board pin, 100 MHz from the PLL (VCO 1600 MHz / 16). A
    // summary's period is its slack plus the time its worst path takes.
    let timing = result
        .timing
        .as_ref()
        .expect("the real flow reports timing");
    let mut periods: Vec<f64> = timing
        .clock_summaries
        .iter()
        .map(|c| c.worst_slack + 1000.0 / c.frequency)
        .collect();
    periods.sort_by(f64::total_cmp);
    assert_eq!(periods.len(), 2, "{:?}", timing.clock_summaries);
    assert!((periods[0] - 8.0).abs() < 1e-6, "{periods:?}");
    assert!((periods[1] - 10.0).abs() < 1e-6, "{periods:?}");
    assert_eq!(timing.target_frequency, 125.0, "the tighter clock leads");
    let hold = timing.hold.as_ref().expect("hold is checked");
    assert_eq!(hold.violations, 0, "{:?}", hold.paths.first());
    assert!(hold.worst_slack > 0.0);

    let bus = skalp_place_route::device::nexus::prjoxide_pack::read_bus_config(
        &db,
        &result.bitstream.data,
    )
    .expect("the bitstream parses");
    let pll_bytes: Vec<_> = bus
        .iter()
        .filter(|&&(address, _)| (0x0e00_0000..0x0e08_0000).contains(&address))
        .collect();
    assert!(!pll_bytes.is_empty(), "the PLL registers are written");
}

#[test]
fn wide_lut_builds_a_bitstream_for_the_real_device() {
    let Some(db) = find_database() else {
        eprintln!("PRJOXIDE_DB not found — skipping");
        return;
    };
    let netlist = netlist_with_pads("nexus_wide_lut.sk", "WideLutBlink");
    assert!(
        netlist
            .cells
            .iter()
            .any(|c| c.function == Some(skalp_lir::CellFunction::Lut5)),
        "the five-input XOR should map to a 5-input LUT"
    );
    let result = skalp_place_route::pnr::place_and_route_nexus_real(
        &netlist,
        NexusVariant::Lfcpnx100,
        PnrConfig::default(),
        &db,
    )
    .expect("the wide LUT design should place and route");
    assert!(result.routing.success);
    assert_eq!(result.routing.unrouted_connections, 0);
    let timing = result.timing.expect("the real flow reports timing");
    assert!(timing.meets_timing);
}

/// Inputs that compare against their bank's reference voltage build, and the
/// pad driving that voltage is set up for them.
#[test]
fn reference_voltage_inputs_build_for_the_real_device() {
    let Some(db) = find_database() else {
        eprintln!("PRJOXIDE_DB not found — skipping");
        return;
    };
    let netlist = netlist_with_pads("nexus_vref_input.sk", "VrefInput");
    let result = skalp_place_route::pnr::place_and_route_nexus_real(
        &netlist,
        NexusVariant::Lfcpnx100,
        PnrConfig::default(),
        &db,
    )
    .expect("SSTL15 inputs should build");
    assert!(result.routing.success);
}

/// The pin driving a bank's reference voltage is the bank's: a design signal
/// placed on it is refused, naming the pin.
#[test]
fn a_signal_on_a_reference_voltage_pin_is_refused() {
    let Some(db) = find_database() else {
        eprintln!("PRJOXIDE_DB not found — skipping");
        return;
    };
    let netlist = netlist_with_pads("nexus_vref_pin_taken.sk", "VrefPinTaken");
    let error = skalp_place_route::pnr::place_and_route_nexus_real(
        &netlist,
        NexusVariant::Lfcpnx100,
        PnrConfig::default(),
        &db,
    )
    .expect_err("the reference voltage pin is taken")
    .to_string();
    assert!(error.contains("PB50A"), "{error}");
    assert!(error.contains("reference voltage"), "{error}");
}

/// A bank has one reference voltage, so inputs needing different levels
/// cannot share it.
#[test]
fn mixed_reference_levels_in_one_bank_are_refused() {
    let Some(db) = find_database() else {
        eprintln!("PRJOXIDE_DB not found — skipping");
        return;
    };
    let netlist = netlist_with_pads("nexus_vref_mixed_levels.sk", "VrefMixedLevels");
    let error = skalp_place_route::pnr::place_and_route_nexus_real(
        &netlist,
        NexusVariant::Lfcpnx100,
        PnrConfig::default(),
        &db,
    )
    .expect_err("SSTL15 and SSTL135 need different reference voltages")
    .to_string();
    assert!(error.contains("bank 4"), "{error}");
}
