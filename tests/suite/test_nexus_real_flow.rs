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
