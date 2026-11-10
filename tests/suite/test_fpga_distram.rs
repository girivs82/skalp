//! FPGA distributed RAM (Lattice `DPR16X4` on Nexus, `DPR16X4C` on ECP5).
//!
//! A memory read combinationally (a register file) cannot live in block RAM,
//! whose reads are synchronous. Where the target has 16 × 4 distributed RAM
//! with an asynchronous read port, it goes there instead of registers. These
//! tests check the cells are used and the gate-level netlist matches the
//! source.

use skalp_lir::{CellFunction, GateNetlist};
use skalp_testing::Testbench;

fn fixture_path(name: &str) -> String {
    format!("{}/tests/fixtures/{}", env!("CARGO_MANIFEST_DIR"), name)
}

fn synthesize(fixture: &str, library: &str) -> GateNetlist {
    let path = std::path::PathBuf::from(fixture_path(fixture));
    let ctx = skalp_frontend::parse_and_build_compilation_context(&path).unwrap();
    let mir = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .unwrap();
    let (hier, _) = skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir);
    let library = skalp_lir::get_stdlib_library(library).unwrap();
    skalp_lir::synthesize_hierarchical(&hier, &library, skalp_lir::synth::SynthPreset::Balanced)
        .flatten()
}

fn count(net: &GateNetlist, f: CellFunction) -> usize {
    net.cells
        .iter()
        .filter(|c| c.function == Some(f.clone()))
        .count()
}

#[test]
fn test_register_files_use_distributed_ram() {
    // 32 x 64, two read ports: per port 2 banks x 16 slices
    for (lib, cell) in [("nexus", "DPR16X4"), ("ecp5", "DPR16X4C")] {
        let net = synthesize("fpga_distram_regfile.sk", lib);
        assert_eq!(count(&net, CellFunction::DistRam16x4), 64, "{lib}");
        assert_eq!(count(&net, CellFunction::Ram), 0, "{lib}: no block RAM");
        let ffs = net
            .cells
            .iter()
            .filter(|c| c.is_sequential() && c.function != Some(CellFunction::DistRam16x4))
            .count();
        assert_eq!(ffs, 0, "{lib}: the memory holds no flip-flops");
        for c in net
            .cells
            .iter()
            .filter(|c| c.function == Some(CellFunction::DistRam16x4))
        {
            assert_eq!(c.cell_type, cell);
            assert_eq!((c.inputs.len(), c.outputs.len()), (13, 4), "{}", c.path);
            assert!(c.clock.is_some(), "{} needs its write clock", c.path);
        }
        // 8 x 32, two read ports: one bank of 8 slices per port
        let net = synthesize("regfile_two_reads.sk", lib);
        assert_eq!(count(&net, CellFunction::DistRam16x4), 16, "{lib}: 8 x 32");
    }
}

#[test]
fn test_targets_without_distributed_ram_use_registers() {
    for lib in ["ice40", "generic_asic"] {
        let net = synthesize("fpga_distram_regfile.sk", lib);
        assert_eq!(count(&net, CellFunction::DistRam16x4), 0, "{lib}");
    }
}

#[test]
fn test_distributed_ram_verilog_ports() {
    let v = synthesize("fpga_distram_regfile.sk", "nexus").to_verilog();
    assert!(
        v.contains("DPR16X4 #(.INITVAL("),
        "DPR16X4 instances carry INITVAL"
    );
    for port in [".DI({", ".WAD({", ".WRE(", ".RAD({", ".WCK(", ".DO({"] {
        assert!(v.contains(port), "missing {port}");
    }
}

/// Write every register with a distinct value, then read all pairs back,
/// interleaving writes and reads so a read in the cycle of a write to the
/// same register sees the old value until the clock edge.
async fn check_regfile(mut tb: Testbench, what: &str) {
    let val = |i: u64| (i << 56) ^ (i.wrapping_mul(0x9E37_79B9_7F4A_7C15) >> 8) ^ 0x55;
    for i in 0..32u64 {
        tb.set("we", 1u8).set("wa", i as u8).set("wd", val(i));
        tb.clock(1).await;
    }
    tb.set("we", 0u8);
    for i in 0..32u64 {
        let j = (i * 7 + 3) % 32;
        tb.set("ra", i as u8).set("rb", j as u8);
        tb.clock(1).await;
        let exp = |r: u64| if r == 0 { 0 } else { val(r) };
        assert_eq!(tb.get_u64("qa").await, exp(i), "{what}: qa = x{i}");
        assert_eq!(tb.get_u64("qb").await, exp(j), "{what}: qb = x{j}");
    }
    // Overwrite x5 while reading it: the old value until the edge
    tb.set("we", 1u8).set("wa", 5u8).set("wd", 0xDEAD_BEEF_u64);
    tb.set("ra", 5u8).set("rb", 21u8);
    tb.clock(1).await;
    assert_eq!(
        tb.get_u64("qa").await,
        0xDEAD_BEEF,
        "{what}: x5 after write"
    );
    assert_eq!(tb.get_u64("qb").await, val(21), "{what}: x21 untouched");
    // A write to x0 is stored but x0 still reads as zero
    tb.set("wa", 0u8).set("wd", 0x1234u64).set("ra", 0u8);
    tb.clock(1).await;
    tb.set("we", 0u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u64("qa").await, 0, "{what}: x0");
}

#[tokio::test]
async fn test_regfile_behavioral() {
    let tb = Testbench::with_top_module(&fixture_path("fpga_distram_regfile.sk"), "RegFile")
        .await
        .unwrap();
    check_regfile(tb, "behavioral").await;
}

#[tokio::test]
async fn test_regfile_gate_level_nexus() {
    let tb = Testbench::gate_level_with_top_and_library(
        &fixture_path("fpga_distram_regfile.sk"),
        "RegFile",
        "nexus",
    )
    .await
    .unwrap();
    check_regfile(tb, "nexus").await;
}

#[tokio::test]
async fn test_regfile_gate_level_ecp5() {
    let tb = Testbench::gate_level_with_top_and_library(
        &fixture_path("fpga_distram_regfile.sk"),
        "RegFile",
        "ecp5",
    )
    .await
    .unwrap();
    check_regfile(tb, "ecp5").await;
}

#[tokio::test]
async fn test_regfile_gate_level_generic() {
    let tb = Testbench::gate_level_with_top_and_library(
        &fixture_path("fpga_distram_regfile.sk"),
        "RegFile",
        "generic_asic",
    )
    .await
    .unwrap();
    check_regfile(tb, "generic_asic").await;
}

/// Place and route the 32 x 64 register file and check every RAM cell sits on
/// a distributed-RAM site whose LUTs (which the RAM uses) hold no other cell,
/// and that every net is routed.
fn check_distram_placement<D: skalp_place_route::Device>(
    device: &D,
    net: &GateNetlist,
    result: &skalp_place_route::PnrResult,
    lib: &str,
) {
    use skalp_place_route::BelType;
    let placements = &result.placement.placements;
    let mut used = std::collections::HashMap::new();
    for (id, loc) in placements {
        used.insert((loc.tile_x, loc.tile_y, loc.bel_index), *id);
    }
    let mut rams = 0;
    for cell in net
        .cells
        .iter()
        .filter(|c| c.function == Some(CellFunction::DistRam16x4))
    {
        let loc = placements
            .get(&cell.id)
            .unwrap_or_else(|| panic!("{lib}: {} not placed", cell.path));
        let tile = device.tile_at(loc.tile_x, loc.tile_y).unwrap();
        assert_eq!(
            tile.bels()[loc.bel_index].bel_type,
            BelType::DistRam,
            "{lib}: {} on a RAM site",
            cell.path
        );
        let footprint = device.bel_footprint(loc.tile_x, loc.tile_y, loc.bel_index);
        assert!(!footprint.is_empty(), "{lib}: a RAM site uses LUTs");
        for lut in footprint {
            assert!(
                !used.contains_key(&(loc.tile_x, loc.tile_y, lut)),
                "{lib}: a cell sits on LUT {lut} under {}",
                cell.path
            );
        }
        for out in &cell.outputs {
            assert!(
                result.routing.routes.contains_key(out),
                "{lib}: {} output net routed",
                cell.path
            );
        }
        rams += 1;
    }
    assert_eq!(rams, 64, "{lib}");
    assert_eq!(
        result.routing.unrouted_connections, 0,
        "{lib}: every connection is routed"
    );
    assert!(result.routing.success, "{lib}: every net is routed");
}

#[test]
fn test_distram_place_and_route_nexus() {
    let net = synthesize("fpga_distram_regfile.sk", "nexus");
    let variant = skalp_place_route::NexusVariant::Lifcl40;
    let result =
        skalp_place_route::place_and_route_nexus(&net, variant, Default::default()).unwrap();
    let device = skalp_place_route::NexusDevice::new(variant);
    check_distram_placement(&device, &net, &result, "nexus");
}

#[test]
fn test_distram_place_and_route_ecp5() {
    let net = synthesize("fpga_distram_regfile.sk", "ecp5");
    let variant = skalp_place_route::Ecp5Variant::Lfe5u25f;
    let result =
        skalp_place_route::place_and_route_ecp5(&net, variant, Default::default()).unwrap();
    let device = skalp_place_route::Ecp5Device::new(variant);
    check_distram_placement(&device, &net, &result, "ecp5");
}
