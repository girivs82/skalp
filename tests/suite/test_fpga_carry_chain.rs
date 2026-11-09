//! FPGA carry chains (Lattice `CCU2` on Nexus and ECP5).
//!
//! Adders, subtractors and magnitude comparators at least
//! `CARRY_CHAIN_MIN_WIDTH` bits wide are mapped onto the dedicated carry chain
//! instead of LUT ripple logic. These tests check that the cells are used and
//! that the gate-level netlist computes the same results as the source.

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

fn ccu2_count(net: &GateNetlist) -> usize {
    net.cells
        .iter()
        .filter(|c| c.function == Some(CellFunction::Ccu2))
        .count()
}

#[test]
fn test_nexus_arithmetic_uses_ccu2_carry_chains() {
    let net = synthesize("fpga_carry_chain_ops.sk", "nexus");
    // 16-bit ops: carry-in half + 16 sum halves (+ a carry-out half for the
    // 17-bit add and each compare) → 9 cells each; the 3-bit add stays in LUTs.
    let ccu2 = ccu2_count(&net);
    assert!(
        ccu2 >= 12 * 9,
        "expected ≥ {} CCU2 cells for 12 wide ops, got {}",
        12 * 9,
        ccu2
    );
    for cell in net
        .cells
        .iter()
        .filter(|c| c.function == Some(CellFunction::Ccu2))
    {
        assert_eq!(cell.cell_type, "CCU2");
        assert_eq!(cell.inputs.len(), 9, "{}", cell.path);
        assert_eq!(cell.outputs.len(), 3, "{}", cell.path);
        assert!(
            skalp_lir::carry_chain::ccu2_inits(cell).is_some(),
            "{} needs INIT0/INIT1",
            cell.path
        );
        assert_eq!(
            cell.parameters.get("INJECT").map(String::as_str),
            Some("NO")
        );
    }
    // Carry-chain operands are internal nets: synthesis exports them from the
    // AIG only to keep their logic, and must not leave them as module outputs
    // (each would become a top-level port and an I/O cell in PnR).
    let ccu2_inputs: std::collections::HashSet<_> = net
        .cells
        .iter()
        .filter(|c| c.function == Some(CellFunction::Ccu2))
        .flat_map(|c| c.inputs.iter().copied())
        .collect();
    let ports = [
        "sum", "sum_c", "diff", "wide", "ltu", "leu", "gtu", "geu", "lts", "les", "gts", "ges",
        "nsum",
    ];
    for id in &net.outputs {
        let name = &net.nets[id.0 as usize].name;
        let base = name.trim_start_matches("top.").split('[').next().unwrap();
        assert!(
            ports.contains(&base) || !ccu2_inputs.contains(id),
            "carry-chain operand {name} is a module output"
        );
    }
}

#[test]
fn test_ice40_and_generic_targets_have_no_ccu2() {
    // iCE40 has SB_CARRY (a different cell) and the generic ASIC library has
    // no carry cell: neither gets CCU2 cells.
    assert_eq!(
        ccu2_count(&synthesize("fpga_carry_chain_ops.sk", "ice40")),
        0
    );
    assert_eq!(
        ccu2_count(&synthesize("fpga_carry_chain_ops.sk", "generic_asic")),
        0
    );
}

/// Deterministic operand pairs: edge values plus a pseudo-random spread.
fn vectors() -> Vec<(u16, u16)> {
    let mut v: Vec<(u16, u16)> = vec![
        (0, 0),
        (0, 1),
        (1, 0),
        (0xFFFF, 1),
        (1, 0xFFFF),
        (0x7FFF, 0x8000),
        (0x8000, 0x7FFF),
        (0x8000, 0x8000),
        (0xFFFF, 0xFFFF),
        (1234, 1234),
        (0xFB2E, 0x04D2),
    ];
    let mut x: u32 = 0x1234_5678;
    for _ in 0..40 {
        x ^= x << 13;
        x ^= x >> 17;
        x ^= x << 5;
        v.push(((x & 0xFFFF) as u16, (x >> 16) as u16));
    }
    v
}

async fn check_carry_ops(mut tb: Testbench, label: &str) {
    for (a, b) in vectors() {
        let (sa, sb) = (a as i16, b as i16);
        let (n, m) = ((a & 7) as u64, (b & 7) as u64);
        tb.set("a", a as u64);
        tb.set("b", b as u64);
        tb.set("sa", a as u64);
        tb.set("sb", b as u64);
        tb.set("n", n);
        tb.set("m", m);
        tb.clock(1).await;
        let ctx = format!("{label}: a={a:#06x} b={b:#06x}");
        assert_eq!(
            tb.get_u64("sum").await,
            a.wrapping_add(b) as u64,
            "sum {ctx}"
        );
        assert_eq!(
            tb.get_u64("sum_c").await,
            a.wrapping_add(1234) as u64,
            "sum_c {ctx}"
        );
        assert_eq!(
            tb.get_u64("diff").await,
            a.wrapping_sub(b) as u64,
            "diff {ctx}"
        );
        assert_eq!(tb.get_u64("wide").await, a as u64 + b as u64, "wide {ctx}");
        assert_eq!(tb.get_u64("ltu").await, (a < b) as u64, "ltu {ctx}");
        assert_eq!(tb.get_u64("leu").await, (a <= b) as u64, "leu {ctx}");
        assert_eq!(tb.get_u64("gtu").await, (a > b) as u64, "gtu {ctx}");
        assert_eq!(tb.get_u64("geu").await, (a >= b) as u64, "geu {ctx}");
        assert_eq!(tb.get_u64("lts").await, (sa < sb) as u64, "lts {ctx}");
        assert_eq!(tb.get_u64("les").await, (sa <= sb) as u64, "les {ctx}");
        assert_eq!(tb.get_u64("gts").await, (sa > sb) as u64, "gts {ctx}");
        assert_eq!(tb.get_u64("ges").await, (sa >= sb) as u64, "ges {ctx}");
        assert_eq!(tb.get_u64("nsum").await, (n + m) & 7, "nsum {ctx}");
    }
}

#[tokio::test]
async fn test_carry_ops_behavioral() {
    let tb = Testbench::with_top_module(&fixture_path("fpga_carry_chain_ops.sk"), "CarryOps")
        .await
        .unwrap();
    check_carry_ops(tb, "behavioral").await;
}

#[tokio::test]
async fn test_carry_ops_gate_level_nexus() {
    let tb = Testbench::gate_level_with_top_and_library(
        &fixture_path("fpga_carry_chain_ops.sk"),
        "CarryOps",
        "nexus",
    )
    .await
    .unwrap();
    check_carry_ops(tb, "nexus").await;
}

#[tokio::test]
async fn test_carry_ops_gate_level_ecp5() {
    let tb = Testbench::gate_level_with_top_and_library(
        &fixture_path("fpga_carry_chain_ops.sk"),
        "CarryOps",
        "ecp5",
    )
    .await
    .unwrap();
    check_carry_ops(tb, "ecp5").await;
}

async fn check_counter_counts(tb: Testbench) {
    let mut tb = tb;
    tb.set("rst", 1u8);
    tb.clock(2).await;
    tb.set("rst", 0u8);
    tb.clock(1).await;
    let first = tb.get_u64("count").await;
    tb.clock(5).await;
    assert_eq!(tb.get_u64("count").await, first + 5);
}

#[tokio::test]
async fn test_carry_counter_gate_level_nexus() {
    let tb = Testbench::gate_level_with_top_and_library(
        &fixture_path("fpga_carry_chain_counter.sk"),
        "CarryCounter",
        "nexus",
    )
    .await
    .unwrap();
    check_counter_counts(tb).await;
}

#[test]
fn test_ccu2_verilog_carries_init_parameters() {
    let net = synthesize("fpga_carry_chain_ops.sk", "nexus");
    let v = net.to_verilog();
    assert!(
        v.contains("CCU2 #(.INIT0(\"0x"),
        "CCU2 instances carry INIT0"
    );
    assert!(v.contains(".INJECT(\"NO\")"));
    assert!(v.contains(".CIN(") && v.contains(".COUT("));
}
