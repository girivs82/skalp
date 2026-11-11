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

async fn block_ram_check_never_fails(mut tb: Testbench) {
    // One write pass and one read pass of 32 addresses held for 8 cycles each.
    tb.clock(1200).await;
    assert_eq!(tb.get_u64("bad").await, 0);
}

#[tokio::test]
async fn test_block_ram_check_gate_level_nexus() {
    let tb = Testbench::gate_level_with_top_and_library(
        &fixture_path("nexus_blockram_check.sk"),
        "RamCheck",
        "nexus",
    )
    .await
    .unwrap();
    block_ram_check_never_fails(tb).await;
}

/// A register assigned under nested conditions must honour every guard on the
/// way to the assignment, in a then branch and in an else branch.
async fn check_nested_if_guards(mut tb: Testbench) {
    tb.set("a", 1u8);
    tb.set("b", 0u8);
    tb.clock(2).await;
    assert_eq!(tb.get_u64("t").await, 0, "t needs both a and b");
    assert_eq!(tb.get_u64("u").await, 0, "u needs !a and b");

    tb.set("a", 1u8);
    tb.set("b", 1u8);
    tb.clock(2).await;
    assert_eq!(tb.get_u64("t").await, 1, "t is set by a and b");
    assert_eq!(tb.get_u64("u").await, 0, "u must not fire while a is set");

    tb.set("a", 0u8);
    tb.set("b", 1u8);
    tb.clock(2).await;
    assert_eq!(tb.get_u64("t").await, 1, "t holds its value");
    assert_eq!(tb.get_u64("u").await, 1, "u is set by !a and b");
}

#[tokio::test]
async fn test_nested_if_guards_gate_level() {
    let tb = Testbench::gate_level_with_top_and_library(
        &fixture_path("nested_if_guards.sk"),
        "NestedIfGuards",
        "nexus",
    )
    .await
    .unwrap();
    check_nested_if_guards(tb).await;
}

async fn block_ram_design_never_reports_bad(fixture: &str, top: &str, cycles: usize) {
    let mut tb = Testbench::gate_level_with_top_and_library(&fixture_path(fixture), top, "nexus")
        .await
        .unwrap();
    tb.clock(cycles).await;
    assert_eq!(tb.get_u64("bad").await, 0, "{top} read back the wrong data");
}

#[tokio::test]
async fn test_block_ram_reads_on_the_next_cycle() {
    block_ram_design_never_reports_bad("nexus_blockram_latency.sk", "RamCheck", 400).await;
}

#[tokio::test]
async fn test_block_ram_banks_decode_their_own_addresses() {
    block_ram_design_never_reports_bad("nexus_blockram_tiled.sk", "RamTiled", 4200).await;
}

#[test]
fn test_block_ram_read_used_as_logic_stays_out_of_block_ram() {
    let net = synthesize("nexus_blockram_comb_read.sk", "nexus");
    assert!(
        !net.cells.iter().any(|c| c.ram_ports().is_some()),
        "a read feeding logic directly needs its data in the same cycle"
    );
}

/// Later assignments win over earlier ones, whatever statements they are in.
async fn check_repeated_assignment(mut tb: Testbench) {
    tb.set("set", 0u8);
    tb.set("clear", 1u8);
    tb.clock(1).await;
    tb.set("clear", 0u8);
    tb.clock(3).await;
    assert_eq!(tb.get_u64("count").await, 3, "count keeps counting");
    assert_eq!(
        tb.get_u64("pulse").await,
        0,
        "the default applies without set"
    );
    assert_eq!(tb.get_u64("state").await, 1, "the leading value applies");

    tb.set("set", 1u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u64("pulse").await, 1, "set overrides the default");
    assert_eq!(tb.get_u64("latched").await, 1);
    assert_eq!(tb.get_u64("state").await, 2, "the match arm overrides");

    tb.set("set", 0u8);
    tb.set("clear", 1u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u64("latched").await, 0, "the later statement wins");
    assert_eq!(
        tb.get_u64("count").await,
        0,
        "clear overrides the increment"
    );
}

#[tokio::test]
async fn test_repeated_assignment_gate_level() {
    let tb = Testbench::gate_level_with_top_and_library(
        &fixture_path("repeated_assignment.sk"),
        "RepeatedAssignment",
        "nexus",
    )
    .await
    .unwrap();
    check_repeated_assignment(tb).await;
}

#[tokio::test]
async fn test_block_ram_read_registers_reset_and_clear() {
    let mut tb = Testbench::gate_level_with_top_and_library(
        &fixture_path("nexus_blockram_reset.sk"),
        "RamReset",
        "nexus",
    )
    .await
    .unwrap();
    tb.set("rst", 0u8);
    tb.set("clear", 0u8);
    tb.set("addr", 7u16);
    tb.set("wdata", 42u8);
    tb.set("we", 1u8);
    tb.clock(1).await;
    tb.set("we", 0u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u64("q").await, 42, "q reads the stored word");
    assert_eq!(tb.get_u64("p").await, 42, "p reads the stored word");

    tb.set("clear", 1u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u64("p").await, 165, "clear loads its value");
    assert_eq!(tb.get_u64("q").await, 42, "clear only affects p");
    tb.set("clear", 0u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u64("p").await, 42, "p reads the RAM again");

    tb.set("rst", 1u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u64("q").await, 0, "reset clears q");
    tb.set("rst", 0u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u64("q").await, 42, "q reads the RAM after reset");
}

#[test]
fn test_block_ram_read_register_with_reset_uses_block_ram() {
    let net = synthesize("nexus_blockram_reset.sk", "nexus");
    let rams = net.cells.iter().filter(|c| c.ram_ports().is_some()).count();
    assert_eq!(rams, 2, "both memories should use block RAM");
}

#[tokio::test]
async fn test_registers_power_up_at_their_initial_value() {
    let mut tb = Testbench::gate_level_with_top_and_library(
        &fixture_path("register_initial_values.sk"),
        "InitialValues",
        "nexus",
    )
    .await
    .unwrap();
    assert_eq!(tb.get_u64("flag").await, 1);
    assert_eq!(tb.get_u64("count").await, 9);
    tb.clock(1).await;
    assert_eq!(tb.get_u64("flag").await, 0);
    assert_eq!(tb.get_u64("count").await, 10);
}

/// Products of operands wider than one hard multiplier must carry correctly
/// between their partial products.
#[tokio::test]
async fn test_wide_multiply_gate_level() {
    for library in ["nexus", "ecp5"] {
        let mut tb = Testbench::gate_level_with_top_and_library(
            &fixture_path("multiply_wide.sk"),
            "MultiplyWide",
            library,
        )
        .await
        .unwrap();
        for (a, b) in [
            (12_345_678u64, 7_654_321u64),
            (0xFF_FFFF, 0xFF_FFFF),
            (0x3_FFFF, 0x3_FFFF),
            (0x4_0000, 0x4_0000),
            (1, 0xFF_FFFF),
        ] {
            tb.set("a", a);
            tb.set("b", b);
            tb.step().await;
            assert_eq!(tb.get_u64("p").await, a * b, "{a} x {b} on {library}");
        }
    }
}

fn dsp_count(net: &GateNetlist) -> usize {
    net.cells.iter().filter(|c| c.dsp_ports().is_some()).count()
}

/// A multiply sized by its operands' significant bits: `{24'b0, a} * {24'b0, b}`
/// needs 2 x 2 blocks, `c * d` (40 x 24 bits) 3 x 2, and the signed 40 x 40
/// multiply truncated to 64 bits 3 x 3 less the one partial product entirely
/// above bit 63.
#[test]
fn test_tiled_multiplies_use_dsp_blocks() {
    let net = synthesize("multiply_tiled.sk", "nexus");
    assert_eq!(dsp_count(&net), 4 + 6 + 8);
}

#[tokio::test]
async fn test_tiled_multiplies_gate_level() {
    let mask = |bits: u32| (1u128 << bits) - 1;
    let to_signed = |v: u64, bits: u32| ((v as i128) << (128 - bits)) >> (128 - bits);
    for library in ["nexus", "ecp5"] {
        let mut tb = Testbench::gate_level_with_top_and_library(
            &fixture_path("multiply_tiled.sk"),
            "MultiplyTiled",
            library,
        )
        .await
        .unwrap();
        for (x, y, z) in [
            (0xFF_FFFFu64, 0xFF_FFFFu64, 0xFF_FFFFu64),
            (12_345_678, 7_654_321, 0xAB_CDEF),
            (0x3_FFFF, 0x4_0000, 1),
            (0xFF_FFFF_FFFF, 0xFF_FFFF_FFFF, 0x80_0000),
            (0x80_0000_0000, 0x7F_FFFF_FFFF, 0x12_3456),
            (1, 0xFF_FFFF_FFFF, 0),
            (0x12_3456_789A, 0xFE_DCBA_9876, 0x5A_5A5A),
        ] {
            let (a, b) = (x & 0xFF_FFFF, y & 0xFF_FFFF);
            let (c, d) = (x & 0xFF_FFFF_FFFF, z & 0xFF_FFFF);
            let (e, f) = (x & 0xFF_FFFF_FFFF, y & 0xFF_FFFF_FFFF);
            tb.set("a", a);
            tb.set("b", b);
            tb.set("c", c);
            tb.set("d", d);
            tb.set("e", e);
            tb.set("f", f);
            tb.step().await;
            assert_eq!(tb.get_u64("p").await, a * b, "{a} x {b} on {library}");
            let q = (c as u128 * d as u128) & mask(64);
            assert_eq!(tb.get_u64("q").await as u128, q, "{c} x {d} on {library}");
            let r = (to_signed(e, 40) * to_signed(f, 40)) as u128 & mask(64);
            assert_eq!(
                tb.get_u64("r").await as u128,
                r,
                "signed {e:#x} x {f:#x} on {library}"
            );
        }
    }
}
