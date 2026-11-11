//! Logic depth of selects and compares after synthesis.
//!
//! A `match` on constants is lowered as a table lookup (a mux tree on the
//! selector's bits, or an AND-OR of the arms for a wide selector) rather than
//! a priority chain with one level per arm, and equality compares and
//! reductions are balanced trees rather than chains with one level per bit.
//! These tests check the depth of the mapped netlist and that the gate-level
//! results match values computed here from the source's table.

use skalp_lir::GateNetlist;
use skalp_testing::Testbench;
use std::collections::HashMap;

const TABLE: &[(u64, Option<u64>)] = &[
    (2, Some(0x3c4f)),
    (3, Some(0x9df1)),
    (4, Some(0x5c88)),
    (5, Some(0x34c3)),
    (6, Some(0x6030)),
    (7, None),
    (9, Some(0x31e2)),
    (13, Some(0x2025)),
    (14, Some(0x1e84)),
    (15, Some(0x6973)),
    (17, Some(0xfe2a)),
    (23, Some(0xdaed)),
    (25, Some(0xa0d7)),
    (26, Some(0xee63)),
    (27, Some(0xe807)),
    (28, Some(0xb921)),
    (30, Some(0x997b)),
    (32, Some(0x7f31)),
    (34, Some(0x5c0a)),
    (35, Some(0x7cfa)),
    (36, Some(0x29e8)),
    (37, Some(0x99ba)),
    (38, Some(0xfd7f)),
    (39, Some(0xafdc)),
    (40, Some(0xe5cd)),
    (41, Some(0x936c)),
    (44, Some(0x257a)),
    (46, Some(0x3c73)),
    (47, Some(0xd614)),
    (48, Some(0x5475)),
    (49, Some(0xaf21)),
    (51, Some(0x4dd0)),
    (52, Some(0xfa59)),
    (57, Some(0xd7e8)),
    (58, Some(0x1412)),
    (59, Some(0x27bd)),
    (60, Some(0xa0a3)),
    (61, Some(0xae24)),
    (62, Some(0xb34a)),
    (63, Some(0xfe4c)),
];
const CASE_TABLE: &[(u64, u64)] = &[
    (6, 0xe7),
    (8, 0xee),
    (10, 0xe7),
    (12, 0x61),
    (14, 0x5e),
    (16, 0xf3),
    (18, 0x5f),
    (20, 0x30),
    (22, 0xe4),
    (24, 0x9b),
    (26, 0x48),
    (28, 0x2e),
    (30, 0x15),
];
const DECODE: &[(u64, u64)] = &[
    (0x1234, 0x11),
    (0xbeef, 0x22),
    (0x0007, 0x33),
    (0x8000, 0x44),
    (0xffff, 0x55),
    (0x4242, 0x66),
];

fn fixture_path() -> String {
    format!(
        "{}/tests/fixtures/logic_depth.sk",
        env!("CARGO_MANIFEST_DIR")
    )
}

fn case_fixture_path() -> String {
    format!(
        "{}/tests/fixtures/logic_depth_case.sk",
        env!("CARGO_MANIFEST_DIR")
    )
}

fn synthesize(path: String, library: &str) -> GateNetlist {
    let path = std::path::PathBuf::from(path);
    let ctx = skalp_frontend::parse_and_build_compilation_context(&path).unwrap();
    let mir = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .unwrap();
    let (hier, _) = skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir);
    let library = skalp_lir::get_stdlib_library(library).unwrap();
    skalp_lir::synthesize_hierarchical(&hier, &library, skalp_lir::synth::SynthPreset::Quick)
        .flatten()
}

/// Cells on the longest path from an input to each output port, by port name.
fn output_depths(net: &GateNetlist) -> HashMap<String, usize> {
    fn depth(net: &GateNetlist, id: usize, memo: &mut HashMap<usize, usize>) -> usize {
        if let Some(&d) = memo.get(&id) {
            return d;
        }
        memo.insert(id, 0);
        let d = match net.nets[id].driver.and_then(|c| net.get_cell(c)) {
            Some(cell) if cell.clock.is_none() => {
                1 + cell
                    .inputs
                    .iter()
                    .map(|i| depth(net, i.0 as usize, memo))
                    .max()
                    .unwrap_or(0)
            }
            _ => 0,
        };
        memo.insert(id, d);
        d
    }
    let mut memo = HashMap::new();
    let mut ports: HashMap<String, usize> = HashMap::new();
    for id in &net.outputs {
        let name = net.nets[id.0 as usize].name.trim_start_matches("top.");
        let port = name.split('[').next().unwrap().to_string();
        let d = depth(net, id.0 as usize, &mut memo);
        let entry = ports.entry(port).or_default();
        *entry = (*entry).max(d);
    }
    ports
}

#[test]
fn test_selects_and_compares_are_shallow_on_luts() {
    let depths = output_depths(&synthesize(fixture_path(), "nexus"));
    println!("{depths:?}");
    // 40-arm table on a 6-bit selector: a priority chain is ~40 LUTs deep.
    assert!(depths["lut"] <= 6, "table lookup depth {}", depths["lut"]);
    // 6 arms on a 16-bit selector.
    assert!(depths["dec"] <= 6, "wide select depth {}", depths["dec"]);
    // 64-bit compares and reductions: chains are 20+ LUT4s deep.
    for port in ["eq", "ne", "all", "any", "parity"] {
        assert!(depths[port] <= 8, "{port} depth {}", depths[port]);
    }
}

#[test]
fn test_clocked_match_is_shallow_on_luts() {
    let net = synthesize(case_fixture_path(), "nexus");
    let depth = register_input_depth(&net);
    println!("deepest register input: {depth}");
    // 17 arms on a 5-bit selector: a priority chain is ~17 LUTs deep.
    assert!(depth <= 6, "register input depth {depth}");
}

/// Cells on the longest path into any register.
fn register_input_depth(net: &GateNetlist) -> usize {
    fn depth(net: &GateNetlist, id: usize, memo: &mut HashMap<usize, usize>) -> usize {
        if let Some(&d) = memo.get(&id) {
            return d;
        }
        memo.insert(id, 0);
        let d = match net.nets[id].driver.and_then(|c| net.get_cell(c)) {
            Some(cell) if cell.clock.is_none() => {
                1 + cell
                    .inputs
                    .iter()
                    .map(|i| depth(net, i.0 as usize, memo))
                    .max()
                    .unwrap_or(0)
            }
            _ => 0,
        };
        memo.insert(id, d);
        d
    }
    let mut memo = HashMap::new();
    net.cells
        .iter()
        .filter(|c| c.clock.is_some())
        .flat_map(|c| c.inputs.iter())
        .map(|i| depth(net, i.0 as usize, &mut memo))
        .max()
        .unwrap_or(0)
}

fn expected_table(s: u64, x: u64) -> u64 {
    match TABLE.iter().find(|(k, _)| *k == s) {
        Some((_, Some(v))) => *v,
        Some((_, None)) => x ^ 0x5A5A,
        None => x,
    }
}

fn expected_decode(c: u64) -> u64 {
    DECODE.iter().find(|(k, _)| *k == c).map_or(0, |(_, v)| *v)
}

fn next(x: &mut u64) -> u64 {
    *x ^= *x << 13;
    *x ^= *x >> 7;
    *x ^= *x << 17;
    *x
}

async fn check(mut tb: Testbench, label: &str) {
    let mut seed = 0x9E37_79B9_7F4A_7C15u64;
    for s in 0..64u64 {
        let x = next(&mut seed) & 0xFFFF;
        // Every decoded value, its neighbours, and random selectors.
        let c = match s as usize {
            i if i < DECODE.len() => DECODE[i].0,
            i if i < 2 * DECODE.len() => DECODE[i - DECODE.len()].0 ^ 1,
            _ => next(&mut seed) & 0xFFFF,
        };
        let a = next(&mut seed);
        let b = match s % 4 {
            0 => a,
            1 => a ^ (1 << (s % 64)),
            _ => next(&mut seed),
        };
        let a = match s % 16 {
            3 => u64::MAX,
            7 => 0,
            _ => a,
        };
        tb.set("s", s);
        tb.set("x", x);
        tb.set("c", c);
        tb.set("a", a);
        tb.set("b", b);
        tb.clock(1).await;
        let ctx = format!("{label}: s={s} x={x:#x} c={c:#x} a={a:#x} b={b:#x}");
        assert_eq!(tb.get_u64("lut").await, expected_table(s, x), "lut {ctx}");
        assert_eq!(tb.get_u64("dec").await, expected_decode(c), "dec {ctx}");
        assert_eq!(tb.get_u64("eq").await, (a == b) as u64, "eq {ctx}");
        assert_eq!(tb.get_u64("ne").await, (a != b) as u64, "ne {ctx}");
        assert_eq!(tb.get_u64("all").await, (a == u64::MAX) as u64, "all {ctx}");
        assert_eq!(tb.get_u64("any").await, (a != 0) as u64, "any {ctx}");
        assert_eq!(
            tb.get_u64("parity").await,
            (a.count_ones() & 1) as u64,
            "parity {ctx}"
        );
    }
}

#[tokio::test]
async fn test_logic_depth_gate_level_nexus() {
    let tb = Testbench::gate_level_with_top_and_library(&fixture_path(), "LogicDepth", "nexus")
        .await
        .unwrap();
    check(tb, "nexus").await;
}

#[tokio::test]
async fn test_logic_depth_gate_level_generic() {
    let tb =
        Testbench::gate_level_with_top_and_library(&fixture_path(), "LogicDepth", "generic_asic")
            .await
            .unwrap();
    check(tb, "generic").await;
}

fn expected_case(s: u64, x: u64, q: u64) -> (u64, u64) {
    match s {
        0 => (x, x),
        1 => (!x & 0xFF, x),
        2 => (x ^ 0x0F, 0x55),
        3 => (q, 0xAA),
        _ => (
            CASE_TABLE
                .iter()
                .find(|(k, _)| *k == s)
                .map_or(0, |(_, v)| *v),
            x,
        ),
    }
}

async fn check_case(mut tb: Testbench, label: &str) {
    let mut seed = 0x0123_4567_89AB_CDEFu64;
    let mut q = 0;
    for step in 0..96u64 {
        let s = if step < 32 {
            step
        } else {
            next(&mut seed) & 31
        };
        let x = next(&mut seed) & 0xFF;
        tb.set("s", s);
        tb.set("x", x);
        tb.clock(1).await;
        let (want_q, want_r) = expected_case(s, x, q);
        q = want_q;
        let ctx = format!("{label}: step {step} s={s} x={x:#x}");
        assert_eq!(tb.get_u64("q").await, want_q, "q {ctx}");
        assert_eq!(tb.get_u64("r").await, want_r, "r {ctx}");
    }
}

#[tokio::test]
async fn test_clocked_match_behavioral() {
    let tb = Testbench::with_top_module(&case_fixture_path(), "CaseDepth")
        .await
        .unwrap();
    check_case(tb, "behavioral").await;
}

#[tokio::test]
async fn test_clocked_match_gate_level_nexus() {
    let tb = Testbench::gate_level_with_top_and_library(&case_fixture_path(), "CaseDepth", "nexus")
        .await
        .unwrap();
    check_case(tb, "nexus").await;
}
