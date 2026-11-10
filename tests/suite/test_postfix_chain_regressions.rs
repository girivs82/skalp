//! Regressions found while writing a processor core in SKALP.
//!
//! The parser emits postfix operations (`.field`, `[i]`, `[h:l]`, `(args)`)
//! as siblings of their base rather than as parents, and several builders
//! picked a single sibling instead of the whole chain:
//!
//! 1. `{32{x[31]}}` replicated all of `x` (the index was dropped), and a
//!    function parameter inside a replication was not substituted when the
//!    function was inlined ("undefined identifier `x`").
//! 2. `f(a, b)[0]` (indexing a call result) silently dropped the enclosing
//!    `let`, `return` or assignment.
//! 3. `mem[s.a[3:0]]` (indexing by a slice of an instance output) indexed
//!    `mem` with the instance itself and failed to lower.
//!
//! Each case is checked both on the generated SystemVerilog and, where it
//! computes a value, in behavioral simulation.

use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::monomorphization::MonomorphizationEngine;
use skalp_frontend::parse::parse;
use skalp_frontend::parse_and_build_hir;
use skalp_mir::lower_to_mir;
use skalp_sim::{HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};

fn compile_to_sv(source: &str) -> String {
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building failed");
    let mut engine = MonomorphizationEngine::new();
    let hir = engine.monomorphize(&hir);
    let mir = lower_to_mir(&hir).expect("MIR lowering failed");
    skalp_codegen::generate_systemverilog_from_mir(&mir).expect("SystemVerilog generation failed")
}

async fn simulator(source: &str) -> UnifiedSimulator {
    let hir = parse_and_build_hir(source).expect("Failed to parse design");
    let mir = lower_to_mir(&hir).expect("Failed to compile to MIR");
    let top_module = &mir.modules[mir.modules.len() - 1];
    let sir = skalp_sir::convert_mir_to_sir_with_hierarchy(&mir, top_module);
    let config = UnifiedSimConfig {
        level: SimLevel::Behavioral,
        hw_accel: HwAccel::Cpu,
        max_cycles: 100,
        capture_waveforms: false,
        ..Default::default()
    };
    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_behavioral(&sir)
        .await
        .expect("Failed to load module");
    sim
}

/// Collapse whitespace so assertions don't depend on codegen formatting.
fn squash(sv: &str) -> String {
    sv.chars().filter(|c| !c.is_whitespace()).collect()
}

// ---------------------------------------------------------------------------
// 1. Replication
// ---------------------------------------------------------------------------

const SEXT_FN: &str = r#"
fn sext32(x: bit[32]) -> bit[64] {
    return {{32{x[31]}}, x}
}
entity A {
    in a: bit[32]
    out q: bit[64]
}
impl A {
    q = sext32(a)
}
"#;

#[test]
fn test_param_in_replication_is_substituted() {
    let sv = squash(&compile_to_sv(SEXT_FN));
    assert!(
        sv.contains("assignq={{32{a[31]}},a};"),
        "sign extension through an inlined function must replicate a[31]:\n{}",
        sv
    );
}

#[tokio::test]
async fn test_param_in_replication_simulates() {
    let mut sim = simulator(SEXT_FN).await;
    for (a, expected) in [
        (0x8000_0001u64, 0xFFFF_FFFF_8000_0001u64),
        (0x7FFF_FFFF, 0x0000_0000_7FFF_FFFF),
        (0xFFFF_FFFF, 0xFFFF_FFFF_FFFF_FFFF),
        (0, 0),
    ] {
        sim.set_input("a", a).await;
        sim.step().await;
        let q = sim.get_output("q").await.unwrap_or(0);
        assert_eq!(q, expected, "sext32({:#x})", a);
    }
}

#[test]
fn test_replication_keeps_postfix_operations() {
    // Each replicated value carries a postfix chain the builder used to cut
    // off after the first sibling, and the count can be an expression.
    let source = r#"
struct P { a: bit[8], b: bit[8] }
entity R {
    in a: bit[32]
    in p: P
    in sel: bit
    out q0: bit[64]
    out q1: bit[16]
    out q2: bit[24]
    out q3: bit[16]
    out q4: bit[8]
    out q5: bit[16]
}
impl R {
    q0 = {{32{a[31]}}, a}
    q1 = {4{a[7:4]}}
    q2 = {{(8 - 5){p.a[7]}}, a[20:0]}
    q3 = {2{p.b}}
    q4 = {8{sel}}
    q5 = {2{a[15:8] ^ p.a}}
}
"#;
    let sv = squash(&compile_to_sv(source));
    for expected in [
        "assignq0={{32{a[31]}},a};",
        "assignq1={4{a[7:4]}};",
        "assignq2={{(8-5){p__a[7]}},a[20:0]};",
        "assignq3={2{p__b}};",
        "assignq4={8{sel}};",
        "assignq5={2{(a[15:8]^p__a)}};",
    ] {
        assert!(sv.contains(expected), "missing `{}` in:\n{}", expected, sv);
    }
}

#[tokio::test]
async fn test_replication_simulates() {
    // Simulation used to ignore replication: `{N{v}}` became a single copy
    // or a 1-bit zero.
    let source = r#"
entity R {
    in a: bit[8]
    out q0: bit[16]
    out q1: bit[12]
    out q2: bit[8]
}
impl R {
    q0 = {{8{a[7]}}, a}
    q1 = {{(8 - 5){a[3:2]}}, a[5:0]}
    q2 = {2{a[3:0]}}
}
"#;
    let mut sim = simulator(source).await;
    for a in [0x00u64, 0x7F, 0x80, 0xA5, 0xFF, 0x3C] {
        sim.set_input("a", a).await;
        sim.step().await;
        let sign = if a & 0x80 != 0 { 0xFF00 } else { 0 };
        let two = (a >> 2) & 0x3;
        let nib = a & 0xF;
        assert_eq!(
            sim.get_output("q0").await.unwrap_or(0),
            sign | a,
            "q0, a={:#x}",
            a
        );
        assert_eq!(
            sim.get_output("q1").await.unwrap_or(0),
            (two << 10) | (two << 8) | (two << 6) | (a & 0x3F),
            "q1, a={:#x}",
            a
        );
        assert_eq!(
            sim.get_output("q2").await.unwrap_or(0),
            (nib << 4) | nib,
            "q2, a={:#x}",
            a
        );
    }
}

// ---------------------------------------------------------------------------
// 2. Indexing a call result
// ---------------------------------------------------------------------------

const INDEX_CALL: &str = r#"
fn f(a: bit[8], b: bit[8]) -> bit[8] {
    return if a < b { 1 } else { 0 }
}
fn x(a: bit[8], b: bit[8]) -> bit[8] {
    return a ^ b
}
fn in_let(a: bit[8], b: bit[8]) -> bit {
    let lt: bit = f(a, b)[0]
    return lt
}
fn in_return(a: bit[8], b: bit[8]) -> bit {
    return x(a, b)[3]
}
entity A {
    in a: bit[8]
    in b: bit[8]
    out lt: bit
    out r3: bit
    out q5: bit
    out s: bit[4]
}
impl A {
    lt = in_let(a, b)
    r3 = in_return(a, b)
    q5 = x(a, b)[5]
    s = x(a, b)[5:2]
}
"#;

#[test]
fn test_index_call_result_compiles() {
    // Before the fix the `let` was dropped ("undefined identifier `lt`"),
    // the `return` lost its value, and `q5 = x(a, b)[5]` vanished.
    let sv = compile_to_sv(INDEX_CALL);
    let squashed = squash(&sv);
    for port in ["lt", "r3", "q5", "s"] {
        assert!(
            squashed.contains(&format!("assign{}=", port)),
            "output `{}` is not driven:\n{}",
            port,
            sv
        );
    }
}

#[tokio::test]
async fn test_index_call_result_simulates() {
    let mut sim = simulator(INDEX_CALL).await;
    for (a, b) in [(3u64, 5u64), (5, 3), (0xA5, 0x0F), (0xFF, 0x00), (0, 0)] {
        sim.set_input("a", a).await;
        sim.set_input("b", b).await;
        sim.step().await;
        let x = a ^ b;
        assert_eq!(
            sim.get_output("lt").await.unwrap_or(9),
            (a < b) as u64,
            "lt for a={:#x} b={:#x}",
            a,
            b
        );
        assert_eq!(
            sim.get_output("r3").await.unwrap_or(9),
            (x >> 3) & 1,
            "r3 for a={:#x} b={:#x}",
            a,
            b
        );
        assert_eq!(
            sim.get_output("q5").await.unwrap_or(9),
            (x >> 5) & 1,
            "q5 for a={:#x} b={:#x}",
            a,
            b
        );
        assert_eq!(
            sim.get_output("s").await.unwrap_or(99),
            (x >> 2) & 0xF,
            "s for a={:#x} b={:#x}",
            a,
            b
        );
    }
}

#[test]
fn test_function_module_let_declared_once() {
    // A function synthesized as its own module declares each `let` binding
    // as a wire and used to declare it again as a `logic` variable of the
    // same name — illegal SystemVerilog.
    let source = r#"
fn f(a: bit[8], b: bit[8]) -> bit[8] {
    return if a < b { 1 } else { 0 }
}
fn g(a: bit[8], b: bit[8]) -> bit {
    let r: bit[8] = f(a, b)
    let lt: bit = r[0]
    return lt
}
entity A {
    in a: bit[8]
    in b: bit[8]
    out q: bit
}
impl A {
    q = g(a, b)
}
"#;
    let sv = compile_to_sv(source);
    let mut declared = std::collections::HashMap::<String, usize>::new();
    for line in sv.lines().map(str::trim) {
        if line.starts_with("module ") {
            declared.clear();
        }
        let Some(rest) = line
            .strip_prefix("wire ")
            .or_else(|| line.strip_prefix("logic "))
            .or_else(|| line.strip_prefix("reg "))
        else {
            continue;
        };
        let name = rest
            .trim_end_matches(';')
            .split_whitespace()
            .last()
            .unwrap_or_default()
            .to_string();
        let count = declared.entry(name.clone()).or_default();
        *count += 1;
        assert_eq!(*count, 1, "`{}` declared twice:\n{}", name, sv);
    }
}

// ---------------------------------------------------------------------------
// 3. Indexing an array by a slice of an instance output
// ---------------------------------------------------------------------------

const INSTANCE_SLICE_INDEX: &str = r#"
entity Src {
    in x: bit[16]
    out a: bit[16]
}
impl Src {
    a = x + 1
}
entity A {
    in clk: clock
    in x: bit[16]
    in wd: bit[8]
    out rd: bit[8]
}
impl A {
    signal mem: bit[8][16]
    inst s = Src { x: x }
    on(clk.rise) {
        mem[s.a[3:0]] = wd
    }
    rd = mem[s.a[3:0]]
}
"#;

#[test]
fn test_array_index_by_instance_output_slice() {
    let sv = squash(&compile_to_sv(INSTANCE_SLICE_INDEX));
    assert!(
        sv.contains("mem[s_a[3:0]]<=wd;"),
        "write must index by the instance output slice:\n{}",
        sv
    );
    assert!(
        sv.contains("assignrd=mem[s_a[3:0]];"),
        "read must index by the instance output slice:\n{}",
        sv
    );
}

async fn rising_edge(sim: &mut UnifiedSimulator) {
    sim.set_input("clk", 0).await;
    sim.step().await;
    sim.set_input("clk", 1).await;
    sim.step().await;
}

#[tokio::test]
async fn test_array_index_by_instance_output_slice_simulates() {
    let mut sim = simulator(INSTANCE_SLICE_INDEX).await;
    // Address = (x + 1)[3:0]; x = 0x00F1 and x = 0x0011 both hit address 2,
    // x = 0x0013 hits address 4.
    sim.set_input("x", 0x00F1).await;
    sim.set_input("wd", 0xAB).await;
    rising_edge(&mut sim).await;
    sim.set_input("x", 0x0013).await;
    sim.set_input("wd", 0xCD).await;
    rising_edge(&mut sim).await;

    sim.set_input("x", 0x0011).await;
    sim.step().await;
    assert_eq!(sim.get_output("rd").await.unwrap_or(0), 0xAB);
    sim.set_input("x", 0x0013).await;
    sim.step().await;
    assert_eq!(sim.get_output("rd").await.unwrap_or(0), 0xCD);
}
