//! Regressions found while writing a memory controller in SKALP.
//!
//! 1. An unsized literal in a sized `let` (`let x: bit[4] = 0`, or as an
//!    `if` arm) made SystemVerilog codegen declare the variable 32 bits
//!    wide, so `{a, x}` shifted `a` out of the concatenation.
//! 2. `let x: bit[8] = GENERIC` was declared with an initializer *and*
//!    driven by a continuous assignment: two drivers, which SystemVerilog
//!    forbids.
//! 3. A const generic used directly as a match-arm value (`5 => MR5,`) was
//!    never substituted, and MIR reported "undefined identifier `MR5`".
//! 4. `inst d = M<P: 1> {..}` (a named generic argument) was pointed at the
//!    all-defaults specialization, and a partial positional list (`M<5>` on
//!    `M<A = 2, B = 4>`) at the unspecialised template.
//!
//! Each case is checked on the generated SystemVerilog and, where it computes
//! a value, in behavioral simulation.

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

/// A behavioral simulator for `top`. Specializations of generic entities
/// are appended after the design's own modules, so the top must be named.
async fn simulator(source: &str, top: &str) -> UnifiedSimulator {
    let sir = elaborate_top(source, top);
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

fn elaborate_top(source: &str, top: &str) -> skalp_sir::SirModule {
    let hir = parse_and_build_hir(source).expect("Failed to parse design");
    let mir = lower_to_mir(&hir).expect("Failed to compile to MIR");
    let top_module = mir
        .modules
        .iter()
        .find(|m| m.name == top)
        .unwrap_or_else(|| panic!("no module {top}"));
    skalp_sir::convert_mir_to_sir_with_hierarchy(&mir, top_module)
}

/// Collapse whitespace so assertions don't depend on codegen formatting.
fn squash(sv: &str) -> String {
    sv.chars().filter(|c| !c.is_whitespace()).collect()
}

/// The body of `module <name> (...) ... endmodule`, squashed.
fn module_body(sv: &str, name: &str) -> String {
    let squashed = squash(sv);
    let start = squashed
        .find(&format!("module{}(", name))
        .unwrap_or_else(|| panic!("no module {name} in:\n{sv}"));
    let rest = &squashed[start..];
    let end = rest.find("endmodule").expect("unterminated module");
    rest[..end].to_string()
}

// ---------------------------------------------------------------------------
// 1 and 2. Widths and drivers of sized `let` bindings
// ---------------------------------------------------------------------------

const SIZED_LET: &str = r#"
entity SizedLet<G: nat = 200> {
    in c: bit
    in y: bit[8]
    in a: bit[4]
    out q_if: bit[8]
    out q_lit: bit[8]
    out q_sized: bit[8]
    out r: bit[8]
}
impl SizedLet {
    let x_if: bit[4] = if c { 0 } else { y[3:0] }
    let x_lit: bit[4] = 0
    let x_sized: bit[4] = if c { 4'd0 } else { y[3:0] }
    let g: bit[8] = G
    q_if = {a, x_if}
    q_lit = {a, x_lit}
    q_sized = {a, x_sized}
    r = g
}
"#;

#[test]
fn test_unsized_literal_keeps_declared_let_width() {
    let sv = squash(&compile_to_sv(SIZED_LET));
    for name in ["x_if", "x_lit", "x_sized"] {
        assert!(
            sv.contains(&format!("logic[3:0]{name}")),
            "`{name}` must be declared 4 bits wide:\n{sv}"
        );
    }
    assert!(
        !sv.contains("logic[31:0]"),
        "no 32-bit variable may appear:\n{sv}"
    );
}

#[test]
fn test_continuously_assigned_let_has_one_driver() {
    let sv = squash(&compile_to_sv(SIZED_LET));
    assert!(
        sv.contains("logic[7:0]g;") && sv.contains("assigng=200;"),
        "`g` must be declared without an initializer and assigned once:\n{sv}"
    );
    assert!(
        !sv.contains("logic[3:0]x_lit=0;"),
        "`x_lit` must not be initialized as well as assigned:\n{sv}"
    );
}

#[tokio::test]
async fn test_sized_let_simulates() {
    let mut sim = simulator(SIZED_LET, "SizedLet_200").await;
    for (c, y, a) in [
        (0u64, 0xA5u64, 0x3u64),
        (1, 0xA5, 0x3),
        (0, 0x0F, 0xC),
        (1, 0xFF, 0xF),
    ] {
        sim.set_input("c", c).await;
        sim.set_input("y", y).await;
        sim.set_input("a", a).await;
        sim.step().await;
        let low = if c == 1 { 0 } else { y & 0xF };
        let expected = (a << 4) | low;
        assert_eq!(
            sim.get_output("q_if").await.unwrap_or(u64::MAX),
            expected,
            "q_if c={c} y={y:#x} a={a:#x}"
        );
        assert_eq!(
            sim.get_output("q_lit").await.unwrap_or(u64::MAX),
            a << 4,
            "q_lit a={a:#x}"
        );
        assert_eq!(
            sim.get_output("q_sized").await.unwrap_or(u64::MAX),
            expected,
            "q_sized c={c} y={y:#x} a={a:#x}"
        );
        assert_eq!(sim.get_output("r").await.unwrap_or(u64::MAX), 200);
    }
}

// ---------------------------------------------------------------------------
// 3. A const generic as a match-arm value
// ---------------------------------------------------------------------------

const MATCH_GENERIC: &str = r#"
entity ModeRegs<MR5: nat = 9, MR6: nat = 12> {
    in clk: clock
    in sel: bit[3]
    out q: bit[8]
    out r: bit[8]
}
impl ModeRegs {
    signal st: bit[8] = 0
    q = match sel {
        5 => MR5,
        6 => MR6 + 1,
        _ => 0
    }
    on(clk.rise) {
        match sel {
            5 => { st = MR5 }
            _ => { st = MR6 }
        }
    }
    r = st
}
"#;

#[test]
fn test_generic_match_arm_value_is_substituted() {
    let sv = squash(&compile_to_sv(MATCH_GENERIC));
    let body = module_body(&sv, "ModeRegs_9_12");
    assert!(
        body.contains("?9:") && body.contains("?13:"),
        "the combinational match must use MR5 = 9 and MR6 + 1 = 13:\n{body}"
    );
    assert!(
        body.contains("st<=9;") && body.contains("st<=12;"),
        "the registered match must use MR5 = 9 and MR6 = 12:\n{body}"
    );
}

async fn rising_edge(sim: &mut UnifiedSimulator) {
    sim.set_input("clk", 0).await;
    sim.step().await;
    sim.set_input("clk", 1).await;
    sim.step().await;
}

#[tokio::test]
async fn test_generic_match_arm_value_simulates() {
    let mut sim = simulator(MATCH_GENERIC, "ModeRegs_9_12").await;
    for (sel, q, r) in [(5u64, 9u64, 9u64), (6, 13, 12), (1, 0, 12)] {
        sim.set_input("sel", sel).await;
        rising_edge(&mut sim).await;
        assert_eq!(
            sim.get_output("q").await.unwrap_or(u64::MAX),
            q,
            "q sel={sel}"
        );
        assert_eq!(
            sim.get_output("r").await.unwrap_or(u64::MAX),
            r,
            "r sel={sel}"
        );
    }
}

// ---------------------------------------------------------------------------
// 4. Named and partial generic arguments on an instance
// ---------------------------------------------------------------------------

const NAMED_GENERIC_INSTANCES: &str = r#"
entity AddConsts<A: nat = 2, B: nat = 4> {
    in a: bit[8]
    out q: bit[8]
}
impl AddConsts {
    q = a + A + B
}
entity NamedGenerics {
    in a: bit[8]
    out q_named: bit[8]
    out q_partial: bit[8]
    out q_positional: bit[8]
    out q_both_named: bit[8]
    out q_default: bit[8]
}
impl NamedGenerics {
    inst named = AddConsts<B: 7> { a: a }
    inst partial = AddConsts<5> { a: a }
    inst positional = AddConsts<5, 6> { a: a }
    inst both_named = AddConsts<A: 1, B: 1> { a: a }
    inst dflt = AddConsts { a: a }
    q_named = named.q
    q_partial = partial.q
    q_positional = positional.q
    q_both_named = both_named.q
    q_default = dflt.q
}
"#;

#[test]
fn test_named_generic_instance_uses_its_specialization() {
    let sv = squash(&compile_to_sv(NAMED_GENERIC_INSTANCES));
    for (module, instance) in [
        ("AddConsts_2_7", "named"),
        ("AddConsts_5_4", "partial"),
        ("AddConsts_5_6", "positional"),
        ("AddConsts_1_1", "both_named"),
        ("AddConsts_2_4", "dflt"),
    ] {
        assert!(
            sv.contains(&format!("{module}{instance}(")),
            "`{instance}` must instantiate `{module}`:\n{sv}"
        );
        module_body(&sv, module);
    }
}

#[tokio::test]
async fn test_named_generic_instance_simulates() {
    let mut sim = simulator(NAMED_GENERIC_INSTANCES, "NamedGenerics").await;
    for a in [0u64, 10, 100] {
        sim.set_input("a", a).await;
        sim.step().await;
        for (output, sum) in [
            ("q_named", 9),
            ("q_partial", 9),
            ("q_positional", 11),
            ("q_both_named", 2),
            ("q_default", 6),
        ] {
            assert_eq!(
                sim.get_output(output).await.unwrap_or(u64::MAX),
                a + sum,
                "{output}, a={a}"
            );
        }
    }
}

// ---------------------------------------------------------------------------
// 5. Elaborating an instance that holds a flattened array
// ---------------------------------------------------------------------------

/// Two write sites make `mem` a flattened array: one register per element,
/// each written through `if a == i { mem_i = d }`.
fn two_port_array(depth: usize) -> String {
    format!(
        r#"
entity TwoPortMem<N: nat = 4> {{
    in clk: clock
    in we0: bit
    in we1: bit
    in a0: nat[8]
    in a1: nat[8]
    in d0: bit[16]
    in d1: bit[16]
    in ra: nat[8]
    out q: bit[16]
}}
impl TwoPortMem {{
    signal mem: [bit[16]; N]
    on(clk.rise) {{
        if we0 {{
            mem[a0] = d0
        }}
        if we1 {{
            mem[a1] = d1
        }}
    }}
    q = mem[ra]
}}
entity TwoPortTop {{
    in clk: clock
    in we0: bit
    in we1: bit
    in a0: nat[8]
    in a1: nat[8]
    in d0: bit[16]
    in d1: bit[16]
    in ra: nat[8]
    out q: bit[16]
}}
impl TwoPortTop {{
    inst m = TwoPortMem<N: {depth}> {{
        clk: clk, we0: we0, we1: we1, a0: a0, a1: a1, d0: d0, d1: d1, ra: ra
    }}
    q = m.q
}}
"#
    )
}

#[test]
fn test_flattened_array_instance_elaborates_in_linear_size() {
    // Every element's `if` used to rebuild `mux(cond, default, default)`
    // for every OTHER element's write as well: N² nodes, and an 8192-entry
    // array never finished elaborating. Doubling the depth must now roughly
    // double the design, not quadruple it.
    let small = elaborate_top(&two_port_array(32), "TwoPortTop")
        .combinational_nodes
        .len();
    let large = elaborate_top(&two_port_array(128), "TwoPortTop")
        .combinational_nodes
        .len();
    assert!(
        large < small * 5,
        "4x the depth must give about 4x the nodes, not 16x: {small} -> {large}"
    );
}

#[tokio::test]
async fn test_flattened_array_instance_simulates() {
    let mut sim = simulator(&two_port_array(16), "TwoPortTop").await;

    // Port 0 writes 3, port 1 writes 9; then both write 5 (port 1 wins, as
    // the later write in the block).
    for (we0, a0, d0, we1, a1, d1) in [
        (1u64, 3u64, 0x1111u64, 1u64, 9u64, 0x2222u64),
        (1, 5, 0x3333, 1, 5, 0x4444),
        (0, 3, 0xFFFF, 0, 9, 0xFFFF),
    ] {
        sim.set_input("we0", we0).await;
        sim.set_input("a0", a0).await;
        sim.set_input("d0", d0).await;
        sim.set_input("we1", we1).await;
        sim.set_input("a1", a1).await;
        sim.set_input("d1", d1).await;
        rising_edge(&mut sim).await;
    }
    for (ra, expected) in [(3u64, 0x1111u64), (9, 0x2222), (5, 0x4444), (0, 0)] {
        sim.set_input("ra", ra).await;
        sim.step().await;
        assert_eq!(
            sim.get_output("q").await.unwrap_or(u64::MAX),
            expected,
            "mem[{ra}]"
        );
    }
}

/// A one-bit signal (`bit[1]`) is declared as a scalar, so `s[0]` must be
/// written as `s`: SystemVerilog cannot select part of a scalar
#[test]
fn bit_zero_of_a_one_bit_signal_is_the_signal() {
    let source = r#"
entity OneBit {
    in clk: clock
    in d: bit
    out q: bit[4]
}

impl OneBit {
    signal s: bit[1] = 0
    on(clk.rise) {
        s = d
    }
    q = {3'b000, s[0]}
}
"#;
    let sv = compile_to_sv(source);
    assert!(!sv.contains("s[0]"), "scalar indexed:\n{sv}");
    assert!(!sv.contains("s[0:0]"), "scalar indexed:\n{sv}");
    assert!(sv.contains("{3'b000, s}"), "{sv}");
}
