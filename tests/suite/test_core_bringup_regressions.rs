//! Second round of regressions found while bringing up an RV64I core in SKALP.
//!
//! 4. A `let` in a child instance read 0 in behavioral simulation.
//! 5. A `let` with the name of a port hid the port, and the only error was
//!    "output port is never driven".
//! 6. SystemVerilog had an index of a slice (`x[31:0][31]`), which is illegal.
//! 7. A bit-select lowered to `(x >> 7) & 1'b1` is as wide as `x` in
//!    SystemVerilog, so `{56{...}}` replicated a 64-bit value.
//! 8. Behavioral simulation did not truncate values to their declared width:
//!    an add's carry and a shift's high bits reached wider readers.
//! 9. A register file read combinationally at two addresses: both reads shared
//!    one read address at gate level, and on FPGA targets the memory went into
//!    synchronous block RAM. A LUT-merging pass also mis-built padded LUTs.

use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::monomorphization::MonomorphizationEngine;
use skalp_frontend::parse::parse;
use skalp_mir::lower_to_mir;
use skalp_testing::Testbench;

fn fixture_path(name: &str) -> String {
    format!("{}/tests/fixtures/{}", env!("CARGO_MANIFEST_DIR"), name)
}

fn compile_to_sv(source: &str) -> String {
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building failed");
    let mut engine = MonomorphizationEngine::new();
    let hir = engine.monomorphize(&hir);
    let mir = lower_to_mir(&hir).expect("MIR lowering failed");
    skalp_codegen::generate_systemverilog_from_mir(&mir).expect("SystemVerilog generation failed")
}

fn squash(sv: &str) -> String {
    sv.chars().filter(|c| !c.is_whitespace()).collect()
}

// 4 ---------------------------------------------------------------------------

#[tokio::test]
async fn test_let_in_child_instance_simulates() {
    let mut tb = Testbench::with_top_module(&fixture_path("let_in_child.sk"), "K")
        .await
        .unwrap();
    tb.set("x", 0x1234_566fu64);
    tb.clock(2).await;
    assert_eq!(
        tb.get_u64("a").await,
        0x6f,
        "child `let lo = r[6:0]` must reach a"
    );
}

// 5 ---------------------------------------------------------------------------

#[test]
fn test_let_shadowing_port_is_reported() {
    let source = r#"
entity A {
    in x: bit[8]
    out m0: bit
}
impl A {
    let m0: bit = x[0]
}
"#;
    let tree = parse(source);
    let errors = build_hir(&tree).expect_err("a let named like a port must be an error");
    let text = format!("{:?}", errors);
    assert!(
        text.contains("`let m0` shadows port `m0`"),
        "unexpected diagnostic: {}",
        text
    );
}

// 6 ---------------------------------------------------------------------------

#[test]
fn test_no_index_of_slice_in_sv() {
    let source = r#"
fn sra32(a: bit[32], sh: bit[5]) -> bit[32] {
    let sg: bit = a[31]
    let fill: bit[32] = if sg == 1 { ~(32'hffffffff >> sh) } else { 0 }
    return (a >> sh) | fill
}
entity B6 {
    in a64: bit[64]
    in sh: bit[5]
    out q: bit[32]
    out t: bit[4]
}
impl B6 {
    q = sra32(a64[31:0], sh)
    t = a64[39:32][5:2]
}
"#;
    let sv = squash(&compile_to_sv(source));
    assert!(!sv.contains("a64[31:0][31]"), "index of a slice:\n{}", sv);
    assert!(
        sv.contains("a64[31]"),
        "bit 31 of the slice is a64[31]:\n{}",
        sv
    );
    assert!(sv.contains("a64[37:34]"), "slice of a slice folds:\n{}", sv);
}

// 7 ---------------------------------------------------------------------------

#[test]
fn test_replicated_bit_select_is_one_bit_in_sv() {
    let source = r#"
fn load_b(word: bit[64], off: bit[3]) -> bit[64] {
    let w: bit[64] = word >> {off, 3'b000}
    let s8: bit = w[7]
    return {{56{s8}}, w[7:0]}
}
entity B7 {
    in word: bit[64]
    in off: bit[3]
    out q: bit[64]
}
impl B7 {
    q = load_b(word, off)
}
"#;
    let sv = squash(&compile_to_sv(source));
    // Each lowered select is size-cast, so the replicated operand is 1 bit wide.
    assert!(
        sv.contains("{56{1'("),
        "replicated select must be sized:\n{}",
        sv
    );
    assert!(sv.contains("8'("), "part-select must be sized:\n{}", sv);
}

// 8 ---------------------------------------------------------------------------

#[tokio::test]
async fn test_sim_truncates_to_declared_width() {
    let mut tb = Testbench::with_top_module(&fixture_path("alu_truncation.sk"), "Trunc")
        .await
        .unwrap();
    tb.set("a", 0xffff_fff0u64).set("b", 0x20u64).set("sh", 8u8);
    tb.clock(1).await;
    assert_eq!(
        tb.get_u64("sum64").await,
        0x10,
        "carry out of bit 31 leaked"
    );
    assert_eq!(
        tb.get_u64("shl64").await,
        0xffff_f000,
        "shifted-out bits leaked"
    );
}

// 9 ---------------------------------------------------------------------------

async fn check_regfile(mut tb: Testbench, what: &str) {
    let val = |i: u64| i * 0x0101_0101 + 7;
    for i in 0..8u64 {
        tb.set("we", 1u8).set("wa", i as u8).set("wd", val(i));
        tb.clock(1).await;
    }
    tb.set("we", 0u8);
    for i in 0..8u64 {
        tb.set("ra", i as u8).set("rb", (7 - i) as u8);
        tb.clock(1).await;
        assert_eq!(tb.get_u64("qa").await, val(i), "{what}: qa = rf[{i}]");
        assert_eq!(
            tb.get_u64("qb").await,
            val(7 - i),
            "{what}: qb = rf[{}]",
            7 - i
        );
    }
}

#[tokio::test]
async fn test_regfile_two_reads_gate_level_asic() {
    let path = fixture_path("regfile_two_reads.sk");
    let tb = Testbench::gate_level_with_top_and_library(&path, "Rf", "generic_asic")
        .await
        .unwrap();
    check_regfile(tb, "generic_asic").await;
}

#[tokio::test]
async fn test_regfile_two_reads_gate_level_nexus() {
    let path = fixture_path("regfile_two_reads.sk");
    let tb = Testbench::gate_level_with_top_and_library(&path, "Rf", "nexus")
        .await
        .unwrap();
    check_regfile(tb, "nexus").await;
}

#[test]
fn test_async_read_memory_stays_out_of_block_ram() {
    let path = std::path::PathBuf::from(fixture_path("regfile_two_reads.sk"));
    let ctx = skalp_frontend::parse_and_build_compilation_context(&path).unwrap();
    let mir = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .unwrap();
    let (hier, _) = skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir);
    let library = skalp_lir::get_stdlib_library("nexus").unwrap();
    let net = skalp_lir::synthesize_hierarchical(
        &hier,
        &library,
        skalp_lir::synth::SynthPreset::Balanced,
    )
    .flatten();
    let rams = net
        .cells
        .iter()
        .filter(|c| c.function == Some(skalp_lir::CellFunction::Ram))
        .count();
    assert_eq!(
        rams, 0,
        "a combinationally read memory must not use block RAM"
    );
}

async fn check_nested_if_bit_set(mut tb: Testbench, what: &str) {
    tb.set("x", 0u8);
    tb.clock(3).await;
    assert_eq!(tb.get_u64("q").await, 0, "{what}: no bit before x is set");
    tb.set("x", 1u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u64("q").await, 1 << 5, "{what}: flags[5] set");
    tb.set("x", 0u8);
    tb.clock(3).await;
    assert_eq!(tb.get_u64("q").await, 1 << 5, "{what}: flags[5] stays set");
}

#[tokio::test]
async fn test_nested_if_bit_set_behavioral() {
    let tb = Testbench::new(&fixture_path("nested_if_bit_set.sk"))
        .await
        .unwrap();
    check_nested_if_bit_set(tb, "behavioral").await;
}

#[tokio::test]
async fn test_nested_if_bit_set_gate_level_asic() {
    let path = fixture_path("nested_if_bit_set.sk");
    let tb = Testbench::gate_level_with_top_and_library(&path, "Bits", "generic_asic")
        .await
        .unwrap();
    check_nested_if_bit_set(tb, "generic_asic").await;
}

#[tokio::test]
async fn test_nested_if_bit_set_gate_level_nexus() {
    let path = fixture_path("nested_if_bit_set.sk");
    let tb = Testbench::gate_level_with_top_and_library(&path, "Bits", "nexus")
        .await
        .unwrap();
    check_nested_if_bit_set(tb, "nexus").await;
}

async fn check_register_part_writes(mut tb: Testbench, what: &str) {
    // Power-up value, before any write.
    tb.set("rst", 0u8)
        .set("op", 1u8)
        .set("v", 0xAu8)
        .set("idx", 0u8);
    assert_eq!(tb.get_u64("r").await, 0xA5, "{what}: power-up value");
    tb.clock(1).await;
    assert_eq!(tb.get_u64("r").await, 0xA5, "{what}: r[7:4] <= v");
    tb.set("rst", 1u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u64("r").await, 0, "{what}: reset");
    assert_eq!(tb.get_u64("p").await, 0x0F, "{what}: p reset");
    tb.set("rst", 0u8).set("op", 0u8).set("v", 1u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u64("r").await, 0x42, "{what}: two bits in one cycle");
    tb.set("op", 1u8).set("v", 0x9u8);
    tb.clock(1).await;
    assert_eq!(
        tb.get_u64("r").await,
        0x92,
        "{what}: range write keeps low bits"
    );
    tb.set("op", 2u8).set("idx", 3u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u64("r").await, 0x9A, "{what}: run-time index");
    assert_eq!(
        tb.get_u64("p").await,
        0x07,
        "{what}: port bit at run-time index"
    );
    tb.set("op", 3u8).set("v", 0xCu8);
    tb.clock(1).await;
    assert_eq!(
        tb.get_u64("r").await,
        0xA9,
        "{what}: whole write after parts"
    );
    assert_eq!(tb.get_u64("p").await, 0x0C, "{what}: port range write");
}

#[tokio::test]
async fn test_register_part_writes_behavioral() {
    let tb = Testbench::new(&fixture_path("register_part_writes.sk"))
        .await
        .unwrap();
    check_register_part_writes(tb, "behavioral").await;
}

#[tokio::test]
async fn test_register_part_writes_gate_level_asic() {
    let path = fixture_path("register_part_writes.sk");
    let tb = Testbench::gate_level_with_top_and_library(&path, "Parts", "generic_asic")
        .await
        .unwrap();
    check_register_part_writes(tb, "generic_asic").await;
}

#[tokio::test]
async fn test_register_part_writes_gate_level_nexus() {
    let path = fixture_path("register_part_writes.sk");
    let tb = Testbench::gate_level_with_top_and_library(&path, "Parts", "nexus")
        .await
        .unwrap();
    check_register_part_writes(tb, "nexus").await;
}

// Instance ports wired to expressions -----------------------------------------
//
// `inst u = M { x: ~a }` lowers the expression into a parent signal
// `u__port__x` that the child port is stitched to. The lowering used by the
// gate flows did not create that signal, so the child input had no driver.

fn port_expression_expected(a: u64, b: u64) -> u64 {
    (!a ^ (b & 0b0110)) & 0xF
}

async fn check_port_expressions(mut tb: Testbench, what: &str) {
    for a in 0..16u64 {
        for b in [0u64, 0b0010, 0b0100, 0b0110, 0b1111] {
            tb.set("a", a).set("b", b);
            tb.step().await;
            assert_eq!(
                tb.get_u64("z").await,
                port_expression_expected(a, b),
                "{what}: a={a:#x} b={b:#x}"
            );
        }
    }
}

#[tokio::test]
async fn test_instance_port_expressions_behavioral() {
    let path = fixture_path("instance_port_expression.sk");
    let tb = Testbench::with_top_module(&path, "PortExpr").await.unwrap();
    check_port_expressions(tb, "behavioral").await;
}

#[tokio::test]
async fn test_instance_port_expressions_gate_level_asic() {
    let path = fixture_path("instance_port_expression.sk");
    let tb = Testbench::gate_level_with_top_and_library(&path, "PortExpr", "generic_asic")
        .await
        .unwrap();
    check_port_expressions(tb, "generic_asic").await;
}

#[tokio::test]
async fn test_instance_port_expressions_gate_level_nexus() {
    let path = fixture_path("instance_port_expression.sk");
    let tb = Testbench::gate_level_with_top_and_library(&path, "PortExpr", "nexus")
        .await
        .unwrap();
    check_port_expressions(tb, "nexus").await;
}

/// The CLI gate flow without optimisation (the path NCL designs take) refuses
/// a netlist with an undriven input, so a lost port expression fails the build.
#[test]
fn test_instance_port_expressions_unoptimised_gate_build() {
    for fixture in [
        "instance_port_expression.sk",
        "instance_port_expression_async.sk",
    ] {
        let out = std::env::temp_dir().join(format!(
            "skalp_port_expr_{}_{}",
            std::process::id(),
            fixture.trim_end_matches(".sk")
        ));
        let output = std::process::Command::new(env!("CARGO_BIN_EXE_skalp"))
            .args(["build", "-s", &fixture_path(fixture), "-o"])
            .arg(&out)
            .args(["--target", "gates", "--no-synth-opt", "--no-async-sta"])
            .output()
            .expect("failed to run skalp");
        assert!(
            output.status.success(),
            "{fixture}: gate build failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        // The expression signals are driven inside the design; they must not
        // turn into primary inputs of the top.
        let json =
            std::fs::read_to_string(out.join("design_gates.json")).expect("gate netlist written");
        let _ = std::fs::remove_dir_all(&out);
        let netlist: skalp_lir::gate_netlist::GateNetlist =
            serde_json::from_str(&json).expect("gate netlist parses");
        let leaked: Vec<&str> = netlist
            .nets
            .iter()
            .filter(|n| n.is_input && n.name.contains("__port__"))
            .map(|n| n.name.as_str())
            .collect();
        assert!(
            leaked.is_empty(),
            "{fixture}: port expressions became top inputs: {leaked:?}"
        );
    }
}

// Operators routed through stdlib entities -------------------------------------
//
// `impl Sub for bit<N>` ends in `subtractor.sum`, naming its `inst subtractor`.
// The instance name did not resolve inside the function body, so inlining
// failed and the operator was lowered as a primitive while the instance it had
// already created stayed behind, unconnected and unused.

#[test]
fn test_operators_use_their_stdlib_instance() {
    // The stdlib's operator impls are preloaded from SKALP_STDLIB_PATH; set it
    // on a child process only, since it changes compilation process-wide.
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_skalp"))
        .env(
            "SKALP_STDLIB_PATH",
            format!("{}/crates/skalp-stdlib", env!("CARGO_MANIFEST_DIR")),
        )
        .args([
            "build",
            "-s",
            &fixture_path("operator_through_stdlib.sk"),
            "--emit",
            "mir",
        ])
        .output()
        .expect("failed to run skalp");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let mir: skalp_mir::Mir = serde_json::from_slice(&output.stdout).expect("MIR dump parses");
    let top = mir
        .modules
        .iter()
        .find(|m| m.name == "OpThroughStdlib")
        .unwrap();
    assert_eq!(top.instances.len(), 2, "one adder per operator");
    for inst in &top.instances {
        let driven = top.assignments.iter().any(|a| {
            matches!(&a.rhs.kind, skalp_mir::ExpressionKind::Ref(skalp_mir::LValue::Signal(id))
                if top.signals.iter().any(|s| s.id == *id && s.name.starts_with(inst.name.as_str())))
        });
        assert!(driven, "output of {} is not used", inst.name);
        // The impl body never binds `sum` in the instantiation (it reads
        // `adder.sum` afterwards); the instance must still drive its signal,
        // or the netlist leaves the port open.
        assert!(
            inst.connections.contains_key("sum"),
            "instance {} leaves its `sum` output unconnected: {:?}",
            inst.name,
            inst.connections.keys().collect::<Vec<_>>()
        );
    }
    for a in &top.assignments {
        assert!(
            !matches!(a.rhs.kind, skalp_mir::ExpressionKind::Binary { .. }),
            "operator lowered inline as well: {:?}",
            a.rhs.kind
        );
    }
}

async fn check_operators(mut tb: Testbench, what: &str) {
    for (a, b) in [
        (100u64, 50u64),
        (0, 0),
        (50, 100),
        (255, 1),
        (1, 255),
        (128, 128),
    ] {
        tb.set("a", a).set("b", b);
        tb.step().await;
        assert_eq!(tb.get_u64("s").await, (a + b) & 0xFF, "{what}: {a}+{b}");
        assert_eq!(
            tb.get_u64("d").await,
            a.wrapping_sub(b) & 0xFF,
            "{what}: {a}-{b}"
        );
    }
}

#[tokio::test]
async fn test_operators_through_stdlib_behavioral() {
    let tb = Testbench::new(&fixture_path("operator_through_stdlib.sk"))
        .await
        .unwrap();
    check_operators(tb, "behavioral").await;
}

#[tokio::test]
async fn test_operators_through_stdlib_gate_level_nexus() {
    let path = fixture_path("operator_through_stdlib.sk");
    let tb = Testbench::gate_level_with_top_and_library(&path, "OpThroughStdlib", "nexus")
        .await
        .unwrap();
    check_operators(tb, "nexus").await;
}

// `skalp synth --emit` ---------------------------------------------------------
//
// `--emit lir` looked the top up by module name in a hierarchy keyed by
// instance path and always failed; every dump was also preceded by banner text
// on stdout, so none of them parsed as JSON.

#[test]
fn test_synth_emit_dumps_are_plain_json() {
    let source = fixture_path("instance_port_expression.sk");
    for stage in ["hir", "mir", "lir"] {
        let output = std::process::Command::new(env!("CARGO_BIN_EXE_skalp"))
            .args(["synth", &source, "-d", "nexus-lfcpnx100", "--emit", stage])
            .output()
            .expect("failed to run skalp");
        assert!(
            output.status.success(),
            "--emit {stage} failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        let dump: serde_json::Value = serde_json::from_slice(&output.stdout)
            .unwrap_or_else(|e| panic!("--emit {stage} is not JSON: {e}"));
        if stage == "lir" {
            assert_eq!(dump["top"]["module"], "PortExpr");
            assert_eq!(dump["top.u"]["module"], "PassThrough");
            assert!(dump["top"]["lir"]["nodes"].is_array());
        }
    }
}

// Bit-manipulation builtins on signals -------------------------------------------
//
// `clz(x)` and friends fold in the frontend when the argument is constant. On
// a signal they reached the function inliner, found no definition, and the
// whole initializer was dropped: `signal msb: nat = 15 - clz(v)` had no
// driver (the floating-input check caught it inside the stdlib's FpDiv). A
// signal initializer that cannot be lowered is now an error.

fn builtin_expected(v: u64, w: u64) -> [u64; 7] {
    let lz = if v == 0 {
        12
    } else {
        (v as u16).leading_zeros() as u64 - 4
    };
    let tz = if v == 0 {
        12
    } else {
        v.trailing_zeros() as u64
    };
    let norm = if v == 0 { 0 } else { (v << lz) & 0xFFF };
    [
        lz,
        tz,
        v.count_ones() as u64,
        (v != 0 && v & (v - 1) == 0) as u64,
        v.min(w),
        v.max(w),
        norm,
    ]
}

async fn check_builtins(mut tb: Testbench, what: &str) {
    let names = ["lz", "tz", "ones", "pow2", "lo", "hi", "norm"];
    for (v, w) in [
        (0u64, 5u64),
        (1, 0),
        (0x800, 0x7FF),
        (0x400, 0x400),
        (0x0F0, 0x100),
        (0xFFF, 0x001),
        (0x028, 0x3A5),
        (0x001, 0xFFF),
    ] {
        tb.set("v", v).set("w", w);
        tb.step().await;
        for (name, expected) in names.iter().zip(builtin_expected(v, w)) {
            assert_eq!(
                tb.get_u64(name).await,
                expected,
                "{what}: {name} for v={v:#x} w={w:#x}"
            );
        }
    }
}

#[tokio::test]
async fn test_runtime_builtins_behavioral() {
    let tb = Testbench::new(&fixture_path("runtime_builtins.sk"))
        .await
        .unwrap();
    check_builtins(tb, "behavioral").await;
}

/// With the standard library loaded, `11 - clz(v)` goes through `impl Sub`
/// (`std_adder<32> { a, b: ~b, carry_in: 1 }`). The 4-bit `clz(v)` has to be
/// widened to 32 bits before the `~`: inverted first, `11 - 11` came out 16
/// in the behavioral simulator and `norm` read 0.
#[tokio::test]
async fn test_runtime_builtins_through_stdlib_operators() {
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        format!("{}/crates/skalp-stdlib", env!("CARGO_MANIFEST_DIR")),
    );
    let tb = Testbench::new(&fixture_path("runtime_builtins.sk"))
        .await
        .unwrap();
    check_builtins(tb, "behavioral, stdlib operators").await;
}

#[tokio::test]
async fn test_runtime_builtins_gate_level_nexus() {
    let path = fixture_path("runtime_builtins.sk");
    let tb = Testbench::gate_level_with_top_and_library(&path, "RuntimeBuiltins", "nexus")
        .await
        .unwrap();
    check_builtins(tb, "nexus").await;
}

#[test]
fn test_signal_initializer_that_cannot_lower_is_an_error() {
    let source = r#"
entity U {
    in v: bit[8]
    out y: bit[8]
}
impl U {
    signal t: bit[8] = no_such_function(v)
    y = t
}
"#;
    let dir = std::env::temp_dir().join(format!("skalp_init_err_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("u.sk");
    std::fs::write(&path, source).unwrap();
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_skalp"))
        .args(["build", "-s"])
        .arg(&path)
        .args(["--emit", "mir"])
        .output()
        .expect("failed to run skalp");
    let _ = std::fs::remove_dir_all(&dir);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!output.status.success(), "build must fail");
    assert!(
        stderr.contains("initializer of signal `t` could not be lowered"),
        "unexpected error: {stderr}"
    );
}

// Floating-point trait methods called by name ------------------------------------
//
// `x.add(y)` in a signal initializer was parsed as the field access `x.add`
// (call and argument dropped), and `sqrt(x)` went through the plain function
// inliner, which reads the impl body's `let sqrt_op = FpSqrt<..> { .. }` as a
// struct literal: dropped when inlined into an entity, and "unknown type
// `FpSqrt`" once the caller had enough calls to be synthesized as a module.

const FP_ONE_POINT_FIVE: u64 = 0x3FC0_0000;
const FP_TWO_POINT_TWO_FIVE: u64 = 0x4010_0000;
const FP_THREE_POINT_SEVEN_FIVE: u64 = 0x4070_0000;
const FP_THREE_POINT_THREE_SEVEN_FIVE: u64 = 0x4058_0000;
const FP_FOUR: u64 = 0x4080_0000;
const FP_TWO: u64 = 0x4000_0000;
const FP_FOUR_POINT_EIGHT_SEVEN_FIVE: u64 = 0x409C_0000;
const FP_FOUR_POINT_FIVE: u64 = 0x4090_0000;
const FP_TWO_THIRDS: u64 = 0x3F2A_AAAB;

#[tokio::test]
async fn test_fp_trait_methods_called_by_name() {
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        format!("{}/crates/skalp-stdlib", env!("CARGO_MANIFEST_DIR")),
    );
    let mut tb = Testbench::new(&fixture_path("fp_trait_method_calls.sk"))
        .await
        .unwrap();

    // 1.5 + 2.25, 1.5 * 2.25 through `x.add(y)` / `x.mul(y)`.
    tb.set("op", 0u64)
        .set("a", FP_ONE_POINT_FIVE)
        .set("b", FP_TWO_POINT_TWO_FIVE);
    tb.step().await;
    assert_eq!(
        tb.get_u64("sum").await,
        FP_THREE_POINT_SEVEN_FIVE,
        "x.add(y)"
    );
    assert_eq!(
        tb.get_u64("prod").await,
        FP_THREE_POINT_THREE_SEVEN_FIVE,
        "x.mul(y)"
    );
    assert_eq!(
        tb.get_u64("unit").await,
        FP_THREE_POINT_SEVEN_FIVE,
        "exec add"
    );

    // The module-synthesized function, each arm.
    for (op, expected, what) in [
        (1u64, FP_THREE_POINT_THREE_SEVEN_FIVE, "exec mul"),
        (2, FP_FOUR_POINT_EIGHT_SEVEN_FIVE, "exec mul-add"),
        (4, FP_TWO_THIRDS, "exec divide"),
        (5, FP_FOUR_POINT_FIVE, "exec b + b"),
    ] {
        tb.set("op", op);
        tb.step().await;
        assert_eq!(tb.get_u64("unit").await, expected, "{what}");
    }

    // sqrt(4.0) in an initializer and in the module-synthesized function.
    tb.set("op", 3u64).set("a", FP_FOUR);
    tb.step().await;
    assert_eq!(tb.get_u64("root").await, FP_TWO, "sqrt(x) initializer");
    assert_eq!(tb.get_u64("unit").await, FP_TWO, "exec sqrt");
}

/// Without `use skalp::numeric::fp::*`, the impl bodies' `FpAdd<IEEE754_32>`
/// named a constant the evaluator could not see from the design, so the
/// instance fell back to the bare generic `FpAdd` — a template with no body —
/// and the outputs floated (`sqrt` also lost its intent default). Every FP
/// instance must be a real specialization with logic in it.
#[test]
fn test_fp_ops_without_import_instantiate_specializations() {
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_skalp"))
        .env(
            "SKALP_STDLIB_PATH",
            format!("{}/crates/skalp-stdlib", env!("CARGO_MANIFEST_DIR")),
        )
        .args([
            "build",
            "-s",
            &fixture_path("fp_trait_method_calls.sk"),
            "--emit",
            "mir",
        ])
        .output()
        .expect("failed to run skalp");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let mir: skalp_mir::Mir = serde_json::from_slice(&output.stdout).expect("MIR dump parses");
    let mut seen = 0;
    for owner in ["FpMethodCalls", "func_exec"] {
        let module = mir.modules.iter().find(|m| m.name == owner).unwrap();
        for inst in &module.instances {
            let target = mir.modules.iter().find(|m| m.id == inst.module).unwrap();
            if target.name == "func_exec" {
                continue;
            }
            seen += 1;
            assert!(
                target.name.ends_with("_fp32") && !target.signals.is_empty(),
                "`{}` in `{owner}` instantiates `{}`, not a populated fp32 specialization",
                inst.name,
                target.name
            );
        }
    }
    assert!(seen >= 8, "expected the FP instances, saw {seen}");
}

// ============================================================================
// Compiled behavioral sim with 65..128-bit values
// ============================================================================
//
// 65..128-bit signals are `unsigned __int128` scalars in the compiled C++ sim
// and wider ones are u32 limb arrays. Constants of that width were emitted as
// invalid C++, a widening 96x96 multiply wrote an array into the scalar,
// truncating an array to a scalar kept only its first limb, wide registers
// had no local copy in the batched kernel, and unaligned 33..128-bit slices of
// limb arrays dropped limbs.

#[tokio::test]
async fn test_compiled_sim_wide_arith_65_to_128_bits() {
    let mut tb = Testbench::new(&fixture_path("wide_arith_128.sk"))
        .await
        .expect("wide_arith_128.sk should build in the compiled behavioral sim");
    let mask48: u128 = (1 << 48) - 1;
    let mask96: u128 = (1 << 96) - 1;
    let bias: u128 = (1u128 << 64) | 3;
    for (a, b) in [
        (3u64, 5u64),
        (0xFFFF_FFFF_FFFF, 0xFFFF_FFFF_FFFF),
        (0x1234_5678_9ABC, 0xFEDC_BA98_7654),
        (0x8000_0000_0001, 0x0000_0001_0000),
    ] {
        tb.set("a", a).set("b", b);
        tb.clock(2).await;
        let prod = (a as u128) * (b as u128);
        let sum = (prod + bias) & mask96;
        let diff = prod.wrapping_sub(bias) & mask96;
        // wide = {prod, 64'h0123456789ABCDEF}; m = wide[119:48]; mid = m[71:24]
        let wide_low = 0x0123_4567_89AB_CDEFu128;
        let m = ((prod << 64 | wide_low) >> 48) & ((1u128 << 72) - 1);
        let expect = [
            ("prod_lo", prod & mask48),
            ("prod_hi", prod >> 48),
            ("sum_hi", sum >> 48),
            ("diff_lo", diff & mask48),
            ("mid", (m >> 24) & mask48),
        ];
        for (port, want) in expect {
            let got = tb.get_u64(port).await;
            assert_eq!(
                got as u128, want,
                "{port} for a={a:#x} b={b:#x}: got {got:#x}, want {want:#x}"
            );
        }
    }
}

// ============================================================================
// SystemVerilog with the stdlib preloaded
// ============================================================================
//
// With SKALP_STDLIB_PATH set, every stdlib entity is monomorphized whether or
// not the design uses it, and all of them were emitted into design.sv. Inside
// CordicSqrt the inlined FP operator methods' `result` locals were declared
// next to the `result` output port, so Icarus rejected the file.

fn build_sv_with_stdlib(fixture: &str) -> String {
    let out = tempfile::tempdir().expect("temp dir");
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_skalp"))
        .env(
            "SKALP_STDLIB_PATH",
            format!("{}/crates/skalp-stdlib", env!("CARGO_MANIFEST_DIR")),
        )
        .args(["build", "-s", &fixture_path(fixture), "-o"])
        .arg(out.path())
        .output()
        .expect("failed to run skalp");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    std::fs::read_to_string(out.path().join("design.sv")).expect("design.sv written")
}

fn sv_module_names(sv: &str) -> Vec<String> {
    sv.lines()
        .filter_map(|l| l.strip_prefix("module "))
        .map(|rest| {
            rest.split(|c: char| !(c.is_alphanumeric() || c == '_'))
                .next()
                .unwrap_or("")
                .to_string()
        })
        .collect()
}

/// Every name declared in a module (ports, nets, variables, instances) must
/// be declared once: SystemVerilog has a single namespace per module.
fn assert_unique_declarations(sv: &str) {
    let mut module = String::new();
    let mut seen = std::collections::HashSet::new();
    for line in sv.lines() {
        if let Some(rest) = line.strip_prefix("module ") {
            module = rest.split([' ', '(', '#']).next().unwrap_or("").to_string();
            seen.clear();
            continue;
        }
        let t = line.trim().trim_end_matches([';', ',']);
        let decl = ["input ", "output ", "inout ", "wire ", "logic ", "reg "]
            .iter()
            .find_map(|k| t.strip_prefix(k));
        let Some(decl) = decl else { continue };
        let decl = decl.split('=').next().unwrap_or("").trim();
        // The name is the last identifier before any unpacked dimension.
        let mut decl = decl;
        while decl.ends_with(']') {
            decl = decl[..decl.rfind('[').unwrap_or(0)].trim_end();
        }
        let name = decl
            .split(|c: char| !(c.is_alphanumeric() || c == '_'))
            .rfind(|s| !s.is_empty())
            .unwrap_or("")
            .to_string();
        assert!(
            seen.insert(name.clone()),
            "`{name}` declared twice in module {module}"
        );
    }
}

#[test]
#[should_panic(expected = "`result` declared twice in module M")]
fn test_unique_declaration_check_catches_a_repeat() {
    assert_unique_declarations(
        "module M (\n    input [31:0] value,\n    output [31:0] result\n);\n    logic [31:0] result;\nendmodule\n",
    );
}

#[test]
fn test_unused_stdlib_entities_stay_out_of_the_netlist() {
    let sv = build_sv_with_stdlib("stdlib_unused.sk");
    assert_eq!(sv_module_names(&sv), vec!["NoStdlib".to_string()], "{sv}");
}

#[test]
fn test_stdlib_entity_netlist_has_only_its_dependencies_and_unique_names() {
    let sv = build_sv_with_stdlib("stdlib_cordic_sqrt_user.sk");
    let modules = sv_module_names(&sv);
    for needed in ["SqrtUser", "CordicSqrt", "CordicHyperbolicIteration"] {
        assert!(
            modules.iter().any(|m| m == needed),
            "{needed} missing: {modules:?}"
        );
    }
    for unused in ["CordicRotate16", "CordicVector16", "CordicRotateIteration"] {
        assert!(
            !modules.iter().any(|m| m == unused),
            "{unused} emitted: {modules:?}"
        );
    }
    assert_unique_declarations(&sv);
}

// ============================================================================
// Instance input bound to a `let` mux, at gate level
// ============================================================================

async fn check_let_mux_instance_input(mut tb: Testbench, what: &str) {
    let mut s: u64 = 12345;
    for i in 0..50 {
        s = s
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        let a = (s >> 20) as u32;
        let b = (s >> 40) as u32 ^ (s as u32);
        let sel = (i % 3 == 0) as u8;
        tb.set("a", a).set("b", b).set("sel", sel);
        tb.clock(1).await;
        let bb = if sel == 1 { 0x3F80_0000 } else { b };
        let ma = (a as u64 & 0x7F_FFFF) | 0x80_0000;
        let mb = (bb as u64 & 0x7F_FFFF) | 0x80_0000;
        assert_eq!(
            (tb.get_u64("p").await, tb.get_u64("q").await),
            (ma * mb, ma ^ mb),
            "{what}: a={a:#x} b={b:#x} sel={sel}"
        );
    }
}

#[tokio::test]
async fn test_let_mux_instance_input_behavioral() {
    let tb = Testbench::with_top_module(
        &fixture_path("let_mux_instance_input.sk"),
        "LetMuxInstInput",
    )
    .await
    .unwrap();
    check_let_mux_instance_input(tb, "behavioral").await;
}

#[tokio::test]
async fn test_let_mux_instance_input_gate_level_nexus() {
    let tb = Testbench::gate_level_with_top_and_library(
        &fixture_path("let_mux_instance_input.sk"),
        "LetMuxInstInput",
        "nexus",
    )
    .await
    .unwrap();
    check_let_mux_instance_input(tb, "nexus").await;
}

#[tokio::test]
async fn test_let_mux_instance_input_gate_level_asic() {
    let tb = Testbench::gate_level_with_top_and_library(
        &fixture_path("let_mux_instance_input.sk"),
        "LetMuxInstInput",
        "generic_asic",
    )
    .await
    .unwrap();
    check_let_mux_instance_input(tb, "generic_asic").await;
}

// ============================================================================
// Concatenation parts keep their declared width in the compiled sim
// ============================================================================
//
// The C++ sim keeps an add/subtract's carry bit in the node's storage. A
// concatenation sized its parts by that stored width, so a 32-bit difference
// took 33 bits and pushed every later part up by one; with a borrow the extra
// bit also landed in the next part.

#[tokio::test]
async fn test_concat_parts_keep_declared_width_in_compiled_sim() {
    let mut tb = Testbench::new(&fixture_path("concat_carry_parts.sk"))
        .await
        .unwrap();
    for (x, y) in [(10u32, 3u32), (3, 10), (0xFFFF_FFFF, 1), (0, 0xFFFF_FFFF)] {
        tb.set("x", x).set("y", y);
        tb.clock(1).await;
        let sum = x.wrapping_add(y) as u64;
        let diff = x.wrapping_sub(y) as u64;
        assert_eq!(
            tb.get_u64("lo").await,
            (sum << 32) | diff,
            "x={x:#x} y={y:#x}"
        );
        assert_eq!(tb.get_u64("hi").await, diff, "x={x:#x} y={y:#x}");
        assert_eq!(
            tb.get_u64("small").await,
            (diff & 0xFFFF) << 16 | (sum & 0xFFFF),
            "x={x:#x} y={y:#x}"
        );
    }
}

/// 160-bit value as three u64 words, least significant first (top word holds 32 bits).
fn wide160(hi: u64, mid: u64, lo32: u32) -> [u64; 3] {
    // {hi(64), mid(64), lo32(32)}: bits 0..32 lo32, 32..96 mid, 96..160 hi
    [
        (lo32 as u64) | (mid << 32),
        (mid >> 32) | (hi << 32),
        hi >> 32,
    ]
}

fn wide160_bit(v: &[u64; 3], i: i64) -> u64 {
    if !(0..160).contains(&i) {
        0
    } else {
        (v[(i / 64) as usize] >> (i % 64)) & 1
    }
}

/// Bits [low, low + 64) of `v` shifted by `shift` (positive = left), with
/// `fill` for bits shifted in from above the top.
fn wide160_window(v: &[u64; 3], low: i64, shift: i64, fill: u64) -> u64 {
    (0..64).fold(0u64, |acc, j| {
        let src = low + j - shift;
        let bit = if src >= 160 {
            fill
        } else {
            wide160_bit(v, src)
        };
        acc | (bit << j)
    })
}

#[tokio::test]
async fn test_compiled_sim_wide_shift_compare_bitwise() {
    let mut tb = Testbench::new(&fixture_path("wide_shift_compare.sk"))
        .await
        .expect("wide_shift_compare.sk should build in the compiled behavioral sim");
    for (a, b, k) in [
        (0x0123_4567_89AB_CDEFu64, 0xFEDC_BA98_7654_3210u64, 0u8),
        (0x0123_4567_89AB_CDEF, 0xFEDC_BA98_7654_3210, 1),
        (0x8000_0000_0000_0001, 0x0000_0000_FFFF_FFFF, 37),
        (0xFFFF_0000_FFFF_0000, 0x1234_5678_9ABC_DEF0, 64),
        (0x8123_4567_89AB_CDEF, 0x0F0F_0F0F_0F0F_0F0F, 100),
        (0x0000_0000_0000_0005, 0x0000_0000_0000_0005, 200),
    ] {
        tb.set("a", a).set("b", b).set("k", k);
        tb.clock(1).await;
        let s = wide160(a, b, 0x89AB_CDEF);
        let t = wide160(b, a, 0x0123_4567);
        let k = k as i64;
        let sign = s[2] >> 31 & 1;
        let x = [s[0] ^ t[0], s[1] ^ t[1], s[2] ^ t[2]];
        let s_lt_t = (s[2], s[1], s[0]) < (t[2], t[1], t[0]);
        let what = format!("a={a:#x} b={b:#x} k={k}");
        assert_eq!(
            tb.get_u64("shl_mid").await,
            wide160_window(&s, 48, k, 0),
            "<< {what}"
        );
        assert_eq!(
            tb.get_u64("shr_mid").await,
            wide160_window(&s, 48, -k, 0),
            ">> {what}"
        );
        assert_eq!(
            tb.get_u64("sar_top").await,
            wide160_window(&s, 96, -k, sign),
            "sar {what}"
        );
        assert_eq!(
            tb.get_u64("xor_mid").await,
            wide160_window(&x, 48, 0, 0),
            "^ {what}"
        );
        assert_eq!(tb.get_u64("lt").await, s_lt_t as u64, "< {what}");
        assert_eq!(tb.get_u64("eq").await, (s == t) as u64, "== {what}");
    }
}
