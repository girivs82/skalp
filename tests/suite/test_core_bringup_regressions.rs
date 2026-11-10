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
