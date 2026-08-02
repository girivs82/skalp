//! HIR-based Code Generation Tests
//!
//! These tests verify the HIR codegen pipeline (parse → HIR → SV/VHDL/skalp)
//! without going through MIR lowering. They port the MIR codegen tests to
//! validate that HIR codegen produces equivalent output.

use skalp_frontend::parse_and_build_hir;
use skalp_hir_codegen::{generate_skalp_source, generate_systemverilog, generate_vhdl};

// ============================================================================
// Helper Functions
// ============================================================================

fn compile_hir_to_sv(source: &str) -> String {
    let hir = parse_and_build_hir(source).expect("HIR build should succeed");
    generate_systemverilog(&hir).expect("SV codegen should succeed")
}

fn compile_hir_to_vhdl(source: &str) -> String {
    let hir = parse_and_build_hir(source).expect("HIR build should succeed");
    generate_vhdl(&hir).expect("VHDL codegen should succeed")
}

fn compile_hir_to_skalp(source: &str) -> String {
    let hir = parse_and_build_hir(source).expect("HIR build should succeed");
    generate_skalp_source(&hir).expect("Skalp codegen should succeed")
}

// ============================================================================
// Tier 1: Core Codegen (Phases 1-2) — Basic module emission, output reg, async reset
// ============================================================================

#[test]
fn test_alu_codegen() {
    let sv = compile_hir_to_sv(
        r#"
entity ALU {
    in a: bit[32]
    in b: bit[32]
    in op: bit[3]
    in clk: clock
    out result: bit[32]
    out zero: bit
}

impl ALU {
    signal result_comb: bit[32]

    result_comb = match op {
        0b000 => a + b,
        0b001 => a - b,
        0b010 => a & b,
        0b011 => a | b,
        _ => 0
    };

    zero = if result_comb == 0 { 1 } else { 0 };

    on(clk.rise) {
        result = result_comb
    }
}
"#,
    );

    assert!(sv.contains("module ALU"), "Should have module declaration");
    assert!(sv.contains("always_ff"), "Should have always_ff block");
    assert!(
        sv.contains("output reg"),
        "Sequential output should be 'output reg'"
    );
}

#[test]
fn test_counter_codegen() {
    let sv = compile_hir_to_sv(
        r#"
entity Counter {
    in clk: clock
    in reset: bit
    in enable: bit
    out count: bit[8]
}

impl Counter {
    signal counter_reg: bit[8]

    on(clk.rise) {
        if reset {
            counter_reg = 0
        } else if enable {
            counter_reg = counter_reg + 1
        }
    }

    count = counter_reg
}
"#,
    );

    assert!(
        sv.contains("module Counter"),
        "Should have module declaration"
    );
    assert!(sv.contains("always_ff"), "Should have always_ff block");
    assert!(
        sv.contains("counter_reg"),
        "Should reference counter register"
    );
}

#[test]
fn test_simple_wire() {
    let sv = compile_hir_to_sv(
        r#"
entity Wire {
    in a: bit[8]
    out b: bit[8]
}

impl Wire {
    b = a
}
"#,
    );

    assert!(sv.contains("module Wire"));
    assert!(sv.contains("assign b = a"));
}

#[test]
fn test_simple_and_gate() {
    let sv = compile_hir_to_sv(
        r#"
entity AndGate {
    in a: bit
    in b: bit
    out y: bit
}

impl AndGate {
    y = a & b
}
"#,
    );

    assert!(sv.contains("module AndGate"));
    assert!(sv.contains("assign y = (a & b)"));
}

#[test]
fn test_simple_mux() {
    let sv = compile_hir_to_sv(
        r#"
entity Mux2 {
    in sel: bit
    in a: bit[8]
    in b: bit[8]
    out y: bit[8]
}

impl Mux2 {
    y = if sel { b } else { a }
}
"#,
    );

    assert!(sv.contains("module Mux2"));
    assert!(sv.contains("?"), "Should use ternary for mux");
}

#[test]
fn test_register() {
    let sv = compile_hir_to_sv(
        r#"
entity Register {
    in clk: clock
    in d: bit[8]
    out q: bit[8]
}

impl Register {
    on(clk.rise) {
        q = d
    }
}
"#,
    );

    assert!(sv.contains("module Register"));
    assert!(sv.contains("always_ff"));
    assert!(
        sv.contains("output reg"),
        "q is assigned in always_ff so should be output reg"
    );
}

#[test]
fn test_output_reg_classification() {
    let sv = compile_hir_to_sv(
        r#"
entity MixedOutputs {
    in clk: clock
    in a: bit[8]
    out comb_out: bit[8]
    out seq_out: bit[8]
}

impl MixedOutputs {
    comb_out = a

    on(clk.rise) {
        seq_out = a
    }
}
"#,
    );

    // comb_out is combinational → "output"
    // seq_out is in always_ff → "output reg"
    assert!(sv.contains("output reg"), "seq_out should be output reg");
    // The combinational output should just be "output" (not "output reg")
    let lines: Vec<&str> = sv.lines().collect();
    let comb_line = lines.iter().find(|l| l.contains("comb_out")).unwrap();
    assert!(
        !comb_line.contains("reg"),
        "comb_out should NOT be output reg"
    );
}

// ============================================================================
// Tier 1: Async Reset Tests
// ============================================================================

#[test]
fn test_async_reset_active_high() {
    let sv = compile_hir_to_sv(
        r#"
entity AsyncResetTest {
    in clk: clock
    in rst: reset(active_high)
    in data: nat[8]
    out result: nat[8]
}

impl AsyncResetTest {
    signal state: nat[8]

    on(clk.rise | rst.active) {
        if rst {
            state = 0
        } else {
            state = data
        }
    }

    result = state
}
"#,
    );

    println!("SV:\n{sv}");
    assert!(
        sv.contains("posedge clk or posedge rst"),
        "Active-high reset with .active edge should produce 'posedge rst' in sensitivity list"
    );
}

#[test]
fn test_async_reset_active_low() {
    let sv = compile_hir_to_sv(
        r#"
entity AsyncResetLowTest {
    in clk: clock
    in rst_n: reset(active_low)
    in data: nat[8]
    out result: nat[8]
}

impl AsyncResetLowTest {
    signal state: nat[8]

    on(clk.rise | rst_n.active) {
        if rst_n {
            state = 0
        } else {
            state = data
        }
    }

    result = state
}
"#,
    );

    println!("SV:\n{sv}");
    assert!(
        sv.contains("posedge clk or negedge rst_n"),
        "Active-low reset with .active edge should produce 'negedge rst_n' in sensitivity list"
    );
}

#[test]
fn test_sync_reset_unchanged() {
    let sv = compile_hir_to_sv(
        r#"
entity SyncResetTest {
    in clk: clock
    in rst: reset(active_high)
    in data: nat[8]
    out result: nat[8]
}

impl SyncResetTest {
    signal state: nat[8]

    on(clk.rise) {
        if rst {
            state = 0
        } else {
            state = data
        }
    }

    result = state
}
"#,
    );

    println!("SV:\n{sv}");
    assert!(
        sv.contains("posedge clk)"),
        "Sync reset should only have posedge clk in sensitivity list"
    );
    assert!(
        !sv.contains("posedge clk or"),
        "Sync reset should NOT have rst in sensitivity list"
    );
}

// ============================================================================
// Tier 2: Formal Assertions (Phase 3)
// ============================================================================

#[test]
fn test_assert_sva_generation() {
    let sv = compile_hir_to_sv(
        r#"
entity AssertTest {
    in clk: clock
    in data: nat[8]
    out valid: bool
}

impl AssertTest {
    signal counter: nat[8] = 0

    on(clk.rise) {
        counter = counter + 1
        assert!(counter < 255, "Counter must not overflow")
    }

    valid = counter > 0
}
"#,
    );

    println!("SV:\n{sv}");
    assert!(
        sv.contains("assert(") || sv.contains("assert ("),
        "Should contain assert statement"
    );
}

#[test]
fn test_assume_sva_generation() {
    let sv = compile_hir_to_sv(
        r#"
entity AssumeTest {
    in clk: clock
    in valid: bool
    in data: nat[8]
    out result: nat[8]
}

impl AssumeTest {
    signal stored: nat[8] = 0

    on(clk.rise) {
        assume!(valid, "Valid signal assumed true")
        if valid {
            stored = data
        }
    }

    result = stored
}
"#,
    );

    println!("SV:\n{sv}");
    assert!(
        sv.contains("assume(") || sv.contains("assume ("),
        "Should contain assume statement"
    );
}

#[test]
fn test_cover_sva_generation() {
    // cover!() must be in an event block with proper syntax
    let sv = compile_hir_to_sv(
        r#"
        entity CoverTest {
            in clk: clock
            in state: nat[2]
            out active: bool
        }

        impl CoverTest {
            on(clk.rise) {
                cover!(state == 3, "all_bits_set");
            }

            assign active = state != 0;
        }
"#,
    );

    println!("SV:\n{sv}");
    assert!(sv.contains("cover"), "Should contain cover statement");
}

#[test]
fn test_mixed_sva_generation() {
    let sv = compile_hir_to_sv(
        r#"
entity MixedSvaTest {
    in clk: clock
    in reset: bool
    in data: nat[8]
    out result: nat[8]
}

impl MixedSvaTest {
    signal counter: nat[8] = 0

    on(clk.rise) {
        assume!(data < 100, "Data assumed in range")

        if reset {
            counter = 0
        } else {
            counter = counter + data
        }

        assert!(counter < 255, "Counter bounds check")
    }

    result = counter
}
"#,
    );

    println!("SV:\n{sv}");
    assert!(
        sv.contains("assert(") || sv.contains("assert ("),
        "Should have assert statements"
    );
    assert!(
        sv.contains("assume(") || sv.contains("assume ("),
        "Should have assume statements"
    );
}

// ============================================================================
// Tier 2: Breakpoint Tests (Phase 4)
// ============================================================================

#[test]
fn test_breakpoint_codegen() {
    let sv = compile_hir_to_sv(
        r#"
entity CodegenTest {
    in clk: bit
    in data: bit[8]
    out valid: bit

    #[breakpoint(name = "DATA_CHECK", condition = "data > 128")]
    signal data_high: bit
}

impl CodegenTest {
    valid = 1
    data_high = data > 128
}
"#,
    );

    println!("SV:\n{sv}");
    assert!(
        sv.contains("// Debug Breakpoint Assertions") || sv.contains("// Breakpoint:"),
        "Should have breakpoint comment"
    );
    assert!(
        sv.contains("BREAKPOINT") || sv.contains("$stop"),
        "Should have breakpoint action"
    );
    assert!(
        sv.contains("DATA_CHECK") || sv.contains("data_high"),
        "Should reference breakpoint name or signal"
    );
}

#[test]
fn test_breakpoint_simple_codegen() {
    let sv = compile_hir_to_sv(
        r#"
entity SimpleBreakpoint {
    in clk: bit
    in error: bit
    out ok: bit

    #[breakpoint]
    signal error_signal: bit
}

impl SimpleBreakpoint {
    ok = !error
    error_signal = error
}
"#,
    );

    println!("SV:\n{sv}");
    assert!(
        sv.contains("error_signal") && sv.contains("$stop"),
        "Should have error_signal breakpoint with $stop"
    );
}

#[test]
fn test_breakpoint_skalp_roundtrip() {
    let sk = compile_hir_to_skalp(
        r#"
entity BpTest {
    in clk: bit
    out ok: bit

    #[breakpoint(name = "test_bp", condition = "count > 10")]
    signal count: bit[8]
}

impl BpTest {
    ok = 1
    count = 0
}
"#,
    );

    println!("Skalp:\n{sk}");
    assert!(
        sk.contains("#[breakpoint("),
        "Should emit #[breakpoint] attribute in skalp output"
    );
    assert!(sk.contains("test_bp"), "Should preserve breakpoint name");
}

// ============================================================================
// Tier 2: CDC Tests (Phase 6)
// ============================================================================

#[test]
fn test_cdc_codegen_two_ff() {
    let sv = compile_hir_to_sv(
        r#"
entity CdcCodegenTest {
    in clk: bit
    in async_input: bit
    out sync_output: bit

    #[cdc(sync_stages = 2)]
    signal synced: bit
}

impl CdcCodegenTest {
    synced = async_input
    sync_output = synced
}
"#,
    );

    println!("SV:\n{sv}");
    assert!(
        sv.contains("CDC") || sv.contains("Synchronizer"),
        "Should have CDC comment"
    );
    assert!(sv.contains("ASYNC_REG"), "Should have ASYNC_REG attribute");
    assert!(sv.contains("synced_sync_0"), "Should have sync stage 0");
    assert!(sv.contains("synced_sync_1"), "Should have sync stage 1");
}

#[test]
fn test_cdc_codegen_gray() {
    let sv = compile_hir_to_sv(
        r#"
entity CdcGrayTest {
    in clk: bit
    in async_counter: bit[4]
    out sync_counter: bit[4]

    #[cdc(cdc_type = gray, sync_stages = 2)]
    signal gray_synced: bit[4]
}

impl CdcGrayTest {
    gray_synced = async_counter
    sync_counter = gray_synced
}
"#,
    );

    println!("SV:\n{sv}");
    assert!(
        sv.contains("CDC") || sv.contains("Synchronizer"),
        "Should have CDC comment"
    );
    assert!(
        sv.contains("gray_synced_gray"),
        "Should have gray-coded signal"
    );
    assert!(
        sv.contains("gray_synced_bin_in"),
        "Should have binary input"
    );
}

#[test]
fn test_cdc_skalp_roundtrip() {
    let sk = compile_hir_to_skalp(
        r#"
entity CdcRoundtrip {
    in clk: bit
    out sync_out: bit

    #[cdc(cdc_type = gray, sync_stages = 3, from = 'src, to = 'dst)]
    signal crossing: bit[8]
}

impl CdcRoundtrip {
    crossing = 0
    sync_out = 0
}
"#,
    );

    println!("Skalp:\n{sk}");
    assert!(sk.contains("#[cdc("), "Should emit #[cdc] attribute");
    assert!(sk.contains("gray"), "Should preserve gray type");
    assert!(sk.contains("stages=3"), "Should preserve stage count");
}

// ============================================================================
// Tier 3: Power Intent Tests (Phase 5)
// ============================================================================

#[test]
fn test_retention_codegen() {
    let sv = compile_hir_to_sv(
        r#"
entity RetentionCodegen {
    in clk: bit
    in data: bit[8]
    out valid: bit

    #[retention]
    signal saved_data: bit[8]
}

impl RetentionCodegen {
    saved_data = data
    valid = 1
}
"#,
    );

    println!("SV:\n{sv}");
    assert!(
        sv.contains("RETAIN") || sv.contains("Power Intent"),
        "Should have retention attributes"
    );
    assert!(
        sv.contains("DONT_TOUCH") || sv.contains("preserve"),
        "Should have preservation attributes"
    );
}

#[test]
fn test_isolation_codegen() {
    let sv = compile_hir_to_sv(
        r#"
entity IsolationCodegen {
    in clk: bit
    in iso_en: bit
    in data: bit[8]
    out valid: bit

    #[isolation(clamp = low, enable = "iso_en")]
    signal isolated_data: bit[8]
}

impl IsolationCodegen {
    isolated_data = data
    valid = 1
}
"#,
    );

    println!("SV:\n{sv}");
    assert!(
        sv.contains("Isolation") || sv.contains("isolated"),
        "Should have isolation comments or logic"
    );
}

#[test]
fn test_level_shift_codegen() {
    let sv = compile_hir_to_sv(
        r#"
entity LevelShiftCodegen {
    in clk: bit
    in data: bit[8]
    out valid: bit

    #[level_shift(from = "core", to = "io")]
    signal shifted_data: bit[8]
}

impl LevelShiftCodegen {
    shifted_data = data
    valid = 1
}
"#,
    );

    println!("SV:\n{sv}");
    assert!(
        sv.contains("LEVEL_SHIFTER") || sv.contains("Level shift"),
        "Should have level shifter attributes"
    );
}

#[test]
fn test_power_skalp_roundtrip() {
    let sk = compile_hir_to_skalp(
        r#"
entity PowerTest {
    in clk: bit
    out ok: bit

    #[retention(strategy = "shadow_register")]
    signal retained: bit[8]
}

impl PowerTest {
    retained = 0
    ok = 1
}
"#,
    );

    println!("Skalp:\n{sk}");
    assert!(sk.contains("#[retain"), "Should emit #[retain] attribute");
}

// ============================================================================
// Tier 3: Vendor IP Tests (Phase 7)
// ============================================================================

#[test]
fn test_vendor_ip_codegen() {
    let sv = compile_hir_to_sv(
        r#"
#[xilinx_ip("xpm_fifo_sync")]
entity XpmFifoWrapper {
    in clk: clock
    in rst: bit
    in din: bit[32]
    in wr_en: bit
    out dout: bit[32]
    in rd_en: bit
    out full: bit
    out empty: bit
}

impl XpmFifoWrapper {
}
"#,
    );

    println!("SV:\n{sv}");
    assert!(
        sv.contains("Vendor IP Wrapper") || sv.contains("xpm_fifo_sync"),
        "Should have vendor IP wrapper or IP name"
    );
    assert!(
        sv.contains("module XpmFifoWrapper"),
        "Should have module declaration"
    );
}

#[test]
fn test_vendor_ip_skalp_roundtrip() {
    let sk = compile_hir_to_skalp(
        r#"
#[xilinx_ip("xpm_fifo_sync", library="xpm")]
entity XpmFifoWrapper {
    in clk: clock
    in din: bit[32]
    out dout: bit[32]
}

impl XpmFifoWrapper {
}
"#,
    );

    println!("Skalp:\n{sk}");
    assert!(
        sk.contains("#[xilinx_ip("),
        "Should emit xilinx_ip attribute"
    );
    assert!(sk.contains("xpm_fifo_sync"), "Should preserve IP name");
    assert!(sk.contains("library="), "Should preserve library parameter");
}

// ============================================================================
// VHDL Output Tests
// ============================================================================

#[test]
fn test_alu_vhdl_codegen() {
    let vhdl = compile_hir_to_vhdl(
        r#"
entity ALU {
    in a: bit[32]
    in b: bit[32]
    in op: bit[3]
    in clk: clock
    out result: bit[32]
    out zero: bit
}

impl ALU {
    signal result_comb: bit[32]

    result_comb = match op {
        0b000 => a + b,
        0b001 => a - b,
        0b010 => a & b,
        0b011 => a | b,
        _ => 0
    };

    zero = if result_comb == 0 { 1 } else { 0 };

    on(clk.rise) {
        result = result_comb
    }
}
"#,
    );

    assert!(vhdl.contains("entity ALU is"), "Should have VHDL entity");
    assert!(
        vhdl.contains("architecture rtl"),
        "Should have VHDL architecture"
    );
    assert!(vhdl.contains("process("), "Should have VHDL process");
    assert!(vhdl.contains("when"), "VHDL should use when...else");
}

#[test]
fn test_counter_vhdl_codegen() {
    let vhdl = compile_hir_to_vhdl(
        r#"
entity Counter {
    in clk: clock
    in reset: bit
    in enable: bit
    out count: bit[8]
}

impl Counter {
    signal counter_reg: bit[8]

    on(clk.rise) {
        if reset {
            counter_reg = 0
        } else if enable {
            counter_reg = counter_reg + 1
        }
    }

    count = counter_reg
}
"#,
    );

    assert!(vhdl.contains("entity Counter is"));
    assert!(
        vhdl.contains("rising_edge"),
        "Should have rising_edge in VHDL"
    );
}

// ============================================================================
// Skalp Round-trip Tests
// ============================================================================

#[test]
fn test_alu_skalp_roundtrip() {
    let sk = compile_hir_to_skalp(
        r#"
entity ALU {
    in a: bit[32]
    in b: bit[32]
    in op: bit[3]
    in clk: clock
    out result: bit[32]
    out zero: bit
}

impl ALU {
    signal result_comb: bit[32]

    result_comb = match op {
        0b000 => a + b,
        0b001 => a - b,
        0b010 => a & b,
        0b011 => a | b,
        _ => 0
    };

    zero = if result_comb == 0 { 1 } else { 0 };

    on(clk.rise) {
        result = result_comb
    }
}
"#,
    );

    assert!(sk.contains("entity ALU"), "Should have skalp entity");
    assert!(sk.contains("on(clk.rise)"), "Should have on block");
    assert!(
        sk.contains("0b000"),
        "Binary literals should preserve bit order"
    );
}

// ============================================================================
// Multi-format Consistency Tests
// ============================================================================

#[test]
fn test_all_three_formats() {
    let source = r#"
entity Adder {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[8]
}

impl Adder {
    sum = a + b
}
"#;

    let sv = compile_hir_to_sv(source);
    let vhdl = compile_hir_to_vhdl(source);
    let sk = compile_hir_to_skalp(source);

    assert!(sv.contains("module Adder"), "SV should have module");
    assert!(vhdl.contains("entity Adder is"), "VHDL should have entity");
    assert!(sk.contains("entity Adder"), "Skalp should have entity");

    // All should reference the addition
    assert!(sv.contains("+"), "SV should have addition");
    assert!(vhdl.contains("+"), "VHDL should have addition");
    assert!(sk.contains("+"), "Skalp should have addition");
}
