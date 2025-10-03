//! Golden File Tests - Example Usage
//!
//! This file demonstrates how to use the golden file testing framework
//! for regression testing of code generation outputs.
//!
//! To update golden files when output changes intentionally:
//! ```bash
//! SKALP_UPDATE_GOLDEN=1 cargo test
//! ```

use skalp_testing::golden::GoldenTest;
use skalp_frontend::parse::parse;
use skalp_frontend::hir_builder::build_hir;

/// Helper to compile a SKALP file to SystemVerilog
fn compile_to_verilog(source: &str) -> String {
    // Parse the source
    let tree = parse(source);

    // Build HIR
    let hir = build_hir(&tree).expect("HIR building should succeed");

    // Compile to Verilog using the e2e function
    skalp_mir::compile_hir_to_verilog(&hir).expect("Verilog generation should succeed")
}

#[test]
fn test_alu_codegen_golden() {
    let source = include_str!("../examples/alu.sk");

    let mut golden = GoldenTest::new("alu");
    let verilog = compile_to_verilog(source);

    // This will compare against tests/golden/alu.sv
    // Run with SKALP_UPDATE_GOLDEN=1 to create/update the golden file
    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_counter_codegen_golden() {
    let source = include_str!("../examples/counter.sk");

    let mut golden = GoldenTest::new("counter");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_simple_adder_golden() {
    let source = r#"
entity Adder {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[9]
}

impl Adder {
    sum = a + b
}
"#;

    let mut golden = GoldenTest::new("simple_adder");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_mux_2to1_golden() {
    let source = r#"
entity Mux2to1 {
    in a: bit[8]
    in b: bit[8]
    in sel: bit[1]
    out out: bit[8]
}

impl Mux2to1 {
    out = if sel { a } else { b }
}
"#;

    let mut golden = GoldenTest::new("mux_2to1");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_mux_4to1_golden() {
    let source = r#"
entity Mux4to1 {
    in a: bit[8]
    in b: bit[8]
    in c: bit[8]
    in d: bit[8]
    in sel: bit[2]
    out out: bit[8]
}

impl Mux4to1 {
    out = match sel {
        0 => a,
        1 => b,
        2 => c,
        3 => d,
        _ => 0
    }
}
"#;

    let mut golden = GoldenTest::new("mux_4to1");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_bit_operations_golden() {
    let source = r#"
entity BitOps {
    in a: bit[8]
    in b: bit[8]
    out and_result: bit[8]
    out or_result: bit[8]
    out xor_result: bit[8]
    out not_result: bit[8]
}

impl BitOps {
    and_result = a & b
    or_result = a | b
    xor_result = a ^ b
    not_result = ~a
}
"#;

    let mut golden = GoldenTest::new("bit_operations");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_shift_operations_golden() {
    let source = r#"
entity ShiftOps {
    in data: bit[16]
    in amount: bit[4]
    out shl_result: bit[16]
    out shr_result: bit[16]
}

impl ShiftOps {
    shl_result = data << amount
    shr_result = data >> amount
}
"#;

    let mut golden = GoldenTest::new("shift_operations");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_comparisons_golden() {
    let source = r#"
entity Comparisons {
    in a: bit[8]
    in b: bit[8]
    out eq: bit[1]
    out ne: bit[1]
    out lt: bit[1]
    out gt: bit[1]
}

impl Comparisons {
    eq = if a == b { 1 } else { 0 }
    ne = if a != b { 1 } else { 0 }
    lt = if a < b { 1 } else { 0 }
    gt = if a > b { 1 } else { 0 }
}
"#;

    let mut golden = GoldenTest::new("comparisons");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_bit_indexing_golden() {
    let source = r#"
entity BitIndexing {
    in data: bit[32]
    out bit0: bit[1]
    out bit31: bit[1]
    out nibble0: bit[4]
    out byte0: bit[8]
}

impl BitIndexing {
    bit0 = data[0]
    bit31 = data[31]
    nibble0 = data[3:0]
    byte0 = data[7:0]
}
"#;

    let mut golden = GoldenTest::new("bit_indexing");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_nested_if_golden() {
    let source = r#"
entity NestedIf {
    in a: bit[4]
    out result: bit[8]
}

impl NestedIf {
    result = if a > 10 {
        if a > 12 {
            255
        } else {
            128
        }
    } else {
        0
    }
}
"#;

    let mut golden = GoldenTest::new("nested_if");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_complex_expression_golden() {
    // This tests the complex nested expression bug that was fixed
    let source = r#"
entity ComplexExpr {
    in a: bit[32]
    in b: bit[32]
    in c: bit[32]
    out overflow: bit[1]
}

impl ComplexExpr {
    overflow = (~a[31] & ~b[31] & c[31]) | (a[31] & b[31] & ~c[31])
}
"#;

    let mut golden = GoldenTest::new("complex_expression");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}
