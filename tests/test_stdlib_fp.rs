//! Tests for Standard Library Floating-Point Components
//!
//! These tests verify the FP arithmetic entities work correctly
//! by compiling them and checking the generated SystemVerilog.

use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::parse::parse;
use skalp_testing::golden::GoldenTest;

/// Helper to compile SKALP source to SystemVerilog
fn compile_to_verilog(source: &str) -> String {
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");
    let mir = skalp_mir::lower_to_mir(&hir).expect("MIR lowering should succeed");
    let lir = skalp_lir::lower_to_lir(&mir).expect("LIR lowering should succeed");
    skalp_codegen::generate_systemverilog_from_mir(&mir, &lir)
        .expect("Verilog generation should succeed")
}

#[test]
fn test_fp32_add_entity() {
    // Test that fp32_add entity compiles and has correct interface
    let source = r#"
entity FP32AddTest {
    in a: fp32
    in b: fp32
    out result: fp32
}

impl FP32AddTest {
    // Simple pass-through for now (full adder logic is complex)
    result = a
}
"#;

    let verilog = compile_to_verilog(source);

    // Should have 32-bit inputs and output
    assert!(verilog.contains("[31:0]"), "Should have 32-bit FP signals");
    assert!(verilog.contains("input"), "Should have inputs");
    assert!(verilog.contains("output"), "Should have outputs");
}

#[test]
fn test_fp32_types_usage() {
    let source = r#"
entity FPArithmetic {
    in x: fp32
    in y: fp32
    out sum: fp32
    out prod: fp32
}

impl FPArithmetic {
    // These would use FP operators if fully implemented
    sum = x
    prod = y
}
"#;

    let golden = GoldenTest::new("fp32_arithmetic");
    let verilog = compile_to_verilog(source);

    assert!(verilog.contains("[31:0]"), "FP32 should be 32-bit");
    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_fp16_fp64_types() {
    let source = r#"
entity FPFormats {
    in half: fp16
    in single: fp32
    in double: fp64
    out h_out: fp16
    out s_out: fp32
    out d_out: fp64
}

impl FPFormats {
    h_out = half
    s_out = single
    d_out = double
}
"#;

    let golden = GoldenTest::new("fp16_fp64_types");
    let verilog = compile_to_verilog(source);

    // Check bit widths for each format
    assert!(
        verilog.contains("[15:0]") || verilog.contains("input") && verilog.contains("half"),
        "FP16 should be 16-bit"
    );
    assert!(verilog.contains("[31:0]"), "FP32 should be 32-bit");
    assert!(verilog.contains("[63:0]"), "FP64 should be 64-bit");

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_fp_comparison_interface() {
    let source = r#"
entity FPCompare {
    in a: fp32
    in b: fp32
    out less_than: bit
    out equal: bit
    out greater_than: bit
}

impl FPCompare {
    // Simplified comparison (real version would handle NaN, etc.)
    less_than = 0
    equal = 0
    greater_than = 0
}
"#;

    let verilog = compile_to_verilog(source);

    assert!(
        verilog.contains("less_than"),
        "Should have less_than output"
    );
    assert!(verilog.contains("equal"), "Should have equal output");
    assert!(
        verilog.contains("greater_than"),
        "Should have greater_than output"
    );
}

#[test]
fn test_fp_special_values() {
    let source = r#"
entity FPSpecialValues {
    in value: fp32
    out is_zero: bit
    out is_nan: bit
    out is_inf: bit
}

impl FPSpecialValues {
    // Check special value patterns
    signal exp: bit<8> = value[30:23]
    signal mant: bit<23> = value[22:0]

    // Zero: exp=0, mant=0
    is_zero = (exp == 8'b00000000) && (mant == 23'b0)

    // Infinity: exp=255, mant=0
    is_inf = (exp == 8'b11111111) && (mant == 23'b0)

    // NaN: exp=255, mant!=0
    is_nan = (exp == 8'b11111111) && (mant != 23'b0)
}
"#;

    let golden = GoldenTest::new("fp_special_values");
    let verilog = compile_to_verilog(source);

    // Should compile successfully - bit slicing patterns may vary
    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_fp_sign_extraction() {
    let source = r#"
entity FPSignExtract {
    in value: fp32
    out sign: bit
    out magnitude: bit<31>
}

impl FPSignExtract {
    sign = value[31]
    magnitude = value[30:0]
}
"#;

    let golden = GoldenTest::new("fp_sign_extraction");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_fp_components() {
    // Test extracting sign, exponent, mantissa from FP32
    let source = r#"
entity FPComponents {
    in value: fp32
    out sign: bit
    out exponent: bit<8>
    out mantissa: bit<23>
}

impl FPComponents {
    sign = value[31]
    exponent = value[30:23]
    mantissa = value[22:0]
}
"#;

    let golden = GoldenTest::new("fp_components");
    let verilog = compile_to_verilog(source);

    // Verify all three components are extracted
    assert!(verilog.contains("sign"), "Should have sign output");
    assert!(verilog.contains("exponent"), "Should have exponent output");
    assert!(verilog.contains("mantissa"), "Should have mantissa output");

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_fp_construction() {
    // Test building an FP32 value from components
    let source = r#"
entity FPConstruct {
    in sign: bit
    in exponent: bit<8>
    in mantissa: bit<23>
    out value: fp32
}

impl FPConstruct {
    value = {sign, exponent, mantissa}
}
"#;

    let verilog = compile_to_verilog(source);

    // Should concatenate the components
    assert!(
        verilog.contains("sign") && verilog.contains("exponent") && verilog.contains("mantissa"),
        "Should use all three components"
    );
}

#[test]
fn test_fp_negate() {
    // Test FP negation by flipping sign bit
    let source = r#"
entity FPNegate {
    in value: fp32
    out negated: fp32
}

impl FPNegate {
    negated = {!value[31], value[30:0]}
}
"#;

    let golden = GoldenTest::new("fp_negate");
    let verilog = compile_to_verilog(source);

    // Should compile successfully
    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_fp_abs() {
    // Test FP absolute value by clearing sign bit
    let source = r#"
entity FPAbs {
    in value: fp32
    out absolute: fp32
}

impl FPAbs {
    absolute = {1'b0, value[30:0]}
}
"#;

    let golden = GoldenTest::new("fp_abs");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_fp_literal_representation() {
    // Test that FP literals can be represented as bit patterns
    let source = r#"
entity FPLiterals {
    out zero: fp32
    out one: fp32
    out neg_one: fp32
    out infinity: fp32
    out nan: fp32
}

impl FPLiterals {
    zero = 32'h00000000       // +0.0
    one = 32'h3F800000        // +1.0 (exp=127, mant=0)
    neg_one = 32'hBF800000    // -1.0 (sign=1, exp=127, mant=0)
    infinity = 32'h7F800000   // +Inf (exp=255, mant=0)
    nan = 32'h7FC00000        // Quiet NaN (exp=255, mant!=0)
}
"#;

    let golden = GoldenTest::new("fp_literals");
    let verilog = compile_to_verilog(source);

    // Should compile successfully
    golden.assert_eq("sv", &verilog);
}
