//! Golden File Tests - Example Usage
//!
//! This file demonstrates how to use the golden file testing framework
//! for regression testing of code generation outputs.
//!
//! To update golden files when output changes intentionally:
//! ```bash
//! SKALP_UPDATE_GOLDEN=1 cargo test
//! ```

use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::parse::parse;
use skalp_testing::golden::GoldenTest;

/// Helper to compile a SKALP file to SystemVerilog
fn compile_to_verilog(source: &str) -> String {
    // Parse the source
    let tree = parse(source);

    // Build HIR
    let hir = build_hir(&tree).expect("HIR building should succeed");

    // Lower to MIR
    let mir = skalp_mir::lower_to_mir(&hir).expect("MIR lowering should succeed");

    // Lower to LIR
    let lir = skalp_lir::lower_to_lir(&mir).expect("LIR lowering should succeed");

    // Generate SystemVerilog using correct generator
    skalp_codegen::generate_systemverilog_from_mir(&mir, &lir)
        .expect("Verilog generation should succeed")
}

#[test]
fn test_alu_codegen_golden() {
    let source = include_str!("../examples/alu.sk");

    let golden = GoldenTest::new("alu");
    let verilog = compile_to_verilog(source);

    // This will compare against tests/golden/alu.sv
    // Run with SKALP_UPDATE_GOLDEN=1 to create/update the golden file
    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_counter_codegen_golden() {
    let source = include_str!("../examples/counter.sk");

    let golden = GoldenTest::new("counter");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_comparison_in_if_golden() {
    // Regression test for comparison bug where `if (x == 0)` was compiled as `if (x)`
    let source = r#"
entity TestComparison {
    in clk: clock
    in rst: reset
    in x: nat[2]
    out y: bit
}

impl TestComparison {
    signal result: bit = 0

    on(clk.rise) {
        if (rst) {
            result <= 0
        } else {
            if (x == 0) {
                result <= 1
            } else {
                result <= 0
            }
        }
    }

    y = result
}
"#;

    let golden = GoldenTest::new("comparison_in_if");
    let verilog = compile_to_verilog(source);

    // Should contain "== 0" comparison, not just a bare signal reference
    // The bug was that `if (x == 0)` compiled to `if (x)` instead of `if (x == 0)`
    assert!(
        verilog.contains("== 0"),
        "Generated Verilog should contain comparison '== 0', got:\n{}",
        verilog
    );

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

    let golden = GoldenTest::new("simple_adder");
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

    let golden = GoldenTest::new("mux_2to1");
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

    let golden = GoldenTest::new("mux_4to1");
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

    let golden = GoldenTest::new("bit_operations");
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

    let golden = GoldenTest::new("shift_operations");
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

    let golden = GoldenTest::new("comparisons");
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

    let golden = GoldenTest::new("bit_indexing");
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

    let golden = GoldenTest::new("nested_if");
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

    let golden = GoldenTest::new("complex_expression");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_fp32_types_golden() {
    let source = r#"
entity FP32Test {
    in a: fp32
    in b: fp32
    out result: fp32
}

impl FP32Test {
    result = a
}
"#;

    let golden = GoldenTest::new("fp32_types");
    let verilog = compile_to_verilog(source);

    // Verify fp32 is 32-bit
    assert!(
        verilog.contains("[31:0]"),
        "FP32 should be 32-bit: {}",
        verilog
    );

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_fp16_fp64_types_golden() {
    let source = r#"
entity FPVariants {
    in a16: fp16
    in a32: fp32
    in a64: fp64
    out o16: fp16
    out o32: fp32
    out o64: fp64
}

impl FPVariants {
    o16 = a16
    o32 = a32
    o64 = a64
}
"#;

    let golden = GoldenTest::new("fp_variants");
    let verilog = compile_to_verilog(source);

    // Verify widths
    assert!(verilog.contains("[15:0] a16"), "FP16 should be 16-bit");
    assert!(verilog.contains("[31:0] a32"), "FP32 should be 32-bit");
    assert!(verilog.contains("[63:0] a64"), "FP64 should be 64-bit");

    golden.assert_eq("sv", &verilog);
}

// DEPRECATED: vec2/vec3/vec4 are no longer built-in types with field access.
// They are now stdlib type aliases to arrays. Use array indexing instead.
#[test]
#[ignore]
fn test_vec2_types_golden() {
    let source = r#"
entity Vec2Test {
    in v: vec2<fp32>
    out x: fp32
    out y: fp32
}

impl Vec2Test {
    x = v.x
    y = v.y
}
"#;

    let golden = GoldenTest::new("vec2_types");
    let verilog = compile_to_verilog(source);

    // Verify vec2<fp32> is flattened into separate 32-bit ports
    assert!(
        verilog.contains("v_x") && verilog.contains("v_y"),
        "vec2<fp32> should be flattened into v_x and v_y:\n{}",
        verilog
    );
    // Verify both components are 32-bit
    assert!(
        verilog.contains("[31:0] v_x") && verilog.contains("[31:0] v_y"),
        "Both components should be [31:0]:\n{}",
        verilog
    );

    golden.assert_eq("sv", &verilog);
}

// DEPRECATED: vec2/vec3/vec4 are no longer built-in types with field access.
#[test]
#[ignore]
fn test_vec3_types_golden() {
    let source = r#"
entity Vec3Test {
    in v: vec3<bit<16>>
    out x: bit<16>
    out y: bit<16>
    out z: bit<16>
}

impl Vec3Test {
    x = v.x
    y = v.y
    z = v.z
}
"#;

    let golden = GoldenTest::new("vec3_types");
    let verilog = compile_to_verilog(source);

    // Verify vec3<bit<16>> is flattened into separate 16-bit ports
    assert!(
        verilog.contains("v_x") && verilog.contains("v_y") && verilog.contains("v_z"),
        "vec3<bit<16>> should be flattened into v_x, v_y, v_z:\n{}",
        verilog
    );
    // Verify all components are 16-bit
    assert!(
        verilog.contains("[15:0] v_x")
            && verilog.contains("[15:0] v_y")
            && verilog.contains("[15:0] v_z"),
        "All components should be [15:0]:\n{}",
        verilog
    );

    golden.assert_eq("sv", &verilog);
}

// DEPRECATED: vec2/vec3/vec4 are no longer built-in types with field access.
#[test]
#[ignore]
fn test_vec4_types_golden() {
    let source = r#"
entity Vec4Test {
    in v: vec4<bit<8>>
    out x: bit<8>
    out y: bit<8>
    out z: bit<8>
    out w: bit<8>
}

impl Vec4Test {
    x = v.x
    y = v.y
    z = v.z
    w = v.w
}
"#;

    let golden = GoldenTest::new("vec4_types");
    let verilog = compile_to_verilog(source);

    // Verify vec4<bit<8>> is flattened into separate 8-bit ports
    assert!(
        verilog.contains("v_x")
            && verilog.contains("v_y")
            && verilog.contains("v_z")
            && verilog.contains("v_w"),
        "vec4<bit<8>> should be flattened into v_x, v_y, v_z, v_w:\n{}",
        verilog
    );
    // Verify all components are 8-bit
    assert!(
        verilog.contains("[7:0] v_x")
            && verilog.contains("[7:0] v_y")
            && verilog.contains("[7:0] v_z")
            && verilog.contains("[7:0] v_w"),
        "All components should be [7:0]:\n{}",
        verilog
    );

    golden.assert_eq("sv", &verilog);
}

// DEPRECATED: vec2/vec3 are no longer built-in types with field access.
#[test]
#[ignore]
fn test_vec_fp_mixed_golden() {
    let source = r#"
entity VecFPMixed {
    in v2: vec2<fp32>
    in v3: vec3<fp16>
    out sum2: fp32
    out sum3: fp16
}

impl VecFPMixed {
    sum2 = v2.x + v2.y
    sum3 = v3.x + v3.y + v3.z
}
"#;

    let golden = GoldenTest::new("vec_fp_mixed");
    let verilog = compile_to_verilog(source);

    // Verify mixed vector and FP types are flattened
    assert!(
        verilog.contains("v2_x") && verilog.contains("v2_y"),
        "vec2<fp32> should be flattened into v2_x, v2_y:\n{}",
        verilog
    );
    assert!(
        verilog.contains("v3_x") && verilog.contains("v3_y") && verilog.contains("v3_z"),
        "vec3<fp16> should be flattened into v3_x, v3_y, v3_z:\n{}",
        verilog
    );

    golden.assert_eq("sv", &verilog);
}
