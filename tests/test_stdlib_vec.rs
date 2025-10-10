//! Tests for Standard Library Vector Components
//!
//! These tests verify the vector operation entities work correctly.

use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::parse::parse;
use skalp_testing::golden::GoldenTest;

fn compile_to_verilog(source: &str) -> String {
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");
    let mir = skalp_mir::lower_to_mir(&hir).expect("MIR lowering should succeed");
    let lir = skalp_lir::lower_to_lir(&mir).expect("LIR lowering should succeed");
    skalp_codegen::generate_systemverilog_from_mir(&mir, &lir)
        .expect("Verilog generation should succeed")
}

#[test]
fn test_vec2_basic_operations() {
    let source = r#"
entity Vec2Ops {
    in a: vec2<fp32>
    in b: vec2<fp32>
    out x_sum: fp32
    out y_sum: fp32
}

impl Vec2Ops {
    x_sum = a.x + b.x
    y_sum = a.y + b.y
}
"#;

    let golden = GoldenTest::new("vec2_basic_ops");
    let verilog = compile_to_verilog(source);

    // vec2<fp32> should be 64-bit (2 * 32)
    assert!(verilog.contains("[63:0]"), "vec2<fp32> should be 64-bit");

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_vec3_basic_operations() {
    let source = r#"
entity Vec3Ops {
    in v: vec3<fp32>
    out x: fp32
    out y: fp32
    out z: fp32
}

impl Vec3Ops {
    x = v.x
    y = v.y
    z = v.z
}
"#;

    let golden = GoldenTest::new("vec3_basic_ops");
    let verilog = compile_to_verilog(source);

    // vec3<fp32> should be 96-bit (3 * 32)
    assert!(verilog.contains("[95:0]"), "vec3<fp32> should be 96-bit");

    // Should have three component accesses
    assert!(verilog.contains("[31:0]"), "Should access x component");
    assert!(verilog.contains("[63:32]"), "Should access y component");
    assert!(verilog.contains("[95:64]"), "Should access z component");

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_vec4_basic_operations() {
    let source = r#"
entity Vec4Ops {
    in v: vec4<fp32>
    out x: fp32
    out y: fp32
    out z: fp32
    out w: fp32
}

impl Vec4Ops {
    x = v.x
    y = v.y
    z = v.z
    w = v.w
}
"#;

    let golden = GoldenTest::new("vec4_basic_ops");
    let verilog = compile_to_verilog(source);

    // vec4<fp32> should be 128-bit (4 * 32)
    assert!(verilog.contains("[127:0]"), "vec4<fp32> should be 128-bit");

    // Should have four component accesses
    assert!(verilog.contains("[31:0]"), "Should access x component");
    assert!(verilog.contains("[63:32]"), "Should access y component");
    assert!(verilog.contains("[95:64]"), "Should access z component");
    assert!(verilog.contains("[127:96]"), "Should access w component");

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_vec2_construction() {
    let source = r#"
entity Vec2Construct {
    in x: fp32
    in y: fp32
    out v: vec2<fp32>
}

impl Vec2Construct {
    v = vec2::<fp32> { x: x, y: y }
}
"#;

    let verilog = compile_to_verilog(source);

    // Should construct from components
    assert!(
        verilog.contains("x") && verilog.contains("y"),
        "Should use x and y components"
    );
}

#[test]
fn test_vec3_construction() {
    let source = r#"
entity Vec3Construct {
    in x: fp32
    in y: fp32
    in z: fp32
    out v: vec3<fp32>
}

impl Vec3Construct {
    v = vec3::<fp32> { x: x, y: y, z: z }
}
"#;

    let verilog = compile_to_verilog(source);

    assert!(
        verilog.contains("x") && verilog.contains("y") && verilog.contains("z"),
        "Should use x, y, z components"
    );
}

#[test]
fn test_vec2_dot_product() {
    let source = r#"
entity Vec2Dot {
    in a: vec2<fp32>
    in b: vec2<fp32>
    out dot: fp32
}

impl Vec2Dot {
    dot = (a.x * b.x) + (a.y * b.y)
}
"#;

    let golden = GoldenTest::new("vec2_dot");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_vec3_dot_product() {
    let source = r#"
entity Vec3Dot {
    in a: vec3<fp32>
    in b: vec3<fp32>
    out dot: fp32
}

impl Vec3Dot {
    dot = (a.x * b.x) + (a.y * b.y) + (a.z * b.z)
}
"#;

    let golden = GoldenTest::new("vec3_dot");
    let verilog = compile_to_verilog(source);

    // Should have three multiplications and two additions
    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_vec3_cross_product() {
    let source = r#"
entity Vec3Cross {
    in a: vec3<fp32>
    in b: vec3<fp32>
    out result: vec3<fp32>
}

impl Vec3Cross {
    result = vec3::<fp32> {
        x: (a.y * b.z) - (a.z * b.y),
        y: (a.z * b.x) - (a.x * b.z),
        z: (a.x * b.y) - (a.y * b.x)
    }
}
"#;

    let golden = GoldenTest::new("vec3_cross");
    let verilog = compile_to_verilog(source);

    // Cross product formula
    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_vec2_component_wise_add() {
    let source = r#"
entity Vec2Add {
    in a: vec2<fp32>
    in b: vec2<fp32>
    out result: vec2<fp32>
}

impl Vec2Add {
    result = vec2::<fp32> {
        x: a.x + b.x,
        y: a.y + b.y
    }
}
"#;

    let golden = GoldenTest::new("vec2_add");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_vec3_component_wise_operations() {
    let source = r#"
entity Vec3ComponentOps {
    in a: vec3<fp32>
    in b: vec3<fp32>
    out add_result: vec3<fp32>
    out sub_result: vec3<fp32>
}

impl Vec3ComponentOps {
    add_result = vec3::<fp32> {
        x: a.x + b.x,
        y: a.y + b.y,
        z: a.z + b.z
    }

    sub_result = vec3::<fp32> {
        x: a.x - b.x,
        y: a.y - b.y,
        z: a.z - b.z
    }
}
"#;

    let golden = GoldenTest::new("vec3_component_ops");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_vec3_scalar_multiply() {
    let source = r#"
entity Vec3Scale {
    in v: vec3<fp32>
    in s: fp32
    out result: vec3<fp32>
}

impl Vec3Scale {
    result = vec3::<fp32> {
        x: v.x * s,
        y: v.y * s,
        z: v.z * s
    }
}
"#;

    let golden = GoldenTest::new("vec3_scale");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_vec_integer_types() {
    // Test vectors with integer element types
    let source = r#"
entity Vec3Int {
    in v: vec3<bit<8>>
    out x: bit<8>
    out y: bit<8>
    out z: bit<8>
}

impl Vec3Int {
    x = v.x
    y = v.y
    z = v.z
}
"#;

    let verilog = compile_to_verilog(source);

    // vec3<bit<8>> should be 24-bit (3 * 8)
    assert!(verilog.contains("[23:0]"), "vec3<bit<8>> should be 24-bit");
}

#[test]
fn test_vec_different_element_sizes() {
    let source = r#"
entity VecSizes {
    in v8: vec2<bit<8>>
    in v16: vec2<bit<16>>
    in v32: vec2<fp32>
    out size8: bit<16>
    out size16: bit<32>
    out size32: bit<64>
}

impl VecSizes {
    size8 = v8
    size16 = v16
    size32 = v32
}
"#;

    let verilog = compile_to_verilog(source);

    // Different vec2 sizes
    assert!(verilog.contains("[15:0]"), "vec2<bit<8>> should be 16-bit");
    assert!(verilog.contains("[31:0]"), "vec2<bit<16>> should be 32-bit");
    assert!(verilog.contains("[63:0]"), "vec2<fp32> should be 64-bit");
}

#[test]
fn test_vec_component_assignment() {
    let source = r#"
entity Vec3ComponentAssign {
    in v: vec3<fp32>
    in new_x: fp32
    out result: vec3<fp32>
}

impl Vec3ComponentAssign {
    result = vec3::<fp32> {
        x: new_x,
        y: v.y,
        z: v.z
    }
}
"#;

    let verilog = compile_to_verilog(source);

    // Should update only x component
    assert!(verilog.contains("new_x"), "Should use new_x");
}

#[test]
fn test_vec2_length_squared() {
    let source = r#"
entity Vec2LengthSq {
    in v: vec2<fp32>
    out length_sq: fp32
}

impl Vec2LengthSq {
    length_sq = (v.x * v.x) + (v.y * v.y)
}
"#;

    let golden = GoldenTest::new("vec2_length_sq");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_vec3_length_squared() {
    let source = r#"
entity Vec3LengthSq {
    in v: vec3<fp32>
    out length_sq: fp32
}

impl Vec3LengthSq {
    length_sq = (v.x * v.x) + (v.y * v.y) + (v.z * v.z)
}
"#;

    let golden = GoldenTest::new("vec3_length_sq");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_vec_min_max() {
    let source = r#"
entity Vec3MinMax {
    in a: vec3<fp32>
    in b: vec3<fp32>
    out min_result: vec3<fp32>
    out max_result: vec3<fp32>
}

impl Vec3MinMax {
    min_result = vec3::<fp32> {
        x: if a.x < b.x { a.x } else { b.x },
        y: if a.y < b.y { a.y } else { b.y },
        z: if a.z < b.z { a.z } else { b.z }
    }

    max_result = vec3::<fp32> {
        x: if a.x > b.x { a.x } else { b.x },
        y: if a.y > b.y { a.y } else { b.y },
        z: if a.z > b.z { a.z } else { b.z }
    }
}
"#;

    let golden = GoldenTest::new("vec3_min_max");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_vec4_swizzle() {
    let source = r#"
entity Vec4Swizzle {
    in v: vec4<fp32>
    out xyz: vec3<fp32>
    out xy: vec2<fp32>
}

impl Vec4Swizzle {
    xyz = vec3::<fp32> { x: v.x, y: v.y, z: v.z }
    xy = vec2::<fp32> { x: v.x, y: v.y }
}
"#;

    let golden = GoldenTest::new("vec4_swizzle");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_vec_mixed_fp_types() {
    let source = r#"
entity VecMixedFP {
    in v16: vec2<fp16>
    in v32: vec2<fp32>
    in v64: vec2<fp64>
    out size16: bit<32>
    out size32: bit<64>
    out size64: bit<128>
}

impl VecMixedFP {
    size16 = v16
    size32 = v32
    size64 = v64
}
"#;

    let verilog = compile_to_verilog(source);

    // Different FP vector sizes
    assert!(verilog.contains("[31:0]"), "vec2<fp16> should be 32-bit");
    assert!(verilog.contains("[63:0]"), "vec2<fp32> should be 64-bit");
    assert!(verilog.contains("[127:0]"), "vec2<fp64> should be 128-bit");
}
