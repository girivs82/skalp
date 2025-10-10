//! Tests for Standard Library Example Designs
//!
//! These tests verify the example designs compile correctly.

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
fn test_ray_sphere_hit_test() {
    // Simplified version of ray-sphere intersection
    let source = r#"
entity RaySphereHitTest {
    in ray_origin: vec3<fp32>
    in ray_direction: vec3<fp32>
    in sphere_center: vec3<fp32>
    in sphere_radius: fp32
    out hit: bit
}

impl RaySphereHitTest {
    // Vector from ray origin to sphere center
    signal oc_x: fp32 = ray_origin.x - sphere_center.x
    signal oc_y: fp32 = ray_origin.y - sphere_center.y
    signal oc_z: fp32 = ray_origin.z - sphere_center.z

    // Project onto ray direction (dot product)
    signal t_closest: fp32 = (oc_x * ray_direction.x) +
                             (oc_y * ray_direction.y) +
                             (oc_z * ray_direction.z)

    // Closest point on ray = origin + t * direction
    signal closest_x: fp32 = ray_origin.x + (ray_direction.x * t_closest)
    signal closest_y: fp32 = ray_origin.y + (ray_direction.y * t_closest)
    signal closest_z: fp32 = ray_origin.z + (ray_direction.z * t_closest)

    // Distance from sphere center to closest point
    signal dist_x: fp32 = sphere_center.x - closest_x
    signal dist_y: fp32 = sphere_center.y - closest_y
    signal dist_z: fp32 = sphere_center.z - closest_z

    signal distance_sq: fp32 = (dist_x * dist_x) +
                               (dist_y * dist_y) +
                               (dist_z * dist_z)

    signal radius_sq: fp32 = sphere_radius * sphere_radius

    // Hit if distance <= radius (distance_sq <= radius_sq)
    // Simplified comparison for now
    hit = 1
}
"#;

    let golden = GoldenTest::new("ray_sphere_hit_test");
    let verilog = compile_to_verilog(source);

    // Should have vec3<fp32> inputs (96-bit)
    assert!(verilog.contains("[95:0]"), "Should have vec3<fp32> inputs");

    // Should have FP32 operations
    assert!(verilog.contains("[31:0]"), "Should have fp32 signals");

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_vector_distance_calculation() {
    let source = r#"
entity Vec3Distance {
    in a: vec3<fp32>
    in b: vec3<fp32>
    out distance_sq: fp32
}

impl Vec3Distance {
    signal diff_x: fp32 = a.x - b.x
    signal diff_y: fp32 = a.y - b.y
    signal diff_z: fp32 = a.z - b.z

    distance_sq = (diff_x * diff_x) + (diff_y * diff_y) + (diff_z * diff_z)
}
"#;

    let golden = GoldenTest::new("vec3_distance");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_vector_lerp() {
    let source = r#"
entity Vec3Lerp {
    in a: vec3<fp32>
    in b: vec3<fp32>
    in t: fp32
    out result: vec3<fp32>
}

impl Vec3Lerp {
    // Linear interpolation: result = a + (b - a) * t
    signal diff_x: fp32 = b.x - a.x
    signal diff_y: fp32 = b.y - a.y
    signal diff_z: fp32 = b.z - a.z

    signal scaled_x: fp32 = diff_x * t
    signal scaled_y: fp32 = diff_y * t
    signal scaled_z: fp32 = diff_z * t

    result = vec3::<fp32> {
        x: a.x + scaled_x,
        y: a.y + scaled_y,
        z: a.z + scaled_z
    }
}
"#;

    let golden = GoldenTest::new("vec3_lerp");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_color_mixer() {
    // Example: RGB color mixing using vector operations
    let source = r#"
entity ColorMixer {
    in color1: vec3<fp32>
    in color2: vec3<fp32>
    in blend: fp32
    out result: vec3<fp32>
}

impl ColorMixer {
    // Linear interpolation between two colors
    signal diff_r: fp32 = color2.x - color1.x
    signal diff_g: fp32 = color2.y - color1.y
    signal diff_b: fp32 = color2.z - color1.z

    result = vec3::<fp32> {
        x: color1.x + (diff_r * blend),
        y: color1.y + (diff_g * blend),
        z: color1.z + (diff_b * blend)
    }
}
"#;

    let golden = GoldenTest::new("color_mixer");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_dot_product_classification() {
    let source = r#"
entity DotProductClassify {
    in a: vec3<fp32>
    in b: vec3<fp32>
    out dot: fp32
    out parallel: bit
    out perpendicular: bit
    out opposite: bit
}

impl DotProductClassify {
    // Compute dot product
    dot = (a.x * b.x) + (a.y * b.y) + (a.z * b.z)

    // Classify based on dot product value
    // (Simplified - real version would compare to thresholds)
    signal zero: fp32 = 32'h00000000
    signal pos_threshold: fp32 = 32'h3F000000  // 0.5

    parallel = 0       // dot close to 1
    perpendicular = 0  // dot close to 0
    opposite = 0       // dot close to -1
}
"#;

    let golden = GoldenTest::new("dot_product_classify");
    let verilog = compile_to_verilog(source);

    // Should compile successfully
    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_bounding_box_check() {
    let source = r#"
entity BoundingBoxCheck {
    in point: vec3<fp32>
    in min_corner: vec3<fp32>
    in max_corner: vec3<fp32>
    out inside: bit
}

impl BoundingBoxCheck {
    signal x_in: bit = (point.x >= min_corner.x) && (point.x <= max_corner.x)
    signal y_in: bit = (point.y >= min_corner.y) && (point.y <= max_corner.y)
    signal z_in: bit = (point.z >= min_corner.z) && (point.z <= max_corner.z)

    inside = x_in && y_in && z_in
}
"#;

    let golden = GoldenTest::new("bounding_box_check");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_vector_reflection() {
    let source = r#"
entity Vec3Reflect {
    in incident: vec3<fp32>
    in normal: vec3<fp32>
    out reflected: vec3<fp32>
}

impl Vec3Reflect {
    // Reflection formula: r = i - 2 * (i · n) * n
    signal dot_in: fp32 = (incident.x * normal.x) +
                         (incident.y * normal.y) +
                         (incident.z * normal.z)

    signal two: fp32 = 32'h40000000  // 2.0
    signal scale: fp32 = two * dot_in

    reflected = vec3::<fp32> {
        x: incident.x - (scale * normal.x),
        y: incident.y - (scale * normal.y),
        z: incident.z - (scale * normal.z)
    }
}
"#;

    let golden = GoldenTest::new("vec3_reflect");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_2d_rotation() {
    // 2D vector rotation (simplified - no trig functions)
    let source = r#"
entity Vec2Rotate90 {
    in v: vec2<fp32>
    out rotated: vec2<fp32>
}

impl Vec2Rotate90 {
    // Rotate 90 degrees counter-clockwise: (x, y) -> (-y, x)
    signal zero: fp32 = 32'h00000000
    signal neg_y: fp32 = zero - v.y

    rotated = vec2::<fp32> {
        x: neg_y,
        y: v.x
    }
}
"#;

    let golden = GoldenTest::new("vec2_rotate90");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_vec4_homogeneous_divide() {
    let source = r#"
entity Vec4PerspectiveDivide {
    in v: vec4<fp32>
    out result: vec3<fp32>
}

impl Vec4PerspectiveDivide {
    // Perspective divide: (x/w, y/w, z/w)
    // Simplified - real version would use FP division
    result = vec3::<fp32> {
        x: v.x,
        y: v.y,
        z: v.z
    }
}
"#;

    let verilog = compile_to_verilog(source);

    // Should have vec4 input and vec3 output
    assert!(verilog.contains("[127:0]"), "Should have vec4 input");
    assert!(verilog.contains("[95:0]"), "Should have vec3 output");
}

#[test]
fn test_multi_vector_operations() {
    // Test using multiple vector operations in one entity
    let source = r#"
entity MultiVectorOps {
    in a: vec3<fp32>
    in b: vec3<fp32>
    in c: vec3<fp32>
    out dot_ab: fp32
    out dot_bc: fp32
    out sum: vec3<fp32>
}

impl MultiVectorOps {
    // Multiple dot products
    dot_ab = (a.x * b.x) + (a.y * b.y) + (a.z * b.z)
    dot_bc = (b.x * c.x) + (b.y * c.y) + (b.z * c.z)

    // Vector addition
    sum = vec3::<fp32> {
        x: a.x + b.x + c.x,
        y: a.y + b.y + c.y,
        z: a.z + b.z + c.z
    }
}
"#;

    let golden = GoldenTest::new("multi_vector_ops");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_vector_normalization_components() {
    // Test the components needed for normalization (without sqrt)
    let source = r#"
entity Vec3NormalizePrep {
    in v: vec3<fp32>
    out length_sq: fp32
    out inv_length: fp32
}

impl Vec3NormalizePrep {
    // Compute length squared
    length_sq = (v.x * v.x) + (v.y * v.y) + (v.z * v.z)

    // Inverse length would require FP division/sqrt
    // Placeholder for now
    inv_length = 32'h3F800000  // 1.0
}
"#;

    let golden = GoldenTest::new("vec3_normalize_prep");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}

#[test]
fn test_vector_clamp() {
    let source = r#"
entity Vec3Clamp {
    in v: vec3<fp32>
    in min_val: vec3<fp32>
    in max_val: vec3<fp32>
    out result: vec3<fp32>
}

impl Vec3Clamp {
    // Clamp each component to [min, max]
    signal x_clamped: fp32 = if v.x < min_val.x {
        min_val.x
    } else if v.x > max_val.x {
        max_val.x
    } else {
        v.x
    }

    signal y_clamped: fp32 = if v.y < min_val.y {
        min_val.y
    } else if v.y > max_val.y {
        max_val.y
    } else {
        v.y
    }

    signal z_clamped: fp32 = if v.z < min_val.z {
        min_val.z
    } else if v.z > max_val.z {
        max_val.z
    } else {
        v.z
    }

    result = vec3::<fp32> {
        x: x_clamped,
        y: y_clamped,
        z: z_clamped
    }
}
"#;

    let golden = GoldenTest::new("vec3_clamp");
    let verilog = compile_to_verilog(source);

    golden.assert_eq("sv", &verilog);
}
