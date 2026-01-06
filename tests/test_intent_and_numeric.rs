//! Comprehensive test suite for intent helpers and numeric (CORDIC) functions
//!
//! This module tests:
//! 1. Intent helper compile-time evaluation (is_latency_optimized, is_area_optimized, is_throughput_optimized)
//! 2. CORDIC-based transcendental functions (sqrt)
//! 3. FP32 arithmetic operations

use skalp_testing::testbench::*;

// ============================================================================
// Intent Helper Tests
// ============================================================================

/// Test is_latency_optimized() returns true (1'b1) by default
#[tokio::test]
async fn test_intent_latency_optimized() {
    let mut tb = Testbench::new("tests/fixtures/intent/intent_latency.sk")
        .await
        .unwrap();

    tb.set("selector", 0u8);
    tb.step().await;

    // Default behavior: is_latency_optimized() returns true (1)
    tb.expect("is_lat_opt", 1u8).await;
}

/// Test is_area_optimized() returns false (1'b0) by default
#[tokio::test]
async fn test_intent_area_optimized() {
    let mut tb = Testbench::new("tests/fixtures/intent/intent_area.sk")
        .await
        .unwrap();

    tb.set("selector", 0u8);
    tb.step().await;

    // Default behavior: is_area_optimized() returns false (0)
    tb.expect("is_area_opt", 0u8).await;
}

/// Test is_throughput_optimized() returns false (1'b0) by default
#[tokio::test]
async fn test_intent_throughput_optimized() {
    let mut tb = Testbench::new("tests/fixtures/intent/intent_throughput.sk")
        .await
        .unwrap();

    tb.set("selector", 0u8);
    tb.step().await;

    // Default behavior: is_throughput_optimized() returns false (0)
    tb.expect("is_tput_opt", 0u8).await;
}

/// Test all three intent helpers together
#[tokio::test]
async fn test_intent_all_three() {
    let mut tb = Testbench::new("tests/fixtures/intent/intent_all_three.sk")
        .await
        .unwrap();

    tb.set("selector", 0u8);
    tb.step().await;

    // Default: latency=1, area=0, throughput=0
    tb.expect("is_lat", 1u8).await;
    tb.expect("is_area", 0u8).await;
    tb.expect("is_tput", 0u8).await;
}

/// Test conditional behavior based on intent helpers
/// By default, is_latency_optimized() is true, so the fast parallel path is taken
#[tokio::test]
async fn test_intent_conditional() {
    let mut tb = Testbench::new("tests/fixtures/intent/intent_conditional.sk")
        .await
        .unwrap();

    // Test with a=5, b=3
    tb.set("a", 5u8).set("b", 3u8);
    tb.step().await;

    // Since is_latency_optimized() is true, result should be a + b = 8
    tb.expect("result", 8u8).await;

    // Test with different values
    tb.set("a", 100u8).set("b", 55u8);
    tb.step().await;
    tb.expect("result", 155u8).await;
}

// ============================================================================
// FP32 Arithmetic Tests
// ============================================================================

/// Test FP32 basic arithmetic operations
#[ignore = "requires stdlib parsing support for fp.sk advanced syntax"]
#[tokio::test]
async fn test_fp32_arithmetic() {
    let mut tb = Testbench::new("tests/fixtures/numeric/fp_arithmetic.sk")
        .await
        .unwrap();

    // Test: a=10.0, b=3.0
    let a: f32 = 10.0;
    let b: f32 = 3.0;

    tb.set("a", a).set("b", b);
    tb.step().await;

    // Addition: 10.0 + 3.0 = 13.0
    tb.expect_fp32("add_result", 13.0, 0.001).await;

    // Subtraction: 10.0 - 3.0 = 7.0
    tb.expect_fp32("sub_result", 7.0, 0.001).await;

    // Multiplication: 10.0 * 3.0 = 30.0
    tb.expect_fp32("mul_result", 30.0, 0.001).await;

    // Division: 10.0 / 3.0 ≈ 3.333...
    tb.expect_fp32("div_result", 3.333333, 0.001).await;
}

/// Test FP32 comparison operations
/// IGNORED: Metal shader gen bug - bitwise ops on fp32 comparison results
#[tokio::test]
#[ignore]
async fn test_fp32_comparison_less() {
    let mut tb = Testbench::new("tests/fixtures/numeric/fp_comparison.sk")
        .await
        .unwrap();

    // Test: a=2.5, b=3.5 (a < b)
    let a: f32 = 2.5;
    let b: f32 = 3.5;

    tb.set("a", a).set("b", b);
    tb.step().await;

    tb.expect("is_lt", 1u8).await; // 2.5 < 3.5 is true
    tb.expect("is_le", 1u8).await; // 2.5 <= 3.5 is true
    tb.expect("is_gt", 0u8).await; // 2.5 > 3.5 is false
    tb.expect("is_ge", 0u8).await; // 2.5 >= 3.5 is false
    tb.expect("is_eq", 0u8).await; // 2.5 == 3.5 is false
    tb.expect("is_ne", 1u8).await; // 2.5 != 3.5 is true
}

/// Test FP32 comparison with equal values
/// IGNORED: Metal shader gen bug - bitwise ops on fp32 comparison results
#[tokio::test]
#[ignore]
async fn test_fp32_comparison_equal() {
    let mut tb = Testbench::new("tests/fixtures/numeric/fp_comparison.sk")
        .await
        .unwrap();

    // Test: a=5.0, b=5.0 (a == b)
    let a: f32 = 5.0;
    let b: f32 = 5.0;

    tb.set("a", a).set("b", b);
    tb.step().await;

    tb.expect("is_lt", 0u8).await; // 5.0 < 5.0 is false
    tb.expect("is_le", 1u8).await; // 5.0 <= 5.0 is true
    tb.expect("is_gt", 0u8).await; // 5.0 > 5.0 is false
    tb.expect("is_ge", 1u8).await; // 5.0 >= 5.0 is true
    tb.expect("is_eq", 1u8).await; // 5.0 == 5.0 is true
    tb.expect("is_ne", 0u8).await; // 5.0 != 5.0 is false
}

// ============================================================================
// FP32 Square Root Tests
// ============================================================================

/// Test FP32 sqrt with perfect squares
#[tokio::test]
async fn test_fp32_sqrt_perfect_squares() {
    let mut tb = Testbench::new("tests/fixtures/numeric/fp_sqrt_simple.sk")
        .await
        .unwrap();

    // sqrt(4.0) = 2.0
    tb.set("x", 4.0f32);
    tb.step().await;
    tb.expect_fp32("result", 2.0, 0.001).await;

    // sqrt(9.0) = 3.0
    tb.set("x", 9.0f32);
    tb.step().await;
    tb.expect_fp32("result", 3.0, 0.001).await;

    // sqrt(16.0) = 4.0
    tb.set("x", 16.0f32);
    tb.step().await;
    tb.expect_fp32("result", 4.0, 0.001).await;

    // sqrt(100.0) = 10.0
    tb.set("x", 100.0f32);
    tb.step().await;
    tb.expect_fp32("result", 10.0, 0.001).await;
}

/// Test FP32 sqrt with non-perfect squares
#[tokio::test]
async fn test_fp32_sqrt_non_perfect() {
    let mut tb = Testbench::new("tests/fixtures/numeric/fp_sqrt_simple.sk")
        .await
        .unwrap();

    // sqrt(2.0) ≈ 1.414
    tb.set("x", 2.0f32);
    tb.step().await;
    tb.expect_fp32("result", std::f32::consts::SQRT_2, 0.001)
        .await;

    // sqrt(3.0) ≈ 1.732
    tb.set("x", 3.0f32);
    tb.step().await;
    tb.expect_fp32("result", 1.732_050_8, 0.001).await;

    // sqrt(5.0) ≈ 2.236
    tb.set("x", 5.0f32);
    tb.step().await;
    tb.expect_fp32("result", 2.236_068, 0.001).await;
}

/// Test FP32 sqrt with small values
#[tokio::test]
async fn test_fp32_sqrt_small_values() {
    let mut tb = Testbench::new("tests/fixtures/numeric/fp_sqrt_simple.sk")
        .await
        .unwrap();

    // sqrt(0.25) = 0.5
    tb.set("x", 0.25f32);
    tb.step().await;
    tb.expect_fp32("result", 0.5, 0.001).await;

    // sqrt(0.01) = 0.1
    tb.set("x", 0.01f32);
    tb.step().await;
    tb.expect_fp32("result", 0.1, 0.001).await;

    // sqrt(1.0) = 1.0
    tb.set("x", 1.0f32);
    tb.step().await;
    tb.expect_fp32("result", 1.0, 0.001).await;
}

// ============================================================================
// FP32 Quadratic Formula Tests
// ============================================================================

/// Test quadratic formula: x^2 - 5x + 6 = 0 has roots x=2, x=3
/// Using (-b + sqrt(b^2 - 4ac)) / 2a
/// IGNORED: sqrt() on intermediate computations returns 0 in Metal shader gen
#[tokio::test]
#[ignore]
async fn test_fp32_quadratic_simple() {
    let mut tb = Testbench::new("tests/fixtures/numeric/fp_quadratic.sk")
        .await
        .unwrap();

    // x^2 - 5x + 6 = 0: a=1, b=-5, c=6
    // discriminant = 25 - 24 = 1
    // x1 = (5 + 1) / 2 = 3
    tb.set("a", 1.0f32).set("b", -5.0f32).set("c", 6.0f32);
    tb.step().await;

    tb.expect_fp32("discriminant", 1.0, 0.001).await;
    tb.expect_fp32("x1", 3.0, 0.001).await;
}

/// Test quadratic formula: x^2 - 4x + 4 = 0 has double root x=2
/// IGNORED: sqrt() on intermediate computations returns 0 in Metal shader gen
#[tokio::test]
#[ignore]
async fn test_fp32_quadratic_double_root() {
    let mut tb = Testbench::new("tests/fixtures/numeric/fp_quadratic.sk")
        .await
        .unwrap();

    // x^2 - 4x + 4 = 0: a=1, b=-4, c=4
    // discriminant = 16 - 16 = 0
    // x1 = 4 / 2 = 2
    tb.set("a", 1.0f32).set("b", -4.0f32).set("c", 4.0f32);
    tb.step().await;

    tb.expect_fp32("discriminant", 0.0, 0.001).await;
    tb.expect_fp32("x1", 2.0, 0.001).await;
}

/// Test quadratic formula: 2x^2 - 8x + 6 = 0 has roots x=1, x=3
/// IGNORED: sqrt() on intermediate computations returns 0 in Metal shader gen
#[tokio::test]
#[ignore]
async fn test_fp32_quadratic_non_unit_a() {
    let mut tb = Testbench::new("tests/fixtures/numeric/fp_quadratic.sk")
        .await
        .unwrap();

    // 2x^2 - 8x + 6 = 0: a=2, b=-8, c=6
    // discriminant = 64 - 48 = 16
    // x1 = (8 + 4) / 4 = 3
    tb.set("a", 2.0f32).set("b", -8.0f32).set("c", 6.0f32);
    tb.step().await;

    tb.expect_fp32("discriminant", 16.0, 0.001).await;
    tb.expect_fp32("x1", 3.0, 0.001).await;
}

// ============================================================================
// FP32 Distance Calculation Tests
// ============================================================================

/// Test 2D distance calculation: distance between (0,0) and (3,4) = 5
/// IGNORED: sqrt() on intermediate computations returns 0 in Metal shader gen
#[tokio::test]
#[ignore]
async fn test_fp32_distance_345_triangle() {
    let mut tb = Testbench::new("tests/fixtures/numeric/fp_distance.sk")
        .await
        .unwrap();

    // Distance from (0,0) to (3,4) = sqrt(9+16) = sqrt(25) = 5
    tb.set("x1", 0.0f32)
        .set("y1", 0.0f32)
        .set("x2", 3.0f32)
        .set("y2", 4.0f32);
    tb.step().await;

    tb.expect_fp32("distance", 5.0, 0.001).await;
}

/// Test 2D distance calculation: distance between (1,1) and (4,5) = 5
/// IGNORED: sqrt() on intermediate computations returns 0 in Metal shader gen
#[tokio::test]
#[ignore]
async fn test_fp32_distance_offset_345_triangle() {
    let mut tb = Testbench::new("tests/fixtures/numeric/fp_distance.sk")
        .await
        .unwrap();

    // Distance from (1,1) to (4,5) = sqrt(9+16) = sqrt(25) = 5
    tb.set("x1", 1.0f32)
        .set("y1", 1.0f32)
        .set("x2", 4.0f32)
        .set("y2", 5.0f32);
    tb.step().await;

    tb.expect_fp32("distance", 5.0, 0.001).await;
}

/// Test 2D distance calculation: distance along x-axis
/// IGNORED: sqrt() on intermediate computations returns 0 in Metal shader gen
#[tokio::test]
#[ignore]
async fn test_fp32_distance_horizontal() {
    let mut tb = Testbench::new("tests/fixtures/numeric/fp_distance.sk")
        .await
        .unwrap();

    // Distance from (2,3) to (7,3) = 5 (horizontal line)
    tb.set("x1", 2.0f32)
        .set("y1", 3.0f32)
        .set("x2", 7.0f32)
        .set("y2", 3.0f32);
    tb.step().await;

    tb.expect_fp32("distance", 5.0, 0.001).await;
}

/// Test 2D distance calculation: distance along y-axis
/// IGNORED: sqrt() on intermediate computations returns 0 in Metal shader gen
#[tokio::test]
#[ignore]
async fn test_fp32_distance_vertical() {
    let mut tb = Testbench::new("tests/fixtures/numeric/fp_distance.sk")
        .await
        .unwrap();

    // Distance from (4,1) to (4,9) = 8 (vertical line)
    tb.set("x1", 4.0f32)
        .set("y1", 1.0f32)
        .set("x2", 4.0f32)
        .set("y2", 9.0f32);
    tb.step().await;

    tb.expect_fp32("distance", 8.0, 0.001).await;
}

/// Test 2D distance calculation: distance = 0 (same point)
#[tokio::test]
async fn test_fp32_distance_zero() {
    let mut tb = Testbench::new("tests/fixtures/numeric/fp_distance.sk")
        .await
        .unwrap();

    // Distance from (5,5) to (5,5) = 0
    tb.set("x1", 5.0f32)
        .set("y1", 5.0f32)
        .set("x2", 5.0f32)
        .set("y2", 5.0f32);
    tb.step().await;

    tb.expect_fp32("distance", 0.0, 0.001).await;
}

/// Test 2D distance with irrational result
/// IGNORED: sqrt() on intermediate computations returns 0 in Metal shader gen
#[tokio::test]
#[ignore]
async fn test_fp32_distance_irrational() {
    let mut tb = Testbench::new("tests/fixtures/numeric/fp_distance.sk")
        .await
        .unwrap();

    // Distance from (0,0) to (1,1) = sqrt(2) ≈ 1.414
    tb.set("x1", 0.0f32)
        .set("y1", 0.0f32)
        .set("x2", 1.0f32)
        .set("y2", 1.0f32);
    tb.step().await;

    tb.expect_fp32("distance", std::f32::consts::SQRT_2, 0.001)
        .await;
}

// ============================================================================
// ImplStyle Attribute Tests
// ============================================================================

/// Test #[impl_style::parallel] attribute on let statement
/// Verifies that the attribute is parsed and computation works correctly
#[tokio::test]
async fn test_impl_style_parallel() {
    let mut tb = Testbench::new("tests/fixtures/intent/intent_impl_style_parallel.sk")
        .await
        .unwrap();

    // Test with entity TestImplStyleParallel: a + b
    tb.set("a", 10u32).set("b", 5u32);
    tb.step().await;
    tb.expect("result", 15u32).await;

    // Test with larger values
    tb.set("a", 1000u32).set("b", 500u32);
    tb.step().await;
    tb.expect("result", 1500u32).await;
}

/// Test #[impl_style::tree] attribute on let statement
/// Verifies that the attribute is parsed and computation works correctly
#[tokio::test]
async fn test_impl_style_tree() {
    let mut tb = Testbench::new("tests/fixtures/intent/intent_impl_style_tree.sk")
        .await
        .unwrap();

    // Test with entity TestImplStyleTree: a * b
    tb.set("a", 6u32).set("b", 7u32);
    tb.step().await;
    tb.expect("result", 42u32).await;

    // Test with other values
    tb.set("a", 100u32).set("b", 20u32);
    tb.step().await;
    tb.expect("result", 2000u32).await;
}

/// Test #[impl_style::sequential] attribute on let statement
/// Verifies that the attribute is parsed and computation works correctly
#[tokio::test]
async fn test_impl_style_sequential() {
    let mut tb = Testbench::new("tests/fixtures/intent/intent_impl_style_sequential.sk")
        .await
        .unwrap();

    // Test with entity TestImplStyleSequential: a - b
    tb.set("a", 20u32).set("b", 7u32);
    tb.step().await;
    tb.expect("result", 13u32).await;

    // Test with other values
    tb.set("a", 1000u32).set("b", 300u32);
    tb.step().await;
    tb.expect("result", 700u32).await;
}

/// Test #[impl_style::auto] attribute on let statement (default behavior)
/// Verifies that the attribute is parsed and computation works correctly
#[tokio::test]
async fn test_impl_style_auto() {
    let mut tb = Testbench::new("tests/fixtures/intent/intent_impl_style_auto.sk")
        .await
        .unwrap();

    // Test with entity TestImplStyleAuto: a | b (bitwise OR)
    tb.set("a", 0b1100u32).set("b", 0b0011u32);
    tb.step().await;
    tb.expect("result", 0b1111u32).await;

    // Test with other values
    tb.set("a", 0xFF00u32).set("b", 0x00FFu32);
    tb.step().await;
    tb.expect("result", 0xFFFFu32).await;
}
