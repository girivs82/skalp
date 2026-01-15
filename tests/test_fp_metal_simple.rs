//! Simple FP32 multiply test via Metal simulator

#![cfg(not(debug_assertions))]

use skalp_testing::Testbench;

const SOURCE: &str = r#"
use skalp::numeric::fp::*;

entity FpMulTest {
    in a: bit[32]
    in b: bit[32]
    out result: bit[32]
}

impl FpMulTest {
    signal a_fp: fp32 = a as fp32
    signal b_fp: fp32 = b as fp32
    signal prod: fp32 = a_fp * b_fp
    result = prod as bit[32]
}
"#;

#[tokio::test]
async fn test_fp_metal_mul() {
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        "/Users/girivs/src/hw/hls/crates/skalp-stdlib",
    );

    // Write source to temp file
    std::fs::write("/tmp/test_fp_metal.sk", SOURCE).unwrap();

    let mut tb = Testbench::with_top_module("/tmp/test_fp_metal.sk", "FpMulTest")
        .await
        .expect("Failed to create testbench");

    // Test 1.0 * 2.0 = 2.0
    let a: u32 = 1.0f32.to_bits(); // 0x3F800000
    let b: u32 = 2.0f32.to_bits(); // 0x40000000

    println!("Testing 1.0 * 2.0:");
    println!("  a = 0x{:08X} ({})", a, f32::from_bits(a));
    println!("  b = 0x{:08X} ({})", b, f32::from_bits(b));

    tb.set("a", a as u64).set("b", b as u64);

    tb.step().await;

    let result: u32 = tb.get_as("result").await;
    let result_f = f32::from_bits(result);

    println!("  result = 0x{:08X} ({})", result, result_f);

    assert!(
        (result_f - 2.0).abs() < 1e-6,
        "1.0 * 2.0 should be 2.0, got {}",
        result_f
    );

    println!("  PASS!");
}
