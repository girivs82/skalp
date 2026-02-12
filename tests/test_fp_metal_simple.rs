//! Simple FP32 multiply test via Metal simulator


use skalp_testing::Testbench;

fn setup_stdlib_path() {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let stdlib_path = format!("{}/crates/skalp-stdlib", manifest_dir);
    std::env::set_var("SKALP_STDLIB_PATH", &stdlib_path);
}

fn fixture_path(name: &str) -> String {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    format!("{}/tests/fixtures/{}", manifest_dir, name)
}

#[tokio::test]
async fn test_fp_metal_mul() {
    setup_stdlib_path();

    let mut tb = Testbench::with_top_module(&fixture_path("fp_mul_test.sk"), "FpMulTest")
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
