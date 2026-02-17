//! Standalone FpAdd test - tests FP32 and FP16 addition directly


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
async fn test_fp32_add_standalone() {
    setup_stdlib_path();

    let mut tb = Testbench::with_top_module(&fixture_path("fpadd_fp32.sk"), "FpAddTest32")
        .await
        .expect("Failed to create testbench");

    let tests: [(f32, f32, f32); 4] = [
        (0.0, 0.0, 0.0),
        (1.0, 1.0, 2.0),
        (1.5, 2.5, 4.0),
        (-1.0, 1.0, 0.0),
    ];

    for (a, b, expected) in tests {
        tb.set("a", a.to_bits() as u64)
            .set("b", b.to_bits() as u64);
        tb.step().await;

        let result_bits: u32 = tb.get_as("result").await;
        let result = f32::from_bits(result_bits);
        println!(
            "FP32: {} + {} = {} (bits: 0x{:08X}, expected: {})",
            a, b, result, result_bits, expected
        );
        assert!(
            (result - expected).abs() < 1e-6,
            "FP32_ADD: {} + {} = {}, got {} (0x{:08X})",
            a, b, expected, result, result_bits
        );
    }
    tb.export_waveform("build/test_fp32_add_standalone.skw.gz").ok();
}

#[tokio::test]
async fn test_fp16_add_standalone() {
    setup_stdlib_path();

    let mut tb = Testbench::with_top_module(&fixture_path("fpadd_fp16.sk"), "FpAddTest16")
        .await
        .expect("Failed to create testbench");

    // (a_fp16_bits, b_fp16_bits, expected_fp16_bits)
    let tests: [(u16, u16, u16); 4] = [
        (0x0000, 0x0000, 0x0000), // 0 + 0 = 0
        (0x3C00, 0x3C00, 0x4000), // 1.0 + 1.0 = 2.0
        (0x3E00, 0x4100, 0x4400), // 1.5 + 2.5 = 4.0
        (0xBC00, 0x3C00, 0x0000), // -1.0 + 1.0 = 0.0
    ];

    for (a, b, expected) in tests {
        tb.set("a", a as u64).set("b", b as u64);
        tb.step().await;

        let result_bits: u16 = tb.get_as("result").await;
        let result_f = half::f16::from_bits(result_bits).to_f32();
        let expected_f = half::f16::from_bits(expected).to_f32();
        println!(
            "FP16: 0x{:04X} + 0x{:04X} = 0x{:04X} ({}) expected 0x{:04X} ({})",
            a, b, result_bits, result_f, expected, expected_f
        );
        assert_eq!(
            result_bits, expected,
            "FP16_ADD: 0x{:04X} + 0x{:04X} = 0x{:04X}, got 0x{:04X}",
            a, b, expected, result_bits
        );
    }
    tb.export_waveform("build/test_fp16_add_standalone.skw.gz").ok();
}
