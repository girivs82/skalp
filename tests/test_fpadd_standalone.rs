//! Standalone FpAdd test - tests FP32 and FP16 addition directly


use skalp_testing::Testbench;

fn setup_stdlib_path() {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let stdlib_path = format!("{}/crates/skalp-stdlib", manifest_dir);
    std::env::set_var("SKALP_STDLIB_PATH", &stdlib_path);
}

const FP32_SOURCE: &str = r#"
use skalp::numeric::fp::*;

pub entity FpAddTest32 {
    in a: bit[32]
    in b: bit[32]
    out result: bit[32]
}

impl FpAddTest32 {
    signal a_fp: fp32 = a as fp32
    signal b_fp: fp32 = b as fp32
    signal sum: fp32 = a_fp + b_fp
    result = sum as bit[32]
}
"#;

const FP16_SOURCE: &str = r#"
use skalp::numeric::fp::*;

pub entity FpAddTest16 {
    in a: bit[16]
    in b: bit[16]
    out result: bit[16]
}

impl FpAddTest16 {
    signal a_fp: fp16 = a as fp16
    signal b_fp: fp16 = b as fp16
    signal sum: fp16 = a_fp + b_fp
    result = sum as bit[16]
}
"#;

#[tokio::test]
async fn test_fp32_add_standalone() {
    setup_stdlib_path();

    let temp_file = std::env::temp_dir().join("test_fpadd32.sk");
    std::fs::write(&temp_file, FP32_SOURCE).expect("write temp file");

    let mut tb = Testbench::with_top_module(temp_file.to_str().unwrap(), "FpAddTest32")
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
}

#[tokio::test]
async fn test_fp16_add_standalone() {
    setup_stdlib_path();

    let temp_file = std::env::temp_dir().join("test_fpadd16.sk");
    std::fs::write(&temp_file, FP16_SOURCE).expect("write temp file");

    let mut tb = Testbench::with_top_module(temp_file.to_str().unwrap(), "FpAddTest16")
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
}
