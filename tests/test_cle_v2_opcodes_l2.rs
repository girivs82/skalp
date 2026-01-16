//! CLE V2 L2 Opcode Tests (Floating Point Operations)
//!
//! Tests all 10 L2 opcodes (18-27):
//! - FP16: ADD, MUL, MAC, DIV, SQRT
//! - FP32: ADD, MUL, MAC, DIV, SQRT
//!
//! Data packing format:
//! - Binary ops: data_read[31:0] = A, data_read[63:32] = B
//! - MAC ops: data_read[31:0] = A, data_read[63:32] = B, needs separate accumulator path

#![cfg(not(debug_assertions))]

use skalp_testing::Testbench;
use std::path::Path;

const CLE_V2_SYNC_SOURCE: &str = "/Users/girivs/src/hw/karythra/rtl/skalp/cle/src/main_v2_sync.sk";
const STDLIB_PATH: &str = "/Users/girivs/src/hw/hls/crates/skalp-stdlib";
const TOP_MODULE: &str = "KarythraCLE_V2_Sync";

// L2 Opcodes (from types.sk)
const OP_FP16_ADD: u64 = 18;
const OP_FP16_MUL: u64 = 19;
const OP_FP16_MAC: u64 = 20;
const OP_FP16_DIV: u64 = 21;
const OP_FP16_SQRT: u64 = 22;
const OP_FP32_ADD: u64 = 23;
const OP_FP32_MUL: u64 = 24;
const OP_FP32_MAC: u64 = 25;
const OP_FP32_DIV: u64 = 26;
const OP_FP32_SQRT: u64 = 27;

// Operating mode
const MODE_NORMAL: u64 = 0b00;

fn source_available() -> bool {
    std::env::set_var("SKALP_STDLIB_PATH", STDLIB_PATH);
    Path::new(CLE_V2_SYNC_SOURCE).exists()
}

/// Helper: Convert f32 to IEEE 754 bit pattern
fn f32_to_bits(f: f32) -> u32 {
    f.to_bits()
}

/// Helper: Convert IEEE 754 bit pattern to f32
fn bits_to_f32(bits: u32) -> f32 {
    f32::from_bits(bits)
}

/// Helper: Convert f32 to FP16 bit pattern (IEEE 754 half-precision)
fn f32_to_fp16(f: f32) -> u16 {
    half::f16::from_f32(f).to_bits()
}

/// Helper: Convert FP16 bit pattern to f32
fn fp16_to_f32(bits: u16) -> f32 {
    half::f16::from_bits(bits).to_f32()
}

/// Helper: Configure CLE for normal mode operation
/// Config loading is a two-stage process:
/// 1. First clock: config_next → config_shadow
/// 2. Second clock (with morph_trigger): config_shadow → config_active
async fn setup_normal_mode(tb: &mut Testbench, opcode: u64) {
    let config_value = opcode << 3;
    println!(
        "DEBUG L2: Setting config_next = {} (0x{:X}) for opcode {}",
        config_value, config_value, opcode
    );

    tb.set("cle_mode", MODE_NORMAL)
        .set("config_next", config_value);

    // Stage 1: Load config_next into config_shadow
    tb.clock(1).await;

    // Stage 2: Trigger morph to load config_shadow into config_active
    tb.set("morph_trigger", 1u64);
    tb.clock(1).await;
    tb.set("morph_trigger", 0u64);

    let func_sel_after: u8 = tb.get_as("function_sel").await;
    println!(
        "DEBUG L2: function_sel AFTER = {} (expected {})",
        func_sel_after, opcode
    );
}

/// Helper: Execute FP32 binary operation
async fn execute_fp32_binary(tb: &mut Testbench, a: f32, b: f32) -> f32 {
    let a_bits = f32_to_bits(a);
    let b_bits = f32_to_bits(b);
    let data = ((b_bits as u64) << 32) | (a_bits as u64);
    tb.set("data_read", data)
        .set("data_read_valid", 1u64)
        .set("execute_enable", 1u64);

    // Debug: check immediate fu_result
    tb.step().await;
    let fu_imm: u32 = tb.get_as("debug_fu_result").await;
    println!("    L2 immediate fu_result: 0x{:08X}", fu_imm);

    tb.clock(4).await; // 4-stage pipeline
    let result: u64 = tb.get_as("data_write").await;
    bits_to_f32((result & 0xFFFFFFFF) as u32)
}

/// Helper: Execute FP32 unary operation (SQRT)
async fn execute_fp32_unary(tb: &mut Testbench, a: f32) -> f32 {
    let a_bits = f32_to_bits(a);
    let data = a_bits as u64;
    tb.set("data_read", data)
        .set("data_read_valid", 1u64)
        .set("execute_enable", 1u64);
    tb.clock(4).await;
    let result: u64 = tb.get_as("data_write").await;
    bits_to_f32((result & 0xFFFFFFFF) as u32)
}

/// Helper: Execute FP16 binary operation
async fn execute_fp16_binary(tb: &mut Testbench, a: f32, b: f32) -> f32 {
    let a_bits = f32_to_fp16(a) as u32;
    let b_bits = f32_to_fp16(b) as u32;
    let data = ((b_bits as u64) << 32) | (a_bits as u64);
    println!(
        "DEBUG FP16: a={} (0x{:04X}), b={} (0x{:04X}), data=0x{:016X}",
        a, a_bits, b, b_bits, data
    );
    tb.set("data_read", data)
        .set("data_read_valid", 1u64)
        .set("execute_enable", 1u64);

    // Debug: check immediate fu_result (same as FP32 test)
    tb.step().await;
    let fu_imm: u32 = tb.get_as("debug_fu_result").await;
    println!("    FP16 immediate fu_result: 0x{:08X}", fu_imm);

    tb.clock(4).await;
    let result: u64 = tb.get_as("data_write").await;
    println!(
        "DEBUG FP16: data_write=0x{:016X}, result_bits=0x{:04X}",
        result,
        (result & 0xFFFF) as u16
    );
    fp16_to_f32((result & 0xFFFF) as u16)
}

/// Helper: Execute FP16 unary operation (SQRT)
async fn execute_fp16_unary(tb: &mut Testbench, a: f32) -> f32 {
    let a_bits = f32_to_fp16(a) as u32;
    let data = a_bits as u64;
    tb.set("data_read", data)
        .set("data_read_valid", 1u64)
        .set("execute_enable", 1u64);
    tb.clock(4).await;
    let result: u64 = tb.get_as("data_write").await;
    fp16_to_f32((result & 0xFFFF) as u16)
}

/// Helper: Check FP equality with tolerance
fn fp_eq(a: f32, b: f32, tolerance: f32) -> bool {
    if a.is_nan() && b.is_nan() {
        return true;
    }
    if a.is_infinite() && b.is_infinite() {
        return a.signum() == b.signum();
    }
    (a - b).abs() <= tolerance
}

// =============================================================================
// FP32 Operations
// =============================================================================

#[tokio::test]
async fn test_l2_fp32_add() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_FP32_ADD).await;

    let tests: [(f32, f32, f32); 6] = [
        (0.0, 0.0, 0.0),
        (1.0, 1.0, 2.0),
        (1.5, 2.5, 4.0),
        (-1.0, 1.0, 0.0),
        (100.5, -50.25, 50.25),
        (1e6, 1e-6, 1e6), // Large + small
    ];

    for (a, b, expected) in tests {
        // Debug: print input values
        let a_bits = f32_to_bits(a);
        let b_bits = f32_to_bits(b);
        println!(
            "DEBUG: a={} (0x{:08X}), b={} (0x{:08X})",
            a, a_bits, b, b_bits
        );

        let result = execute_fp32_binary(&mut tb, a, b).await;

        // Debug: check intermediate values
        let fu_result: u32 = tb.get_as("debug_fu_result").await;
        println!(
            "DEBUG: debug_fu_result=0x{:08X}, result={}",
            fu_result, result
        );

        assert!(
            fp_eq(result, expected, 1e-5),
            "FP32_ADD: {} + {} = {}, got {}",
            a,
            b,
            expected,
            result
        );
    }
    println!("L2 FP32_ADD: all tests passed");
}

#[tokio::test]
async fn test_l2_fp32_mul() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_FP32_MUL).await;

    let tests: [(f32, f32, f32); 6] = [
        (0.0, 100.0, 0.0),
        (1.0, 1.0, 1.0),
        (2.0, 3.0, 6.0),
        (-2.0, 3.0, -6.0),
        (1.5, 2.0, 3.0),
        (100.0, 0.01, 1.0),
    ];

    for (a, b, expected) in tests {
        let result = execute_fp32_binary(&mut tb, a, b).await;
        assert!(
            fp_eq(result, expected, 1e-5),
            "FP32_MUL: {} * {} = {}, got {}",
            a,
            b,
            expected,
            result
        );
    }
    println!("L2 FP32_MUL: all tests passed");
}

#[tokio::test]
async fn test_l2_fp32_div() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_FP32_DIV).await;

    let tests: [(f32, f32, f32); 5] = [
        (0.0, 1.0, 0.0),
        (6.0, 2.0, 3.0),
        (10.0, 4.0, 2.5),
        (-6.0, 2.0, -3.0),
        (1.0, 3.0, 0.333333),
    ];

    for (a, b, expected) in tests {
        let result = execute_fp32_binary(&mut tb, a, b).await;
        assert!(
            fp_eq(result, expected, 1e-4),
            "FP32_DIV: {} / {} = {}, got {}",
            a,
            b,
            expected,
            result
        );
    }
    println!("L2 FP32_DIV: all tests passed");
}

#[tokio::test]
async fn test_l2_fp32_sqrt() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_FP32_SQRT).await;

    let tests: [(f32, f32); 5] = [
        (0.0, 0.0),
        (1.0, 1.0),
        (4.0, 2.0),
        (9.0, 3.0),
        (2.0, 1.41421356),
    ];

    for (a, expected) in tests {
        let result = execute_fp32_unary(&mut tb, a).await;
        assert!(
            fp_eq(result, expected, 1e-4),
            "FP32_SQRT: sqrt({}) = {}, got {}",
            a,
            expected,
            result
        );
    }
    println!("L2 FP32_SQRT: all tests passed");
}

// =============================================================================
// FP16 Operations
// =============================================================================

#[tokio::test]
async fn test_l2_fp16_add() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_FP16_ADD).await;

    let tests: [(f32, f32, f32); 5] = [
        (0.0, 0.0, 0.0),
        (1.0, 1.0, 2.0),
        (1.5, 2.5, 4.0),
        (-1.0, 1.0, 0.0),
        (10.0, -5.0, 5.0),
    ];

    for (a, b, expected) in tests {
        let result = execute_fp16_binary(&mut tb, a, b).await;
        // FP16 has lower precision, use larger tolerance
        assert!(
            fp_eq(result, expected, 0.01),
            "FP16_ADD: {} + {} = {}, got {}",
            a,
            b,
            expected,
            result
        );
    }
    println!("L2 FP16_ADD: all tests passed");
}

#[tokio::test]
async fn test_l2_fp16_mul() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_FP16_MUL).await;

    let tests: [(f32, f32, f32); 5] = [
        (0.0, 100.0, 0.0),
        (1.0, 1.0, 1.0),
        (2.0, 3.0, 6.0),
        (-2.0, 3.0, -6.0),
        (1.5, 2.0, 3.0),
    ];

    for (a, b, expected) in tests {
        let result = execute_fp16_binary(&mut tb, a, b).await;
        assert!(
            fp_eq(result, expected, 0.01),
            "FP16_MUL: {} * {} = {}, got {}",
            a,
            b,
            expected,
            result
        );
    }
    println!("L2 FP16_MUL: all tests passed");
}

#[tokio::test]
async fn test_l2_fp16_div() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_FP16_DIV).await;

    let tests: [(f32, f32, f32); 4] = [
        (0.0, 1.0, 0.0),
        (6.0, 2.0, 3.0),
        (10.0, 4.0, 2.5),
        (-6.0, 2.0, -3.0),
    ];

    for (a, b, expected) in tests {
        let result = execute_fp16_binary(&mut tb, a, b).await;
        assert!(
            fp_eq(result, expected, 0.01),
            "FP16_DIV: {} / {} = {}, got {}",
            a,
            b,
            expected,
            result
        );
    }
    println!("L2 FP16_DIV: all tests passed");
}

#[tokio::test]
async fn test_l2_fp16_sqrt() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_FP16_SQRT).await;

    let tests: [(f32, f32); 4] = [(0.0, 0.0), (1.0, 1.0), (4.0, 2.0), (9.0, 3.0)];

    for (a, expected) in tests {
        let result = execute_fp16_unary(&mut tb, a).await;
        assert!(
            fp_eq(result, expected, 0.01),
            "FP16_SQRT: sqrt({}) = {}, got {}",
            a,
            expected,
            result
        );
    }
    println!("L2 FP16_SQRT: all tests passed");
}

// =============================================================================
// MAC Operations (Note: These require accumulator path which may need special handling)
// =============================================================================

#[tokio::test]
async fn test_l2_fp32_mac() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_FP32_MAC).await;

    // MAC: (a * b) + c where c comes from a separate path
    // For now test with c=0 (which makes it equivalent to MUL)
    // Full MAC testing requires understanding accumulator path
    let tests: [(f32, f32, f32); 3] = [
        (2.0, 3.0, 6.0),   // 2*3 + 0 = 6
        (1.5, 2.0, 3.0),   // 1.5*2 + 0 = 3
        (-2.0, 3.0, -6.0), // -2*3 + 0 = -6
    ];

    for (a, b, expected) in tests {
        let result = execute_fp32_binary(&mut tb, a, b).await;
        assert!(
            fp_eq(result, expected, 1e-4),
            "FP32_MAC: {} * {} + 0 = {}, got {}",
            a,
            b,
            expected,
            result
        );
    }
    println!("L2 FP32_MAC: basic tests passed (c=0)");
}

#[tokio::test]
async fn test_l2_fp16_mac() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_FP16_MAC).await;

    // MAC with c=0
    let tests: [(f32, f32, f32); 3] = [(2.0, 3.0, 6.0), (1.5, 2.0, 3.0), (-2.0, 3.0, -6.0)];

    for (a, b, expected) in tests {
        let result = execute_fp16_binary(&mut tb, a, b).await;
        assert!(
            fp_eq(result, expected, 0.01),
            "FP16_MAC: {} * {} + 0 = {}, got {}",
            a,
            b,
            expected,
            result
        );
    }
    println!("L2 FP16_MAC: basic tests passed (c=0)");
}
