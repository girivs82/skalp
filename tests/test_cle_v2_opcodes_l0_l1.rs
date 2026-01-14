//! CLE V2 L0-L1 Opcode Tests (Basic Integer Operations)
//!
//! Tests all 18 L0-L1 opcodes (0-17):
//! - Arithmetic: ADD, SUB, MUL
//! - Logic: AND, OR, XOR, NOT
//! - Shifts: SLL, SRL, SRA
//! - Comparison: EQ, NE, LT, GE, LTU, GEU
//! - Min/Max: MIN, MAX

#![cfg(not(debug_assertions))]

use skalp_testing::Testbench;
use std::path::Path;

const CLE_V2_SYNC_SOURCE: &str = "/Users/girivs/src/hw/karythra/rtl/skalp/cle/src/main_v2_sync.sk";
const STDLIB_PATH: &str = "/Users/girivs/src/hw/hls/crates/skalp-stdlib";
const TOP_MODULE: &str = "KarythraCLE_V2_Sync";

// L0-L1 Opcodes (from types.sk)
const OP_ADD: u64 = 0;
const OP_SUB: u64 = 1;
const OP_MUL: u64 = 2;
const OP_AND: u64 = 3;
const OP_OR: u64 = 4;
const OP_XOR: u64 = 5;
const OP_NOT: u64 = 6;
const OP_SLL: u64 = 7;
const OP_SRL: u64 = 8;
const OP_SRA: u64 = 9;
const OP_EQ: u64 = 10;
const OP_NE: u64 = 11;
const OP_LT: u64 = 12;
const OP_GE: u64 = 13;
const OP_LTU: u64 = 14;
const OP_GEU: u64 = 15;
const OP_MIN: u64 = 16;
const OP_MAX: u64 = 17;

// Operating modes
const MODE_NORMAL: u64 = 0b00; // Uses data_read for operands
                               // const MODE_DIRECT: u64 = 0b01;  // Uses internal registers (requires pre-loading)
                               // const MODE_SYSTOLIC: u64 = 0b10;  // Uses systolic links

fn source_available() -> bool {
    std::env::set_var("SKALP_STDLIB_PATH", STDLIB_PATH);
    Path::new(CLE_V2_SYNC_SOURCE).exists()
}

/// Helper: Configure CLE for normal mode operation
/// Normal mode reads operands from data_read input
///
/// Config loading is a two-stage process:
/// 1. First clock: config_next → config_shadow
/// 2. Second clock (with morph_trigger): config_shadow → config_active
async fn setup_normal_mode(tb: &mut Testbench, opcode: u64) {
    tb.set("cle_mode", MODE_NORMAL)
        .set("config_next", opcode << 3);

    // Stage 1: Load config_next into config_shadow
    tb.clock(1).await;

    // Stage 2: Trigger morph to load config_shadow into config_active
    tb.set("morph_trigger", 1u64);
    tb.clock(1).await;
    tb.set("morph_trigger", 0u64);
}

/// Helper: Execute operation with given operands
async fn execute_op(tb: &mut Testbench, a: u32, b: u32) -> u32 {
    let data = ((b as u64) << 32) | (a as u64);
    tb.set("data_read", data)
        .set("data_read_valid", 1u64)
        .set("execute_enable", 1u64);

    // Debug: check immediate fu_result
    tb.step().await;
    let fu_imm: u32 = tb.get_as("debug_fu_result").await;
    println!("    L0L1 immediate fu_result: 0x{:08X}", fu_imm);

    tb.clock(4).await; // 4-stage pipeline
    let result: u64 = tb.get_as("data_write").await;
    (result & 0xFFFFFFFF) as u32
}

// =============================================================================
// Arithmetic Operations
// =============================================================================

#[tokio::test]
async fn test_l0_l1_add() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    // Debug: check function_sel before/after setup
    let func_sel_before: u8 = tb.get_as("function_sel").await;
    println!("DEBUG L0L1: function_sel BEFORE = {}", func_sel_before);

    setup_normal_mode(&mut tb, OP_ADD).await;

    let func_sel_after: u8 = tb.get_as("function_sel").await;
    println!(
        "DEBUG L0L1: function_sel AFTER = {} (expected {})",
        func_sel_after, OP_ADD
    );

    // Test cases: (a, b, expected)
    let tests = [
        (0u32, 0u32, 0u32),
        (1, 1, 2),
        (100, 50, 150),
        (0xFFFFFFFF, 1, 0), // Overflow wraps
        (0x7FFFFFFF, 1, 0x80000000),
        (123456, 654321, 777777),
    ];

    for (a, b, expected) in tests {
        let result = execute_op(&mut tb, a, b).await;
        assert_eq!(
            result, expected,
            "ADD: {} + {} = {}, got {}",
            a, b, expected, result
        );
    }
    println!("L0-L1 ADD: all tests passed");
}

#[tokio::test]
async fn test_l0_l1_sub() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    let func_sel_before: u8 = tb.get_as("function_sel").await;
    println!("DEBUG SUB: function_sel BEFORE = {}", func_sel_before);

    setup_normal_mode(&mut tb, OP_SUB).await;

    let func_sel_after: u8 = tb.get_as("function_sel").await;
    println!(
        "DEBUG SUB: function_sel AFTER = {} (expected {})",
        func_sel_after, OP_SUB
    );

    let tests = [
        (10u32, 5u32, 5u32),
        (100, 100, 0),
        (0, 1, 0xFFFFFFFF), // Underflow wraps
        (1000000, 1, 999999),
        (0x80000000, 1, 0x7FFFFFFF),
    ];

    for (a, b, expected) in tests {
        let result = execute_op(&mut tb, a, b).await;
        assert_eq!(
            result, expected,
            "SUB: {} - {} = {}, got {}",
            a, b, expected, result
        );
    }
    println!("L0-L1 SUB: all tests passed");
}

#[tokio::test]
async fn test_l0_l1_mul() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_MUL).await;

    let tests = [
        (0u32, 100u32, 0u32),
        (1, 12345, 12345),
        (2, 3, 6),
        (100, 100, 10000),
        (256, 256, 65536),
        (1000, 1000, 1000000),
    ];

    for (a, b, expected) in tests {
        let result = execute_op(&mut tb, a, b).await;
        assert_eq!(
            result, expected,
            "MUL: {} * {} = {}, got {}",
            a, b, expected, result
        );
    }
    println!("L0-L1 MUL: all tests passed");
}

// =============================================================================
// Logic Operations
// =============================================================================

#[tokio::test]
async fn test_l0_l1_and() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_AND).await;

    let tests = [
        (0xFFFFFFFFu32, 0xFFFFFFFFu32, 0xFFFFFFFFu32),
        (0xFFFFFFFF, 0x00000000, 0x00000000),
        (0xAAAAAAAA, 0x55555555, 0x00000000),
        (0xFF00FF00, 0x0F0F0F0F, 0x0F000F00),
        (0x12345678, 0xFF000000, 0x12000000),
    ];

    for (a, b, expected) in tests {
        let result = execute_op(&mut tb, a, b).await;
        assert_eq!(
            result, expected,
            "AND: 0x{:08X} & 0x{:08X} = 0x{:08X}, got 0x{:08X}",
            a, b, expected, result
        );
    }
    println!("L0-L1 AND: all tests passed");
}

#[tokio::test]
async fn test_l0_l1_or() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_OR).await;

    let tests = [
        (0x00000000u32, 0x00000000u32, 0x00000000u32),
        (0xFFFFFFFF, 0x00000000, 0xFFFFFFFF),
        (0xAAAAAAAA, 0x55555555, 0xFFFFFFFF),
        (0xFF000000, 0x00FF0000, 0xFFFF0000),
        (0x12340000, 0x00005678, 0x12345678),
    ];

    for (a, b, expected) in tests {
        let result = execute_op(&mut tb, a, b).await;
        assert_eq!(
            result, expected,
            "OR: 0x{:08X} | 0x{:08X} = 0x{:08X}, got 0x{:08X}",
            a, b, expected, result
        );
    }
    println!("L0-L1 OR: all tests passed");
}

#[tokio::test]
async fn test_l0_l1_xor() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_XOR).await;

    let tests = [
        (0x00000000u32, 0x00000000u32, 0x00000000u32),
        (0xFFFFFFFF, 0x00000000, 0xFFFFFFFF),
        (0xFFFFFFFF, 0xFFFFFFFF, 0x00000000),
        (0xAAAAAAAA, 0x55555555, 0xFFFFFFFF),
        (0x12345678, 0x12345678, 0x00000000),
    ];

    for (a, b, expected) in tests {
        let result = execute_op(&mut tb, a, b).await;
        assert_eq!(
            result, expected,
            "XOR: 0x{:08X} ^ 0x{:08X} = 0x{:08X}, got 0x{:08X}",
            a, b, expected, result
        );
    }
    println!("L0-L1 XOR: all tests passed");
}

#[tokio::test]
async fn test_l0_l1_not() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_NOT).await;

    let tests = [
        (0x00000000u32, 0xFFFFFFFFu32),
        (0xFFFFFFFF, 0x00000000),
        (0xAAAAAAAA, 0x55555555),
        (0x0F0F0F0F, 0xF0F0F0F0),
        (0x12345678, 0xEDCBA987),
    ];

    for (a, expected) in tests {
        let result = execute_op(&mut tb, a, 0).await; // b ignored for NOT
        assert_eq!(
            result, expected,
            "NOT: ~0x{:08X} = 0x{:08X}, got 0x{:08X}",
            a, expected, result
        );
    }
    println!("L0-L1 NOT: all tests passed");
}

// =============================================================================
// Shift Operations
// =============================================================================

#[tokio::test]
async fn test_l0_l1_sll() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_SLL).await;

    let tests = [
        (1u32, 0u32, 1u32),
        (1, 1, 2),
        (1, 4, 16),
        (1, 31, 0x80000000),
        (0xFF, 8, 0xFF00),
        (0x12345678, 4, 0x23456780),
    ];

    for (a, shift, expected) in tests {
        let result = execute_op(&mut tb, a, shift).await;
        assert_eq!(
            result, expected,
            "SLL: 0x{:08X} << {} = 0x{:08X}, got 0x{:08X}",
            a, shift, expected, result
        );
    }
    println!("L0-L1 SLL: all tests passed");
}

#[tokio::test]
async fn test_l0_l1_srl() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_SRL).await;

    let tests = [
        (1u32, 0u32, 1u32),
        (2, 1, 1),
        (16, 4, 1),
        (0x80000000, 31, 1),
        (0xFF00, 8, 0xFF),
        (0x12345678, 4, 0x01234567),
    ];

    for (a, shift, expected) in tests {
        let result = execute_op(&mut tb, a, shift).await;
        assert_eq!(
            result, expected,
            "SRL: 0x{:08X} >> {} = 0x{:08X}, got 0x{:08X}",
            a, shift, expected, result
        );
    }
    println!("L0-L1 SRL: all tests passed");
}

#[tokio::test]
async fn test_l0_l1_sra() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_SRA).await;

    // Arithmetic right shift preserves sign bit
    let tests = [
        (0x80000000u32, 1u32, 0xC0000000u32), // Sign extension
        (0x80000000, 4, 0xF8000000),
        (0x80000000, 31, 0xFFFFFFFF),
        (0x7FFFFFFF, 1, 0x3FFFFFFF), // Positive stays positive
        (0x40000000, 4, 0x04000000),
    ];

    for (a, shift, expected) in tests {
        let result = execute_op(&mut tb, a, shift).await;
        assert_eq!(
            result, expected,
            "SRA: 0x{:08X} >>> {} = 0x{:08X}, got 0x{:08X}",
            a, shift, expected, result
        );
    }
    println!("L0-L1 SRA: all tests passed");
}

// =============================================================================
// Comparison Operations
// =============================================================================

#[tokio::test]
async fn test_l0_l1_eq() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_EQ).await;

    let tests = [
        (0u32, 0u32, 1u32),
        (100, 100, 1),
        (100, 101, 0),
        (0xFFFFFFFF, 0xFFFFFFFF, 1),
        (0, 1, 0),
    ];

    for (a, b, expected) in tests {
        let result = execute_op(&mut tb, a, b).await;
        assert_eq!(
            result, expected,
            "EQ: {} == {} = {}, got {}",
            a, b, expected, result
        );
    }
    println!("L0-L1 EQ: all tests passed");
}

#[tokio::test]
async fn test_l0_l1_ne() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_NE).await;

    let tests = [
        (0u32, 0u32, 0u32),
        (100, 100, 0),
        (100, 101, 1),
        (0xFFFFFFFF, 0xFFFFFFFF, 0),
        (0, 1, 1),
    ];

    for (a, b, expected) in tests {
        let result = execute_op(&mut tb, a, b).await;
        assert_eq!(
            result, expected,
            "NE: {} != {} = {}, got {}",
            a, b, expected, result
        );
    }
    println!("L0-L1 NE: all tests passed");
}

#[tokio::test]
async fn test_l0_l1_lt() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_LT).await;

    // Signed comparison
    let tests = [
        (0u32, 1u32, 1u32),
        (1, 0, 0),
        (100, 100, 0),
        (0xFFFFFFFF, 0, 1),          // -1 < 0 (signed)
        (0x7FFFFFFF, 0x80000000, 0), // MAX_INT > MIN_INT (signed)
    ];

    for (a, b, expected) in tests {
        let result = execute_op(&mut tb, a, b).await;
        assert_eq!(
            result, expected,
            "LT (signed): {} < {} = {}, got {}",
            a as i32, b as i32, expected, result
        );
    }
    println!("L0-L1 LT: all tests passed");
}

#[tokio::test]
async fn test_l0_l1_ge() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_GE).await;

    let tests = [
        (1u32, 0u32, 1u32),
        (0, 1, 0),
        (100, 100, 1),
        (0, 0xFFFFFFFF, 1),          // 0 >= -1 (signed)
        (0x80000000, 0x7FFFFFFF, 0), // MIN_INT < MAX_INT (signed)
    ];

    for (a, b, expected) in tests {
        let result = execute_op(&mut tb, a, b).await;
        assert_eq!(
            result, expected,
            "GE (signed): {} >= {} = {}, got {}",
            a as i32, b as i32, expected, result
        );
    }
    println!("L0-L1 GE: all tests passed");
}

#[tokio::test]
async fn test_l0_l1_ltu() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_LTU).await;

    // Unsigned comparison
    let tests = [
        (0u32, 1u32, 1u32),
        (1, 0, 0),
        (100, 100, 0),
        (0, 0xFFFFFFFF, 1),          // 0 < MAX_UINT (unsigned)
        (0x7FFFFFFF, 0x80000000, 1), // Unsigned: 0x7FFF... < 0x8000...
    ];

    for (a, b, expected) in tests {
        let result = execute_op(&mut tb, a, b).await;
        assert_eq!(
            result, expected,
            "LTU (unsigned): {} < {} = {}, got {}",
            a, b, expected, result
        );
    }
    println!("L0-L1 LTU: all tests passed");
}

#[tokio::test]
async fn test_l0_l1_geu() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_GEU).await;

    let tests = [
        (1u32, 0u32, 1u32),
        (0, 1, 0),
        (100, 100, 1),
        (0xFFFFFFFF, 0, 1),          // MAX_UINT >= 0 (unsigned)
        (0x80000000, 0x7FFFFFFF, 1), // Unsigned: 0x8000... > 0x7FFF...
    ];

    for (a, b, expected) in tests {
        let result = execute_op(&mut tb, a, b).await;
        assert_eq!(
            result, expected,
            "GEU (unsigned): {} >= {} = {}, got {}",
            a, b, expected, result
        );
    }
    println!("L0-L1 GEU: all tests passed");
}

// =============================================================================
// Min/Max Operations
// =============================================================================

#[tokio::test]
async fn test_l0_l1_min() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    setup_normal_mode(&mut tb, OP_MIN).await;

    let tests = [
        (0u32, 0u32, 0u32),
        (10, 20, 10),
        (20, 10, 10),
        (100, 100, 100),
        (0, 0xFFFFFFFF, 0xFFFFFFFF), // Signed: -1 < 0
    ];

    for (a, b, expected) in tests {
        let result = execute_op(&mut tb, a, b).await;
        assert_eq!(
            result, expected,
            "MIN: min({}, {}) = {}, got {}",
            a, b, expected, result
        );
    }
    println!("L0-L1 MIN: all tests passed");
}

#[tokio::test]
async fn test_l0_l1_max() {
    if !source_available() {
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    let func_sel_before: u8 = tb.get_as("function_sel").await;
    println!("DEBUG MAX: function_sel BEFORE = {}", func_sel_before);

    setup_normal_mode(&mut tb, OP_MAX).await;

    let func_sel_after: u8 = tb.get_as("function_sel").await;
    println!(
        "DEBUG MAX: function_sel AFTER = {} (expected {})",
        func_sel_after, OP_MAX
    );

    let tests = [
        (0u32, 0u32, 0u32),
        (10, 20, 20),
        (20, 10, 20),
        (100, 100, 100),
        (0, 0xFFFFFFFF, 0), // Signed: 0 > -1
    ];

    for (a, b, expected) in tests {
        let result = execute_op(&mut tb, a, b).await;
        assert_eq!(
            result, expected,
            "MAX: max({}, {}) = {}, got {}",
            a, b, expected, result
        );
    }
    println!("L0-L1 MAX: all tests passed");
}
