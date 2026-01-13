//! CLE V2 Asynchronous (NCL) Implementation Tests
//!
//! Tests the NCL (Null Convention Logic) version of the CLE:
//! - Dual-rail encoding verification
//! - Completion detection
//! - Three operating modes (Normal, Direct, Systolic)
//! - Handshaking protocol
//!
//! Reference: docs/GOLDEN/CLE_V2_ASYNC.md

#![cfg(not(debug_assertions))]

use skalp_testing::Testbench;
use std::path::Path;

const CLE_V2_ASYNC_SOURCE: &str =
    "/Users/girivs/src/hw/karythra/rtl/skalp/cle/src/main_v2_async.sk";
const STDLIB_PATH: &str = "/Users/girivs/src/hw/hls/crates/skalp-stdlib";

// Operating mode constants (must match main_v2_async.sk)
const MODE_NORMAL: u8 = 0b00;
const MODE_DIRECT: u8 = 0b01;
const MODE_SYSTOLIC: u8 = 0b10;

// Function select constants (L0-L1 opcodes)
const FUNC_ADD: u64 = 0;
const FUNC_SUB: u64 = 1;
const FUNC_AND: u64 = 4;
const FUNC_OR: u64 = 5;
const FUNC_XOR: u64 = 6;

/// Helper to check if source exists
fn source_available() -> bool {
    std::env::set_var("SKALP_STDLIB_PATH", STDLIB_PATH);
    Path::new(CLE_V2_ASYNC_SOURCE).exists()
}

// Top module entity name in the source file
const TOP_MODULE: &str = "KarythraCLE_V2_Async";

/// Helper: encode single-rail value to dual-rail (true_rail, false_rail)
fn encode_dual_rail(value: u64, width: usize) -> (u64, u64) {
    let mask = (1u64 << width) - 1;
    let v = value & mask;
    (v, !v & mask) // true_rail = value, false_rail = complement
}

/// Helper: encode to NULL state (both rails 0)
fn encode_null(width: usize) -> (u64, u64) {
    let _ = width;
    (0, 0)
}

/// Test: Basic compilation
#[tokio::test]
async fn test_cle_v2_async_compiles() {
    if !source_available() {
        println!("Skipping: CLE V2 Async source not found");
        return;
    }

    let result = Testbench::with_top_module(CLE_V2_ASYNC_SOURCE, TOP_MODULE).await;
    assert!(
        result.is_ok(),
        "CLE V2 Async should compile: {:?}",
        result.err()
    );
    println!("CLE V2 Async compiled successfully");
}

/// Test: Dual-rail encoding - verify NULL state
#[tokio::test]
async fn test_cle_v2_async_null_state() {
    if !source_available() {
        println!("Skipping: CLE V2 Async source not found");
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_ASYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    // Set all inputs to NULL state (both rails = 0)
    let (mode_t, mode_f) = encode_null(2);
    tb.set("cle_mode_t", mode_t).set("cle_mode_f", mode_f);

    let (config_t, config_f) = encode_null(9);
    tb.set("config_t", config_t).set("config_f", config_f);

    let (data_t, data_f) = encode_null(64);
    tb.set("data_read_t", data_t).set("data_read_f", data_f);

    tb.set("rst", 0u64);

    // Step simulation
    tb.step().await;

    // In NULL state, pipeline_complete should be 0 (no valid data)
    let complete: u64 = tb.get_as("pipeline_complete").await;
    assert_eq!(complete, 0, "Pipeline should not be complete in NULL state");

    println!("  NULL state verification: PASS");
}

/// Test: Direct mode - ADD operation with dual-rail encoding
#[tokio::test]
async fn test_cle_v2_async_direct_mode_add() {
    if !source_available() {
        println!("Skipping: CLE V2 Async source not found");
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_ASYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    // Configure for Direct mode
    let (mode_t, mode_f) = encode_dual_rail(MODE_DIRECT as u64, 2);
    tb.set("cle_mode_t", mode_t).set("cle_mode_f", mode_f);

    // Configure for ADD operation
    let config_val = FUNC_ADD << 3; // Function in upper bits
    let (config_t, config_f) = encode_dual_rail(config_val, 9);
    tb.set("config_t", config_t).set("config_f", config_f);

    // Enable execution
    let (exec_t, exec_f) = encode_dual_rail(1, 1);
    tb.set("execute_enable_t", exec_t)
        .set("execute_enable_f", exec_f);

    // Provide operands (pack into 64-bit: {b[31:0], a[31:0]})
    let a: u32 = 100;
    let b: u32 = 50;
    let data = ((b as u64) << 32) | (a as u64);
    let (data_t, data_f) = encode_dual_rail(data, 64);
    tb.set("data_read_t", data_t).set("data_read_f", data_f);

    // Valid data indicator
    let (valid_t, valid_f) = encode_dual_rail(1, 1);
    tb.set("data_read_valid_t", valid_t)
        .set("data_read_valid_f", valid_f);

    tb.set("rst", 0u64);

    // Step to let combinational logic settle
    tb.step().await;

    // Check completion detection
    let complete: u64 = tb.get_as("pipeline_complete").await;
    println!("  Pipeline complete: {}", complete);

    // Check result (decode from true rail)
    let result_t: u64 = tb.get_as("data_write_t").await;
    let expected = (a + b) as u64;
    assert_eq!(
        result_t & 0xFFFFFFFF,
        expected,
        "Direct mode ADD: {} + {} = {}, got {}",
        a,
        b,
        expected,
        result_t & 0xFFFFFFFF
    );

    println!("  Direct mode ADD: {} + {} = {} PASS", a, b, expected);
}

/// Test: Systolic mode - data forwarding
#[tokio::test]
async fn test_cle_v2_async_systolic_forwarding() {
    if !source_available() {
        println!("Skipping: CLE V2 Async source not found");
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_ASYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    // Configure for Systolic mode
    let (mode_t, mode_f) = encode_dual_rail(MODE_SYSTOLIC as u64, 2);
    tb.set("cle_mode_t", mode_t).set("cle_mode_f", mode_f);

    // Set up systolic inputs
    let west_val: u32 = 0xABCD1234;
    let north_val: u32 = 0x5678EFAB;

    let (west_t, west_f) = encode_dual_rail(west_val as u64, 32);
    let (north_t, north_f) = encode_dual_rail(north_val as u64, 32);

    tb.set("sys_in_w_t", west_t).set("sys_in_w_f", west_f);
    tb.set("sys_in_n_t", north_t).set("sys_in_n_f", north_f);

    // Configure for ADD operation
    let config_val = FUNC_ADD << 3;
    let (config_t, config_f) = encode_dual_rail(config_val, 9);
    tb.set("config_t", config_t).set("config_f", config_f);

    // Enable execution
    let (exec_t, exec_f) = encode_dual_rail(1, 1);
    tb.set("execute_enable_t", exec_t)
        .set("execute_enable_f", exec_f);

    tb.set("rst", 0u64);

    // Step simulation
    tb.step().await;

    // Check that west input is forwarded to east output
    let east_out_t: u64 = tb.get_as("sys_out_e_t").await;
    assert_eq!(
        east_out_t, west_val as u64,
        "Systolic forward W->E: expected 0x{:08X}, got 0x{:08X}",
        west_val, east_out_t
    );

    // Check that north input is forwarded to south output
    let south_out_t: u64 = tb.get_as("sys_out_s_t").await;
    assert_eq!(
        south_out_t, north_val as u64,
        "Systolic forward N->S: expected 0x{:08X}, got 0x{:08X}",
        north_val, south_out_t
    );

    println!(
        "  Systolic forwarding: W(0x{:08X})->E, N(0x{:08X})->S PASS",
        west_val, north_val
    );
}

/// Test: Mode switching
#[tokio::test]
async fn test_cle_v2_async_mode_switching() {
    if !source_available() {
        println!("Skipping: CLE V2 Async source not found");
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_ASYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    tb.set("rst", 0u64);

    // Test Normal mode
    let (mode_t, mode_f) = encode_dual_rail(MODE_NORMAL as u64, 2);
    tb.set("cle_mode_t", mode_t).set("cle_mode_f", mode_f);
    tb.step().await;
    let mode: u64 = tb.get_as("current_mode").await;
    assert_eq!(mode, MODE_NORMAL as u64, "Should be in Normal mode");
    println!("  Normal mode (0b00): PASS");

    // Test Direct mode
    let (mode_t, mode_f) = encode_dual_rail(MODE_DIRECT as u64, 2);
    tb.set("cle_mode_t", mode_t).set("cle_mode_f", mode_f);
    tb.step().await;
    let mode: u64 = tb.get_as("current_mode").await;
    assert_eq!(mode, MODE_DIRECT as u64, "Should be in Direct mode");
    println!("  Direct mode (0b01): PASS");

    // Test Systolic mode
    let (mode_t, mode_f) = encode_dual_rail(MODE_SYSTOLIC as u64, 2);
    tb.set("cle_mode_t", mode_t).set("cle_mode_f", mode_f);
    tb.step().await;
    let mode: u64 = tb.get_as("current_mode").await;
    assert_eq!(mode, MODE_SYSTOLIC as u64, "Should be in Systolic mode");
    println!("  Systolic mode (0b10): PASS");
}

/// Test: Fabric routing (control plane passthrough)
#[tokio::test]
async fn test_cle_v2_async_fabric_routing() {
    if !source_available() {
        println!("Skipping: CLE V2 Async source not found");
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_ASYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    // Set up control mode (Normal)
    let (mode_t, mode_f) = encode_dual_rail(MODE_NORMAL as u64, 2);
    tb.set("cle_mode_t", mode_t).set("cle_mode_f", mode_f);

    // Test fabric routing: N->S and W->E
    let north_val: u8 = 0xAB;
    let west_val: u8 = 0xCD;

    let (north_t, north_f) = encode_dual_rail(north_val as u64, 8);
    let (west_t, west_f) = encode_dual_rail(west_val as u64, 8);

    tb.set("fabric_in_n_t", north_t)
        .set("fabric_in_n_f", north_f);
    tb.set("fabric_in_w_t", west_t).set("fabric_in_w_f", west_f);

    tb.set("rst", 0u64);

    tb.step().await;

    // Check N->S routing
    let south_out_t: u64 = tb.get_as("fabric_out_s_t").await;
    assert_eq!(
        south_out_t, north_val as u64,
        "Fabric N->S: expected 0x{:02X}, got 0x{:02X}",
        north_val, south_out_t
    );

    // Check W->E routing
    let east_out_t: u64 = tb.get_as("fabric_out_e_t").await;
    assert_eq!(
        east_out_t, west_val as u64,
        "Fabric W->E: expected 0x{:02X}, got 0x{:02X}",
        west_val, east_out_t
    );

    println!(
        "  Fabric routing: N(0x{:02X})->S, W(0x{:02X})->E PASS",
        north_val, west_val
    );
}
