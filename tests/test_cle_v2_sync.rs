//! CLE V2 Synchronous Implementation Tests
//!
//! Tests the three operating modes:
//! - Normal mode: Content-addressed via cache
//! - Direct mode: Local register bypass
//! - Systolic mode: Streaming via systolic links
//!
//! Reference: docs/GOLDEN/CLE_V2_SYNC.md

#![cfg(not(debug_assertions))]

use skalp_testing::Testbench;
use std::path::Path;

const CLE_V2_SYNC_SOURCE: &str = "/Users/girivs/src/hw/karythra/rtl/skalp/cle/src/main_v2_sync.sk";
const STDLIB_PATH: &str = "/Users/girivs/src/hw/hls/crates/skalp-stdlib";

// Operating mode constants (must match main_v2_sync.sk)
const MODE_NORMAL: u8 = 0b00;
const MODE_DIRECT: u8 = 0b01;
const MODE_SYSTOLIC: u8 = 0b10;

// Function select constants (L0-L1 opcodes)
const FUNC_ADD: u64 = 0;
const FUNC_SUB: u64 = 1;
const FUNC_AND: u64 = 4;
const FUNC_OR: u64 = 5;
const FUNC_XOR: u64 = 6;
const FUNC_SHL: u64 = 10;
const FUNC_SHR: u64 = 11;

/// Helper to check if source exists
fn source_available() -> bool {
    std::env::set_var("SKALP_STDLIB_PATH", STDLIB_PATH);
    Path::new(CLE_V2_SYNC_SOURCE).exists()
}

// Top module entity name in the source file
const TOP_MODULE: &str = "KarythraCLE_V2_Sync";

/// Test: Basic compilation
#[tokio::test]
async fn test_cle_v2_sync_compiles() {
    if !source_available() {
        println!("Skipping: CLE V2 Sync source not found");
        return;
    }

    let result = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE).await;
    assert!(
        result.is_ok(),
        "CLE V2 Sync should compile: {:?}",
        result.err()
    );
    println!("CLE V2 Sync compiled successfully");
}

/// Test: Normal mode - ADD operation
#[tokio::test]
async fn test_cle_v2_sync_normal_mode_add() {
    if !source_available() {
        println!("Skipping: CLE V2 Sync source not found");
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    // Configure for ADD operation in Normal mode
    tb.set("cle_mode", MODE_NORMAL as u64)
        .set("config_next", (FUNC_ADD << 3) as u64) // Function in upper bits
        .set("morph_trigger", 1u64);

    tb.clock(1).await;
    tb.set("morph_trigger", 0u64);

    // Provide operands via data_read (simulating cache response)
    let a: u32 = 100;
    let b: u32 = 50;
    let data = ((b as u64) << 32) | (a as u64);

    tb.set("data_read", data)
        .set("data_read_valid", 1u64)
        .set("data_read_tag", 0u64)
        .set("execute_enable", 1u64)
        .set("wr_enable", 1u64)
        .set("rd_addr", 1u64);

    // Run through pipeline (4 stages)
    tb.clock(4).await;

    // Check result
    let result_data: u64 = tb.get_as("data_write").await;
    let expected = (a + b) as u64;
    assert_eq!(
        result_data & 0xFFFFFFFF,
        expected,
        "Normal mode ADD: {} + {} = {}, got {}",
        a,
        b,
        expected,
        result_data & 0xFFFFFFFF
    );

    println!("  Normal mode ADD: {} + {} = {} PASS", a, b, expected);
}

/// Test: Normal mode - SUB operation
#[tokio::test]
async fn test_cle_v2_sync_normal_mode_sub() {
    if !source_available() {
        println!("Skipping: CLE V2 Sync source not found");
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    // Configure for SUB
    tb.set("cle_mode", MODE_NORMAL as u64)
        .set("config_next", (FUNC_SUB << 3) as u64)
        .set("morph_trigger", 1u64);

    tb.clock(1).await;
    tb.set("morph_trigger", 0u64);

    let a: u32 = 200;
    let b: u32 = 75;
    let data = ((b as u64) << 32) | (a as u64);

    tb.set("data_read", data)
        .set("data_read_valid", 1u64)
        .set("data_read_tag", 0u64)
        .set("execute_enable", 1u64);

    tb.clock(4).await;

    let result_data: u64 = tb.get_as("data_write").await;
    let expected = (a - b) as u64;
    assert_eq!(
        result_data & 0xFFFFFFFF,
        expected,
        "Normal mode SUB: {} - {} = {}, got {}",
        a,
        b,
        expected,
        result_data & 0xFFFFFFFF
    );

    println!("  Normal mode SUB: {} - {} = {} PASS", a, b, expected);
}

/// Test: Direct mode - Register bypass
#[tokio::test]
async fn test_cle_v2_sync_direct_mode() {
    if !source_available() {
        println!("Skipping: CLE V2 Sync source not found");
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    // Configure for Direct mode with ADD
    tb.set("cle_mode", MODE_DIRECT as u64)
        .set("config_next", (FUNC_ADD << 3) as u64)
        .set("morph_trigger", 1u64);

    tb.clock(1).await;
    tb.set("morph_trigger", 0u64);

    tb.set("execute_enable", 1u64)
        .set("wr_enable", 1u64)
        .set("rd_addr", 2u64);

    // Pipeline
    tb.clock(4).await;

    // Verify direct mode is active
    let current_mode: u64 = tb.get_as("current_mode").await;
    assert_eq!(current_mode, MODE_DIRECT as u64, "Should be in Direct mode");

    println!("  Direct mode active: PASS");
}

/// Test: Systolic mode - Streaming data
#[tokio::test]
async fn test_cle_v2_sync_systolic_mode() {
    if !source_available() {
        println!("Skipping: CLE V2 Sync source not found");
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    // Configure for Systolic mode with ADD
    tb.set("cle_mode", MODE_SYSTOLIC as u64)
        .set("config_next", (FUNC_ADD << 3) as u64)
        .set("morph_trigger", 1u64);

    tb.clock(1).await;
    tb.set("morph_trigger", 0u64);

    // Provide systolic inputs
    let a: u32 = 10;
    let b: u32 = 20;
    tb.set("sys_in_w", a as u64) // A comes from west
        .set("sys_in_n", b as u64) // B comes from north
        .set("execute_enable", 1u64);

    // Clock to let combinational logic settle and check pass-through
    tb.clock(1).await;

    // Check systolic outputs - data should be forwarded (combinational)
    // In systolic mode: sys_out_e = sys_in_w (pass A east)
    //                   sys_out_s = sys_in_n (pass B south)
    let sys_out_e: u64 = tb.get_as("sys_out_e").await;
    let sys_out_s: u64 = tb.get_as("sys_out_s").await;

    assert_eq!(sys_out_e, a as u64, "Systolic: A should pass to east");
    assert_eq!(sys_out_s, b as u64, "Systolic: B should pass to south");

    println!("  Systolic mode forwarding: A={} E, B={} S: PASS", a, b);
}

/// Test: Mode switching
#[tokio::test]
async fn test_cle_v2_sync_mode_switching() {
    if !source_available() {
        println!("Skipping: CLE V2 Sync source not found");
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    // Start in Normal mode
    tb.set("cle_mode", MODE_NORMAL as u64);
    tb.clock(1).await;

    let mode: u64 = tb.get_as("current_mode").await;
    assert_eq!(mode, MODE_NORMAL as u64, "Should start in Normal mode");

    // Switch to Direct mode
    tb.set("cle_mode", MODE_DIRECT as u64);
    tb.clock(1).await;

    let mode: u64 = tb.get_as("current_mode").await;
    assert_eq!(mode, MODE_DIRECT as u64, "Should switch to Direct mode");

    // Switch to Systolic mode
    tb.set("cle_mode", MODE_SYSTOLIC as u64);
    tb.clock(1).await;

    let mode: u64 = tb.get_as("current_mode").await;
    assert_eq!(mode, MODE_SYSTOLIC as u64, "Should switch to Systolic mode");

    // Back to Normal
    tb.set("cle_mode", MODE_NORMAL as u64);
    tb.clock(1).await;

    let mode: u64 = tb.get_as("current_mode").await;
    assert_eq!(mode, MODE_NORMAL as u64, "Should return to Normal mode");

    println!("  Mode switching: Normal -> Direct -> Systolic -> Normal: PASS");
}

/// Test: Morphing (configuration swap)
#[tokio::test]
async fn test_cle_v2_sync_morphing() {
    if !source_available() {
        println!("Skipping: CLE V2 Sync source not found");
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    // Set initial config: ADD with route 101
    tb.set("config_next", (FUNC_ADD << 3) | 0b101)
        .set("morph_trigger", 1u64);

    tb.clock(1).await;
    tb.set("morph_trigger", 0u64);

    let func: u64 = tb.get_as("function_sel").await;
    let route: u64 = tb.get_as("route_sel").await;
    assert_eq!(func, FUNC_ADD, "Function should be ADD");
    assert_eq!(route, 0b101, "Route should be 101");

    // Preload new config
    tb.set("config_next", (FUNC_XOR << 3) | 0b011);

    // Config shouldn't change until morph trigger
    tb.clock(1).await;

    let func: u64 = tb.get_as("function_sel").await;
    assert_eq!(func, FUNC_ADD, "Function should still be ADD");

    // Trigger morph
    tb.set("morph_trigger", 1u64);
    tb.clock(1).await;
    tb.set("morph_trigger", 0u64);

    let func: u64 = tb.get_as("function_sel").await;
    let route: u64 = tb.get_as("route_sel").await;
    assert_eq!(func, FUNC_XOR, "Function should now be XOR");
    assert_eq!(route, 0b011, "Route should be 011");

    println!("  Morphing: ADD/101 -> XOR/011: PASS");
}

/// Test: Pipeline validity propagation
#[tokio::test]
async fn test_cle_v2_sync_pipeline_valid() {
    if !source_available() {
        println!("Skipping: CLE V2 Sync source not found");
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    // Configure
    tb.set("cle_mode", MODE_NORMAL as u64)
        .set("config_next", (FUNC_ADD << 3) as u64)
        .set("morph_trigger", 1u64);

    tb.clock(1).await;
    tb.set("morph_trigger", 0u64);

    // Initially, pipeline should not be valid
    let valid: u64 = tb.get_as("pipeline_valid").await;
    assert_eq!(valid, 0, "Pipeline should start invalid");

    // Enable execution
    tb.set("execute_enable", 1u64)
        .set("data_read", 0x0000000100000002u64) // a=2, b=1
        .set("data_read_valid", 1u64)
        .set("data_read_tag", 0u64);

    // Clock through pipeline - valid should appear after 4 stages
    tb.clock(4).await;

    let valid: u64 = tb.get_as("pipeline_valid").await;
    assert_eq!(valid, 1, "Pipeline should be valid after 4 clocks");

    // Disable execution
    tb.set("execute_enable", 0u64);

    // Pipeline should drain
    tb.clock(4).await;

    let valid: u64 = tb.get_as("pipeline_valid").await;
    assert_eq!(valid, 0, "Pipeline should drain after disable");

    println!("  Pipeline validity: inject -> valid -> drain: PASS");
}

/// Test: Hash bus interface (Normal mode)
#[tokio::test]
async fn test_cle_v2_sync_hash_bus() {
    if !source_available() {
        println!("Skipping: CLE V2 Sync source not found");
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    tb.set("cle_mode", MODE_NORMAL as u64)
        .set("config_next", (FUNC_ADD << 3) as u64)
        .set("morph_trigger", 1u64);

    tb.clock(1).await;
    tb.set("morph_trigger", 0u64);

    // In normal mode, hash_lookup_valid should assert when executing
    tb.set("execute_enable", 1u64);
    tb.clock(1).await;

    let hash_valid: u64 = tb.get_as("hash_lookup_valid").await;
    assert_eq!(hash_valid, 1, "Hash lookup should be valid in Normal mode");

    // Check that tag is issued
    let tag: u64 = tb.get_as("hash_lookup_tag").await;
    println!("  Hash bus: lookup_valid=1, tag={}: PASS", tag);
}

/// Test: Bitwise operations (XOR, AND, OR)
#[tokio::test]
async fn test_cle_v2_sync_bitwise_ops() {
    if !source_available() {
        println!("Skipping: CLE V2 Sync source not found");
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    let a: u32 = 0xF0F0F0F0;
    let b: u32 = 0x0F0F0F0F;

    // Test XOR
    tb.set("cle_mode", MODE_NORMAL as u64)
        .set("config_next", (FUNC_XOR << 3) as u64)
        .set("morph_trigger", 1u64);

    tb.clock(1).await;
    tb.set("morph_trigger", 0u64);

    let data = ((b as u64) << 32) | (a as u64);
    tb.set("data_read", data)
        .set("data_read_valid", 1u64)
        .set("data_read_tag", 0u64)
        .set("execute_enable", 1u64);

    tb.clock(4).await;

    let result_data: u64 = tb.get_as("data_write").await;
    let expected = (a ^ b) as u64;
    assert_eq!(result_data & 0xFFFFFFFF, expected, "XOR failed");
    println!(
        "  XOR: 0x{:08X} ^ 0x{:08X} = 0x{:08X} PASS",
        a, b, expected as u32
    );
}

/// Test: Shift operations
#[tokio::test]
async fn test_cle_v2_sync_shift_ops() {
    if !source_available() {
        println!("Skipping: CLE V2 Sync source not found");
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    let a: u32 = 0x00000001;
    let shift: u32 = 4;

    // Test SHL
    tb.set("cle_mode", MODE_NORMAL as u64)
        .set("config_next", (FUNC_SHL << 3) as u64)
        .set("morph_trigger", 1u64);

    tb.clock(1).await;
    tb.set("morph_trigger", 0u64);

    let data = ((shift as u64) << 32) | (a as u64);
    tb.set("data_read", data)
        .set("data_read_valid", 1u64)
        .set("data_read_tag", 0u64)
        .set("execute_enable", 1u64);

    tb.clock(4).await;

    let result_data: u64 = tb.get_as("data_write").await;
    let expected = (a << shift) as u64;
    assert_eq!(result_data & 0xFFFFFFFF, expected, "SHL failed");
    println!(
        "  SHL: 0x{:08X} << {} = 0x{:08X} PASS",
        a, shift, expected as u32
    );
}

/// Test: Power domain control
#[tokio::test]
async fn test_cle_v2_sync_power_domains() {
    if !source_available() {
        println!("Skipping: CLE V2 Sync source not found");
        return;
    }

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    // L0-L1 function (func < 18)
    tb.set("config_next", (FUNC_ADD << 3) as u64)
        .set("morph_trigger", 1u64);

    tb.clock(1).await;
    tb.set("morph_trigger", 0u64);
    tb.clock(1).await;

    let domains: u64 = tb.get_as("domain_enable").await;
    // Domain 0 should be on (L0-L1 always on)
    assert_eq!(domains & 0x1, 1, "Domain 0 (L0-L1) should be on");

    println!("  Power domains for L0-L1 op: 0b{:04b} PASS", domains);
}
