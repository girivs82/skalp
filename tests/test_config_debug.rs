//! Minimal test to debug config flow

#![cfg(not(debug_assertions))]

use skalp_testing::Testbench;

const CLE_V2_SYNC_SOURCE: &str = "/Users/girivs/src/hw/karythra/rtl/skalp/cle/src/main_v2_sync.sk";
const STDLIB_PATH: &str = "/Users/girivs/src/hw/hls/crates/skalp-stdlib";
const TOP_MODULE: &str = "KarythraCLE_V2_Sync";

#[tokio::test]
async fn test_config_debug() {
    std::env::set_var("SKALP_STDLIB_PATH", STDLIB_PATH);

    let mut tb = Testbench::with_top_module(CLE_V2_SYNC_SOURCE, TOP_MODULE)
        .await
        .expect("Failed to create testbench");

    // Test specific opcode 8 (first failing case)
    let opcode = 8u64;
    let config_value = opcode << 3; // 64 = 0b01000000
    println!(
        "Testing opcode {}: config_next = {} (0b{:09b})",
        opcode, config_value, config_value
    );

    // Initialize
    tb.set("cle_mode", 0u64)
        .set("config_next", config_value)
        .set("morph_trigger", 0u64);

    // Read initial state
    println!("Before any clocks:");
    let func_sel: u8 = tb.get_as("function_sel").await;
    println!("  function_sel = {}", func_sel);

    // Stage 1: Load config_next into config_shadow
    println!("\nAfter clock 1 (config_next -> config_shadow):");
    tb.clock(1).await;
    let func_sel: u8 = tb.get_as("function_sel").await;
    println!("  function_sel = {}", func_sel);

    // Stage 2: Trigger morph
    println!("\nAfter clock 2 with morph_trigger=1 (config_shadow -> config_active):");
    tb.set("morph_trigger", 1u64);
    tb.clock(1).await;
    let func_sel: u8 = tb.get_as("function_sel").await;
    println!("  function_sel = {} (expected {})", func_sel, opcode);

    // Clear morph trigger
    tb.set("morph_trigger", 0u64);

    // Additional clock to verify stability
    println!("\nAfter clock 3 with morph_trigger=0:");
    tb.clock(1).await;
    let func_sel: u8 = tb.get_as("function_sel").await;
    println!("  function_sel = {} (expected {})", func_sel, opcode);

    // Test expected value
    assert_eq!(func_sel, opcode as u8, "function_sel mismatch");
}
