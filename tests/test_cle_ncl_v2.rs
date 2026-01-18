//! NCL simulation test for CLE v2 async
//!
//! Tests that NCL (Null Convention Logic) gate-level simulation works
//! on the CLE v2 async design with encoding at async entity boundaries.

use skalp_testing::Testbench;

const CLE_V2_ASYNC_SOURCE: &str =
    "/Users/girivs/src/hw/karythra/rtl/skalp/cle/src/main_v2_async.sk";
const TOP_MODULE: &str = "KarythraCLE_V2_Async";

#[tokio::test]
async fn test_cle_v2_async_ncl_creation() {
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        "/Users/girivs/src/hw/hls/crates/skalp-stdlib",
    );

    println!("=== CLE v2 Async NCL Simulation Test ===\n");
    println!("Creating NCL testbench for {}...", TOP_MODULE);

    let tb = Testbench::ncl_with_top_module(CLE_V2_ASYNC_SOURCE, TOP_MODULE)
        .await
        .expect("NCL testbench creation should succeed");

    println!("\n✅ SUCCESS: NCL testbench created!");

    let signals = tb.ncl_signal_names();
    println!("NCL signals: {}", signals.len());

    // Should have some NCL signals
    assert!(!signals.is_empty(), "Should have NCL signals");

    // Show some signal info
    println!("\nSample NCL signals (first 10):");
    for sig in signals.iter().take(10) {
        println!("  - {}", sig);
    }
}
