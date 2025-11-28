// Minimal test case demonstrating Bug #77
//
// Run with: cargo test --release
//
// Expected: ADD operation 10 + 20 = 30
// Actual: Returns 0
//
// Regression introduced between:
// - Working: acd67b8 "Fix Bug #74"
// - Broken: a304529 "Phase 0 Quick Wins" and all subsequent commits

use skalp_testing::testbench::*;

#[tokio::test]
#[ignore = "requires manual setup of /tmp/test_simple_add.sk"]
async fn test_simple_add() {
    let mut tb = Testbench::new("/tmp/test_simple_add.sk").await.unwrap();

    // Set inputs: a=10, b=20
    tb.set("a", 10u32);
    tb.set("b", 20u32);

    // Clock once to latch inputs
    tb.clock(1).await;

    // Clock again to get result
    tb.clock(1).await;

    // Read result
    let result: u32 = tb.get_as("result").await;

    println!("ADD: 10 + 20 = {} (expected 30)", result);

    // This assertion fails with Bug #77
    assert_eq!(result, 30, "ADD: 10 + 20 failed: got {}, expected 30", result);
}
