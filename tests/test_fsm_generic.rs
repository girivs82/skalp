//! Generic FSM Simulation Tests
//!
//! Tests for skalp compiler/simulator functionality including:
//! - State machine transitions with match statements
//! - Boolean NOT operations (BUG #117r fix verification)
//! - Nested if-else in match arms (mux conditions)
//! - Fault latching behavior
//! - Counter comparisons

use skalp_testing::testbench::*;

const FSM_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/examples/test_fsm_generic.sk");
const TOP_MODULE: &str = "GenericFsm";

/// Test basic initialization and reset
#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS"
)]
async fn test_fsm_init() {
    let mut tb = Testbench::behavioral_with_top(FSM_PATH, TOP_MODULE).await.unwrap();

    // Apply reset
    tb.set("rst", 1u8);
    tb.clock(5).await;

    // Release reset
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Verify initial state (should be Idle = 0)
    let state: u8 = tb.get_as("state").await;
    assert_eq!(state, 0, "Should be in Idle state after reset");

    // Verify outputs are inactive
    let active: u8 = tb.get_as("active_out").await;
    let done: u8 = tb.get_as("done_out").await;
    let error: u8 = tb.get_as("error_out").await;
    assert_eq!(active, 0, "active_out should be 0 in Idle");
    assert_eq!(done, 0, "done_out should be 0 in Idle");
    assert_eq!(error, 0, "error_out should be 0 in Idle");

    println!("✓ FSM initialization test passed");
}

/// Test NOT operation - verifies BUG #117r fix
/// The NOT operation should correctly invert boolean values
#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS"
)]
async fn test_not_operation() {
    let mut tb = Testbench::behavioral_with_top(FSM_PATH, TOP_MODULE).await.unwrap();

    // Reset
    tb.set("rst", 1u8);
    tb.clock(5).await;
    tb.set("rst", 0u8);

    // Test NOT when enable=0: not_enable should be 1
    tb.set("enable", 0u8);
    tb.clock(1).await;
    let not_en: u8 = tb.get_as("not_enable").await;
    assert_eq!(not_en, 1, "NOT(0) should be 1");

    // Test NOT when enable=1: not_enable should be 0
    tb.set("enable", 1u8);
    tb.clock(1).await;
    let not_en: u8 = tb.get_as("not_enable").await;
    assert_eq!(not_en, 0, "NOT(1) should be 0");

    // Test again to ensure consistency
    tb.set("enable", 0u8);
    tb.clock(1).await;
    let not_en: u8 = tb.get_as("not_enable").await;
    assert_eq!(not_en, 1, "NOT(0) should still be 1");

    println!("✓ NOT operation test passed (BUG #117r verification)");
}

/// Test state machine transition from Idle to WaitReady
/// This tests match statements with NOT operation in condition
#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS"
)]
async fn test_state_transition_idle_to_waitready() {
    let mut tb = Testbench::behavioral_with_top(FSM_PATH, TOP_MODULE).await.unwrap();

    // Reset
    tb.set("rst", 1u8);
    tb.clock(5).await;
    tb.set("rst", 0u8);

    // Clear all fault sources
    tb.set("error_input", 0u8);
    tb.set("ready", 0u8);
    tb.set("data_valid", 0u8);
    tb.clock(1).await;

    // Verify in Idle state
    let state: u8 = tb.get_as("state").await;
    assert_eq!(state, 0, "Should be in Idle state initially");

    // Enable the FSM - should transition to WaitReady
    // This tests the condition: enable && !any_fault
    tb.set("enable", 1u8);

    // Debug: check signals before clock
    let en_before: u8 = tb.get_as("enable").await;
    let fault_a_before: u8 = tb.get_as("faults.error_a").await;
    let fault_b_before: u8 = tb.get_as("faults.error_b").await;
    let fault_t_before: u8 = tb.get_as("faults.timeout").await;
    let state_before: u8 = tb.get_as("state").await;
    println!("Before clock: enable={}, fault_a={}, fault_b={}, fault_t={}, state={}",
        en_before, fault_a_before, fault_b_before, fault_t_before, state_before);

    tb.clock(1).await;

    // Debug: check signals after clock
    let en_after: u8 = tb.get_as("enable").await;
    let state_after: u8 = tb.get_as("state").await;
    let not_en: u8 = tb.get_as("not_enable").await;
    println!("After clock: enable={}, not_enable={}, state={}", en_after, not_en, state_after);

    let state: u8 = tb.get_as("state").await;
    assert_eq!(state, 1, "Should be in WaitReady state after enable");

    println!("✓ State transition Idle->WaitReady test passed");
}

/// Test full transition sequence: Idle -> WaitReady -> Active -> Done
#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS"
)]
async fn test_full_transition_sequence() {
    let mut tb = Testbench::behavioral_with_top(FSM_PATH, TOP_MODULE).await.unwrap();

    // Reset
    tb.set("rst", 1u8);
    tb.clock(5).await;
    tb.set("rst", 0u8);

    // Clear all inputs
    tb.set("error_input", 0u8);
    tb.set("ready", 0u8);
    tb.set("data_valid", 0u8);
    tb.set("enable", 0u8);
    tb.clock(1).await;

    // Step 1: Enable -> Idle to WaitReady
    tb.set("enable", 1u8);
    tb.clock(1).await;
    let state: u8 = tb.get_as("state").await;
    assert_eq!(state, 1, "Should be in WaitReady (1)");

    // Step 2: Set ready -> WaitReady to Active
    // Tests nested if-else: if any_fault { Error } else if ready { if !error_input { Active } }
    tb.set("ready", 1u8);
    tb.clock(1).await;
    let state: u8 = tb.get_as("state").await;
    assert_eq!(state, 2, "Should be in Active (2)");
    let active: u8 = tb.get_as("active_out").await;
    assert_eq!(active, 1, "active_out should be 1 in Active state");

    // Step 3: Set data_valid -> Active to Done
    tb.set("data_valid", 1u8);
    tb.clock(1).await;
    let state: u8 = tb.get_as("state").await;
    assert_eq!(state, 3, "Should be in Done (3)");
    let done: u8 = tb.get_as("done_out").await;
    assert_eq!(done, 1, "done_out should be 1 in Done state");

    // Step 4: Clear enable -> Done to Idle
    // Tests condition: !enable
    tb.set("enable", 0u8);
    tb.clock(1).await;
    let state: u8 = tb.get_as("state").await;
    assert_eq!(state, 0, "Should be back in Idle (0) after clearing enable");

    println!("✓ Full transition sequence test passed");
}

/// Test fault detection and latching
#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS"
)]
async fn test_fault_detection() {
    let mut tb = Testbench::behavioral_with_top(FSM_PATH, TOP_MODULE).await.unwrap();

    // Reset
    tb.set("rst", 1u8);
    tb.clock(5).await;
    tb.set("rst", 0u8);

    // Get to Active state first
    tb.set("error_input", 0u8);
    tb.set("enable", 1u8);
    tb.clock(1).await;
    tb.set("ready", 1u8);
    tb.clock(1).await;

    let state: u8 = tb.get_as("state").await;
    assert_eq!(state, 2, "Should be in Active state before fault");

    // Trigger error_input fault
    tb.set("error_input", 1u8);
    tb.clock(1).await;

    // Fault should be latched
    let fault_a: u8 = tb.get_as("faults.error_a").await;
    assert_eq!(fault_a, 1, "Fault error_a should be latched");

    // Next cycle should transition to Error due to any_fault
    tb.clock(1).await;
    let state: u8 = tb.get_as("state").await;
    assert_eq!(state, 4, "Should be in Error state (4) after fault");

    let error_out: u8 = tb.get_as("error_out").await;
    assert_eq!(error_out, 1, "error_out should be 1 in Error state");

    // Fault should remain latched even if error_input is cleared
    tb.set("error_input", 0u8);
    tb.clock(10).await;
    let fault_a: u8 = tb.get_as("faults.error_a").await;
    assert_eq!(fault_a, 1, "Fault should remain latched until reset");

    // Reset should clear the fault
    tb.set("rst", 1u8);
    tb.clock(1).await;
    tb.set("rst", 0u8);
    tb.clock(1).await;
    let fault_a: u8 = tb.get_as("faults.error_a").await;
    assert_eq!(fault_a, 0, "Fault should be cleared after reset");

    println!("✓ Fault detection test passed");
}

/// Test that fault blocks state transition from Idle
/// When any_fault is true, enable should NOT cause transition
#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS"
)]
async fn test_fault_blocks_transition() {
    let mut tb = Testbench::behavioral_with_top(FSM_PATH, TOP_MODULE).await.unwrap();

    // Reset
    tb.set("rst", 1u8);
    tb.clock(5).await;
    tb.set("rst", 0u8);

    // First, latch a fault while in Idle
    tb.set("error_input", 1u8);
    tb.clock(1).await;

    // Verify fault is latched
    let fault_a: u8 = tb.get_as("faults.error_a").await;
    assert_eq!(fault_a, 1, "Fault should be latched");

    // Now try to enable - should NOT transition due to !any_fault being false
    tb.set("enable", 1u8);
    tb.set("error_input", 0u8); // Clear input but fault stays latched
    tb.clock(5).await;

    // Should still be in Idle because any_fault is true
    let state: u8 = tb.get_as("state").await;
    assert_eq!(state, 0, "Should remain in Idle when fault is latched");

    println!("✓ Fault blocks transition test passed");
}

/// Test hierarchical signal access (struct fields via name registry)
#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS"
)]
async fn test_hierarchical_signal_access() {
    let mut tb = Testbench::behavioral_with_top(FSM_PATH, TOP_MODULE).await.unwrap();

    // Reset
    tb.set("rst", 1u8);
    tb.clock(5).await;
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Access struct fields using hierarchical paths
    let fault_a: u8 = tb.get_as("faults.error_a").await;
    let fault_b: u8 = tb.get_as("faults.error_b").await;
    let fault_timeout: u8 = tb.get_as("faults.timeout").await;

    assert_eq!(fault_a, 0, "faults.error_a should be 0 after reset");
    assert_eq!(fault_b, 0, "faults.error_b should be 0 after reset");
    assert_eq!(fault_timeout, 0, "faults.timeout should be 0 after reset");

    // Trigger a fault and verify hierarchical access still works
    tb.set("error_input", 1u8);
    tb.clock(1).await;

    let fault_a: u8 = tb.get_as("faults.error_a").await;
    assert_eq!(fault_a, 1, "faults.error_a should be 1 after error_input");

    println!("✓ Hierarchical signal access test passed");
}

/// Test counter comparison
#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS"
)]
async fn test_counter_comparison() {
    let mut tb = Testbench::behavioral_with_top(FSM_PATH, TOP_MODULE).await.unwrap();

    // Reset
    tb.set("rst", 1u8);
    tb.clock(5).await;
    tb.set("rst", 0u8);

    // Set threshold to a specific value
    tb.set("counter_threshold", 10u8);

    // Run until counter matches threshold (counter starts at 0, increments each cycle)
    // After reset, counter is 0, so it needs 10 cycles to reach 10
    tb.clock(10).await;

    // Check counter_match output
    let match_out: u8 = tb.get_as("counter_match").await;
    assert_eq!(match_out, 1, "counter_match should be 1 when counter equals threshold");

    // Run one more cycle - counter will be 11, should no longer match
    tb.clock(1).await;
    let match_out: u8 = tb.get_as("counter_match").await;
    assert_eq!(match_out, 0, "counter_match should be 0 when counter != threshold");

    println!("✓ Counter comparison test passed");
}
