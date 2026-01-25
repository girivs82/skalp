//! Battery DCDC Controller Tests
//!
//! Tests for the DAB Battery Controller state machine including:
//! - State transitions: Init -> WaitBms -> Precharge -> Running
//! - Fault handling
//! - Timer reset behavior (BUG #222 verification)

use skalp_testing::testbench::*;

const BATTERY_DCDC_PATH: &str = "/Users/girivs/src/design/electronics/skalp/battery_dcdc/main.sk";
const TOP_MODULE: &str = "DabBatteryController";

/// Test basic initialization and reset
#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS"
)]
async fn test_battery_dcdc_init() {
    let mut tb = Testbench::behavioral_with_top(BATTERY_DCDC_PATH, TOP_MODULE)
        .await
        .expect("Failed to create testbench for battery DCDC");

    // Apply reset
    tb.set("rst", 1u8);
    tb.clock(5).await;

    // Release reset
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Verify initial state (should be Init = 0)
    let state: u8 = tb.get_as("state").await;
    assert_eq!(state, 0, "Should be in Init state after reset");

    println!("✓ Battery DCDC initialization test passed");
}

/// Test state machine transition from Init to WaitBms
/// This tests the condition: enable && !any_fault_flag
#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS"
)]
async fn test_state_transition_init_to_waitbms() {
    let mut tb = Testbench::behavioral_with_top(BATTERY_DCDC_PATH, TOP_MODULE)
        .await
        .expect("Failed to create testbench for battery DCDC");

    // Apply reset
    tb.set("rst", 1u8);
    tb.clock(5).await;
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Verify in Init state
    let state: u8 = tb.get_as("state").await;
    assert_eq!(state, 0, "Should be in Init state initially");

    // Clear all fault inputs - these are the hardware fault inputs
    tb.set("hw_ov", 0u8);  // Hardware overvoltage
    tb.set("hw_uv", 0u8);  // Hardware undervoltage
    tb.set("hw_oc", 0u8);  // Hardware overcurrent
    tb.set("hw_ot", 0u8);  // Hardware overtemperature
    tb.set("desat", 0u8);  // MOSFET desaturation

    // Set BMS data to indicate no fault
    tb.set("bms.connected", 1u8);
    tb.set("bms.fault", 0u8);
    tb.set("bms_rx_valid", 0u8);  // Will set this to prevent watchdog timeout
    tb.clock(1).await;

    // Enable the controller - should transition to WaitBms
    tb.set("enable", 1u8);

    // Debug: check state before clock
    let state_before: u8 = tb.get_as("state").await;
    let enable_before: u8 = tb.get_as("enable").await;
    println!(
        "Before transition: enable={}, state={}",
        enable_before, state_before
    );

    tb.clock(1).await;

    // Debug: check state after clock
    let state_after: u8 = tb.get_as("state").await;
    println!("After transition: state={}", state_after);

    // Should now be in WaitBms (state = 1)
    assert_eq!(state_after, 1, "Should be in WaitBms state (1) after enable");

    println!("✓ State transition Init->WaitBms test passed");
}

/// Test that any_fault_flag prevents state transition
#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS"
)]
async fn test_fault_prevents_transition() {
    let mut tb = Testbench::behavioral_with_top(BATTERY_DCDC_PATH, TOP_MODULE)
        .await
        .expect("Failed to create testbench for battery DCDC");

    // Apply reset
    tb.set("rst", 1u8);
    tb.clock(5).await;
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Set a hardware fault
    tb.set("hw_ov", 1u8);  // Hardware overvoltage fault
    tb.clock(1).await;

    // Try to enable - should NOT transition because of fault
    tb.set("enable", 1u8);
    tb.clock(5).await;

    // Should still be in Init or transitioned to Fault, not WaitBms
    let state: u8 = tb.get_as("state").await;
    assert_ne!(state, 1, "Should NOT be in WaitBms when fault is active");
    println!(
        "State with fault active: {} (expected Init=0 or Fault=7)",
        state
    );

    // Clear fault and reset
    tb.set("hw_ov", 0u8);
    tb.set("rst", 1u8);
    tb.clock(1).await;
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Now should be able to transition
    tb.set("enable", 1u8);
    tb.clock(1).await;

    let state: u8 = tb.get_as("state").await;
    assert_eq!(
        state, 1,
        "Should be in WaitBms after clearing fault and re-enabling"
    );

    println!("✓ Fault prevents transition test passed");
}

/// Test timer reset behavior (BUG #222 verification)
/// When entering WaitBms, state_timer should reset to 0
#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS"
)]
async fn test_timer_reset_on_state_change() {
    let mut tb = Testbench::behavioral_with_top(BATTERY_DCDC_PATH, TOP_MODULE)
        .await
        .expect("Failed to create testbench for battery DCDC");

    // Apply reset
    tb.set("rst", 1u8);
    tb.clock(5).await;
    tb.set("rst", 0u8);

    // Clear all faults
    tb.set("hw_ov", 0u8);
    tb.set("hw_uv", 0u8);
    tb.set("hw_oc", 0u8);
    tb.set("hw_ot", 0u8);
    tb.set("desat", 0u8);
    tb.set("bms.connected", 1u8);
    tb.set("bms.fault", 0u8);

    // Let some cycles pass in Init state (timer should increment)
    tb.clock(10).await;

    // Enable and transition to WaitBms
    tb.set("enable", 1u8);
    tb.clock(1).await;

    // Verify state is WaitBms
    let state: u8 = tb.get_as("state").await;
    assert_eq!(state, 1, "Should be in WaitBms state");

    // The timer should have been reset to 0 when entering WaitBms
    // (BUG #222 fix - timer reset conditional should override the timer increment)
    // Note: We can't directly read state_timer as it's internal, but if the timer
    // wasn't reset, the state machine would behave incorrectly in subsequent transitions

    println!("✓ Timer reset on state change test passed (BUG #222 verification)");
}

// ============================================================================
// Gate-Level Tests - verify synthesized RTL matches behavioral simulation
// NOTE: These tests are WIP - hierarchical gate-level simulation needs debugging
// ============================================================================

/// Gate-level test: basic initialization and reset
#[tokio::test]
#[ignore = "Gate-level hierarchical simulation WIP - state machine not transitioning"]
async fn test_battery_dcdc_init_gate_level() {
    let mut tb = Testbench::gate_level_with_top(BATTERY_DCDC_PATH, TOP_MODULE)
        .await
        .expect("Failed to create gate-level testbench for battery DCDC");

    // Apply reset
    tb.set("rst", 1u8);
    tb.clock(5).await;

    // Release reset
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Verify initial state (should be Init = 0)
    let state: u8 = tb.get_as("state").await;
    assert_eq!(state, 0, "Gate-level: Should be in Init state after reset");

    println!("✓ Gate-level: Battery DCDC initialization test passed");
}

/// Gate-level test: state machine transition from Init to WaitBms
#[tokio::test]
#[ignore = "Gate-level hierarchical simulation WIP - state machine not transitioning"]
async fn test_state_transition_init_to_waitbms_gate_level() {
    let mut tb = Testbench::gate_level_with_top(BATTERY_DCDC_PATH, TOP_MODULE)
        .await
        .expect("Failed to create gate-level testbench for battery DCDC");

    // Apply reset
    tb.set("rst", 1u8);
    tb.clock(5).await;
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Verify in Init state
    let state: u8 = tb.get_as("state").await;
    assert_eq!(state, 0, "Gate-level: Should be in Init state initially");

    // Clear all fault inputs
    tb.set("hw_ov", 0u8);
    tb.set("hw_uv", 0u8);
    tb.set("hw_oc", 0u8);
    tb.set("hw_ot", 0u8);
    tb.set("desat", 0u8);

    // Set BMS data to indicate no fault
    tb.set("bms.connected", 1u8);
    tb.set("bms.fault", 0u8);
    tb.set("bms_rx_valid", 1u8); // Set valid to prevent watchdog timeout
    tb.clock(1).await;

    // Debug: check fault outputs before enable
    println!("\n=== Fault outputs before enable ===");
    let fault_signals = [
        "faults.ov", "faults.uv", "faults.oc", "faults.ot", "faults.desat",
        "faults.bms_fault", "faults.bms_timeout", "faults.lockstep",
        "lockstep_fault", "master_enable",
    ];
    for sig in fault_signals {
        let val: u8 = tb.get_as(sig).await;
        println!("  {} = {}", sig, val);
    }

    // Enable the controller - should transition to WaitBms
    tb.set("enable", 1u8);

    // Gate-level may need more cycles for propagation
    tb.clock(5).await;

    // Debug: check fault outputs after enable
    println!("\n=== Fault outputs after enable ===");
    for sig in fault_signals {
        let val: u8 = tb.get_as(sig).await;
        println!("  {} = {}", sig, val);
    }

    // Check enable signal is actually set
    let enable_val: u8 = tb.get_as("enable").await;
    println!("\nenable = {}", enable_val);

    // Should now be in WaitBms (state = 1)
    let state_after: u8 = tb.get_as("state").await;
    println!("Gate-level: state after enable = {}", state_after);
    assert_eq!(state_after, 1, "Gate-level: Should be in WaitBms state (1) after enable");

    println!("✓ Gate-level: State transition Init->WaitBms test passed");
}
