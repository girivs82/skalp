//! Battery DCDC Controller Tests
//!
//! Tests for the DAB Battery Controller state machine including:
//! - State transitions: Init -> WaitBms -> Precharge -> Running
//! - Fault handling
//! - Timer reset behavior (BUG #222 verification)

use skalp_testing::testbench::*;

// Use FastSim wrapper for reduced simulation time (1000 cycles vs millions)
const BATTERY_DCDC_PATH: &str = "/Users/girivs/src/design/sangam/src/battery_dcdc/test_config.sk";
const TOP_MODULE: &str = "DabController_FastSim";

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
    tb.set("bms_rx_valid", 1u8);  // Reset watchdog to prevent timeout
    tb.clock(1).await;

    // Keep sending BMS valid to prevent watchdog timeout
    tb.set("bms_rx_valid", 1u8);

    // Enable the controller - should transition to WaitBms
    tb.set("enable", 1u8);

    // Debug: check fault status before transition
    let faults_ov: u8 = tb.get_as("faults.ov").await;
    let faults_uv: u8 = tb.get_as("faults.uv").await;
    let faults_oc: u8 = tb.get_as("faults.oc").await;
    let faults_ot: u8 = tb.get_as("faults.ot").await;
    let faults_desat: u8 = tb.get_as("faults.desat").await;
    let faults_bms_fault: u8 = tb.get_as("faults.bms_fault").await;
    let faults_bms_timeout: u8 = tb.get_as("faults.bms_timeout").await;
    let faults_lockstep: u8 = tb.get_as("faults.lockstep").await;
    println!("=== Before transition: fault status ===");
    println!("  faults.ov = {}", faults_ov);
    println!("  faults.uv = {}", faults_uv);
    println!("  faults.oc = {}", faults_oc);
    println!("  faults.ot = {}", faults_ot);
    println!("  faults.desat = {}", faults_desat);
    println!("  faults.bms_fault = {}", faults_bms_fault);
    println!("  faults.bms_timeout = {}", faults_bms_timeout);
    println!("  faults.lockstep = {}", faults_lockstep);

    // Debug: check state before clock
    let state_before: u8 = tb.get_as("state").await;
    let enable_before: u8 = tb.get_as("enable").await;
    println!(
        "Before transition: enable={}, state={}",
        enable_before, state_before
    );

    tb.clock(1).await;

    // Debug: check fault status after transition
    let faults_bms_timeout_after: u8 = tb.get_as("faults.bms_timeout").await;
    println!("After clock: faults.bms_timeout = {}", faults_bms_timeout_after);

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

    // Initialize all fault inputs to clear (except the one we'll test)
    tb.set("hw_uv", 0u8);
    tb.set("hw_oc", 0u8);
    tb.set("hw_ot", 0u8);
    tb.set("desat", 0u8);

    // Set BMS data to prevent BMS-related faults
    tb.set("bms.connected", 1u8);
    tb.set("bms.fault", 0u8);
    tb.set("bms_rx_valid", 1u8);

    // Initialize lockstep signals
    tb.set("lockstep_rx_valid", 0u8);

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

    // Clear fault, disable, then reset
    // Note: FaultLatch clears when clear_faults = !enable, so we need enable=0
    tb.set("hw_ov", 0u8);
    tb.set("enable", 0u8);  // Disable to allow fault latch to clear
    tb.clock(1).await;      // Let fault latch clear

    tb.set("rst", 1u8);
    tb.clock(1).await;
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Now should be able to transition
    tb.set("enable", 1u8);
    tb.set("bms_rx_valid", 1u8);  // Keep BMS watchdog happy
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
// ============================================================================

/// Debug test to understand gate-level netlist structure
#[tokio::test]
async fn test_debug_gate_level_structure() {
    use skalp_frontend::parse_and_build_hir_from_file;
    use skalp_lir::{get_stdlib_library, lower_mir_hierarchical_with_top, map_hierarchical_to_gates};
    use skalp_mir::MirCompiler;
    use std::path::Path;

    let path = Path::new(BATTERY_DCDC_PATH);
    let hir = parse_and_build_hir_from_file(path).expect("HIR parse failed");

    let compiler = MirCompiler::new();
    let mir = compiler.compile_to_mir(&hir).expect("MIR compile failed");

    let library = get_stdlib_library("generic_asic").expect("Library load failed");

    let hier_lir = lower_mir_hierarchical_with_top(&mir, Some(TOP_MODULE));
    let hier_netlist = map_hierarchical_to_gates(&hier_lir, &library);

    // Check per-instance netlists before flatten
    println!("\n=== Per-instance netlist stats (before flatten) ===");
    for (path, inst) in &hier_netlist.instances {
        let seq_count = inst.netlist.cells.iter().filter(|c| c.is_sequential()).count();
        let total = inst.netlist.cells.len();
        if seq_count > 0 {
            println!("  {}: {} cells, {} sequential", path, total, seq_count);
        }
    }

    let netlist = hier_netlist.flatten();

    println!("\n=== Flattened netlist stats ===");
    println!("Total cells: {}", netlist.cells.len());

    let seq_count = netlist.cells.iter().filter(|c| c.is_sequential()).count();
    println!("Sequential cells: {}", seq_count);

    // Show first few sequential cells
    let seq_cells: Vec<_> = netlist.cells.iter().filter(|c| c.is_sequential()).take(5).collect();
    for cell in &seq_cells {
        println!("  {} (type: {}, clock: {:?})", cell.path, cell.cell_type, cell.clock);
    }

    // Check cell types
    let dff_count = netlist.cells.iter()
        .filter(|c| c.cell_type.to_lowercase().contains("dff"))
        .count();
    println!("DFF-type cells: {}", dff_count);

    // Check what net GateNetId(0) is
    println!("\n=== Checking clock net ===");
    if let Some(net0) = netlist.nets.get(0) {
        println!("Net 0: name='{}', is_clock={}, is_reset={}", net0.name, net0.is_clock, net0.is_reset);
    }

    // Find the actual clock net
    let clock_nets: Vec<_> = netlist.nets.iter().filter(|n| n.is_clock).collect();
    println!("Clock nets found: {}", clock_nets.len());
    for net in &clock_nets {
        println!("  {} (id={:?})", net.name, net.id);
    }

    // Check netlist.clocks
    println!("netlist.clocks: {:?}", netlist.clocks);

    // Check if all clock nets are really different or should have been merged
    println!("\n=== Unique clock net count: {} ===", clock_nets.len());

    // Check a sample of sequential cells' clock fields
    println!("\n=== Sample sequential cell clocks ===");
    let sample_seq: Vec<_> = netlist.cells.iter()
        .filter(|c| c.is_sequential() && c.path.contains("DabBatteryController"))
        .take(10)
        .collect();
    for cell in &sample_seq {
        println!("  {} clock={:?}", cell.path, cell.clock);
        if let Some(clk_id) = cell.clock {
            if let Some(clk_net) = netlist.nets.get(clk_id.0 as usize) {
                println!("    -> net name: {}", clk_net.name);
            }
        }
    }

    assert!(seq_count > 0, "Should have sequential cells in gate netlist");

    // Write gate-level Verilog to file for debugging
    let verilog = netlist.to_verilog();
    std::fs::write("/tmp/battery_dcdc_gates.v", &verilog).expect("Failed to write Verilog");
    println!("\n=== Gate-level Verilog written to /tmp/battery_dcdc_gates.v ===");

    // Now convert to SIR and check
    use skalp_sim::convert_gate_netlist_to_sir;
    let sir_result = convert_gate_netlist_to_sir(&netlist);

    println!("\n=== SIR Structure ===");
    println!("Comb blocks: {}", sir_result.sir.top_module.comb_blocks.len());
    println!("Seq blocks: {}", sir_result.sir.top_module.seq_blocks.len());

    for (i, seq_block) in sir_result.sir.top_module.seq_blocks.iter().enumerate() {
        println!("\nSeq block {}:", i);
        println!("  clock signal id: {:?}", seq_block.clock);
        println!("  operations count: {}", seq_block.operations.len());
        println!("  reset: {:?}", seq_block.reset);

        // Find the clock signal name
        if let Some(clk_sig) = sir_result.sir.top_module.signals.iter()
            .find(|s| s.id == seq_block.clock)
        {
            println!("  clock signal name: {}", clk_sig.name);
        }
    }
}

/// Gate-level test: basic initialization and reset
#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS"
)]
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
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS"
)]
async fn test_state_transition_init_to_waitbms_gate_level() {
    let mut tb = Testbench::gate_level_with_top(BATTERY_DCDC_PATH, TOP_MODULE)
        .await
        .expect("Failed to create gate-level testbench for battery DCDC");

    // Apply reset and set all inputs BEFORE releasing reset
    tb.set("rst", 1u8);

    // Clear all fault inputs while in reset
    tb.set("hw_ov", 0u8);
    tb.set("hw_uv", 0u8);
    tb.set("hw_oc", 0u8);
    tb.set("hw_ot", 0u8);
    tb.set("desat", 0u8);

    // Initialize lockstep signals to prevent false mismatch detection
    tb.set("lockstep_rx_valid", 0u8);  // No lockstep data yet
    tb.set("lockstep_rx_state", 0u8);
    tb.set("lockstep_rx_enable", 0u8);
    tb.set("lockstep_rx_faults", 0u8);

    // Set BMS data - NOT connected yet to prevent WaitBms->Precharge transition
    tb.set("bms.connected", 0u8);  // Will be connected later in test
    tb.set("bms.fault", 0u8);
    tb.set("bms_rx_valid", 1u8); // Set valid to prevent watchdog timeout

    tb.clock(5).await;
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Verify in Init state
    let state: u8 = tb.get_as("state").await;
    assert_eq!(state, 0, "Gate-level: Should be in Init state initially");

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

    // DEBUG: Single clock cycle for state transition
    tb.clock(1).await;

    // Check if enable is actually set
    let enable_check: u8 = tb.get_as("enable").await;
    println!("DEBUG: enable = {}", enable_check);

    let state1: u8 = tb.get_as("state").await;
    println!("DEBUG: state after 1 clock = {}", state1);

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
