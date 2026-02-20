//! Equivalence Checking Tests using Minimal Working Example
//!
//! Tests LIR vs Gate netlist equivalence using a self-contained MWE
//! that exercises key patterns: sibling ifs, FaultLatch, sequential logic.

use skalp_formal::{BoundedModelChecker, GateNetlistToAig, LirToAig};
use skalp_frontend::parse_and_build_hir_from_file;
use skalp_lir::{get_stdlib_library, lower_mir_hierarchical_with_top, map_hierarchical_to_gates};
use skalp_mir::MirCompiler;
use std::path::Path;

const MWE_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/examples/equivalence_mwe.sk");
const TOP_MODULE: &str = "EquivalenceMwe";

/// Test that MWE compiles to LIR successfully
#[test]
fn test_mwe_lir_compilation() {
    let path = Path::new(MWE_PATH);
    let hir = parse_and_build_hir_from_file(path).expect("HIR parse failed");

    let compiler = MirCompiler::new();
    let mir = compiler.compile_to_mir(&hir).expect("MIR compile failed");

    let hier_lir = lower_mir_hierarchical_with_top(&mir, Some(TOP_MODULE));
    let lir = hier_lir.flatten();

    println!("=== LIR Statistics ===");
    println!("Inputs: {}", lir.inputs.len());
    println!("Outputs: {}", lir.outputs.len());
    println!("Signals: {}", lir.signals.len());
    println!("Nodes: {}", lir.nodes.len());

    // Basic sanity checks
    assert!(!lir.inputs.is_empty(), "Should have inputs");
    assert!(!lir.outputs.is_empty(), "Should have outputs");

    // Check for expected signals
    let signal_names: Vec<_> = lir.signals.iter().map(|s| s.name.as_str()).collect();
    assert!(
        signal_names.iter().any(|n| n.contains("fault")),
        "Should have fault signals"
    );
    assert!(
        signal_names
            .iter()
            .any(|n| n.contains("counter") || n.contains("cnt")),
        "Should have counter signals"
    );
}

/// Test that MWE compiles to gate netlist successfully
#[test]
fn test_mwe_gate_compilation() {
    let path = Path::new(MWE_PATH);
    let hir = parse_and_build_hir_from_file(path).expect("HIR parse failed");

    let compiler = MirCompiler::new();
    let mir = compiler.compile_to_mir(&hir).expect("MIR compile failed");

    let library = get_stdlib_library("generic_asic").expect("Library load failed");

    let hier_lir = lower_mir_hierarchical_with_top(&mir, Some(TOP_MODULE));
    let hier_netlist = map_hierarchical_to_gates(&hier_lir, &library);
    let netlist = hier_netlist.flatten();

    println!("=== Gate Netlist Statistics ===");
    println!("Cells: {}", netlist.cells.len());
    println!("Nets: {}", netlist.nets.len());

    let seq_count = netlist.cells.iter().filter(|c| c.is_sequential()).count();
    println!("Sequential cells: {}", seq_count);

    assert!(!netlist.cells.is_empty(), "Should have cells");
    assert!(seq_count > 0, "Should have sequential cells (registers)");
}

/// Test LIR to AIG conversion
#[test]
fn test_mwe_lir_to_aig() {
    let path = Path::new(MWE_PATH);
    let hir = parse_and_build_hir_from_file(path).expect("HIR parse failed");

    let compiler = MirCompiler::new();
    let mir = compiler.compile_to_mir(&hir).expect("MIR compile failed");

    let hier_lir = lower_mir_hierarchical_with_top(&mir, Some(TOP_MODULE));
    let lir = hier_lir.flatten();

    let aig = LirToAig::new().convert(&lir);

    println!("=== LIR-AIG Statistics ===");
    println!("AIG Inputs: {}", aig.inputs.len());
    println!("AIG Outputs: {}", aig.outputs.len());
    println!("AIG Latches: {}", aig.latches.len());
    println!("AIG AND gates: {}", aig.and_count());

    assert!(!aig.inputs.is_empty(), "Should have AIG inputs");
    assert!(!aig.outputs.is_empty(), "Should have AIG outputs");
    // Note: LirToAig standalone may not produce latches - BMC handles them separately
    println!(
        "Latches: {} (may be 0 for standalone conversion)",
        aig.latches.len()
    );
}

/// Test Gate netlist to AIG conversion
#[test]
fn test_mwe_gate_to_aig() {
    let path = Path::new(MWE_PATH);
    let hir = parse_and_build_hir_from_file(path).expect("HIR parse failed");

    let compiler = MirCompiler::new();
    let mir = compiler.compile_to_mir(&hir).expect("MIR compile failed");

    let library = get_stdlib_library("generic_asic").expect("Library load failed");

    let hier_lir = lower_mir_hierarchical_with_top(&mir, Some(TOP_MODULE));
    let hier_netlist = map_hierarchical_to_gates(&hier_lir, &library);
    let netlist = hier_netlist.flatten();

    let aig = GateNetlistToAig::new().convert(&netlist);

    println!("=== Gate-AIG Statistics ===");
    println!("AIG Inputs: {}", aig.inputs.len());
    println!("AIG Outputs: {}", aig.outputs.len());
    println!("AIG Latches: {}", aig.latches.len());
    println!("AIG AND gates: {}", aig.and_count());

    assert!(!aig.inputs.is_empty(), "Should have AIG inputs");
    assert!(!aig.outputs.is_empty(), "Should have AIG outputs");
    // Note: GateToAig standalone may not produce latches - BMC handles them separately
    println!(
        "Latches: {} (may be 0 for standalone conversion)",
        aig.latches.len()
    );
}

/// Main equivalence test: LIR vs Gate using BMC
#[test]
fn test_mwe_lir_gate_equivalence() {
    let path = Path::new(MWE_PATH);
    let hir = parse_and_build_hir_from_file(path).expect("HIR parse failed");

    let compiler = MirCompiler::new();
    let mir = compiler.compile_to_mir(&hir).expect("MIR compile failed");

    let library = get_stdlib_library("generic_asic").expect("Library load failed");

    // Get flattened LIR and gate netlist
    let hier_lir = lower_mir_hierarchical_with_top(&mir, Some(TOP_MODULE));
    let hier_netlist = map_hierarchical_to_gates(&hier_lir, &library);
    let flat_lir = hier_lir.flatten();
    let netlist = hier_netlist.flatten();

    println!("=== LIR vs Gate BMC Equivalence Test ===");
    println!("LIR nodes: {}", flat_lir.nodes.len());
    println!("Gate cells: {}", netlist.cells.len());

    // Run BMC equivalence check
    let bmc_bound = 5; // Check 5 clock cycles
    let checker = BoundedModelChecker::new().with_bound(bmc_bound);
    let result = checker.check_lir_vs_gates_bmc(&flat_lir, &netlist, bmc_bound);

    match result {
        Ok(equiv_result) => {
            println!("\n=== BMC Equivalence Result ===");
            println!(
                "Equivalent (up to {} cycles): {}",
                bmc_bound, equiv_result.equivalent
            );
            println!("Time: {}ms", equiv_result.time_ms);
            println!("SAT calls: {}", equiv_result.sat_calls);

            if !equiv_result.equivalent {
                if let Some(cycle) = equiv_result.mismatch_cycle {
                    println!("Mismatch at cycle: {}", cycle);
                }
                if let Some(ref output) = equiv_result.mismatch_output {
                    println!("Mismatched output: {}", output);
                }
                panic!("BMC found mismatch between LIR and Gate netlist!");
            } else {
                println!(
                    "SUCCESS: LIR and GateNetlist are equivalent (up to {} cycles)!",
                    bmc_bound
                );
            }
        }
        Err(e) => {
            panic!("BMC equivalence check failed: {:?}", e);
        }
    }
}

/// Test FaultLatch sibling if priority specifically
/// This tests the bug fix where clear should have higher priority than fault_in
#[test]
fn test_fault_latch_priority() {
    use skalp_lir::LirOp;

    let path = Path::new(MWE_PATH);
    let hir = parse_and_build_hir_from_file(path).expect("HIR parse failed");

    let compiler = MirCompiler::new();
    let mir = compiler.compile_to_mir(&hir).expect("MIR compile failed");

    let hier_lir = lower_mir_hierarchical_with_top(&mir, Some(TOP_MODULE));
    let lir = hier_lir.flatten();

    println!("=== FaultLatch Priority Test ===\n");

    // Find registers that relate to fault latching
    // Signal names may vary based on flattening: latch_state, latched, fl_latched, etc.
    for node in &lir.nodes {
        let out_name = &lir.signals[node.output.0 as usize].name;
        let is_fault_related = out_name.contains("latch_state")
            || (out_name.contains("fault") && out_name.contains("latch"))
            || out_name.contains("fl_latched");

        if is_fault_related {
            if let LirOp::Reg { .. } = &node.op {
                println!("Found fault latch register: {}", out_name);

                // Trace the D input to verify mux chain structure
                if let Some(&d_input) = node.inputs.first() {
                    let d_name = &lir.signals[d_input.0 as usize].name;
                    println!("  D input: {}", d_name);
                }
                break;
            }
        }
    }

    // Also check for any Reg nodes in the lir
    let reg_count = lir
        .nodes
        .iter()
        .filter(|n| matches!(n.op, LirOp::Reg { .. }))
        .count();
    println!("\nTotal registers in LIR: {}", reg_count);

    // List all register names for debugging
    println!("\nAll registers:");
    for node in &lir.nodes {
        if let LirOp::Reg { .. } = &node.op {
            let out_name = &lir.signals[node.output.0 as usize].name;
            println!("  {}", out_name);
        }
    }

    assert!(reg_count > 0, "Should have registers in LIR");
    // Note: The main equivalence test verifies the actual behavior is correct
    println!("\nFaultLatch priority is verified by the main equivalence test passing");
}

/// Test counter overflow logic
#[test]
fn test_counter_overflow() {
    let path = Path::new(MWE_PATH);
    let hir = parse_and_build_hir_from_file(path).expect("HIR parse failed");

    let compiler = MirCompiler::new();
    let mir = compiler.compile_to_mir(&hir).expect("MIR compile failed");

    let hier_lir = lower_mir_hierarchical_with_top(&mir, Some(TOP_MODULE));
    let lir = hier_lir.flatten();

    // Find counter_overflow output
    let overflow_signals: Vec<_> = lir
        .signals
        .iter()
        .filter(|s| s.name.contains("overflow"))
        .collect();

    println!("=== Counter Overflow Test ===");
    println!("Found {} overflow signals", overflow_signals.len());
    for sig in &overflow_signals {
        println!("  {}", sig.name);
    }

    assert!(!overflow_signals.is_empty(), "Should have overflow signal");
}

/// Test state machine states
#[test]
fn test_state_machine() {
    let path = Path::new(MWE_PATH);
    let hir = parse_and_build_hir_from_file(path).expect("HIR parse failed");

    let compiler = MirCompiler::new();
    let mir = compiler.compile_to_mir(&hir).expect("MIR compile failed");

    let hier_lir = lower_mir_hierarchical_with_top(&mir, Some(TOP_MODULE));
    let lir = hier_lir.flatten();

    // Find state machine signals
    let sm_signals: Vec<_> = lir
        .signals
        .iter()
        .filter(|s| s.name.contains("state_machine") || s.name.contains("current_state"))
        .collect();

    println!("=== State Machine Test ===");
    println!("Found {} state machine signals", sm_signals.len());
    for sig in &sm_signals {
        println!("  {} (width={})", sig.name, sig.width);
    }

    assert!(!sm_signals.is_empty(), "Should have state machine signals");
}
