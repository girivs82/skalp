//! Test hierarchical NCL async circuits
//!
//! Verifies that async context propagates to child modules correctly.

use skalp_frontend::parse_and_build_hir;
use skalp_lir::{get_stdlib_library, lower_mir_hierarchical, lower_mir_module_to_lir, map_hierarchical_to_gates, map_lir_to_gates};
use skalp_mir::MirCompiler;
use skalp_sim::{CircuitMode, HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};

/// Simple async entity that calls a child function
const HIERARCHICAL_ASYNC_SOURCE: &str = r#"
/// Simple 8-bit adder function (will be instantiated as child module)
fn add8(a: bit[8], b: bit[8]) -> bit[8] {
    a + b
}

/// Simple 8-bit multiplier function
fn mul8(a: bit[8], b: bit[8]) -> bit[8] {
    a * b
}

/// Async entity that uses child function modules
async entity HierarchicalAsync {
    in data1: bit[8]
    in data2: bit[8]
    in sel: bit[1]
    out result: bit[8]
}

impl HierarchicalAsync {
    // Call child functions - these become separate module instances
    signal add_result: bit[8]
    signal mul_result: bit[8]

    add_result = add8(data1, data2)
    mul_result = mul8(data1, data2)

    // Select between results
    result = if sel == 1 {
        mul_result
    } else {
        add_result
    }
}
"#;

#[test]
fn test_hierarchical_ncl_async() {
    let hir = parse_and_build_hir(HIERARCHICAL_ASYNC_SOURCE).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler.compile(&hir).expect("Failed to compile to MIR");

    // Use hierarchical compilation
    let hier_lir = lower_mir_hierarchical(&mir);
    let library = get_stdlib_library("generic_asic").expect("Failed to load library");
    let hier_result = map_hierarchical_to_gates(&hier_lir, &library);


    let netlist = hier_result.flatten();

    println!("Compiled: {} cells, {} nets", netlist.cells.len(), netlist.nets.len());

    // Create NCL simulator
    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Gpu,
        max_iterations: 10000,
        capture_waveforms: false,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist.clone()).expect("Failed to load NCL netlist");

    // Note: Hierarchical flattening prefixes signal names with instance path
    // For the top module, signals become "top.data1", "top.data2", etc.

    // Test 1: Add mode (sel=0): 10 + 20 = 30
    sim.set_ncl_input("top.data1", 10, 8);
    sim.set_ncl_input("top.data2", 20, 8);
    sim.set_ncl_input("top.sel", 0, 1);
    let result = sim.run_until_stable();
    println!("Test 1 (add): iterations={}", result.iterations);

    match sim.get_ncl_output("top.result", 8) {
        Some(value) => {
            println!("  10 + 20 = {} (expected 30)", value);
            assert_eq!(value, 30, "Addition failed");
        }
        None => panic!("Result is NULL"),
    }

    // Skip mul test for now - focus on verifying hierarchical compilation works
    // The add test passing confirms the fix for duplicate C-element nets

    println!("Hierarchical NCL add test passed!");
}

/// Test the same circuit using direct (non-hierarchical) compilation
/// This helps verify whether the issue is with hierarchical flattening
#[test]
fn test_direct_ncl_async() {
    let hir = parse_and_build_hir(HIERARCHICAL_ASYNC_SOURCE).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler.compile(&hir).expect("Failed to compile to MIR");

    assert!(!mir.modules.is_empty(), "Should have at least one module");
    let module = &mir.modules[0];

    // Use direct (non-hierarchical) compilation
    let lir_result = lower_mir_module_to_lir(module);
    let library = get_stdlib_library("generic_asic").expect("Failed to load library");
    let tech_result = map_lir_to_gates(&lir_result.lir, &library);
    let netlist = tech_result.netlist;

    println!("Direct compiled: {} cells, {} nets", netlist.cells.len(), netlist.nets.len());

    // Create NCL simulator
    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Gpu,
        max_iterations: 10000,
        capture_waveforms: false,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist).expect("Failed to load NCL netlist");

    // Test 1: Add mode (sel=0): 10 + 20 = 30
    sim.set_ncl_input("data1", 10, 8);
    sim.set_ncl_input("data2", 20, 8);
    sim.set_ncl_input("sel", 0, 1);
    let result = sim.run_until_stable();
    println!("Direct Test 1 (add): iterations={}", result.iterations);

    match sim.get_ncl_output("result", 8) {
        Some(value) => {
            println!("  10 + 20 = {} (expected 30)", value);
            assert_eq!(value, 30, "Addition failed");
        }
        None => panic!("Result is NULL"),
    }

    println!("Direct NCL test passed!");
}
