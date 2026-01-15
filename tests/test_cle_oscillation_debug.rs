//! Quick diagnostic test to identify oscillating cells in CLE

#![cfg(not(debug_assertions))]

use skalp_frontend::parse_and_build_compilation_context;
use skalp_lir::{get_stdlib_library, lower_mir_hierarchical, map_hierarchical_to_gates};
use skalp_mir::MirCompiler;
use skalp_sim::{CircuitMode, HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
use std::path::Path;

#[tokio::test]
#[ignore = "requires karythra CLE with FP trait imports (use skalp::numeric::fp::*)"]
async fn test_identify_oscillating_cells() {
    println!("\n=== CLE Oscillation Diagnostic ===");

    // Set up module search path
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        "/Users/girivs/src/hw/karythra/rtl/skalp/cle/lib",
    );

    let cle_path = Path::new("/Users/girivs/src/hw/karythra/rtl/skalp/cle/src/main_async.sk");
    if !cle_path.exists() {
        println!("Karythra async CLE not found - skipping");
        return;
    }

    println!("Compiling CLE...");
    let context = parse_and_build_compilation_context(cle_path).expect("Failed to parse async CLE");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
        .expect("Failed to compile to MIR");

    let hier_lir = lower_mir_hierarchical(&mir);
    let library = get_stdlib_library("generic_asic").expect("Failed to load library");
    let hier_result = map_hierarchical_to_gates(&hier_lir, &library);
    let netlist = hier_result.flatten();

    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Auto,
        max_iterations: 500, // Higher limit to see if oscillation resolves
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist.clone())
        .expect("Failed to load NCL netlist");

    // Set inputs for L0 add operation (opcode 0)
    sim.set_ncl_input("top.function_sel", 0, 6);
    sim.set_ncl_input("top.route_sel", 0, 3);
    sim.set_ncl_input("top.data1", 5, 256);
    sim.set_ncl_input("top.data2", 3, 256);

    println!("\nRunning 20 iterations to identify oscillation pattern...");
    let result = sim.run_until_stable().await;

    println!(
        "\nResult: iterations={}, stable={}",
        result.iterations, result.is_stable
    );

    // Oscillation detection happens automatically when max_iterations is reached

    // Try to read output even if not stable
    match sim.get_ncl_output("top.result", 32) {
        Some(v) => println!("\nResult value: 0x{:08X}", v),
        None => println!("\nResult: NULL (circuit not converged)"),
    }
}
