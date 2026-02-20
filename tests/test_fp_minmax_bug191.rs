//! Minimal test for BUG #191: FP min/max mux selection issue
//!
//! Tests that fp_min and fp_max correctly select the minimum/maximum value
//! when compiled to NCL gate-level simulation.

use skalp_frontend::parse_and_build_compilation_context;
use skalp_lir::{get_stdlib_library, lower_mir_hierarchical, map_hierarchical_to_gates};
use skalp_mir::MirCompiler;
use skalp_sim::{CircuitMode, HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
use std::io::Write;
use tempfile::TempDir;

fn f32_to_bits(f: f32) -> u64 {
    f.to_bits() as u64
}

fn bits_to_f32(bits: u64) -> f32 {
    f32::from_bits(bits as u32)
}

fn compile_fp_minmax_test() -> skalp_lir::gate_netlist::GateNetlist {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");

    // Create the shared_fp_ops module
    let fp_ops_path = temp_dir.path().join("shared_fp_ops.sk");
    let mut fp_ops_file =
        std::fs::File::create(&fp_ops_path).expect("Failed to create fp_ops file");
    writeln!(
        fp_ops_file,
        r#"
/// FP32 Min
pub fn fp_min(a: bit[32], b: bit[32]) -> bit[32] {{
    let a_fp = a as fp32;
    let b_fp = b as fp32;
    let result = if a_fp < b_fp {{ a_fp }} else {{ b_fp }};
    return result as bit[32]
}}

/// FP32 Max
pub fn fp_max(a: bit[32], b: bit[32]) -> bit[32] {{
    let a_fp = a as fp32;
    let b_fp = b as fp32;
    let result = if a_fp > b_fp {{ a_fp }} else {{ b_fp }};
    return result as bit[32]
}}
"#
    )
    .expect("Failed to write fp_ops");

    // Create the main test module
    let main_path = temp_dir.path().join("main.sk");
    let mut main_file = std::fs::File::create(&main_path).expect("Failed to create main file");
    writeln!(
        main_file,
        r#"
use shared_fp_ops::*;

async entity FpMinMaxTest {{
    in a: bit[32]
    in b: bit[32]
    out min_result: bit[32]
    out max_result: bit[32]
}}

impl FpMinMaxTest {{
    min_result = fp_min(a, b)
    max_result = fp_max(a, b)
}}
"#
    )
    .expect("Failed to write main");

    // Set up module search path
    std::env::set_var("SKALP_STDLIB_PATH", temp_dir.path().to_str().unwrap());

    // Parse and compile
    let context = parse_and_build_compilation_context(&main_path).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
        .expect("Failed to compile to MIR");

    // Lower to gate netlist
    let hier_lir = lower_mir_hierarchical(&mir);
    let library = get_stdlib_library("generic_asic").expect("Failed to load library");
    let hier_result = map_hierarchical_to_gates(&hier_lir, &library);
    hier_result.flatten()
}

#[tokio::test]
#[ignore = "BUG #191: FP comparison with negative numbers not yet fixed in NCL"]
async fn test_fp_min_negative_vs_positive() {
    let netlist = compile_fp_minmax_test();

    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // Debug: show input and output nets from the netlist
    println!("  Input nets:");
    for net in &netlist.nets {
        if net.is_input {
            println!("    {} (id={})", net.name, net.id.0);
        }
    }
    println!("  Output nets:");
    for net in &netlist.nets {
        if net.is_output {
            println!("    {} (id={})", net.name, net.id.0);
        }
    }

    // Debug: show FP comparison cells and their connections
    for cell in &netlist.cells {
        if cell.cell_type.contains("FP32") {
            println!(
                "  Found {} cell id={}: {} inputs, {} outputs",
                cell.cell_type,
                cell.id.0,
                cell.inputs.len(),
                cell.outputs.len()
            );
            println!("    outputs: {:?}", cell.outputs);
        }
    }

    // Find cells that use the FP comparison output (should be TH22 gates in the mux)
    let fp_lt_outputs: Vec<_> = netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.contains("FP32_LT"))
        .flat_map(|c| c.outputs.clone())
        .collect();
    println!("  FP32_LT output nets: {:?}", fp_lt_outputs);

    for cell in &netlist.cells {
        for net in &fp_lt_outputs {
            if cell.inputs.contains(net) {
                println!(
                    "  Cell {} ({}) uses FP32_LT output as input",
                    cell.id.0, cell.cell_type
                );
            }
        }
    }

    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Auto, // Use GPU if available
        max_iterations: 100000,
        ncl_debug: true, // Enable debug output
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist.clone())
        .expect("Failed to load NCL netlist");

    // Debug: print input and output names found by the simulator
    let inputs = sim.get_input_names();
    let outputs = sim.get_output_names();
    println!("  Available inputs ({} groups): {:?}", inputs.len(), inputs);
    println!(
        "  Available outputs ({} groups): {:?}",
        outputs.len(),
        outputs
    );

    // Test: fp_min(-10.0, 10.0) should return -10.0
    let a = f32_to_bits(-10.0);
    let b = f32_to_bits(10.0);

    println!("\n=== Test: fp_min(-10.0, 10.0) ===");
    println!("  a = -10.0, bits = 0x{:08X}", a);
    println!("  b = 10.0, bits = 0x{:08X}", b);

    // Try both naming conventions
    if inputs.contains(&"top.a".to_string()) {
        println!("  Setting top.a and top.b");
        sim.set_ncl_input("top.a", a, 32);
        sim.set_ncl_input("top.b", b, 32);
    } else if inputs.contains(&"a".to_string()) {
        println!("  Setting a and b");
        sim.set_ncl_input("a", a, 32);
        sim.set_ncl_input("b", b, 32);
    } else {
        println!("  WARNING: No matching input names found!");
    }

    // Run until stable with periodic debug output
    let mut iterations = 0;
    let mut last_output: Option<u64> = None;
    while !sim.is_ncl_complete() && iterations < 500 {
        sim.step().await;
        iterations += 1;

        // Print first few iterations and changes
        let current_output = sim.get_ncl_output("top.min_result", 32);
        if iterations <= 5 || iterations % 100 == 0 || current_output != last_output {
            println!(
                "  Iteration {}: min_result = {:?}",
                iterations, current_output
            );
            last_output = current_output;
        }
    }
    println!(
        "  Converged after {} iterations (complete={})",
        iterations,
        sim.is_ncl_complete()
    );

    // Get results
    if let Some(min_bits) = sim.get_ncl_output("top.min_result", 32) {
        let min_val = bits_to_f32(min_bits);
        println!("  min_result = {} (bits: 0x{:08X})", min_val, min_bits);
        println!("  Expected: -10.0 (bits: 0x{:08X})", f32_to_bits(-10.0));

        if (min_val - (-10.0)).abs() < 0.01 {
            println!("  ✓ PASS: min_result is correct");
        } else {
            println!("  ✗ FAIL: min_result is wrong");
            // More debug: check what value this is
            if (min_val - 10.0).abs() < 0.01 {
                println!("  BUG #191 CONFIRMED: Got the ELSE branch (b=10.0) instead of THEN branch (a=-10.0)");
            }
        }
        assert!(
            (min_val - (-10.0)).abs() < 0.01,
            "fp_min(-10.0, 10.0) should be -10.0, got {}",
            min_val
        );
    } else {
        println!("  min_result = NULL (not stable)");
        panic!("min_result did not converge");
    }
}

#[tokio::test]
#[ignore = "BUG #191: FP comparison with negative numbers not yet fixed in NCL"]
async fn test_fp_max_negative_vs_positive() {
    let netlist = compile_fp_minmax_test();

    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Cpu,
        max_iterations: 100000,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist.clone())
        .expect("Failed to load NCL netlist");

    // Test: fp_max(-10.0, 10.0) should return 10.0
    let a = f32_to_bits(-10.0);
    let b = f32_to_bits(10.0);

    println!("\n=== Test: fp_max(-10.0, 10.0) ===");
    sim.set_ncl_input("top.a", a, 32);
    sim.set_ncl_input("top.b", b, 32);

    let mut iterations = 0;
    while !sim.is_ncl_complete() && iterations < 100000 {
        sim.step().await;
        iterations += 1;
    }
    println!("  Converged after {} iterations", iterations);

    if let Some(max_bits) = sim.get_ncl_output("top.max_result", 32) {
        let max_val = bits_to_f32(max_bits);
        println!("  max_result = {} (expected 10.0)", max_val);
        assert!(
            (max_val - 10.0).abs() < 0.01,
            "fp_max(-10.0, 10.0) should be 10.0, got {}",
            max_val
        );
    } else {
        panic!("max_result did not converge");
    }
}
