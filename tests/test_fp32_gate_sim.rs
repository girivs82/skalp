//! FP32 Gate-Level Simulation Test
//!
//! Verifies that FP32 operations produce correct arithmetic results
//! after gate-level synthesis and optimization.

#![cfg(not(debug_assertions))]

use skalp_frontend::parse_and_build_compilation_context;
use skalp_lir::{
    apply_boundary_ncl_to_hierarchy, get_stdlib_library, lower_mir_hierarchical_for_optimize_first,
    map_hierarchical_to_gates, NclConfig,
};
use skalp_mir::MirCompiler;
use skalp_sim::{CircuitMode, HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
use std::path::Path;

/// Compile an FP32 test file to gate netlist
fn compile_fp32_test(source_path: &Path) -> skalp_lir::GateNetlist {
    // Set up stdlib path
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        "/Users/girivs/src/hw/hls/crates/skalp-stdlib",
    );

    let context =
        parse_and_build_compilation_context(source_path).expect("Failed to parse FP32 test");

    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
        .expect("Failed to compile to MIR");

    // Use optimize-first approach for proper boundary-only NCL handling
    // This ensures child async modules are coalesced with parent (not separately expanded)
    let (hier_lir_raw, has_async) = lower_mir_hierarchical_for_optimize_first(&mir);

    // Apply boundary-only NCL to the hierarchy
    let hier_lir = if has_async {
        let ncl_config = NclConfig::default(); // boundary_only is true by default
        apply_boundary_ncl_to_hierarchy(&hier_lir_raw, &ncl_config)
    } else {
        hier_lir_raw
    };

    let library = get_stdlib_library("generic_asic").expect("Failed to load library");
    let hier_result = map_hierarchical_to_gates(&hier_lir, &library);
    hier_result.flatten()
}

/// Run gate-level simulation for FP32 add
async fn simulate_fp32_add(netlist: &skalp_lir::GateNetlist, a: f32, b: f32) -> Option<f32> {
    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Cpu, // Force CPU for consistency
        max_iterations: 10000,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist.clone())
        .expect("Failed to load NCL netlist");

    // Set inputs
    let a_bits = a.to_bits() as u64;
    let b_bits = b.to_bits() as u64;

    println!(
        "  Setting inputs: a=0x{:08X} ({}) b=0x{:08X} ({})",
        a_bits, a, b_bits, b
    );
    sim.set_ncl_input("top.a", a_bits, 32);
    sim.set_ncl_input("top.b", b_bits, 32);

    // Run simulation
    let result = sim.run_until_stable().await;
    println!(
        "  Converged after {} iterations, stable={}",
        result.iterations, result.is_stable
    );

    if !result.is_stable {
        println!(
            "WARNING: Simulation did not converge after {} iterations",
            result.iterations
        );
        return None;
    }

    // Debug: list available output names
    let available_outputs = sim.get_output_names();
    if available_outputs.len() <= 10 {
        println!("  Available outputs: {:?}", available_outputs);
    } else {
        println!(
            "  Available outputs ({} total): first 10 = {:?}",
            available_outputs.len(),
            &available_outputs[..10]
        );
    }

    // Try multiple output names - the flattening may create different names
    let output_names = [
        "top.result",
        "top.sum",
        "top.__adder_result_9.result",
        "top.__adder_result_9_result",
        "result",
        "FP32AddTest.result",
    ];
    for name in output_names {
        if let Some(v) = sim.get_ncl_output(name, 32) {
            let result_f32 = f32::from_bits(v as u32);
            println!("  Output '{}' = 0x{:08X} ({})", name, v, result_f32);
            return Some(result_f32);
        }
    }

    // Get output from first available
    None
}

#[tokio::test]
async fn test_fp32_add_simple() {
    let source_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/test_fpadd_sim.sk");

    println!("Compiling FP32 add test...");
    let netlist = compile_fp32_test(&source_path);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // Test cases: (a, b, expected_sum)
    let test_cases = [
        (2.0f32, 3.0f32, 5.0f32),
        (1.0f32, 1.0f32, 2.0f32),
        (10.0f32, -3.0f32, 7.0f32),
        (0.5f32, 0.25f32, 0.75f32),
        (1.5f32, 2.5f32, 4.0f32),
    ];

    let mut passed = 0;
    let mut failed = 0;

    for (a, b, expected) in test_cases {
        print!("  {:.2} + {:.2} = ", a, b);

        match simulate_fp32_add(&netlist, a, b).await {
            Some(result) => {
                let error = (result - expected).abs();
                let relative_error = if expected != 0.0 {
                    error / expected.abs()
                } else {
                    error
                };

                if relative_error < 1e-6 || (result == expected) {
                    println!("{:.6} ✓", result);
                    passed += 1;
                } else {
                    println!(
                        "{:.6} ✗ (expected {:.6}, error={:.2e})",
                        result, expected, relative_error
                    );
                    failed += 1;
                }
            }
            None => {
                println!("FAILED (no result)");
                failed += 1;
            }
        }
    }

    println!("\nResults: {} passed, {} failed", passed, failed);
    assert_eq!(failed, 0, "Some FP32 add tests failed");
}

#[tokio::test]
async fn test_fp32_add_edge_cases() {
    let source_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/test_fpadd_sim.sk");

    let netlist = compile_fp32_test(&source_path);

    // Edge cases
    let test_cases = [
        // Zero handling
        (0.0f32, 5.0f32, 5.0f32, "0 + x = x"),
        (5.0f32, 0.0f32, 5.0f32, "x + 0 = x"),
        (0.0f32, 0.0f32, 0.0f32, "0 + 0 = 0"),
        // Negative numbers
        (-1.0f32, -2.0f32, -3.0f32, "(-1) + (-2) = -3"),
        (5.0f32, -5.0f32, 0.0f32, "x + (-x) = 0"),
        // Large difference in exponents
        (1.0f32, 1e-7f32, 1.0f32, "1 + tiny ≈ 1"),
        (1e10f32, 1.0f32, 1e10f32, "huge + 1 ≈ huge"),
    ];

    let mut passed = 0;
    let mut failed = 0;

    for (a, b, expected, desc) in test_cases {
        print!("  {}: ", desc);

        match simulate_fp32_add(&netlist, a, b).await {
            Some(result) => {
                let error = (result - expected).abs();
                // Allow for FP rounding errors
                let tolerance = expected.abs() * 1e-6 + 1e-7;

                if error <= tolerance {
                    println!("✓ ({:.6})", result);
                    passed += 1;
                } else {
                    println!("✗ got {:.6}, expected {:.6}", result, expected);
                    failed += 1;
                }
            }
            None => {
                println!("FAILED (no result)");
                failed += 1;
            }
        }
    }

    println!("\nEdge cases: {} passed, {} failed", passed, failed);
    assert_eq!(failed, 0, "Some FP32 edge case tests failed");
}
