//! FP32 Gate-Level Simulation Test
//!
//! Verifies that FP32 operations produce correct arithmetic results
//! after gate-level synthesis and optimization.

use skalp_frontend::parse_and_build_compilation_context;
use skalp_lir::{
    apply_boundary_ncl_to_hierarchy, get_stdlib_library, lower_mir_hierarchical_for_optimize_first,
    map_hierarchical_to_gates, NclConfig,
};
use skalp_mir::MirCompiler;
use skalp_sim::ncl_sim::{NclSimConfig, NclSimulator};
use skalp_sim::{CircuitMode, HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
use std::path::Path;

/// Compile an FP32 test file to gate netlist (async entity → NCL)
fn compile_fp32_test(source_path: &Path) -> skalp_lir::GateNetlist {
    let stdlib_path = format!("{}/crates/skalp-stdlib", env!("CARGO_MANIFEST_DIR"));
    std::env::set_var("SKALP_STDLIB_PATH", &stdlib_path);

    let context =
        parse_and_build_compilation_context(source_path).expect("Failed to parse FP32 test");

    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
        .expect("Failed to compile to MIR");

    let (hier_lir_raw, has_async) = lower_mir_hierarchical_for_optimize_first(&mir);

    let hier_lir = if has_async {
        let ncl_config = NclConfig::default();
        apply_boundary_ncl_to_hierarchy(&hier_lir_raw, &ncl_config)
    } else {
        hier_lir_raw
    };

    let library = get_stdlib_library("generic_asic").expect("Failed to load library");
    let hier_result = map_hierarchical_to_gates(&hier_lir, &library);
    hier_result.flatten()
}

/// Compile a synchronous FP32 test file to gate netlist (no NCL)
fn compile_fp32_sync_test(source_path: &Path) -> skalp_lir::GateNetlist {
    let stdlib_path = format!("{}/crates/skalp-stdlib", env!("CARGO_MANIFEST_DIR"));
    std::env::set_var("SKALP_STDLIB_PATH", &stdlib_path);

    let context =
        parse_and_build_compilation_context(source_path).expect("Failed to parse FP32 sync test");

    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
        .expect("Failed to compile to MIR");

    // Sync entity: has_async will be false, no NCL expansion
    let (hier_lir_raw, has_async) = lower_mir_hierarchical_for_optimize_first(&mir);
    assert!(
        !has_async,
        "Sync test fixture should not have async entities"
    );

    let library = get_stdlib_library("generic_asic").expect("Failed to load library");
    let hier_result = map_hierarchical_to_gates(&hier_lir_raw, &library);
    hier_result.flatten()
}

/// Direct gate-level evaluation of a synchronous (non-NCL) GateNetlist.
/// Sets single-rail inputs, iterates combinational logic until stable, reads outputs.
fn evaluate_sync_gate_netlist(
    netlist: &skalp_lir::GateNetlist,
    input_a: u32,
    input_b: u32,
) -> Option<u32> {
    // Use NclSimulator's iterate() for combinational propagation (works for non-NCL too)
    let config = NclSimConfig {
        max_iterations: 10000,
        debug: false,
        track_stages: false,
    };
    let mut sim = NclSimulator::new(netlist.clone(), config);

    // Set input bits directly (single-rail, no dual-rail encoding)
    // For sync netlists, inputs are named like "top.a[0]", "top.a[1]", etc.
    for bit in 0..32u32 {
        let a_val = (input_a >> bit) & 1 != 0;
        let b_val = (input_b >> bit) & 1 != 0;

        let a_name = format!("top.a[{}]", bit);
        let b_name = format!("top.b[{}]", bit);

        // Find and set the net by name
        for net in &netlist.nets {
            if net.name == a_name && net.is_input {
                sim.set_net(net.id, a_val);
            }
            if net.name == b_name && net.is_input {
                sim.set_net(net.id, b_val);
            }
        }
    }

    // Iterate until stable
    let iterations = sim.run_until_stable(10000);
    let is_stable = sim.stats().is_stable;
    println!(
        "  Converged after {} iterations, stable={}",
        iterations, is_stable
    );

    if !is_stable {
        println!("  WARNING: did not converge");
        return None;
    }

    // Read output bits (single-rail)
    let mut result = 0u32;
    let mut found_bits = 0;
    for bit in 0..32u32 {
        let name = format!("top.result[{}]", bit);
        for net in &netlist.nets {
            if net.name == name && net.is_output {
                let resolved = netlist.resolve_alias(net.id);
                if sim.get_net(resolved) {
                    result |= 1 << bit;
                }
                found_bits += 1;
            }
        }
    }

    if found_bits == 0 {
        println!("  WARNING: No output bits found for 'top.result[N]'");
        return None;
    }

    println!("  Read {} output bits, result=0x{:08X}", found_bits, result);
    Some(result)
}

/// Run NCL gate-level simulation for FP32 add
async fn simulate_fp32_add(netlist: &skalp_lir::GateNetlist, a: f32, b: f32) -> Option<f32> {
    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Cpu,
        max_iterations: 10000,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist.clone())
        .expect("Failed to load NCL netlist");

    let a_bits = a.to_bits() as u64;
    let b_bits = b.to_bits() as u64;

    println!(
        "  Setting inputs: a=0x{:08X} ({}) b=0x{:08X} ({})",
        a_bits, a, b_bits, b
    );
    sim.set_ncl_input("top.a", a_bits, 32);
    sim.set_ncl_input("top.b", b_bits, 32);

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

    None
}

#[tokio::test]
async fn test_fp32_add_simple() {
    let source_path =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/test_fpadd_sim.sk");

    println!("Compiling FP32 add test (async/NCL)...");
    let netlist = compile_fp32_test(&source_path);
    println!(
        "Compiled: {} cells, {} nets, is_ncl={}",
        netlist.cells.len(),
        netlist.nets.len(),
        netlist.is_ncl
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
                    println!("{:.6} OK", result);
                    passed += 1;
                } else {
                    println!(
                        "{:.6} FAIL (expected {:.6}, error={:.2e})",
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

/// Synchronous (non-NCL) gate-level test for FP32 addition.
/// This isolates whether the bug is in NCL boundary expansion or gate-level synthesis.
#[test]
fn test_fp32_add_sync() {
    let source_path =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/test_fpadd_sync.sk");

    println!("Compiling FP32 add test (sync, no NCL)...");
    let netlist = compile_fp32_sync_test(&source_path);
    println!(
        "Compiled: {} cells, {} nets, is_ncl={}",
        netlist.cells.len(),
        netlist.nets.len(),
        netlist.is_ncl
    );

    let test_cases: Vec<(f32, f32, f32)> = vec![
        (2.0f32, 3.0f32, 5.0f32),
        (1.0f32, 1.0f32, 2.0f32),
        (10.0f32, -3.0f32, 7.0f32),
        (0.5f32, 0.25f32, 0.75f32),
        (1.5f32, 2.5f32, 4.0f32),
        // Edge cases
        (0.0f32, 5.0f32, 5.0f32),
        (5.0f32, -5.0f32, 0.0f32),
        (1.0f32, 1e-7f32, 1.0f32),  // large exp_diff
        (1e10f32, 1.0f32, 1e10f32), // large exp_diff
        (-1.0f32, -2.0f32, -3.0f32),
    ];

    let mut passed = 0;
    let mut failed = 0;

    for (a, b, expected) in &test_cases {
        let (a, b, expected) = (*a, *b, *expected);
        print!("  {:.2} + {:.2} = ", a, b);

        let a_bits = a.to_bits();
        let b_bits = b.to_bits();

        match evaluate_sync_gate_netlist(&netlist, a_bits, b_bits) {
            Some(result_bits) => {
                let result = f32::from_bits(result_bits);
                let error = (result - expected).abs();
                let relative_error = if expected != 0.0 {
                    error / expected.abs()
                } else {
                    error
                };

                if relative_error < 1e-6 || (result == expected) {
                    println!("{:.6} OK", result);
                    passed += 1;
                } else {
                    println!(
                        "{:.6} FAIL (expected {:.6}, bits=0x{:08X}, error={:.2e})",
                        result, expected, result_bits, relative_error
                    );
                    // Decode the FP32 fields
                    let sign = (result_bits >> 31) & 1;
                    let exp = (result_bits >> 23) & 0xFF;
                    let frac = result_bits & 0x7FFFFF;
                    println!(
                        "    Decoded: sign={}, exp={} (bias={}), frac=0x{:06X}",
                        sign,
                        exp,
                        exp as i32 - 127,
                        frac
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
    assert_eq!(failed, 0, "Some sync FP32 add tests failed");
}

#[tokio::test]
async fn test_fp32_add_edge_cases() {
    let source_path =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/test_fpadd_sim.sk");

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
                let tolerance = expected.abs() * 1e-6 + 1e-7;

                if error <= tolerance {
                    println!("OK ({:.6})", result);
                    passed += 1;
                } else {
                    println!("FAIL got {:.6}, expected {:.6}", result, expected);
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

/// Test that BitSelect and simple if-else chains compile correctly
#[test]
fn test_bitselect_isolation() {
    let source_path =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/test_bitselect.sk");

    println!("Compiling BitSelect test...");
    let netlist = compile_fp32_sync_test(&source_path);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // Test with 0xA0000000 (bits 31 and 29 set)
    let test_input: u32 = 0xA0000000;
    println!("Testing with input 0x{:08X} (bits 31,29 set)", test_input);

    let config = NclSimConfig {
        max_iterations: 10000,
        debug: false,
        track_stages: false,
    };
    let mut sim = NclSimulator::new(netlist.clone(), config);

    // Set input bits
    for bit in 0..32u32 {
        let val = (test_input >> bit) & 1 != 0;
        let name = format!("top.x[{}]", bit);
        for net in &netlist.nets {
            if net.name == name && net.is_input {
                sim.set_net(net.id, val);
            }
        }
    }

    let iterations = sim.run_until_stable(10000);
    println!("Converged after {} iterations", iterations);

    // Read outputs
    let read_output = |prefix: &str, width: usize| -> u64 {
        let mut val = 0u64;
        for bit in 0..width {
            let name = format!("{}[{}]", prefix, bit);
            for net in &netlist.nets {
                if net.name == name {
                    let resolved = netlist.resolve_alias(net.id);
                    if sim.get_net(resolved) {
                        val |= 1u64 << bit;
                    }
                }
            }
        }
        val
    };

    let read_1bit = |name: &str| -> bool {
        for net in &netlist.nets {
            if net.name == name {
                let resolved = netlist.resolve_alias(net.id);
                return sim.get_net(resolved);
            }
        }
        false
    };

    let bit31 = read_1bit("top.bit31");
    let bit30 = read_1bit("top.bit30");
    let bit0 = read_1bit("top.bit0");
    let result = read_output("top.result", 32);

    println!("bit31 = {} (expected true)", bit31);
    println!("bit30 = {} (expected false)", bit30);
    println!("bit0 = {} (expected false)", bit0);
    println!("result = {} (expected 0, since x[31]=1)", result);

    assert!(bit31, "x[31] should be 1 for input 0xA0000000");
    assert!(!bit30, "x[30] should be 0 for input 0xA0000000");
    assert!(!bit0, "x[0] should be 0 for input 0xA0000000");
    assert_eq!(result, 0, "if x[31] should return 0");
}
