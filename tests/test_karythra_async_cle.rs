//! Karythra Async CLE Integration Tests
//!
//! Tests the asynchronous (NCL) version of the Karythra CLE.
//! Verifies that all function unit levels (L0-L5) work correctly
//! with NCL dual-rail encoding and completion detection.
//!
//! Note: These tests compile a large circuit and may stack overflow in debug mode.
//! Run with `cargo test --release` for reliable execution.

// Skip all tests in this file in debug mode due to stack overflow during compilation
#![cfg(not(debug_assertions))]

use skalp_frontend::parse_and_build_compilation_context;
use skalp_lir::gate_netlist::GateNetlist;
use skalp_lir::{get_stdlib_library, lower_mir_hierarchical, map_hierarchical_to_gates};
use skalp_mir::MirCompiler;
use skalp_sim::{CircuitMode, HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
use std::path::Path;
use std::sync::OnceLock;

/// Cached compiled netlist - compile once, share across all tests
static COMPILED_CLE: OnceLock<GateNetlist> = OnceLock::new();

/// Get or compile the Karythra async CLE to gate netlist
/// Uses OnceLock to ensure compilation happens only once across all tests
fn compile_karythra_async_cle() -> &'static GateNetlist {
    COMPILED_CLE.get_or_init(|| {
        println!("🔧 Compiling Karythra async CLE (one-time)...");
        let start = std::time::Instant::now();

        // Set up module search path BEFORE parsing
        // This must be set before the module resolver is initialized
        std::env::set_var(
            "SKALP_STDLIB_PATH",
            "/Users/girivs/src/hw/karythra/rtl/skalp/cle/lib",
        );

        // Use file-based parsing which properly handles module resolution
        let cle_path = Path::new("/Users/girivs/src/hw/karythra/rtl/skalp/cle/src/main_async.sk");

        if !cle_path.exists() {
            panic!("Karythra async CLE not found at {:?}", cle_path);
        }

        // BUG #176 FIX: Use parse_and_build_compilation_context and compile_to_mir_with_modules
        // to properly resolve imported enum discriminant values
        let context =
            parse_and_build_compilation_context(cle_path).expect("Failed to parse async CLE");
        let mir_compiler = MirCompiler::new();
        let mir = mir_compiler
            .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
            .expect("Failed to compile to MIR");

        assert!(!mir.modules.is_empty(), "Should have at least one module");

        // Use hierarchical compilation (depth >= 5 instantiates submodules)
        let hier_lir = lower_mir_hierarchical(&mir);
        let library = get_stdlib_library("generic_asic").expect("Failed to load library");
        let hier_result = map_hierarchical_to_gates(&hier_lir, &library);
        let netlist = hier_result.flatten();

        println!(
            "✅ Compiled in {:.2}s: {} cells, {} nets",
            start.elapsed().as_secs_f64(),
            netlist.cells.len(),
            netlist.nets.len()
        );

        netlist
    })
}

/// Test NCL simulation with expected outputs using UnifiedSimulator (GPU-accelerated)
fn test_async_cle_operation(
    netlist: &skalp_lir::gate_netlist::GateNetlist,
    opcode: u64,
    data1: u64,
    data2: u64,
    expected_result: u64,
    description: &str,
) -> bool {
    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Auto, // Use GPU if available
        max_iterations: 100000,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist.clone())
        .expect("Failed to load NCL netlist");

    println!(
        "  Using: {} ({} cells, {} nets)",
        sim.device_info(),
        netlist.cells.len(),
        netlist.nets.len()
    );

    // Debug: Check what signals are available (first test only)
    static DEBUG_PRINTED: std::sync::Once = std::sync::Once::new();
    DEBUG_PRINTED.call_once(|| {
        let signals = sim.ncl_signal_names();
        let top_signals: Vec<_> = signals
            .iter()
            .filter(|s| s.starts_with("top."))
            .filter(|s| {
                s.contains("function_sel")
                    || s.contains("route_sel")
                    || s.contains("data1")
                    || s.contains("data2")
                    || s.contains("result")
            })
            .take(30)
            .collect();
        println!("  [DEBUG] Sample top signals: {:?}", top_signals);
        println!(
            "  [DEBUG] Has top.function_sel: {}",
            sim.has_ncl_signal("top.function_sel")
        );
        println!(
            "  [DEBUG] Has top.data1: {}",
            sim.has_ncl_signal("top.data1")
        );
        println!(
            "  [DEBUG] Has top.result: {}",
            sim.has_ncl_signal("top.result")
        );
    });

    // Set function_sel (6 bits) - use full hierarchical path
    sim.set_ncl_input("top.function_sel", opcode, 6);

    // Set route_sel (3 bits) - use register writeback mode
    sim.set_ncl_input("top.route_sel", 0, 3);

    // Set data1 (256 bits, but we only use lower 32)
    sim.set_ncl_input("top.data1", data1, 256);

    // Set data2 (256 bits, but we only use lower 32)
    sim.set_ncl_input("top.data2", data2, 256);

    println!("  [op={}] Running NCL simulation...", opcode);

    // Run until stable
    let result = sim.run_until_stable();

    if !result.is_stable {
        println!(
            "  {} FAIL: Did not converge after {} iterations",
            description, result.iterations
        );
        return false;
    }

    println!(
        "  Converged in {} iterations (GPU: {})",
        result.iterations, result.used_gpu
    );

    // Get result (lower 32 bits) - use full hierarchical path
    match sim.get_ncl_output("top.result", 32) {
        Some(actual) => {
            let pass = (actual & 0xFFFFFFFF) == (expected_result & 0xFFFFFFFF);
            if pass {
                println!("  {} PASS: result=0x{:08X}", description, actual);
            } else {
                println!(
                    "  {} FAIL: expected 0x{:08X}, got 0x{:08X}",
                    description, expected_result, actual
                );
            }
            pass
        }
        None => {
            println!("  {} FAIL: result is NULL or invalid", description);
            false
        }
    }
}

/// Test L2 FP32 operation using UnifiedSimulator
/// Returns (pass, Option<result_bits>)
fn test_l2_fp32_operation(
    netlist: &GateNetlist,
    opcode: u64,
    a_bits: u64,
    b_bits: u64,
) -> Option<u64> {
    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Auto,
        max_iterations: 100000,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist.clone())
        .expect("Failed to load NCL netlist");

    // Set inputs with "top." prefix
    sim.set_ncl_input("top.function_sel", opcode, 6);
    sim.set_ncl_input("top.route_sel", 0, 3);
    // data1[31:0] = a, data1[63:32] = b
    let data1 = a_bits | (b_bits << 32);
    sim.set_ncl_input("top.data1", data1, 256);
    sim.set_ncl_input("top.data2", 0, 256);

    let result = sim.run_until_stable();
    if !result.is_stable {
        println!("  Did not converge after {} iterations", result.iterations);
        return None;
    }

    // Read from debug_l2
    sim.get_ncl_output("top.debug_l2", 32)
}

/// Test L3 Vec3 operation using UnifiedSimulator
/// Uses u128 for data1/data2 to support 96-bit vector values (3 x 32-bit floats)
fn test_l3_vec3_operation(
    netlist: &GateNetlist,
    opcode: u64,
    data1: u128,
    data2: u128,
) -> Option<u64> {
    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Auto,
        max_iterations: 100000,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist.clone())
        .expect("Failed to load NCL netlist");

    sim.set_ncl_input("top.function_sel", opcode, 6);
    sim.set_ncl_input("top.route_sel", 0, 3);
    sim.set_ncl_input_u128("top.data1", data1, 256);
    sim.set_ncl_input_u128("top.data2", data2, 256);

    let result = sim.run_until_stable();
    if !result.is_stable {
        println!("  Did not converge after {} iterations", result.iterations);
        return None;
    }

    // Read from debug_l3
    sim.get_ncl_output("top.debug_l3", 32)
}

/// Test L5 bit operation using UnifiedSimulator
fn test_l5_bit_operation(netlist: &GateNetlist, opcode: u64, data1: u64) -> Option<u64> {
    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Auto,
        max_iterations: 100000,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist.clone())
        .expect("Failed to load NCL netlist");

    sim.set_ncl_input("top.function_sel", opcode, 6);
    sim.set_ncl_input("top.route_sel", 0, 3);
    sim.set_ncl_input("top.data1", data1, 256);
    sim.set_ncl_input("top.data2", 0, 256);

    let result = sim.run_until_stable();
    if !result.is_stable {
        println!("  Did not converge after {} iterations", result.iterations);
        return None;
    }

    // Read from debug_l4_l5 (L4 and L5 share this debug output)
    sim.get_ncl_output("top.debug_l4_l5", 32)
}

// =============================================================================
// Debug: Signal check test
// =============================================================================

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_signal_check() {
    println!("\n=== Signal Check Test ===");
    let netlist = compile_karythra_async_cle();

    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Auto,
        max_iterations: 10, // Very short - just checking signals exist
        ncl_debug: false,
        ..Default::default()
    };

    let sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    // Note: Don't load netlist yet, we want to load it first
    let mut sim = sim;
    sim.load_ncl_gate_level(netlist.clone())
        .expect("Failed to load NCL netlist");

    println!("Total signals: {}", sim.ncl_signal_names().len());

    // Check key signals
    let key_signals = [
        "top.function_sel",
        "top.route_sel",
        "top.data1",
        "top.data2",
        "top.result",
    ];

    for signal in key_signals {
        let exists = sim.has_ncl_signal(signal);
        println!(
            "  {}: {}",
            signal,
            if exists { "EXISTS" } else { "MISSING" }
        );
    }

    // Print some matching signals
    let signals = sim.ncl_signal_names();
    println!("\nSample signals starting with 'top.':");
    for sig in signals.iter().filter(|s| s.starts_with("top.")).take(10) {
        println!("  {}", sig);
    }

    // Check if we have the expected top-level signals
    assert!(sim.has_ncl_signal("top.data1"), "top.data1 should exist");
    assert!(sim.has_ncl_signal("top.data2"), "top.data2 should exist");
    println!("\nSignal check passed!");
}

// =============================================================================
// L0-L1 Tests (Basic 8-bit Operations)
// =============================================================================

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_async_cle_l0_add() {
    println!("\n=== Async CLE L0 ADD Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // L0L1Opcode::ADD_8 = 0
    let test_cases: [(u64, u64, u64); 4] = [(10, 20, 30), (100, 50, 150), (255, 1, 256), (0, 0, 0)];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        let desc = format!("{} + {} = {}", a, b, expected);
        let pass = test_async_cle_operation(netlist, 0, a, b, expected, &desc);
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "Async CLE L0 ADD tests failed");
}

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_async_cle_l0_sub() {
    println!("\n=== Async CLE L0 SUB Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // L0L1Opcode::SUB_8 = 1
    let test_cases: [(u64, u64, u64); 3] = [(50, 20, 30), (100, 100, 0), (255, 1, 254)];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        let desc = format!("{} - {} = {}", a, b, expected);
        let pass = test_async_cle_operation(netlist, 1, a, b, expected, &desc);
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "Async CLE L0 SUB tests failed");
}

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_async_cle_l0_and() {
    println!("\n=== Async CLE L0 AND Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // L0L1Opcode::AND_8 = 3
    let test_cases: [(u64, u64, u64); 4] = [
        (0xFF, 0xFF, 0xFF),
        (0xAA, 0x55, 0x00),
        (0xF0, 0x0F, 0x00),
        (0xFF, 0x0F, 0x0F),
    ];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        let desc = format!("0x{:02X} & 0x{:02X} = 0x{:02X}", a, b, expected);
        let pass = test_async_cle_operation(netlist, 3, a, b, expected, &desc);
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "Async CLE L0 AND tests failed");
}

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_async_cle_l0_or() {
    println!("\n=== Async CLE L0 OR Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // L0L1Opcode::OR_8 = 4
    let test_cases: [(u64, u64, u64); 4] = [
        (0x00, 0x00, 0x00),
        (0xAA, 0x55, 0xFF),
        (0xF0, 0x0F, 0xFF),
        (0xFF, 0x00, 0xFF),
    ];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        let desc = format!("0x{:02X} | 0x{:02X} = 0x{:02X}", a, b, expected);
        let pass = test_async_cle_operation(netlist, 4, a, b, expected, &desc);
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "Async CLE L0 OR tests failed");
}

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_async_cle_l0_xor() {
    println!("\n=== Async CLE L0 XOR Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // L0L1Opcode::XOR_8 = 5
    let test_cases: [(u64, u64, u64); 4] = [
        (0x00, 0x00, 0x00),
        (0xFF, 0xFF, 0x00),
        (0xAA, 0x55, 0xFF),
        (0xF0, 0x0F, 0xFF),
    ];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        let desc = format!("0x{:02X} ^ 0x{:02X} = 0x{:02X}", a, b, expected);
        let pass = test_async_cle_operation(netlist, 5, a, b, expected, &desc);
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "Async CLE L0 XOR tests failed");
}

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_async_cle_l0_comparisons() {
    println!("\n=== Async CLE L0 Comparison Tests ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let mut all_pass = true;

    // L0L1Opcode::EQ_8 = 10
    println!("\nEQ tests:");
    all_pass = all_pass && test_async_cle_operation(netlist, 10, 42, 42, 1, "42 == 42");
    all_pass = all_pass && test_async_cle_operation(netlist, 10, 42, 43, 0, "42 == 43");

    // L0L1Opcode::LT_8 = 12
    println!("\nLT tests:");
    all_pass = all_pass && test_async_cle_operation(netlist, 12, 10, 20, 1, "10 < 20");
    all_pass = all_pass && test_async_cle_operation(netlist, 12, 20, 10, 0, "20 < 10");
    all_pass = all_pass && test_async_cle_operation(netlist, 12, 10, 10, 0, "10 < 10");

    assert!(all_pass, "Async CLE L0 comparison tests failed");
}

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_async_cle_l0_shifts() {
    println!("\n=== Async CLE L0 Shift Tests ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let mut all_pass = true;

    // L0L1Opcode::SLL_8 = 7 (shift left logical)
    println!("\nSLL tests:");
    all_pass = all_pass && test_async_cle_operation(netlist, 7, 0x01, 1, 0x02, "0x01 << 1");
    all_pass = all_pass && test_async_cle_operation(netlist, 7, 0x01, 4, 0x10, "0x01 << 4");

    // L0L1Opcode::SRL_8 = 8 (shift right logical)
    println!("\nSRL tests:");
    all_pass = all_pass && test_async_cle_operation(netlist, 8, 0x80, 1, 0x40, "0x80 >> 1");
    all_pass = all_pass && test_async_cle_operation(netlist, 8, 0xF0, 4, 0x0F, "0xF0 >> 4");

    assert!(all_pass, "Async CLE L0 shift tests failed");
}

// =============================================================================
// L2 Tests (FP32 Operations)
// =============================================================================

/// Helper to convert f32 to bits for testing
fn f32_to_bits(f: f32) -> u64 {
    f.to_bits() as u64
}

/// Helper to convert bits to f32 for result checking
fn bits_to_f32(bits: u64) -> f32 {
    f32::from_bits(bits as u32)
}

/// Check if two f32 values are approximately equal (within tolerance)
fn f32_approx_eq(a: f32, b: f32, tolerance: f32) -> bool {
    (a - b).abs() < tolerance
}

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_async_cle_l2_fp32_add() {
    println!("\n=== Async CLE L2 FP32 ADD Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let mut all_pass = true;

    // L2Opcode::FP32_ADD = 23
    // Test: 2.0 + 3.0 = 5.0
    {
        let a = f32_to_bits(2.0);
        let b = f32_to_bits(3.0);
        if let Some(result_bits) = test_l2_fp32_operation(netlist, 23, a, b) {
            let result = bits_to_f32(result_bits);
            let pass = f32_approx_eq(result, 5.0, 0.001);
            println!(
                "FP32 ADD: 2.0 + 3.0 = {} (expected 5.0) [{}]",
                result,
                if pass { "PASS" } else { "FAIL" }
            );
            all_pass = all_pass && pass;
        } else {
            println!("FP32 ADD: 2.0 + 3.0 result is NULL [FAIL]");
            all_pass = false;
        }
    }

    // Test: -1.5 + 2.5 = 1.0
    {
        let a = f32_to_bits(-1.5);
        let b = f32_to_bits(2.5);
        if let Some(result_bits) = test_l2_fp32_operation(netlist, 23, a, b) {
            let result = bits_to_f32(result_bits);
            let pass = f32_approx_eq(result, 1.0, 0.001);
            println!(
                "FP32 ADD: -1.5 + 2.5 = {} (expected 1.0) [{}]",
                result,
                if pass { "PASS" } else { "FAIL" }
            );
            all_pass = all_pass && pass;
        } else {
            println!("FP32 ADD: -1.5 + 2.5 result is NULL [FAIL]");
            all_pass = false;
        }
    }

    assert!(all_pass, "Async CLE L2 FP32 ADD tests failed");
}

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_async_cle_l2_fp32_mul() {
    println!("\n=== Async CLE L2 FP32 MUL Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let mut all_pass = true;

    // L2Opcode::FP32_MUL = 24
    // Test: 3.0 * 4.0 = 12.0
    {
        let a = f32_to_bits(3.0);
        let b = f32_to_bits(4.0);
        if let Some(result_bits) = test_l2_fp32_operation(netlist, 24, a, b) {
            let result = bits_to_f32(result_bits);
            let pass = f32_approx_eq(result, 12.0, 0.001);
            println!(
                "FP32 MUL: 3.0 * 4.0 = {} (expected 12.0) [{}]",
                result,
                if pass { "PASS" } else { "FAIL" }
            );
            all_pass = all_pass && pass;
        } else {
            println!("FP32 MUL: 3.0 * 4.0 result is NULL [FAIL]");
            all_pass = false;
        }
    }

    // Test: 2.5 * -2.0 = -5.0
    {
        let a = f32_to_bits(2.5);
        let b = f32_to_bits(-2.0);
        if let Some(result_bits) = test_l2_fp32_operation(netlist, 24, a, b) {
            let result = bits_to_f32(result_bits);
            let pass = f32_approx_eq(result, -5.0, 0.001);
            println!(
                "FP32 MUL: 2.5 * -2.0 = {} (expected -5.0) [{}]",
                result,
                if pass { "PASS" } else { "FAIL" }
            );
            all_pass = all_pass && pass;
        } else {
            println!("FP32 MUL: 2.5 * -2.0 result is NULL [FAIL]");
            all_pass = false;
        }
    }

    assert!(all_pass, "Async CLE L2 FP32 MUL tests failed");
}

// =============================================================================
// L3 Tests (Vector Operations)
// =============================================================================

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_async_cle_l3_vec3_add() {
    println!("\n=== Async CLE L3 VEC3 ADD Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let mut all_pass = true;

    // L3Opcode::VEC3_ADD = 32
    // Test: (1.0, 2.0, 3.0) + (4.0, 5.0, 6.0) = (5.0, 7.0, 9.0)
    {
        // data1 = vec A: ax[31:0], ay[63:32], az[95:64]
        let ax = f32_to_bits(1.0) as u128;
        let ay = f32_to_bits(2.0) as u128;
        let az = f32_to_bits(3.0) as u128;
        let data1 = ax | (ay << 32) | (az << 64);

        // data2 = vec B: bx[31:0], by[63:32], bz[95:64]
        let bx = f32_to_bits(4.0) as u128;
        let by = f32_to_bits(5.0) as u128;
        let bz = f32_to_bits(6.0) as u128;
        let data2 = bx | (by << 32) | (bz << 64);

        // Check debug_l3 (lower 32 bits = rx)
        if let Some(result_bits) = test_l3_vec3_operation(netlist, 32, data1, data2) {
            let rx = bits_to_f32(result_bits);
            let pass = f32_approx_eq(rx, 5.0, 0.01);
            println!(
                "VEC3 ADD: rx = {} (expected 5.0) [{}]",
                rx,
                if pass { "PASS" } else { "FAIL" }
            );
            all_pass = all_pass && pass;
        } else {
            println!("VEC3 ADD: result is NULL [FAIL]");
            all_pass = false;
        }
    }

    assert!(all_pass, "Async CLE L3 VEC3 ADD tests failed");
}

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_async_cle_l3_vec3_dot() {
    println!("\n=== Async CLE L3 VEC3 DOT Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let mut all_pass = true;

    // L3Opcode::VEC3_DOT = 35
    // Test: (1.0, 2.0, 3.0) · (4.0, 5.0, 6.0) = 1*4 + 2*5 + 3*6 = 32.0
    {
        // data1 = vec A: ax[31:0], ay[63:32], az[95:64]
        let ax = f32_to_bits(1.0) as u128;
        let ay = f32_to_bits(2.0) as u128;
        let az = f32_to_bits(3.0) as u128;
        let data1 = ax | (ay << 32) | (az << 64);

        // data2 = vec B: bx[31:0], by[63:32], bz[95:64]
        let bx = f32_to_bits(4.0) as u128;
        let by = f32_to_bits(5.0) as u128;
        let bz = f32_to_bits(6.0) as u128;
        let data2 = bx | (by << 32) | (bz << 64);

        if let Some(result_bits) = test_l3_vec3_operation(netlist, 35, data1, data2) {
            let dot = bits_to_f32(result_bits);
            let pass = f32_approx_eq(dot, 32.0, 0.1);
            println!(
                "VEC3 DOT: (1,2,3)·(4,5,6) = {} (expected 32.0) [{}]",
                dot,
                if pass { "PASS" } else { "FAIL" }
            );
            all_pass = all_pass && pass;
        } else {
            println!("VEC3 DOT: result is NULL [FAIL]");
            all_pass = false;
        }
    }

    assert!(all_pass, "Async CLE L3 VEC3 DOT tests failed");
}

// =============================================================================
// L4 Tests (Algorithm Operations - Quadratic, Bezier, Ray-AABB)
// =============================================================================

/// Test L4 algorithm operation using UnifiedSimulator
/// Returns the debug_l4_l5 output (lower 32 bits of result)
/// data1 and data2 are passed as (low_128bits, high_128bits) tuples
fn test_l4_algorithm_operation(
    netlist: &GateNetlist,
    opcode: u64,
    data1: (u128, u128),
    data2: (u128, u128),
) -> Option<u64> {
    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Auto,
        max_iterations: 100000,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist.clone())
        .expect("Failed to load NCL netlist");

    sim.set_ncl_input("top.function_sel", opcode, 6);
    sim.set_ncl_input("top.route_sel", 0, 3);
    sim.set_ncl_input_u256("top.data1", data1.0, data1.1, 256);
    sim.set_ncl_input_u256("top.data2", data2.0, data2.1, 256);

    let result = sim.run_until_stable();
    if !result.is_stable {
        println!("  Did not converge after {} iterations", result.iterations);
        return None;
    }

    // Read from debug_l4_l5 (L4 and L5 share this debug output)
    sim.get_ncl_output("top.debug_l4_l5", 32)
}

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_async_cle_l4_quadratic() {
    println!("\n=== Async CLE L4 QUADRATIC Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let mut all_pass = true;

    // L4Opcode::QUADRATIC = 45
    // Solve x² - 5x + 6 = 0 -> roots are x=2 and x=3
    // a=1.0, b=-5.0, c=6.0
    {
        let a = f32_to_bits(1.0) as u128;
        let b = f32_to_bits(-5.0) as u128;
        let c = f32_to_bits(6.0) as u128;
        // Pack into data1: a[31:0], b[63:32], c[95:64]
        let data1_low = a | (b << 32) | (c << 64);

        // Result format from quadratic_solve: (valid, x1, x2) where x1=2.0, x2=3.0
        // debug_l4_l5 captures lower 32 bits which should be x1
        if let Some(result_bits) = test_l4_algorithm_operation(&netlist, 45, (data1_low, 0), (0, 0))
        {
            let x1 = bits_to_f32(result_bits);
            // x1 should be 2.0 (the smaller root: (-b - sqrt(disc))/2a)
            let pass = f32_approx_eq(x1, 2.0, 0.1);
            println!(
                "QUADRATIC x² - 5x + 6 = 0: x1 = {} (expected 2.0) [{}]",
                x1,
                if pass { "PASS" } else { "FAIL" }
            );
            all_pass = all_pass && pass;
        } else {
            println!("QUADRATIC: result is NULL [FAIL]");
            all_pass = false;
        }
    }

    assert!(all_pass, "Async CLE L4 QUADRATIC tests failed");
}

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_async_cle_l4_bezier() {
    println!("\n=== Async CLE L4 BEZIER Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let mut all_pass = true;

    // L4Opcode::BEZIER = 46
    // Evaluate cubic Bezier curve at t
    // P(t) = (1-t)³p0 + 3(1-t)²tp1 + 3(1-t)t²p2 + t³p3
    {
        // Simple test: linear-like Bezier with p0=0, p1=1, p2=2, p3=3 at t=0.5
        // Expected: close to 1.5 (midpoint of curve)
        let p0 = f32_to_bits(0.0) as u128;
        let p1 = f32_to_bits(1.0) as u128;
        let p2 = f32_to_bits(2.0) as u128;
        let p3 = f32_to_bits(3.0) as u128;
        let t = f32_to_bits(0.5) as u128;
        // Pack into data1: p0[31:0], p1[63:32], p2[95:64], p3[127:96] in low, t[31:0] in high
        let data1_low = p0 | (p1 << 32) | (p2 << 64) | (p3 << 96);
        let data1_high = t; // t goes in bits [159:128], which is bits [31:0] of the high word

        if let Some(result_bits) =
            test_l4_algorithm_operation(&netlist, 46, (data1_low, data1_high), (0, 0))
        {
            let result = bits_to_f32(result_bits);
            // At t=0.5: (1-0.5)³×0 + 3×(1-0.5)²×0.5×1 + 3×(1-0.5)×0.5²×2 + 0.5³×3
            // = 0 + 3×0.25×0.5×1 + 3×0.5×0.25×2 + 0.125×3
            // = 0.375 + 0.75 + 0.375 = 1.5
            let pass = f32_approx_eq(result, 1.5, 0.1);
            println!(
                "BEZIER p0=0,p1=1,p2=2,p3=3,t=0.5: result = {} (expected 1.5) [{}]",
                result,
                if pass { "PASS" } else { "FAIL" }
            );
            all_pass = all_pass && pass;
        } else {
            println!("BEZIER: result is NULL [FAIL]");
            all_pass = false;
        }
    }

    // Test at t=0: should equal p0
    {
        let p0 = f32_to_bits(5.0) as u128;
        let p1 = f32_to_bits(10.0) as u128;
        let p2 = f32_to_bits(15.0) as u128;
        let p3 = f32_to_bits(20.0) as u128;
        let t = f32_to_bits(0.0) as u128;
        let data1_low = p0 | (p1 << 32) | (p2 << 64) | (p3 << 96);
        let data1_high = t;

        if let Some(result_bits) =
            test_l4_algorithm_operation(&netlist, 46, (data1_low, data1_high), (0, 0))
        {
            let result = bits_to_f32(result_bits);
            let pass = f32_approx_eq(result, 5.0, 0.01);
            println!(
                "BEZIER t=0: result = {} (expected 5.0) [{}]",
                result,
                if pass { "PASS" } else { "FAIL" }
            );
            all_pass = all_pass && pass;
        } else {
            println!("BEZIER t=0: result is NULL [FAIL]");
            all_pass = false;
        }
    }

    // Test at t=1: should equal p3
    {
        let p0 = f32_to_bits(5.0) as u128;
        let p1 = f32_to_bits(10.0) as u128;
        let p2 = f32_to_bits(15.0) as u128;
        let p3 = f32_to_bits(20.0) as u128;
        let t = f32_to_bits(1.0) as u128;
        let data1_low = p0 | (p1 << 32) | (p2 << 64) | (p3 << 96);
        let data1_high = t;

        if let Some(result_bits) =
            test_l4_algorithm_operation(&netlist, 46, (data1_low, data1_high), (0, 0))
        {
            let result = bits_to_f32(result_bits);
            let pass = f32_approx_eq(result, 20.0, 0.01);
            println!(
                "BEZIER t=1: result = {} (expected 20.0) [{}]",
                result,
                if pass { "PASS" } else { "FAIL" }
            );
            all_pass = all_pass && pass;
        } else {
            println!("BEZIER t=1: result is NULL [FAIL]");
            all_pass = false;
        }
    }

    assert!(all_pass, "Async CLE L4 BEZIER tests failed");
}

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_async_cle_l4_ray_aabb() {
    println!("\n=== Async CLE L4 RAY_AABB Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let mut all_pass = true;

    // L4Opcode::RAY_AABB = 47
    // Ray-AABB intersection test
    // Inputs: ray origin (3x fp32), ray dir (3x fp32), AABB min (3x fp32), AABB max (3x fp32)
    // Returns: (hit, t_near, t_far)

    // Test case: Ray pointing at unit cube centered at origin
    // Use non-zero direction components to avoid division by zero in slab calculations
    {
        // Ray origin: (-2, 0, 0) - starting from -X side
        let ray_ox = f32_to_bits(-2.0) as u128;
        let ray_oy = f32_to_bits(0.0) as u128;
        let ray_oz = f32_to_bits(0.0) as u128;
        // Ray direction: (1, 0.1, 0.1) - mostly +X with slight Y/Z to avoid inf
        let ray_dx = f32_to_bits(1.0) as u128;
        let ray_dy = f32_to_bits(0.1) as u128;
        let ray_dz = f32_to_bits(0.1) as u128;

        // AABB from (-1, -1, -1) to (1, 1, 1) - unit cube at origin
        let aabb_min_x = f32_to_bits(-1.0) as u128;
        let aabb_min_y = f32_to_bits(-1.0) as u128;
        let aabb_min_z = f32_to_bits(-1.0) as u128;
        let aabb_max_x = f32_to_bits(1.0) as u128;
        let aabb_max_y = f32_to_bits(1.0) as u128;
        let aabb_max_z = f32_to_bits(1.0) as u128;

        // Pack into data1: ray_ox, ray_oy, ray_oz, ray_dx (128 bits low), ray_dy, ray_dz (64 bits high)
        let data1_low = ray_ox | (ray_oy << 32) | (ray_oz << 64) | (ray_dx << 96);
        let data1_high = ray_dy | (ray_dz << 32);

        // Pack into data2: aabb_min_x, aabb_min_y, aabb_min_z, aabb_max_x (128 bits low), aabb_max_y, aabb_max_z (64 bits high)
        let data2_low = aabb_min_x | (aabb_min_y << 32) | (aabb_min_z << 64) | (aabb_max_x << 96);
        let data2_high = aabb_max_y | (aabb_max_z << 32);

        // Expected: hit=1, t_near=1.0 (ray from (-2,0,0) dir (1,0.1,0.1) hits box at x=-1, t=1)
        // The X slab: tmin_x = (-1 - (-2))/1 = 1, tmax_x = (1 - (-2))/1 = 3
        // Y slab: tmin_y = (-1 - 0)/0.1 = -10, tmax_y = (1 - 0)/0.1 = 10
        // Z slab: tmin_z = (-1 - 0)/0.1 = -10, tmax_z = (1 - 0)/0.1 = 10
        // t_near = max(1, -10, -10) = 1
        if let Some(result_bits) = test_l4_algorithm_operation(
            &netlist,
            47,
            (data1_low, data1_high),
            (data2_low, data2_high),
        ) {
            let t_near = bits_to_f32(result_bits);
            // t_near should be 1.0
            let pass = f32_approx_eq(t_near, 1.0, 0.1);
            println!(
                "RAY_AABB ray=(-2,0,0)->(1,0.1,0.1) box [-1,1]³: t_near = {} (expected 1.0) [{}]",
                t_near,
                if pass { "PASS" } else { "FAIL" }
            );
            all_pass = all_pass && pass;
        } else {
            println!("RAY_AABB: result is NULL [FAIL]");
            all_pass = false;
        }
    }

    assert!(all_pass, "Async CLE L4 RAY_AABB tests failed");
}

// =============================================================================
// L5 Tests (Bitops Operations)
// =============================================================================

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_async_cle_l5_clz() {
    println!("\n=== Async CLE L5 CLZ Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let mut all_pass = true;

    // L5Opcode::CLZ = 55
    // Test cases for count leading zeros
    let test_cases = [
        (0x80000000u64, 0u64, "0x80000000 -> 0 leading zeros"),
        (0x00000001u64, 31u64, "0x00000001 -> 31 leading zeros"),
        (0x0000FFFFu64, 16u64, "0x0000FFFF -> 16 leading zeros"),
        (0x00010000u64, 15u64, "0x00010000 -> 15 leading zeros"),
    ];

    for (input, expected, desc) in test_cases {
        if let Some(result) = test_l5_bit_operation(netlist, 55, input) {
            let pass = result == expected;
            println!(
                "CLZ {}: got {} [{}]",
                desc,
                result,
                if pass { "PASS" } else { "FAIL" }
            );
            all_pass = all_pass && pass;
        } else {
            println!("CLZ {}: result is NULL [FAIL]", desc);
            all_pass = false;
        }
    }

    assert!(all_pass, "Async CLE L5 CLZ tests failed");
}

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_async_cle_l5_popcount() {
    println!("\n=== Async CLE L5 POPCOUNT Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let mut all_pass = true;

    // L5Opcode::POPCOUNT = 57
    let test_cases = [
        (0x00000000u64, 0u64, "0x00000000 -> 0 bits"),
        (0xFFFFFFFFu64, 32u64, "0xFFFFFFFF -> 32 bits"),
        (0x0000000Fu64, 4u64, "0x0000000F -> 4 bits"),
        (0xAAAAAAAAu64, 16u64, "0xAAAAAAAA -> 16 bits"),
        (0x12345678u64, 13u64, "0x12345678 -> 13 bits"),
    ];

    for (input, expected, desc) in test_cases {
        if let Some(result) = test_l5_bit_operation(netlist, 57, input) {
            let pass = result == expected;
            println!(
                "POPCOUNT {}: got {} [{}]",
                desc,
                result,
                if pass { "PASS" } else { "FAIL" }
            );
            all_pass = all_pass && pass;
        } else {
            println!("POPCOUNT {}: result is NULL [FAIL]", desc);
            all_pass = false;
        }
    }

    assert!(all_pass, "Async CLE L5 POPCOUNT tests failed");
}

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_async_cle_l5_bitreverse() {
    println!("\n=== Async CLE L5 BITREVERSE Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let mut all_pass = true;

    // L5Opcode::BITREVERSE = 58
    let test_cases = [
        (0x00000001u64, 0x80000000u64, "0x00000001 reversed"),
        (0x80000000u64, 0x00000001u64, "0x80000000 reversed"),
        (0x0F0F0F0Fu64, 0xF0F0F0F0u64, "0x0F0F0F0F reversed"),
    ];

    for (input, expected, desc) in test_cases {
        if let Some(result) = test_l5_bit_operation(netlist, 58, input) {
            let pass = result == expected;
            println!(
                "BITREVERSE {}: got 0x{:08X} (expected 0x{:08X}) [{}]",
                desc,
                result,
                expected,
                if pass { "PASS" } else { "FAIL" }
            );
            all_pass = all_pass && pass;
        } else {
            println!("BITREVERSE {}: result is NULL [FAIL]", desc);
            all_pass = false;
        }
    }

    assert!(all_pass, "Async CLE L5 BITREVERSE tests failed");
}

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_async_cle_l5_parity() {
    println!("\n=== Async CLE L5 PARITY Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let mut all_pass = true;

    // L5Opcode::PARITY = 62
    let test_cases = [
        (0x00000000u64, 0u64, "0x00000000 -> even parity"),
        (0x00000001u64, 1u64, "0x00000001 -> odd parity"),
        (0x00000003u64, 0u64, "0x00000003 -> even parity"),
        (0xFFFFFFFFu64, 0u64, "0xFFFFFFFF -> even parity (32 bits)"),
        (0x0000000Fu64, 0u64, "0x0000000F -> even parity (4 bits)"),
    ];

    for (input, expected, desc) in test_cases {
        if let Some(result) = test_l5_bit_operation(netlist, 62, input) {
            let pass = result == expected;
            println!(
                "PARITY {}: got {} [{}]",
                desc,
                result,
                if pass { "PASS" } else { "FAIL" }
            );
            all_pass = all_pass && pass;
        } else {
            println!("PARITY {}: result is NULL [FAIL]", desc);
            all_pass = false;
        }
    }

    assert!(all_pass, "Async CLE L5 PARITY tests failed");
}

// =============================================================================
// Quick smoke test for compilation
// =============================================================================

#[test]
#[ignore = "requires karythra CLE with FP trait imports"]
fn test_async_cle_compiles() {
    println!("\n=== Async CLE Compilation Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // Verify it's a reasonable size
    assert!(
        netlist.cells.len() > 1000,
        "CLE should have significant complexity"
    );
    assert!(netlist.nets.len() > 1000, "CLE should have many nets");

    // Verify NCL dual-rail structure exists
    let has_t_rails = netlist.nets.iter().any(|n| n.name.contains("_t["));
    let has_f_rails = netlist.nets.iter().any(|n| n.name.contains("_f["));
    assert!(has_t_rails, "Should have true rails");
    assert!(has_f_rails, "Should have false rails");

    println!("NCL structure verified: has true and false rails");
}
