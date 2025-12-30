//! Karythra Async CLE Integration Tests
//!
//! Tests the asynchronous (NCL) version of the Karythra CLE.
//! Verifies that all function unit levels (L0-L5) work correctly
//! with NCL dual-rail encoding and completion detection.

use skalp_frontend::parse_and_build_compilation_context;
use skalp_lir::gate_netlist::GateNetlist;
use skalp_lir::{get_stdlib_library, lower_mir_hierarchical, map_hierarchical_to_gates};
use skalp_mir::MirCompiler;
use skalp_sim::{
    CircuitMode, HwAccel, NclSimConfig, NclSimulator, SimLevel, UnifiedSimConfig, UnifiedSimulator,
};
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

    // Set function_sel (6 bits)
    sim.set_ncl_input("function_sel", opcode, 6);

    // Set route_sel (3 bits) - use register writeback mode
    sim.set_ncl_input("route_sel", 0, 3);

    // Set data1 (256 bits, but we only use lower 32)
    sim.set_ncl_input("data1", data1, 256);

    // Set data2 (256 bits, but we only use lower 32)
    sim.set_ncl_input("data2", data2, 256);

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

    // Get result (lower 32 bits)
    match sim.get_ncl_output("result", 32) {
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

// =============================================================================
// L0-L1 Tests (Basic 8-bit Operations)
// =============================================================================

#[test]
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
        let pass = test_async_cle_operation(&netlist, 0, a, b, expected, &desc);
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "Async CLE L0 ADD tests failed");
}

#[test]
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
        let pass = test_async_cle_operation(&netlist, 1, a, b, expected, &desc);
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "Async CLE L0 SUB tests failed");
}

#[test]
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
        let pass = test_async_cle_operation(&netlist, 3, a, b, expected, &desc);
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "Async CLE L0 AND tests failed");
}

#[test]
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
        let pass = test_async_cle_operation(&netlist, 4, a, b, expected, &desc);
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "Async CLE L0 OR tests failed");
}

#[test]
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
        let pass = test_async_cle_operation(&netlist, 5, a, b, expected, &desc);
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "Async CLE L0 XOR tests failed");
}

#[test]
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
    all_pass = all_pass && test_async_cle_operation(&netlist, 10, 42, 42, 1, "42 == 42");
    all_pass = all_pass && test_async_cle_operation(&netlist, 10, 42, 43, 0, "42 == 43");

    // L0L1Opcode::LT_8 = 12
    println!("\nLT tests:");
    all_pass = all_pass && test_async_cle_operation(&netlist, 12, 10, 20, 1, "10 < 20");
    all_pass = all_pass && test_async_cle_operation(&netlist, 12, 20, 10, 0, "20 < 10");
    all_pass = all_pass && test_async_cle_operation(&netlist, 12, 10, 10, 0, "10 < 10");

    assert!(all_pass, "Async CLE L0 comparison tests failed");
}

#[test]
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
    all_pass = all_pass && test_async_cle_operation(&netlist, 7, 0x01, 1, 0x02, "0x01 << 1");
    all_pass = all_pass && test_async_cle_operation(&netlist, 7, 0x01, 4, 0x10, "0x01 << 4");

    // L0L1Opcode::SRL_8 = 8 (shift right logical)
    println!("\nSRL tests:");
    all_pass = all_pass && test_async_cle_operation(&netlist, 8, 0x80, 1, 0x40, "0x80 >> 1");
    all_pass = all_pass && test_async_cle_operation(&netlist, 8, 0xF0, 4, 0x0F, "0xF0 >> 4");

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
fn test_async_cle_l2_fp32_add() {
    println!("\n=== Async CLE L2 FP32 ADD Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let config = NclSimConfig {
        max_iterations: 100000,
        debug: false,
        track_stages: false,
    };

    let mut all_pass = true;

    // L2Opcode::FP32_ADD = 23
    // Test: 2.0 + 3.0 = 5.0
    {
        let mut sim = NclSimulator::new(netlist.clone(), config.clone());
        sim.set_dual_rail_value("function_sel", 23, 6);
        sim.set_dual_rail_value("route_sel", 0, 3);
        // data1[31:0] = a, data1[63:32] = b
        let a = f32_to_bits(2.0);
        let b = f32_to_bits(3.0);
        sim.set_dual_rail_value("data1", a | (b << 32), 256);
        sim.set_dual_rail_value("data2", 0, 256);
        sim.run_until_stable(100000);

        if let Some(result_bits) = sim.get_dual_rail_value("debug_l2", 32) {
            let result = bits_to_f32(result_bits);
            let pass = f32_approx_eq(result, 5.0, 0.001);
            println!(
                "FP32 ADD: 2.0 + 3.0 = {} (expected 5.0) [{}]",
                result,
                if pass { "PASS" } else { "FAIL" }
            );
            all_pass = all_pass && pass;
        } else {
            println!("FP32 ADD: result is NULL [FAIL]");
            all_pass = false;
        }
    }

    // Test: -1.5 + 2.5 = 1.0
    {
        let mut sim = NclSimulator::new(netlist.clone(), config.clone());
        sim.set_dual_rail_value("function_sel", 23, 6);
        sim.set_dual_rail_value("route_sel", 0, 3);
        let a = f32_to_bits(-1.5);
        let b = f32_to_bits(2.5);
        sim.set_dual_rail_value("data1", a | (b << 32), 256);
        sim.set_dual_rail_value("data2", 0, 256);
        sim.run_until_stable(100000);

        if let Some(result_bits) = sim.get_dual_rail_value("debug_l2", 32) {
            let result = bits_to_f32(result_bits);
            let pass = f32_approx_eq(result, 1.0, 0.001);
            println!(
                "FP32 ADD: -1.5 + 2.5 = {} (expected 1.0) [{}]",
                result,
                if pass { "PASS" } else { "FAIL" }
            );
            all_pass = all_pass && pass;
        } else {
            println!("FP32 ADD: result is NULL [FAIL]");
            all_pass = false;
        }
    }

    assert!(all_pass, "Async CLE L2 FP32 ADD tests failed");
}

#[test]
fn test_async_cle_l2_fp32_mul() {
    println!("\n=== Async CLE L2 FP32 MUL Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let config = NclSimConfig {
        max_iterations: 100000,
        debug: false,
        track_stages: false,
    };

    let mut all_pass = true;

    // L2Opcode::FP32_MUL = 24
    // Test: 3.0 * 4.0 = 12.0
    {
        let mut sim = NclSimulator::new(netlist.clone(), config.clone());
        sim.set_dual_rail_value("function_sel", 24, 6);
        sim.set_dual_rail_value("route_sel", 0, 3);
        let a = f32_to_bits(3.0);
        let b = f32_to_bits(4.0);
        sim.set_dual_rail_value("data1", a | (b << 32), 256);
        sim.set_dual_rail_value("data2", 0, 256);
        sim.run_until_stable(100000);

        if let Some(result_bits) = sim.get_dual_rail_value("debug_l2", 32) {
            let result = bits_to_f32(result_bits);
            let pass = f32_approx_eq(result, 12.0, 0.001);
            println!(
                "FP32 MUL: 3.0 * 4.0 = {} (expected 12.0) [{}]",
                result,
                if pass { "PASS" } else { "FAIL" }
            );
            all_pass = all_pass && pass;
        } else {
            println!("FP32 MUL: result is NULL [FAIL]");
            all_pass = false;
        }
    }

    // Test: 2.5 * -2.0 = -5.0
    {
        let mut sim = NclSimulator::new(netlist.clone(), config.clone());
        sim.set_dual_rail_value("function_sel", 24, 6);
        sim.set_dual_rail_value("route_sel", 0, 3);
        let a = f32_to_bits(2.5);
        let b = f32_to_bits(-2.0);
        sim.set_dual_rail_value("data1", a | (b << 32), 256);
        sim.set_dual_rail_value("data2", 0, 256);
        sim.run_until_stable(100000);

        if let Some(result_bits) = sim.get_dual_rail_value("debug_l2", 32) {
            let result = bits_to_f32(result_bits);
            let pass = f32_approx_eq(result, -5.0, 0.001);
            println!(
                "FP32 MUL: 2.5 * -2.0 = {} (expected -5.0) [{}]",
                result,
                if pass { "PASS" } else { "FAIL" }
            );
            all_pass = all_pass && pass;
        } else {
            println!("FP32 MUL: result is NULL [FAIL]");
            all_pass = false;
        }
    }

    assert!(all_pass, "Async CLE L2 FP32 MUL tests failed");
}

// =============================================================================
// L3 Tests (Vector Operations)
// =============================================================================

#[test]
fn test_async_cle_l3_vec3_add() {
    println!("\n=== Async CLE L3 VEC3 ADD Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let config = NclSimConfig {
        max_iterations: 100000,
        debug: false,
        track_stages: false,
    };

    let mut all_pass = true;

    // L3Opcode::VEC3_ADD = 32
    // Test: (1.0, 2.0, 3.0) + (4.0, 5.0, 6.0) = (5.0, 7.0, 9.0)
    {
        let mut sim = NclSimulator::new(netlist.clone(), config.clone());
        sim.set_dual_rail_value("function_sel", 32, 6);
        sim.set_dual_rail_value("route_sel", 0, 3);

        // data1 = vec A: ax[31:0], ay[63:32], az[95:64]
        let ax = f32_to_bits(1.0);
        let ay = f32_to_bits(2.0);
        let _az = f32_to_bits(3.0); // z component not packed due to u64 limitation
                                    // Pack vec3 into lower 64 bits (ax, ay); az would need u128 for proper packing
        sim.set_dual_rail_value("data1", ax | (ay << 32), 256);

        // data2 = vec B: bx[31:0], by[63:32], bz[95:64]
        let bx = f32_to_bits(4.0);
        let by = f32_to_bits(5.0);
        let _bz = f32_to_bits(6.0);
        // Pack vec3 into 96-bit portion of 256-bit data
        sim.set_dual_rail_value("data2", bx | (by << 32), 256);

        sim.run_until_stable(100000);

        // Check debug_l3 (lower 32 bits = rx)
        if let Some(result_bits) = sim.get_dual_rail_value("debug_l3", 32) {
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
fn test_async_cle_l3_vec3_dot() {
    println!("\n=== Async CLE L3 VEC3 DOT Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let config = NclSimConfig {
        max_iterations: 100000,
        debug: false,
        track_stages: false,
    };

    let mut all_pass = true;

    // L3Opcode::VEC3_DOT = 35
    // Test: (1.0, 2.0, 3.0) · (4.0, 5.0, 6.0) = 1*4 + 2*5 + 3*6 = 32.0
    {
        let mut sim = NclSimulator::new(netlist.clone(), config.clone());
        sim.set_dual_rail_value("function_sel", 35, 6);
        sim.set_dual_rail_value("route_sel", 0, 3);

        // data1 = vec A
        let ax = f32_to_bits(1.0);
        let ay = f32_to_bits(2.0);
        let _az = f32_to_bits(3.0); // z component not packed due to u64 limitation
                                    // Pack vec3 into lower 64 bits (ax, ay); az would need u128 for proper packing
        sim.set_dual_rail_value("data1", ax | (ay << 32), 256);

        // data2 = vec B
        let bx = f32_to_bits(4.0);
        let by = f32_to_bits(5.0);
        let _bz = f32_to_bits(6.0); // z component not packed due to u64 limitation
                                    // Pack vec3 into lower 64 bits (bx, by); bz would need u128 for proper packing
        sim.set_dual_rail_value("data2", bx | (by << 32), 256);

        sim.run_until_stable(100000);

        if let Some(result_bits) = sim.get_dual_rail_value("debug_l3", 32) {
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
// L5 Tests (Bitops Operations)
// =============================================================================

#[test]
fn test_async_cle_l5_clz() {
    println!("\n=== Async CLE L5 CLZ Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let config = NclSimConfig {
        max_iterations: 100000,
        debug: false,
        track_stages: false,
    };

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
        let mut sim = NclSimulator::new(netlist.clone(), config.clone());
        sim.set_dual_rail_value("function_sel", 55, 6);
        sim.set_dual_rail_value("route_sel", 0, 3);
        sim.set_dual_rail_value("data1", input, 256);
        sim.set_dual_rail_value("data2", 0, 256);
        sim.run_until_stable(100000);

        if let Some(result) = sim.get_dual_rail_value("debug_l4_l5", 32) {
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
fn test_async_cle_l5_popcount() {
    println!("\n=== Async CLE L5 POPCOUNT Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let config = NclSimConfig {
        max_iterations: 100000,
        debug: false,
        track_stages: false,
    };

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
        let mut sim = NclSimulator::new(netlist.clone(), config.clone());
        sim.set_dual_rail_value("function_sel", 57, 6);
        sim.set_dual_rail_value("route_sel", 0, 3);
        sim.set_dual_rail_value("data1", input, 256);
        sim.set_dual_rail_value("data2", 0, 256);
        sim.run_until_stable(100000);

        if let Some(result) = sim.get_dual_rail_value("debug_l4_l5", 32) {
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
fn test_async_cle_l5_bitreverse() {
    println!("\n=== Async CLE L5 BITREVERSE Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let config = NclSimConfig {
        max_iterations: 100000,
        debug: false,
        track_stages: false,
    };

    let mut all_pass = true;

    // L5Opcode::BITREVERSE = 58
    let test_cases = [
        (0x00000001u64, 0x80000000u64, "0x00000001 reversed"),
        (0x80000000u64, 0x00000001u64, "0x80000000 reversed"),
        (0x0F0F0F0Fu64, 0xF0F0F0F0u64, "0x0F0F0F0F reversed"),
    ];

    for (input, expected, desc) in test_cases {
        let mut sim = NclSimulator::new(netlist.clone(), config.clone());
        sim.set_dual_rail_value("function_sel", 58, 6);
        sim.set_dual_rail_value("route_sel", 0, 3);
        sim.set_dual_rail_value("data1", input, 256);
        sim.set_dual_rail_value("data2", 0, 256);
        sim.run_until_stable(100000);

        if let Some(result) = sim.get_dual_rail_value("debug_l4_l5", 32) {
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
fn test_async_cle_l5_parity() {
    println!("\n=== Async CLE L5 PARITY Test ===");
    let netlist = compile_karythra_async_cle();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let config = NclSimConfig {
        max_iterations: 100000,
        debug: false,
        track_stages: false,
    };

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
        let mut sim = NclSimulator::new(netlist.clone(), config.clone());
        sim.set_dual_rail_value("function_sel", 62, 6);
        sim.set_dual_rail_value("route_sel", 0, 3);
        sim.set_dual_rail_value("data1", input, 256);
        sim.set_dual_rail_value("data2", 0, 256);
        sim.run_until_stable(100000);

        if let Some(result) = sim.get_dual_rail_value("debug_l4_l5", 32) {
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
