//! Comprehensive NCL (Null Convention Logic) Async Simulation Tests
//!
//! Tests both CPU and GPU backends for NCL simulation with:
//! - Logical operations (AND, OR, XOR, NOT)
//! - Arithmetic operations (ADD, SUB, MUL)
//! - Comparators (EQ, LT)
//! - Multi-bit operations
//!
//! NCL circuits use dual-rail encoding and THmn threshold gates.

use skalp_frontend::parse_and_build_hir;
use skalp_lir::{get_stdlib_library, lower_mir_module_to_lir, map_lir_to_gates};
use skalp_mir::MirCompiler;
use skalp_sim::{
    CircuitMode, HwAccel, NclSimConfig, NclSimulator, UnifiedSimConfig, UnifiedSimulator,
};

/// Helper to compile async Skalp source to GateNetlist
fn compile_async_to_netlist(source: &str) -> skalp_lir::gate_netlist::GateNetlist {
    let hir = parse_and_build_hir(source).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    assert!(!mir.modules.is_empty(), "Should have at least one module");
    let module = &mir.modules[0];

    let lir_result = lower_mir_module_to_lir(module);
    let library = get_stdlib_library("generic_asic").expect("Failed to load library");
    let tech_result = map_lir_to_gates(&lir_result.lir, &library);

    tech_result.netlist
}

/// Test NCL simulation with CPU backend
fn test_ncl_cpu(
    netlist: skalp_lir::gate_netlist::GateNetlist,
    inputs: &[(&str, u64, usize)],
    expected_outputs: &[(&str, u64, usize)],
) -> bool {
    let config = NclSimConfig {
        max_iterations: 10000,
        debug: false,
        track_stages: true,
    };

    let mut sim = NclSimulator::new(netlist, config);

    // Set inputs
    for (name, value, width) in inputs {
        sim.set_dual_rail_value(name, *value, *width);
    }

    // Run until stable
    let iterations = sim.run_until_stable(10000);
    println!(
        "  CPU: {} iterations, stable={}",
        iterations,
        sim.stats().is_stable
    );

    // Debug: show all net values
    println!("  NET VALUES:");
    for i in 0..10 {
        // Show first 10 nets
        let val = sim.get_net(skalp_lir::gate_netlist::GateNetId(i as u32));
        println!("    net[{}] = {}", i, val);
    }

    // Check outputs
    let mut all_pass = true;
    for (name, expected, width) in expected_outputs {
        // Debug: show individual rail values
        for bit in 0..*width {
            let ncl_val = sim.get_dual_rail(name, bit);
            println!("  DEBUG: {}[{}] = {:?}", name, bit, ncl_val);
        }
        match sim.get_dual_rail_value(name, *width) {
            Some(actual) => {
                let pass = actual == *expected;
                if !pass {
                    println!("  FAIL: {} = {} (expected {})", name, actual, expected);
                    all_pass = false;
                } else {
                    println!("  PASS: {} = {}", name, actual);
                }
            }
            None => {
                println!("  FAIL: {} is NULL or invalid", name);
                all_pass = false;
            }
        }
    }

    all_pass
}

/// Test NCL simulation with unified runtime (supports both CPU and GPU)
fn test_ncl_unified(
    netlist: skalp_lir::gate_netlist::GateNetlist,
    inputs: &[(&str, u64, usize)],
    expected_outputs: &[(&str, u64, usize)],
    use_gpu: bool,
) -> bool {
    let config = UnifiedSimConfig {
        level: skalp_sim::SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: if use_gpu { HwAccel::Gpu } else { HwAccel::Cpu },
        max_iterations: 10000,
        capture_waveforms: false,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load netlist");

    let device = sim.device_info();
    println!("  Device: {}", device);

    // Set inputs
    for (name, value, width) in inputs {
        sim.set_ncl_input(name, *value, *width);
    }

    // Run until stable
    let result = sim.run_until_stable();
    println!(
        "  Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    // Check outputs
    let mut all_pass = true;
    for (name, expected, width) in expected_outputs {
        match sim.get_ncl_output(name, *width) {
            Some(actual) => {
                let pass = actual == *expected;
                if !pass {
                    println!("  FAIL: {} = {} (expected {})", name, actual, expected);
                    all_pass = false;
                } else {
                    println!("  PASS: {} = {}", name, actual);
                }
            }
            None => {
                println!("  FAIL: {} is NULL or invalid", name);
                all_pass = false;
            }
        }
    }

    all_pass
}

// =============================================================================
// Logical Operation Tests
// =============================================================================

#[test]
fn test_ncl_inverter() {
    println!("\n=== NCL Inverter Test ===");

    let source = r#"
        async entity NclInverter {
            in a: bit[1]
            out y: bit[1]
        }
        impl NclInverter {
            y = ~a
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // Debug: print net names to understand structure
    println!("Nets:");
    for net in &netlist.nets {
        println!(
            "  {:?} '{}' input={} output={}",
            net.id, net.name, net.is_input, net.is_output
        );
    }
    println!("Cells:");
    for cell in &netlist.cells {
        println!("  {} type={}", cell.path, cell.cell_type);
    }

    // Test case 1: a=0 -> y=1
    println!("\nTest: ~0 = 1");
    let pass1 = test_ncl_cpu(netlist.clone(), &[("a", 0, 1)], &[("y", 1, 1)]);

    // Test case 2: a=1 -> y=0
    println!("\nTest: ~1 = 0");
    let pass2 = test_ncl_cpu(netlist.clone(), &[("a", 1, 1)], &[("y", 0, 1)]);

    assert!(pass1 && pass2, "NCL inverter tests failed");
}

#[test]
fn test_ncl_and_gate() {
    println!("\n=== NCL AND Gate Test ===");

    let source = r#"
        async entity NclAnd {
            in a: bit[1]
            in b: bit[1]
            out y: bit[1]
        }
        impl NclAnd {
            y = a & b
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // Debug: print net names to understand structure
    println!("Nets:");
    for net in &netlist.nets {
        println!(
            "  {:?} '{}' input={} output={}",
            net.id, net.name, net.is_input, net.is_output
        );
    }
    println!("Cells:");
    for cell in &netlist.cells {
        println!(
            "  {:?} type={:?} inputs={:?} outputs={:?}",
            cell.id, cell.cell_type, cell.inputs, cell.outputs
        );
    }

    let test_cases = [(0, 0, 0), (0, 1, 0), (1, 0, 0), (1, 1, 1)];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        println!("\nTest: {} & {} = {}", a, b, expected);
        let pass = test_ncl_cpu(
            netlist.clone(),
            &[("a", a, 1), ("b", b, 1)],
            &[("y", expected, 1)],
        );
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL AND gate tests failed");
}

#[test]
fn test_ncl_or_gate() {
    println!("\n=== NCL OR Gate Test ===");

    let source = r#"
        async entity NclOr {
            in a: bit[1]
            in b: bit[1]
            out y: bit[1]
        }
        impl NclOr {
            y = a | b
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let test_cases = [(0, 0, 0), (0, 1, 1), (1, 0, 1), (1, 1, 1)];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        println!("\nTest: {} | {} = {}", a, b, expected);
        let pass = test_ncl_cpu(
            netlist.clone(),
            &[("a", a, 1), ("b", b, 1)],
            &[("y", expected, 1)],
        );
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL OR gate tests failed");
}

#[test]
fn test_ncl_xor_gate() {
    println!("\n=== NCL XOR Gate Test ===");

    let source = r#"
        async entity NclXor {
            in a: bit[1]
            in b: bit[1]
            out y: bit[1]
        }
        impl NclXor {
            y = a ^ b
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let test_cases = [(0, 0, 0), (0, 1, 1), (1, 0, 1), (1, 1, 0)];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        println!("\nTest: {} ^ {} = {}", a, b, expected);
        let pass = test_ncl_cpu(
            netlist.clone(),
            &[("a", a, 1), ("b", b, 1)],
            &[("y", expected, 1)],
        );
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL XOR gate tests failed");
}

// =============================================================================
// Multi-bit Logical Operation Tests
// =============================================================================

#[test]
fn test_ncl_and_8bit() {
    println!("\n=== NCL 8-bit AND Test ===");

    let source = r#"
        async entity NclAnd8 {
            in a: bit[8]
            in b: bit[8]
            out y: bit[8]
        }
        impl NclAnd8 {
            y = a & b
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let test_cases: [(u64, u64, u64); 4] = [
        (0x00, 0x00, 0x00),
        (0xFF, 0xFF, 0xFF),
        (0xAA, 0x55, 0x00),
        (0xF0, 0x0F, 0x00),
    ];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        println!("\nTest: 0x{:02X} & 0x{:02X} = 0x{:02X}", a, b, expected);
        let pass = test_ncl_cpu(
            netlist.clone(),
            &[("a", a, 8), ("b", b, 8)],
            &[("y", expected, 8)],
        );
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL 8-bit AND tests failed");
}

#[test]
fn test_ncl_or_8bit() {
    println!("\n=== NCL 8-bit OR Test ===");

    let source = r#"
        async entity NclOr8 {
            in a: bit[8]
            in b: bit[8]
            out y: bit[8]
        }
        impl NclOr8 {
            y = a | b
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let test_cases: [(u64, u64, u64); 4] = [
        (0x00, 0x00, 0x00),
        (0xFF, 0xFF, 0xFF),
        (0xAA, 0x55, 0xFF),
        (0xF0, 0x0F, 0xFF),
    ];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        println!("\nTest: 0x{:02X} | 0x{:02X} = 0x{:02X}", a, b, expected);
        let pass = test_ncl_cpu(
            netlist.clone(),
            &[("a", a, 8), ("b", b, 8)],
            &[("y", expected, 8)],
        );
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL 8-bit OR tests failed");
}

#[test]
fn test_ncl_xor_8bit() {
    println!("\n=== NCL 8-bit XOR Test ===");

    let source = r#"
        async entity NclXor8 {
            in a: bit[8]
            in b: bit[8]
            out y: bit[8]
        }
        impl NclXor8 {
            y = a ^ b
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let test_cases: [(u64, u64, u64); 4] = [
        (0x00, 0x00, 0x00),
        (0xFF, 0xFF, 0x00),
        (0xAA, 0x55, 0xFF),
        (0xF0, 0x0F, 0xFF),
    ];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        println!("\nTest: 0x{:02X} ^ 0x{:02X} = 0x{:02X}", a, b, expected);
        let pass = test_ncl_cpu(
            netlist.clone(),
            &[("a", a, 8), ("b", b, 8)],
            &[("y", expected, 8)],
        );
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL 8-bit XOR tests failed");
}

#[test]
fn test_ncl_not_8bit() {
    println!("\n=== NCL 8-bit NOT Test ===");

    let source = r#"
        async entity NclNot8 {
            in a: bit[8]
            out y: bit[8]
        }
        impl NclNot8 {
            y = ~a
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let test_cases: [(u64, u64); 4] = [(0x00, 0xFF), (0xFF, 0x00), (0xAA, 0x55), (0xF0, 0x0F)];

    let mut all_pass = true;
    for (a, expected) in test_cases {
        println!("\nTest: ~0x{:02X} = 0x{:02X}", a, expected);
        let pass = test_ncl_cpu(netlist.clone(), &[("a", a, 8)], &[("y", expected, 8)]);
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL 8-bit NOT tests failed");
}

// =============================================================================
// Arithmetic Operation Tests
// =============================================================================

#[test]
fn test_ncl_add_8bit() {
    println!("\n=== NCL 8-bit ADD Test ===");

    let source = r#"
        async entity NclAdd8 {
            in a: bit[8]
            in b: bit[8]
            out sum: bit[8]
        }
        impl NclAdd8 {
            sum = a + b
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let test_cases: [(u64, u64, u64); 6] = [
        (0, 0, 0),
        (1, 1, 2),
        (10, 20, 30),
        (100, 50, 150),
        (255, 0, 255),
        (128, 127, 255),
    ];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        println!("\nTest: {} + {} = {}", a, b, expected);
        let pass = test_ncl_cpu(
            netlist.clone(),
            &[("a", a, 8), ("b", b, 8)],
            &[("sum", expected, 8)],
        );
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL 8-bit ADD tests failed");
}

#[test]
fn test_ncl_sub_8bit() {
    println!("\n=== NCL 8-bit SUB Test ===");

    let source = r#"
        async entity NclSub8 {
            in a: bit[8]
            in b: bit[8]
            out diff: bit[8]
        }
        impl NclSub8 {
            diff = a - b
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let test_cases: [(u64, u64, u64); 5] = [
        (0, 0, 0),
        (5, 3, 2),
        (100, 50, 50),
        (255, 255, 0),
        (200, 100, 100),
    ];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        println!("\nTest: {} - {} = {}", a, b, expected);
        let pass = test_ncl_cpu(
            netlist.clone(),
            &[("a", a, 8), ("b", b, 8)],
            &[("diff", expected, 8)],
        );
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL 8-bit SUB tests failed");
}

#[test]
fn test_ncl_mul_4bit() {
    println!("\n=== NCL 4-bit MUL Test ===");

    // Use 4-bit to keep gate count manageable
    let source = r#"
        async entity NclMul4 {
            in a: bit[4]
            in b: bit[4]
            out prod: bit[8]
        }
        impl NclMul4 {
            prod = a * b
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let test_cases: [(u64, u64, u64); 6] = [
        (0, 0, 0),
        (1, 1, 1),
        (2, 3, 6),
        (5, 5, 25),
        (15, 1, 15),
        (7, 8, 56),
    ];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        println!("\nTest: {} * {} = {}", a, b, expected);
        let pass = test_ncl_cpu(
            netlist.clone(),
            &[("a", a, 4), ("b", b, 4)],
            &[("prod", expected, 8)],
        );
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL 4-bit MUL tests failed");
}

// =============================================================================
// Comparison Tests
// =============================================================================

#[test]
fn test_ncl_eq_8bit() {
    println!("\n=== NCL 8-bit EQ Test ===");

    let source = r#"
        async entity NclEq8 {
            in a: bit[8]
            in b: bit[8]
            out eq: bit[1]
        }
        impl NclEq8 {
            eq = a == b
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let test_cases: [(u64, u64, u64); 5] = [
        (0, 0, 1),
        (42, 42, 1),
        (255, 255, 1),
        (0, 1, 0),
        (100, 200, 0),
    ];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        println!("\nTest: {} == {} -> {}", a, b, expected);
        let pass = test_ncl_cpu(
            netlist.clone(),
            &[("a", a, 8), ("b", b, 8)],
            &[("eq", expected, 1)],
        );
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL 8-bit EQ tests failed");
}

#[test]
fn test_ncl_lt_8bit() {
    println!("\n=== NCL 8-bit LT Test ===");

    let source = r#"
        async entity NclLt8 {
            in a: bit[8]
            in b: bit[8]
            out lt: bit[1]
        }
        impl NclLt8 {
            lt = a < b
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let test_cases: [(u64, u64, u64); 6] = [
        (0, 0, 0),
        (0, 1, 1),
        (1, 0, 0),
        (50, 100, 1),
        (100, 50, 0),
        (255, 255, 0),
    ];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        println!("\nTest: {} < {} -> {}", a, b, expected);
        let pass = test_ncl_cpu(
            netlist.clone(),
            &[("a", a, 8), ("b", b, 8)],
            &[("lt", expected, 1)],
        );
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL 8-bit LT tests failed");
}

// =============================================================================
// GPU Backend Tests
// =============================================================================

#[test]
#[cfg(target_os = "macos")]
fn test_ncl_gpu_inverter() {
    println!("\n=== NCL GPU Inverter Test ===");

    let source = r#"
        async entity NclInverterGpu {
            in a: bit[1]
            out y: bit[1]
        }
        impl NclInverterGpu {
            y = ~a
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // Test with GPU
    println!("\nTest: ~0 = 1 (GPU)");
    let pass1 = test_ncl_unified(
        netlist.clone(),
        &[("a", 0, 1)],
        &[("y", 1, 1)],
        true, // use GPU
    );

    println!("\nTest: ~1 = 0 (GPU)");
    let pass2 = test_ncl_unified(netlist.clone(), &[("a", 1, 1)], &[("y", 0, 1)], true);

    assert!(pass1 && pass2, "NCL GPU inverter tests failed");
}

#[test]
#[cfg(target_os = "macos")]
fn test_ncl_gpu_and_8bit() {
    println!("\n=== NCL GPU 8-bit AND Test ===");

    let source = r#"
        async entity NclAnd8Gpu {
            in a: bit[8]
            in b: bit[8]
            out y: bit[8]
        }
        impl NclAnd8Gpu {
            y = a & b
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let test_cases: [(u64, u64, u64); 4] = [
        (0x00, 0x00, 0x00),
        (0xFF, 0xFF, 0xFF),
        (0xAA, 0x55, 0x00),
        (0xF0, 0x0F, 0x00),
    ];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        println!(
            "\nTest: 0x{:02X} & 0x{:02X} = 0x{:02X} (GPU)",
            a, b, expected
        );
        let pass = test_ncl_unified(
            netlist.clone(),
            &[("a", a, 8), ("b", b, 8)],
            &[("y", expected, 8)],
            true,
        );
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL GPU 8-bit AND tests failed");
}

#[test]
#[cfg(target_os = "macos")]
fn test_ncl_gpu_add_8bit() {
    println!("\n=== NCL GPU 8-bit ADD Test ===");

    let source = r#"
        async entity NclAdd8Gpu {
            in a: bit[8]
            in b: bit[8]
            out sum: bit[8]
        }
        impl NclAdd8Gpu {
            sum = a + b
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let test_cases: [(u64, u64, u64); 5] = [
        (0, 0, 0),
        (1, 1, 2),
        (10, 20, 30),
        (100, 50, 150),
        (128, 127, 255),
    ];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        println!("\nTest: {} + {} = {} (GPU)", a, b, expected);
        let pass = test_ncl_unified(
            netlist.clone(),
            &[("a", a, 8), ("b", b, 8)],
            &[("sum", expected, 8)],
            true,
        );
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL GPU 8-bit ADD tests failed");
}

#[test]
#[cfg(target_os = "macos")]
fn test_ncl_gpu_vs_cpu_consistency() {
    println!("\n=== NCL GPU vs CPU Consistency Test ===");

    let source = r#"
        async entity NclConsistency {
            in a: bit[8]
            in b: bit[8]
            out and_out: bit[8]
            out or_out: bit[8]
            out xor_out: bit[8]
            out sum: bit[8]
        }
        impl NclConsistency {
            and_out = a & b
            or_out = a | b
            xor_out = a ^ b
            sum = a + b
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let test_cases: [(u64, u64); 5] = [
        (0x00, 0x00),
        (0xFF, 0xFF),
        (0xAA, 0x55),
        (42, 58),
        (100, 155),
    ];

    let mut all_pass = true;
    for (a, b) in test_cases {
        let expected_and = a & b;
        let expected_or = a | b;
        let expected_xor = a ^ b;
        let expected_sum = (a + b) & 0xFF;

        println!("\nTest: a=0x{:02X}, b=0x{:02X}", a, b);

        // CPU test
        println!("  CPU:");
        let cpu_pass = test_ncl_unified(
            netlist.clone(),
            &[("a", a, 8), ("b", b, 8)],
            &[
                ("and_out", expected_and, 8),
                ("or_out", expected_or, 8),
                ("xor_out", expected_xor, 8),
                ("sum", expected_sum, 8),
            ],
            false,
        );

        // GPU test
        println!("  GPU:");
        let gpu_pass = test_ncl_unified(
            netlist.clone(),
            &[("a", a, 8), ("b", b, 8)],
            &[
                ("and_out", expected_and, 8),
                ("or_out", expected_or, 8),
                ("xor_out", expected_xor, 8),
                ("sum", expected_sum, 8),
            ],
            true,
        );

        if cpu_pass && gpu_pass {
            println!("  ✓ CPU and GPU results match");
        } else {
            println!("  ✗ CPU/GPU mismatch!");
            all_pass = false;
        }
    }

    assert!(all_pass, "NCL GPU vs CPU consistency tests failed");
}

// =============================================================================
// Complex Operation Tests
// =============================================================================

#[test]
fn test_ncl_mux() {
    println!("\n=== NCL MUX Test ===");

    let source = r#"
        async entity NclMux {
            in sel: bit[1]
            in a: bit[8]
            in b: bit[8]
            out y: bit[8]
        }
        impl NclMux {
            y = if sel == 1 { b } else { a }
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // sel=0: output a
    println!("\nTest: sel=0, a=42, b=100 -> 42");
    let pass1 = test_ncl_cpu(
        netlist.clone(),
        &[("sel", 0, 1), ("a", 42, 8), ("b", 100, 8)],
        &[("y", 42, 8)],
    );

    // sel=1: output b
    println!("\nTest: sel=1, a=42, b=100 -> 100");
    let pass2 = test_ncl_cpu(
        netlist.clone(),
        &[("sel", 1, 1), ("a", 42, 8), ("b", 100, 8)],
        &[("y", 100, 8)],
    );

    assert!(pass1 && pass2, "NCL MUX tests failed");
}

#[test]
fn test_ncl_combined_logic() {
    println!("\n=== NCL Combined Logic Test ===");

    let source = r#"
        async entity NclCombined {
            in a: bit[8]
            in b: bit[8]
            out y: bit[8]
        }
        impl NclCombined {
            // y = (a & b) | (~a & ~b) = ~(a ^ b)
            y = ~(a ^ b)
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let test_cases: [(u64, u64, u64); 4] = [
        (0x00, 0x00, 0xFF), // ~(0 ^ 0) = ~0 = FF
        (0xFF, 0xFF, 0xFF), // ~(FF ^ FF) = ~0 = FF
        (0xAA, 0x55, 0x00), // ~(AA ^ 55) = ~FF = 00
        (0xF0, 0xF0, 0xFF), // ~(F0 ^ F0) = ~0 = FF
    ];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        println!("\nTest: ~(0x{:02X} ^ 0x{:02X}) = 0x{:02X}", a, b, expected);
        let pass = test_ncl_cpu(
            netlist.clone(),
            &[("a", a, 8), ("b", b, 8)],
            &[("y", expected, 8)],
        );
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL combined logic tests failed");
}

#[test]
fn test_ncl_shift_left() {
    println!("\n=== NCL Shift Left Test ===");

    let source = r#"
        async entity NclShl {
            in a: bit[8]
            out y: bit[8]
        }
        impl NclShl {
            y = a << 1
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let test_cases: [(u64, u64); 4] = [(0x00, 0x00), (0x01, 0x02), (0x40, 0x80), (0x55, 0xAA)];

    let mut all_pass = true;
    for (a, expected) in test_cases {
        println!("\nTest: 0x{:02X} << 1 = 0x{:02X}", a, expected);
        let pass = test_ncl_cpu(netlist.clone(), &[("a", a, 8)], &[("y", expected, 8)]);
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL shift left tests failed");
}

#[test]
fn test_ncl_shift_right() {
    println!("\n=== NCL Shift Right Test ===");

    let source = r#"
        async entity NclShr {
            in a: bit[8]
            out y: bit[8]
        }
        impl NclShr {
            y = a >> 1
        }
    "#;

    let netlist = compile_async_to_netlist(source);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let test_cases: [(u64, u64); 4] = [(0x00, 0x00), (0x02, 0x01), (0x80, 0x40), (0xAA, 0x55)];

    let mut all_pass = true;
    for (a, expected) in test_cases {
        println!("\nTest: 0x{:02X} >> 1 = 0x{:02X}", a, expected);
        let pass = test_ncl_cpu(netlist.clone(), &[("a", a, 8)], &[("y", expected, 8)]);
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL shift right tests failed");
}
