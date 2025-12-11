//! Test demonstrating unified simulation: same testbench, different backends
//!
//! This test shows how the same source code and testbench can run on:
//! 1. Behavioral simulation (MIR → behavioral SIR)
//! 2. Gate-level simulation (MIR → LIR → structural SIR)
//!
//! Both produce identical results for functionally correct designs.

use skalp_frontend::parse_and_build_hir;
use skalp_lir::lower_to_lir;
use skalp_mir::MirCompiler;
use skalp_sim::{
    GateLevelRuntime, SimLevel, SimulationMode, UnifiedSimConfig, UnifiedSimulator,
};
use skalp_sir::convert_mir_to_sir_with_hierarchy;

/// Helper function to compile source to both behavioral SIR and LIR (gate netlist)
fn compile_to_both(
    source: &str,
    module_name: &str,
) -> (skalp_sir::SirModule, skalp_lir::lir::Lir) {
    // Parse and compile to HIR and MIR
    let hir = parse_and_build_hir(source).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir_design = mir_compiler.compile(&hir).expect("Failed to compile to MIR");

    // Find the module
    let mir_module = mir_design
        .modules
        .iter()
        .find(|m| m.name == module_name)
        .expect(&format!("Module '{}' not found", module_name))
        .clone();

    // Path 1: MIR → behavioral SIR
    let behavioral_sir = convert_mir_to_sir_with_hierarchy(&mir_design, &mir_module);

    // Path 2: MIR → LIR (gate netlist)
    let lir_results = lower_to_lir(&mir_design).expect("Failed to lower to LIR");
    let lir = lir_results
        .into_iter()
        .find(|r| r.lir.name == module_name)
        .map(|r| r.lir)
        .expect(&format!("LIR for '{}' not found", module_name));

    (behavioral_sir, lir)
}

// ============================================================================
// TESTS
// ============================================================================

#[test]
fn test_unified_adder_behavioral_vs_gate_level() {
    let source = r#"
        entity Adder<'clk> {
            in a: bit[8],
            in b: bit[8],
            out sum: bit[8],
        }

        impl Adder<'clk> {
            sum = a + b;
        }
    "#;

    let (behavioral_sir, lir) = compile_to_both(source, "Adder");

    println!("=== Behavioral Simulation ===");
    let behavioral_config = UnifiedSimConfig::behavioral();
    let mut behavioral_sim =
        UnifiedSimulator::from_behavioral_sir(behavioral_sir, behavioral_config)
            .expect("Failed to create behavioral simulator");

    println!("=== Gate-Level Simulation ===");
    let gate_config = UnifiedSimConfig::gate_level().with_hw_accel(skalp_sim::HwAccel::Cpu);
    let mut gate_sim = UnifiedSimulator::from_lir(&lir, gate_config)
        .expect("Failed to create gate-level simulator");

    // Test cases: (a, b) -> expected sum
    let test_cases: Vec<(u64, u64)> = vec![
        (0, 0),
        (1, 0),
        (0, 1),
        (1, 1),
        (5, 3),
        (10, 20),
        (127, 128),
        (255, 1),
    ];

    let mut mismatches = 0;

    for (a, b) in &test_cases {
        // Behavioral
        behavioral_sim.set_input("a", *a).unwrap();
        behavioral_sim.set_input("b", *b).unwrap();
        behavioral_sim.step().unwrap();
        let b_sum = behavioral_sim.get_output("sum").unwrap_or(0);

        // Gate-level
        gate_sim.set_input("a", *a).unwrap();
        gate_sim.set_input("b", *b).unwrap();
        gate_sim.step().unwrap();
        let g_sum = gate_sim.get_output("sum").unwrap_or(0);

        let expected = (a + b) & 0xFF;
        println!(
            "{} + {} = behavioral:{}, gate:{}, expected:{}",
            a, b, b_sum, g_sum, expected
        );

        if b_sum != g_sum {
            println!("  ^ MISMATCH between behavioral and gate-level!");
            mismatches += 1;
        }
    }

    println!("\n=== Comparison ===");
    println!("Behavioral level: {:?}", behavioral_sim.level());
    println!("Gate-level level: {:?}", gate_sim.level());
    println!("Behavioral GPU: {}", behavioral_sim.is_using_gpu());
    println!("Gate-level GPU: {}", gate_sim.is_using_gpu());
    println!("Gate-level device: {}", gate_sim.device_info());

    println!("\nTotal tests: {}", test_cases.len());
    println!("Mismatches: {}", mismatches);
    assert_eq!(mismatches, 0, "Behavioral and gate-level results should match");
}

#[test]
fn test_unified_bitwise_ops() {
    let source = r#"
        entity BitwiseOps<'clk> {
            in a: bit[8],
            in b: bit[8],
            out and_out: bit[8],
            out or_out: bit[8],
            out xor_out: bit[8],
        }

        impl BitwiseOps<'clk> {
            and_out = a & b;
            or_out = a | b;
            xor_out = a ^ b;
        }
    "#;

    let (behavioral_sir, lir) = compile_to_both(source, "BitwiseOps");

    // Test cases
    let test_cases: Vec<(u64, u64)> = vec![
        (0x00, 0x00),
        (0xFF, 0xFF),
        (0xAA, 0x55),
        (0x0F, 0xF0),
        (0x12, 0x34),
    ];

    println!("=== Testing Bitwise Operations ===\n");

    // Behavioral simulation
    let behavioral_config = UnifiedSimConfig::behavioral();
    let mut behavioral_sim =
        UnifiedSimulator::from_behavioral_sir(behavioral_sir, behavioral_config).unwrap();

    // Gate-level simulation (use CPU for now - GPU has bugs)
    let gate_config = UnifiedSimConfig::gate_level().with_hw_accel(skalp_sim::HwAccel::Cpu);
    let mut gate_sim = UnifiedSimulator::from_lir(&lir, gate_config).unwrap();

    let mut all_match = true;

    for (a, b) in test_cases {
        // Behavioral
        behavioral_sim.set_input("a", a).unwrap();
        behavioral_sim.set_input("b", b).unwrap();
        behavioral_sim.step().unwrap();
        let b_and = behavioral_sim.get_output("and_out").unwrap_or(0);
        let b_or = behavioral_sim.get_output("or_out").unwrap_or(0);
        let b_xor = behavioral_sim.get_output("xor_out").unwrap_or(0);

        // Gate-level
        gate_sim.set_input("a", a).unwrap();
        gate_sim.set_input("b", b).unwrap();
        gate_sim.step().unwrap();
        let g_and = gate_sim.get_output("and_out").unwrap_or(0);
        let g_or = gate_sim.get_output("or_out").unwrap_or(0);
        let g_xor = gate_sim.get_output("xor_out").unwrap_or(0);

        let expected_and = a & b;
        let expected_or = a | b;
        let expected_xor = a ^ b;

        println!(
            "a=0x{:02X}, b=0x{:02X}: AND(b={:02X},g={:02X},e={:02X}) OR(b={:02X},g={:02X},e={:02X}) XOR(b={:02X},g={:02X},e={:02X})",
            a, b, b_and, g_and, expected_and, b_or, g_or, expected_or, b_xor, g_xor, expected_xor
        );

        if b_and != g_and || b_or != g_or || b_xor != g_xor {
            println!("  ^ MISMATCH between behavioral and gate-level!");
            all_match = false;
        }
    }

    assert!(all_match, "All behavioral and gate-level results should match");
}

#[test]
#[ignore = "GpuGateRuntime has bugs in combinational evaluation - needs fix"]
fn test_gate_level_cpu_vs_gpu() {
    // Test that CPU and GPU backends produce the same results
    let source = r#"
        entity SimpleAlu<'clk> {
            in a: bit[8],
            in b: bit[8],
            in op: bit[2],
            out result: bit[8],
        }

        impl SimpleAlu<'clk> {
            result = match op {
                0 => a + b,
                1 => a - b,
                2 => a & b,
                _ => a | b,
            };
        }
    "#;

    let (_, lir) = compile_to_both(source, "SimpleAlu");

    println!("=== Gate-Level CPU vs GPU Comparison ===\n");

    // CPU backend
    let mut cpu_sim = GateLevelRuntime::from_lir_with_mode(&lir, SimulationMode::Cpu)
        .expect("Failed to create CPU gate-level simulator");

    // GPU backend (will fall back to CPU if GPU unavailable)
    let mut gpu_sim = GateLevelRuntime::from_lir_with_mode(&lir, SimulationMode::Auto)
        .expect("Failed to create GPU gate-level simulator");

    println!("CPU backend: {}", cpu_sim.device_info());
    println!("GPU backend: {} (using GPU: {})", gpu_sim.device_info(), gpu_sim.is_using_gpu());

    let test_cases: Vec<(u64, u64, u64)> = vec![
        (10, 5, 0), // ADD
        (10, 5, 1), // SUB
        (0xAA, 0x55, 2), // AND
        (0xAA, 0x55, 3), // OR
    ];

    let mut all_match = true;

    for (a, b, op) in test_cases {
        // CPU
        cpu_sim.set_input_u64("a", a).unwrap();
        cpu_sim.set_input_u64("b", b).unwrap();
        cpu_sim.set_input_u64("op", op).unwrap();
        cpu_sim.step().unwrap();
        let cpu_result = cpu_sim.get_output_u64("result").unwrap_or(0);

        // GPU
        gpu_sim.set_input_u64("a", a).unwrap();
        gpu_sim.set_input_u64("b", b).unwrap();
        gpu_sim.set_input_u64("op", op).unwrap();
        gpu_sim.step().unwrap();
        let gpu_result = gpu_sim.get_output_u64("result").unwrap_or(0);

        let op_name = match op {
            0 => "ADD",
            1 => "SUB",
            2 => "AND",
            _ => "OR",
        };

        println!(
            "{}: a={}, b={} => CPU={}, GPU={}",
            op_name, a, b, cpu_result, gpu_result
        );

        if cpu_result != gpu_result {
            println!("  ^ MISMATCH!");
            all_match = false;
        }
    }

    assert!(all_match, "CPU and GPU backends should produce identical results");
}

#[test]
fn test_unified_simulation_info() {
    let source = r#"
        entity TestModule<'clk> {
            in a: bit[8],
            out y: bit[8],
        }
        impl TestModule<'clk> {
            y = a;
        }
    "#;

    let (behavioral_sir, lir) = compile_to_both(source, "TestModule");

    let behavioral_sim = UnifiedSimulator::from_behavioral_sir(
        behavioral_sir,
        UnifiedSimConfig::behavioral(),
    )
    .unwrap();

    let gate_sim = UnifiedSimulator::from_lir(
        &lir,
        UnifiedSimConfig::gate_level().with_hw_accel(skalp_sim::HwAccel::Cpu),
    )
    .unwrap();

    println!("=== Unified Simulation Info ===");
    println!("Behavioral:");
    println!("  Level: {:?}", behavioral_sim.level());
    println!("  Using GPU: {}", behavioral_sim.is_using_gpu());
    println!("  Device: {}", behavioral_sim.device_info());

    println!("\nGate-Level:");
    println!("  Level: {:?}", gate_sim.level());
    println!("  Using GPU: {}", gate_sim.is_using_gpu());
    println!("  Device: {}", gate_sim.device_info());

    assert_eq!(behavioral_sim.level(), SimLevel::Behavioral);
    assert_eq!(gate_sim.level(), SimLevel::GateLevel);
}

/// Test that demonstrates the same testbench working on both modes
#[test]
fn test_same_testbench_both_modes() {
    let source = r#"
        entity Comparator<'clk> {
            in a: bit[8],
            in b: bit[8],
            out eq: bit,
            out lt: bit,
            out gt: bit,
        }

        impl Comparator<'clk> {
            eq = a == b;
            lt = a < b;
            gt = a > b;
        }
    "#;

    let (behavioral_sir, lir) = compile_to_both(source, "Comparator");

    // Create both simulators
    let mut behavioral_sim = UnifiedSimulator::from_behavioral_sir(
        behavioral_sir,
        UnifiedSimConfig::behavioral(),
    ).unwrap();

    let mut gate_sim = UnifiedSimulator::from_lir(
        &lir,
        UnifiedSimConfig::gate_level().with_hw_accel(skalp_sim::HwAccel::Cpu),
    ).unwrap();

    println!("=== Same Testbench, Two Modes ===\n");
    println!("Behavioral: {}", behavioral_sim.device_info());
    println!("Gate-Level: {}", gate_sim.device_info());
    println!();

    // THE SAME TESTBENCH FUNCTION works for both!
    fn run_testbench(sim: &mut UnifiedSimulator, name: &str) -> Vec<(u64, u64, u64, u64, u64)> {
        let test_cases = vec![
            (5, 5),   // equal
            (3, 7),   // less than
            (10, 2),  // greater than
            (0, 0),   // both zero
            (255, 0), // max vs zero
        ];

        let mut results = Vec::new();

        for (a, b) in test_cases {
            sim.set_input("a", a).unwrap();
            sim.set_input("b", b).unwrap();
            sim.step().unwrap();

            let eq = sim.get_output("eq").unwrap_or(0);
            let lt = sim.get_output("lt").unwrap_or(0);
            let gt = sim.get_output("gt").unwrap_or(0);

            println!("[{}] a={}, b={}: eq={}, lt={}, gt={}", name, a, b, eq, lt, gt);
            results.push((a, b, eq, lt, gt));
        }

        results
    }

    let behavioral_results = run_testbench(&mut behavioral_sim, "Behavioral");
    println!();
    let gate_results = run_testbench(&mut gate_sim, "Gate-Level");

    // Compare
    println!("\n=== Comparison ===");
    let mut all_match = true;
    for (i, (b_res, g_res)) in behavioral_results.iter().zip(gate_results.iter()).enumerate() {
        if b_res != g_res {
            println!("Test {}: MISMATCH behavioral={:?} vs gate={:?}", i, b_res, g_res);
            all_match = false;
        }
    }

    if all_match {
        println!("✓ All {} tests match between behavioral and gate-level!", behavioral_results.len());
    }

    assert!(all_match, "Same testbench should produce same results on both modes");
}
