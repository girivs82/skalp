//! MWE for debugging NCL convergence issues
//!
//! This test creates a circuit similar to the CLE (function selection + routing)
//! but at a smaller scale to debug convergence issues.

use skalp_frontend::parse_and_build_hir;
use skalp_lir::{get_stdlib_library, lower_mir_hierarchical, map_hierarchical_to_gates};
use skalp_mir::MirCompiler;
use skalp_sim::{CircuitMode, HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};

/// Simple CLE-like circuit with function selection
const CLE_MWE_SOURCE: &str = r#"
/// L0: Basic ALU operations
fn alu_l0(a: bit[8], b: bit[8], op: bit[2]) -> bit[8] {
    if op == 0 {
        a + b
    } else if op == 1 {
        a - b
    } else if op == 2 {
        a & b
    } else {
        a | b
    }
}

/// L1: Comparison operations
fn cmp_l1(a: bit[8], b: bit[8], op: bit[2]) -> bit[8] {
    let lt: bit[8] = if a < b { 1 } else { 0 }
    let eq: bit[8] = if a == b { 1 } else { 0 }
    let gt: bit[8] = if a > b { 1 } else { 0 }

    if op == 0 {
        lt
    } else if op == 1 {
        eq
    } else {
        gt
    }
}

/// MWE CLE: selects between ALU and comparison units
async entity CleMwe {
    in data1: bit[8]
    in data2: bit[8]
    in function_sel: bit[1]
    in op_sel: bit[2]
    out result: bit[8]
}

impl CleMwe {
    signal alu_result: bit[8]
    signal cmp_result: bit[8]

    alu_result = alu_l0(data1, data2, op_sel)
    cmp_result = cmp_l1(data1, data2, op_sel)

    result = if function_sel == 0 {
        alu_result
    } else {
        cmp_result
    }
}
"#;

#[test]
fn test_ncl_convergence_mwe() {
    println!("\n=== NCL Convergence MWE Test ===\n");

    let hir = parse_and_build_hir(CLE_MWE_SOURCE).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    // Use hierarchical compilation like CLE
    let hier_lir = lower_mir_hierarchical(&mir);
    let library = get_stdlib_library("generic_asic").expect("Failed to load library");
    let hier_result = map_hierarchical_to_gates(&hier_lir, &library);
    let netlist = hier_result.flatten();

    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // Create NCL simulator with verbose debug output
    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Gpu,
        max_iterations: 1000, // Lower max for faster feedback
        capture_waveforms: false,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist.clone())
        .expect("Failed to load NCL netlist");

    println!("\n--- Test 1: ALU Add (fn_sel=0, op=0): 10 + 20 = 30 ---");
    sim.set_ncl_input("top.data1", 10, 8);
    sim.set_ncl_input("top.data2", 20, 8);
    sim.set_ncl_input("top.function_sel", 0, 1);
    sim.set_ncl_input("top.op_sel", 0, 2);

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 8) {
        Some(value) => {
            println!("Result: {} (expected 30)", value);
            assert_eq!(value, 30, "Addition failed");
        }
        None => {
            println!("FAIL: Result is NULL");
            // Don't panic yet - print debug info
        }
    }

    println!("\n--- Test 2: ALU Sub (fn_sel=0, op=1): 30 - 10 = 20 ---");
    sim.reset(); // Reset before new test
    sim.set_ncl_input("top.data1", 30, 8);
    sim.set_ncl_input("top.data2", 10, 8);
    sim.set_ncl_input("top.function_sel", 0, 1);
    sim.set_ncl_input("top.op_sel", 1, 2);

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 8) {
        Some(value) => {
            println!("Result: {} (expected 20)", value);
            assert_eq!(value, 20, "Subtraction failed");
        }
        None => {
            println!("FAIL: Result is NULL");
        }
    }

    println!("\n--- Test 3: Compare LT (fn_sel=1, op=0): 10 < 20 = 1 ---");
    sim.reset(); // Reset before new test
    sim.set_ncl_input("top.data1", 10, 8);
    sim.set_ncl_input("top.data2", 20, 8);
    sim.set_ncl_input("top.function_sel", 1, 1);
    sim.set_ncl_input("top.op_sel", 0, 2);

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 8) {
        Some(value) => {
            println!("Result: {} (expected 1)", value);
            assert_eq!(value, 1, "Less-than comparison failed");
        }
        None => {
            println!("FAIL: Result is NULL");
        }
    }

    println!("\n=== MWE Tests Complete ===\n");
}

/// Test with just mux selection to isolate the issue
const SIMPLE_MUX_SOURCE: &str = r#"
async entity SimpleMux {
    in a: bit[8]
    in b: bit[8]
    in sel: bit[1]
    out result: bit[8]
}

impl SimpleMux {
    result = if sel == 1 {
        b
    } else {
        a
    }
}
"#;

#[test]
fn test_simple_ncl_mux() {
    println!("\n=== Simple NCL Mux Test ===\n");

    let hir = parse_and_build_hir(SIMPLE_MUX_SOURCE).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
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
        hw_accel: HwAccel::Gpu,
        max_iterations: 500,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    // Test 1: sel=0, should output a=10
    sim.set_ncl_input("top.a", 10, 8);
    sim.set_ncl_input("top.b", 20, 8);
    sim.set_ncl_input("top.sel", 0, 1);

    let result = sim.run_until_stable();
    println!(
        "Test (sel=0): iterations={}, stable={}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 8) {
        Some(value) => {
            println!("Result: {} (expected 10)", value);
            assert_eq!(value, 10);
        }
        None => panic!("Result is NULL"),
    }

    // Test 2: sel=1, should output b=20
    // Reset the simulation to clear previous state
    sim.reset();
    sim.set_ncl_input("top.a", 10, 8);
    sim.set_ncl_input("top.b", 20, 8);
    sim.set_ncl_input("top.sel", 1, 1);
    let result = sim.run_until_stable();
    println!(
        "Test (sel=1): iterations={}, stable={}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 8) {
        Some(value) => {
            println!("Result: {} (expected 20)", value);
            assert_eq!(value, 20);
        }
        None => panic!("Result is NULL"),
    }

    println!("Simple NCL mux test PASSED!\n");
}

/// Test with comparison operation only
const SIMPLE_LT_SOURCE: &str = r#"
async entity SimpleLt {
    in a: bit[8]
    in b: bit[8]
    out result: bit[1]
}

impl SimpleLt {
    result = if a < b { 1 } else { 0 }
}
"#;

#[test]
fn test_simple_ncl_lt() {
    println!("\n=== Simple NCL Less-Than Test ===\n");

    let hir = parse_and_build_hir(SIMPLE_LT_SOURCE).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
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
        hw_accel: HwAccel::Gpu,
        max_iterations: 500,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    // Test: 10 < 20 should be 1
    sim.set_ncl_input("top.a", 10, 8);
    sim.set_ncl_input("top.b", 20, 8);

    let result = sim.run_until_stable();
    println!(
        "Test (10 < 20): iterations={}, stable={}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 1) {
        Some(value) => {
            println!("Result: {} (expected 1)", value);
            assert_eq!(value, 1);
        }
        None => panic!("Result is NULL"),
    }

    // Test: 30 < 20 should be 0
    sim.reset();
    sim.set_ncl_input("top.a", 30, 8);
    sim.set_ncl_input("top.b", 20, 8);
    let result = sim.run_until_stable();
    println!(
        "Test (30 < 20): iterations={}, stable={}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 1) {
        Some(value) => {
            println!("Result: {} (expected 0)", value);
            assert_eq!(value, 0);
        }
        None => panic!("Result is NULL"),
    }

    println!("Simple NCL less-than test PASSED!\n");
}

/// Test simple 2-bit Eq comparison
const SIMPLE_EQ_SOURCE: &str = r#"
async entity SimpleEq {
    in a: bit[2]
    in b: bit[2]
    out result: bit[1]
}

impl SimpleEq {
    result = if a == b { 1 } else { 0 }
}
"#;

#[test]
fn test_simple_eq() {
    println!("\n=== Simple Eq Test ===\n");

    let hir = parse_and_build_hir(SIMPLE_EQ_SOURCE).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
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
        hw_accel: HwAccel::Gpu,
        max_iterations: 500,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    // Test: 2 == 2 should be 1
    sim.set_ncl_input("top.a", 2, 2);
    sim.set_ncl_input("top.b", 2, 2);

    let result = sim.run_until_stable();
    println!(
        "Test (2 == 2): iterations={}, stable={}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 1) {
        Some(value) => {
            println!("Result: {} (expected 1)", value);
            assert_eq!(value, 1);
        }
        None => panic!("Result is NULL"),
    }

    // Test: 1 == 2 should be 0
    sim.reset();
    sim.set_ncl_input("top.a", 1, 2);
    sim.set_ncl_input("top.b", 2, 2);

    let result = sim.run_until_stable();
    println!(
        "Test (1 == 2): iterations={}, stable={}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 1) {
        Some(value) => {
            println!("Result: {} (expected 0)", value);
            assert_eq!(value, 0);
        }
        None => panic!("Result is NULL"),
    }

    println!("Simple Eq test PASSED!\n");
}

/// Test if/else with selector
const SIMPLE_IF_ELSE_SOURCE: &str = r#"
async entity SimpleIfElse {
    in a: bit[8]
    in b: bit[8]
    in op: bit[2]
    out result: bit[8]
}

impl SimpleIfElse {
    result = if op == 0 {
        a + b
    } else if op == 1 {
        a - b
    } else {
        a & b
    }
}
"#;

#[test]
fn test_simple_if_else() {
    println!("\n=== Simple If/Else Test ===\n");

    let hir = parse_and_build_hir(SIMPLE_IF_ELSE_SOURCE).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
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
        hw_accel: HwAccel::Gpu,
        max_iterations: 500,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    // Test op=0: a + b
    sim.set_ncl_input("top.a", 10, 8);
    sim.set_ncl_input("top.b", 20, 8);
    sim.set_ncl_input("top.op", 0, 2);

    let result = sim.run_until_stable();
    println!(
        "Test (op=0, add): iterations={}, stable={}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 8) {
        Some(value) => {
            println!("10 + 20 = {} (expected 30)", value);
            assert_eq!(value, 30);
        }
        None => panic!("Result is NULL"),
    }

    // Test op=1: a - b
    sim.reset();
    sim.set_ncl_input("top.a", 30, 8);
    sim.set_ncl_input("top.b", 10, 8);
    sim.set_ncl_input("top.op", 1, 2);

    let result = sim.run_until_stable();
    println!(
        "Test (op=1, sub): iterations={}, stable={}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 8) {
        Some(value) => {
            println!("30 - 10 = {} (expected 20)", value);
            assert_eq!(value, 20);
        }
        None => panic!("Result is NULL"),
    }

    println!("Simple if/else test PASSED!\n");
}

/// Test just Lt + Gt (without Eq) to isolate interaction
const LT_GT_ONLY_SOURCE: &str = r#"
async entity LtGtOnly {
    in a: bit[8]
    in b: bit[8]
    in sel: bit[1]
    out result: bit[1]
}

impl LtGtOnly {
    signal lt_out: bit[1]
    signal gt_out: bit[1]

    lt_out = if a < b { 1 } else { 0 }
    gt_out = if a > b { 1 } else { 0 }

    result = if sel == 0 { lt_out } else { gt_out }
}
"#;

#[test]
fn test_lt_gt_only() {
    println!("\n=== Lt + Gt Only Test (without Eq) ===\n");

    let hir = parse_and_build_hir(LT_GT_ONLY_SOURCE).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
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
        hw_accel: HwAccel::Gpu,
        max_iterations: 500,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    // Test LT: 10 < 20 (sel=0)
    sim.set_ncl_input("top.a", 10, 8);
    sim.set_ncl_input("top.b", 20, 8);
    sim.set_ncl_input("top.sel", 0, 1);

    let result = sim.run_until_stable();
    println!(
        "Test LT (10 < 20, sel=0): iterations={}, stable={}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 1) {
        Some(value) => println!("Result: {} (expected 1)", value),
        None => println!("FAIL: Result is NULL"),
    }

    // Test GT: 30 > 20 (sel=1)
    sim.reset();
    sim.set_ncl_input("top.a", 30, 8);
    sim.set_ncl_input("top.b", 20, 8);
    sim.set_ncl_input("top.sel", 1, 1);

    let result = sim.run_until_stable();
    println!(
        "Test GT (30 > 20, sel=1): iterations={}, stable={}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 1) {
        Some(value) => println!("Result: {} (expected 1)", value),
        None => println!("FAIL: Result is NULL"),
    }

    // Test equal values: 20 == 20 (sel=0 for lt, should be 0)
    sim.reset();
    sim.set_ncl_input("top.a", 20, 8);
    sim.set_ncl_input("top.b", 20, 8);
    sim.set_ncl_input("top.sel", 0, 1);

    let result = sim.run_until_stable();
    println!(
        "Test LT (20 < 20, sel=0): iterations={}, stable={}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 1) {
        Some(value) => println!("Result: {} (expected 0)", value),
        None => println!("FAIL: Result is NULL"),
    }

    println!("Lt + Gt only test complete\n");
}

/// Test combined Lt, Eq, Gt (simulating cmp_l1 structure)
const COMBINED_CMP_SOURCE: &str = r#"
async entity CombinedCmp {
    in a: bit[8]
    in b: bit[8]
    in op: bit[2]
    out result: bit[8]
}

impl CombinedCmp {
    signal lt_out: bit[8]
    signal eq_out: bit[8]
    signal gt_out: bit[8]

    lt_out = if a < b { 1 } else { 0 }
    eq_out = if a == b { 1 } else { 0 }
    gt_out = if a > b { 1 } else { 0 }

    result = if op == 0 {
        lt_out
    } else if op == 1 {
        eq_out
    } else {
        gt_out
    }
}
"#;

#[test]
fn test_combined_cmp() {
    println!("\n=== Combined Cmp (Lt, Eq, Gt) Test ===\n");

    let hir = parse_and_build_hir(COMBINED_CMP_SOURCE).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
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
        hw_accel: HwAccel::Gpu,
        max_iterations: 500,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    // Test LT: 10 < 20 should be 1
    sim.set_ncl_input("top.a", 10, 8);
    sim.set_ncl_input("top.b", 20, 8);
    sim.set_ncl_input("top.op", 0, 2);

    let result = sim.run_until_stable();
    println!(
        "Test LT (10 < 20): iterations={}, stable={}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 8) {
        Some(value) => {
            println!("Result: {} (expected 1)", value);
            assert_eq!(value, 1);
        }
        None => println!("FAIL: Result is NULL"),
    }

    // Test EQ: 20 == 20 should be 1
    sim.reset();
    sim.set_ncl_input("top.a", 20, 8);
    sim.set_ncl_input("top.b", 20, 8);
    sim.set_ncl_input("top.op", 1, 2);

    let result = sim.run_until_stable();
    println!(
        "Test EQ (20 == 20): iterations={}, stable={}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 8) {
        Some(value) => {
            println!("Result: {} (expected 1)", value);
            assert_eq!(value, 1);
        }
        None => println!("FAIL: Result is NULL"),
    }

    // Test GT: 30 > 20 should be 1
    sim.reset();
    sim.set_ncl_input("top.a", 30, 8);
    sim.set_ncl_input("top.b", 20, 8);
    sim.set_ncl_input("top.op", 2, 2);

    let result = sim.run_until_stable();
    println!(
        "Test GT (30 > 20): iterations={}, stable={}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 8) {
        Some(value) => {
            println!("Result: {} (expected 1)", value);
            assert_eq!(value, 1);
        }
        None => println!("FAIL: Result is NULL"),
    }

    println!("Combined cmp test complete\n");
}

/// Test simple Gt comparison
const SIMPLE_GT_SOURCE: &str = r#"
async entity SimpleGt {
    in a: bit[8]
    in b: bit[8]
    out result: bit[1]
}

impl SimpleGt {
    result = if a > b { 1 } else { 0 }
}
"#;

#[test]
fn test_simple_ncl_gt() {
    println!("\n=== Simple NCL Greater-Than Test ===\n");

    let hir = parse_and_build_hir(SIMPLE_GT_SOURCE).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
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
        hw_accel: HwAccel::Gpu,
        max_iterations: 500,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    // Test: 30 > 20 should be 1
    sim.set_ncl_input("top.a", 30, 8);
    sim.set_ncl_input("top.b", 20, 8);

    let result = sim.run_until_stable();
    println!(
        "Test (30 > 20): iterations={}, stable={}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 1) {
        Some(value) => {
            println!("Result: {} (expected 1)", value);
            assert_eq!(value, 1);
        }
        None => panic!("Result is NULL"),
    }

    // Test: 10 > 20 should be 0
    sim.reset();
    sim.set_ncl_input("top.a", 10, 8);
    sim.set_ncl_input("top.b", 20, 8);
    let result = sim.run_until_stable();
    println!(
        "Test (10 > 20): iterations={}, stable={}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 1) {
        Some(value) => {
            println!("Result: {} (expected 0)", value);
            assert_eq!(value, 0);
        }
        None => panic!("Result is NULL"),
    }

    println!("Simple NCL greater-than test PASSED!\n");
}

/// Test with even simpler circuit to isolate issues
const SIMPLE_ADD_SOURCE: &str = r#"
async entity SimpleAdd {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[8]
}

impl SimpleAdd {
    sum = a + b
}
"#;

#[test]
fn test_simple_ncl_add() {
    println!("\n=== Simple NCL Add Test ===\n");

    let hir = parse_and_build_hir(SIMPLE_ADD_SOURCE).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
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
        hw_accel: HwAccel::Gpu,
        max_iterations: 1000,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    sim.set_ncl_input("top.a", 10, 8);
    sim.set_ncl_input("top.b", 20, 8);

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.sum", 8) {
        Some(value) => {
            println!("10 + 20 = {} (expected 30)", value);
            assert_eq!(value, 30);
        }
        None => panic!("Result is NULL - NCL simulation failed to produce valid output"),
    }

    println!("Simple NCL add test PASSED!\n");
}
