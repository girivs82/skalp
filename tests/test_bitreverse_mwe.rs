// Minimal bitreverse test to isolate the issue
//
// Note: These tests compile large circuits and may stack overflow in debug mode.
// Run with `cargo test --release` for reliable execution.

// Skip all tests in this file in debug mode due to stack overflow during compilation
#![cfg(not(debug_assertions))]

use skalp_frontend::parse_and_build_hir;
use skalp_lir::{get_stdlib_library, lower_mir_hierarchical, map_hierarchical_to_gates};
use skalp_mir::MirCompiler;
use skalp_sim::{CircuitMode, HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};

const BITREVERSE_MWE: &str = r#"
async entity BitreverseMWE {
    in data: bit[32]
    out result: bit[32]
}

impl BitreverseMWE {
    let mut x = data;

    // Swap consecutive pairs
    x = ((x & 0x55555555) << 1) | ((x >> 1) & 0x55555555);
    // Swap consecutive pairs of pairs
    x = ((x & 0x33333333) << 2) | ((x >> 2) & 0x33333333);
    // Swap nibbles
    x = ((x & 0x0F0F0F0F) << 4) | ((x >> 4) & 0x0F0F0F0F);
    // Swap bytes
    x = ((x & 0x00FF00FF) << 8) | ((x >> 8) & 0x00FF00FF);
    // Swap 16-bit halves
    x = (x << 16) | (x >> 16);

    result = x
}
"#;

// Simpler: just one step
const ONE_STEP: &str = r#"
async entity OneStep {
    in data: bit[32]
    out result: bit[32]
}

impl OneStep {
    let x = ((data & 0x55555555) << 1) | ((data >> 1) & 0x55555555);
    result = x
}
"#;

// Two steps
const TWO_STEPS: &str = r#"
async entity TwoSteps {
    in data: bit[32]
    out result: bit[32]
}

impl TwoSteps {
    let mut x = data;
    // Step 1: Swap consecutive pairs
    x = ((x & 0x55555555) << 1) | ((x >> 1) & 0x55555555);
    // Step 2: Swap pairs of pairs
    x = ((x & 0x33333333) << 2) | ((x >> 2) & 0x33333333);
    result = x
}
"#;

// Even simpler: passthrough
const PASSTHROUGH: &str = r#"
async entity Passthrough {
    in data: bit[32]
    out result: bit[32]
}

impl Passthrough {
    result = data
}
"#;

fn compile_and_test(source: &str, name: &str, input: u64, expected: u64) {
    println!("\n=== Testing {} ===", name);

    let hir = parse_and_build_hir(source).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    let hier_lir = lower_mir_hierarchical(&mir);
    let library = get_stdlib_library("generic_asic").expect("lib");
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
        max_iterations: 10000,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    sim.set_ncl_input("top.data", input, 32);

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 32) {
        Some(value) => {
            println!("Input:    0x{:08X}", input);
            println!("Got:      0x{:08X}", value);
            println!("Expected: 0x{:08X}", expected);
            if value == expected {
                println!("PASS");
            } else {
                println!("FAIL");
            }
        }
        None => {
            println!("Result: NULL");
        }
    }
}

#[test]
fn test_passthrough() {
    compile_and_test(PASSTHROUGH, "Passthrough", 0x12345678, 0x12345678);
}

#[test]
fn test_one_step() {
    // First step of bitreverse: swap consecutive pairs
    // 0x00000001 -> pairs swapped: 0x00000002
    compile_and_test(ONE_STEP, "OneStep", 0x00000001, 0x00000002);
}

#[test]
fn test_two_steps() {
    // First two steps of bitreverse
    // 0x00000001 -> step1: 0x00000002 -> step2: 0x00000008
    compile_and_test(TWO_STEPS, "TwoSteps", 0x00000001, 0x00000008);
}

#[test]
fn test_bitreverse_mwe() {
    compile_and_test(BITREVERSE_MWE, "BitreverseMWE", 0x00000001, 0x80000000);
}

// Test bitreverse as function call inside match expression
const FUNC_IN_MATCH: &str = r#"
pub fn bitreverse32(value: bit[32]) -> bit[32] {
    let mut x = value;
    x = ((x & 0x55555555) << 1) | ((x >> 1) & 0x55555555);
    x = ((x & 0x33333333) << 2) | ((x >> 2) & 0x33333333);
    x = ((x & 0x0F0F0F0F) << 4) | ((x >> 4) & 0x0F0F0F0F);
    x = ((x & 0x00FF00FF) << 8) | ((x >> 8) & 0x00FF00FF);
    x = (x << 16) | (x >> 16);
    return x
}

async entity FuncInMatch {
    in opcode: bit[6]
    in data: bit[256]
    out result: bit[256]
    out debug: bit[32]
}

impl FuncInMatch {
    result = match opcode {
        58 => {
            let value = data[31:0];
            let reversed = bitreverse32(value);
            {224'b0, reversed}
        },
        _ => 256'b0
    }
    
    debug = result[31:0]
}
"#;

#[test]
fn test_func_in_match() {
    println!("\n=== Testing FuncInMatch ===");

    let hir = parse_and_build_hir(FUNC_IN_MATCH).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    let hier_lir = lower_mir_hierarchical(&mir);
    let library = get_stdlib_library("generic_asic").expect("lib");
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
        max_iterations: 10000,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    sim.set_ncl_input("top.opcode", 58, 6);
    sim.set_ncl_input("top.data", 0x00000001, 256);

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.debug", 32) {
        Some(value) => {
            println!("Input:    0x00000001");
            println!("Got:      0x{:08X}", value);
            println!("Expected: 0x80000000");
            if value == 0x80000000 {
                println!("PASS");
            } else {
                println!("FAIL");
                panic!("Function in match test failed");
            }
        }
        None => {
            println!("Result: NULL");
            panic!("Result is NULL");
        }
    }
}

// Test CLE-like structure with enum opcodes
const CLE_LIKE: &str = r#"
pub enum L5Opcode: bit[6] {
    POPCOUNT    = 57,
    BITREVERSE  = 58
}

pub fn bitreverse32(value: bit[32]) -> bit[32] {
    let mut x = value;
    x = ((x & 0x55555555) << 1) | ((x >> 1) & 0x55555555);
    x = ((x & 0x33333333) << 2) | ((x >> 2) & 0x33333333);
    x = ((x & 0x0F0F0F0F) << 4) | ((x >> 4) & 0x0F0F0F0F);
    x = ((x & 0x00FF00FF) << 8) | ((x >> 8) & 0x00FF00FF);
    x = (x << 16) | (x >> 16);
    return x
}

pub fn popcount32(value: bit[32]) -> nat[6] {
    let mut x = value;
    x = (x & 0x55555555) + ((x >> 1) & 0x55555555);
    x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
    x = (x & 0x0F0F0F0F) + ((x >> 4) & 0x0F0F0F0F);
    x = (x & 0x00FF00FF) + ((x >> 8) & 0x00FF00FF);
    x = (x & 0x0000FFFF) + ((x >> 16) & 0x0000FFFF);
    return x[5:0] as nat[6]
}

pub fn exec_l4_l5(opcode: bit[6], data1: bit[256], data2: bit[256]) -> bit[256] {
    return match opcode {
        L5Opcode::POPCOUNT => {
            let value = data1[31:0];
            let count = popcount32(value);
            {224'b0, count as bit[32]}
        },
        L5Opcode::BITREVERSE => {
            let value = data1[31:0];
            let reversed = bitreverse32(value);
            {224'b0, reversed}
        },
        _ => 256'b0
    }
}

async entity CleLikeBitreverse {
    in function_sel: bit[6]
    in data1: bit[256]
    out debug_l4_l5: bit[32]
}

impl CleLikeBitreverse {
    signal l4_l5_result: bit[256]
    l4_l5_result = exec_l4_l5(function_sel, data1, 0)
    debug_l4_l5 = l4_l5_result[31:0]
}
"#;

#[test]
fn test_cle_like_popcount() {
    println!("\n=== Testing CLE-like POPCOUNT ===");

    let hir = parse_and_build_hir(CLE_LIKE).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    let hier_lir = lower_mir_hierarchical(&mir);
    let library = get_stdlib_library("generic_asic").expect("lib");
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
        max_iterations: 10000,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    sim.set_ncl_input("top.function_sel", 57, 6); // POPCOUNT
    sim.set_ncl_input("top.data1", 1, 256); // Try with just 1 bit set for simpler test

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    // Debug: Try multiple output name variations
    for name in &[
        "top.debug_l4_l5",
        "debug_l4_l5",
        "top.l4_l5_result",
        "l4_l5_result",
    ] {
        let result = sim.get_ncl_output(name, 32);
        println!("get_ncl_output('{}', 32) = {:?}", name, result);
    }

    match sim.get_ncl_output("top.debug_l4_l5", 32) {
        Some(value) => {
            println!("Input:    1 (1 bit set)");
            println!("Got:      {}", value);
            println!("Expected: 1");
            if value == 1 {
                println!("PASS");
            } else {
                println!("FAIL");
                panic!("CLE-like POPCOUNT test failed");
            }
        }
        None => {
            println!("Result: NULL");
            panic!("Result is NULL");
        }
    }
}

#[test]
fn test_cle_like_bitreverse() {
    println!("\n=== Testing CLE-like BITREVERSE ===");

    let hir = parse_and_build_hir(CLE_LIKE).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    let hier_lir = lower_mir_hierarchical(&mir);
    let library = get_stdlib_library("generic_asic").expect("lib");
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
        max_iterations: 10000,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    sim.set_ncl_input("top.function_sel", 58, 6); // BITREVERSE
    sim.set_ncl_input("top.data1", 0x00000001, 256);

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.debug_l4_l5", 32) {
        Some(value) => {
            println!("Input:    0x00000001");
            println!("Got:      0x{:08X}", value);
            println!("Expected: 0x80000000");
            if value == 0x80000000 {
                println!("PASS");
            } else {
                println!("FAIL");
                panic!("CLE-like BITREVERSE test failed");
            }
        }
        None => {
            println!("Result: NULL");
            panic!("Result is NULL");
        }
    }
}

#[test]
fn test_cle_like_signals_debug() {
    println!("\n=== Testing CLE-like signals debug ===");

    let hir = parse_and_build_hir(CLE_LIKE).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    let hier_lir = lower_mir_hierarchical(&mir);
    let library = get_stdlib_library("generic_asic").expect("lib");
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
        max_iterations: 10000,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist.clone())
        .expect("Failed to load NCL netlist");

    // List all signals that contain "debug"
    let all_signals = sim.ncl_signal_names();
    let debug_signals: Vec<_> = all_signals.iter().filter(|s| s.contains("debug")).collect();
    println!("Debug signals: {:?}", debug_signals);

    // List signals containing "l4_l5"
    let l4_l5_signals: Vec<_> = all_signals.iter().filter(|s| s.contains("l4_l5")).collect();
    println!("L4_L5 signals: {:?}", l4_l5_signals);

    // Test both POPCOUNT and BITREVERSE back to back
    println!("\n--- POPCOUNT test ---");
    sim.set_ncl_input("top.function_sel", 57, 6);
    sim.set_ncl_input("top.data1", 0xFFFFFFFF, 256);
    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );
    match sim.get_ncl_output("top.debug_l4_l5", 32) {
        Some(v) => println!("POPCOUNT result: {}", v),
        None => println!("POPCOUNT result: NULL"),
    }

    println!("\n--- BITREVERSE test ---");
    sim.reset();
    sim.set_ncl_input("top.function_sel", 58, 6);
    sim.set_ncl_input("top.data1", 0x00000001, 256);
    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );
    match sim.get_ncl_output("top.debug_l4_l5", 32) {
        Some(v) => println!("BITREVERSE result: 0x{:08X}", v),
        None => println!("BITREVERSE result: NULL"),
    }
}

// Test with swapped match arm order
const CLE_LIKE_SWAPPED: &str = r#"
pub enum L5Opcode: bit[6] {
    POPCOUNT    = 57,
    BITREVERSE  = 58
}

pub fn bitreverse32(value: bit[32]) -> bit[32] {
    let mut x = value;
    x = ((x & 0x55555555) << 1) | ((x >> 1) & 0x55555555);
    x = ((x & 0x33333333) << 2) | ((x >> 2) & 0x33333333);
    x = ((x & 0x0F0F0F0F) << 4) | ((x >> 4) & 0x0F0F0F0F);
    x = ((x & 0x00FF00FF) << 8) | ((x >> 8) & 0x00FF00FF);
    x = (x << 16) | (x >> 16);
    return x
}

pub fn popcount32(value: bit[32]) -> nat[6] {
    let mut x = value;
    x = (x & 0x55555555) + ((x >> 1) & 0x55555555);
    x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
    x = (x & 0x0F0F0F0F) + ((x >> 4) & 0x0F0F0F0F);
    x = (x & 0x00FF00FF) + ((x >> 8) & 0x00FF00FF);
    x = (x & 0x0000FFFF) + ((x >> 16) & 0x0000FFFF);
    return x[5:0] as nat[6]
}

pub fn exec_l4_l5(opcode: bit[6], data1: bit[256], data2: bit[256]) -> bit[256] {
    // SWAPPED ORDER: BITREVERSE first, then POPCOUNT
    return match opcode {
        L5Opcode::BITREVERSE => {
            let value = data1[31:0];
            let reversed = bitreverse32(value);
            {224'b0, reversed}
        },
        L5Opcode::POPCOUNT => {
            let value = data1[31:0];
            let count = popcount32(value);
            {224'b0, count as bit[32]}
        },
        _ => 256'b0
    }
}

async entity CleLikeSwapped {
    in function_sel: bit[6]
    in data1: bit[256]
    out debug_l4_l5: bit[32]
}

impl CleLikeSwapped {
    signal l4_l5_result: bit[256]
    l4_l5_result = exec_l4_l5(function_sel, data1, 0)
    debug_l4_l5 = l4_l5_result[31:0]
}
"#;

#[test]
fn test_cle_like_swapped() {
    println!("\n=== Testing CLE-like SWAPPED order ===");

    let hir = parse_and_build_hir(CLE_LIKE_SWAPPED).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    let hier_lir = lower_mir_hierarchical(&mir);
    let library = get_stdlib_library("generic_asic").expect("lib");
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
        max_iterations: 10000,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    // Test POPCOUNT (now second arm)
    println!("\n--- POPCOUNT test (now second arm) ---");
    sim.set_ncl_input("top.function_sel", 57, 6);
    sim.set_ncl_input("top.data1", 0xFFFFFFFF, 256);
    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );
    match sim.get_ncl_output("top.debug_l4_l5", 32) {
        Some(v) => println!("POPCOUNT result: {} (expected 32)", v),
        None => println!("POPCOUNT result: NULL"),
    }

    // Test BITREVERSE (now first arm)
    println!("\n--- BITREVERSE test (now first arm) ---");
    sim.reset();
    sim.set_ncl_input("top.function_sel", 58, 6);
    sim.set_ncl_input("top.data1", 0x00000001, 256);
    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );
    match sim.get_ncl_output("top.debug_l4_l5", 32) {
        Some(v) => println!("BITREVERSE result: 0x{:08X} (expected 0x80000000)", v),
        None => println!("BITREVERSE result: NULL"),
    }
}

// Test popcount returning bit[32] directly (no nat conversion)
const POPCOUNT_DIRECT: &str = r#"
pub fn popcount32_direct(value: bit[32]) -> bit[32] {
    let mut x = value;
    x = (x & 0x55555555) + ((x >> 1) & 0x55555555);
    x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
    x = (x & 0x0F0F0F0F) + ((x >> 4) & 0x0F0F0F0F);
    x = (x & 0x00FF00FF) + ((x >> 8) & 0x00FF00FF);
    x = (x & 0x0000FFFF) + ((x >> 16) & 0x0000FFFF);
    return x
}

async entity PopcountDirect {
    in data: bit[32]
    out result: bit[32]
}

impl PopcountDirect {
    result = popcount32_direct(data)
}
"#;

#[test]
fn test_popcount_direct() {
    println!("\n=== Testing popcount_direct (returns bit[32]) ===");

    let hir = parse_and_build_hir(POPCOUNT_DIRECT).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    let hier_lir = lower_mir_hierarchical(&mir);
    let library = get_stdlib_library("generic_asic").expect("lib");
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
        max_iterations: 10000,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    sim.set_ncl_input("top.data", 0xFFFFFFFF, 32);
    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 32) {
        Some(v) => println!("Result: {} (expected 32)", v),
        None => println!("Result: NULL"),
    }
}

// Test popcount returning nat[6] (original)
const POPCOUNT_NAT: &str = r#"
pub fn popcount32_nat(value: bit[32]) -> nat[6] {
    let mut x = value;
    x = (x & 0x55555555) + ((x >> 1) & 0x55555555);
    x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
    x = (x & 0x0F0F0F0F) + ((x >> 4) & 0x0F0F0F0F);
    x = (x & 0x00FF00FF) + ((x >> 8) & 0x00FF00FF);
    x = (x & 0x0000FFFF) + ((x >> 16) & 0x0000FFFF);
    return x[5:0] as nat[6]
}

async entity PopcountNat {
    in data: bit[32]
    out result: bit[32]
}

impl PopcountNat {
    let count = popcount32_nat(data);
    result = count as bit[32]
}
"#;

#[test]
fn test_popcount_nat() {
    println!("\n=== Testing popcount_nat (returns nat[6]) ===");

    let hir = parse_and_build_hir(POPCOUNT_NAT).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    let hier_lir = lower_mir_hierarchical(&mir);
    let library = get_stdlib_library("generic_asic").expect("lib");
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
        max_iterations: 10000,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    sim.set_ncl_input("top.data", 0xFFFFFFFF, 32);
    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 32) {
        Some(v) => println!("Result: {} (expected 32)", v),
        None => println!("Result: NULL"),
    }
}

// Debug: check what values are being read from intermediate signals
const DEBUG_INPUT_CHECK: &str = r#"
pub fn bitreverse32(value: bit[32]) -> bit[32] {
    let mut x = value;
    x = ((x & 0x55555555) << 1) | ((x >> 1) & 0x55555555);
    x = ((x & 0x33333333) << 2) | ((x >> 2) & 0x33333333);
    x = ((x & 0x0F0F0F0F) << 4) | ((x >> 4) & 0x0F0F0F0F);
    x = ((x & 0x00FF00FF) << 8) | ((x >> 8) & 0x00FF00FF);
    x = (x << 16) | (x >> 16);
    return x
}

async entity DebugInputCheck {
    in opcode: bit[6]
    in data: bit[256]
    out result: bit[256]
    out debug_opcode: bit[6]
    out debug_value: bit[32]
    out debug_reversed: bit[32]
}

impl DebugInputCheck {
    // Extract value first
    signal value: bit[32]
    value = data[31:0]
    
    // Compute reverse
    signal reversed: bit[32]
    reversed = bitreverse32(value)
    
    // Output based on opcode
    result = if opcode == 58 {
        {224'b0, reversed}
    } else {
        256'b0
    }
    
    // Debug outputs
    debug_opcode = opcode
    debug_value = value
    debug_reversed = reversed
}
"#;

#[test]
fn test_debug_input_check() {
    println!("\n=== Debug Input Check ===");

    let hir = parse_and_build_hir(DEBUG_INPUT_CHECK).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    let hier_lir = lower_mir_hierarchical(&mir);
    let library = get_stdlib_library("generic_asic").expect("lib");
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
        max_iterations: 10000,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    // Set inputs
    sim.set_ncl_input("top.opcode", 58, 6);
    sim.set_ncl_input("top.data", 0x00000001, 256);

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    // Read debug values
    match sim.get_ncl_output("top.debug_opcode", 6) {
        Some(v) => println!("debug_opcode: {} (expected 58)", v),
        None => println!("debug_opcode: NULL"),
    }
    match sim.get_ncl_output("top.debug_value", 32) {
        Some(v) => println!("debug_value: 0x{:08X} (expected 0x00000001)", v),
        None => println!("debug_value: NULL"),
    }
    match sim.get_ncl_output("top.debug_reversed", 32) {
        Some(v) => println!("debug_reversed: 0x{:08X} (expected 0x80000000)", v),
        None => println!("debug_reversed: NULL"),
    }
    match sim.get_ncl_output("top.result", 256) {
        Some(v) => println!(
            "result[31:0]: 0x{:08X} (expected 0x80000000)",
            v & 0xFFFFFFFF
        ),
        None => println!("result: NULL"),
    }
}

// Test: is the issue the concat or the conditional?
const CONCAT_ONLY: &str = r#"
pub fn bitreverse32(value: bit[32]) -> bit[32] {
    let mut x = value;
    x = ((x & 0x55555555) << 1) | ((x >> 1) & 0x55555555);
    x = ((x & 0x33333333) << 2) | ((x >> 2) & 0x33333333);
    x = ((x & 0x0F0F0F0F) << 4) | ((x >> 4) & 0x0F0F0F0F);
    x = ((x & 0x00FF00FF) << 8) | ((x >> 8) & 0x00FF00FF);
    x = (x << 16) | (x >> 16);
    return x
}

async entity ConcatOnly {
    in data: bit[256]
    out result: bit[256]
    out debug_result32: bit[32]
}

impl ConcatOnly {
    signal value: bit[32]
    value = data[31:0]
    
    signal reversed: bit[32]
    reversed = bitreverse32(value)
    
    // No conditional, just concat
    result = {224'b0, reversed}
    debug_result32 = result[31:0]
}
"#;

#[test]
fn test_concat_only() {
    println!("\n=== Test Concat Only ===");

    let hir = parse_and_build_hir(CONCAT_ONLY).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    let hier_lir = lower_mir_hierarchical(&mir);
    let library = get_stdlib_library("generic_asic").expect("lib");
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
        max_iterations: 10000,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    sim.set_ncl_input("top.data", 0x00000001, 256);

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 256) {
        Some(v) => println!(
            "result (256-bit): lower 32 bits = 0x{:08X} (expected 0x80000000)",
            v & 0xFFFFFFFF
        ),
        None => println!("result (256-bit): NULL"),
    }
    match sim.get_ncl_output("top.debug_result32", 32) {
        Some(v) => println!("debug_result32: 0x{:08X} (expected 0x80000000)", v),
        None => println!("debug_result32: NULL"),
    }
}

// Test: does a 256-bit constant work?
const CONSTANT_256: &str = r#"
async entity Constant256 {
    out result: bit[256]
}

impl Constant256 {
    result = 256'b0
}
"#;

// Test: does a 64-bit constant with concat work?
const CONSTANT_CONCAT: &str = r#"
async entity ConstantConcat {
    in data: bit[32]
    out result: bit[64]
}

impl ConstantConcat {
    result = {32'b0, data}
}
"#;

#[test]
fn test_constant_256() {
    println!("\n=== Test 256-bit Constant ===");

    let hir = parse_and_build_hir(CONSTANT_256).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    let hier_lir = lower_mir_hierarchical(&mir);
    let library = get_stdlib_library("generic_asic").expect("lib");
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
        max_iterations: 10000,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 256) {
        Some(v) => println!("result: {} (expected 0)", v),
        None => println!("result: NULL"),
    }
}

#[test]
fn test_constant_concat_64() {
    println!("\n=== Test 64-bit Concat ===");

    let hir = parse_and_build_hir(CONSTANT_CONCAT).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    let hier_lir = lower_mir_hierarchical(&mir);
    let library = get_stdlib_library("generic_asic").expect("lib");
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
        max_iterations: 10000,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    sim.set_ncl_input("top.data", 0xABCD1234, 32);

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 64) {
        Some(v) => println!("result: 0x{:016X} (expected 0x00000000ABCD1234)", v),
        None => println!("result: NULL"),
    }
}

// Test with #[parallel] attribute like the CLE
const PARALLEL_MUX: &str = r#"
pub fn bitreverse32(value: bit[32]) -> bit[32] {
    let mut x = value;
    x = ((x & 0x55555555) << 1) | ((x >> 1) & 0x55555555);
    x = ((x & 0x33333333) << 2) | ((x >> 2) & 0x33333333);
    x = ((x & 0x0F0F0F0F) << 4) | ((x >> 4) & 0x0F0F0F0F);
    x = ((x & 0x00FF00FF) << 8) | ((x >> 8) & 0x00FF00FF);
    x = (x << 16) | (x >> 16);
    return x
}

pub fn popcount32(value: bit[32]) -> bit[32] {
    let mut x = value;
    x = (x & 0x55555555) + ((x >> 1) & 0x55555555);
    x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
    x = (x & 0x0F0F0F0F) + ((x >> 4) & 0x0F0F0F0F);
    x = (x & 0x00FF00FF) + ((x >> 8) & 0x00FF00FF);
    x = (x & 0x0000FFFF) + ((x >> 16) & 0x0000FFFF);
    return x
}

pub fn exec_test(opcode: bit[6], data1: bit[32]) -> bit[32] {
    #[parallel]
    return match opcode {
        // Add several arms like CLE
        50 => data1 + 1,
        51 => data1 + 2,
        52 => data1 + 3,
        53 => data1 + 4,
        54 => data1 + 5,
        55 => data1 + 6,
        56 => data1 + 7,
        57 => popcount32(data1),
        58 => bitreverse32(data1),
        59 => data1 + 10,
        60 => data1 + 11,
        _ => 0
    }
}

async entity ParallelMux {
    in function_sel: bit[6]
    in data1: bit[32]
    out result: bit[32]
}

impl ParallelMux {
    result = exec_test(function_sel, data1)
}
"#;

#[test]
fn test_parallel_mux_popcount() {
    println!("\n=== Test Parallel Mux POPCOUNT ===");

    let hir = parse_and_build_hir(PARALLEL_MUX).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    let hier_lir = lower_mir_hierarchical(&mir);
    let library = get_stdlib_library("generic_asic").expect("lib");
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
        max_iterations: 10000,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    sim.set_ncl_input("top.function_sel", 57, 6);
    sim.set_ncl_input("top.data1", 0xFFFFFFFF, 32);

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 32) {
        Some(v) => println!("POPCOUNT result: {} (expected 32)", v),
        None => println!("POPCOUNT result: NULL"),
    }
}

#[test]
fn test_parallel_mux_bitreverse() {
    println!("\n=== Test Parallel Mux BITREVERSE ===");

    let hir = parse_and_build_hir(PARALLEL_MUX).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    let hier_lir = lower_mir_hierarchical(&mir);
    let library = get_stdlib_library("generic_asic").expect("lib");
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
        max_iterations: 10000,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    sim.set_ncl_input("top.function_sel", 58, 6);
    sim.set_ncl_input("top.data1", 0x00000001, 32);

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 32) {
        Some(v) => println!("BITREVERSE result: 0x{:08X} (expected 0x80000000)", v),
        None => println!("BITREVERSE result: NULL"),
    }
}
