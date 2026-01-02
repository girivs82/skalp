use skalp_frontend::parse_and_build_hir;
use skalp_lir::{get_stdlib_library, lower_mir_hierarchical, map_hierarchical_to_gates};
use skalp_mir::MirCompiler;
use skalp_sim::{CircuitMode, HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};

// This uses bit[32] return type - WORKS
const SIMPLE_MUX_BIT32: &str = r#"
pub fn popcount32(value: bit[32]) -> bit[32] {
    let mut x = value;
    x = (x & 0x55555555) + ((x >> 1) & 0x55555555);
    x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
    x = (x & 0x0F0F0F0F) + ((x >> 4) & 0x0F0F0F0F);
    x = (x & 0x00FF00FF) + ((x >> 8) & 0x00FF00FF);
    x = (x & 0x0000FFFF) + ((x >> 16) & 0x0000FFFF);
    return x
}

async entity TestMux {
    in opcode: bit[6]
    in data: bit[32]
    out result: bit[32]
}

impl TestMux {
    result = match opcode {
        57 => popcount32(data),
        _ => 32'b0
    }
}
"#;

// This uses nat[6] return type with cast - FAILS
const SIMPLE_MUX_NAT6: &str = r#"
pub fn popcount32(value: bit[32]) -> nat[6] {
    let mut x = value;
    x = (x & 0x55555555) + ((x >> 1) & 0x55555555);
    x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
    x = (x & 0x0F0F0F0F) + ((x >> 4) & 0x0F0F0F0F);
    x = (x & 0x00FF00FF) + ((x >> 8) & 0x00FF00FF);
    x = (x & 0x0000FFFF) + ((x >> 16) & 0x0000FFFF);
    return x[5:0] as nat[6]
}

async entity TestMux {
    in opcode: bit[6]
    in data: bit[32]
    out result: bit[32]
}

impl TestMux {
    result = match opcode {
        57 => popcount32(data) as bit[32],
        _ => 32'b0
    }
}
"#;

// Test simple [5:0] extraction - does this work?
const SIMPLE_EXTRACT: &str = r#"
async entity TestExtract {
    in data: bit[32]
    out result: bit[6]
}

impl TestExtract {
    result = data[5:0]
}
"#;

// Test simplified popcount - just first step
const SIMPLE_POPCOUNT_STEP1: &str = r#"
async entity TestPopcount1 {
    in data: bit[32]
    out result: bit[32]
}

impl TestPopcount1 {
    result = (data & 0x55555555) + ((data >> 1) & 0x55555555)
}
"#;

// Test simplified popcount returning 6-bit
#[allow(dead_code)]
const SIMPLE_POPCOUNT_NAT6: &str = r#"
async entity TestPopcount6 {
    in data: bit[32]
    out result: bit[6]
}

impl TestPopcount6 {
    let x = (data & 0x55555555) + ((data >> 1) & 0x55555555);
    result = x[5:0]
}
"#;

// Test full popcount returning bit[32] (no extraction)
const FULL_POPCOUNT_BIT32: &str = r#"
async entity TestPopcount32 {
    in data: bit[32]
    out result: bit[32]
}

impl TestPopcount32 {
    let mut x = data;
    x = (x & 0x55555555) + ((x >> 1) & 0x55555555);
    x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
    x = (x & 0x0F0F0F0F) + ((x >> 4) & 0x0F0F0F0F);
    x = (x & 0x00FF00FF) + ((x >> 8) & 0x00FF00FF);
    x = (x & 0x0000FFFF) + ((x >> 16) & 0x0000FFFF);
    result = x
}
"#;

// Test full popcount but extract [5:0] at the end
const FULL_POPCOUNT_EXTRACT: &str = r#"
async entity TestPopcountEx {
    in data: bit[32]
    out result: bit[6]
}

impl TestPopcountEx {
    let mut x = data;
    x = (x & 0x55555555) + ((x >> 1) & 0x55555555);
    x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
    x = (x & 0x0F0F0F0F) + ((x >> 4) & 0x0F0F0F0F);
    x = (x & 0x00FF00FF) + ((x >> 8) & 0x00FF00FF);
    x = (x & 0x0000FFFF) + ((x >> 16) & 0x0000FFFF);
    result = x[5:0]
}
"#;

// Test zero extension from 6-bit to 32-bit
const ZERO_EXTEND: &str = r#"
async entity TestZeroExtend {
    in data: bit[6]
    out result: bit[32]
}

impl TestZeroExtend {
    result = {26'b0, data}
}
"#;

// Direct cast without match - test this first
const DIRECT_CAST: &str = r#"
pub fn popcount32(value: bit[32]) -> nat[6] {
    let mut x = value;
    x = (x & 0x55555555) + ((x >> 1) & 0x55555555);
    x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
    x = (x & 0x0F0F0F0F) + ((x >> 4) & 0x0F0F0F0F);
    x = (x & 0x00FF00FF) + ((x >> 8) & 0x00FF00FF);
    x = (x & 0x0000FFFF) + ((x >> 16) & 0x0000FFFF);
    return x[5:0] as nat[6]
}

async entity TestDirect {
    in data: bit[32]
    out result: bit[32]
}

impl TestDirect {
    result = popcount32(data) as bit[32]
}
"#;

fn compile_and_simulate_src(source: &str, inputs: &[(&str, u64, usize)]) -> Option<u64> {
    let hir = parse_and_build_hir(source).expect("parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler.compile(&hir).expect("mir");

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

    let mut sim = UnifiedSimulator::new(config).expect("sim");
    sim.load_ncl_gate_level(netlist).expect("load");

    for (name, value, width) in inputs {
        sim.set_ncl_input(name, *value, *width);
    }
    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    sim.get_ncl_output("top.result", 32)
}

fn compile_and_simulate_any_output(
    source: &str,
    inputs: &[(&str, u64, usize)],
    output_name: &str,
    output_width: usize,
) -> Option<u64> {
    let hir = parse_and_build_hir(source).expect("parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler.compile(&hir).expect("mir");

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

    let mut sim = UnifiedSimulator::new(config).expect("sim");
    sim.load_ncl_gate_level(netlist).expect("load");

    for (name, value, width) in inputs {
        sim.set_ncl_input(name, *value, *width);
    }
    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    sim.get_ncl_output(output_name, output_width)
}

#[test]
fn test_full_popcount_bit32() {
    println!("\n=== Testing full popcount returning bit[32] ===");
    let result = compile_and_simulate_any_output(
        FULL_POPCOUNT_BIT32,
        &[("top.data", 0xFFFFFFFF, 32)], // All 32 bits set
        "top.result",
        32,
    );
    match result {
        Some(v) => {
            println!("Result: {} (expected 32)", v);
            assert_eq!(v, 32);
        }
        None => panic!("Result is NULL"),
    }
}

#[test]
fn test_full_popcount_extract6() {
    println!("\n=== Testing full popcount with [5:0] extraction - multiple tests ===");

    // Test with 1 bit set
    let result = compile_and_simulate_any_output(
        FULL_POPCOUNT_EXTRACT,
        &[("top.data", 1, 32)],
        "top.result",
        6,
    );
    assert_eq!(result, Some(1), "popcount(1) should be 1");

    // Test with 5 bits set
    let result = compile_and_simulate_any_output(
        FULL_POPCOUNT_EXTRACT,
        &[("top.data", 0x1F, 32)], // 5 bits set
        "top.result",
        6,
    );
    assert_eq!(result, Some(5), "popcount(0x1F) should be 5");

    // Test with 31 bits set (should not overflow 6 bits)
    let result = compile_and_simulate_any_output(
        FULL_POPCOUNT_EXTRACT,
        &[("top.data", 0x7FFFFFFF, 32)], // 31 bits set
        "top.result",
        6,
    );
    println!("popcount(0x7FFFFFFF) result: {:?}", result);
    assert_eq!(result, Some(31), "popcount(0x7FFFFFFF) should be 31");

    // Test with 32 bits set (uses bit 5)
    let result = compile_and_simulate_any_output(
        FULL_POPCOUNT_EXTRACT,
        &[("top.data", 0xFFFFFFFF, 32)], // 32 bits set
        "top.result",
        6,
    );
    println!("popcount(0xFFFFFFFF) result: {:?}", result);
    // 32 = 0b100000, so x[5:0] = 32
    assert_eq!(result, Some(32), "popcount(0xFFFFFFFF) should be 32");
}

#[test]
fn test_popcount_step1() {
    println!("\n=== Testing popcount step 1 only ===");
    let result = compile_and_simulate_any_output(
        SIMPLE_POPCOUNT_STEP1,
        &[("top.data", 0xF, 32)], // 4 bits set (0b1111)
        "top.result",
        32,
    );
    match result {
        Some(v) => {
            // Step 1: count pairs - 0xF = 0b1111 -> each pair 01 + 01 = 10, so 0b1010
            // After step 1 with 0xF, the result should be 2 in each 2-bit field
            println!("Result: 0x{:X}", v);
            assert!(v > 0, "Result should not be 0");
        }
        None => panic!("Result is NULL"),
    }
}

#[test]
fn test_zero_extend() {
    println!("\n=== Testing zero extension 6-bit to 32-bit ===");
    let result = compile_and_simulate_any_output(
        ZERO_EXTEND,
        &[("top.data", 32, 6)], // 32 = 0b100000
        "top.result",
        32,
    );
    match result {
        Some(v) => {
            println!("Result: {} (expected 32)", v);
            assert_eq!(v, 32);
        }
        None => panic!("Result is NULL"),
    }
}

#[test]
fn test_simple_extract() {
    println!("\n=== Testing simple [5:0] extraction ===");
    let result = compile_and_simulate_any_output(
        SIMPLE_EXTRACT,
        &[("top.data", 0xFF, 32)], // All 1s in low 8 bits
        "top.result",
        6,
    );
    match result {
        Some(v) => {
            println!("Result: {} (expected 63)", v);
            assert_eq!(v, 63); // 0b111111 = 63
        }
        None => panic!("Result is NULL"),
    }
}

#[test]
fn test_direct_cast_works() {
    println!("\n=== Testing direct cast (no match) ===");
    let result = compile_and_simulate_src(DIRECT_CAST, &[("top.data", 1, 32)]);
    match result {
        Some(v) => {
            println!("Result: {} (expected 1)", v);
            assert_eq!(v, 1);
        }
        None => panic!("Result is NULL"),
    }
}

#[test]
fn test_popcount_bit32_in_mux() {
    println!("\n=== Testing POPCOUNT (bit[32]) with input=1 ===");
    let result = compile_and_simulate_src(
        SIMPLE_MUX_BIT32,
        &[("top.opcode", 57, 6), ("top.data", 1, 32)],
    );
    match result {
        Some(v) => {
            println!("Result: {} (expected 1)", v);
            assert_eq!(v, 1);
        }
        None => panic!("Result is NULL"),
    }
}

#[test]
fn test_popcount_nat6_in_mux() {
    println!("\n=== Testing POPCOUNT (nat[6]) with input=1 ===");
    let result = compile_and_simulate_src(
        SIMPLE_MUX_NAT6,
        &[("top.opcode", 57, 6), ("top.data", 1, 32)],
    );
    match result {
        Some(v) => {
            println!("Result: {} (expected 1)", v);
            assert_eq!(v, 1);
        }
        None => panic!("Result is NULL - BUG: nat[6] cast in match fails"),
    }
}
