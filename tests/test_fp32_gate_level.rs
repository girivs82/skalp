// Test FP32 operations at gate level in NCL mode
// This isolates the FP32 logic from the full CLE

use skalp_frontend::parse_and_build_hir;
use skalp_lir::{get_stdlib_library, lower_mir_hierarchical, map_hierarchical_to_gates};
use skalp_mir::MirCompiler;
use skalp_sim::{CircuitMode, HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};

// Minimal FP32 add test
const FP32_ADD_MWE: &str = r#"
async entity FP32AddMWE {
    in a: bit[32]
    in b: bit[32]
    out result: bit[32]
}

impl FP32AddMWE {
    let a_fp = a as fp32;
    let b_fp = b as fp32;
    let sum = a_fp + b_fp;
    result = sum as bit[32]
}
"#;

// Simpler: just passthrough to test dual-rail encoding/decoding
const PASSTHROUGH: &str = r#"
async entity Passthrough {
    in a: bit[32]
    out result: bit[32]
}

impl Passthrough {
    result = a
}
"#;

// Integer add for comparison
const INT_ADD: &str = r#"
async entity IntAdd {
    in a: bit[32]
    in b: bit[32]
    out result: bit[32]
}

impl IntAdd {
    result = a + b
}
"#;

fn compile_and_simulate(source: &str, name: &str) -> Option<skalp_lir::GateNetlist> {
    println!("\n=== Compiling {} ===\n", name);

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
        "Netlist: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // Print some sample cells
    let fp_cells: Vec<_> = netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.contains("FP32") || c.cell_type.contains("fp"))
        .take(5)
        .collect();
    if !fp_cells.is_empty() {
        println!("FP32 cells found:");
        for cell in fp_cells {
            println!(
                "  {} ({}) - {} inputs, {} outputs",
                cell.path,
                cell.cell_type,
                cell.inputs.len(),
                cell.outputs.len()
            );
        }
    }

    Some(netlist)
}

#[test]
fn test_passthrough() {
    let netlist = compile_and_simulate(PASSTHROUGH, "Passthrough").unwrap();

    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Auto,
        max_iterations: 1000,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    // Test: input = 0x12345678, expect same output
    sim.set_ncl_input("top.a", 0x12345678, 32);

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 32) {
        Some(value) => {
            println!("Result: 0x{:08X} (expected 0x12345678)", value);
            assert_eq!(value, 0x12345678, "Passthrough should preserve value");
        }
        None => {
            println!("Result: NULL");
            panic!("Result should not be NULL");
        }
    }
}

#[test]
fn test_int_add_gate_level() {
    let netlist = compile_and_simulate(INT_ADD, "IntAdd").unwrap();

    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Auto,
        max_iterations: 1000,
        ncl_debug: false,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    // Test: 10 + 5 = 15
    sim.set_ncl_input("top.a", 10, 32);
    sim.set_ncl_input("top.b", 5, 32);

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 32) {
        Some(value) => {
            println!("Result: {} (expected 15)", value);
            assert_eq!(value, 15, "10 + 5 should equal 15");
        }
        None => {
            println!("Result: NULL");
            panic!("Result should not be NULL");
        }
    }
}

#[test]
fn test_fp32_add_gate_level() {
    let netlist = compile_and_simulate(FP32_ADD_MWE, "FP32AddMWE").unwrap();

    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Auto,
        max_iterations: 1000,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist)
        .expect("Failed to load NCL netlist");

    // Test: 2.0 + 3.0 = 5.0
    let a_bits = 2.0_f32.to_bits() as u64;
    let b_bits = 3.0_f32.to_bits() as u64;
    let expected_bits = 5.0_f32.to_bits() as u64;

    println!("Input A (2.0): 0x{:08X}", a_bits);
    println!("Input B (3.0): 0x{:08X}", b_bits);
    println!("Expected (5.0): 0x{:08X}", expected_bits);

    sim.set_ncl_input("top.a", a_bits, 32);
    sim.set_ncl_input("top.b", b_bits, 32);

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 32) {
        Some(value) => {
            let result_f32 = f32::from_bits(value as u32);
            println!(
                "Result: 0x{:08X} = {} (expected {} = 0x{:08X})",
                value, result_f32, 5.0_f32, expected_bits
            );

            if value == 0 {
                println!("FAIL: Result is 0, FP32 not working at gate level");
            } else if value == expected_bits {
                println!("PASS: FP32 add works correctly");
            } else {
                println!("UNEXPECTED: Got different value");
            }
        }
        None => {
            println!("Result: NULL");
        }
    }
}
