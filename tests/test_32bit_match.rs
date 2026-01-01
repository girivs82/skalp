// Test 32-bit match with multiple arms
// This is closer to the actual L0/L1 structure

use skalp_frontend::parse_and_build_hir;
use skalp_lir::{get_stdlib_library, lower_mir_hierarchical, map_hierarchical_to_gates};
use skalp_mir::MirCompiler;
use skalp_sim::{CircuitMode, HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};

const MATCH_32BIT: &str = r#"
async entity Match32Bit {
    in opcode: bit[4]
    in a: bit[32]
    in b: bit[32]
    out result: bit[32]
}
impl Match32Bit {
    result = match opcode {
        0 => a + b,
        1 => a - b,
        2 => a & b,
        3 => a | b,
        4 => a ^ b,
        5 => a << b[4:0],
        6 => a >> b[4:0],
        _ => 0
    }
}
"#;

#[test]
fn test_32bit_match() {
    println!("\n=== 32-bit Match Structure ===\n");

    let hir = parse_and_build_hir(MATCH_32BIT).expect("Failed to parse");
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
        hw_accel: HwAccel::Gpu,
        max_iterations: 1000,
        capture_waveforms: false,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist.clone())
        .expect("Failed to load NCL netlist");

    // Test ADD (opcode=0)
    println!("\n--- Test: ADD (opcode=0), 100 + 50 ---");
    sim.set_ncl_input("top.opcode", 0, 4);
    sim.set_ncl_input("top.a", 100, 32);
    sim.set_ncl_input("top.b", 50, 32);

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 32) {
        Some(value) => {
            println!("Result: {} (expected 150)", value);
        }
        None => {
            println!("Result: NULL");
        }
    }

    if !result.is_stable {
        println!("❌ OSCILLATES!");
    } else {
        println!("✅ Converges");
    }

    assert!(result.is_stable, "32-bit match should converge");
}
