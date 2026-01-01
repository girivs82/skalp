// Test SRA pattern that causes oscillation

use skalp_frontend::parse_and_build_hir;
use skalp_lir::{get_stdlib_library, lower_mir_hierarchical, map_hierarchical_to_gates};
use skalp_mir::MirCompiler;
use skalp_sim::{CircuitMode, HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};

// Exact SRA pattern from L0/L1 - with 32 - shift_amt mask calculation
const SRA_FULL: &str = r#"
async entity SraFull {
    in a: bit[32]
    in b: bit[32]
    out result: bit[32]
}
impl SraFull {
    let sign = a[31];
    let shifted = a >> b[4:0];
    let shift_amt = b[4:0];
    result = if sign && shift_amt > 0 {
        let mask = (0xFFFFFFFF << (32 - shift_amt));
        shifted | mask
    } else {
        shifted
    }
}
"#;

// Same SRA but with match wrapper (like in L0/L1)
const SRA_IN_MATCH: &str = r#"
async entity SraInMatch {
    in opcode: bit[4]
    in a: bit[32]
    in b: bit[32]
    out result: bit[32]
}
impl SraInMatch {
    result = match opcode {
        0 => a + b,
        1 => {
            let sign = a[31];
            let shifted = a >> b[4:0];
            let shift_amt = b[4:0];
            if sign && shift_amt > 0 {
                let mask = (0xFFFFFFFF << (32 - shift_amt));
                shifted | mask
            } else {
                shifted
            }
        },
        _ => 0
    }
}
"#;

#[test]
fn test_sra_full_alone() {
    println!("\n=== SRA Full (standalone) ===\n");

    let hir = parse_and_build_hir(SRA_FULL).expect("Failed to parse");
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
        max_iterations: 500,
        capture_waveforms: false,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist.clone())
        .expect("Failed to load NCL netlist");

    // Test: -1 >> 4 should stay -1 (0xFFFFFFFF)
    println!("--- Test: -1 >> 4 (should stay -1) ---");
    sim.set_ncl_input("top.a", 0xFFFFFFFF, 32);
    sim.set_ncl_input("top.b", 4, 32);

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 32) {
        Some(value) => {
            println!("Result: 0x{:08X} (expected 0xFFFFFFFF)", value);
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

    assert!(result.is_stable, "SRA alone should converge");
}

#[test]
fn test_sra_in_match() {
    println!("\n=== SRA in Match ===\n");

    let hir = parse_and_build_hir(SRA_IN_MATCH).expect("Failed to parse");
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

    // Check for oscillating cell patterns
    let sub_cells: Vec<_> = netlist
        .cells
        .iter()
        .filter(|c| c.path.contains("sub_"))
        .take(5)
        .collect();
    println!("Sample SUB cells:");
    for cell in &sub_cells {
        println!("  {} ({})", cell.path, cell.cell_type);
    }

    let config = UnifiedSimConfig {
        level: SimLevel::GateLevel,
        circuit_mode: CircuitMode::Ncl,
        hw_accel: HwAccel::Gpu,
        max_iterations: 500,
        capture_waveforms: false,
        ncl_debug: true,
        ..Default::default()
    };

    let mut sim = UnifiedSimulator::new(config).expect("Failed to create simulator");
    sim.load_ncl_gate_level(netlist.clone())
        .expect("Failed to load NCL netlist");

    // Test: opcode=0 (ADD), 10 + 5 = 15
    println!("--- Test: ADD (opcode=0), 10 + 5 = 15 ---");
    sim.set_ncl_input("top.opcode", 0, 4);
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
        }
        None => {
            println!("Result: NULL");
        }
    }

    if !result.is_stable {
        println!("❌ OSCILLATES - THIS IS THE BUG!");
    } else {
        println!("✅ Converges");
    }

    // Don't assert - we want to see the result even if it oscillates
    // assert!(result.is_stable, "SRA in match should converge");
}
