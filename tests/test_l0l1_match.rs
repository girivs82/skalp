// MWE for L0/L1 match structure with SUB operations

use skalp_frontend::parse_and_build_hir;
use skalp_lir::{get_stdlib_library, lower_mir_hierarchical, map_hierarchical_to_gates};
use skalp_mir::MirCompiler;
use skalp_sim::{CircuitMode, HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
use std::collections::HashMap;

// Minimal L0/L1-like structure with ADD and SUB
const L0L1_MATCH: &str = r#"
/// Minimal L0/L1-like ALU with match statement
async entity L0L1Match {
    in opcode: bit[4]
    in a: bit[32]
    in b: bit[32]
    out result: bit[32]
}
impl L0L1Match {
    result = match opcode {
        0 => a + b,           // ADD
        1 => a - b,           // SUB
        2 => a & b,           // AND
        3 => a | b,           // OR
        4 => a ^ b,           // XOR
        5 => {                // SRA-like with 32 - shift_amt
            let shift_amt = b[4:0];
            let sign = a[31];
            let shifted = a >> shift_amt;
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
fn test_l0l1_match_structure() {
    println!("\n=== L0/L1 Match Structure MWE ===\n");

    let hir = parse_and_build_hir(L0L1_MATCH).expect("Failed to parse");
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

    // Count cell types
    let mut cell_types: HashMap<String, usize> = HashMap::new();
    for cell in &netlist.cells {
        *cell_types.entry(cell.cell_type.clone()).or_insert(0) += 1;
    }
    println!("Cell distribution:");
    let mut sorted: Vec<_> = cell_types.iter().collect();
    sorted.sort_by_key(|(_, count)| std::cmp::Reverse(*count));
    for (cell_type, count) in sorted.iter().take(15) {
        println!("  {}: {}", cell_type, count);
    }

    // Check for SUB cells
    let sub_cells: Vec<_> = netlist
        .cells
        .iter()
        .filter(|c| c.path.contains("sub_"))
        .take(10)
        .collect();
    println!("\nSample SUB cells:");
    for cell in &sub_cells {
        println!("  {} ({})", cell.path, cell.cell_type);
    }

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

    // Test 1: ADD (opcode=0), 10 + 5 = 15
    println!("\n--- Test 1: ADD (opcode=0), 10 + 5 = 15 ---");
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

    // Test 2: SUB (opcode=1), 10 - 5 = 5
    println!("\n--- Test 2: SUB (opcode=1), 10 - 5 = 5 ---");
    sim.set_ncl_input("top.opcode", 1, 4);

    let result2 = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result2.iterations, result2.is_stable
    );

    match sim.get_ncl_output("top.result", 32) {
        Some(value) => {
            println!("Result: {} (expected 5)", value);
        }
        None => {
            println!("Result: NULL");
        }
    }

    // Test 3: SRA (opcode=5), -1 >> 4 with sign extension
    println!("\n--- Test 3: SRA (opcode=5), a=-1 >> 4 ---");
    sim.set_ncl_input("top.opcode", 5, 4);
    sim.set_ncl_input("top.a", 0xFFFFFFFF, 32); // -1
    sim.set_ncl_input("top.b", 4, 32); // shift by 4

    let result3 = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result3.iterations, result3.is_stable
    );

    match sim.get_ncl_output("top.result", 32) {
        Some(value) => {
            println!("Result: 0x{:08X} (expected 0xFFFFFFFF)", value);
        }
        None => {
            println!("Result: NULL");
        }
    }

    if !result.is_stable || !result2.is_stable || !result3.is_stable {
        println!("\n⚠️ Some operations oscillate - this is the bug!");
    } else {
        println!("\n✅ All operations converge");
    }

    assert!(result.is_stable, "ADD should converge");
    assert!(result2.is_stable, "SUB should converge");
    assert!(result3.is_stable, "SRA should converge");
}
