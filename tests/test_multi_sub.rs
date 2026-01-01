// Test multiple SUB operations causing net name collision
// Hypothesis: Two SUB ops with empty paths create identical cell names

use skalp_frontend::parse_and_build_hir;
use skalp_lir::{get_stdlib_library, lower_mir_hierarchical, map_hierarchical_to_gates};
use skalp_mir::MirCompiler;
use skalp_sim::{CircuitMode, HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
use std::collections::HashMap;

// Two SUB operations with same width - should they collide?
const TWO_SUB_SAME_WIDTH: &str = r#"
async entity TwoSubSameWidth {
    in a: bit[8]
    in b: bit[8]
    in c: bit[8]
    out result1: bit[8]
    out result2: bit[8]
}
impl TwoSubSameWidth {
    result1 = a - b
    result2 = a - c
}
"#;

// Two SUB in match - the issue pattern
const TWO_SUB_IN_MATCH: &str = r#"
async entity TwoSubInMatch {
    in sel: bit[1]
    in a: bit[32]
    in b: bit[32]
    in c: bit[8]
    out result: bit[32]
}
impl TwoSubInMatch {
    result = match sel {
        0 => a - b,
        1 => {
            let small = (32 - c);  // 8-bit SUB
            (a >> small[4:0]) as bit[32]
        },
        _ => 0
    }
}
"#;

#[test]
fn test_two_sub_same_width() {
    println!("\n=== Two SUB Same Width ===\n");

    let hir = parse_and_build_hir(TWO_SUB_SAME_WIDTH).expect("Failed to parse");
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

    // Check for duplicate cell names
    let mut cell_names: HashMap<String, usize> = HashMap::new();
    for cell in &netlist.cells {
        *cell_names.entry(cell.path.clone()).or_insert(0) += 1;
    }

    let duplicates: Vec<_> = cell_names.iter().filter(|(_, &count)| count > 1).collect();
    if !duplicates.is_empty() {
        println!("\n🚨 DUPLICATE CELL NAMES FOUND:");
        for (name, count) in duplicates.iter().take(10) {
            println!("  {} - {} times", name, count);
        }
    }

    // Check SUB cell paths
    let sub_cells: Vec<_> = netlist
        .cells
        .iter()
        .filter(|c| c.path.contains("sub_"))
        .collect();
    println!("\nTotal SUB cells: {}", sub_cells.len());

    // Count unique SUB cell path prefixes
    let mut sub_prefixes: HashMap<String, usize> = HashMap::new();
    for cell in &sub_cells {
        // Extract prefix before "sub_"
        if let Some(pos) = cell.path.find("sub_") {
            let prefix = &cell.path[..pos];
            *sub_prefixes.entry(prefix.to_string()).or_insert(0) += 1;
        }
    }
    println!("SUB cell path prefixes:");
    for (prefix, count) in &sub_prefixes {
        println!("  '{}' - {} cells", prefix, count);
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

    sim.set_ncl_input("top.a", 10, 8);
    sim.set_ncl_input("top.b", 3, 8);
    sim.set_ncl_input("top.c", 2, 8);

    let result = sim.run_until_stable();
    println!(
        "\nIterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result1", 8) {
        Some(value) => println!("result1: {} (expected 7)", value),
        None => println!("result1: NULL"),
    }
    match sim.get_ncl_output("top.result2", 8) {
        Some(value) => println!("result2: {} (expected 8)", value),
        None => println!("result2: NULL"),
    }

    if !result.is_stable {
        println!("\n❌ OSCILLATES - cell name collision likely!");
    } else {
        println!("\n✅ Converges");
    }
}

#[test]
fn test_two_sub_in_match() {
    println!("\n=== Two SUB in Match ===\n");

    let hir = parse_and_build_hir(TWO_SUB_IN_MATCH).expect("Failed to parse");
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

    // Check for duplicate cell names
    let mut cell_names: HashMap<String, usize> = HashMap::new();
    for cell in &netlist.cells {
        *cell_names.entry(cell.path.clone()).or_insert(0) += 1;
    }

    let duplicates: Vec<_> = cell_names.iter().filter(|(_, &count)| count > 1).collect();
    if !duplicates.is_empty() {
        println!("\n🚨 DUPLICATE CELL NAMES FOUND:");
        for (name, count) in duplicates.iter().take(10) {
            println!("  {} - {} times", name, count);
        }
    }

    // Check SUB cell paths
    let sub_cells: Vec<_> = netlist
        .cells
        .iter()
        .filter(|c| c.path.contains("sub_"))
        .collect();
    println!("\nTotal SUB cells: {}", sub_cells.len());

    // List first few SUB cells to see their paths
    println!("Sample SUB cells:");
    for cell in sub_cells.iter().take(10) {
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

    // Test: sel=0 (first SUB), a - b = 10 - 3 = 7
    println!("\n--- Test: sel=0, a=10, b=3 (expect a - b = 7) ---");
    sim.set_ncl_input("top.sel", 0, 1);
    sim.set_ncl_input("top.a", 10, 32);
    sim.set_ncl_input("top.b", 3, 32);
    sim.set_ncl_input("top.c", 2, 8);

    let result = sim.run_until_stable();
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    match sim.get_ncl_output("top.result", 32) {
        Some(value) => println!("Result: {} (expected 7)", value),
        None => println!("Result: NULL"),
    }

    if !result.is_stable {
        println!("\n❌ OSCILLATES - this confirms the bug!");
    } else {
        println!("\n✅ Converges");
    }
}
