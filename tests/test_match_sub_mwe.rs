// Minimal MWE for match SUB oscillation
// This tests if SUB inside a match statement causes oscillation

use indexmap::IndexMap;
use skalp_frontend::parse_and_build_hir;
use skalp_lir::{get_stdlib_library, lower_mir_hierarchical, map_hierarchical_to_gates};
use skalp_mir::MirCompiler;
use skalp_sim::{CircuitMode, HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};

// Simplest case: just ADD and SUB in match
const MATCH_ADD_SUB: &str = r#"
async entity MatchAddSub {
    in sel: bit[1]
    in a: bit[8]
    in b: bit[8]
    out result: bit[8]
}
impl MatchAddSub {
    result = match sel {
        0 => a + b,
        1 => a - b,
        _ => 0
    }
}
"#;

// Compare with if-else (should behave the same but let's check)
const IFELSE_ADD_SUB: &str = r#"
async entity IfelseAddSub {
    in sel: bit[1]
    in a: bit[8]
    in b: bit[8]
    out result: bit[8]
}
impl IfelseAddSub {
    result = if sel == 0 {
        a + b
    } else {
        a - b
    }
}
"#;

// SUB only in match (to see if SUB alone is the issue)
const MATCH_SUB_ONLY: &str = r#"
async entity MatchSubOnly {
    in sel: bit[1]
    in a: bit[8]
    in b: bit[8]
    out result: bit[8]
}
impl MatchSubOnly {
    result = match sel {
        0 => a - b,
        1 => 0,
        _ => 0
    }
}
"#;

// SUB alone (no match)
const SUB_ALONE: &str = r#"
async entity SubAlone {
    in a: bit[8]
    in b: bit[8]
    out result: bit[8]
}
impl SubAlone {
    result = a - b
}
"#;

async fn run_test(name: &str, source: &str, test_inputs: Vec<(&str, u64, usize)>) -> bool {
    println!("\n=== {} ===", name);

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

    // Count cell types
    let mut cell_types: IndexMap<String, usize> = IndexMap::new();
    for cell in &netlist.cells {
        *cell_types.entry(cell.cell_type.clone()).or_insert(0) += 1;
    }
    let th22 = cell_types.get("TH22_X1").copied().unwrap_or(0);
    let th12 = cell_types.get("TH12_X1").copied().unwrap_or(0);
    println!("  TH22: {}, TH12: {}", th22, th12);

    // Check for SUB cells
    let sub_cells = netlist
        .cells
        .iter()
        .filter(|c| c.path.contains("sub_"))
        .count();
    println!("  SUB cells: {}", sub_cells);

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

    // Set inputs
    for (name, value, width) in &test_inputs {
        sim.set_ncl_input(&format!("top.{}", name), *value, *width);
    }

    let result = sim.run_until_stable().await;
    println!(
        "Iterations: {}, Stable: {}",
        result.iterations, result.is_stable
    );

    if !result.is_stable {
        println!("❌ OSCILLATES!");
        return false;
    }

    match sim.get_ncl_output("top.result", 8) {
        Some(value) => {
            println!("Result: {}", value);
        }
        None => {
            println!("Result: NULL");
        }
    }

    println!("✅ Converges");
    true
}

#[tokio::test]
async fn test_sub_alone() {
    // SUB without match should work
    let pass = run_test(
        "SUB alone (no match)",
        SUB_ALONE,
        vec![("a", 10, 8), ("b", 3, 8)],
    )
    .await;
    assert!(pass, "SUB alone should converge");
}

#[tokio::test]
async fn test_match_sub_only() {
    // SUB in match - does this oscillate?
    let pass = run_test(
        "MATCH with SUB only",
        MATCH_SUB_ONLY,
        vec![("sel", 0, 1), ("a", 10, 8), ("b", 3, 8)],
    )
    .await;
    // Note: if this oscillates, the bug is in the combination of match + SUB
    if !pass {
        println!("BUG: SUB in match causes oscillation!");
    }
}

#[tokio::test]
async fn test_match_add_sub() {
    // ADD and SUB in match - the key test
    let pass = run_test(
        "MATCH ADD+SUB (sel=0, ADD)",
        MATCH_ADD_SUB,
        vec![("sel", 0, 1), ("a", 10, 8), ("b", 3, 8)],
    )
    .await;
    if !pass {
        println!("BUG: ADD+SUB match causes oscillation even when selecting ADD!");
    }
}

#[tokio::test]
async fn test_ifelse_add_sub() {
    // Same as match but with if-else - for comparison
    let pass = run_test(
        "IF-ELSE ADD+SUB (sel=0, ADD)",
        IFELSE_ADD_SUB,
        vec![("sel", 0, 1), ("a", 10, 8), ("b", 3, 8)],
    )
    .await;
    assert!(pass, "IF-ELSE should converge");
}
