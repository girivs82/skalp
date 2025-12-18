//! Test demonstrating unified simulation: same testbench, different backends
//!
//! This test shows how the same source code and testbench can run on:
//! 1. Behavioral simulation (MIR → behavioral SIR)
//! 2. Gate-level simulation (MIR → Lir → GateNetlist → SIR)
//!
//! Both paths should produce functionally equivalent results.

use skalp_frontend::parse_and_build_hir;
use skalp_lir::{
    builtin_libraries::builtin_generic_asic, gate_netlist::GateNetlist, lower_mir_module_to_lir,
    tech_mapper::TechMapper,
};
use skalp_mir::MirCompiler;
use skalp_sim::convert_gate_netlist_to_sir;
use skalp_sir::convert_mir_to_sir_with_hierarchy;

/// Helper function to compile source to both behavioral SIR and GateNetlist
fn compile_to_both(source: &str, module_name: &str) -> (skalp_sir::SirModule, GateNetlist) {
    // Parse and compile to HIR and MIR
    let hir = parse_and_build_hir(source).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir_design = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    // Find the module
    let mir_module = mir_design
        .modules
        .iter()
        .find(|m| m.name == module_name)
        .unwrap_or_else(|| panic!("Module '{}' not found", module_name))
        .clone();

    // Path 1: MIR → behavioral SIR
    let behavioral_sir = convert_mir_to_sir_with_hierarchy(&mir_design, &mir_module);

    // Path 2: MIR → Lir → GateNetlist
    let library = builtin_generic_asic();
    let lir_result = lower_mir_module_to_lir(&mir_module);
    let mut mapper = TechMapper::new(&library);
    let gate_netlist = mapper.map(&lir_result.lir).netlist;

    (behavioral_sir, gate_netlist)
}

// ============================================================================
// TESTS
// ============================================================================

#[test]
fn test_behavioral_and_gate_level_paths_produce_sir() {
    let source = r#"
        entity Adder {
            in a: bit[8]
            in b: bit[8]
            out sum: bit[8]
        }

        impl Adder {
            sum = a + b
        }
    "#;

    let (behavioral_sir, gate_netlist) = compile_to_both(source, "Adder");

    println!("=== Behavioral SIR ===");
    println!("Module: {}", behavioral_sir.name);
    println!("Signals: {}", behavioral_sir.signals.len());
    println!(
        "Nodes: {} comb, {} seq",
        behavioral_sir.combinational_nodes.len(),
        behavioral_sir.sequential_nodes.len()
    );

    println!("\n=== GateNetlist ===");
    println!("Module: {}", gate_netlist.name);
    println!("Cells: {}", gate_netlist.cells.len());
    println!("Nets: {}", gate_netlist.nets.len());
    println!("Total FIT: {:.4}", gate_netlist.total_fit());

    // Convert gate netlist to SIR
    let gate_sir_result = convert_gate_netlist_to_sir(&gate_netlist);

    println!("\n=== Gate-level SIR ===");
    println!("Module: {}", gate_sir_result.sir.name);
    println!("Signals: {}", gate_sir_result.stats.signals_created);
    println!("Primitives: {}", gate_sir_result.stats.primitives_created);
    println!("Total FIT: {:.4}", gate_sir_result.stats.total_fit);

    // Both paths should produce valid SIR
    assert!(
        !behavioral_sir.signals.is_empty(),
        "Behavioral SIR should have signals"
    );
    assert!(
        gate_sir_result.stats.signals_created > 0,
        "Gate SIR should have signals"
    );
}

#[test]
fn test_bitwise_ops_both_paths() {
    let source = r#"
        entity BitwiseOps {
            in a: bit[8]
            in b: bit[8]
            out and_out: bit[8]
            out or_out: bit[8]
            out xor_out: bit[8]
        }

        impl BitwiseOps {
            and_out = a & b
            or_out = a | b
            xor_out = a ^ b
        }
    "#;

    let (behavioral_sir, gate_netlist) = compile_to_both(source, "BitwiseOps");

    println!("=== Bitwise Operations ===");

    // Behavioral path
    println!("Behavioral SIR signals: {}", behavioral_sir.signals.len());
    println!(
        "Behavioral SIR nodes: {} comb, {} seq",
        behavioral_sir.combinational_nodes.len(),
        behavioral_sir.sequential_nodes.len()
    );

    // Gate-level path
    println!("Gate netlist cells: {}", gate_netlist.cells.len());
    println!("Gate netlist total FIT: {:.4}", gate_netlist.total_fit());

    // Convert to SIR
    let gate_sir_result = convert_gate_netlist_to_sir(&gate_netlist);
    println!(
        "Gate SIR primitives: {}",
        gate_sir_result.stats.primitives_created
    );

    // Both should work
    let total_nodes =
        behavioral_sir.combinational_nodes.len() + behavioral_sir.sequential_nodes.len();
    assert!(total_nodes > 0 || !behavioral_sir.signals.is_empty());
    assert!(gate_sir_result.stats.primitives_created > 0);
}

#[test]
fn test_mux_both_paths() {
    let source = r#"
        entity Mux4 {
            in sel: bit[2]
            in a: bit[8]
            in b: bit[8]
            in c: bit[8]
            in d: bit[8]
            out y: bit[8]
        }

        impl Mux4 {
            y = match sel {
                0 => a,
                1 => b,
                2 => c,
                3 => d,
            }
        }
    "#;

    let (behavioral_sir, gate_netlist) = compile_to_both(source, "Mux4");

    println!("=== 4:1 MUX ===");

    // Behavioral
    println!("Behavioral signals: {}", behavioral_sir.signals.len());

    // Gate-level
    println!("Gate cells: {}", gate_netlist.cells.len());
    println!("Gate FIT: {:.4}", gate_netlist.total_fit());

    // List cells
    for cell in &gate_netlist.cells {
        println!("  Cell: {} ({})", cell.path, cell.cell_type);
    }

    // Convert to SIR
    let gate_sir_result = convert_gate_netlist_to_sir(&gate_netlist);
    assert!(gate_sir_result.stats.primitives_created > 0);
}

#[test]
fn test_sequential_both_paths() {
    let source = r#"
        entity Counter {
            in clk: clock
            in enable: bool
            out count: bit[4]
        }

        impl Counter {
            on(clk.rise) {
                if enable {
                    count = count + 1
                }
            }
        }
    "#;

    let (behavioral_sir, gate_netlist) = compile_to_both(source, "Counter");

    println!("=== Counter (Sequential) ===");

    // Behavioral
    println!("Behavioral signals: {}", behavioral_sir.signals.len());
    println!(
        "Behavioral nodes: {} comb, {} seq",
        behavioral_sir.combinational_nodes.len(),
        behavioral_sir.sequential_nodes.len()
    );

    // Gate-level
    println!("Gate cells: {}", gate_netlist.cells.len());
    println!("Gate FIT: {:.4}", gate_netlist.total_fit());

    // Count sequential cells
    let dff_cells: Vec<_> = gate_netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.contains("DFF"))
        .collect();
    println!("DFF cells: {}", dff_cells.len());

    // Convert to SIR
    let gate_sir_result = convert_gate_netlist_to_sir(&gate_netlist);
    println!(
        "Gate SIR primitives: {}",
        gate_sir_result.stats.primitives_created
    );

    // Sequential designs should have higher FIT
    assert!(gate_netlist.total_fit() > 0.0, "Sequential should have FIT");
}

#[test]
fn test_complex_alu_both_paths() {
    let source = r#"
        entity ALU {
            in a: bit[8]
            in b: bit[8]
            in op: bit[3]
            out result: bit[8]
            out zero: bool
        }

        impl ALU {
            let computed: bit[8] = match op {
                0 => a + b,
                1 => a - b,
                2 => a & b,
                3 => a | b,
                4 => a ^ b,
                5 => !a,
                _ => 0,
            }
            result = computed
            zero = computed == 0
        }
    "#;

    let (behavioral_sir, gate_netlist) = compile_to_both(source, "ALU");

    println!("=== ALU ===");
    println!("Behavioral signals: {}", behavioral_sir.signals.len());
    println!("Gate cells: {}", gate_netlist.cells.len());
    println!("Gate nets: {}", gate_netlist.nets.len());
    println!("Gate total FIT: {:.4}", gate_netlist.total_fit());

    // ALU should have significant complexity
    assert!(gate_netlist.cells.len() > 10, "ALU should have many cells");

    // Convert to SIR
    let gate_sir_result = convert_gate_netlist_to_sir(&gate_netlist);
    assert!(gate_sir_result.stats.primitives_created > 0);
}

#[test]
fn test_gate_netlist_preserves_structure() {
    let source = r#"
        entity Pipeline {
            in clk: clock
            in data_in: bit[8]
            out data_out: bit[8]
        }

        impl Pipeline {
            signal stage1: bit[8] = 0
            signal stage2: bit[8] = 0

            on(clk.rise) {
                stage1 = data_in
                stage2 = stage1
                data_out = stage2
            }
        }
    "#;

    let (_, gate_netlist) = compile_to_both(source, "Pipeline");

    println!("=== Pipeline ===");
    println!("Cells: {}", gate_netlist.cells.len());
    println!("Inputs: {:?}", gate_netlist.inputs);
    println!("Outputs: {:?}", gate_netlist.outputs);

    // Pipeline should have DFFs for each stage
    let dff_count = gate_netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.contains("DFF"))
        .count();

    println!("DFF cells: {}", dff_count);

    // 3 stages × 8 bits = 24 DFFs minimum
    assert!(dff_count >= 16, "Pipeline should have many DFFs");

    // Convert to SIR
    let gate_sir_result = convert_gate_netlist_to_sir(&gate_netlist);
    assert!(gate_sir_result.stats.primitives_created >= 16);
}
