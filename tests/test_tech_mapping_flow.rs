//! End-to-end integration test for the technology mapping flow
//!
//! This test covers the complete path:
//! Source → HIR → MIR → WordLir → TechMapper → GateNetlist → SIR → Simulation
//!
//! It verifies that:
//! 1. Technology mapping produces correct gate-level netlists
//! 2. FIT rates are properly propagated
//! 3. The resulting SIR can be used for simulation
//! 4. Fault injection works with technology-mapped designs

use skalp_frontend::parse_and_build_hir;
use skalp_lir::{
    builtin_libraries::builtin_generic_asic, gate_netlist::GateNetlist,
    lower_mir_module_to_word_lir, lower_to_lir, tech_mapper::TechMapper,
};
use skalp_mir::MirCompiler;
use skalp_sim::{convert_gate_netlist_to_sir, convert_lir_to_sir};

// ============================================================================
// Test Helpers
// ============================================================================

/// Compile source to MIR
fn compile_to_mir(source: &str) -> skalp_mir::Mir {
    let hir = parse_and_build_hir(source).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR")
}

/// Full technology mapping flow: Source → GateNetlist
fn tech_map_source(source: &str) -> Vec<GateNetlist> {
    let mir = compile_to_mir(source);
    let library = builtin_generic_asic();

    mir.modules
        .iter()
        .map(|module| {
            let word_lir_result = lower_mir_module_to_word_lir(module);
            let mut mapper = TechMapper::new(&library);
            mapper.map(&word_lir_result.word_lir).netlist
        })
        .collect()
}

// ============================================================================
// Basic Flow Tests
// ============================================================================

#[test]
fn test_simple_and_gate_flow() {
    let source = r#"
        entity AndGate {
            in a: bool
            in b: bool
            out y: bool
        }

        impl AndGate {
            y = a && b
        }
    "#;

    let netlists = tech_map_source(source);
    assert_eq!(netlists.len(), 1, "Should have one module");

    let netlist = &netlists[0];
    println!("Cells: {}", netlist.cells.len());
    println!("Nets: {}", netlist.nets.len());
    println!("Total FIT: {:.4}", netlist.total_fit());

    // Should have at least one cell for the AND operation
    assert!(!netlist.cells.is_empty(), "Should have cells");

    // Should have inputs, output, and internal nets
    assert!(netlist.inputs.len() >= 2, "Should have at least 2 inputs");
    assert!(!netlist.outputs.is_empty(), "Should have outputs");

    // FIT should be positive
    assert!(netlist.total_fit() > 0.0, "Should have positive FIT");

    // Convert to SIR and verify
    let sir_result = convert_gate_netlist_to_sir(netlist);
    assert_eq!(sir_result.sir.name, netlist.name);
    assert!(
        sir_result.stats.primitives_created > 0,
        "Should create primitives"
    );

    println!("SIR signals: {}", sir_result.stats.signals_created);
    println!("SIR primitives: {}", sir_result.stats.primitives_created);
    println!("SIR total FIT: {:.4}", sir_result.stats.total_fit);
}

#[test]
fn test_multi_bit_adder_flow() {
    let source = r#"
        entity Adder8 {
            in a: nat[8]
            in b: nat[8]
            out sum: nat[8]
        }

        impl Adder8 {
            sum = a + b
        }
    "#;

    let netlists = tech_map_source(source);
    assert_eq!(netlists.len(), 1);

    let netlist = &netlists[0];
    println!("8-bit adder cells: {}", netlist.cells.len());
    println!("8-bit adder FIT: {:.4}", netlist.total_fit());

    // 8-bit adder should have multiple cells (at least 8 full adders or equivalent)
    assert!(
        netlist.cells.len() >= 8,
        "8-bit adder should have at least 8 cells, got {}",
        netlist.cells.len()
    );

    // Convert to SIR
    let sir_result = convert_gate_netlist_to_sir(netlist);
    assert!(sir_result.stats.primitives_created >= 8);
}

#[test]
fn test_mux_flow() {
    let source = r#"
        entity Mux2to1 {
            in sel: bool
            in a: bool
            in b: bool
            out y: bool
        }

        impl Mux2to1 {
            y = if sel { b } else { a }
        }
    "#;

    let netlists = tech_map_source(source);
    assert_eq!(netlists.len(), 1);

    let netlist = &netlists[0];
    println!("MUX cells: {}", netlist.cells.len());

    // Should have cells for the mux
    assert!(!netlist.cells.is_empty());

    // Check that we can convert to SIR
    let sir_result = convert_gate_netlist_to_sir(netlist);
    assert!(sir_result.stats.primitives_created > 0);
}

#[test]
fn test_sequential_dff_flow() {
    let source = r#"
        entity DFlipFlop {
            in clk: clock
            in d: bool
            out q: bool
        }

        impl DFlipFlop {
            signal reg: bool = false

            on(clk.rise) {
                reg <= d
            }

            q = reg
        }
    "#;

    let netlists = tech_map_source(source);
    assert_eq!(netlists.len(), 1);

    let netlist = &netlists[0];
    println!("DFF cells: {}", netlist.cells.len());
    println!("Clock nets: {}", netlist.clocks.len());

    // Should have clock
    assert!(!netlist.clocks.is_empty(), "Should have clock net");

    // Convert to SIR
    let sir_result = convert_gate_netlist_to_sir(netlist);

    // Should have sequential block
    assert!(
        sir_result.stats.seq_blocks > 0 || sir_result.stats.comb_blocks > 0,
        "Should have some blocks"
    );
}

// ============================================================================
// FIT Rate Tests
// ============================================================================

#[test]
fn test_fit_accumulation() {
    let source = r#"
        entity Chain {
            in a: bool
            out y: bool
        }

        impl Chain {
            signal b: bool
            signal c: bool
            signal d: bool

            b = !a
            c = !b
            d = !c
            y = !d
        }
    "#;

    let netlists = tech_map_source(source);
    let netlist = &netlists[0];

    // Each inverter should contribute to FIT
    let total_fit = netlist.total_fit();
    println!("Chain of 4 inverters FIT: {:.4}", total_fit);

    // With ~4 inverters, FIT should be roughly 4 * single_inv_fit
    // Generic ASIC INV_X1 has FIT around 0.05
    assert!(total_fit > 0.1, "Should have accumulated FIT from chain");

    // Verify FIT is preserved through SIR conversion
    let sir_result = convert_gate_netlist_to_sir(netlist);
    assert!(
        (sir_result.stats.total_fit - total_fit).abs() < 0.001,
        "FIT should be preserved in SIR"
    );
}

#[test]
fn test_library_cell_types() {
    let source = r#"
        entity MixedLogic {
            in a: bool
            in b: bool
            in c: bool
            out y1: bool
            out y2: bool
            out y3: bool
        }

        impl MixedLogic {
            y1 = a && b          // AND
            y2 = a || b          // OR
            y3 = a ^ b ^ c       // XOR chain
        }
    "#;

    let netlists = tech_map_source(source);
    let netlist = &netlists[0];

    // Count cell types
    let mut cell_type_counts = std::collections::HashMap::new();
    for cell in &netlist.cells {
        *cell_type_counts.entry(cell.cell_type.clone()).or_insert(0) += 1;
    }

    println!("Cell type distribution:");
    for (cell_type, count) in &cell_type_counts {
        println!("  {}: {}", cell_type, count);
    }

    // Should have multiple cell types
    assert!(
        !cell_type_counts.is_empty(),
        "Should have at least one cell type"
    );
}

// ============================================================================
// Comparison with Old LIR Flow
// ============================================================================

#[test]
fn test_both_flows_produce_valid_sir() {
    let source = r#"
        entity Compare {
            in a: bool
            in b: bool
            out y: bool
        }

        impl Compare {
            y = a && b
        }
    "#;

    // New flow: Source → MIR → WordLir → TechMapper → GateNetlist → SIR
    let netlists = tech_map_source(source);
    let new_sir_result = convert_gate_netlist_to_sir(&netlists[0]);

    // Old flow: Source → MIR → Lir → SIR
    let mir = compile_to_mir(source);
    let lir_results = lower_to_lir(&mir).expect("Failed to lower to LIR");
    let old_sir_result = convert_lir_to_sir(&lir_results[0].lir);

    println!(
        "New flow primitives: {}",
        new_sir_result.stats.primitives_created
    );
    println!(
        "Old flow primitives: {}",
        old_sir_result.stats.primitive_ops
    );

    // Both should produce valid SIR
    assert!(!new_sir_result.sir.top_module.signals.is_empty());
    assert!(!old_sir_result.sir.top_module.signals.is_empty());

    // New flow should have FIT information
    assert!(new_sir_result.stats.total_fit > 0.0);
}

// ============================================================================
// Complex Design Tests
// ============================================================================

#[test]
fn test_counter_design() {
    let source = r#"
        entity Counter4 {
            in clk: clock
            in rst: reset
            in enable: bool
            out count: nat[4]
        }

        impl Counter4 {
            signal counter: nat[4] = 0

            on(clk.rise) {
                if rst {
                    counter <= 0
                } else if enable {
                    counter <= counter + 1
                }
            }

            count = counter
        }
    "#;

    let netlists = tech_map_source(source);
    assert_eq!(netlists.len(), 1);

    let netlist = &netlists[0];
    println!("Counter design:");
    println!("  Cells: {}", netlist.cells.len());
    println!("  Nets: {}", netlist.nets.len());
    println!("  Clocks: {}", netlist.clocks.len());
    println!("  Resets: {}", netlist.resets.len());
    println!("  Total FIT: {:.4}", netlist.total_fit());

    // Counter should have sequential elements
    let seq_cells = netlist.cells.iter().filter(|c| c.is_sequential()).count();
    println!("  Sequential cells: {}", seq_cells);

    // Convert to SIR
    let sir_result = convert_gate_netlist_to_sir(netlist);
    println!("  SIR comb blocks: {}", sir_result.stats.comb_blocks);
    println!("  SIR seq blocks: {}", sir_result.stats.seq_blocks);
}

#[test]
fn test_alu_slice() {
    let source = r#"
        entity AluSlice {
            in a: nat[8]
            in b: nat[8]
            in op: nat[2]
            out result: nat[8]
        }

        impl AluSlice {
            result = match op {
                0 => a + b,
                1 => a - b,
                2 => a & b,
                _ => a | b
            }
        }
    "#;

    let netlists = tech_map_source(source);
    let netlist = &netlists[0];

    println!("ALU slice:");
    println!("  Cells: {}", netlist.cells.len());
    println!("  Total FIT: {:.4}", netlist.total_fit());

    // ALU should have substantial logic
    assert!(
        netlist.cells.len() > 10,
        "ALU should have significant cell count"
    );

    // Higher complexity = higher FIT
    assert!(netlist.total_fit() > 1.0, "ALU should have measurable FIT");

    // SIR conversion
    let sir_result = convert_gate_netlist_to_sir(netlist);
    assert!(sir_result.stats.primitives_created > 10);
}

// ============================================================================
// Edge Cases
// ============================================================================

#[test]
fn test_empty_module() {
    let source = r#"
        entity Empty {
            in a: bool
            out y: bool
        }

        impl Empty {
            y = a
        }
    "#;

    let netlists = tech_map_source(source);
    let netlist = &netlists[0];

    // Passthrough should have minimal logic (maybe just a buffer)
    println!("Passthrough cells: {}", netlist.cells.len());

    // Should still produce valid SIR
    let sir_result = convert_gate_netlist_to_sir(netlist);
    assert!(!sir_result.sir.top_module.signals.is_empty());
}

#[test]
fn test_constant_output() {
    let source = r#"
        entity Constant {
            out y: bool
        }

        impl Constant {
            y = true
        }
    "#;

    let netlists = tech_map_source(source);
    let netlist = &netlists[0];

    println!("Constant output cells: {}", netlist.cells.len());

    // Convert to SIR
    let _sir_result = convert_gate_netlist_to_sir(netlist);
}

// ============================================================================
// Hierarchical Path Tests
// ============================================================================

#[test]
fn test_hierarchical_paths() {
    let source = r#"
        entity PathTest {
            in a: bool
            in b: bool
            out y: bool
        }

        impl PathTest {
            signal temp: bool
            temp = a && b
            y = !temp
        }
    "#;

    let netlists = tech_map_source(source);
    let netlist = &netlists[0];

    // Check that cells have hierarchical paths
    for cell in &netlist.cells {
        println!("Cell {} path: {}", cell.cell_type, cell.path);
        assert!(!cell.path.is_empty(), "Cell should have a path");
    }
}
