//! End-to-end integration test for the technology mapping flow
//!
//! This test covers the complete path:
//! Source → HIR → MIR → Lir → TechMapper → GateNetlist → SIR → Simulation
//!
//! It verifies that:
//! 1. Technology mapping produces correct gate-level netlists
//! 2. FIT rates are properly propagated
//! 3. The resulting SIR can be used for simulation

use skalp_frontend::parse_and_build_hir;
use skalp_lir::{
    builtin_libraries::builtin_generic_asic, gate_netlist::GateNetlist, lower_mir_module_to_lir,
    tech_mapper::TechMapper,
};
use skalp_mir::MirCompiler;
use skalp_sim::convert_gate_netlist_to_sir;

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
            let lir_result = lower_mir_module_to_lir(module);
            let mut mapper = TechMapper::new(&library);
            mapper.map(&lir_result.lir).netlist
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
            in a: bit[8]
            in b: bit[8]
            out sum: bit[8]
        }

        impl Adder8 {
            sum = a + b
        }
    "#;

    let netlists = tech_map_source(source);
    assert_eq!(netlists.len(), 1);

    let netlist = &netlists[0];
    println!("8-bit Adder:");
    println!("  Cells: {}", netlist.cells.len());
    println!("  Total FIT: {:.4}", netlist.total_fit());

    // Multi-bit adder should decompose to multiple cells
    assert!(
        netlist.cells.len() > 1,
        "Should decompose to multiple cells"
    );

    // Convert to SIR
    let sir_result = convert_gate_netlist_to_sir(netlist);
    println!("  SIR primitives: {}", sir_result.stats.primitives_created);
}

#[test]
fn test_mux_flow() {
    let source = r#"
        entity Mux2 {
            in sel: bool
            in a: bit[16]
            in b: bit[16]
            out y: bit[16]
        }

        impl Mux2 {
            y = if sel { b } else { a }
        }
    "#;

    let netlists = tech_map_source(source);
    assert_eq!(netlists.len(), 1);

    let netlist = &netlists[0];
    println!("16-bit Mux:");
    println!("  Cells: {}", netlist.cells.len());
    println!("  Total FIT: {:.4}", netlist.total_fit());

    // Convert to SIR
    let sir_result = convert_gate_netlist_to_sir(netlist);
    assert!(sir_result.stats.primitives_created > 0);
}

#[test]
fn test_register_flow() {
    let source = r#"
        entity Reg8 {
            in clk: clock
            in d: bit[8]
            out q: bit[8]
        }

        impl Reg8 {
            on(clk.rise) {
                q = d
            }
        }
    "#;

    let netlists = tech_map_source(source);
    assert_eq!(netlists.len(), 1);

    let netlist = &netlists[0];
    println!("8-bit Register:");
    println!("  Cells: {}", netlist.cells.len());
    println!("  Total FIT: {:.4}", netlist.total_fit());

    // Registers have higher FIT than combinational logic
    assert!(
        netlist.total_fit() > 0.5,
        "Registers should have significant FIT"
    );

    // Convert to SIR
    let sir_result = convert_gate_netlist_to_sir(netlist);
    assert!(
        sir_result.stats.primitives_created >= 8,
        "Should have at least 8 DFFs"
    );
}

#[test]
fn test_fit_propagation() {
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

    let netlists = tech_map_source(source);
    let netlist = &netlists[0];

    // Check that FIT is accumulated correctly
    let total_fit = netlist.total_fit();
    let cell_fit_sum: f64 = netlist.cells.iter().map(|c| c.fit).sum();

    println!("Counter FIT analysis:");
    println!("  Total FIT: {:.4}", total_fit);
    println!("  Sum of cell FITs: {:.4}", cell_fit_sum);

    // Total should equal sum of parts
    assert!(
        (total_fit - cell_fit_sum).abs() < 0.001,
        "FIT should be sum of cells"
    );
}

#[test]
fn test_complex_design_flow() {
    let source = r#"
        entity ALU {
            in a: bit[8]
            in b: bit[8]
            in op: bit[2]
            out result: bit[8]
        }

        impl ALU {
            result = match op {
                0b00 => a + b,
                0b01 => a - b,
                0b10 => a & b,
                0b11 => a | b,
            }
        }
    "#;

    let netlists = tech_map_source(source);
    assert_eq!(netlists.len(), 1);

    let netlist = &netlists[0];
    println!("ALU:");
    println!("  Cells: {}", netlist.cells.len());
    println!("  Nets: {}", netlist.nets.len());
    println!("  Total FIT: {:.4}", netlist.total_fit());

    // ALU should have significant logic
    assert!(netlist.cells.len() > 10, "ALU should have many cells");

    // Convert to SIR
    let sir_result = convert_gate_netlist_to_sir(netlist);
    println!("  SIR primitives: {}", sir_result.stats.primitives_created);
    assert!(sir_result.stats.primitives_created > 0);
}
