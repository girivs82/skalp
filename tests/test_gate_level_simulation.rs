//! Integration test for gate-level simulation pipeline
//!
//! This test demonstrates the complete flow:
//! 1. Parse Skalp source code
//! 2. Build HIR
//! 3. Lower to MIR
//! 4. Transform to Lir (word-level)
//! 5. Technology map to GateNetlist
//! 6. Convert to SIR
//! 7. Evaluate with gate-level primitives

use skalp_frontend::parse_and_build_hir;
use skalp_lir::{
    builtin_libraries::builtin_generic_asic, gate_netlist::GateNetlist, lower_mir_module_to_lir,
    tech_mapper::TechMapper,
};
use skalp_mir::MirCompiler;
use skalp_sim::convert_gate_netlist_to_sir;

/// Helper to compile Skalp source to GateNetlist
fn compile_to_gate_netlist(source: &str) -> Vec<GateNetlist> {
    let hir = parse_and_build_hir(source).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

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

#[test]
fn test_simple_and_gate_pipeline() {
    let source = r#"
        entity AndGate {
            in a: bit
            in b: bit
            out y: bit
        }

        impl AndGate {
            y = a & b
        }
    "#;

    let netlists = compile_to_gate_netlist(source);
    assert!(!netlists.is_empty(), "Should produce at least one netlist");

    let netlist = &netlists[0];
    println!("=== AND Gate Pipeline ===");
    println!("Module: {}", netlist.name);
    println!("Cells: {}", netlist.cells.len());
    println!("Nets: {}", netlist.nets.len());
    println!("Total FIT: {:.4}", netlist.total_fit());

    // Check for AND cells
    let and_cells: Vec<_> = netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.contains("AND") || c.cell_type.contains("and"))
        .collect();
    println!("AND cells: {}", and_cells.len());

    // Print all cells
    for cell in &netlist.cells {
        println!(
            "  {} (type: {}, FIT: {:.4})",
            cell.path, cell.cell_type, cell.fit
        );
    }

    assert!(!netlist.cells.is_empty(), "Should have at least one cell");

    // Convert to SIR and verify
    let sir_result = convert_gate_netlist_to_sir(netlist);
    println!("SIR primitives: {}", sir_result.stats.primitives_created);
    assert!(sir_result.stats.primitives_created > 0);
}

#[test]
fn test_xor_gate_pipeline() {
    let source = r#"
        entity XorGate {
            in a: bit
            in b: bit
            out y: bit
        }

        impl XorGate {
            y = a ^ b
        }
    "#;

    let netlists = compile_to_gate_netlist(source);
    let netlist = &netlists[0];

    println!("=== XOR Gate Pipeline ===");
    println!("Cells: {}", netlist.cells.len());

    for cell in &netlist.cells {
        println!("  {} (type: {})", cell.path, cell.cell_type);
    }

    assert!(!netlist.cells.is_empty(), "Should have cells for XOR");

    // Convert to SIR
    let sir_result = convert_gate_netlist_to_sir(netlist);
    assert!(sir_result.stats.primitives_created > 0);
}

#[test]
fn test_mux_pipeline() {
    let source = r#"
        entity Mux2 {
            in sel: bit
            in a: bit
            in b: bit
            out y: bit
        }

        impl Mux2 {
            y = if sel { a } else { b }
        }
    "#;

    let netlists = compile_to_gate_netlist(source);
    let netlist = &netlists[0];

    println!("=== MUX Pipeline ===");
    println!("Cells: {}", netlist.cells.len());

    for cell in &netlist.cells {
        println!("  {} (type: {})", cell.path, cell.cell_type);
    }

    assert!(!netlist.cells.is_empty(), "Should have cells for MUX");

    // Convert to SIR
    let sir_result = convert_gate_netlist_to_sir(netlist);
    assert!(sir_result.stats.primitives_created > 0);
}

#[test]
fn test_multi_bit_adder_pipeline() {
    let source = r#"
        entity Adder4 {
            in a: bit[4]
            in b: bit[4]
            out sum: bit[4]
        }

        impl Adder4 {
            sum = a + b
        }
    "#;

    let netlists = compile_to_gate_netlist(source);
    let netlist = &netlists[0];

    println!("=== 4-bit Adder Pipeline ===");
    println!("Cells: {}", netlist.cells.len());
    println!("Total FIT: {:.4}", netlist.total_fit());

    // 4-bit adder should have multiple cells
    assert!(
        netlist.cells.len() > 1,
        "Should decompose to multiple cells"
    );

    // Convert to SIR
    let sir_result = convert_gate_netlist_to_sir(netlist);
    println!("SIR primitives: {}", sir_result.stats.primitives_created);
    assert!(sir_result.stats.primitives_created > 0);
}

#[test]
fn test_register_pipeline() {
    let source = r#"
        entity Reg4 {
            in clk: clock
            in d: bit[4]
            out q: bit[4]
        }

        impl Reg4 {
            on(clk.rise) {
                q = d
            }
        }
    "#;

    let netlists = compile_to_gate_netlist(source);
    let netlist = &netlists[0];

    println!("=== 4-bit Register Pipeline ===");
    println!("Cells: {}", netlist.cells.len());
    println!("Total FIT: {:.4}", netlist.total_fit());

    for cell in &netlist.cells {
        println!(
            "  {} (type: {}, FIT: {:.4})",
            cell.path, cell.cell_type, cell.fit
        );
    }

    // Should have DFF cells
    let dff_cells: Vec<_> = netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.contains("DFF") || c.cell_type.contains("dff"))
        .collect();

    println!("DFF cells: {}", dff_cells.len());
    assert!(
        dff_cells.len() >= 4,
        "Should have at least 4 DFFs for 4-bit register"
    );

    // Convert to SIR
    let sir_result = convert_gate_netlist_to_sir(netlist);
    assert!(sir_result.stats.primitives_created >= 4);
}

#[test]
fn test_inverter_chain_pipeline() {
    let source = r#"
        entity InvChain {
            in a: bit
            out y: bit
        }

        impl InvChain {
            let b: bit = !a
            let c: bit = !b
            y = !c
        }
    "#;

    let netlists = compile_to_gate_netlist(source);
    let netlist = &netlists[0];

    println!("=== Inverter Chain Pipeline ===");
    println!("Cells: {}", netlist.cells.len());

    for cell in &netlist.cells {
        println!("  {} (type: {})", cell.path, cell.cell_type);
    }

    // Should have inverter cells
    let inv_cells: Vec<_> = netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.contains("INV") || c.cell_type.contains("inv"))
        .collect();

    println!("INV cells: {}", inv_cells.len());

    // Convert to SIR
    let sir_result = convert_gate_netlist_to_sir(netlist);
    assert!(sir_result.stats.primitives_created > 0);
}

#[test]
fn test_complex_logic_pipeline() {
    let source = r#"
        entity ComplexLogic {
            in a: bit
            in b: bit
            in c: bit
            out y: bit
        }

        impl ComplexLogic {
            // y = (a AND b) OR (NOT c)
            let ab: bit = a & b
            let nc: bit = !c
            y = ab | nc
        }
    "#;

    let netlists = compile_to_gate_netlist(source);
    let netlist = &netlists[0];

    println!("=== Complex Logic Pipeline ===");
    println!("Cells: {}", netlist.cells.len());
    println!("Nets: {}", netlist.nets.len());

    for cell in &netlist.cells {
        println!(
            "  {} (type: {}, inputs: {})",
            cell.path,
            cell.cell_type,
            cell.inputs.len()
        );
    }

    // Should have multiple cells for the logic
    assert!(
        netlist.cells.len() >= 2,
        "Should have at least AND, OR/INV cells"
    );

    // Convert to SIR
    let sir_result = convert_gate_netlist_to_sir(netlist);
    assert!(sir_result.stats.primitives_created > 0);
}

#[test]
fn test_sir_conversion_stats() {
    let source = r#"
        entity TestDesign {
            in a: bit[8]
            in b: bit[8]
            out sum: bit[8]
        }

        impl TestDesign {
            sum = a + b
        }
    "#;

    let netlists = compile_to_gate_netlist(source);
    let netlist = &netlists[0];

    let sir_result = convert_gate_netlist_to_sir(netlist);

    println!("=== SIR Conversion Stats ===");
    println!("Signals created: {}", sir_result.stats.signals_created);
    println!(
        "Primitives created: {}",
        sir_result.stats.primitives_created
    );
    println!("Total FIT: {:.4}", sir_result.stats.total_fit);

    // Verify stats make sense
    assert!(sir_result.stats.signals_created > 0, "Should have signals");
    assert!(
        sir_result.stats.primitives_created > 0,
        "Should have primitives"
    );
    assert!(sir_result.stats.total_fit > 0.0, "Should have positive FIT");

    // SIR should have matching name
    assert_eq!(sir_result.sir.name, netlist.name);
}
