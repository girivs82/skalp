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
    gate_netlist::GateNetlist, get_stdlib_library, lower_mir_module_to_lir, tech_mapper::TechMapper,
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

    let library = get_stdlib_library("generic_asic").expect("Failed to load library");

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

// ============================================================================
// Functional Simulation Tests - Verify actual output values
// ============================================================================

use skalp_sim::gate_simulator::GateLevelSimulator;

/// Helper to compile and create simulator
fn compile_and_simulate(source: &str) -> GateLevelSimulator {
    compile_and_simulate_with_debug(source, false)
}

/// Helper to compile and create simulator with optional debug output
fn compile_and_simulate_with_debug(source: &str, debug: bool) -> GateLevelSimulator {
    let netlists = compile_to_gate_netlist(source);
    let netlist = &netlists[0];
    if debug {
        println!("=== Netlist Debug ===");
        println!("Module: {}", netlist.name);
        println!("Cells ({}):", netlist.cells.len());
        for cell in &netlist.cells {
            println!(
                "  {} (type: {}) inputs: {:?} outputs: {:?}",
                cell.path, cell.cell_type, cell.inputs, cell.outputs
            );
        }
        println!("Nets ({}):", netlist.nets.len());
        for (i, net) in netlist.nets.iter().enumerate() {
            println!(
                "  [{}] {} (input:{}, output:{}, driver:{:?})",
                i, net.name, net.is_input, net.is_output, net.driver
            );
        }
    }
    let sir_result = convert_gate_netlist_to_sir(netlist);
    if debug {
        println!("=== SIR Debug ===");
        println!("Signals: {}", sir_result.stats.signals_created);
        println!("Primitives: {}", sir_result.stats.primitives_created);
    }
    GateLevelSimulator::new(&sir_result.sir)
}

#[test]
fn test_and_gate_functional() {
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

    let mut sim = compile_and_simulate(source);

    // AND(0,0) = 0
    sim.set_input("a", &[false]);
    sim.set_input("b", &[false]);
    sim.step();
    assert_eq!(
        sim.get_output("y"),
        Some(vec![false]),
        "AND(0,0) should be 0"
    );

    // AND(0,1) = 0
    sim.set_input("a", &[false]);
    sim.set_input("b", &[true]);
    sim.step();
    assert_eq!(
        sim.get_output("y"),
        Some(vec![false]),
        "AND(0,1) should be 0"
    );

    // AND(1,0) = 0
    sim.set_input("a", &[true]);
    sim.set_input("b", &[false]);
    sim.step();
    assert_eq!(
        sim.get_output("y"),
        Some(vec![false]),
        "AND(1,0) should be 0"
    );

    // AND(1,1) = 1
    sim.set_input("a", &[true]);
    sim.set_input("b", &[true]);
    sim.step();
    assert_eq!(
        sim.get_output("y"),
        Some(vec![true]),
        "AND(1,1) should be 1"
    );
}

#[test]
fn test_or_gate_functional() {
    let source = r#"
        entity OrGate {
            in a: bit
            in b: bit
            out y: bit
        }

        impl OrGate {
            y = a | b
        }
    "#;

    let mut sim = compile_and_simulate(source);

    // OR(0,0) = 0
    sim.set_input("a", &[false]);
    sim.set_input("b", &[false]);
    sim.step();
    assert_eq!(
        sim.get_output("y"),
        Some(vec![false]),
        "OR(0,0) should be 0"
    );

    // OR(0,1) = 1
    sim.set_input("a", &[false]);
    sim.set_input("b", &[true]);
    sim.step();
    assert_eq!(sim.get_output("y"), Some(vec![true]), "OR(0,1) should be 1");

    // OR(1,0) = 1
    sim.set_input("a", &[true]);
    sim.set_input("b", &[false]);
    sim.step();
    assert_eq!(sim.get_output("y"), Some(vec![true]), "OR(1,0) should be 1");

    // OR(1,1) = 1
    sim.set_input("a", &[true]);
    sim.set_input("b", &[true]);
    sim.step();
    assert_eq!(sim.get_output("y"), Some(vec![true]), "OR(1,1) should be 1");
}

#[test]
fn test_xor_gate_functional() {
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

    let mut sim = compile_and_simulate(source);

    // XOR(0,0) = 0
    sim.set_input("a", &[false]);
    sim.set_input("b", &[false]);
    sim.step();
    assert_eq!(
        sim.get_output("y"),
        Some(vec![false]),
        "XOR(0,0) should be 0"
    );

    // XOR(0,1) = 1
    sim.set_input("a", &[false]);
    sim.set_input("b", &[true]);
    sim.step();
    assert_eq!(
        sim.get_output("y"),
        Some(vec![true]),
        "XOR(0,1) should be 1"
    );

    // XOR(1,0) = 1
    sim.set_input("a", &[true]);
    sim.set_input("b", &[false]);
    sim.step();
    assert_eq!(
        sim.get_output("y"),
        Some(vec![true]),
        "XOR(1,0) should be 1"
    );

    // XOR(1,1) = 0
    sim.set_input("a", &[true]);
    sim.set_input("b", &[true]);
    sim.step();
    assert_eq!(
        sim.get_output("y"),
        Some(vec![false]),
        "XOR(1,1) should be 0"
    );
}

#[test]
fn test_inverter_functional() {
    let source = r#"
        entity Inverter {
            in a: bit
            out y: bit
        }

        impl Inverter {
            y = !a
        }
    "#;

    let mut sim = compile_and_simulate(source);

    // NOT(0) = 1
    sim.set_input("a", &[false]);
    sim.step();
    assert_eq!(sim.get_output("y"), Some(vec![true]), "NOT(0) should be 1");

    // NOT(1) = 0
    sim.set_input("a", &[true]);
    sim.step();
    assert_eq!(sim.get_output("y"), Some(vec![false]), "NOT(1) should be 0");
}

#[test]
fn test_mux2_functional() {
    let source = r#"
        entity Mux2 {
            in sel: bit
            in a: bit
            in b: bit
            out y: bit
        }

        impl Mux2 {
            y = if sel { b } else { a }
        }
    "#;

    let mut sim = compile_and_simulate(source);

    // sel=0: y = a
    sim.set_input("sel", &[false]);
    sim.set_input("a", &[true]);
    sim.set_input("b", &[false]);
    sim.step();
    assert_eq!(
        sim.get_output("y"),
        Some(vec![true]),
        "sel=0 should select a=1"
    );

    sim.set_input("sel", &[false]);
    sim.set_input("a", &[false]);
    sim.set_input("b", &[true]);
    sim.step();
    assert_eq!(
        sim.get_output("y"),
        Some(vec![false]),
        "sel=0 should select a=0"
    );

    // sel=1: y = b
    sim.set_input("sel", &[true]);
    sim.set_input("a", &[true]);
    sim.set_input("b", &[false]);
    sim.step();
    assert_eq!(
        sim.get_output("y"),
        Some(vec![false]),
        "sel=1 should select b=0"
    );

    sim.set_input("sel", &[true]);
    sim.set_input("a", &[false]);
    sim.set_input("b", &[true]);
    sim.step();
    assert_eq!(
        sim.get_output("y"),
        Some(vec![true]),
        "sel=1 should select b=1"
    );
}

#[test]
fn test_4bit_adder_functional() {
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

    let mut sim = compile_and_simulate(source);

    // Test cases: (a, b) -> expected sum (mod 16)
    let test_cases = [
        (0, 0, 0),
        (1, 1, 2),
        (3, 4, 7),
        (7, 8, 15),
        (15, 1, 0), // Overflow wraps
        (8, 8, 0),  // Overflow wraps
        (5, 10, 15),
    ];

    for (a, b, expected) in test_cases {
        // Set inputs bit by bit
        for i in 0..4 {
            sim.set_input(&format!("a[{}]", i), &[((a >> i) & 1) == 1]);
            sim.set_input(&format!("b[{}]", i), &[((b >> i) & 1) == 1]);
        }
        sim.step();

        // Collect output bits
        let mut result = 0u64;
        for i in 0..4 {
            if let Some(bits) = sim.get_output(&format!("sum[{}]", i)) {
                if bits.first() == Some(&true) {
                    result |= 1 << i;
                }
            }
        }

        assert_eq!(
            result, expected,
            "Adder: {} + {} should be {} (got {})",
            a, b, expected, result
        );
    }
}

#[test]
fn test_8bit_adder_functional() {
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

    let mut sim = compile_and_simulate(source);

    // Test cases: (a, b) -> expected sum (mod 256)
    let test_cases = [
        (0, 0, 0),
        (1, 1, 2),
        (100, 50, 150),
        (200, 100, 44), // 300 mod 256 = 44
        (255, 1, 0),    // Overflow
        (128, 128, 0),  // Overflow
    ];

    for (a, b, expected) in test_cases {
        for i in 0..8 {
            sim.set_input(&format!("a[{}]", i), &[((a >> i) & 1) == 1]);
            sim.set_input(&format!("b[{}]", i), &[((b >> i) & 1) == 1]);
        }
        sim.step();

        let mut result = 0u64;
        for i in 0..8 {
            if let Some(bits) = sim.get_output(&format!("sum[{}]", i)) {
                if bits.first() == Some(&true) {
                    result |= 1 << i;
                }
            }
        }

        assert_eq!(
            result, expected,
            "Adder8: {} + {} should be {} (got {})",
            a, b, expected, result
        );
    }
}

#[test]
fn test_comparator_functional() {
    let source = r#"
        entity Compare {
            in a: bit[4]
            in b: bit[4]
            out lt: bit
            out eq: bit
            out gt: bit
        }

        impl Compare {
            lt = if a < b { 1 } else { 0 }
            eq = if a == b { 1 } else { 0 }
            gt = if a > b { 1 } else { 0 }
        }
    "#;

    let mut sim = compile_and_simulate(source);

    // Test cases: (a, b) -> (lt, eq, gt)
    let test_cases = [
        (0, 0, (false, true, false)),  // 0 == 0
        (0, 1, (true, false, false)),  // 0 < 1
        (1, 0, (false, false, true)),  // 1 > 0
        (5, 5, (false, true, false)),  // 5 == 5
        (3, 7, (true, false, false)),  // 3 < 7
        (15, 1, (false, false, true)), // 15 > 1
        (8, 8, (false, true, false)),  // 8 == 8
        (0, 15, (true, false, false)), // 0 < 15
    ];

    for (a, b, (exp_lt, exp_eq, exp_gt)) in test_cases {
        for i in 0..4 {
            let a_bit = ((a >> i) & 1) == 1;
            let b_bit = ((b >> i) & 1) == 1;
            sim.set_input(&format!("a[{}]", i), &[a_bit]);
            sim.set_input(&format!("b[{}]", i), &[b_bit]);
        }

        sim.step();

        let lt = sim.get_output("lt").map(|b| b[0]).unwrap_or(false);
        let eq = sim.get_output("eq").map(|b| b[0]).unwrap_or(false);
        let gt = sim.get_output("gt").map(|b| b[0]).unwrap_or(false);

        assert_eq!(
            (lt, eq, gt),
            (exp_lt, exp_eq, exp_gt),
            "Compare {} vs {}: expected ({},{},{}), got ({},{},{})",
            a,
            b,
            exp_lt,
            exp_eq,
            exp_gt,
            lt,
            eq,
            gt
        );
    }
}
