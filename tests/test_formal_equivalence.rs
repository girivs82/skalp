//! Formal Equivalence Checking Tests
//!
//! Tests the formal verification of synthesis correctness by proving
//! that the synthesized gate netlist is functionally equivalent to
//! the original LIR (behavioral) representation.

use skalp_formal::{EquivalenceChecker, LirToAig};
use skalp_lir::{Lir, LirOp};

/// Create a simple test LIR: 2-input AND gate
fn create_and_gate_lir() -> Lir {
    let mut lir = Lir::new("and_gate".to_string());

    // Create input signals
    let a = lir.add_input("a".to_string(), 1);
    let b = lir.add_input("b".to_string(), 1);

    // Create output signal
    let out = lir.add_output("out".to_string(), 1);

    // Create AND node
    lir.add_node(LirOp::And { width: 1 }, vec![a, b], out, "and_gate".to_string());

    lir
}

/// Create a simple test LIR: 2-input OR gate
fn create_or_gate_lir() -> Lir {
    let mut lir = Lir::new("or_gate".to_string());

    // Create input signals
    let a = lir.add_input("a".to_string(), 1);
    let b = lir.add_input("b".to_string(), 1);

    // Create output signal
    let out = lir.add_output("out".to_string(), 1);

    // Create OR node
    lir.add_node(LirOp::Or { width: 1 }, vec![a, b], out, "or_gate".to_string());

    lir
}

/// Create a simple test LIR: XOR gate (more complex)
fn create_xor_gate_lir() -> Lir {
    let mut lir = Lir::new("xor_gate".to_string());

    // Create input signals
    let a = lir.add_input("a".to_string(), 1);
    let b = lir.add_input("b".to_string(), 1);

    // Create output signal
    let out = lir.add_output("out".to_string(), 1);

    // Create XOR node
    lir.add_node(LirOp::Xor { width: 1 }, vec![a, b], out, "xor_gate".to_string());

    lir
}

/// Create a 2-bit adder LIR
fn create_adder_lir() -> Lir {
    let mut lir = Lir::new("adder".to_string());

    // Create input signals (2-bit each)
    let a = lir.add_input("a".to_string(), 2);
    let b = lir.add_input("b".to_string(), 2);

    // Create output signal (3-bit with carry)
    let sum = lir.add_output("sum".to_string(), 3);

    // Create ADD node
    lir.add_node(
        LirOp::Add {
            width: 2,
            has_carry: true,
        },
        vec![a, b],
        sum,
        "adder".to_string(),
    );

    lir
}

/// Create a 2:1 MUX LIR
fn create_mux_lir() -> Lir {
    let mut lir = Lir::new("mux".to_string());

    // Create input signals
    let sel = lir.add_input("sel".to_string(), 1);
    let a = lir.add_input("a".to_string(), 4);
    let b = lir.add_input("b".to_string(), 4);

    // Create output signal
    let out = lir.add_output("out".to_string(), 4);

    // Create MUX node
    lir.add_node(
        LirOp::Mux2 { width: 4 },
        vec![sel, a, b],
        out,
        "mux".to_string(),
    );

    lir
}

/// Test that LIR to AIG conversion produces correct results
#[test]
fn test_lir_to_aig_and_gate() {
    let lir = create_and_gate_lir();
    let aig = LirToAig::new().convert(&lir);

    // Should have 2 inputs, 1 output
    assert_eq!(aig.inputs.len(), 2);
    assert_eq!(aig.outputs.len(), 1);

    // Should have 1 AND gate
    assert_eq!(aig.and_count(), 1);

    println!(
        "AND gate LIR -> AIG: {} nodes, {} AND gates",
        aig.nodes.len(),
        aig.and_count()
    );
}

/// Test that LIR to AIG conversion works for XOR
#[test]
fn test_lir_to_aig_xor_gate() {
    let lir = create_xor_gate_lir();
    let aig = LirToAig::new().convert(&lir);

    // Should have 2 inputs, 1 output
    assert_eq!(aig.inputs.len(), 2);
    assert_eq!(aig.outputs.len(), 1);

    // XOR requires multiple AND gates (a XOR b = (a AND !b) OR (!a AND b))
    assert!(aig.and_count() > 0);

    println!(
        "XOR gate LIR -> AIG: {} nodes, {} AND gates",
        aig.nodes.len(),
        aig.and_count()
    );
}

/// Test that LIR to AIG conversion works for adder
#[test]
fn test_lir_to_aig_adder() {
    let lir = create_adder_lir();
    let aig = LirToAig::new().convert(&lir);

    // Should have 4 inputs (2 bits each), 3 outputs (2-bit sum + carry)
    assert_eq!(aig.inputs.len(), 4);
    assert_eq!(aig.outputs.len(), 3);

    println!(
        "2-bit adder LIR -> AIG: {} nodes, {} AND gates",
        aig.nodes.len(),
        aig.and_count()
    );
}

/// Test that LIR to AIG conversion works for MUX
#[test]
fn test_lir_to_aig_mux() {
    let lir = create_mux_lir();
    let aig = LirToAig::new().convert(&lir);

    // Should have 9 inputs (1 sel + 4 a + 4 b), 4 outputs
    assert_eq!(aig.inputs.len(), 9);
    assert_eq!(aig.outputs.len(), 4);

    println!(
        "4-bit MUX LIR -> AIG: {} nodes, {} AND gates",
        aig.nodes.len(),
        aig.and_count()
    );
}

/// Test equivalence checking between identical LIRs
#[test]
fn test_equivalence_identical() {
    let lir1 = create_and_gate_lir();
    let lir2 = create_and_gate_lir();

    let checker = EquivalenceChecker::new();
    let result = checker.check_lir_equivalence(&lir1, &lir2).unwrap();

    assert!(result.equivalent, "Identical LIRs should be equivalent");
    assert!(result.counterexample.is_none());

    println!("Equivalence check passed in {}ms", result.time_ms);
}

/// Test equivalence checking detects differences (AND vs OR)
#[test]
fn test_equivalence_and_vs_or() {
    let lir1 = create_and_gate_lir();
    let lir2 = create_or_gate_lir();

    let checker = EquivalenceChecker::new();
    let result = checker.check_lir_equivalence(&lir1, &lir2).unwrap();

    assert!(!result.equivalent, "AND and OR should NOT be equivalent");
    assert!(
        result.counterexample.is_some(),
        "Should have counterexample"
    );

    if let Some(ce) = &result.counterexample {
        println!("Counterexample found: {:?}", ce.trace[0].assignments);
        // Verify the counterexample makes sense
        // AND(a,b) != OR(a,b) when exactly one input is 1
    }
}

/// Test equivalence checking detects differences (AND vs XOR)
#[test]
fn test_equivalence_and_vs_xor() {
    let lir1 = create_and_gate_lir();
    let lir2 = create_xor_gate_lir();

    let checker = EquivalenceChecker::new();
    let result = checker.check_lir_equivalence(&lir1, &lir2).unwrap();

    assert!(!result.equivalent, "AND and XOR should NOT be equivalent");
    assert!(
        result.counterexample.is_some(),
        "Should have counterexample"
    );

    if let Some(ce) = &result.counterexample {
        println!("Counterexample (AND vs XOR): {:?}", ce.trace[0].assignments);
    }
}

/// Test that adder equivalence checking works
#[test]
fn test_equivalence_adder() {
    let lir1 = create_adder_lir();
    let lir2 = create_adder_lir();

    let checker = EquivalenceChecker::new();
    let result = checker.check_lir_equivalence(&lir1, &lir2).unwrap();

    assert!(result.equivalent, "Identical adders should be equivalent");
    println!("Adder equivalence check passed in {}ms", result.time_ms);
}

/// Test that MUX equivalence checking works
#[test]
fn test_equivalence_mux() {
    let lir1 = create_mux_lir();
    let lir2 = create_mux_lir();

    let checker = EquivalenceChecker::new();
    let result = checker.check_lir_equivalence(&lir1, &lir2).unwrap();

    assert!(result.equivalent, "Identical MUXes should be equivalent");
    println!("MUX equivalence check passed in {}ms", result.time_ms);
}

/// Test De Morgan's law: !(a AND b) == (!a OR !b)
#[test]
fn test_demorgan_nand() {
    // Create NAND gate: !(a AND b)
    let mut lir_nand = Lir::new("nand".to_string());
    let a1 = lir_nand.add_input("a".to_string(), 1);
    let b1 = lir_nand.add_input("b".to_string(), 1);
    let out1 = lir_nand.add_output("out".to_string(), 1);
    lir_nand.add_node(
        LirOp::Nand { width: 1 },
        vec![a1, b1],
        out1,
        "nand".to_string(),
    );

    // Create equivalent using NOR of inverted inputs: !(!a) OR !(!b) -> after double neg: !a OR !b
    // Actually, let's create OR of inverted inputs using NOT + OR
    let mut lir_equiv = Lir::new("demorgan".to_string());
    let a2 = lir_equiv.add_input("a".to_string(), 1);
    let b2 = lir_equiv.add_input("b".to_string(), 1);
    let not_a = lir_equiv.add_signal("not_a".to_string(), 1);
    let not_b = lir_equiv.add_signal("not_b".to_string(), 1);
    let out2 = lir_equiv.add_output("out".to_string(), 1);

    lir_equiv.add_node(LirOp::Not { width: 1 }, vec![a2], not_a, "not_a".to_string());
    lir_equiv.add_node(LirOp::Not { width: 1 }, vec![b2], not_b, "not_b".to_string());
    lir_equiv.add_node(
        LirOp::Or { width: 1 },
        vec![not_a, not_b],
        out2,
        "or".to_string(),
    );

    let checker = EquivalenceChecker::new();
    let result = checker.check_lir_equivalence(&lir_nand, &lir_equiv).unwrap();

    assert!(
        result.equivalent,
        "De Morgan's law: NAND(a,b) == OR(!a, !b) should hold"
    );
    println!("De Morgan's law verified in {}ms", result.time_ms);
}

/// Test that comparison operations work
#[test]
fn test_equivalence_comparison() {
    // Create equality checker
    let mut lir1 = Lir::new("eq".to_string());
    let a1 = lir1.add_input("a".to_string(), 4);
    let b1 = lir1.add_input("b".to_string(), 4);
    let out1 = lir1.add_output("out".to_string(), 1);
    lir1.add_node(LirOp::Eq { width: 4 }, vec![a1, b1], out1, "eq".to_string());

    // Create identical equality checker
    let mut lir2 = Lir::new("eq2".to_string());
    let a2 = lir2.add_input("a".to_string(), 4);
    let b2 = lir2.add_input("b".to_string(), 4);
    let out2 = lir2.add_output("out".to_string(), 1);
    lir2.add_node(LirOp::Eq { width: 4 }, vec![a2, b2], out2, "eq2".to_string());

    let checker = EquivalenceChecker::new();
    let result = checker.check_lir_equivalence(&lir1, &lir2).unwrap();

    assert!(result.equivalent, "Identical EQ should be equivalent");
    println!("4-bit equality check verified in {}ms", result.time_ms);
}

/// Test that EQ and NE are complements
#[test]
fn test_eq_ne_complement() {
    // Create EQ with NOT
    let mut lir1 = Lir::new("not_eq".to_string());
    let a1 = lir1.add_input("a".to_string(), 4);
    let b1 = lir1.add_input("b".to_string(), 4);
    let eq1 = lir1.add_signal("eq".to_string(), 1);
    let out1 = lir1.add_output("out".to_string(), 1);
    lir1.add_node(LirOp::Eq { width: 4 }, vec![a1, b1], eq1, "eq".to_string());
    lir1.add_node(LirOp::Not { width: 1 }, vec![eq1], out1, "not".to_string());

    // Create NE directly
    let mut lir2 = Lir::new("ne".to_string());
    let a2 = lir2.add_input("a".to_string(), 4);
    let b2 = lir2.add_input("b".to_string(), 4);
    let out2 = lir2.add_output("out".to_string(), 1);
    lir2.add_node(LirOp::Ne { width: 4 }, vec![a2, b2], out2, "ne".to_string());

    let checker = EquivalenceChecker::new();
    let result = checker.check_lir_equivalence(&lir1, &lir2).unwrap();

    assert!(result.equivalent, "NOT(EQ(a,b)) should equal NE(a,b)");
    println!("EQ/NE complement verified in {}ms", result.time_ms);
}
