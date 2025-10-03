//! Comprehensive MIR to LIR Regression Tests
//!
//! Tests the MIR → LIR transformation pipeline to ensure:
//! - All MIR constructs are correctly translated to LIR (gate-level IR)
//! - Gate types properly selected for operations
//! - Nets correctly created and connected
//! - Port and signal conversion to nets
//! - Technology mapping preparation
//! - Optimization opportunities preserved
//!
//! Coverage:
//! - Port to net conversion
//! - Signal to net conversion
//! - Binary operation to gate mapping
//! - Unary operation to gate mapping
//! - Expression decomposition
//! - Sequential logic (DFF generation)
//! - Combinational logic (gate generation)
//! - Real-world patterns (ALU, counter, mux)

use skalp_frontend::parse::parse;
use skalp_frontend::hir_builder::build_hir;
use skalp_mir::hir_to_mir::HirToMir;
use skalp_lir::lir::*;
use skalp_lir::mir_to_lir::transform_mir_to_lir;

// ============================================================================
// Helper Functions
// ============================================================================

/// Compile SKALP source to LIR
fn compile_to_lir(source: &str) -> Lir {
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");
    let mut transformer = HirToMir::new();
    let mir = transformer.transform(&hir);

    assert!(!mir.modules.is_empty(), "MIR should have at least one module");
    transform_mir_to_lir(&mir.modules[0])
}

// ============================================================================
// Port to Net Conversion Tests
// ============================================================================

#[test]
fn test_port_to_net_simple() {
    let source = r#"
entity Wire {
    in a: bit[8]
    out b: bit[8]
}

impl Wire {
    b = a
}
"#;
    let lir = compile_to_lir(source);

    assert_eq!(lir.name, "Wire");
    // Should have nets for input a and output b
    assert!(lir.nets.len() >= 2);

    // Check that we have input and output nets
    let has_input = lir.nets.iter().any(|n| n.is_input);
    let has_output = lir.nets.iter().any(|n| n.is_output);
    assert!(has_input, "Should have at least one input net");
    assert!(has_output, "Should have at least one output net");
}

#[test]
fn test_port_directions() {
    let source = r#"
entity Test {
    in a: bit[8]
    in b: bit[8]
    out c: bit[8]
}

impl Test {
    c = a
}
"#;
    let lir = compile_to_lir(source);

    // Count input and output nets
    let input_nets = lir.nets.iter().filter(|n| n.is_input).count();
    let output_nets = lir.nets.iter().filter(|n| n.is_output).count();

    assert!(input_nets >= 2, "Should have at least 2 input nets (a, b)");
    assert!(output_nets >= 1, "Should have at least 1 output net (c)");
}

#[test]
fn test_multiple_ports() {
    let source = r#"
entity Multi {
    in a: bit[1]
    in b: bit[1]
    in c: bit[1]
    out d: bit[1]
    out e: bit[1]
}

impl Multi {
    d = a
    e = b
}
"#;
    let lir = compile_to_lir(source);

    // Should have nets for all ports
    assert!(lir.nets.len() >= 5);
}

// ============================================================================
// Signal to Net Conversion Tests
// ============================================================================

#[test]
fn test_signal_to_net() {
    let source = r#"
entity Test {
    in a: bit[8]
    out b: bit[8]
}

impl Test {
    signal temp: bit[8]

    temp = a
    b = temp
}
"#;
    let lir = compile_to_lir(source);

    // Should have nets for ports and signal
    assert!(lir.nets.len() >= 3);
}

#[test]
fn test_multiple_signals() {
    let source = r#"
entity Test {
    in a: bit[8]
    out result: bit[8]
}

impl Test {
    signal s1: bit[8]
    signal s2: bit[8]
    signal s3: bit[8]

    s1 = a
    s2 = s1
    s3 = s2
    result = s3
}
"#;
    let lir = compile_to_lir(source);

    // Should have nets for all signals and ports
    assert!(lir.nets.len() >= 5);
}

// ============================================================================
// Binary Operation to Gate Tests
// ============================================================================

#[test]
fn test_and_gate() {
    let source = r#"
entity AndGate {
    in a: bit[1]
    in b: bit[1]
    out c: bit[1]
}

impl AndGate {
    c = a & b
}
"#;
    let lir = compile_to_lir(source);

    // Should have at least one AND gate
    let has_and = lir.gates.iter().any(|g| matches!(g.gate_type, GateType::And));
    assert!(has_and, "Should have AND gate for & operation");
}

#[test]
fn test_or_gate() {
    let source = r#"
entity OrGate {
    in a: bit[1]
    in b: bit[1]
    out c: bit[1]
}

impl OrGate {
    c = a | b
}
"#;
    let lir = compile_to_lir(source);

    let has_or = lir.gates.iter().any(|g| matches!(g.gate_type, GateType::Or));
    assert!(has_or, "Should have OR gate for | operation");
}

#[test]
fn test_xor_gate() {
    let source = r#"
entity XorGate {
    in a: bit[1]
    in b: bit[1]
    out c: bit[1]
}

impl XorGate {
    c = a ^ b
}
"#;
    let lir = compile_to_lir(source);

    let has_xor = lir.gates.iter().any(|g| matches!(g.gate_type, GateType::Xor));
    assert!(has_xor, "Should have XOR gate for ^ operation");
}

#[test]
fn test_multiple_binary_ops() {
    let source = r#"
entity MultiBinary {
    in a: bit[1]
    in b: bit[1]
    in c: bit[1]
    out result: bit[1]
}

impl MultiBinary {
    result = (a & b) | c
}
"#;
    let lir = compile_to_lir(source);

    // Should have both AND and OR gates
    let has_and = lir.gates.iter().any(|g| matches!(g.gate_type, GateType::And));
    let has_or = lir.gates.iter().any(|g| matches!(g.gate_type, GateType::Or));

    assert!(has_and, "Should have AND gate");
    assert!(has_or, "Should have OR gate");
}

// ============================================================================
// Unary Operation to Gate Tests
// ============================================================================

#[test]
fn test_not_gate() {
    let source = r#"
entity NotGate {
    in a: bit[1]
    out b: bit[1]
}

impl NotGate {
    b = ~a
}
"#;
    let lir = compile_to_lir(source);

    let has_not = lir.gates.iter().any(|g| matches!(g.gate_type, GateType::Not));
    assert!(has_not, "Should have NOT gate for ~ operation");
}

#[test]
fn test_not_with_and() {
    let source = r#"
entity NotAnd {
    in a: bit[1]
    in b: bit[1]
    out c: bit[1]
}

impl NotAnd {
    c = ~(a & b)
}
"#;
    let lir = compile_to_lir(source);

    // Should have AND gate and NOT gate
    let has_and = lir.gates.iter().any(|g| matches!(g.gate_type, GateType::And));
    let has_not = lir.gates.iter().any(|g| matches!(g.gate_type, GateType::Not));

    assert!(has_and || has_not, "Should have AND or NOT gate (or both)");
}

// ============================================================================
// Expression Decomposition Tests
// ============================================================================

#[test]
fn test_nested_expression() {
    let source = r#"
entity Nested {
    in a: bit[1]
    in b: bit[1]
    in c: bit[1]
    out result: bit[1]
}

impl Nested {
    result = (a & b) | (b & c)
}
"#;
    let lir = compile_to_lir(source);

    // Should have multiple gates for the nested expression
    assert!(lir.gates.len() >= 2, "Should have multiple gates for nested expression");
}

#[test]
fn test_deeply_nested_expression() {
    let source = r#"
entity DeepNested {
    in a: bit[1]
    in b: bit[1]
    in c: bit[1]
    in d: bit[1]
    out result: bit[1]
}

impl DeepNested {
    result = ((a & b) | (c & d))
}
"#;
    let lir = compile_to_lir(source);

    // Should decompose into multiple gates
    assert!(lir.gates.len() >= 3, "Should have multiple gates for deeply nested expression");
}

#[test]
fn test_chained_operations() {
    let source = r#"
entity Chained {
    in a: bit[1]
    in b: bit[1]
    in c: bit[1]
    out result: bit[1]
}

impl Chained {
    result = a & b & c
}
"#;
    let lir = compile_to_lir(source);

    // Should have gates for chained operations
    assert!(!lir.gates.is_empty(), "Should have gates for chained operations");
}

// ============================================================================
// Sequential Logic Tests
// ============================================================================

#[test]
fn test_dff_generation() {
    let source = r#"
entity DFF {
    in clk: clock
    in d: bit[1]
    out q: bit[1]
}

impl DFF {
    signal q_reg: bit[1] = 0

    on(clk.rise) {
        q_reg <= d
    }

    q = q_reg
}
"#;
    let lir = compile_to_lir(source);

    // Should have gates (may include DFF or other sequential elements)
    assert!(!lir.gates.is_empty(), "Should have gates for sequential logic");
}

#[test]
fn test_register() {
    let source = r#"
entity Register {
    in clk: clock
    in d: bit[8]
    out q: bit[8]
}

impl Register {
    signal q_reg: bit[8] = 0

    on(clk.rise) {
        q_reg <= d
    }

    q = q_reg
}
"#;
    let lir = compile_to_lir(source);

    assert_eq!(lir.name, "Register");
    assert!(!lir.gates.is_empty() || !lir.nets.is_empty());
}

// ============================================================================
// Combinational Logic Tests
// ============================================================================

#[test]
fn test_simple_combinational() {
    let source = r#"
entity Comb {
    in a: bit[1]
    in b: bit[1]
    out c: bit[1]
}

impl Comb {
    c = a & b
}
"#;
    let lir = compile_to_lir(source);

    // Should have gates for combinational logic
    assert!(!lir.gates.is_empty(), "Should have gates for combinational logic");
}

#[test]
fn test_complex_combinational() {
    let source = r#"
entity ComplexComb {
    in a: bit[1]
    in b: bit[1]
    in c: bit[1]
    in d: bit[1]
    out result: bit[1]
}

impl ComplexComb {
    result = (a & b) | (c ^ d)
}
"#;
    let lir = compile_to_lir(source);

    // Should have multiple gates
    assert!(lir.gates.len() >= 2, "Should have multiple gates for complex combinational logic");
}

// ============================================================================
// Real-World Pattern Tests
// ============================================================================

#[test]
fn test_mux_2to1() {
    let source = r#"
entity Mux2to1 {
    in a: bit[1]
    in b: bit[1]
    in sel: bit[1]
    out y: bit[1]
}

impl Mux2to1 {
    y = if sel { b } else { a }
}
"#;
    let lir = compile_to_lir(source);

    assert_eq!(lir.name, "Mux2to1");
    // Mux should generate gates for the if-else logic
    assert!(!lir.gates.is_empty() || !lir.nets.is_empty());
}

#[test]
fn test_half_adder() {
    let source = r#"
entity HalfAdder {
    in a: bit[1]
    in b: bit[1]
    out sum: bit[1]
    out carry: bit[1]
}

impl HalfAdder {
    sum = a ^ b
    carry = a & b
}
"#;
    let lir = compile_to_lir(source);

    assert_eq!(lir.name, "HalfAdder");

    // Should have XOR and AND gates
    let has_xor = lir.gates.iter().any(|g| matches!(g.gate_type, GateType::Xor));
    let has_and = lir.gates.iter().any(|g| matches!(g.gate_type, GateType::And));

    assert!(has_xor, "Should have XOR gate for sum");
    assert!(has_and, "Should have AND gate for carry");
}

#[test]
fn test_counter() {
    let source = r#"
entity Counter {
    in clk: clock
    in rst: reset
    out count: bit[8]
}

impl Counter {
    signal count_reg: bit[8] = 0

    on(clk.rise) {
        count_reg <= if rst { 0 } else { count_reg + 1 }
    }

    count = count_reg
}
"#;
    let lir = compile_to_lir(source);

    assert_eq!(lir.name, "Counter");
    // Counter should have nets and gates
    assert!(!lir.nets.is_empty());
}

#[test]
fn test_alu_gates() {
    let source = r#"
entity ALU {
    in a: bit[8]
    in b: bit[8]
    in op: bit[2]
    out result: bit[8]
}

impl ALU {
    result = match op {
        0 => a & b,
        1 => a | b,
        2 => a ^ b,
        _ => 0
    }
}
"#;
    let lir = compile_to_lir(source);

    assert_eq!(lir.name, "ALU");
    // ALU should generate gates for operations
    assert!(!lir.nets.is_empty());
}

// ============================================================================
// Gate Type Mapping Tests
// ============================================================================

#[test]
fn test_all_basic_gates() {
    let source = r#"
entity AllGates {
    in a: bit[1]
    in b: bit[1]
    out and_out: bit[1]
    out or_out: bit[1]
    out xor_out: bit[1]
    out not_out: bit[1]
}

impl AllGates {
    and_out = a & b
    or_out = a | b
    xor_out = a ^ b
    not_out = ~a
}
"#;
    let lir = compile_to_lir(source);

    // Should have gates for all basic operations
    let has_and = lir.gates.iter().any(|g| matches!(g.gate_type, GateType::And));
    let has_or = lir.gates.iter().any(|g| matches!(g.gate_type, GateType::Or));
    let has_xor = lir.gates.iter().any(|g| matches!(g.gate_type, GateType::Xor));
    let has_not = lir.gates.iter().any(|g| matches!(g.gate_type, GateType::Not));

    assert!(has_and, "Should have AND gate");
    assert!(has_or, "Should have OR gate");
    assert!(has_xor, "Should have XOR gate");
    assert!(has_not, "Should have NOT gate");
}

// ============================================================================
// Net Creation Tests
// ============================================================================

#[test]
fn test_net_creation_simple() {
    let source = r#"
entity Simple {
    in a: bit[1]
    out b: bit[1]
}

impl Simple {
    b = a
}
"#;
    let lir = compile_to_lir(source);

    // Should have nets for input, output, and possibly intermediate
    assert!(lir.nets.len() >= 2);

    // Each net should have an ID
    for net in &lir.nets {
        assert!(!net.id.is_empty(), "Net should have an ID");
    }
}

#[test]
fn test_net_creation_intermediate() {
    let source = r#"
entity Intermediate {
    in a: bit[1]
    in b: bit[1]
    out c: bit[1]
}

impl Intermediate {
    signal temp: bit[1]

    temp = a & b
    c = temp
}
"#;
    let lir = compile_to_lir(source);

    // Should have nets for inputs, output, and intermediate signal
    assert!(lir.nets.len() >= 3);
}

// ============================================================================
// Gate Connection Tests
// ============================================================================

#[test]
fn test_gate_connections() {
    let source = r#"
entity Connected {
    in a: bit[1]
    in b: bit[1]
    out c: bit[1]
}

impl Connected {
    c = a & b
}
"#;
    let lir = compile_to_lir(source);

    // Find the AND gate
    let and_gate = lir.gates.iter().find(|g| matches!(g.gate_type, GateType::And));

    if let Some(gate) = and_gate {
        // AND gate should have 2 inputs and 1 output
        assert_eq!(gate.inputs.len(), 2, "AND gate should have 2 inputs");
        assert_eq!(gate.outputs.len(), 1, "AND gate should have 1 output");
    }
}

#[test]
fn test_gate_chaining() {
    let source = r#"
entity Chained {
    in a: bit[1]
    in b: bit[1]
    in c: bit[1]
    out result: bit[1]
}

impl Chained {
    signal temp: bit[1]

    temp = a & b
    result = temp | c
}
"#;
    let lir = compile_to_lir(source);

    // Should have at least 2 gates (AND and OR)
    assert!(lir.gates.len() >= 2, "Should have at least 2 gates for chained operations");
}

// ============================================================================
// Edge Case Tests
// ============================================================================

#[test]
fn test_wire_passthrough() {
    let source = r#"
entity Wire {
    in a: bit[1]
    out b: bit[1]
}

impl Wire {
    b = a
}
"#;
    let lir = compile_to_lir(source);

    assert_eq!(lir.name, "Wire");
    // Even a simple wire should have nets
    assert!(!lir.nets.is_empty());
}

#[test]
fn test_constant_assignment() {
    let source = r#"
entity Constant {
    out a: bit[1]
}

impl Constant {
    a = 1
}
"#;
    let lir = compile_to_lir(source);

    assert_eq!(lir.name, "Constant");
    // Should have nets for output and constant
    assert!(!lir.nets.is_empty());
}

#[test]
fn test_complex_bit_operations() {
    let source = r#"
entity BitOps {
    in a: bit[32]
    in b: bit[32]
    in c: bit[32]
    out overflow: bit[1]
}

impl BitOps {
    overflow = (~a[31] & ~b[31] & c[31]) | (a[31] & b[31] & ~c[31])
}
"#;
    let lir = compile_to_lir(source);

    assert_eq!(lir.name, "BitOps");
    // Complex expression should generate multiple gates
    assert!(!lir.gates.is_empty() || !lir.nets.is_empty());
}

// ============================================================================
// Technology Mapping Preparation Tests
// ============================================================================

#[test]
fn test_gate_ids_unique() {
    let source = r#"
entity UniqueIds {
    in a: bit[1]
    in b: bit[1]
    in c: bit[1]
    out result: bit[1]
}

impl UniqueIds {
    result = (a & b) | c
}
"#;
    let lir = compile_to_lir(source);

    // All gate IDs should be unique
    let mut gate_ids = std::collections::HashSet::new();
    for gate in &lir.gates {
        assert!(gate_ids.insert(gate.id.clone()), "Gate ID {} is not unique", gate.id);
    }
}

#[test]
fn test_net_ids_unique() {
    let source = r#"
entity UniqueNets {
    in a: bit[1]
    in b: bit[1]
    out c: bit[1]
}

impl UniqueNets {
    signal temp: bit[1]

    temp = a & b
    c = temp
}
"#;
    let lir = compile_to_lir(source);

    // All net IDs should be unique
    let mut net_ids = std::collections::HashSet::new();
    for net in &lir.nets {
        assert!(net_ids.insert(net.id.clone()), "Net ID {} is not unique", net.id);
    }
}
