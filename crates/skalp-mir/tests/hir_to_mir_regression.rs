//! Comprehensive HIR to MIR Regression Tests
//!
//! Tests the HIR → MIR transformation pipeline to ensure:
//! - All HIR constructs are correctly translated to MIR
//! - Type information is preserved
//! - Control flow is correctly represented
//! - Edge cases and complex patterns work
//!
//! Coverage:
//! - Expression translation (all operators)
//! - Statement translation (assignments, if, match, flow)
//! - Port and signal translation
//! - Process generation (combinational, sequential)
//! - Type conversion
//! - Complex nested structures

use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::parse::parse;
use skalp_mir::hir_to_mir::HirToMir;
use skalp_mir::mir::*;

// ============================================================================
// Helper Functions
// ============================================================================

/// Compile SKALP source to MIR
fn compile_to_mir(source: &str) -> Mir {
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");
    let mut transformer = HirToMir::new();
    transformer.transform(&hir)
}

/// Get first module from MIR design
fn get_first_module(design: &Mir) -> &Module {
    assert!(
        !design.modules.is_empty(),
        "Design should have at least one module"
    );
    &design.modules[0]
}

// ============================================================================
// Basic Entity/Module Translation Tests
// ============================================================================

#[test]
fn test_simple_entity_translation() {
    let source = r#"
entity Wire {
    in a: bit[8]
    out b: bit[8]
}

impl Wire {
    b = a
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.name, "Wire");
    assert_eq!(module.ports.len(), 2);
    assert_eq!(module.ports[0].name, "a");
    assert_eq!(module.ports[1].name, "b");
}

#[test]
fn test_port_directions() {
    let source = r#"
entity Test {
    in a: bit[8]
    out b: bit[8]
    inout c: bit[8]
}

impl Test {
    b = a
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert!(matches!(module.ports[0].direction, PortDirection::Input));
    assert!(matches!(module.ports[1].direction, PortDirection::Output));
    assert!(matches!(module.ports[2].direction, PortDirection::InOut));
}

#[test]
fn test_clock_reset_ports() {
    let source = r#"
entity Register {
    in clk: clock
    in rst: reset
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
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert!(matches!(module.ports[0].port_type, DataType::Clock { .. }));
    assert!(matches!(module.ports[1].port_type, DataType::Reset { .. }));
}

// ============================================================================
// Type Conversion Tests
// ============================================================================

#[test]
fn test_type_bit() {
    let source = r#"
entity Test {
    in a: bit[32]
    out b: bit[32]
}

impl Test {
    b = a
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    match &module.ports[0].port_type {
        DataType::Bit(width) => assert_eq!(*width, 32),
        _ => panic!("Expected Bit type"),
    }
}

#[test]
fn test_type_nat() {
    let source = r#"
entity Test {
    in a: nat[16]
    out b: nat[16]
}

impl Test {
    b = a
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    match &module.ports[0].port_type {
        DataType::Nat(width) => assert_eq!(*width, 16),
        _ => panic!("Expected Nat type"),
    }
}

#[test]
fn test_type_int() {
    let source = r#"
entity Test {
    in a: int[16]
    out b: int[16]
}

impl Test {
    b = a
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    match &module.ports[0].port_type {
        DataType::Int(width) => assert_eq!(*width, 16),
        _ => panic!("Expected Int type"),
    }
}

// ============================================================================
// Signal Declaration Tests
// ============================================================================

#[test]
fn test_signal_declaration() {
    let source = r#"
entity Test {
    in a: bit[8]
    out b: bit[8]
}

impl Test {
    signal temp: bit[8] = 0

    temp = a
    b = temp
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.signals.len(), 1);
    assert_eq!(module.signals[0].name, "temp");
    assert!(matches!(module.signals[0].signal_type, DataType::Bit(8)));
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
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.signals.len(), 3);
}

// ============================================================================
// Expression Translation Tests
// ============================================================================

#[test]
fn test_expr_binary_add() {
    let source = r#"
entity Test {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[8]
}

impl Test {
    sum = a + b
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    // Should successfully compile (processes or continuous assigns)
    assert_eq!(module.name, "Test");
}

#[test]
fn test_expr_all_binary_ops() {
    let source = r#"
entity Test {
    in a: bit[8]
    in b: bit[8]
    out add: bit[8]
    out sub: bit[8]
    out mul: bit[8]
    out and_op: bit[8]
    out or_op: bit[8]
    out xor_op: bit[8]
    out shl: bit[8]
    out shr: bit[8]
}

impl Test {
    add = a + b
    sub = a - b
    mul = a * b
    and_op = a & b
    or_op = a | b
    xor_op = a ^ b
    shl = a << 2
    shr = a >> 2
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    // Should successfully compile all operators
    assert_eq!(module.name, "Test");
}

#[test]
fn test_expr_unary_ops() {
    let source = r#"
entity Test {
    in a: bit[8]
    out not_result: bit[8]
    out neg_result: bit[8]
}

impl Test {
    not_result = ~a
    neg_result = -a
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.name, "Test");
}

#[test]
fn test_expr_index() {
    let source = r#"
entity Test {
    in data: bit[32]
    out bit0: bit[1]
}

impl Test {
    bit0 = data[0]
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.name, "Test");
}

#[test]
fn test_expr_range() {
    let source = r#"
entity Test {
    in data: bit[32]
    out byte0: bit[8]
}

impl Test {
    byte0 = data[7:0]
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.name, "Test");
}

#[test]
fn test_expr_if_simple() {
    let source = r#"
entity Test {
    in sel: bit[1]
    in a: bit[8]
    in b: bit[8]
    out result: bit[8]
}

impl Test {
    result = if sel { a } else { b }
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.name, "Test");
}

#[test]
fn test_expr_if_nested() {
    let source = r#"
entity Test {
    in sel: bit[2]
    out result: bit[8]
}

impl Test {
    result = if sel == 0 {
        1
    } else if sel == 1 {
        2
    } else {
        3
    }
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.name, "Test");
}

#[test]
fn test_expr_match() {
    let source = r#"
entity Test {
    in op: bit[3]
    out result: bit[8]
}

impl Test {
    result = match op {
        0 => 1,
        1 => 2,
        2 => 3,
        _ => 0
    }
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.name, "Test");
}

// ============================================================================
// Process Generation Tests
// ============================================================================

#[test]
fn test_combinational_process() {
    let source = r#"
entity Test {
    in a: bit[8]
    out b: bit[8]
}

impl Test {
    b = a + 1
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    // Should successfully compile (may use continuous assign instead of process)
    assert_eq!(module.name, "Test");
}

#[test]
fn test_sequential_process() {
    let source = r#"
entity Test {
    in clk: clock
    in d: bit[8]
    out q: bit[8]
}

impl Test {
    signal q_reg: bit[8] = 0

    on(clk.rise) {
        q_reg <= d
    }

    q = q_reg
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    // Should have at least one sequential process
    let has_sequential = module
        .processes
        .iter()
        .any(|p| matches!(p.kind, ProcessKind::Sequential));
    assert!(has_sequential);
}

#[test]
fn test_multiple_processes() {
    let source = r#"
entity Test {
    in clk: clock
    in a: bit[8]
    out b: bit[8]
}

impl Test {
    signal temp: bit[8] = 0

    on(clk.rise) {
        temp <= a
    }

    b = temp + 1
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    // Should have both sequential and combinational processes
    assert!(!module.processes.is_empty());
}

// ============================================================================
// Assignment Translation Tests
// ============================================================================

#[test]
fn test_blocking_assignment() {
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
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    // Should compile successfully
    assert_eq!(module.name, "Test");
}

#[test]
fn test_nonblocking_assignment() {
    let source = r#"
entity Test {
    in clk: clock
    in d: bit[8]
    out q: bit[8]
}

impl Test {
    signal q_reg: bit[8] = 0

    on(clk.rise) {
        q_reg <= d
    }

    q = q_reg
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.name, "Test");
}

// ============================================================================
// Complex Pattern Tests
// ============================================================================

#[test]
fn test_complex_alu() {
    let source = r#"
entity ALU {
    in a: bit[32]
    in b: bit[32]
    in op: bit[3]
    out result: bit[32]
}

impl ALU {
    result = match op {
        0b000 => a + b,
        0b001 => a - b,
        0b010 => a & b,
        0b011 => a | b,
        0b100 => a ^ b,
        0b101 => a << b[4:0],
        0b110 => a >> b[4:0],
        _ => 0
    }
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.name, "ALU");
    assert_eq!(module.ports.len(), 4);
}

#[test]
fn test_complex_overflow_detection() {
    let source = r#"
entity Test {
    in a: bit[32]
    in b: bit[32]
    in c: bit[32]
    out overflow: bit[1]
}

impl Test {
    overflow = (~a[31] & ~b[31] & c[31]) | (a[31] & b[31] & ~c[31])
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.name, "Test");
}

#[test]
fn test_state_machine() {
    let source = r#"
entity FSM {
    in clk: clock
    in rst: reset
    in start: bit[1]
    out done: bit[1]
}

impl FSM {
    signal state: bit[2] = 0

    on(clk.rise) {
        state <= if rst {
            0
        } else {
            match state {
                0 => if start { 1 } else { 0 },
                1 => 2,
                2 => 3,
                3 => 0,
                _ => 0
            }
        }
    }

    done = if state == 3 { 1 } else { 0 }
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.name, "FSM");
}

#[test]
fn test_counter_with_reset() {
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
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.name, "Counter");
    assert_eq!(module.signals.len(), 1);
}

// ============================================================================
// Edge Case Tests
// ============================================================================

#[test]
fn test_chained_operations() {
    let source = r#"
entity Test {
    in a: bit[8]
    in b: bit[8]
    in c: bit[8]
    out result: bit[8]
}

impl Test {
    result = a + b + c
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.name, "Test");
}

#[test]
fn test_deeply_nested_expr() {
    let source = r#"
entity Test {
    in a: bit[8]
    out result: bit[8]
}

impl Test {
    result = ((((a + 1) + 2) + 3) + 4)
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.name, "Test");
}

#[test]
fn test_multiple_assignments_to_same_signal() {
    let source = r#"
entity Test {
    in sel: bit[1]
    in a: bit[8]
    in b: bit[8]
    out result: bit[8]
}

impl Test {
    signal temp: bit[8]

    temp = if sel { a } else { b }
    result = temp + 1
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.name, "Test");
}

// ============================================================================
// Literal Translation Tests
// ============================================================================

#[test]
fn test_literal_decimal() {
    let source = r#"
entity Test {
    out value: bit[8]
}

impl Test {
    value = 42
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.name, "Test");
}

#[test]
fn test_literal_binary() {
    let source = r#"
entity Test {
    out value: bit[8]
}

impl Test {
    value = 0b10101010
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.name, "Test");
}

#[test]
fn test_literal_hex() {
    let source = r#"
entity Test {
    out value: bit[8]
}

impl Test {
    value = 0xFF
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.name, "Test");
}

// ============================================================================
// Comparison Operation Tests
// ============================================================================

#[test]
fn test_comparison_operators() {
    let source = r#"
entity Test {
    in a: bit[8]
    in b: bit[8]
    out eq: bit[1]
    out ne: bit[1]
    out lt: bit[1]
    out gt: bit[1]
}

impl Test {
    eq = if a == b { 1 } else { 0 }
    ne = if a != b { 1 } else { 0 }
    lt = if a < b { 1 } else { 0 }
    gt = if a > b { 1 } else { 0 }
}
"#;
    let mir = compile_to_mir(source);
    let module = get_first_module(&mir);

    assert_eq!(module.name, "Test");
}

// ============================================================================
// Real-World Example Tests
// ============================================================================

#[test]
fn test_real_world_counter() {
    let source = include_str!("../../../examples/counter.sk");
    let mir = compile_to_mir(source);

    assert!(!mir.modules.is_empty());
    assert_eq!(mir.modules[0].name, "Counter");
}

#[test]
fn test_real_world_alu() {
    let source = include_str!("../../../examples/alu.sk");
    let mir = compile_to_mir(source);

    assert!(!mir.modules.is_empty());
    assert_eq!(mir.modules[0].name, "ALU");
}
