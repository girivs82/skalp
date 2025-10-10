//! Comprehensive test suite for SKALP language features
//! Tests each feature defined in LANGUAGE_SPECIFICATION.md
//!
//! This ensures we know exactly what's implemented vs what's in the spec

use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::parse::parse;

/// Helper to test if source compiles
fn compiles(source: &str) -> bool {
    let tree = parse(source);
    build_hir(&tree).is_ok()
}

/// Helper to compile to SystemVerilog
fn compile_to_verilog(source: &str) -> Result<String, String> {
    let tree = parse(source);
    let hir = build_hir(&tree).map_err(|e| format!("HIR building failed: {:?}", e))?;
    let mir = skalp_mir::lower_to_mir(&hir).map_err(|e| format!("MIR lowering failed: {}", e))?;
    let lir = skalp_lir::lower_to_lir(&mir).map_err(|e| format!("LIR lowering failed: {}", e))?;
    skalp_codegen::generate_systemverilog_from_mir(&mir, &lir).map_err(|e| e.to_string())
}

// ============================================================================
// PRIMITIVE TYPES
// ============================================================================

#[test]
fn test_bool_type() {
    let source = r#"
entity TestBool {
    in enable: bool
    out result: bit
}

impl TestBool {
    result = enable as bit
}
"#;
    assert!(compiles(source), "bool type should compile");
}

#[test]
fn test_bit_type() {
    let source = r#"
entity TestBit {
    in a: bit
    in b: bit[8]
    out c: bit
}

impl TestBit {
    c = a
}
"#;
    assert!(compiles(source), "bit and bit[N] types should compile");
}

#[test]
fn test_nat_type() {
    let source = r#"
entity TestNat {
    in a: nat[8]
    out b: nat[16]
}

impl TestNat {
    b = a
}
"#;
    assert!(compiles(source), "nat[N] type should compile");
}

#[test]
fn test_int_type() {
    let source = r#"
entity TestInt {
    in a: int[8]
    out b: int[16]
}

impl TestInt {
    b = a
}
"#;
    assert!(compiles(source), "int[N] type should compile");
}

// ============================================================================
// COMPOSITE TYPES
// ============================================================================

#[test]
fn test_struct_type() {
    let source = r#"
struct Point {
    x: nat[8],
    y: nat[8]
}

entity TestStruct {
    in p: Point
    out x_out: nat[8]
}

impl TestStruct {
    x_out = p.x
}
"#;
    assert!(compiles(source), "struct type should compile");
}

#[test]
fn test_enum_type() {
    let source = r#"
enum State {
    IDLE = 0,
    ACTIVE = 1,
    DONE = 2
}

entity TestEnum {
    in clk: clock
    out busy: bit
}

impl TestEnum {
    signal state: State = State::IDLE

    busy = (state != State::IDLE) as bit
}
"#;
    assert!(compiles(source), "enum type should compile");
}

#[test]
fn test_array_type_signal() {
    let source = r#"
entity TestArray {
    in idx: nat[2]
    out result: nat[8]
}

impl TestArray {
    signal mem: nat[8][4]

    result = mem[idx]
}
"#;
    assert!(compiles(source), "array type in signal should compile");
}

#[test]
fn test_array_type_port() {
    let source = r#"
entity TestArrayPort {
    in data: nat[8][4]
    out first: nat[8]
}

impl TestArrayPort {
    first = data[0]
}
"#;
    assert!(compiles(source), "array type in port should compile");
}

// ============================================================================
// PORT DIRECTIONS
// ============================================================================

#[test]
fn test_input_port() {
    let source = r#"
entity TestInput {
    in data: nat[8]
    out result: nat[8]
}

impl TestInput {
    result = data
}
"#;
    assert!(compiles(source), "input port should compile");
}

#[test]
fn test_output_port() {
    let source = r#"
entity TestOutput {
    in data: nat[8]
    out result: nat[8]
}

impl TestOutput {
    result = data
}
"#;
    assert!(compiles(source), "output port should compile");
}

#[test]
fn test_inout_port() {
    let source = r#"
entity TestInout {
    inout data: bit
    out oe: bit
}

impl TestInout {
    oe = 1
    data = 0  // Drive when oe=1
}
"#;
    assert!(compiles(source), "inout port should compile");
}

// ============================================================================
// CONTROL FLOW
// ============================================================================

#[test]
fn test_if_else() {
    let source = r#"
entity TestIf {
    in clk: clock
    in cond: bit
    out result: nat[8]
}

impl TestIf {
    signal val: nat[8] = 0

    on(clk.rise) {
        if (cond == 1) {
            val <= 1
        } else {
            val <= 0
        }
    }

    result = val
}
"#;
    assert!(compiles(source), "if/else should compile");
}

#[test]
fn test_match_expression() {
    let source = r#"
enum Op {
    ADD = 0,
    SUB = 1,
    MUL = 2
}

entity TestMatch {
    in op: Op
    in a: nat[8]
    in b: nat[8]
    out result: nat[8]
}

impl TestMatch {
    result = match op {
        Op::ADD => a + b,
        Op::SUB => a - b,
        Op::MUL => a * b
    }
}
"#;
    assert!(compiles(source), "match expression should compile");
}

// ============================================================================
// GENERICS
// ============================================================================

#[test]
fn test_generic_entity_type() {
    let source = r#"
entity Register<T> {
    in clk: clock
    in data: T
    out q: T
}

impl<T> Register<T> {
    signal reg: T

    on(clk.rise) {
        reg <= data
    }

    q = reg
}
"#;
    assert!(
        compiles(source),
        "generic entity with type parameter should compile"
    );
}

#[test]
fn test_generic_entity_const() {
    let source = r#"
entity Counter<const WIDTH: nat> {
    in clk: clock
    out count: nat[WIDTH]
}

impl<const WIDTH: nat> Counter<WIDTH> {
    signal counter: nat[WIDTH] = 0

    on(clk.rise) {
        counter <= counter + 1
    }

    count = counter
}
"#;
    assert!(
        compiles(source),
        "generic entity with const parameter should compile"
    );
}

#[test]
#[ignore] // clog2 in type position not implemented yet
fn test_const_expression_in_type() {
    let source = r#"
entity AddressDecoder<const SIZE: nat> {
    in addr: nat[clog2(SIZE)]
    out valid: bit
}

impl<const SIZE: nat> AddressDecoder<SIZE> {
    valid = (addr < SIZE) as bit
}
"#;
    assert!(compiles(source), "const expression in type should compile");
}

// ============================================================================
// OPERATORS
// ============================================================================

#[test]
fn test_comparison_operators() {
    let source = r#"
entity TestComparison {
    in a: nat[8]
    in b: nat[8]
    out eq: bit
    out ne: bit
    out lt: bit
    out le: bit
    out gt: bit
    out ge: bit
}

impl TestComparison {
    eq = (a == b) as bit
    ne = (a != b) as bit
    lt = (a < b) as bit
    le = (a <= b) as bit
    gt = (a > b) as bit
    ge = (a >= b) as bit
}
"#;
    let result = compile_to_verilog(source);
    assert!(result.is_ok(), "comparison operators should compile");

    let verilog = result.unwrap();
    assert!(verilog.contains("=="), "should contain == operator");
    assert!(verilog.contains("!="), "should contain != operator");
    assert!(verilog.contains("<"), "should contain < operator");
}

#[test]
fn test_arithmetic_operators() {
    let source = r#"
entity TestArithmetic {
    in a: nat[8]
    in b: nat[8]
    out sum: nat[8]
    out diff: nat[8]
    out prod: nat[8]
    out quot: nat[8]
    out rem: nat[8]
}

impl TestArithmetic {
    sum = a + b
    diff = a - b
    prod = a * b
    quot = a / b
    rem = a % b
}
"#;
    assert!(compiles(source), "arithmetic operators should compile");
}

#[test]
fn test_bitwise_operators() {
    let source = r#"
entity TestBitwise {
    in a: bit[8]
    in b: bit[8]
    out and_result: bit[8]
    out or_result: bit[8]
    out xor_result: bit[8]
    out not_result: bit[8]
}

impl TestBitwise {
    and_result = a & b
    or_result = a | b
    xor_result = a ^ b
    not_result = !a
}
"#;
    assert!(compiles(source), "bitwise operators should compile");
}

// ============================================================================
// SPECIAL FEATURES
// ============================================================================

#[test]
fn test_clock_domain() {
    let source = r#"
entity TestClockDomain {
    in clk: clock
    in data: nat[8]
    out q: nat[8]
}

impl TestClockDomain {
    signal reg: nat[8] = 0

    on(clk.rise) {
        reg <= data
    }

    q = reg
}
"#;
    assert!(compiles(source), "clock domains should compile");
}

#[test]
fn test_reset_signal() {
    let source = r#"
entity TestReset {
    in clk: clock
    in rst: reset
    out count: nat[8]
}

impl TestReset {
    signal counter: nat[8] = 0

    on(clk.rise) {
        if (rst) {
            counter <= 0
        } else {
            counter <= counter + 1
        }
    }

    count = counter
}
"#;
    assert!(compiles(source), "reset signals should compile");
}

// ============================================================================
// SUMMARY TEST
// ============================================================================

#[test]
fn test_feature_summary() {
    println!("\n=== SKALP Language Feature Implementation Status ===\n");
    println!("Run with `cargo test test_language_features -- --include-ignored --show-output`");
    println!("to see which features are implemented vs specified.");
}
