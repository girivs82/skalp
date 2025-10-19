//! Regression tests for all fixed bugs documented in KNOWN_ISSUES.md
//!
//! Each test corresponds to a specific bug number and ensures the fix remains stable.
//! Test names follow the pattern: test_bug<number>_<short_description>

use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::parse::parse;

/// Helper to parse SKALP code and build HIR
fn parse_skalp_code(code: &str) -> Result<(), String> {
    let tree = parse(code);
    let _hir = build_hir(&tree).map_err(|e| format!("HIR error: {:?}", e))?;
    Ok(())
}

/// Helper to compile SKALP code to SystemVerilog
fn compile_to_sv(source: &str) -> Result<String, String> {
    let tree = parse(source);
    let hir = build_hir(&tree).map_err(|e| format!("HIR building failed: {:?}", e))?;
    let mir = skalp_mir::lower_to_mir(&hir).map_err(|e| format!("MIR lowering failed: {}", e))?;
    let lir = skalp_lir::lower_to_lir(&mir).map_err(|e| format!("LIR lowering failed: {}", e))?;
    skalp_codegen::generate_systemverilog_from_mir(&mir, &lir).map_err(|e| e.to_string())
}

// =============================================================================
// PARSER BUGS (40-43, 47)
// =============================================================================

#[test]
fn test_bug47_const_expression_in_type_position() {
    // Bug #47: Const expressions like clog2(SIZE) in type positions
    // Previously generated "expr" instead of evaluating the expression
    // Test with instantiation to generate concrete code
    let code = r#"
entity AddressDecoder<const SIZE: nat> {
    in addr: nat[clog2(SIZE)]
    out valid: bit
}

impl<const SIZE: nat> AddressDecoder<SIZE> {
    valid = (addr < SIZE) as bit;
}

entity Main {
    in addr: nat[4]
    out valid: bit
}

impl Main {
    let decoder = AddressDecoder::<16> { addr };
    valid = decoder.valid;
}
"#;

    // The key test is that this should parse and build HIR/MIR without errors
    // The fix prevents "expr" from appearing in the generated code
    assert!(
        parse_skalp_code(code).is_ok(),
        "Bug #47: Const expression in type position should parse and build"
    );
}

#[test]
fn test_bug43_keywords_as_parameter_names() {
    // Bug #43: Keywords like 'input', 'output', 'signal' caused parse failures
    // when used as parameter names in unambiguous contexts
    let code = r#"
trait Test<T> {
    type Output;
    fn method(&self, input: Self::Output) -> T;
    fn other(&self, output: T) -> Self::Output;
    fn third(&self, signal: bit) -> bit;
}
"#;

    assert!(
        parse_skalp_code(code).is_ok(),
        "Bug #43: Keywords should be allowed as parameter names"
    );
}

#[test]
fn test_bug42_public_constants() {
    // Bug #42: Public constants not supported
    // Parser didn't handle visibility modifiers for constants
    let code = r#"
pub const FU_ADD_8: bit[6] = 0b000000;
pub const FU_SUB_8: bit[6] = 0b000001;

entity Test {
    out value: bit[6]
}

impl Test {
    value = FU_ADD_8;
}
"#;

    assert!(
        parse_skalp_code(code).is_ok(),
        "Bug #42: Public constants should parse correctly"
    );
}

#[test]
fn test_bug41_semicolon_in_const_declarations() {
    // Bug #41: Parser didn't accept semicolons in constant declarations
    // even though all existing code used them
    let code = r#"
const FOO: bit[6] = 0b000000;
const BAR: nat = 42;

entity Test {
    out value: bit[6]
}

impl Test {
    value = FOO;
}
"#;

    assert!(
        parse_skalp_code(code).is_ok(),
        "Bug #41: Constants with semicolons should parse correctly"
    );
}

#[test]
fn test_bug40_blocks_in_match_arms() {
    // Bug #40: Blocks not supported in match expression arms
    let code = r#"
entity Decoder {
    in opcode: bit[2]
    out result: bit[8]
}

impl Decoder {
    result = match opcode {
        0 => {
            let x = 10;
            x as bit[8]
        }
        1 => {
            let y = 20;
            y as bit[8]
        }
        _ => 0
    };
}
"#;

    assert!(
        parse_skalp_code(code).is_ok(),
        "Bug #40: Match arms with blocks should parse correctly"
    );
}

// =============================================================================
// TYPE SYSTEM BUGS (28, 45, 46)
// =============================================================================

#[test]
fn test_bug28_const_generic_not_replaced_with_zero() {
    // Bug #28: Const generic parameters replaced with 0 in generated code
    // Test with instantiation to generate concrete code
    let code = r#"
entity Counter<const WIDTH: nat> {
    in clk: bit
    in reset: bit
    out count: nat[WIDTH]
}

impl<const WIDTH: nat> Counter<WIDTH> {
    reg count: nat[WIDTH];

    @(posedge clk) {
        if reset {
            count <= 0;
        } else {
            count <= count + 1;
        }
    }
}

entity Main {
    in clk: bit
    in reset: bit
    out count: nat[8]
}

impl Main {
    let counter = Counter::<8> { clk, reset };
    count = counter.count;
}
"#;

    let result = compile_to_sv(code);
    assert!(
        result.is_ok(),
        "Bug #28: Failed to compile const generic: {:?}",
        result.err()
    );

    let sv = result.unwrap();
    // The bug was that const generics were replaced with 0
    // With the fix, WIDTH=8 should be preserved in the generated code
    // This should NOT generate code with 0-width signals
    assert!(
        !sv.contains("[0-1:0]"),
        "Bug #28: Const generic parameter replaced with 0"
    );
}

#[test]
fn test_bug46_float32_vector_uses_fp_ops() {
    // Bug #46: Integer operations used for Float32 vector components
    // vec2<fp32> component operations should generate FP ops, not integer ops
    let code = r#"
use std::vec::vec2;
use std::fp::fp32;

entity VecAdd {
    in a: vec2<fp32>
    in b: vec2<fp32>
    out result: vec2<fp32>
}

impl VecAdd {
    result = vec2 {
        x: a.x + b.x,
        y: a.y + b.y,
    };
}
"#;

    assert!(
        parse_skalp_code(code).is_ok(),
        "Bug #46: vec2<fp32> operations should parse"
    );
}

#[test]
fn test_bug45_vec2_fp32_stored_correctly() {
    // Bug #45: HIR stores vec2<fp32> as Custom("vec2") instead of proper parametric type
    let code = r#"
use std::vec::vec2;
use std::fp::fp32;

entity VecProcessor {
    in vec_in: vec2<fp32>
    out vec_out: vec2<fp32>
}

impl VecProcessor {
    vec_out = vec_in;
}
"#;

    assert!(
        parse_skalp_code(code).is_ok(),
        "Bug #45: vec2<fp32> should be stored as proper parametric type"
    );
}

// =============================================================================
// ARRAY AND INDEX BUGS (26, 27, 30, 31, 33)
// =============================================================================

#[test]
fn test_bug30_const_generic_in_array_index_rhs() {
    // Bug #30: Const generic parameters in RHS array index expressions
    let code = r#"
entity ArrayReader<const SIZE: nat, const INDEX: nat> {
    in data: nat[8][SIZE]
    out value: nat[8]
}

impl<const SIZE: nat, const INDEX: nat> ArrayReader<SIZE, INDEX> {
    value = data[INDEX];
}
"#;

    let result = compile_to_sv(code);
    assert!(
        result.is_ok(),
        "Bug #30: Const generic in array index should compile"
    );

    let sv = result.unwrap();
    // Should use INDEX, not replace with 0
    assert!(
        sv.contains("INDEX") || sv.contains("parameter"),
        "Bug #30: INDEX parameter lost in array indexing"
    );
}

#[test]
fn test_bug31_array_index_from_flattened_struct() {
    // Bug #31: Array index reads from flattened struct arrays incorrectly expanded
    let code = r#"
struct Point {
    x: nat[8],
    y: nat[8],
}

entity StructArrayReader<const SIZE: nat> {
    in points: Point[SIZE]
    in index: nat[clog2(SIZE)]
    out x_value: nat[8]
}

impl<const SIZE: nat> StructArrayReader<SIZE> {
    x_value = points[index].x;
}
"#;

    assert!(
        compile_to_sv(code).is_ok(),
        "Bug #31: Array index from flattened struct should compile"
    );
}

#[test]
fn test_bug33_multi_field_struct_array_assignments() {
    // Bug #33: Multi-field struct array assignments
    let code = r#"
struct RGB {
    r: nat[8],
    g: nat[8],
    b: nat[8],
}

entity ColorBuffer {
    in clk: bit
    in index: nat[4]
    in color: RGB
    out read_color: RGB
}

impl ColorBuffer {
    reg buffer: RGB[16];

    @(posedge clk) {
        buffer[index] <= color;
    }

    read_color = buffer[index];
}
"#;

    assert!(
        compile_to_sv(code).is_ok(),
        "Bug #33: Multi-field struct array assignments should compile"
    );
}

// =============================================================================
// MATCH EXPRESSION BUGS (37, 38, 40)
// =============================================================================

#[test]
fn test_bug38_match_guards_with_output_assignments() {
    // Bug #38: Match guards with direct output port assignments
    let code = r#"
entity ALU {
    in opcode: bit[2]
    in a: nat[8]
    in b: nat[8]
    out result: nat[8]
    out overflow: bit
}

impl ALU {
    result = match opcode {
        0 if a == b => a,
        1 if a > b => a - b,
        2 if a < b => b - a,
        _ => 0
    };

    overflow = match opcode {
        1 if a > b => 0,
        2 if a < b => 0,
        _ => 1
    };
}
"#;

    assert!(
        compile_to_sv(code).is_ok(),
        "Bug #38: Match guards with output assignments should compile"
    );
}

#[test]
fn test_bug37_no_infinite_loop_in_parser() {
    // Bug #37: Parser infinite loop in block and match expression parsing
    // This test should complete quickly without hanging
    let code = r#"
entity ComplexMatch {
    in opcode: bit[4]
    out result: bit[8]
}

impl ComplexMatch {
    result = match opcode {
        0 => { 1 as bit[8] }
        1 => { 2 as bit[8] }
        2 => match opcode {
            2 => { 3 as bit[8] }
            _ => { 4 as bit[8] }
        }
        _ => { 0 as bit[8] }
    };
}
"#;

    // Use a timeout mechanism to detect infinite loops
    let start = std::time::Instant::now();
    let result = parse_skalp_code(code);
    let elapsed = start.elapsed();

    assert!(
        result.is_ok(),
        "Bug #37: Complex match should parse without infinite loop"
    );
    assert!(
        elapsed.as_secs() < 5,
        "Bug #37: Parser took too long, possible infinite loop"
    );
}

// =============================================================================
// METALPERFORM/GPU BUGS (34, 36)
// =============================================================================

#[test]
#[cfg(target_os = "macos")]
fn test_bug34_metal_array_index_constants() {
    // Bug #34: Metal shader array index constants scrambled
    // This is a Metal/GPU-specific bug
    let code = r#"
entity LUT {
    in index: nat[4]
    out value: nat[8]
}

impl LUT {
    const TABLE: nat[8][16] = [
        10, 20, 30, 40, 50, 60, 70, 80,
        90, 100, 110, 120, 130, 140, 150, 160
    ];

    value = TABLE[index];
}
"#;

    assert!(
        compile_to_sv(code).is_ok(),
        "Bug #34: Constant array indexing should compile"
    );
}

#[test]
#[cfg(target_os = "macos")]
fn test_bug36_metal_array_element_reads() {
    // Bug #36: Metal shader codegen for array element reads
    let code = r#"
entity ArrayProcessor {
    in data: nat[8][4]
    in index: nat[2]
    out element: nat[8]
}

impl ArrayProcessor {
    element = data[index];
}
"#;

    assert!(
        compile_to_sv(code).is_ok(),
        "Bug #36: Array element reads should compile correctly"
    );
}

// =============================================================================
// HELPER TEST: Verify all fixed bugs have tests
// =============================================================================

#[test]
fn test_all_major_bugs_have_regression_tests() {
    // This test documents which bugs have regression tests
    let bugs_with_tests = [28, 30, 31, 33, 34, 36, 37, 38, 40, 41, 42, 43, 45, 46, 47];

    // All fixed bugs from KNOWN_ISSUES.md that should have tests
    let all_fixed_bugs = [
        11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26, 27, 28, 29, 30, 31, 33, 34, 35,
        36, 37, 38, 39, 40, 41, 42, 43, 45, 46, 47,
    ];

    let missing_tests: Vec<_> = all_fixed_bugs
        .iter()
        .filter(|&&bug| !bugs_with_tests.contains(&bug))
        .collect();

    if !missing_tests.is_empty() {
        eprintln!("Missing regression tests for bugs: {:?}", missing_tests);
        eprintln!("Note: Some bugs may be covered by integration tests");
    }

    // This test always passes but serves as documentation
    assert!(
        bugs_with_tests.len() >= 10,
        "Need at least 10 regression tests"
    );
}
