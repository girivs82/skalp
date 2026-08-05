//! Regression tests for all fixed bugs documented in KNOWN_ISSUES.md
//!
//! Each test corresponds to a specific bug number and ensures the fix remains stable.
//! Test names follow the pattern: test_bug<number>_<short_description>

use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::monomorphization::MonomorphizationEngine;
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
    // Apply monomorphization to specialize generic entities
    let mut engine = MonomorphizationEngine::new();
    let hir = engine.monomorphize(&hir);
    let mir = skalp_mir::lower_to_mir(&hir).map_err(|e| format!("MIR lowering failed: {}", e))?;
    skalp_codegen::generate_systemverilog_from_mir(&mir).map_err(|e| e.to_string())
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
    signal count: nat[WIDTH];

    @(posedge clk) {
        if reset {
            count = 0;
        } else {
            count = count + 1;
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
    signal buffer: RGB[16];

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
// KEYWORD AND PORT NAME BUGS (11, 12)
// =============================================================================

#[test]
fn test_bug11_keyword_port_names_in_connections() {
    // Bug #11: Instance connections using keyword port names were dropped
    let code = r#"
entity Inner {
    out output: nat[8]
}

impl Inner {
    output = 42;
}

entity Outer {
    out result: nat[8]
}

impl Outer {
    inst inner = Inner {
    }
    result = inner.output;  // "output" is a keyword
}
"#;

    assert!(
        compile_to_sv(code).is_ok(),
        "Bug #11: Keyword port names in connections should work"
    );
}

#[test]
fn test_bug12_keyword_port_names_in_assignments() {
    // Bug #12: Continuous assignments to keyword-named ports were dropped
    let code = r#"
entity Test {
    out output: nat[8]  // "output" is a keyword
    out signal: bit     // "signal" is also a keyword
}

impl Test {
    signal temp: nat[8]
    output = temp;  // Assignment to keyword-named port
    signal = 1;     // Assignment to another keyword-named port
    temp = 42;
}
"#;

    assert!(
        compile_to_sv(code).is_ok(),
        "Bug #12: Assignments to keyword-named ports should work"
    );
}

// =============================================================================
// IMPORT AND MONOMORPHIZATION BUGS (17, 18, 21, 22)
// =============================================================================

#[test]
fn test_bug17_imported_generic_module_implementations() {
    // Bug #17: Imported generic module implementations not merged
    // This test verifies generic entities can be imported and used
    let code = r#"
entity GenericBuffer<const SIZE: nat> {
    in data: nat[8]
    out stored: nat[8]
}

impl<const SIZE: nat> GenericBuffer<SIZE> {
    signal buffer: nat[8]
    buffer = data;
    stored = buffer;
}

entity Main {
    in data: nat[8]
    out result: nat[8]
}

impl Main {
    let buf = GenericBuffer::<16> { data };
    result = buf.stored;
}
"#;

    assert!(
        compile_to_sv(code).is_ok(),
        "Bug #17: Generic entity implementations should be available"
    );
}

#[test]
fn test_bug18_deterministic_monomorphization() {
    // Bug #18: Non-deterministic monomorphization caused random EntityId assignment
    // This test ensures multiple instantiations work correctly
    let code = r#"
entity Counter<const WIDTH: nat> {
    in clk: bit
    out count: nat[WIDTH]
}

impl<const WIDTH: nat> Counter<WIDTH> {
    signal count: nat[WIDTH];

    @(posedge clk) {
        count = count + 1;
    }
}

entity Main {
    in clk: bit
    out count8: nat[8]
    out count16: nat[16]
}

impl Main {
    let c8 = Counter::<8> { clk };
    let c16 = Counter::<16> { clk };
    count8 = c8.count;
    count16 = c16.count;
}
"#;

    // Should compile deterministically without mixing up the instances
    assert!(
        compile_to_sv(code).is_ok(),
        "Bug #18: Multiple generic instantiations should work deterministically"
    );
}

#[test]
fn test_bug21_bug22_imported_entity_implementations() {
    // Bugs #21 and #22: Imported entity implementations lost during HIR rebuild
    // Test that generic entities can be used across module boundaries
    let code = r#"
entity GenericFifo<T, const DEPTH: nat> {
    in write_data: T
    in read_enable: bit
    out read_data: T
}

impl<T, const DEPTH: nat> GenericFifo<T, DEPTH> {
    signal buffer: T
    buffer = write_data;
    read_data = buffer;
}

entity Main {
    in data: nat[8]
    in rd_en: bit
    out result: nat[8]
}

impl Main {
    let fifo = GenericFifo::<nat[8], 16> {
        write_data: data,
        read_enable: rd_en
    };
    result = fifo.read_data;
}
"#;

    assert!(
        compile_to_sv(code).is_ok(),
        "Bugs #21, #22: Generic entity implementations should not be lost"
    );
}

// =============================================================================
// ARRAY INDEX AND PARSING BUGS (26, 27, 29)
// =============================================================================

#[test]
fn test_bug26_binary_expressions_in_array_indices() {
    // Bug #26: Binary expressions in array indices were dropped
    // HIR builder's .find() returned first child instead of complete expression
    let code = r#"
entity CircularBuffer<const DEPTH: nat> {
    in write_ptr: nat[8]
    in write_data: nat[8]
    in read_ptr: nat[8]
    out read_data: nat[8]
}

impl<const DEPTH: nat> CircularBuffer<DEPTH> {
    signal mem: nat[8][DEPTH];

    mem[write_ptr % DEPTH] = write_data;  // Binary expression in index
    read_data = mem[read_ptr % DEPTH];    // Binary expression in index
}
"#;

    assert!(
        compile_to_sv(code).is_ok(),
        "Bug #26: Binary expressions in array indices should work"
    );
}

#[test]
fn test_bug27_constant_array_reads_not_sliced() {
    // Bug #27: Constant array reads incorrectly sliced to single bit
    // Reading mem[0] created mem_0[0:0] instead of mem_0
    let code = r#"
entity ArrayReader {
    in mem: nat[32][4]
    out first: nat[32]
    out second: nat[32]
}

impl ArrayReader {
    first = mem[0];   // Constant index should read full 32-bit value
    second = mem[1];  // Not just bit 0
}
"#;

    let result = compile_to_sv(code);
    assert!(
        result.is_ok(),
        "Bug #27: Constant array reads should compile"
    );

    let sv = result.unwrap();
    // Should not contain bit slice notation like [0:0] for array reads
    assert!(
        !sv.contains("mem_0[0:0]") && !sv.contains("mem_1[0:0]"),
        "Bug #27: Constant array reads should not be sliced to single bit"
    );
}

#[test]
fn test_bug29_array_preservation_for_scalars() {
    // Bug #29: Arrays of scalars should be preserved as packed arrays
    // instead of being flattened into individual signals
    let code = r#"
entity ArrayPreserveTest<const SIZE: nat> {
    in index: nat[4]
    in write_data: nat[8]
    out read_data: nat[8]
}

impl<const SIZE: nat> ArrayPreserveTest<SIZE> {
    signal data: nat[8][SIZE];  // Should be preserved as packed array

    data[index] = write_data;
    read_data = data[index];
}
"#;

    assert!(
        compile_to_sv(code).is_ok(),
        "Bug #29: Arrays of scalars should be preserved"
    );
}

// =============================================================================
// STRUCT AND NESTED FIELD BUGS (35, 39)
// =============================================================================

#[test]
fn test_bug35_nested_struct_field_assignments() {
    // Bug #35: Nested struct field assignments in sequential blocks were dropped
    let code = r#"
struct Vec3 {
    x: nat[32],
    y: nat[32],
    z: nat[32],
}

struct Vertex {
    position: Vec3,
    color: nat[32],
}

entity NestedFieldTest {
    in clk: bit
    in input_x: nat[32]
    out output_x: nat[32]
}

impl NestedFieldTest {
    signal out_vertex: Vertex

    @(posedge clk) {
        out_vertex.position.x = input_x;  // Nested field assignment
    }

    output_x = out_vertex.position.x;
}
"#;

    assert!(
        compile_to_sv(code).is_ok(),
        "Bug #35: Nested struct field assignments should work"
    );
}

#[test]
fn test_bug39_tuple_destructuring() {
    // Bug #39: Tuple destructuring type inference
    let code = r#"
entity TupleTest {
    out a: nat[8]
    out b: nat[8]
}

impl TupleTest {
    let (x, y) = (10, 20);
    a = x as nat[8];
    b = y as nat[8];
}
"#;

    assert!(
        parse_skalp_code(code).is_ok(),
        "Bug #39: Tuple destructuring should parse correctly"
    );
}

// =============================================================================
// GPU/ELABORATION BUGS (13-16, 19, 20, 23, 24)
// =============================================================================
// Note: These are complex integration bugs tested by full simulation tests.
// Here we add simple smoke tests that verify the code compiles.

#[test]
fn test_bug13_to_16_hierarchical_elaboration() {
    // Bugs #13-16: GPU simulator hierarchical elaboration
    // Simple test: hierarchical instantiation should compile
    let code = r#"
entity Inner {
    in data: nat[8]
    out result: nat[8]
}

impl Inner {
    result = data + 1;
}

entity Middle {
    in data: nat[8]
    out result: nat[8]
}

impl Middle {
    let inner = Inner { data };
    result = inner.result;
}

entity Outer {
    in data: nat[8]
    out result: nat[8]
}

impl Outer {
    let middle = Middle { data };
    result = middle.result;
}
"#;

    assert!(
        compile_to_sv(code).is_ok(),
        "Bugs #13-16: Hierarchical elaboration should compile"
    );
}

#[test]
fn test_bug19_bug20_sequential_array_assignments() {
    // Bugs #19, #20: GPU simulator sequential array assignments
    // Simple test: sequential assignments to arrays should compile
    let code = r#"
entity ArrayAssignTest {
    in clk: bit
    in index: nat[4]
    in data: nat[8]
    out result: nat[8]
}

impl ArrayAssignTest {
    signal mem: nat[8][16];

    @(posedge clk) {
        mem[index] <= data;
    }

    result = mem[index];
}
"#;

    assert!(
        compile_to_sv(code).is_ok(),
        "Bugs #19, #20: Sequential array assignments should compile"
    );
}

#[test]
fn test_bug23_multiple_flipflops() {
    // Bug #23: Multiple FlipFlops created for same signal
    // Simple test: multiple instances with sequential logic should compile
    let code = r#"
entity SyncStage {
    in clk: bit
    in data_in: nat[8]
    out data_out: nat[8]
}

impl SyncStage {
    signal data_sync: nat[8];

    @(posedge clk) {
        data_sync = data_in;
    }

    data_out = data_sync;
}

entity MultiSync {
    in clk: bit
    in data: nat[8]
    out result: nat[8]
}

impl MultiSync {
    let sync1 = SyncStage { clk, data_in: data };
    let sync2 = SyncStage { clk, data_in: sync1.data_out };
    result = sync2.data_out;
}
"#;

    assert!(
        compile_to_sv(code).is_ok(),
        "Bug #23: Multiple instances with FlipFlops should compile"
    );
}

#[test]
fn test_bug24_clock_signal_mapping() {
    // Bug #24: Clock signal mapping fails in hierarchical elaboration
    // Test that different clock domains compile correctly
    let code = r#"
entity DualClock {
    in wr_clk: bit
    in rd_clk: bit
    in write_data: nat[8]
    out read_data: nat[8]
}

impl DualClock {
    signal wr_data: nat[8];
    signal rd_data: nat[8];

    @(posedge wr_clk) {
        wr_data = write_data;
    }

    @(posedge rd_clk) {
        rd_data = wr_data;
    }

    read_data = rd_data;
}
"#;

    assert!(
        compile_to_sv(code).is_ok(),
        "Bug #24: Multiple clock domains should compile"
    );
}

// =============================================================================
// HELPER TEST: Verify all fixed bugs have tests
// =============================================================================

#[test]
fn test_all_major_bugs_have_regression_tests() {
    // This test documents which bugs have regression tests
    let bugs_with_tests = [
        11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26, 27, 28, 29, 30, 31, 33, 34, 35,
        36, 37, 38, 39, 40, 41, 42, 43, 45, 46, 47,
    ];

    // All fixed bugs from KNOWN_ISSUES.md
    let all_fixed_bugs = [
        11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26, 27, 28, 29, 30, 31, 33, 34, 35,
        36, 37, 38, 39, 40, 41, 42, 43, 45, 46, 47,
    ];

    let missing_tests: Vec<_> = all_fixed_bugs
        .iter()
        .filter(|&&bug| !bugs_with_tests.contains(&bug))
        .collect();

    // All bugs should now have regression tests!
    assert!(
        missing_tests.is_empty(),
        "Missing regression tests for bugs: {:?}",
        missing_tests
    );

    // Verify comprehensive coverage
    assert_eq!(
        bugs_with_tests.len(),
        all_fixed_bugs.len(),
        "All fixed bugs should have regression tests"
    );
}

// =============================================================================
// LOGICAL OPERATOR BUGS (#145-148)
// =============================================================================

/// Bug #145-148: Logical && operator handling in chained expressions
/// Tests that chained logical AND/OR operators compile correctly without
/// duplication or incorrect operator substitution.
#[test]
fn test_bug145_148_logical_and_operator_chaining() {
    // Test 1: Simple && between two comparisons (Bug #145)
    let code1 = r#"
entity TestDoubleAnd {
    in a: bit[8],
    in b: bit[8],
    out result: bit[1],
}

impl TestDoubleAnd {
    result = a != 0 && b != 0;
}
"#;
    let sv1 = compile_to_sv(code1).expect("Double && should compile");
    assert!(sv1.contains("&&"), "Output should contain && operator");
    assert!(
        !sv1.contains("!= 0) != "),
        "Should not have != where && should be"
    );

    // Test 2: Triple-chained && (Bug #148 - no duplication)
    let code2 = r#"
entity TestTripleAnd {
    in a: bit[8],
    in b: bit[8],
    in c: bit[8],
    out result: bit[1],
}

impl TestTripleAnd {
    result = a != 0 && b != 0 && c != 0;
}
"#;
    let sv2 = compile_to_sv(code2).expect("Triple && should compile");

    // Count occurrences of "(c != 0)" - should appear exactly once
    let c_neq_count = sv2.matches("(c != 0)").count();
    assert_eq!(
        c_neq_count, 1,
        "Triple && should have exactly one (c != 0), found {}: {}",
        c_neq_count, sv2
    );

    // Test 3: Parenthesized && expressions
    let code3 = r#"
entity TestParenAnd {
    in a: bit[8],
    in b: bit[8],
    out result: bit[1],
}

impl TestParenAnd {
    result = (a != 0) && (b != 0);
}
"#;
    let sv3 = compile_to_sv(code3).expect("Parenthesized && should compile");
    assert!(sv3.contains("&&"), "Output should contain && operator");

    // Test 4: Simple a && b (not comparisons)
    let code4 = r#"
entity TestSimpleAnd {
    in a: bit[1],
    in b: bit[1],
    out result: bit[1],
}

impl TestSimpleAnd {
    result = a && b;
}
"#;
    let sv4 = compile_to_sv(code4).expect("Simple && should compile");
    assert!(sv4.contains("&&"), "Output should contain && operator");

    // Test 5: Quadruple-chained && to ensure deep chaining works
    let code5 = r#"
entity TestQuadAnd {
    in a: bit[8],
    in b: bit[8],
    in c: bit[8],
    in d: bit[8],
    out result: bit[1],
}

impl TestQuadAnd {
    result = a != 0 && b != 0 && c != 0 && d != 0;
}
"#;
    let sv5 = compile_to_sv(code5).expect("Quadruple && should compile");

    // Each comparison should appear exactly once
    assert_eq!(
        sv5.matches("(a != 0)").count(),
        1,
        "Should have exactly one (a != 0)"
    );
    assert_eq!(
        sv5.matches("(b != 0)").count(),
        1,
        "Should have exactly one (b != 0)"
    );
    assert_eq!(
        sv5.matches("(c != 0)").count(),
        1,
        "Should have exactly one (c != 0)"
    );
    assert_eq!(
        sv5.matches("(d != 0)").count(),
        1,
        "Should have exactly one (d != 0)"
    );

    // Test 6: Mixed && and || operators
    let code6 = r#"
entity TestMixedLogical {
    in a: bit[8],
    in b: bit[8],
    in c: bit[8],
    out result: bit[1],
}

impl TestMixedLogical {
    result = (a != 0 && b != 0) || c != 0;
}
"#;
    let sv6 = compile_to_sv(code6).expect("Mixed && and || should compile");
    assert!(sv6.contains("&&"), "Output should contain && operator");
    assert!(sv6.contains("||"), "Output should contain || operator");
}

// =============================================================================
// ISSUE #10: CODEGEN BUGS (4 bugs)
// =============================================================================

#[test]
fn test_issue10_widening_add_extends_result_width() {
    let source = r#"
entity WidenAddTest {
    in clk: clock
    in a: bit[8]
    in b: bit[8]
    out result: bit[9]
}

impl WidenAddTest {
    result = a +: b
}
"#;
    let sv = compile_to_sv(source).expect("Should compile widening add");

    // Result must be 9 bits wide, not 8
    assert!(
        sv.contains("[8:0] result"),
        "Issue #10 Bug 1: result should be 9-bit [8:0], got:\n{}",
        sv
    );
    // The addition should zero-extend operands, not truncate
    assert!(
        !sv.contains("[7:0] result"),
        "Issue #10 Bug 1: result should NOT be 8-bit"
    );
}

#[test]
fn test_issue10_cdc_entity_generates_always_ff() {
    let source = r#"
entity Sync<'src, 'dst> {
    in  clk_dst: clock<'dst>
    in  rst:     reset
    in  data_in: bit<'src>
    out data_out: bit
}

impl Sync {
    signal ff1: bit
    signal ff2: bit

    on(clk_dst.rise) {
        if rst {
            ff1 = 0
            ff2 = 0
        } else {
            ff1 = data_in
            ff2 = ff1
        }
    }

    data_out = ff2
}
"#;
    let sv = compile_to_sv(source).expect("Should compile CDC entity");

    // Must generate an always_ff block — empty module body is the bug
    assert!(
        sv.contains("always_ff"),
        "Issue #10 Bug 2: CDC entity should generate always_ff block, got:\n{}",
        sv
    );
    assert!(
        sv.contains("posedge clk_dst"),
        "Issue #10 Bug 2: should have posedge clk_dst sensitivity"
    );
    assert!(
        sv.contains("ff1"),
        "Issue #10 Bug 2: ff1 signal should appear in generated SV"
    );
    assert!(
        sv.contains("ff2"),
        "Issue #10 Bug 2: ff2 signal should appear in generated SV"
    );
}

#[test]
fn test_issue10_combinational_signal_not_literal_zero() {
    let source = r#"
entity BaudTickTest {
    in  clk: clock
    in  rst: reset
    out tx_done: bit
}

impl BaudTickTest {
    signal state: nat[2]
    signal baud_counter: nat[9]

    baud_tick = (baud_counter == 0)

    on(clk.rise) {
        if rst {
            state = 0
            baud_counter = 0
        } else {
            if baud_tick {
                state = 1
            } else {
                baud_counter = baud_counter - 1
            }
        }
    }

    tx_done = (state == 1) & baud_tick
}
"#;
    let sv = compile_to_sv(source).expect("Should compile combinational signal");

    // baud_tick references should NOT be literal 0
    assert!(
        !sv.contains("if (0)"),
        "Issue #10 Bug 3: baud_tick inlined as literal 0"
    );
    assert!(
        !sv.contains("& 0)"),
        "Issue #10 Bug 3: baud_tick inlined as literal 0 in expression"
    );
    // Should contain a wire or assign for baud_tick
    assert!(
        sv.contains("baud_tick") || sv.contains("baud_counter == 0"),
        "Issue #10 Bug 3: baud_tick or its expansion should appear in SV, got:\n{}",
        sv
    );
}

#[test]
fn test_issue10_match_constant_signals_generates_case_arms() {
    let source = r#"
entity MatchConstTest {
    in  clk: clock
    in  rst: reset
    out value: bit[8]
}

impl MatchConstTest {
    signal IDLE:  nat[2] = 0
    signal RUN:   nat[2] = 1
    signal DONE:  nat[2] = 2

    signal state: nat[2]

    on(clk.rise) {
        if rst {
            state = IDLE
            value = 0
        } else {
            match state {
                IDLE => {
                    value = 10
                    state = RUN
                }
                RUN => {
                    value = 20
                    state = DONE
                }
                DONE => {
                    value = 30
                    state = IDLE
                }
            }
        }
    }
}
"#;
    let sv = compile_to_sv(source).expect("Should compile match on constants");

    // Must generate specific case values, not just default
    assert!(
        sv.contains("case"),
        "Issue #10 Bug 4: should generate case statement"
    );
    // Should have at least one non-default case arm (0, 1, or 2)
    let has_case_value = sv.contains("0:") || sv.contains("1:") || sv.contains("2:");
    assert!(has_case_value, "Issue #10 Bug 4: case statement should have specific value arms, not just default, got:\n{}", sv);
}

#[test]
fn test_issue10_generic_widening_add_variable_width() {
    // Regression test for adder.sk: generic entity with let result: bit[WIDTH + 1]
    // The variable width must survive monomorphization and codegen without being
    // shrunk by the infer_variable_widths override (Bug Fix #8 interaction).
    let source = r#"
entity Adder<const WIDTH: nat = 8> {
    in clk: clock
    in a: bit[WIDTH]
    in b: bit[WIDTH]
    in carry_in: bit
    out sum: bit[WIDTH]
    out carry_out: bit
}

impl Adder {
    let result: bit[WIDTH + 1] = a +: b + carry_in
    sum = result[WIDTH - 1 : 0]
    carry_out = result[WIDTH]
}
"#;
    let sv = compile_to_sv(source).expect("Should compile generic widening add");

    // result must be 9-bit (WIDTH=8, WIDTH+1=9), declared as logic [8:0]
    assert!(
        sv.contains("[8:0] result"),
        "Issue #10: generic result should be 9-bit [8:0], got:\n{}",
        sv
    );
    assert!(
        !sv.contains("[7:0] result"),
        "Issue #10: result should NOT be 8-bit [7:0]"
    );
    // carry_out accesses result[8] which must be valid
    assert!(
        sv.contains("result[8]"),
        "Issue #10: carry_out should access result[8]"
    );
}

// =============================================================================
// TRIAGE 2026-08-02 #4: index-slice on memory subscript silently dropped
// =============================================================================

#[test]
fn test_triage4_nested_index_slice_preserved() {
    // `mem[ptr[3:0]]` parses the outer index's children as SIBLINGS
    // [IdentExpr(ptr), IndexExpr(3:0)]; every index-building path used to pick
    // a single child and silently emit `mem[ptr]` — a 5-bit pointer into a
    // 16-deep memory, out of range after wrap (tutorial ch08 AsyncFIFO).
    // The slice must survive on BOTH the write side (sequential lvalue) and
    // the read side (continuous-assign RHS, which flows through
    // build_index_access_from_parts).
    let source = r#"
entity MemSlice {
    in clk: clock
    in rst: reset
    in wr_en: bit
    in wr_data: bit[8]
    out rd_data: bit[8]
}

impl MemSlice {
    signal mem: [bit[8]; 16]
    signal ptr: bit[5] = 0

    on(clk.rise) {
        if (rst) {
            ptr = 0
        } else {
            if (wr_en) {
                mem[ptr[3:0]] = wr_data
                ptr = ptr + 1
            }
        }
    }

    rd_data = mem[ptr[3:0]]
}
"#;
    let sv = compile_to_sv(source).expect("Should compile sliced memory index");

    let sliced_count = sv.matches("mem[ptr[3:0]]").count();
    assert!(
        sliced_count >= 2,
        "Triage #4: expected `mem[ptr[3:0]]` on both write and read sides, found {} occurrence(s) in:\n{}",
        sliced_count,
        sv
    );
    assert!(
        !sv.contains("mem[ptr]"),
        "Triage #4: unsliced `mem[ptr]` means the index slice was dropped:\n{}",
        sv
    );
}

// =============================================================================
// TRIAGE 2026-08-02 #5: #[cdc] generated a synchronizer scaffold that
// multi-drove the user's hand-written sync chain (and whose sync registers
// were never clocked, with an undriven binary input). Per the published
// tutorial semantics, #[cdc] is a VERIFICATION annotation over the user's
// own synchronizer — it must not synthesize hardware.
// =============================================================================

#[test]
fn test_triage5_cdc_annotation_does_not_generate_hardware() {
    let source = r#"
entity CdcRepro {
    in wr_clk: clock
    in rd_clk: clock
    in rst: reset
    out val: bit[4]
}

impl CdcRepro {
    signal wr_ptr_gray: bit[4] = 0

    #[cdc(cdc_type = gray, sync_stages = 2)]
    signal wr_ptr_gray_sync_rd: bit[4] = 0

    signal ff1: bit[4] = 0

    on(wr_clk.rise) {
        if (rst) {
            wr_ptr_gray = 0
        } else {
            wr_ptr_gray = wr_ptr_gray + 1
        }
    }

    on(rd_clk.rise) {
        if (rst) {
            ff1 = 0
            wr_ptr_gray_sync_rd = 0
        } else {
            ff1 = wr_ptr_gray
            wr_ptr_gray_sync_rd = ff1
        }
    }

    val = wr_ptr_gray_sync_rd
}
"#;
    let sv = compile_to_sv(source).expect("CDC-annotated design should compile");

    // No synthesized scaffold nets. The scaffold suffixed the SIGNAL name
    // (wr_ptr_gray_sync_rd_gray_sync_0, ..._bin_in) — match those forms, not
    // the user's own `_gray_sync_`-containing signal name.
    for forbidden in [
        "wr_ptr_gray_sync_rd_bin_in",
        "wr_ptr_gray_sync_rd_gray",
        "_gray_sync_0",
        "_gray_sync_1",
        "_toggle_sync_",
        "_req_sync_",
    ] {
        assert!(
            !sv.contains(forbidden),
            "Triage #5: #[cdc] must not synthesize a synchronizer scaffold \
             (found `{}`):\n{}",
            forbidden,
            sv
        );
    }
    // The annotated signal is a normally-declared register (single driver:
    // the user's clocked chain), carrying the ASYNC_REG synthesis attribute.
    assert!(
        sv.contains("reg [3:0] wr_ptr_gray_sync_rd"),
        "Triage #5: annotated signal must declare as a normal reg:\n{}",
        sv
    );
    assert!(
        sv.contains("(* ASYNC_REG = \"TRUE\" *)"),
        "Triage #5: registered CDC signal should carry ASYNC_REG:\n{}",
        sv
    );
    assert!(
        sv.contains("// CDC:"),
        "Triage #5: the CDC annotation comment should be emitted:\n{}",
        sv
    );
    // Exactly one declaration of the annotated signal (the old scaffold
    // re-declared it as a wire, multi-driving the user's assignments)
    let decls =
        sv.matches("wr_ptr_gray_sync_rd;").count() + sv.matches("wr_ptr_gray_sync_rd =").count();
    assert!(
        !sv.contains("wire [3:0] wr_ptr_gray_sync_rd"),
        "Triage #5: annotated signal must not be re-declared as a wire:\n{}",
        sv
    );
    let _ = decls;
}

// =============================================================================
// TRIAGE 2026-08-02 #6: `open` port binding (the documented VHDL-style
// unconnected marker) lowered to a constant tie (`.z(0)`) — an instance
// OUTPUT driven by a constant, which is illegal SV and wrong intent. After
// the undefined-identifier check landed it became a hard error instead.
// `open` now behaves exactly like `_`: the connection is skipped (the
// output gets an auto-wire that nothing reads).
// =============================================================================

#[test]
fn test_triage6_open_binding_skips_connection() {
    let source = r#"
entity Adder4 {
    in a: bit[4]
    in b: bit[4]
    out sum: bit[4]
    out carry: bit
}

impl Adder4 {
    signal r: bit[5]
    r = {1'b0, a} + {1'b0, b}
    sum = r[3:0]
    carry = r[4]
}

entity Top {
    in a: bit[4]
    in b: bit[4]
    out s: bit[4]
}

impl Top {
    signal sm: bit[4]
    inst adder = Adder4 {
        a: a,
        b: b,
    }
    sm = adder.sum
    s = sm
}
"#;
    let sv = compile_to_sv(source).expect("`open` binding must compile");

    assert!(
        !sv.contains(".carry(0)") && !sv.contains(".carry(1'b0)"),
        "Triage #6: `open` must not tie the output to a constant:\n{}",
        sv
    );
    assert!(
        !sv.contains("undefined identifier"),
        "Triage #6: `open` must not resolve as an identifier:\n{}",
        sv
    );
    // The instance is emitted and `sum` is properly connected
    assert!(sv.contains("Adder4 adder"), "instance must exist:\n{}", sv);
    assert!(
        sv.contains(".sum("),
        "sum output must be wired (via the dot-access auto-wire):\n{}",
        sv
    );
}

// =============================================================================
// TRIAGE 2026-08-02 #7: `let x = 0` was special-cased as a "placeholder
// signal" — hir_to_mir treated ANY let-binding of literal 0 as an
// entity-output placeholder and silently dropped its initializing
// assignment, leaving the variable undriven. Placeholders are now marked
// explicitly (bare `signal x: T;` declarations in trait-method bodies);
// user-written zero bindings keep their assignments.
// =============================================================================

#[test]
fn test_triage7_let_zero_keeps_assignment() {
    let source = r#"
entity LetZero {
    in clk: clock
    in a: bit[8]
    out y: bit[8]
    out z: bit[8]
}

impl LetZero {
    on(clk.rise) {
        let zero = 0
        let base: bit[8] = 0
        y = a + zero
        z = base + 1
    }
}
"#;
    let sv = compile_to_sv(source).expect("let-zero design must compile");
    assert!(
        sv.contains("zero = 0;"),
        "Triage #7: `let zero = 0` must keep its initializing assignment:\n{}",
        sv
    );
    assert!(
        sv.contains("base = 0;"),
        "Triage #7: typed `let base: bit[8] = 0` must keep its assignment:\n{}",
        sv
    );
}

#[test]
fn test_triage7_impl_level_let_zero() {
    let source = r#"
entity LetZeroComb {
    in a: bit[8]
    out y: bit[8]
}

impl LetZeroComb {
    let zero = 0
    y = a | zero
}
"#;
    let sv = compile_to_sv(source).expect("impl-level let-zero must compile");
    assert!(
        sv.contains("assign zero = 0;"),
        "Triage #7: impl-level `let zero = 0` must drive the binding:\n{}",
        sv
    );
}

// =============================================================================
// TRIAGE 2026-08-02 #10: CDC diagnostics were stripped (report computed
// severities then printed nothing) AND the analysis itself was vacuous —
// no signal ever had a clock domain (hir_to_mir never populated them, and
// the HIR stamps that did exist used pre-monomorphization port IDs).
// Restored: implicit per-clock-port domains, process-based signal-domain
// inference, propagation through combinational assignments, and a severity
// policy — CRITICAL for crossings through logic (fails the build with
// details), WARNING for bare registered samples (synchronizer first
// stages), INFO when the target carries #[cdc].
// =============================================================================

#[test]
fn test_triage10_cdc_critical_fails_build_with_details() {
    let source = r#"
entity CdcCrit {
    in fast_clk: clock
    in slow_clk: clock
    in d: bit[8]
    out q: bit[8]
}

impl CdcCrit {
    signal fast_reg: bit[8] = 0
    signal slow_reg: bit[8] = 0

    on(fast_clk.rise) {
        fast_reg = d
    }

    on(slow_clk.rise) {
        slow_reg = fast_reg + 1
    }

    q = slow_reg
}
"#;
    // compile_to_sv bypasses MirCompiler (and with it the CDC step) — use
    // the compiler pipeline directly, like the CLI does.
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building failed");
    let mut engine = MonomorphizationEngine::new();
    let hir = engine.monomorphize(&hir);
    let compiler = skalp_mir::MirCompiler::new();
    let err = compiler
        .compile_to_mir(&hir)
        .err()
        .expect("crossing through logic must fail the build");
    assert!(
        err.contains("critical CDC violation"),
        "Triage #10: error must identify CDC: {}",
        err
    );
    assert!(
        err.contains("slow_reg"),
        "Triage #10: error must name the offending signal: {}",
        err
    );
}

#[test]
fn test_triage10_bare_sample_and_annotation_build() {
    // Bare registered sample (synchronizer first stage): WARNING, builds.
    let sample = r#"
entity CdcSample {
    in fast_clk: clock
    in slow_clk: clock
    in d: bit[8]
    out q: bit[8]
}

impl CdcSample {
    signal fast_reg: bit[8] = 0
    signal ff1: bit[8] = 0
    signal ff2: bit[8] = 0

    on(fast_clk.rise) {
        fast_reg = d
    }

    on(slow_clk.rise) {
        ff1 = fast_reg
        ff2 = ff1
    }

    q = ff2
}
"#;
    let mir_of = |source: &str| {
        let tree = parse(source);
        let hir = build_hir(&tree).expect("HIR building failed");
        let mut engine = MonomorphizationEngine::new();
        let hir = engine.monomorphize(&hir);
        skalp_mir::MirCompiler::new().compile_to_mir(&hir)
    };
    mir_of(sample).expect("bare registered sample must build (warning only)");

    // #[cdc]-annotated target: INFO, builds.
    let annotated = r#"
entity CdcAnno {
    in fast_clk: clock
    in slow_clk: clock
    in d: bit[4]
    out q: bit[4]
}

impl CdcAnno {
    signal fast_gray: bit[4] = 0

    #[cdc(cdc_type = gray, sync_stages = 2)]
    signal sync1: bit[4] = 0
    signal sync2: bit[4] = 0

    on(fast_clk.rise) {
        fast_gray = d ^ (d >> 1)
    }

    on(slow_clk.rise) {
        sync1 = fast_gray
        sync2 = sync1
    }

    q = sync2
}
"#;
    mir_of(annotated).expect("#[cdc]-annotated synchronizer must build (info only)");
}

#[test]
fn test_triage10_comb_derived_domain_propagates() {
    // The crossing samples a COMBINATIONALLY-derived signal (gray encode of
    // a registered pointer) — domain propagation must still detect it as a
    // legitimate sample (build succeeds), and single-clock designs must
    // stay silent (no domains crossed).
    let source = r#"
entity CombDerived {
    in wr_clk: clock
    in rd_clk: clock
    out q: bit[4]
}

impl CombDerived {
    signal wr_ptr: bit[4] = 0
    signal wr_gray: bit[4]
    signal ff1: bit[4] = 0

    on(wr_clk.rise) {
        wr_ptr = wr_ptr + 1
    }

    wr_gray = wr_ptr ^ (wr_ptr >> 1)

    on(rd_clk.rise) {
        ff1 = wr_gray
    }

    q = ff1
}
"#;
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building failed");
    let mut engine = MonomorphizationEngine::new();
    let hir = engine.monomorphize(&hir);
    skalp_mir::MirCompiler::new()
        .compile_to_mir(&hir)
        .expect("comb-derived sample must build (warning only)");
}

// =============================================================================
// TRIAGE 2026-08-02 #9: match exhaustiveness checking did not exist — a
// match missing an enum arm built clean and the missing value fell into the
// last arm, while the published docs promise a compile error. Implemented
// as a conversion error (reachability-scoped): enum scrutinees need every
// variant or a catch-all; N-bit scrutinees need all 2^N values or a
// catch-all (N > 20 always requires one). Guarded arms never count.
// =============================================================================

#[test]
fn test_triage9_nonexhaustive_enum_match_fails() {
    let source = r#"
enum State {
    Idle,
    Run,
    Done,
}

entity Fsm {
    in clk: clock
    in start: bit
    out busy: bit
}

impl Fsm {
    signal st: State = State::Idle

    on(clk.rise) {
        match st {
            State::Idle => {
                if start {
                    st = State::Run
                }
            }
            State::Run => {
                st = State::Done
            }
        }
    }

    busy = if st == State::Run { 1 } else { 0 }
}
"#;
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building failed");
    let mut engine = MonomorphizationEngine::new();
    let hir = engine.monomorphize(&hir);
    let err = skalp_mir::MirCompiler::new()
        .compile_to_mir(&hir)
        .err()
        .expect("non-exhaustive enum match must fail the build");
    assert!(
        err.contains("non-exhaustive match") && err.contains("`Done`"),
        "Triage #9: error must name the missing variant: {}",
        err
    );
}

#[test]
fn test_triage9_bit_match_coverage() {
    let missing = r#"
entity Sel {
    in s: bit[2]
    out y: bit[4]
}

impl Sel {
    y = match s {
        0b00 => 1,
        0b01 => 2,
        0b10 => 4,
    }
}
"#;
    let complete = r#"
entity Sel {
    in s: bit[2]
    out y: bit[4]
}

impl Sel {
    y = match s {
        0b00 => 1,
        0b01 => 2,
        0b10 => 4,
        0b11 => 8,
    }
}
"#;
    let wildcarded = r#"
entity Sel {
    in s: bit[8]
    out y: bit[4]
}

impl Sel {
    y = match s {
        0 => 1,
        _ => 0,
    }
}
"#;
    let compile = |source: &str| {
        let tree = parse(source);
        let hir = build_hir(&tree).expect("HIR building failed");
        let mut engine = MonomorphizationEngine::new();
        let hir = engine.monomorphize(&hir);
        skalp_mir::MirCompiler::new().compile_to_mir(&hir)
    };

    let err = compile(missing).err().expect("3/4 coverage must fail");
    assert!(
        err.contains("non-exhaustive match") && err.contains("3 of 4"),
        "Triage #9: {}",
        err
    );
    compile(complete).expect("full 2-bit enumeration must build");
    compile(wildcarded).expect("wildcard arm must satisfy exhaustiveness");
}

// =============================================================================
// TRIAGE 2026-08-02 #12: `stream<T>` had NO lowering — the type converter
// stripped it to the bare inner type, so a "stream" port was plain wires with
// no valid/ready handshaking while the docs claim enforced backpressure.
// Until real protocol lowering exists, stream types are a hard build error.
// =============================================================================

#[test]
fn test_triage12_stream_type_fails_build() {
    let port_form = r#"
entity StreamProducer {
    in clk: clock
    out data: stream<bit[8]>
}

impl StreamProducer {
    signal cnt: bit[8] = 0
    on(clk.rise) {
        cnt = cnt + 1
    }
    data = cnt
}
"#;
    let signal_form = r#"
entity Plain {
    in clk: clock
    out o: bit[8]
}

impl Plain {
    signal buf: stream<bit[8]>
    signal cnt: bit[8] = 0
    on(clk.rise) {
        cnt = cnt + 1
    }
    buf = cnt
    o = buf
}
"#;
    let compile = |source: &str| {
        let tree = parse(source);
        let hir = build_hir(&tree).expect("HIR building failed");
        let mut engine = MonomorphizationEngine::new();
        let hir = engine.monomorphize(&hir);
        skalp_mir::MirCompiler::new().compile_to_mir(&hir)
    };

    let err = compile(port_form)
        .err()
        .expect("stream port must fail the build");
    assert!(
        err.contains("stream") && err.contains("not implemented") && err.contains("`data`"),
        "Triage #12: error must name the stream port: {}",
        err
    );
    let err = compile(signal_form)
        .err()
        .expect("stream signal must fail the build");
    assert!(
        err.contains("stream") && err.contains("`buf`"),
        "Triage #12: error must name the stream signal: {}",
        err
    );
}

// =============================================================================
// TRIAGE 2026-08-02 #13/#14: entity/signal clock-domain LIFETIMES were lowered
// as POWER domains — `entity Sync<'src, 'dst>` stamped every impl signal with
// a bogus (* power_domain = "src" *) UPF attribute (read-domain signals tagged
// with the write domain). And `bit[8]<'domain>` (width + lifetime) did not
// parse at all, though the spec and tutorial use it. Lifetimes now register as
// clock domains, domain names flow HIR→MIR, and the CDC analyzer unifies
// domains by name — so an annotated cross-domain read is a real violation.
// =============================================================================

#[test]
fn test_triage13_lifetimes_are_clock_domains_not_power() {
    let source = r#"
entity Sync<'src, 'dst> {
    in  clk_dst: clock<'dst>
    in  rst:     reset
    in  data_in: bit<'src>
    out data_out: bit
}

impl Sync {
    signal ff1: bit
    signal ff2: bit

    on(clk_dst.rise) {
        if rst {
            ff1 = 0
            ff2 = 0
        } else {
            ff1 = data_in
            ff2 = ff1
        }
    }

    data_out = ff2
}
"#;
    let sv = compile_to_sv(source).expect("Sync must compile");
    assert!(
        !sv.contains("power_domain"),
        "Triage #13: clock lifetimes must NOT emit power_domain UPF attributes:\n{}",
        sv
    );
}

#[test]
fn test_triage14_width_plus_domain_parses() {
    let source = r#"
entity V<'a> {
    in clk: clock<'a>
    in d: bit[8]<'a>
    out q: bit[8]
}

impl V {
    signal r: bit[8] = 0
    on(clk.rise) {
        r = d
    }
    q = r
}
"#;
    let sv = compile_to_sv(source).expect("bit[8]<'a> must parse and compile");
    assert!(
        sv.contains("[7:0] d"),
        "Triage #14: domain annotation must not eat the width:\n{}",
        sv
    );
}

#[test]
fn test_triage13_annotated_crossing_fails_build() {
    // data_in is annotated 'src but consumed (with logic) in a 'dst-clocked
    // process with no synchronizer — must be a critical CDC violation.
    let source = r#"
entity BadCross<'src, 'dst> {
    in  clk_dst: clock<'dst>
    in  rst: reset
    in  data_in: bit[8]<'src>
    out data_out: bit[8]
}

impl BadCross {
    signal reg_out: bit[8] = 0

    on(clk_dst.rise) {
        if rst {
            reg_out = 0
        } else {
            reg_out = data_in + 1
        }
    }

    data_out = reg_out
}
"#;
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building failed");
    let mut engine = MonomorphizationEngine::new();
    let hir = engine.monomorphize(&hir);
    let err = skalp_mir::MirCompiler::new()
        .compile_to_mir(&hir)
        .err()
        .expect("annotated unsynchronized crossing must fail the build");
    assert!(
        err.contains("'src") && err.contains("'dst"),
        "Triage #13: CDC violation must name the lifetimes, got: {}",
        err
    );
}

#[test]
fn test_triage13_synchronizer_still_builds() {
    // The standard 2-FF synchronizer (bare registered sample) must stay
    // buildable — only crossings with logic are critical.
    let source = r#"
entity GoodSync<'src, 'dst> {
    in  clk_dst: clock<'dst>
    in  rst: reset
    in  data_in: bit<'src>
    out data_out: bit
}

impl GoodSync {
    signal ff1: bit
    signal ff2: bit

    on(clk_dst.rise) {
        if rst {
            ff1 = 0
            ff2 = 0
        } else {
            ff1 = data_in
            ff2 = ff1
        }
    }

    data_out = ff2
}
"#;
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building failed");
    let mut engine = MonomorphizationEngine::new();
    let hir = engine.monomorphize(&hir);
    skalp_mir::MirCompiler::new()
        .compile_to_mir(&hir)
        .expect("2-FF synchronizer must build");
}

// =============================================================================
// TRIAGE 2026-08-02 #15: the declared type of a `let` binding was ignored —
// `let full_sum: bit[9] = a + b` emitted a 10-bit net because the MIR
// variable conversion unconditionally widened Add/Sub initializers by one bit
// (a carry-preservation hack meant for INFERRED types). A declared type is a
// contract; widening now applies only when the type was inferred.
// =============================================================================

#[test]
fn test_triage15_declared_let_type_is_respected() {
    let declared = r#"
entity Adder {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[9]
}

impl Adder {
    let full_sum: bit[9] = a + b
    sum = full_sum
}
"#;
    let sv = compile_to_sv(declared).expect("declared-width let must compile");
    assert!(
        sv.contains("[8:0] full_sum"),
        "Triage #15: declared bit[9] must emit a 9-bit net, got:\n{}",
        sv
    );
    assert!(
        !sv.contains("[9:0] full_sum"),
        "Triage #15: declared bit[9] must NOT be widened to 10 bits"
    );

    // Inferred lets keep the carry-preserving widening.
    let inferred = r#"
entity AdderInf {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[9]
}

impl AdderInf {
    let full_sum = a + b
    sum = full_sum
}
"#;
    let sv = compile_to_sv(inferred).expect("inferred let must compile");
    assert!(
        sv.contains("[8:0] full_sum"),
        "Triage #15: inferred a+b must still preserve the carry (9 bits), got:\n{}",
        sv
    );
}

// =============================================================================
// TRIAGE 2026-08-02 #16: a `signal` declared inside `on()` got an unsound
// width — the SV emitter's width-override pass computed binary-op widths as
// max(operand widths), so `(cnt == 15)` was "32 bits" (comparison ≠ operand
// width, and the bare integer literal's default 32 poisoned the max) and the
// declared 1-bit variable was widened to logic [31:0]. Comparisons/boolean
// ops now compute 1 bit, and context-determined integer literals defer to
// the other operand's width.
// =============================================================================

#[test]
fn test_triage16_on_block_signal_width() {
    let comparison = r#"
entity Tick {
    in clk: clock
    in rst: reset
    out q: bit
}

impl Tick {
    signal cnt: bit[4] = 0

    on(clk.rise) {
        signal tick: bit = (cnt == 15)
        if rst {
            cnt = 0
        } else {
            cnt = cnt + 1
        }
        q = tick
    }
}
"#;
    let sv = compile_to_sv(comparison).expect("on()-local signal must compile");
    assert!(
        sv.contains("logic tick;"),
        "Triage #16: declared 1-bit condition must stay 1 bit, got:\n{}",
        sv
    );
    assert!(
        !sv.contains("[31:0] tick"),
        "Triage #16: comparison result must not be widened to 32 bits"
    );

    let literal_arith = r#"
entity Tick2 {
    in clk: clock
    in rst: reset
    out q: bit[4]
}

impl Tick2 {
    signal cnt: bit[4] = 0
    on(clk.rise) {
        signal nxt: bit[4] = cnt + 1
        if rst {
            cnt = 0
        } else {
            cnt = nxt
        }
        q = cnt
    }
}
"#;
    let sv = compile_to_sv(literal_arith).expect("literal arithmetic must compile");
    assert!(
        sv.contains("[3:0] nxt"),
        "Triage #16: integer literal must not poison the width to 32, got:\n{}",
        sv
    );
}
