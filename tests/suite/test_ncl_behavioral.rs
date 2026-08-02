//! Tests for NCL behavioral simulation runtime.
//!
//! Validates that NclBehavioralRuntime correctly models DATA/NULL phase
//! alternation at word level using the compiled C++ kernel.
//!
//! These tests mirror the gate-level tests in test_ncl_async_simulation.rs —
//! same designs, same test vectors, but running at behavioral level (compiled C++)
//! instead of gate level (dual-rail THmn evaluation).
#![cfg(target_os = "macos")]

use skalp_frontend::parse_and_build_hir;
use skalp_mir::MirCompiler;
use skalp_sim::NclBehavioralRuntime;
use skalp_sir::convert_mir_to_sir_with_hierarchy;

/// Compile skalp source to behavioral SIR module
fn compile_to_sir(source: &str, module_name: &str) -> skalp_sir::SirModule {
    let hir = parse_and_build_hir(source).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir_design = mir_compiler
        .compile(&hir)
        .expect("Failed to compile to MIR");

    let mir_module = mir_design
        .modules
        .iter()
        .find(|m| m.name == module_name)
        .unwrap_or_else(|| panic!("Module '{}' not found", module_name))
        .clone();

    convert_mir_to_sir_with_hierarchy(&mir_design, &mir_module)
}

/// Helper: compile source, create runtime, run one DATA phase with given inputs,
/// check expected outputs. Tests multiple input vectors against the same design.
/// One test case: (inputs, expected outputs), each as (name, value, width).
type BehavioralCase<'a> = (&'a [(&'a str, u64, usize)], &'a [(&'a str, u64, usize)]);

fn test_behavioral_combinational(source: &str, module_name: &str, test_cases: &[BehavioralCase]) {
    let sir = compile_to_sir(source, module_name);
    let mut sim = NclBehavioralRuntime::new(&sir).expect("Failed to create runtime");

    for (inputs, expected_outputs) in test_cases {
        // Set inputs
        for (name, value, width) in *inputs {
            sim.set_input(name, *value, *width);
        }

        // DATA phase
        sim.step_data();

        // Check outputs
        for (name, expected, _width) in *expected_outputs {
            let actual = sim.get_output(name);
            assert_eq!(
                actual,
                Some(*expected),
                "Output '{}': expected {}, got {:?}",
                name,
                expected,
                actual
            );
        }

        // NULL phase (reset for next test vector)
        sim.step_null();
    }
}

// ============================================================================
// Logical operations (mirrors gate-level tests)
// ============================================================================

#[test]
fn test_ncl_behavioral_inverter() {
    let source = r#"
        async entity NclInverter {
            in a: bit[1]
            out y: bit[1]
        }
        impl NclInverter {
            y = ~a
        }
    "#;

    test_behavioral_combinational(
        source,
        "NclInverter",
        &[
            (&[("a", 0, 1)], &[("y", 1, 1)]),
            (&[("a", 1, 1)], &[("y", 0, 1)]),
        ],
    );
}

#[test]
fn test_ncl_behavioral_and_gate() {
    let source = r#"
        async entity NclAnd {
            in a: bit[1]
            in b: bit[1]
            out y: bit[1]
        }
        impl NclAnd {
            y = a & b
        }
    "#;

    test_behavioral_combinational(
        source,
        "NclAnd",
        &[
            (&[("a", 0, 1), ("b", 0, 1)], &[("y", 0, 1)]),
            (&[("a", 0, 1), ("b", 1, 1)], &[("y", 0, 1)]),
            (&[("a", 1, 1), ("b", 0, 1)], &[("y", 0, 1)]),
            (&[("a", 1, 1), ("b", 1, 1)], &[("y", 1, 1)]),
        ],
    );
}

#[test]
fn test_ncl_behavioral_or_gate() {
    let source = r#"
        async entity NclOr {
            in a: bit[1]
            in b: bit[1]
            out y: bit[1]
        }
        impl NclOr {
            y = a | b
        }
    "#;

    test_behavioral_combinational(
        source,
        "NclOr",
        &[
            (&[("a", 0, 1), ("b", 0, 1)], &[("y", 0, 1)]),
            (&[("a", 0, 1), ("b", 1, 1)], &[("y", 1, 1)]),
            (&[("a", 1, 1), ("b", 0, 1)], &[("y", 1, 1)]),
            (&[("a", 1, 1), ("b", 1, 1)], &[("y", 1, 1)]),
        ],
    );
}

#[test]
fn test_ncl_behavioral_xor_gate() {
    let source = r#"
        async entity NclXor {
            in a: bit[1]
            in b: bit[1]
            out y: bit[1]
        }
        impl NclXor {
            y = a ^ b
        }
    "#;

    test_behavioral_combinational(
        source,
        "NclXor",
        &[
            (&[("a", 0, 1), ("b", 0, 1)], &[("y", 0, 1)]),
            (&[("a", 0, 1), ("b", 1, 1)], &[("y", 1, 1)]),
            (&[("a", 1, 1), ("b", 0, 1)], &[("y", 1, 1)]),
            (&[("a", 1, 1), ("b", 1, 1)], &[("y", 0, 1)]),
        ],
    );
}

// ============================================================================
// Multi-bit logical operations
// ============================================================================

#[test]
fn test_ncl_behavioral_and_8bit() {
    let source = r#"
        async entity NclAnd8 {
            in a: bit[8]
            in b: bit[8]
            out y: bit[8]
        }
        impl NclAnd8 {
            y = a & b
        }
    "#;

    test_behavioral_combinational(
        source,
        "NclAnd8",
        &[
            (&[("a", 0x00, 8), ("b", 0x00, 8)], &[("y", 0x00, 8)]),
            (&[("a", 0xFF, 8), ("b", 0xFF, 8)], &[("y", 0xFF, 8)]),
            (&[("a", 0xAA, 8), ("b", 0x55, 8)], &[("y", 0x00, 8)]),
            (&[("a", 0xF0, 8), ("b", 0x0F, 8)], &[("y", 0x00, 8)]),
        ],
    );
}

#[test]
fn test_ncl_behavioral_or_8bit() {
    let source = r#"
        async entity NclOr8 {
            in a: bit[8]
            in b: bit[8]
            out y: bit[8]
        }
        impl NclOr8 {
            y = a | b
        }
    "#;

    test_behavioral_combinational(
        source,
        "NclOr8",
        &[
            (&[("a", 0x00, 8), ("b", 0x00, 8)], &[("y", 0x00, 8)]),
            (&[("a", 0xFF, 8), ("b", 0xFF, 8)], &[("y", 0xFF, 8)]),
            (&[("a", 0xAA, 8), ("b", 0x55, 8)], &[("y", 0xFF, 8)]),
            (&[("a", 0xF0, 8), ("b", 0x0F, 8)], &[("y", 0xFF, 8)]),
        ],
    );
}

#[test]
fn test_ncl_behavioral_xor_8bit() {
    let source = r#"
        async entity NclXor8 {
            in a: bit[8]
            in b: bit[8]
            out y: bit[8]
        }
        impl NclXor8 {
            y = a ^ b
        }
    "#;

    test_behavioral_combinational(
        source,
        "NclXor8",
        &[
            (&[("a", 0x00, 8), ("b", 0x00, 8)], &[("y", 0x00, 8)]),
            (&[("a", 0xFF, 8), ("b", 0xFF, 8)], &[("y", 0x00, 8)]),
            (&[("a", 0xAA, 8), ("b", 0x55, 8)], &[("y", 0xFF, 8)]),
            (&[("a", 0xF0, 8), ("b", 0x0F, 8)], &[("y", 0xFF, 8)]),
        ],
    );
}

#[test]
fn test_ncl_behavioral_not_8bit() {
    let source = r#"
        async entity NclNot8 {
            in a: bit[8]
            out y: bit[8]
        }
        impl NclNot8 {
            y = ~a
        }
    "#;

    test_behavioral_combinational(
        source,
        "NclNot8",
        &[
            (&[("a", 0x00, 8)], &[("y", 0xFF, 8)]),
            (&[("a", 0xFF, 8)], &[("y", 0x00, 8)]),
            (&[("a", 0xAA, 8)], &[("y", 0x55, 8)]),
            (&[("a", 0xF0, 8)], &[("y", 0x0F, 8)]),
        ],
    );
}

// ============================================================================
// Arithmetic operations
// ============================================================================

#[test]
fn test_ncl_behavioral_add_8bit() {
    let source = r#"
        async entity NclAdd8 {
            in a: bit[8]
            in b: bit[8]
            out sum: bit[8]
        }
        impl NclAdd8 {
            sum = a + b
        }
    "#;

    test_behavioral_combinational(
        source,
        "NclAdd8",
        &[
            (&[("a", 0, 8), ("b", 0, 8)], &[("sum", 0, 8)]),
            (&[("a", 1, 8), ("b", 1, 8)], &[("sum", 2, 8)]),
            (&[("a", 10, 8), ("b", 20, 8)], &[("sum", 30, 8)]),
            (&[("a", 100, 8), ("b", 50, 8)], &[("sum", 150, 8)]),
            (&[("a", 255, 8), ("b", 0, 8)], &[("sum", 255, 8)]),
            (&[("a", 128, 8), ("b", 127, 8)], &[("sum", 255, 8)]),
        ],
    );
}

#[test]
fn test_ncl_behavioral_sub_8bit() {
    let source = r#"
        async entity NclSub8 {
            in a: bit[8]
            in b: bit[8]
            out diff: bit[8]
        }
        impl NclSub8 {
            diff = a - b
        }
    "#;

    test_behavioral_combinational(
        source,
        "NclSub8",
        &[
            (&[("a", 0, 8), ("b", 0, 8)], &[("diff", 0, 8)]),
            (&[("a", 5, 8), ("b", 3, 8)], &[("diff", 2, 8)]),
            (&[("a", 100, 8), ("b", 50, 8)], &[("diff", 50, 8)]),
            (&[("a", 255, 8), ("b", 255, 8)], &[("diff", 0, 8)]),
            (&[("a", 200, 8), ("b", 100, 8)], &[("diff", 100, 8)]),
        ],
    );
}

#[test]
fn test_ncl_behavioral_mul_4bit() {
    let source = r#"
        async entity NclMul4 {
            in a: bit[4]
            in b: bit[4]
            out prod: bit[8]
        }
        impl NclMul4 {
            prod = a * b
        }
    "#;

    test_behavioral_combinational(
        source,
        "NclMul4",
        &[
            (&[("a", 0, 4), ("b", 0, 4)], &[("prod", 0, 8)]),
            (&[("a", 1, 4), ("b", 1, 4)], &[("prod", 1, 8)]),
            (&[("a", 2, 4), ("b", 3, 4)], &[("prod", 6, 8)]),
            (&[("a", 5, 4), ("b", 5, 4)], &[("prod", 25, 8)]),
            (&[("a", 15, 4), ("b", 1, 4)], &[("prod", 15, 8)]),
            (&[("a", 7, 4), ("b", 8, 4)], &[("prod", 56, 8)]),
        ],
    );
}

// ============================================================================
// Comparison operations
// ============================================================================

#[test]
fn test_ncl_behavioral_eq_8bit() {
    let source = r#"
        async entity NclEq8 {
            in a: bit[8]
            in b: bit[8]
            out eq: bit[1]
        }
        impl NclEq8 {
            eq = a == b
        }
    "#;

    test_behavioral_combinational(
        source,
        "NclEq8",
        &[
            (&[("a", 0, 8), ("b", 0, 8)], &[("eq", 1, 1)]),
            (&[("a", 42, 8), ("b", 42, 8)], &[("eq", 1, 1)]),
            (&[("a", 255, 8), ("b", 255, 8)], &[("eq", 1, 1)]),
            (&[("a", 0, 8), ("b", 1, 8)], &[("eq", 0, 1)]),
            (&[("a", 100, 8), ("b", 200, 8)], &[("eq", 0, 1)]),
        ],
    );
}

#[test]
fn test_ncl_behavioral_lt_8bit() {
    let source = r#"
        async entity NclLt8 {
            in a: bit[8]
            in b: bit[8]
            out lt: bit[1]
        }
        impl NclLt8 {
            lt = a < b
        }
    "#;

    test_behavioral_combinational(
        source,
        "NclLt8",
        &[
            (&[("a", 0, 8), ("b", 1, 8)], &[("lt", 1, 1)]),
            (&[("a", 50, 8), ("b", 100, 8)], &[("lt", 1, 1)]),
            (&[("a", 0, 8), ("b", 0, 8)], &[("lt", 0, 1)]),
            (&[("a", 100, 8), ("b", 50, 8)], &[("lt", 0, 1)]),
            (&[("a", 255, 8), ("b", 255, 8)], &[("lt", 0, 1)]),
        ],
    );
}

// ============================================================================
// Shift operations
// ============================================================================

#[test]
fn test_ncl_behavioral_shift_left() {
    let source = r#"
        async entity NclShl {
            in a: bit[8]
            out y: bit[8]
        }
        impl NclShl {
            y = a << 1
        }
    "#;

    test_behavioral_combinational(
        source,
        "NclShl",
        &[
            (&[("a", 0x00, 8)], &[("y", 0x00, 8)]),
            (&[("a", 0x01, 8)], &[("y", 0x02, 8)]),
            (&[("a", 0x40, 8)], &[("y", 0x80, 8)]),
            (&[("a", 0x55, 8)], &[("y", 0xAA, 8)]),
        ],
    );
}

#[test]
fn test_ncl_behavioral_shift_right() {
    let source = r#"
        async entity NclShr {
            in a: bit[8]
            out y: bit[8]
        }
        impl NclShr {
            y = a >> 1
        }
    "#;

    test_behavioral_combinational(
        source,
        "NclShr",
        &[
            (&[("a", 0x00, 8)], &[("y", 0x00, 8)]),
            (&[("a", 0x02, 8)], &[("y", 0x01, 8)]),
            (&[("a", 0x80, 8)], &[("y", 0x40, 8)]),
            (&[("a", 0xAA, 8)], &[("y", 0x55, 8)]),
        ],
    );
}

// ============================================================================
// Mux and combined logic
// ============================================================================

#[test]
fn test_ncl_behavioral_mux() {
    let source = r#"
        async entity NclMux {
            in sel: bit[1]
            in a: bit[8]
            in b: bit[8]
            out y: bit[8]
        }
        impl NclMux {
            y = if sel == 1 { b } else { a }
        }
    "#;

    test_behavioral_combinational(
        source,
        "NclMux",
        &[
            (
                &[("sel", 0, 1), ("a", 42, 8), ("b", 100, 8)],
                &[("y", 42, 8)],
            ),
            (
                &[("sel", 1, 1), ("a", 42, 8), ("b", 100, 8)],
                &[("y", 100, 8)],
            ),
        ],
    );
}

#[test]
fn test_ncl_behavioral_combined_logic() {
    let source = r#"
        async entity NclCombined {
            in a: bit[8]
            in b: bit[8]
            out y: bit[8]
        }
        impl NclCombined {
            y = ~(a ^ b)
        }
    "#;

    test_behavioral_combinational(
        source,
        "NclCombined",
        &[
            (&[("a", 0x00, 8), ("b", 0x00, 8)], &[("y", 0xFF, 8)]),
            (&[("a", 0xFF, 8), ("b", 0xFF, 8)], &[("y", 0xFF, 8)]),
            (&[("a", 0xAA, 8), ("b", 0x55, 8)], &[("y", 0x00, 8)]),
            (&[("a", 0xF0, 8), ("b", 0xF0, 8)], &[("y", 0xFF, 8)]),
        ],
    );
}

// ============================================================================
// DATA/NULL phase alternation
// ============================================================================

#[test]
fn test_ncl_behavioral_phase_alternation() {
    let source = r#"
        async entity NclAdder {
            in a: bit[8]
            in b: bit[8]
            out sum: bit[8]
        }
        impl NclAdder {
            sum = a + b
        }
    "#;

    let sir = compile_to_sir(source, "NclAdder");
    let mut sim = NclBehavioralRuntime::new(&sir).expect("Failed to create runtime");

    // DATA phase: a=10, b=20 -> sum=30
    sim.set_input("a", 10, 8);
    sim.set_input("b", 20, 8);
    sim.step_data();
    assert_eq!(sim.get_output("sum"), Some(30));

    // NULL phase: outputs should go to zero
    sim.step_null();
    assert_eq!(sim.get_output("sum"), Some(0));

    // Another DATA phase: a=100, b=55 -> sum=155
    sim.set_input("a", 100, 8);
    sim.set_input("b", 55, 8);
    sim.step_data();
    assert_eq!(sim.get_output("sum"), Some(155));
}

// ============================================================================
// Sequential: counter with enable (NCL on() block)
// ============================================================================

#[test]
fn test_ncl_behavioral_counter() {
    let source = r#"
        async entity NclCounter {
            in enable: bit
            out count: bit[8]
        }

        impl NclCounter {
            signal counter: bit[8] = 0

            on() {
                if enable {
                    counter = counter + 1
                }
            }

            count = counter
        }
    "#;

    let sir = compile_to_sir(source, "NclCounter");
    let mut sim = NclBehavioralRuntime::new(&sir).expect("Failed to create runtime");

    // Cycle 1: enable=1 -> counter should increment to 1
    sim.set_input("enable", 1, 1);
    sim.step_cycle();
    assert_eq!(sim.get_output("count"), Some(1));

    // Cycle 2: enable=1 -> counter=2
    sim.set_input("enable", 1, 1);
    sim.step_cycle();
    assert_eq!(sim.get_output("count"), Some(2));

    // Cycle 3: enable=0 -> counter stays 2
    sim.set_input("enable", 0, 1);
    sim.step_cycle();
    assert_eq!(sim.get_output("count"), Some(2));

    // Cycles 4-8: enable=1 -> counter goes to 7
    sim.set_input("enable", 1, 1);
    for _ in 0..5 {
        sim.step_cycle();
    }
    assert_eq!(sim.get_output("count"), Some(7));

    // Verify stats
    let stats = sim.stats();
    assert_eq!(stats.cycles, 8);
    assert_eq!(stats.data_phases, 8);
    assert_eq!(stats.null_phases, 8);
}

// ============================================================================
// Sequential: shift register
// ============================================================================

#[test]
fn test_ncl_behavioral_shift_register() {
    let source = r#"
        async entity NclShiftReg {
            in data_in: bit[8]
            out s0: bit[8]
            out s1: bit[8]
            out s2: bit[8]
            out s3: bit[8]
        }

        impl NclShiftReg {
            signal stage0: bit[8] = 0
            signal stage1: bit[8] = 0
            signal stage2: bit[8] = 0
            signal stage3: bit[8] = 0

            on() {
                stage0 = data_in
                stage1 = stage0
                stage2 = stage1
                stage3 = stage2
            }

            s0 = stage0
            s1 = stage1
            s2 = stage2
            s3 = stage3
        }
    "#;

    let sir = compile_to_sir(source, "NclShiftReg");
    let mut sim = NclBehavioralRuntime::new(&sir).expect("Failed to create runtime");

    // Cycle 1: push 0xAA into stage0
    sim.set_input("data_in", 0xAA, 8);
    sim.step_cycle();
    assert_eq!(sim.get_output("s0"), Some(0xAA));
    assert_eq!(sim.get_output("s1"), Some(0));
    assert_eq!(sim.get_output("s2"), Some(0));
    assert_eq!(sim.get_output("s3"), Some(0));

    // Cycle 2: push 0xBB, 0xAA shifts to stage1
    sim.set_input("data_in", 0xBB, 8);
    sim.step_cycle();
    assert_eq!(sim.get_output("s0"), Some(0xBB));
    assert_eq!(sim.get_output("s1"), Some(0xAA));
    assert_eq!(sim.get_output("s2"), Some(0));
    assert_eq!(sim.get_output("s3"), Some(0));

    // Cycle 3: push 0xCC
    sim.set_input("data_in", 0xCC, 8);
    sim.step_cycle();
    assert_eq!(sim.get_output("s0"), Some(0xCC));
    assert_eq!(sim.get_output("s1"), Some(0xBB));
    assert_eq!(sim.get_output("s2"), Some(0xAA));
    assert_eq!(sim.get_output("s3"), Some(0));

    // Cycle 4: push 0xDD — all stages filled
    sim.set_input("data_in", 0xDD, 8);
    sim.step_cycle();
    assert_eq!(sim.get_output("s0"), Some(0xDD));
    assert_eq!(sim.get_output("s1"), Some(0xCC));
    assert_eq!(sim.get_output("s2"), Some(0xBB));
    assert_eq!(sim.get_output("s3"), Some(0xAA));
}

// ============================================================================
// run_cycles batch API
// ============================================================================

#[test]
fn test_ncl_behavioral_run_cycles() {
    let source = r#"
        async entity NclIncrementor {
            in dummy: bit
            out count: bit[8]
        }

        impl NclIncrementor {
            signal counter: bit[8] = 0

            on() {
                counter = counter + 1
            }

            count = counter
        }
    "#;

    let sir = compile_to_sir(source, "NclIncrementor");
    let mut sim = NclBehavioralRuntime::new(&sir).expect("Failed to create runtime");

    sim.set_input("dummy", 1, 1);
    let results = sim.run_cycles(10);

    assert_eq!(results.len(), 10);
    // After 10 cycles, counter should be 10
    assert_eq!(sim.get_output("count"), Some(10));
}

// ============================================================================
// Reset behavior
// ============================================================================

#[test]
fn test_ncl_behavioral_reset() {
    let source = r#"
        async entity NclResetTest {
            in enable: bit
            out count: bit[8]
        }

        impl NclResetTest {
            signal counter: bit[8] = 0

            on() {
                if enable {
                    counter = counter + 1
                }
            }

            count = counter
        }
    "#;

    let sir = compile_to_sir(source, "NclResetTest");
    let mut sim = NclBehavioralRuntime::new(&sir).expect("Failed to create runtime");

    // Count to 5
    sim.set_input("enable", 1, 1);
    sim.run_cycles(5);
    assert_eq!(sim.get_output("count"), Some(5));

    // Reset — counter should go back to 0
    sim.reset();
    assert_eq!(sim.get_output("count"), Some(0));
    assert_eq!(sim.stats().cycles, 0);

    // Count again — should start from 0
    sim.set_input("enable", 1, 1);
    sim.run_cycles(3);
    assert_eq!(sim.get_output("count"), Some(3));
}
