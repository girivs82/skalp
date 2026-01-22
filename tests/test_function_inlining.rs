/// Test suite for function inlining feature
///
/// Tests are organized by implementation phase:
/// - Phase 1: Error handling for unsupported cases
/// - Phase 2: Simple functions (no local variables)
/// - Phase 3: Functions with let bindings
/// - Phase 4: Control flow (if/match expressions)
/// - Phase 5: Recursion detection
use skalp_testing::testbench::*;

/// Phase 2 Tests: Simple Function Inlining (No Local Variables)

#[tokio::test]
async fn test_simple_add_function() {
    let mut tb = Testbench::new("tests/fixtures/functions/simple_add.sk")
        .await
        .unwrap();

    // Test: result = add(5, 3) should give 8
    tb.set("x", 5u8).set("y", 3u8);
    tb.clock(1).await;
    tb.expect("result", 8u8).await;

    // Test: result = add(10, 20) should give 30
    tb.set("x", 10u8).set("y", 20u8);
    tb.clock(1).await;
    tb.expect("result", 30u8).await;
}

#[tokio::test]
async fn test_simple_sub_function() {
    let mut tb = Testbench::new("tests/fixtures/functions/simple_sub.sk")
        .await
        .unwrap();

    tb.set("x", 10u8).set("y", 3u8);
    tb.clock(1).await;
    tb.expect("result", 7u8).await;
}

#[tokio::test]
async fn test_simple_bitwise_and() {
    let mut tb = Testbench::new("tests/fixtures/functions/simple_and.sk")
        .await
        .unwrap();

    tb.set("x", 0b11110000u8).set("y", 0b10101010u8);
    tb.clock(1).await;
    tb.expect("result", 0b10100000u8).await;
}

#[tokio::test]
async fn test_multiple_function_calls() {
    // Test calling multiple different functions in same module
    let mut tb = Testbench::new("tests/fixtures/functions/multiple_calls.sk")
        .await
        .unwrap();

    tb.set("a", 5u8).set("b", 3u8).set("c", 2u8);
    tb.clock(1).await;

    // r1 = add(a, b) = 5 + 3 = 8
    tb.expect("r1", 8u8).await;

    // r2 = sub(a, b) = 5 - 3 = 2
    tb.expect("r2", 2u8).await;

    // r3 = mul(a, c) = 5 * 2 = 10
    tb.expect("r3", 10u8).await;
}

#[tokio::test]
async fn test_nested_function_calls() {
    // Test: result = add(add(a, b), c)
    let mut tb = Testbench::new("tests/fixtures/functions/nested_calls.sk")
        .await
        .unwrap();

    tb.set("a", 5u8).set("b", 3u8).set("c", 2u8);
    tb.clock(1).await;

    // add(add(5, 3), 2) = add(8, 2) = 10
    tb.expect("result", 10u8).await;
}

#[tokio::test]
async fn test_function_in_expression() {
    // Test: result = add(a, b) * 2
    let mut tb = Testbench::new("tests/fixtures/functions/function_in_expr.sk")
        .await
        .unwrap();

    tb.set("a", 5u8).set("b", 3u8);
    tb.clock(1).await;

    // (5 + 3) * 2 = 8 * 2 = 16
    tb.expect("result", 16u8).await;
}

/// Phase 3 Tests: Functions with Let Bindings

#[tokio::test]
async fn test_function_with_single_let() {
    // fn add_double(a, b) { let sum = a + b; return sum * 2 }
    let mut tb = Testbench::new("tests/fixtures/functions/let_single.sk")
        .await
        .unwrap();

    tb.set("x", 5u8).set("y", 3u8);
    tb.clock(1).await;

    // (5 + 3) * 2 = 16
    tb.expect("result", 16u8).await;
}

#[tokio::test]
async fn test_function_with_multiple_lets() {
    // fn complex(a, b, c) {
    //     let sum = a + b
    //     let prod = sum * c
    //     return prod
    // }
    let mut tb = Testbench::new("tests/fixtures/functions/let_multiple.sk")
        .await
        .unwrap();

    tb.set("a", 5u8).set("b", 3u8).set("c", 2u8);
    tb.clock(1).await;

    // ((5 + 3) * 2) = 16
    tb.expect("result", 16u8).await;
}

#[tokio::test]
async fn test_function_with_dependent_lets() {
    // fn chain(a, b, c) {
    //     let x = a + b
    //     let y = x * c
    //     let z = y + a
    //     return z
    // }
    let mut tb = Testbench::new("tests/fixtures/functions/let_chain.sk")
        .await
        .unwrap();

    tb.set("a", 2u8).set("b", 3u8).set("c", 4u8);
    tb.clock(1).await;

    // x = 2 + 3 = 5
    // y = 5 * 4 = 20
    // z = 20 + 2 = 22
    tb.expect("result", 22u8).await;
}

/// Phase 4 Tests: Control Flow (If/Match Expressions)

#[tokio::test]
async fn test_function_with_if_expr() {
    // fn max(a, b) { if a > b { return a } else { return b } }
    let mut tb = Testbench::new("tests/fixtures/functions/if_expr.sk")
        .await
        .unwrap();

    // Test: max(10, 5) = 10
    tb.set("a", 10u8).set("b", 5u8);
    tb.clock(1).await;
    tb.expect("result", 10u8).await;

    // Test: max(3, 8) = 8
    tb.set("a", 3u8).set("b", 8u8);
    tb.clock(1).await;
    tb.expect("result", 8u8).await;
}

#[tokio::test]
async fn test_function_with_nested_if() {
    // fn clamp(x, min, max) {
    //     if x < min { return min }
    //     else if x > max { return max }
    //     else { return x }
    // }
    let mut tb = Testbench::new("tests/fixtures/functions/if_nested.sk")
        .await
        .unwrap();

    // Test: clamp(5, 0, 10) = 5 (within range)
    tb.set("x", 5u8).set("min", 0u8).set("max", 10u8);
    tb.clock(1).await;
    tb.expect("result", 5u8).await;

    // Test: clamp(15, 0, 10) = 10 (clamped to max)
    tb.set("x", 15u8).set("min", 0u8).set("max", 10u8);
    tb.clock(1).await;
    tb.expect("result", 10u8).await;

    // Test: clamp(0, 5, 10) = 5 (clamped to min)
    tb.set("x", 0u8).set("min", 5u8).set("max", 10u8);
    tb.clock(1).await;
    tb.expect("result", 5u8).await;
}

#[tokio::test]
async fn test_function_with_match_expr() {
    // fn decode(op) {
    //     match op {
    //         0 => return 0x00,
    //         1 => return 0x11,
    //         2 => return 0x22,
    //         _ => return 0xFF,
    //     }
    // }
    let mut tb = Testbench::new("tests/fixtures/functions/match_expr.sk")
        .await
        .unwrap();

    tb.set("op", 0u8);
    tb.clock(1).await;
    tb.expect("result", 0x00u8).await;

    tb.set("op", 1u8);
    tb.clock(1).await;
    tb.expect("result", 0x11u8).await;

    tb.set("op", 2u8);
    tb.clock(1).await;
    tb.expect("result", 0x22u8).await;

    tb.set("op", 99u8);
    tb.clock(1).await;
    tb.expect("result", 0xFFu8).await;
}

/// Phase 5 Tests: Edge Cases and Limitations

#[tokio::test]
#[ignore = "Bug: Recursive functions cause stack overflow instead of proper error detection"]
#[should_panic(expected = "Recursive function calls are not supported")]
async fn test_recursive_function_errors() {
    // fn factorial(n) { if n == 0 { return 1 } else { return n * factorial(n-1) } }
    // Should error during compilation
    let _tb = Testbench::new("tests/fixtures/functions/recursive.sk")
        .await
        .unwrap(); // Should panic here
}

#[tokio::test]
async fn test_function_with_wide_types() {
    // Test functions work with wider bit widths
    let mut tb = Testbench::new("tests/fixtures/functions/wide_types.sk")
        .await
        .unwrap();

    tb.set("x", 1000u32).set("y", 234u32);
    tb.clock(1).await;
    tb.expect("result", 1234u32).await;
}

#[tokio::test]
async fn test_function_in_sequential_block() {
    // Test function calls work in sequential (clocked) logic
    let mut tb = Testbench::new("tests/fixtures/functions/sequential.sk")
        .await
        .unwrap();

    tb.set("rst", 1u8);
    tb.clock(1).await;
    tb.set("rst", 0u8);

    tb.set("a", 5u8).set("b", 3u8);
    tb.clock(1).await;
    tb.expect("result", 8u8).await;

    tb.set("a", 10u8).set("b", 20u8);
    tb.clock(1).await;
    tb.expect("result", 30u8).await;
}

#[tokio::test]
async fn test_multiple_functions_same_impl() {
    // Test module with multiple function definitions
    let mut tb = Testbench::new("tests/fixtures/functions/multiple_defs.sk")
        .await
        .unwrap();

    tb.set("sel", 0u8).set("a", 10u8).set("b", 3u8);
    tb.clock(1).await;
    tb.expect("result", 13u8).await; // add

    tb.set("sel", 1u8);
    tb.clock(1).await;
    tb.expect("result", 7u8).await; // sub

    tb.set("sel", 2u8);
    tb.clock(1).await;
    tb.expect("result", 30u8).await; // mul
}
