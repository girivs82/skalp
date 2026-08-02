//! Tests for named generic arguments in function calls
//!
//! Tests the syntax: func::<NAME: value>(args) is correctly parsed
//! and stored in HirCallExpr.named_type_args.

use skalp_frontend::hir::HirExpression;
use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::parse::parse;
use skalp_mir::hir_to_mir::HirToMir;

#[test]
fn test_named_generic_function_call_parsing() {
    println!("=== Testing Named Generic Function Call Parsing ===");

    let source = r#"
// Generic function with a width parameter
pub fn generic_add<W>(a: bit[W], b: bit[W]) -> bit[W] {
    return a + b
}

entity TestEntity {
    in x: bit[32],
    in y: bit[32],
    out result: bit[32],
}

impl TestEntity {
    // Call with named generic argument: W: 32
    result = generic_add::<W: 32>(x, y)
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    // Find the implementation for TestEntity
    assert!(
        !hir.implementations.is_empty(),
        "Should have at least one implementation"
    );

    let impl_block = hir.implementations.iter().find(|i| {
        // Find the impl associated with the entity
        hir.entities
            .iter()
            .any(|e| e.id == i.entity && e.name == "TestEntity")
    });

    assert!(
        impl_block.is_some(),
        "Should have implementation for TestEntity"
    );
    let impl_block = impl_block.unwrap();

    println!(
        "Found implementation with {} assignments",
        impl_block.assignments.len()
    );

    for (idx, assign) in impl_block.assignments.iter().enumerate() {
        println!("Assignment {}: lhs={:?}", idx, assign.lhs);
        if let HirExpression::Call(call) = &assign.rhs {
            println!(
                "  Found call to '{}' with {} type_args and {} named_type_args",
                call.function,
                call.type_args.len(),
                call.named_type_args.len()
            );

            if call.function == "generic_add" {
                // Check that named_type_args has "W" with value 32
                assert!(
                    call.named_type_args.contains_key("W"),
                    "Should have named type arg 'W'"
                );

                println!("  Named type args: {:?}", call.named_type_args);
                println!("Named generic function call test PASSED!");
                return;
            }
        }
    }

    panic!("Did not find generic_add call with named type args");
}

#[test]
fn test_mixed_positional_and_named_generics() {
    println!("=== Testing Mixed Positional and Named Generics ===");

    let source = r#"
// Generic function with multiple parameters
pub fn generic_op<W, M>(a: bit[W], b: bit[W]) -> bit[M] {
    return a[M-1:0]
}

entity MixedTest {
    in x: bit[32],
    in y: bit[32],
    out result: bit[16],
}

impl MixedTest {
    // Call with positional followed by named generic
    result = generic_op::<32, M: 16>(x, y)
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let impl_block = hir.implementations.iter().find(|i| {
        hir.entities
            .iter()
            .any(|e| e.id == i.entity && e.name == "MixedTest")
    });

    assert!(
        impl_block.is_some(),
        "Should have implementation for MixedTest"
    );
    let impl_block = impl_block.unwrap();

    for assign in &impl_block.assignments {
        if let HirExpression::Call(call) = &assign.rhs {
            if call.function == "generic_op" {
                println!("Found generic_op call:");
                println!("  type_args (positional): {:?}", call.type_args);
                println!("  named_type_args: {:?}", call.named_type_args);

                // Should have one positional and one named
                assert_eq!(call.type_args.len(), 1, "Should have 1 positional type arg");
                assert_eq!(
                    call.named_type_args.len(),
                    1,
                    "Should have 1 named type arg"
                );
                assert!(
                    call.named_type_args.contains_key("M"),
                    "Should have named arg 'M'"
                );

                println!("Mixed positional and named generics test PASSED!");
                return;
            }
        }
    }

    panic!("Did not find generic_op call");
}

#[test]
fn test_named_generic_mir_compilation() {
    println!("=== Testing Named Generic MIR Compilation ===");

    let source = r#"
pub fn width_add<W>(a: bit[W], b: bit[W]) -> bit[W] {
    return a + b
}

entity WidthTest {
    in x: bit[64],
    in y: bit[64],
    out result: bit[64],
}

impl WidthTest {
    result = width_add::<W: 64>(x, y)
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    // Compile to MIR
    let mut transformer = HirToMir::new();
    let mir = transformer.transform(&hir);

    assert!(
        !mir.modules.is_empty(),
        "MIR should have at least one module"
    );

    println!(
        "MIR compilation successful with {} modules",
        mir.modules.len()
    );

    let top_module = mir.modules.iter().find(|m| m.name == "WidthTest");
    assert!(top_module.is_some(), "Should have WidthTest module");

    println!("Named generic MIR compilation test PASSED!");
}
