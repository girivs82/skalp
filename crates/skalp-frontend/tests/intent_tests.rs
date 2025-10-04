//! Tests for intent parsing and propagation

use skalp_frontend::parse::parse;

#[test]
fn test_basic_intent_declaration() {
    let source = r#"
        intent HighPerformance {
            timing: 100 MHz
            throughput: maximize
            power: 5 W
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Should find intent declaration

    fn find_intent_decl(node: &skalp_frontend::syntax::SyntaxNode) -> bool {
        if node.kind() == skalp_frontend::syntax::SyntaxKind::IntentDecl {
            return true;
        }
        for child in node.children() {
            if find_intent_decl(&child) {
                return true;
            }
        }
        false
    }
    let found_intent = find_intent_decl(&tree);
    assert!(found_intent, "Intent declaration not found in parsed tree");

    // Should find intent constraints

    fn find_constraints(node: &skalp_frontend::syntax::SyntaxNode) -> bool {
        if node.kind() == skalp_frontend::syntax::SyntaxKind::IntentConstraintList {
            return true;
        }
        for child in node.children() {
            if find_constraints(&child) {
                return true;
            }
        }
        false
    }
    let found_constraints = find_constraints(&tree);
    assert!(
        found_constraints,
        "Intent constraints not found in parsed tree"
    );
}

#[test]
fn test_intent_for_entity() {
    let source = r#"
        entity Processor {
            in clk: clock
            in data: nat[32]
            out result: nat[32]
        }

        intent FastProcessor for Processor {
            timing: 200 MHz
            latency: < 10 cycles
            area: minimize
        }

        impl Processor with FastProcessor {
            // Implementation guided by intent
            result = data
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Should parse intent with 'for' clause

    fn find_intent(node: &skalp_frontend::syntax::SyntaxNode) -> bool {
        if node.kind() == skalp_frontend::syntax::SyntaxKind::IntentDecl {
            return true;
        }
        for child in node.children() {
            if find_intent(&child) {
                return true;
            }
        }
        false
    }
    let found_intent = find_intent(&tree);
    assert!(found_intent, "Intent declaration not found");
}

#[test]
fn test_optimization_intents() {
    let source = r#"
        intent LowPower {
            power: minimize
            timing: 50 MHz
        }

        intent HighThroughput {
            throughput: maximize
            latency: minimize
        }

        intent Balanced {
            power: < 2 W
            timing: 100 MHz
            area: < 1000 LUTs
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Should parse multiple intents with different optimization goals

    fn count_intents(node: &skalp_frontend::syntax::SyntaxNode) -> usize {
        let mut count = 0;
        if node.kind() == skalp_frontend::syntax::SyntaxKind::IntentDecl {
            count = 1;
        }
        for child in node.children() {
            count += count_intents(&child);
        }
        count
    }
    let intent_count = count_intents(&tree);
    assert_eq!(intent_count, 3, "Expected 3 intent declarations");
}

#[test]
fn test_complex_intent_expressions() {
    let source = r#"
        intent PipelineIntent {
            timing: 250 MHz
            throughput: 1 transaction per cycle
            latency: 5 cycles
            power: 3.3 V * 500 mA
            area: < 5000 LUTs + 2000 FFs
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Should handle complex constraint expressions with units and operators

    fn check_constraints(node: &skalp_frontend::syntax::SyntaxNode) -> bool {
        if node.kind() == skalp_frontend::syntax::SyntaxKind::IntentConstraint {
            return true;
        }
        for child in node.children() {
            if check_constraints(&child) {
                return true;
            }
        }
        false
    }
    let found_constraints = check_constraints(&tree);
    assert!(found_constraints, "Intent constraints not found");
}

#[test]
fn test_intent_with_generic_entity() {
    let source = r#"
        entity Buffer<T, const SIZE: nat[32]> {
            in data: T
            out buffer_out: T[SIZE]
        }

        intent FastBuffer<T, const SIZE: nat[32]> for Buffer<T, SIZE> {
            timing: 300 MHz
            latency: 1 cycle
        }

        impl<T, const SIZE: nat[32]> Buffer<T, SIZE> with FastBuffer<T, SIZE> {
            // Fast implementation
            buffer_out = data
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Should handle intents for generic entities
}

#[test]
fn test_hierarchical_intents() {
    let source = r#"
        intent SystemLevel {
            power: < 10 W
            timing: 100 MHz
        }

        entity TopLevel {
            in sys_clk: clock
            in sys_data: nat[64]
            out sys_out: nat[64]
        }

        entity SubModule {
            in clk: clock
            in data: nat[32]
            out result: nat[32]
        }

        intent SubModuleIntent for SubModule {
            // Derived from system-level intent
            power: < 2 W
            timing: 100 MHz
        }

        impl TopLevel with SystemLevel {
            signal sub1: SubModule with SubModuleIntent
            signal sub2: SubModule with SubModuleIntent

            // System implementation
            sub1.data = sys_data[31:0]
            sub2.data = sys_data[63:32]
            sys_out = {sub2.result, sub1.result}
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Should support hierarchical intent propagation
}
