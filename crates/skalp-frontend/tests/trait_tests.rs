//! Tests for trait definitions and implementations

use skalp_frontend::parse::parse;

#[test]
fn test_trait_definition() {
    let source = r#"
        trait Resetable {
            type ResetValue: nat[8];
            const DEFAULT_RESET: nat[8] = 0;

            reset() -> bit;
            get_reset_value() -> ResetValue;
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Should find the trait definition
    
    fn find_trait_def(node: &skalp_frontend::syntax::SyntaxNode) -> bool {
        if node.kind() == skalp_frontend::syntax::SyntaxKind::TraitDef {
            return true;
        }
        for child in node.children() {
            if find_trait_def(&child) {
                return true;
            }
        }
        false
    }
    let found_trait = find_trait_def(&tree);
    assert!(found_trait, "Trait definition not found in parsed tree");
}

#[test]
fn test_trait_implementation() {
    let source = r#"
        trait Configurable {
            type ConfigData;
            configure(data: ConfigData) -> bit;
        }

        entity ConfigurableCounter {
            in clk: clock
            in config: nat[8]
            out count: nat[8]
        }

        impl Configurable for ConfigurableCounter {
            type ConfigData = nat[8];

            configure(data: ConfigData) -> bit {
                config = data
            }
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Should find both trait definition and implementation
    
    fn find_trait_impl(node: &skalp_frontend::syntax::SyntaxNode) -> bool {
        if node.kind() == skalp_frontend::syntax::SyntaxKind::TraitImpl {
            return true;
        }
        for child in node.children() {
            if find_trait_impl(&child) {
                return true;
            }
        }
        false
    }
    let found_trait_impl = find_trait_impl(&tree);
    assert!(
        found_trait_impl,
        "Trait implementation not found in parsed tree"
    );
}

#[test]
fn test_trait_with_where_clause() {
    let source = r#"
        trait Pipeline<T> where T: nat[32] {
            type Output;

            process(input: T) -> Output;
        }

        entity DataPipeline {
            in data: nat[32]
            out result: nat[32]
        }

        impl Pipeline<nat[32]> for DataPipeline where nat[32]: Default {
            type Output = nat[32];

            process(input: nat[32]) -> nat[32] {
                result = input
            }
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Should parse where clauses
    
    fn find_where_clause(node: &skalp_frontend::syntax::SyntaxNode) -> bool {
        if node.kind() == skalp_frontend::syntax::SyntaxKind::WhereClause {
            return true;
        }
        for child in node.children() {
            if find_where_clause(&child) {
                return true;
            }
        }
        false
    }
    let found_where_clause = find_where_clause(&tree);
    assert!(found_where_clause, "Where clause not found in parsed tree");
}

#[test]
fn test_trait_with_super_traits() {
    let source = r#"
        trait Verifiable {
            verify() -> bit;
        }

        trait Testable : Verifiable {
            test() -> bit;

            run_all_tests() -> bit {
                verify() && test()
            }
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Should parse trait bounds
    
    fn find_trait_bounds(node: &skalp_frontend::syntax::SyntaxNode) -> bool {
        if node.kind() == skalp_frontend::syntax::SyntaxKind::TraitBoundList {
            return true;
        }
        for child in node.children() {
            if find_trait_bounds(&child) {
                return true;
            }
        }
        false
    }
    let found_trait_bounds = find_trait_bounds(&tree);
    assert!(found_trait_bounds, "Trait bounds not found in parsed tree");
}
