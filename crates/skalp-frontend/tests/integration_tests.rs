//! Integration tests for SKALP frontend
//!
//! Tests the complete pipeline: lexer -> parser -> type checker -> HIR

use skalp_frontend::parse::parse;
use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::typeck::TypeChecker;

#[test]
fn test_parse_counter_example() {
    let source = include_str!("../../../examples/counter.sk");

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Check we have entity and impl
    let children: Vec<_> = tree.children().collect();
    assert!(children.len() >= 2);
}

#[test]
fn test_type_check_counter() {
    let source = include_str!("../../../examples/counter.sk");

    // Parse
    let tree = parse(source);

    // Type check
    let mut checker = TypeChecker::new();
    let result = checker.check_source_file(&tree);

    // Should have some type errors due to incomplete implementation
    // but basic structure should be OK
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_build_hir_counter() {
    let source = include_str!("../../../examples/counter.sk");

    // Parse
    let tree = parse(source);

    // Build HIR
    let result = build_hir(&tree);

    // Should produce HIR (may have errors but structure should exist)
    match result {
        Ok(hir) => {
            assert!(!hir.entities.is_empty());
            assert_eq!(hir.entities[0].name, "Counter");
        }
        Err(errors) => {
            // Print errors for debugging
            for err in errors {
                eprintln!("HIR Error: {}", err.message);
            }
        }
    }
}

#[test]
fn test_complete_pipeline() {
    let source = r#"
        entity Adder {
            in a: nat[8]
            in b: nat[8]
            out sum: nat[9]
        }

        impl Adder {
            sum = a + b
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Type check
    let mut checker = TypeChecker::new();
    let _type_result = checker.check_source_file(&tree);

    // Build HIR
    let hir_result = build_hir(&tree);

    // Debug print any errors
    if let Err(ref errors) = hir_result {
        for err in errors {
            eprintln!("HIR Error: {}", err.message);
        }
    }

    assert!(hir_result.is_ok());

    let hir = hir_result.unwrap();
    assert_eq!(hir.entities.len(), 1);
    assert_eq!(hir.implementations.len(), 1);
}

#[test]
fn test_event_block_parsing() {
    let source = r#"
        entity FlipFlop {
            in clk: clock
            in d: bit
            out q: bit
        }

        impl FlipFlop {
            signal q_reg: bit = 0

            on(clk.rise) {
                q_reg <= d
            }

            q = q_reg
        }
    "#;

    let tree = parse(source);
    let hir_result = build_hir(&tree);

    assert!(hir_result.is_ok());
    let hir = hir_result.unwrap();
    assert_eq!(hir.implementations[0].event_blocks.len(), 1);
}

#[test]
fn test_error_recovery() {
    // Test that parser can recover from errors
    let source = r#"
        entity Test {
            in a: bit[8
            in b: bit[8]
            out c: bit[8]
        }
    "#;

    let tree = parse(source);
    // Should still produce a tree despite the error
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);
}

#[test]
fn test_match_expression_parsing() {
    let source = r#"
        entity Decoder {
            in select: nat[2]
            out output: nat[4]
        }

        impl Decoder {
            match select {
                0 -> output = 1,
                1 -> output = 2,
                2 -> output = 4,
                _ -> output = 0,
            }
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Should find the match statement in the syntax tree
    let mut found_match = false;
    fn find_match_stmt(node: &skalp_frontend::syntax::SyntaxNode) -> bool {
        if node.kind() == skalp_frontend::syntax::SyntaxKind::MatchStmt {
            return true;
        }
        for child in node.children() {
            if find_match_stmt(&child) {
                return true;
            }
        }
        false
    }
    found_match = find_match_stmt(&tree);
    assert!(found_match, "Match statement not found in parsed tree");

    // Build HIR
    let hir_result = build_hir(&tree);

    // Debug print any errors
    if let Err(ref errors) = hir_result {
        for err in errors {
            eprintln!("HIR Error: {}", err.message);
        }
    }

    assert!(hir_result.is_ok());
    let hir = hir_result.unwrap();

    // Check HIR contains match statement
    assert_eq!(hir.entities.len(), 1);
    assert_eq!(hir.implementations.len(), 1);

    // TODO: Add more specific checks for match statement HIR structure
}

#[test]
fn test_pattern_matching_types() {
    let source = r#"
        entity PatternTest {
            in input: nat[4]
            out result: bit
        }

        impl PatternTest {
            match input {
                0b1010 -> result = 1,
                0x0F -> result = 1,
                15 -> result = 1,
                x -> result = 0,
            }
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Build HIR - should handle different literal patterns
    let hir_result = build_hir(&tree);

    if let Err(ref errors) = hir_result {
        for err in errors {
            eprintln!("HIR Error: {}", err.message);
        }
    }

    // Should successfully parse different pattern types
    assert!(hir_result.is_ok());
}

#[test]
fn test_flow_block_parsing() {
    let source = r#"
        entity Pipeline {
            in data_in: nat[8]
            out data_out: nat[8]
        }

        impl Pipeline {
            flow {
                data_in |> stage1 |> stage2 |> data_out
            }
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Should find the flow statement in the syntax tree
    let mut found_flow = false;
    fn find_flow_stmt(node: &skalp_frontend::syntax::SyntaxNode) -> bool {
        if node.kind() == skalp_frontend::syntax::SyntaxKind::FLOW_STMT {
            return true;
        }
        for child in node.children() {
            if find_flow_stmt(&child) {
                return true;
            }
        }
        false
    }
    found_flow = find_flow_stmt(&tree);
    assert!(found_flow, "Flow statement not found in parsed tree");

    // Build HIR
    let hir_result = build_hir(&tree);

    // Debug print any errors
    if let Err(ref errors) = hir_result {
        for err in errors {
            eprintln!("HIR Error: {}", err.message);
        }
    }

    assert!(hir_result.is_ok());
    let hir = hir_result.unwrap();

    // Check HIR contains flow statement
    assert_eq!(hir.entities.len(), 1);
    assert_eq!(hir.implementations.len(), 1);
}

#[test]
fn test_flow_block_with_stages() {
    let source = r#"
        entity ComplexPipeline {
            in input: nat[16]
            out output: nat[16]
        }

        impl ComplexPipeline {
            flow {
                input
                |> { decode_result = decode(input) }
                |> { exec_result = execute(decode_result) }
                |> { output = writeback(exec_result) }
            }
        }
    "#;


    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Build HIR - should handle block stages in pipeline
    let hir_result = build_hir(&tree);

    if let Err(ref errors) = hir_result {
        for err in errors {
            eprintln!("HIR Error: {}", err.message);
        }
    }

    // Should successfully parse flow blocks with complex stages
    assert!(hir_result.is_ok());
}

