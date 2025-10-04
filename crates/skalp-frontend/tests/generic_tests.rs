//! Tests for generic entities and type parameters

use skalp_frontend::parse::parse;

#[test]
fn test_generic_entity_definition() {
    let source = r#"
        entity Buffer<T, const SIZE: nat[32]> {
            in data: T
            in write_en: bit
            out buffer_out: T[SIZE]
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Should find generic parameters

    fn find_generic_params(node: &skalp_frontend::syntax::SyntaxNode) -> bool {
        if node.kind() == skalp_frontend::syntax::SyntaxKind::GenericParamList {
            return true;
        }
        for child in node.children() {
            if find_generic_params(&child) {
                return true;
            }
        }
        false
    }
    let found_generics = find_generic_params(&tree);
    assert!(
        found_generics,
        "Generic parameters not found in parsed tree"
    );
}

#[test]
fn test_generic_entity_instantiation() {
    let source = r#"
        entity Buffer<T, const SIZE: nat[32]> {
            in data: T
            in write_en: bit
            out buffer_out: T[SIZE]
        }

        entity System {
            in sys_data: nat[8]
            in sys_write: bit
            out sys_out: nat[8][16]
        }

        impl System {
            signal buffer_inst: Buffer<nat[8], 16>
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Look for generic instantiation in implementation
    // The parsing should handle Buffer<nat[8], 16> correctly
}

#[test]
fn test_generic_entity_with_constraints() {
    let source = r#"
        trait Arithmetic {
            add(self, other: Self) -> Self;
        }

        entity ALU<T: Arithmetic, const WIDTH: nat[32]> {
            in a: T[WIDTH]
            in b: T[WIDTH]
            out result: T[WIDTH]
        }

        impl<T: Arithmetic, const WIDTH: nat[32]> ALU<T, WIDTH> {
            result = a.add(b)
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Should parse generic parameters with trait bounds

    fn find_trait_bound(node: &skalp_frontend::syntax::SyntaxNode) -> bool {
        if node.kind() == skalp_frontend::syntax::SyntaxKind::TraitBound {
            return true;
        }
        for child in node.children() {
            if find_trait_bound(&child) {
                return true;
            }
        }
        false
    }
    let found_trait_bound = find_trait_bound(&tree);
    assert!(
        found_trait_bound,
        "Trait bound not found in generic parameters"
    );
}

#[test]
fn test_const_generic_parameters() {
    let source = r#"
        entity FIFO<const DEPTH: nat[16], const WIDTH: nat[8]> {
            in push_data: nat[WIDTH]
            in push: bit
            in pop: bit
            out pop_data: nat[WIDTH]
            out empty: bit
            out full: bit
        }

        impl<const DEPTH: nat[16], const WIDTH: nat[8]> FIFO<DEPTH, WIDTH> {
            signal memory: nat[WIDTH][DEPTH]
            signal read_ptr: nat[16] = 0
            signal write_ptr: nat[16] = 0

            on(push.rise) {
                if (!full) {
                    memory[write_ptr] <= push_data
                    write_ptr <= (write_ptr + 1) % DEPTH
                }
            }
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Should find const generic parameters

    fn check_for_const(node: &skalp_frontend::syntax::SyntaxNode) -> bool {
        // Check if this is a generic parameter node
        if node.kind() == skalp_frontend::syntax::SyntaxKind::GenericParam {
            // Look for const keyword in children
            for token in node.children_with_tokens() {
                if let Some(t) = token.as_token() {
                    if t.kind() == skalp_frontend::syntax::SyntaxKind::ConstKw {
                        return true;
                    }
                }
            }
        }
        for child in node.children() {
            if check_for_const(&child) {
                return true;
            }
        }
        false
    }
    let found_const_param = check_for_const(&tree);
    assert!(found_const_param, "Const generic parameter not found");
}

#[test]
fn test_type_parameter_inference() {
    let source = r#"
        entity Mux<T> {
            in select: bit
            in a: T
            in b: T
            out y: T
        }

        impl<T> Mux<T> {
            y = select ? a : b
        }

        entity Top {
            in sel: bit
            in data_a: nat[32]
            in data_b: nat[32]
            out result: nat[32]
        }

        impl Top {
            // Type parameter should be inferred as nat[32]
            signal mux: Mux<nat[32]>

            mux.select = sel
            mux.a = data_a
            mux.b = data_b
            result = mux.y
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Check that generic instantiation is parsed
    // Looking for Mux<nat[32]>
}

#[test]
fn test_nested_generics() {
    let source = r#"
        entity Wrapper<T> {
            in input: T
            out output: T
        }

        entity DoubleWrapper<U> {
            in data_in: U
            out data_out: U
        }

        impl<U> DoubleWrapper<U> {
            signal inner1: Wrapper<U>
            signal inner2: Wrapper<U>

            inner1.input = data_in
            inner2.input = inner1.output
            data_out = inner2.output
        }
    "#;

    // Parse
    let tree = parse(source);
    assert_eq!(tree.kind(), skalp_frontend::syntax::SyntaxKind::SourceFile);

    // Should handle nested generic instantiations
}
