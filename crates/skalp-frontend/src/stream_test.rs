#[cfg(test)]
mod stream_tests {
    use crate::parse::{parse, parse_with_errors};
    use crate::syntax::SyntaxKind;

    #[test]
    fn test_stream_type_parsing() {
        let source = r#"
        entity StreamProcessor {
            in data: stream<bit[32]>
            out result: stream<bit[16]>
        }
        "#;

        let (tree, errors) = parse_with_errors(source);

        // Should parse without errors
        assert!(errors.is_empty(), "Parse errors: {:?}", errors);

        // Check that stream types are in the syntax tree
        let mut found_stream_types = 0;
        fn count_stream_types(node: &crate::syntax::SyntaxNode) -> usize {
            let mut count = if node.kind() == SyntaxKind::StreamType { 1 } else { 0 };
            for child in node.children() {
                count += count_stream_types(&child);
            }
            count
        }

        found_stream_types = count_stream_types(&tree);
        assert_eq!(found_stream_types, 2, "Should have found 2 stream types");
    }

    #[test]
    fn test_nested_stream_types() {
        let source = r#"
        entity ComplexStream {
            in nested: stream<stream<bit[8]>>
        }
        "#;

        let (tree, errors) = parse_with_errors(source);

        // Should parse without errors
        assert!(errors.is_empty(), "Parse errors: {:?}", errors);

        // Check for nested stream types
        fn check_nested_streams(node: &crate::syntax::SyntaxNode) -> bool {
            if node.kind() == SyntaxKind::StreamType {
                // Check if this stream contains another stream
                // It might be wrapped in a TypeAnnotation node
                for child in node.children() {
                    // Look for nested StreamType either directly or through TypeAnnotation
                    if child.kind() == SyntaxKind::StreamType {
                        return true;
                    }
                    if child.kind() == SyntaxKind::TypeAnnotation {
                        // Check if TypeAnnotation contains a StreamType
                        for grandchild in child.children() {
                            if grandchild.kind() == SyntaxKind::StreamType {
                                return true;
                            }
                        }
                    }
                    if check_nested_streams(&child) {
                        return true;
                    }
                }
            }
            for child in node.children() {
                if check_nested_streams(&child) {
                    return true;
                }
            }
            false
        }

        let found_nested = check_nested_streams(&tree);
        assert!(found_nested, "Should have found nested stream types");
    }
}