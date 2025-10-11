//! Tests for module system parsing

use skalp_frontend::parse::parse;
use skalp_frontend::syntax::SyntaxKind;

#[test]
fn test_simple_use_statement() {
    let source = r#"
        use foo::bar::Baz;
    "#;

    let tree = parse(source);
    assert_eq!(tree.kind(), SyntaxKind::SourceFile);

    // Find the use declaration
    let use_decl = tree.children().find(|n| n.kind() == SyntaxKind::UseDecl);
    assert!(use_decl.is_some(), "Expected UseDecl node");
}

#[test]
fn test_renamed_import() {
    let source = r#"
        use foo::bar::Baz as Qux;
    "#;

    let tree = parse(source);
    assert_eq!(tree.kind(), SyntaxKind::SourceFile);

    let use_decl = tree.children().find(|n| n.kind() == SyntaxKind::UseDecl);
    assert!(use_decl.is_some(), "Expected UseDecl node");
}

#[test]
fn test_glob_import() {
    let source = r#"
        use foo::bar::*;
    "#;

    let tree = parse(source);
    assert_eq!(tree.kind(), SyntaxKind::SourceFile);

    let use_decl = tree.children().find(|n| n.kind() == SyntaxKind::UseDecl);
    assert!(use_decl.is_some(), "Expected UseDecl node");
}

#[test]
fn test_nested_imports() {
    let source = r#"
        use foo::bar::{Baz, Qux};
    "#;

    let tree = parse(source);
    assert_eq!(tree.kind(), SyntaxKind::SourceFile);

    let use_decl = tree.children().find(|n| n.kind() == SyntaxKind::UseDecl);
    assert!(use_decl.is_some(), "Expected UseDecl node");
}

#[test]
fn test_external_module() {
    let source = r#"
        mod foo;
    "#;

    let tree = parse(source);
    assert_eq!(tree.kind(), SyntaxKind::SourceFile);

    let mod_decl = tree.children().find(|n| n.kind() == SyntaxKind::ModuleDecl);
    assert!(mod_decl.is_some(), "Expected ModuleDecl node");
}

#[test]
fn test_inline_module() {
    let source = r#"
        mod foo {
            entity Bar {
                in x: bit[8]
                out y: bit[8]
            }
        }
    "#;

    let tree = parse(source);
    assert_eq!(tree.kind(), SyntaxKind::SourceFile);

    let mod_decl = tree.children().find(|n| n.kind() == SyntaxKind::ModuleDecl);
    assert!(mod_decl.is_some(), "Expected ModuleDecl node");
}

#[test]
fn test_pub_module() {
    let source = r#"
        pub mod foo {
            entity Bar {
                in x: bit[8]
                out y: bit[8]
            }
        }
    "#;

    let tree = parse(source);
    assert_eq!(tree.kind(), SyntaxKind::SourceFile);

    let mod_decl = tree.children().find(|n| n.kind() == SyntaxKind::ModuleDecl);
    assert!(
        mod_decl.is_some(),
        "Expected ModuleDecl node with pub visibility"
    );
}

#[test]
fn test_pub_crate_visibility() {
    let source = r#"
        pub(crate) mod foo;
    "#;

    let tree = parse(source);
    assert_eq!(tree.kind(), SyntaxKind::SourceFile);

    let mod_decl = tree.children().find(|n| n.kind() == SyntaxKind::ModuleDecl);
    assert!(
        mod_decl.is_some(),
        "Expected ModuleDecl node with pub(crate) visibility"
    );
}

#[test]
fn test_module_with_use_statements() {
    let source = r#"
        mod foo {
            use bar::Baz;

            entity Test {
                in x: bit[8]
                out y: bit[8]
            }
        }
    "#;

    let tree = parse(source);
    assert_eq!(tree.kind(), SyntaxKind::SourceFile);

    let mod_decl = tree.children().find(|n| n.kind() == SyntaxKind::ModuleDecl);
    assert!(mod_decl.is_some(), "Expected ModuleDecl node");
}
