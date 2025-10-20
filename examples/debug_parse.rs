use skalp_frontend::parse;
use skalp_frontend::syntax::SyntaxNode;

fn print_tree(node: &SyntaxNode, indent: usize) {
    println!("{}{:?} {:?}", "  ".repeat(indent), node.kind(), node.text());
    for child in node.children() {
        print_tree(&child, indent + 1);
    }
}

fn main() {
    // Test case 1: FAILING - nested if with 3 params
    let failing_source = r#"
fn clamp(x: bit[8], min: bit[8], max: bit[8]) -> bit[8] {
    return if x < min { min } else { if x > max { max } else { x } };
}

entity IfNestedTest {
    in clk: clock
    in x: bit[8]
    in min: bit[8]
    in max: bit[8]
    out result: bit[8]
}

impl IfNestedTest {
    result = clamp(x, min, max)
}
"#;

    // Test case 2: WORKING - simple if with 2 params
    let working_source = r#"
fn max(a: bit[8], b: bit[8]) -> bit[8] {
    return if a > b { a } else { b };
}

entity IfExprTest {
    in clk: clock
    in a: bit[8]
    in b: bit[8]
    out result: bit[8]
}

impl IfExprTest {
    result = max(a, b)
}
"#;

    println!("============================");
    println!("FAILING CASE (nested if, 3 params):");
    println!("============================");
    let (tree1, errors1) = parse::parse_with_errors(failing_source);
    println!("=== PARSE ERRORS ===");
    for error in &errors1 {
        println!("{:?}", error);
    }
    println!("\n=== FUNCTION DECL ===");
    for child in tree1.children() {
        let kind_str = format!("{:?}", child.kind());
        if kind_str.contains("Function") {
            print_tree(&child, 0);
        }
    }

    println!("\n\n============================");
    println!("WORKING CASE (simple if, 2 params):");
    println!("============================");
    let (tree2, errors2) = parse::parse_with_errors(working_source);
    println!("=== PARSE ERRORS ===");
    for error in &errors2 {
        println!("{:?}", error);
    }
    println!("\n=== FUNCTION DECL ===");
    for child in tree2.children() {
        let kind_str = format!("{:?}", child.kind());
        if kind_str.contains("Function") {
            print_tree(&child, 0);
        }
    }
}
