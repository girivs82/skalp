#[cfg(test)]
mod if_debug_tests {
    use skalp_frontend::parse_and_build_hir;

    #[test]
    fn test_if_else_parsing() {
        let simple_source = r#"
        entity TestIf {
            in clk: clock
            in rst: bit
            out result: nat[8]
        }

        impl TestIf {
            signal counter: nat[8] = 0

            on(clk.rise) {
                if (rst) {
                    counter = 0
                } else {
                    counter = counter + 1
                    result = counter + 2
                }
            }
        }
    "#;

        println!("=== Testing if-else parsing ===");
        let hir = parse_and_build_hir(simple_source).expect("Failed to parse");

        println!("=== HIR Event Block Assignments ===");
        for implementation in &hir.implementations {
            for event_block in &implementation.event_blocks {
                println!(
                    "Event block has {} statements",
                    event_block.statements.len()
                );
                for (i, stmt) in event_block.statements.iter().enumerate() {
                    println!("Statement {}: {:?}", i, stmt);
                }
            }
        }
    }
}
