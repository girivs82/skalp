#[cfg(test)]
mod debug_assert {
    use skalp_frontend::parse_and_build_hir;

    #[test]
    fn debug_simple_assert() {
        // Try just the assert statement alone
        let source = r#"
        entity TestModule {
            in clk: clock
        }

        impl TestModule {
            on(clk.rise) {
                assert(true, "Test message");
            }
        }
        "#;

        match parse_and_build_hir(source) {
            Ok(hir) => {
                println!("✅ Simple assert parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());
                if !hir.implementations.is_empty() {
                    println!(
                        "Event blocks: {}",
                        hir.implementations[0].event_blocks.len()
                    );
                    if !hir.implementations[0].event_blocks.is_empty() {
                        println!(
                            "Statements in first event block: {}",
                            hir.implementations[0].event_blocks[0].statements.len()
                        );
                    }
                }
            }
            Err(e) => {
                println!("❌ Simple assert failed: {:?}", e);
            }
        }
    }

    #[test]
    fn debug_entity_only() {
        // Try just the entity without assert
        let source = r#"
        entity TestModule {
            in clk: clock
        }

        impl TestModule {
            on(clk.rise) {
                // Empty for now
            }
        }
        "#;

        match parse_and_build_hir(source) {
            Ok(hir) => {
                println!("✅ Entity without assert parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());
            }
            Err(e) => {
                println!("❌ Entity without assert failed: {:?}", e);
            }
        }
    }
}
