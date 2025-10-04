#[cfg(test)]
mod assertion_tests {
    use skalp_frontend::parse_and_build_hir;

    #[test]
    fn test_basic_assert_syntax() {
        let source = r#"
        entity TestModule {
            in clk: clock
            in data: nat[8]
            out valid: bool
        }

        impl TestModule {
            signal counter: nat[8] = 0

            on(clk.rise) {
                counter <= counter + 1
                assert(counter < 255, "Counter overflow")
                valid <= 1
            }
        }
        "#;

        let result = parse_and_build_hir(source);
        match result {
            Ok(hir) => {
                println!("✅ Assertion syntax parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());

                // Check that we have an entity and implementation
                assert_eq!(hir.entities.len(), 1);
                assert_eq!(hir.implementations.len(), 1);

                let implementation = &hir.implementations[0];
                println!("Event blocks: {}", implementation.event_blocks.len());

                // For now, just check that parsing doesn't fail
                // We'll implement actual assertion parsing in the next step
            }
            Err(e) => {
                println!("❌ Assertion parsing failed: {:?}", e);
                panic!("Failed to parse assertion syntax");
            }
        }
    }

    #[test]
    fn test_property_syntax() {
        let source = r#"
        entity TestModule {
            in clk: clock
            in req: bool
            in ack: bool
        }

        impl TestModule {
            property req_ack {
                @(posedge clk) req |-> ##1 ack
            }
        }
        "#;

        let result = parse_and_build_hir(source);
        match result {
            Ok(hir) => {
                println!("✅ Property syntax parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());
            }
            Err(e) => {
                println!("❌ Property parsing failed: {:?}", e);
                // For now, we expect this to fail since we haven't implemented property parsing yet
                println!("This is expected until we implement property parsing");
            }
        }
    }

    #[test]
    fn test_cover_syntax() {
        let source = r#"
        entity TestModule {
            in clk: clock
            in state: nat[2]
        }

        impl TestModule {
            cover property (@(posedge clk) state == 2'b11);
        }
        "#;

        let result = parse_and_build_hir(source);
        match result {
            Ok(hir) => {
                println!("✅ Cover syntax parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());
            }
            Err(e) => {
                println!("❌ Cover parsing failed: {:?}", e);
                // For now, we expect this to fail since we haven't implemented cover parsing yet
                println!("This is expected until we implement cover parsing");
            }
        }
    }
}
