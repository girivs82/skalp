//! Test that generic function calls are parsed correctly

#[cfg(test)]
mod test_parse_generics {
    use skalp_frontend::parse_and_build_hir;

    #[test]
    fn test_parse_generic_call() {
        let skalp_code = r#"
    fn add<const W: nat>(a: bit[W], b: bit[W]) -> bit[W] {
        return a + b
    }

    entity Test {
        in x: bit[32]
        in y: bit[32]
        out z: bit[32]
    }

    impl Test {
        signal temp: bit[32] = 0

        on(_) {
            // This call should have type_args
            temp = add::<32>(x, y);
            z = temp;
        }
    }
        "#;

        let hir = parse_and_build_hir(skalp_code)
            .expect("Failed to parse");

        println!("\n=== Checking HIR Structure ===");
        println!("Functions: {}", hir.functions.len());
        println!("Implementations: {}", hir.implementations.len());

        // Check if function has generics
        if let Some(func) = hir.functions.first() {
            println!("\nFunction: {}", func.name);
            println!("  Generics: {}", func.generics.len());
        }

        // Check if implementation has event blocks
        if let Some(impl_block) = hir.implementations.first() {
            println!("\nImplementation:");
            println!("  Event blocks: {}", impl_block.event_blocks.len());

            if let Some(event_block) = impl_block.event_blocks.first() {
                println!("  Event block statements: {}", event_block.statements.len());

                // Look for the assignment statement
                for stmt in &event_block.statements {
                    if let skalp_frontend::hir::HirStatement::Assignment(assign) = stmt {
                        println!("\n  Found assignment:");

                        // Check if RHS is a call
                        if let skalp_frontend::hir::HirExpression::Call(call) = &assign.rhs {
                            println!("    Function: {}", call.function);
                            println!("    Type args: {}", call.type_args.len());
                            println!("    Value args: {}", call.args.len());

                            // Print type args
                            for (i, ty) in call.type_args.iter().enumerate() {
                                println!("      Type arg [{}]: {:?}", i, ty);
                            }

                            // This should be 1 (the `::<32>` part)
                            assert!(call.type_args.len() > 0,
                                "Expected at least one type argument for add::<32> call, got {}",
                                call.type_args.len());

                            println!("\n✅ Type arguments were parsed correctly!");
                        } else {
                            println!("    RHS is not a call expression: {:?}", assign.rhs);
                        }
                    }
                }
            }
        }
    }
}
