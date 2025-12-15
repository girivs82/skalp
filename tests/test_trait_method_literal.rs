//! Test: Trait method calls with literal receivers

#[cfg(test)]
mod test_trait_method_literal {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::monomorphize::Monomorphizer;

    #[test]
    fn test_method_call_with_literal() {
        let skalp_code = r#"
    trait Numeric {
        fn add(self, other: Self) -> Self;
    }

    impl Numeric for nat[32] {
        fn add(self, other: Self) -> Self {
            return self + other
        }
    }

    fn test_literal() -> nat[32] {
        // Cast to make type explicit
        return (5 as nat[32]).add(3 as nat[32])
    }
        "#;

        println!("\n=== Testing Method Call with Explicit Types ===");
        println!("Code:\n{}", skalp_code);

        match parse_and_build_hir(skalp_code) {
            Ok(hir) => {
                println!("✅ HIR build succeeded");

                // Run monomorphization
                let mut mono = Monomorphizer::new();
                let monomorphized_hir = mono.monomorphize(&hir);

                println!("\n✅ Monomorphization completed");
                println!(
                    "Functions after monomorphization: {}",
                    monomorphized_hir.functions.len()
                );
            }
            Err(e) => {
                println!("❌ FAILED: {:?}", e);
                panic!("HIR build failed");
            }
        }
    }
}
