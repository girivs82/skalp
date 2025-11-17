//! Test: Trait method registry building

#[cfg(test)]
mod test_trait_method_registry {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::monomorphize::Monomorphizer;

    #[test]
    fn test_trait_registry_builds() {
        let skalp_code = r#"
    trait Numeric {
        fn add(self, other: Self) -> Self;
        fn sub(self, other: Self) -> Self;
    }

    impl Numeric for nat[32] {
        fn add(self, other: Self) -> Self {
            return self + other
        }

        fn sub(self, other: Self) -> Self {
            return self - other
        }
    }

    impl Numeric for fp32 {
        fn add(self, other: Self) -> Self {
            return self + other
        }

        fn sub(self, other: Self) -> Self {
            return self - other
        }
    }
        "#;

        println!("\n=== Testing Trait Method Registry ===");
        println!("Code:\n{}", skalp_code);

        match parse_and_build_hir(skalp_code) {
            Ok(hir) => {
                println!("✅ HIR build succeeded");
                println!("Traits: {}", hir.trait_definitions.len());
                println!("Impls: {}", hir.trait_implementations.len());

                // Now test monomorphization with trait registry
                let mut mono = Monomorphizer::new();
                let _monomorphized_hir = mono.monomorphize(&hir);

                println!("✅ Monomorphization completed");
                println!("Check stderr output above for trait registry messages");
            }
            Err(e) => {
                println!("❌ FAILED: {:?}", e);
                panic!("HIR build failed");
            }
        }
    }
}
