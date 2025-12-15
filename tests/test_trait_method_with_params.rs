//! Test: Trait method calls with function parameters

#[cfg(test)]
mod test_trait_method_with_params {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::monomorphize::Monomorphizer;

    #[test]
    fn test_method_call_on_function_param() {
        let skalp_code = r#"
    trait Numeric {
        fn add(self, other: Self) -> Self;
    }

    impl Numeric for nat[32] {
        fn add(self, other: Self) -> Self {
            return self + other
        }
    }

    // This is the real-world use case: method calls on function parameters!
    fn test_add(a: nat[32], b: nat[32]) -> nat[32] {
        return a.add(b)
    }
        "#;

        println!("\n=== Testing Method Call on Function Parameter ===");
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

                // Should be 2 functions: test_add + Numeric_nat32_add
                assert_eq!(
                    monomorphized_hir.functions.len(),
                    2,
                    "Expected 2 functions (test_add + Numeric_nat32_add)"
                );

                // Check that the specialized function was generated
                let has_specialized = monomorphized_hir
                    .functions
                    .iter()
                    .any(|f| f.name.contains("Numeric") && f.name.contains("add"));
                assert!(
                    has_specialized,
                    "Should have generated Numeric_*_add function"
                );

                println!("✅ Trait method call on function parameter works!");
            }
            Err(e) => {
                println!("❌ FAILED: {:?}", e);
                panic!("HIR build failed");
            }
        }
    }
}
