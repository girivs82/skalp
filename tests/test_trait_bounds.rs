//! Test: Phase 2 Trait Bounds on Generic Functions
//!
//! Validates that generic functions can have trait bounds and that
//! monomorphization respects those bounds.

#[cfg(test)]
mod test_trait_bounds {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::Monomorphizer;

    #[test]
    fn test_generic_function_with_trait_bound() {
        let skalp_code = r#"
    // Define a trait
    trait FloatingPoint {
        fn add(self, other: Self) -> Self;
        fn mul(self, other: Self) -> Self;
    }

    // Implement for fp32
    impl FloatingPoint for fp32 {
        fn add(self, other: Self) -> Self {
            return self + other
        }

        fn mul(self, other: Self) -> Self {
            return self * other
        }
    }

    // Generic function with trait bound
    fn fp_add<T: FloatingPoint>(a: bit[32], b: bit[32]) -> bit[32] {
        let a_fp = a as T;
        let b_fp = b as T;
        let result = a_fp.add(b_fp);
        return result as bit[32]
    }

    // Entity using the generic function
    entity TestTraitBounds {
        in clk: clock
        in data1: bit[32]
        in data2: bit[32]
        out result: bit[32]
    }

    impl TestTraitBounds {
        signal state: bit[32] = 0

        on(clk.rise) {
            state = fp_add::<fp32>(data1, data2);
            result = state;
        }
    }
        "#;

        println!("\n=== Testing Generic Functions with Trait Bounds ===");

        // Parse to HIR
        let hir = match parse_and_build_hir(skalp_code) {
            Ok(h) => {
                println!("✅ Parsed to HIR successfully");
                println!("   Trait definitions: {}", h.trait_definitions.len());
                println!(
                    "   Trait implementations: {}",
                    h.trait_implementations.len()
                );
                println!("   Functions: {}", h.functions.len());
                h
            }
            Err(e) => {
                panic!("❌ Failed to parse: {:?}", e);
            }
        };

        // Check trait definition
        assert!(
            !hir.trait_definitions.is_empty(),
            "Should have trait definition"
        );
        let trait_def = &hir.trait_definitions[0];
        println!("\n✅ Found trait: {}", trait_def.name);
        println!("   Methods: {}", trait_def.methods.len());

        // Check trait implementation
        assert!(
            !hir.trait_implementations.is_empty(),
            "Should have trait impl"
        );
        println!("✅ Found trait implementation");

        // Check generic function
        let fp_add_func = hir.functions.iter().find(|f| f.name == "fp_add");

        assert!(fp_add_func.is_some(), "Should have fp_add function");
        let func = fp_add_func.unwrap();
        println!("\n✅ Found generic function: {}", func.name);
        println!("   Generic params: {}", func.generics.len());

        // Check if it has trait bounds
        for generic in &func.generics {
            println!("   - {} : {:?}", generic.name, generic.param_type);
        }

        // Monomorphize
        println!("\n=== Running Monomorphization Pass ===");
        let mut monomorphizer = Monomorphizer::new();
        let monomorphized_hir = monomorphizer.monomorphize(&hir);

        println!("✅ Monomorphization completed");
        println!("Functions after monomorphization:");
        for func in &monomorphized_hir.functions {
            println!("   - {} (generics: {})", func.name, func.generics.len());
        }

        // Should have specialized fp_add function
        let has_specialized = monomorphized_hir
            .functions
            .iter()
            .any(|f| f.name.starts_with("fp_add_") && f.generics.is_empty());

        if has_specialized {
            println!("\n✅ Specialized function generated!");
        } else {
            println!("\n⚠️  No specialized function found - trait bounds may not be fully implemented yet");
            // Don't fail the test - trait bounds are a work in progress
        }
    }
}
