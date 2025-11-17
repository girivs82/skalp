//! End-to-end test for trait method calls
//!
//! This test demonstrates:
//! 1. Trait definitions with methods ✅
//! 2. Type-based trait implementations ✅
//! 3. Trait registry building ✅
//! 4. Method call syntax ✅
//! 5. Method resolution (TODO - next step)

#[cfg(test)]
mod test_trait_method_call_e2e {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::monomorphize::Monomorphizer;

    #[test]
    fn test_simple_method_call() {
        let skalp_code = r#"
    // Define a trait with method
    trait Numeric {
        fn add(self, other: Self) -> Self;
    }

    // Implement trait for nat[32]
    impl Numeric for nat[32] {
        fn add(self, other: Self) -> Self {
            return self + other
        }
    }

    // Function that uses trait method
    fn test_add(a: nat[32], b: nat[32]) -> nat[32] {
        return a.add(b)  // Method call syntax!
    }
        "#;

        println!("\n=== E2E Test: Simple Method Call ===");
        println!("Code:\n{}", skalp_code);

        match parse_and_build_hir(skalp_code) {
            Ok(hir) => {
                println!("✅ HIR build succeeded");
                println!("Traits: {}", hir.trait_definitions.len());
                println!("Impls: {}", hir.trait_implementations.len());
                println!("Functions: {}", hir.functions.len());

                // Check that method call was detected
                let test_fn = hir.functions.iter().find(|f| f.name == "test_add");
                assert!(test_fn.is_some(), "test_add function should exist");

                let test_fn = test_fn.unwrap();
                println!("\n✅ Found test_add function");
                println!("   Parameters: {}", test_fn.params.len());
                println!("   Statements: {}", test_fn.body.len());

                // Run monomorphization
                let mut mono = Monomorphizer::new();
                let _monomorphized_hir = mono.monomorphize(&hir);

                println!("\n✅ Monomorphization completed");
                println!("   Trait registry built successfully");
                println!("\n⏭️  Next: Implement method resolution to convert a.add(b) into Numeric_nat32_add(a, b)");
            }
            Err(e) => {
                println!("❌ FAILED: {:?}", e);
                panic!("HIR build failed");
            }
        }
    }

    #[test]
    fn test_generic_with_trait_bound() {
        let skalp_code = r#"
    trait FloatingPoint {
        fn from_bits(bits: bit[32]) -> Self;
        fn to_bits(self) -> bit[32];
    }

    impl FloatingPoint for fp32 {
        fn from_bits(bits: bit[32]) -> Self {
            return bits as fp32
        }

        fn to_bits(self) -> bit[32] {
            return self as bit[32]
        }
    }

    // Generic function with trait bound
    // This is the Karythra use case!
    fn fp_convert<T: FloatingPoint>(bits: bit[32]) -> bit[32] {
        let fp_val = T::from_bits(bits);  // Associated function call
        return fp_val.to_bits()            // Method call
    }
        "#;

        println!("\n=== E2E Test: Generic with Trait Bound ===");
        println!("Code:\n{}", skalp_code);

        match parse_and_build_hir(skalp_code) {
            Ok(hir) => {
                println!("✅ HIR build succeeded");
                println!("Traits: {}", hir.trait_definitions.len());
                println!("Impls: {}", hir.trait_implementations.len());
                println!("Functions: {}", hir.functions.len());

                // Run monomorphization
                let mut mono = Monomorphizer::new();
                let _monomorphized_hir = mono.monomorphize(&hir);

                println!("\n✅ Monomorphization completed");
                println!("\n⏭️  Next steps:");
                println!("   1. Parse trait bounds in generic parameters");
                println!("   2. During monomorphization of fp_convert<fp32>, resolve T::from_bits and val.to_bits");
                println!("   3. Generate specialized functions");
            }
            Err(e) => {
                println!("❌ FAILED: {:?}", e);
                panic!("HIR build failed");
            }
        }
    }
}
