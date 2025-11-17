//! Test: impl Trait for Type syntax

#[cfg(test)]
mod test_impl_for_type {
    use skalp_frontend::parse_and_build_hir;

    #[test]
    fn test_impl_for_primitive_type() {
        let skalp_code = r#"
    trait Addable {
        fn add(self, other: Self) -> Self;
    }

    impl Addable for nat[32] {
        fn add(self, other: Self) -> Self {
            return self + other
        }
    }
        "#;

        println!("\n=== Testing impl Trait for Type ===");
        println!("Code:\n{}", skalp_code);

        match parse_and_build_hir(skalp_code) {
            Ok(hir) => {
                println!("✅ SUCCESS!");
                println!("Traits: {}", hir.trait_definitions.len());
                println!("Impls: {}", hir.trait_implementations.len());
            }
            Err(e) => {
                println!("❌ FAILED: {:?}", e);
                panic!("Parse failed");
            }
        }
    }
}
