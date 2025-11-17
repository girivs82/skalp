//! Test: Simple trait bound parsing

#[cfg(test)]
mod test_simple_trait_bound {
    use skalp_frontend::parse_and_build_hir;

    #[test]
    fn test_parse_type_param_with_bound() {
        // Simplest possible case: just a generic function with a trait bound
        let skalp_code = r#"
    fn test<T: FloatingPoint>(a: T) -> T {
        return a
    }
        "#;

        println!("\n=== Testing Simple Trait Bound Parsing ===");
        println!("Code:\n{}", skalp_code);

        match parse_and_build_hir(skalp_code) {
            Ok(hir) => {
                println!("✅ Parsed successfully!");
                println!("Functions: {}", hir.functions.len());

                if let Some(func) = hir.functions.first() {
                    println!("\nFunction: {}", func.name);
                    println!("Generic params: {}", func.generics.len());

                    for generic in &func.generics {
                        println!("  - {} : {:?}", generic.name, generic.param_type);
                    }

                    assert_eq!(func.generics.len(), 1);
                    let generic = &func.generics[0];
                    assert_eq!(generic.name, "T");

                    match &generic.param_type {
                        skalp_frontend::hir::HirGenericType::TypeWithBounds(bounds) => {
                            println!("\n✅ Found trait bounds: {:?}", bounds);
                            assert_eq!(bounds.len(), 1);
                            assert_eq!(bounds[0], "FloatingPoint");
                        }
                        other => {
                            panic!("❌ Expected TypeWithBounds, got {:?}", other);
                        }
                    }
                }
            }
            Err(e) => {
                panic!("❌ Failed to parse: {:?}", e);
            }
        }
    }

    #[test]
    fn test_parse_multiple_trait_bounds() {
        let skalp_code = r#"
    fn test<T: FloatingPoint + Comparable>(a: T, b: T) -> T {
        return a
    }
        "#;

        println!("\n=== Testing Multiple Trait Bounds ===");

        match parse_and_build_hir(skalp_code) {
            Ok(hir) => {
                println!("✅ Parsed successfully!");

                let func = hir.functions.first().unwrap();
                let generic = &func.generics[0];

                match &generic.param_type {
                    skalp_frontend::hir::HirGenericType::TypeWithBounds(bounds) => {
                        println!("✅ Found trait bounds: {:?}", bounds);
                        assert_eq!(bounds.len(), 2);
                        assert_eq!(bounds[0], "FloatingPoint");
                        assert_eq!(bounds[1], "Comparable");
                    }
                    other => {
                        panic!("❌ Expected TypeWithBounds, got {:?}", other);
                    }
                }
            }
            Err(e) => {
                panic!("❌ Failed to parse: {:?}", e);
            }
        }
    }
}
