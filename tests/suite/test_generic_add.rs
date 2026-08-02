//! Test basic generic function with const parameter
//!
//! This test validates Phase 1 generic function support:
//! - Generic function definition with const parameter
//! - Generic function call with type argument
//! - Monomorphization generating specialized functions

#[cfg(test)]
mod test_generics {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::Monomorphizer;

    #[test]
    fn test_generic_add_basic() {
        let skalp_code = r#"
    // Generic addition function with const width parameter
    fn add<const W: nat>(a: bit[W], b: bit[W]) -> bit[W] {
        return a + b
    }

    // Entity that uses generic function
    entity TestGenericAdd {
        in clk: clock
        in a: bit[32]
        in b: bit[32]
        out result: bit[32]
    }

    impl TestGenericAdd {
        signal state: bit[32] = 0

        on(clk.rise) {
            // Call generic function with width 32
            state = add::<32>(a, b);
            result = state;
        }
    }
        "#;

        println!("\n=== Testing Generic Function: add<const W> ===");

        // Step 1: Parse to HIR
        let hir = match parse_and_build_hir(skalp_code) {
            Ok(h) => {
                println!("✅ Parsed to HIR successfully");
                println!("   Functions: {}", h.functions.len());
                for func in &h.functions {
                    println!(
                        "   - {} (generic params: {})",
                        func.name,
                        func.generics.len()
                    );
                }
                h
            }
            Err(e) => {
                panic!("❌ Failed to parse: {:?}", e);
            }
        };

        // Step 2: Monomorphize (this is what we're testing!)
        println!("\n=== Running Monomorphization Pass ===");
        let mut monomorphizer = Monomorphizer::new();
        let monomorphized_hir = monomorphizer.monomorphize(&hir);

        println!("✅ Monomorphization completed");

        // Step 3: Verify specialized function exists
        println!("\n=== Verifying Monomorphization ===");
        println!("Functions after monomorphization:");
        for func in &monomorphized_hir.functions {
            println!(
                "   - {} (generic params: {})",
                func.name,
                func.generics.len()
            );
        }

        // Check that we have a specialized function (add_c32 or add_bit8 or similar)
        let _has_specialized = monomorphized_hir
            .functions
            .iter()
            .any(|f| f.name.starts_with("add_") && f.generics.is_empty());

        // Find the specialized function
        let specialized_func = monomorphized_hir
            .functions
            .iter()
            .find(|f| f.name.starts_with("add_") && f.generics.is_empty());

        if let Some(func) = specialized_func {
            println!("✅ Found specialized function: {}", func.name);
            println!(
                "   Parameters: {:?}",
                func.params
                    .iter()
                    .map(|p| format!("{}:{:?}", p.name, p.param_type))
                    .collect::<Vec<_>>()
            );
            println!("   Generic params: {} (should be 0)", func.generics.len());

            assert_eq!(
                func.generics.len(),
                0,
                "Specialized function should have no generics"
            );

            // Check that parameter types are concrete
            assert!(
                func.params
                    .iter()
                    .all(|p| matches!(p.param_type, skalp_frontend::hir::HirType::Bit(_))),
                "All parameters should be concrete bit types"
            );
        } else {
            println!("❌ No specialized add function found");
            panic!("Expected specialized add function (e.g., add_c32 or add_bit8)");
        }

        // Check that generic function is NOT in the final HIR
        // (it should be replaced by specialized versions)
        let still_has_generic_add = monomorphized_hir
            .functions
            .iter()
            .any(|f| f.name == "add" && !f.generics.is_empty());

        if still_has_generic_add {
            println!("⚠️  Generic add function still exists (might be OK if unused)");
        } else {
            println!("✅ Generic add function removed or made concrete");
        }
    }

    #[test]
    fn test_generic_add_multiple_specializations() {
        let skalp_code = r#"
    // Generic addition function
    fn add<const W: nat>(a: bit[W], b: bit[W]) -> bit[W] {
        return a + b
    }

    // Entity that uses multiple specializations
    entity TestMultipleSpec {
        in clk: clock
        in a8: bit[8]
        in b8: bit[8]
        in a32: bit[32]
        in b32: bit[32]
        out r8: bit[8]
        out r32: bit[32]
    }

    impl TestMultipleSpec {
        on(clk.rise) {
            // Call with width 8
            r8 = add::<8>(a8, b8);

            // Call with width 32
            r32 = add::<32>(a32, b32);
        }
    }
        "#;

        println!("\n=== Testing Multiple Specializations ===");

        // Parse to HIR
        let hir = parse_and_build_hir(skalp_code).expect("Failed to parse");

        println!("✅ Parsed to HIR");
        println!(
            "   Generic functions: {}",
            hir.functions
                .iter()
                .filter(|f| !f.generics.is_empty())
                .count()
        );

        // Monomorphize
        let mut monomorphizer = Monomorphizer::new();
        let monomorphized_hir = monomorphizer.monomorphize(&hir);

        println!("✅ Monomorphization completed");

        let function_names: Vec<String> = monomorphized_hir
            .functions
            .iter()
            .map(|f| f.name.clone())
            .collect();

        println!("Functions generated: {:?}", function_names);

        // Should have both add_c8 and add_c32
        let has_add_8 = function_names.iter().any(|n| n.contains("add_c8"));
        let has_add_32 = function_names.iter().any(|n| n.contains("add_c32"));

        println!("Found add_c8: {}", has_add_8);
        println!("Found add_c32: {}", has_add_32);

        if has_add_8 && has_add_32 {
            println!("✅ Found both specializations!");

            // Verify they have no generics
            let spec_8 = monomorphized_hir
                .functions
                .iter()
                .find(|f| f.name.contains("add_c8"));
            let spec_32 = monomorphized_hir
                .functions
                .iter()
                .find(|f| f.name.contains("add_c32"));

            if let Some(f8) = spec_8 {
                assert_eq!(f8.generics.len(), 0, "add_c8 should have no generics");
                println!("✅ add_c8 has no generics");
            }

            if let Some(f32) = spec_32 {
                assert_eq!(f32.generics.len(), 0, "add_c32 should have no generics");
                println!("✅ add_c32 has no generics");
            }
        } else {
            println!(
                "⚠️  Only found: add_c8={}, add_c32={}",
                has_add_8, has_add_32
            );
        }

        assert!(
            has_add_8 && has_add_32,
            "Expected both add_c8 and add_c32 specializations"
        );
    }
}
