//! Test: Phase 1 Generics Applied to Real Karythra Pattern
//!
//! This test validates that generic functions work with the actual
//! Karythra CLE execution pattern.

#[cfg(test)]
mod test_karythra_generics {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::Monomorphizer;

    #[test]
    fn test_generic_exec_karythra_pattern() {
        let skalp_code = r#"
    /// Generic execution function with parameterized bit width
    pub fn exec_generic<const W: nat>(opcode: bit[6], data1: bit[256], data2: bit[256]) -> bit[256] {
        // Extract W-bit operands from 256-bit content words
        let a = data1[31:0];  // Simplified: use fixed 32-bit for now
        let b = data2[31:0];

        // Compute result
        let result = match opcode {
            0 => a + b,     // ADD
            1 => a - b,     // SUB
            2 => a * b,     // MUL
            3 => a & b,     // AND
            4 => a | b,     // OR
            5 => a ^ b,     // XOR
            _ => a          // Default
        };

        // Pack result back into 256-bit word
        return result as bit[256];
    }

    /// Test entity that uses generic function with explicit type arguments
    entity TestGenericExec {
        in clk: clock
        in opcode: bit[6]
        in data1: bit[256]
        in data2: bit[256]
        out result32: bit[256]
        out result64: bit[256]
    }

    impl TestGenericExec {
        signal state32: bit[256] = 0
        signal state64: bit[256] = 0

        on(clk.rise) {
            // Call with 32-bit width (should generate exec_generic_c32)
            state32 <= exec_generic::<32>(opcode, data1, data2);
            result32 = state32;

            // Call with 64-bit width (should generate exec_generic_c64)
            state64 <= exec_generic::<64>(opcode, data1, data2);
            result64 = state64;
        }
    }
        "#;

        println!("\n=== Testing Generic Functions with Karythra Pattern ===");

        // Step 1: Parse to HIR
        let hir = match parse_and_build_hir(skalp_code) {
            Ok(h) => {
                println!("✅ Parsed to HIR successfully");
                println!("   Functions: {}", h.functions.len());
                for func in &h.functions {
                    println!("   - {} (generics: {})", func.name, func.generics.len());
                }
                h
            }
            Err(e) => {
                panic!("❌ Failed to parse: {:?}", e);
            }
        };

        // Verify we have a generic function
        assert_eq!(hir.functions.len(), 1, "Should have 1 function");
        let func = &hir.functions[0];
        assert_eq!(func.name, "exec_generic");
        assert!(!func.generics.is_empty(), "Function should have generics");
        println!("\n✅ Found generic function: {} with {} generic params",
                 func.name, func.generics.len());

        // Step 2: Monomorphize
        println!("\n=== Running Monomorphization Pass ===");
        let mut monomorphizer = Monomorphizer::new();
        let monomorphized_hir = monomorphizer.monomorphize(&hir);

        println!("✅ Monomorphization completed");

        // Step 3: Verify specialized functions were generated
        println!("\n=== Verifying Specializations ===");
        println!("Functions after monomorphization:");
        for func in &monomorphized_hir.functions {
            println!("   - {} (generics: {})", func.name, func.generics.len());
        }

        // Should have specialized functions for 32 and 64
        let specialized_functions: Vec<_> = monomorphized_hir.functions.iter()
            .filter(|f| f.name.starts_with("exec_generic_") && f.generics.is_empty())
            .collect();

        println!("\nFound {} specialized functions:", specialized_functions.len());
        for func in &specialized_functions {
            println!("   - {}", func.name);
        }

        // We should have at least 2 specializations (c32 and c64)
        assert!(specialized_functions.len() >= 2,
                "Expected at least 2 specializations (c32 and c64), got {}",
                specialized_functions.len());

        // Check that each specialization has no generics
        for func in &specialized_functions {
            assert_eq!(func.generics.len(), 0,
                       "Specialized function {} should have no generics", func.name);
        }

        println!("\n✅ SUCCESS: Generic Karythra pattern works!");
        println!("   - Generic function defined with <const W: nat>");
        println!("   - Called with explicit type arguments ::<32> and ::<64>");
        println!("   - Monomorphizer generated {} specialized functions",
                 specialized_functions.len());
        println!("   - All specializations have concrete types (no generics)");
    }

    #[test]
    fn test_multiple_widths_same_pattern() {
        let skalp_code = r#"
    // Simple generic function
    fn add_width<const W: nat>(a: bit[32], b: bit[32]) -> bit[32] {
        return a + b
    }

    entity TestMultiWidth {
        in clk: clock
        in x: bit[32]
        in y: bit[32]
        out r8: bit[32]
        out r16: bit[32]
        out r32: bit[32]
        out r64: bit[32]
    }

    impl TestMultiWidth {
        on(clk.rise) {
            r8 = add_width::<8>(x, y);
            r16 = add_width::<16>(x, y);
            r32 = add_width::<32>(x, y);
            r64 = add_width::<64>(x, y);
        }
    }
        "#;

        println!("\n=== Testing Multiple Width Specializations ===");

        let hir = parse_and_build_hir(skalp_code)
            .expect("Failed to parse");

        let mut monomorphizer = Monomorphizer::new();
        let monomorphized_hir = monomorphizer.monomorphize(&hir);

        // Should have 4 specializations
        let specialized = monomorphized_hir.functions.iter()
            .filter(|f| f.name.starts_with("add_width_c") && f.generics.is_empty())
            .count();

        println!("Generated {} specializations", specialized);
        assert_eq!(specialized, 4, "Expected 4 specializations (8, 16, 32, 64)");

        println!("✅ Multiple width specializations work!");
    }
}
