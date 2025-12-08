//! Tests for for loop and unroll attribute support

#[cfg(test)]
mod unroll_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_frontend::hir::{HirStatement, UnrollConfig};
    use skalp_mir::{MirCompiler, OptimizationLevel};

    #[test]
    fn test_basic_for_loop_parse() {
        let source = r#"
        /// Basic for loop without unroll
        pub fn count_bits(input: bit[8]) -> bit[4] {
            let mut count = 0 as bit[4];
            for i in 0..8 {
                count = count + ((input >> i) & 1) as bit[4];
            }
            return count;
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse for loop");

        // Check that we have a function
        assert_eq!(hir.functions.len(), 1);
        let func = &hir.functions[0];
        assert_eq!(func.name, "count_bits");

        // Find the for loop in the function body
        let has_for_loop = func.body.iter().any(|stmt| {
            matches!(stmt, HirStatement::For(_))
        });
        assert!(has_for_loop, "Should have a for loop statement");

        println!("Basic for loop parsing test passed");
    }

    #[test]
    fn test_unroll_full_attribute_parse() {
        let source = r#"
        /// Full unroll with #[unroll] attribute
        pub fn reverse_4bits(input: bit[4]) -> bit[4] {
            let mut result = 0 as bit[4];
            #[unroll]
            for i in 0..4 {
                result = result | ((input >> i) & 1) << (3 - i);
            }
            return result;
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse unroll attribute");

        // Check that we have a function
        assert_eq!(hir.functions.len(), 1);
        let func = &hir.functions[0];

        // Find the for loop and check it has unroll config
        let for_loop = func.body.iter().find_map(|stmt| {
            if let HirStatement::For(for_stmt) = stmt {
                Some(for_stmt)
            } else {
                None
            }
        });

        assert!(for_loop.is_some(), "Should have a for loop");
        let for_loop = for_loop.unwrap();

        // Check that unroll is set to Full
        assert!(for_loop.unroll.is_some(), "For loop should have unroll config");
        assert_eq!(for_loop.unroll.as_ref().unwrap(), &UnrollConfig::Full, "Unroll should be Full");

        println!("Full unroll attribute test passed");
    }

    #[test]
    fn test_unroll_factor_attribute_parse() {
        let source = r#"
        /// Partial unroll with #[unroll(4)] attribute
        pub fn sum_array(input: bit[8]) -> bit[16] {
            let mut sum = 0 as bit[16];
            #[unroll(4)]
            for i in 0..8 {
                sum = sum + (input >> i) as bit[16];
            }
            return sum;
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse unroll(N) attribute");

        // Check that we have a function
        assert_eq!(hir.functions.len(), 1);
        let func = &hir.functions[0];

        // Find the for loop and check it has unroll config
        let for_loop = func.body.iter().find_map(|stmt| {
            if let HirStatement::For(for_stmt) = stmt {
                Some(for_stmt)
            } else {
                None
            }
        });

        assert!(for_loop.is_some(), "Should have a for loop");
        let for_loop = for_loop.unwrap();

        // Check that unroll is set to Factor(4)
        assert!(for_loop.unroll.is_some(), "For loop should have unroll config");
        assert_eq!(for_loop.unroll.as_ref().unwrap(), &UnrollConfig::Factor(4), "Unroll should be Factor(4)");

        println!("Partial unroll(4) attribute test passed");
    }

    #[test]
    fn test_for_loop_mir_conversion() {
        let source = r#"
        /// Test for loop conversion to MIR
        pub fn xor_fold(input: bit[8]) -> bit[1] {
            let mut result = 0 as bit[1];
            for i in 0..8 {
                result = result ^ ((input >> i) & 1) as bit[1];
            }
            return result;
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse");

        // Verify function exists in HIR
        assert_eq!(hir.functions.len(), 1);
        let func = &hir.functions[0];
        assert_eq!(func.name, "xor_fold");

        // Verify the function has a for loop
        let has_for_loop = func.body.iter().any(|stmt| {
            matches!(stmt, HirStatement::For(_))
        });
        assert!(has_for_loop, "Should have a for loop in function body");

        // Convert to MIR - this tests that MIR conversion doesn't crash
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let mir = compiler.compile_to_mir(&hir).expect("Failed to compile to MIR");

        println!("MIR conversion successful for function with for loop");
        println!("MIR name: {}", mir.name);

        println!("For loop MIR conversion test passed");
    }

    #[test]
    fn test_unrolled_loop_mir_conversion() {
        let source = r#"
        /// Test unrolled loop generates expanded statements
        pub fn bit_reverse_8(input: bit[8]) -> bit[8] {
            let mut result = 0 as bit[8];
            #[unroll]
            for i in 0..8 {
                result = result | ((input >> i) & 1) << (7 - i);
            }
            return result;
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse");

        // Verify function exists in HIR
        assert_eq!(hir.functions.len(), 1);
        let func = &hir.functions[0];
        assert_eq!(func.name, "bit_reverse_8");

        // Verify the function has an unrolled for loop
        let for_loop = func.body.iter().find_map(|stmt| {
            if let HirStatement::For(for_stmt) = stmt {
                Some(for_stmt)
            } else {
                None
            }
        });
        assert!(for_loop.is_some(), "Should have a for loop");
        assert_eq!(for_loop.unwrap().unroll, Some(UnrollConfig::Full), "For loop should be marked for full unroll");

        // Convert to MIR - this tests that MIR conversion with unrolling doesn't crash
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let mir = compiler.compile_to_mir(&hir).expect("Failed to compile to MIR");

        println!("MIR conversion successful for function with unrolled for loop");
        println!("MIR name: {}", mir.name);

        println!("Unrolled loop MIR conversion test passed");
    }
}
