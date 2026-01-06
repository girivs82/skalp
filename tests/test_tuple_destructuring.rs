// Tests for tuple destructuring from function returns
// BUG FIX #85: Verifies that tuple elements are correctly mapped to their
// respective result signals (result_0, result_1, result_2, etc.)

#[cfg(test)]
mod tuple_destructuring_tests {
    use skalp_frontend::{parse_and_build_hir, parse_and_build_hir_from_file};
    use skalp_mir::lower_to_mir;
    use skalp_sim::{SimulationConfig, Simulator};
    use std::io::Write;

    async fn setup_cpu_simulator_with_entity(source: &str, entity_name: &str) -> Simulator {
        // Parse and build HIR (for non-stdlib tests)
        let hir = parse_and_build_hir(source).expect("Failed to parse design");

        // Compile to MIR
        let mir = lower_to_mir(&hir).expect("Failed to compile to MIR");

        // Find the entity module by name (functions may be synthesized as separate modules)
        assert!(!mir.modules.is_empty());
        let top_module = mir
            .modules
            .iter()
            .find(|m| m.name == entity_name)
            .unwrap_or_else(|| {
                panic!(
                    "Entity '{}' not found in MIR modules: {:?}",
                    entity_name,
                    mir.modules.iter().map(|m| &m.name).collect::<Vec<_>>()
                )
            });
        let sir = skalp_sir::convert_mir_to_sir_with_hierarchy(&mir, top_module);

        // Create CPU simulator
        let config = SimulationConfig {
            use_gpu: false, // CPU simulation
            max_cycles: 100,
            timeout_ms: 5000,
            capture_waveforms: false,
            parallel_threads: 1,
        };

        let mut simulator = Simulator::new(config)
            .await
            .expect("Failed to create simulator");

        simulator
            .load_module(&sir)
            .await
            .expect("Failed to load module");

        simulator
    }

    async fn setup_cpu_simulator_with_stdlib(source: &str, entity_name: &str) -> Simulator {
        // Set stdlib path
        std::env::set_var("SKALP_STDLIB_PATH", "./crates/skalp-stdlib");

        // Write source to temp file for module resolution
        let temp_dir = std::env::temp_dir();
        let temp_file = temp_dir.join("tuple_fp_test.sk");
        let mut file = std::fs::File::create(&temp_file).expect("Failed to create temp file");
        file.write_all(source.as_bytes())
            .expect("Failed to write temp file");

        // Parse with module resolution (loads stdlib imports)
        let hir =
            parse_and_build_hir_from_file(&temp_file).expect("Failed to parse design with stdlib");

        // Compile to MIR
        let mir = lower_to_mir(&hir).expect("Failed to compile to MIR");

        // Find the entity module by name
        assert!(!mir.modules.is_empty());
        let top_module = mir
            .modules
            .iter()
            .find(|m| m.name == entity_name)
            .unwrap_or_else(|| {
                panic!(
                    "Entity '{}' not found in MIR modules: {:?}",
                    entity_name,
                    mir.modules.iter().map(|m| &m.name).collect::<Vec<_>>()
                )
            });
        let sir = skalp_sir::convert_mir_to_sir_with_hierarchy(&mir, top_module);

        // Create CPU simulator
        let config = SimulationConfig {
            use_gpu: false,
            max_cycles: 100,
            timeout_ms: 5000,
            capture_waveforms: false,
            parallel_threads: 1,
        };

        let mut simulator = Simulator::new(config)
            .await
            .expect("Failed to create simulator");

        simulator
            .load_module(&sir)
            .await
            .expect("Failed to load module");

        simulator
    }

    /// Test basic tuple return with two elements
    /// Verifies that (a, b) destructuring works correctly
    #[tokio::test]
    async fn test_tuple_two_elements() {
        let source = r#"
        fn swap(x: bit[32], y: bit[32]) -> (bit[32], bit[32]) {
            return (y, x)
        }

        entity TupleSwap {
            in a: bit[32]
            in b: bit[32]
            out first: bit[32]
            out second: bit[32]
        }

        impl TupleSwap {
            let (f, s) = swap(a, b);
            first = f
            second = s
        }
        "#;

        let mut sim = setup_cpu_simulator_with_entity(source, "TupleSwap").await;

        // Test: swap(10, 20) should return (20, 10)
        sim.set_input("a", 10u32.to_le_bytes().to_vec())
            .await
            .unwrap();
        sim.set_input("b", 20u32.to_le_bytes().to_vec())
            .await
            .unwrap();
        sim.step_simulation().await.unwrap();

        let first_bytes = sim.get_output("first").await.unwrap();
        let second_bytes = sim.get_output("second").await.unwrap();

        let first = u32::from_le_bytes([
            first_bytes[0],
            first_bytes[1],
            first_bytes[2],
            first_bytes[3],
        ]);
        let second = u32::from_le_bytes([
            second_bytes[0],
            second_bytes[1],
            second_bytes[2],
            second_bytes[3],
        ]);

        assert_eq!(first, 20, "First element should be 20 (swapped from b)");
        assert_eq!(second, 10, "Second element should be 10 (swapped from a)");
    }

    /// Test tuple return with three elements including a boolean
    /// This is the exact pattern that triggered BUG #85
    #[tokio::test]
    async fn test_tuple_three_elements_with_bool() {
        let source = r#"
        fn divide_safe(a: bit[32], b: bit[32]) -> (bit, bit[32], bit[32]) {
            if b == 0 {
                return (0, 0, 0)
            }
            let quotient = a / b;
            let remainder = a % b;
            return (1, quotient, remainder)
        }

        entity SafeDivide {
            in dividend: bit[32]
            in divisor: bit[32]
            out valid: bit[32]
            out quotient: bit[32]
            out remainder: bit[32]
        }

        impl SafeDivide {
            let (v, q, r) = divide_safe(dividend, divisor);
            valid = v as bit[32]
            quotient = q
            remainder = r
        }
        "#;

        let mut sim = setup_cpu_simulator_with_entity(source, "SafeDivide").await;

        // Test: 17 / 5 = quotient 3, remainder 2
        sim.set_input("dividend", 17u32.to_le_bytes().to_vec())
            .await
            .unwrap();
        sim.set_input("divisor", 5u32.to_le_bytes().to_vec())
            .await
            .unwrap();
        sim.step_simulation().await.unwrap();

        let valid_bytes = sim.get_output("valid").await.unwrap();
        let quotient_bytes = sim.get_output("quotient").await.unwrap();
        let remainder_bytes = sim.get_output("remainder").await.unwrap();

        let valid = u32::from_le_bytes([
            valid_bytes[0],
            valid_bytes[1],
            valid_bytes[2],
            valid_bytes[3],
        ]);
        let quotient = u32::from_le_bytes([
            quotient_bytes[0],
            quotient_bytes[1],
            quotient_bytes[2],
            quotient_bytes[3],
        ]);
        let remainder = u32::from_le_bytes([
            remainder_bytes[0],
            remainder_bytes[1],
            remainder_bytes[2],
            remainder_bytes[3],
        ]);

        assert_eq!(valid, 1, "Division should be valid (divisor != 0)");
        assert_eq!(quotient, 3, "17 / 5 = 3");
        assert_eq!(remainder, 2, "17 % 5 = 2");
    }

    /// Test tuple with division by zero (invalid case)
    #[tokio::test]
    async fn test_tuple_invalid_case() {
        let source = r#"
        fn divide_safe(a: bit[32], b: bit[32]) -> (bit, bit[32], bit[32]) {
            if b == 0 {
                return (0, 0, 0)
            }
            let quotient = a / b;
            let remainder = a % b;
            return (1, quotient, remainder)
        }

        entity SafeDivide {
            in dividend: bit[32]
            in divisor: bit[32]
            out valid: bit[32]
            out quotient: bit[32]
            out remainder: bit[32]
        }

        impl SafeDivide {
            let (v, q, r) = divide_safe(dividend, divisor);
            valid = v as bit[32]
            quotient = q
            remainder = r
        }
        "#;

        let mut sim = setup_cpu_simulator_with_entity(source, "SafeDivide").await;

        // Test: division by zero should return valid=0
        sim.set_input("dividend", 17u32.to_le_bytes().to_vec())
            .await
            .unwrap();
        sim.set_input("divisor", 0u32.to_le_bytes().to_vec())
            .await
            .unwrap();
        sim.step_simulation().await.unwrap();

        let valid_bytes = sim.get_output("valid").await.unwrap();
        let valid = u32::from_le_bytes([
            valid_bytes[0],
            valid_bytes[1],
            valid_bytes[2],
            valid_bytes[3],
        ]);

        assert_eq!(valid, 0, "Division by zero should return valid=0");
    }

    /// Test floating-point tuple return (quadratic solver pattern)
    /// This is the exact test case that exposed BUG #85
    #[ignore = "requires stdlib parsing support for fp.sk advanced syntax"]
    #[tokio::test]
    async fn test_tuple_fp32_quadratic_solver() {
        let source = r#"
        use skalp::numeric::fp::*;
        use skalp::numeric::formats::fp32;

        fn fp_mul(a: fp32, b: fp32) -> fp32 {
            return a * b
        }

        fn fp_sub(a: fp32, b: fp32) -> fp32 {
            return a - b
        }

        fn fp_add(a: fp32, b: fp32) -> fp32 {
            return a + b
        }

        fn fp_div(a: fp32, b: fp32) -> fp32 {
            return a / b
        }

        fn fp_sqrt(x: fp32) -> fp32 {
            // Hardware sqrt - simplified for test
            return x.sqrt()
        }

        fn fp_lt(a: fp32, b: fp32) -> bit {
            return a < b
        }

        fn fp_neg(x: fp32) -> fp32 {
            return -x
        }

        /// Quadratic Solver: Solves ax² + bx + c = 0
        /// Returns (valid, x1, x2) where valid indicates if real solutions exist
        fn quadratic_solve(a: fp32, b: fp32, c: fp32) -> (bit, fp32, fp32) {
            // Compute discriminant: b² - 4ac
            let four_fp: fp32 = 4.0 as fp32;
            let b_squared = fp_mul(b, b);
            let four_a = fp_mul(four_fp, a);
            let four_ac = fp_mul(four_a, c);
            let discriminant = fp_sub(b_squared, four_ac);

            let zero_fp: fp32 = 0.0 as fp32;

            // Check if discriminant is negative (no real solutions)
            if fp_lt(discriminant, zero_fp) != 0 {
                return (0, 0 as fp32, 0 as fp32)
            }

            // Compute roots: x = (-b ± √discriminant) / 2a
            let sqrt_disc = fp_sqrt(discriminant);
            let two_fp: fp32 = 2.0 as fp32;
            let two_a = fp_mul(two_fp, a);
            let neg_b = fp_neg(b);

            let x1 = fp_div(fp_sub(neg_b, sqrt_disc), two_a);
            let x2 = fp_div(fp_add(neg_b, sqrt_disc), two_a);

            return (1, x1, x2)
        }

        entity QuadraticSolver {
            in a: fp32
            in b: fp32
            in c: fp32
            out valid: bit[32]
            out x1: fp32
            out x2: fp32
        }

        impl QuadraticSolver {
            let (v, r1, r2) = quadratic_solve(a, b, c);
            valid = v as bit[32]
            x1 = r1
            x2 = r2
        }
        "#;

        let mut sim = setup_cpu_simulator_with_stdlib(source, "QuadraticSolver").await;

        // Test: x² - 5x + 6 = 0 has roots x=2 and x=3
        // a=1.0, b=-5.0, c=6.0
        let a: f32 = 1.0;
        let b: f32 = -5.0;
        let c: f32 = 6.0;

        sim.set_input("a", a.to_bits().to_le_bytes().to_vec())
            .await
            .unwrap();
        sim.set_input("b", b.to_bits().to_le_bytes().to_vec())
            .await
            .unwrap();
        sim.set_input("c", c.to_bits().to_le_bytes().to_vec())
            .await
            .unwrap();
        sim.step_simulation().await.unwrap();

        let valid_bytes = sim.get_output("valid").await.unwrap();
        let x1_bytes = sim.get_output("x1").await.unwrap();
        let x2_bytes = sim.get_output("x2").await.unwrap();

        let valid = u32::from_le_bytes([
            valid_bytes[0],
            valid_bytes[1],
            valid_bytes[2],
            valid_bytes[3],
        ]);
        let x1 = f32::from_bits(u32::from_le_bytes([
            x1_bytes[0],
            x1_bytes[1],
            x1_bytes[2],
            x1_bytes[3],
        ]));
        let x2 = f32::from_bits(u32::from_le_bytes([
            x2_bytes[0],
            x2_bytes[1],
            x2_bytes[2],
            x2_bytes[3],
        ]));

        assert_eq!(valid, 1, "Quadratic should have real solutions");

        // Roots should be 2.0 and 3.0 (in some order)
        let roots_correct = ((x1 - 2.0).abs() < 0.01 && (x2 - 3.0).abs() < 0.01)
            || ((x1 - 3.0).abs() < 0.01 && (x2 - 2.0).abs() < 0.01);

        assert!(
            roots_correct,
            "Roots of x²-5x+6=0 should be 2.0 and 3.0, got x1={}, x2={}",
            x1, x2
        );
    }

    /// Test quadratic with no real solutions (negative discriminant)
    /// NOTE: This test is disabled because conditional early returns in synthesized
    /// functions are not yet properly handled (separate bug from tuple destructuring).
    /// The if-return pattern needs to be converted to mux logic during inlining.
    #[ignore = "requires stdlib parsing support for fp.sk advanced syntax"]
    #[tokio::test]
    async fn test_tuple_fp32_quadratic_no_real_roots() {
        let source = r#"
        use skalp::numeric::fp::*;
        use skalp::numeric::formats::fp32;

        fn fp_mul(a: fp32, b: fp32) -> fp32 {
            return a * b
        }

        fn fp_sub(a: fp32, b: fp32) -> fp32 {
            return a - b
        }

        fn fp_add(a: fp32, b: fp32) -> fp32 {
            return a + b
        }

        fn fp_div(a: fp32, b: fp32) -> fp32 {
            return a / b
        }

        fn fp_sqrt(x: fp32) -> fp32 {
            return x.sqrt()
        }

        fn fp_lt(a: fp32, b: fp32) -> bit {
            return a < b
        }

        fn fp_neg(x: fp32) -> fp32 {
            return -x
        }

        fn quadratic_solve(a: fp32, b: fp32, c: fp32) -> (bit, fp32, fp32) {
            let four_fp: fp32 = 4.0 as fp32;
            let b_squared = fp_mul(b, b);
            let four_a = fp_mul(four_fp, a);
            let four_ac = fp_mul(four_a, c);
            let discriminant = fp_sub(b_squared, four_ac);

            let zero_fp: fp32 = 0.0 as fp32;

            if fp_lt(discriminant, zero_fp) != 0 {
                return (0, 0 as fp32, 0 as fp32)
            }

            let sqrt_disc = fp_sqrt(discriminant);
            let two_fp: fp32 = 2.0 as fp32;
            let two_a = fp_mul(two_fp, a);
            let neg_b = fp_neg(b);

            let x1 = fp_div(fp_sub(neg_b, sqrt_disc), two_a);
            let x2 = fp_div(fp_add(neg_b, sqrt_disc), two_a);

            return (1, x1, x2)
        }

        entity QuadraticSolver {
            in a: fp32
            in b: fp32
            in c: fp32
            out valid: bit[32]
            out x1: fp32
            out x2: fp32
        }

        impl QuadraticSolver {
            let (v, r1, r2) = quadratic_solve(a, b, c);
            valid = v as bit[32]
            x1 = r1
            x2 = r2
        }
        "#;

        let mut sim = setup_cpu_simulator_with_stdlib(source, "QuadraticSolver").await;

        // Test: x² + x + 1 = 0 has no real roots (discriminant = 1 - 4 = -3)
        // a=1.0, b=1.0, c=1.0
        let a: f32 = 1.0;
        let b: f32 = 1.0;
        let c: f32 = 1.0;

        sim.set_input("a", a.to_bits().to_le_bytes().to_vec())
            .await
            .unwrap();
        sim.set_input("b", b.to_bits().to_le_bytes().to_vec())
            .await
            .unwrap();
        sim.set_input("c", c.to_bits().to_le_bytes().to_vec())
            .await
            .unwrap();
        sim.step_simulation().await.unwrap();

        let valid_bytes = sim.get_output("valid").await.unwrap();
        let valid = u32::from_le_bytes([
            valid_bytes[0],
            valid_bytes[1],
            valid_bytes[2],
            valid_bytes[3],
        ]);

        assert_eq!(
            valid, 0,
            "Quadratic x²+x+1=0 should have no real solutions (discriminant < 0)"
        );
    }

    /// Test tuple with four elements to verify general N-element support
    #[tokio::test]
    async fn test_tuple_four_elements() {
        let source = r#"
        fn compute_stats(a: bit[32], b: bit[32]) -> (bit[32], bit[32], bit[32], bit[32]) {
            let sum = a + b;
            let diff = a - b;
            let prod = a * b;
            let max_val = if a > b { a } else { b };
            return (sum, diff, prod, max_val)
        }

        entity Stats {
            in x: bit[32]
            in y: bit[32]
            out sum: bit[32]
            out diff: bit[32]
            out prod: bit[32]
            out max_val: bit[32]
        }

        impl Stats {
            let (s, d, p, m) = compute_stats(x, y);
            sum = s
            diff = d
            prod = p
            max_val = m
        }
        "#;

        let mut sim = setup_cpu_simulator_with_entity(source, "Stats").await;

        // Test: x=10, y=3
        sim.set_input("x", 10u32.to_le_bytes().to_vec())
            .await
            .unwrap();
        sim.set_input("y", 3u32.to_le_bytes().to_vec())
            .await
            .unwrap();
        sim.step_simulation().await.unwrap();

        let sum_bytes = sim.get_output("sum").await.unwrap();
        let diff_bytes = sim.get_output("diff").await.unwrap();
        let prod_bytes = sim.get_output("prod").await.unwrap();
        let max_bytes = sim.get_output("max_val").await.unwrap();

        let sum = u32::from_le_bytes([sum_bytes[0], sum_bytes[1], sum_bytes[2], sum_bytes[3]]);
        let diff = u32::from_le_bytes([diff_bytes[0], diff_bytes[1], diff_bytes[2], diff_bytes[3]]);
        let prod = u32::from_le_bytes([prod_bytes[0], prod_bytes[1], prod_bytes[2], prod_bytes[3]]);
        let max_val = u32::from_le_bytes([max_bytes[0], max_bytes[1], max_bytes[2], max_bytes[3]]);

        assert_eq!(sum, 13, "10 + 3 = 13");
        assert_eq!(diff, 7, "10 - 3 = 7");
        assert_eq!(prod, 30, "10 * 3 = 30");
        assert_eq!(max_val, 10, "max(10, 3) = 10");
    }
}
