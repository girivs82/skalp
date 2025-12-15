// Test for tuple destructuring in CPU simulation (Bug fix #85)
// This verifies that when a function returns a tuple, all elements
// are correctly extracted via TupleFieldAccess in the SIR path.

#[cfg(test)]
mod tuple_destructuring_sim_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::lower_to_mir;
    use skalp_sim::{SimulationConfig, Simulator};

    async fn setup_cpu_simulator(source: &str) -> Simulator {
        // Parse and build HIR
        let hir = parse_and_build_hir(source).expect("Failed to parse design");

        // Compile to MIR
        let mir = lower_to_mir(&hir).expect("Failed to compile to MIR");

        // Convert to SIR
        assert!(!mir.modules.is_empty());
        let top_module = &mir.modules[mir.modules.len() - 1];
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

    /// Test that tuple destructuring correctly extracts all elements
    /// When a function returns (bool, f32, f32), we should get:
    ///   - valid from result_0
    ///   - x1 from result_1
    ///   - x2 from result_2
    #[tokio::test]
    async fn test_tuple_destructuring_three_elements() {
        // A function that returns a 3-tuple (bool, f32, f32)
        // This mimics a quadratic solve that returns (valid, x1, x2)
        let source = r#"
        fn compute_roots(a: fp32, b: fp32, c: fp32) -> (bool, fp32, fp32) {
            // Simple test: return (true, -b, c) for testing purposes
            // In real quadratic: would compute discriminant and roots
            (true, -b, c)
        }

        entity TupleTest {
            in a: fp32
            in b: fp32
            in c: fp32
            out valid: bit<1>
            out x1: fp32
            out x2: fp32
        }

        impl TupleTest {
            // Call function and destructure tuple
            let (valid_flag, root1, root2) = compute_roots(a, b, c)

            valid = valid_flag
            x1 = root1
            x2 = root2
        }
        "#;

        let mut sim = setup_cpu_simulator(source).await;

        // Test with a=1.0, b=5.0, c=6.0
        // Expected: valid=true (1), x1=-5.0, x2=6.0
        let a: f32 = 1.0;
        let b: f32 = 5.0;
        let c: f32 = 6.0;

        sim.set_input("a", a.to_le_bytes().to_vec()).await.unwrap();
        sim.set_input("b", b.to_le_bytes().to_vec()).await.unwrap();
        sim.set_input("c", c.to_le_bytes().to_vec()).await.unwrap();
        sim.step_simulation().await.unwrap();

        // Check valid output (should be 1)
        let valid_bytes = sim.get_output("valid").await.unwrap();
        println!("valid_bytes = {:?}", valid_bytes);
        assert_eq!(
            valid_bytes[0], 1,
            "valid should be true (1), got {}",
            valid_bytes[0]
        );

        // Check x1 output (should be -5.0)
        let x1_bytes = sim.get_output("x1").await.unwrap();
        let x1 = f32::from_le_bytes([x1_bytes[0], x1_bytes[1], x1_bytes[2], x1_bytes[3]]);
        println!("x1 = {} (expected -5.0)", x1);
        assert_eq!(x1, -5.0, "x1 should be -5.0, got {}", x1);

        // Check x2 output (should be 6.0)
        let x2_bytes = sim.get_output("x2").await.unwrap();
        let x2 = f32::from_le_bytes([x2_bytes[0], x2_bytes[1], x2_bytes[2], x2_bytes[3]]);
        println!("x2 = {} (expected 6.0)", x2);
        assert_eq!(x2, 6.0, "x2 should be 6.0, got {}", x2);

        println!("✅ Tuple destructuring test passed!");
    }

    /// Test simpler 2-tuple case
    #[tokio::test]
    async fn test_tuple_destructuring_two_elements() {
        let source = r#"
        fn swap_values(x: fp32, y: fp32) -> (fp32, fp32) {
            (y, x)
        }

        entity SwapTest {
            in a: fp32
            in b: fp32
            out swapped_a: fp32
            out swapped_b: fp32
        }

        impl SwapTest {
            let (first, second) = swap_values(a, b)
            swapped_a = first
            swapped_b = second
        }
        "#;

        let mut sim = setup_cpu_simulator(source).await;

        // Test with a=3.0, b=7.0
        // Expected: swapped_a=7.0, swapped_b=3.0
        let a: f32 = 3.0;
        let b: f32 = 7.0;

        sim.set_input("a", a.to_le_bytes().to_vec()).await.unwrap();
        sim.set_input("b", b.to_le_bytes().to_vec()).await.unwrap();
        sim.step_simulation().await.unwrap();

        // Check swapped_a output (should be 7.0)
        let sa_bytes = sim.get_output("swapped_a").await.unwrap();
        let sa = f32::from_le_bytes([sa_bytes[0], sa_bytes[1], sa_bytes[2], sa_bytes[3]]);
        println!("swapped_a = {} (expected 7.0)", sa);
        assert_eq!(sa, 7.0, "swapped_a should be 7.0, got {}", sa);

        // Check swapped_b output (should be 3.0)
        let sb_bytes = sim.get_output("swapped_b").await.unwrap();
        let sb = f32::from_le_bytes([sb_bytes[0], sb_bytes[1], sb_bytes[2], sb_bytes[3]]);
        println!("swapped_b = {} (expected 3.0)", sb);
        assert_eq!(sb, 3.0, "swapped_b should be 3.0, got {}", sb);

        println!("✅ Two-element tuple destructuring test passed!");
    }
}
