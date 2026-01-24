#[cfg(test)]
mod minimal_pipeline_tests {
    #[cfg(target_os = "macos")]
    use skalp_frontend::parse_and_build_hir;
    #[cfg(target_os = "macos")]
    use skalp_mir::{MirCompiler, OptimizationLevel};
    #[cfg(target_os = "macos")]
    use skalp_sim::{
        simulator::SimulationConfig,
        testbench::{TestVectorBuilder, Testbench},
    };
    #[cfg(target_os = "macos")]
    use skalp_sir::convert_mir_to_sir;

    #[tokio::test]
    #[cfg(target_os = "macos")]
    async fn test_simple_pipeline_valid() {
        let source = r#"
        entity MinimalPipeline {
            in clk: clock
            in rst: reset
            out valid: bool
        }

        impl MinimalPipeline {
            signal pipeline_valid: nat[4] = 0

            on(clk.rise) {
                if (rst) {
                    pipeline_valid = 0
                } else {
                    pipeline_valid = pipeline_valid + 1
                }
            }

            valid = pipeline_valid[3]
        }
        "#;

        // Compile to HIR
        let hir = parse_and_build_hir(source).expect("Failed to parse");

        // Compile to MIR
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        // Convert to SIR
        let sir = convert_mir_to_sir(&mir.modules[0]);

        println!("\n=== Minimal Pipeline SIR Analysis ===");
        println!("Module: {}", sir.name);
        println!(
            "State elements: {:?}",
            sir.state_elements.keys().collect::<Vec<_>>()
        );

        // Generate and print Metal shader
        let shader_code = skalp_sir::generate_metal_shader(&sir);
        println!("\n=== Generated Metal Shader ===");
        println!("{}", &shader_code);

        // Create simulation config
        let config = SimulationConfig {
            use_gpu: true,
            max_cycles: 20,
            timeout_ms: 5000,
            capture_waveforms: false,
            parallel_threads: 1,
        };

        // Create testbench
        let mut testbench = Testbench::new(config)
            .await
            .expect("Failed to create testbench");

        // Load the module
        testbench
            .load_module(&sir)
            .await
            .expect("Failed to load module");

        // Test sequence: Reset, then check valid bit

        // Reset for a few cycles
        for cycle in 0..4 {
            testbench.add_test_vector(
                TestVectorBuilder::new(cycle * 2)
                    .with_input("rst", vec![1])
                    .with_expected_output("valid", vec![0])
                    .build(),
            );
        }

        // Release reset and wait for valid bit
        // The testbench steps twice per test vector (rising + falling edge).
        // Counter increments on rising edge, so after 8 non-reset cycles, counter = 8.
        // After vector 11 (8th non-reset), counter becomes 8, valid = bit[3] = 1.
        for cycle in 4..16 {
            // Counter reaches 8 after 8 non-reset cycles (vector 11), valid goes high
            let expected_valid = if cycle >= 11 { 1 } else { 0 };
            testbench.add_test_vector(
                TestVectorBuilder::new(cycle * 2)
                    .with_input("rst", vec![0])
                    .with_expected_output("valid", vec![expected_valid])
                    .build(),
            );
        }

        // Run the test
        let results = testbench.run_test().await.expect("Failed to run test");

        // Print report
        println!("\n{}", testbench.generate_report());

        // Assert all tests passed
        assert!(testbench.all_tests_passed(), "Some tests failed");
        assert!(!results.is_empty(), "Should have test results");

        println!("\n✅ Minimal pipeline test passed!");
    }
}
