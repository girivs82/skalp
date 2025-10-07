#[cfg(test)]
mod debug_pipeline_valid_tests {
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
    async fn test_pipeline_valid_only() {
        let source = r#"
        entity SimpleCounter {
            in clk: clock
            in rst: reset
            out valid: bool
        }

        impl SimpleCounter {
            signal counter: nat[4] = 0

            on(clk.rise) {
                if (rst) {
                    counter <= 0
                } else {
                    counter <= counter + 1
                }
            }

            valid = counter[3]
        }
        "#;

        // Compile through the pipeline
        let hir = parse_and_build_hir(source).expect("Failed to parse");
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");
        let sir = convert_mir_to_sir(&mir.modules[0]);

        println!("\n=== Counter SIR Analysis ===");
        println!(
            "State elements: {:?}",
            sir.state_elements.keys().collect::<Vec<_>>()
        );
        println!("Combinational nodes: {}", sir.combinational_nodes.len());
        for node in &sir.combinational_nodes {
            println!("  Node {}: kind={:?}", node.id, node.kind);
        }

        // Generate Metal shader
        let shader_code = skalp_sir::generate_metal_shader(&sir);
        println!("\n=== Generated Metal Shader ===");
        println!("{}", &shader_code);

        // Create simulation
        let config = SimulationConfig {
            use_gpu: true,
            max_cycles: 20,
            timeout_ms: 5000,
            capture_waveforms: false,
            parallel_threads: 1,
        };

        let mut testbench = Testbench::new(config)
            .await
            .expect("Failed to create testbench");
        testbench
            .load_module(&sir)
            .await
            .expect("Failed to load module");

        // Reset first
        testbench.add_test_vector(
            TestVectorBuilder::new(0)
                .with_input("rst", vec![1])
                .with_expected_output("valid", vec![0])
                .build(),
        );

        // Release reset and wait for counter to reach 8
        // Testbench applies vector at cycle X, then steps twice (rising+falling edge)
        // The output is checked after the first step (at clock=1)
        // counter increments at odd cycles when clock goes high
        // counter: cycle 3->0, 5->1, 7->2, 9->3, 11->4, 13->5, 15->6, 17->7, 19->8, 21->9
        // Vector at cycle 18: step to 19 (counter becomes 9), check output
        // So we expect valid=1 starting at vector cycle 18
        for cycle in 1..12 {
            // Testbench steps twice, so outputs checked 2 cycles after test vector cycle
            // With correct execution order, valid goes high one cycle earlier
            let expected_valid = if cycle * 2 >= 16 { 1 } else { 0 };
            testbench.add_test_vector(
                TestVectorBuilder::new(cycle * 2)
                    .with_input("rst", vec![0])
                    .with_expected_output("valid", vec![expected_valid])
                    .build(),
            );
        }

        // Run test
        let _results = testbench.run_test().await.expect("Failed to run test");
        println!("\n{}", testbench.generate_report());

        assert!(testbench.all_tests_passed(), "Counter test failed");
        println!("\n✅ Simple counter test passed!");
    }
}
