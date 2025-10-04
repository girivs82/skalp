#[cfg(test)]
mod counter_sim_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};
    use skalp_sim::{
        simulator::SimulationConfig,
        testbench::{TestVectorBuilder, Testbench},
    };
    use skalp_sir::convert_mir_to_sir;

    #[tokio::test]
    #[cfg(target_os = "macos")]
    async fn test_counter_gpu_simulation() {
        let source = r#"
        entity Counter {
            in clk: clock
            in rst: reset
            out count: nat[8]
        }

        impl Counter {
            signal counter: nat[8] = 0

            on(clk.rise) {
                if (rst) {
                    counter <= 0
                } else {
                    counter <= counter + 1
                }
            }

            count = counter
        }
        "#;

        // Compile to HIR
        let hir = parse_and_build_hir(source).expect("Failed to parse");

        // Debug: Print HIR port info
        println!("\n=== HIR Port Info ===");
        for entity in &hir.entities {
            for port in &entity.ports {
                println!("Port '{}': type={:?}", port.name, port.port_type);
            }
        }

        // Compile to MIR
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::Basic);
        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        // Debug: Print MIR module info
        println!("\n=== MIR Module Info ===");
        for port in &mir.modules[0].ports {
            println!("Port '{}': type={:?}", port.name, port.port_type);
        }
        for signal in &mir.modules[0].signals {
            println!("Signal '{}': type={:?}", signal.name, signal.signal_type);
        }

        // Convert to SIR
        let sir = convert_mir_to_sir(&mir.modules[0]);

        println!("\n=== SIR Analysis ===");
        println!("Module: {}", sir.name);
        println!(
            "State elements: {:?}",
            sir.state_elements.keys().collect::<Vec<_>>()
        );
        println!("Combinational nodes: {}", sir.combinational_nodes.len());
        println!("Sequential nodes: {}", sir.sequential_nodes.len());

        // Check for ADD operation
        let has_add = sir.combinational_nodes.iter().any(|node| {
            matches!(
                node.kind,
                skalp_sir::sir::SirNodeKind::BinaryOp(skalp_sir::sir::BinaryOperation::Add)
            )
        });

        assert!(has_add, "Should have ADD operation in combinational logic");

        // Print all signals for debugging
        println!("\n=== All SIR Signals ===");
        for signal in &sir.signals {
            println!(
                "Signal '{}': width={}, is_state={}, driver={:?}",
                signal.name, signal.width, signal.is_state, signal.driver_node
            );
        }

        // Extract cones
        let cones = sir.extract_combinational_cones();
        println!("\nCombinational cones: {}", cones.len());
        for (i, cone) in cones.iter().enumerate() {
            println!("  Cone {}: nodes={:?}", i, cone.nodes);
        }
        assert!(
            !cones.is_empty(),
            "Should have at least one combinational cone"
        );

        // Generate and print Metal shader for debugging
        let shader_code = skalp_sir::generate_metal_shader(&sir);
        println!("\n=== Generated Metal Shader ===");
        println!("{}", &shader_code);
        println!("\n... (total {} chars)", shader_code.len());

        // Debug connectivity
        println!("\n=== Debug Connectivity ===");
        for node in &sir.combinational_nodes {
            println!("Node {}: {:?}", node.id, node.kind);
            if !node.outputs.is_empty() {
                println!("  Output: {}", node.outputs[0].signal_id);
            }
        }
        for node in &sir.combinational_nodes {
            if matches!(
                node.kind,
                skalp_sir::sir::SirNodeKind::BinaryOp(skalp_sir::sir::BinaryOperation::Add)
            ) {
                println!("ADD Node {}:", node.id);
                println!("  Inputs: {:?}", node.inputs);
                println!("  Outputs: {:?}", node.outputs);
            }
        }
        for node in &sir.sequential_nodes {
            println!("FF Node {}:", node.id);
            println!("  Inputs: {:?}", node.inputs);
            println!("  Outputs: {:?}", node.outputs);
        }

        // Create simulation config
        let config = SimulationConfig {
            use_gpu: true,
            max_cycles: 100,
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

        // Create test vectors
        // Apply reset for first few cycles
        testbench.add_test_vector(TestVectorBuilder::new(0).with_input("rst", vec![1]).build());

        testbench.add_test_vector(
            TestVectorBuilder::new(2)
                .with_input("rst", vec![1])
                .with_expected_output("count", vec![0])
                .build(),
        );

        // Release reset
        // With synchronous reset, counter increments on the first clock edge where rst=0
        testbench.add_test_vector(
            TestVectorBuilder::new(4)
                .with_input("rst", vec![0])
                .with_expected_output("count", vec![1]) // Correct: increments on this clock edge
                .build(),
        );

        testbench.add_test_vector(
            TestVectorBuilder::new(6)
                .with_input("rst", vec![0])
                .with_expected_output("count", vec![2])
                .build(),
        );

        testbench.add_test_vector(
            TestVectorBuilder::new(8)
                .with_input("rst", vec![0])
                .with_expected_output("count", vec![3])
                .build(),
        );

        testbench.add_test_vector(
            TestVectorBuilder::new(10)
                .with_input("rst", vec![0])
                .with_expected_output("count", vec![4])
                .build(),
        );

        testbench.add_test_vector(
            TestVectorBuilder::new(12)
                .with_input("rst", vec![0])
                .with_expected_output("count", vec![5])
                .build(),
        );

        testbench.add_test_vector(
            TestVectorBuilder::new(14)
                .with_input("rst", vec![0])
                .with_expected_output("count", vec![6])
                .build(),
        );

        // Run the test
        let results = testbench.run_test().await.expect("Failed to run test");

        // Print report
        println!("{}", testbench.generate_report());

        // Debug: Print actual values for failing tests
        if !testbench.all_tests_passed() {
            println!("\n=== Debug: Checking state elements ===");

            // Print the actual SIR structure
            println!("\n=== SIR Signal Details ===");
            for signal in &sir.signals {
                if signal.name == "count"
                    || signal.name == "counter"
                    || signal.name.contains("node_6")
                {
                    println!(
                        "Signal '{}': width={}, is_state={}, driver={:?}",
                        signal.name, signal.width, signal.is_state, signal.driver_node
                    );
                }
            }

            println!("\n=== Flip-flop connections ===");
            for node in &sir.sequential_nodes {
                println!(
                    "FF Node {}: inputs={:?}, outputs={:?}",
                    node.id, node.inputs, node.outputs
                );
            }
        }

        // Assert all tests passed
        assert!(testbench.all_tests_passed(), "Some tests failed");
        assert!(!results.is_empty(), "Should have test results");
    }
}
