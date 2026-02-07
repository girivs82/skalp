#[cfg(test)]
mod counter_sim_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};
    use skalp_sim::{HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
    use skalp_sir::convert_mir_to_sir;

    #[tokio::test]
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
                    counter = 0
                } else {
                    counter = counter + 1
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

        // Create simulation config
        let config = UnifiedSimConfig {
            level: SimLevel::Behavioral,
            hw_accel: if cfg!(target_os = "macos") { HwAccel::Gpu } else { HwAccel::Cpu },
            max_cycles: 100,
            capture_waveforms: false,
            ..Default::default()
        };

        // Create simulator
        let mut simulator = UnifiedSimulator::new(config)
            .expect("Failed to create simulator");

        // Load the module
        simulator
            .load_behavioral(&sir)
            .await
            .expect("Failed to load module");

        // Test sequence: Reset, then count
        // Apply reset
        simulator.set_input("rst", 1).await;
        simulator.set_input("clk", 0).await;

        // Reset for 2 cycles
        for i in 0..4 {
            simulator.set_input("clk", (i % 2) as u64).await;
            simulator.step().await;
        }

        // Verify count is 0 after reset
        let count = simulator.get_output("count").await.unwrap_or(0);
        assert_eq!(count, 0, "Counter should be 0 after reset");

        // Release reset
        simulator.set_input("rst", 0).await;

        // Count for a few cycles
        for i in 0..10 {
            simulator.set_input("clk", (i % 2) as u64).await;
            simulator.step().await;
        }

        // Verify counter has incremented
        let count = simulator.get_output("count").await.unwrap_or(0);
        println!("Final count: {}", count);
        assert!(count > 0, "Counter should have incremented");

        println!("Counter simulation test passed!");
    }
}
