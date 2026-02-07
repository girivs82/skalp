#[cfg(test)]
mod debug_pipeline_valid_tests {
    #[cfg(target_os = "macos")]
    use skalp_frontend::parse_and_build_hir;
    #[cfg(target_os = "macos")]
    use skalp_mir::{MirCompiler, OptimizationLevel};
    #[cfg(target_os = "macos")]
    use skalp_sim::{HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
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
                    counter = 0
                } else {
                    counter = counter + 1
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

        // Create simulation config
        let config = UnifiedSimConfig {
            level: SimLevel::Behavioral,
            hw_accel: HwAccel::Gpu,
            max_cycles: 20,
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

        // Reset first
        simulator.set_input("rst", 1).await;
        simulator.set_input("clk", 0).await;

        // Run a few reset cycles
        for _ in 0..4 {
            simulator.step().await;
        }

        // Check valid is 0 during reset
        let valid = simulator.get_output("valid").await.unwrap_or(0);
        assert_eq!(valid, 0, "valid should be 0 during reset");

        // Release reset and run cycles until counter reaches 8 (bit[3] = 1)
        simulator.set_input("rst", 0).await;

        // Run enough cycles for counter to reach 8
        for i in 0..20 {
            simulator.set_input("clk", (i % 2) as u64).await;
            simulator.step().await;
        }

        // After 10 full clock cycles, counter should be >= 8, so valid should be 1
        let valid = simulator.get_output("valid").await.unwrap_or(0);
        println!("valid after 10 cycles: {}", valid);
        assert_eq!(valid, 1, "valid should be 1 after counter >= 8");

        println!("\nSimple counter test passed!");
    }
}
