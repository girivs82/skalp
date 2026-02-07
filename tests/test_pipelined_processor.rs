#[cfg(test)]
mod pipelined_processor_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};
    use skalp_sim::{HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
    use skalp_sir::convert_mir_to_sir;

    #[tokio::test]
    async fn test_pipelined_processor_gpu() {
        let source = r#"
        entity PipelinedProcessor {
            in clk: clock
            in rst: reset
            in instruction: nat[16]
            in data_in: nat[8]
            out result: nat[8]
            out valid: bool
        }

        impl PipelinedProcessor {
            // Pipeline stages
            signal fetch_instruction: nat[16] = 0
            signal decode_opcode: nat[4] = 0
            signal decode_operand: nat[8] = 0
            signal execute_result: nat[8] = 0
            signal writeback_data: nat[8] = 0
            signal pipeline_valid: nat[4] = 0

            on(clk.rise) {
                if (rst) {
                    fetch_instruction = 0
                    decode_opcode = 0
                    decode_operand = 0
                    execute_result = 0
                    writeback_data = 0
                    pipeline_valid = 0
                } else {
                    // Stage 1: Fetch
                    fetch_instruction = instruction

                    // Stage 2: Decode
                    decode_opcode = fetch_instruction[15:12]
                    decode_operand = fetch_instruction[7:0]

                    // Stage 3: Execute (simplified)
                    execute_result = decode_operand + data_in

                    // Stage 4: Writeback
                    writeback_data = execute_result

                    // Valid pipeline - simplified approach
                    pipeline_valid = pipeline_valid + 1
                }
            }

            result = writeback_data
            valid = pipeline_valid[3]
        }
        "#;

        // Compile to HIR
        let hir = parse_and_build_hir(source).expect("Failed to parse");

        // Debug: Print HIR info
        println!("\n=== HIR Port Info ===");
        for entity in &hir.entities {
            for port in &entity.ports {
                println!("Port '{}': type={:?}", port.name, port.port_type);
            }
        }

        // Compile to MIR (disable optimizations to prevent signal removal)
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
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

        // Extract cones
        let cones = sir.extract_combinational_cones();
        println!("\nCombinational cones: {}", cones.len());
        for (i, cone) in cones.iter().enumerate() {
            println!("  Cone {}: {} nodes", i, cone.nodes.len());
        }

        // Generate Metal shader for debugging
        let shader_code = skalp_sir::generate_metal_shader(&sir);
        println!("\n=== Generated Metal Shader (first 2000 chars) ===");
        println!("{}", &shader_code[..shader_code.len().min(2000)]);

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

        // Test sequence: Reset, then pipeline operations

        // Reset sequence
        simulator.set_input("rst", 1).await;
        simulator.set_input("clk", 0).await;
        simulator.set_input("instruction", 0).await;
        simulator.set_input("data_in", 0).await;

        for i in 0..8 {
            simulator.set_input("clk", (i % 2) as u64).await;
            simulator.step().await;
        }

        // Release reset
        simulator.set_input("rst", 0).await;

        // Feed ADD instruction (opcode=1, operand=10)
        let add_instr = (1u64 << 12) | 10u64;
        simulator.set_input("instruction", add_instr).await;

        // Run a few cycles
        for i in 0..8 {
            simulator.set_input("clk", (i % 2) as u64).await;
            if i % 2 == 0 {
                simulator.set_input("data_in", 5).await; // Add 5 to operand
            }
            simulator.step().await;
        }

        // Check result
        let result = simulator.get_output("result").await.unwrap_or(0);
        println!("\nResult after pipeline: {}", result);

        // Check valid signal
        let valid = simulator.get_output("valid").await.unwrap_or(0);
        println!("Valid signal: {}", valid);

        println!("\n Pipelined processor simulation completed!");
    }
}
