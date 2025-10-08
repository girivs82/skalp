#[cfg(test)]
mod simulation_suite {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};
    use skalp_sim::{SimulationConfig, Simulator};
    use skalp_sir::convert_mir_to_sir;
    use std::fs;
    use std::time::Instant;

    async fn setup_simulator(source: &str, use_gpu: bool) -> Simulator {
        // Parse and build HIR
        let hir = parse_and_build_hir(source).expect("Failed to parse design");

        // Compile to MIR
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::Basic);
        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        // Convert to SIR
        assert!(!mir.modules.is_empty());
        eprintln!(
            "DEBUG setup_simulator: module={}, ports={}, signals={}, processes={}, assignments={}",
            mir.modules[0].name,
            mir.modules[0].ports.len(),
            mir.modules[0].signals.len(),
            mir.modules[0].processes.len(),
            mir.modules[0].assignments.len()
        );
        let sir = convert_mir_to_sir(&mir.modules[0]);
        eprintln!("DEBUG setup_simulator: SIR inputs={}, outputs={}, comb_nodes={}, seq_nodes={}, states={}",
            sir.inputs.len(), sir.outputs.len(), sir.combinational_nodes.len(),
            sir.sequential_nodes.len(), sir.state_elements.len());

        // Create simulator
        let config = SimulationConfig {
            use_gpu,
            max_cycles: 1000,
            timeout_ms: 10000,
            capture_waveforms: true,
            parallel_threads: 4,
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

    #[tokio::test]
    #[cfg_attr(
        not(target_os = "macos"),
        ignore = "GPU simulation only available on macOS, CPU runtime not yet implemented"
    )]
    async fn test_counter_increments() {
        let counter_source =
            fs::read_to_string("examples/counter.sk").expect("Failed to read counter.sk");

        let mut sim = setup_simulator(&counter_source, true).await;

        // Initialize
        sim.set_input("rst", vec![1]).await.unwrap();
        sim.set_input("clk", vec![0]).await.unwrap();

        // Reset for 2 cycles
        for i in 0..4 {
            sim.set_input("clk", vec![(i % 2) as u8]).await.unwrap();
            sim.step_simulation().await.unwrap();
        }

        // Release reset
        sim.set_input("rst", vec![0]).await.unwrap();

        // Count for 10 cycles and verify increment
        let mut prev_count = 0u32;
        for i in 0..20 {
            sim.set_input("clk", vec![((i + 4) % 2) as u8])
                .await
                .unwrap();
            sim.step_simulation().await.unwrap();

            if i % 2 == 1 {
                // After rising edge
                let count_bytes = sim.get_output("count").await.unwrap();
                let count = u32::from_le_bytes([
                    count_bytes[0],
                    count_bytes.get(1).copied().unwrap_or(0),
                    count_bytes.get(2).copied().unwrap_or(0),
                    count_bytes.get(3).copied().unwrap_or(0),
                ]);

                if i > 1 {
                    // After first real count
                    assert_eq!(count, prev_count + 1, "Counter should increment");
                }
                prev_count = count;
            }
        }
    }

    #[tokio::test]
    #[cfg_attr(
        not(target_os = "macos"),
        ignore = "GPU simulation only available on macOS, CPU runtime not yet implemented"
    )]
    async fn test_alu_operations() {
        let alu_source = fs::read_to_string("examples/alu.sk").expect("Failed to read alu.sk");

        let mut sim = setup_simulator(&alu_source, true).await;

        // Test ADD operation
        sim.set_input("a", vec![0x05, 0x00, 0x00, 0x00])
            .await
            .unwrap();
        sim.set_input("b", vec![0x03, 0x00, 0x00, 0x00])
            .await
            .unwrap();
        sim.set_input("op", vec![0b000]).await.unwrap();
        sim.set_input("clk", vec![0]).await.unwrap();

        sim.step_simulation().await.unwrap();
        sim.set_input("clk", vec![1]).await.unwrap();
        sim.step_simulation().await.unwrap();

        let result = sim.get_output("result").await.unwrap();
        let result_val = u32::from_le_bytes([result[0], result[1], result[2], result[3]]);
        assert_eq!(result_val, 8, "5 + 3 should equal 8");

        // Test SUB operation
        sim.set_input("op", vec![0b001]).await.unwrap();
        sim.set_input("clk", vec![0]).await.unwrap();
        sim.step_simulation().await.unwrap();
        sim.set_input("clk", vec![1]).await.unwrap();
        sim.step_simulation().await.unwrap();

        let result = sim.get_output("result").await.unwrap();
        let result_val = u32::from_le_bytes([result[0], result[1], result[2], result[3]]);
        assert_eq!(result_val, 2, "5 - 3 should equal 2");

        // Test AND operation
        sim.set_input("a", vec![0xFF, 0x00, 0xFF, 0x00])
            .await
            .unwrap();
        sim.set_input("b", vec![0x0F, 0xF0, 0x0F, 0xF0])
            .await
            .unwrap();
        sim.set_input("op", vec![0b010]).await.unwrap();
        sim.set_input("clk", vec![0]).await.unwrap();
        sim.step_simulation().await.unwrap();
        sim.set_input("clk", vec![1]).await.unwrap();
        sim.step_simulation().await.unwrap();

        let result = sim.get_output("result").await.unwrap();
        assert_eq!(result[0], 0x0F);
        assert_eq!(result[1], 0x00);
        assert_eq!(result[2], 0x0F);
        assert_eq!(result[3], 0x00);
    }

    #[tokio::test]
    #[ignore = "FIFO count logic issue: doesn't handle simultaneous read/write correctly"]
    async fn test_fifo_operations() {
        let fifo_source = fs::read_to_string("examples/fifo.sk").expect("Failed to read fifo.sk");

        let mut sim = setup_simulator(&fifo_source, true).await;

        // Reset
        sim.set_input("rst", vec![1]).await.unwrap();
        sim.set_input("clk", vec![0]).await.unwrap();
        sim.set_input("wr_en", vec![0]).await.unwrap();
        sim.set_input("rd_en", vec![0]).await.unwrap();
        sim.set_input("wr_data", vec![0]).await.unwrap();

        // Clock pulse with reset
        sim.step_simulation().await.unwrap();
        sim.set_input("clk", vec![1]).await.unwrap();
        sim.step_simulation().await.unwrap();

        // Release reset
        sim.set_input("rst", vec![0]).await.unwrap();
        sim.set_input("clk", vec![0]).await.unwrap();
        sim.step_simulation().await.unwrap();

        // Verify empty
        let empty = sim.get_output("empty").await.unwrap();
        assert_eq!(empty[0], 1, "FIFO should be empty after reset");

        // Write data
        for i in 0..8u8 {
            sim.set_input("wr_data", vec![i * 10]).await.unwrap();
            sim.set_input("wr_en", vec![1]).await.unwrap();
            sim.set_input("clk", vec![0]).await.unwrap();
            sim.step_simulation().await.unwrap();
            sim.set_input("clk", vec![1]).await.unwrap();
            sim.step_simulation().await.unwrap();
        }

        sim.set_input("wr_en", vec![0]).await.unwrap();

        // Verify not empty
        let empty = sim.get_output("empty").await.unwrap();
        assert_eq!(empty[0], 0, "FIFO should not be empty after writes");

        // Read data back
        for i in 0..8u8 {
            sim.set_input("rd_en", vec![1]).await.unwrap();
            let data = sim.get_output("rd_data").await.unwrap();
            assert_eq!(data[0], i * 10, "Read data should match written data");

            sim.set_input("clk", vec![0]).await.unwrap();
            sim.step_simulation().await.unwrap();
            sim.set_input("clk", vec![1]).await.unwrap();
            sim.step_simulation().await.unwrap();
        }

        sim.set_input("rd_en", vec![0]).await.unwrap();

        // Verify empty again
        let empty = sim.get_output("empty").await.unwrap();
        assert_eq!(empty[0], 1, "FIFO should be empty after reading all data");
    }

    #[tokio::test]
    #[ignore = "CPU runtime not fully implemented - evaluate_combinational is a stub"]
    async fn test_cpu_vs_gpu_performance() {
        let counter_source =
            fs::read_to_string("examples/counter.sk").expect("Failed to read counter.sk");

        // CPU simulation
        let start = Instant::now();
        let mut cpu_sim = setup_simulator(&counter_source, false).await;

        cpu_sim.set_input("rst", vec![0]).await.unwrap();
        for i in 0..1000 {
            cpu_sim.set_input("clk", vec![(i % 2) as u8]).await.unwrap();
            cpu_sim.step_simulation().await.unwrap();
        }
        let cpu_duration = start.elapsed();

        // GPU simulation
        let start = Instant::now();
        let mut gpu_sim = setup_simulator(&counter_source, true).await;

        gpu_sim.set_input("rst", vec![0]).await.unwrap();
        for i in 0..1000 {
            gpu_sim.set_input("clk", vec![(i % 2) as u8]).await.unwrap();
            gpu_sim.step_simulation().await.unwrap();
        }
        let gpu_duration = start.elapsed();

        println!("CPU simulation: {:?}", cpu_duration);
        println!("GPU simulation: {:?}", gpu_duration);

        // GPU might not always be faster for small designs, but should be comparable
        let speedup = cpu_duration.as_secs_f64() / gpu_duration.as_secs_f64();
        println!("Speedup: {:.2}x", speedup);

        // Verify both produce same results
        let cpu_states = cpu_sim.get_waveforms().await;
        let gpu_states = gpu_sim.get_waveforms().await;

        assert_eq!(
            cpu_states.len(),
            gpu_states.len(),
            "CPU and GPU should produce same number of states"
        );

        // Compare final state
        if let (Some(cpu_final), Some(gpu_final)) = (cpu_states.last(), gpu_states.last()) {
            assert_eq!(
                cpu_final.signals, gpu_final.signals,
                "Final signals should match"
            );
            assert_eq!(
                cpu_final.registers, gpu_final.registers,
                "Final registers should match"
            );
        }
    }

    #[tokio::test]
    #[cfg_attr(
        not(target_os = "macos"),
        ignore = "GPU simulation only available on macOS, CPU runtime not yet implemented"
    )]
    async fn test_simulation_state_consistency() {
        let counter_source =
            fs::read_to_string("examples/counter.sk").expect("Failed to read counter.sk");

        let mut sim = setup_simulator(&counter_source, true).await;

        // Set known initial state
        sim.set_input("rst", vec![1]).await.unwrap();
        sim.set_input("clk", vec![0]).await.unwrap();
        sim.step_simulation().await.unwrap();

        // Capture state
        let states1 = sim.get_waveforms().await;
        let initial_len = states1.len();

        // Continue simulation
        for i in 0..10 {
            sim.set_input("clk", vec![(i % 2) as u8]).await.unwrap();
            sim.step_simulation().await.unwrap();
        }

        // Capture state again
        let states2 = sim.get_waveforms().await;

        // Verify states are accumulated
        assert!(states2.len() > initial_len, "States should accumulate");
        assert_eq!(
            states2[0..initial_len],
            states1[..],
            "Early states should remain unchanged"
        );
    }
}
