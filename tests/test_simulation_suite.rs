#[cfg(test)]
mod simulation_suite {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};
    use skalp_sim::{SimulationConfig, Simulator};
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

        // Convert to SIR with hierarchical elaboration
        assert!(!mir.modules.is_empty());
        let top_module = &mir.modules[mir.modules.len() - 1]; // Last module is typically top
        eprintln!(
            "DEBUG setup_simulator: module={}, ports={}, signals={}, processes={}, assignments={}, instances={}",
            top_module.name,
            top_module.ports.len(),
            top_module.signals.len(),
            top_module.processes.len(),
            top_module.assignments.len(),
            top_module.instances.len()
        );
        let sir = skalp_sir::convert_mir_to_sir_with_hierarchy(&mir, top_module);
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

        // One more cycle to see registered output
        sim.set_input("clk", vec![0]).await.unwrap();
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

        // One more cycle to see registered output
        sim.set_input("clk", vec![0]).await.unwrap();
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

        // One more cycle to see registered output
        sim.set_input("clk", vec![0]).await.unwrap();
        sim.step_simulation().await.unwrap();

        let result = sim.get_output("result").await.unwrap();
        assert_eq!(result[0], 0x0F);
        assert_eq!(result[1], 0x00);
        assert_eq!(result[2], 0x0F);
        assert_eq!(result[3], 0x00);
    }

    #[tokio::test]
    #[cfg_attr(
        not(target_os = "macos"),
        ignore = "GPU simulation only available on macOS, CPU runtime not yet implemented"
    )]
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
            // Read the data BEFORE advancing rd_ptr
            // rd_data is combinational: rd_data = memory[rd_ptr]
            let data = sim.get_output("rd_data").await.unwrap();
            assert_eq!(data[0], i * 10, "Read data should match written data");

            // Now assert rd_en and clock to advance rd_ptr for next read
            sim.set_input("rd_en", vec![1]).await.unwrap();
            sim.set_input("clk", vec![0]).await.unwrap();
            sim.step_simulation().await.unwrap();
            sim.set_input("clk", vec![1]).await.unwrap();
            sim.step_simulation().await.unwrap();
        }

        sim.set_input("rd_en", vec![0]).await.unwrap();

        // Clock once more to see empty flag update
        sim.set_input("clk", vec![0]).await.unwrap();
        sim.step_simulation().await.unwrap();

        // Verify empty again
        let empty = sim.get_output("empty").await.unwrap();
        assert_eq!(empty[0], 1, "FIFO should be empty after reading all data");
    }

    #[tokio::test]
    #[cfg_attr(
        not(target_os = "macos"),
        ignore = "GPU simulation only available on macOS"
    )]
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

        // Compare final state - only compare actual outputs, not internal nodes
        // Internal node values may differ in width/representation between CPU and GPU
        // Also, registers are stored in 32-bit buffers but counter is 8-bit, so only compare first byte
        if let (Some(cpu_final), Some(gpu_final)) = (cpu_states.last(), gpu_states.last()) {
            // Compare only actual module outputs (count), not internal node_X_out signals
            let cpu_count = cpu_final.signals.get("count");
            let gpu_count = gpu_final.signals.get("count");
            assert_eq!(
                cpu_count, gpu_count,
                "Final output 'count' should match between CPU and GPU"
            );
            // Compare counter register value (8-bit counter stored in 32-bit buffer)
            // Only compare the actual value byte, not padding bytes which may differ
            let cpu_counter = cpu_final
                .registers
                .get("counter")
                .map(|v| v.first().copied());
            let gpu_counter = gpu_final
                .registers
                .get("counter")
                .map(|v| v.first().copied());
            assert_eq!(
                cpu_counter, gpu_counter,
                "Final counter register value should match between CPU and GPU"
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

    #[tokio::test]
    #[cfg_attr(
        not(target_os = "macos"),
        ignore = "GPU simulation only available on macOS"
    )]
    async fn test_hierarchical_pipeline() {
        // Test hierarchical module instantiation in simulation
        // This tests the full hierarchical elaboration pipeline:
        // - Parser creates instance declarations
        // - HIR builder resolves signal connections
        // - MIR converter creates ModuleInstance structs
        // - SIR converter flattens hierarchy with prefixed signals
        // - Simulator runs flattened design
        let hierarchy_source = r#"
entity Register {
    in clk: clock
    in rst: reset
    in data_in: bit[8]
    out data_out: bit[8]
}

impl Register {
    signal reg: bit[8] = 0

    on(clk.rise) {
        if (rst) {
            reg = 0
        } else {
            reg = data_in
        }
    }

    data_out = reg
}

entity Pipeline2 {
    in clk: clock
    in rst: reset
    in data_in: bit[8]
    out data_out: bit[8]
}

impl Pipeline2 {
    signal stage1_out: bit[8] = 0
    signal stage2_out: bit[8] = 0

    data_out = stage2_out

    let stage1 = Register {
        clk: clk,
        rst: rst,
        data_in: data_in,
        data_out: stage1_out
    }

    let stage2 = Register {
        clk: clk,
        rst: rst,
        data_in: stage1_out,
        data_out: stage2_out
    }
}
"#;

        let mut sim = setup_simulator(hierarchy_source, true).await;

        // Reset pipeline
        sim.set_input("rst", vec![1]).await.unwrap();
        sim.set_input("clk", vec![0]).await.unwrap();
        sim.set_input("data_in", vec![0]).await.unwrap();

        // Reset for 2 cycles
        for i in 0..4 {
            sim.set_input("clk", vec![(i % 2) as u8]).await.unwrap();
            sim.step_simulation().await.unwrap();
        }

        // Release reset
        sim.set_input("rst", vec![0]).await.unwrap();

        // Feed data through the 2-stage pipeline
        // The pipeline has 2 clock latency
        let test_sequence = [0x42u8, 0x55u8, 0xAAu8, 0xFFu8];

        for (cycle, &input_val) in test_sequence.iter().enumerate() {
            // Set input
            sim.set_input("data_in", vec![input_val]).await.unwrap();

            // Clock low
            sim.set_input("clk", vec![0]).await.unwrap();
            sim.step_simulation().await.unwrap();

            // Clock high (rising edge)
            sim.set_input("clk", vec![1]).await.unwrap();
            sim.step_simulation().await.unwrap();

            // Read output
            let output_bytes = sim.get_output("data_out").await.unwrap();
            let output = output_bytes[0];

            eprintln!(
                "Cycle {}: input=0x{:02x}, output=0x{:02x}",
                cycle, input_val, output
            );

            // With "settle after clock" semantics, outputs reflect state AFTER the clock edge.
            // A 2-stage pipeline means data takes 2 clock edges to move from input to output:
            // - Cycle 0: stage1 captures 0x42, stage2 captures 0 (old stage1) -> output = 0x00
            // - Cycle 1: stage1 captures 0x55, stage2 captures 0x42 (old stage1) -> output = 0x42
            // - Cycle 2: stage1 captures 0xAA, stage2 captures 0x55 -> output = 0x55
            // - Cycle 3: stage1 captures 0xFF, stage2 captures 0xAA -> output = 0xAA
            // So at cycle N (≥1), output = test_sequence[N-1]
            if cycle == 0 {
                assert_eq!(
                    output, 0,
                    "Cycle 0: expected 0x00 (pipeline empty), got 0x{:02x}",
                    output
                );
            } else {
                let expected = test_sequence[cycle - 1];
                assert_eq!(
                    output, expected,
                    "Cycle {}: expected 0x{:02x}, got 0x{:02x} (1-cycle propagation delay)",
                    cycle, expected, output
                );
            }
        }

        println!("✅ Hierarchical pipeline simulation test passed!");
    }

    #[tokio::test]
    async fn test_hierarchical_pipeline_cpu() {
        // Test hierarchical module instantiation with CPU runtime
        // This verifies the CPU simulator correctly handles flattened hierarchical designs
        let hierarchy_source = r#"
entity Register {
    in clk: clock
    in rst: reset
    in data_in: bit[8]
    out data_out: bit[8]
}

impl Register {
    signal reg: bit[8] = 0

    on(clk.rise) {
        if (rst) {
            reg = 0
        } else {
            reg = data_in
        }
    }

    data_out = reg
}

entity Pipeline2 {
    in clk: clock
    in rst: reset
    in data_in: bit[8]
    out data_out: bit[8]
}

impl Pipeline2 {
    signal stage1_out: bit[8] = 0
    signal stage2_out: bit[8] = 0

    data_out = stage2_out

    let stage1 = Register {
        clk: clk,
        rst: rst,
        data_in: data_in,
        data_out: stage1_out
    }

    let stage2 = Register {
        clk: clk,
        rst: rst,
        data_in: stage1_out,
        data_out: stage2_out
    }
}
"#;

        let mut sim = setup_simulator(hierarchy_source, false).await; // false = CPU

        // Reset pipeline
        sim.set_input("rst", vec![1]).await.unwrap();
        sim.set_input("clk", vec![0]).await.unwrap();
        sim.set_input("data_in", vec![0]).await.unwrap();

        // Reset for 2 cycles
        for i in 0..4 {
            sim.set_input("clk", vec![(i % 2) as u8]).await.unwrap();
            sim.step_simulation().await.unwrap();
        }

        // Release reset
        sim.set_input("rst", vec![0]).await.unwrap();

        // Feed data through the 2-stage pipeline
        let test_sequence = [0x42u8, 0x55u8, 0xAAu8, 0xFFu8];

        for (cycle, &input_val) in test_sequence.iter().enumerate() {
            // Set input
            sim.set_input("data_in", vec![input_val]).await.unwrap();

            // Clock low
            sim.set_input("clk", vec![0]).await.unwrap();
            sim.step_simulation().await.unwrap();

            // Clock high (rising edge)
            sim.set_input("clk", vec![1]).await.unwrap();
            sim.step_simulation().await.unwrap();

            // Read output
            let output_bytes = sim.get_output("data_out").await.unwrap();
            let output = output_bytes[0];

            eprintln!(
                "CPU: Cycle {}: input=0x{:02x}, output=0x{:02x}",
                cycle, input_val, output
            );

            // With "settle after clock" semantics, outputs reflect state AFTER the clock edge.
            // A 2-stage pipeline: at cycle N (≥1), output = test_sequence[N-1]
            if cycle == 0 {
                assert_eq!(
                    output, 0,
                    "CPU: Cycle 0: expected 0x00 (pipeline empty), got 0x{:02x}",
                    output
                );
            } else {
                let expected = test_sequence[cycle - 1];
                assert_eq!(
                    output, expected,
                    "CPU: Cycle {}: expected 0x{:02x}, got 0x{:02x}",
                    cycle, expected, output
                );
            }
        }

        println!("✅ CPU hierarchical pipeline simulation test passed!");
    }

    #[tokio::test]
    async fn test_register_file_cpu() {
        // Test register file with array operations (ArrayRead/ArrayWrite)
        let regfile_source = r#"
entity SimpleRegFile {
    in clk: clock
    in rst: reset
    in we: bit
    in waddr: nat[2]
    in wdata: nat[8]
    in raddr: nat[2]
    out rdata: nat[8]
}

impl SimpleRegFile {
    signal regs: nat[8][4] = 0

    on(clk.rise) {
        if (rst) {
            regs = 0
        } else {
            if (we == 1) {
                regs[waddr] = wdata
            }
        }
    }

    rdata = regs[raddr]
}
"#;

        let mut sim = setup_simulator(regfile_source, false).await; // CPU

        // Reset
        sim.set_input("rst", vec![1]).await.unwrap();
        sim.set_input("we", vec![0]).await.unwrap();
        sim.set_input("waddr", vec![0, 0]).await.unwrap();
        sim.set_input("wdata", vec![0]).await.unwrap();
        sim.set_input("raddr", vec![0, 0]).await.unwrap();
        sim.set_input("clk", vec![0]).await.unwrap();

        // Reset for 2 cycles
        for i in 0..4 {
            sim.set_input("clk", vec![(i % 2) as u8]).await.unwrap();
            sim.step_simulation().await.unwrap();
        }

        // Release reset
        sim.set_input("rst", vec![0]).await.unwrap();

        // Write test values to registers
        let test_values = [0x42u8, 0x55u8, 0xAAu8, 0xFFu8];
        for (addr, &value) in test_values.iter().enumerate() {
            sim.set_input("we", vec![1]).await.unwrap();
            sim.set_input("waddr", vec![addr as u8, 0]).await.unwrap();
            sim.set_input("wdata", vec![value]).await.unwrap();

            // Clock cycle
            sim.set_input("clk", vec![0]).await.unwrap();
            sim.step_simulation().await.unwrap();
            sim.set_input("clk", vec![1]).await.unwrap();
            sim.step_simulation().await.unwrap();
        }

        // Disable write
        sim.set_input("we", vec![0]).await.unwrap();

        // Read back and verify
        for (addr, &expected) in test_values.iter().enumerate() {
            sim.set_input("raddr", vec![addr as u8, 0]).await.unwrap();

            // Clock cycle (for registered output timing)
            sim.set_input("clk", vec![0]).await.unwrap();
            sim.step_simulation().await.unwrap();

            let rdata = sim.get_output("rdata").await.unwrap();
            assert_eq!(
                rdata[0], expected,
                "Register {} should contain 0x{:02x}, got 0x{:02x}",
                addr, expected, rdata[0]
            );
        }

        println!("✅ CPU register file simulation test passed!");
    }

    #[tokio::test]
    #[cfg(target_os = "macos")]
    async fn test_register_file_gpu() {
        // Test register file with array operations on GPU (Metal backend)
        let regfile_source = r#"
entity SimpleRegFile {
    in clk: clock
    in rst: reset
    in we: bit
    in waddr: nat[2]
    in wdata: nat[8]
    in raddr: nat[2]
    out rdata: nat[8]
}

impl SimpleRegFile {
    signal regs: nat[8][4] = 0

    on(clk.rise) {
        if (rst) {
            regs = 0
        } else {
            if (we == 1) {
                regs[waddr] = wdata
            }
        }
    }

    rdata = regs[raddr]
}
"#;

        let mut sim = setup_simulator(regfile_source, true).await; // GPU

        // Reset
        sim.set_input("rst", vec![1]).await.unwrap();
        sim.set_input("we", vec![0]).await.unwrap();
        sim.set_input("waddr", vec![0]).await.unwrap(); // nat[2] = 1 byte
        sim.set_input("wdata", vec![0]).await.unwrap();
        sim.set_input("raddr", vec![0]).await.unwrap(); // nat[2] = 1 byte
        sim.set_input("clk", vec![0]).await.unwrap();

        // Reset for 2 cycles
        for i in 0..4 {
            sim.set_input("clk", vec![(i % 2) as u8]).await.unwrap();
            sim.step_simulation().await.unwrap();
        }

        // Release reset
        sim.set_input("rst", vec![0]).await.unwrap();

        // Write test values to registers
        let test_values = [0x42u8, 0x55u8, 0xAAu8, 0xFFu8];
        for (addr, &value) in test_values.iter().enumerate() {
            sim.set_input("we", vec![1]).await.unwrap();
            sim.set_input("waddr", vec![addr as u8]).await.unwrap(); // nat[2] = 1 byte
            sim.set_input("wdata", vec![value]).await.unwrap();

            // Clock cycle
            sim.set_input("clk", vec![0]).await.unwrap();
            sim.step_simulation().await.unwrap();
            sim.set_input("clk", vec![1]).await.unwrap();
            sim.step_simulation().await.unwrap();
        }

        // Disable write
        sim.set_input("we", vec![0]).await.unwrap();

        // Read back and verify
        for (addr, &expected) in test_values.iter().enumerate() {
            sim.set_input("raddr", vec![addr as u8]).await.unwrap(); // nat[2] = 1 byte

            // Clock cycle (for registered output timing)
            sim.set_input("clk", vec![0]).await.unwrap();
            sim.step_simulation().await.unwrap();

            let rdata = sim.get_output("rdata").await.unwrap();
            assert_eq!(
                rdata[0], expected,
                "GPU Register {} should contain 0x{:02x}, got 0x{:02x}",
                addr, expected, rdata[0]
            );
        }

        println!("✅ GPU register file simulation test passed!");
    }
}
