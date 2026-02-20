#[cfg(test)]
mod simulation_suite {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};
    use skalp_sim::{HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
    use std::fs;
    use std::time::Instant;

    async fn setup_simulator(source: &str, use_gpu: bool) -> UnifiedSimulator {
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
        let config = UnifiedSimConfig {
            level: SimLevel::Behavioral,
            hw_accel: if use_gpu { HwAccel::Gpu } else { HwAccel::Cpu },
            max_cycles: 1000,
            capture_waveforms: true,
            ..Default::default()
        };

        let mut simulator = UnifiedSimulator::new(config).expect("Failed to create simulator");

        simulator
            .load_behavioral(&sir)
            .await
            .expect("Failed to load module");

        simulator
    }

    #[tokio::test]
    async fn test_counter_increments() {
        let counter_source =
            fs::read_to_string("examples/counter.sk").expect("Failed to read counter.sk");

        let mut sim = setup_simulator(&counter_source, cfg!(target_os = "macos")).await;

        // Initialize
        sim.set_input("rst", 1).await;
        sim.set_input("clk", 0).await;

        // Reset for 2 cycles
        for i in 0..4 {
            sim.set_input("clk", (i % 2) as u64).await;
            sim.step().await;
        }

        // Release reset
        sim.set_input("rst", 0).await;

        // Count for 10 cycles and verify increment
        let mut prev_count = 0u64;
        for i in 0..20 {
            sim.set_input("clk", ((i + 4) % 2) as u64).await;
            sim.step().await;

            if i % 2 == 1 {
                // After rising edge
                let count = sim.get_output("count").await.unwrap_or(0);

                if i > 1 {
                    // After first real count
                    assert_eq!(count, prev_count + 1, "Counter should increment");
                }
                prev_count = count;
            }
        }
    }

    #[tokio::test]
    async fn test_alu_operations() {
        let alu_source = fs::read_to_string("examples/alu.sk").expect("Failed to read alu.sk");

        let mut sim = setup_simulator(&alu_source, cfg!(target_os = "macos")).await;

        // Test ADD operation
        sim.set_input("a", 5).await;
        sim.set_input("b", 3).await;
        sim.set_input("op", 0b000).await;
        sim.set_input("clk", 0).await;

        sim.step().await;
        sim.set_input("clk", 1).await;
        sim.step().await;

        // One more cycle to see registered output
        sim.set_input("clk", 0).await;
        sim.step().await;

        let result = sim.get_output("result").await.unwrap_or(0);
        assert_eq!(result, 8, "5 + 3 should equal 8");

        // Test SUB operation
        sim.set_input("op", 0b001).await;
        sim.set_input("clk", 0).await;
        sim.step().await;
        sim.set_input("clk", 1).await;
        sim.step().await;

        // One more cycle to see registered output
        sim.set_input("clk", 0).await;
        sim.step().await;

        let result = sim.get_output("result").await.unwrap_or(0);
        assert_eq!(result, 2, "5 - 3 should equal 2");

        // Test AND operation
        sim.set_input("a", 0x00FF00FF).await;
        sim.set_input("b", 0xF00FF00F).await;
        sim.set_input("op", 0b010).await;
        sim.set_input("clk", 0).await;
        sim.step().await;
        sim.set_input("clk", 1).await;
        sim.step().await;

        // One more cycle to see registered output
        sim.set_input("clk", 0).await;
        sim.step().await;

        let result = sim.get_output("result").await.unwrap_or(0);
        assert_eq!(result as u32, 0x000F000F, "AND result mismatch");
    }

    #[tokio::test]
    async fn test_fifo_operations() {
        let fifo_source = fs::read_to_string("examples/fifo.sk").expect("Failed to read fifo.sk");

        let mut sim = setup_simulator(&fifo_source, cfg!(target_os = "macos")).await;

        // Reset
        sim.set_input("rst", 1).await;
        sim.set_input("clk", 0).await;
        sim.set_input("wr_en", 0).await;
        sim.set_input("rd_en", 0).await;
        sim.set_input("wr_data", 0).await;

        // Clock pulse with reset
        sim.step().await;
        sim.set_input("clk", 1).await;
        sim.step().await;

        // Release reset
        sim.set_input("rst", 0).await;
        sim.set_input("clk", 0).await;
        sim.step().await;

        // Verify empty
        let empty = sim.get_output("empty").await.unwrap_or(0);
        assert_eq!(empty, 1, "FIFO should be empty after reset");

        // Write data
        for i in 0..8u64 {
            sim.set_input("wr_data", i * 10).await;
            sim.set_input("wr_en", 1).await;
            sim.set_input("clk", 0).await;
            sim.step().await;
            sim.set_input("clk", 1).await;
            sim.step().await;
        }

        sim.set_input("wr_en", 0).await;

        // Verify not empty
        let empty = sim.get_output("empty").await.unwrap_or(0);
        assert_eq!(empty, 0, "FIFO should not be empty after writes");

        // Read data back
        for i in 0..8u64 {
            // Read the data BEFORE advancing rd_ptr
            // rd_data is combinational: rd_data = memory[rd_ptr]
            let data = sim.get_output("rd_data").await.unwrap_or(0);
            assert_eq!(data, i * 10, "Read data should match written data");

            // Now assert rd_en and clock to advance rd_ptr for next read
            sim.set_input("rd_en", 1).await;
            sim.set_input("clk", 0).await;
            sim.step().await;
            sim.set_input("clk", 1).await;
            sim.step().await;
        }

        sim.set_input("rd_en", 0).await;

        // Clock once more to see empty flag update
        sim.set_input("clk", 0).await;
        sim.step().await;

        // Verify empty again
        let empty = sim.get_output("empty").await.unwrap_or(0);
        assert_eq!(empty, 1, "FIFO should be empty after reading all data");
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

        cpu_sim.set_input("rst", 0).await;
        for i in 0..1000 {
            cpu_sim.set_input("clk", (i % 2) as u64).await;
            cpu_sim.step().await;
        }
        let cpu_duration = start.elapsed();

        // GPU simulation
        let start = Instant::now();
        let mut gpu_sim = setup_simulator(&counter_source, true).await;

        gpu_sim.set_input("rst", 0).await;
        for i in 0..1000 {
            gpu_sim.set_input("clk", (i % 2) as u64).await;
            gpu_sim.step().await;
        }
        let gpu_duration = start.elapsed();

        println!("CPU simulation: {:?}", cpu_duration);
        println!("GPU simulation: {:?}", gpu_duration);

        // GPU might not always be faster for small designs, but should be comparable
        let speedup = cpu_duration.as_secs_f64() / gpu_duration.as_secs_f64();
        println!("Speedup: {:.2}x", speedup);

        // Verify both produce same final count
        let cpu_count = cpu_sim.get_output("count").await.unwrap_or(0);
        let gpu_count = gpu_sim.get_output("count").await.unwrap_or(0);

        assert_eq!(
            cpu_count, gpu_count,
            "Final output 'count' should match between CPU and GPU"
        );
    }

    #[tokio::test]
    async fn test_simulation_state_consistency() {
        let counter_source =
            fs::read_to_string("examples/counter.sk").expect("Failed to read counter.sk");

        let mut sim = setup_simulator(&counter_source, cfg!(target_os = "macos")).await;

        // Set known initial state
        sim.set_input("rst", 1).await;
        sim.set_input("clk", 0).await;
        sim.step().await;

        // Continue simulation
        for i in 0..10 {
            sim.set_input("clk", (i % 2) as u64).await;
            sim.step().await;
        }

        // Verify we can get output
        let count = sim.get_output("count").await;
        assert!(count.is_some(), "Should be able to read count output");
    }

    #[tokio::test]
    async fn test_hierarchical_pipeline() {
        // Test hierarchical module instantiation in simulation
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

        let mut sim = setup_simulator(hierarchy_source, cfg!(target_os = "macos")).await;

        // Reset pipeline
        sim.set_input("rst", 1).await;
        sim.set_input("clk", 0).await;
        sim.set_input("data_in", 0).await;

        // Reset for 2 cycles
        for i in 0..4 {
            sim.set_input("clk", (i % 2) as u64).await;
            sim.step().await;
        }

        // Release reset
        sim.set_input("rst", 0).await;

        // Feed data through the 2-stage pipeline
        let test_sequence = [0x42u64, 0x55u64, 0xAAu64, 0xFFu64];

        for (cycle, &input_val) in test_sequence.iter().enumerate() {
            // Set input
            sim.set_input("data_in", input_val).await;

            // Clock low
            sim.set_input("clk", 0).await;
            sim.step().await;

            // Clock high (rising edge)
            sim.set_input("clk", 1).await;
            sim.step().await;

            // Read output
            let output = sim.get_output("data_out").await.unwrap_or(0);

            eprintln!(
                "Cycle {}: input=0x{:02x}, output=0x{:02x}",
                cycle, input_val, output
            );

            // With "settle after clock" semantics, outputs reflect state AFTER the clock edge.
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

        println!("Hierarchical pipeline simulation test passed!");
    }

    #[tokio::test]
    async fn test_hierarchical_pipeline_cpu() {
        // Test hierarchical module instantiation with CPU runtime
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
        sim.set_input("rst", 1).await;
        sim.set_input("clk", 0).await;
        sim.set_input("data_in", 0).await;

        // Reset for 2 cycles
        for i in 0..4 {
            sim.set_input("clk", (i % 2) as u64).await;
            sim.step().await;
        }

        // Release reset
        sim.set_input("rst", 0).await;

        // Feed data through the 2-stage pipeline
        let test_sequence = [0x42u64, 0x55u64, 0xAAu64, 0xFFu64];

        for (cycle, &input_val) in test_sequence.iter().enumerate() {
            // Set input
            sim.set_input("data_in", input_val).await;

            // Clock low
            sim.set_input("clk", 0).await;
            sim.step().await;

            // Clock high (rising edge)
            sim.set_input("clk", 1).await;
            sim.step().await;

            // Read output
            let output = sim.get_output("data_out").await.unwrap_or(0);

            eprintln!(
                "CPU: Cycle {}: input=0x{:02x}, output=0x{:02x}",
                cycle, input_val, output
            );

            // With "settle after clock" semantics, outputs reflect state AFTER the clock edge.
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

        println!("CPU hierarchical pipeline simulation test passed!");
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
        sim.set_input("rst", 1).await;
        sim.set_input("we", 0).await;
        sim.set_input("waddr", 0).await;
        sim.set_input("wdata", 0).await;
        sim.set_input("raddr", 0).await;
        sim.set_input("clk", 0).await;

        // Reset for 2 cycles
        for i in 0..4 {
            sim.set_input("clk", (i % 2) as u64).await;
            sim.step().await;
        }

        // Release reset
        sim.set_input("rst", 0).await;

        // Write test values to registers
        let test_values = [0x42u64, 0x55u64, 0xAAu64, 0xFFu64];
        for (addr, &value) in test_values.iter().enumerate() {
            sim.set_input("we", 1).await;
            sim.set_input("waddr", addr as u64).await;
            sim.set_input("wdata", value).await;

            // Clock cycle
            sim.set_input("clk", 0).await;
            sim.step().await;
            sim.set_input("clk", 1).await;
            sim.step().await;
        }

        // Disable write
        sim.set_input("we", 0).await;

        // Read back and verify
        for (addr, &expected) in test_values.iter().enumerate() {
            sim.set_input("raddr", addr as u64).await;

            // Clock cycle (for registered output timing)
            sim.set_input("clk", 0).await;
            sim.step().await;

            let rdata = sim.get_output("rdata").await.unwrap_or(0);
            assert_eq!(
                rdata, expected,
                "Register {} should contain 0x{:02x}, got 0x{:02x}",
                addr, expected, rdata
            );
        }

        println!("CPU register file simulation test passed!");
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
        sim.set_input("rst", 1).await;
        sim.set_input("we", 0).await;
        sim.set_input("waddr", 0).await;
        sim.set_input("wdata", 0).await;
        sim.set_input("raddr", 0).await;
        sim.set_input("clk", 0).await;

        // Reset for 2 cycles
        for i in 0..4 {
            sim.set_input("clk", (i % 2) as u64).await;
            sim.step().await;
        }

        // Release reset
        sim.set_input("rst", 0).await;

        // Write test values to registers
        let test_values = [0x42u64, 0x55u64, 0xAAu64, 0xFFu64];
        for (addr, &value) in test_values.iter().enumerate() {
            sim.set_input("we", 1).await;
            sim.set_input("waddr", addr as u64).await;
            sim.set_input("wdata", value).await;

            // Clock cycle
            sim.set_input("clk", 0).await;
            sim.step().await;
            sim.set_input("clk", 1).await;
            sim.step().await;
        }

        // Disable write
        sim.set_input("we", 0).await;

        // Read back and verify
        for (addr, &expected) in test_values.iter().enumerate() {
            sim.set_input("raddr", addr as u64).await;

            // Clock cycle (for registered output timing)
            sim.set_input("clk", 0).await;
            sim.step().await;

            let rdata = sim.get_output("rdata").await.unwrap_or(0);
            assert_eq!(
                rdata, expected,
                "GPU Register {} should contain 0x{:02x}, got 0x{:02x}",
                addr, expected, rdata
            );
        }

        println!("GPU register file simulation test passed!");
    }
}
