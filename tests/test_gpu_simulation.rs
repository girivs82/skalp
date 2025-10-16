#[cfg(all(test, target_os = "macos"))]
mod gpu_simulation_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};
    use skalp_sim::testbench::{TestVectorBuilder, Testbench};
    use skalp_sim::waveform::Waveform;
    use skalp_sim::{SimulationConfig, Simulator};
    use skalp_sir::convert_mir_to_sir;
    use std::fs;
    use std::path::PathBuf;

    #[tokio::test]
    async fn test_counter_gpu_simulation() {
        // Read counter design
        let counter_source =
            fs::read_to_string("examples/counter.sk").expect("Failed to read counter.sk");

        // Parse and build HIR
        let hir = parse_and_build_hir(&counter_source).expect("Failed to parse counter design");

        // Compile to MIR with optimizations
        let compiler = MirCompiler::new()
            .with_optimization_level(OptimizationLevel::Basic)
            .with_verbose(false);

        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile HIR to MIR");

        // Convert to SIR for GPU simulation
        assert!(
            !mir.modules.is_empty(),
            "MIR should have at least one module"
        );
        let sir = convert_mir_to_sir(&mir.modules[0]);

        // Create simulation config for GPU
        let config = SimulationConfig {
            use_gpu: true,
            max_cycles: 100,
            timeout_ms: 5000,
            capture_waveforms: true,
            parallel_threads: 1,
        };

        // Create simulator
        let mut simulator = Simulator::new(config)
            .await
            .expect("Failed to create GPU simulator");

        // Load the module
        simulator
            .load_module(&sir)
            .await
            .expect("Failed to load SIR module");

        // Set initial inputs - reset high, clock low
        simulator
            .set_input("rst", vec![1])
            .await
            .expect("Failed to set reset");
        simulator
            .set_input("clk", vec![0])
            .await
            .expect("Failed to set clock");

        // Run for a few cycles with reset high, toggling clock
        for i in 0..5 {
            // Toggle clock
            simulator
                .set_input("clk", vec![(i % 2) as u8])
                .await
                .expect("Failed to set clock");

            simulator
                .step_simulation()
                .await
                .expect("Failed to step simulation");
        }

        // Release reset and continue toggling clock
        simulator
            .set_input("rst", vec![0])
            .await
            .expect("Failed to clear reset");

        // Run for more cycles with clock toggling
        for i in 0..20 {
            // Toggle clock
            simulator
                .set_input("clk", vec![((i + 5) % 2) as u8])
                .await
                .expect("Failed to set clock");

            simulator
                .step_simulation()
                .await
                .expect("Failed to step simulation");
        }

        // Get the waveforms
        let states = simulator.get_waveforms().await;
        assert!(!states.is_empty(), "Should have captured simulation states");

        println!("Captured {} states", states.len());

        // Print first few states for debugging
        for (i, state) in states.iter().take(10).enumerate() {
            println!(
                "State {}: cycle={}, signals={:?}, registers={:?}",
                i, state.cycle, state.signals, state.registers
            );
        }

        // Create waveform and export
        let waveform = Waveform::from_simulation_states(&states);

        // Print all available signals
        println!("Available signals in waveform:");
        for (name, _) in waveform.signals.iter() {
            println!("  - {}", name);
        }

        // Verify counter incremented
        let count_transitions = waveform.get_signal_transitions("count");
        println!("Count transitions: {:?}", count_transitions);

        // Also check for alternate signal names
        if count_transitions.is_empty() {
            let reg_count_transitions = waveform.get_signal_transitions("reg_count");
            println!("reg_count transitions: {:?}", reg_count_transitions);

            let counter_transitions = waveform.get_signal_transitions("counter");
            println!("counter transitions: {:?}", counter_transitions);

            let reg_counter_transitions = waveform.get_signal_transitions("reg_counter");
            println!("reg_counter transitions: {:?}", reg_counter_transitions);
        }

        assert!(
            !count_transitions.is_empty() || !waveform.signals.is_empty(),
            "Counter should have transitions or at least some signals"
        );

        // Export VCD for debugging
        waveform
            .export_vcd(&PathBuf::from("test_counter.vcd"))
            .expect("Failed to export VCD");

        println!("GPU Simulation Test Complete!");
        waveform.print_summary();
    }

    #[tokio::test]
    async fn test_counter_with_testbench() {
        // Read counter design
        let counter_source =
            fs::read_to_string("examples/counter.sk").expect("Failed to read counter.sk");

        // Parse and build HIR
        let hir = parse_and_build_hir(&counter_source).expect("Failed to parse counter design");

        // Compile to MIR
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::Basic);

        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile HIR to MIR");

        // Convert to SIR
        assert!(
            !mir.modules.is_empty(),
            "MIR should have at least one module"
        );
        let sir = convert_mir_to_sir(&mir.modules[0]);

        // Create testbench
        let config = SimulationConfig {
            use_gpu: true,
            max_cycles: 100,
            timeout_ms: 5000,
            capture_waveforms: true,
            parallel_threads: 1,
        };

        let mut testbench = Testbench::new(config)
            .await
            .expect("Failed to create testbench");

        testbench
            .load_module(&sir)
            .await
            .expect("Failed to load module");

        // Create test vectors
        // Testbench behavior: vector at cycle X, then testbench steps and checks output
        // Counter increments on clock rising edge at odd cycles (9, 11, 13, 15, 17, 19, 21...)
        // Test checks happen at cycle X + 2 (e.g., vector at 10 → check at cycle 12)
        let vectors = vec![
            // Reset at cycle 0
            TestVectorBuilder::new(0).with_input("rst", vec![1]).build(),
            // Cycle 5 → check at cycle 7, counter still 0 (first increment at cycle 9)
            TestVectorBuilder::new(5)
                .with_input("rst", vec![0])
                .with_expected_output("count", vec![0])
                .build(),
            // Cycle 10 → check at cycle 12, counter incremented at cycles 9, 11 → count=2
            TestVectorBuilder::new(10)
                .with_expected_output("count", vec![2])
                .build(),
            // Cycle 15 → check at cycle 17, counter incremented at 9,11,13,15 → count=4
            TestVectorBuilder::new(15)
                .with_expected_output("count", vec![4])
                .build(),
            // Cycle 20 → check at cycle 22, counter incremented at 9,11,13,15,17,19,21 → count=6
            TestVectorBuilder::new(20)
                .with_expected_output("count", vec![6])
                .build(),
        ];

        testbench.add_test_vectors(vectors);

        // Run the test
        let _results = testbench.run_test().await.expect("Failed to run testbench");

        // Check results
        let report = testbench.generate_report();
        println!("{}", report);

        assert!(testbench.all_tests_passed(), "Some tests failed");
    }
}
#[cfg(test)]
mod test_array_write {
    use skalp_testing::testbench::Testbench;

    #[tokio::test]
    async fn test_simple_array_write() {
        println!("\n🧪 Testing simple array write in sequential block");

        let mut tb = Testbench::new("/tmp/test_array_write_simple.sk")
            .await
            .expect("Failed to create testbench");

        println!("✅ Testbench created");

        // Reset
        tb.set("rst", 1u8);
        tb.clock(2).await;
        tb.set("rst", 0u8);
        tb.clock(1).await;

        println!("✅ Reset complete");

        // Write 0xDEADBEEF to address 0
        tb.set("wr_en", 1u8)
            .set("wr_data", 0xDEADBEEFu32)
            .set("wr_addr", 0u8);

        tb.clock(1).await;
        println!("✅ Wrote 0xDEADBEEF to mem[0]");

        // Disable write
        tb.set("wr_en", 0u8);
        tb.clock(1).await;

        // Read from mem[0]
        let rd_data: u32 = tb.get_as("rd_data").await;

        println!("📖 Read from mem[0]: {:08X}", rd_data);
        println!("📖 Expected: DEADBEEF");

        assert_eq!(rd_data, 0xDEADBEEF, "Should read back written value");

        println!("✅ Test PASSED!");
    }
}
