#[cfg(all(test, target_os = "macos"))]
mod gpu_simulation_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};
    use skalp_sim::{HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
    use skalp_sir::convert_mir_to_sir;
    use std::fs;

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
        let config = UnifiedSimConfig {
            level: SimLevel::Behavioral,
            hw_accel: HwAccel::Gpu,
            max_cycles: 100,
            capture_waveforms: true,
            ..Default::default()
        };

        // Create simulator
        let mut simulator = UnifiedSimulator::new(config).expect("Failed to create GPU simulator");

        // Load the module
        simulator
            .load_behavioral(&sir)
            .await
            .expect("Failed to load SIR module");

        // Helper to resolve user-facing names to internal names
        let resolve = |name: &str| -> String {
            sir.name_registry
                .resolve(name)
                .map(|s| s.to_string())
                .unwrap_or_else(|| name.to_string())
        };

        // Resolve clock and reset names for use in loops
        let clk_name = resolve("clk");
        let rst_name = resolve("rst");

        // Set initial inputs - reset high, clock low
        simulator.set_input(&rst_name, 1).await;
        simulator.set_input(&clk_name, 0).await;

        // Run for a few cycles with reset high, toggling clock
        for i in 0..5 {
            // Toggle clock
            simulator.set_input(&clk_name, (i % 2) as u64).await;
            simulator.step().await;
        }

        // Release reset and continue toggling clock
        simulator.set_input(&rst_name, 0).await;

        // Run for more cycles with clock toggling
        for i in 0..20 {
            // Toggle clock
            simulator.set_input(&clk_name, ((i + 5) % 2) as u64).await;
            simulator.step().await;
        }

        // Get the waveforms
        let result = simulator.run(0).await;
        let states = &result.waveforms;
        assert!(
            !states.is_empty() || !result.outputs.is_empty(),
            "Should have captured simulation states or outputs"
        );

        println!(
            "Captured {} snapshot states, {} outputs",
            states.len(),
            result.outputs.len()
        );

        println!("GPU Simulation Test Complete!");
    }
}

#[cfg(test)]
mod test_array_write {
    use skalp_testing::testbench::Testbench;

    #[tokio::test]
    async fn test_simple_array_write() {
        println!("\n Testing simple array write in sequential block");

        let mut tb = Testbench::new("tests/fixtures/test_array_write_simple.sk")
            .await
            .expect("Failed to create testbench");

        println!("Testbench created");

        // Reset
        tb.set("rst", 1u8);
        tb.clock(2).await;
        tb.set("rst", 0u8);
        tb.clock(1).await;

        println!("Reset complete");

        // Write 0xDEADBEEF to address 0
        tb.set("wr_en", 1u8)
            .set("wr_data", 0xDEADBEEFu32)
            .set("wr_addr", 0u8);

        tb.clock(1).await;
        println!("Wrote 0xDEADBEEF to mem[0]");

        // Disable write
        tb.set("wr_en", 0u8);
        tb.clock(1).await;

        // Read from mem[0]
        let rd_data: u32 = tb.get_as("rd_data").await;

        println!("Read from mem[0]: {:08X}", rd_data);
        println!("Expected: DEADBEEF");

        assert_eq!(rd_data, 0xDEADBEEF, "Should read back written value");

        println!("Test PASSED!");
    }
}
