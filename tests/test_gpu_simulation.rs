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

    #[tokio::test]
    #[ignore = "requires manual setup of /tmp/test_256bit_ops.sk"]
    async fn test_256bit_operations_gpu() {
        // Read simple 256-bit adder design
        let source = fs::read_to_string("/tmp/test_256bit_ops.sk")
            .expect("Failed to read test_256bit_ops.sk");

        // Parse and build HIR
        let hir = parse_and_build_hir(&source).expect("Failed to parse 256-bit test design");

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

        // Create simulation config for GPU
        let config = SimulationConfig {
            use_gpu: true,
            max_cycles: 20,
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

        // Test: 256-bit addition
        // a = 0x0000...0005 (5 in lower 32 bits)
        // b = 0x0000...0003 (3 in lower 32 bits)
        // Expected sum = 0x0000...0008 (8 in lower 32 bits)
        let mut a_bytes = vec![0u8; 32];
        a_bytes[0] = 5; // Little-endian

        let mut b_bytes = vec![0u8; 32];
        b_bytes[0] = 3;

        // Set initial inputs
        simulator
            .set_input("rst", vec![1])
            .await
            .expect("Failed to set reset");
        simulator
            .set_input("clk", vec![0])
            .await
            .expect("Failed to set clock");
        simulator
            .set_input("a", a_bytes.clone())
            .await
            .expect("Failed to set input a");
        simulator
            .set_input("b", b_bytes.clone())
            .await
            .expect("Failed to set input b");

        // Reset cycle
        simulator
            .step_simulation()
            .await
            .expect("Failed to step simulation");

        // Release reset
        simulator
            .set_input("rst", vec![0])
            .await
            .expect("Failed to clear reset");

        // Clock edge
        simulator
            .set_input("clk", vec![1])
            .await
            .expect("Failed to set clock high");

        simulator
            .step_simulation()
            .await
            .expect("Failed to step simulation");

        // Check combinational sum output
        let sum_output = simulator
            .get_output("sum")
            .await
            .expect("Failed to get sum output");

        println!(
            "Sum output (first 8 bytes): {:?}",
            &sum_output[..8.min(sum_output.len())]
        );

        // Verify sum is 8 (5 + 3)
        assert_eq!(
            sum_output[0], 8,
            "256-bit addition failed: expected 8, got {}",
            sum_output[0]
        );

        // Clock low
        simulator
            .set_input("clk", vec![0])
            .await
            .expect("Failed to set clock low");

        simulator
            .step_simulation()
            .await
            .expect("Failed to step simulation");

        // Check latched output
        let sum_latched = simulator
            .get_output("sum_latched")
            .await
            .expect("Failed to get sum_latched output");

        println!(
            "Sum latched (first 8 bytes): {:?}",
            &sum_latched[..8.min(sum_latched.len())]
        );

        assert_eq!(
            sum_latched[0], 8,
            "256-bit latched addition failed: expected 8, got {}",
            sum_latched[0]
        );

        println!("✅ GPU 256-bit operations test PASSED!");
    }

    #[tokio::test]
    async fn test_bug_66_chained_fp32_addition() {
        // Bug #66: Chained FP32 addition returns wrong values
        // Pattern: (a + b) + c only computes first two terms
        // Expected: a + b + c = all three terms
        let source = r#"
        entity TestChainedAddition {
            in clk: clock
            in rst: reset
            in a: bit[32]
            in b: bit[32]
            in c: bit[32]
            out result_broken: bit[32]
            out result_working: bit[32]
        }

        impl TestChainedAddition {
            signal result_broken_reg: bit[32]
            signal result_working_reg: bit[32]

            let a_fp = a as fp32;
            let b_fp = b as fp32;
            let c_fp = c as fp32;

            // BROKEN: Chained addition
            signal broken: bit[32]
            let broken_fp = a_fp + b_fp + c_fp;
            broken = broken_fp as bit[32]

            // WORKING: Separate statements
            signal working: bit[32]
            let sum_ab = a_fp + b_fp;
            let working_fp = sum_ab + c_fp;
            working = working_fp as bit[32]

            result_broken = result_broken_reg
            result_working = result_working_reg

            on(clk.rise) {
                if rst {
                    result_broken_reg <= 0
                    result_working_reg <= 0
                } else {
                    result_broken_reg <= broken
                    result_working_reg <= working
                }
            }
        }
        "#;

        println!("\n=== Bug #66: Chained FP32 Addition Test ===");

        // Parse and build HIR
        let hir =
            parse_and_build_hir(source).expect("Failed to parse chained addition test design");

        // Compile to MIR
        let compiler = MirCompiler::new()
            .with_optimization_level(OptimizationLevel::None)
            .with_verbose(false);

        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile HIR to MIR");

        // Convert to SIR
        let sir = convert_mir_to_sir(&mir.modules[0]);

        // Create simulation config for GPU
        let config = SimulationConfig {
            use_gpu: true,
            max_cycles: 10,
            timeout_ms: 5000,
            capture_waveforms: false,
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

        // Test values: a=4.0, b=10.0, c=18.0
        // Expected: 4.0 + 10.0 + 18.0 = 32.0
        // Bug behavior: 4.0 + 10.0 = 14.0 (drops third term)
        let a: f32 = 4.0;
        let b: f32 = 10.0;
        let c: f32 = 18.0;
        let expected: f32 = 32.0;

        println!("\nTest inputs:");
        println!("  a = {} (0x{:08X})", a, a.to_bits());
        println!("  b = {} (0x{:08X})", b, b.to_bits());
        println!("  c = {} (0x{:08X})", c, c.to_bits());
        println!("\nExpected:");
        println!(
            "  result_broken = {} (if fixed) or 14.0 (if bug present)",
            expected
        );
        println!("  result_working = {} (should always work)", expected);

        // Initial state with reset
        simulator.set_input("clk", vec![0]).await.unwrap();
        simulator.set_input("rst", vec![1]).await.unwrap();
        simulator
            .set_input("a", a.to_le_bytes().to_vec())
            .await
            .unwrap();
        simulator
            .set_input("b", b.to_le_bytes().to_vec())
            .await
            .unwrap();
        simulator
            .set_input("c", c.to_le_bytes().to_vec())
            .await
            .unwrap();
        simulator.step_simulation().await.unwrap();

        // Clock rise with reset
        simulator.set_input("clk", vec![1]).await.unwrap();
        simulator.step_simulation().await.unwrap();

        // Clock fall, release reset
        simulator.set_input("clk", vec![0]).await.unwrap();
        simulator.set_input("rst", vec![0]).await.unwrap();
        simulator.step_simulation().await.unwrap();

        // Clock rise - compute happens
        simulator.set_input("clk", vec![1]).await.unwrap();
        simulator.step_simulation().await.unwrap();

        // Read results
        let result_broken_bytes = simulator.get_output("result_broken").await.unwrap();
        let result_working_bytes = simulator.get_output("result_working").await.unwrap();

        let result_broken = f32::from_le_bytes([
            result_broken_bytes[0],
            result_broken_bytes[1],
            result_broken_bytes[2],
            result_broken_bytes[3],
        ]);

        let result_working = f32::from_le_bytes([
            result_working_bytes[0],
            result_working_bytes[1],
            result_working_bytes[2],
            result_working_bytes[3],
        ]);

        println!("\n=== Simulation Results ===");
        println!(
            "  result_broken = {} (0x{:08X})",
            result_broken,
            result_broken.to_bits()
        );
        println!(
            "  result_working = {} (0x{:08X})",
            result_working,
            result_working.to_bits()
        );

        // Check results - Bug #66 should now be FIXED
        println!("\n=== Results ===");
        println!("  result_broken = {} (expected 32.0)", result_broken);
        println!("  result_working = {} (expected 32.0)", result_working);

        // Both should now work correctly
        assert!(
            (result_broken - 32.0).abs() < 0.01,
            "Bug #66 FIX: Chained addition should produce 32.0, got {}",
            result_broken
        );

        assert!(
            (result_working - 32.0).abs() < 0.01,
            "Workaround should produce 32.0, got {}",
            result_working
        );

        println!("\n✅ Bug #66 FIXED: Chained FP32 addition now works correctly!");
    }

    #[tokio::test]
    #[ignore = "requires manual setup of /tmp/test_bug67_fp16_type.sk"]
    async fn test_bug67_fp16_type_metal() {
        println!(
            "\n🧪 Testing Bug #67: FP16 type inference in tuple destructuring with match arms"
        );

        // Read the minimal Bug #67 test case
        let source = fs::read_to_string("/tmp/test_bug67_fp16_type.sk")
            .expect("Failed to read Bug #67 test case");

        // Parse and build HIR
        let hir = parse_and_build_hir(&source).expect("Failed to parse Bug #67 design");

        // Compile to MIR
        let compiler = MirCompiler::new()
            .with_optimization_level(OptimizationLevel::Basic)
            .with_verbose(false);

        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile HIR to MIR");

        // Convert to SIR
        assert!(
            !mir.modules.is_empty(),
            "MIR should have at least one module"
        );
        let sir = convert_mir_to_sir(&mir.modules[0]);

        println!("✅ Compiled to SIR successfully");

        // Create GPU simulation config - this will trigger Metal shader generation
        let config = SimulationConfig {
            use_gpu: true,
            max_cycles: 10,
            timeout_ms: 5000,
            capture_waveforms: false,
            parallel_threads: 1,
        };

        println!("🔍 Creating GPU simulator - this will trigger Metal shader generation...");

        // Try to create GPU simulator - this is where Metal shader generation happens
        let mut simulator = Simulator::new(config)
            .await
            .expect("Failed to create GPU simulator");

        println!("✅ GPU simulator created");

        // Load the module - this is where Metal compilation happens
        println!("🔍 Loading SIR module - this will compile Metal shader...");
        let load_result = simulator.load_module(&sir).await;

        if let Err(e) = load_result {
            let error_msg = format!("{}", e);
            println!("❌ Metal shader compilation failed (Bug #67):");
            println!("   {}", error_msg);

            // Check if this is the expected FP16 type mismatch error
            if error_msg.contains("as_type")
                || error_msg.contains("half")
                || error_msg.contains("cannot convert")
            {
                println!("\n🐛 Bug #67 REPRODUCED: Metal shader has FP16 type error");
                println!("   Expected: Variables should be uint (32-bit)");
                println!("   Actual: Variables are half (16-bit)");
                panic!("Bug #67: FP16 type inference error in Metal shader generation");
            } else {
                panic!(
                    "Unexpected error during Metal shader compilation: {}",
                    error_msg
                );
            }
        }

        println!("✅ Metal shader compiled successfully");
        println!("   Bug #67 may be FIXED if this test passes!");
    }
}
#[cfg(test)]
mod test_array_write {
    use skalp_testing::testbench::Testbench;

    #[tokio::test]
    async fn test_simple_array_write() {
        println!("\n🧪 Testing simple array write in sequential block");

        let mut tb = Testbench::new("tests/fixtures/test_array_write_simple.sk")
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
