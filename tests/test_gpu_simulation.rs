#[cfg(all(test, target_os = "macos"))]
mod gpu_simulation_tests {
    use skalp_frontend::{parse_and_build_hir, parse_and_build_hir_from_file};
    use skalp_mir::{MirCompiler, OptimizationLevel};
    use skalp_sim::waveform::Waveform;
    use skalp_sim::{HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
    use skalp_sir::convert_mir_to_sir;
    use std::fs;
    use std::io::Write;
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
        let config = UnifiedSimConfig {
            level: SimLevel::Behavioral,
            hw_accel: HwAccel::Gpu,
            max_cycles: 100,
            capture_waveforms: true,
            ..Default::default()
        };

        // Create simulator
        let mut simulator = UnifiedSimulator::new(config)
            .expect("Failed to create GPU simulator");

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
        assert!(!states.is_empty() || !result.outputs.is_empty(), "Should have captured simulation states or outputs");

        println!("Captured {} snapshot states, {} outputs", states.len(), result.outputs.len());

        println!("GPU Simulation Test Complete!");
    }

    #[tokio::test]
    #[ignore = "Testbench module was removed in unified simulator refactoring"]
    async fn test_counter_with_testbench() {
        // This test used the old Testbench API which was removed.
        // It needs to be rewritten to use UnifiedSimulator directly.
        panic!("Testbench module was removed");
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
        let config = UnifiedSimConfig {
            level: SimLevel::Behavioral,
            hw_accel: HwAccel::Gpu,
            max_cycles: 20,
            capture_waveforms: true,
            ..Default::default()
        };

        // Create simulator
        let mut simulator = UnifiedSimulator::new(config)
            .expect("Failed to create GPU simulator");

        // Load the module
        simulator
            .load_behavioral(&sir)
            .await
            .expect("Failed to load SIR module");

        // Test: 256-bit addition
        // a = 0x0000...0005 (5 in lower 32 bits)
        // b = 0x0000...0003 (3 in lower 32 bits)
        // Expected sum = 0x0000...0008 (8 in lower 32 bits)
        let a_value: u64 = 5;
        let b_value: u64 = 3;

        // Set initial inputs
        simulator.set_input("rst", 1).await;
        simulator.set_input("clk", 0).await;
        simulator.set_input("a", a_value).await;
        simulator.set_input("b", b_value).await;

        // Reset cycle
        simulator.step().await;

        // Release reset
        simulator.set_input("rst", 0).await;

        // Clock edge
        simulator.set_input("clk", 1).await;
        simulator.step().await;

        // Check combinational sum output
        let sum_output = simulator
            .get_output("sum")
            .await
            .expect("Failed to get sum output");

        println!("Sum output: {}", sum_output);

        // Verify sum is 8 (5 + 3)
        assert_eq!(
            sum_output, 8,
            "256-bit addition failed: expected 8, got {}",
            sum_output
        );

        // Clock low
        simulator.set_input("clk", 0).await;
        simulator.step().await;

        // Check latched output
        let sum_latched = simulator
            .get_output("sum_latched")
            .await
            .expect("Failed to get sum_latched output");

        println!("Sum latched: {}", sum_latched);

        assert_eq!(
            sum_latched, 8,
            "256-bit latched addition failed: expected 8, got {}",
            sum_latched
        );

        println!("GPU 256-bit operations test PASSED!");
    }

    #[ignore = "requires stdlib parsing support for fp.sk advanced syntax"]
    #[tokio::test]
    async fn test_bug_66_chained_fp32_addition() {
        // Bug #66: Chained FP32 addition returns wrong values
        // Pattern: (a + b) + c only computes first two terms
        // Expected: a + b + c = all three terms
        let source = r#"
        use skalp::numeric::fp::*;
        use skalp::numeric::formats::fp32;

        entity TestChainedAddition {
            in clk: clock
            in rst: reset
            in a: fp32
            in b: fp32
            in c: fp32
            out result_broken: fp32
            out result_working: fp32
        }

        impl TestChainedAddition {
            signal result_broken_reg: fp32
            signal result_working_reg: fp32

            // BROKEN: Chained addition
            signal broken: fp32
            let broken_fp = a + b + c;
            broken = broken_fp

            // WORKING: Separate statements
            signal working: fp32
            let sum_ab = a + b;
            let working_fp = sum_ab + c;
            working = working_fp

            result_broken = result_broken_reg
            result_working = result_working_reg

            on(clk.rise) {
                if rst {
                    result_broken_reg = 0 as fp32
                    result_working_reg = 0 as fp32
                } else {
                    result_broken_reg = broken
                    result_working_reg = working
                }
            }
        }
        "#;

        println!("\n=== Bug #66: Chained FP32 Addition Test ===");

        // Set stdlib path
        std::env::set_var("SKALP_STDLIB_PATH", "./crates/skalp-stdlib");

        // Write source to temp file for module resolution
        let temp_dir = std::env::temp_dir();
        let temp_file = temp_dir.join("chained_fp32_test.sk");
        let mut file = std::fs::File::create(&temp_file).expect("Failed to create temp file");
        file.write_all(source.as_bytes())
            .expect("Failed to write temp file");

        // Parse with module resolution (loads stdlib imports)
        let hir = parse_and_build_hir_from_file(&temp_file)
            .expect("Failed to parse chained addition test design with stdlib");

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
        let config = UnifiedSimConfig {
            level: SimLevel::Behavioral,
            hw_accel: HwAccel::Gpu,
            max_cycles: 10,
            capture_waveforms: false,
            ..Default::default()
        };

        // Create simulator
        let mut simulator = UnifiedSimulator::new(config)
            .expect("Failed to create GPU simulator");

        // Load the module
        simulator
            .load_behavioral(&sir)
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
        simulator.set_input("clk", 0).await;
        simulator.set_input("rst", 1).await;
        simulator.set_input("a", a.to_bits() as u64).await;
        simulator.set_input("b", b.to_bits() as u64).await;
        simulator.set_input("c", c.to_bits() as u64).await;
        simulator.step().await;

        // Clock rise with reset
        simulator.set_input("clk", 1).await;
        simulator.step().await;

        // Clock fall, release reset
        simulator.set_input("clk", 0).await;
        simulator.set_input("rst", 0).await;
        simulator.step().await;

        // Clock rise - compute happens
        simulator.set_input("clk", 1).await;
        simulator.step().await;

        // Read results
        let result_broken_bits = simulator.get_output("result_broken").await.unwrap_or(0);
        let result_working_bits = simulator.get_output("result_working").await.unwrap_or(0);

        let result_broken = f32::from_bits(result_broken_bits as u32);
        let result_working = f32::from_bits(result_working_bits as u32);

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

        println!("\n Bug #66 FIXED: Chained FP32 addition now works correctly!");
    }

    #[tokio::test]
    #[ignore = "requires manual setup of /tmp/test_bug67_fp16_type.sk"]
    async fn test_bug67_fp16_type_metal() {
        println!(
            "\n Testing Bug #67: FP16 type inference in tuple destructuring with match arms"
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

        println!("Compiled to SIR successfully");

        // Create GPU simulation config - this will trigger Metal shader generation
        let config = UnifiedSimConfig {
            level: SimLevel::Behavioral,
            hw_accel: HwAccel::Gpu,
            max_cycles: 10,
            capture_waveforms: false,
            ..Default::default()
        };

        println!("Creating GPU simulator - this will trigger Metal shader generation...");

        // Try to create GPU simulator - this is where Metal shader generation happens
        let mut simulator = UnifiedSimulator::new(config)
            .expect("Failed to create GPU simulator");

        println!("GPU simulator created");

        // Load the module - this is where Metal compilation happens
        println!("Loading SIR module - this will compile Metal shader...");
        let load_result = simulator.load_behavioral(&sir).await;

        if let Err(e) = load_result {
            let error_msg = format!("{}", e);
            println!("Metal shader compilation failed (Bug #67):");
            println!("   {}", error_msg);

            // Check if this is the expected FP16 type mismatch error
            if error_msg.contains("as_type")
                || error_msg.contains("half")
                || error_msg.contains("cannot convert")
            {
                println!("\n Bug #67 REPRODUCED: Metal shader has FP16 type error");
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

        println!("Metal shader compiled successfully");
        println!("   Bug #67 may be FIXED if this test passes!");
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
