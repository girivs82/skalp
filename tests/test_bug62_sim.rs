#[cfg(all(test, target_os = "macos"))]
mod test_bug62_direct {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};
    use skalp_sim::{SimulationConfig, Simulator};
    use skalp_sir::convert_mir_to_sir;
    use std::fs;

    // This test requires:
    // 1. An external file that may not exist in CI environments
    // 2. Trait-based FP32 operations via stdlib (impl Add for fp32)
    // Currently skipped until stdlib trait integration is complete.
    #[ignore = "requires external file and stdlib FP32 trait implementations"]
    #[tokio::test]
    async fn test_bug62_with_simulator() {
        // This test requires an external file that may not exist in CI environments.
        // Skip gracefully if the file doesn't exist.
        let source = match fs::read_to_string(
            "/Users/girivs/src/hw/karythra/reproducer_tests/test_match_fp_bug.sk",
        ) {
            Ok(s) => s,
            Err(_) => {
                println!("⚠️  Skipping test_bug62_with_simulator: external test file not found");
                return;
            }
        };

        // Parse and build HIR
        let hir = parse_and_build_hir(&source).expect("Failed to parse design");

        // Compile to MIR
        let compiler = MirCompiler::new()
            .with_optimization_level(OptimizationLevel::Basic)
            .with_verbose(false);

        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile HIR to MIR");

        // Convert to SIR for GPU simulation
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

        // Test FP32 addition: 1.5 + 2.5 = 4.0
        let a: f32 = 1.5;
        let b: f32 = 2.5;
        let expected: f32 = 4.0;

        simulator.set_input("opcode", vec![23]).await.unwrap();
        simulator
            .set_input("a", a.to_bits().to_le_bytes().to_vec())
            .await
            .unwrap();
        simulator
            .set_input("b", b.to_bits().to_le_bytes().to_vec())
            .await
            .unwrap();

        simulator.step_simulation().await.unwrap();

        let result_bytes: Vec<u8> = simulator.get_output("result").await.unwrap();
        let result_bits = u32::from_le_bytes([
            result_bytes[0],
            result_bytes[1],
            result_bytes[2],
            result_bytes[3],
        ]);
        let result = f32::from_bits(result_bits);

        println!(
            "✅ Bug #62 test with Simulator: {} + {} = {} (expected {})",
            a, b, result, expected
        );
        assert!(
            (result - expected).abs() < 0.0001,
            "Bug #62: Expected {}, got {}",
            expected,
            result
        );
    }
}
