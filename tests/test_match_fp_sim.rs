#[cfg(all(test, target_os = "macos"))]
mod test_match_fp {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};
    use skalp_sim::{SimulationConfig, Simulator};
    use skalp_sir::convert_mir_to_sir;
    use std::fs;

    #[tokio::test]
    #[ignore = "requires manual setup of /tmp/test_match_fp.sk"]
    async fn test_match_fp_addition() {
        // Read test design
        let source = fs::read_to_string("/tmp/test_match_fp.sk").expect("Failed to read test file");

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

        // Test FP32 addition: 1.0 + 2.0 = 3.0
        let a: f32 = 1.0;
        let b: f32 = 2.0;
        let expected: f32 = 3.0;

        simulator.set_input("sel", vec![0]).await.unwrap();
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
            "✅ Match FP addition: {} + {} = {} (expected {})",
            a, b, result, expected
        );
        assert!((result - expected).abs() < 0.0001, "FP addition mismatch");
    }
}
