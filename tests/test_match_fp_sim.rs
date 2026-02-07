#[cfg(all(test, target_os = "macos"))]
mod test_match_fp {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};
    use skalp_sim::{HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
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

        // Test FP32 addition: 1.0 + 2.0 = 3.0
        let a: f32 = 1.0;
        let b: f32 = 2.0;
        let expected: f32 = 3.0;

        simulator.set_input("sel", 0).await;
        simulator.set_input("a", a.to_bits() as u64).await;
        simulator.set_input("b", b.to_bits() as u64).await;

        simulator.step().await;

        let result_bits = simulator.get_output("result").await.unwrap_or(0) as u32;
        let result = f32::from_bits(result_bits);

        println!(
            "Match FP addition: {} + {} = {} (expected {})",
            a, b, result, expected
        );
        assert!((result - expected).abs() < 0.0001, "FP addition mismatch");
    }
}
