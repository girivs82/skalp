// CPU simulation tests for floating-point arithmetic
// These tests verify proper IEEE 754 FP operations in the CPU simulator

#[cfg(test)]
mod cpu_fp_simulation_tests {
    use skalp_frontend::parse_and_build_hir_from_file;
    use skalp_mir::lower_to_mir;
    use skalp_sim::{HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
    use std::io::Write;

    async fn setup_cpu_simulator(source: &str) -> UnifiedSimulator {
        // Set stdlib path
        std::env::set_var("SKALP_STDLIB_PATH", "./crates/skalp-stdlib");

        // Write source to temp file for module resolution
        let temp_dir = std::env::temp_dir();
        let temp_file = temp_dir.join("cpu_fp_test.sk");
        let mut file = std::fs::File::create(&temp_file).expect("Failed to create temp file");
        file.write_all(source.as_bytes())
            .expect("Failed to write temp file");

        // Parse with module resolution (loads stdlib imports)
        let hir =
            parse_and_build_hir_from_file(&temp_file).expect("Failed to parse design with stdlib");

        // Compile to MIR
        let mir = lower_to_mir(&hir).expect("Failed to compile to MIR");

        // Convert to SIR
        assert!(!mir.modules.is_empty());
        let top_module = &mir.modules[mir.modules.len() - 1];
        let sir = skalp_sir::convert_mir_to_sir_with_hierarchy(&mir, top_module);

        // Create CPU simulator using UnifiedSimulator
        let config = UnifiedSimConfig {
            level: SimLevel::Behavioral,
            hw_accel: HwAccel::Cpu,
            max_cycles: 100,
            capture_waveforms: false,
            ..Default::default()
        };

        let mut simulator = UnifiedSimulator::new(config)
            .expect("Failed to create simulator");

        simulator
            .load_behavioral(&sir)
            .await
            .expect("Failed to load module");

        simulator
    }

    #[ignore = "requires stdlib parsing support for fp.sk advanced syntax"]
    #[tokio::test]
    async fn test_fp32_addition_cpu() {
        let source = r#"
        use skalp::numeric::fp::*;
        use skalp::numeric::formats::fp32;

        entity FP32Add {
            in a: fp32
            in b: fp32
            out result: fp32
        }

        impl FP32Add {
            result = a + b
        }
        "#;

        let mut sim = setup_cpu_simulator(source).await;

        // Test: 2.5 + 3.5 = 6.0
        let a: f32 = 2.5;
        let b: f32 = 3.5;
        let expected: f32 = 6.0;

        sim.set_input("a", u32::from_le_bytes(a.to_le_bytes()) as u64).await;
        sim.set_input("b", u32::from_le_bytes(b.to_le_bytes()) as u64).await;
        sim.run(1).await;

        let result_u64 = sim.get_output_u64("result").await.unwrap_or(0);
        let result = f32::from_le_bytes((result_u64 as u32).to_le_bytes());

        assert_eq!(
            result, expected,
            "FP32 addition: 2.5 + 3.5 should equal 6.0"
        );
    }

    #[ignore = "requires stdlib parsing support for fp.sk advanced syntax"]
    #[tokio::test]
    async fn test_fp32_multiplication_cpu() {
        let source = r#"
        use skalp::numeric::fp::*;
        use skalp::numeric::formats::fp32;

        entity FP32Mul {
            in a: fp32
            in b: fp32
            out result: fp32
        }

        impl FP32Mul {
            result = a * b
        }
        "#;

        let mut sim = setup_cpu_simulator(source).await;

        // Test: 3.0 * 4.0 = 12.0
        let a: f32 = 3.0;
        let b: f32 = 4.0;
        let expected: f32 = 12.0;

        sim.set_input("a", u32::from_le_bytes(a.to_le_bytes()) as u64).await;
        sim.set_input("b", u32::from_le_bytes(b.to_le_bytes()) as u64).await;
        sim.run(1).await;

        let result_u64 = sim.get_output_u64("result").await.unwrap_or(0);
        let result = f32::from_le_bytes((result_u64 as u32).to_le_bytes());

        assert_eq!(
            result, expected,
            "FP32 multiplication: 3.0 * 4.0 should equal 12.0"
        );
    }

    #[ignore = "requires stdlib parsing support for fp.sk advanced syntax"]
    #[tokio::test]
    async fn test_fp32_comparison_cpu() {
        let source = r#"
        use skalp::numeric::fp::*;
        use skalp::numeric::formats::fp32;

        entity FP32Compare {
            in a: fp32
            in b: fp32
            out lt: bit<1>
            out eq: bit<1>
            out gt: bit<1>
        }

        impl FP32Compare {
            lt = a < b
            eq = a == b
            gt = a > b
        }
        "#;

        let mut sim = setup_cpu_simulator(source).await;

        // Test: 2.5 < 3.5
        let a: f32 = 2.5;
        let b: f32 = 3.5;

        sim.set_input("a", u32::from_le_bytes(a.to_le_bytes()) as u64).await;
        sim.set_input("b", u32::from_le_bytes(b.to_le_bytes()) as u64).await;
        sim.run(1).await;

        let lt = sim.get_output_u64("lt").await.unwrap_or(0);
        let eq = sim.get_output_u64("eq").await.unwrap_or(0);
        let gt = sim.get_output_u64("gt").await.unwrap_or(0);

        assert_eq!(lt, 1, "2.5 < 3.5 should be true");
        assert_eq!(eq, 0, "2.5 == 3.5 should be false");
        assert_eq!(gt, 0, "2.5 > 3.5 should be false");
    }

    #[ignore = "requires stdlib parsing support for fp.sk advanced syntax"]
    #[tokio::test]
    async fn test_fp32_negation_cpu() {
        let source = r#"
        use skalp::numeric::fp::*;
        use skalp::numeric::formats::fp32;

        entity FP32Negate {
            in x: fp32
            out neg: fp32
        }

        impl FP32Negate {
            neg = -x
        }
        "#;

        let mut sim = setup_cpu_simulator(source).await;

        // Test with x = -9.0
        let x: f32 = -9.0;

        sim.set_input("x", u32::from_le_bytes(x.to_le_bytes()) as u64).await;
        sim.run(1).await;

        // Negation: -(-9.0) = 9.0
        let neg_u64 = sim.get_output_u64("neg").await.unwrap_or(0);
        let neg = f32::from_le_bytes((neg_u64 as u32).to_le_bytes());
        assert_eq!(neg, 9.0, "Negation of -9.0 should be 9.0");
    }
}
