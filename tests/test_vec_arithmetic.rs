// Vector arithmetic tests for both GPU and CPU simulation
// Tests component-wise operations on vec2/3/4 types

#[cfg(test)]
mod vec_arithmetic_tests {
    use skalp_frontend::parse_and_build_hir_from_file;
    use skalp_mir::lower_to_mir;
    use skalp_sim::{HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
    use std::io::Write;

    async fn setup_simulator(source: &str, use_gpu: bool) -> UnifiedSimulator {
        // Set stdlib path
        std::env::set_var("SKALP_STDLIB_PATH", "./crates/skalp-stdlib");

        // Write source to temp file for module resolution
        let temp_dir = std::env::temp_dir();
        let temp_file = temp_dir.join("vec_arith_test.sk");
        let mut file = std::fs::File::create(&temp_file).expect("Failed to create temp file");
        file.write_all(source.as_bytes())
            .expect("Failed to write temp file");

        // Parse with module resolution (loads stdlib imports)
        let hir =
            parse_and_build_hir_from_file(&temp_file).expect("Failed to parse design with stdlib");

        let mir = lower_to_mir(&hir).expect("Failed to compile to MIR");
        assert!(!mir.modules.is_empty());
        let top_module = &mir.modules[mir.modules.len() - 1];
        let sir = skalp_sir::convert_mir_to_sir_with_hierarchy(&mir, top_module);

        let config = UnifiedSimConfig {
            level: SimLevel::Behavioral,
            hw_accel: if use_gpu { HwAccel::Gpu } else { HwAccel::Cpu },
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

    #[ignore = "vec2/vec3 port flattening not implemented - ports remain as 'a', not 'a_x', 'a_y'"]
    #[tokio::test]
    #[cfg_attr(
        not(target_os = "macos"),
        ignore = "GPU simulation only available on macOS"
    )]
    async fn test_vec2_component_addition_gpu() {
        let source = r#"
        use skalp::numeric::fp::*;
        use skalp::numeric::formats::fp32;

        entity Vec2Add {
            in a: vec2<fp32>
            in b: vec2<fp32>
            out x_sum: fp32
            out y_sum: fp32
        }

        impl Vec2Add {
            x_sum = a.x + b.x
            y_sum = a.y + b.y
        }
        "#;

        let mut sim = setup_simulator(source, true).await;

        // vec2 ports are flattened to individual components: a_x, a_y, b_x, b_y
        // a = [1.0, 2.0], b = [3.0, 4.0]
        // Expected: x_sum = 1.0 + 3.0 = 4.0, y_sum = 2.0 + 4.0 = 6.0
        sim.set_input("a_x", (1.0f32).to_bits() as u64).await;
        sim.set_input("a_y", (2.0f32).to_bits() as u64).await;
        sim.set_input("b_x", (3.0f32).to_bits() as u64).await;
        sim.set_input("b_y", (4.0f32).to_bits() as u64).await;
        sim.step().await;

        // Check x_sum = 4.0
        let x_bits = sim.get_output("x_sum").await.unwrap_or(0) as u32;
        let x_sum = f32::from_bits(x_bits);
        assert_eq!(x_sum, 4.0, "x component: 1.0 + 3.0 should equal 4.0");

        // Check y_sum = 6.0
        let y_bits = sim.get_output("y_sum").await.unwrap_or(0) as u32;
        let y_sum = f32::from_bits(y_bits);
        assert_eq!(y_sum, 6.0, "y component: 2.0 + 4.0 should equal 6.0");
    }

    #[ignore = "vec2/vec3 port flattening not implemented - ports remain as 'a', not 'a_x', 'a_y'"]
    #[tokio::test]
    async fn test_vec2_component_addition_cpu() {
        let source = r#"
        use skalp::numeric::fp::*;
        use skalp::numeric::formats::fp32;

        entity Vec2Add {
            in a: vec2<fp32>
            in b: vec2<fp32>
            out x_sum: fp32
            out y_sum: fp32
        }

        impl Vec2Add {
            x_sum = a.x + b.x
            y_sum = a.y + b.y
        }
        "#;

        let mut sim = setup_simulator(source, false).await;

        // vec2 ports are flattened to individual components: a_x, a_y, b_x, b_y
        // a = [1.0, 2.0], b = [3.0, 4.0]
        sim.set_input("a_x", (1.0f32).to_bits() as u64).await;
        sim.set_input("a_y", (2.0f32).to_bits() as u64).await;
        sim.set_input("b_x", (3.0f32).to_bits() as u64).await;
        sim.set_input("b_y", (4.0f32).to_bits() as u64).await;
        sim.step().await;

        // Check x_sum = 4.0
        let x_bits = sim.get_output("x_sum").await.unwrap_or(0) as u32;
        let x_sum = f32::from_bits(x_bits);
        assert_eq!(x_sum, 4.0, "CPU: x component should equal 4.0");

        // Check y_sum = 6.0
        let y_bits = sim.get_output("y_sum").await.unwrap_or(0) as u32;
        let y_sum = f32::from_bits(y_bits);
        assert_eq!(y_sum, 6.0, "CPU: y component should equal 6.0");
    }

    #[ignore = "vec2/vec3 port flattening not implemented - ports remain as 'a', not 'a_x', 'a_y'"]
    #[tokio::test]
    #[cfg_attr(
        not(target_os = "macos"),
        ignore = "GPU simulation only available on macOS"
    )]
    async fn test_vec3_component_multiply_gpu() {
        let source = r#"
        use skalp::numeric::fp::*;
        use skalp::numeric::formats::fp32;

        entity Vec3Mul {
            in a: vec3<fp32>
            in b: vec3<fp32>
            out x_prod: fp32
            out y_prod: fp32
            out z_prod: fp32
        }

        impl Vec3Mul {
            x_prod = a.x * b.x
            y_prod = a.y * b.y
            z_prod = a.z * b.z
        }
        "#;

        let mut sim = setup_simulator(source, true).await;

        // vec3 ports are flattened to individual components: a_x, a_y, a_z, b_x, b_y, b_z
        // a = [2.0, 3.0, 4.0], b = [5.0, 6.0, 7.0]
        // Expected: [10.0, 18.0, 28.0]
        sim.set_input("a_x", (2.0f32).to_bits() as u64).await;
        sim.set_input("a_y", (3.0f32).to_bits() as u64).await;
        sim.set_input("a_z", (4.0f32).to_bits() as u64).await;
        sim.set_input("b_x", (5.0f32).to_bits() as u64).await;
        sim.set_input("b_y", (6.0f32).to_bits() as u64).await;
        sim.set_input("b_z", (7.0f32).to_bits() as u64).await;
        sim.step().await;

        // Check x: 2.0 * 5.0 = 10.0
        let x_bits = sim.get_output("x_prod").await.unwrap_or(0) as u32;
        let x = f32::from_bits(x_bits);
        assert_eq!(x, 10.0, "x: 2.0 * 5.0 should equal 10.0");

        // Check y: 3.0 * 6.0 = 18.0
        let y_bits = sim.get_output("y_prod").await.unwrap_or(0) as u32;
        let y = f32::from_bits(y_bits);
        assert_eq!(y, 18.0, "y: 3.0 * 6.0 should equal 18.0");

        // Check z: 4.0 * 7.0 = 28.0
        let z_bits = sim.get_output("z_prod").await.unwrap_or(0) as u32;
        let z = f32::from_bits(z_bits);
        assert_eq!(z, 28.0, "z: 4.0 * 7.0 should equal 28.0");
    }

    #[ignore = "vec2/vec3 port flattening not implemented - ports remain as 'a', not 'a_x', 'a_y'"]
    #[tokio::test]
    async fn test_vec3_component_multiply_cpu() {
        let source = r#"
        use skalp::numeric::fp::*;
        use skalp::numeric::formats::fp32;

        entity Vec3Mul {
            in a: vec3<fp32>
            in b: vec3<fp32>
            out x_prod: fp32
            out y_prod: fp32
            out z_prod: fp32
        }

        impl Vec3Mul {
            x_prod = a.x * b.x
            y_prod = a.y * b.y
            z_prod = a.z * b.z
        }
        "#;

        let mut sim = setup_simulator(source, false).await;

        // vec3 ports are flattened to individual components: a_x, a_y, a_z, b_x, b_y, b_z
        sim.set_input("a_x", (2.0f32).to_bits() as u64).await;
        sim.set_input("a_y", (3.0f32).to_bits() as u64).await;
        sim.set_input("a_z", (4.0f32).to_bits() as u64).await;
        sim.set_input("b_x", (5.0f32).to_bits() as u64).await;
        sim.set_input("b_y", (6.0f32).to_bits() as u64).await;
        sim.set_input("b_z", (7.0f32).to_bits() as u64).await;
        sim.step().await;

        let x_bits = sim.get_output("x_prod").await.unwrap_or(0) as u32;
        let x = f32::from_bits(x_bits);
        assert_eq!(x, 10.0, "CPU: x should equal 10.0");

        let y_bits = sim.get_output("y_prod").await.unwrap_or(0) as u32;
        let y = f32::from_bits(y_bits);
        assert_eq!(y, 18.0, "CPU: y should equal 18.0");

        let z_bits = sim.get_output("z_prod").await.unwrap_or(0) as u32;
        let z = f32::from_bits(z_bits);
        assert_eq!(z, 28.0, "CPU: z should equal 28.0");
    }
}
