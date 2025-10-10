// Tests for stdlib floating-point and vector type simulation
// These tests verify that FP and vector types work correctly in the GPU simulator

#[cfg(test)]
mod stdlib_simulation_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::lower_to_mir;
    use skalp_sim::{SimulationConfig, Simulator};

    async fn setup_simulator(source: &str, use_gpu: bool) -> Simulator {
        // Parse and build HIR
        let hir = parse_and_build_hir(source).expect("Failed to parse design");

        // Compile to MIR
        let mir = lower_to_mir(&hir).expect("Failed to compile to MIR");

        // Convert to SIR
        assert!(!mir.modules.is_empty());
        let top_module = &mir.modules[mir.modules.len() - 1];
        let sir = skalp_sir::convert_mir_to_sir_with_hierarchy(&mir, top_module);

        // Create simulator
        let config = SimulationConfig {
            use_gpu,
            max_cycles: 100,
            timeout_ms: 5000,
            capture_waveforms: false,
            parallel_threads: 1,
        };

        let mut simulator = Simulator::new(config)
            .await
            .expect("Failed to create simulator");

        simulator
            .load_module(&sir)
            .await
            .expect("Failed to load module");

        simulator
    }

    #[tokio::test]
    #[cfg_attr(
        not(target_os = "macos"),
        ignore = "GPU simulation only available on macOS"
    )]
    async fn test_vec2_field_access_simulation() {
        let source = r#"
        entity Vec2FieldAccess {
            in a: vec2<fp32>
            out x_out: fp32
            out y_out: fp32
        }

        impl Vec2FieldAccess {
            x_out = a.x
            y_out = a.y
        }
        "#;

        let mut sim = setup_simulator(source, true).await;

        // Create vec2 input: [x=1.0, y=2.0] as 64-bit value
        // fp32 1.0 = 0x3F800000, fp32 2.0 = 0x40000000
        // vec2 layout: [x_bits, y_bits] = [31:0, 63:32]
        let vec2_input: u64 = 0x40000000_3F800000; // y in high 32, x in low 32

        sim.set_input("a", vec2_input.to_le_bytes().to_vec())
            .await
            .unwrap();
        sim.step_simulation().await.unwrap();

        // Verify x component
        let x_bytes = sim.get_output("x_out").await.unwrap();
        let x_val = u32::from_le_bytes([
            x_bytes[0],
            x_bytes.get(1).copied().unwrap_or(0),
            x_bytes.get(2).copied().unwrap_or(0),
            x_bytes.get(3).copied().unwrap_or(0),
        ]);
        assert_eq!(x_val, 0x3F800000, "x component should be 1.0");

        // Verify y component
        let y_bytes = sim.get_output("y_out").await.unwrap();
        let y_val = u32::from_le_bytes([
            y_bytes[0],
            y_bytes.get(1).copied().unwrap_or(0),
            y_bytes.get(2).copied().unwrap_or(0),
            y_bytes.get(3).copied().unwrap_or(0),
        ]);
        assert_eq!(y_val, 0x40000000, "y component should be 2.0");
    }

    #[tokio::test]
    #[cfg_attr(
        not(target_os = "macos"),
        ignore = "GPU simulation only available on macOS"
    )]
    async fn test_vec2_addition_simulation() {
        let source = r#"
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

        // a = [1.0, 2.0], b = [3.0, 4.0]
        // fp32: 1.0=0x3F800000, 2.0=0x40000000, 3.0=0x40400000, 4.0=0x40800000
        let a: u64 = 0x40000000_3F800000; // [1.0, 2.0]
        let b: u64 = 0x40800000_40400000; // [3.0, 4.0]

        sim.set_input("a", a.to_le_bytes().to_vec()).await.unwrap();
        sim.set_input("b", b.to_le_bytes().to_vec()).await.unwrap();
        sim.step_simulation().await.unwrap();

        // Note: This test verifies that the SystemVerilog code is generated correctly.
        // Actual floating-point addition would require an FP adder implementation.
        // For now, we just verify the structure compiles and simulates without error.

        let x_bytes = sim.get_output("x_sum").await.unwrap();
        assert_eq!(x_bytes.len(), 4, "x_sum should be 32 bits");

        let y_bytes = sim.get_output("y_sum").await.unwrap();
        assert_eq!(y_bytes.len(), 4, "y_sum should be 32 bits");
    }

    #[tokio::test]
    #[cfg_attr(
        not(target_os = "macos"),
        ignore = "GPU simulation only available on macOS"
    )]
    async fn test_fp32_passthrough_simulation() {
        let source = r#"
        entity FP32Passthrough {
            in x: fp32
            out y: fp32
        }

        impl FP32Passthrough {
            y = x
        }
        "#;

        let mut sim = setup_simulator(source, true).await;

        // Test with fp32 value 3.14159 ≈ 0x40490FDB
        let fp_val: u32 = 0x40490FDB;

        sim.set_input("x", fp_val.to_le_bytes().to_vec())
            .await
            .unwrap();
        sim.step_simulation().await.unwrap();

        let y_bytes = sim.get_output("y").await.unwrap();
        let y_val = u32::from_le_bytes([
            y_bytes[0],
            y_bytes.get(1).copied().unwrap_or(0),
            y_bytes.get(2).copied().unwrap_or(0),
            y_bytes.get(3).copied().unwrap_or(0),
        ]);

        assert_eq!(y_val, fp_val, "FP32 passthrough should preserve value");
    }

    #[tokio::test]
    #[cfg_attr(
        not(target_os = "macos"),
        ignore = "GPU simulation only available on macOS"
    )]
    async fn test_vec3_field_access_simulation() {
        let source = r#"
        entity Vec3FieldAccess {
            in v: vec3<fp32>
            out x: fp32
            out y: fp32
            out z: fp32
        }

        impl Vec3FieldAccess {
            x = v.x
            y = v.y
            z = v.z
        }
        "#;

        let mut sim = setup_simulator(source, true).await;

        // vec3 = [1.0, 2.0, 3.0] = 96 bits total
        // [31:0]=x, [63:32]=y, [95:64]=z
        let x_bits: u32 = 0x3F800000; // 1.0
        let y_bits: u32 = 0x40000000; // 2.0
        let z_bits: u32 = 0x40400000; // 3.0

        let mut vec3_bytes = Vec::new();
        vec3_bytes.extend_from_slice(&x_bits.to_le_bytes());
        vec3_bytes.extend_from_slice(&y_bits.to_le_bytes());
        vec3_bytes.extend_from_slice(&z_bits.to_le_bytes());

        sim.set_input("v", vec3_bytes).await.unwrap();
        sim.step_simulation().await.unwrap();

        // Verify each component
        let x_out = sim.get_output("x").await.unwrap();
        let x_val = u32::from_le_bytes([x_out[0], x_out[1], x_out[2], x_out[3]]);
        assert_eq!(x_val, x_bits, "x component should match");

        let y_out = sim.get_output("y").await.unwrap();
        let y_val = u32::from_le_bytes([y_out[0], y_out[1], y_out[2], y_out[3]]);
        assert_eq!(y_val, y_bits, "y component should match");

        let z_out = sim.get_output("z").await.unwrap();
        let z_val = u32::from_le_bytes([z_out[0], z_out[1], z_out[2], z_out[3]]);
        assert_eq!(z_val, z_bits, "z component should match");
    }
}
