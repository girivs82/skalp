//! Minimal test for 64-bit register Metal codegen (BUG #117 verification)
//!
//! Verifies that 64-bit registers use uint2 (not uint4) in Metal shaders.

#![cfg(all(test, target_os = "macos"))]

use skalp_frontend::parse_and_build_hir;
use skalp_mir::{MirCompiler, OptimizationLevel};
use skalp_sim::{HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
use skalp_sir::convert_mir_to_sir;
use std::fs;

const TEST_SOURCE: &str = r#"
// Minimal test case for 64-bit register Metal codegen
entity Test64BitReg {
    in clk: clock
    in rst: reset
    in data_in: bit[64]
    in enable: bit[1]
    out data_out: bit[64]
}

impl Test64BitReg {
    signal reg64: bit[64] = 0

    on(clk.rise) {
        if (rst) {
            reg64 = 0
        } else if (enable) {
            reg64 = data_in
        }
    }

    data_out = reg64
}
"#;

#[tokio::test]
async fn test_64bit_register_metal_codegen() {
    // Parse and build HIR
    let hir = parse_and_build_hir(TEST_SOURCE).expect("Failed to parse test design");

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
        max_cycles: 10,
        capture_waveforms: true,
        ..Default::default()
    };

    // Create simulator - this generates the Metal shader
    let mut simulator = UnifiedSimulator::new(config)
        .expect("Failed to create GPU simulator");

    // Load the module - this generates the Metal shader
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

    // Resolve names
    let rst_name = resolve("rst");
    let clk_name = resolve("clk");
    let data_in_name = resolve("data_in");
    let enable_name = resolve("enable");

    // Set initial inputs (using resolved internal names)
    simulator.set_input(&rst_name, 1).await;
    simulator.set_input(&clk_name, 0).await;
    simulator.set_input(&data_in_name, 0).await;
    simulator.set_input(&enable_name, 0).await;

    // Run a few cycles
    for _ in 0..5 {
        simulator.step().await;
    }

    // Read and verify the Metal shader
    let shader_content =
        fs::read_to_string("/tmp/skalp_shader.metal").expect("Metal shader should exist");

    // Resolve the internal name for reg64
    let reg64_internal = resolve("reg64");

    // Check that 64-bit register uses uint2, not uint4
    // The register struct should contain "uint2 <internal_name>;" for a 64-bit register
    println!("Generated Metal shader snippet (reg64 -> {}):", reg64_internal);
    for line in shader_content.lines() {
        if line.contains("struct Registers") || line.contains(&reg64_internal) {
            println!("  {}", line);
        }
    }

    // Verify the register type - check for uint2 with the internal name
    let expected_pattern = format!("uint2 {};", reg64_internal);
    assert!(
        shader_content.contains(&expected_pattern),
        "64-bit register should use uint2 type, but shader contains:\n{}",
        shader_content
            .lines()
            .filter(|l| l.contains(&reg64_internal))
            .collect::<Vec<_>>()
            .join("\n")
    );

    println!("BUG #117 fix verified: 64-bit register correctly uses uint2 type");
}
