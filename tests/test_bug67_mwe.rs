// Test for Bug #67 MWE: Does it reproduce the FP16 type error?
// NOTE: This test requires manual setup of /tmp/bug67_mwe_entity.sk

#[cfg(all(test, target_os = "macos"))]
#[tokio::test]
#[ignore = "requires manual setup of /tmp/bug67_mwe_entity.sk"]
async fn test_bug67_mwe_metal() {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};
    use skalp_sim::{HwAccel, SimLevel, UnifiedSimConfig, UnifiedSimulator};
    use skalp_sir::convert_mir_to_sir;
    use std::fs;

    println!("\n Testing Bug #67 MWE - Does it reproduce the half vs uint error?");

    // Read the MWE (entity-based version)
    let source =
        fs::read_to_string("/tmp/bug67_mwe_entity.sk").expect("Failed to read Bug #67 MWE");

    // Parse and build HIR
    let hir = parse_and_build_hir(&source).expect("Failed to parse Bug #67 MWE");

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

    // Try to create GPU simulator
    let mut simulator = UnifiedSimulator::new(config)
        .expect("Failed to create GPU simulator");

    println!("GPU simulator created");

    // Load the module - this is where Metal compilation happens
    println!("Loading SIR module - this will compile Metal shader...");
    let load_result = simulator.load_behavioral(&sir).await;

    if let Err(e) = load_result {
        let error_msg = format!("{}", e);
        println!("Metal shader compilation failed (Bug #67 MWE):");
        println!("   {}", error_msg);

        // Check if this is the expected FP16 type mismatch error
        if error_msg.contains("as_type")
            && (error_msg.contains("half") || error_msg.contains("ushort"))
            && error_msg.contains("uint")
        {
            println!("\n Bug #67 REPRODUCED by MWE!");
            println!("   Expected: Variables should be uint (32-bit)");
            println!("   Actual: Variables are half/ushort (16-bit)");
            println!("\n   This MWE successfully reproduces Bug #67!");
            panic!("Bug #67 MWE: Reproduced FP16 type inference error in Metal shader generation");
        } else {
            panic!(
                "Unexpected error during Metal shader compilation: {}",
                error_msg
            );
        }
    }

    println!("Metal shader compiled successfully");
    println!("   Bug #67 appears to be FIXED if this test passes!");
}
