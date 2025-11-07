// Test for Bug #67: Testing if nested if-else calling exec_l3 triggers the bug

#[cfg(all(test, target_os = "macos"))]
#[tokio::test]
async fn test_bug67_nested_metal() {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};
    use skalp_sim::{SimulationConfig, Simulator};
    use skalp_sir::convert_mir_to_sir;
    use std::fs;

    println!("\n🧪 Testing Bug #67 with nested if-else - Does calling exec_l3 from if-else trigger the bug?");

    // Read the nested if-else MWE
    let source =
        fs::read_to_string("/tmp/bug67_mwe_nested.sk").expect("Failed to read Bug #67 nested MWE");

    // Parse and build HIR
    let hir = parse_and_build_hir(&source).expect("Failed to parse Bug #67 nested MWE");

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

    // Create GPU simulation config
    let config = SimulationConfig {
        use_gpu: true,
        max_cycles: 10,
        timeout_ms: 5000,
        capture_waveforms: false,
        parallel_threads: 1,
    };

    println!("🔍 Creating GPU simulator...");

    let mut simulator = Simulator::new(config)
        .await
        .expect("Failed to create GPU simulator");

    println!("✅ GPU simulator created");
    println!("🔍 Loading SIR module - compiling Metal shader...");

    let load_result = simulator.load_module(&sir).await;

    if let Err(e) = load_result {
        let error_msg = format!("{}", e);
        println!("❌ Metal shader compilation failed:");
        println!("   {}", error_msg);

        // Check if this is the FP16 type mismatch error
        if error_msg.contains("as_type")
            && (error_msg.contains("half") || error_msg.contains("ushort"))
            && error_msg.contains("uint")
        {
            println!("\n🐛 Bug #67 REPRODUCED with nested if-else!");
            println!("   The bug IS triggered by calling exec_l3 from within if-else chain!");
            println!("   This matches the Karythra pattern exactly!");
            panic!("Bug #67: FP16 type error reproduced with nested if-else pattern");
        } else {
            panic!("Unexpected error: {}", error_msg);
        }
    }

    println!("✅ Metal shader compiled successfully");
    println!("   Nested if-else pattern does NOT trigger the bug");
}
