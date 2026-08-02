//! Test for 64-bit register Metal codegen (BUG #117 verification)
//!
//! Verifies that 64-bit registers use uint2 (not uint) in Metal shaders,
//! using the shared codegen path (MetalBackend::generate).

#![cfg(test)]

use skalp_frontend::parse_and_build_hir;
use skalp_mir::{MirCompiler, OptimizationLevel};
use skalp_sir::{convert_mir_to_sir, MetalBackend};

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

#[test]
fn test_64bit_register_metal_codegen() {
    // Parse and build HIR
    let hir = parse_and_build_hir(TEST_SOURCE).expect("Failed to parse test design");

    // Compile to MIR with optimizations
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

    // Generate Metal shader directly via shared codegen
    let shader_content = MetalBackend::generate(&sir);

    // Resolve the internal name for reg64
    let reg64_internal = sir
        .name_registry
        .resolve("reg64")
        .map(|s| s.to_string())
        .unwrap_or_else(|| "reg64".to_string());

    // Debug: show relevant lines
    println!(
        "Generated Metal shader snippet (reg64 -> {}):",
        reg64_internal
    );
    for line in shader_content.lines() {
        if line.contains("struct Registers")
            || line.contains("struct Inputs")
            || line.contains(&reg64_internal)
        {
            println!("  {}", line.trim());
        }
    }

    // Verify the register type - 64-bit register should use uint2 in Metal
    // Check both the Registers struct (state element) and Inputs struct (if present)
    let has_uint2 = shader_content
        .lines()
        .any(|l| l.contains("uint2") && l.contains(&reg64_internal));

    assert!(
        has_uint2,
        "64-bit register should use uint2 type in Metal shader, but relevant lines are:\n{}",
        shader_content
            .lines()
            .filter(|l| l.contains(&reg64_internal))
            .collect::<Vec<_>>()
            .join("\n")
    );

    println!("BUG #117 fix verified: 64-bit register correctly uses uint2 type");
}
