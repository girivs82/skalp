#[cfg(test)]
#[test]
fn test_bug71_metal_288bit_tuple_generation() {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};
    use skalp_sir::convert_mir_to_sir;

    println!("\n🧪 Testing Bug #71: Metal shader generation for 288-bit tuples\n");

    let source = r#"
// AUDIT-2 #5: rewritten without stdlib types — the original used
// vec3<fp32> but parse_and_build_hir has no stdlib context, so the
// struct literals never resolved and the let bindings vanished
// ("undefined identifier v1"). Plain 96-bit lanes preserve the
// 288-bit-tuple intent the test exists for.
fn make_vecs() -> (bit[96], bit[96], bit[96]) {
    let v1: bit[96] = 1;
    let v2: bit[96] = 2;
    let v3: bit[96] = 3;
    return (v1, v2, v3);
}

entity Test {
    port clk: clock;
    port out: bit[32];
}

impl Test {
    signal result: bit[32];

    on(clk.rise) {
        let triple = make_vecs();
        let v1 = triple.0;
        result = v1[31:0];
    }

    out = result;
}
"#;

    println!("📝 Parsing source...");
    let hir = parse_and_build_hir(source).expect("Parse failed");

    println!("🔧 Compiling to MIR...");
    let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
    let mir = compiler.compile_to_mir(&hir).expect("MIR failed");

    println!("🔄 Converting to SIR...");
    let sir = convert_mir_to_sir(&mir.modules[0]);

    println!("🎨 Generating Metal shader...");
    println!("   (This would panic with 'Unsupported bit width 288' before the fix)");

    // This is the critical test - generate Metal shader
    // Before the fix, this panics with: "Unsupported bit width 288 for Metal codegen"
    let shader = skalp_sir::MetalBackend::generate(&sir);

    assert!(!shader.is_empty(), "Metal shader should not be empty");
    assert!(
        shader.contains("#include <metal_stdlib>"),
        "Should be a Metal shader"
    );

    // Check if decomposition happened
    if shader.contains("_part0") {
        println!("✅ Found decomposed signals in Metal shader");
        println!("   Wide signals were split into 256-bit parts");
    }

    println!("✅ Metal shader generated successfully!");
    println!("   Generated {} bytes of shader code", shader.len());

    // Print shader for inspection
    println!("\n=== GENERATED METAL SHADER ===");
    println!("{}", shader);
    println!("=== END ===\n");
}
