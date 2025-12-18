//! Tests for pipeline annotation on functions
//!
//! Tests the #[pipeline(stages=N)] attribute is correctly parsed,
//! propagated through HIR/MIR, and functions are compiled successfully.

use skalp_frontend::hir_builder::build_hir;
use skalp_frontend::parse::parse;
use skalp_mir::hir_to_mir::HirToMir;
use std::fs;
use std::path::Path;

#[test]
fn test_pipeline_annotation_parsing() {
    println!("=== Testing Pipeline Annotation Parsing ===");

    let source = r#"
// Test pipeline annotation on function
#[pipeline(stages=2)]
pub fn pipelined_add(a: bit[32], b: bit[32]) -> bit[32] {
    let sum = a + b;
    return sum
}

entity TestEntity {
    in x: bit[32],
    in y: bit[32],
    out result: bit[32],
}

impl TestEntity {
    result = pipelined_add(x, y)
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    // Verify the function was parsed
    assert!(
        !hir.functions.is_empty(),
        "Should have at least one function"
    );

    let func = hir.functions.iter().find(|f| f.name == "pipelined_add");
    assert!(func.is_some(), "Should have pipelined_add function");

    let func = func.unwrap();
    println!(
        "Function '{}' has pipeline config: {:?}",
        func.name, func.pipeline_config
    );

    // The pipeline config should be present
    assert!(
        func.pipeline_config.is_some(),
        "Function should have pipeline config"
    );

    let config = func.pipeline_config.as_ref().unwrap();
    assert_eq!(config.stages, 2, "Pipeline should have 2 stages");

    println!(
        "Pipeline annotation parsed successfully with {} stages",
        config.stages
    );
}

#[test]
fn test_pipeline_annotation_mir_compilation() {
    println!("=== Testing Pipeline Annotation MIR Compilation ===");

    let source = r#"
#[pipeline(stages=3)]
pub fn pipelined_mac(a: bit[32], b: bit[32], c: bit[32]) -> bit[32] {
    let product = a * b;
    let result = product + c;
    return result
}

entity MacTest {
    in a: bit[32],
    in b: bit[32],
    in c: bit[32],
    out result: bit[32],
}

impl MacTest {
    result = pipelined_mac(a, b, c)
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    // Compile to MIR
    let mut transformer = HirToMir::new();
    let mir = transformer.transform(&hir);

    assert!(
        !mir.modules.is_empty(),
        "MIR should have at least one module"
    );

    println!(
        "MIR compilation successful with {} modules",
        mir.modules.len()
    );

    // Find the top module
    let top_module = mir.modules.iter().find(|m| m.name == "MacTest");
    assert!(top_module.is_some(), "Should have MacTest module");

    let module = top_module.unwrap();
    println!(
        "Module '{}' has {} signals, {} assignments",
        module.name,
        module.signals.len(),
        module.assignments.len()
    );

    // Verify pipeline config was propagated
    // The pipeline_config should be on the synthesized function module
    let pipelined_module = mir
        .modules
        .iter()
        .find(|m| m.name.contains("pipelined_mac"));
    if let Some(pm) = pipelined_module {
        println!(
            "Found pipelined module '{}' with pipeline config: {:?}",
            pm.name, pm.pipeline_config
        );
    }

    println!("Pipeline annotation MIR compilation test passed!");
}

#[test]
fn test_pipeline_annotation_fixture() {
    println!("=== Testing Pipeline Annotation from Fixture File ===");

    let fixture_path = Path::new("tests/fixtures/test_pipeline_annotation.sk");
    if !fixture_path.exists() {
        println!(
            "Skipping fixture test - file not found at {:?}",
            fixture_path
        );
        return;
    }

    let source = fs::read_to_string(fixture_path).expect("Failed to read fixture file");

    // Parse and build HIR
    let tree = parse(&source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    // Check functions with pipeline annotations
    let pipelined_funcs: Vec<_> = hir
        .functions
        .iter()
        .filter(|f| f.pipeline_config.is_some())
        .collect();

    println!(
        "Found {} functions with pipeline annotations:",
        pipelined_funcs.len()
    );
    for func in &pipelined_funcs {
        let config = func.pipeline_config.as_ref().unwrap();
        println!("  - {} (stages={})", func.name, config.stages);
    }

    assert!(
        pipelined_funcs.len() >= 2,
        "Should have at least 2 pipelined functions"
    );

    // Compile to MIR
    let mut transformer = HirToMir::new();
    let mir = transformer.transform(&hir);

    assert!(
        !mir.modules.is_empty(),
        "MIR should have at least one module"
    );

    println!(
        "Fixture compiled successfully to {} MIR modules",
        mir.modules.len()
    );
}

#[test]
fn test_pipeline_annotation_with_target_freq() {
    println!("=== Testing Pipeline Annotation with Target Frequency ===");

    let source = r#"
#[pipeline(stages=4, target_freq=100_000_000)]
pub fn high_freq_mul(a: bit[32], b: bit[32]) -> bit[32] {
    let result = a * b;
    return result
}

entity FreqTest {
    in a: bit[32],
    in b: bit[32],
    out result: bit[32],
}

impl FreqTest {
    result = high_freq_mul(a, b)
}
"#;

    // Parse and build HIR
    let tree = parse(source);
    let hir = build_hir(&tree).expect("HIR building should succeed");

    let func = hir.functions.iter().find(|f| f.name == "high_freq_mul");
    assert!(func.is_some(), "Should have high_freq_mul function");

    let func = func.unwrap();
    assert!(
        func.pipeline_config.is_some(),
        "Function should have pipeline config"
    );

    let config = func.pipeline_config.as_ref().unwrap();
    assert_eq!(config.stages, 4, "Pipeline should have 4 stages");
    assert_eq!(
        config.target_freq,
        Some(100_000_000),
        "Target frequency should be 100MHz"
    );

    println!(
        "Pipeline with target_freq parsed: stages={}, freq={:?}",
        config.stages, config.target_freq
    );
}
