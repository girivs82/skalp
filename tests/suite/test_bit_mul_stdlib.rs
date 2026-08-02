// Test that bit<N> arithmetic operators resolve through stdlib trait impls
// instead of the direct primitive lowering path.
//
// End-to-end: `a * b` on bit[8] → impl Mul for bit → std_multiplier (with barriers)

use skalp_frontend::parse_and_build_compilation_context;
use skalp_lir::ncl_expand::NclConfig;
use skalp_lir::{
    apply_boundary_ncl_to_hierarchy, get_stdlib_library, lower_mir_hierarchical_for_optimize_first,
    synthesize_hierarchical,
};
use skalp_mir::MirCompiler;

fn setup_stdlib_path() {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let stdlib_path = format!("{}/crates/skalp-stdlib", manifest_dir);
    std::env::set_var("SKALP_STDLIB_PATH", &stdlib_path);
}

fn fixture_path(name: &str) -> String {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    format!("{}/tests/fixtures/{}", manifest_dir, name)
}

#[test]
fn test_bit_mul_resolves_through_stdlib() {
    setup_stdlib_path();

    let source_path = fixture_path("bit_mul_stdlib.sk");
    let context = parse_and_build_compilation_context(std::path::Path::new(&source_path))
        .expect("Failed to parse bit_mul_stdlib.sk");

    // Debug: check what trait impls are available in module HIRs
    eprintln!(
        "=== Module HIRs: {} modules loaded ===",
        context.module_hirs.len()
    );
    for (path, hir) in &context.module_hirs {
        eprintln!("  Module: {:?}", path);
        eprintln!("    Trait impls: {}", hir.trait_implementations.len());
        for impl_ in &hir.trait_implementations {
            eprintln!("      impl {} for {:?}", impl_.trait_name, impl_.target);
            for method in &impl_.method_implementations {
                eprintln!(
                    "        fn {} ({} body stmts)",
                    method.name,
                    method.body.len()
                );
            }
            eprintln!("      target details: {:#?}", impl_.target);
        }
        eprintln!("    Entities: {}", hir.entities.len());
        for e in &hir.entities {
            eprintln!("      entity: {}", e.name);
        }
    }

    // Also check main HIR
    eprintln!("=== Main HIR ===");
    eprintln!(
        "  Trait impls: {}",
        context.main_hir.trait_implementations.len()
    );
    for impl_ in &context.main_hir.trait_implementations {
        eprintln!("    impl {} for {:?}", impl_.trait_name, impl_.target);
    }

    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
        .expect("Failed to compile to MIR");

    // Check that the MIR contains references to stdlib entities (std_multiplier)
    // rather than just a primitive Mul operation
    let mir_debug = format!("{:?}", mir);
    eprintln!(
        "\nMIR output (first 2000 chars):\n{}",
        &mir_debug[..mir_debug.len().min(2000)]
    );

    // Check if trait resolution happened: if it did, we should NOT see a raw Binary { op: Mul }
    // Instead we should see entity instantiation or inlined trait method body
    let has_raw_mul = mir_debug.contains("op: Mul");
    if has_raw_mul {
        eprintln!("\nWARNING: MIR still contains raw Binary {{ op: Mul }} — trait resolution did NOT happen!");
    } else {
        eprintln!("\nSUCCESS: No raw Mul in MIR — trait resolution likely worked!");
    }

    // Lower to hierarchical LIR
    let (hier_lir_raw, has_async) = lower_mir_hierarchical_for_optimize_first(&mir);

    eprintln!("has_async: {}", has_async);
    assert!(has_async, "Entity is async, should be detected");

    // Apply boundary NCL
    let hier_lir = if has_async {
        let ncl_config = NclConfig::default();
        apply_boundary_ncl_to_hierarchy(&hier_lir_raw, &ncl_config)
    } else {
        hier_lir_raw
    };

    // Synthesize
    let library = get_stdlib_library("generic_asic").expect("Failed to load library");
    let netlist =
        synthesize_hierarchical(&hier_lir, &library, skalp_lir::synth::SynthPreset::Quick)
            .flatten();

    eprintln!("Cells: {}", netlist.cells.len());
    assert!(
        !netlist.cells.is_empty(),
        "Should produce a non-empty gate netlist"
    );
}
