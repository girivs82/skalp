//! Test to verify NCL expansion handlers for RedOr, RedAnd, RedXor, Slt, Sge, Sgt, Sle
//! are correctly implemented (no undriven signals)

use skalp_frontend::parse_and_build_compilation_context;
use skalp_lir::lower_mir_hierarchical;
use skalp_mir::MirCompiler;
use std::path::Path;

#[test]
fn test_fp32_ncl_no_undriven_signals() {
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        "/Users/girivs/src/hw/hls/crates/skalp-stdlib",
    );

    let source_path =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/test_fpadd_sim.sk");
    let context = parse_and_build_compilation_context(&source_path).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
        .expect("Failed to compile");
    let hier_lir = lower_mir_hierarchical(&mir);

    // Find the FpAdd instance dynamically (name suffix depends on compiler-assigned IDs)
    let adder_entry = hier_lir
        .instances
        .iter()
        .find(|(path, inst)| path.contains("adder") || inst.module_name.contains("FpAdd"));

    let (adder_path, adder_inst) = adder_entry.expect("FP32 adder instance not found in hierarchy");

    // Verify all signals have drivers (no undriven signals)
    let mut undriven_used = 0;
    for signal in &adder_inst.lir_result.lir.signals {
        let has_driver = adder_inst
            .lir_result
            .lir
            .nodes
            .iter()
            .any(|n| n.output == signal.id);
        if !has_driver && !signal.is_input {
            let readers: Vec<_> = adder_inst
                .lir_result
                .lir
                .nodes
                .iter()
                .filter(|n| n.inputs.contains(&signal.id))
                .collect();
            if !readers.is_empty() {
                undriven_used += 1;
                println!(
                    "PROBLEM: Signal {} '{}' has no driver but {} readers",
                    signal.id.0,
                    signal.name,
                    readers.len()
                );
            }
        }
    }

    println!("=== NCL Expansion Fix Verification ===");
    println!("FP32 adder instance: {}", adder_path);
    println!("FP32 adder module: {}", adder_inst.module_name);
    println!("Signals: {}", adder_inst.lir_result.lir.signals.len());
    println!("Nodes: {}", adder_inst.lir_result.lir.nodes.len());
    println!("Undriven signals used by nodes: {}", undriven_used);

    assert_eq!(
        undriven_used, 0,
        "Found {} undriven signals that are used",
        undriven_used
    );
    println!("✅ All signals have proper drivers");
}

#[test]
fn test_passthrough_ncl_works() {
    use skalp_frontend::parse_and_build_hir;
    use skalp_lir::lower_mir_module_to_lir;

    let source = r#"
        async entity Passthrough {
            in a: bit[8]
            out y: bit[8]
        }
        impl Passthrough {
            y = a
        }
    "#;

    let hir = parse_and_build_hir(source).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler.compile(&hir).expect("Failed to compile");

    for module in &mir.modules {
        let lir_result = lower_mir_module_to_lir(module);

        // Check for undriven signals
        let mut undriven = 0;
        for signal in &lir_result.lir.signals {
            let has_driver = lir_result.lir.nodes.iter().any(|n| n.output == signal.id);
            let has_reader = lir_result
                .lir
                .nodes
                .iter()
                .any(|n| n.inputs.contains(&signal.id));
            if !has_driver && !signal.is_input && has_reader {
                undriven += 1;
                println!("Undriven: {} '{}'", signal.id.0, signal.name);
            }
        }

        println!(
            "Passthrough: {} signals, {} nodes, {} undriven",
            lir_result.lir.signals.len(),
            lir_result.lir.nodes.len(),
            undriven
        );
        assert_eq!(undriven, 0, "Found {} undriven signals", undriven);
    }
    println!("✅ Passthrough NCL works correctly");
}

#[test]
fn test_integer_add_ncl_works() {
    use skalp_frontend::parse_and_build_hir;
    use skalp_lir::lower_mir_module_to_lir;

    let source = r#"
        async entity IntAdd {
            in a: bit[32]
            in b: bit[32]
            out y: bit[32]
        }
        impl IntAdd {
            y = a + b
        }
    "#;

    let hir = parse_and_build_hir(source).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler.compile(&hir).expect("Failed to compile");

    for module in &mir.modules {
        let lir_result = lower_mir_module_to_lir(module);

        // Check for undriven signals
        let mut undriven = 0;
        for signal in &lir_result.lir.signals {
            let has_driver = lir_result.lir.nodes.iter().any(|n| n.output == signal.id);
            let has_reader = lir_result
                .lir
                .nodes
                .iter()
                .any(|n| n.inputs.contains(&signal.id));
            if !has_driver && !signal.is_input && has_reader {
                undriven += 1;
                println!("Undriven: {} '{}'", signal.id.0, signal.name);
            }
        }

        println!(
            "IntAdd: {} signals, {} nodes, {} undriven",
            lir_result.lir.signals.len(),
            lir_result.lir.nodes.len(),
            undriven
        );
        assert_eq!(undriven, 0, "Found {} undriven signals", undriven);
    }
    println!("✅ Integer add NCL works correctly");
}
