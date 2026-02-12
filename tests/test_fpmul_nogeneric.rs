// Skip in debug mode due to stack overflow (complex FP32 expansion)

// Test non-generic FpMul implementation
use skalp_frontend::parse_and_build_compilation_context;
use skalp_lir::gate_netlist::GateNetlist;
use skalp_lir::ncl_expand::NclConfig;
use skalp_lir::{
    apply_boundary_ncl_to_hierarchy, get_stdlib_library, lower_mir_hierarchical_for_optimize_first,
    map_hierarchical_to_gates,
};
use skalp_mir::MirCompiler;
use skalp_sim::ncl_sim::{NclSimConfig, NclSimulator};
use std::io::Write;

fn setup_stdlib_path() {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let stdlib_path = format!("{}/crates/skalp-stdlib", manifest_dir);
    std::env::set_var("SKALP_STDLIB_PATH", &stdlib_path);
}

fn fixture_path(name: &str) -> String {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    format!("{}/tests/fixtures/{}", manifest_dir, name)
}

fn compile_to_gates(fixture_name: &str) -> GateNetlist {
    setup_stdlib_path();

    let source_path = fixture_path(fixture_name);
    let context = parse_and_build_compilation_context(std::path::Path::new(&source_path)).expect("Failed to parse");
    let mir_compiler = MirCompiler::new();
    let mir = mir_compiler
        .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
        .expect("Failed to compile to MIR");

    let (hier_lir_raw, has_async) = lower_mir_hierarchical_for_optimize_first(&mir);

    let hier_lir = if has_async {
        let ncl_config = NclConfig::default();
        apply_boundary_ncl_to_hierarchy(&hier_lir_raw, &ncl_config)
    } else {
        hier_lir_raw
    };

    let library = get_stdlib_library("generic_asic").expect("Failed to load library");
    let hier_result = map_hierarchical_to_gates(&hier_lir, &library);
    hier_result.flatten()
}

#[test]
fn test_fpmul32_entity() {
    let netlist = compile_to_gates("fpmul_nogeneric.sk");
    eprintln!("Cells: {}", netlist.cells.len());

    let config = NclSimConfig {
        max_iterations: 1000,
        debug: false,
        ..Default::default()
    };

    let mut sim = NclSimulator::new(netlist, config);

    eprintln!("Available inputs: {:?}", sim.input_names());
    eprintln!("Available outputs: {:?}", sim.output_names());

    // Test 2.0 * 3.0 = 6.0
    let a = 0x40000000u64; // 2.0
    let b = 0x40400000u64; // 3.0
    let expected = 0x40C00000u64; // 6.0

    eprintln!("\nTest: 2.0 * 3.0 = 6.0");
    eprintln!("Input a = 0x{:08X}", a);
    eprintln!("Input b = 0x{:08X}", b);

    sim.set_dual_rail_value("top.a", a, 32);
    sim.set_dual_rail_value("top.b", b, 32);

    let iters = sim.run_until_stable(1000);
    eprintln!("Iterations: {}", iters);

    if let Some(result) = sim.get_dual_rail_value("top.result", 32) {
        let sign = (result >> 31) & 1;
        let exp = (result >> 23) & 0xFF;
        let mant = result & 0x7FFFFF;
        eprintln!(
            "Result = 0x{:08X} (sign={}, exp={}, mant=0x{:06X})",
            result, sign, exp, mant
        );

        if result == expected {
            eprintln!("PASS ✓");
        } else {
            eprintln!("FAIL: expected 0x{:08X}", expected);
        }
        assert_eq!(result, expected, "FpMul32 entity should give 6.0");
    } else {
        panic!("Simulation did not converge!");
    }
}
