// Minimal FP32 mul debug test
// Skip in debug mode due to stack overflow (complex FP32 expansion)
#![cfg(not(debug_assertions))]

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

fn compile_to_gates(source: &str) -> GateNetlist {
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        "/Users/girivs/src/hw/hls/crates/skalp-stdlib",
    );

    let thread_id = std::thread::current().id();
    let temp_path_str = format!("/tmp/test_fpmul_debug_{:?}.sk", thread_id);
    let temp_path = std::path::Path::new(&temp_path_str);
    let mut file = std::fs::File::create(temp_path).expect("Failed to create temp file");
    file.write_all(source.as_bytes())
        .expect("Failed to write temp file");
    drop(file);

    let context = parse_and_build_compilation_context(temp_path).expect("Failed to parse");
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
fn test_fpmul_2x3() {
    let source = r#"
        use skalp::numeric::fp::*;

        async entity TestMul {
            in a: bit[32]
            in b: bit[32]
            out result: bit[32]
        }
        impl TestMul {
            signal a_fp: fp32 = a as fp32
            signal b_fp: fp32 = b as fp32
            signal prod: fp32 = a_fp * b_fp
            result = prod as bit[32]
        }
    "#;

    let netlist = compile_to_gates(source);
    eprintln!("Cells: {}", netlist.cells.len());

    let config = NclSimConfig {
        max_iterations: 1000,
        debug: false,
        ..Default::default()
    };

    let mut sim = NclSimulator::new(netlist, config);

    // Print available inputs/outputs
    eprintln!("Available inputs: {:?}", sim.input_names());
    eprintln!("Available outputs: {:?}", sim.output_names());

    // Test 2.0 * 3.0 = 6.0
    let a = 0x40000000u64; // 2.0
    let b = 0x40400000u64; // 3.0
    let expected = 0x40C00000u64; // 6.0

    eprintln!("Input a = 0x{:08X} (2.0)", a);
    eprintln!("Input b = 0x{:08X} (3.0)", b);
    eprintln!("Expected = 0x{:08X} (6.0)", expected);

    sim.set_dual_rail_value("top.a", a, 32);
    sim.set_dual_rail_value("top.b", b, 32);

    let iters = sim.run_until_stable(1000);
    eprintln!("Iterations: {}", iters);

    if let Some(result) = sim.get_dual_rail_value("top.result", 32) {
        eprintln!("Result = 0x{:08X}", result);

        // Decode result
        let sign = (result >> 31) & 1;
        let exp = ((result >> 23) & 0xFF) as i32;
        let mant = result & 0x7FFFFF;

        eprintln!("  sign={}, exp={} (biased), mant=0x{:06X}", sign, exp, mant);

        if exp == 0 {
            eprintln!("  value = 0 or denorm");
        } else if exp == 255 {
            if mant == 0 {
                eprintln!("  value = {}inf", if sign == 1 { "-" } else { "" });
            } else {
                eprintln!("  value = NaN");
            }
        } else {
            let fval = (1.0 + mant as f64 / 8388608.0) * 2f64.powi(exp - 127);
            let fval = if sign == 1 { -fval } else { fval };
            eprintln!("  value = {}", fval);
        }

        if result == expected {
            eprintln!("PASS: 2.0 * 3.0 = 6.0");
        } else {
            eprintln!("FAIL: expected 0x{:08X}, got 0x{:08X}", expected, result);
        }
        assert_eq!(result, expected, "2.0 * 3.0 should equal 6.0");
    } else {
        panic!("Simulation did not converge");
    }
}
