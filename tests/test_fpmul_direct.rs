// Skip in debug mode due to stack overflow (complex FP32 expansion)

// Test FpMul directly by instantiating it
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
    let temp_path_str = format!("/tmp/test_fpmul_direct_{:?}.sk", thread_id);
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
fn test_fpmul_via_operator() {
    // Use the * operator which instantiates FpMul
    let source = r#"
        use skalp::numeric::fp::*;

        async entity TestMulOp {
            in a: bit[32]
            in b: bit[32]
            out result: bit[32]
        }
        impl TestMulOp {
            signal a_fp: fp32 = a as fp32
            signal b_fp: fp32 = b as fp32
            signal prod: fp32 = a_fp * b_fp
            result = prod as bit[32]
        }
    "#;

    run_mul_test(source, "Operator (*)", "top.a", "top.b", "top.result");
}

#[test]
fn test_fpmul_direct_instantiation() {
    // Directly instantiate FpMul
    let source = r#"
        use skalp::numeric::fp::*;

        async entity TestMulDirect {
            in a: fp32
            in b: fp32
            out result: fp32
        }
        impl TestMulDirect {
            signal mul: FpMul<IEEE754_32>
            mul.a = a
            mul.b = b
            result = mul.result
        }
    "#;

    run_mul_test(source, "Direct FpMul", "top.a", "top.b", "top.result");
}

#[test]
fn test_inline_mul_logic() {
    // Inline the FpMul logic without using the entity
    let source = r#"
        async entity TestMulInline {
            in a: bit[32]
            in b: bit[32]
            out result: bit[32]
        }
        impl TestMulInline {
            // Extract mantissa bits and add implicit 1
            signal a_mant: bit[23] = a[22:0]
            signal b_mant: bit[23] = b[22:0]
            signal a_frac: bit[24] = {1'b1, a_mant}
            signal b_frac: bit[24] = {1'b1, b_mant}

            // Multiply
            signal product: bit[48] = a_frac * b_frac

            // Normalize
            signal prod_overflow: bit = product[47]
            signal product_norm: bit[48] = prod_overflow ? (product >> 1) : product

            // Extract mantissa from bits [45:23] (skipping implicit 1 at bit 46)
            signal mant_extracted: bit[23] = product_norm[45:23]

            // Calculate exponent
            signal a_exp: bit[8] = a[30:23]
            signal b_exp: bit[8] = b[30:23]
            signal exp_sum: bit[9] = a_exp + b_exp + (prod_overflow ? 1 : 0)
            signal result_exp: bit[8] = exp_sum - 127

            // Assemble result
            signal result_sign: bit = a[31] ^ b[31]
            result = {result_sign, result_exp, mant_extracted}
        }
    "#;

    run_mul_test(source, "Inline logic", "top.a", "top.b", "top.result");
}

fn run_mul_test(source: &str, name: &str, a_port: &str, b_port: &str, result_port: &str) {
    eprintln!("\n========== {} ==========", name);

    let netlist = compile_to_gates(source);
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

    eprintln!("Input a = 0x{:08X} (2.0)", a);
    eprintln!("Input b = 0x{:08X} (3.0)", b);
    eprintln!("Expected = 0x{:08X} (6.0)", expected);

    sim.set_dual_rail_value(a_port, a, 32);
    sim.set_dual_rail_value(b_port, b, 32);

    let iters = sim.run_until_stable(1000);
    eprintln!("Iterations: {}", iters);

    if let Some(result) = sim.get_dual_rail_value(result_port, 32) {
        eprintln!("Result = 0x{:08X}", result);

        let sign = (result >> 31) & 1;
        let exp = ((result >> 23) & 0xFF) as i32;
        let mant = result & 0x7FFFFF;

        eprintln!("  sign={}, exp={}, mant=0x{:06X}", sign, exp, mant);

        if result == expected {
            eprintln!("PASS");
        } else {
            eprintln!("FAIL: expected 0x{:08X}", expected);
            // Decode expected
            let exp_exp = (expected >> 23) & 0xFF;
            let exp_mant = expected & 0x7FFFFF;
            eprintln!("  expected: exp={}, mant=0x{:06X}", exp_exp, exp_mant);
        }
    } else {
        eprintln!("Simulation did not converge!");
    }
}
