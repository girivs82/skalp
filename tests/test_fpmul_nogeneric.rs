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

fn compile_to_gates(source: &str) -> GateNetlist {
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        "/Users/girivs/src/hw/hls/crates/skalp-stdlib",
    );

    let thread_id = std::thread::current().id();
    let temp_path_str = format!("/tmp/test_fpmul_nogeneric_{:?}.sk", thread_id);
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
fn test_fpmul32_entity() {
    // Define a non-generic FpMul32 entity with hardcoded constants
    let source = r#"
        /// Non-generic FP32 multiplication entity
        async entity FpMul32 {
            in a: bit[32]
            in b: bit[32]
            out result: bit[32]
            out flags: bit[5]
        }
        impl FpMul32 {
            // Hardcoded constants for fp32
            const W: nat = 32
            const E: nat = 8
            const M: nat = 23
            const BIAS: int = 127
            const MAX_EXP: nat = 255
            const FRAC_BITS: nat = 24  // M + 1
            const PROD_BITS: nat = 48  // FRAC_BITS * 2
            const GUARD_BITS: nat = 3

            // Extract fields
            signal a_sign: bit = a[31]
            signal a_exp: bit[8] = a[30:23]
            signal a_mant: bit[23] = a[22:0]

            signal b_sign: bit = b[31]
            signal b_exp: bit[8] = b[30:23]
            signal b_mant: bit[23] = b[22:0]

            // Result sign
            signal result_sign: bit = a_sign ^ b_sign

            // Classify
            signal a_zero: bit = (a_exp == 0) && (a_mant == 0)
            signal a_denorm: bit = (a_exp == 0) && (a_mant != 0)
            signal a_inf: bit = (a_exp == 255) && (a_mant == 0)
            signal a_nan: bit = (a_exp == 255) && (a_mant != 0)

            signal b_zero: bit = (b_exp == 0) && (b_mant == 0)
            signal b_denorm: bit = (b_exp == 0) && (b_mant != 0)
            signal b_inf: bit = (b_exp == 255) && (b_mant == 0)
            signal b_nan: bit = (b_exp == 255) && (b_mant != 0)

            // Build fractions with implicit 1
            signal a_frac: bit[24] = {(!a_denorm), a_mant}
            signal b_frac: bit[24] = {(!b_denorm), b_mant}

            // Effective exponents
            signal a_exp_eff: bit[8] = a_denorm ? 1 : a_exp
            signal b_exp_eff: bit[8] = b_denorm ? 1 : b_exp

            // Multiply fractions
            signal product: bit[48] = a_frac * b_frac

            // Normalize
            signal prod_overflow: bit = product[47]
            signal product_norm: bit[48] = prod_overflow ? (product >> 1) : product

            // Calculate exponent
            signal a_exp_masked: nat = (a_exp_eff as nat) & 255
            signal b_exp_masked: nat = (b_exp_eff as nat) & 255
            signal exp_sum: int = (a_exp_masked as int) + (b_exp_masked as int) - 127 +
                                 (prod_overflow ? 1 : 0)

            // Extract mantissa and round bits
            // After normalization, the implicit 1 is at bit 46
            // Extract from bit 45 down (skipping implicit 1)
            signal mant_with_guard: bit[26] = product_norm[45:20]

            signal mant_raw: bit[23] = mant_with_guard[25:3]
            signal guard: bit = mant_with_guard[2]
            signal round: bit = mant_with_guard[1]
            signal sticky: bit = |mant_with_guard[0:0] || |product_norm[19:0]

            // Round
            signal round_up: bit = guard && (round || sticky || mant_raw[0])
            signal mant_rounded: bit[24] = {1'b0, mant_raw} + round_up
            signal exp_adj: int = exp_sum + (mant_rounded[23] as int)

            signal result_mant: bit[23] = mant_rounded[23] ? mant_rounded[23:1] : mant_rounded[22:0]

            // Overflow/underflow
            signal overflow: bit = (exp_adj >= 255)
            signal underflow: bit = (exp_adj <= 0)
            signal result_exp: bit[8] = underflow ? 0 :
                                        overflow ? 255 :
                                        exp_adj as bit[8]

            // Assemble
            signal normal_result: bit[32] = {result_sign, result_exp, result_mant}

            // Special cases
            signal invalid: bit = ((a_zero && b_inf) || (a_inf && b_zero)) || a_nan || b_nan
            signal qnan: bit[32] = 0x7FC00000
            signal inf_val: bit[32] = {result_sign, 8'hFF, 23'h0}
            signal zero_val: bit[32] = {result_sign, 8'h0, 23'h0}

            result = invalid ? qnan :
                     a_nan ? a :
                     b_nan ? b :
                     (a_inf || b_inf) ? inf_val :
                     (a_zero || b_zero) ? zero_val :
                     overflow ? inf_val :
                     underflow ? zero_val :
                     normal_result

            flags = {invalid, 1'b0, overflow && !invalid, underflow && !invalid,
                    guard || round || sticky}
        }

        async entity TestFpMul32 {
            in a: bit[32]
            in b: bit[32]
            out result: bit[32]
        }
        impl TestFpMul32 {
            signal mul: FpMul32
            mul.a = a
            mul.b = b
            result = mul.result
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
