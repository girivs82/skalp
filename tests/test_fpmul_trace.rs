// Skip in debug mode due to stack overflow (complex FP32 expansion)

// Trace intermediate values in FP32 multiplication
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
    let temp_path_str = format!("/tmp/test_fpmul_trace_{:?}.sk", thread_id);
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
fn test_fpmul_trace_internals() {
    // Expose internal signals for debugging
    let source = r#"
        use skalp::numeric::fp::*;

        async entity TestMulTrace {
            in a: bit[32]
            in b: bit[32]
            out result: bit[32]
            out product_lo: bit[32]
            out product_hi: bit[16]
            out mant_debug: bit[26]
            out mant_raw_out: bit[23]
        }
        impl TestMulTrace {
            signal a_fp: fp32 = a as fp32
            signal b_fp: fp32 = b as fp32

            // Extract mantissa with implicit 1
            signal a_mant: bit[23] = a[22:0]
            signal b_mant: bit[23] = b[22:0]
            signal a_frac: bit[24] = {1'b1, a_mant}
            signal b_frac: bit[24] = {1'b1, b_mant}

            // Multiply - should be 48 bits
            signal product: bit[48] = a_frac * b_frac

            // Check overflow (bit 47)
            signal prod_overflow: bit = product[47]
            signal product_norm: bit[48] = prod_overflow ? (product >> 1) : product

            // Extract mantissa - skip implicit 1 at bit 46
            // We want bits [45:20] for M+GUARD_BITS=26 bits
            signal mant_with_guard: bit[26] = product_norm[45:20]

            // Extract raw mantissa [25:3] from mant_with_guard
            signal mant_raw: bit[23] = mant_with_guard[25:3]

            // Now do the full FP mul
            signal prod: fp32 = a_fp * b_fp

            // Outputs
            result = prod as bit[32]
            product_lo = product[31:0]
            product_hi = product[47:32]
            mant_debug = mant_with_guard
            mant_raw_out = mant_raw
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

    eprintln!("\n=== Input ===");
    eprintln!("a = 0x{:08X} (2.0)", a);
    eprintln!("b = 0x{:08X} (3.0)", b);
    eprintln!("Expected = 0x{:08X} (6.0)", expected);

    // a_frac = 0x800000 (1.0)
    // b_frac = 0xC00000 (1.5)
    // product = 0x600000000000 (48-bit, bits 46 and 45 set)
    eprintln!("\n=== Expected intermediate values ===");
    eprintln!("a_frac = 0x{:06X} (1.0 in Q1.23)", 0x800000u32);
    eprintln!("b_frac = 0x{:06X} (1.5 in Q1.23)", 0xC00000u32);
    let expected_product: u64 = 0x800000 * 0xC00000;
    eprintln!("product = 0x{:012X} (expected)", expected_product);
    eprintln!(
        "product_lo = 0x{:08X}",
        (expected_product & 0xFFFFFFFF) as u32
    );
    eprintln!("product_hi = 0x{:04X}", (expected_product >> 32) as u16);
    // mant_with_guard should be bits [45:20] of product
    let expected_mant_with_guard = ((expected_product >> 20) & 0x3FFFFFF) as u32;
    eprintln!(
        "mant_with_guard = 0x{:07X} (bits [45:20])",
        expected_mant_with_guard
    );
    // mant_raw should be bits [25:3] of mant_with_guard
    let expected_mant_raw = (expected_mant_with_guard >> 3) & 0x7FFFFF;
    eprintln!(
        "mant_raw = 0x{:06X} (bits [25:3] of mant_with_guard)",
        expected_mant_raw
    );

    sim.set_dual_rail_value("top.a", a, 32);
    sim.set_dual_rail_value("top.b", b, 32);

    let iters = sim.run_until_stable(1000);
    eprintln!("\n=== Simulation result (iterations: {}) ===", iters);

    // Get intermediate values
    if let Some(product_lo) = sim.get_dual_rail_value("top.product_lo", 32) {
        eprintln!("product_lo = 0x{:08X}", product_lo);
    } else {
        eprintln!("product_lo: INVALID");
    }

    if let Some(product_hi) = sim.get_dual_rail_value("top.product_hi", 16) {
        eprintln!("product_hi = 0x{:04X}", product_hi);
    } else {
        eprintln!("product_hi: INVALID");
    }

    if let Some(mant_debug) = sim.get_dual_rail_value("top.mant_debug", 26) {
        eprintln!("mant_debug = 0x{:07X}", mant_debug);
    } else {
        eprintln!("mant_debug: INVALID");
    }

    if let Some(mant_raw) = sim.get_dual_rail_value("top.mant_raw_out", 23) {
        eprintln!("mant_raw_out = 0x{:06X}", mant_raw);
    } else {
        eprintln!("mant_raw_out: INVALID");
    }

    if let Some(result) = sim.get_dual_rail_value("top.result", 32) {
        eprintln!("\nresult = 0x{:08X}", result);
        let sign = (result >> 31) & 1;
        let exp = ((result >> 23) & 0xFF) as i32;
        let mant = result & 0x7FFFFF;
        eprintln!("  sign={}, exp={}, mant=0x{:06X}", sign, exp, mant);

        if result == expected {
            eprintln!("PASS");
        } else {
            eprintln!("FAIL: expected 0x{:08X}", expected);
        }
    } else {
        panic!("Simulation did not converge");
    }
}
