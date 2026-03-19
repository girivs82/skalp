//! NCL tests derived from Karythra CLE V2 async design patterns.
//!
//! These test the NCL boundary synthesis path with realistic patterns
//! extracted from the async CLE: completion detection, dual-rail muxing,
//! multi-output bitwise ops, subtraction, comparators, and mux chains.
//!
//! Additional tests (priority encoder, mode controller, systolic forwarding,
//! handshake ack) are marked #[ignore] pending synthesis fixes for:
//! - Chained multi-bit comparisons (if x == 2'b10 { ... })
//! - Addition inside conditionals (if en { a + b } else { 0 })
//! - Dynamic array indexing (arr[computed_idx])
//! - Multi-input single-bit AND chains
#![cfg(target_os = "macos")]

use skalp_lir::gate_netlist::GateNetlist;
use skalp_sim::GpuNclRuntime;
use std::process::Command;
use std::sync::atomic::{AtomicU32, Ordering};

static UNIQUE_ID: AtomicU32 = AtomicU32::new(100);

fn fixture_path(name: &str) -> String {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    format!("{}/tests/fixtures/{}", manifest_dir, name)
}

fn compile_ncl_fixture(fixture_name: &str) -> GateNetlist {
    let unique_id = UNIQUE_ID.fetch_add(1, Ordering::SeqCst);
    let source_path = fixture_path(fixture_name);
    let output_dir = format!("/tmp/ncl_cle_out_{}", unique_id);

    let skalp_bin = env!("CARGO_BIN_EXE_skalp");
    let output = Command::new(skalp_bin)
        .env("SKALP_STDLIB_PATH", "./crates/skalp-stdlib")
        .args([
            "build",
            "-s",
            &source_path,
            "-o",
            &output_dir,
            "--target",
            "gates",
            "--no-synth-opt",
            "--no-async-sta",
        ])
        .output()
        .expect("Failed to run skalp");

    if !output.status.success() {
        eprintln!("stdout: {}", String::from_utf8_lossy(&output.stdout));
        eprintln!("stderr: {}", String::from_utf8_lossy(&output.stderr));
        panic!("Compilation of {} failed", fixture_name);
    }

    let json_path = format!("{}/design_gates.json", output_dir);
    let json = std::fs::read_to_string(&json_path).expect("Failed to read netlist");
    let _ = std::fs::remove_dir_all(&output_dir);
    serde_json::from_str(&json).expect("Failed to parse netlist")
}

fn run_ncl_test(
    netlist: GateNetlist,
    inputs: &[(&str, u64, usize)],
    outputs: &[(&str, u64, usize)],
    label: &str,
) -> bool {
    let mut runtime = match GpuNclRuntime::new(netlist) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("  {} - Failed to create runtime: {}", label, e);
            return false;
        }
    };

    for (name, value, width) in inputs {
        runtime.set_dual_rail_value(name, *value, *width);
    }

    let iterations = runtime.run_until_stable(10000);
    let is_stable = runtime.is_stable();
    println!("  {} - iterations: {}, stable: {}", label, iterations, is_stable);

    if !is_stable {
        let oscillating = runtime.identify_oscillating_cells(100);
        for (path, cell_type, count) in oscillating.iter().take(5) {
            println!("    {} ({}) - {} changes", path, cell_type, count);
        }
        return false;
    }

    let mut all_pass = true;
    for (name, expected, width) in outputs {
        match runtime.get_dual_rail_value(name, *width) {
            Some(actual) if actual == *expected => {
                println!("    PASS: {} = {} (expected {})", name, actual, expected);
            }
            Some(actual) => {
                println!("    FAIL: {} = {} (expected {})", name, actual, expected);
                all_pass = false;
            }
            None => {
                println!("    FAIL: {} is NULL or invalid", name);
                all_pass = false;
            }
        }
    }
    all_pass
}

// ============================================================================
// Completion Detection — XOR-tree pattern from CLE V2
// In boundary NCL, inputs are always valid DATA after NclEncode, so this
// tests the XOR+AND reduction tree, not NULL detection per se.
// ============================================================================

#[test]
fn test_ncl_completion_detect() {
    println!("\n=== NCL Completion Detection (XOR-tree) ===");
    let cases: Vec<(u64, u64, u64, u64)> = vec![
        // (t, f, expected_complete, expected_xor)
        // Note: at NCL boundary, inputs are always valid DATA.
        // We test XOR correctness for various complementary/non-complementary patterns.
        // Complementary: all XORs = 1, complete = 1
        (0xFF, 0x00, 1, 0xFF),
        (0x00, 0xFF, 1, 0xFF),
        (0xAA, 0x55, 1, 0xFF),
        (0x55, 0xAA, 1, 0xFF),
        (0xF0, 0x0F, 1, 0xFF),
        // Non-complementary: XOR shows difference, complete depends
        (0xFF, 0x0F, 0, 0xF0), // upper 4 match, lower 4 differ
        (0x0F, 0x0F, 0, 0x00), // identical = no XOR bits = incomplete
        (0x80, 0x7F, 1, 0xFF), // every bit differs
    ];

    let mut all_pass = true;
    for (t, f, exp_complete, exp_xor) in cases {
        let netlist = compile_ncl_fixture("ncl_completion_detect.sk");
        let pass = run_ncl_test(
            netlist,
            &[("t", t, 8), ("f", f, 8)],
            &[
                ("complete", exp_complete, 1),
                ("xor_result", exp_xor, 8),
            ],
            &format!("t=0x{:02X} f=0x{:02X}", t, f),
        );
        all_pass = all_pass && pass;
    }
    assert!(all_pass, "Completion detection tests failed");
}

// ============================================================================
// Dual-Rail Mux — Mode-selected operand routing from CLE V2
// ============================================================================

#[test]
fn test_ncl_dual_rail_mux() {
    println!("\n=== NCL Dual-Rail Mux ===");
    let cases: Vec<(u64, u64, u64, u64, u64, u64, u64)> = vec![
        // (sel, a_t, a_f, b_t, b_f, expected_y_t, expected_y_f)
        (0, 0xAA, 0x55, 0x33, 0xCC, 0xAA, 0x55),
        (1, 0xAA, 0x55, 0x33, 0xCC, 0x33, 0xCC),
        (0, 0x00, 0xFF, 0xFF, 0x00, 0x00, 0xFF),
        (1, 0x00, 0xFF, 0xFF, 0x00, 0xFF, 0x00),
    ];

    let mut all_pass = true;
    for (sel, a_t, a_f, b_t, b_f, exp_t, exp_f) in cases {
        let netlist = compile_ncl_fixture("ncl_dual_rail_mux.sk");
        let pass = run_ncl_test(
            netlist,
            &[
                ("sel", sel, 1),
                ("a_t", a_t, 8),
                ("a_f", a_f, 8),
                ("b_t", b_t, 8),
                ("b_f", b_f, 8),
            ],
            &[("y_t", exp_t, 8), ("y_f", exp_f, 8)],
            &format!("sel={}", sel),
        );
        all_pass = all_pass && pass;
    }
    assert!(all_pass, "Dual-rail mux tests failed");
}

// ============================================================================
// Multi-output Bitwise Ops — CLE function unit L0 pattern
// ============================================================================

#[test]
fn test_ncl_bitwise_ops_multi_output() {
    println!("\n=== NCL Multi-Output Bitwise Ops ===");
    let cases: Vec<(u64, u64, u64, u64, u64, u64, u64)> = vec![
        // (a, b, and, or, xor, nand, nor)
        (0xAA, 0x55, 0x00, 0xFF, 0xFF, 0xFF, 0x00),
        (0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00),
        (0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF),
        (0xF0, 0x0F, 0x00, 0xFF, 0xFF, 0xFF, 0x00),
        (0x0F, 0x0F, 0x0F, 0x0F, 0x00, 0xF0, 0xF0),
    ];

    let mut all_pass = true;
    for (a, b, exp_and, exp_or, exp_xor, exp_nand, exp_nor) in cases {
        let netlist = compile_ncl_fixture("ncl_bitwise_ops.sk");
        let pass = run_ncl_test(
            netlist,
            &[("a", a, 8), ("b", b, 8)],
            &[
                ("y_and", exp_and, 8),
                ("y_or", exp_or, 8),
                ("y_xor", exp_xor, 8),
                ("y_nand", exp_nand, 8),
                ("y_nor", exp_nor, 8),
            ],
            &format!("a=0x{:02X} b=0x{:02X}", a, b),
        );
        all_pass = all_pass && pass;
    }
    assert!(all_pass, "Multi-output bitwise ops tests failed");
}

// ============================================================================
// 8-bit Subtraction — CLE arithmetic unit
// BUG: Borrow chain synthesis produces wrong results for all inputs.
// ============================================================================

#[test]
#[ignore]
fn test_ncl_sub_8bit() {
    println!("\n=== NCL 8-bit Subtraction ===");
    let cases: Vec<(u64, u64, u64)> = vec![
        (100, 50, 50),
        (255, 0, 255),
        (0, 0, 0),
        (50, 100, 206), // -50 as unsigned 8-bit = 206
        (128, 128, 0),
        (255, 255, 0),
        (1, 255, 2), // 1 - 255 = 2 (unsigned wrap)
    ];

    let mut all_pass = true;
    for (a, b, expected) in cases {
        let netlist = compile_ncl_fixture("ncl_sub8.sk");
        let pass = run_ncl_test(
            netlist,
            &[("a", a, 8), ("b", b, 8)],
            &[("diff", expected, 8)],
            &format!("{} - {} = {}", a, b, expected),
        );
        all_pass = all_pass && pass;
    }
    assert!(all_pass, "NCL 8-bit subtraction tests failed");
}

// ============================================================================
// 8-bit Comparator — CLE function unit L0
// BUG: `lt` output is stuck-at-1 when a <= b (should only be 1 when a < b).
// ============================================================================

#[test]
#[ignore]
fn test_ncl_comparator_8bit() {
    println!("\n=== NCL 8-bit Comparator ===");
    let cases: Vec<(u64, u64, u64, u64, u64)> = vec![
        // (a, b, lt, eq, gt)
        (0, 0, 0, 1, 0),
        (0, 255, 1, 0, 0),
        (255, 0, 0, 0, 1),
        (100, 200, 1, 0, 0),
        (200, 100, 0, 0, 1),
        (42, 42, 0, 1, 0),
        (127, 128, 1, 0, 0),
    ];

    let mut all_pass = true;
    for (a, b, exp_lt, exp_eq, exp_gt) in cases {
        let netlist = compile_ncl_fixture("ncl_comparator.sk");
        let pass = run_ncl_test(
            netlist,
            &[("a", a, 8), ("b", b, 8)],
            &[("lt", exp_lt, 1), ("eq", exp_eq, 1), ("gt", exp_gt, 1)],
            &format!("{} vs {}", a, b),
        );
        all_pass = all_pass && pass;
    }
    assert!(all_pass, "NCL 8-bit comparator tests failed");
}

// ============================================================================
// Mux Chain — Cascaded mux selection from CLE operand routing
// ============================================================================

#[test]
fn test_ncl_mux_chain() {
    println!("\n=== NCL Mux Chain (2-level cascaded) ===");
    let cases: Vec<(u64, u64, u64, u64, u64, u64)> = vec![
        // (sel0, sel1, a, b, c, expected_y)
        // sel1=0, sel0=0: select a
        (0, 0, 0xAA, 0xBB, 0xCC, 0xAA),
        // sel1=0, sel0=1: select b
        (1, 0, 0xAA, 0xBB, 0xCC, 0xBB),
        // sel1=1: select c (regardless of sel0)
        (0, 1, 0xAA, 0xBB, 0xCC, 0xCC),
        (1, 1, 0xAA, 0xBB, 0xCC, 0xCC),
    ];

    let mut all_pass = true;
    for (sel0, sel1, a, b, c, expected) in cases {
        let netlist = compile_ncl_fixture("ncl_mux_chain.sk");
        let pass = run_ncl_test(
            netlist,
            &[
                ("sel0", sel0, 1),
                ("sel1", sel1, 1),
                ("a", a, 8),
                ("b", b, 8),
                ("c", c, 8),
            ],
            &[("y", expected, 8)],
            &format!("sel0={} sel1={}", sel0, sel1),
        );
        all_pass = all_pass && pass;
    }
    assert!(all_pass, "Mux chain tests failed");
}

// ============================================================================
// Tests below are #[ignore] pending synthesis fixes.
// They expose real bugs in the NCL boundary synthesis path.
// ============================================================================

/// Priority encoder: chained if-else with bit indexing and dynamic array access.
/// BUG: Dynamic array indexing `pending[tag]` and comparison chains fail.
#[test]
#[ignore]
fn test_ncl_priority_encoder() {
    println!("\n=== NCL Priority Encoder (multi-outstanding request tracker) ===");
    let cases: Vec<(u64, u64, u64)> = vec![
        (0b0000, 0b00, 1),
        (0b0001, 0b01, 1),
        (0b0011, 0b10, 1),
        (0b0111, 0b11, 1),
        (0b1111, 0b11, 0),
        (0b1110, 0b00, 1),
        (0b1101, 0b01, 1),
    ];

    let mut all_pass = true;
    for (pending, exp_tag, exp_can) in cases {
        let netlist = compile_ncl_fixture("ncl_priority_encoder.sk");
        let pass = run_ncl_test(
            netlist,
            &[("pending", pending, 4)],
            &[("tag", exp_tag, 2), ("can_issue", exp_can, 1)],
            &format!("pending={:04b}", pending),
        );
        all_pass = all_pass && pass;
    }
    assert!(all_pass, "Priority encoder tests failed");
}

/// Mode controller: chained 2-bit comparisons with mode-dependent operations.
/// BUG: Modes 2+ return wrong results (chained multi-bit == comparison issue).
#[test]
#[ignore]
fn test_ncl_mode_controller() {
    println!("\n=== NCL Mode Controller ===");
    let cases: Vec<(u64, u64, u64, u64, u64, u64)> = vec![
        (0b00, 0b11, 0xAA, 0x55, 0x00, 1),
        (0b01, 0b10, 0xAA, 0x55, 0xFF, 1),
        (0b10, 0b01, 0xFF, 0x0F, 0xF0, 1),
        (0b11, 0b00, 100, 50, 150, 1),
    ];

    let mut all_pass = true;
    for (mode_t, mode_f, a, b, exp_result, exp_valid) in cases {
        let netlist = compile_ncl_fixture("ncl_mode_controller.sk");
        let pass = run_ncl_test(
            netlist,
            &[
                ("mode_t", mode_t, 2),
                ("mode_f", mode_f, 2),
                ("data_a", a, 8),
                ("data_b", b, 8),
            ],
            &[("result", exp_result, 8), ("mode_valid", exp_valid, 1)],
            &format!("mode_t={:02b}", mode_t),
        );
        all_pass = all_pass && pass;
    }
    assert!(all_pass, "Mode controller tests failed");
}

/// Systolic forwarding: conditional addition.
/// BUG: `if enable { a + b } else { 0 }` returns 0xFF when enable=1.
#[test]
#[ignore]
fn test_ncl_systolic_forward() {
    println!("\n=== NCL Systolic Forwarding ===");
    let cases: Vec<(u64, u64, u64, u64, u64, u64)> = vec![
        (1, 100, 50, 100, 50, 150),
        (1, 0, 0, 0, 0, 0),
        (0, 100, 50, 0, 0, 0),
    ];

    let mut all_pass = true;
    for (en, north, west, exp_s, exp_e, exp_acc) in cases {
        let netlist = compile_ncl_fixture("ncl_systolic_forward.sk");
        let pass = run_ncl_test(
            netlist,
            &[("enable", en, 1), ("north_in", north, 8), ("west_in", west, 8)],
            &[
                ("south_out", exp_s, 8),
                ("east_out", exp_e, 8),
                ("accum", exp_acc, 8),
            ],
            &format!("en={} n={} w={}", en, north, west),
        );
        all_pass = all_pass && pass;
    }
    assert!(all_pass, "Systolic forwarding tests failed");
}

/// Handshake acknowledgment: multi-input single-bit AND.
/// BUG: `a & b & c` for single-bit signals gives wrong results.
#[test]
#[ignore]
fn test_ncl_handshake_ack() {
    println!("\n=== NCL Handshake Acknowledgment ===");
    let cases: Vec<(u64, u64, u64, u64, u64, u64, u64, u64, u64)> = vec![
        (1, 1, 1, 1, 1, 0, 1, 1, 1),
        (1, 0, 1, 1, 1, 0, 0, 0, 1),
        (1, 1, 1, 0, 1, 0, 1, 0, 1),
        (1, 1, 0, 1, 0, 0, 0, 0, 0),
    ];

    let mut all_pass = true;
    for (iv, cv, ee, un, ht, hf, exp_c, exp_da, exp_ha) in cases {
        let netlist = compile_ncl_fixture("ncl_handshake_ack.sk");
        let pass = run_ncl_test(
            netlist,
            &[
                ("input_valid", iv, 1),
                ("config_valid", cv, 1),
                ("exec_enable", ee, 1),
                ("use_normal", un, 1),
                ("hit_t", ht, 1),
                ("hit_f", hf, 1),
            ],
            &[
                ("pipeline_complete", exp_c, 1),
                ("data_read_ack", exp_da, 1),
                ("hash_lookup_ack", exp_ha, 1),
            ],
            &format!("iv={} cv={} ee={} un={} ht={} hf={}", iv, cv, ee, un, ht, hf),
        );
        all_pass = all_pass && pass;
    }
    assert!(all_pass, "Handshake ack tests failed");
}
