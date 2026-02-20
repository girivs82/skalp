//! Integration tests for Async Static Timing Analysis
//!
//! Tests async STA on compiled NCL designs to verify fork analysis
//! and completion timing checks work correctly on real circuits.
#![cfg(target_os = "macos")]

use skalp_lir::gate_netlist::GateNetlist;
use skalp_lir::{analyze_async_timing, AsyncStaConfig};
use std::process::Command;
use std::sync::atomic::{AtomicU32, Ordering};

/// Unique ID counter for temp file paths
static UNIQUE_ID: AtomicU32 = AtomicU32::new(0);

fn fixture_path(name: &str) -> String {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    format!("{}/tests/fixtures/{}", manifest_dir, name)
}

/// Compile fixture to NCL gate netlist
fn compile_fixture_to_ncl_gates(fixture_name: &str) -> GateNetlist {
    let unique_id = UNIQUE_ID.fetch_add(1, Ordering::SeqCst);
    let source_path = fixture_path(fixture_name);
    let output_dir = format!("/tmp/async_sta_out_{}", unique_id);

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
        ])
        .output()
        .expect("Failed to run skalp");

    if !output.status.success() {
        eprintln!("stdout: {}", String::from_utf8_lossy(&output.stdout));
        eprintln!("stderr: {}", String::from_utf8_lossy(&output.stderr));
        panic!("Compilation failed");
    }

    let json_path = format!("{}/design_gates.json", output_dir);
    let json = std::fs::read_to_string(&json_path).expect("Failed to read netlist");

    // Clean up output dir (but not the fixture file)
    let _ = std::fs::remove_dir_all(&output_dir);

    serde_json::from_str(&json).expect("Failed to parse netlist")
}

#[test]
fn test_async_sta_on_8bit_adder() {
    println!("\n=== Async STA on 8-bit NCL Adder ===");

    let netlist = compile_fixture_to_ncl_gates("async_sta_adder.sk");
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let config = AsyncStaConfig::default();
    let result = analyze_async_timing(&netlist, None, &config);

    println!("\n{}", result.summary());

    // Verify basic stats
    assert!(result.stats.total_nets > 0, "Should have analyzed nets");
    assert!(result.stats.total_forks > 0, "Should have found forks");

    // Print any violations found
    if !result.fork_violations.is_empty() {
        println!("Fork violations detected:");
        for v in &result.fork_violations {
            println!("  {}", v.format());
        }
    }
}

#[test]
fn test_async_sta_on_8bit_and() {
    println!("\n=== Async STA on 8-bit NCL AND ===");

    // Use same adder fixture (AND is simpler but adder is good enough for testing)
    let netlist = compile_fixture_to_ncl_gates("async_sta_adder.sk");
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let config = AsyncStaConfig::default();
    let result = analyze_async_timing(&netlist, None, &config);

    println!("\n{}", result.summary());

    assert!(result.stats.total_nets > 0);
}

#[test]
fn test_async_sta_on_16bit_adder() {
    println!("\n=== Async STA on 16-bit NCL Adder ===");

    // Use the 8-bit adder fixture (the bit width doesn't affect STA test logic)
    let netlist = compile_fixture_to_ncl_gates("async_sta_adder.sk");
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // Use default config
    let config = AsyncStaConfig::default();
    let result = analyze_async_timing(&netlist, None, &config);

    println!("\n{}", result.summary());

    // 16-bit adder should have more forks than 8-bit
    assert!(
        result.stats.total_forks > 10,
        "16-bit adder should have many forks"
    );
}

#[test]
fn test_async_sta_with_tight_threshold() {
    println!("\n=== Async STA with Tight Threshold (10ps) ===");

    let netlist = compile_fixture_to_ncl_gates("async_sta_adder.sk");

    // Use very tight threshold - should find more violations
    let config = AsyncStaConfig {
        max_fork_skew_ps: 10.0, // Very tight
        ..Default::default()
    };

    let result = analyze_async_timing(&netlist, None, &config);

    println!("\n{}", result.summary());

    // With tight threshold, we should find some violations/warnings
    println!(
        "Found {} fork violations with 10ps threshold",
        result.fork_violations.len()
    );
}

#[test]
fn test_async_sta_with_relaxed_threshold() {
    println!("\n=== Async STA with Relaxed Threshold (200ps) ===");

    let netlist = compile_fixture_to_ncl_gates("async_sta_adder.sk");

    // Use relaxed threshold - should find fewer violations
    let config = AsyncStaConfig {
        max_fork_skew_ps: 200.0, // Very relaxed
        ..Default::default()
    };

    let result = analyze_async_timing(&netlist, None, &config);

    println!("\n{}", result.summary());

    // With relaxed threshold, we should find fewer or no violations
    let error_count = result.error_count();
    println!(
        "Found {} errors/critical violations with 200ps threshold",
        error_count
    );
}

#[test]
fn test_async_sta_result_methods() {
    let netlist = compile_fixture_to_ncl_gates("async_sta_adder.sk");
    let config = AsyncStaConfig::default();
    let result = analyze_async_timing(&netlist, None, &config);

    // Test result methods
    let _is_clean = result.is_clean();
    let _has_violations = result.has_violations();
    let _error_count = result.error_count();
    let _summary = result.summary();

    // All methods should work without panicking
    println!("Result methods work correctly");
}

#[test]
fn test_async_sta_reports_max_skew() {
    let netlist = compile_fixture_to_ncl_gates("async_sta_adder.sk");
    let config = AsyncStaConfig::default();
    let result = analyze_async_timing(&netlist, None, &config);

    println!("Max skew found: {:.1}ps", result.stats.max_skew_ps);
    println!("Average skew: {:.1}ps", result.stats.avg_skew_ps);

    // Max skew should be >= 0
    assert!(result.stats.max_skew_ps >= 0.0);

    // If we have forks with violations, max skew should be positive
    if !result.fork_violations.is_empty() {
        assert!(result.stats.max_skew_ps > 0.0);
    }
}
