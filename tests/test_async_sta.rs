//! Integration tests for Async Static Timing Analysis
//!
//! Tests async STA on compiled NCL designs to verify fork analysis
//! and completion timing checks work correctly on real circuits.

use skalp_lir::gate_netlist::GateNetlist;
use skalp_lir::{analyze_async_timing, AsyncStaConfig};
use std::process::Command;
use std::sync::atomic::{AtomicU32, Ordering};

/// Unique ID counter for temp file paths
static UNIQUE_ID: AtomicU32 = AtomicU32::new(0);

/// Compile source to NCL gate netlist
fn compile_to_ncl_gates(source: &str) -> GateNetlist {
    let unique_id = UNIQUE_ID.fetch_add(1, Ordering::SeqCst);
    let source_path = format!("/tmp/test_async_sta_{}.sk", unique_id);
    let output_dir = format!("/tmp/async_sta_out_{}", unique_id);

    std::fs::write(&source_path, source).unwrap();

    let output = Command::new("./target/release/skalp")
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

    // Clean up
    let _ = std::fs::remove_file(&source_path);
    let _ = std::fs::remove_dir_all(&output_dir);

    serde_json::from_str(&json).expect("Failed to parse netlist")
}

/// Simple 8-bit NCL adder
const ADDER_8BIT: &str = r#"
async entity NclAdd8 {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[8]
}

impl NclAdd8 {
    sum = a + b
}
"#;

/// Simple 8-bit NCL AND
const AND_8BIT: &str = r#"
async entity NclAnd8 {
    in a: bit[8]
    in b: bit[8]
    out y: bit[8]
}

impl NclAnd8 {
    y = a & b
}
"#;

/// 16-bit adder (more complex)
const ADDER_16BIT: &str = r#"
async entity NclAdd16 {
    in a: bit[16]
    in b: bit[16]
    out sum: bit[16]
}

impl NclAdd16 {
    sum = a + b
}
"#;

#[test]
fn test_async_sta_on_8bit_adder() {
    println!("\n=== Async STA on 8-bit NCL Adder ===");

    let netlist = compile_to_ncl_gates(ADDER_8BIT);
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

    let netlist = compile_to_ncl_gates(AND_8BIT);
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

    let netlist = compile_to_ncl_gates(ADDER_16BIT);
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

    let netlist = compile_to_ncl_gates(ADDER_8BIT);

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

    let netlist = compile_to_ncl_gates(ADDER_8BIT);

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
    let netlist = compile_to_ncl_gates(AND_8BIT);
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
    let netlist = compile_to_ncl_gates(ADDER_16BIT);
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
