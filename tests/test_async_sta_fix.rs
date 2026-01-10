//! Integration tests for Async STA Fix (buffer insertion)
//!
//! Tests the full flow: compile → simulate → STA → fix → verify

use skalp_lir::gate_netlist::GateNetlist;
use skalp_lir::{
    analyze_async_timing, analyze_async_timing_with_oscillations, fix_fork_violations,
    run_iterative_sta_fix, AsyncStaConfig, AsyncStaFixConfig, FixStrategy,
};
use skalp_sim::GpuNclRuntime;
use std::process::Command;
use std::sync::atomic::{AtomicU32, Ordering};

/// Unique ID counter for temp file paths
static UNIQUE_ID: AtomicU32 = AtomicU32::new(0);

/// Compile source to NCL gate netlist
fn compile_to_ncl_gates(source: &str) -> GateNetlist {
    let unique_id = UNIQUE_ID.fetch_add(1, Ordering::SeqCst);
    let source_path = format!("/tmp/test_sta_fix_{}.sk", unique_id);
    let output_dir = format!("/tmp/sta_fix_out_{}", unique_id);

    std::fs::write(&source_path, source).unwrap();

    // Use --no-synth-opt to preserve NCL gate structure for simulation
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
            "--no-synth-opt",
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

/// 8-bit NCL adder - simple design with potential fork violations
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

/// 16-bit NCL adder - larger design with more forks
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

/// 8-bit NCL ALU - more complex design
const ALU_8BIT: &str = r#"
async entity NclAlu8 {
    in a: bit[8]
    in b: bit[8]
    in op: bit[2]
    out result: bit[8]
}

impl NclAlu8 {
    result = match op {
        0b00 => a + b,
        0b01 => a - b,
        0b10 => a & b,
        0b11 => a | b,
    }
}
"#;

#[test]
fn test_sta_fix_on_8bit_adder() {
    println!("\n=== Async STA Fix on 8-bit NCL Adder ===");

    let mut netlist = compile_to_ncl_gates(ADDER_8BIT);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // Run STA with tight threshold to find violations
    let sta_config = AsyncStaConfig {
        max_fork_skew_ps: 10.0, // Tight threshold
        ..Default::default()
    };

    let result = analyze_async_timing(&netlist, None, &sta_config);
    println!("\nBefore fix:");
    println!("  Forks analyzed: {}", result.stats.total_forks);
    println!("  Violations: {}", result.fork_violations.len());
    println!("  Max skew: {:.1}ps", result.stats.max_skew_ps);

    if result.fork_violations.is_empty() {
        println!("  No violations to fix!");
        return;
    }

    // Fix violations
    let fix_config = AsyncStaFixConfig::default();
    let fix_result = fix_fork_violations(&mut netlist, &result, &fix_config);

    println!("\nFix result:");
    println!("  Violations fixed: {}", fix_result.violations_fixed);
    println!("  Buffers inserted: {}", fix_result.buffers_inserted);

    // Re-run STA to verify
    let result_after = analyze_async_timing(&netlist, None, &sta_config);
    println!("\nAfter fix:");
    println!("  Violations: {}", result_after.fork_violations.len());
    println!("  Max skew: {:.1}ps", result_after.stats.max_skew_ps);

    // Verify buffers were added
    let buffer_count = netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.contains("BUF"))
        .count();
    println!("  Total buffers in netlist: {}", buffer_count);

    assert!(
        fix_result.buffers_inserted > 0 || result.fork_violations.is_empty(),
        "Should have inserted buffers for violations"
    );
}

#[test]
fn test_iterative_sta_fix_on_16bit_adder() {
    println!("\n=== Iterative Async STA Fix on 16-bit NCL Adder ===");

    let mut netlist = compile_to_ncl_gates(ADDER_16BIT);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let sta_config = AsyncStaConfig {
        max_fork_skew_ps: 15.0, // Moderate threshold
        ..Default::default()
    };

    let fix_config = AsyncStaFixConfig {
        buffer_delay_ps: 20.0,
        max_buffers_per_violation: 5,
        ..Default::default()
    };

    // Run iterative flow
    let result = run_iterative_sta_fix(&mut netlist, &sta_config, &fix_config, 5);

    println!("{}", result.summary());

    // Check that we made progress
    if result.iterations > 1 {
        assert!(
            result.total_buffers_inserted > 0,
            "Should have inserted some buffers"
        );
    }
}

#[test]
fn test_sta_fix_with_simulation_data() {
    println!("\n=== Async STA Fix with Simulation Data ===");

    let mut netlist = compile_to_ncl_gates(ADDER_8BIT);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // Clone netlist for simulation (GpuNclRuntime takes ownership)
    let sim_netlist = netlist.clone();

    // Run simulation to get oscillation data
    let mut runtime = match GpuNclRuntime::new(sim_netlist) {
        Ok(r) => r,
        Err(e) => {
            println!("Skipping simulation-based test: {}", e);
            return;
        }
    };

    // Set some test inputs
    runtime.set_dual_rail_value("a", 100, 8);
    runtime.set_dual_rail_value("b", 55, 8);

    let iterations = runtime.run_until_stable(10000);
    println!("Simulation converged in {} iterations", iterations);

    // Get oscillation data
    let cell_osc = runtime.get_oscillation_counts();
    let net_osc = runtime.get_net_oscillation_counts();

    println!("Oscillation data collected:");
    println!("  Cells with oscillation data: {}", cell_osc.len());
    println!("  Nets with oscillation data: {}", net_osc.len());

    // Run STA with oscillation data
    let sta_config = AsyncStaConfig {
        max_fork_skew_ps: 20.0,
        ..Default::default()
    };

    let result =
        analyze_async_timing_with_oscillations(&netlist, None, &sta_config, cell_osc, net_osc);

    println!("\nSTA with oscillation data:");
    println!("  Forks analyzed: {}", result.stats.total_forks);
    println!("  Violations: {}", result.fork_violations.len());
    println!("  Max skew: {:.1}ps", result.stats.max_skew_ps);

    // Fix if needed
    if !result.fork_violations.is_empty() {
        let fix_config = AsyncStaFixConfig::default();
        let fix_result = fix_fork_violations(&mut netlist, &result, &fix_config);
        println!("\nFix result:");
        println!("  Buffers inserted: {}", fix_result.buffers_inserted);
    }
}

#[test]
fn test_sta_fix_on_alu_per_fork() {
    println!("\n=== Async STA Fix on 8-bit NCL ALU (Per-Fork Buffering) ===");

    let mut netlist = compile_to_ncl_gates(ALU_8BIT);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let sta_config = AsyncStaConfig {
        max_fork_skew_ps: 25.0,
        ..Default::default()
    };

    // Use per-fork buffering strategy (original approach)
    let fix_config = AsyncStaFixConfig {
        strategy: FixStrategy::PerForkBuffering,
        ..Default::default()
    };

    // Run iterative flow
    let result = run_iterative_sta_fix(&mut netlist, &sta_config, &fix_config, 3);

    println!("{}", result.summary());

    // Verify netlist integrity after fixes
    for cell in &netlist.cells {
        // Each cell's inputs should reference valid nets
        for input_net_id in &cell.inputs {
            assert!(
                netlist.nets.iter().any(|n| n.id == *input_net_id),
                "Cell {} has invalid input net {:?}",
                cell.id.0,
                input_net_id
            );
        }
    }

    // Each net's fanout should reference valid cells
    for net in &netlist.nets {
        for (cell_id, _) in &net.fanout {
            assert!(
                netlist.cells.iter().any(|c| c.id == *cell_id),
                "Net {} has invalid fanout cell {:?}",
                net.name,
                cell_id
            );
        }
    }

    println!("Netlist integrity verified after fixes");
}

#[test]
fn test_fix_details_report() {
    println!("\n=== Detailed Fix Report ===");

    let mut netlist = compile_to_ncl_gates(ADDER_8BIT);

    let sta_config = AsyncStaConfig {
        max_fork_skew_ps: 5.0, // Very tight to ensure violations
        ..Default::default()
    };

    let result = analyze_async_timing(&netlist, None, &sta_config);

    if result.fork_violations.is_empty() {
        println!("No violations found with 5ps threshold");
        return;
    }

    // Use PerForkBuffering to see per-violation details
    let fix_config = AsyncStaFixConfig {
        strategy: FixStrategy::PerForkBuffering,
        buffer_delay_ps: 15.0,
        ..Default::default()
    };

    let fix_result = fix_fork_violations(&mut netlist, &result, &fix_config);

    println!("Detailed fixes:");
    for fix in &fix_result.fixes {
        println!("  Fork: {}", fix.fork_net_name);
        println!("    Fast branch: cell_{}", fix.fast_branch_cell.0);
        println!("    Buffers inserted: {}", fix.buffers_inserted);
        println!(
            "    Skew: {:.1}ps -> {:.1}ps (estimated)",
            fix.original_skew_ps, fix.estimated_skew_ps
        );
    }
}

#[test]
fn test_sta_fix_delay_ready_signal() {
    println!("\n=== Async STA Fix with Ready Signal Delay ===");

    let mut netlist = compile_to_ncl_gates(ALU_8BIT);
    let original_cells = netlist.cells.len();
    let original_nets = netlist.nets.len();
    println!("Compiled: {} cells, {} nets", original_cells, original_nets);

    // Check for detection nets
    let detection_nets: Vec<_> = netlist
        .nets
        .iter()
        .filter(|n| n.is_detection)
        .map(|n| n.name.clone())
        .collect();
    println!("Detection nets: {:?}", detection_nets);

    let sta_config = AsyncStaConfig {
        max_fork_skew_ps: 25.0,
        ..Default::default()
    };

    // Run initial STA
    let result = analyze_async_timing(&netlist, None, &sta_config);
    println!(
        "\nBefore fix: {} violations, max skew {:.1}ps",
        result.fork_violations.len(),
        result.stats.max_skew_ps
    );

    if result.fork_violations.is_empty() {
        println!("No violations to fix!");
        return;
    }

    // Use DelayReadySignal strategy (default)
    let fix_config = AsyncStaFixConfig {
        strategy: FixStrategy::DelayReadySignal,
        buffer_delay_ps: 20.0,
        ready_delay_margin_ps: 10.0,
        ..Default::default()
    };

    // Single-pass fix
    let fix_result = fix_fork_violations(&mut netlist, &result, &fix_config);

    println!("\nFix result (DelayReadySignal):");
    println!("  Strategy: DelayReadySignal");
    println!("  Violations fixed: {}", fix_result.violations_fixed);
    println!("  Buffers inserted: {}", fix_result.buffers_inserted);
    println!("  New cell count: {}", netlist.cells.len());
    println!("  New net count: {}", netlist.nets.len());

    for fix in &fix_result.fixes {
        println!(
            "  Fixed: {} with {} buffers",
            fix.fork_net_name, fix.buffers_inserted
        );
    }

    // Verify single-pass convergence for DelayReadySignal
    // Run STA again - violations should still exist (forks still there)
    // but the ready signal delay covers them
    let result_after = analyze_async_timing(&netlist, None, &sta_config);
    println!(
        "\nAfter fix: {} violations (expected - forks unchanged)",
        result_after.fork_violations.len()
    );

    // The key benefit: buffers are concentrated on ready signal path
    // not scattered throughout the datapath
    let ready_buffers = netlist
        .cells
        .iter()
        .filter(|c| c.source_op.as_deref() == Some("async_sta_fix_ready_delay"))
        .count();
    println!("Ready delay buffers: {}", ready_buffers);

    // Verify netlist integrity
    for cell in &netlist.cells {
        for input_net_id in &cell.inputs {
            assert!(
                netlist.nets.iter().any(|n| n.id == *input_net_id),
                "Cell {} has invalid input net {:?}",
                cell.id.0,
                input_net_id
            );
        }
    }
    println!("Netlist integrity verified");
}
