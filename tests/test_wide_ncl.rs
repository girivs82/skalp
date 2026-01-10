//! Test for wide (256-bit) NCL operations
//! This helps diagnose oscillation issues in the Karythra CLE

use skalp_lir::gate_netlist::GateNetlist;
use skalp_sim::GpuNclRuntime;
use std::process::Command;

fn compile_wide_ncl() -> GateNetlist {
    let sk_code = r#"
async entity WideNcl {
    in a: bit[256]
    in b: bit[256]
    out sum: bit[256]
}

impl WideNcl {
    sum = a + b
}
"#;

    std::fs::write("/tmp/test_wide_ncl.sk", sk_code).unwrap();

    // Use --no-synth-opt to preserve NCL gate structure for simulation
    let output = Command::new("./target/release/skalp")
        .env("SKALP_STDLIB_PATH", "./crates/skalp-stdlib")
        .args([
            "build",
            "-s",
            "/tmp/test_wide_ncl.sk",
            "-o",
            "/tmp/wide_ncl_out",
            "--target",
            "gates",
            "--no-synth-opt",
        ])
        .output()
        .expect("Failed to run skalp");

    if !output.status.success() {
        eprintln!("stderr: {}", String::from_utf8_lossy(&output.stderr));
        panic!("Compilation failed");
    }

    let json = std::fs::read_to_string("/tmp/wide_ncl_out/design_gates.json")
        .expect("Failed to read netlist");
    serde_json::from_str(&json).expect("Failed to parse netlist")
}

#[test]
fn test_wide_ncl_256bit_add() {
    let netlist = compile_wide_ncl();
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    let mut runtime = GpuNclRuntime::new(netlist).expect("Failed to create runtime");

    // Test 1: Simple small values
    println!("\nTest 1: 100 + 50 = 150");
    runtime.set_dual_rail_value("a", 100, 256);
    runtime.set_dual_rail_value("b", 50, 256);

    let iterations = runtime.run_until_stable(10000);
    let sum = runtime.get_dual_rail_value("sum", 64).unwrap_or(0); // Get lower 64 bits

    println!(
        "  Iterations: {}, stable: {}",
        iterations,
        runtime.is_stable()
    );
    println!("  sum = {} (expected 150)", sum);

    if !runtime.is_stable() {
        // Debug: identify oscillating cells
        println!("\n  Identifying oscillating cells...");
        let oscillating = runtime.identify_oscillating_cells(100);
        println!("  Found {} oscillating cells:", oscillating.len());
        for (path, cell_type, count) in oscillating.iter().take(10) {
            println!("    {} ({}) - {} changes", path, cell_type, count);
        }
    }

    assert!(runtime.is_stable(), "256-bit NCL should stabilize");
    assert_eq!(sum, 150, "100 + 50 should equal 150");

    // Test 2: Larger values - create fresh runtime
    println!("\nTest 2: 0xFFFF_FFFF + 1 = 0x1_0000_0000");
    let netlist2 = compile_wide_ncl();
    let mut runtime2 = GpuNclRuntime::new(netlist2).expect("Failed to create runtime");
    runtime2.set_dual_rail_value("a", 0xFFFF_FFFF, 256);
    runtime2.set_dual_rail_value("b", 1, 256);

    let iterations2 = runtime2.run_until_stable(10000);
    let sum2 = runtime2.get_dual_rail_value("sum", 64).unwrap_or(0);

    println!(
        "  Iterations: {}, stable: {}",
        iterations2,
        runtime2.is_stable()
    );
    println!("  sum = 0x{:X} (expected 0x100000000)", sum2);

    assert!(runtime2.is_stable(), "256-bit NCL should stabilize");
    assert_eq!(sum2, 0x1_0000_0000, "Carry should propagate correctly");
}
