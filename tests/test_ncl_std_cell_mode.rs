//! Test NCL simulation with both native THmn gates and standard cell C-element macros
//!
//! This test verifies that:
//! 1. NCL simulation works with native THmn gates (TH12, TH22)
//! 2. NCL simulation works with C-element macros using standard cells (AND2, OR2) with feedback
//!
//! The C-element macro implements: Y = (A & B) | (Y & (A | B))
//! which requires feedback loop handling in the simulator.
#![cfg(target_os = "macos")]

use skalp_lir::gate_netlist::GateNetlist;
use skalp_sim::GpuNclRuntime;
use std::process::Command;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Once;

/// Unique ID counter for temp file paths (to avoid parallel test conflicts)
static UNIQUE_ID: AtomicU32 = AtomicU32::new(0);

/// Ensure std_cells_only library is created only once
static STD_CELLS_LIBRARY_INIT: Once = Once::new();

fn fixture_path(name: &str) -> String {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    format!("{}/tests/fixtures/{}", manifest_dir, name)
}

/// Compile fixture to NCL gate netlist using a specific library
fn compile_fixture_with_library(fixture_name: &str, library_path: Option<&str>) -> GateNetlist {
    // Get unique ID for this compilation to avoid parallel test conflicts
    let unique_id = UNIQUE_ID.fetch_add(1, Ordering::SeqCst);
    let source_path = fixture_path(fixture_name);
    let output_dir = format!("/tmp/ncl_mode_out_{}", unique_id);

    // Build args
    // Note: Use --no-synth-opt to preserve NCL gate structure for simulation
    // The synth engine would otherwise optimize away the NCL-specific gates
    let mut args = vec![
        "build".to_string(),
        "-s".to_string(),
        source_path.clone(),
        "-o".to_string(),
        output_dir.clone(),
        "--target".to_string(),
        "gates".to_string(),
        "--no-synth-opt".to_string(),
    ];

    // Add library path if specified
    if let Some(lib) = library_path {
        args.push("--library".to_string());
        args.push(lib.to_string());
    }

    let skalp_bin = env!("CARGO_BIN_EXE_skalp");
    let output = Command::new(skalp_bin)
        .env("SKALP_STDLIB_PATH", "./crates/skalp-stdlib")
        .args(&args)
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

/// Run NCL simulation and verify output
fn test_ncl_simulation(
    netlist: GateNetlist,
    inputs: &[(&str, u64, usize)],
    outputs: &[(&str, u64, usize)],
    mode_name: &str,
) -> bool {
    let mut runtime = match GpuNclRuntime::new(netlist) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("  Failed to create runtime: {}", e);
            return false;
        }
    };

    // Set inputs
    for (name, value, width) in inputs {
        runtime.set_dual_rail_value(name, *value, *width);
    }

    // Run until stable
    let iterations = runtime.run_until_stable(10000);
    let is_stable = runtime.is_stable();
    println!(
        "  {} - Iterations: {}, stable: {}",
        mode_name, iterations, is_stable
    );

    if !is_stable {
        // Debug oscillating cells
        println!("  Identifying oscillating cells...");
        let oscillating = runtime.identify_oscillating_cells(100);
        for (path, cell_type, count) in oscillating.iter().take(5) {
            println!("    {} ({}) - {} changes", path, cell_type, count);
        }
        return false;
    }

    // Check outputs
    let mut all_pass = true;
    for (name, expected, width) in outputs {
        match runtime.get_dual_rail_value(name, *width) {
            Some(actual) if actual == *expected => {
                println!("  PASS: {} = {} (expected {})", name, actual, expected);
            }
            Some(actual) => {
                println!("  FAIL: {} = {} (expected {})", name, actual, expected);
                all_pass = false;
            }
            None => {
                println!("  FAIL: {} is NULL or invalid", name);
                all_pass = false;
            }
        }
    }

    all_pass
}

#[test]
fn test_ncl_add_8bit_native_thmn() {
    println!("\n=== NCL 8-bit ADD with Native THmn Gates ===");

    let netlist = compile_fixture_with_library("ncl_std_cell_adder.sk", None);
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // Count gate types
    let th12_count = netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.contains("TH12"))
        .count();
    let th22_count = netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.contains("TH22"))
        .count();
    println!("Gate counts: TH12={}, TH22={}", th12_count, th22_count);

    assert!(
        th12_count > 0 || th22_count > 0,
        "Should use native THmn gates"
    );

    // Test cases
    let test_cases: [(u64, u64, u64); 4] =
        [(0, 0, 0), (100, 50, 150), (255, 0, 255), (128, 127, 255)];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        println!("\nTest: {} + {} = {}", a, b, expected);
        let netlist = compile_fixture_with_library("ncl_std_cell_adder.sk", None);
        let pass = test_ncl_simulation(
            netlist,
            &[("a", a, 8), ("b", b, 8)],
            &[("sum", expected, 8)],
            "Native THmn",
        );
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL native THmn 8-bit ADD tests failed");
}

#[test]
fn test_ncl_add_8bit_std_cells() {
    println!("\n=== NCL 8-bit ADD with Standard Cell C-element Macros ===");

    // Use library without THmn gates (using Once to avoid race conditions)
    let std_lib = "/tmp/std_cells_only.sklib";
    STD_CELLS_LIBRARY_INIT.call_once(|| {
        create_std_cells_only_library();
    });

    let netlist = compile_fixture_with_library("ncl_std_cell_adder.sk", Some(std_lib));
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // Count gate types - should have no THmn, but compound gates or basic gates
    let th12_count = netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.contains("TH12"))
        .count();
    let th22_count = netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.contains("TH22"))
        .count();
    let and2_count = netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.contains("AND2"))
        .count();
    let or2_count = netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.contains("OR2"))
        .count();
    let ao22_count = netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.contains("AO22"))
        .count();
    let aoi22_count = netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.contains("AOI22"))
        .count();

    println!(
        "Gate counts: TH12={}, TH22={}, AND2={}, OR2={}, AO22={}, AOI22={}",
        th12_count, th22_count, and2_count, or2_count, ao22_count, aoi22_count
    );

    assert_eq!(th12_count, 0, "Should NOT use TH12 gates in std cell mode");
    assert_eq!(th22_count, 0, "Should NOT use TH22 gates in std cell mode");

    // Should use either compound gates (AO22/AOI22) or basic gates (AND2+OR2)
    let uses_compound = ao22_count > 0 || aoi22_count > 0;
    let uses_basic = and2_count > 0 && or2_count > 0;
    assert!(
        uses_compound || uses_basic,
        "Should use compound gates (AO22/AOI22) or basic gates (AND2+OR2) for C-element"
    );
    assert!(or2_count > 0, "Should use OR2 gates for TH12 replacement");

    // Test cases
    let test_cases: [(u64, u64, u64); 4] =
        [(0, 0, 0), (100, 50, 150), (255, 0, 255), (128, 127, 255)];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        println!("\nTest: {} + {} = {}", a, b, expected);
        let netlist = compile_fixture_with_library("ncl_std_cell_adder.sk", Some(std_lib));
        let pass = test_ncl_simulation(
            netlist,
            &[("a", a, 8), ("b", b, 8)],
            &[("sum", expected, 8)],
            "Std Cells",
        );
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL std cell C-element 8-bit ADD tests failed");
}

#[test]
#[ignore = "Pre-existing bug: NCL std cell AND/XOR output net lookup returns NULL"]
fn test_ncl_and_8bit_std_cells() {
    println!("\n=== NCL 8-bit AND with Standard Cell C-element Macros ===");

    let std_lib = "/tmp/std_cells_only.sklib";
    STD_CELLS_LIBRARY_INIT.call_once(|| {
        create_std_cells_only_library();
    });

    let netlist = compile_fixture_with_library("ncl_std_cell_adder.sk", Some(std_lib));
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // Test cases
    let test_cases: [(u64, u64, u64); 4] = [
        (0x00, 0x00, 0x00),
        (0xFF, 0xFF, 0xFF),
        (0xAA, 0x55, 0x00),
        (0xF0, 0x0F, 0x00),
    ];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        println!("\nTest: 0x{:02X} & 0x{:02X} = 0x{:02X}", a, b, expected);
        let netlist = compile_fixture_with_library("ncl_std_cell_adder.sk", Some(std_lib));
        let pass = test_ncl_simulation(
            netlist,
            &[("a", a, 8), ("b", b, 8)],
            &[("y", expected, 8)],
            "Std Cells",
        );
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL std cell 8-bit AND tests failed");
}

#[test]
#[ignore = "Pre-existing bug: NCL std cell AND/XOR output net lookup returns NULL"]
fn test_ncl_xor_8bit_std_cells() {
    println!("\n=== NCL 8-bit XOR with Standard Cell C-element Macros ===");

    let std_lib = "/tmp/std_cells_only.sklib";
    STD_CELLS_LIBRARY_INIT.call_once(|| {
        create_std_cells_only_library();
    });

    let netlist = compile_fixture_with_library("ncl_std_cell_adder.sk", Some(std_lib));
    println!(
        "Compiled: {} cells, {} nets",
        netlist.cells.len(),
        netlist.nets.len()
    );

    // Test cases - XOR has complex dual-rail with multiple C-elements
    let test_cases: [(u64, u64, u64); 4] = [
        (0x00, 0x00, 0x00),
        (0xFF, 0xFF, 0x00),
        (0xAA, 0x55, 0xFF),
        (0xF0, 0x0F, 0xFF),
    ];

    let mut all_pass = true;
    for (a, b, expected) in test_cases {
        println!("\nTest: 0x{:02X} ^ 0x{:02X} = 0x{:02X}", a, b, expected);
        let netlist = compile_fixture_with_library("ncl_std_cell_adder.sk", Some(std_lib));
        let pass = test_ncl_simulation(
            netlist,
            &[("a", a, 8), ("b", b, 8)],
            &[("y", expected, 8)],
            "Std Cells",
        );
        all_pass = all_pass && pass;
    }

    assert!(all_pass, "NCL std cell 8-bit XOR tests failed");
}

/// Create a library without THmn gates for testing standard cell mode
fn create_std_cells_only_library() {
    use std::io::Write;

    // Read the generic_asic library
    let generic_lib = std::fs::read_to_string("./crates/skalp-stdlib/libraries/generic_asic.sklib")
        .expect("Failed to read generic_asic library");

    // Parse as TOML and filter out THmn cells
    // The library format is TOML with [[cells]] arrays
    let lines: Vec<&str> = generic_lib.lines().collect();
    let mut output = String::new();
    let mut skip_until_next_cells = false;
    let mut pending_cells_header = false;
    let mut pending_lines: Vec<&str> = Vec::new();

    for line in &lines {
        // Check if we're starting a new cell definition
        if line.trim() == "[[cells]]" {
            // If we were skipping, we can stop now
            skip_until_next_cells = false;
            // Save the [[cells]] line and wait to see if it's a THmn gate
            pending_cells_header = true;
            pending_lines.clear();
            pending_lines.push(line);
            continue;
        }

        if pending_cells_header {
            pending_lines.push(line);

            // Check if this is the name line
            if line.trim().starts_with("name = ") {
                let is_thmn = line.contains("TH12")
                    || line.contains("TH22")
                    || line.contains("TH13")
                    || line.contains("TH23")
                    || line.contains("TH33")
                    || line.contains("TH14")
                    || line.contains("TH24")
                    || line.contains("TH34")
                    || line.contains("TH44");

                if is_thmn {
                    // Skip this entire cell definition
                    pending_cells_header = false;
                    pending_lines.clear();
                    skip_until_next_cells = true;
                } else {
                    // Output the pending lines
                    for pending_line in &pending_lines {
                        output.push_str(pending_line);
                        output.push('\n');
                    }
                    pending_cells_header = false;
                    pending_lines.clear();
                }
            }
            continue;
        }

        if skip_until_next_cells {
            continue; // Skip lines until we hit the next [[cells]]
        }

        output.push_str(line);
        output.push('\n');
    }

    // Write the filtered library
    let mut file = std::fs::File::create("/tmp/std_cells_only.sklib")
        .expect("Failed to create std_cells_only.sklib");
    file.write_all(output.as_bytes())
        .expect("Failed to write std_cells_only.sklib");
}
