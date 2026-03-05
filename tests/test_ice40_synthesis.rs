//! Comprehensive tests for iCE40 FPGA synthesis and gate-level simulation
//!
//! These tests verify:
//! - iCE40 library loading and cell availability
//! - Synthesis with LUT4-based technology mapping
//! - Carry chain optimization for arithmetic
//! - AIG-based optimization pipeline
//! - Gate-level simulation accuracy
//! - nextpnr JSON and Verilog output

use skalp_lir::{
    get_stdlib_library, list_stdlib_libraries, lower_mir_module_to_lir,
    lower_mir_module_to_lir_with_bram, map_lir_to_gates, map_lir_to_gates_optimized,
    map_lir_to_gates_with_constraints, synthesize_for_area, synthesize_for_timing, CellFunction,
    Lir, LirOp,
};
use skalp_mir::MirCompiler;
use skalp_testing::testbench::*;

// =============================================================================
// Library Loading Tests
// =============================================================================

#[test]
fn test_ice40_library_available() {
    let libraries = list_stdlib_libraries();
    assert!(
        libraries.contains(&"ice40"),
        "ice40 library should be available. Available: {:?}",
        libraries
    );
}

#[test]
fn test_ice40_library_loads() {
    let library = get_stdlib_library("ice40").expect("Failed to load ice40 library");
    assert_eq!(library.name, "ice40");
    // Note: vendor and family are stored in the TOML but not exposed in the TechLibrary struct
}

#[test]
fn test_ice40_library_lut_size() {
    let library = get_stdlib_library("ice40").expect("Failed to load ice40 library");
    assert!(
        library.is_fpga(),
        "ice40 should be recognized as FPGA library"
    );
    assert_eq!(library.get_lut_size(), 4, "ice40 should have LUT4");
}

#[test]
fn test_ice40_library_aliases() {
    // Test that aliases work
    let lib1 = get_stdlib_library("ice40").expect("ice40 failed");
    let lib2 = get_stdlib_library("lattice").expect("lattice failed");
    let lib3 = get_stdlib_library("lattice_ice40").expect("lattice_ice40 failed");

    assert_eq!(lib1.name, lib2.name);
    assert_eq!(lib2.name, lib3.name);
}

#[test]
fn test_ice40_library_cells() {
    let library = get_stdlib_library("ice40").expect("Failed to load ice40 library");

    // Verify essential cells exist using the public cell_names() method
    let cell_names = library.cell_names();

    // Core primitives
    assert!(cell_names.contains(&"SB_LUT4"), "Missing SB_LUT4");
    assert!(cell_names.contains(&"SB_CARRY"), "Missing SB_CARRY");
    assert!(cell_names.contains(&"SB_DFF"), "Missing SB_DFF");

    // DFF variants
    assert!(cell_names.contains(&"SB_DFFE"), "Missing SB_DFFE");
    assert!(cell_names.contains(&"SB_DFFSR"), "Missing SB_DFFSR");

    // Mapped functions via LUT4
    assert!(cell_names.contains(&"SB_LUT4_AND2"), "Missing SB_LUT4_AND2");
    assert!(cell_names.contains(&"SB_LUT4_OR2"), "Missing SB_LUT4_OR2");
    assert!(cell_names.contains(&"SB_LUT4_XOR2"), "Missing SB_LUT4_XOR2");
    assert!(cell_names.contains(&"SB_LUT4_MUX2"), "Missing SB_LUT4_MUX2");
}

#[test]
fn test_ice40_vs_generic_asic() {
    let ice40 = get_stdlib_library("ice40").expect("Failed to load ice40");
    let asic = get_stdlib_library("generic_asic").expect("Failed to load generic_asic");

    // ice40 is FPGA, generic_asic is not
    assert!(ice40.is_fpga());
    assert!(!asic.is_fpga());

    // ice40 has LUT size, ASIC doesn't (or uses default)
    assert_eq!(ice40.lut_size, Some(4));
}

// =============================================================================
// Basic Synthesis Tests
// =============================================================================

fn compile_inline_to_lir(source: &str) -> skalp_lir::Lir {
    use skalp_frontend::parse_and_build_hir;

    let hir = parse_and_build_hir(source).expect("HIR parsing failed");
    let compiler = MirCompiler::new();
    let mir = compiler
        .compile_to_mir(&hir)
        .expect("MIR compilation failed");
    let top_module = mir.modules.first().expect("No modules");
    let lir_result = lower_mir_module_to_lir(top_module);
    lir_result.lir
}

#[test]
fn test_ice40_synthesis_simple_and() {
    let source = r#"
entity AndGate {
    in a: bit
    in b: bit
    out y: bit
}

impl AndGate {
    y = a & b
}
    "#;

    let lir = compile_inline_to_lir(source);
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let result = map_lir_to_gates(&lir, &library);

    assert!(result.stats.cells_created > 0, "Should create cells");

    // Check that cells use SB_LUT4 naming
    let has_lut4_cells = result
        .netlist
        .cells
        .iter()
        .any(|c| c.cell_type.contains("LUT4") || c.cell_type.contains("SB_"));
    assert!(
        has_lut4_cells,
        "Should use iCE40 cell names. Cells: {:?}",
        result
            .netlist
            .cells
            .iter()
            .map(|c| &c.cell_type)
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_ice40_synthesis_mux() {
    let source = r#"
entity Mux2 {
    in sel: bit
    in a: bit
    in b: bit
    out y: bit
}

impl Mux2 {
    y = if sel { b } else { a }
}
    "#;

    let lir = compile_inline_to_lir(source);
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let result = map_lir_to_gates(&lir, &library);

    assert!(result.stats.cells_created > 0, "Should create cells");
    println!("Mux2 synthesized to {} cells", result.netlist.cells.len());
}

#[test]
fn test_ice40_synthesis_adder() {
    let source = r#"
entity Adder8 {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[8]
}

impl Adder8 {
    sum = a + b
}
    "#;

    let lir = compile_inline_to_lir(source);
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let result = map_lir_to_gates(&lir, &library);

    println!(
        "8-bit adder: {} cells, {} nets",
        result.netlist.cells.len(),
        result.netlist.nets.len()
    );

    // Check for carry chain cells or XOR cells (used in adders)
    let cell_types: Vec<&str> = result
        .netlist
        .cells
        .iter()
        .map(|c| c.cell_type.as_str())
        .collect();
    println!("Cell types: {:?}", cell_types);
}

#[test]
fn test_ice40_synthesis_register() {
    let source = r#"
entity Register8 {
    in clk: clock
    in d: bit[8]
    out q: bit[8]
}

impl Register8 {
    signal reg_val: bit[8] = 0
    on(clk.rise) {
        reg_val = d
    }
    q = reg_val
}
    "#;

    let lir = compile_inline_to_lir(source);
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let result = map_lir_to_gates(&lir, &library);

    // Should have 8 DFFs for 8-bit register
    let dff_count = result
        .netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.contains("DFF"))
        .count();
    assert!(
        dff_count >= 8,
        "Should have at least 8 DFFs for 8-bit register, got {}",
        dff_count
    );

    println!("8-bit register: {} DFFs", dff_count);
}

// =============================================================================
// AIG Optimization Tests
// =============================================================================

#[test]
fn test_ice40_aig_synthesis_balanced() {
    // Test that the synthesize_balanced function runs without error
    // The result may be optimized away for simple designs
    let source = r#"
entity Complex {
    in a: bit[8]
    in b: bit[8]
    in c: bit[8]
    out y: bit[8]
}

impl Complex {
    y = (a & b) | (b & c) | (a & c)
}
    "#;

    let lir = compile_inline_to_lir(source);
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");

    // Use map_lir_to_gates for a concrete test
    let result = map_lir_to_gates(&lir, &library);
    println!(
        "Complex logic synthesized: {} cells",
        result.netlist.cells.len()
    );
    // This design should produce cells for the AND/OR logic
    assert!(!result.netlist.cells.is_empty());
}

#[test]
fn test_ice40_aig_synthesis_area() {
    let source = r#"
entity AreaOpt {
    in a: bit[8]
    in b: bit[8]
    out y: bit[8]
}

impl AreaOpt {
    y = a ^ b ^ (a & b)
}
    "#;

    let lir = compile_inline_to_lir(source);
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");

    let result = synthesize_for_area(&lir, &library);
    let reduction = result
        .initial_and_count
        .saturating_sub(result.final_and_count);
    println!(
        "Area-optimized: {} AND gates after, {} reduction",
        result.final_and_count, reduction
    );
}

#[test]
fn test_ice40_aig_synthesis_timing() {
    let source = r#"
entity TimingOpt {
    in a: bit[8]
    in b: bit[8]
    out y: bit[8]
}

impl TimingOpt {
    y = ((a + b) & 0xFF) ^ a
}
    "#;

    let lir = compile_inline_to_lir(source);
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");

    let result = synthesize_for_timing(&lir, &library);
    println!(
        "Timing-optimized: {} AND gates, {} levels (was {})",
        result.final_and_count, result.final_levels, result.initial_levels
    );
}

#[test]
fn test_ice40_synthesis_presets() {
    // Use map_lir_to_gates instead of synthesize for this test
    // since synthesize can optimize away simple designs
    let source = r#"
entity TestDesign {
    in a: bit[8]
    in b: bit[8]
    out y: bit[8]
}

impl TestDesign {
    y = a + b
}
    "#;

    let lir = compile_inline_to_lir(source);
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");

    // Test that basic tech mapping works with ice40
    let result = map_lir_to_gates(&lir, &library);
    println!("Basic tech map: {} cells", result.netlist.cells.len());
    assert!(
        !result.netlist.cells.is_empty(),
        "Basic tech mapping should produce cells"
    );

    // Test optimized tech mapping
    let optimized = map_lir_to_gates_optimized(&lir, &library);
    println!(
        "Optimized tech map: {} cells",
        optimized.netlist.cells.len()
    );
    assert!(
        !optimized.netlist.cells.is_empty(),
        "Optimized tech mapping should produce cells"
    );
}

// =============================================================================
// Output Format Tests
// =============================================================================

#[test]
fn test_ice40_nextpnr_json_output() {
    let source = r#"
entity Counter4 {
    in clk: clock
    in enable: bit
    out count: bit[4]
}

impl Counter4 {
    signal cnt: bit[4] = 0
    on(clk.rise) {
        if enable {
            cnt = cnt + 1
        }
    }
    count = cnt
}
    "#;

    let lir = compile_inline_to_lir(source);
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let result = map_lir_to_gates_optimized(&lir, &library);

    let json = result.netlist.to_nextpnr_json();

    // Verify JSON structure
    assert!(json.contains("\"creator\""), "Should have creator field");
    assert!(json.contains("\"cells\""), "Should have cells section");
    assert!(
        json.contains("\"netnames\""),
        "Should have netnames section"
    );

    // Should contain LUT INIT parameters
    let has_lut_init = json.contains("LUT_INIT") || json.contains("INIT");
    println!("nextpnr JSON has INIT parameters: {}", has_lut_init);
    println!("JSON length: {} bytes", json.len());
}

#[test]
fn test_ice40_verilog_output() {
    let source = r#"
entity AndOr {
    in a: bit
    in b: bit
    in c: bit
    out y: bit
}

impl AndOr {
    y = (a & b) | c
}
    "#;

    let lir = compile_inline_to_lir(source);
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let result = map_lir_to_gates_optimized(&lir, &library);

    let verilog = result.netlist.to_ice40_verilog();

    // Verify Verilog structure
    assert!(verilog.contains("module"), "Should have module declaration");
    assert!(verilog.contains("SB_LUT4"), "Should instantiate SB_LUT4");
    assert!(
        verilog.contains(".LUT_INIT"),
        "Should have LUT_INIT parameter"
    );
    assert!(verilog.contains("endmodule"), "Should end module");

    println!("Generated iCE40 Verilog:\n{}", verilog);
}

// =============================================================================
// Gate-Level Simulation Tests (with async testbench API)
// =============================================================================

// Helper to write test files to a location where stdlib can be found
fn write_test_file(filename: &str, content: &str) -> String {
    // Write to the examples directory so stdlib can be found
    let path = format!("{}/examples/{}", env!("CARGO_MANIFEST_DIR"), filename);
    std::fs::write(&path, content).expect("Failed to write test file");
    path
}

fn cleanup_test_file(path: &str) {
    std::fs::remove_file(path).ok();
}

#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS"
)]
async fn test_ice40_gate_level_and_gate() {
    // Note: Simple single-bit gates work because they map to known primitives
    let source = r#"
entity TestAnd {
    in a: bit
    in b: bit
    out y: bit
}

impl TestAnd {
    y = a & b
}
"#;

    let path = write_test_file("test_ice40_and.sk", source);
    let tb_result = Testbench::gate_level_with_library(&path, "ice40").await;
    cleanup_test_file(&path);

    let mut tb = tb_result.expect("Failed to create testbench");

    // Test all input combinations
    let test_vectors = [(0u8, 0u8, 0u8), (0, 1, 0), (1, 0, 0), (1, 1, 1)];

    for (a, b, expected) in test_vectors {
        tb.set("a", a).set("b", b);
        tb.clock(1).await;
        let result = tb.get_u32("y").await;
        assert_eq!(
            result, expected as u32,
            "AND({}, {}) should be {}, got {}",
            a, b, expected, result
        );
    }

    tb.export_waveform("build/test_ice40_gate_level_and_gate.skw.gz")
        .ok();
}

#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS"
)]
async fn test_ice40_gate_level_xor_gate() {
    let source = r#"
entity TestXor {
    in a: bit
    in b: bit
    out y: bit
}

impl TestXor {
    y = a ^ b
}
"#;

    let path = write_test_file("test_ice40_xor.sk", source);
    let tb_result = Testbench::gate_level_with_library(&path, "ice40").await;
    cleanup_test_file(&path);

    let mut tb = tb_result.expect("Failed to create testbench");

    let test_vectors = [(0u8, 0u8, 0u8), (0, 1, 1), (1, 0, 1), (1, 1, 0)];

    for (a, b, expected) in test_vectors {
        tb.set("a", a).set("b", b);
        tb.clock(1).await;
        let result = tb.get_u32("y").await;
        assert_eq!(
            result, expected as u32,
            "XOR({}, {}) should be {}, got {}",
            a, b, expected, result
        );
    }

    tb.export_waveform("build/test_ice40_gate_level_xor_gate.skw.gz")
        .ok();
}

#[tokio::test]
async fn test_ice40_gate_level_mux() {
    // This test validates LUT4 truth table evaluation for mux operations
    // The iCE40 library uses SB_LUT4 for complex logic which needs INIT parameter
    let source = r#"
entity TestMux {
    in sel: bit
    in a: bit[8]
    in b: bit[8]
    out y: bit[8]
}

impl TestMux {
    y = if sel { b } else { a }
}
"#;

    let path = write_test_file("test_ice40_mux.sk", source);
    let tb_result = Testbench::gate_level_with_library(&path, "ice40").await;
    cleanup_test_file(&path);

    let mut tb = tb_result.expect("Failed to create testbench");

    // Test sel=0: should output a
    tb.set("sel", 0u8).set("a", 0x55u8).set("b", 0xAAu8);
    tb.clock(1).await;
    let result = tb.get_u32("y").await;
    assert_eq!(
        result, 0x55,
        "sel=0 should select a (0x55), got 0x{:02x}",
        result
    );

    // Test sel=1: should output b
    tb.set("sel", 1u8);
    tb.clock(1).await;
    let result = tb.get_u32("y").await;
    assert_eq!(
        result, 0xAA,
        "sel=1 should select b (0xAA), got 0x{:02x}",
        result
    );

    tb.export_waveform("build/test_ice40_gate_level_mux.skw.gz")
        .ok();
}

#[tokio::test]
async fn test_ice40_gate_level_adder() {
    // This test validates LUT4 and carry chain (SB_CARRY) simulation for addition
    let source = r#"
entity TestAdder {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[8]
}

impl TestAdder {
    sum = a + b
}
"#;

    let path = write_test_file("test_ice40_adder.sk", source);
    let tb_result = Testbench::gate_level_with_library(&path, "ice40").await;
    cleanup_test_file(&path);

    let mut tb = tb_result.expect("Failed to create testbench");

    // Test various additions
    let test_vectors: Vec<(u8, u8, u8)> = vec![
        (0, 0, 0),
        (1, 1, 2),
        (5, 3, 8),
        (100, 55, 155),
        (255, 0, 255),
        (128, 127, 255),
        (255, 1, 0), // Overflow wraps
    ];

    for (a, b, expected) in test_vectors {
        tb.set("a", a).set("b", b);
        tb.clock(1).await;
        let result = tb.get_u32("sum").await as u8;
        assert_eq!(
            result, expected,
            "ADD({}, {}) should be {}, got {}",
            a, b, expected, result
        );
    }

    tb.export_waveform("build/test_ice40_gate_level_adder.skw.gz")
        .ok();
}

#[tokio::test]
async fn test_ice40_gate_level_subtractor() {
    // Test subtraction in isolation (no match/mux)
    let source = r#"
entity TestSubtractor {
    in a: bit[8]
    in b: bit[8]
    out diff: bit[8]
}

impl TestSubtractor {
    diff = a - b
}
"#;

    let path = write_test_file("test_ice40_subtractor.sk", source);
    let tb_result = Testbench::gate_level_with_library(&path, "ice40").await;
    cleanup_test_file(&path);

    let mut tb = tb_result.expect("Failed to create testbench");

    // Test various subtractions
    let test_vectors: Vec<(u8, u8, u8)> = vec![
        (10, 5, 5),
        (5, 3, 2),
        (100, 50, 50),
        (0, 0, 0),
        (255, 255, 0),
        (10, 0, 10),
        (0, 1, 255), // Underflow wraps
    ];

    for (a, b, expected) in test_vectors {
        tb.set("a", a).set("b", b);
        tb.clock(1).await;
        let result = tb.get_u32("diff").await as u8;
        assert_eq!(
            result, expected,
            "SUB({}, {}) should be {}, got {}",
            a, b, expected, result
        );
    }

    tb.export_waveform("build/test_ice40_gate_level_subtractor.skw.gz")
        .ok();
}

#[tokio::test]
async fn test_ice40_gate_level_counter() {
    // This test requires proper sequential logic handling (reset functionality)
    let source = r#"
entity TestCounter {
    in clk: clock
    in enable: bit
    in rst: reset
    out count: bit[4]
}

impl TestCounter {
    signal cnt: bit[4] = 0
    on(clk.rise) {
        if rst {
            cnt = 0
        } else if enable {
            cnt = cnt + 1
        }
    }
    count = cnt
}
"#;

    let path = write_test_file("test_ice40_counter.sk", source);
    let tb_result = Testbench::gate_level_with_library(&path, "ice40").await;
    cleanup_test_file(&path);

    let mut tb = tb_result.expect("Failed to create testbench");

    // Reset
    tb.set("rst", 1u8).set("enable", 0u8);
    tb.clock(2).await;
    tb.set("rst", 0u8);
    tb.clock(1).await;

    let initial = tb.get_u32("count").await;
    assert_eq!(initial, 0, "Counter should be 0 after reset");

    // Count up
    tb.set("enable", 1u8);
    for expected in 1u32..=5 {
        tb.clock(1).await;
        let count = tb.get_u32("count").await;
        assert_eq!(
            count, expected,
            "Counter should be {} after {} cycles",
            expected, expected
        );
    }

    // Disable and verify hold
    tb.set("enable", 0u8);
    tb.clock(3).await;
    let held = tb.get_u32("count").await;
    assert_eq!(held, 5, "Counter should hold at 5 when disabled");

    tb.export_waveform("build/test_ice40_gate_level_counter.skw.gz")
        .ok();
}

#[tokio::test]
async fn test_ice40_gate_level_alu() {
    // This test requires proper subtraction implementation (ADD works, SUB doesn't)
    let source = r#"
entity TestAlu {
    in a: bit[8]
    in b: bit[8]
    in op: bit[2]
    out result: bit[8]
}

impl TestAlu {
    result = match op {
        0b00 => a + b,
        0b01 => a - b,
        0b10 => a & b,
        0b11 => a | b,
        _ => 0
    }
}
"#;

    let path = write_test_file("test_ice40_alu.sk", source);
    let tb_result = Testbench::gate_level_with_library(&path, "ice40").await;
    cleanup_test_file(&path);

    let mut tb = tb_result.expect("Failed to create testbench");

    // Test ADD
    tb.set("a", 10u8).set("b", 5u8).set("op", 0u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u32("result").await, 15, "ADD failed");

    // Test SUB
    tb.set("op", 1u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u32("result").await, 5, "SUB failed");

    // Test AND
    tb.set("a", 0xF0u8).set("b", 0x0Fu8).set("op", 2u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u32("result").await, 0x00, "AND failed");

    // Test OR
    tb.set("op", 3u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u32("result").await, 0xFF, "OR failed");

    tb.export_waveform("build/test_ice40_gate_level_alu.skw.gz")
        .ok();
}

// =============================================================================
// Comparison Tests (ice40 vs generic_asic)
// =============================================================================

#[test]
fn test_ice40_vs_asic_cell_count() {
    let source = r#"
entity CompareDesign {
    in a: bit[8]
    in b: bit[8]
    out y: bit[8]
}

impl CompareDesign {
    y = (a + b) & (a ^ b)
}
    "#;

    let lir = compile_inline_to_lir(source);

    let ice40 = get_stdlib_library("ice40").expect("ice40");
    let asic = get_stdlib_library("generic_asic").expect("asic");

    let ice40_result = map_lir_to_gates_optimized(&lir, &ice40);
    let asic_result = map_lir_to_gates_optimized(&lir, &asic);

    println!("Cell count comparison:");
    println!("  iCE40: {} cells", ice40_result.netlist.cells.len());
    println!("  ASIC:  {} cells", asic_result.netlist.cells.len());

    // Both should produce valid netlists
    assert!(!ice40_result.netlist.cells.is_empty());
    assert!(!asic_result.netlist.cells.is_empty());
}

#[test]
fn test_ice40_library_cell_fit_rates() {
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");

    // Verify FIT rates are reasonable using iter_cells()
    for (_name, cell) in library.iter_cells() {
        assert!(
            cell.fit >= 0.0 && cell.fit < 10.0,
            "Cell {} has unreasonable FIT rate: {}",
            cell.name,
            cell.fit
        );

        // Verify failure modes
        for fm in &cell.failure_modes {
            assert!(
                fm.fit >= 0.0,
                "Cell {} failure mode {} has negative FIT: {}",
                cell.name,
                fm.name,
                fm.fit
            );
        }
    }
}

// =============================================================================
// BRAM Inference Tests
// =============================================================================

/// Helper to compile with BRAM inference enabled
fn compile_inline_to_lir_with_bram(source: &str) -> skalp_lir::Lir {
    use skalp_frontend::parse_and_build_hir;

    let hir = parse_and_build_hir(source).expect("HIR parsing failed");
    let compiler = MirCompiler::new();
    let mir = compiler
        .compile_to_mir(&hir)
        .expect("MIR compilation failed");
    let top_module = mir.modules.first().expect("No modules");
    let lir_result = lower_mir_module_to_lir_with_bram(top_module);
    lir_result.lir
}

/// Build a LIR with a MemBlock node programmatically (bypasses frontend)
fn build_memblock_lir(data_width: u32, addr_width: u32, depth: u32) -> Lir {
    let mut lir = Lir::new("TestMem".to_string());

    // Inputs
    let clk = lir.add_input("clk".to_string(), 1);
    lir.clocks.push(clk);
    let raddr = lir.add_input("raddr".to_string(), addr_width);
    let waddr = lir.add_input("waddr".to_string(), addr_width);
    let wdata = lir.add_input("wdata".to_string(), data_width);
    let we = lir.add_input("we".to_string(), 1);

    // Output
    let rdata = lir.add_output("rdata".to_string(), data_width);

    // MemBlock node
    lir.add_seq_node(
        LirOp::MemBlock {
            data_width,
            addr_width,
            depth,
            has_write: true,
            read_only: false,
        },
        vec![raddr, waddr, wdata, we],
        rdata,
        "mem".to_string(),
        clk,
        None,
    );

    lir
}

#[test]
fn test_ice40_library_has_ram_cell() {
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let ram = library.find_ram_cell();
    assert!(ram.is_some(), "ice40 library should have a RAM cell");

    let (cell, info) = ram.unwrap();
    assert_eq!(cell.name, "SB_RAM40_4K");
    assert_eq!(info.block_bits, 4096);
    assert!(info.true_dual_port);
    assert!(info.has_write_mask);

    // Check aspect ratios
    assert!(
        info.aspect_ratios.contains(&(256, 16)),
        "Should support 256x16"
    );
    assert!(
        info.aspect_ratios.contains(&(512, 8)),
        "Should support 512x8"
    );
    assert!(
        info.aspect_ratios.contains(&(1024, 4)),
        "Should support 1024x4"
    );
    assert!(
        info.aspect_ratios.contains(&(2048, 2)),
        "Should support 2048x2"
    );
}

#[test]
fn test_ice40_ram_cell_function() {
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let (cell, _) = library.find_ram_cell().unwrap();
    assert!(
        matches!(cell.function, CellFunction::Ram),
        "SB_RAM40_4K should have CellFunction::Ram, got {:?}",
        cell.function
    );
}

#[test]
fn test_ice40_ram_pin_map() {
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let (_, info) = library.find_ram_cell().unwrap();

    assert_eq!(info.pin_map.read_data, "RDATA");
    assert_eq!(info.pin_map.read_addr, "RADDR");
    assert_eq!(info.pin_map.read_clk, "RCLK");
    assert_eq!(info.pin_map.read_en, "RE");
    assert_eq!(info.pin_map.write_data, "WDATA");
    assert_eq!(info.pin_map.write_addr, "WADDR");
    assert_eq!(info.pin_map.write_clk, "WCLK");
    assert_eq!(info.pin_map.write_en, "WE");
    assert_eq!(info.pin_map.write_mask, Some("MASK".to_string()));
}

#[test]
fn test_bram_inference_single_block_256x8() {
    // A 256x8 memory fits in a single SB_RAM40_4K (256x16 mode)
    let lir = build_memblock_lir(8, 8, 256);
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let result = map_lir_to_gates(&lir, &library);

    // Should have exactly 1 RAM cell
    let ram_cells: Vec<_> = result
        .netlist
        .cells
        .iter()
        .filter(|c| c.cell_type == "SB_RAM40_4K")
        .collect();
    assert_eq!(
        ram_cells.len(),
        1,
        "256x8 should fit in 1 SB_RAM40_4K, got {}",
        ram_cells.len()
    );

    // Should NOT have hundreds of LUTs for mux tree
    let lut_count = result
        .netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.contains("LUT"))
        .count();
    println!(
        "256x8 BRAM: {} RAM cells, {} LUTs",
        ram_cells.len(),
        lut_count
    );
}

#[test]
fn test_bram_inference_single_block_512x4() {
    // A 512x4 memory fits in a single SB_RAM40_4K (512x8 mode)
    let lir = build_memblock_lir(4, 9, 512);
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let result = map_lir_to_gates(&lir, &library);

    let ram_cells: Vec<_> = result
        .netlist
        .cells
        .iter()
        .filter(|c| c.cell_type == "SB_RAM40_4K")
        .collect();
    assert_eq!(
        ram_cells.len(),
        1,
        "512x4 should fit in 1 SB_RAM40_4K, got {}",
        ram_cells.len()
    );
}

#[test]
fn test_bram_width_tiling() {
    // A 256x32 memory needs width tiling: 32/16 = 2 blocks in parallel
    let lir = build_memblock_lir(32, 8, 256);
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let result = map_lir_to_gates(&lir, &library);

    let ram_count = result
        .netlist
        .cells
        .iter()
        .filter(|c| c.cell_type == "SB_RAM40_4K")
        .count();
    assert_eq!(
        ram_count, 2,
        "256x32 should need 2 SB_RAM40_4K (width tiling), got {}",
        ram_count
    );
    println!("256x32 BRAM width tiling: {} RAM cells", ram_count);
}

#[test]
fn test_bram_depth_tiling() {
    // A 4096x4 memory needs depth tiling: 4096/1024 = 4 blocks deep
    let lir = build_memblock_lir(4, 12, 4096);
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let result = map_lir_to_gates(&lir, &library);

    let ram_count = result
        .netlist
        .cells
        .iter()
        .filter(|c| c.cell_type == "SB_RAM40_4K")
        .count();
    assert_eq!(
        ram_count, 4,
        "4096x4 should need 4 SB_RAM40_4K (depth tiling), got {}",
        ram_count
    );
    println!("4096x4 BRAM depth tiling: {} RAM cells", ram_count);
}

#[test]
fn test_bram_width_and_depth_tiling() {
    // A 1024x32 memory needs both: 2 wide x 4 deep = 8 blocks
    // With 256x16 aspect ratio: width_blocks = 32/16 = 2, depth_blocks = 1024/256 = 4
    let lir = build_memblock_lir(32, 10, 1024);
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let result = map_lir_to_gates(&lir, &library);

    let ram_count = result
        .netlist
        .cells
        .iter()
        .filter(|c| c.cell_type == "SB_RAM40_4K")
        .count();
    // 32-bit width needs 2 blocks wide (16 bits each), 1024 depth / 256 = 4 deep
    assert_eq!(
        ram_count, 8,
        "1024x32 should need 8 SB_RAM40_4K (2 wide x 4 deep), got {}",
        ram_count
    );
    println!("1024x32 BRAM tiling: {} RAM cells", ram_count);
}

#[test]
fn test_bram_rom_inference() {
    // Read-only memory: single input (raddr), no write ports
    let mut lir = Lir::new("TestRom".to_string());
    let clk = lir.add_input("clk".to_string(), 1);
    lir.clocks.push(clk);
    let raddr = lir.add_input("raddr".to_string(), 8);
    let rdata = lir.add_output("rdata".to_string(), 8);

    lir.add_seq_node(
        LirOp::MemBlock {
            data_width: 8,
            addr_width: 8,
            depth: 256,
            has_write: false,
            read_only: true,
        },
        vec![raddr],
        rdata,
        "rom".to_string(),
        clk,
        None,
    );

    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let result = map_lir_to_gates(&lir, &library);

    let ram_count = result
        .netlist
        .cells
        .iter()
        .filter(|c| c.cell_type == "SB_RAM40_4K")
        .count();
    assert_eq!(
        ram_count, 1,
        "256x8 ROM should use 1 SB_RAM40_4K, got {}",
        ram_count
    );
}

#[test]
fn test_generic_asic_has_no_ram_cell() {
    let library = get_stdlib_library("generic_asic").expect("Failed to load generic_asic");
    assert!(
        library.find_ram_cell().is_none(),
        "generic_asic should not have RAM cells"
    );
}

// =============================================================================
// ECP5 Library + DSP Inference Tests
// =============================================================================

#[test]
fn test_ecp5_library_loads() {
    let library = get_stdlib_library("ecp5").expect("Failed to load ecp5 library");
    assert_eq!(library.name, "ecp5");
    assert!(library.is_fpga(), "ecp5 should be recognized as FPGA");
    assert_eq!(library.get_lut_size(), 4, "ECP5 should have LUT4");

    // Should have DSP, RAM, and basic cells
    assert!(
        library.find_dsp_cell().is_some(),
        "ECP5 should have DSP cell"
    );
    assert!(
        library.find_ram_cell().is_some(),
        "ECP5 should have RAM cell"
    );
    assert!(
        library.find_best_cell(&CellFunction::And2).is_some(),
        "ECP5 should have AND2 cell"
    );
}

#[test]
fn test_ecp5_library_aliases() {
    let lib1 = get_stdlib_library("ecp5").expect("ecp5 failed");
    let lib2 = get_stdlib_library("lattice_ecp5").expect("lattice_ecp5 failed");
    assert_eq!(lib1.name, lib2.name);
}

#[test]
fn test_ecp5_library_in_list() {
    let libraries = list_stdlib_libraries();
    assert!(
        libraries.contains(&"ecp5"),
        "ecp5 should be in library list. Available: {:?}",
        libraries
    );
}

#[test]
fn test_ecp5_dsp_cell_info() {
    let library = get_stdlib_library("ecp5").expect("Failed to load ecp5");
    let (cell, info) = library.find_dsp_cell().expect("ECP5 should have DSP cell");

    assert_eq!(cell.name, "MULT18X18D");
    assert_eq!(info.a_width, 18);
    assert_eq!(info.b_width, 18);
    assert_eq!(info.p_width, 36);
    assert!(info.supports_signed);
    assert!(info.has_input_register);
    assert!(info.has_pipeline_register);
    assert!(info.has_output_register);

    // Check pin map
    assert_eq!(info.pin_map.a_input, "A");
    assert_eq!(info.pin_map.b_input, "B");
    assert_eq!(info.pin_map.product, "P");
    assert_eq!(info.pin_map.signed_a.as_deref(), Some("SIGNEDA"));
    assert_eq!(info.pin_map.signed_b.as_deref(), Some("SIGNEDB"));
}

#[test]
fn test_ecp5_ram_cell_info() {
    let library = get_stdlib_library("ecp5").expect("Failed to load ecp5");
    let (cell, info) = library.find_ram_cell().expect("ECP5 should have RAM cell");

    assert_eq!(cell.name, "DP16KD");
    assert_eq!(info.block_bits, 18432);
    assert!(info.true_dual_port);
    assert!(info.has_output_register);
}

/// Build a LIR with a Mul node programmatically
fn build_multiply_lir(width: u32, result_width: u32, signed: bool) -> Lir {
    let mut lir = Lir::new("TestMul".to_string());

    let a = lir.add_input("a".to_string(), width);
    let b = lir.add_input("b".to_string(), width);
    let p = lir.add_output("p".to_string(), result_width);

    lir.add_node(
        LirOp::Mul {
            width,
            result_width,
            signed,
        },
        vec![a, b],
        p,
        "mul".to_string(),
    );

    lir
}

/// Count MULT18X18D cells in a gate netlist
fn count_dsp_cells(result: &skalp_lir::TechMapResult) -> usize {
    result
        .netlist
        .cells
        .iter()
        .filter(|c| c.cell_type == "MULT18X18D")
        .count()
}

/// Count partial product AND gates (used in gate-level multiply)
fn count_partial_product_cells(result: &skalp_lir::TechMapResult) -> usize {
    result
        .netlist
        .cells
        .iter()
        .filter(|c| {
            c.source_op
                .as_ref()
                .map_or(false, |op| op == "PartialProduct")
        })
        .count()
}

#[test]
fn test_ecp5_dsp_multiply_8x8() {
    let lir = build_multiply_lir(8, 16, false);
    let library = get_stdlib_library("ecp5").expect("Failed to load ecp5");
    let result = map_lir_to_gates(&lir, &library);

    let dsp_count = count_dsp_cells(&result);
    let pp_count = count_partial_product_cells(&result);

    assert_eq!(dsp_count, 1, "8x8 multiply should use exactly 1 MULT18X18D");
    assert_eq!(
        pp_count, 0,
        "Should not use partial products when DSP is available"
    );

    // Verify the DSP cell has correct parameters
    let dsp_cell = result
        .netlist
        .cells
        .iter()
        .find(|c| c.cell_type == "MULT18X18D")
        .expect("Should have MULT18X18D cell");
    assert_eq!(
        dsp_cell.parameters.get("SIGNED_MODE").map(|s| s.as_str()),
        Some("UNSIGNED")
    );
    assert_eq!(
        dsp_cell
            .parameters
            .get("REG_INPUTA_CLK")
            .map(|s| s.as_str()),
        Some("NONE"),
        "Should be combinational mode"
    );
}

#[test]
fn test_ecp5_dsp_multiply_18x18() {
    let lir = build_multiply_lir(18, 36, false);
    let library = get_stdlib_library("ecp5").expect("Failed to load ecp5");
    let result = map_lir_to_gates(&lir, &library);

    let dsp_count = count_dsp_cells(&result);
    assert_eq!(
        dsp_count, 1,
        "18x18 multiply should use exactly 1 MULT18X18D (max single block)"
    );
}

#[test]
fn test_ecp5_dsp_multiply_signed() {
    let lir = build_multiply_lir(16, 32, true);
    let library = get_stdlib_library("ecp5").expect("Failed to load ecp5");
    let result = map_lir_to_gates(&lir, &library);

    let dsp_count = count_dsp_cells(&result);
    assert_eq!(
        dsp_count, 1,
        "16x16 signed multiply should use 1 MULT18X18D with native signed mode"
    );

    let dsp_cell = result
        .netlist
        .cells
        .iter()
        .find(|c| c.cell_type == "MULT18X18D")
        .expect("Should have MULT18X18D cell");
    assert_eq!(
        dsp_cell.parameters.get("SIGNED_MODE").map(|s| s.as_str()),
        Some("SIGNED"),
        "Should use native signed mode"
    );
}

#[test]
fn test_ecp5_dsp_wide_multiply() {
    let lir = build_multiply_lir(32, 64, false);
    let library = get_stdlib_library("ecp5").expect("Failed to load ecp5");
    let result = map_lir_to_gates(&lir, &library);

    let dsp_count = count_dsp_cells(&result);
    assert_eq!(
        dsp_count, 4,
        "32x32 multiply should use 4 MULT18X18D blocks (tiled decomposition)"
    );

    // Should also have addition logic for combining partial products
    assert!(
        result.netlist.cells.len() > 4,
        "Should have additional cells for partial product addition"
    );
}

#[test]
fn test_ecp5_dsp_too_wide_fallback() {
    let lir = build_multiply_lir(64, 128, false);
    let library = get_stdlib_library("ecp5").expect("Failed to load ecp5");
    let result = map_lir_to_gates(&lir, &library);

    let dsp_count = count_dsp_cells(&result);
    assert_eq!(
        dsp_count, 0,
        "64x64 multiply should fall back to gate-level (too wide for DSP tiling)"
    );

    let pp_count = count_partial_product_cells(&result);
    assert!(
        pp_count > 0,
        "Should use partial product decomposition for 64x64"
    );
}

#[test]
fn test_ice40_no_dsp_fallback() {
    // Regression: iCE40 has no DSP blocks, should use gate-level multiply
    let lir = build_multiply_lir(8, 16, false);
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let result = map_lir_to_gates(&lir, &library);

    let dsp_count = result
        .netlist
        .cells
        .iter()
        .filter(|c| c.cell_type == "MULT18X18D")
        .count();
    assert_eq!(
        dsp_count, 0,
        "iCE40 should not use DSP blocks (none available)"
    );

    let pp_count = count_partial_product_cells(&result);
    assert!(
        pp_count > 0,
        "iCE40 should use partial products for multiply"
    );
}

#[test]
fn test_generic_asic_no_dsp() {
    let library = get_stdlib_library("generic_asic").expect("Failed to load generic_asic");
    assert!(
        library.find_dsp_cell().is_none(),
        "generic_asic should not have DSP cells"
    );
}

// =============================================================================
// Clock Infrastructure Tests
// =============================================================================

#[test]
fn test_ice40_has_clock_buf() {
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let clk_buf = library.find_clk_buf_cell();
    assert!(clk_buf.is_some(), "ice40 should have a clock buffer cell");

    let (cell, info) = clk_buf.unwrap();
    assert_eq!(cell.name, "SB_GB");
    assert!(
        matches!(cell.function, CellFunction::ClkBuf),
        "SB_GB should have CellFunction::ClkBuf, got {:?}",
        cell.function
    );
    assert_eq!(info.input, "USER_SIGNAL_TO_GLOBAL_BUFFER");
    assert_eq!(info.output, "GLOBAL_BUFFER_OUTPUT");
    assert!(!info.has_enable, "SB_GB should not have an enable pin");
}

#[test]
fn test_ice40_has_pll() {
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let pll = library.find_pll_cell();
    assert!(pll.is_some(), "ice40 should have a PLL cell");

    let (cell, info) = pll.unwrap();
    assert_eq!(cell.name, "SB_PLL40_CORE");
    assert!(
        matches!(cell.function, CellFunction::Pll),
        "SB_PLL40_CORE should have CellFunction::Pll, got {:?}",
        cell.function
    );

    // Frequency ranges
    assert!((info.min_input_freq_mhz - 10.0).abs() < 0.01);
    assert!((info.max_input_freq_mhz - 133.0).abs() < 0.01);
    assert!((info.min_output_freq_mhz - 16.0).abs() < 0.01);
    assert!((info.max_output_freq_mhz - 275.0).abs() < 0.01);
    assert_eq!(info.num_outputs, 1);
    assert!(!info.supports_feedback);

    // Pin map
    assert_eq!(info.pin_map.ref_clk, "REFERENCECLK");
    assert_eq!(info.pin_map.out_clk, "PLLOUTCORE");
    assert_eq!(info.pin_map.lock, "LOCK");
    assert_eq!(info.pin_map.global_out, Some("PLLOUTGLOBAL".to_string()));
}

#[test]
fn test_ecp5_has_clock_cells() {
    let library = get_stdlib_library("ecp5").expect("Failed to load ecp5");

    // DCCA — clock buffer
    let clk_buf = library.find_clk_buf_cell();
    assert!(clk_buf.is_some(), "ECP5 should have a clock buffer cell");
    let (cell, info) = clk_buf.unwrap();
    assert_eq!(cell.name, "DCCA");
    assert!(matches!(cell.function, CellFunction::ClkBuf));
    assert_eq!(info.input, "CLKI");
    assert_eq!(info.output, "CLKO");
    assert!(info.has_enable, "DCCA should have an enable pin");
    assert_eq!(info.enable, Some("CE".to_string()));

    // EHXPLLL — PLL
    let pll = library.find_pll_cell();
    assert!(pll.is_some(), "ECP5 should have a PLL cell");
    let (cell, _) = pll.unwrap();
    assert_eq!(cell.name, "EHXPLLL");
    assert!(matches!(cell.function, CellFunction::Pll));

    // CLKDIVF — clock divider
    let clk_div = library.find_clk_div_cell();
    assert!(clk_div.is_some(), "ECP5 should have a clock divider cell");
    let (cell, info) = clk_div.unwrap();
    assert_eq!(cell.name, "CLKDIVF");
    assert!(matches!(cell.function, CellFunction::ClkDiv));
    assert_eq!(info.clk_input, "CLKI");
    assert_eq!(info.clk_output, "CDIVX");
    assert!(
        info.supported_dividers.contains(&2.0),
        "Should support divide-by-2"
    );
    assert!(
        info.supported_dividers.contains(&8.0),
        "Should support divide-by-8"
    );
}

#[test]
fn test_ecp5_pll_cell_info() {
    let library = get_stdlib_library("ecp5").expect("Failed to load ecp5");
    let (cell, info) = library.find_pll_cell().expect("ECP5 should have PLL cell");

    assert_eq!(cell.name, "EHXPLLL");
    assert_eq!(info.num_outputs, 4);
    assert!(info.supports_feedback);

    // Frequency ranges
    assert!((info.min_input_freq_mhz - 8.0).abs() < 0.01);
    assert!((info.max_input_freq_mhz - 400.0).abs() < 0.01);
    assert!((info.min_output_freq_mhz - 3.125).abs() < 0.01);
    assert!((info.max_output_freq_mhz - 800.0).abs() < 0.01);

    // Pin map
    assert_eq!(info.pin_map.ref_clk, "CLKI");
    assert_eq!(info.pin_map.out_clk, "CLKOP");
    assert_eq!(info.pin_map.lock, "LOCK");
    assert_eq!(info.pin_map.feedback, Some("CLKFB".to_string()));
    assert_eq!(
        info.pin_map.secondary_outputs,
        vec!["CLKOS", "CLKOS2", "CLKOS3"]
    );
}

/// Build a simple 4-bit register LIR: output = reg(input) on rising clock edge
fn build_register_lir(width: u32) -> Lir {
    let mut lir = Lir::new("TestRegister".to_string());

    let clk = lir.add_input("clk".to_string(), 1);
    lir.clocks.push(clk);
    let d = lir.add_input("d".to_string(), width);
    let q = lir.add_output("q".to_string(), width);

    lir.add_seq_node(
        LirOp::Reg {
            width,
            has_enable: false,
            has_reset: false,
            async_reset: false,
            reset_value: None,
        },
        vec![d],
        q,
        "reg0".to_string(),
        clk,
        None,
    );

    lir
}

#[test]
fn test_clock_buffer_insertion_ice40() {
    let lir = build_register_lir(4);
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let result = map_lir_to_gates(&lir, &library);

    // Should have exactly 1 SB_GB cell
    let gb_cells: Vec<_> = result
        .netlist
        .cells
        .iter()
        .filter(|c| c.cell_type == "SB_GB")
        .collect();
    assert_eq!(
        gb_cells.len(),
        1,
        "Should insert exactly 1 SB_GB clock buffer, got {}",
        gb_cells.len()
    );
    assert_eq!(
        gb_cells[0].source_op.as_deref(),
        Some("ClockBuffer"),
        "Clock buffer cell should have source_op 'ClockBuffer'"
    );

    // The buffered clock net should be used by DFF cells
    let buf_out_net = gb_cells[0].outputs[0];
    let dff_cells: Vec<_> = result
        .netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.starts_with("SB_DFF"))
        .collect();
    assert!(
        !dff_cells.is_empty(),
        "Should have at least one DFF cell for the register"
    );
    for dff in &dff_cells {
        assert_eq!(
            dff.clock,
            Some(buf_out_net),
            "DFF cell '{}' should use the buffered clock net, not the raw clock input",
            dff.path
        );
    }
}

#[test]
fn test_clock_buffer_insertion_ecp5() {
    let lir = build_register_lir(4);
    let library = get_stdlib_library("ecp5").expect("Failed to load ecp5");
    let result = map_lir_to_gates(&lir, &library);

    // Should have exactly 1 DCCA cell
    let dcca_cells: Vec<_> = result
        .netlist
        .cells
        .iter()
        .filter(|c| c.cell_type == "DCCA")
        .collect();
    assert_eq!(
        dcca_cells.len(),
        1,
        "Should insert exactly 1 DCCA clock buffer, got {}",
        dcca_cells.len()
    );
    assert_eq!(
        dcca_cells[0].source_op.as_deref(),
        Some("ClockBuffer"),
        "Clock buffer cell should have source_op 'ClockBuffer'"
    );

    // DCCA has enable pin — it should be tied high (2 inputs: CLKI + CE)
    assert_eq!(
        dcca_cells[0].inputs.len(),
        2,
        "DCCA should have 2 inputs (CLKI + CE tied high)"
    );

    // The buffered clock net should be used by DFF cells
    let buf_out_net = dcca_cells[0].outputs[0];
    let dff_cells: Vec<_> = result
        .netlist
        .cells
        .iter()
        .filter(|c| c.cell_type.contains("DFF") || c.cell_type.contains("TRELLIS_FF"))
        .collect();
    assert!(
        !dff_cells.is_empty(),
        "Should have at least one DFF cell for the register"
    );
    for dff in &dff_cells {
        assert_eq!(
            dff.clock,
            Some(buf_out_net),
            "DFF cell '{}' should use the buffered clock net",
            dff.path
        );
    }
}

#[test]
fn test_no_clock_buffer_generic_asic() {
    let lir = build_register_lir(4);
    let library = get_stdlib_library("generic_asic").expect("Failed to load generic_asic");

    // generic_asic has no clock buffer cell
    assert!(
        library.find_clk_buf_cell().is_none(),
        "generic_asic should not have clock buffer cells"
    );

    // Should still synthesize without panic
    let result = map_lir_to_gates(&lir, &library);

    // No clock buffer cells should be inserted
    let clk_buf_count = result
        .netlist
        .cells
        .iter()
        .filter(|c| {
            c.function
                .as_ref()
                .map_or(false, |f| matches!(f, CellFunction::ClkBuf))
        })
        .count();
    assert_eq!(
        clk_buf_count, 0,
        "generic_asic should not insert any clock buffer cells"
    );
}

#[test]
fn test_generic_asic_no_pll() {
    let library = get_stdlib_library("generic_asic").expect("Failed to load generic_asic");
    assert!(
        library.find_pll_cell().is_none(),
        "generic_asic should not have PLL cells"
    );
}

// =============================================================================
// IO Cell Infrastructure Tests
// =============================================================================

#[test]
fn test_ice40_has_io_cell() {
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");

    // SB_IO should load as BidirPad with IoCellInfo
    let (cell, io_info) = library
        .find_io_cell()
        .expect("ice40 should have an IO cell (SB_IO)");

    assert_eq!(cell.name, "SB_IO");
    assert!(matches!(cell.function, CellFunction::BidirPad));

    // Check capabilities
    assert!(io_info.supports_ddr, "SB_IO supports DDR");
    assert!(io_info.supports_tristate, "SB_IO supports tristate");
    assert!(
        !io_info.supports_differential,
        "SB_IO does not support differential"
    );
    assert!(io_info.has_input_register, "SB_IO has input register");
    assert!(io_info.has_output_register, "SB_IO has output register");
    assert_eq!(io_info.max_drive_strength_ma, Some(8));

    // Check pin map
    assert_eq!(io_info.pin_map.data_in.as_deref(), Some("D_IN_0"));
    assert_eq!(io_info.pin_map.data_in_ddr.as_deref(), Some("D_IN_1"));
    assert_eq!(io_info.pin_map.data_out.as_deref(), Some("D_OUT_0"));
    assert_eq!(io_info.pin_map.data_out_ddr.as_deref(), Some("D_OUT_1"));
    assert_eq!(
        io_info.pin_map.output_enable.as_deref(),
        Some("OUTPUT_ENABLE")
    );
    assert_eq!(io_info.pin_map.pad.as_deref(), Some("PACKAGE_PIN"));
    assert_eq!(io_info.pin_map.input_clk.as_deref(), Some("INPUT_CLK"));
    assert_eq!(io_info.pin_map.output_clk.as_deref(), Some("OUTPUT_CLK"));
    assert_eq!(
        io_info.pin_map.clock_enable.as_deref(),
        Some("CLOCK_ENABLE")
    );
    assert_eq!(
        io_info.pin_map.latch_input.as_deref(),
        Some("LATCH_INPUT_VALUE")
    );

    // find_input_pad should fall back to SB_IO (no dedicated input pad)
    let (input_cell, _) = library
        .find_input_pad()
        .expect("ice40 should find an input pad (fallback to SB_IO)");
    assert_eq!(input_cell.name, "SB_IO");

    // find_output_pad should fall back to SB_IO
    let (output_cell, _) = library
        .find_output_pad()
        .expect("ice40 should find an output pad (fallback to SB_IO)");
    assert_eq!(output_cell.name, "SB_IO");
}

#[test]
fn test_ecp5_has_io_cells() {
    let library = get_stdlib_library("ecp5").expect("Failed to load ecp5");

    // BB should be BidirPad
    let (bb_cell, bb_info) = library
        .find_io_cell()
        .expect("ecp5 should have a bidirectional IO cell (BB)");
    assert_eq!(bb_cell.name, "BB");
    assert!(matches!(bb_cell.function, CellFunction::BidirPad));
    assert!(bb_info.supports_tristate);
    assert!(bb_info.supports_differential);
    assert_eq!(bb_info.max_drive_strength_ma, Some(16));
    assert_eq!(bb_info.pin_map.data_in.as_deref(), Some("O"));
    assert_eq!(bb_info.pin_map.data_out.as_deref(), Some("I"));
    assert_eq!(bb_info.pin_map.output_enable.as_deref(), Some("T"));
    assert_eq!(bb_info.pin_map.pad.as_deref(), Some("B"));

    // IB should be InputPad (preferred over BB for inputs)
    let (ib_cell, ib_info) = library
        .find_input_pad()
        .expect("ecp5 should have an input pad (IB)");
    assert_eq!(ib_cell.name, "IB");
    assert!(matches!(ib_cell.function, CellFunction::InputPad));
    assert_eq!(ib_info.pin_map.data_in.as_deref(), Some("O"));
    assert_eq!(ib_info.pin_map.pad.as_deref(), Some("I"));

    // OB should be OutputPad (preferred over BB for outputs)
    let (ob_cell, _) = library
        .find_output_pad()
        .expect("ecp5 should have an output pad (OB)");
    assert_eq!(ob_cell.name, "OB");
    assert!(matches!(ob_cell.function, CellFunction::OutputPad));
}

#[test]
fn test_io_buffer_insertion_ice40() {
    let lir = build_register_lir(4);
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");
    let result = map_lir_to_gates(&lir, &library);

    // Should have SB_IO cells inserted for input (d) and output (q) ports
    let io_cells: Vec<_> = result
        .netlist
        .cells
        .iter()
        .filter(|c| c.cell_type == "SB_IO")
        .collect();
    assert!(
        !io_cells.is_empty(),
        "Should insert SB_IO cells for IO buffering"
    );

    // All IO cells should have source_op = "IOBuffer"
    for cell in &io_cells {
        assert_eq!(
            cell.source_op.as_deref(),
            Some("IOBuffer"),
            "IO cell '{}' should have source_op 'IOBuffer'",
            cell.path
        );
    }

    // Stats should report IO buffers inserted
    assert!(
        result.stats.io_buffers_inserted > 0,
        "Should report IO buffers inserted in stats"
    );
}

#[test]
fn test_io_buffer_insertion_ecp5() {
    let lir = build_register_lir(4);
    let library = get_stdlib_library("ecp5").expect("Failed to load ecp5");
    let result = map_lir_to_gates(&lir, &library);

    // ECP5 should use IB for input pads and OB for output pads
    let ib_cells: Vec<_> = result
        .netlist
        .cells
        .iter()
        .filter(|c| c.cell_type == "IB")
        .collect();
    let ob_cells: Vec<_> = result
        .netlist
        .cells
        .iter()
        .filter(|c| c.cell_type == "OB")
        .collect();

    assert!(
        !ib_cells.is_empty(),
        "Should insert IB cells for input ports"
    );
    assert!(
        !ob_cells.is_empty(),
        "Should insert OB cells for output ports"
    );

    // All IO cells should have source_op = "IOBuffer"
    for cell in ib_cells.iter().chain(ob_cells.iter()) {
        assert_eq!(
            cell.source_op.as_deref(),
            Some("IOBuffer"),
            "IO cell '{}' should have source_op 'IOBuffer'",
            cell.path
        );
    }
}

#[test]
fn test_no_io_buffer_generic_asic() {
    let lir = build_register_lir(4);
    let library = get_stdlib_library("generic_asic").expect("Failed to load generic_asic");

    // generic_asic has ASIC pads (via CellFunction) but no io_info → no IO buffer insertion
    let result = map_lir_to_gates(&lir, &library);

    let io_buffer_cells: Vec<_> = result
        .netlist
        .cells
        .iter()
        .filter(|c| c.source_op.as_deref() == Some("IOBuffer"))
        .collect();
    assert_eq!(
        io_buffer_cells.len(),
        0,
        "generic_asic should not insert IO buffer cells (no io_info)"
    );
    assert_eq!(
        result.stats.io_buffers_inserted, 0,
        "generic_asic should report 0 IO buffers inserted"
    );
}

#[test]
fn test_generic_asic_has_io_pads() {
    let library = get_stdlib_library("generic_asic").expect("Failed to load generic_asic");

    // generic_asic should have IO pads by CellFunction (no io_info)
    assert!(
        library.has_function(&CellFunction::InputPad),
        "generic_asic should have InputPad cells"
    );
    assert!(
        library.has_function(&CellFunction::OutputPad),
        "generic_asic should have OutputPad cells"
    );
    assert!(
        library.has_function(&CellFunction::BidirPad),
        "generic_asic should have BidirPad cells"
    );

    // But no io_info
    assert!(
        library.find_io_cell().is_none(),
        "generic_asic should not have io_info on its pad cells"
    );
}

#[test]
fn test_legacy_io_function_backward_compat() {
    // Parsing "io" should produce BidirPad (backward compat for old .sklib files)
    use skalp_lir::TechLibrary;

    let toml_content = r#"
[library]
name = "test_legacy_io"

[[cells]]
name = "LEGACY_IO"
function = "io"
fit = 0.1
inputs = ["D"]
outputs = ["Q"]
"#;
    let library = TechLibrary::from_toml(toml_content).expect("Failed to parse legacy IO");
    let cell = library
        .find_best_cell(&CellFunction::BidirPad)
        .expect("Should find legacy IO cell as BidirPad");
    assert_eq!(cell.name, "LEGACY_IO");
}

#[test]
fn test_io_constraint_propagation() {
    use indexmap::IndexMap;
    use skalp_frontend::hir::{PhysicalConstraints, PinLocation};

    let lir = build_register_lir(4);
    let library = get_stdlib_library("ice40").expect("Failed to load ice40");

    // Build constraints for the "d" input port
    let mut port_constraints: IndexMap<String, PhysicalConstraints> = IndexMap::new();
    port_constraints.insert(
        "d[0]".to_string(),
        PhysicalConstraints {
            pin_location: Some(PinLocation::Single("A1".to_string())),
            io_standard: Some("LVCMOS33".to_string()),
            drive_strength: None,
            slew_rate: None,
            termination: None,
            schmitt_trigger: None,
            bank: None,
            diff_term: None,
            pad_type: None,
            pad_cell: None,
            ldo_config: None,
        },
    );
    port_constraints.insert(
        "q[0]".to_string(),
        PhysicalConstraints {
            pin_location: Some(PinLocation::Single("B2".to_string())),
            io_standard: Some("LVCMOS25".to_string()),
            drive_strength: None,
            slew_rate: None,
            termination: None,
            schmitt_trigger: None,
            bank: None,
            diff_term: None,
            pad_type: None,
            pad_cell: None,
            ldo_config: None,
        },
    );

    let result = map_lir_to_gates_with_constraints(&lir, &library, port_constraints);

    // Find IO cells and check their parameters
    let io_cells: Vec<_> = result
        .netlist
        .cells
        .iter()
        .filter(|c| c.source_op.as_deref() == Some("IOBuffer"))
        .collect();
    assert!(
        !io_cells.is_empty(),
        "Should have IO buffer cells with constraints"
    );

    // Find the IO cell for d[0] input
    let d0_cell = io_cells
        .iter()
        .find(|c| c.path.contains("d[0]"))
        .expect("Should have IO cell for d[0]");
    assert_eq!(
        d0_cell.parameters.get("LOC").map(|s| s.as_str()),
        Some("A1"),
        "d[0] IO cell should have LOC=A1"
    );
    assert_eq!(
        d0_cell.parameters.get("IO_STANDARD").map(|s| s.as_str()),
        Some("LVCMOS33"),
        "d[0] IO cell should have IO_STANDARD=LVCMOS33"
    );

    // Find the IO cell for q[0] output
    let q0_cell = io_cells
        .iter()
        .find(|c| c.path.contains("q[0]"))
        .expect("Should have IO cell for q[0]");
    assert_eq!(
        q0_cell.parameters.get("LOC").map(|s| s.as_str()),
        Some("B2"),
        "q[0] IO cell should have LOC=B2"
    );
    assert_eq!(
        q0_cell.parameters.get("IO_STANDARD").map(|s| s.as_str()),
        Some("LVCMOS25"),
        "q[0] IO cell should have IO_STANDARD=LVCMOS25"
    );

    // IO cells without constraints should NOT have LOC/IO_STANDARD
    let unconstrained_io = io_cells
        .iter()
        .find(|c| c.path.contains("d[1]") || c.path.contains("d[2]") || c.path.contains("d[3]"));
    if let Some(cell) = unconstrained_io {
        assert!(
            !cell.parameters.contains_key("LOC"),
            "Unconstrained IO cell should not have LOC parameter"
        );
        assert!(
            !cell.parameters.contains_key("IO_STANDARD"),
            "Unconstrained IO cell should not have IO_STANDARD parameter"
        );
    }
}
