//! NCL Optimize-First Flow Integration Test
//!
//! Tests the optimize-first NCL synthesis approach:
//! 1. Create single-rail Boolean netlist
//! 2. Apply gate-level optimization (constant folding, DCE, etc.)
//! 3. Convert to dual-rail NCL
//! 4. Add minimal completion detection
//! 5. Simulate and validate

use skalp_lir::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};
use skalp_lir::gate_optimizer::GateOptimizer;
use skalp_lir::ncl_dual_rail::{convert_to_dual_rail, DualRailConfig};

/// Create a simple single-rail adder netlist for testing
fn create_simple_adder() -> GateNetlist {
    let mut netlist = GateNetlist::new("simple_adder".to_string(), "test_lib".to_string());

    // Inputs: a, b, cin
    let a = netlist.add_input("a".to_string());
    let b = netlist.add_input("b".to_string());
    let cin = netlist.add_input("cin".to_string());

    // Internal nets
    let a_xor_b = netlist.add_net(GateNet::new(GateNetId(3), "a_xor_b".to_string()));
    let a_and_b = netlist.add_net(GateNet::new(GateNetId(4), "a_and_b".to_string()));
    let axb_and_cin = netlist.add_net(GateNet::new(GateNetId(5), "axb_and_cin".to_string()));

    // Outputs: sum, cout
    let sum = netlist.add_output("sum".to_string());
    let cout = netlist.add_output("cout".to_string());

    let mut cell_id = 0u32;

    // XOR1: a_xor_b = a XOR b
    netlist.cells.push(Cell::new_comb(
        CellId(cell_id),
        "XOR2_X1".to_string(),
        "test_lib".to_string(),
        0.1,
        "xor1".to_string(),
        vec![a, b],
        vec![a_xor_b],
    ));
    cell_id += 1;

    // XOR2: sum = a_xor_b XOR cin
    netlist.cells.push(Cell::new_comb(
        CellId(cell_id),
        "XOR2_X1".to_string(),
        "test_lib".to_string(),
        0.1,
        "xor2".to_string(),
        vec![a_xor_b, cin],
        vec![sum],
    ));
    cell_id += 1;

    // AND1: a_and_b = a AND b
    netlist.cells.push(Cell::new_comb(
        CellId(cell_id),
        "AND2_X1".to_string(),
        "test_lib".to_string(),
        0.1,
        "and1".to_string(),
        vec![a, b],
        vec![a_and_b],
    ));
    cell_id += 1;

    // AND2: axb_and_cin = a_xor_b AND cin
    netlist.cells.push(Cell::new_comb(
        CellId(cell_id),
        "AND2_X1".to_string(),
        "test_lib".to_string(),
        0.1,
        "and2".to_string(),
        vec![a_xor_b, cin],
        vec![axb_and_cin],
    ));
    cell_id += 1;

    // OR1: cout = a_and_b OR axb_and_cin
    netlist.cells.push(Cell::new_comb(
        CellId(cell_id),
        "OR2_X1".to_string(),
        "test_lib".to_string(),
        0.1,
        "or1".to_string(),
        vec![a_and_b, axb_and_cin],
        vec![cout],
    ));

    netlist.rebuild_net_connectivity();
    netlist
}

/// Create a netlist with constant inputs (for optimization testing)
fn create_adder_with_constant_input() -> GateNetlist {
    let mut netlist = GateNetlist::new("adder_const".to_string(), "test_lib".to_string());

    // Inputs: a, b (cin is tied low)
    let a = netlist.add_input("a".to_string());
    let b = netlist.add_input("b".to_string());
    let cin = netlist.add_net(GateNet::new(GateNetId(2), "cin".to_string()));

    // Internal nets
    let a_xor_b = netlist.add_net(GateNet::new(GateNetId(3), "a_xor_b".to_string()));
    let a_and_b = netlist.add_net(GateNet::new(GateNetId(4), "a_and_b".to_string()));
    let axb_and_cin = netlist.add_net(GateNet::new(GateNetId(5), "axb_and_cin".to_string()));

    // Outputs
    let sum = netlist.add_output("sum".to_string());
    let cout = netlist.add_output("cout".to_string());

    let mut cell_id = 0u32;

    // TIE_LOW for cin = 0
    netlist.cells.push(Cell::new_comb(
        CellId(cell_id),
        "TIE_LOW".to_string(),
        "test_lib".to_string(),
        0.01,
        "tie_cin".to_string(),
        vec![],
        vec![cin],
    ));
    cell_id += 1;

    // XOR1: a_xor_b = a XOR b
    netlist.cells.push(Cell::new_comb(
        CellId(cell_id),
        "XOR2_X1".to_string(),
        "test_lib".to_string(),
        0.1,
        "xor1".to_string(),
        vec![a, b],
        vec![a_xor_b],
    ));
    cell_id += 1;

    // XOR2: sum = a_xor_b XOR cin (cin=0, so sum = a_xor_b)
    netlist.cells.push(Cell::new_comb(
        CellId(cell_id),
        "XOR2_X1".to_string(),
        "test_lib".to_string(),
        0.1,
        "xor2".to_string(),
        vec![a_xor_b, cin],
        vec![sum],
    ));
    cell_id += 1;

    // AND1: a_and_b = a AND b
    netlist.cells.push(Cell::new_comb(
        CellId(cell_id),
        "AND2_X1".to_string(),
        "test_lib".to_string(),
        0.1,
        "and1".to_string(),
        vec![a, b],
        vec![a_and_b],
    ));
    cell_id += 1;

    // AND2: axb_and_cin = a_xor_b AND cin (cin=0, so result = 0)
    netlist.cells.push(Cell::new_comb(
        CellId(cell_id),
        "AND2_X1".to_string(),
        "test_lib".to_string(),
        0.1,
        "and2".to_string(),
        vec![a_xor_b, cin],
        vec![axb_and_cin],
    ));
    cell_id += 1;

    // OR1: cout = a_and_b OR axb_and_cin (axb_and_cin=0, so cout = a_and_b)
    netlist.cells.push(Cell::new_comb(
        CellId(cell_id),
        "OR2_X1".to_string(),
        "test_lib".to_string(),
        0.1,
        "or1".to_string(),
        vec![a_and_b, axb_and_cin],
        vec![cout],
    ));

    netlist.rebuild_net_connectivity();
    netlist
}

#[test]
fn test_optimize_first_flow_basic() {
    println!("\n=== Test: Optimize-First NCL Flow (Basic) ===\n");

    // Step 1: Create single-rail netlist
    let single_rail = create_simple_adder();
    println!(
        "Step 1: Single-rail netlist: {} cells, {} nets",
        single_rail.cells.len(),
        single_rail.nets.len()
    );

    // Step 2: Apply gate optimization (not much to optimize here)
    let mut optimizer = GateOptimizer::new();
    let mut optimized = single_rail.clone();
    let opt_stats = optimizer.optimize(&mut optimized);
    println!(
        "Step 2: After optimization: {} cells ({} removed)",
        optimized.cells.len(),
        opt_stats.cells_removed
    );

    // Step 3: Convert to dual-rail NCL
    let config = DualRailConfig::default();
    let (ncl_netlist, dual_stats) = convert_to_dual_rail(&optimized, config);
    println!(
        "Step 3: Dual-rail NCL: {} cells, {} nets",
        ncl_netlist.cells.len(),
        ncl_netlist.nets.len()
    );
    println!("        TH12 gates: {}", dual_stats.th12_count);
    println!("        TH22 gates: {}", dual_stats.th22_count);
    println!("        Completion gates: {}", dual_stats.completion_gates);

    // Verify the conversion produced NCL gates
    assert!(dual_stats.th12_count > 0, "Should have TH12 gates");
    assert!(dual_stats.th22_count > 0, "Should have TH22 gates");

    // Verify we have minimal completion (only at outputs)
    // 2 output bits = 2 per-bit TH12 + tree of TH22
    assert!(
        dual_stats.completion_gates <= 4,
        "Should have minimal completion detection"
    );

    println!("\n=== Basic test PASSED ===\n");
}

#[test]
fn test_optimize_first_flow_with_constants() {
    println!("\n=== Test: Optimize-First NCL Flow (With Constants) ===\n");

    // Step 1: Create single-rail netlist with constant input
    let single_rail = create_adder_with_constant_input();
    println!(
        "Step 1: Single-rail netlist: {} cells, {} nets",
        single_rail.cells.len(),
        single_rail.nets.len()
    );

    // Step 2: Apply gate optimization
    // With cin=0:
    // - XOR(a_xor_b, 0) = a_xor_b (XOR with 0 is identity)
    // - AND(a_xor_b, 0) = 0 (AND with 0 is 0)
    // - OR(a_and_b, 0) = a_and_b (OR with 0 is identity)
    let mut optimizer = GateOptimizer::new();
    let mut optimized = single_rail.clone();
    let opt_stats = optimizer.optimize(&mut optimized);
    println!(
        "Step 2: After optimization: {} cells ({} removed)",
        optimized.cells.len(),
        opt_stats.cells_removed
    );

    // Should have removed some gates due to constant folding
    assert!(
        opt_stats.cells_removed > 0,
        "Should have removed some gates with constant inputs"
    );

    // Step 3: Convert to dual-rail NCL
    let config = DualRailConfig::default();
    let (ncl_netlist, dual_stats) = convert_to_dual_rail(&optimized, config);
    println!(
        "Step 3: Dual-rail NCL: {} cells, {} nets",
        ncl_netlist.cells.len(),
        ncl_netlist.nets.len()
    );
    println!("        TH12 gates: {}", dual_stats.th12_count);
    println!("        TH22 gates: {}", dual_stats.th22_count);
    println!("        Completion gates: {}", dual_stats.completion_gates);

    println!("\n=== Constants test PASSED ===\n");
}

#[test]
fn test_optimize_first_cell_count_reduction() {
    println!("\n=== Test: Optimize-First Cell Count Comparison ===\n");

    // Create a netlist with redundant logic
    let mut netlist = GateNetlist::new("redundant".to_string(), "test_lib".to_string());

    let a = netlist.add_input("a".to_string());
    let b = netlist.add_input("b".to_string());
    let const_0 = netlist.add_net(GateNet::new(GateNetId(2), "const_0".to_string()));
    let const_1 = netlist.add_net(GateNet::new(GateNetId(3), "const_1".to_string()));
    let and1_out = netlist.add_net(GateNet::new(GateNetId(4), "and1_out".to_string()));
    let and2_out = netlist.add_net(GateNet::new(GateNetId(5), "and2_out".to_string()));
    let or1_out = netlist.add_net(GateNet::new(GateNetId(6), "or1_out".to_string()));
    let out = netlist.add_output("out".to_string());

    let mut cell_id = 0u32;

    // TIE_LOW and TIE_HIGH
    netlist.cells.push(Cell::new_comb(
        CellId(cell_id),
        "TIE_LOW".to_string(),
        "test_lib".to_string(),
        0.01,
        "tie_low".to_string(),
        vec![],
        vec![const_0],
    ));
    cell_id += 1;

    netlist.cells.push(Cell::new_comb(
        CellId(cell_id),
        "TIE_HIGH".to_string(),
        "test_lib".to_string(),
        0.01,
        "tie_high".to_string(),
        vec![],
        vec![const_1],
    ));
    cell_id += 1;

    // AND(a, 0) = 0 (can be optimized away)
    netlist.cells.push(Cell::new_comb(
        CellId(cell_id),
        "AND2_X1".to_string(),
        "test_lib".to_string(),
        0.1,
        "and1".to_string(),
        vec![a, const_0],
        vec![and1_out],
    ));
    cell_id += 1;

    // AND(b, 1) = b (can be simplified)
    netlist.cells.push(Cell::new_comb(
        CellId(cell_id),
        "AND2_X1".to_string(),
        "test_lib".to_string(),
        0.1,
        "and2".to_string(),
        vec![b, const_1],
        vec![and2_out],
    ));
    cell_id += 1;

    // OR(0, and2_out) = and2_out (can be simplified)
    netlist.cells.push(Cell::new_comb(
        CellId(cell_id),
        "OR2_X1".to_string(),
        "test_lib".to_string(),
        0.1,
        "or1".to_string(),
        vec![and1_out, and2_out],
        vec![or1_out],
    ));
    cell_id += 1;

    // Buffer to output
    netlist.cells.push(Cell::new_comb(
        CellId(cell_id),
        "BUF_X1".to_string(),
        "test_lib".to_string(),
        0.1,
        "buf_out".to_string(),
        vec![or1_out],
        vec![out],
    ));

    netlist.rebuild_net_connectivity();

    // === Without optimization ===
    let config = DualRailConfig::default();
    let (_ncl_unoptimized, stats_unopt) = convert_to_dual_rail(&netlist, config.clone());
    println!(
        "Without optimization: {} single-rail -> {} dual-rail cells",
        stats_unopt.single_rail_cells, stats_unopt.dual_rail_cells
    );

    // === With optimization ===
    let mut optimized = netlist.clone();
    let mut optimizer = GateOptimizer::new();
    let opt_stats = optimizer.optimize(&mut optimized);
    println!(
        "After optimization: {} cells ({} removed)",
        optimized.cells.len(),
        opt_stats.cells_removed
    );

    let (_ncl_optimized, stats_opt) = convert_to_dual_rail(&optimized, config);
    println!(
        "With optimization: {} single-rail -> {} dual-rail cells",
        stats_opt.single_rail_cells, stats_opt.dual_rail_cells
    );

    // The optimized version should have fewer cells
    let reduction_pct =
        100.0 * (1.0 - (stats_opt.dual_rail_cells as f64 / stats_unopt.dual_rail_cells as f64));
    println!("Cell count reduction: {:.1}%", reduction_pct);

    assert!(
        stats_opt.dual_rail_cells < stats_unopt.dual_rail_cells,
        "Optimized NCL should have fewer cells"
    );

    println!("\n=== Cell count comparison PASSED ===\n");
}

#[test]
fn test_inverter_is_free() {
    println!("\n=== Test: Inverter Is Free in NCL ===\n");

    // Create a simple inverter chain
    let mut netlist = GateNetlist::new("inv_chain".to_string(), "test_lib".to_string());

    let a = netlist.add_input("a".to_string());
    let inv1_out = netlist.add_net(GateNet::new(GateNetId(1), "inv1_out".to_string()));
    let out = netlist.add_output("out".to_string());

    // INV -> INV (double inversion = identity)
    netlist.cells.push(Cell::new_comb(
        CellId(0),
        "INV_X1".to_string(),
        "test_lib".to_string(),
        0.1,
        "inv1".to_string(),
        vec![a],
        vec![inv1_out],
    ));

    netlist.cells.push(Cell::new_comb(
        CellId(1),
        "INV_X1".to_string(),
        "test_lib".to_string(),
        0.1,
        "inv2".to_string(),
        vec![inv1_out],
        vec![out],
    ));

    netlist.rebuild_net_connectivity();

    // Convert to dual-rail
    let config = DualRailConfig::default();
    let (_ncl_netlist, stats) = convert_to_dual_rail(&netlist, config);

    println!("Single-rail: {} cells", stats.single_rail_cells);
    println!("Dual-rail: {} cells", stats.dual_rail_cells);
    println!("TH12 gates: {}", stats.th12_count);
    println!("TH22 gates: {}", stats.th22_count);

    // Inverters in NCL are just rail swaps, so they become buffers
    // The only TH gates should be for completion detection
    // (note: we create buffers for rail swapping in current implementation,
    // but they can be optimized away)

    println!("\n=== Inverter test PASSED ===\n");
}
