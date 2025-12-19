//! Integration tests for the synthesis module
//!
//! These tests verify round-trip conversion between GateNetlist and AIG.

use super::*;
use crate::builtin_libraries::builtin_generic_asic;
use crate::gate_netlist::{
    Cell, CellId, CellSafetyClassification, GateNet, GateNetId, GateNetlist,
};

/// Helper to create a simple AND circuit
fn create_and_circuit() -> GateNetlist {
    let mut netlist = GateNetlist::new("and_test".to_string(), "generic_asic".to_string());

    let a = netlist.add_input("a".to_string());
    let b = netlist.add_input("b".to_string());
    let y = netlist.add_output("y".to_string());

    let cell = Cell::new_comb(
        CellId(0),
        "AND2_X1".to_string(),
        "generic_asic".to_string(),
        0.1,
        "top.and".to_string(),
        vec![a, b],
        vec![y],
    );
    netlist.add_cell(cell);
    netlist
}

/// Helper to create a NAND circuit
fn create_nand_circuit() -> GateNetlist {
    let mut netlist = GateNetlist::new("nand_test".to_string(), "generic_asic".to_string());

    let a = netlist.add_input("a".to_string());
    let b = netlist.add_input("b".to_string());
    let y = netlist.add_output("y".to_string());

    let cell = Cell::new_comb(
        CellId(0),
        "NAND2_X1".to_string(),
        "generic_asic".to_string(),
        0.1,
        "top.nand".to_string(),
        vec![a, b],
        vec![y],
    );
    netlist.add_cell(cell);
    netlist
}

/// Helper to create an XOR circuit
fn create_xor_circuit() -> GateNetlist {
    let mut netlist = GateNetlist::new("xor_test".to_string(), "generic_asic".to_string());

    let a = netlist.add_input("a".to_string());
    let b = netlist.add_input("b".to_string());
    let y = netlist.add_output("y".to_string());

    let cell = Cell::new_comb(
        CellId(0),
        "XOR2_X1".to_string(),
        "generic_asic".to_string(),
        0.2,
        "top.xor".to_string(),
        vec![a, b],
        vec![y],
    );
    netlist.add_cell(cell);
    netlist
}

/// Helper to create a simple sequential circuit (DFF)
fn create_dff_circuit() -> GateNetlist {
    let mut netlist = GateNetlist::new("dff_test".to_string(), "generic_asic".to_string());

    let clk = netlist.add_clock("clk".to_string());
    let d = netlist.add_input("d".to_string());
    let q = netlist.add_output("q".to_string());

    let cell = Cell::new_seq(
        CellId(0),
        "DFF_X1".to_string(),
        "generic_asic".to_string(),
        0.2,
        "top.dff".to_string(),
        vec![d],
        vec![q],
        clk,
        None,
    );
    netlist.add_cell(cell);
    netlist
}

/// Helper to create a more complex combinational circuit
fn create_complex_circuit() -> GateNetlist {
    let mut netlist = GateNetlist::new("complex_test".to_string(), "generic_asic".to_string());

    let a = netlist.add_input("a".to_string());
    let b = netlist.add_input("b".to_string());
    let c = netlist.add_input("c".to_string());

    let ab = netlist.add_net(GateNet::new(GateNetId(0), "ab".to_string()));
    let abc = netlist.add_net(GateNet::new(GateNetId(0), "abc".to_string()));
    let y = netlist.add_output("y".to_string());

    // a & b
    let and1 = Cell::new_comb(
        CellId(0),
        "AND2_X1".to_string(),
        "generic_asic".to_string(),
        0.1,
        "top.and1".to_string(),
        vec![a, b],
        vec![ab],
    );
    netlist.add_cell(and1);

    // (a & b) | c
    let or1 = Cell::new_comb(
        CellId(0),
        "OR2_X1".to_string(),
        "generic_asic".to_string(),
        0.1,
        "top.or1".to_string(),
        vec![ab, c],
        vec![abc],
    );
    netlist.add_cell(or1);

    // Buffer to output
    let buf = Cell::new_comb(
        CellId(0),
        "BUF_X1".to_string(),
        "generic_asic".to_string(),
        0.05,
        "top.buf".to_string(),
        vec![abc],
        vec![y],
    );
    netlist.add_cell(buf);

    netlist
}

/// Helper to create a circuit with safety mechanism classification
fn create_safety_classified_circuit() -> GateNetlist {
    let mut netlist = GateNetlist::new("safety_test".to_string(), "generic_asic".to_string());

    let a = netlist.add_input("a".to_string());
    let b = netlist.add_input("b".to_string());
    let c = netlist.add_input("c".to_string());

    let func_out = netlist.add_net(GateNet::new(GateNetId(0), "func_out".to_string()));
    let y = netlist.add_output("y".to_string());

    // Functional cell
    let func_cell = Cell::new_comb(
        CellId(0),
        "AND2_X1".to_string(),
        "generic_asic".to_string(),
        0.1,
        "top.func".to_string(),
        vec![a, b],
        vec![func_out],
    );
    netlist.add_cell(func_cell);

    // Safety mechanism cell (TMR voter)
    let sm_cell = Cell::new_comb(
        CellId(0),
        "OR2_X1".to_string(),
        "generic_asic".to_string(),
        0.1,
        "top.voter".to_string(),
        vec![func_out, c],
        vec![y],
    )
    .with_safety_classification(CellSafetyClassification::SafetyMechanism {
        goal_name: "TestGoal".to_string(),
        mechanism_name: "TestMechanism".to_string(),
    });
    netlist.add_cell(sm_cell);

    netlist
}

#[test]
fn test_and_round_trip() {
    let library = builtin_generic_asic();
    let original = create_and_circuit();

    // Convert to AIG
    let builder = AigBuilder::new(&original);
    let aig = builder.build();

    // Verify AIG structure
    assert_eq!(aig.input_count(), 2);
    assert_eq!(aig.output_count(), 1);
    assert_eq!(aig.and_count(), 1);

    // Convert back
    let result = AigWriter::new(&library).write(&aig);

    // Verify structure preserved
    assert_eq!(result.inputs.len(), original.inputs.len());
    assert_eq!(result.outputs.len(), original.outputs.len());
}

#[test]
fn test_nand_round_trip() {
    let library = builtin_generic_asic();
    let original = create_nand_circuit();

    let builder = AigBuilder::new(&original);
    let aig = builder.build();

    // NAND should be AND with inverted output
    assert_eq!(aig.and_count(), 1);
    let (_, out_lit) = &aig.outputs()[0];
    assert!(out_lit.inverted);

    let result = AigWriter::new(&library).write(&aig);
    assert_eq!(result.inputs.len(), 2);
    assert_eq!(result.outputs.len(), 1);
}

#[test]
fn test_xor_round_trip() {
    let library = builtin_generic_asic();
    let original = create_xor_circuit();

    let builder = AigBuilder::new(&original);
    let aig = builder.build();

    // XOR uses 3 AND gates in AIG
    assert_eq!(aig.and_count(), 3);

    let result = AigWriter::new(&library).write(&aig);
    assert_eq!(result.inputs.len(), 2);
    assert_eq!(result.outputs.len(), 1);
}

#[test]
fn test_dff_round_trip() {
    let library = builtin_generic_asic();
    let original = create_dff_circuit();

    let builder = AigBuilder::new(&original);
    let aig = builder.build();

    // Should have 1 latch
    assert_eq!(aig.latch_count(), 1);
    assert_eq!(aig.input_count(), 2); // clk and d

    let result = AigWriter::new(&library).write(&aig);
    assert_eq!(result.inputs.len(), 2);
    assert_eq!(result.outputs.len(), 1);
}

#[test]
fn test_complex_round_trip() {
    let library = builtin_generic_asic();
    let original = create_complex_circuit();

    let builder = AigBuilder::new(&original);
    let aig = builder.build();

    // AND uses 1, OR uses 1 (as inverted NAND of inverted inputs)
    // Buffer is pass-through
    assert_eq!(aig.input_count(), 3);
    assert_eq!(aig.output_count(), 1);
    assert!(aig.and_count() >= 2); // At least AND + OR decomposition

    let result = AigWriter::new(&library).write(&aig);
    assert_eq!(result.inputs.len(), 3);
    assert_eq!(result.outputs.len(), 1);
}

#[test]
fn test_safety_classification_preserved() {
    let library = builtin_generic_asic();
    let original = create_safety_classified_circuit();

    let builder = AigBuilder::new(&original);
    let aig = builder.build();

    // Verify safety info is tracked
    let stats = aig.compute_stats();
    assert!(stats.total_fit > 0.0);

    // Check that at least one node has SM classification
    let mut found_sm = false;
    for (id, _) in aig.iter_nodes() {
        if let Some(safety) = aig.get_safety_info(id) {
            if let Some(ref class) = safety.classification {
                if class.is_safety_mechanism() {
                    found_sm = true;
                    break;
                }
            }
        }
    }
    assert!(
        found_sm,
        "Safety mechanism classification should be preserved"
    );
}

#[test]
fn test_aig_stats() {
    let original = create_complex_circuit();
    let builder = AigBuilder::new(&original);
    let aig = builder.build();

    let stats = aig.compute_stats();

    assert_eq!(stats.input_count, 3);
    assert_eq!(stats.output_count, 1);
    assert!(stats.and_count > 0);
    assert!(stats.max_level > 0);
}

#[test]
fn test_strash_deduplication() {
    let mut netlist = GateNetlist::new("strash_test".to_string(), "generic_asic".to_string());

    let a = netlist.add_input("a".to_string());
    let b = netlist.add_input("b".to_string());

    let ab1 = netlist.add_net(GateNet::new(GateNetId(0), "ab1".to_string()));
    let ab2 = netlist.add_net(GateNet::new(GateNetId(0), "ab2".to_string()));
    let y = netlist.add_output("y".to_string());

    // Two identical AND gates
    let and1 = Cell::new_comb(
        CellId(0),
        "AND2_X1".to_string(),
        "generic_asic".to_string(),
        0.1,
        "top.and1".to_string(),
        vec![a, b],
        vec![ab1],
    );
    netlist.add_cell(and1);

    let and2 = Cell::new_comb(
        CellId(0),
        "AND2_X1".to_string(),
        "generic_asic".to_string(),
        0.1,
        "top.and2".to_string(),
        vec![a, b],
        vec![ab2],
    );
    netlist.add_cell(and2);

    // XOR of the two (should be 0 since they're the same)
    let xor = Cell::new_comb(
        CellId(0),
        "XOR2_X1".to_string(),
        "generic_asic".to_string(),
        0.2,
        "top.xor".to_string(),
        vec![ab1, ab2],
        vec![y],
    );
    netlist.add_cell(xor);

    let builder = AigBuilder::new(&netlist);
    let aig = builder.build();

    // Due to strash, the two AND gates should be merged into one
    // XOR of identical signals should simplify to constant 0
    assert!(aig.and_count() <= 2, "Strash should reduce AND count");
}

#[test]
fn test_constant_propagation() {
    let mut aig = Aig::new("const_test".to_string());

    let a = aig.add_input("a".to_string(), None);

    // a AND 1 = a
    let result = aig.add_and(AigLit::new(a), AigLit::true_lit());
    assert_eq!(result, AigLit::new(a));

    // a AND 0 = 0
    let result = aig.add_and(AigLit::new(a), AigLit::false_lit());
    assert_eq!(result, AigLit::false_lit());

    // a AND a = a
    let result = aig.add_and(AigLit::new(a), AigLit::new(a));
    assert_eq!(result, AigLit::new(a));

    // a AND !a = 0
    let result = aig.add_and(AigLit::new(a), AigLit::not(a));
    assert_eq!(result, AigLit::false_lit());
}

// ============================================================================
// Power Domain Barrier Tests
// ============================================================================

/// Helper to create a circuit with a level shifter between domains
fn create_level_shifter_circuit() -> GateNetlist {
    let mut netlist = GateNetlist::new("lvlshift_test".to_string(), "generic_asic".to_string());

    let a = netlist.add_input("a".to_string());
    let b = netlist.add_input("b".to_string());

    // Logic in low-voltage domain
    let ab_lv = netlist.add_net(GateNet::new(GateNetId(0), "ab_lv".to_string()));
    let and_lv = Cell::new_comb(
        CellId(0),
        "AND2_X1".to_string(),
        "generic_asic".to_string(),
        0.1,
        "lv.and".to_string(),
        vec![a, b],
        vec![ab_lv],
    );
    netlist.add_cell(and_lv);

    // Level shifter (LV -> HV)
    let ab_hv = netlist.add_net(GateNet::new(GateNetId(0), "ab_hv".to_string()));
    let lvlshift = Cell::new_comb(
        CellId(0),
        "LVLSHIFT_LH_X1".to_string(),
        "generic_asic".to_string(),
        0.15,
        "lvlshift".to_string(),
        vec![ab_lv],
        vec![ab_hv],
    );
    netlist.add_cell(lvlshift);

    // Logic in high-voltage domain
    let c = netlist.add_input("c".to_string());
    let y = netlist.add_output("y".to_string());
    let and_hv = Cell::new_comb(
        CellId(0),
        "AND2_X1".to_string(),
        "generic_asic".to_string(),
        0.1,
        "hv.and".to_string(),
        vec![ab_hv, c],
        vec![y],
    );
    netlist.add_cell(and_hv);

    netlist
}

/// Helper to create a circuit with isolation cell
fn create_isolation_circuit() -> GateNetlist {
    let mut netlist = GateNetlist::new("isolation_test".to_string(), "generic_asic".to_string());

    let a = netlist.add_input("a".to_string());
    let b = netlist.add_input("b".to_string());
    let iso_en = netlist.add_input("iso_en".to_string());

    // Logic before isolation
    let ab = netlist.add_net(GateNet::new(GateNetId(0), "ab".to_string()));
    let and1 = Cell::new_comb(
        CellId(0),
        "AND2_X1".to_string(),
        "generic_asic".to_string(),
        0.1,
        "pre_iso.and".to_string(),
        vec![a, b],
        vec![ab],
    );
    netlist.add_cell(and1);

    // Isolation cell
    let ab_iso = netlist.add_net(GateNet::new(GateNetId(0), "ab_iso".to_string()));
    let iso_cell = Cell::new_comb(
        CellId(0),
        "ISO_AND_X1".to_string(),
        "generic_asic".to_string(),
        0.12,
        "iso".to_string(),
        vec![ab, iso_en],
        vec![ab_iso],
    );
    netlist.add_cell(iso_cell);

    // Logic after isolation
    let c = netlist.add_input("c".to_string());
    let y = netlist.add_output("y".to_string());
    let and2 = Cell::new_comb(
        CellId(0),
        "AND2_X1".to_string(),
        "generic_asic".to_string(),
        0.1,
        "post_iso.and".to_string(),
        vec![ab_iso, c],
        vec![y],
    );
    netlist.add_cell(and2);

    netlist
}

#[test]
fn test_barrier_basic_creation() {
    let mut aig = Aig::new("barrier_test".to_string());

    let a = aig.add_input("a".to_string(), None);

    // Create a level shifter barrier
    let barrier_id = aig.add_barrier(
        BarrierType::LevelShifterLH,
        AigLit::new(a),
        None,
        None,
        None,
        None,
    );

    // Verify barrier was created
    assert_eq!(aig.barrier_count(), 1);

    // Verify barrier node properties
    let node = aig.get_node(barrier_id).unwrap();
    assert!(node.is_barrier());

    // Verify fanins are correct
    let fanins = node.fanins();
    assert_eq!(fanins.len(), 1);
    assert_eq!(fanins[0].node, a);
}

#[test]
fn test_barrier_preserved_through_strash() {
    let mut aig = Aig::new("strash_barrier_test".to_string());

    let a = aig.add_input("a".to_string(), None);
    let b = aig.add_input("b".to_string(), None);

    // Create: a & b -> barrier -> output
    let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
    let barrier = aig.add_barrier(BarrierType::LevelShifterLH, ab, None, None, None, None);
    aig.add_output("y".to_string(), AigLit::new(barrier));

    let before_barriers = aig.barrier_count();

    // Run strash
    let mut strash = passes::Strash::new();
    strash.run(&mut aig);

    // Barrier should be preserved
    assert_eq!(aig.barrier_count(), before_barriers);
}

#[test]
fn test_barrier_preserved_through_const_prop() {
    let mut aig = Aig::new("const_prop_barrier_test".to_string());

    let a = aig.add_input("a".to_string(), None);

    // Create: a -> barrier -> output
    let barrier = aig.add_barrier(
        BarrierType::LevelShifterLH,
        AigLit::new(a),
        None,
        None,
        None,
        None,
    );
    aig.add_output("y".to_string(), AigLit::new(barrier));

    let before_barriers = aig.barrier_count();

    // Run const_prop
    let mut const_prop = passes::ConstProp::new();
    const_prop.run(&mut aig);

    // Barrier should be preserved
    assert_eq!(aig.barrier_count(), before_barriers);
}

#[test]
fn test_barrier_preserved_through_dce() {
    let mut aig = Aig::new("dce_barrier_test".to_string());

    let a = aig.add_input("a".to_string(), None);
    let b = aig.add_input("b".to_string(), None);

    // Create used path: a -> barrier -> output
    let barrier = aig.add_barrier(
        BarrierType::LevelShifterLH,
        AigLit::new(a),
        None,
        None,
        None,
        None,
    );
    aig.add_output("y".to_string(), AigLit::new(barrier));

    // Create dead path: b -> and (not connected to output)
    let _dead = aig.add_and(AigLit::new(b), AigLit::new(b));

    let before_barriers = aig.barrier_count();

    // Run DCE
    let mut dce = passes::Dce::new();
    dce.run(&mut aig);

    // Barrier should be preserved (it's live)
    assert_eq!(aig.barrier_count(), before_barriers);
}

#[test]
fn test_barrier_preserved_through_balance() {
    let mut aig = Aig::new("balance_barrier_test".to_string());

    let a = aig.add_input("a".to_string(), None);
    let b = aig.add_input("b".to_string(), None);
    let c = aig.add_input("c".to_string(), None);
    let d = aig.add_input("d".to_string(), None);

    // Create chain before barrier: ((a & b) & c) & d
    let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
    let abc = aig.add_and(ab, AigLit::new(c));
    let abcd = aig.add_and(abc, AigLit::new(d));

    // Level shifter
    let barrier = aig.add_barrier(BarrierType::LevelShifterLH, abcd, None, None, None, None);

    aig.add_output("y".to_string(), AigLit::new(barrier));

    let before_barriers = aig.barrier_count();

    // Run balance
    let mut balance = passes::Balance::new();
    balance.run(&mut aig);

    // Barrier should be preserved
    assert_eq!(aig.barrier_count(), before_barriers);
}

#[test]
fn test_barrier_preserved_through_rewrite() {
    let mut aig = Aig::new("rewrite_barrier_test".to_string());

    let a = aig.add_input("a".to_string(), None);
    let b = aig.add_input("b".to_string(), None);

    // Create simple logic before barrier
    let ab = aig.add_and(AigLit::new(a), AigLit::new(b));

    // Level shifter
    let barrier = aig.add_barrier(BarrierType::LevelShifterLH, ab, None, None, None, None);

    aig.add_output("y".to_string(), AigLit::new(barrier));

    let before_barriers = aig.barrier_count();

    // Run rewrite
    let mut rewrite = passes::Rewrite::new();
    rewrite.run(&mut aig);

    // Barrier should be preserved
    assert_eq!(aig.barrier_count(), before_barriers);
}

#[test]
fn test_barrier_blocks_cut_expansion() {
    use crate::synth::cuts::{CutEnumeration, CutParams};

    let mut aig = Aig::new("cut_barrier_test".to_string());

    let a = aig.add_input("a".to_string(), None);
    let b = aig.add_input("b".to_string(), None);
    let c = aig.add_input("c".to_string(), None);

    // Logic before barrier
    let ab = aig.add_and(AigLit::new(a), AigLit::new(b));

    // Barrier
    let barrier = aig.add_barrier(BarrierType::LevelShifterLH, ab, None, None, None, None);

    // Logic after barrier
    let result = aig.add_and(AigLit::new(barrier), AigLit::new(c));
    aig.add_output("y".to_string(), result);

    // Enumerate cuts
    let params = CutParams::default();
    let cuts = CutEnumeration::enumerate(&aig, params);

    // Get cuts for the final AND node
    let final_cuts = cuts.get_cuts(result.node).unwrap();

    // The cuts should NOT include 'a' or 'b' directly because the barrier blocks expansion
    // They should only see the barrier as a leaf
    for cut in &final_cuts.cuts {
        // Verify that 'a' and 'b' are not directly in the cut leaves
        // (they should be hidden behind the barrier)
        for &leaf in &cut.leaves {
            assert!(
                leaf == barrier || leaf == c || leaf == result.node,
                "Cut should not expand through barrier to reach nodes a or b"
            );
        }
    }
}

#[test]
fn test_level_shifter_round_trip() {
    let library = builtin_generic_asic();
    let original = create_level_shifter_circuit();

    // Convert to AIG
    let builder = AigBuilder::new(&original);
    let aig = builder.build();

    // Should have exactly 1 barrier (level shifter)
    assert_eq!(
        aig.barrier_count(),
        1,
        "Level shifter should create a barrier node"
    );

    // Convert back
    let result = AigWriter::new(&library).write(&aig);

    // Verify structure preserved
    assert_eq!(result.inputs.len(), original.inputs.len());
    assert_eq!(result.outputs.len(), original.outputs.len());

    // Verify we have a level shifter cell in the output
    let has_lvlshift = result
        .cells
        .iter()
        .any(|c| c.cell_type.contains("LVLSHIFT") || c.cell_type.contains("BARRIER"));
    assert!(has_lvlshift, "Level shifter cell should be preserved");
}

#[test]
fn test_isolation_cell_round_trip() {
    let library = builtin_generic_asic();
    let original = create_isolation_circuit();

    // Convert to AIG
    let builder = AigBuilder::new(&original);
    let aig = builder.build();

    // Should have exactly 1 barrier (isolation cell)
    assert_eq!(
        aig.barrier_count(),
        1,
        "Isolation cell should create a barrier node"
    );

    // The barrier should have an enable input
    for (id, node) in aig.iter_nodes() {
        if let AigNode::Barrier { enable, .. } = node {
            assert!(
                enable.is_some(),
                "Isolation cell barrier should have enable input"
            );
            break;
        }
    }

    // Convert back
    let result = AigWriter::new(&library).write(&aig);

    // Verify structure preserved
    assert_eq!(result.inputs.len(), original.inputs.len());
    assert_eq!(result.outputs.len(), original.outputs.len());
}

#[test]
fn test_barrier_prevents_cross_domain_optimization() {
    let mut aig = Aig::new("cross_domain_test".to_string());

    let a = aig.add_input("a".to_string(), None);
    let b = aig.add_input("b".to_string(), None);
    let c = aig.add_input("c".to_string(), None);

    // Domain 1: a & b
    let ab = aig.add_and(AigLit::new(a), AigLit::new(b));

    // Level shifter (domain boundary)
    let barrier = aig.add_barrier(BarrierType::LevelShifterLH, ab, None, None, None, None);

    // Domain 2: barrier & c
    let result = aig.add_and(AigLit::new(barrier), AigLit::new(c));
    aig.add_output("y".to_string(), result);

    let before_and_count = aig.and_count();
    let before_barrier_count = aig.barrier_count();

    // Run full optimization sequence
    let mut strash = passes::Strash::new();
    strash.run(&mut aig);

    let mut const_prop = passes::ConstProp::new();
    const_prop.run(&mut aig);

    let mut balance = passes::Balance::new();
    balance.run(&mut aig);

    let mut rewrite = passes::Rewrite::new();
    rewrite.run(&mut aig);

    let mut dce = passes::Dce::new();
    dce.run(&mut aig);

    // Barrier must be preserved
    assert_eq!(
        aig.barrier_count(),
        before_barrier_count,
        "Barrier should be preserved through all optimization passes"
    );

    // The AND nodes on both sides of the barrier should remain separate
    // (they shouldn't be merged across the barrier)
    assert!(
        aig.and_count() >= 2,
        "AND nodes should remain on both sides of barrier"
    );
}

#[test]
fn test_multiple_barriers_preserved() {
    let mut aig = Aig::new("multi_barrier_test".to_string());

    let a = aig.add_input("a".to_string(), None);
    let b = aig.add_input("b".to_string(), None);

    // Domain 1 -> Domain 2
    let barrier1 = aig.add_barrier(
        BarrierType::LevelShifterLH,
        AigLit::new(a),
        None,
        None,
        None,
        None,
    );

    // Domain 2 -> Domain 3
    let barrier2 = aig.add_barrier(
        BarrierType::LevelShifterHL,
        AigLit::new(barrier1),
        None,
        None,
        None,
        None,
    );

    // Logic in Domain 3
    let result = aig.add_and(AigLit::new(barrier2), AigLit::new(b));
    aig.add_output("y".to_string(), result);

    assert_eq!(aig.barrier_count(), 2);

    // Run optimization passes
    let mut strash = passes::Strash::new();
    strash.run(&mut aig);

    let mut balance = passes::Balance::new();
    balance.run(&mut aig);

    let mut dce = passes::Dce::new();
    dce.run(&mut aig);

    // Both barriers should be preserved
    assert_eq!(aig.barrier_count(), 2, "Both barriers should be preserved");
}

#[test]
fn test_barrier_types() {
    let mut aig = Aig::new("barrier_types_test".to_string());

    let a = aig.add_input("a".to_string(), None);
    let en = aig.add_input("en".to_string(), None);

    // Test different barrier types
    let lvl_lh = aig.add_barrier(
        BarrierType::LevelShifterLH,
        AigLit::new(a),
        None,
        None,
        None,
        None,
    );

    let lvl_hl = aig.add_barrier(
        BarrierType::LevelShifterHL,
        AigLit::new(lvl_lh),
        None,
        None,
        None,
        None,
    );

    let iso = aig.add_barrier(
        BarrierType::IsolationAnd,
        AigLit::new(lvl_hl),
        Some(AigLit::new(en)),
        None,
        None,
        None,
    );

    let aon = aig.add_barrier(
        BarrierType::AlwaysOnBuf,
        AigLit::new(iso),
        None,
        None,
        None,
        None,
    );

    aig.add_output("y".to_string(), AigLit::new(aon));

    // Verify all 4 barriers are created
    assert_eq!(aig.barrier_count(), 4);

    // Run passes and verify all are preserved
    let mut dce = passes::Dce::new();
    dce.run(&mut aig);

    assert_eq!(
        aig.barrier_count(),
        4,
        "All barrier types should be preserved"
    );
}
