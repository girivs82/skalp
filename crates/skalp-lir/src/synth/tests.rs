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
