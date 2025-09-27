//! Tests for LIR generation and optimization

use skalp_lir::*;
use skalp_lir::lir::{Gate, GateType, Lir, Net};
use skalp_lir::optimization::*;
use skalp_lir::timing::TimingAnalyzer;

#[test]
fn test_lir_creation() {
    let mut lir = Lir::new("test_module".to_string());

    // Add some nets
    lir.nets.push(Net {
        id: "a".to_string(),
        width: 1,
        driver: Some("gate1".to_string()),
        loads: vec!["gate2".to_string()],
    });

    lir.nets.push(Net {
        id: "b".to_string(),
        width: 1,
        driver: Some("gate1".to_string()),
        loads: vec!["gate2".to_string()],
    });

    lir.nets.push(Net {
        id: "out".to_string(),
        width: 1,
        driver: Some("gate2".to_string()),
        loads: vec![],
    });

    // Add gates
    lir.gates.push(Gate {
        id: "gate1".to_string(),
        gate_type: GateType::And,
        inputs: vec!["a".to_string(), "b".to_string()],
        outputs: vec!["out".to_string()],
    });

    assert_eq!(lir.name, "test_module");
    assert_eq!(lir.gates.len(), 1);
    assert_eq!(lir.nets.len(), 3);
}

#[test]
fn test_constant_folding() {
    let mut lir = Lir::new("const_test".to_string());

    // Create a gate with constant inputs
    lir.gates.push(Gate {
        id: "and1".to_string(),
        gate_type: GateType::And,
        inputs: vec!["tie_high".to_string(), "tie_low".to_string()],
        outputs: vec!["out".to_string()],
    });

    let mut optimizer = ConstantFolding;
    let result = optimizer.optimize(&mut lir);

    assert!(result.success);
    assert_eq!(result.gates_after, 0); // Gate should be removed
}

#[test]
fn test_dead_code_elimination() {
    let mut lir = Lir::new("dce_test".to_string());

    // Add output net
    lir.nets.push(Net {
        id: "out_result".to_string(),
        width: 1,
        driver: Some("gate1".to_string()),
        loads: vec![],
    });

    // Add intermediate net
    lir.nets.push(Net {
        id: "temp".to_string(),
        width: 1,
        driver: Some("gate2".to_string()),
        loads: vec![],
    });

    // Live gate (drives output)
    lir.gates.push(Gate {
        id: "gate1".to_string(),
        gate_type: GateType::Buffer,
        inputs: vec!["in".to_string()],
        outputs: vec!["out_result".to_string()],
    });

    // Dead gate (doesn't drive output)
    lir.gates.push(Gate {
        id: "gate2".to_string(),
        gate_type: GateType::Not,
        inputs: vec!["in".to_string()],
        outputs: vec!["temp".to_string()],
    });

    let initial_gates = lir.gates.len();

    let mut optimizer = DeadCodeElimination;
    let result = optimizer.optimize(&mut lir);

    assert!(result.success);
    assert_eq!(result.gates_before, initial_gates);
    assert_eq!(result.gates_after, 1); // Only live gate remains
}

#[test]
fn test_common_subexpression_elimination() {
    let mut lir = Lir::new("cse_test".to_string());

    // Two identical AND gates
    lir.gates.push(Gate {
        id: "and1".to_string(),
        gate_type: GateType::And,
        inputs: vec!["a".to_string(), "b".to_string()],
        outputs: vec!["out1".to_string()],
    });

    lir.gates.push(Gate {
        id: "and2".to_string(),
        gate_type: GateType::And,
        inputs: vec!["a".to_string(), "b".to_string()],
        outputs: vec!["out2".to_string()],
    });

    let mut optimizer = CommonSubexpressionElimination;
    let result = optimizer.optimize(&mut lir);

    assert!(result.success);
    assert_eq!(result.gates_before, 2);
    assert_eq!(result.gates_after, 1); // One duplicate removed
}

#[test]
fn test_boolean_simplification() {
    let mut lir = Lir::new("bool_test".to_string());

    // Double negation
    lir.gates.push(Gate {
        id: "not1".to_string(),
        gate_type: GateType::Not,
        inputs: vec!["a".to_string()],
        outputs: vec!["temp".to_string()],
    });

    lir.gates.push(Gate {
        id: "not2".to_string(),
        gate_type: GateType::Not,
        inputs: vec!["temp".to_string()],
        outputs: vec!["out".to_string()],
    });

    let mut optimizer = BooleanSimplification;
    let result = optimizer.optimize(&mut lir);

    assert!(result.success);
    // Double negation should be simplified to a buffer
    assert!(lir.gates.iter().any(|g| g.gate_type == GateType::Buffer));
}

#[test]
fn test_optimization_pipeline() {
    let mut lir = Lir::new("pipeline_test".to_string());

    // Add various gates for optimization
    lir.gates.push(Gate {
        id: "const_and".to_string(),
        gate_type: GateType::And,
        inputs: vec!["tie_low".to_string(), "a".to_string()],
        outputs: vec!["const_out".to_string()],
    });

    lir.gates.push(Gate {
        id: "duplicate1".to_string(),
        gate_type: GateType::Or,
        inputs: vec!["b".to_string(), "c".to_string()],
        outputs: vec!["dup_out1".to_string()],
    });

    lir.gates.push(Gate {
        id: "duplicate2".to_string(),
        gate_type: GateType::Or,
        inputs: vec!["b".to_string(), "c".to_string()],
        outputs: vec!["dup_out2".to_string()],
    });

    let initial_gates = lir.gates.len();

    let mut pipeline = OptimizationPipeline::standard();
    let results = pipeline.optimize(&mut lir);

    assert!(!results.is_empty());
    assert!(lir.gates.len() < initial_gates);
}

#[test]
fn test_timing_analysis() {
    let mut lir = Lir::new("timing_test".to_string());

    // Create a simple path
    lir.nets.push(Net {
        id: "in_a".to_string(),
        width: 1,
        driver: None,
        loads: vec!["gate1".to_string()],
    });

    lir.nets.push(Net {
        id: "temp".to_string(),
        width: 1,
        driver: Some("gate1".to_string()),
        loads: vec!["gate2".to_string()],
    });

    lir.nets.push(Net {
        id: "out_result".to_string(),
        width: 1,
        driver: Some("gate2".to_string()),
        loads: vec![],
    });

    lir.gates.push(Gate {
        id: "gate1".to_string(),
        gate_type: GateType::Not,
        inputs: vec!["in_a".to_string()],
        outputs: vec!["temp".to_string()],
    });

    lir.gates.push(Gate {
        id: "gate2".to_string(),
        gate_type: GateType::Buffer,
        inputs: vec!["temp".to_string()],
        outputs: vec!["out_result".to_string()],
    });

    let mut analyzer = TimingAnalyzer::new(1000.0); // 1ns clock period
    analyzer.build_graph(&lir);
    let report = analyzer.analyze();

    assert!(report.critical_path_delay > 0.0);
    assert!(!report.critical_path.is_empty());

    // With 1ns period and simple gates, we shouldn't have violations
    assert!(report.setup_violations.is_empty());
}