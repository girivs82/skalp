//! Tests for reset event support

use skalp_frontend::hir::*;
use skalp_mir::mir::{EdgeType, ProcessKind, SensitivityList};
use skalp_mir::{HirToMir, SystemVerilogGenerator};

/// Create a counter HIR with reset event syntax
fn create_counter_with_reset_event() -> Hir {
    let mut hir = Hir {
        name: "reset_counter_design".to_string(),
        entities: vec![],
        implementations: vec![],
        protocols: vec![],
        intents: vec![],
        requirements: vec![],
        trait_definitions: vec![],
        trait_implementations: vec![],
        global_constraints: vec![],
    };

    // Create counter entity
    let entity = HirEntity {
        id: EntityId(1),
        name: "ResetCounter".to_string(),
        generics: vec![],
        clock_domains: vec![],
        ports: vec![
            HirPort {
                id: PortId(1),
                name: "clk".to_string(),
                direction: HirPortDirection::Input,
                port_type: HirType::Clock(None),
                physical_constraints: None,
            },
            HirPort {
                id: PortId(2),
                name: "reset".to_string(),
                direction: HirPortDirection::Input,
                port_type: HirType::Reset(None),
                physical_constraints: None,
            },
            HirPort {
                id: PortId(3),
                name: "count".to_string(),
                direction: HirPortDirection::Output,
                port_type: HirType::Bit(8),
                physical_constraints: None,
            },
        ],
    };
    hir.entities.push(entity);

    // Create implementation with reset event
    let implementation = HirImplementation {
        entity: EntityId(1),
        signals: vec![HirSignal {
            id: SignalId(1),
            name: "count_reg".to_string(),
            signal_type: HirType::Bit(8),
            clock_domain: None,
            initial_value: None,
        }],
        variables: vec![],
        constants: vec![],
        event_blocks: vec![
            // Clock event block
            HirEventBlock {
                id: BlockId(1),
                triggers: vec![HirEventTrigger {
                    signal: HirEventSignal::Signal(SignalId(101)), // clk
                    edge: HirEdgeType::Rising,
                }],
                statements: vec![HirStatement::Assignment(HirAssignment {
                    id: AssignmentId(1),
                    lhs: HirLValue::Signal(SignalId(1)),
                    rhs: HirExpression::Binary(HirBinaryExpr {
                        left: Box::new(HirExpression::Signal(SignalId(1))),
                        op: HirBinaryOp::Add,
                        right: Box::new(HirExpression::Literal(HirLiteral::Integer(1))),
                    }),
                    assignment_type: HirAssignmentType::NonBlocking,
                })],
            },
            // Reset event block (asynchronous reset)
            HirEventBlock {
                id: BlockId(2),
                triggers: vec![HirEventTrigger {
                    signal: HirEventSignal::Signal(SignalId(102)), // reset
                    edge: HirEdgeType::Active,
                }],
                statements: vec![HirStatement::Assignment(HirAssignment {
                    id: AssignmentId(2),
                    lhs: HirLValue::Signal(SignalId(1)),
                    rhs: HirExpression::Literal(HirLiteral::Integer(0)),
                    assignment_type: HirAssignmentType::NonBlocking,
                })],
            },
        ],
        assignments: vec![
            // Output assignment
            HirAssignment {
                id: AssignmentId(3),
                lhs: HirLValue::Signal(SignalId(103)), // count output
                rhs: HirExpression::Signal(SignalId(1)), // count_reg
                assignment_type: HirAssignmentType::Combinational,
            },
        ],
        instances: vec![],
        covergroups: vec![],
        formal_blocks: vec![],
    };
    hir.implementations.push(implementation);

    hir
}

/// Create a combined reset and clock event example
fn create_combined_clock_reset_event() -> Hir {
    let mut hir = Hir {
        name: "combined_event_design".to_string(),
        entities: vec![],
        implementations: vec![],
        protocols: vec![],
        intents: vec![],
        requirements: vec![],
        trait_definitions: vec![],
        trait_implementations: vec![],
        global_constraints: vec![],
    };

    // Create entity
    let entity = HirEntity {
        id: EntityId(1),
        name: "CombinedCounter".to_string(),
        generics: vec![],
        clock_domains: vec![],
        ports: vec![
            HirPort {
                id: PortId(1),
                name: "clk".to_string(),
                direction: HirPortDirection::Input,
                port_type: HirType::Clock(None),
                physical_constraints: None,
            },
            HirPort {
                id: PortId(2),
                name: "reset".to_string(),
                direction: HirPortDirection::Input,
                port_type: HirType::Reset(None),
                physical_constraints: None,
            },
            HirPort {
                id: PortId(3),
                name: "count".to_string(),
                direction: HirPortDirection::Output,
                port_type: HirType::Bit(8),
                physical_constraints: None,
            },
        ],
    };
    hir.entities.push(entity);

    // Implementation with combined clock and reset sensitivity
    let implementation = HirImplementation {
        entity: EntityId(1),
        signals: vec![HirSignal {
            id: SignalId(1),
            name: "count_reg".to_string(),
            signal_type: HirType::Bit(8),
            clock_domain: None,
            initial_value: None,
        }],
        variables: vec![],
        constants: vec![],
        event_blocks: vec![
            // Combined clock and reset event block
            HirEventBlock {
                id: BlockId(1),
                triggers: vec![
                    HirEventTrigger {
                        signal: HirEventSignal::Signal(SignalId(101)), // clk
                        edge: HirEdgeType::Rising,
                    },
                    HirEventTrigger {
                        signal: HirEventSignal::Signal(SignalId(102)), // reset
                        edge: HirEdgeType::Active,
                    },
                ],
                statements: vec![HirStatement::Assignment(HirAssignment {
                    id: AssignmentId(1),
                    lhs: HirLValue::Signal(SignalId(1)),
                    rhs: HirExpression::Binary(HirBinaryExpr {
                        left: Box::new(HirExpression::Signal(SignalId(1))),
                        op: HirBinaryOp::Add,
                        right: Box::new(HirExpression::Literal(HirLiteral::Integer(1))),
                    }),
                    assignment_type: HirAssignmentType::NonBlocking,
                })],
            },
        ],
        assignments: vec![
            // Output assignment
            HirAssignment {
                id: AssignmentId(2),
                lhs: HirLValue::Signal(SignalId(103)), // count output
                rhs: HirExpression::Signal(SignalId(1)), // count_reg
                assignment_type: HirAssignmentType::Combinational,
            },
        ],
        instances: vec![],
        covergroups: vec![],
        formal_blocks: vec![],
    };
    hir.implementations.push(implementation);

    hir
}

#[test]
#[ignore = "Test helper creates invalid HIR with mismatched signal IDs - needs fix"]
fn test_reset_event_transformation() {
    let hir = create_counter_with_reset_event();
    let mut transformer = HirToMir::new();
    let mir = transformer.transform(&hir);

    // Check that we have a module
    assert_eq!(mir.modules.len(), 1);
    let module = &mir.modules[0];

    // Should have 2 processes (one for clock, one for reset)
    assert_eq!(module.processes.len(), 2);

    // Check that both processes are sequential
    for process in &module.processes {
        assert_eq!(process.kind, ProcessKind::Sequential);
    }

    // Check that we have edge sensitivity lists
    let reset_process = module.processes.iter().find(|p| {
        if let SensitivityList::Edge(edges) = &p.sensitivity {
            edges.iter().any(|e| matches!(e.edge, EdgeType::Active))
        } else {
            false
        }
    });
    assert!(reset_process.is_some(), "Should have a reset event process");
}

#[test]
#[ignore = "Test helper creates invalid HIR with mismatched signal IDs - needs fix"]
fn test_combined_reset_clock_event() {
    let hir = create_combined_clock_reset_event();
    let mut transformer = HirToMir::new();
    let mir = transformer.transform(&hir);

    // Check that we have a module
    assert_eq!(mir.modules.len(), 1);
    let module = &mir.modules[0];

    // Should have 1 process with combined sensitivity
    assert_eq!(module.processes.len(), 1);
    let process = &module.processes[0];

    // Check that it's sequential
    assert_eq!(process.kind, ProcessKind::Sequential);

    // Check that it has both clock and reset edges
    if let SensitivityList::Edge(edges) = &process.sensitivity {
        assert_eq!(edges.len(), 2);
        let has_clock = edges.iter().any(|e| matches!(e.edge, EdgeType::Rising));
        let has_reset = edges.iter().any(|e| matches!(e.edge, EdgeType::Active));
        assert!(has_clock, "Should have clock edge");
        assert!(has_reset, "Should have reset edge");
    } else {
        panic!("Expected edge sensitivity list");
    }
}

#[test]
#[ignore = "Test helper creates invalid HIR with mismatched signal IDs - needs fix"]
fn test_reset_event_codegen() {
    let hir = create_combined_clock_reset_event();
    let mut transformer = HirToMir::new();
    let mir = transformer.transform(&hir);

    let mut generator = SystemVerilogGenerator::new();
    let verilog = generator.generate(&mir);

    // Check that the sensitivity list includes both clock and reset
    assert!(verilog.contains("always_ff @(posedge"));
    assert!(verilog.contains("or posedge")); // Should have "or posedge reset" for active-high reset

    println!("Generated reset event Verilog:\n{}", verilog);
}

#[test]
#[ignore = "Test helper creates invalid HIR with mismatched signal IDs - needs fix"]
fn test_inactive_reset_event() {
    let mut hir = create_counter_with_reset_event();

    // Change reset to inactive edge
    if let Some(impl_block) = hir.implementations.get_mut(0) {
        if let Some(event_block) = impl_block.event_blocks.get_mut(1) {
            if let Some(trigger) = event_block.triggers.get_mut(0) {
                trigger.edge = HirEdgeType::Inactive;
            }
        }
    }

    let mut transformer = HirToMir::new();
    let mir = transformer.transform(&hir);

    let mut generator = SystemVerilogGenerator::new();
    let verilog = generator.generate(&mir);

    // Should generate negedge for inactive reset
    assert!(verilog.contains("negedge"));

    println!("Generated inactive reset Verilog:\n{}", verilog);
}
