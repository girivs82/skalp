//! Tests for HIR to MIR transformation

use skalp_frontend::hir::*;
use skalp_mir::hir_to_mir::HirToMir;

/// Helper function to create a simple entity HIR
fn create_simple_entity() -> Hir {
    let mut hir = Hir {
        name: "test_module".to_string(),
        entities: vec![],
        implementations: vec![],
        protocols: vec![],
        intents: vec![],
        requirements: vec![],
    };

    // Create entity
    let entity = HirEntity {
        id: EntityId(1),
        name: "counter".to_string(),
        generics: vec![],
        ports: vec![
            HirPort {
                id: PortId(1),
                name: "clk".to_string(),
                direction: HirPortDirection::Input,
                port_type: HirType::Clock,
            },
            HirPort {
                id: PortId(2),
                name: "rst".to_string(),
                direction: HirPortDirection::Input,
                port_type: HirType::Reset,
            },
            HirPort {
                id: PortId(3),
                name: "count".to_string(),
                direction: HirPortDirection::Output,
                port_type: HirType::Bit(8),
            },
        ],
    };

    hir.entities.push(entity);

    // Create implementation
    let implementation = HirImplementation {
        entity: EntityId(1),
        signals: vec![
            HirSignal {
                id: SignalId(1),
                name: "next_count".to_string(),
                signal_type: HirType::Bit(8),
                initial_value: None,
            },
        ],
        variables: vec![],
        constants: vec![],
        event_blocks: vec![
            HirEventBlock {
                id: BlockId(1),
                triggers: vec![
                    HirEventTrigger {
                        signal: SignalId(1),  // clk (actually should be mapped to port)
                        edge: HirEdgeType::Rising,
                    },
                ],
                statements: vec![
                    HirStatement::Assignment(HirAssignment {
                        id: AssignmentId(1),
                        lhs: HirLValue::Signal(SignalId(1)),
                        rhs: HirExpression::Binary(HirBinaryExpr {
                            left: Box::new(HirExpression::Signal(SignalId(1))),
                            op: HirBinaryOp::Add,
                            right: Box::new(HirExpression::Literal(HirLiteral::Integer(1))),
                        }),
                        assignment_type: HirAssignmentType::NonBlocking,
                    }),
                ],
            },
        ],
        assignments: vec![],
        instances: vec![],
    };

    hir.implementations.push(implementation);

    hir
}

#[test]
fn test_simple_entity_conversion() {
    let hir = create_simple_entity();
    let mut transformer = HirToMir::new();
    let mir = transformer.transform(&hir);

    // Check MIR was created
    assert_eq!(mir.name, "test_module");
    assert_eq!(mir.modules.len(), 1);

    let module = &mir.modules[0];
    assert_eq!(module.name, "counter");
    assert_eq!(module.ports.len(), 3);
    assert_eq!(module.signals.len(), 1);
    assert_eq!(module.processes.len(), 1);
}

#[test]
fn test_port_conversion() {
    let hir = create_simple_entity();
    let mut transformer = HirToMir::new();
    let mir = transformer.transform(&hir);

    let module = &mir.modules[0];

    // Check clock port
    assert_eq!(module.ports[0].name, "clk");
    assert!(matches!(module.ports[0].direction, skalp_mir::mir::PortDirection::Input));
    assert!(matches!(module.ports[0].port_type, skalp_mir::mir::DataType::Clock));

    // Check reset port
    assert_eq!(module.ports[1].name, "rst");
    assert!(matches!(module.ports[1].direction, skalp_mir::mir::PortDirection::Input));
    assert!(matches!(module.ports[1].port_type, skalp_mir::mir::DataType::Reset { .. }));

    // Check output port
    assert_eq!(module.ports[2].name, "count");
    assert!(matches!(module.ports[2].direction, skalp_mir::mir::PortDirection::Output));
    assert!(matches!(module.ports[2].port_type, skalp_mir::mir::DataType::Bit(8)));
}

#[test]
fn test_process_conversion() {
    let hir = create_simple_entity();
    let mut transformer = HirToMir::new();
    let mir = transformer.transform(&hir);

    let module = &mir.modules[0];
    let process = &module.processes[0];

    // Check process type
    assert!(matches!(process.kind, skalp_mir::mir::ProcessKind::Sequential));

    // Check sensitivity list
    match &process.sensitivity {
        skalp_mir::mir::SensitivityList::Edge(edges) => {
            assert_eq!(edges.len(), 1);
            assert!(matches!(edges[0].edge, skalp_mir::mir::EdgeType::Rising));
        }
        _ => panic!("Expected edge sensitivity"),
    }
}