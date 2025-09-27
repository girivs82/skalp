//! Tests for Clock Domain Crossing (CDC) detection

use skalp_mir::*;
use skalp_frontend::hir::{self as hir, *};

/// Create a test HIR with CDC violations
fn create_cdc_test_hir() -> Hir {
    let mut hir = Hir {
        name: "cdc_test_design".to_string(),
        entities: vec![],
        implementations: vec![],
        protocols: vec![],
        intents: vec![],
        requirements: vec![],
    };

    // Create entity with multiple clock domains
    let entity = HirEntity {
        id: EntityId(1),
        name: "CdcTestModule".to_string(),
        generics: vec![],
        clock_domains: vec![
            HirClockDomain {
                id: hir::ClockDomainId(1),
                name: "clk1".to_string(),
            },
            HirClockDomain {
                id: hir::ClockDomainId(2),
                name: "clk2".to_string(),
            },
        ],
        ports: vec![
            HirPort {
                id: PortId(1),
                name: "clk1".to_string(),
                direction: HirPortDirection::Input,
                port_type: HirType::Clock(Some(hir::ClockDomainId(1))),
            },
            HirPort {
                id: PortId(2),
                name: "clk2".to_string(),
                direction: HirPortDirection::Input,
                port_type: HirType::Clock(Some(hir::ClockDomainId(2))),
            },
            HirPort {
                id: PortId(3),
                name: "data_in".to_string(),
                direction: HirPortDirection::Input,
                port_type: HirType::Bit(8),
            },
            HirPort {
                id: PortId(4),
                name: "data_out".to_string(),
                direction: HirPortDirection::Output,
                port_type: HirType::Bit(8),
            },
        ],
    };
    hir.entities.push(entity);

    // Create implementation with CDC violation
    let implementation = HirImplementation {
        entity: EntityId(1),
        signals: vec![
            HirSignal {
                id: SignalId(1),
                name: "reg1".to_string(),
                signal_type: HirType::Bit(8),
                initial_value: None,
            },
            HirSignal {
                id: SignalId(2),
                name: "reg2".to_string(),
                signal_type: HirType::Bit(8),
                initial_value: None,
            },
        ],
        variables: vec![],
        constants: vec![],
        event_blocks: vec![
            // Process in clk1 domain
            HirEventBlock {
                id: BlockId(1),
                triggers: vec![
                    HirEventTrigger {
                        signal: SignalId(101), // clk1 port
                        edge: HirEdgeType::Rising,
                    },
                ],
                statements: vec![
                    HirStatement::Assignment(HirAssignment {
                        id: AssignmentId(1),
                        lhs: HirLValue::Signal(SignalId(1)), // reg1
                        rhs: HirExpression::Signal(SignalId(103)), // data_in port
                        assignment_type: HirAssignmentType::NonBlocking,
                    }),
                ],
            },
            // Process in clk2 domain - CDC violation!
            HirEventBlock {
                id: BlockId(2),
                triggers: vec![
                    HirEventTrigger {
                        signal: SignalId(102), // clk2 port
                        edge: HirEdgeType::Rising,
                    },
                ],
                statements: vec![
                    HirStatement::Assignment(HirAssignment {
                        id: AssignmentId(2),
                        lhs: HirLValue::Signal(SignalId(2)), // reg2
                        rhs: HirExpression::Signal(SignalId(1)), // reg1 from clk1 domain!
                        assignment_type: HirAssignmentType::NonBlocking,
                    }),
                ],
            },
        ],
        assignments: vec![
            // Output assignment
            HirAssignment {
                id: AssignmentId(3),
                lhs: HirLValue::Signal(SignalId(104)), // data_out port
                rhs: HirExpression::Signal(SignalId(2)), // reg2
                assignment_type: HirAssignmentType::Combinational,
            },
        ],
        instances: vec![],
    };
    hir.implementations.push(implementation);

    hir
}

#[test]
fn test_cdc_detection() {
    let hir = create_cdc_test_hir();

    // Transform HIR to MIR
    let mut transformer = HirToMir::new();
    let mir = transformer.transform(&hir);

    // Analyze for CDC violations
    let mut cdc_analyzer = CdcAnalyzer::new();
    let violations = cdc_analyzer.analyze(&mir);

    // We should detect at least one CDC violation
    assert!(!violations.is_empty(), "Expected to find CDC violations");

    // Print violations for debugging
    for violation in &violations {
        println!("CDC Violation: {:?}", violation.violation_type);
        println!("  Location: {}", violation.location.module_name);
        println!("  Severity: {:?}", violation.severity);
        println!("  Suggestion: {}", violation.suggestion);
        println!();
    }

    // Check that we found the expected violation type
    let has_unsync_crossing = violations.iter().any(|v| {
        matches!(v.violation_type, CdcViolationType::UnsynchronizedCrossing { .. })
    });

    assert!(has_unsync_crossing, "Expected to find unsynchronized crossing violation");
}

#[test]
fn test_cdc_analyzer_creation() {
    let analyzer = CdcAnalyzer::new();
    assert_eq!(analyzer.analyze(&Mir::new("test".to_string())).len(), 0);
}