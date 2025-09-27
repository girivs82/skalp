use skalp_verify::*;
use skalp_verify::assertions::*;
use skalp_verify::coverage::*;
use skalp_verify::properties::*;
use skalp_verify::requirements::*;
use skalp_verify::testbench;
use skalp_mir::mir::{Expression, Value};

#[test]
fn test_immediate_assertions() {
    let assertion = Assertion {
        id: "test_imm".to_string(),
        kind: AssertionKind::Immediate(ImmediateAssertion {
            condition: Expression::Literal(Value::Integer(1)),
            timing: AssertionTiming::Always,
            action: FailureAction::Error,
        }),
        location: SourceLocation {
            file: "test.sv".to_string(),
            line: 10,
            column: 5,
        },
        message: Some("Test assertion".to_string()),
        severity: Severity::Error,
    };

    let mut checker = AssertionChecker::new();
    checker.add_assertion(assertion.clone());

    let result = checker.check_immediate(
        match &assertion.kind {
            AssertionKind::Immediate(imm) => imm,
            _ => panic!("Expected immediate assertion"),
        },
        100,
    );

    assert!(result);
}

#[test]
fn test_property_builder() {
    let property = PropertyBuilder::new("safety_prop")
        .expression(PropertyExpr::Atom("valid".to_string()))
        .clock("clk")
        .reset("rst")
        .safety()
        .build()
        .unwrap();

    assert_eq!(property.name, "safety_prop");
    assert_eq!(property.clock, Some("clk".to_string()));
    assert!(matches!(property.kind, PropertyKind::Safety));
}

#[test]
fn test_sequence_builder() {
    let sequence = SequenceBuilder::new("req_ack_seq")
        .expr("req")
        .delay(1)
        .expr("ack")
        .build();

    assert_eq!(sequence.name, "req_ack_seq");
    assert_eq!(sequence.elements.len(), 3);
}

#[test]
fn test_coverage_collection() {
    let config = CoverageConfig {
        statement: true,
        branch: true,
        condition: true,
        fsm: true,
        toggle: true,
        cross: false,
        goal: 80.0,
    };

    let mut coverage = Coverage::new(config);

    // Record some coverage
    coverage.record_statement(StatementId(0));
    coverage.record_statement(StatementId(1));
    coverage.record_branch(BranchId(0), true);
    coverage.record_branch(BranchId(0), false);

    let metrics = coverage.get_metrics();
    assert!(metrics.overall >= 0.0);
}

#[test]
fn test_requirements_tracking() {
    let mut tracker = RequirementTracker::new();

    let req = Requirement {
        id: "REQ-001".to_string(),
        description: "System shall support reset".to_string(),
        priority: Priority::Critical,
        req_type: RequirementType::Functional,
        parent: None,
        children: vec![],
        criteria: vec![
            VerificationCriterion {
                description: "Reset clears all registers".to_string(),
                method: VerificationMethod::Simulation,
                pass_criteria: "All registers == 0 after reset".to_string(),
                status: CriterionStatus::NotTested,
            },
        ],
        properties: vec!["reset_prop".to_string()],
        tests: vec!["test_reset".to_string()],
        status: RequirementStatus::Implemented,
        notes: vec![],
    };

    tracker.add_requirement(req);

    let retrieved = tracker.get_requirement("REQ-001").unwrap();
    assert_eq!(retrieved.id, "REQ-001");

    let coverage = tracker.check_coverage("REQ-001");
    assert_eq!(coverage.total_criteria, 1);
    assert!(coverage.properties_covered);
    assert!(coverage.tests_covered);
}

#[test]
fn test_requirement_hierarchy() {
    let mut tracker = RequirementTracker::new();

    let parent = Requirement {
        id: "REQ-100".to_string(),
        description: "High-level requirement".to_string(),
        priority: Priority::High,
        req_type: RequirementType::Functional,
        parent: None,
        children: vec!["REQ-101".to_string()],
        criteria: vec![],
        properties: vec![],
        tests: vec![],
        status: RequirementStatus::InProgress,
        notes: vec![],
    };

    let child = Requirement {
        id: "REQ-101".to_string(),
        description: "Derived requirement".to_string(),
        priority: Priority::High,
        req_type: RequirementType::Functional,
        parent: Some("REQ-100".to_string()),
        children: vec![],
        criteria: vec![],
        properties: vec![],
        tests: vec![],
        status: RequirementStatus::Verified,
        notes: vec![],
    };

    tracker.add_requirement(parent);
    tracker.add_requirement(child);

    let children = tracker.get_children("REQ-100");
    assert_eq!(children.len(), 1);
    assert_eq!(children[0].id, "REQ-101");
}

#[test]
fn test_verification_report() {
    let mut tracker = RequirementTracker::new();

    // Add various requirements
    for i in 0..5 {
        let req = Requirement {
            id: format!("REQ-{:03}", i),
            description: format!("Requirement {}", i),
            priority: if i == 0 { Priority::Critical } else { Priority::Medium },
            req_type: RequirementType::Functional,
            parent: None,
            children: vec![],
            criteria: vec![],
            properties: vec![],
            tests: vec![],
            status: if i < 3 {
                RequirementStatus::Verified
            } else {
                RequirementStatus::Implemented
            },
            notes: vec![],
        };
        tracker.add_requirement(req);
    }

    let report = tracker.generate_report();
    assert_eq!(report.total_requirements, 5);
    assert_eq!(report.coverage_percentage, 60.0); // 3 out of 5 verified
}

#[test]
fn test_testbench_builder() {
    let clock = testbench::Signal::new(false);
    let reset = testbench::Signal::new(true);

    let _tb = testbench::TestbenchBuilder::new("test_tb")
        .clock_period(10)
        .timeout(1000)
        .initial(testbench::ClockGenerator::new(clock.clone(), 10))
        .build();

    // Can't directly access private fields, but test compiles
}

#[test]
fn test_signal_operations() {
    let signal = testbench::Signal::new(false);
    assert!(!signal.value());

    signal.set(true);
    assert!(signal.value());

    signal.toggle();
    assert!(!signal.value());
}

#[test]
fn test_property_evaluator() {
    let mut evaluator = PropertyEvaluator::new();

    let property = Property {
        name: "test_prop".to_string(),
        expression: PropertyExpr::Atom("signal_a".to_string()),
        clock: Some("clk".to_string()),
        reset: Some("rst".to_string()),
        kind: PropertyKind::Safety,
    };

    evaluator.add_property(property);
    evaluator.update_signal("signal_a", true);
    evaluator.step();

    let report = evaluator.get_report();
    assert_eq!(report.total, 1);
}

#[test]
fn test_coverage_report() {
    let config = CoverageConfig::default();
    let coverage = Coverage::new(config);
    
    let report = coverage.generate_report();
    assert_eq!(report.goal, 90.0);
    assert!(!report.goal_met); // No coverage yet
}

#[test]
fn test_assertion_report() {
    let checker = AssertionChecker::new();
    let report = checker.get_report();
    
    assert_eq!(report.total_assertions, 0);
    assert_eq!(report.passed, 0);
    assert_eq!(report.failed, 0);
}

#[test]
fn test_traceability_report() {
    let tracker = RequirementTracker::new();
    let report = tracker.generate_traceability_report();
    
    assert_eq!(report.orphan_requirements.len(), 0);
    assert_eq!(report.untested_requirements.len(), 0);
}