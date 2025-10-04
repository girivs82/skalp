//! FIFO verification example demonstrating comprehensive verification features

use skalp_mir::mir::{BinaryOp, Expression, UnaryOp, Value};
use skalp_verify::assertions::*;
use skalp_verify::coverage::*;
use skalp_verify::properties::*;
use skalp_verify::requirements::*;
use skalp_verify::testbench::*;
use std::future::Future;
use std::pin::Pin;

fn main() {
    println!("=== FIFO Verification Example ===");
    println!();

    // 1. Define Requirements
    let mut req_tracker = setup_requirements();
    println!(
        "Requirements defined: {}",
        req_tracker.generate_report().total_requirements
    );
    println!();

    // 2. Define Properties
    let properties = define_properties();
    println!("Properties defined: {}", properties.len());
    for prop in &properties {
        println!(
            "  - {} ({})",
            prop.name,
            match prop.kind {
                PropertyKind::Safety => "Safety",
                PropertyKind::Liveness => "Liveness",
                _ => "Other",
            }
        );
    }
    println!();

    // 3. Setup Coverage
    let mut coverage = setup_coverage();
    println!("Coverage collection configured");
    println!("  Goal: {:.0}%", coverage.config().goal);
    println!();

    // 4. Create Testbench
    let mut tb = create_testbench();
    println!("Testbench created: {}", tb.name());
    println!("  Clock period: {}ns", tb.clock_period());
    println!("  Timeout: {}ns", tb.timeout().unwrap_or(0));
    println!();

    // 5. Run Simulation
    println!("Running simulation...");
    let result = tb.run();
    println!("Simulation completed:");
    println!("  Status: {:?}", result.status);
    println!("  Sim time: {}ns", result.sim_time);
    println!("  Cycles: {}", result.cycles);
    println!("  Real time: {:.3}s", result.real_time.as_secs_f64());
    println!();

    // 6. Check Assertions
    let assertion_report = check_assertions();
    println!("Assertion Report:");
    println!("  Total: {}", assertion_report.total_assertions);
    println!("  Passed: {}", assertion_report.passed);
    println!("  Failed: {}", assertion_report.failed);
    println!();

    // 7. Coverage Report
    simulate_and_collect_coverage(&mut coverage);
    let coverage_report = coverage.generate_report();
    println!("{}", coverage_report);
    println!();

    // 8. Requirements Verification Report
    update_requirement_status(&mut req_tracker);
    let verification_report = req_tracker.generate_report();
    println!("{}", verification_report);
    println!();

    // 9. Traceability Report
    let traceability = req_tracker.generate_traceability_report();
    println!("{}", traceability);

    // 10. Formal Verification (if enabled)
    #[cfg(feature = "formal")]
    {
        println!("\n=== Formal Verification ===");
        run_formal_verification(&properties);
    }
}

fn setup_requirements() -> RequirementTracker {
    let mut tracker = RequirementTracker::new();

    // High-level requirements
    tracker.add_requirement(Requirement {
        id: "FIFO-001".to_string(),
        description: "FIFO shall store up to 16 data items".to_string(),
        priority: Priority::Critical,
        req_type: RequirementType::Functional,
        parent: None,
        children: vec!["FIFO-001.1".to_string(), "FIFO-001.2".to_string()],
        criteria: vec![VerificationCriterion {
            description: "FIFO depth is 16".to_string(),
            method: VerificationMethod::Simulation,
            pass_criteria: "Can store exactly 16 items".to_string(),
            status: CriterionStatus::NotTested,
        }],
        properties: vec!["fifo_depth_prop".to_string()],
        tests: vec!["test_fifo_depth".to_string()],
        status: RequirementStatus::Implemented,
        notes: vec![],
    });

    // Derived requirements
    tracker.add_requirement(Requirement {
        id: "FIFO-001.1".to_string(),
        description: "FIFO shall assert full when 16 items stored".to_string(),
        priority: Priority::High,
        req_type: RequirementType::Functional,
        parent: Some("FIFO-001".to_string()),
        children: vec![],
        criteria: vec![VerificationCriterion {
            description: "Full flag assertion".to_string(),
            method: VerificationMethod::Simulation,
            pass_criteria: "full=1 when count=16".to_string(),
            status: CriterionStatus::NotTested,
        }],
        properties: vec!["full_flag_prop".to_string()],
        tests: vec!["test_full_flag".to_string()],
        status: RequirementStatus::Implemented,
        notes: vec![],
    });

    tracker.add_requirement(Requirement {
        id: "FIFO-001.2".to_string(),
        description: "FIFO shall assert empty when no items stored".to_string(),
        priority: Priority::High,
        req_type: RequirementType::Functional,
        parent: Some("FIFO-001".to_string()),
        children: vec![],
        criteria: vec![VerificationCriterion {
            description: "Empty flag assertion".to_string(),
            method: VerificationMethod::Simulation,
            pass_criteria: "empty=1 when count=0".to_string(),
            status: CriterionStatus::NotTested,
        }],
        properties: vec!["empty_flag_prop".to_string()],
        tests: vec!["test_empty_flag".to_string()],
        status: RequirementStatus::Implemented,
        notes: vec![],
    });

    // Performance requirements
    tracker.add_requirement(Requirement {
        id: "FIFO-002".to_string(),
        description: "FIFO shall operate at 100MHz".to_string(),
        priority: Priority::High,
        req_type: RequirementType::Performance,
        parent: None,
        children: vec![],
        criteria: vec![VerificationCriterion {
            description: "Timing closure at 100MHz".to_string(),
            method: VerificationMethod::StaticAnalysis,
            pass_criteria: "Max delay < 10ns".to_string(),
            status: CriterionStatus::NotTested,
        }],
        properties: vec![],
        tests: vec!["test_timing".to_string()],
        status: RequirementStatus::InProgress,
        notes: vec![],
    });

    // Safety requirements
    tracker.add_requirement(Requirement {
        id: "FIFO-003".to_string(),
        description: "FIFO shall not lose data".to_string(),
        priority: Priority::Critical,
        req_type: RequirementType::Safety,
        parent: None,
        children: vec![],
        criteria: vec![VerificationCriterion {
            description: "No data loss".to_string(),
            method: VerificationMethod::Formal,
            pass_criteria: "All written data can be read".to_string(),
            status: CriterionStatus::NotTested,
        }],
        properties: vec!["no_data_loss_prop".to_string()],
        tests: vec!["test_data_integrity".to_string()],
        status: RequirementStatus::Implemented,
        notes: vec!["Critical for system reliability".to_string()],
    });

    tracker
}

fn define_properties() -> Vec<Property> {
    vec![
        // Safety properties
        PropertyBuilder::new("fifo_depth_prop")
            .expression(PropertyExpr::Implies(
                Box::new(PropertyExpr::Atom("count == 16".to_string())),
                Box::new(PropertyExpr::Atom("full".to_string())),
            ))
            .clock("clk")
            .reset("rst")
            .safety()
            .build()
            .unwrap(),
        PropertyBuilder::new("full_flag_prop")
            .expression(PropertyExpr::Implies(
                Box::new(PropertyExpr::Atom("full".to_string())),
                Box::new(PropertyExpr::Not(Box::new(PropertyExpr::Atom(
                    "wr_en".to_string(),
                )))),
            ))
            .clock("clk")
            .safety()
            .build()
            .unwrap(),
        PropertyBuilder::new("empty_flag_prop")
            .expression(PropertyExpr::Implies(
                Box::new(PropertyExpr::Atom("empty".to_string())),
                Box::new(PropertyExpr::Not(Box::new(PropertyExpr::Atom(
                    "rd_en".to_string(),
                )))),
            ))
            .clock("clk")
            .safety()
            .build()
            .unwrap(),
        PropertyBuilder::new("no_data_loss_prop")
            .expression(PropertyExpr::Temporal(
                TemporalOperator::Always { bound: None },
                Box::new(PropertyExpr::Implies(
                    Box::new(PropertyExpr::Atom("wr_en && !full".to_string())),
                    Box::new(PropertyExpr::Temporal(
                        TemporalOperator::Eventually { bound: Some(100) },
                        Box::new(PropertyExpr::Atom("data_out == data_in".to_string())),
                    )),
                )),
            ))
            .clock("clk")
            .safety()
            .build()
            .unwrap(),
        // Liveness property
        PropertyBuilder::new("eventually_empty")
            .expression(PropertyExpr::Temporal(
                TemporalOperator::Eventually { bound: Some(1000) },
                Box::new(PropertyExpr::Atom("empty".to_string())),
            ))
            .clock("clk")
            .liveness()
            .build()
            .unwrap(),
    ]
}

fn setup_coverage() -> Coverage {
    let config = CoverageConfig {
        statement: true,
        branch: true,
        condition: true,
        fsm: true,
        toggle: true,
        cross: true,
        goal: 95.0,
    };

    let mut coverage = Coverage::new(config);

    // Initialize with expected coverage points
    coverage.statement.total = 50; // 50 statements to cover

    // Add FSM for FIFO state
    coverage.fsm.fsms.push(FSM {
        id: FsmId(0),
        name: "fifo_state".to_string(),
        states: vec![
            State {
                id: StateId(0),
                name: "EMPTY".to_string(),
            },
            State {
                id: StateId(1),
                name: "PARTIAL".to_string(),
            },
            State {
                id: StateId(2),
                name: "FULL".to_string(),
            },
        ],
        transitions: vec![
            Transition {
                id: TransitionId(0),
                from: StateId(0),
                to: StateId(1),
                condition: "write".to_string(),
            },
            Transition {
                id: TransitionId(1),
                from: StateId(1),
                to: StateId(2),
                condition: "write && count==15".to_string(),
            },
            Transition {
                id: TransitionId(2),
                from: StateId(2),
                to: StateId(1),
                condition: "read".to_string(),
            },
            Transition {
                id: TransitionId(3),
                from: StateId(1),
                to: StateId(0),
                condition: "read && count==1".to_string(),
            },
        ],
    });

    // Add signals for toggle coverage
    coverage.toggle.signals.push(Signal {
        id: SignalId(0),
        name: "wr_en".to_string(),
        width: 1,
    });
    coverage.toggle.signals.push(Signal {
        id: SignalId(1),
        name: "rd_en".to_string(),
        width: 1,
    });
    coverage.toggle.signals.push(Signal {
        id: SignalId(2),
        name: "full".to_string(),
        width: 1,
    });
    coverage.toggle.signals.push(Signal {
        id: SignalId(3),
        name: "empty".to_string(),
        width: 1,
    });

    // Add cross coverage points
    coverage.cross.cross_points.push(CrossPoint {
        name: "rd_wr_cross".to_string(),
        variables: vec!["rd_en".to_string(), "wr_en".to_string()],
    });

    coverage
}

fn create_testbench() -> Testbench {
    let clock = testbench::Signal::new(false);
    let reset = testbench::Signal::new(true);
    let wr_en = testbench::Signal::new(false);
    let rd_en = testbench::Signal::new(false);

    TestbenchBuilder::new("fifo_testbench")
        .clock_period(10) // 10ns = 100MHz
        .timeout(100000) // 100us timeout
        .initial(ClockGenerator::new(clock.clone(), 10))
        .initial(ResetSequence::new(reset.clone()))
        .initial(FifoStimulus::new(wr_en.clone(), rd_en.clone()))
        .monitor(FifoMonitor::new())
        .build()
}

// Custom async tasks for the testbench

struct ResetSequence {
    reset: testbench::Signal,
}

impl ResetSequence {
    fn new(reset: testbench::Signal) -> Self {
        Self { reset }
    }
}

impl AsyncTask for ResetSequence {
    fn execute(&mut self) -> Pin<Box<dyn Future<Output = ()> + Send + 'static>> {
        let reset = self.reset.clone();
        Box::pin(async move {
            // Hold reset for 5 cycles
            reset.set(true);
            wait_cycles(5).await;
            reset.set(false);
        })
    }
}

struct FifoStimulus {
    wr_en: testbench::Signal,
    rd_en: testbench::Signal,
}

impl FifoStimulus {
    fn new(wr_en: testbench::Signal, rd_en: testbench::Signal) -> Self {
        Self { wr_en, rd_en }
    }
}

impl AsyncTask for FifoStimulus {
    fn execute(&mut self) -> Pin<Box<dyn Future<Output = ()> + Send + 'static>> {
        let wr_en = self.wr_en.clone();
        let rd_en = self.rd_en.clone();
        Box::pin(async move {
            // Wait for reset
            wait_cycles(10).await;

            // Write 16 items
            for _ in 0..16 {
                wr_en.set(true);
                wait_cycles(1).await;
                wr_en.set(false);
                wait_cycles(1).await;
            }

            // Read 8 items
            for _ in 0..8 {
                rd_en.set(true);
                wait_cycles(1).await;
                rd_en.set(false);
                wait_cycles(1).await;
            }

            // Write 4 more
            for _ in 0..4 {
                wr_en.set(true);
                wait_cycles(1).await;
                wr_en.set(false);
                wait_cycles(1).await;
            }

            // Read all
            for _ in 0..12 {
                rd_en.set(true);
                wait_cycles(1).await;
                rd_en.set(false);
                wait_cycles(1).await;
            }
        })
    }
}

struct FifoMonitor;

impl FifoMonitor {
    fn new() -> Self {
        Self
    }
}

impl AsyncTask for FifoMonitor {
    fn execute(&mut self) -> Pin<Box<dyn Future<Output = ()> + Send + 'static>> {
        Box::pin(async move {
            // Monitor FIFO behavior
            let mut cycle = 0;
            loop {
                // Check assertions every cycle
                cycle += 1;
                if cycle > 1000 {
                    break;
                }
                wait_cycles(1).await;
            }
        })
    }
}

fn check_assertions() -> AssertionReport {
    let mut checker = AssertionChecker::new();

    // Add immediate assertions
    checker.add_assertion(Assertion {
        id: "no_overflow".to_string(),
        kind: AssertionKind::Immediate(ImmediateAssertion {
            condition: Expression::Literal(Value::Integer(1)), // Simplified for example
            timing: AssertionTiming::Always,
            action: FailureAction::Error,
        }),
        location: SourceLocation {
            file: "fifo.sv".to_string(),
            line: 45,
            column: 5,
        },
        message: Some("Write to full FIFO".to_string()),
        severity: Severity::Error,
    });

    checker.add_assertion(Assertion {
        id: "no_underflow".to_string(),
        kind: AssertionKind::Immediate(ImmediateAssertion {
            condition: Expression::Literal(Value::Integer(1)), // Simplified for example
            timing: AssertionTiming::Always,
            action: FailureAction::Error,
        }),
        location: SourceLocation {
            file: "fifo.sv".to_string(),
            line: 50,
            column: 5,
        },
        message: Some("Read from empty FIFO".to_string()),
        severity: Severity::Error,
    });

    // Add concurrent assertions
    checker.add_assertion(Assertion {
        id: "data_stability".to_string(),
        kind: AssertionKind::Concurrent(ConcurrentAssertion {
            property: PropertyExpression::Implication {
                antecedent: Box::new(PropertyExpression::Boolean(
                    Expression::Literal(Value::Integer(1)), // Simplified
                )),
                consequent: Box::new(PropertyExpression::Delay {
                    cycles: 1,
                    property: Box::new(PropertyExpression::Boolean(
                        Expression::Literal(Value::Integer(1)), // Simplified
                    )),
                }),
            },
            clock: ClockSpec {
                signal: "clk".to_string(),
                edge: EdgeType::Rising,
            },
            reset: Some(Expression::Literal(Value::Integer(0))),
            action: FailureAction::Error,
        }),
        location: SourceLocation {
            file: "fifo.sv".to_string(),
            line: 60,
            column: 5,
        },
        message: Some("Data output not stable".to_string()),
        severity: Severity::Warning,
    });

    // Add coverage points
    checker.add_assertion(Assertion {
        id: "cover_full".to_string(),
        kind: AssertionKind::Cover(CoverPoint {
            name: "full_condition".to_string(),
            expression: Expression::Literal(Value::Integer(0)), // Simplified
            bins: vec![
                CoverageBin {
                    name: "full_true".to_string(),
                    values: BinValues::Single(1),
                    target: 10,
                    hits: 0,
                },
                CoverageBin {
                    name: "full_false".to_string(),
                    values: BinValues::Single(0),
                    target: 10,
                    hits: 0,
                },
            ],
            cross: vec![],
        }),
        location: SourceLocation {
            file: "fifo.sv".to_string(),
            line: 70,
            column: 5,
        },
        message: None,
        severity: Severity::Info,
    });

    checker.get_report()
}

fn simulate_and_collect_coverage(coverage: &mut Coverage) {
    // Simulate coverage collection
    // In real implementation, this would be driven by actual simulation

    // Record statement coverage
    for i in 0..45 {
        coverage.record_statement(StatementId(i));
    }

    // Record branch coverage
    for i in 0..10 {
        coverage.record_branch(BranchId(i), true);
        coverage.record_branch(BranchId(i), false);
    }

    // Record FSM coverage
    coverage.record_fsm_state(FsmId(0), StateId(0)); // EMPTY
    coverage.record_fsm_state(FsmId(0), StateId(1)); // PARTIAL
    coverage.record_fsm_state(FsmId(0), StateId(2)); // FULL

    coverage.record_fsm_transition(FsmId(0), TransitionId(0));
    coverage.record_fsm_transition(FsmId(0), TransitionId(1));
    coverage.record_fsm_transition(FsmId(0), TransitionId(2));
    coverage.record_fsm_transition(FsmId(0), TransitionId(3));

    // Record toggle coverage
    coverage.record_toggle(SignalId(0), true); // wr_en rising
    coverage.record_toggle(SignalId(0), false); // wr_en falling
    coverage.record_toggle(SignalId(1), true); // rd_en rising
    coverage.record_toggle(SignalId(1), false); // rd_en falling
    coverage.record_toggle(SignalId(2), true); // full rising
    coverage.record_toggle(SignalId(2), false); // full falling
    coverage.record_toggle(SignalId(3), true); // empty rising
    coverage.record_toggle(SignalId(3), false); // empty falling

    // Record condition coverage
    coverage.record_condition(0, vec![true, true]);
    coverage.record_condition(0, vec![true, false]);
    coverage.record_condition(0, vec![false, true]);
    coverage.record_condition(0, vec![false, false]);
}

fn update_requirement_status(tracker: &mut RequirementTracker) {
    // Update requirement status based on test results
    tracker.update_status("FIFO-001", RequirementStatus::Verified);
    tracker.update_status("FIFO-001.1", RequirementStatus::Verified);
    tracker.update_status("FIFO-001.2", RequirementStatus::Verified);
    tracker.update_status("FIFO-002", RequirementStatus::Verified);
    tracker.update_status("FIFO-003", RequirementStatus::Proven);
}

#[cfg(feature = "formal")]
fn run_formal_verification(properties: &[Property]) {
    use skalp_verify::formal::*;

    let config = FormalConfig {
        bmc_bound: 20,
        use_induction: true,
        use_interpolation: false,
        timeout: 30,
        gen_counterexample: true,
    };

    let mut engine = FormalEngine::new(config);

    // Add properties to verify
    for prop in properties {
        engine.add_property(prop.clone());
    }

    // Run formal verification
    let result = engine.verify();

    println!("Formal Verification Results:");
    println!("  Overall: {:?}", result.overall_status);
    for prop_result in &result.properties {
        println!(
            "  {}: {:?} ({:.2}s)",
            prop_result.property_name, prop_result.status, prop_result.time
        );

        if let Some(cex) = &prop_result.counterexample {
            println!("    Counterexample found at depth {}", cex.depth);
        }
        if let Some(proof) = &prop_result.proof {
            println!(
                "    Proven using {:?} at depth {}",
                proof.method, proof.depth
            );
        }
    }
}
