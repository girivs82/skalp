//! End-to-end test for FI-driven FMEA generation
//!
//! This test demonstrates the "double advantage" of the FI-driven approach:
//! 1. Failure effect identification - which faults cause which safety violations
//! 2. Measured diagnostic coverage - which faults are detected by which mechanisms

#[cfg(test)]
mod fi_driven_fmea_tests {
    use skalp_safety::{
        asil::AsilLevel,
        fault_simulation::{
            CompareOp, CompareValue, ConditionTerm, EffectCondition, FailureEffectDef, FaultSite,
            FaultType, SafetyGoalSimSpec,
        },
        hierarchy::{DesignRef, Severity},
        safety_driven_fmea::{
            EffectMonitor, FaultEffectResult, FiDrivenConfig, SafetyDrivenFmeaGenerator,
        },
    };
    use std::collections::HashMap;

    /// Test that the effect monitor correctly evaluates simple conditions
    #[test]
    fn test_effect_monitor_basic() {
        println!("🔬 Testing Effect Monitor - Basic Conditions");

        // Create safety goal for EPS controller
        let mut spec = SafetyGoalSimSpec::new("SG-001-PreventUnintendedTorque", AsilLevel::D);

        // Add effect: Unintended motor activation
        // Condition: motor_enable != 0 AND safe_mode == 0
        spec.add_effect(FailureEffectDef {
            name: "unintended_motor_active".to_string(),
            description: Some("Motor enabled without being in safe mode".to_string()),
            condition: EffectCondition::And(vec![
                EffectCondition::Term(ConditionTerm {
                    signal: DesignRef::parse("motor_enable"),
                    op: CompareOp::NotEqual,
                    value: CompareValue::Literal(0),
                }),
                EffectCondition::Term(ConditionTerm {
                    signal: DesignRef::parse("safe_mode"),
                    op: CompareOp::Equal,
                    value: CompareValue::Literal(0),
                }),
            ]),
            severity: Severity::S3,
            target_dc: 0.99,
        });

        let monitor = EffectMonitor::new(&spec);

        // Test case 1: Safe state (motor off, safe_mode on)
        let mut signals = HashMap::new();
        signals.insert("motor_enable".to_string(), 0u64);
        signals.insert("safe_mode".to_string(), 1u64);
        let effects = monitor.check_effects(&signals);
        assert!(effects.is_empty(), "Should not trigger when motor is off");
        println!("   ✅ Safe state (motor_enable=0, safe_mode=1): No effect triggered");

        // Test case 2: Motor on but in safe mode
        signals.insert("motor_enable".to_string(), 1u64);
        signals.insert("safe_mode".to_string(), 1u64);
        let effects = monitor.check_effects(&signals);
        assert!(
            effects.is_empty(),
            "Should not trigger when in safe mode even with motor on"
        );
        println!("   ✅ Protected state (motor_enable=1, safe_mode=1): No effect triggered");

        // Test case 3: Dangerous - motor on without safe mode
        signals.insert("motor_enable".to_string(), 1u64);
        signals.insert("safe_mode".to_string(), 0u64);
        let effects = monitor.check_effects(&signals);
        assert_eq!(effects.len(), 1, "Should trigger unintended motor effect");
        assert_eq!(effects[0], "unintended_motor_active");
        println!("   ✅ Dangerous state (motor_enable=1, safe_mode=0): Effect triggered!");
    }

    /// Test multiple effect conditions (EPS controller example)
    #[test]
    fn test_eps_controller_effects() {
        println!("\n🚗 Testing EPS Controller Safety Goal Effects");

        let mut spec = SafetyGoalSimSpec::new("SG-001-PreventUnintendedTorque", AsilLevel::D);

        // Effect 1: Unintended motor activation
        spec.add_effect(FailureEffectDef {
            name: "unintended_motor_active".to_string(),
            description: Some("Motor commanded while system in unsafe state".to_string()),
            condition: EffectCondition::And(vec![
                EffectCondition::Term(ConditionTerm {
                    signal: DesignRef::parse("motor_enable"),
                    op: CompareOp::NotEqual,
                    value: CompareValue::Literal(0),
                }),
                EffectCondition::Term(ConditionTerm {
                    signal: DesignRef::parse("safe_mode"),
                    op: CompareOp::Equal,
                    value: CompareValue::Literal(0),
                }),
            ]),
            severity: Severity::S3,
            target_dc: 0.99,
        });

        // Effect 2: TMR voter disagreement undetected
        spec.add_effect(FailureEffectDef {
            name: "tmr_failure_undetected".to_string(),
            description: Some("TMR disagreement not flagged".to_string()),
            condition: EffectCondition::And(vec![
                EffectCondition::Term(ConditionTerm {
                    signal: DesignRef::parse("tmr_disagreement"),
                    op: CompareOp::NotEqual,
                    value: CompareValue::Literal(0),
                }),
                EffectCondition::Term(ConditionTerm {
                    signal: DesignRef::parse("fault_detected"),
                    op: CompareOp::Equal,
                    value: CompareValue::Literal(0),
                }),
            ]),
            severity: Severity::S2,
            target_dc: 0.95,
        });

        // Effect 3: Watchdog timeout missed
        spec.add_effect(FailureEffectDef {
            name: "watchdog_failure".to_string(),
            description: Some("Watchdog timeout without response".to_string()),
            condition: EffectCondition::And(vec![
                EffectCondition::Term(ConditionTerm {
                    signal: DesignRef::parse("watchdog_timeout"),
                    op: CompareOp::NotEqual,
                    value: CompareValue::Literal(0),
                }),
                EffectCondition::Term(ConditionTerm {
                    signal: DesignRef::parse("safe_mode"),
                    op: CompareOp::Equal,
                    value: CompareValue::Literal(0),
                }),
            ]),
            severity: Severity::S3,
            target_dc: 0.99,
        });

        let monitor = EffectMonitor::new(&spec);

        // Simulate a fault scenario: TMR disagreement detected properly
        let mut signals = HashMap::new();
        signals.insert("motor_enable".to_string(), 0u64);
        signals.insert("safe_mode".to_string(), 0u64);
        signals.insert("tmr_disagreement".to_string(), 1u64);
        signals.insert("fault_detected".to_string(), 1u64);
        signals.insert("watchdog_timeout".to_string(), 0u64);

        let effects = monitor.check_effects(&signals);
        assert!(
            !effects.contains(&"tmr_failure_undetected".to_string()),
            "TMR failure should be detected when fault_detected is set"
        );
        println!("   ✅ TMR disagreement properly detected - no safety effect");

        // Now simulate TMR failure going undetected
        signals.insert("fault_detected".to_string(), 0u64);
        let effects = monitor.check_effects(&signals);
        assert!(
            effects.contains(&"tmr_failure_undetected".to_string()),
            "TMR failure should be flagged when not detected"
        );
        println!("   ✅ TMR disagreement undetected - safety effect triggered!");
    }

    /// Test the full FI-driven FMEA generation flow
    #[test]
    fn test_fi_driven_fmea_generation() {
        println!("\n🔧 Testing FI-Driven FMEA Generation");

        // Create comprehensive safety goal spec for EPS
        let mut spec = SafetyGoalSimSpec::new("SG-001-PreventUnintendedTorque", AsilLevel::D);

        // Define the hazardous effect
        spec.add_effect(FailureEffectDef {
            name: "unintended_motor_torque".to_string(),
            description: Some("Motor provides unintended torque".to_string()),
            condition: EffectCondition::Term(ConditionTerm {
                signal: DesignRef::parse("motor_enable"),
                op: CompareOp::NotEqual,
                value: CompareValue::Literal(0),
            }),
            severity: Severity::S3,
            target_dc: 0.99,
        });

        let config = FiDrivenConfig::default();
        let generator = SafetyDrivenFmeaGenerator::new(spec, config);

        // Simulate fault injection results (as if from simulation)
        let fault_results = vec![
            // Fault 1: Stuck-at-0 on TMR voter output - DETECTED by TMR
            FaultEffectResult {
                fault_site: FaultSite::new(
                    DesignRef::parse("top.eps.tmr_voter::gate_0"),
                    FaultType::StuckAt0,
                ),
                primitive_path: "top.eps.tmr_voter::gate_0".to_string(),
                triggered_effects: vec!["unintended_motor_torque".to_string()],
                detected: true,
                detected_by: Some("TmrVoter".to_string()),
                effect_cycle: Some(10),
                detection_cycle: Some(5),
                is_safety_mechanism: false,
                detection_mode: None,
                is_boot_time_only: false,
            },
            // Fault 2: Stuck-at-1 on TMR voter - DETECTED by TMR
            FaultEffectResult {
                fault_site: FaultSite::new(
                    DesignRef::parse("top.eps.tmr_voter::gate_1"),
                    FaultType::StuckAt1,
                ),
                primitive_path: "top.eps.tmr_voter::gate_1".to_string(),
                triggered_effects: vec!["unintended_motor_torque".to_string()],
                detected: true,
                detected_by: Some("TmrVoter".to_string()),
                effect_cycle: Some(8),
                detection_cycle: Some(4),
                is_safety_mechanism: false,
                detection_mode: None,
                is_boot_time_only: false,
            },
            // Fault 3: Watchdog counter fault - DETECTED by watchdog
            FaultEffectResult {
                fault_site: FaultSite::new(
                    DesignRef::parse("top.eps.watchdog::counter_reg"),
                    FaultType::StuckAt0,
                ),
                primitive_path: "top.eps.watchdog::counter_reg".to_string(),
                triggered_effects: vec!["unintended_motor_torque".to_string()],
                detected: true,
                detected_by: Some("WatchdogTimer".to_string()),
                effect_cycle: Some(1000),
                detection_cycle: Some(1000),
                is_safety_mechanism: false,
                detection_mode: None,
                is_boot_time_only: false,
            },
            // Fault 4: Safe state controller - NOT DETECTED (dangerous!)
            FaultEffectResult {
                fault_site: FaultSite::new(
                    DesignRef::parse("top.eps.safe_state::state_reg"),
                    FaultType::StuckAt1,
                ),
                primitive_path: "top.eps.safe_state::state_reg".to_string(),
                triggered_effects: vec!["unintended_motor_torque".to_string()],
                detected: false,
                detected_by: None,
                effect_cycle: Some(15),
                detection_cycle: None,
                is_safety_mechanism: false,
                detection_mode: None,
                is_boot_time_only: false,
            },
            // Fault 5: Motor driver - DETECTED by comparator
            FaultEffectResult {
                fault_site: FaultSite::new(
                    DesignRef::parse("top.eps.motor_driver::pwm_out"),
                    FaultType::StuckAt1,
                ),
                primitive_path: "top.eps.motor_driver::pwm_out".to_string(),
                triggered_effects: vec!["unintended_motor_torque".to_string()],
                detected: true,
                detected_by: Some("TorqueComparator".to_string()),
                effect_cycle: Some(5),
                detection_cycle: Some(5),
                is_safety_mechanism: false,
                detection_mode: None,
                is_boot_time_only: false,
            },
            // Fault 6: Safe fault - no effect triggered
            FaultEffectResult {
                fault_site: FaultSite::new(
                    DesignRef::parse("top.eps.debug_led::mux"),
                    FaultType::StuckAt0,
                ),
                primitive_path: "top.eps.debug_led::mux".to_string(),
                triggered_effects: vec![], // Safe fault - no safety effect
                detected: false,
                detected_by: None,
                effect_cycle: None,
                detection_cycle: None,
                is_safety_mechanism: false,
                detection_mode: None,
                is_boot_time_only: false,
            },
        ];

        // Generate FMEA from simulated results
        let result = generator.generate_from_campaign_results(
            &fault_results,
            100, // 100 primitives in design
            "EpsController",
            500.0, // 500 FIT total failure rate
        );

        println!("\n📊 FI-Driven FMEA Results:");
        println!("   Goal: {}", result.goal_name);
        println!("   Total injections: {}", result.total_injections);

        // Analyze effect
        if let Some(analysis) = result.effect_analyses.get("unintended_motor_torque") {
            println!("\n   Effect: unintended_motor_torque");
            println!(
                "     - Total faults causing: {}",
                analysis.total_faults_causing
            );
            println!("     - Faults detected: {}", analysis.faults_detected);
            println!("     - Measured DC: {:.1}%", analysis.measured_dc * 100.0);
            println!("     - Meets target (≥99%): {}", analysis.meets_target);
            println!(
                "     - Undetected sites: {}",
                analysis.undetected_sites.len()
            );

            // The key assertion: 4 out of 5 dangerous faults were detected = 80% DC
            assert_eq!(
                analysis.total_faults_causing, 5,
                "Should have 5 dangerous faults"
            );
            assert_eq!(analysis.faults_detected, 4, "Should have 4 detected faults");
            assert!(
                (analysis.measured_dc - 0.80).abs() < 0.01,
                "DC should be ~80%"
            );
            assert!(!analysis.meets_target, "Should NOT meet 99% target");
        }

        // Check auto-generated FMEA
        println!("\n   📋 Auto-Generated FMEA Entries:");
        for entry in &result.auto_fmea.entries {
            println!(
                "     - {}: DC={:.1}%",
                entry.name,
                entry.measured_dc * 100.0
            );
        }

        // Summary metrics
        println!("\n   📈 Overall Metrics:");
        println!("     - SPFM: {:.1}%", result.measured_spfm * 100.0);
        println!("     - LFM: {:.1}%", result.measured_lf * 100.0);
        if let Some(pmhf) = result.measured_pmhf {
            println!("     - PMHF: {:.1} FIT", pmhf);
        }
        println!("     - Meets ASIL-D: {}", result.meets_asil);

        // The system does NOT meet ASIL-D because DC is only 80%
        assert!(!result.meets_asil, "Should not meet ASIL-D with 80% DC");
        println!("\n   ⚠️  Gap identified: safe_state::state_reg needs additional protection");
    }

    /// Test the "double advantage" - same simulation provides effect ID and measured DC
    #[test]
    fn test_double_advantage_demonstration() {
        println!("\n⭐ Demonstrating the Double Advantage");

        let mut spec = SafetyGoalSimSpec::new("SG-001-BrakingSystem", AsilLevel::D);

        // Multiple effects from same design
        spec.add_effect(FailureEffectDef {
            name: "brake_loss".to_string(),
            description: Some("Complete braking capability loss".to_string()),
            condition: EffectCondition::Term(ConditionTerm {
                signal: DesignRef::parse("brake_pressure"),
                op: CompareOp::Equal,
                value: CompareValue::Literal(0),
            }),
            severity: Severity::S3,
            target_dc: 0.99,
        });

        spec.add_effect(FailureEffectDef {
            name: "brake_asymmetry".to_string(),
            description: Some("Unequal braking between wheels".to_string()),
            condition: EffectCondition::Term(ConditionTerm {
                signal: DesignRef::parse("brake_diff"),
                op: CompareOp::GreaterThan,
                value: CompareValue::Literal(100),
            }),
            severity: Severity::S2,
            target_dc: 0.95,
        });

        let config = FiDrivenConfig::default();
        let generator = SafetyDrivenFmeaGenerator::new(spec, config);

        // Simulated results showing different effects from different faults
        let fault_results = vec![
            // Fault causes brake_loss
            FaultEffectResult {
                fault_site: FaultSite::new(
                    DesignRef::parse("top.brake.valve::solenoid"),
                    FaultType::Open,
                ),
                primitive_path: "top.brake.valve::solenoid".to_string(),
                triggered_effects: vec!["brake_loss".to_string()],
                detected: true,
                detected_by: Some("PressureSensor".to_string()),
                effect_cycle: Some(50),
                detection_cycle: Some(30),
                is_safety_mechanism: false,
                detection_mode: None,
                is_boot_time_only: false,
            },
            // Fault causes brake_asymmetry
            FaultEffectResult {
                fault_site: FaultSite::new(
                    DesignRef::parse("top.brake.distribution::valve_left"),
                    FaultType::StuckAt1,
                ),
                primitive_path: "top.brake.distribution::valve_left".to_string(),
                triggered_effects: vec!["brake_asymmetry".to_string()],
                detected: true,
                detected_by: Some("WheelSpeedMonitor".to_string()),
                effect_cycle: Some(100),
                detection_cycle: Some(80),
                is_safety_mechanism: false,
                detection_mode: None,
                is_boot_time_only: false,
            },
            // Fault causes BOTH effects (severe!)
            FaultEffectResult {
                fault_site: FaultSite::new(
                    DesignRef::parse("top.brake.master::hydraulic_pump"),
                    FaultType::StuckAt0,
                ),
                primitive_path: "top.brake.master::hydraulic_pump".to_string(),
                triggered_effects: vec!["brake_loss".to_string(), "brake_asymmetry".to_string()],
                detected: false, // Dangerous - not detected!
                detected_by: None,
                effect_cycle: Some(10),
                detection_cycle: None,
                is_safety_mechanism: false,
                detection_mode: None,
                is_boot_time_only: false,
            },
        ];

        let result =
            generator.generate_from_campaign_results(&fault_results, 50, "BrakingSystem", 200.0);

        println!("\n   DOUBLE ADVANTAGE RESULTS:");
        println!("   ═════════════════════════");

        println!("\n   Advantage 1: Effect Identification");
        println!("   ──────────────────────────────────");
        for (effect_name, analysis) in &result.effect_analyses {
            if !effect_name.starts_with('_') {
                println!(
                    "     📍 {} - {} faults cause this effect",
                    effect_name, analysis.total_faults_causing
                );
            }
        }

        println!("\n   Advantage 2: Measured DC (not estimated!)");
        println!("   ─────────────────────────────────────────");
        for (effect_name, analysis) in &result.effect_analyses {
            if !effect_name.starts_with('_') {
                println!(
                    "     📊 {}: DC = {:.1}% (target: {:.0}%)",
                    effect_name,
                    analysis.measured_dc * 100.0,
                    analysis.target_dc * 100.0
                );
                if analysis.meets_target {
                    println!("        ✅ Meets target");
                } else {
                    println!("        ❌ Below target - needs investigation");
                }
            }
        }

        // Verify the double advantage
        let brake_loss = result.effect_analyses.get("brake_loss").unwrap();
        assert_eq!(
            brake_loss.total_faults_causing, 2,
            "Two faults cause brake_loss"
        );
        assert_eq!(brake_loss.faults_detected, 1, "One detected");
        assert!((brake_loss.measured_dc - 0.50).abs() < 0.01, "DC = 50%");

        let asymmetry = result.effect_analyses.get("brake_asymmetry").unwrap();
        assert_eq!(
            asymmetry.total_faults_causing, 2,
            "Two faults cause asymmetry"
        );
        assert_eq!(asymmetry.faults_detected, 1, "One detected");

        println!("\n   ✅ Double advantage demonstrated:");
        println!("      - Same simulation identified which effects each fault causes");
        println!("      - Same simulation measured actual DC per effect");
        println!("      - No lookup tables or assumptions needed!");
    }

    /// Test detection mechanism attribution
    #[test]
    fn test_detection_mechanism_attribution() {
        println!("\n🔍 Testing Detection Mechanism Attribution");

        let mut spec = SafetyGoalSimSpec::new("TestGoal", AsilLevel::D);
        spec.add_effect(FailureEffectDef {
            name: "output_error".to_string(),
            description: None,
            condition: EffectCondition::Term(ConditionTerm {
                signal: DesignRef::parse("output"),
                op: CompareOp::NotEqual,
                value: CompareValue::Golden("golden_output".to_string()),
            }),
            severity: Severity::S2,
            target_dc: 0.95,
        });

        let config = FiDrivenConfig::default();
        let generator = SafetyDrivenFmeaGenerator::new(spec, config);

        let fault_results = vec![
            FaultEffectResult {
                fault_site: FaultSite::new(DesignRef::parse("cell_0"), FaultType::StuckAt0),
                primitive_path: "cell_0".to_string(),
                triggered_effects: vec!["output_error".to_string()],
                detected: true,
                detected_by: Some("CRC_Checker".to_string()),
                effect_cycle: Some(1),
                detection_cycle: Some(1),
                is_safety_mechanism: false,
                detection_mode: None,
                is_boot_time_only: false,
            },
            FaultEffectResult {
                fault_site: FaultSite::new(DesignRef::parse("cell_1"), FaultType::StuckAt0),
                primitive_path: "cell_1".to_string(),
                triggered_effects: vec!["output_error".to_string()],
                detected: true,
                detected_by: Some("CRC_Checker".to_string()),
                effect_cycle: Some(1),
                detection_cycle: Some(1),
                is_safety_mechanism: false,
                detection_mode: None,
                is_boot_time_only: false,
            },
            FaultEffectResult {
                fault_site: FaultSite::new(DesignRef::parse("cell_2"), FaultType::StuckAt0),
                primitive_path: "cell_2".to_string(),
                triggered_effects: vec!["output_error".to_string()],
                detected: true,
                detected_by: Some("TMR_Voter".to_string()),
                effect_cycle: Some(1),
                detection_cycle: Some(1),
                is_safety_mechanism: false,
                detection_mode: None,
                is_boot_time_only: false,
            },
            FaultEffectResult {
                fault_site: FaultSite::new(DesignRef::parse("cell_3"), FaultType::StuckAt0),
                primitive_path: "cell_3".to_string(),
                triggered_effects: vec!["output_error".to_string()],
                detected: false,
                detected_by: None,
                effect_cycle: Some(1),
                detection_cycle: None,
                is_safety_mechanism: false,
                detection_mode: None,
                is_boot_time_only: false,
            },
        ];

        let result =
            generator.generate_from_campaign_results(&fault_results, 10, "TestDesign", 100.0);

        let analysis = result.effect_analyses.get("output_error").unwrap();

        println!("   Detection by mechanism:");
        for (mech, count) in &analysis.detection_by_mechanism {
            let pct = *count as f64 / analysis.faults_detected as f64 * 100.0;
            println!("     - {}: {} faults ({:.0}%)", mech, count, pct);
        }

        // Verify mechanism attribution
        assert_eq!(
            analysis
                .detection_by_mechanism
                .get("CRC_Checker")
                .copied()
                .unwrap_or(0),
            2,
            "CRC detected 2 faults"
        );
        assert_eq!(
            analysis
                .detection_by_mechanism
                .get("TMR_Voter")
                .copied()
                .unwrap_or(0),
            1,
            "TMR detected 1 fault"
        );
        println!("   ✅ Mechanism attribution verified");
    }
}
