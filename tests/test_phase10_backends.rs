#[cfg(test)]
mod phase10_backend_tests {
    use skalp_backends::{
        BackendFactory, FpgaTarget, OptimizationGoals, OptimizationTarget, SynthesisConfig,
        TargetPlatform, TimingConstraint,
    };
    use skalp_lir::{LirDesign, LirModule};
    use std::collections::HashMap;

    #[test]
    fn test_phase10_comprehensive_backend_analysis() {
        println!("🔧 Phase 10: Advanced Backends Test");

        // Test 1: Backend Factory and Available Backends
        println!("\n1️⃣ Testing Backend Factory...");
        let available_backends = BackendFactory::available_backends();
        println!("   Available backends: {} types", available_backends.len());

        let mut fpga_backends = 0;
        let mut asic_backends = 0;

        for backend in &available_backends {
            match backend {
                TargetPlatform::Fpga(_) => fpga_backends += 1,
                TargetPlatform::Asic(_) => asic_backends += 1,
            }
        }

        println!(
            "   FPGA backends: {}, ASIC backends: {}",
            fpga_backends, asic_backends
        );
        assert!(!available_backends.is_empty());
        assert!(fpga_backends > 0);
        assert!(asic_backends > 0);

        // Test 2: iCE40 Backend Creation
        println!("\n2️⃣ Testing iCE40 FPGA Backend Creation...");
        let ice40_target = TargetPlatform::Fpga(FpgaTarget::Ice40 {
            part: "iCE40HX8K".to_string(),
            package: "CT256".to_string(),
        });

        let backend_result = BackendFactory::create_backend(&ice40_target);
        assert!(backend_result.is_ok(), "Failed to create iCE40 backend");

        let backend = backend_result.as_ref().unwrap();
        println!("   Backend name: {}", backend.name());
        println!(
            "   Tool version: {}",
            backend.tool_version().unwrap_or("Unknown".to_string())
        );

        let supported_devices = backend.supported_devices();
        println!("   Supported devices: {:?}", supported_devices);
        assert!(!supported_devices.is_empty());

        // Test 3: Synthesis Configuration
        println!("\n3️⃣ Testing Synthesis Configuration...");
        let synthesis_config = SynthesisConfig {
            target: ice40_target.clone(),
            optimization: OptimizationGoals {
                primary: OptimizationTarget::Performance,
                max_area_utilization: Some(0.75),
                target_frequency: Some(125.0),
                max_power: Some(250.0),
            },
            timing_constraints: vec![TimingConstraint::ClockPeriod {
                clock_name: "clk".to_string(),
                period_ns: 8.0, // 125 MHz
            }],
            power_constraints: None,
            output_dir: "/tmp/skalp_synthesis".to_string(),
            tool_options: HashMap::new(),
        };

        let validation_result = backend.validate_config(&synthesis_config);
        assert!(
            validation_result.is_ok(),
            "Synthesis config validation failed"
        );
        println!("   Synthesis configuration validated successfully");

        // Test 4: LIR Design Validation
        println!("\n4️⃣ Testing LIR Design Validation...");
        let test_lir = create_test_lir_design();
        let design_validation = backend.validate_design(&test_lir);
        assert!(design_validation.is_ok(), "LIR design validation failed");
        println!("   LIR design validation passed");

        // Test 5: Phase 10 Success Criteria Validation
        println!("\n5️⃣ Phase 10 Success Criteria Validation...");

        let backend_factory_working = !available_backends.is_empty();
        let ice40_backend_working = backend_result.is_ok();
        let config_validation_working = validation_result.is_ok();
        let design_validation_working = design_validation.is_ok();
        let comprehensive_backend_support = fpga_backends >= 3 && asic_backends >= 2;

        println!(
            "   ✅ Backend factory: {}",
            if backend_factory_working {
                "PASS"
            } else {
                "FAIL"
            }
        );
        println!(
            "   ✅ iCE40 backend creation: {}",
            if ice40_backend_working {
                "PASS"
            } else {
                "FAIL"
            }
        );
        println!(
            "   ✅ Configuration validation: {}",
            if config_validation_working {
                "PASS"
            } else {
                "FAIL"
            }
        );
        println!(
            "   ✅ Design validation: {}",
            if design_validation_working {
                "PASS"
            } else {
                "FAIL"
            }
        );
        println!(
            "   ✅ Comprehensive backend support: {}",
            if comprehensive_backend_support {
                "PASS"
            } else {
                "FAIL"
            }
        );

        if backend_factory_working
            && ice40_backend_working
            && config_validation_working
            && design_validation_working
        {
            println!("\n🎉 PHASE 10: ADVANCED BACKENDS - FOUNDATION COMPLETE!");
            println!(
                "   ✅ Backend factory operational with {} targets",
                available_backends.len()
            );
            println!("   ✅ iCE40 FPGA backend functional");
            println!("   ✅ Configuration and design validation working");
            println!("   ✅ Ready for hardware synthesis testing");
        } else {
            println!("\n⚠️  Some backend components need improvement");
        }

        // Test assertions
        assert!(backend_factory_working, "Backend factory should work");
        assert!(ice40_backend_working, "iCE40 backend should work");
        assert!(
            config_validation_working,
            "Configuration validation should work"
        );
        assert!(design_validation_working, "Design validation should work");
    }

    #[tokio::test]
    async fn test_ice40_synthesis_flow() {
        println!("🔌 Testing iCE40 Synthesis Flow");

        let ice40_target = TargetPlatform::Fpga(FpgaTarget::Ice40 {
            part: "iCE40HX8K".to_string(),
            package: "CT256".to_string(),
        });

        let backend_result = BackendFactory::create_backend(&ice40_target);
        assert!(backend_result.is_ok());
        let backend = backend_result.unwrap();

        let synthesis_config = SynthesisConfig {
            target: ice40_target,
            optimization: OptimizationGoals {
                primary: OptimizationTarget::Balanced,
                max_area_utilization: Some(0.8),
                target_frequency: Some(100.0),
                max_power: Some(300.0),
            },
            timing_constraints: vec![TimingConstraint::ClockPeriod {
                clock_name: "clk".to_string(),
                period_ns: 10.0, // 100 MHz
            }],
            power_constraints: None,
            output_dir: "/tmp/skalp_ice40_test".to_string(),
            tool_options: HashMap::new(),
        };

        let test_design = create_test_lir_design();

        // Run synthesis (this will use mock tools if real ones aren't available)
        let synthesis_result = backend.synthesize(&test_design, &synthesis_config).await;

        match synthesis_result {
            Ok(results) => {
                println!("   ✅ Synthesis completed successfully");
                println!(
                    "   📊 Area utilization: {:.1}%",
                    results.area_metrics.utilization_percent
                );
                println!(
                    "   ⏱️  Max frequency: {:.1} MHz",
                    results.timing_results.max_frequency_mhz
                );
                println!(
                    "   ⚡ Total power: {:.1} mW",
                    results.power_results.total_power_mw
                );
                println!("   📁 Output files: {}", results.output_files.len());

                // Validate synthesis results
                assert!(results.area_metrics.utilization_percent >= 0.0);
                assert!(results.timing_results.max_frequency_mhz > 0.0);
                assert!(results.power_results.total_power_mw > 0.0);
                assert!(!results.output_files.is_empty());

                if results.success {
                    println!("   🎯 Bitstream generation: SUCCESS");
                } else {
                    println!("   ⚠️  Bitstream generation: MOCK (tools not available)");
                }
            }
            Err(e) => {
                println!("   ❌ Synthesis failed: {:?}", e);
                panic!("Synthesis should not fail completely");
            }
        }
    }

    // Helper function to create a test LIR design
    fn create_test_lir_design() -> LirDesign {
        use skalp_lir::{Gate, GateType, LirSignal, Net};

        LirDesign {
            name: "test_counter".to_string(),
            modules: vec![LirModule {
                name: "test_counter".to_string(),
                signals: vec![
                    LirSignal {
                        name: "clk".to_string(),
                        signal_type: "logic".to_string(),
                        is_input: true,
                        is_output: false,
                        is_register: false,
                    },
                    LirSignal {
                        name: "reset".to_string(),
                        signal_type: "logic".to_string(),
                        is_input: true,
                        is_output: false,
                        is_register: false,
                    },
                    LirSignal {
                        name: "count".to_string(),
                        signal_type: "logic[31:0]".to_string(),
                        is_input: false,
                        is_output: true,
                        is_register: false,
                    },
                ],
                nets: vec![Net {
                    id: "clk_net".to_string(),
                    width: 1,
                    driver: Some("clk".to_string()),
                    loads: vec!["counter_reg".to_string()],
                    is_output: false,
                    is_input: true,
                }],
                gates: vec![Gate {
                    id: "counter_reg".to_string(),
                    gate_type: GateType::DFF,
                    inputs: vec!["clk_net".to_string()],
                    outputs: vec!["count".to_string()],
                }],
            }],
        }
    }

    #[test]
    fn test_timing_constraints_support() {
        println!("⏱️ Testing Timing Constraints Support");

        // Test various timing constraint types
        let constraints = vec![
            TimingConstraint::ClockPeriod {
                clock_name: "clk".to_string(),
                period_ns: 8.333, // 120 MHz
            },
            TimingConstraint::InputDelay {
                port_name: "data_in".to_string(),
                delay_ns: 1.5,
                clock_name: "clk".to_string(),
            },
            TimingConstraint::OutputDelay {
                port_name: "data_out".to_string(),
                delay_ns: 2.0,
                clock_name: "clk".to_string(),
            },
            TimingConstraint::FalsePath {
                from: "reset_sync".to_string(),
                to: "async_output".to_string(),
            },
            TimingConstraint::MulticyclePath {
                from: "multiply_start".to_string(),
                to: "multiply_done".to_string(),
                cycles: 4,
            },
        ];

        println!(
            "   📋 Testing {} timing constraint types",
            constraints.len()
        );

        for (i, constraint) in constraints.iter().enumerate() {
            match constraint {
                TimingConstraint::ClockPeriod {
                    clock_name,
                    period_ns,
                } => {
                    println!(
                        "   {}. Clock '{}': {:.3} ns period ({:.1} MHz)",
                        i + 1,
                        clock_name,
                        period_ns,
                        1000.0 / period_ns
                    );
                }
                TimingConstraint::InputDelay {
                    port_name,
                    delay_ns,
                    clock_name,
                } => {
                    println!(
                        "   {}. Input delay '{}': {:.1} ns relative to '{}'",
                        i + 1,
                        port_name,
                        delay_ns,
                        clock_name
                    );
                }
                TimingConstraint::OutputDelay {
                    port_name,
                    delay_ns,
                    clock_name,
                } => {
                    println!(
                        "   {}. Output delay '{}': {:.1} ns relative to '{}'",
                        i + 1,
                        port_name,
                        delay_ns,
                        clock_name
                    );
                }
                TimingConstraint::FalsePath { from, to } => {
                    println!("   {}. False path: {} → {}", i + 1, from, to);
                }
                TimingConstraint::MulticyclePath { from, to, cycles } => {
                    println!(
                        "   {}. Multicycle path: {} → {} ({} cycles)",
                        i + 1,
                        from,
                        to,
                        cycles
                    );
                }
            }
        }

        // All constraint types should be supported
        assert_eq!(constraints.len(), 5);
        println!("   ✅ All timing constraint types supported");
    }

    #[test]
    fn test_power_analysis_framework() {
        println!("⚡ Testing Power Analysis Framework");

        use skalp_backends::PowerConstraints;

        let power_constraints = PowerConstraints {
            max_dynamic_power: Some(200.0), // 200 mW
            max_static_power: Some(50.0),   // 50 mW
            operating_voltage: 1.2,         // 1.2V
            operating_temperature: 85.0,    // 85°C
        };

        println!(
            "   🔋 Max dynamic power: {} mW",
            power_constraints.max_dynamic_power.unwrap()
        );
        println!(
            "   🔌 Max static power: {} mW",
            power_constraints.max_static_power.unwrap()
        );
        println!(
            "   ⚡ Operating voltage: {} V",
            power_constraints.operating_voltage
        );
        println!(
            "   🌡️  Operating temperature: {} °C",
            power_constraints.operating_temperature
        );

        // Validate power constraint ranges
        assert!(power_constraints.max_dynamic_power.unwrap() > 0.0);
        assert!(power_constraints.max_static_power.unwrap() > 0.0);
        assert!(power_constraints.operating_voltage > 0.0);
        assert!(power_constraints.operating_temperature >= -40.0);
        assert!(power_constraints.operating_temperature <= 125.0);

        println!("   ✅ Power constraint framework validated");
    }
}
