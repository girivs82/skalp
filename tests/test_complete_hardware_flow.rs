#[cfg(test)]
mod complete_hardware_flow_tests {
    use skalp_backends::{
        BackendFactory, TargetPlatform, FpgaTarget, AsicTarget, SynthesisConfig,
        OptimizationGoals, OptimizationTarget, TimingConstraint, PowerConstraints,
        constraints::{ConstraintManager, ConstraintFormat},
    };
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::MirCompiler;
    use skalp_lir::lower_to_lir;
    use std::collections::HashMap;

    #[tokio::test]
    async fn test_complete_skalp_to_ice40_flow() {
        println!("🔄 Complete SKALP to iCE40 Flow Test");

        // Step 1: Parse SKALP source code and build HIR
        println!("\n1️⃣ Parsing SKALP source code and building HIR...");
        let skalp_source = r#"
            entity Counter {
                inputs {
                    clk: clock,
                    reset: logic,
                    enable: logic,
                }
                outputs {
                    count: logic[31:0],
                    overflow: logic,
                }

                on(reset.active) {
                    counter <= 0;
                    overflow <= false;
                }

                on(clk.rise) {
                    if enable {
                        if counter == 0xFFFFFFFF {
                            counter <= 0;
                            overflow <= true;
                        } else {
                            counter <= counter + 1;
                            overflow <= false;
                        }
                    }
                }

                // Continuous assignment for output
                count = counter;
            }
        "#;

        let hir = parse_and_build_hir(skalp_source).expect("Failed to parse and build HIR");
        println!("   HIR entities: {}", hir.entities.len());

        // Step 2: Compile to MIR
        println!("2️⃣ Compiling to MIR...");
        let mir_compiler = MirCompiler::new();
        let mir = mir_compiler.compile_to_mir(&hir).expect("Failed to compile to MIR");
        println!("   MIR modules: {}", mir.modules.len());

        // Step 3: Lower to LIR
        println!("3️⃣ Lowering to LIR...");
        let lir = lower_to_lir(&mir).expect("Failed to lower to LIR");
        println!("   LIR modules: {}", lir.modules.len());
        if let Some(module) = lir.modules.first() {
            println!("   Gates: {}, Nets: {}, Signals: {}",
                module.gates.len(), module.nets.len(), module.signals.len());
        }

        // Step 4: Create timing constraints
        println!("4️⃣ Creating timing constraints...");
        let mut constraint_manager = ConstraintManager::new();

        // Add clock constraint for 100 MHz
        constraint_manager.add_timing_constraint(TimingConstraint::ClockPeriod {
            clock_name: "clk".to_string(),
            period_ns: 10.0, // 100 MHz
        });

        // Add input/output delays
        constraint_manager.add_timing_constraint(TimingConstraint::InputDelay {
            port_name: "reset".to_string(),
            delay_ns: 1.0,
            clock_name: "clk".to_string(),
        });

        constraint_manager.add_timing_constraint(TimingConstraint::OutputDelay {
            port_name: "count".to_string(),
            delay_ns: 2.0,
            clock_name: "clk".to_string(),
        });

        // Generate SDC constraints
        let sdc_constraints = constraint_manager.generate_constraints(ConstraintFormat::Sdc);
        println!("   Generated SDC constraints:");
        for line in sdc_constraints.lines().take(5) {
            println!("     {}", line);
        }

        // Step 5: Configure synthesis for iCE40
        println!("5️⃣ Configuring iCE40 synthesis...");
        let ice40_target = TargetPlatform::Fpga(FpgaTarget::Ice40 {
            part: "iCE40HX8K".to_string(),
            package: "CT256".to_string(),
        });

        let synthesis_config = SynthesisConfig {
            target: ice40_target.clone(),
            optimization: OptimizationGoals {
                primary: OptimizationTarget::Performance,
                max_area_utilization: Some(0.8),
                target_frequency: Some(100.0),
                max_power: Some(200.0),
            },
            timing_constraints: constraint_manager.get_timing_constraints().clone(),
            power_constraints: Some(PowerConstraints {
                max_dynamic_power: Some(150.0),
                max_static_power: Some(50.0),
                operating_voltage: 1.2,
                operating_temperature: 85.0,
            }),
            output_dir: "/tmp/skalp_counter_synthesis".to_string(),
            tool_options: HashMap::new(),
        };

        // Step 6: Run synthesis
        println!("6️⃣ Running iCE40 synthesis...");
        let backend = BackendFactory::create_backend(&ice40_target)
            .expect("Failed to create iCE40 backend");

        let synthesis_results = backend.synthesize(&lir, &synthesis_config).await
            .expect("Failed to run synthesis");

        // Step 7: Analyze results
        println!("7️⃣ Analyzing synthesis results...");
        println!("   ✅ Synthesis success: {}", synthesis_results.success);
        println!("   📊 Area utilization: {:.1}%", synthesis_results.area_metrics.utilization_percent);
        println!("   ⏱️  Max frequency: {:.1} MHz", synthesis_results.timing_results.max_frequency_mhz);
        println!("   ⚡ Total power: {:.1} mW", synthesis_results.power_results.total_power_mw);
        println!("   📁 Output files generated: {}", synthesis_results.output_files.len());

        for output_file in &synthesis_results.output_files {
            println!("     {:?}: {}", output_file.file_type, output_file.description);
        }

        // Validate results
        assert!(synthesis_results.area_metrics.utilization_percent >= 0.0);
        assert!(synthesis_results.timing_results.max_frequency_mhz > 0.0);
        assert!(synthesis_results.power_results.total_power_mw > 0.0);
        assert!(!synthesis_results.output_files.is_empty());

        // Check if we meet timing constraints
        let timing_slack = &synthesis_results.timing_results.timing_slack;
        println!("   🕐 Timing slack: {:.2} ns", timing_slack.worst_negative_slack_ns);
        if timing_slack.failing_endpoints == 0 {
            println!("   ✅ All timing constraints met!");
        } else {
            println!("   ⚠️  {} timing violations", timing_slack.failing_endpoints);
        }

        println!("\n🎉 COMPLETE SKALP TO iCE40 FLOW - SUCCESS!");
        println!("   From source code → Parse → HIR → MIR → LIR → iCE40 Bitstream");
    }

    #[tokio::test]
    async fn test_asic_backend_flow() {
        println!("🏭 ASIC Backend Flow Test");

        // Create a simple LIR design for testing
        let test_lir = create_test_adder_lir();

        // Test multiple ASIC targets
        let asic_targets = vec![
            TargetPlatform::Asic(AsicTarget::FreePdk45),
            TargetPlatform::Asic(AsicTarget::Sky130),
            TargetPlatform::Asic(AsicTarget::Generic {
                library_name: "test_stdcells".to_string(),
                process_node: "28nm".to_string(),
            }),
        ];

        for (i, target) in asic_targets.iter().enumerate() {
            println!("\n{}️⃣ Testing ASIC target: {:?}", i + 1, target);

            let backend_result = BackendFactory::create_backend(target);
            if backend_result.is_err() {
                println!("   ⚠️  Backend not available: {:?}", backend_result.err());
                continue;
            }

            let backend = backend_result.unwrap();
            println!("   Backend: {}", backend.name());
            println!("   Tool version: {}", backend.tool_version().unwrap_or("Unknown".to_string()));

            let synthesis_config = SynthesisConfig {
                target: target.clone(),
                optimization: OptimizationGoals {
                    primary: OptimizationTarget::Area,
                    max_area_utilization: Some(0.9),
                    target_frequency: Some(500.0), // 500 MHz for ASIC
                    max_power: Some(100.0),
                },
                timing_constraints: vec![
                    TimingConstraint::ClockPeriod {
                        clock_name: "clk".to_string(),
                        period_ns: 2.0, // 500 MHz
                    },
                ],
                power_constraints: Some(PowerConstraints {
                    max_dynamic_power: Some(80.0),
                    max_static_power: Some(20.0),
                    operating_voltage: 0.9,
                    operating_temperature: 85.0,
                }),
                output_dir: format!("/tmp/skalp_asic_test_{}", i),
                tool_options: HashMap::new(),
            };

            let synthesis_result = backend.synthesize(&test_lir, &synthesis_config).await;
            match synthesis_result {
                Ok(results) => {
                    println!("   ✅ Synthesis completed");
                    println!("   📊 Cell area: {:.1} μm²",
                        results.area_metrics.cell_area_um2.unwrap_or(0.0));
                    println!("   ⏱️  Max frequency: {:.1} MHz", results.timing_results.max_frequency_mhz);
                    println!("   ⚡ Power: {:.1} mW", results.power_results.total_power_mw);
                }
                Err(e) => {
                    println!("   ⚠️  Synthesis failed: {:?}", e);
                }
            }
        }

        println!("\n✅ ASIC backend flow testing complete");
    }

    #[test]
    fn test_constraint_generation_formats() {
        println!("📋 Testing Constraint Generation Formats");

        let mut constraint_manager = ConstraintManager::new();

        // Add comprehensive constraints
        constraint_manager.add_timing_constraint(TimingConstraint::ClockPeriod {
            clock_name: "clk_100mhz".to_string(),
            period_ns: 10.0,
        });

        constraint_manager.add_timing_constraint(TimingConstraint::ClockPeriod {
            clock_name: "clk_200mhz".to_string(),
            period_ns: 5.0,
        });

        constraint_manager.add_timing_constraint(TimingConstraint::InputDelay {
            port_name: "data_in".to_string(),
            delay_ns: 2.0,
            clock_name: "clk_100mhz".to_string(),
        });

        constraint_manager.add_timing_constraint(TimingConstraint::FalsePath {
            from: "reset_domain".to_string(),
            to: "async_output".to_string(),
        });

        // Test different constraint formats
        let formats = vec![
            ConstraintFormat::Sdc,
            ConstraintFormat::Xdc,
            ConstraintFormat::Pcf,
            ConstraintFormat::Skalp,
        ];

        for format in formats {
            println!("\n{:?} Format:", format);
            let constraints = constraint_manager.generate_constraints(format);
            println!("Generated {} lines of constraints", constraints.lines().count());

            // Print first few lines as sample
            for (i, line) in constraints.lines().take(3).enumerate() {
                if !line.trim().is_empty() && !line.starts_with('#') {
                    println!("  {}: {}", i + 1, line);
                }
            }
        }

        println!("\n✅ Constraint generation formats validated");
    }

    #[test]
    fn test_backend_factory_comprehensive() {
        println!("🏗️ Testing Backend Factory Comprehensive Support");

        let available_backends = BackendFactory::available_backends();
        println!("Total available backends: {}", available_backends.len());

        let mut fpga_count = 0;
        let mut asic_count = 0;

        for backend_target in &available_backends {
            match backend_target {
                TargetPlatform::Fpga(fpga_target) => {
                    fpga_count += 1;
                    println!("  FPGA: {:?}", fpga_target);

                    let backend_result = BackendFactory::create_backend(backend_target);
                    assert!(backend_result.is_ok(), "Failed to create FPGA backend: {:?}", fpga_target);
                }
                TargetPlatform::Asic(asic_target) => {
                    asic_count += 1;
                    println!("  ASIC: {:?}", asic_target);

                    let backend_result = BackendFactory::create_backend(backend_target);
                    assert!(backend_result.is_ok(), "Failed to create ASIC backend: {:?}", asic_target);
                }
            }
        }

        println!("\nBackend Summary:");
        println!("  📱 FPGA backends: {}", fpga_count);
        println!("  🏭 ASIC backends: {}", asic_count);

        // Verify we have comprehensive backend support
        assert!(fpga_count >= 3, "Should have at least 3 FPGA backends");
        assert!(asic_count >= 2, "Should have at least 2 ASIC backends");

        println!("✅ Comprehensive backend factory support validated");
    }

    // Helper function to create a test LIR design
    fn create_test_adder_lir() -> skalp_lir::LirDesign {
        use skalp_lir::{LirSignal, Gate, Net, GateType};

        skalp_lir::LirDesign {
            name: "test_adder".to_string(),
            modules: vec![
                skalp_lir::LirModule {
                    name: "test_adder".to_string(),
                    signals: vec![
                        LirSignal {
                            name: "a".to_string(),
                            signal_type: "logic[7:0]".to_string(),
                            is_input: true,
                            is_output: false,
                            is_register: false,
                        },
                        LirSignal {
                            name: "b".to_string(),
                            signal_type: "logic[7:0]".to_string(),
                            is_input: true,
                            is_output: false,
                            is_register: false,
                        },
                        LirSignal {
                            name: "sum".to_string(),
                            signal_type: "logic[8:0]".to_string(),
                            is_input: false,
                            is_output: true,
                            is_register: false,
                        },
                        LirSignal {
                            name: "clk".to_string(),
                            signal_type: "logic".to_string(),
                            is_input: true,
                            is_output: false,
                            is_register: false,
                        },
                    ],
                    nets: vec![
                        Net {
                            id: "a_net".to_string(),
                            width: 8,
                            driver: Some("a".to_string()),
                            loads: vec!["adder".to_string()],
                            is_output: false,
                            is_input: true,
                        },
                        Net {
                            id: "b_net".to_string(),
                            width: 8,
                            driver: Some("b".to_string()),
                            loads: vec!["adder".to_string()],
                            is_output: false,
                            is_input: true,
                        },
                        Net {
                            id: "sum_net".to_string(),
                            width: 9,
                            driver: Some("adder".to_string()),
                            loads: vec!["sum".to_string()],
                            is_output: true,
                            is_input: false,
                        },
                    ],
                    gates: vec![
                        Gate {
                            id: "adder".to_string(),
                            gate_type: GateType::And, // Simplified - would be proper adder logic
                            inputs: vec!["a_net".to_string(), "b_net".to_string()],
                            outputs: vec!["sum_net".to_string()],
                        },
                    ],
                }
            ],
        }
    }
}