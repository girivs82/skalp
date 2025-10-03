//! Backend demonstration example
//!
//! Shows how to use the SKALP backends framework to synthesize designs
//! for both FPGA and ASIC targets.

use skalp_backends::*;
use std::collections::HashMap;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== SKALP Backend Framework Demo ===\n");

    // Create a mock LIR design for demonstration
    let demo_design = create_demo_design();

    // Demo 1: FPGA synthesis (iCE40)
    println!("1. FPGA Synthesis Demo (iCE40)");
    let fpga_result = demo_fpga_synthesis(&demo_design).await?;
    print_synthesis_summary("iCE40 FPGA", &fpga_result);

    // Demo 2: ASIC synthesis (Generic 45nm)
    println!("2. ASIC Synthesis Demo (Generic 45nm)");
    let asic_result = demo_asic_synthesis(&demo_design).await?;
    print_synthesis_summary("Generic 45nm ASIC", &asic_result);

    // Demo 3: Timing analysis
    println!("3. Timing Analysis Demo");
    demo_timing_analysis().await?;

    // Demo 4: Power analysis
    println!("4. Power Analysis Demo");
    demo_power_analysis().await?;

    // Demo 5: Constraint management
    println!("5. Constraint Management Demo");
    demo_constraint_management().await?;

    println!("\n=== Demo Complete ===");
    println!("The SKALP backend framework successfully demonstrates:");
    println!("• Multi-target synthesis (FPGA and ASIC)");
    println!("• Comprehensive timing analysis");
    println!("• Power estimation and optimization");
    println!("• Constraint parsing and validation");
    println!("• Tool integration frameworks");

    Ok(())
}

/// Create a mock design for demonstration
fn create_demo_design() -> mock_lir::Design {
    mock_lir::Design {
        name: "demo_counter".to_string(),
        ports: vec![
            mock_lir::Port {
                name: "clk".to_string(),
                direction: mock_lir::PortDirection::Input,
                width: 1,
            },
            mock_lir::Port {
                name: "reset".to_string(),
                direction: mock_lir::PortDirection::Input,
                width: 1,
            },
            mock_lir::Port {
                name: "enable".to_string(),
                direction: mock_lir::PortDirection::Input,
                width: 1,
            },
            mock_lir::Port {
                name: "count".to_string(),
                direction: mock_lir::PortDirection::Output,
                width: 8,
            },
        ],
        wires: vec![
            mock_lir::Wire {
                name: "count_next".to_string(),
                width: 8,
                source: "adder output".to_string(),
            },
            mock_lir::Wire {
                name: "count_reg".to_string(),
                width: 8,
                source: "counter register".to_string(),
            },
        ],
        gates: vec![
            // Counter register
            mock_lir::Gate {
                id: 1,
                gate_type: mock_lir::GateType::FlipFlop,
                inputs: vec![
                    "count_next".to_string(),
                    "clk".to_string(),
                    "reset".to_string(),
                ],
                outputs: vec!["count_reg".to_string()],
                parameters: HashMap::new(),
            },
            // Increment logic (simplified)
            mock_lir::Gate {
                id: 2,
                gate_type: mock_lir::GateType::And,
                inputs: vec!["count_reg".to_string(), "enable".to_string()],
                outputs: vec!["count_next".to_string()],
                parameters: HashMap::new(),
            },
            // Output buffer
            mock_lir::Gate {
                id: 3,
                gate_type: mock_lir::GateType::Buffer,
                inputs: vec!["count_reg".to_string()],
                outputs: vec!["count".to_string()],
                parameters: HashMap::new(),
            },
        ],
    }
}

/// Demo FPGA synthesis
async fn demo_fpga_synthesis(design: &mock_lir::Design) -> BackendResult<SynthesisResults> {
    let target = TargetPlatform::Fpga(FpgaTarget::Ice40 {
        part: "iCE40HX8K".to_string(),
        package: "CT256".to_string(),
    });

    let config = SynthesisConfig {
        target: target.clone(),
        optimization: OptimizationGoals {
            primary: OptimizationTarget::Performance,
            max_area_utilization: Some(0.8),
            target_frequency: Some(100.0),
            max_power: Some(500.0),
        },
        timing_constraints: vec![TimingConstraint::ClockPeriod {
            clock_name: "clk".to_string(),
            period_ns: 10.0, // 100 MHz
        }],
        power_constraints: None,
        output_dir: "/tmp/fpga_demo".to_string(),
        tool_options: HashMap::new(),
    };

    let backend = BackendFactory::create_backend(&target)?;
    backend.synthesize(design, &config).await
}

/// Demo ASIC synthesis
async fn demo_asic_synthesis(design: &mock_lir::Design) -> BackendResult<SynthesisResults> {
    let target = TargetPlatform::Asic(AsicTarget::Generic {
        library_name: "demo_stdcells".to_string(),
        process_node: "45nm".to_string(),
    });

    let config = SynthesisConfig {
        target: target.clone(),
        optimization: OptimizationGoals {
            primary: OptimizationTarget::Area,
            max_area_utilization: Some(0.7),
            target_frequency: Some(500.0),
            max_power: Some(100.0),
        },
        timing_constraints: vec![TimingConstraint::ClockPeriod {
            clock_name: "clk".to_string(),
            period_ns: 2.0, // 500 MHz
        }],
        power_constraints: Some(PowerConstraints {
            max_dynamic_power: Some(80.0),
            max_static_power: Some(20.0),
            operating_voltage: 1.1,
            operating_temperature: 85.0,
        }),
        output_dir: "/tmp/asic_demo".to_string(),
        tool_options: HashMap::new(),
    };

    let backend = BackendFactory::create_backend(&target)?;
    backend.synthesize(design, &config).await
}

/// Demo timing analysis
async fn demo_timing_analysis() -> BackendResult<()> {
    use timing::*;

    let mut analyzer = create_timing_analyzer();

    // Add constraints
    let constraints = vec![
        TimingConstraint::ClockPeriod {
            clock_name: "clk".to_string(),
            period_ns: 8.0, // 125 MHz
        },
        TimingConstraint::InputDelay {
            port_name: "enable".to_string(),
            delay_ns: 2.0,
            clock_name: "clk".to_string(),
        },
        TimingConstraint::OutputDelay {
            port_name: "count".to_string(),
            delay_ns: 3.0,
            clock_name: "clk".to_string(),
        },
    ];

    analyzer.apply_constraints(&constraints)?;

    // Run analysis
    let results = analyzer.analyze_timing("demo_counter.v")?;

    // Generate report
    let report = analyzer.generate_report(&results);
    println!("{}", report);

    Ok(())
}

/// Demo power analysis
async fn demo_power_analysis() -> BackendResult<()> {
    use power::*;

    let analyzer = create_power_analyzer();

    // Mock cell counts for analysis
    let mut cell_counts = HashMap::new();
    cell_counts.insert("DFF_X1".to_string(), 8); // 8-bit counter
    cell_counts.insert("AND2_X1".to_string(), 4); // Enable logic
    cell_counts.insert("INV_X1".to_string(), 2); // Inverters

    let results = analyzer.analyze_power(
        "ASIC_45nm",
        &cell_counts,
        125.0,       // 125 MHz
        Some(100.0), // 100 um^2 area
    )?;

    // Check constraints
    let constraints = PowerConstraints {
        max_dynamic_power: Some(50.0),
        max_static_power: Some(10.0),
        operating_voltage: 1.1,
        operating_temperature: 25.0,
    };

    let violations = analyzer.check_constraints(&results, &Some(constraints));
    if violations.is_empty() {
        println!("✅ Power constraints satisfied");
    } else {
        println!("❌ Power constraint violations:");
        for violation in violations {
            println!("   {}", violation);
        }
    }

    // Generate report
    let report = analyzer.generate_report(&results);
    println!("{}", report);

    Ok(())
}

/// Demo constraint management
async fn demo_constraint_management() -> BackendResult<()> {
    use constraints::*;

    let mut manager = ConstraintManager::new();

    // Load constraints from different formats
    let sdc_content = r#"
# Demo SDC constraints
create_clock -period 8.0 [get_ports clk]
set_input_delay -clock clk 2.0 [get_ports enable]
set_output_delay -clock clk 3.0 [get_ports count]
"#;

    manager.parse_constraints(sdc_content, ConstraintFormat::Sdc)?;

    // Add pin constraints
    let pin_constraint = PinConstraint {
        signal_name: "clk".to_string(),
        location: "A1".to_string(),
        io_standard: Some("LVCMOS33".to_string()),
        drive_strength: Some("12mA".to_string()),
        slew_rate: Some(SlewRate::Fast),
        termination: None,
    };

    manager.add_pin_constraint(pin_constraint);

    // Validate constraints
    let errors = manager.validate();
    if errors.is_empty() {
        println!("✅ All constraints valid");
    } else {
        println!("❌ Constraint validation errors:");
        for error in errors {
            println!("   {}", error);
        }
    }

    // Generate different formats
    println!("Generated SDC format:");
    println!("{}", manager.generate_constraints(ConstraintFormat::Sdc));

    println!("Generated PCF format:");
    println!("{}", manager.generate_constraints(ConstraintFormat::Pcf));

    Ok(())
}

/// Print synthesis summary
fn print_synthesis_summary(target_name: &str, results: &SynthesisResults) {
    println!("  Target: {}", target_name);
    println!("  Success: {}", if results.success { "✅" } else { "❌" });

    if let Some(luts) = results.area_metrics.luts_used {
        println!("  LUTs used: {}", luts);
    }
    println!("  Flip-flops: {}", results.area_metrics.flip_flops_used);

    if let Some(area) = results.area_metrics.cell_area_um2 {
        println!("  Area: {:.1} um²", area);
    }

    println!(
        "  Utilization: {:.1}%",
        results.area_metrics.utilization_percent
    );
    println!(
        "  Max frequency: {:.1} MHz",
        results.timing_results.max_frequency_mhz
    );
    println!(
        "  Total power: {:.1} mW",
        results.power_results.total_power_mw
    );
    println!("  Output files: {}", results.output_files.len());
    println!("  Log messages: {}", results.log_messages.len());
    println!();
}
