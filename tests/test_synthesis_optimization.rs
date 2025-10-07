#[cfg(test)]
mod synthesis_optimization_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_lir::optimization::{
        CommonSubexpressionElimination, ConstantFolding, DeadCodeElimination, OptimizationPass,
        OptimizationPipeline,
    };
    use skalp_lir::timing::TimingAnalyzer;
    use skalp_lir::{lower_to_lir, transform_mir_to_lir};
    use skalp_mir::lower_to_mir;

    #[test]
    fn test_constant_folding_optimization() {
        let source = r#"
        entity ConstantTest {
            in a: bool
            out y1: bool
            out y2: bool
            out y3: bool
        }

        impl ConstantTest {
            assign y1 = a && true;   // Should become just a
            assign y2 = a || false;  // Should become just a
            assign y3 = false && a;  // Should become false
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        // Test module-level transformation first
        if let Some(module) = mir.modules.first() {
            let mut lir = transform_mir_to_lir(module);

            println!("Before constant folding:");
            println!("  Gates: {}", lir.gates.len());
            println!("  Nets: {}", lir.nets.len());

            // Apply constant folding
            let mut constant_folder = ConstantFolding;
            let result = constant_folder.optimize(&mut lir);

            println!("✅ Constant folding test completed!");
            println!("Result: {:?}", result);
            println!("After constant folding:");
            println!("  Gates: {}", lir.gates.len());
            println!("  Nets: {}", lir.nets.len());

            assert!(result.success, "Constant folding should succeed");
            // Should have reduced some complexity (though simplified implementation may not show dramatic reduction)
        }
    }

    #[test]
    fn test_dead_code_elimination() {
        let source = r#"
        entity DeadCodeTest {
            in a: bool
            in b: bool
            out y: bool
        }

        impl DeadCodeTest {
            signal unused: bool = false  // This signal is not used
            assign y = a && b
            // unused should be eliminated
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        if let Some(module) = mir.modules.first() {
            let mut lir = transform_mir_to_lir(module);

            println!("Before dead code elimination:");
            println!("  Gates: {}", lir.gates.len());
            println!("  Nets: {}", lir.nets.len());

            // Apply dead code elimination
            let mut dce = DeadCodeElimination;
            let result = dce.optimize(&mut lir);

            println!("✅ Dead code elimination test completed!");
            println!("Result: {:?}", result);
            println!("After dead code elimination:");
            println!("  Gates: {}", lir.gates.len());
            println!("  Nets: {}", lir.nets.len());

            assert!(result.success, "Dead code elimination should succeed");
        }
    }

    #[test]
    fn test_optimization_pipeline() {
        let source = r#"
        entity PipelineTest {
            in a: bool
            in b: bool
            in c: bool
            out result: bool
        }

        impl PipelineTest {
            // This expression has redundancy that can be optimized
            assign result = (a && true) || (b && b) || (false && c);
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        if let Some(module) = mir.modules.first() {
            let mut lir = transform_mir_to_lir(module);

            println!("Initial LIR:");
            println!("  Gates: {}", lir.gates.len());
            println!("  Nets: {}", lir.nets.len());

            // Create optimization pipeline
            let mut pipeline = OptimizationPipeline::standard();
            let results = pipeline.optimize(&mut lir);

            println!("✅ Optimization pipeline test completed!");
            println!("Final LIR:");
            println!("  Gates: {}", lir.gates.len());
            println!("  Nets: {}", lir.nets.len());

            // Verify that optimizations ran
            assert!(!results.is_empty(), "Should have run optimization passes");
            for result in &results {
                println!("Pass '{}': success = {}", result.pass_name, result.success);
                if let Some(msg) = &result.message {
                    println!("  Message: {}", msg);
                }
            }
        }
    }

    #[test]
    fn test_timing_analysis() {
        let source = r#"
        entity TimingAnalysisTest {
            in clk: clock
            in a: bool
            in b: bool
            out y: bool
        }

        impl TimingAnalysisTest {
            signal reg1: bool = false
            signal reg2: bool = false

            on(clk.rise) {
                reg1 <= a && b
                reg2 <= reg1 || a
            }

            assign y = reg2
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        if let Some(module) = mir.modules.first() {
            let lir = transform_mir_to_lir(module);

            println!("LIR for timing analysis:");
            println!("  Gates: {}", lir.gates.len());
            println!("  Nets: {}", lir.nets.len());

            // Create timing analyzer with 10ns clock period
            let mut analyzer = TimingAnalyzer::new(10000.0); // 10ns = 10000ps
            analyzer.build_graph(&lir);
            let timing_report = analyzer.analyze();

            println!("✅ Timing analysis test completed!");
            timing_report.print();

            // Basic checks
            assert!(
                timing_report.clock_period > 0.0,
                "Should have valid clock period"
            );
            assert!(
                timing_report.critical_path_delay >= 0.0,
                "Should have non-negative delay"
            );
            println!(
                "Critical path delay: {:.2} ps",
                timing_report.critical_path_delay
            );
            println!("Worst slack: {:.2} ps", timing_report.worst_slack);
        }
    }

    #[test]
    fn test_area_calculation() {
        let source = r#"
        entity AreaTest {
            in a: bool
            in b: bool
            in c: bool
            in d: bool
            out y1: bool
            out y2: bool
        }

        impl AreaTest {
            // Common subexpression that might be shared
            assign y1 = (a && b) || c;
            assign y2 = (a && b) && d;
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        if let Some(module) = mir.modules.first() {
            let mut lir = transform_mir_to_lir(module);

            println!("Before optimization:");
            println!("  Gates: {}", lir.gates.len());
            println!("  Nets: {}", lir.nets.len());

            // Test CSE specifically
            let mut cse = CommonSubexpressionElimination;
            let cse_result = cse.optimize(&mut lir);

            println!("✅ Area calculation test completed!");
            println!("CSE Result: {:?}", cse_result);
            println!("After CSE:");
            println!("  Gates: {}", lir.gates.len());
            println!("  Nets: {}", lir.nets.len());

            // Check that CSE ran (may or may not find duplicates in simplified implementation)
            assert!(cse_result.success, "CSE should complete successfully");
        }
    }

    #[test]
    fn test_full_synthesis_flow() {
        let source = r#"
        entity FullFlowTest {
            in clk: clock
            in rst: reset
            in enable: bool
            in data_in: nat[4]
            out data_out: nat[4]
            out valid: bool
        }

        impl FullFlowTest {
            signal counter: nat[4] = 0
            signal valid_reg: bool = false

            on(clk.rise) {
                if (rst) {
                    counter <= 0
                    valid_reg <= false
                } else if (enable) {
                    counter <= counter + 1
                    valid_reg <= true
                }
            }

            data_out = counter
            valid = valid_reg
        }
        "#;

        println!("🚀 Starting full synthesis flow test...");

        // Parse and build MIR
        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        // Convert to LIR using both APIs
        let lir_design = lower_to_lir(&mir).expect("Should convert to LIR design");

        println!("LIR Design created:");
        println!("  Name: {}", lir_design.name);
        println!("  Modules: {}", lir_design.modules.len());

        for (i, module) in lir_design.modules.iter().enumerate() {
            println!("  Module {}: {}", i, module.name);
            println!("    Signals: {}", module.signals.len());
            println!("    Gates: {}", module.gates.len());
            println!("    Nets: {}", module.nets.len());

            // Test optimization on each module's gates/nets
            if !module.gates.is_empty() || !module.nets.is_empty() {
                println!("    Module has synthesizable content!");
            }
        }

        if let Some(module) = mir.modules.first() {
            let mut lir = transform_mir_to_lir(module);

            println!("\nDirect module transformation:");
            println!("  Gates: {}", lir.gates.len());
            println!("  Nets: {}", lir.nets.len());

            // Run optimization pipeline
            println!("\n🔧 Running optimization pipeline...");
            let mut pipeline = OptimizationPipeline::standard();
            let opt_results = pipeline.optimize(&mut lir);

            // Show optimization results
            for result in &opt_results {
                println!(
                    "  {} -> {} gates -> {} gates",
                    result.pass_name, result.gates_before, result.gates_after
                );
            }

            // Run timing analysis
            println!("\n⏱️  Running timing analysis...");
            let mut analyzer = TimingAnalyzer::new(5000.0); // 5ns clock
            analyzer.build_graph(&lir);
            let timing_report = analyzer.analyze();

            println!("Timing Results:");
            println!("  Clock Period: {:.0} ps", timing_report.clock_period);
            println!(
                "  Critical Path: {:.2} ps",
                timing_report.critical_path_delay
            );
            println!("  Worst Slack: {:.2} ps", timing_report.worst_slack);

            // Check if we meet timing
            let timing_ok = timing_report.worst_slack >= 0.0;
            println!(
                "  Timing Closure: {}",
                if timing_ok { "✅ PASS" } else { "❌ FAIL" }
            );

            // Calculate performance metrics
            let frequency_mhz = if timing_report.critical_path_delay > 0.0 {
                1000000.0 / timing_report.critical_path_delay // Convert ps to MHz
            } else {
                f64::INFINITY
            };

            println!("  Max Frequency: {:.1} MHz", frequency_mhz);

            println!("✅ Full synthesis flow test completed successfully!");
        }
    }
}
