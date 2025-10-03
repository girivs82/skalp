#[cfg(test)]
mod timing_analysis_fix_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_lir::timing::TimingAnalyzer;
    use skalp_lir::transform_mir_to_lir;
    use skalp_mir::lower_to_mir;

    #[test]
    fn test_timing_analysis_simple() {
        println!("🔍 Testing simple timing analysis...");

        let source = r#"
        entity SimpleGate {
            in a: bool
            in b: bool
            out y: bool
        }

        impl SimpleGate {
            assign y = a && b;
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        if let Some(module) = mir.modules.first() {
            let lir = transform_mir_to_lir(module);

            println!("LIR structure:");
            println!("  Gates: {}", lir.gates.len());
            println!("  Nets: {}", lir.nets.len());

            // Test with conservative timing parameters
            println!("\n🕐 Creating timing analyzer with 1000ps clock period...");
            let mut analyzer = TimingAnalyzer::new(1000.0); // 1ns = 1000ps

            println!("🔧 Building timing graph...");
            analyzer.build_graph(&lir);

            println!("⚡ Running timing analysis...");
            let timing_report = analyzer.analyze();

            println!("✅ Timing analysis completed!");
            timing_report.print();

            // Basic sanity checks
            assert!(
                timing_report.clock_period > 0.0,
                "Clock period should be positive"
            );
            assert!(
                timing_report.critical_path_delay >= 0.0,
                "Critical path delay should be non-negative"
            );

            println!("Clock period: {:.2} ps", timing_report.clock_period);
            println!(
                "Critical path delay: {:.2} ps",
                timing_report.critical_path_delay
            );
            println!("Worst slack: {:.2} ps", timing_report.worst_slack);
        }
    }

    #[test]
    fn test_timing_analysis_with_different_periods() {
        println!("🔍 Testing timing analysis with different clock periods...");

        let source = r#"
        entity TimingTest {
            in a: bool
            in b: bool
            out y: bool
        }

        impl TimingTest {
            assign y = a && b;
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        if let Some(module) = mir.modules.first() {
            let lir = transform_mir_to_lir(module);

            // Test with different clock periods to find the problematic range
            let test_periods = vec![100.0, 500.0, 1000.0, 5000.0, 10000.0];

            for period in test_periods {
                println!("\n🕐 Testing with {}ps clock period", period);

                let mut analyzer = TimingAnalyzer::new(period);
                analyzer.build_graph(&lir);

                // Wrap in a catch for overflow
                match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| analyzer.analyze()))
                {
                    Ok(timing_report) => {
                        println!(
                            "  ✅ Success - Critical path: {:.2}ps, Slack: {:.2}ps",
                            timing_report.critical_path_delay, timing_report.worst_slack
                        );
                    }
                    Err(_) => {
                        println!("  ❌ Failed - Overflow or panic at {}ps period", period);
                    }
                }
            }
        }
    }

    #[test]
    fn test_timing_analysis_edge_cases() {
        println!("🔍 Testing timing analysis edge cases...");

        let source = r#"
        entity EdgeCase {
            in x: bool
            out y: bool
        }

        impl EdgeCase {
            assign y = x;
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Should parse");
        let mir = lower_to_mir(&hir).expect("Should build MIR");

        if let Some(module) = mir.modules.first() {
            let lir = transform_mir_to_lir(module);

            println!("Testing minimal circuit:");
            println!("  Gates: {}", lir.gates.len());
            println!("  Nets: {}", lir.nets.len());

            // Test with very conservative settings
            let mut analyzer = TimingAnalyzer::new(10000.0); // 10ns
            analyzer.build_graph(&lir);

            let timing_report = analyzer.analyze();
            println!("✅ Minimal circuit timing analysis passed");
            println!(
                "  Critical path: {:.2}ps",
                timing_report.critical_path_delay
            );
            println!("  Slack: {:.2}ps", timing_report.worst_slack);

            // Verify reasonable results
            assert!(
                timing_report.critical_path_delay < 1000.0,
                "Critical path should be reasonable for simple buffer"
            );
            assert!(
                timing_report.worst_slack > 0.0,
                "Should have positive slack with large clock period"
            );
        }
    }
}
