#[cfg(test)]
mod flow_block_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};

    #[test]
    fn test_simple_pipeline_flow() {
        let source = r#"
        entity DataProcessor {
            in clk: clock
            in rst: reset
            in data_in: nat[8]
            out result: nat[8]
        }

        impl DataProcessor {
            signal temp1: nat[8] = 0
            signal temp2: nat[8] = 0

            on(clk.rise) {
                if (!rst) {
                    flow {
                        data_in |> temp1 |> temp2 |> result
                    }
                }
            }
        }
        "#;

        // Test HIR generation
        let hir = parse_and_build_hir(source).expect("Failed to parse");
        println!("HIR generation successful for simple pipeline");

        // Test that we have the expected structures
        assert_eq!(hir.entities.len(), 1);
        assert_eq!(hir.implementations.len(), 1);

        let implementation = &hir.implementations[0];

        // Verify we have the expected signals
        println!("Found {} signals:", implementation.signals.len());
        for signal in &implementation.signals {
            println!("  Signal: {}", signal.name);
        }
        assert!(
            implementation.signals.len() >= 2,
            "Should have temp1 and temp2 signals"
        );

        // Find the flow statement in the HIR
        println!("Found {} event blocks:", implementation.event_blocks.len());
        for (i, event_block) in implementation.event_blocks.iter().enumerate() {
            println!(
                "  Event block {}: {} statements",
                i,
                event_block.statements.len()
            );
            for (j, stmt) in event_block.statements.iter().enumerate() {
                match stmt {
                    skalp_frontend::hir::HirStatement::Flow(_) => {
                        println!("    Statement {}: Flow", j);
                    }
                    skalp_frontend::hir::HirStatement::If(if_stmt) => {
                        println!(
                            "    Statement {}: If with {} then statements",
                            j,
                            if_stmt.then_statements.len()
                        );
                        for (k, then_stmt) in if_stmt.then_statements.iter().enumerate() {
                            match then_stmt {
                                skalp_frontend::hir::HirStatement::Flow(_) => {
                                    println!("      Then statement {}: Flow", k);
                                }
                                skalp_frontend::hir::HirStatement::Assert(_) => {
                                    println!("      Then statement {}: Assert", k);
                                }
                                skalp_frontend::hir::HirStatement::Property(_) => {
                                    println!("      Then statement {}: Property", k);
                                }
                                skalp_frontend::hir::HirStatement::Cover(_) => {
                                    println!("      Then statement {}: Cover", k);
                                }
                                _ => {
                                    println!("      Then statement {}: Other", k);
                                }
                            }
                        }
                    }
                    skalp_frontend::hir::HirStatement::Assignment(_) => {
                        println!("    Statement {}: Assignment", j);
                    }
                    skalp_frontend::hir::HirStatement::Match(_) => {
                        println!("    Statement {}: Match", j);
                    }
                    skalp_frontend::hir::HirStatement::Block(stmts) => {
                        println!("    Statement {}: Block with {} statements", j, stmts.len());
                        for (k, sub_stmt) in stmts.iter().enumerate() {
                            match sub_stmt {
                                skalp_frontend::hir::HirStatement::Flow(_) => {
                                    println!("      Sub-statement {}: Flow", k);
                                }
                                skalp_frontend::hir::HirStatement::Assert(_) => {
                                    println!("      Sub-statement {}: Assert", k);
                                }
                                skalp_frontend::hir::HirStatement::Property(_) => {
                                    println!("      Sub-statement {}: Property", k);
                                }
                                skalp_frontend::hir::HirStatement::Cover(_) => {
                                    println!("      Sub-statement {}: Cover", k);
                                }
                                _ => {
                                    println!("      Sub-statement {}: Other", k);
                                }
                            }
                        }
                    }
                    skalp_frontend::hir::HirStatement::Assert(_) => {
                        println!("    Statement {}: Assert", j);
                    }
                    skalp_frontend::hir::HirStatement::Property(_) => {
                        println!("    Statement {}: Property", j);
                    }
                    skalp_frontend::hir::HirStatement::Cover(_) => {
                        println!("    Statement {}: Cover", j);
                    }
                    skalp_frontend::hir::HirStatement::Let(_) => {
                        println!("    Statement {}: Let", j);
                    }
                    skalp_frontend::hir::HirStatement::Return(_) => {
                        println!("    Statement {}: Return", j);
                    }
                    skalp_frontend::hir::HirStatement::Expression(_) => {
                        println!("    Statement {}: Expression", j);
                    }
                    skalp_frontend::hir::HirStatement::For(_) => {
                        println!("    Statement {}: For", j);
                    }
                    skalp_frontend::hir::HirStatement::Assume(_) => {
                        println!("    Statement {}: Assume", j);
                    }
                    skalp_frontend::hir::HirStatement::GenerateFor(_) => {
                        println!("    Statement {}: GenerateFor", j);
                    }
                    skalp_frontend::hir::HirStatement::GenerateIf(_) => {
                        println!("    Statement {}: GenerateIf", j);
                    }
                    skalp_frontend::hir::HirStatement::GenerateMatch(_) => {
                        println!("    Statement {}: GenerateMatch", j);
                    }
                }
            }
        }

        fn has_flow_in_statements(stmts: &[skalp_frontend::hir::HirStatement]) -> bool {
            stmts.iter().any(|stmt| match stmt {
                skalp_frontend::hir::HirStatement::Flow(_) => true,
                skalp_frontend::hir::HirStatement::Block(sub_stmts) => {
                    has_flow_in_statements(sub_stmts)
                }
                skalp_frontend::hir::HirStatement::If(if_stmt) => {
                    has_flow_in_statements(&if_stmt.then_statements)
                        || if_stmt
                            .else_statements
                            .as_ref()
                            .is_some_and(|else_stmts| has_flow_in_statements(else_stmts))
                }
                _ => false,
            })
        }

        let has_flow_statement = implementation
            .event_blocks
            .iter()
            .any(|event_block| has_flow_in_statements(&event_block.statements));

        assert!(has_flow_statement, "Should have flow statement in HIR");

        // Test MIR compilation
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        println!("MIR compilation successful for simple pipeline");
        println!("Module: {}", mir.modules[0].name);
        println!("Processes: {}", mir.modules[0].processes.len());

        println!("✅ Simple pipeline flow compiled successfully!");
    }

    #[test]
    fn test_pipeline_with_blocks() {
        let source = r#"
        entity ComplexProcessor {
            in clk: clock
            in data: nat[16]
            out processed: nat[16]
        }

        impl ComplexProcessor {
            signal stage1: nat[16] = 0
            signal stage2: nat[16] = 0

            on(clk.rise) {
                flow {
                    data |> {
                        stage1 <= data + 1
                    } |> {
                        stage2 <= stage1 * 2
                    } |> {
                        processed <= stage2 - 5
                    }
                }
            }
        }
        "#;

        // Test HIR generation
        let hir = parse_and_build_hir(source).expect("Failed to parse");
        println!("HIR generation successful for block pipeline");

        // Test MIR compilation
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        println!("MIR compilation successful for block pipeline");

        println!("✅ Pipeline with blocks compiled successfully!");
    }

    #[test]
    fn test_mixed_pipeline_stages() {
        let source = r#"
        entity MixedPipeline {
            in clk: clock
            in input_val: nat[8]
            out output_val: nat[8]
        }

        impl MixedPipeline {
            signal temp: nat[8] = 0

            on(clk.rise) {
                flow {
                    input_val |> temp |> {
                        output_val <= temp + 10
                    }
                }
            }
        }
        "#;

        // Test HIR generation
        let hir = parse_and_build_hir(source).expect("Failed to parse");
        println!("HIR generation successful for mixed pipeline");

        // Test MIR compilation
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        println!("MIR compilation successful for mixed pipeline");

        println!("✅ Mixed pipeline stages compiled successfully!");
    }
}
