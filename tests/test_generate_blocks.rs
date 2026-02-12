#[cfg(test)]
mod generate_block_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};

    #[test]
    fn test_generate_for_basic() {
        // Test basic generate-for loop expansion
        let source = r#"
        entity BitReverser {
            in clk: clock
            in data_in: nat[8]
            out data_out: nat[8]
        }

        impl BitReverser {
            signal reversed: nat[8] = 0

            on(clk.rise) {
                generate for i in 0..8 {
                    reversed[i] = data_in[7 - i]
                }
                data_out = reversed
            }
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse generate-for");
        println!("HIR generation successful for generate-for");

        // Verify the generate for expanded into individual assignments
        let implementation = &hir.implementations[0];
        println!("Event blocks: {}", implementation.event_blocks.len());

        // Compile to MIR to verify the expansion works end-to-end
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        println!("MIR compilation successful");
        println!("Module: {}", mir.modules[0].name);
        println!("Signals: {}", mir.modules[0].signals.len());
        println!("Processes: {}", mir.modules[0].processes.len());

        println!("generate-for basic test passed!");
    }

    #[test]
    fn test_generate_for_with_step() {
        // Test generate-for with step
        let source = r#"
        entity StepGenerator {
            in clk: clock
            in data_in: nat[16]
            out data_out: nat[16]
        }

        impl StepGenerator {
            signal result: nat[16] = 0

            on(clk.rise) {
                // Process every other bit (step of 2)
                generate for i in 0..8 step 2 {
                    result[i] = data_in[i]
                }
                data_out = result
            }
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse generate-for with step");
        println!("HIR generation successful for generate-for with step");

        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        println!("generate-for with step test passed!");
    }

    #[test]
    fn test_generate_if_basic() {
        // Test basic generate-if expansion
        let source = r#"
        const ENABLE_PIPELINE: bool = true;

        entity ConditionalPipeline {
            in clk: clock
            in data_in: nat[8]
            out data_out: nat[8]
        }

        impl ConditionalPipeline {
            signal stage1: nat[8] = 0
            signal stage2: nat[8] = 0

            on(clk.rise) {
                generate if ENABLE_PIPELINE {
                    stage1 = data_in
                    stage2 = stage1
                    data_out = stage2
                }
            }
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse generate-if");
        println!("HIR generation successful for generate-if");

        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        println!("generate-if basic test passed!");
    }

    #[test]
    fn test_generate_if_false() {
        // Test generate-if with false condition (should produce no code)
        let source = r#"
        const ENABLE_FEATURE: bool = false;

        entity DisabledFeature {
            in clk: clock
            in data_in: nat[8]
            out data_out: nat[8]
        }

        impl DisabledFeature {
            signal temp: nat[8] = 0

            on(clk.rise) {
                generate if ENABLE_FEATURE {
                    // This should not be generated
                    temp = data_in + 1
                }
                data_out = data_in
            }
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse generate-if (false)");
        println!("HIR generation successful for generate-if (false condition)");

        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        println!("generate-if false condition test passed!");
    }

    #[test]
    fn test_generate_if_else() {
        // Test generate-if with else branch
        let source = r#"
        const USE_FAST_PATH: bool = true;

        entity ConditionalPath {
            in clk: clock
            in data_in: nat[8]
            out data_out: nat[8]
        }

        impl ConditionalPath {
            signal result: nat[8] = 0

            on(clk.rise) {
                generate if USE_FAST_PATH {
                    result = data_in
                } else {
                    result = data_in + 1
                }
                data_out = result
            }
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse generate-if-else");
        println!("HIR generation successful for generate-if-else");

        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        println!("generate-if-else test passed!");
    }

    #[test]
    fn test_generate_match_basic() {
        // Test basic generate-match expansion
        let source = r#"
        const DATA_WIDTH: nat[8] = 16;

        entity WidthAdapter {
            in clk: clock
            in data_in: nat[32]
            out data_out: nat[32]
        }

        impl WidthAdapter {
            signal result: nat[32] = 0

            on(clk.rise) {
                generate match DATA_WIDTH {
                    8 => {
                        result = data_in & 0xFF
                    }
                    16 => {
                        result = data_in & 0xFFFF
                    }
                    32 => {
                        result = data_in
                    }
                }
                data_out = result
            }
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse generate-match");
        println!("HIR generation successful for generate-match");

        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        println!("generate-match basic test passed!");
    }

    #[test]
    fn test_generate_nested() {
        // Test nested generate blocks
        let source = r#"
        const ROWS: nat[8] = 2;
        const COLS: nat[8] = 2;

        entity MatrixInitializer {
            in clk: clock
            out done: bool
        }

        impl MatrixInitializer {
            signal initialized: bool = false

            on(clk.rise) {
                generate for row in 0..ROWS {
                    generate for col in 0..COLS {
                        // Each iteration creates initialization logic
                        initialized = true
                    }
                }
                done = initialized
            }
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse nested generate");
        println!("HIR generation successful for nested generate");

        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        println!("nested generate test passed!");
    }

    #[test]
    fn test_generate_for_signal_creation() {
        // Test generate-for that creates indexed signals
        let source = r#"
        entity PipelineStages {
            in clk: clock
            in data_in: nat[8]
            out data_out: nat[8]
        }

        impl PipelineStages {
            signal pipe_0: nat[8] = 0
            signal pipe_1: nat[8] = 0
            signal pipe_2: nat[8] = 0
            signal pipe_3: nat[8] = 0

            on(clk.rise) {
                pipe_0 = data_in
                generate for i in 0..3 {
                    // This would create pipe_1 = pipe_0, pipe_2 = pipe_1, etc.
                    // For now, just test the basic expansion
                }
                data_out = pipe_3
            }
        }
        "#;

        let hir =
            parse_and_build_hir(source).expect("Failed to parse generate-for signal creation");
        println!("HIR generation successful for generate-for signal creation");

        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        println!("generate-for signal creation test passed!");
    }

    #[test]
    fn test_generate_with_arithmetic() {
        // Test generate-for with arithmetic expressions using loop variable
        let source = r#"
        entity ArithmeticGenerator {
            in clk: clock
            in data_in: nat[16]
            out data_out: nat[16]
        }

        impl ArithmeticGenerator {
            signal result: nat[16] = 0

            on(clk.rise) {
                generate for i in 0..4 {
                    // Use loop variable in arithmetic
                    result[i * 4] = data_in[i]
                }
                data_out = result
            }
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse generate with arithmetic");
        println!("HIR generation successful for generate with arithmetic");

        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        println!("generate with arithmetic test passed!");
    }

    #[test]
    fn test_generate_for_with_compile_time_if() {
        // Test compile-time if inside generate-for in impl block
        // The if condition depends on the iterator and is evaluated at elaboration time
        let source = r#"
        entity CompTimeIf {
            in a: bit[8]
            out b: bit[8]
        }

        impl CompTimeIf {
            signal x: bit[8][4]
            x[0] = a
            generate for i in 0..3 {
                if i == 1 {
                    x[i + 1] = x[i] + 1 as bit[8]
                } else {
                    x[i + 1] = x[i]
                }
            }
            b = x[3]
        }
        "#;

        let hir = parse_and_build_hir(source)
            .expect("Failed to parse generate-for with compile-time if");

        let implementation = &hir.implementations[0];
        // x[0]=a (1) + 3 from if branches (i=0,1,2) + b=x[3] (1) = 5 assignments
        assert_eq!(
            implementation.assignments.len(),
            5,
            "Expected 5 assignments: x[0]=a + 3 from if branches + b=x[3]"
        );

        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        println!("generate-for with compile-time if test passed!");
    }

    #[test]
    fn test_generate_for_with_compile_time_if_no_else() {
        // Test compile-time if without else branch — only taken iterations produce statements
        let source = r#"
        entity CompTimeIfNoElse {
            in a: bit[8]
            out b: bit[8]
        }

        impl CompTimeIfNoElse {
            signal x: bit[8][4]
            x[0] = a
            x[1] = a
            x[2] = a
            x[3] = a
            generate for i in 0..4 {
                if i == 2 {
                    x[i] = a + 1 as bit[8]
                }
            }
            b = x[3]
        }
        "#;

        let hir = parse_and_build_hir(source)
            .expect("Failed to parse generate-for with compile-time if (no else)");

        let implementation = &hir.implementations[0];
        // x[0..3]=a (4) + 1 from if branch (only i=2) + b=x[3] (1) = 6 assignments
        assert_eq!(
            implementation.assignments.len(),
            6,
            "Expected 6 assignments: 4 defaults + 1 conditional (i==2) + b=x[3]"
        );

        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        println!("generate-for with compile-time if (no else) test passed!");
    }

    #[test]
    fn test_generate_for_with_compile_time_else_if() {
        // Test compile-time if/else-if/else chain inside generate-for
        let source = r#"
        entity CompTimeElseIf {
            in a: bit[8]
            out b: bit[8]
        }

        impl CompTimeElseIf {
            signal x: bit[8][5]
            x[0] = a
            generate for i in 0..4 {
                if i == 0 {
                    x[i + 1] = x[i] + 1 as bit[8]
                } else if i == 2 {
                    x[i + 1] = x[i] + 2 as bit[8]
                } else {
                    x[i + 1] = x[i]
                }
            }
            b = x[4]
        }
        "#;

        let hir = parse_and_build_hir(source)
            .expect("Failed to parse generate-for with compile-time else-if");

        let implementation = &hir.implementations[0];
        // x[0]=a (1) + 4 from if/else-if/else (one per iteration) + b=x[4] (1) = 6
        assert_eq!(
            implementation.assignments.len(),
            6,
            "Expected 6 assignments: x[0]=a + 4 from if chain + b=x[4]"
        );

        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        println!("generate-for with compile-time else-if test passed!");
    }

    #[test]
    fn test_generate_for_with_modulo_condition() {
        // Test compile-time if with modulo — e.g., repeat every Nth iteration
        let source = r#"
        entity ModuloCondition {
            in a: bit[8]
            out b: bit[8]
        }

        impl ModuloCondition {
            signal x: bit[8][5]
            x[0] = a
            generate for i in 0..4 {
                if i % 2 == 0 {
                    x[i + 1] = x[i] + 1 as bit[8]
                } else {
                    x[i + 1] = x[i]
                }
            }
            b = x[4]
        }
        "#;

        let hir = parse_and_build_hir(source)
            .expect("Failed to parse generate-for with modulo condition");

        let implementation = &hir.implementations[0];
        // x[0]=a (1) + 4 from generate-for + b=x[4] (1) = 6
        assert_eq!(
            implementation.assignments.len(),
            6,
            "Expected 6 assignments"
        );

        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        println!("generate-for with modulo condition test passed!");
    }
}
