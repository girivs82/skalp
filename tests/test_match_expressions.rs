#[cfg(test)]
mod match_expression_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};

    #[test]
    #[ignore = "Test hangs during full suite - passes individually, test isolation issue"]
    fn test_state_machine_with_match() {
        let source = r#"
        entity StateMachine {
            in clk: clock
            in rst: reset
            in ready: bool
            out state_out: nat[2]
        }

        impl StateMachine {
            signal current_state: nat[2] = 0
            signal next_state: nat[2] = 0

            on(clk.rise) {
                if (rst) {
                    current_state = 0
                } else {
                    current_state = next_state
                }

                // Use match expression with guards and literal patterns inside event block
                match current_state {
                    0 => next_state = 1,
                    1 if ready => next_state = 2,
                    1 => next_state = 1,
                    2 => next_state = 0,
                    _ => next_state = 0,
                }
            }

            state_out = current_state
        }
        "#;

        // Test HIR generation
        let hir = parse_and_build_hir(source).expect("Failed to parse");
        println!("HIR generation successful");

        // Test that we have the expected structures
        assert_eq!(hir.entities.len(), 1);
        assert_eq!(hir.implementations.len(), 1);

        let implementation = &hir.implementations[0];
        // Verify we have the expected signals
        assert!(
            implementation.signals.len() >= 2,
            "Should have current_state and next_state signals"
        );

        // Find the state signals
        let current_state = implementation
            .signals
            .iter()
            .find(|s| s.name == "current_state");
        let next_state = implementation
            .signals
            .iter()
            .find(|s| s.name == "next_state");
        assert!(current_state.is_some(), "Should have current_state signal");
        assert!(next_state.is_some(), "Should have next_state signal");

        // Test MIR compilation
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        println!("MIR compilation successful");
        println!("Module: {}", mir.modules[0].name);
        println!("Processes: {}", mir.modules[0].processes.len());

        // Verify that match statement was converted to case statement
        let has_case_statement = mir.modules[0].processes.iter().any(|process| {
            process
                .body
                .statements
                .iter()
                .any(|stmt| matches!(stmt, skalp_mir::Statement::Case(_)))
        });

        assert!(
            has_case_statement,
            "Should have case statement from match expression"
        );

        println!("✅ State machine with match expressions compiled successfully!");
    }

    #[test]
    #[ignore = "Test hangs during full suite - passes individually, test isolation issue"]
    fn test_simple_match_with_literals() {
        let source = r#"
        entity SimpleMatch {
            in clk: clock
            in input_val: nat[4]
            out result: nat[8]
        }

        impl SimpleMatch {
            on(clk.rise) {
                match input_val {
                    0 => result = 10,
                    1 => result = 20,
                    2 => result = 30,
                    _ => result = 0,
                }
            }
        }
        "#;

        // Test HIR generation
        let hir = parse_and_build_hir(source).expect("Failed to parse");
        println!("HIR generation successful for simple match");

        // Test MIR compilation
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        println!("MIR compilation successful for simple match");

        // Verify that match statement was converted to case statement
        let has_case_statement = mir.modules[0].processes.iter().any(|process| {
            process
                .body
                .statements
                .iter()
                .any(|stmt| matches!(stmt, skalp_mir::Statement::Case(_)))
        });

        assert!(
            has_case_statement,
            "Should have case statement from literal match"
        );

        println!("✅ Simple literal match compiled successfully!");
    }

    #[test]
    #[ignore = "Test hangs during full suite - passes individually, test isolation issue"]
    fn test_match_with_guards() {
        let source = r#"
        entity GuardedMatch {
            in clk: clock
            in state_val: nat[2]
            in enable: bool
            out output: nat[4]
        }

        impl GuardedMatch {
            on(clk.rise) {
                match state_val {
                    0 if enable => output = 5,
                    0 => output = 1,
                    1 if enable => output = 6,
                    1 => output = 2,
                    _ => output = 0,
                }
            }
        }
        "#;

        // Test HIR generation
        let hir = parse_and_build_hir(source).expect("Failed to parse");
        println!("HIR generation successful for guarded match");

        // Test MIR compilation
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        println!("MIR compilation successful for guarded match");

        println!("✅ Guarded match compiled successfully!");
    }
}
