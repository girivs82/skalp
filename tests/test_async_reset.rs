#[cfg(test)]
mod async_reset_tests {
    use skalp_codegen::generate_systemverilog_from_mir;
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};

    #[test]
    fn test_async_reset_active_high() {
        let source = r#"
        entity AsyncResetTest {
            in clk: clock
            in rst: reset(active_high)
            in data: nat[8]
            out result: nat[8]
        }

        impl AsyncResetTest {
            signal state: nat[8]

            on(clk.rise, rst.active) {
                if rst {
                    state = 0
                } else {
                    state = data
                }
            }

            result = state
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse async reset");
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        let sv = generate_systemverilog_from_mir(&mir).expect("Failed to generate SV");
        println!("Generated SV:\n{}", sv);

        // active_high reset + .active → posedge rst
        assert!(
            sv.contains("posedge clk or posedge rst"),
            "Expected 'posedge clk or posedge rst' in SV output, got:\n{}",
            sv
        );
    }

    #[test]
    fn test_async_reset_active_low() {
        let source = r#"
        entity AsyncResetLowTest {
            in clk: clock
            in rst_n: reset(active_low)
            in data: nat[8]
            out result: nat[8]
        }

        impl AsyncResetLowTest {
            signal state: nat[8]

            on(clk.rise, rst_n.active) {
                if rst_n {
                    state = 0
                } else {
                    state = data
                }
            }

            result = state
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse async reset active_low");
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        let sv = generate_systemverilog_from_mir(&mir).expect("Failed to generate SV");
        println!("Generated SV:\n{}", sv);

        // active_low reset + .active → negedge rst_n
        assert!(
            sv.contains("posedge clk or negedge rst_n"),
            "Expected 'posedge clk or negedge rst_n' in SV output, got:\n{}",
            sv
        );
    }

    #[test]
    fn test_sync_reset_unchanged() {
        // Ensure that sync reset (no .active/.inactive in on()) still works the same
        let source = r#"
        entity SyncResetTest {
            in clk: clock
            in rst: reset(active_high)
            in data: nat[8]
            out result: nat[8]
        }

        impl SyncResetTest {
            signal state: nat[8]

            on(clk.rise) {
                if rst {
                    state = 0
                } else {
                    state = data
                }
            }

            result = state
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse sync reset");
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        let sv = generate_systemverilog_from_mir(&mir).expect("Failed to generate SV");
        println!("Generated SV:\n{}", sv);

        // Sync reset: only posedge clk, no rst in sensitivity list
        assert!(
            sv.contains("posedge clk)"),
            "Expected sync reset to only have 'posedge clk' in sensitivity list, got:\n{}",
            sv
        );
        assert!(
            !sv.contains("posedge clk or"),
            "Sync reset should NOT have rst in sensitivity list, got:\n{}",
            sv
        );
    }

    #[test]
    fn test_inactive_edge_type() {
        let source = r#"
        entity InactiveTest {
            in clk: clock
            in rst: reset(active_high)
            in data: nat[8]
            out result: nat[8]
        }

        impl InactiveTest {
            signal state: nat[8]

            on(clk.rise, rst.inactive) {
                if rst {
                    state = 0
                } else {
                    state = data
                }
            }

            result = state
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse inactive edge");
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        let sv = generate_systemverilog_from_mir(&mir).expect("Failed to generate SV");
        println!("Generated SV:\n{}", sv);

        // active_high reset + .inactive → negedge rst
        assert!(
            sv.contains("posedge clk or negedge rst"),
            "Expected 'posedge clk or negedge rst' in SV output, got:\n{}",
            sv
        );
    }
}
