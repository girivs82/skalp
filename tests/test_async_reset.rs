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

/// Full pipeline tests: parse → MIR → LIR → gate synthesis → EC
#[cfg(test)]
mod async_reset_pipeline_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_lir::{
        get_stdlib_library, lower_mir_module_to_lir, map_lir_to_gates, CellFunction, LirOp,
    };
    use skalp_mir::{MirCompiler, OptimizationLevel};

    /// Helper: compile source to MIR
    fn compile_to_mir(source: &str) -> skalp_mir::Mir {
        let hir = parse_and_build_hir(source).expect("Failed to parse");
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR")
    }

    #[test]
    fn test_async_reset_lir_has_async_flag() {
        let source = r#"
        entity AsyncRegLir {
            in clk: clock
            in rst: reset(active_high)
            in d: nat[8]
            out q: nat[8]
        }

        impl AsyncRegLir {
            signal state: nat[8]
            on(clk.rise, rst.active) {
                if rst {
                    state = 0
                } else {
                    state = d
                }
            }
            q = state
        }
        "#;

        let mir = compile_to_mir(source);
        let lir_result = lower_mir_module_to_lir(&mir.modules[0]);
        let lir = &lir_result.lir;

        println!("LIR signals: {}", lir.signals.len());
        println!("LIR nodes: {}", lir.nodes.len());

        // Find Reg nodes and verify they have async_reset = true
        let mut found_async_reg = false;
        for node in &lir.nodes {
            if let LirOp::Reg {
                async_reset,
                has_reset,
                ..
            } = &node.op
            {
                println!(
                    "Reg node: async_reset={}, has_reset={}, path={}",
                    async_reset, has_reset, node.path
                );
                assert!(*async_reset, "Reg should have async_reset=true");
                assert!(*has_reset, "Reg should have has_reset=true");
                found_async_reg = true;
            }
        }
        assert!(found_async_reg, "Should have found at least one Reg node");
    }

    #[test]
    fn test_sync_reset_lir_has_no_async_flag() {
        let source = r#"
        entity SyncRegLir {
            in clk: clock
            in rst: reset(active_high)
            in d: nat[8]
            out q: nat[8]
        }

        impl SyncRegLir {
            signal state: nat[8]
            on(clk.rise) {
                if rst {
                    state = 0
                } else {
                    state = d
                }
            }
            q = state
        }
        "#;

        let mir = compile_to_mir(source);
        let lir_result = lower_mir_module_to_lir(&mir.modules[0]);
        let lir = &lir_result.lir;

        // Find Reg nodes and verify they have async_reset = false
        for node in &lir.nodes {
            if let LirOp::Reg { async_reset, .. } = &node.op {
                assert!(
                    !*async_reset,
                    "Sync reset Reg should have async_reset=false, path={}",
                    node.path
                );
            }
        }
    }

    #[test]
    fn test_async_reset_synthesis_uses_dffr() {
        let source = r#"
        entity AsyncSynth {
            in clk: clock
            in rst: reset(active_high)
            in d: nat[8]
            out q: nat[8]
        }

        impl AsyncSynth {
            signal state: nat[8]
            on(clk.rise, rst.active) {
                if rst {
                    state = 0
                } else {
                    state = d
                }
            }
            q = state
        }
        "#;

        let mir = compile_to_mir(source);
        let lir_result = lower_mir_module_to_lir(&mir.modules[0]);
        let library = get_stdlib_library("generic_asic").expect("Failed to load library");
        let tech_result = map_lir_to_gates(&lir_result.lir, &library);
        let netlist = &tech_result.netlist;

        println!(
            "Gate netlist: {} cells, {} nets",
            netlist.cells.len(),
            netlist.nets.len()
        );

        // Async reset should use DffR cells (with async reset pin), NOT plain Dff
        let mut dffr_count = 0;
        let mut dff_count = 0;
        let mut reset_mux_count = 0;
        for cell in &netlist.cells {
            match &cell.function {
                Some(CellFunction::DffR) => dffr_count += 1,
                Some(CellFunction::Dff) => dff_count += 1,
                _ => {}
            }
            if cell.source_op.as_deref() == Some("ResetMux") {
                reset_mux_count += 1;
            }
        }

        println!("DffR cells: {}", dffr_count);
        println!("Dff cells: {}", dff_count);
        println!("ResetMux cells: {}", reset_mux_count);

        assert!(
            dffr_count > 0,
            "Async reset should produce DffR cells, got {} DffR and {} Dff",
            dffr_count,
            dff_count
        );
        assert_eq!(
            reset_mux_count, 0,
            "Async reset should NOT have ResetMux cells (no MUX on D-input)"
        );
    }

    #[test]
    fn test_sync_reset_synthesis_uses_dff_with_mux() {
        let source = r#"
        entity SyncSynth {
            in clk: clock
            in rst: reset(active_high)
            in d: nat[8]
            out q: nat[8]
        }

        impl SyncSynth {
            signal state: nat[8]
            on(clk.rise) {
                if rst {
                    state = 0
                } else {
                    state = d
                }
            }
            q = state
        }
        "#;

        let mir = compile_to_mir(source);
        let lir_result = lower_mir_module_to_lir(&mir.modules[0]);
        let library = get_stdlib_library("generic_asic").expect("Failed to load library");
        let tech_result = map_lir_to_gates(&lir_result.lir, &library);
        let netlist = &tech_result.netlist;

        // Sync reset should use plain Dff cells with ResetMux
        let mut dffr_count = 0;
        let mut dff_count = 0;
        let mut reset_mux_count = 0;
        for cell in &netlist.cells {
            match &cell.function {
                Some(CellFunction::DffR) => dffr_count += 1,
                Some(CellFunction::Dff) => dff_count += 1,
                _ => {}
            }
            if cell.source_op.as_deref() == Some("ResetMux") {
                reset_mux_count += 1;
            }
        }

        println!("DffR cells: {}", dffr_count);
        println!("Dff cells: {}", dff_count);
        println!("ResetMux cells: {}", reset_mux_count);

        assert_eq!(dffr_count, 0, "Sync reset should NOT produce DffR cells");
        assert!(dff_count > 0, "Sync reset should produce plain Dff cells");
        assert!(reset_mux_count > 0, "Sync reset should have ResetMux cells");
    }

    #[test]
    fn test_async_reset_equivalence_check() {
        use skalp_formal::{check_sequential_equivalence_sat, GateNetlistToAig, LirToAig};

        let source = r#"
        entity AsyncEc {
            in clk: clock
            in rst: reset(active_high)
            in d: nat[8]
            out q: nat[8]
        }

        impl AsyncEc {
            signal state: nat[8]
            on(clk.rise, rst.active) {
                if rst {
                    state = 0
                } else {
                    state = d
                }
            }
            q = state
        }
        "#;

        let mir = compile_to_mir(source);
        let lir_result = lower_mir_module_to_lir(&mir.modules[0]);
        let library = get_stdlib_library("generic_asic").expect("Failed to load library");
        let tech_result = map_lir_to_gates(&lir_result.lir, &library);

        println!(
            "LIR: {} signals, {} nodes",
            lir_result.lir.signals.len(),
            lir_result.lir.nodes.len()
        );
        println!(
            "Gate: {} cells, {} nets",
            tech_result.netlist.cells.len(),
            tech_result.netlist.nets.len()
        );

        // Build AIGs for both representations
        let lir_aig = LirToAig::new().convert_sequential(&lir_result.lir);
        let gate_aig = GateNetlistToAig::new().convert_sequential(&tech_result.netlist);

        println!(
            "LIR AIG: {} nodes, {} inputs, {} outputs, {} latches",
            lir_aig.nodes.len(),
            lir_aig.inputs.len(),
            lir_aig.outputs.len(),
            lir_aig.latches.len()
        );
        println!(
            "Gate AIG: {} nodes, {} inputs, {} outputs, {} latches",
            gate_aig.nodes.len(),
            gate_aig.inputs.len(),
            gate_aig.outputs.len(),
            gate_aig.latches.len()
        );

        // Run sequential equivalence check
        let result = check_sequential_equivalence_sat(&lir_aig, &gate_aig, false)
            .expect("EC should not error");

        println!("EC result: equivalent={}", result.equivalent);
        if !result.equivalent {
            if let Some(ce) = &result.counterexample {
                println!("Counterexample: differing_signal={:?}", ce.differing_signal);
                println!("  inputs: {:?}", ce.inputs);
                println!("  state: {:?}", ce.state);
            }
        }
        println!("Proven gates: {}", result.proven_gates.len());
        println!("Unresolved gates: {}", result.unresolved_gates.len());

        assert!(
            result.equivalent,
            "Async reset: LIR and gate netlist should be equivalent"
        );
    }

    #[test]
    fn test_async_reset_nonzero_value_synthesis() {
        // Test that async reset with non-zero reset value uses the DffR inversion trick
        let source = r#"
        entity AsyncNonZero {
            in clk: clock
            in rst: reset(active_high)
            in d: nat[4]
            out q: nat[4]
        }

        impl AsyncNonZero {
            signal state: nat[4]
            on(clk.rise, rst.active) {
                if rst {
                    state = 5
                } else {
                    state = d
                }
            }
            q = state
        }
        "#;

        let mir = compile_to_mir(source);
        let lir_result = lower_mir_module_to_lir(&mir.modules[0]);
        let library = get_stdlib_library("generic_asic").expect("Failed to load library");
        let tech_result = map_lir_to_gates(&lir_result.lir, &library);
        let netlist = &tech_result.netlist;

        // Should have DffR cells and inverters for the reset-to-1 bits (5 = 0b0101)
        let mut dffr_count = 0;
        let mut async_inv_count = 0;
        for cell in &netlist.cells {
            if matches!(&cell.function, Some(CellFunction::DffR)) {
                dffr_count += 1;
            }
            if cell.source_op.as_deref() == Some("AsyncResetInvD")
                || cell.source_op.as_deref() == Some("AsyncResetInvQ")
            {
                async_inv_count += 1;
            }
        }

        println!(
            "DffR cells: {}, AsyncReset inverters: {}",
            dffr_count, async_inv_count
        );

        assert_eq!(dffr_count, 4, "Should have 4 DffR cells (4-bit register)");
        // 5 = 0b0101: bits 0 and 2 are 1, need INV pairs (2 INV each)
        assert_eq!(
            async_inv_count, 4,
            "Should have 4 inverters (2 bits reset-to-1 × 2 INV each)"
        );
    }

    #[test]
    fn test_async_reset_nonzero_equivalence_check() {
        use skalp_formal::{check_sequential_equivalence_sat, GateNetlistToAig, LirToAig};

        let source = r#"
        entity AsyncNonZeroEc {
            in clk: clock
            in rst: reset(active_high)
            in d: nat[4]
            out q: nat[4]
        }

        impl AsyncNonZeroEc {
            signal state: nat[4]
            on(clk.rise, rst.active) {
                if rst {
                    state = 5
                } else {
                    state = d
                }
            }
            q = state
        }
        "#;

        let mir = compile_to_mir(source);
        let lir_result = lower_mir_module_to_lir(&mir.modules[0]);
        let library = get_stdlib_library("generic_asic").expect("Failed to load library");
        let tech_result = map_lir_to_gates(&lir_result.lir, &library);

        let lir_aig = LirToAig::new().convert_sequential(&lir_result.lir);
        let gate_aig = GateNetlistToAig::new().convert_sequential(&tech_result.netlist);

        let result = check_sequential_equivalence_sat(&lir_aig, &gate_aig, false)
            .expect("EC should not error");

        assert!(
            result.equivalent,
            "Async reset with non-zero value: LIR and gate netlist should be equivalent"
        );
    }
}
