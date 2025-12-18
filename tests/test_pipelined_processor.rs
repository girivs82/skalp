#[cfg(test)]
mod pipelined_processor_tests {
    #[cfg(target_os = "macos")]
    use skalp_frontend::hir::HirStatement;
    #[cfg(target_os = "macos")]
    use skalp_frontend::parse_and_build_hir;
    #[cfg(target_os = "macos")]
    use skalp_mir::{ExpressionKind, Statement};
    #[cfg(target_os = "macos")]
    use skalp_mir::{MirCompiler, OptimizationLevel};
    #[cfg(target_os = "macos")]
    use skalp_sim::{
        simulator::SimulationConfig,
        testbench::{TestVectorBuilder, Testbench},
    };
    #[cfg(target_os = "macos")]
    use skalp_sir::convert_mir_to_sir;

    #[tokio::test]
    #[cfg(target_os = "macos")]
    async fn test_pipelined_processor_gpu() {
        let source = r#"
        entity PipelinedProcessor {
            in clk: clock
            in rst: reset
            in instruction: nat[16]
            in data_in: nat[8]
            out result: nat[8]
            out valid: bool
        }

        impl PipelinedProcessor {
            // Pipeline stages
            signal fetch_instruction: nat[16] = 0
            signal decode_opcode: nat[4] = 0
            signal decode_operand: nat[8] = 0
            signal execute_result: nat[8] = 0
            signal writeback_data: nat[8] = 0
            signal pipeline_valid: nat[4] = 0

            on(clk.rise) {
                if (rst) {
                    fetch_instruction = 0
                    decode_opcode = 0
                    decode_operand = 0
                    execute_result = 0
                    writeback_data = 0
                    pipeline_valid = 0
                } else {
                    // Stage 1: Fetch
                    fetch_instruction = instruction

                    // Stage 2: Decode
                    decode_opcode = fetch_instruction[15:12]
                    decode_operand = fetch_instruction[7:0]

                    // Stage 3: Execute (simplified)
                    execute_result = decode_operand + data_in

                    // Stage 4: Writeback
                    writeback_data = execute_result

                    // Valid pipeline - simplified approach
                    pipeline_valid = pipeline_valid + 1
                }
            }

            result = writeback_data
            valid = pipeline_valid[3]
        }
        "#;

        // Compile to HIR
        let hir = parse_and_build_hir(source).expect("Failed to parse");

        // Debug: Print HIR info
        println!("\n=== HIR Port Info ===");
        for entity in &hir.entities {
            for port in &entity.ports {
                println!("Port '{}': type={:?}", port.name, port.port_type);
            }
        }

        println!("\n=== HIR Signal Info ===");
        for implementation in &hir.implementations {
            for signal in &implementation.signals {
                println!("Signal '{}': type={:?}", signal.name, signal.signal_type);
            }
        }

        // Debug: Check HIR assignments
        println!("\n=== HIR Event Block Assignments ===");
        for implementation in &hir.implementations {
            for event_block in &implementation.event_blocks {
                println!(
                    "Event block has {} statements",
                    event_block.statements.len()
                );
                check_hir_assignments(&event_block.statements, 0);
            }
        }

        // Compile to MIR (disable optimizations to prevent signal removal)
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile to MIR");

        // Debug: Print MIR module info
        println!("\n=== MIR Module Info ===");
        for port in &mir.modules[0].ports {
            println!("Port '{}': type={:?}", port.name, port.port_type);
        }
        for signal in &mir.modules[0].signals {
            println!("Signal '{}': type={:?}", signal.name, signal.signal_type);
        }

        // Debug: Check what happens to pipeline assignments
        println!("\n=== Pipeline Assignment Debug ===");
        for process in &mir.modules[0].processes {
            if matches!(process.kind, skalp_mir::ProcessKind::Sequential) {
                println!(
                    "Sequential process has {} statements",
                    process.body.statements.len()
                );
                check_pipeline_assignments(&process.body.statements, 0);
            }
        }

        // Convert to SIR
        let sir = convert_mir_to_sir(&mir.modules[0]);

        println!("\n=== SIR Analysis ===");
        println!("Module: {}", sir.name);
        println!(
            "State elements: {:?}",
            sir.state_elements.keys().collect::<Vec<_>>()
        );
        println!("Combinational nodes: {}", sir.combinational_nodes.len());
        println!("Sequential nodes: {}", sir.sequential_nodes.len());

        // Extract cones
        let cones = sir.extract_combinational_cones();
        println!("\nCombinational cones: {}", cones.len());
        for (i, cone) in cones.iter().enumerate() {
            println!("  Cone {}: {} nodes", i, cone.nodes.len());
        }

        // Generate and print Metal shader for debugging
        let shader_code = skalp_sir::generate_metal_shader(&sir);
        println!("\n=== Generated Metal Shader ===");
        println!("{}", &shader_code);

        // Create simulation config
        let config = SimulationConfig {
            use_gpu: true,
            max_cycles: 100,
            timeout_ms: 10000,
            capture_waveforms: false,
            parallel_threads: 1,
        };

        // Create testbench
        let mut testbench = Testbench::new(config)
            .await
            .expect("Failed to create testbench");

        // Load the module
        testbench
            .load_module(&sir)
            .await
            .expect("Failed to load module");

        // Test sequence: Reset, then pipeline operations following f1->c1->f2->c2->f3 pattern

        // Reset sequence (cycles 0-3)
        for cycle in 0..4 {
            testbench.add_test_vector(
                TestVectorBuilder::new(cycle * 2)
                    .with_input("rst", vec![1])
                    .with_input("instruction", vec![0, 0])
                    .with_input("data_in", vec![0])
                    .with_expected_output("valid", vec![0])
                    .build(),
            );
        }

        // Following the pipeline stages:
        // Stage 1: Fetch (fetch_instruction = instruction)
        // Stage 2: Decode (decode_opcode = fetch_instruction[15:12], decode_operand = fetch_instruction[7:0])
        // Stage 3: Execute (execute_result = decode_operand + data_in)
        // Stage 4: Writeback (writeback_data = execute_result)

        // Pipeline analysis: 4-stage pipeline with 2-cycle delay from fetch to execute
        // Stage 1: Fetch    (fetch_instruction = instruction)
        // Stage 2: Decode   (decode_operand = fetch_instruction[7:0], decode_opcode = fetch_instruction[15:12])
        // Stage 3: Execute  (execute_result = decode_operand + data_in)
        // Stage 4: Writeback (writeback_data = execute_result)

        // Instructions reach execute stage 2 cycles after being fed
        // Results reach output (writeback_data) 1 cycle after execute

        // Cycle 4: Feed ADD instruction (0x100A = opcode=1, operand=10)
        let add_instr = (1u16 << 12) | 10u16; // 0x100A
        testbench.add_test_vector(
            TestVectorBuilder::new(8) // GPU cycle 4
                .with_input("rst", vec![0])
                .with_input(
                    "instruction",
                    vec![(add_instr & 0xFF) as u8, (add_instr >> 8) as u8],
                )
                .with_input("data_in", vec![0])
                .build(),
        );

        // Cycle 5: Feed SUB instruction (0x2014 = opcode=2, operand=20)
        let sub_instr = (2u16 << 12) | 20u16; // 0x2014
        testbench.add_test_vector(
            TestVectorBuilder::new(10) // GPU cycle 5
                .with_input("rst", vec![0])
                .with_input(
                    "instruction",
                    vec![(sub_instr & 0xFF) as u8, (sub_instr >> 8) as u8],
                )
                .with_input("data_in", vec![0])
                .build(),
        );

        // Cycle 6: Feed XOR instruction + provide data_in=5 for ADD execution
        // ADD (fed at cycle 4) reaches execute stage
        let xor_instr = (4u16 << 12) | 15u16; // 0x400F
        testbench.add_test_vector(
            TestVectorBuilder::new(12) // GPU cycle 6
                .with_input("rst", vec![0])
                .with_input(
                    "instruction",
                    vec![(xor_instr & 0xFF) as u8, (xor_instr >> 8) as u8],
                )
                .with_input("data_in", vec![5]) // ADD executes: decode_operand(10) + data_in(5) = 15
                .build(),
        );

        // Cycle 7: Feed NOP + provide data_in=3 for SUB execution + check ADD result
        // SUB (fed at cycle 5) reaches execute stage, ADD result now in writeback_data
        testbench.add_test_vector(
            TestVectorBuilder::new(14) // GPU cycle 7
                .with_input("rst", vec![0])
                .with_input("instruction", vec![0, 0])
                .with_input("data_in", vec![3]) // SUB executes: decode_operand(20) + data_in(3) = 23
                .with_expected_output("result", vec![15]) // ADD result: 10 + 5 = 15
                .build(),
        );

        // Cycle 8: Feed NOP + provide data_in=7 for XOR execution + check SUB result
        // XOR (fed at cycle 6) reaches execute stage, SUB result now in writeback_data
        testbench.add_test_vector(
            TestVectorBuilder::new(16) // GPU cycle 8
                .with_input("rst", vec![0])
                .with_input("instruction", vec![0, 0])
                .with_input("data_in", vec![7]) // XOR executes: decode_operand(15) + data_in(7) = 22
                .with_expected_output("result", vec![23]) // SUB result: 20 + 3 = 23
                .build(),
        );

        // Cycle 9: Feed NOP + check XOR result
        // XOR result now in writeback_data
        testbench.add_test_vector(
            TestVectorBuilder::new(18) // GPU cycle 9
                .with_input("rst", vec![0])
                .with_input("instruction", vec![0, 0])
                .with_input("data_in", vec![0])
                .with_expected_output("result", vec![22]) // XOR result: 15 + 7 = 22
                .build(),
        );

        // Continue with NOPs
        for cycle in 10..15 {
            testbench.add_test_vector(
                TestVectorBuilder::new(cycle * 2)
                    .with_input("rst", vec![0])
                    .with_input("instruction", vec![0, 0])
                    .with_input("data_in", vec![0])
                    .build(),
            );
        }

        // Check that valid signal becomes 1 when pipeline_valid[3] is set
        // pipeline_valid increments each cycle, so bit 3 is set when counter >= 8
        testbench.add_test_vector(
            TestVectorBuilder::new(24) // GPU cycle 12
                .with_input("rst", vec![0])
                .with_input("instruction", vec![0, 0])
                .with_input("data_in", vec![0])
                .with_expected_output("valid", vec![1])
                .build(),
        );

        // Run the test
        let results = testbench.run_test().await.expect("Failed to run test");

        // Print report
        println!("\n{}", testbench.generate_report());

        // Assert all tests passed
        assert!(testbench.all_tests_passed(), "Some tests failed");
        assert!(!results.is_empty(), "Should have test results");

        println!("\n✅ Pipelined processor GPU simulation passed!");
    }

    #[allow(dead_code)]
    #[cfg(target_os = "macos")]
    fn find_binary_operations(statements: &[Statement], indent: usize) {
        let indent_str = "  ".repeat(indent);
        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    if let ExpressionKind::Binary { op, left, right } = &assign.rhs.kind {
                        println!(
                            "{}⚙️ BINARY: {:?} <= {:?} {:?} {:?}",
                            indent_str, assign.lhs, left, op, right
                        );
                    }
                }
                Statement::If(if_stmt) => {
                    find_binary_operations(&if_stmt.then_block.statements, indent + 1);
                    if let Some(else_block) = &if_stmt.else_block {
                        find_binary_operations(&else_block.statements, indent + 1);
                    }
                }
                Statement::ResolvedConditional(resolved) => {
                    // Check for binary operations in the resolved cases
                    for case in &resolved.resolved.cases {
                        if let ExpressionKind::Binary { op, left, right } = &case.value.kind {
                            println!(
                                "{}⚙️ CONDITIONAL BINARY: {:?} {:?} {:?}",
                                indent_str, left, op, right
                            );
                        }
                    }
                }
                Statement::Block(block) => {
                    find_binary_operations(&block.statements, indent + 1);
                }
                _ => {}
            }
        }
    }

    #[allow(dead_code)]
    #[cfg(target_os = "macos")]
    fn check_pipeline_assignments(statements: &[Statement], indent: usize) {
        let indent_str = "  ".repeat(indent);
        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let assignment_str = format!("{:?} <= {:?}", assign.lhs, assign.rhs);
                    // Print ALL assignments for now to see what's there
                    println!("{}📋 ALL: {}", indent_str, assignment_str);
                }
                Statement::If(if_stmt) => {
                    check_pipeline_assignments(&if_stmt.then_block.statements, indent + 1);
                    if let Some(else_block) = &if_stmt.else_block {
                        check_pipeline_assignments(&else_block.statements, indent + 1);
                    }
                }
                Statement::ResolvedConditional(resolved) => {
                    let target_str = format!("{:?}", resolved.target);
                    if target_str.contains("decode_operand")
                        || target_str.contains("decode_opcode")
                        || target_str.contains("fetch_instruction")
                        || target_str.contains("writeback_data")
                        || target_str.contains("pipeline_valid")
                    {
                        println!(
                            "{}🔄 RESOLVED PIPELINE: {} <= {} cases",
                            indent_str,
                            target_str,
                            resolved.resolved.cases.len()
                        );
                        // Show what the resolved cases contain
                        for (i, case) in resolved.resolved.cases.iter().enumerate() {
                            println!(
                                "{}   Case {}: when {:?} then {:?}",
                                indent_str, i, case.condition, case.value
                            );
                        }
                        println!("{}   Default: {:?}", indent_str, resolved.resolved.default);
                    }
                }
                Statement::Block(block) => {
                    check_pipeline_assignments(&block.statements, indent + 1);
                }
                _ => {}
            }
        }
    }

    #[allow(dead_code)]
    #[cfg(target_os = "macos")]
    fn check_input_assignments(statements: &[Statement], indent: usize) {
        let indent_str = "  ".repeat(indent);
        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let assignment_str = format!("{:?} <= {:?}", assign.lhs, assign.rhs);
                    // Look for assignments that read from input ports
                    if assignment_str.contains("Port(PortId(0))") || // instruction
                       assignment_str.contains("Port(PortId(3))") || // data_in
                       assignment_str.contains("instruction") ||
                       assignment_str.contains("data_in")
                    {
                        println!("{}🔌 INPUT: {}", indent_str, assignment_str);
                    }
                    // Look for range select operations
                    if assignment_str.contains("RangeSelect") {
                        println!("{}✂️ SLICE: {}", indent_str, assignment_str);
                    }
                }
                Statement::If(if_stmt) => {
                    check_input_assignments(&if_stmt.then_block.statements, indent + 1);
                    if let Some(else_block) = &if_stmt.else_block {
                        check_input_assignments(&else_block.statements, indent + 1);
                    }
                }
                Statement::ResolvedConditional(resolved) => {
                    let target_str = format!("{:?}", resolved.target);
                    if target_str.contains("execute_result") {
                        println!(
                            "{}🔄 EXECUTE: {} <= PriorityMux with {} cases",
                            indent_str,
                            target_str,
                            resolved.resolved.cases.len()
                        );
                    }
                }
                Statement::Block(block) => {
                    check_input_assignments(&block.statements, indent + 1);
                }
                _ => {}
            }
        }
    }

    #[allow(dead_code)]
    #[cfg(target_os = "macos")]
    fn check_hir_assignments(statements: &[HirStatement], indent: usize) {
        let indent_str = "  ".repeat(indent);
        for stmt in statements {
            match stmt {
                HirStatement::Assignment(assign) => {
                    let assignment_str = format!("{:?} <= {:?}", assign.lhs, assign.rhs);
                    println!("{}📋 HIR: {}", indent_str, assignment_str);
                    // Check for specific pipeline assignments
                    if assignment_str.contains("decode_opcode")
                        || assignment_str.contains("decode_operand")
                        || assignment_str.contains("writeback_data")
                        || assignment_str.contains("pipeline_valid")
                    {
                        println!("{}🎯 PIPELINE HIR: {}", indent_str, assignment_str);
                    }
                }
                HirStatement::If(if_stmt) => {
                    check_hir_assignments(&if_stmt.then_statements, indent + 1);
                    if let Some(else_statements) = &if_stmt.else_statements {
                        check_hir_assignments(else_statements, indent + 1);
                    }
                }
                _ => {}
            }
        }
    }
}
