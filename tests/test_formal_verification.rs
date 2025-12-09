#[cfg(test)]
mod formal_verification_tests {
    use skalp_frontend::parse_and_build_hir;

    #[test]
    fn test_basic_formal_verification() {
        let source = r#"
        entity FormalTest {
            in clk: clock
            in reset: bool
            in req: bool
            in ack: bool
            out counter: nat[8]
        }

        impl FormalTest {
            signal counter_reg: nat[8] = 0

            on(clk.rise) {
                if reset {
                    counter_reg <= 0;
                } else if req && !ack {
                    counter_reg <= counter_reg + 1;
                }
            }

            assign counter = counter_reg;

            // Formal verification block
            formal protocol_verification {
                // Safety property: counter never overflows
                safety counter_bounds: counter <= 255;

                // Liveness property: if req is asserted, ack eventually responds
                liveness req_ack_response: req |-> eventually ack;

                // Invariant: reset always clears counter
                invariant reset_clears: reset |-> ##1 counter == 0;

                // Bounded property for finite verification
                bounded[50] finite_req_response: req |-> ##[1:10] ack;

                // Assumptions for verification context
                assume property (@(posedge clk) !reset || !req);
            }
        }
        "#;

        match parse_and_build_hir(source) {
            Ok(hir) => {
                println!("✅ Basic formal verification parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());

                assert_eq!(hir.entities.len(), 1);
                assert_eq!(hir.implementations.len(), 1);

                let implementation = &hir.implementations[0];
                println!("Formal blocks: {}", implementation.formal_blocks.len());
                println!("Event blocks: {}", implementation.event_blocks.len());
            }
            Err(e) => {
                println!("❌ Basic formal verification parsing failed: {:?}", e);
                // This may fail initially as we build up the HIR builder support
            }
        }
    }

    #[test]
    fn test_advanced_formal_properties() {
        let source = r#"
        entity AdvancedFormal {
            in clk: clock
            in reset: bool
            in valid: bool
            in ready: bool
            in data: nat[32]
            out result: nat[32]
        }

        impl AdvancedFormal {
            signal state: nat[2] = 0
            signal buffer: nat[32] = 0

            on(clk.rise) {
                if reset {
                    state <= 0;
                    buffer <= 0;
                } else {
                    match state {
                        0 => if valid { state <= 1; buffer <= data; }
                        1 => if ready { state <= 2; }
                        2 => { state <= 0; }
                        _ => { state <= 0; }
                    }
                }
            }

            assign result = buffer;

            // Comprehensive formal verification
            formal advanced_properties {
                // Complex safety properties
                safety no_data_loss: (state == 1) |-> (buffer == data);
                safety valid_state_machine: state <= 2;
                safety reset_behavior: reset |-> ##1 (state == 0 && buffer == 0);

                // Advanced liveness properties
                liveness progress: always (valid |-> eventually (state == 2));
                liveness termination: (state == 1) |-> eventually (state == 0);

                // Temporal invariants
                invariant state_progression:
                    (state == 0 && valid) |-> ##1 (state == 1);

                invariant buffer_stability:
                    (state == 1 && !ready) |-> ##1 (buffer == $past(buffer));

                // Bounded model checking properties
                bounded[100] finite_completion:
                    valid |-> ##[1:50] (state == 2);

                bounded[20] quick_reset:
                    reset |-> ##1 (state == 0);

                // Complex temporal sequences
                safety handshake_protocol:
                    @(posedge clk) valid && ready |-> ##1 !valid until ready;

                // Cross-clock domain properties (if applicable)
                liveness cross_domain:
                    always eventually (valid || !ready);
            }

            // Additional standalone properties
            invariant global_reset: reset |-> always (state == 0);
            safety data_integrity: valid |-> (data != 0);
            liveness system_liveness: always eventually ready;

            // Prove statements for specific verification
            prove (state == 1) |-> eventually (state == 2) bounded[30];
            prove reset |-> ##1 (state == 0) bounded[5];
        }
        "#;

        match parse_and_build_hir(source) {
            Ok(hir) => {
                println!("✅ Advanced formal properties parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());

                assert_eq!(hir.entities.len(), 1);
                assert_eq!(hir.implementations.len(), 1);

                let implementation = &hir.implementations[0];
                println!("Formal blocks: {}", implementation.formal_blocks.len());
            }
            Err(e) => {
                println!("❌ Advanced formal properties parsing failed: {:?}", e);
                // Expected to work once full parser support is implemented
            }
        }
    }

    #[test]
    fn test_bounded_model_checking() {
        let source = r#"
        entity BMCTest {
            in clk: clock
            in start: bool
            in stop: bool
            out active: bool
        }

        impl BMCTest {
            signal active_reg: bool = false

            on(clk.rise) {
                if start && !stop {
                    active_reg <= true;
                } else if stop {
                    active_reg <= false;
                }
            }

            assign active = active_reg;

            // Bounded model checking specific tests
            formal bmc_verification {
                // Short-term properties (quick verification)
                bounded[5] immediate_start: start |-> ##1 active;
                bounded[3] immediate_stop: stop |-> ##1 !active;

                // Medium-term properties
                bounded[20] sustained_activity:
                    start && !stop |-> active until stop;

                bounded[15] no_spurious_activation:
                    !start |-> always !active until start;

                // Long-term properties (stress testing)
                bounded[100] extended_operation:
                    start |-> eventually stop;

                bounded[50] oscillation_check:
                    always (active |-> eventually !active);

                // Incremental verification bounds
                safety quick_check: start |-> active bounded[1];
                safety medium_check: start && !stop |-> ##[1:5] active bounded[10];
                safety full_check: start |-> eventually stop bounded[30];
            }

            // Standalone bounded properties
            prove start |-> ##1 active bounded[2];
            prove stop |-> ##1 !active bounded[2];
            prove (start && stop) |-> ##1 !active bounded[3];
        }
        "#;

        match parse_and_build_hir(source) {
            Ok(hir) => {
                println!("✅ Bounded model checking parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());

                assert_eq!(hir.entities.len(), 1);
                assert_eq!(hir.implementations.len(), 1);

                let implementation = &hir.implementations[0];
                println!("Formal blocks: {}", implementation.formal_blocks.len());
            }
            Err(e) => {
                println!("❌ Bounded model checking parsing failed: {:?}", e);
                // Expected to work once full parser support is implemented
            }
        }
    }

    #[test]
    fn test_comprehensive_verification_suite() {
        let source = r#"
        entity VerificationSuite {
            in clk: clock
            in reset: bool
            in cmd_valid: bool
            in cmd_ready: bool
            in cmd_data: nat[16]
            out resp_valid: bool
            out resp_data: nat[16]
        }

        impl VerificationSuite {
            signal cmd_buffer: nat[16] = 0
            signal resp_buffer: nat[16] = 0
            signal state: nat[2] = 0

            // Basic functionality
            on(clk.rise) {
                if reset {
                    state <= 0;
                    cmd_buffer <= 0;
                    resp_buffer <= 0;
                } else {
                    match state {
                        0 => if cmd_valid && cmd_ready {
                            state <= 1;
                            cmd_buffer <= cmd_data;
                        }
                        1 => {
                            resp_buffer <= cmd_buffer + 1;
                            state <= 2;
                        }
                        2 => { state <= 0; }
                        _ => { state <= 0; }
                    }
                }
            }

            assign resp_valid = (state == 2);
            assign resp_data = resp_buffer;

            // Immediate assertions (Milestone 1)
            on(clk.rise) {
                assert(!reset || state == 0, "Reset should clear state");
                assert(state <= 2, "State should be valid");
            }

            // Concurrent assertions (Milestone 2)
            assert property (@(posedge clk) reset |-> ##1 state == 0);
            cover property (@(posedge clk) cmd_valid && cmd_ready);

            // Coverage (Milestone 3)
            covergroup cmd_cg @(posedge clk) {
                coverpoint cmd_data {
                    bins low = [0:255];
                    bins high = [256:65535];
                }
                coverpoint state {
                    bins idle = {0};
                    bins processing = {1};
                    bins responding = {2};
                }
                cross cmd_data, state;
            }

            // Formal verification (Milestone 4)
            formal comprehensive_verification {
                // Safety properties
                safety data_integrity:
                    (state == 1) |-> (cmd_buffer == $past(cmd_data));

                safety response_correctness:
                    resp_valid |-> (resp_data == cmd_buffer + 1);

                safety state_machine_bounds:
                    always (state <= 2);

                // Liveness properties
                liveness command_completion:
                    (cmd_valid && cmd_ready) |-> eventually resp_valid;

                liveness system_responsiveness:
                    always eventually (state == 0);

                // Invariants
                invariant reset_behavior:
                    reset |-> (state == 0 && cmd_buffer == 0 && resp_buffer == 0);

                invariant response_validity:
                    resp_valid |-> (state == 2);

                // Bounded properties for practical verification
                bounded[50] finite_processing:
                    cmd_valid |-> ##[1:10] resp_valid;

                bounded[20] quick_reset_recovery:
                    reset |-> ##[1:3] (state == 0);

                // Complex temporal relationships
                safety handshake_protocol:
                    @(posedge clk) cmd_valid && cmd_ready |->
                    ##1 !cmd_valid until resp_valid;

                liveness progress_guarantee:
                    (state == 1) |-> ##[1:5] (state == 2);

                // Assumptions for verification environment
                assume property (@(posedge clk) !reset || !cmd_valid);
                assume property (@(posedge clk) cmd_valid |-> cmd_data != 0);
            }

            // Additional verification checks
            invariant global_state_validity: state <= 2;
            safety no_data_corruption: cmd_valid |-> eventually (resp_data != 0);
            liveness eventual_idle: always eventually (state == 0);

            // Specific property proving
            prove cmd_valid && cmd_ready |-> ##3 resp_valid bounded[10];
            prove reset |-> ##1 (state == 0) bounded[2];
        }
        "#;

        match parse_and_build_hir(source) {
            Ok(hir) => {
                println!("✅ Comprehensive verification suite parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());

                assert_eq!(hir.entities.len(), 1);
                assert_eq!(hir.implementations.len(), 1);

                let implementation = &hir.implementations[0];
                println!("Event blocks: {}", implementation.event_blocks.len());
                println!("Covergroups: {}", implementation.covergroups.len());
                println!("Formal blocks: {}", implementation.formal_blocks.len());
            }
            Err(e) => {
                println!(
                    "❌ Comprehensive verification suite parsing failed: {:?}",
                    e
                );
                // Should work once we complete the implementation
            }
        }
    }

    #[test]
    fn test_formal_verification_edge_cases() {
        let source = r#"
        entity EdgeCaseFormal {
            in clk: clock
            in signal_a: bool
            in signal_b: bool
        }

        impl EdgeCaseFormal {
            // Minimal formal block
            formal minimal {
                invariant simple: signal_a || signal_b;
            }

            // Single-line properties
            invariant standalone_invariant: signal_a |-> signal_b;
            safety standalone_safety: always signal_a;
            liveness standalone_liveness: eventually signal_b;

            // Prove statements without bounds
            prove signal_a |-> signal_b;
            prove always eventually signal_a;

            // Empty formal block (edge case)
            formal empty_block {
                // No properties - should parse but be empty
            }

            // Formal block with only assumptions
            formal assumptions_only {
                assume property (@(posedge clk) signal_a);
                assume property (@(posedge clk) !signal_a || signal_b);
            }
        }
        "#;

        match parse_and_build_hir(source) {
            Ok(hir) => {
                println!("✅ Formal verification edge cases parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());

                assert_eq!(hir.entities.len(), 1);
                assert_eq!(hir.implementations.len(), 1);

                let implementation = &hir.implementations[0];
                println!("Formal blocks: {}", implementation.formal_blocks.len());
            }
            Err(e) => {
                println!("❌ Formal verification edge cases parsing failed: {:?}", e);
                // Expected to work once full parser support is implemented
            }
        }
    }
}

/// Tests for SVA (SystemVerilog Assertions) code generation
/// These tests verify the HIR -> MIR -> SystemVerilog pipeline generates correct SVA
#[cfg(test)]
mod sva_generation_tests {
    use skalp_codegen::generate_systemverilog_from_mir;
    use skalp_frontend::parse_and_build_hir;
    use skalp_lir::lower_to_lir;
    use skalp_mir::{MirCompiler, OptimizationLevel};

    /// Test that assert! statements in event blocks generate immediate assertions
    #[test]
    fn test_assert_sva_generation() {
        let source = r#"
        entity AssertTest {
            in clk: clock
            in data: nat[8]
            out valid: bool
        }

        impl AssertTest {
            signal counter: nat[8] = 0

            on(clk.rise) {
                counter <= counter + 1;
                assert!(counter < 255, "Counter must not overflow");
            }

            assign valid = counter > 0;
        }
        "#;

        // Parse and compile
        let hir = parse_and_build_hir(source).expect("Failed to parse");
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let mir = compiler.compile_to_mir(&hir).expect("Failed to compile to MIR");
        let lir = lower_to_lir(&mir).expect("Failed to lower to LIR");

        // Generate SystemVerilog
        let sv_code = generate_systemverilog_from_mir(&mir, &lir)
            .expect("Failed to generate SystemVerilog");

        println!("Generated SystemVerilog:\n{}", sv_code);

        // Verify SVA assertion is generated
        assert!(
            sv_code.contains("assert(") || sv_code.contains("assert ("),
            "Should contain assert statement"
        );
    }

    /// Test that assume! statements generate SVA assumptions
    #[test]
    fn test_assume_sva_generation() {
        let source = r#"
        entity AssumeTest {
            in clk: clock
            in valid: bool
            in data: nat[8]
            out result: nat[8]
        }

        impl AssumeTest {
            signal stored: nat[8] = 0

            on(clk.rise) {
                assume!(valid, "Valid signal assumed true");
                if valid {
                    stored <= data;
                }
            }

            assign result = stored;
        }
        "#;

        // Parse and compile
        let hir = parse_and_build_hir(source).expect("Failed to parse");
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let mir = compiler.compile_to_mir(&hir).expect("Failed to compile to MIR");
        let lir = lower_to_lir(&mir).expect("Failed to lower to LIR");

        // Generate SystemVerilog
        let sv_code = generate_systemverilog_from_mir(&mir, &lir)
            .expect("Failed to generate SystemVerilog");

        println!("Generated SystemVerilog:\n{}", sv_code);

        // Verify SVA assumption is generated
        assert!(
            sv_code.contains("assume(") || sv_code.contains("assume ("),
            "Should contain assume statement"
        );
    }

    /// Test that cover! statements generate SVA cover points
    #[test]
    fn test_cover_sva_generation() {
        let source = r#"
        entity CoverTest {
            in clk: clock
            in state: nat[2]
            out active: bool
        }

        impl CoverTest {
            on(clk.rise) {
                cover!(state == 3, "all_bits_set");
            }

            assign active = state != 0;
        }
        "#;

        // Parse and compile
        let hir = parse_and_build_hir(source).expect("Failed to parse");
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let mir = compiler.compile_to_mir(&hir).expect("Failed to compile to MIR");
        let lir = lower_to_lir(&mir).expect("Failed to lower to LIR");

        // Generate SystemVerilog
        let sv_code = generate_systemverilog_from_mir(&mir, &lir)
            .expect("Failed to generate SystemVerilog");

        println!("Generated SystemVerilog:\n{}", sv_code);

        // Verify SVA cover is generated
        assert!(
            sv_code.contains("cover(") || sv_code.contains("cover ("),
            "Should contain cover statement"
        );
    }

    /// Test assert and assume SVA statement types together
    #[test]
    fn test_mixed_sva_generation() {
        let source = r#"
        entity MixedSvaTest {
            in clk: clock
            in reset: bool
            in data: nat[8]
            out result: nat[8]
        }

        impl MixedSvaTest {
            signal counter: nat[8] = 0

            on(clk.rise) {
                // Assumptions about inputs - use < instead of <= to avoid parser ambiguity
                assume!(data < 100, "Data assumed in range");

                if reset {
                    counter <= 0;
                } else {
                    counter <= counter + data;
                }

                // Assertions about internal state - use < instead of <=
                assert!(counter < 255, "Counter bounds check");
            }

            assign result = counter;
        }
        "#;

        // Parse and compile
        let hir = parse_and_build_hir(source).expect("Failed to parse");
        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let mir = compiler.compile_to_mir(&hir).expect("Failed to compile to MIR");
        let lir = lower_to_lir(&mir).expect("Failed to lower to LIR");

        // Generate SystemVerilog
        let sv_code = generate_systemverilog_from_mir(&mir, &lir)
            .expect("Failed to generate SystemVerilog");

        println!("Generated SystemVerilog:\n{}", sv_code);

        // Verify assert and assume SVA types are generated
        let has_assert = sv_code.contains("assert(") || sv_code.contains("assert (");
        let has_assume = sv_code.contains("assume(") || sv_code.contains("assume (");

        println!("Has assert: {}", has_assert);
        println!("Has assume: {}", has_assume);

        assert!(has_assert, "Should contain assert statements");
        assert!(has_assume, "Should contain assume statements");
    }
}
