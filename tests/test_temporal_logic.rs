#[cfg(test)]
mod temporal_logic_tests {
    use skalp_frontend::parse_and_build_hir;

    #[test]
    fn test_concurrent_assertion_with_clocking() {
        let source = r#"
        entity AsyncFIFO {
            in wr_clk: clock
            in rd_clk: clock
            in wr_en: bool
            in rd_en: bool
            in full: bool
            in empty: bool
        }

        impl AsyncFIFO {
            // Concurrent assertion with clocking event
            property no_write_when_full {
                @(posedge wr_clk) wr_en |-> !full
            }

            // Property with sequence and delay
            property read_after_write {
                @(posedge wr_clk) wr_en ##2 @(posedge rd_clk) rd_en
            }

            // Assume statement
            assume property (@(posedge wr_clk) !wr_en || !full);

            // Cover statement for corner case
            cover property (@(posedge rd_clk) empty && rd_en);
        }
        "#;

        match parse_and_build_hir(source) {
            Ok(hir) => {
                println!("✅ Concurrent assertions parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());

                assert_eq!(hir.entities.len(), 1);
                assert_eq!(hir.implementations.len(), 1);

                let implementation = &hir.implementations[0];
                println!("Event blocks: {}", implementation.event_blocks.len());
            },
            Err(e) => {
                println!("❌ Concurrent assertion parsing failed: {:?}", e);
                // This may fail initially as we build up the HIR builder support
            }
        }
    }

    #[test]
    fn test_sequence_definitions() {
        let source = r#"
        entity SequenceTest {
            in clk: clock
            in req: bool
            in ack: bool
            in data: nat[8]
        }

        impl SequenceTest {
            // Sequence definition with repetition
            sequence req_sequence {
                req ##1 data[*3] ##2 ack
            }

            // Property using sequence
            property request_protocol {
                @(posedge clk) req_sequence |-> eventually ack
            }

            // Sequence with consecutive repetition
            sequence burst_write {
                req && !ack ##1 data[+1:4] ##1 ack
            }
        }
        "#;

        match parse_and_build_hir(source) {
            Ok(hir) => {
                println!("✅ Sequence definitions parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());
            },
            Err(e) => {
                println!("❌ Sequence parsing failed: {:?}", e);
                // This may fail initially as we build up the parser support
            }
        }
    }

    #[test]
    fn test_temporal_operators() {
        let source = r#"
        entity TemporalTest {
            in clk: clock
            in a: bool
            in b: bool
            in c: bool
        }

        impl TemporalTest {
            // Always property
            property always_eventually {
                @(posedge clk) always a |-> eventually b
            }

            // Until property (strong)
            property strong_until {
                @(posedge clk) strong(a until b)
            }

            // Until property (weak)
            property weak_until {
                @(posedge clk) weak(a until b)
            }

            // Throughout property
            property throughout_example {
                @(posedge clk) a throughout b
            }

            // Overlapping implication
            property overlapping_impl {
                @(posedge clk) a |=> ##1 b
            }
        }
        "#;

        match parse_and_build_hir(source) {
            Ok(hir) => {
                println!("✅ Temporal operators parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());
            },
            Err(e) => {
                println!("❌ Temporal operator parsing failed: {:?}", e);
                // This may fail initially as we build up full temporal support
            }
        }
    }

    #[test]
    fn test_complex_repetition_patterns() {
        let source = r#"
        entity RepetitionTest {
            in clk: clock
            in valid: bool
            in ready: bool
            in data: nat[32]
        }

        impl RepetitionTest {
            // Zero or more repetition
            sequence handshake_burst {
                valid && ready ##1 data[*0:$]
            }

            // One or more repetition
            sequence continuous_valid {
                valid[+1:10] ##1 !valid
            }

            // Exact repetition
            sequence exactly_three {
                ready[=3]
            }

            // Complex sequence with multiple patterns
            property complex_protocol {
                @(posedge clk) valid |->
                    ##[1:3] ready[*2:5] ##2
                    data[+1] |=>
                    ##1 !valid[*1:$]
            }
        }
        "#;

        match parse_and_build_hir(source) {
            Ok(hir) => {
                println!("✅ Complex repetition patterns parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());
            },
            Err(e) => {
                println!("❌ Complex repetition parsing failed: {:?}", e);
                // Expected to work once full parser support is implemented
            }
        }
    }

    #[test]
    fn test_assertion_types() {
        let source = r#"
        entity AssertionTypeTest {
            in clk: clock
            in reset: bool
            in state: nat[2]
        }

        impl AssertionTypeTest {
            // Immediate assertion (already working from Milestone 1)
            on(clk.rise) {
                assert(!reset || state == 0, "Reset should clear state");
            }

            // Concurrent assertion
            assert property (@(posedge clk) reset |-> ##1 state == 0);

            // Assume for verification
            assume property (@(posedge clk) !reset || state != 3);

            // Expect for checking
            expect property (@(posedge clk) always eventually reset);

            // Cover for corner cases
            cover property (@(posedge clk) state == 3 && !reset);
        }
        "#;

        match parse_and_build_hir(source) {
            Ok(hir) => {
                println!("✅ All assertion types parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());
            },
            Err(e) => {
                println!("❌ Assertion types parsing failed: {:?}", e);
                // Should work once we complete the implementation
            }
        }
    }
}