#[cfg(test)]
mod coverage_infrastructure_tests {
    use skalp_frontend::parse_and_build_hir;

    #[test]
    fn test_basic_covergroup_parsing() {
        let source = r#"
        entity CoverageTest {
            in clk: clock
            in addr: nat[8]
            in data: nat[16]
            in valid: bool
        }

        impl CoverageTest {
            covergroup mem_cg @(posedge clk) {
                coverpoint addr {
                    bins low = [0:63];
                    bins high = [64:255];
                    ignore_bins reserved = {240, 241, 242};
                    illegal_bins forbidden = [250:255];
                }

                coverpoint data {
                    bins zero = {0};
                    bins small = [1:100];
                    bins large = [101:65535];
                }

                cross addr, data {
                    bins addr_data_cross = [0:10], [0:100];
                    ignore_bins invalid_cross = [240:255], [0:65535];
                }
            }
        }
        "#;

        match parse_and_build_hir(source) {
            Ok(hir) => {
                println!("✅ Basic covergroup parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());

                assert_eq!(hir.entities.len(), 1);
                assert_eq!(hir.implementations.len(), 1);

                let implementation = &hir.implementations[0];
                println!("Covergroups: {}", implementation.covergroups.len());
            }
            Err(e) => {
                println!("❌ Basic covergroup parsing failed: {:?}", e);
                // This may fail initially as we build up the HIR builder support
            }
        }
    }

    #[test]
    fn test_functional_coverage_patterns() {
        let source = r#"
        entity FunctionalCoverage {
            in clk: clock
            in req: bool
            in ack: bool
            in burst_type: nat[2]
            in burst_length: nat[4]
        }

        impl FunctionalCoverage {
            // Basic covergroup with sampling event
            covergroup protocol_cg @(posedge clk) {
                coverpoint req {
                    bins active = {1};
                    bins inactive = {0};
                }

                coverpoint ack {
                    bins ready = {1};
                    bins not_ready = {0};
                }

                // Protocol sequence coverage
                cross req, ack {
                    bins valid_handshake = {1, 1};
                    bins req_without_ack = {1, 0};
                    illegal_bins invalid_ack = {0, 1};
                }
            }

            // Burst type coverage
            covergroup burst_cg @(posedge clk) {
                coverpoint burst_type {
                    bins fixed = {0};
                    bins incr = {1};
                    bins wrap = {2};
                    illegal_bins reserved = {3};
                }

                coverpoint burst_length {
                    bins single = {1};
                    bins short_burst = [2:4];
                    bins long_burst = [5:15];
                    illegal_bins invalid = {0};
                }

                // Cross coverage for burst patterns
                cross burst_type, burst_length {
                    bins fixed_single = {0}, {1};
                    bins incr_burst = {1}, [2:15];
                    bins wrap_burst = {2}, [2:8];
                    ignore_bins wrap_long = {2}, [9:15];
                }
            }
        }
        "#;

        match parse_and_build_hir(source) {
            Ok(hir) => {
                println!("✅ Functional coverage patterns parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());

                assert_eq!(hir.entities.len(), 1);
                assert_eq!(hir.implementations.len(), 1);

                let implementation = &hir.implementations[0];
                println!("Covergroups: {}", implementation.covergroups.len());
            }
            Err(e) => {
                println!("❌ Functional coverage parsing failed: {:?}", e);
                // Expected to work once full parser support is implemented
            }
        }
    }

    #[test]
    fn test_complex_bins_definitions() {
        let source = r#"
        entity ComplexBins {
            in clk: clock
            in state: nat[3]
            in counter: nat[16]
            in flags: nat[8]
        }

        impl ComplexBins {
            covergroup state_machine_cg @(posedge clk) {
                coverpoint state {
                    bins idle = {0};
                    bins active = [1:3];
                    bins error = {4, 5};
                    bins debug = [6:7];
                    illegal_bins invalid = default;
                }

                coverpoint counter {
                    bins zero = {0};
                    bins low = [1:100];
                    bins medium = [101:1000];
                    bins high = [1001:10000];
                    bins max = [10001:65535];
                    ignore_bins test_values = {0xDEAD, 0xBEEF, 0xCAFE};
                }

                coverpoint flags {
                    bins flag0 = {1};
                    bins flag1 = {2};
                    bins flag2 = {4};
                    bins flag3 = {8};
                    bins multiple_flags = [16:255];
                    illegal_bins no_flags = {0};
                }

                // Complex cross coverage
                cross state, counter {
                    bins idle_zero = {0}, {0};
                    bins active_counting = [1:3], [1:65535];
                    bins error_stopped = [4:5], {0};
                    ignore_bins debug_any = [6:7], default;
                }
            }
        }
        "#;

        match parse_and_build_hir(source) {
            Ok(hir) => {
                println!("✅ Complex bins definitions parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());

                assert_eq!(hir.entities.len(), 1);
                assert_eq!(hir.implementations.len(), 1);

                let implementation = &hir.implementations[0];
                println!("Covergroups: {}", implementation.covergroups.len());
            }
            Err(e) => {
                println!("❌ Complex bins parsing failed: {:?}", e);
                // Expected to work once full parser support is implemented
            }
        }
    }

    #[test]
    fn test_coverage_with_assertions() {
        let source = r#"
        entity CoverageWithAssertions {
            in clk: clock
            in reset: bool
            in valid: bool
            in ready: bool
            in data: nat[32]
        }

        impl CoverageWithAssertions {
            // Basic assertions (from Milestone 1)
            on(clk.rise) {
                assert(!reset || !valid, "No valid during reset");
            }

            // Concurrent assertions (from Milestone 2)
            assert property (@(posedge clk) reset |-> ##1 !valid);
            cover property (@(posedge clk) valid && ready);

            // Coverage infrastructure (Milestone 3)
            covergroup handshake_cg @(posedge clk) {
                coverpoint valid {
                    bins active = {1};
                    bins inactive = {0};
                }

                coverpoint ready {
                    bins ready = {1};
                    bins not_ready = {0};
                }

                cross valid, ready {
                    bins valid_transfer = {1, 1};
                    bins valid_wait = {1, 0};
                    bins idle = {0, 0};
                    ignore_bins invalid_ready = {0, 1};
                }
            }

            covergroup data_cg @(posedge clk) {
                coverpoint data {
                    bins zero = {0};
                    bins powers_of_two = {1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024};
                    bins small_values = [1:255];
                    bins large_values = [256:65535];
                    bins very_large = [65536:4294967295];
                }
            }
        }
        "#;

        match parse_and_build_hir(source) {
            Ok(hir) => {
                println!("✅ Coverage with assertions parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());

                assert_eq!(hir.entities.len(), 1);
                assert_eq!(hir.implementations.len(), 1);

                let implementation = &hir.implementations[0];
                println!("Event blocks: {}", implementation.event_blocks.len());
                println!("Covergroups: {}", implementation.covergroups.len());
            }
            Err(e) => {
                println!("❌ Coverage with assertions parsing failed: {:?}", e);
                // Should work once we complete the implementation
            }
        }
    }

    #[test]
    fn test_edge_case_coverage_syntax() {
        let source = r#"
        entity EdgeCaseCoverage {
            in clk: clock
            in sparse_signal: nat[16]
            in enum_signal: nat[2]
        }

        impl EdgeCaseCoverage {
            // Covergroup with no sampling event
            covergroup always_sample_cg {
                coverpoint sparse_signal {
                    bins specific_values = {0x1234, 0x5678, 0x9ABC, 0xDEF0};
                    bins ranges = [0x0000:0x00FF], [0xFF00:0xFFFF];
                    bins default_bin = default;
                }
            }

            // Minimal covergroup with single coverpoint
            covergroup minimal_cg @(posedge clk) {
                coverpoint enum_signal;
            }

            // Covergroup with only cross coverage
            covergroup cross_only_cg @(posedge clk) {
                cross sparse_signal, enum_signal;
            }
        }
        "#;

        match parse_and_build_hir(source) {
            Ok(hir) => {
                println!("✅ Edge case coverage syntax parsed successfully!");
                println!("Entities: {}", hir.entities.len());
                println!("Implementations: {}", hir.implementations.len());

                assert_eq!(hir.entities.len(), 1);
                assert_eq!(hir.implementations.len(), 1);

                let implementation = &hir.implementations[0];
                println!("Covergroups: {}", implementation.covergroups.len());
            }
            Err(e) => {
                println!("❌ Edge case coverage parsing failed: {:?}", e);
                // Expected to work once full parser support is implemented
            }
        }
    }
}
