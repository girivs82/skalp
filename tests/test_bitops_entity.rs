//! Tests for entity-based bitops implementations using generate blocks

#[cfg(test)]
mod bitops_entity_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};

    /// Test BitReverser8 entity with generate-for
    #[test]
    fn test_bit_reverser_8() {
        let source = r#"
        entity BitReverser8 {
            in clk: clock
            in data_in: nat[8]
            out data_out: nat[8]
        }

        impl BitReverser8 {
            signal reversed: nat[8] = 0

            on(clk.rise) {
                generate for i in 0..8 {
                    reversed[i] <= data_in[7 - i]
                }
                data_out <= reversed
            }
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse BitReverser8");
        println!("HIR generation successful for BitReverser8");

        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile BitReverser8 to MIR");

        println!("MIR compilation successful");
        println!("Module: {}", mir.modules[0].name);

        println!("BitReverser8 test passed!");
    }

    /// Test BitReverser32 entity with generate-for over 32 bits
    #[test]
    fn test_bit_reverser_32() {
        let source = r#"
        entity BitReverser32 {
            in clk: clock
            in data_in: nat[32]
            out data_out: nat[32]
        }

        impl BitReverser32 {
            signal reversed: nat[32] = 0

            on(clk.rise) {
                generate for i in 0..32 {
                    reversed[i] <= data_in[31 - i]
                }
                data_out <= reversed
            }
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse BitReverser32");
        println!("HIR generation successful for BitReverser32");

        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile BitReverser32 to MIR");

        println!("BitReverser32 test passed!");
    }

    /// Test BinaryToGray8 converter using generate-for
    #[test]
    fn test_binary_to_gray() {
        let source = r#"
        entity BinaryToGray8 {
            in clk: clock
            in binary: nat[8]
            out gray: nat[8]
        }

        impl BinaryToGray8 {
            signal gray_code: nat[8] = 0

            on(clk.rise) {
                // MSB stays the same
                gray_code[7] <= binary[7]

                // Each other bit is XOR of adjacent binary bits
                generate for i in 0..7 {
                    gray_code[i] <= binary[i] ^ binary[i + 1]
                }

                gray <= gray_code
            }
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse BinaryToGray8");
        println!("HIR generation successful for BinaryToGray8");

        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile BinaryToGray8 to MIR");

        println!("BinaryToGray8 test passed!");
    }

    /// Test Parity8 calculator
    #[test]
    fn test_parity_8() {
        let source = r#"
        entity Parity8 {
            in clk: clock
            in data_in: nat[8]
            out parity: bit
        }

        impl Parity8 {
            signal p: bit = 0

            on(clk.rise) {
                // XOR all bits together
                p <= data_in[0] ^ data_in[1] ^ data_in[2] ^ data_in[3] ^
                     data_in[4] ^ data_in[5] ^ data_in[6] ^ data_in[7]
                parity <= p
            }
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse Parity8");
        println!("HIR generation successful for Parity8");

        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile Parity8 to MIR");

        println!("Parity8 test passed!");
    }

    /// Test generate-if for compile-time configuration
    #[test]
    fn test_configurable_mask() {
        let source = r#"
        const USE_16BIT_MODE: bool = true;

        entity ConfigurableMask {
            in clk: clock
            in data_in: nat[32]
            out data_out: nat[32]
        }

        impl ConfigurableMask {
            signal result: nat[32] = 0

            on(clk.rise) {
                generate if USE_16BIT_MODE {
                    result <= data_in & 0x0000FFFF
                } else {
                    result <= data_in
                }
                data_out <= result
            }
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse ConfigurableMask");
        println!("HIR generation successful for ConfigurableMask");

        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile ConfigurableMask to MIR");

        println!("ConfigurableMask test passed!");
    }

    /// Test nested generate-for (matrix pattern)
    #[test]
    fn test_nested_generate_matrix() {
        let source = r#"
        const ROWS: nat[8] = 2;
        const COLS: nat[8] = 2;

        entity MatrixInitializer {
            in clk: clock
            in enable: bit
            out done: bit
        }

        impl MatrixInitializer {
            signal initialized: bit = 0

            on(clk.rise) {
                generate for row in 0..ROWS {
                    generate for col in 0..COLS {
                        initialized <= 1
                    }
                }
                done <= initialized
            }
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse MatrixInitializer");
        println!("HIR generation successful for MatrixInitializer");

        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile MatrixInitializer to MIR");

        println!("MatrixInitializer (nested generate) test passed!");
    }

    /// Test generate-for with step
    #[test]
    fn test_generate_with_step() {
        let source = r#"
        entity SteppedProcessor {
            in clk: clock
            in data_in: nat[16]
            out data_out: nat[16]
        }

        impl SteppedProcessor {
            signal result: nat[16] = 0

            on(clk.rise) {
                // Process every other bit (step of 2)
                generate for i in 0..8 step 2 {
                    result[i] <= data_in[i]
                }
                data_out <= result
            }
        }
        "#;

        let hir = parse_and_build_hir(source).expect("Failed to parse SteppedProcessor");
        println!("HIR generation successful for SteppedProcessor");

        let compiler = MirCompiler::new().with_optimization_level(OptimizationLevel::None);
        let _mir = compiler
            .compile_to_mir(&hir)
            .expect("Failed to compile SteppedProcessor to MIR");

        println!("SteppedProcessor (generate with step) test passed!");
    }
}
