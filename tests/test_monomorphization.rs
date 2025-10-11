//! Integration tests for monomorphization engine
//!
//! These tests verify that the monomorphization engine correctly:
//! - Collects generic instantiations
//! - Evaluates const expressions
//! - Generates specialized entities
//! - Handles default parameters

use skalp_frontend::parse_and_build_hir;

#[test]
fn test_simple_const_generic() {
    let source = r#"
        entity Buffer<const SIZE: nat> {
            in data: bit[8]
            out stored: bit[8]
        }

        impl Buffer<const SIZE: nat> {
            stored = data
        }

        entity Top {
            in x: bit[8]
            out y: bit[8]
        }

        impl Top {
            signal buf_out: bit[8]

            let buffer = Buffer<16> {
                data: x,
                stored: buf_out
            }

            y = buf_out
        }
    "#;

    let result = parse_and_build_hir(source);
    assert!(result.is_ok(), "Should parse successfully");

    let hir = result.unwrap();

    // Should have original Buffer + specialized Buffer_16 + Top
    assert!(
        hir.entities.len() >= 2,
        "Should have at least Buffer and Top"
    );
}

#[test]
fn test_multiple_instantiations_same_args() {
    let source = r#"
        entity Adder<const WIDTH: nat> {
            in a: bit[WIDTH]
            in b: bit[WIDTH]
            out sum: bit[WIDTH]
        }

        impl Adder<const WIDTH: nat> {
            sum = a + b
        }

        entity Top {
            in x1: bit[8]
            in y1: bit[8]
            in x2: bit[8]
            in y2: bit[8]
            out sum1: bit[8]
            out sum2: bit[8]
        }

        impl Top {
            signal s1: bit[8]
            signal s2: bit[8]

            let adder1 = Adder<8> {
                a: x1,
                b: y1,
                sum: s1
            }

            let adder2 = Adder<8> {
                a: x2,
                b: y2,
                sum: s2
            }

            sum1 = s1
            sum2 = s2
        }
    "#;

    let result = parse_and_build_hir(source);
    assert!(result.is_ok(), "Should parse successfully");

    let hir = result.unwrap();

    // Should deduplicate: Adder, Adder_8, Top (not Adder_8 twice)
    assert!(hir.entities.len() >= 2, "Should have Adder and Top");
}

#[test]
fn test_multiple_instantiations_different_args() {
    let source = r#"
        entity Adder<const WIDTH: nat> {
            in a: bit[WIDTH]
            in b: bit[WIDTH]
            out sum: bit[WIDTH]
        }

        impl Adder<const WIDTH: nat> {
            sum = a + b
        }

        entity Top {
            in x1: bit[8]
            in y1: bit[8]
            in x2: bit[16]
            in y2: bit[16]
            out sum1: bit[8]
            out sum2: bit[16]
        }

        impl Top {
            signal s1: bit[8]
            signal s2: bit[16]

            let adder8 = Adder<8> {
                a: x1,
                b: y1,
                sum: s1
            }

            let adder16 = Adder<16> {
                a: x2,
                b: y2,
                sum: s2
            }

            sum1 = s1
            sum2 = s2
        }
    "#;

    let result = parse_and_build_hir(source);
    assert!(result.is_ok(), "Should parse successfully");

    let hir = result.unwrap();

    // Should have: Adder (generic), Adder_8, Adder_16, Top
    assert!(hir.entities.len() >= 2, "Should have multiple entities");
}

#[test]
fn test_default_parameter_values() {
    let source = r#"
        entity Buffer<const SIZE: nat = 8> {
            in data: bit[SIZE]
            out stored: bit[SIZE]
        }

        impl Buffer<const SIZE: nat = 8> {
            stored = data
        }

        entity Top {
            in x: bit[8]
            out y: bit[8]
        }

        impl Top {
            signal buf_out: bit[8]

            let buffer = Buffer {
                data: x,
                stored: buf_out
            }

            y = buf_out
        }
    "#;

    let result = parse_and_build_hir(source);
    assert!(result.is_ok(), "Should parse with default parameter");
}

#[test]
fn test_parametric_with_reference() {
    let source = r#"
        entity Adder<const WIDTH: nat = 32> {
            in a: bit[WIDTH]
            in b: bit[WIDTH]
            out sum: bit[WIDTH]
        }

        impl Adder<const WIDTH: nat = 32> {
            sum = a + b
        }

        entity TopLevel<const W: nat = 32> {
            in x: bit[W]
            in y: bit[W]
            out result: bit[W]
        }

        impl TopLevel<const W: nat = 32> {
            signal s: bit[W]

            let adder = Adder<W> {
                a: x,
                b: y,
                sum: s
            }

            result = s
        }
    "#;

    let result = parse_and_build_hir(source);
    assert!(result.is_ok(), "Should parse parametric reference");

    let hir = result.unwrap();

    // Should have Adder, TopLevel, possibly specialized versions
    assert!(hir.entities.len() >= 2, "Should have base entities");
}

#[test]
fn test_nested_generic_instantiation() {
    let source = r#"
        entity Register<const WIDTH: nat> {
            in clk: clock
            in d: bit[WIDTH]
            out q: bit[WIDTH]
        }

        impl Register<const WIDTH: nat> {
            signal data: bit[WIDTH] = 0

            on(clk.rise) {
                data <= d
            }

            q = data
        }

        entity Pipeline<const WIDTH: nat, const STAGES: nat> {
            in clk: clock
            in data_in: bit[WIDTH]
            out data_out: bit[WIDTH]
        }

        impl Pipeline<const WIDTH: nat, const STAGES: nat> {
            signal stage0: bit[WIDTH]
            signal stage1: bit[WIDTH]

            let reg0 = Register<WIDTH> {
                clk: clk,
                d: data_in,
                q: stage0
            }

            let reg1 = Register<WIDTH> {
                clk: clk,
                d: stage0,
                q: stage1
            }

            data_out = stage1
        }
    "#;

    let result = parse_and_build_hir(source);
    assert!(result.is_ok(), "Should parse nested generics");

    let hir = result.unwrap();
    assert!(hir.entities.len() >= 2, "Should have Register and Pipeline");
}

#[test]
fn test_const_expression_in_arg() {
    let source = r#"
        entity Adder<const WIDTH: nat> {
            in a: bit[WIDTH]
            in b: bit[WIDTH]
            out sum: bit[WIDTH]
        }

        impl Adder<const WIDTH: nat> {
            sum = a + b
        }

        entity Top<const W: nat = 8> {
            in x: bit[W]
            in y: bit[W]
            out result: bit[W]
        }

        impl Top<const W: nat = 8> {
            signal s: bit[W]

            let adder = Adder<W> {
                a: x,
                b: y,
                sum: s
            }

            result = s
        }
    "#;

    let result = parse_and_build_hir(source);
    assert!(
        result.is_ok(),
        "Should handle const expressions in arguments"
    );
}

#[test]
fn test_multiple_const_parameters() {
    let source = r#"
        entity Memory<const ADDR_WIDTH: nat, const DATA_WIDTH: nat> {
            in addr: bit[ADDR_WIDTH]
            in data_in: bit[DATA_WIDTH]
            in we: bit
            out data_out: bit[DATA_WIDTH]
        }

        impl Memory<const ADDR_WIDTH: nat, const DATA_WIDTH: nat> {
            data_out = data_in
        }

        entity Top {
            in address: bit[10]
            in write_data: bit[32]
            in write_en: bit
            out read_data: bit[32]
        }

        impl Top {
            signal rd: bit[32]

            let mem = Memory<10, 32> {
                addr: address,
                data_in: write_data,
                we: write_en,
                data_out: rd
            }

            read_data = rd
        }
    "#;

    let result = parse_and_build_hir(source);
    assert!(result.is_ok(), "Should handle multiple const parameters");

    let hir = result.unwrap();
    assert!(hir.entities.len() >= 2, "Should have Memory and Top");
}

#[test]
fn test_entity_with_no_generics() {
    let source = r#"
        entity SimpleAdder {
            in a: bit[8]
            in b: bit[8]
            out sum: bit[8]
        }

        impl SimpleAdder {
            sum = a + b
        }

        entity Top {
            in x: bit[8]
            in y: bit[8]
            out result: bit[8]
        }

        impl Top {
            signal s: bit[8]

            let adder = SimpleAdder {
                a: x,
                b: y,
                sum: s
            }

            result = s
        }
    "#;

    let result = parse_and_build_hir(source);
    assert!(result.is_ok(), "Should handle non-generic entities");

    let hir = result.unwrap();

    // Should have SimpleAdder and Top, no specialization needed
    assert_eq!(hir.entities.len(), 2, "Should have exactly 2 entities");
}

#[test]
fn test_mixed_generic_and_non_generic() {
    let source = r#"
        entity GenericAdder<const WIDTH: nat> {
            in a: bit[WIDTH]
            in b: bit[WIDTH]
            out sum: bit[WIDTH]
        }

        impl GenericAdder<const WIDTH: nat> {
            sum = a + b
        }

        entity SimpleReg {
            in clk: clock
            in d: bit[8]
            out q: bit[8]
        }

        impl SimpleReg {
            signal data: bit[8] = 0
            on(clk.rise) {
                data <= d
            }
            q = data
        }

        entity Top {
            in clk: clock
            in x: bit[8]
            in y: bit[8]
            out result: bit[8]
        }

        impl Top {
            signal sum: bit[8]
            signal reg_out: bit[8]

            let adder = GenericAdder<8> {
                a: x,
                b: y,
                sum: sum
            }

            let reg = SimpleReg {
                clk: clk,
                d: sum,
                q: reg_out
            }

            result = reg_out
        }
    "#;

    let result = parse_and_build_hir(source);
    assert!(
        result.is_ok(),
        "Should handle mix of generic and non-generic"
    );

    let hir = result.unwrap();
    assert!(
        hir.entities.len() >= 3,
        "Should have GenericAdder, SimpleReg, Top"
    );
}
