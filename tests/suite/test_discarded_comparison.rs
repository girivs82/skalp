// Regression: `r <= r + 1` as a statement inside on() is the SystemVerilog
// non-blocking habit — it used to parse as a DISCARDED comparison and
// synthesize an empty always block (silent no-op). It must be a hard error.

#[cfg(test)]
mod discarded_comparison {
    use skalp_frontend::parse_and_build_hir;

    #[test]
    fn statement_lte_in_on_block_is_an_error() {
        let source = r#"
        entity C {
            in clk: clock
            out q: bit[8]
        }

        impl C {
            signal r: bit[8] = 0
            on(clk.rise) {
                r <= r + 1
            }
            q = r
        }
        "#;
        let err = parse_and_build_hir(source).expect_err("discarded `<=` must fail the build");
        let msg = format!("{err:#}");
        assert!(
            msg.contains("discards the result of `<=`"),
            "wrong error: {msg}"
        );
    }

    #[test]
    fn comparison_in_condition_still_works() {
        let source = r#"
        entity C {
            in clk: clock
            out q: bit[8]
        }

        impl C {
            signal r: bit[8] = 0
            on(clk.rise) {
                if r <= 100 {
                    r = r + 1
                }
            }
            q = r
        }
        "#;
        parse_and_build_hir(source).expect("comparison in a condition is fine");
    }

    #[test]
    fn trailing_expression_in_fn_body_still_works() {
        let source = r#"
        fn le(a: bit[8], b: bit[8]) -> bit {
            return a <= b
        }

        entity C {
            in x: bit[8]
            in y: bit[8]
            out le_out: bit
        }

        impl C {
            le_out = le(x, y)
        }
        "#;
        parse_and_build_hir(source).expect("comparison as fn return value is fine");
    }
}
