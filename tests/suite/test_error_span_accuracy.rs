// Regression: HIR error spans drifted above the real source line because the
// lexer skipped whitespace entirely — the syntax tree's text was the source
// minus every space, so node offsets mapped to earlier lines. Whitespace is
// now a trivia token (tree text == source text) and make_span trims trailing
// trivia, so diagnostics land on the exact line and column.

#[cfg(test)]
mod error_span_accuracy {
    use skalp_frontend::parse_and_build_hir;

    #[test]
    fn discarded_comparison_error_points_at_the_statement() {
        let source = "\
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
";
        let err = parse_and_build_hir(source).expect_err("must fail");
        let msg = format!("{err:#}");
        // The offending statement is on line 9. Before the fix this reported
        // line 7 (`signal r`).
        assert!(
            msg.contains(":9:") || msg.contains("9:9"),
            "span must point at line 9, got: {msg}"
        );
    }

    #[test]
    fn inst_output_binding_error_points_at_the_inst() {
        let source = "\
entity Add {
    in a: bit[8]
    out s: bit[8]
}

impl Add {
    s = a
}

entity Top {
    in x: bit[8]
    out y: bit[8]
}

impl Top {
    inst adder = Add { a: x, s: y }
    y = adder.s
}
";
        let err = parse_and_build_hir(source).expect_err("must fail");
        let msg = format!("{err:#}");
        // The inst statement is on line 16. Before the fix this reported
        // line 10 (`entity Top`).
        assert!(
            msg.contains(":16:") || msg.contains("16:5"),
            "span must point at line 16, got: {msg}"
        );
    }
}
