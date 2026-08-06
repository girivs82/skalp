// Regression: `q = match ... with intent::x` — build_assignment's RHS
// node-kind filter lacked WithIntentExpr (the intent suffix wraps the whole
// RHS), so exprs.len() < 2 and the entire port assignment was silently
// dropped (caught later only as an undriven-output error).

#[cfg(test)]
mod with_intent_assignment {
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::lower_to_mir;

    #[test]
    fn match_with_intent_drives_the_port() {
        let source = r#"
        intent fast = mux_style::parallel;

        entity Sel {
            in s: bit[2]
            in a: bit[8]
            in b: bit[8]
            in c: bit[8]
            in d: bit[8]
            out q: bit[8]
        }

        impl Sel {
            q = match s {
                0b00 => a,
                0b01 => b,
                0b10 => c,
                _ => d
            } with intent::fast
        }
        "#;
        let hir = parse_and_build_hir(source).expect("parse");
        assert_eq!(
            hir.implementations[0].assignments.len(),
            1,
            "the with-intent assignment must survive HIR building"
        );
        let mir = lower_to_mir(&hir).expect("mir");
        let sel = mir.modules.iter().find(|m| m.name == "Sel").expect("Sel");
        assert!(!sel.assignments.is_empty(), "port q must be driven in MIR");
    }
}
