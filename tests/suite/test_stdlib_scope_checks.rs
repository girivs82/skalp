// Regression: with the stdlib preloaded as module HIRs, building a
// functions-only file (no entities) failed the undriven-output check on
// stdlib Cordic entities the design never instantiates — the empty
// main_entity_names fallback made EVERY module a check root, including
// generic stdlib templates monomorphized wholesale. External (module-HIR)
// entities are now excluded from the fallback root set; designs that
// genuinely instantiate stdlib hardware keep full checks via the
// instantiation edge.

#[cfg(test)]
mod stdlib_scope_checks {
    use skalp_frontend::parse_and_build_compilation_context;
    use std::io::Write;

    fn compile(source: &str, tag: &str) -> Result<skalp_mir::Mir, String> {
        std::env::set_var("SKALP_STDLIB_PATH", "./crates/skalp-stdlib");
        let temp =
            std::env::temp_dir().join(format!("stdlib_scope_{}_{}.sk", tag, std::process::id()));
        let mut f = std::fs::File::create(&temp).expect("temp file");
        f.write_all(source.as_bytes()).expect("write");
        let ctx = parse_and_build_compilation_context(&temp).expect("parse + modules");
        let _ = std::fs::remove_file(&temp);
        skalp_mir::MirCompiler::new()
            .with_optimization_level(skalp_mir::OptimizationLevel::None)
            .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
    }

    #[test]
    fn functions_only_file_builds_with_stdlib_loaded() {
        // No entities at all: nothing user-reachable exists, so unused stdlib
        // templates must not be subjected to silently-wrong-hardware checks.
        let source = r#"
        use skalp::numeric::fp::*;

        pub fn double(x: bit[8]) -> bit[8] {
            return x + x
        }
        "#;
        compile(source, "fns_only").expect("functions-only file must compile");
    }

    #[test]
    fn real_stdlib_usage_still_fully_checked() {
        // A design that instantiates stdlib fp hardware keeps the full checks
        // (Cordic reached via FpSqrt) — and passes them.
        let source = r#"
        use skalp::numeric::fp::*;
        use skalp::numeric::formats::fp32;

        entity SqrtUnit {
            in x: fp32
            out y: fp32
        }

        impl SqrtUnit {
            y = x.sqrt()
        }
        "#;
        let mir = compile(source, "real_usage").expect("sqrt design must compile");
        assert!(
            mir.modules.iter().any(|m| m.name == "FpSqrt_fp32"),
            "stdlib hardware must still be lowered when actually used"
        );
    }
}
