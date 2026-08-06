// Regression: `x.sqrt()` on fp32 compiled through the merged-HIR test path
// but FAILED through the CLI pipeline (compile_to_mir_with_modules with the
// stdlib as separate module HIRs): impls built from module HIRs carry port
// references as GenericParam("x"), and the GenericParam resolution chain
// never checked the current entity's ports by name — every port read in
// FpSqrt_fp32's impl was "undefined identifier `x`".

#[cfg(test)]
mod sqrt_cli_path {
    use skalp_frontend::parse_and_build_compilation_context;
    use std::io::Write;

    #[test]
    fn sqrt_compiles_through_module_hirs_pipeline() {
        std::env::set_var("SKALP_STDLIB_PATH", "./crates/skalp-stdlib");
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
        let temp = std::env::temp_dir().join(format!("sqrt_cli_path_{}.sk", std::process::id()));
        let mut f = std::fs::File::create(&temp).expect("temp file");
        f.write_all(source.as_bytes()).expect("write");

        let ctx = parse_and_build_compilation_context(&temp).expect("parse + modules");
        let _ = std::fs::remove_file(&temp);

        // The CLI's exact MIR pipeline: modules kept as separate HIRs.
        let compiler = skalp_mir::MirCompiler::new()
            .with_optimization_level(skalp_mir::OptimizationLevel::None);
        let mir = compiler
            .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
            .expect("sqrt design must compile through the module-HIRs pipeline");

        let fpsqrt = mir
            .modules
            .iter()
            .find(|m| m.name == "FpSqrt_fp32")
            .expect("FpSqrt_fp32 module must exist");
        // The impl's port reads must have resolved: the specialized sqrt
        // module is not a stub — it computes through internal signals and
        // CordicSqrt instances.
        assert!(
            !fpsqrt.assignments.is_empty() || !fpsqrt.instances.is_empty(),
            "FpSqrt_fp32 lowered to an empty module — port references unresolved"
        );
    }
}
