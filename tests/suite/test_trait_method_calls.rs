//! Trait methods called by name rather than through their operator.
//!
//! `x * y` and `x.mul(y)` should reach the same `impl Mul for fp32`. They took
//! different resolution paths, and only the operator path worked.

/// `function_name_to_trait_name` mapped `add`/`sub`/`mul`/`div` to
/// `Addable`/`Subtractable`/`Multipliable`/`Divisible` — names that exist
/// nowhere in the tree, stdlib included. Resolution could never match, so
/// `x.mul(y)` failed to lower while `x * y` worked. The stdlib declares
/// `trait Mul { fn mul(..) }`, so the trait name is the method name
/// capitalised.
#[test]
fn arithmetic_trait_methods_lower() {
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        concat!(env!("CARGO_MANIFEST_DIR"), "/crates/skalp-stdlib"),
    );

    // `abs` is deliberately absent: it collides with the free functions
    // `fn abs<T: Numeric>` in vector.sk and `fn abs(x: int)` in fixed.sk and
    // resolves to those instead of the trait method. Separate defect.
    for expr in [
        "k.add(k)",
        "k.sub(k)",
        "k.mul(k)",
        "k.div(k)",
        "k.neg()",
        "k.sqrt()",
    ] {
        let src = format!(
            r#"
use skalp::numeric::fp::{{fp32}};

entity T1 {{
    in k: fp32
    out o: fp32
}}

impl T1 {{
    signal p: fp32
    p = {expr}
    o = p
}}
"#
        );
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("m.sk");
        std::fs::write(&path, src).unwrap();

        let ctx = skalp_frontend::parse_and_build_compilation_context(&path)
            .unwrap_or_else(|e| panic!("`{expr}` must parse: {e}"));
        skalp_mir::MirCompiler::new()
            .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
            .unwrap_or_else(|e| panic!("`{expr}` must lower: {e}"));
    }
}

/// The operator and the method spelling must agree. This is the control for
/// the test above: `*` kept working throughout, which is precisely why the
/// broken method path went unnoticed.
#[test]
fn the_operator_and_the_method_agree() {
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        concat!(env!("CARGO_MANIFEST_DIR"), "/crates/skalp-stdlib"),
    );

    let build = |expr: &str| {
        let src = format!(
            r#"
use skalp::numeric::fp::{{fp32}};

entity T1 {{
    in k: fp32
    out o: fp32
}}

impl T1 {{
    signal p: fp32
    p = {expr}
    o = p
}}
"#
        );
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("m.sk");
        std::fs::write(&path, src).unwrap();
        let ctx = skalp_frontend::parse_and_build_compilation_context(&path)
            .unwrap_or_else(|e| panic!("`{expr}` must parse: {e}"));
        skalp_mir::MirCompiler::new()
            .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
            .unwrap_or_else(|e| panic!("`{expr}` must lower: {e}"));
    };

    build("k * k");
    build("k.mul(k)");
}
