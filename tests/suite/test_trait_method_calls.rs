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

/// `vec3<fp32>` was stored as `HirType::Custom("vec3")`, dropping the element
/// type. convert_type then had to guess it (defaulting to fp32, with a TODO
/// saying so), and infer_hir_type had nothing to return for `v[i]`, so indexing
/// fell to its `_ => Bit(1)` case and `v[i].mul(k)` looked for
/// `impl Mul for bit[1]`. The Vec2/3/4 variants already carry an element type
/// and MIR already converts them; the builder just was not producing them.
#[test]
fn a_vector_element_keeps_its_type() {
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        concat!(env!("CARGO_MANIFEST_DIR"), "/crates/skalp-stdlib"),
    );

    let src = r#"
use skalp::numeric::fp::{fp32};
use skalp::numeric::vector::{vec3};

entity ScaleC {
    in v: vec3<fp32>
    in k: fp32
    out o: vec3<fp32>
}

impl ScaleC {
    signal out_v: vec3<fp32>
    generate for i in 0..3 {
        out_v[i] = v[i].mul(k)
    }
    o = out_v
}
"#;
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("m.sk");
    std::fs::write(&path, src).unwrap();

    let ctx = skalp_frontend::parse_and_build_compilation_context(&path).expect("parse");
    let mir = skalp_mir::MirCompiler::new()
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .expect("`v[i].mul(k)` must lower");
    let sv = skalp_codegen::generate_systemverilog_from_mir(&mir).expect("sv");

    // Each component must reach its own multiplier as a WHOLE component. The
    // first cut of this fix made the type resolve while still lowering `v[0]`
    // as a bit-select, emitting `.a(v__x[0])` — bit 0 of the component. That
    // built cleanly and was wrong, which is why this asserts the connection
    // rather than that the build succeeded.
    for c in ["x", "y", "z"] {
        assert!(
            sv.contains(&format!(".a(v__{c})")),
            "component {c} must feed its multiplier whole:\n{sv}"
        );
        assert!(
            !sv.contains(&format!(".a(v__{c}[0])")),
            "component {c} must not be bit-selected:\n{sv}"
        );
    }
}

/// Reading a whole vector output off an instance into an output PORT. Only a
/// signal destination was handled, so this was dropped; ports became the
/// common case once vectors started flattening into x/y/z like any other
/// aggregate. Failed before this change too, for the same reason.
#[test]
fn a_whole_vector_instance_output_reaches_a_port() {
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        concat!(env!("CARGO_MANIFEST_DIR"), "/crates/skalp-stdlib"),
    );

    let src = r#"
use skalp::numeric::fp::{fp32};
use skalp::numeric::vector::{vec3};

entity Child { in v: vec3<fp32> out o: vec3<fp32> }
impl Child { o = v }

entity Par { in v: vec3<fp32> out o: vec3<fp32> }
impl Par {
    inst c = Child { v: v }
    o = c.o
}
"#;
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("m.sk");
    std::fs::write(&path, src).unwrap();

    let ctx = skalp_frontend::parse_and_build_compilation_context(&path).expect("parse");
    let mir = skalp_mir::MirCompiler::new()
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .expect("`o = c.o` must lower");
    let sv = skalp_codegen::generate_systemverilog_from_mir(&mir).expect("sv");

    for c in ["x", "y", "z"] {
        assert!(
            sv.contains(&format!("assign o__{c} = c_o__{c};")),
            "component {c} must be wired through:\n{sv}"
        );
    }
}

/// Reading a whole ARRAY output off an instance, with no vectors involved.
/// The flattener PRESERVES an array of scalars as one array port (see
/// should_preserve_array) while flattening the same array in a signal, so the
/// child's output arrives as `o__0..o__n` and nothing matched it against the
/// single destination port — the assignment was dropped. Vectors dodged this
/// by being a different kind that flattens; plain arrays did not.
#[test]
fn a_whole_array_instance_output_reaches_an_array_port() {
    let src = r#"
entity AChild { in v: bit[8][4] out o: bit[8][4] }
impl AChild { o = v }

entity APar { in v: bit[8][4] out o: bit[8][4] }
impl APar {
    inst c = AChild { v: v }
    o = c.o
}
"#;
    let hir = skalp_frontend::parse_and_build_hir(src).expect("parse");
    let mir = skalp_mir::MirCompiler::new()
        .compile_to_mir(&hir)
        .expect("`o = c.o` between array ports must lower");
    let sv = skalp_codegen::generate_systemverilog_from_mir(&mir).expect("sv");

    for i in 0..4 {
        assert!(
            sv.contains(&format!("assign o[{i}] = c_o_{i};")),
            "element {i} must be wired through:\n{sv}"
        );
    }
}
