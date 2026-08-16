//! Monomorphization of TYPE parameters.
//!
//! Const generics specialized correctly throughout (`FIR<8>` -> `FIR_8`).
//! Type generics only did when the parameter carried no trait bound, which is
//! the opposite of how they are usually written.

fn hir(src: &str) -> skalp_frontend::CompilationContext {
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        concat!(env!("CARGO_MANIFEST_DIR"), "/crates/skalp-stdlib"),
    );
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("m.sk");
    std::fs::write(&path, src).unwrap();
    // Leaked so the temp dir outlives the borrow; test process is short-lived.
    std::mem::forget(dir);
    skalp_frontend::parse_and_build_compilation_context(&path).expect("parse")
}

fn port_types(ctx: &skalp_frontend::CompilationContext, name: &str) -> Vec<String> {
    ctx.main_hir
        .entities
        .iter()
        .find(|e| e.name == name)
        .unwrap_or_else(|| {
            panic!(
                "no entity `{name}`; have {:?}",
                ctx.main_hir
                    .entities
                    .iter()
                    .map(|e| &e.name)
                    .collect::<Vec<_>>()
            )
        })
        .ports
        .iter()
        .map(|p| format!("{:?}", p.port_type))
        .collect()
}

const IDENTITY: &str = r#"
use skalp::numeric::fp::{fp32};

entity Idty<T PARAM> { in a: T out o: T }
impl Idty<T PARAM> { o = a }

entity Top3 { in a: fp32 out o: fp32 }
impl Top3 {
    inst i = Idty<fp32> { a: a }
    o = i.o
}
"#;

/// A bound does not change what the parameter is. The collector matched
/// `HirGenericType::Type` but not `TypeWithBounds`, so `<T: Numeric>` recorded
/// no type argument, built no specialization, and the instantiation resolved to
/// a copy of the template with its generics list emptied and `T` still standing
/// in its ports.
#[test]
fn a_trait_bound_does_not_disable_monomorphization() {
    let unbounded = hir(&IDENTITY.replace(" PARAM", ""));
    let bounded = hir(&IDENTITY.replace("PARAM", ": Numeric"));

    let a = port_types(&unbounded, "Idty_fp32");
    let b = port_types(&bounded, "Idty_fp32");
    assert_eq!(a, b, "a bound must not change the specialization");
    assert!(
        a.iter().all(|t| t.contains("fp32")),
        "T must be substituted, got {a:?}"
    );
}

/// A parameter can sit INSIDE an aggregate: `v: vec3<T>` names T, but the
/// connected expression has type `vec3<fp32>`, so T is its ELEMENT. Taking the
/// name from the element and the type from the whole connection bound
/// T = vec3<fp32> and specialized as `Scale_vec3_fp32`, every `vec3<T>` port
/// becoming a vector of vectors.
///
/// And the port a connection names belongs to the entity whose implementation
/// holds the instance. Scanning every entity for a matching raw PortId is
/// wrong — PortIds restart per entity — and inferred T for this design from an
/// unrelated `flags: bit[5]` port, yielding `Scale_bit5`.
#[test]
fn a_parameter_inside_an_aggregate_binds_to_the_element() {
    let ctx = hir(r#"
use skalp::numeric::fp::{fp32};
use skalp::numeric::vector::{vec3};

entity Scale<T: Numeric> {
    in v: vec3<T>
    in k: T
    out o: vec3<T>
}

impl Scale<T: Numeric> {
    signal out_v: vec3<T>
    generate for i in 0..3 {
        out_v[i] = v[i].mul(k)
    }
    o = out_v
}

entity TopS {
    in v: vec3<fp32>
    in k: fp32
    out o: vec3<fp32>
}

impl TopS {
    inst s = Scale<fp32> { v: v, k: k }
    o = s.o
}
"#);

    let names: Vec<&String> = ctx.main_hir.entities.iter().map(|e| &e.name).collect();
    assert!(
        names.iter().any(|n| *n == "Scale_fp32"),
        "T must bind to the ELEMENT fp32, not vec3<fp32> or bit[5]; got {names:?}"
    );

    let mir = skalp_mir::MirCompiler::new()
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .expect("the specialized entity must lower");
    let sv = skalp_codegen::generate_systemverilog_from_mir(&mir).expect("sv");

    // Each element multiplied whole, by its own multiplier.
    for i in 0..3 {
        assert!(
            sv.contains(&format!(".a(v[{i}])")),
            "element {i} must feed its own multiplier:\n{sv}"
        );
    }
}

/// `vec<T, N>` is the general alias (`pub type vec<T, const N: nat> = T[N]`),
/// so it carries a size as well as an element and the size may be the parameter
/// itself. It degraded to `Custom("vec")`, losing both, which is why an entity
/// declaring `vec<T, N>` ports could not have T inferred from them.
#[test]
fn the_generic_vec_alias_keeps_its_arguments() {
    let ctx = hir(r#"
use skalp::numeric::fp::{fp32};
use skalp::numeric::vector::{vec3, VecAdd};

entity TV {
    in a: vec3<fp32>
    in b: vec3<fp32>
    out o: vec3<fp32>
}

impl TV {
    inst s = VecAdd<fp32, 3> { a: a, b: b }
    o = s.result
}
"#);
    let names: Vec<&String> = ctx.main_hir.entities.iter().map(|e| &e.name).collect();
    assert!(
        names.iter().any(|n| *n == "VecAdd_3_fp32"),
        "T must be inferred from `vec<T, N>` ports; got {names:?}"
    );
}

/// Instantiating a generic entity from INSIDE another generic entity connects
/// ports that are themselves symbolic, so inference read T back as `T` and
/// bound T -> T. substitute_type followed that cycle forever: a stack overflow,
/// which is not a diagnostic and cannot be caught. Reaching the end of this
/// test at all is the assertion.
#[test]
fn a_parameter_is_never_bound_to_itself() {
    let ctx = hir(r#"
use skalp::numeric::fp::{fp32};
use skalp::numeric::vector::{vec3, VecAdd};

entity Inner<T: Numeric> {
    in a: vec3<T>
    in b: vec3<T>
    out o: vec3<T>
}

impl Inner<T: Numeric> {
    // `a` and `b` are still `vec<T, _>` here — the connected types are
    // symbolic, so nothing concrete can be inferred for the child.
    inst s = VecAdd<T, 3> { a: a, b: b }
    o = s.result
}

entity Outer {
    in a: vec3<fp32>
    in b: vec3<fp32>
    out o: vec3<fp32>
}

impl Outer {
    inst i = Inner<fp32> { a: a, b: b }
    o = i.o
}
"#);
    // Whether it lowers is a separate question; it must not recurse forever.
    let _ = skalp_mir::MirCompiler::new()
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs);
}
