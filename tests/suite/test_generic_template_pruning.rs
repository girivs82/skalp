//! Generic templates that produce no hardware should not reach the output.
//!
//! Monomorphization emits `FpMAC_fp32` with the logic and leaves the generic
//! `FpMAC` behind as parameters and ports with an empty body. Nothing
//! instantiates it, so it reached SystemVerilog as
//! `module FpMAC #(parameter F) (...); endmodule` — dead output that reads
//! like a compiler bug.

use skalp_mir::mir::{Mir, Module};

fn compile(src: &str) -> Mir {
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        concat!(env!("CARGO_MANIFEST_DIR"), "/crates/skalp-stdlib"),
    );
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("m.sk");
    std::fs::write(&path, src).unwrap();
    let ctx = skalp_frontend::parse_and_build_compilation_context(&path).expect("parse");
    skalp_mir::MirCompiler::new()
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .expect("lower")
}

fn has_no_body(m: &Module) -> bool {
    m.assignments.is_empty()
        && m.processes.is_empty()
        && m.instances.is_empty()
        && m.generate_blocks.is_empty()
}

/// A generic entity whose specialization carries the hardware. The template
/// itself must not survive.
#[test]
fn a_specialized_template_is_not_emitted() {
    let mir = compile(
        r#"
use skalp::numeric::fp::{fp32};
use skalp::numeric::formats::{FloatFormat};

entity Mac<const F: FloatFormat> {
    in a: fp<F>
    in b: fp<F>
    out o: fp<F>
}

impl Mac<const F: FloatFormat> {
    o = a * b
}

entity MacTop {
    in a: fp32
    in b: fp32
    out o: fp32
}

impl MacTop {
    inst m = Mac<IEEE754_32> { a: a, b: b }
    o = m.o
}
"#,
    );
    let names: Vec<&str> = mir.modules.iter().map(|m| m.name.as_str()).collect();
    assert!(
        names.iter().any(|n| n.starts_with("Mac_")),
        "the specialization must be emitted, got {names:?}"
    );
    let template = mir.modules.iter().find(|m| m.name == "Mac");
    assert!(
        template.is_none(),
        "the empty generic template must not be emitted, got {names:?}"
    );
}

/// The guard that matters most. A module with NO parameters and no body is
/// not a template — it is the "ports but no logic" miscompile this project
/// shipped until 7c258b9, and a cleanup pass must never tidy it out of the
/// output where a reader can see it.
///
/// `Hollow` has no OUTPUT deliberately. With one, the undriven-output check
/// rejects the design before the prune is ever reached — which is the real
/// first line of defence, and the reason this guard is belt-and-braces rather
/// than the only thing standing between a bodiless module and a silent drop.
#[test]
fn an_empty_module_that_is_not_a_template_survives() {
    let mir = compile(
        r#"
entity Hollow {
    in a: bit
}

impl Hollow {
}

entity HollowTop {
    in a: bit
    out o: bit
}

impl HollowTop {
    o = a
}
"#,
    );
    let hollow = mir
        .modules
        .iter()
        .find(|m| m.name == "Hollow")
        .expect("a bodiless NON-generic module must still be emitted");
    assert!(
        hollow.parameters.is_empty() && has_no_body(hollow),
        "this test is only meaningful while Hollow is bodiless and unparameterized"
    );
}

/// A template something instantiates stays, whatever its body looks like, or
/// the instance dangles at elaboration. The stdlib currently does instantiate
/// unspecialized templates from inlined trait operators, so this is not
/// hypothetical.
#[test]
fn an_instantiated_template_is_kept() {
    let mir = compile(
        r#"
use skalp::numeric::fp::{fp32};

entity KeepTop {
    in a: fp32
    in b: fp32
    out o: fp32
}

impl KeepTop {
    signal s: fp32
    s = a + b
    o = s
}
"#,
    );
    // Every instance must resolve to a module that is still present.
    for m in &mir.modules {
        for inst in &m.instances {
            assert!(
                mir.modules.iter().any(|t| t.id == inst.module),
                "instance `{}` in `{}` points at a module that was pruned",
                inst.name,
                m.name
            );
        }
    }
}

/// Whole-design check on the example that motivated this: no module may be
/// both bodiless and unreferenced unless it is a root the design declared.
#[test]
fn nothing_bodiless_and_unreferenced_survives_in_the_showcase() {
    let src = std::fs::read_to_string(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/examples/stdlib_showcase.sk"
    ))
    .expect("the showcase example must exist");
    let mir = compile(&src);
    let instantiated: std::collections::HashSet<_> = mir
        .modules
        .iter()
        .flat_map(|m| m.instances.iter().map(|i| i.module))
        .collect();
    let first = mir.modules.first().map(|m| m.id);
    let stragglers: Vec<&str> = mir
        .modules
        .iter()
        .filter(|m| !m.parameters.is_empty())
        .filter(|m| has_no_body(m))
        .filter(|m| !instantiated.contains(&m.id))
        .filter(|m| Some(m.id) != first)
        // A template with no specialization anywhere is a template the design
        // never used; it is kept deliberately rather than silently dropped.
        .filter(|m| {
            mir.modules
                .iter()
                .any(|o| o.name.rsplit_once('_').map(|(b, _)| b) == Some(m.name.as_str()))
        })
        .map(|m| m.name.as_str())
        .collect();
    assert!(
        stragglers.is_empty(),
        "these specialized templates produce no hardware and nothing references them: {stragglers:?}"
    );
}
