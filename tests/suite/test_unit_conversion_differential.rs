//! Does the same entity convert the same way, whichever path reaches it?
//!
//! An entity can arrive in a design two ways: imported into the main HIR, or
//! left in a module HIR and only instantiated. Those should produce the same
//! hardware. Today they do not — implementations are converted for the main HIR
//! only, so the second path yields a module with ports and no body.
//!
//! This is a differential harness rather than a log: the traces interleave
//! every module across up to eight specialization rounds (`CordicSqrt` appears
//! 27 times in one run), so attributing a line to one conversion of one entity
//! by grepping is unreliable — I misread a neighbouring module's signal as this
//! entity's while chasing exactly that. Comparing two MIRs in-process has no
//! such ambiguity.

use skalp_mir::mir::Mir;

/// What a module actually contains. Ports are deliberately excluded: the
/// failure being pinned is a module that has its ports and nothing else.
#[derive(Debug, PartialEq, Eq)]
struct Shape {
    assignments: usize,
    processes: usize,
    instances: usize,
}

impl Shape {
    fn is_empty(&self) -> bool {
        self.assignments == 0 && self.processes == 0 && self.instances == 0
    }
}

fn compile(src: &str) -> Result<Mir, String> {
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        concat!(env!("CARGO_MANIFEST_DIR"), "/crates/skalp-stdlib"),
    );
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("m.sk");
    std::fs::write(&path, src).unwrap();
    let ctx = skalp_frontend::parse_and_build_compilation_context(&path)
        .map_err(|e| format!("frontend: {e}"))?;
    skalp_mir::MirCompiler::new().compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
}

fn shape_of(mir: &Mir, module: &str) -> Option<Shape> {
    mir.modules.iter().find(|m| m.name == module).map(|m| Shape {
        assignments: m.assignments.len(),
        processes: m.processes.len(),
        instances: m.instances.len(),
    })
}

/// Reached by importing `cordic` directly, so `CordicSqrt` is merged into the
/// main HIR and converted with it.
const VIA_DIRECT_IMPORT: &str = r#"
use skalp::numeric::cordic::{CordicSqrt};
use skalp::numeric::fp::{fp32};

entity DA {
    in v: fp32
    out o: fp32
}

impl DA {
    inst c = CordicSqrt { value: v }
    o = c.result
}
"#;

/// Reached only through `fp`: fp.sk does `use skalp::numeric::cordic::CordicSqrt`
/// and FpSqrt instantiates it, so `CordicSqrt` stays in a module HIR and this
/// design never names it.
const VIA_MODULE_HIR: &str = r#"
use skalp::numeric::fp::{fp32, FpSqrt, IEEE754_32};

entity DB {
    in v: fp32
    out o: fp32
}

impl DB {
    inst s = FpSqrt<IEEE754_32> { x: v }
    o = s.result
}
"#;

/// The good side, pinned so it cannot silently rot: reached by direct import,
/// the entity gets a real body.
#[test]
fn an_entity_imported_directly_gets_a_body() {
    let mir = compile(VIA_DIRECT_IMPORT).expect("the direct-import design must build");
    let shape = shape_of(&mir, "CordicSqrt").expect("CordicSqrt must be in the design");
    assert!(
        !shape.is_empty(),
        "CordicSqrt must have a body, got {shape:?}"
    );
}

/// The differential itself. Un-ignore when module-HIR implementations are
/// converted; it is the acceptance test for that change.
///
/// Today the second design does not even compile: `CordicSqrt` is emitted as
/// ports with no logic, and the undriven-output check catches it. That check is
/// the only thing standing between this and a silently empty module in real
/// hardware — a design whose output happened to be driven elsewhere would emit
/// dead logic and say nothing.
#[test]
#[ignore = "known defect: module-HIR implementations are never converted"]
fn the_same_entity_converts_the_same_whichever_path_reaches_it() {
    let direct = compile(VIA_DIRECT_IMPORT).expect("the direct-import design must build");
    let via_module = match compile(VIA_MODULE_HIR) {
        Ok(mir) => mir,
        Err(e) => panic!(
            "reaching CordicSqrt only through a module HIR fails to compile, \
             where importing it directly succeeds:\n  {e}"
        ),
    };

    let a = shape_of(&direct, "CordicSqrt").expect("CordicSqrt via direct import");
    let b = shape_of(&via_module, "CordicSqrt").expect("CordicSqrt via module HIR");
    assert_eq!(
        a, b,
        "the same entity produced different hardware depending on how it was \
         reached: direct import {a:?}, via module HIR {b:?}"
    );
}
