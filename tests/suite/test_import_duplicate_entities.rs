//! Importing a symbol that is also pulled in as a trait-method dependency.
//!
//! `use skalp::numeric::fp::{fp32, FpAdd}` merges fp32 first, and fp32's
//! `impl Add for fp32` instantiates FpAdd, so FpAdd arrives as a method
//! dependency before the import list ever reaches it. Merging the explicit
//! FpAdd on top produced a SECOND entity with the same name, its own
//! implementation, and its own monomorphization — two definitions of one
//! module name in the emitted SystemVerilog.

use std::collections::HashMap;

fn build_sv(src: &str) -> String {
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        concat!(env!("CARGO_MANIFEST_DIR"), "/crates/skalp-stdlib"),
    );
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("m.sk");
    std::fs::write(&path, src).unwrap();
    let ctx = skalp_frontend::parse_and_build_compilation_context(&path).expect("parse");
    let mir = skalp_mir::MirCompiler::new()
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .expect("lower");
    skalp_codegen::generate_systemverilog_from_mir(&mir).expect("sv")
}

/// Module names must be unique. A repeated name is not cosmetic: each copy is
/// a separate lowering of the same entity, so the netlist carries two of the
/// hardware, and no SystemVerilog tool will accept two definitions of one
/// module.
fn assert_module_names_unique(sv: &str) {
    let mut counts: HashMap<&str, usize> = HashMap::new();
    for line in sv.lines() {
        if let Some(rest) = line.strip_prefix("module ") {
            let name = rest
                .split(|c: char| !(c.is_alphanumeric() || c == '_'))
                .next()
                .unwrap_or("");
            if !name.is_empty() {
                *counts.entry(name).or_default() += 1;
            }
        }
    }
    assert!(!counts.is_empty(), "no modules were emitted at all");
    let dups: Vec<_> = counts.iter().filter(|(_, &n)| n > 1).collect();
    assert!(
        dups.is_empty(),
        "these module names are defined more than once: {dups:?}"
    );
}

/// The reported case: the trait-dependency path and the explicit import name
/// the same entity.
#[test]
fn importing_an_entity_its_trait_impl_already_pulled_in_defines_it_once() {
    let sv = build_sv(
        r#"
use skalp::numeric::fp::{fp32, FpAdd};
use skalp::numeric::formats::{IEEE754_32};

entity DupA {
    in a: fp32
    in b: fp32
    out o: fp32
}

impl DupA {
    inst m = FpAdd<IEEE754_32> { a: a, b: b }
    o = m.result
}
"#,
    );
    assert_module_names_unique(&sv);
}

/// The whole family at once, in the order `examples/stdlib_showcase.sk` uses.
/// Every one of these was emitted twice — both the generic entity and its
/// monomorphization — because fp32 leads the list.
#[test]
fn a_full_import_list_defines_each_entity_once() {
    let sv = build_sv(
        r#"
use skalp::numeric::fp::{fp32, FpAdd, FpMul, FpSqrt, FpSub, FpDiv};
use skalp::numeric::formats::{IEEE754_32};

entity DupB {
    in a: fp32
    in b: fp32
    out o: fp32
}

impl DupB {
    inst s = FpAdd<IEEE754_32> { a: a, b: b }
    inst d = FpSub<IEEE754_32> { a: a, b: b }
    inst m = FpMul<IEEE754_32> { a: a, b: b }
    inst q = FpDiv<IEEE754_32> { a: a, b: b }
    inst r = FpSqrt<IEEE754_32> { x: a }
    o = s.result
}
"#,
    );
    assert_module_names_unique(&sv);
}

/// The same rule on the glob path, which merged every public entity without
/// checking either. Importing a module twice — once by name, once by glob —
/// must not define anything twice.
#[test]
fn a_glob_import_over_an_explicit_one_defines_each_entity_once() {
    let sv = build_sv(
        r#"
use skalp::numeric::fp::{fp32, FpAdd};
use skalp::numeric::fp::*;
use skalp::numeric::formats::{IEEE754_32};

entity DupC {
    in a: fp32
    in b: fp32
    out o: fp32
}

impl DupC {
    inst m = FpAdd<IEEE754_32> { a: a, b: b }
    o = m.result
}
"#,
    );
    assert_module_names_unique(&sv);
}

/// Deduplicating must not cost the kept copy its logic — the failure mode
/// would be keeping the bodiless one. FpAdd_fp32 is the monomorphization that
/// carries the arithmetic.
#[test]
fn the_surviving_copy_still_has_its_body() {
    let sv = build_sv(
        r#"
use skalp::numeric::fp::{fp32, FpAdd};
use skalp::numeric::formats::{IEEE754_32};

entity DupD {
    in a: fp32
    in b: fp32
    out o: fp32
}

impl DupD {
    inst m = FpAdd<IEEE754_32> { a: a, b: b }
    o = m.result
}
"#,
    );
    let body = sv
        .split("module FpAdd_fp32")
        .nth(1)
        .and_then(|s| s.split("endmodule").next())
        .expect("FpAdd_fp32 must be emitted");
    assert!(
        body.contains("assign"),
        "FpAdd_fp32 kept no logic:\n{body}"
    );
}
