//! Operators that nest must each reach their own specialization.
//!
//! `a + b + c` inlines the outer `+`, and converting its left operand inlines
//! the inner one. Both bind the same generic (`N` from `impl Add for bit<N>`),
//! and the inner unbound it on the way out instead of restoring what it
//! shadowed. The outer body was then left with no `N`, so
//! `inst adder = std_adder<N>` could not evaluate its argument, the specialized
//! name collapsed from `std_adder_8` to `std_adder`, and the design was emitted
//! with an instance of a module that is never defined.
//!
//! `skalp build` exited 0. The netlist could not elaborate.

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

/// Every module an instance names must be defined in the same output. This is
/// the property the emitted netlist actually needs, and the one that was
/// violated.
fn assert_every_instance_resolves(sv: &str) {
    let defined: std::collections::HashSet<&str> = sv
        .lines()
        .filter_map(|l| l.strip_prefix("module "))
        .map(|r| {
            r.split(|c: char| !(c.is_alphanumeric() || c == '_'))
                .next()
                .unwrap_or("")
        })
        .collect();
    let keywords = [
        "input", "output", "inout", "wire", "reg", "assign", "always", "if", "else", "case",
        "begin", "end", "logic", "generate", "endgenerate", "initial", "module", "endmodule",
        "function", "task", "parameter", "localparam",
    ];
    let mut missing: Vec<&str> = Vec::new();
    for line in sv.lines() {
        // An instance is indented; a `module Foo (` declaration starts at
        // column 0 and otherwise looks identical to one. Without this the
        // declaration of the design's own top reads as an instance of a module
        // called "module", which is what this assertion first reported.
        if !line.starts_with(char::is_whitespace) {
            continue;
        }
        let t = line.trim_end();
        if !t.ends_with('(') {
            continue;
        }
        let words: Vec<&str> = t.trim_end_matches('(').split_whitespace().collect();
        if words.len() != 2 {
            continue;
        }
        let target = words[0];
        if keywords.contains(&target) || !target.chars().all(|c| c.is_alphanumeric() || c == '_') {
            continue;
        }
        if !defined.contains(target) {
            missing.push(target);
        }
    }
    missing.sort_unstable();
    missing.dedup();
    assert!(
        missing.is_empty(),
        "these instantiated modules are defined nowhere in the output: {missing:?}\n{sv}"
    );
}

#[test]
fn a_chained_addition_specializes_every_adder() {
    let sv = build_sv(
        r#"
entity Chain {
    in a: bit[8]
    in b: bit[8]
    in c: bit[8]
    out o: bit[8]
}

impl Chain {
    signal s: bit[8] = a + b + c
    o = s
}
"#,
    );
    assert_every_instance_resolves(&sv);
    assert!(
        !sv.contains("std_adder __"),
        "an adder was left unspecialized:\n{sv}"
    );
}

/// Four deep, and mixed with another operator so the nesting is not uniform.
/// The binding that gets shadowed belongs to whichever impl is inlined
/// outermost, so depth and mixture are what vary the failure.
#[test]
fn deeper_and_mixed_operator_chains_specialize() {
    let sv = build_sv(
        r#"
entity Deep {
    in a: bit[8]
    in b: bit[8]
    in c: bit[8]
    in d: bit[8]
    out o: bit[8]
}

impl Deep {
    signal s: bit[8] = a + b + c + d
    signal t: bit[8] = (a + b) - (c + d)
    o = s ^ t
}
"#,
    );
    assert_every_instance_resolves(&sv);
    assert!(
        !sv.contains("std_adder __") && !sv.contains("std_subtractor __"),
        "an operator was left unspecialized:\n{sv}"
    );
}

/// Widths that differ across the chain: the restored binding must be the
/// OUTER one, not merely some binding. If the inner value were left in place
/// the outer adder would specialize to the wrong width, which is worse than
/// failing to specialize because it builds and is wrong.
#[test]
fn a_chain_over_two_widths_keeps_each_adder_at_its_own_width() {
    let sv = build_sv(
        r#"
entity Widths {
    in a: bit[8]
    in b: bit[8]
    in w: bit[16]
    in x: bit[16]
    out o: bit[8]
    out p: bit[16]
}

impl Widths {
    signal narrow: bit[8] = a + b + a
    signal wide: bit[16] = w + x + w
    o = narrow
    p = wide
}
"#,
    );
    assert_every_instance_resolves(&sv);
    assert!(
        sv.contains("std_adder_8 ") && sv.contains("std_adder_16 "),
        "each width must get its own adder:\n{sv}"
    );
}
