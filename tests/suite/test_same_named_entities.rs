//! Entities of the same name defined in different files.
//!
//! A design may hold two entities of one name: a wrapper imports `Unit`
//! from one file while the top imports a different `Unit` (a trimmed copy,
//! say) from another. Merged into one design, only one `Unit` survived:
//! whichever import came first took the name, every instance of either
//! `Unit` became an instance of it, and lowering then paired one entity's
//! body with the other's ports and signals, failing with "could not be
//! lowered" or silently building the wrong hardware.
//!
//! Each file must get the `Unit` it imported or defined. An entity whose
//! name is defined in several files keeps the name in the main file and is
//! named after its file elsewhere (`counter_by_one_Unit`).

use skalp_testing::testbench::*;

fn fixture(name: &str) -> String {
    format!(
        "{}/tests/fixtures/same_named_entities/{name}.sk",
        env!("CARGO_MANIFEST_DIR")
    )
}

fn build_sv(name: &str) -> String {
    let ctx =
        skalp_frontend::parse_and_build_compilation_context(std::path::Path::new(&fixture(name)))
            .expect("frontend");
    let mir = skalp_mir::MirCompiler::new()
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .expect("lower to MIR");
    skalp_codegen::generate_systemverilog_from_mir(&mir).expect("SystemVerilog")
}

/// The text of `module name ( ... endmodule`
fn module<'a>(sv: &'a str, name: &str) -> &'a str {
    let start = sv
        .find(&format!("module {name} ("))
        .unwrap_or_else(|| panic!("no module {name} in:\n{sv}"));
    let end = sv[start..].find("endmodule").expect("endmodule") + start;
    &sv[start..end]
}

/// The design's modules, less the stdlib ones a test elsewhere in this
/// process can add by setting SKALP_STDLIB_PATH, sorted
fn module_names(sv: &str) -> Vec<&str> {
    let mut names: Vec<&str> = sv
        .lines()
        .filter_map(|line| line.strip_prefix("module "))
        .map(|rest| {
            rest.split(|c: char| !(c.is_alphanumeric() || c == '_'))
                .next()
                .unwrap()
        })
        .filter(|name| !name.starts_with("std_"))
        .collect();
    names.sort();
    names
}

/// Both `Unit`s are in the design, each with its own body, and each user
/// instantiates its own one.
fn assert_both_units(sv: &str) {
    assert_eq!(
        module_names(sv),
        [
            "Top",
            "Wrapper",
            "counter_by_one_Unit",
            "counter_by_step_Unit"
        ],
        "{sv}"
    );
    // Told apart by their ports and registers: how an add is emitted
    // depends on whether a stdlib is in reach
    let by_one = module(sv, "counter_by_one_Unit");
    let by_step = module(sv, "counter_by_step_Unit");
    assert!(
        by_one.contains("reg [7:0] c") && !by_one.contains("step"),
        "{sv}"
    );
    assert!(
        by_step.contains("input [7:0] step") && by_step.contains("acc"),
        "{sv}"
    );
    assert!(
        module(sv, "Wrapper").contains("counter_by_one_Unit u ("),
        "the wrapper must instantiate the Unit it imported:\n{sv}"
    );
    assert!(
        module(sv, "Top").contains("counter_by_step_Unit v ("),
        "the top must instantiate the Unit it imported:\n{sv}"
    );
}

#[test]
fn each_file_gets_the_entity_it_imported() {
    assert_both_units(&build_sv("top"));
}

/// The first import used to win the name, so the order mattered
#[test]
fn import_order_does_not_choose_the_entity() {
    assert_both_units(&build_sv("top_reversed"));
}

/// A name the main file defines stays as it is there
#[test]
fn the_main_file_keeps_its_own_entity_name() {
    let sv = build_sv("top_own");
    assert_eq!(
        module_names(&sv),
        ["Top", "Unit", "Wrapper", "counter_by_one_Unit"],
        "{sv}"
    );
    assert!(
        module(&sv, "Unit").contains("~go") || module(&sv, "Unit").contains("!go"),
        "{sv}"
    );
    assert!(
        module(&sv, "Wrapper").contains("counter_by_one_Unit u ("),
        "{sv}"
    );
    assert!(module(&sv, "Top").contains("Unit v ("), "{sv}");
}

/// One file reached along several import paths defines one entity: nothing
/// is renamed
#[test]
fn one_entity_imported_along_several_paths_stays_one() {
    let sv = build_sv("top_diamond");
    assert_eq!(
        module_names(&sv),
        ["OtherWrapper", "Top", "Unit", "Wrapper"],
        "{sv}"
    );
}

/// Two globs that bring in different entities of one name, for a file that
/// uses the name: an error that names both files, not a guess
#[test]
fn two_globs_with_different_entities_of_one_name_are_an_error() {
    let err = skalp_frontend::parse_and_build_compilation_context(std::path::Path::new(&fixture(
        "top_ambiguous",
    )))
    .err()
    .expect("an ambiguous name must not build");
    let message = format!("{err:#}");
    assert!(
        message.contains("`Unit`")
            && message.contains("ambiguous")
            && message.contains("counter_by_one.sk")
            && message.contains("counter_by_step.sk"),
        "{message}"
    );
}

/// A named import comes before what a glob brings in
#[test]
fn a_named_import_comes_before_a_glob() {
    assert_both_units(&build_sv("top_glob"));
}

/// A glob import of a module that imported a public entity keeps that
/// entity: the module's rebuild used to drop it, and the wrapper's instance
/// then pointed at the wrapper itself
#[test]
fn a_glob_import_keeps_the_entities_its_module_imported() {
    let sv = build_sv("top_glob_only");
    assert_eq!(module_names(&sv), ["Top", "Unit", "Wrapper"], "{sv}");
    assert!(module(&sv, "Wrapper").contains("Unit u ("), "{sv}");
}

/// The two `Unit`s behave as written: one counts by one, the other by its
/// step, and their `busy` outputs (read as `w.busy` and `v.busy`) differ in
/// timing
#[tokio::test]
async fn both_entities_simulate_as_written() {
    let mut tb = Testbench::behavioral_with_top(&fixture("top"), "Top")
        .await
        .unwrap();
    tb.set("go", 1u8);
    tb.clock(1).await;
    assert_eq!(
        tb.get_u64("b_busy").await,
        1,
        "the stepping Unit is busy at once"
    );
    tb.clock(3).await;
    assert_eq!(
        tb.get_u64("a_count").await,
        4,
        "the wrapper's Unit counts by one"
    );
    assert_eq!(
        tb.get_u64("b_count").await,
        12,
        "the top's Unit counts by 3"
    );
    assert_eq!(tb.get_u64("a_busy").await, 1);
    tb.set("go", 0u8);
    tb.clock(1).await;
    assert_eq!(tb.get_u64("a_busy").await, 0);
    assert_eq!(tb.get_u64("b_busy").await, 0);
    assert_eq!(tb.get_u64("a_count").await, 4);
    assert_eq!(tb.get_u64("b_count").await, 12);
}
