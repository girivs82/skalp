//! A generic entity chosen as the top of a testbench runs with its default
//! generics, even when the same file also uses a specialization of it. The
//! top used to be found by name prefix, so `Stepper` matched `Stepper_3`, the
//! wrapper's specialization, and the test simulated the wrong design.

use skalp_testing::testbench::Testbench;

const DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/generic_top/");

fn fixture(name: &str) -> String {
    format!("{DIR}{name}")
}

/// `count` after reset and `cycles` clocks, running `top` of `file`
async fn count_after(file: &str, top: &str, cycles: usize) -> u32 {
    let mut tb = Testbench::behavioral_with_top(&fixture(file), top)
        .await
        .expect("build");
    tb.set("rst", 1u8);
    tb.clock(2).await;
    tb.set("rst", 0u8);
    tb.clock(cycles).await;
    tb.get_u32("count").await
}

/// `Stepper` (step 1 by default) runs with step 1 although the file's
/// wrapper uses `Stepper<3>`
#[tokio::test]
async fn a_generic_top_runs_with_its_defaults() {
    assert_eq!(count_after("stepper.sk", "Stepper", 4).await, 4);
}

/// The wrapper still runs its own specialization
#[tokio::test]
async fn a_wrapper_runs_its_specialization() {
    assert_eq!(count_after("stepper.sk", "TripleStepper", 4).await, 12);
}

/// A generic entity no one instantiates runs with its defaults
#[tokio::test]
async fn an_uninstantiated_generic_top_runs_with_its_defaults() {
    assert_eq!(count_after("default_only.sk", "Stepper", 4).await, 8);
}

/// A generic top with a generic that has no default cannot run on its own:
/// that is an error, not a run of some specialization the file happens to use
#[tokio::test]
async fn a_generic_top_without_defaults_is_an_error() {
    let error = match Testbench::behavioral_with_top(&fixture("no_default.sk"), "Stepper").await {
        Ok(_) => panic!("built a top whose generic has no value"),
        Err(e) => format!("{e:#}"),
    };
    assert!(
        error.contains("`Stepper`") && error.contains("without a default"),
        "{error}"
    );
    assert_eq!(count_after("no_default.sk", "DoubleStepper", 4).await, 8);
}

/// Building for a top adds its default specialization only: a design run from
/// its wrapper has no module for the generic entity's defaults
#[test]
fn only_a_requested_top_gets_a_default_specialization() {
    let path = std::path::PathBuf::from(fixture("stepper.sk"));
    let names = |ctx: skalp_frontend::CompilationContext| -> Vec<String> {
        ctx.main_hir
            .entities
            .iter()
            .map(|e| e.name.clone())
            .collect()
    };
    let plain = names(skalp_frontend::parse_and_build_compilation_context(&path).unwrap());
    assert!(!plain.contains(&"Stepper_1".to_string()), "{plain:?}");

    let ctx = skalp_frontend::parse_and_build_compilation_context_for_top(&path, None, "Stepper")
        .unwrap();
    assert_eq!(ctx.top_module.as_deref(), Some("Stepper_1"));
    let with_top = names(ctx);
    assert!(with_top.contains(&"Stepper_1".to_string()), "{with_top:?}");
    assert!(with_top.contains(&"Stepper_3".to_string()), "{with_top:?}");

    let ctx =
        skalp_frontend::parse_and_build_compilation_context_for_top(&path, None, "TripleStepper")
            .unwrap();
    assert_eq!(ctx.top_module.as_deref(), Some("TripleStepper"));
}
