//! `#[pipeline(stages = N)]` adds stages in MIR, so behavioural and
//! gate-level simulation see the same latency.
//!
//! The fixture feeds one LFSR stream to a two-stage multiply-add-normalise
//! unit and to the same unit with `#[pipeline(stages = 4)]`, and compares the
//! second with the first delayed two cycles. led0 latches a mismatch; led1
//! rises once 255 results have been compared. The fault twin compares with a
//! one-cycle delay, so its led0 must rise.

use skalp_testing::Testbench;

fn fixture_path(name: &str) -> String {
    format!("{}/tests/fixtures/{}", env!("CARGO_MANIFEST_DIR"), name)
}

fn compile(source: &std::path::Path) -> Result<skalp_mir::Mir, String> {
    let ctx = skalp_frontend::parse_and_build_compilation_context(source)
        .map_err(|e| format!("{e:?}"))?;
    skalp_mir::MirCompiler::new().compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
}

fn latency(mir: &skalp_mir::Mir, name: &str) -> Option<u32> {
    let module = mir.modules.iter().find(|m| m.name == name).unwrap();
    skalp_mir::pipelining::module_latency(mir, module)
}

#[test]
fn test_pipeline_stages_reach_the_requested_latency() {
    let mir = compile(std::path::Path::new(&fixture_path(
        "pipeline_stages_check.sk",
    )))
    .unwrap();
    assert_eq!(latency(&mir, "MulAddNorm"), Some(2));
    assert_eq!(latency(&mir, "MulAddNormPiped"), Some(4));
    // The original body lives on unchanged beside the wrapper
    assert_eq!(latency(&mir, "MulAddNormPiped__core"), Some(2));
}

#[test]
fn test_pipeline_stages_below_the_current_latency_is_an_error() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("too_few.sk");
    std::fs::write(
        &path,
        r#"
#[pipeline(stages = 1)]
entity TwoDeep {
    in clk: clock
    in x: bit[8]
    out y: bit[8]
}
impl TwoDeep {
    signal r1: bit[8] = 0
    signal r2: bit[8] = 0
    y = r2
    on(clk.rise) {
        r1 = x + 1
        r2 = r1
    }
}
"#,
    )
    .unwrap();
    let error = compile(&path).unwrap_err();
    assert!(
        error.contains("already has a latency of 2"),
        "unexpected error: {error}"
    );
}

/// `#[latency(N)]` on an instance must match the entity's latency, stages
/// included: the fixture's instances pass, and a wrong count is an error
#[test]
fn test_instance_latency_attribute_is_checked() {
    let fixture = std::fs::read_to_string(fixture_path("pipeline_stages_check.sk")).unwrap();
    assert!(fixture.contains("#[latency(4)]\n    inst piped"));
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("wrong_latency.sk");
    std::fs::write(
        &path,
        fixture.replacen(
            "#[latency(4)]\n    inst piped",
            "#[latency(3)]\n    inst piped",
            1,
        ),
    )
    .unwrap();
    let error = compile(&path).unwrap_err();
    assert!(
        error.contains(
            "in `PipelineStagesCheck`: instance `piped` has #[latency(3)] but `MulAddNormPiped` has a latency of 4 cycles"
        ),
        "unexpected error: {error}"
    );
}

async fn flags_after(top: &str, gate_level: bool) -> u64 {
    let path = fixture_path("pipeline_stages_check.sk");
    let mut tb = if gate_level {
        Testbench::gate_level_with_top_and_library(&path, top, "nexus")
            .await
            .unwrap()
    } else {
        Testbench::with_top_module(&path, top).await.unwrap()
    };
    tb.clock(1_000).await;
    tb.get_u64("led").await & 0b11
}

#[tokio::test]
async fn test_pipeline_stages_match_the_unpipelined_unit_delayed() {
    for gate_level in [false, true] {
        let flags = flags_after("PipelineStagesCheck", gate_level).await;
        assert_eq!(flags, 0b10, "gate level {gate_level}: flags {flags:#b}");
        let flags = flags_after("PipelineStagesFault", gate_level).await;
        assert_eq!(flags, 0b11, "gate level {gate_level}: flags {flags:#b}");
    }
}

/// Synthesis moves the added stages into the unit: its registers are
/// replaced by retimed ones, and the self-check above shows the result
/// still matches
#[test]
fn test_pipeline_stages_are_retimed_into_the_logic() {
    let mut mir = compile(std::path::Path::new(&fixture_path(
        "pipeline_stages_check.sk",
    )))
    .unwrap();
    assert!(mir.retain_reachable_from("PipelineStagesCheck"));
    let (hier, _) = skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir);
    assert_eq!(hier.retime_regions, vec!["top.piped".to_string()]);
    let library = skalp_lir::get_stdlib_library("nexus").unwrap();
    let netlist = skalp_lir::synthesize_hierarchical(
        &hier,
        &library,
        skalp_lir::synth::SynthPreset::Balanced,
    )
    .flatten();
    let in_unit = |c: &&skalp_lir::gate_netlist::Cell| c.path.starts_with("top.piped.");
    let retimed = netlist
        .cells
        .iter()
        .filter(in_unit)
        .filter(|c| c.path.contains(".retime"))
        .count();
    assert!(retimed > 0, "no registers were retimed");
    // The unpipelined unit next to it is left alone
    assert!(!netlist
        .cells
        .iter()
        .any(|c| c.path.starts_with("top.plain.") && c.path.contains(".retime")));
}

/// How many stages a target period needs: none for a slack target, more as
/// the target tightens, and never fewer for a tighter target
#[test]
fn test_extra_stages_for_a_target_period() {
    let mut mir = compile(std::path::Path::new(&fixture_path(
        "pipeline_stages_check.sk",
    )))
    .unwrap();
    assert!(mir.retain_reachable_from("PipelineStagesCheck"));
    let (hier, _) = skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir);
    let library = skalp_lir::get_stdlib_library("nexus").unwrap();
    let netlist = skalp_lir::synthesize_hierarchical(
        &hier,
        &library,
        skalp_lir::synth::SynthPreset::Balanced,
    )
    .flatten();
    let delays = skalp_lir::retime::LevelDelays;
    let needed: Vec<(i64, u32)> = (2..=40)
        .map(|t| t * 100)
        .filter_map(|target| {
            skalp_lir::retime::extra_stages_for_period(&netlist, &delays, "top.plain", target, 6)
                .map(|k| (target, k))
        })
        .collect();
    assert!(!needed.is_empty(), "no target was reachable");
    assert_eq!(
        needed.last().unwrap().1,
        0,
        "a slack target needs no stages: {needed:?}"
    );
    assert!(
        needed.iter().any(|&(_, k)| k > 0),
        "no target needed a stage: {needed:?}"
    );
    assert!(
        needed.windows(2).all(|w| w[0].1 >= w[1].1),
        "a tighter target needed fewer stages: {needed:?}"
    );
}

/// The fixture with `MulAddNormPiped` leaving its latency to synthesis
/// (`#[pipeline]` alone), written to `dir` with `lock` as its pipeline lock
/// file when given
fn auto_fixture(dir: &std::path::Path, lock: Option<&str>) -> std::path::PathBuf {
    let fixture = std::fs::read_to_string(fixture_path("pipeline_stages_check.sk")).unwrap();
    assert!(fixture.contains("#[pipeline(stages = 4)]\nentity"));
    let path = dir.join("auto.sk");
    std::fs::write(
        &path,
        fixture
            .replace(
                "#[pipeline(stages = 4)]\nentity",
                "#[pipeline(max_stages = 6)]\nentity",
            )
            // the instance checks would hold the latency at 4
            .replace("#[latency(4)]\n    inst piped", "inst piped"),
    )
    .unwrap();
    if let Some(lock) = lock {
        std::fs::write(skalp_frontend::pipeline_lock::lock_path(&path), lock).unwrap();
    }
    path
}

/// A bare #[pipeline] keeps the entity's own latency until the lock file
/// records one, then takes the recorded latency in every flow
#[test]
fn test_pipeline_without_stages_takes_the_locked_latency() {
    let dir = tempfile::tempdir().unwrap();
    let mir = compile(&auto_fixture(dir.path(), None)).unwrap();
    assert_eq!(latency(&mir, "MulAddNormPiped"), Some(2));

    let dir = tempfile::tempdir().unwrap();
    let mir = compile(&auto_fixture(dir.path(), Some("MulAddNormPiped = 5\n"))).unwrap();
    assert_eq!(latency(&mir, "MulAddNormPiped"), Some(5));

    let dir = tempfile::tempdir().unwrap();
    let error = compile(&auto_fixture(dir.path(), Some("MulAddNormPiped = 1\n"))).unwrap_err();
    assert!(
        error.contains("the pipeline lock file records 1; synthesize again"),
        "unexpected error: {error}"
    );

    let dir = tempfile::tempdir().unwrap();
    let error = compile(&auto_fixture(dir.path(), Some("MulAddNormPiped = 7\n"))).unwrap_err();
    assert!(
        error.contains("#[pipeline(max_stages = 6)], but the pipeline lock file records 7"),
        "unexpected error: {error}"
    );
}

/// Synthesis chooses the latency from the clock period: its own for a
/// slack clock, more for a tight one, never above max_stages
#[test]
fn test_pipeline_latency_is_chosen_for_the_clock() {
    let dir = tempfile::tempdir().unwrap();
    let mut mir = compile(&auto_fixture(dir.path(), None)).unwrap();
    assert!(mir.retain_reachable_from("PipelineStagesCheck"));
    let library = skalp_lir::get_stdlib_library("nexus").unwrap();
    let choose = |period_ps| {
        let choices = skalp_lir::pipeline_choice::choose_pipeline_stages(
            &mir,
            &library,
            skalp_lir::synth::SynthPreset::Balanced,
            &skalp_lir::retime::LevelDelays,
            period_ps,
        );
        assert_eq!(choices.len(), 1, "{choices:?}");
        assert_eq!(choices[0].entity, "MulAddNormPiped");
        assert_eq!(choices[0].own_latency, 2);
        assert_eq!(choices[0].limit, 6);
        choices[0].latency
    };
    assert_eq!(choose(1_000_000), Some(2));
    let tight: Vec<(i64, Option<u32>)> = (2..=20).map(|t| (t * 100, choose(t * 100))).collect();
    assert!(
        tight.iter().any(|&(_, l)| l.is_some_and(|l| l > 2)),
        "no period needed more latency: {tight:?}"
    );
    assert!(
        tight.iter().all(|&(_, l)| l.is_none_or(|l| l <= 6)),
        "a latency went over max_stages: {tight:?}"
    );
    assert!(
        tight.windows(2).all(|w| match (w[0].1, w[1].1) {
            (Some(a), Some(b)) => a >= b,
            (None, _) => true,
            (Some(_), None) => false,
        }),
        "a tighter clock got less latency: {tight:?}"
    );
}
