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
