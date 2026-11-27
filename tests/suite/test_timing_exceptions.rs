//! Timing exceptions on registers (`#[cdc]`, `#[cdc(cdc_type = quasi_static)]`).
//!
//! Synthesis must keep them: the flip-flops of a quasi-static register carry
//! `FF_TIMING_PARAM = UNTIMED_FROM`, the first stage of a synchronizer
//! `UNTIMED_TO`, through hierarchy and every optimisation pass.

use skalp_lir::gate_netlist::{ff_timing_exception, TimingException};
use skalp_lir::GateNetlist;

fn synthesize(preset: skalp_lir::synth::SynthPreset) -> GateNetlist {
    let path = std::path::PathBuf::from(format!(
        "{}/tests/fixtures/timing_exceptions.sk",
        env!("CARGO_MANIFEST_DIR")
    ));
    let ctx = skalp_frontend::parse_and_build_compilation_context(&path).unwrap();
    let mir = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .unwrap();
    let (hier, _) = skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir);
    let library = skalp_lir::get_stdlib_library("nexus").unwrap();
    skalp_lir::synthesize_hierarchical(&hier, &library, preset).flatten()
}

/// Four `mode` flip-flops are untimed-from, the one `meta` flip-flop is
/// untimed-to, and `count` and `held` carry no exception
#[test]
fn timing_exceptions_survive_synthesis() {
    for preset in [
        skalp_lir::synth::SynthPreset::Quick,
        skalp_lir::synth::SynthPreset::Balanced,
        skalp_lir::synth::SynthPreset::Full,
        skalp_lir::synth::SynthPreset::Timing,
    ] {
        let net = synthesize(preset);
        let flip_flops: Vec<_> = net.cells.iter().filter(|c| c.clock.is_some()).collect();
        assert_eq!(flip_flops.len(), 10, "{preset:?}: {flip_flops:#?}");
        let count = |e: TimingException| {
            flip_flops
                .iter()
                .filter(|c| ff_timing_exception(c) == Some(e))
                .count()
        };
        assert_eq!(count(TimingException::UntimedFrom), 4, "{preset:?}: mode");
        assert_eq!(count(TimingException::UntimedTo), 1, "{preset:?}: meta");
    }
}
