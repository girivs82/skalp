//! Five-input LUTs (a Nexus slice's two LUT4s and the mux between them).
//!
//! A function of five inputs maps to one `OXIDE_LUT5` cell on Nexus, one
//! level instead of two, and the gate-level netlist computes the same values
//! as the source for every input.

use skalp_lir::{CellFunction, GateNetlist};
use skalp_testing::Testbench;

fn fixture_path() -> String {
    format!(
        "{}/tests/fixtures/wide_lut_functions.sk",
        env!("CARGO_MANIFEST_DIR")
    )
}

fn synthesize(library: &str) -> GateNetlist {
    let path = std::path::PathBuf::from(fixture_path());
    let ctx = skalp_frontend::parse_and_build_compilation_context(&path).unwrap();
    let mir = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .unwrap();
    let (hier, _) = skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir);
    let library = skalp_lir::get_stdlib_library(library).unwrap();
    skalp_lir::synthesize_hierarchical(&hier, &library, skalp_lir::synth::SynthPreset::Quick)
        .flatten_for_library(&library)
}

fn expected(a: u8) -> [u64; 3] {
    let bit = |i: u8| (a >> i) & 1 == 1;
    let parity = a.count_ones() % 2 == 1;
    let select = if bit(4) {
        bit(0) && bit(1)
    } else {
        bit(2) || bit(3)
    };
    let vote = (bit(0) && bit(1) && bit(2)) || (bit(3) && bit(4));
    [parity as u64, select as u64, vote as u64]
}

#[test]
fn each_five_input_function_is_one_lut_on_nexus() {
    let net = synthesize("nexus");
    let logic: Vec<_> = net
        .cells
        .iter()
        .filter(|c| c.lut_init.is_some() || c.function == Some(CellFunction::Lut5))
        .collect();
    let wide = logic
        .iter()
        .filter(|c| c.function == Some(CellFunction::Lut5))
        .count();
    assert_eq!(
        (wide, logic.len()),
        (3, 3),
        "{:?}",
        logic.iter().map(|c| &c.cell_type).collect::<Vec<_>>()
    );
}

#[tokio::test]
async fn five_input_functions_match_the_source_at_gate_level() {
    let mut tb =
        Testbench::gate_level_with_top_and_library(&fixture_path(), "WideFunctions", "nexus")
            .await
            .unwrap();
    for a in 0..32u8 {
        tb.set("a", a);
        tb.clock(1).await;
        let got = [
            tb.get_u64("parity").await,
            tb.get_u64("select").await,
            tb.get_u64("vote").await,
        ];
        assert_eq!(got, expected(a), "a = {a:#07b}");
    }
}
