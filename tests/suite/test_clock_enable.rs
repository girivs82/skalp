//! Clock enables extracted from registers that hold their value.
//!
//! A register written only under some condition keeps its value through a
//! clock-enable flip-flop instead of feeding its output back through a LUT.
//! The fixture checks such registers, with an asynchronous and with a
//! synchronous reset, against reference registers that cannot get an enable;
//! a second top with a wrong reference shows that the check sees an error.

use skalp_lir::{CellFunction, GateNetlist};
use skalp_testing::Testbench;

fn fixture_path(name: &str) -> String {
    format!("{}/tests/fixtures/{}", env!("CARGO_MANIFEST_DIR"), name)
}

fn synthesize(library: &str) -> GateNetlist {
    let top = "EnableCheck";
    let path = std::path::PathBuf::from(fixture_path("clock_enable_check.sk"));
    let ctx = skalp_frontend::parse_and_build_compilation_context(&path).unwrap();
    let mir = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .unwrap();
    let hier = skalp_lir::lower_mir_hierarchical_with_top(&mir, Some(top));
    let library = skalp_lir::get_stdlib_library(library).unwrap();
    skalp_lir::synthesize_hierarchical(&hier, &library, skalp_lir::synth::SynthPreset::Balanced)
        .flatten()
}

fn count(net: &GateNetlist, f: CellFunction) -> usize {
    net.cells
        .iter()
        .filter(|c| c.function == Some(f.clone()))
        .count()
}

#[test]
fn test_held_registers_get_clock_enables_on_nexus() {
    let net = synthesize("nexus");
    // GatedAsync's 32 register bits keep their asynchronous reset and get
    // an enable; GatedSync's take their reset through the enable
    assert_eq!(count(&net, CellFunction::DffRE), 32);
    assert!(count(&net, CellFunction::DffE) >= 32);
    // An enable flip-flop takes [D, enable] with its reset beside them
    for cell in &net.cells {
        if matches!(
            cell.function,
            Some(CellFunction::DffE) | Some(CellFunction::DffRE)
        ) {
            assert_eq!(cell.inputs.len(), 2, "{}", cell.path);
            assert_eq!(
                cell.reset.is_some(),
                cell.function == Some(CellFunction::DffRE),
                "{}",
                cell.path
            );
        }
    }
}

async fn flags_after(fixture: &str, top: &str, library: &str, cycles: usize) -> u64 {
    let mut tb = Testbench::gate_level_with_top_and_library(&fixture_path(fixture), top, library)
        .await
        .unwrap();
    tb.clock(cycles).await;
    tb.get_u64("led").await
}

#[tokio::test]
async fn test_clock_enable_registers_match_the_reference_at_gate_level() {
    // 255 cycles of power-on reset, then about 1800 checked cycles
    for library in ["nexus", "ice40", "generic_asic"] {
        let flags = flags_after("clock_enable_check.sk", "EnableCheck", library, 2048).await;
        assert_eq!(flags, 0, "{library}: flags {flags:#b}");
    }
}

#[tokio::test]
async fn test_clock_enable_check_sees_a_wrong_reference() {
    // b of both copies differs from the reference: flags 1 and 4
    let flags = flags_after("clock_enable_fault.sk", "EnableFault", "nexus", 2048).await;
    assert_eq!(flags, 0b01_0010);
}
