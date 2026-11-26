//! Registers on the falling edge of a clock (`on(clk.fall)`).
//!
//! Synthesis must keep the edge: the flip-flops carry
//! `FF_CLOCK_EDGE_PARAM = FF_CLOCK_FALLING` with the clock itself on their
//! clock pin, and the gate-level simulation loads them on the falling edge.
//! A backend that cannot build them refuses the design.

use skalp_lir::gate_netlist::ff_clock_falling;
use skalp_lir::GateNetlist;
use skalp_testing::Testbench;

fn fixture_path(name: &str) -> String {
    format!("{}/tests/fixtures/{}", env!("CARGO_MANIFEST_DIR"), name)
}

fn synthesize(fixture: &str, library: &str) -> GateNetlist {
    let path = std::path::PathBuf::from(fixture_path(fixture));
    let ctx = skalp_frontend::parse_and_build_compilation_context(&path).unwrap();
    let mir = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .unwrap();
    let (hier, _) = skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir);
    let library = skalp_lir::get_stdlib_library(library).unwrap();
    skalp_lir::synthesize_hierarchical(&hier, &library, skalp_lir::synth::SynthPreset::Balanced)
        .flatten()
}

/// Four flip-flops on each edge, all on the clock net itself (no inverter
/// in front of the falling-edge ones)
#[test]
fn falling_edge_registers_keep_their_edge_through_synthesis() {
    let net = synthesize("falling_edge_ff.sk", "nexus");
    let flip_flops: Vec<_> = net.cells.iter().filter(|c| c.clock.is_some()).collect();
    assert_eq!(flip_flops.len(), 8, "{flip_flops:#?}");
    let falling = flip_flops.iter().filter(|c| ff_clock_falling(c)).count();
    assert_eq!(
        falling, 4,
        "f loads on the falling edge, r on the rising edge"
    );
    let clocks: std::collections::HashSet<_> = flip_flops.iter().map(|c| c.clock).collect();
    assert_eq!(
        clocks.len(),
        1,
        "every flip-flop is on the clock net itself"
    );
}

/// Drive the clock by hand (it starts low): `r` takes `d` at a rising edge,
/// and `f` takes `r + 1` only at the next falling edge
async fn check_half_cycle_behaviour(tb: &mut Testbench, what: &str) {
    tb.set("clk", 0u8).set("d", 5u8);
    tb.step().await;
    tb.set("clk", 1u8);
    tb.step().await;
    assert_eq!(
        tb.get_u64("p").await,
        5,
        "{what}: r loads at the rising edge"
    );
    assert_eq!(
        tb.get_u64("q").await,
        0,
        "{what}: f has seen no falling edge yet"
    );
    tb.set("clk", 0u8).set("d", 9u8);
    tb.step().await;
    assert_eq!(
        tb.get_u64("p").await,
        5,
        "{what}: r holds at the falling edge"
    );
    assert_eq!(
        tb.get_u64("q").await,
        6,
        "{what}: f loads at the falling edge"
    );
    tb.set("clk", 1u8);
    tb.step().await;
    assert_eq!(
        tb.get_u64("p").await,
        9,
        "{what}: r loads at the next rising edge"
    );
    assert_eq!(
        tb.get_u64("q").await,
        6,
        "{what}: f holds at the rising edge"
    );
    tb.set("clk", 0u8);
    tb.step().await;
    assert_eq!(tb.get_u64("q").await, 10, "{what}");
}

#[tokio::test]
async fn falling_edge_registers_behavioural() {
    let mut tb = Testbench::behavioral_with_top(&fixture_path("falling_edge_ff.sk"), "FallingEdge")
        .await
        .unwrap();
    check_half_cycle_behaviour(&mut tb, "behavioural").await;
}

#[tokio::test]
async fn falling_edge_registers_gate_level() {
    let mut tb = Testbench::gate_level_with_top_and_library(
        &fixture_path("falling_edge_ff.sk"),
        "FallingEdge",
        "nexus",
    )
    .await
    .unwrap();
    check_half_cycle_behaviour(&mut tb, "gate level").await;
}

/// The iCE40 backend does not build falling-edge flip-flops yet, so
/// placement says so rather than building them on the rising edge
#[test]
fn backends_without_falling_edge_registers_refuse_them() {
    let net = synthesize("falling_edge_ff.sk", "ice40");
    assert!(net.cells.iter().any(ff_clock_falling));
    let err =
        skalp_place_route::pnr::place_and_route_hx1k(&net, skalp_place_route::PnrConfig::default())
            .expect_err("placement refuses a falling-edge flip-flop");
    assert!(err.to_string().contains("falling edge"), "{err}");
}
