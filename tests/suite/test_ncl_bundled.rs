//! A clocked parent driving async (NCL) instances through the implicit
//! `ncl_req`/`ncl_ack` handshake, simulated behaviourally and at gate level.

use skalp_testing::testbench::Testbench;

const FIXTURE: &str = "tests/fixtures/ncl_bundled_handshake.sk";

/// Most clock cycles one handshake phase may take
const PHASE_LIMIT: usize = 20;

/// Run one four-phase handshake with inputs `x`, `y`; returns (sum, total)
async fn handshake(tb: &mut Testbench, x: u8, y: u8) -> (u32, u32) {
    tb.set("x", x).set("y", y).set("go", 1u8);
    let mut cycles = 0;
    while tb.get_u32("done").await == 0 {
        tb.clock(1).await;
        cycles += 1;
        assert!(cycles < PHASE_LIMIT, "no acknowledge for x={x} y={y}");
    }
    // The outputs are registered once more in the parent
    tb.clock(1).await;
    let sum = tb.get_u32("sum").await;
    let total = tb.get_u32("total").await;
    tb.set("go", 0u8);
    cycles = 0;
    while tb.get_u32("done").await != 0 {
        tb.clock(1).await;
        cycles += 1;
        assert!(
            cycles < PHASE_LIMIT,
            "acknowledge never fell for x={x} y={y}"
        );
    }
    (sum, total)
}

async fn run(mut tb: Testbench) {
    tb.set("x", 0u8).set("y", 0u8).set("go", 0u8);
    tb.clock(4).await;
    assert_eq!(tb.get_u32("done").await, 0, "acknowledge high at rest");
    let mut total: u8 = 0;
    for (x, y) in [(3u8, 4u8), (200, 100), (17, 0), (255, 1), (90, 90)] {
        let (sum, got_total) = handshake(&mut tb, x, y).await;
        total = total.wrapping_add(x);
        assert_eq!(sum, x.wrapping_add(y) as u32, "sum of {x} and {y}");
        assert_eq!(
            got_total, total as u32,
            "accumulated total after adding {x}"
        );
    }
}

#[tokio::test]
async fn bundled_handshake_behavioural() {
    let tb = Testbench::behavioral_with_top(FIXTURE, "HsTop")
        .await
        .expect("build behavioural testbench");
    run(tb).await;
}

#[tokio::test]
async fn bundled_handshake_gate_level() {
    let tb = Testbench::gate_level_with_top(FIXTURE, "HsTop")
        .await
        .expect("build gate-level testbench");
    run(tb).await;
}

/// Run one four-phase handshake on the register file; returns the read data
async fn rf_handshake(tb: &mut Testbench, write: Option<(u8, u8)>, ra: u8) -> u32 {
    let (we, wa, wd) = write.map_or((0u8, 0u8, 0u8), |(a, d)| (1, a, d));
    tb.set("we", we).set("wa", wa).set("wd", wd).set("ra", ra);
    tb.set("go", 1u8);
    let mut cycles = 0;
    while tb.get_u32("done").await == 0 {
        tb.clock(1).await;
        cycles += 1;
        assert!(cycles < PHASE_LIMIT, "no acknowledge for {write:?}");
    }
    tb.clock(1).await;
    let rd = tb.get_u32("rd").await;
    tb.set("go", 0u8).set("we", 0u8);
    cycles = 0;
    while tb.get_u32("done").await != 0 {
        tb.clock(1).await;
        cycles += 1;
        assert!(cycles < PHASE_LIMIT, "acknowledge never fell for {write:?}");
    }
    rd
}

async fn run_rf(mut tb: Testbench) {
    tb.set("we", 0u8)
        .set("wa", 0u8)
        .set("wd", 0u8)
        .set("ra", 0u8)
        .set("go", 0u8);
    tb.clock(4).await;
    let mut model = [None::<u8>; 8];
    for (k, (a, d)) in [(0u8, 0x11u8), (5, 0xA5), (7, 0xFF), (5, 0x3C), (2, 0x80)]
        .into_iter()
        .enumerate()
    {
        // Each write lands once, in the addressed word only
        let rd = rf_handshake(&mut tb, Some((a, d)), a).await;
        model[a as usize] = Some(d);
        assert_eq!(rd, d as u32, "write {k}: read back word {a}");
        for (w, value) in model.iter().enumerate() {
            if let Some(v) = value {
                let rd = rf_handshake(&mut tb, None, w as u8).await;
                assert_eq!(rd, *v as u32, "after write {k}: word {w}");
            }
        }
    }
}

#[tokio::test]
async fn bundled_register_file_behavioural() {
    let tb = Testbench::behavioral_with_top(FIXTURE, "HsRfTop")
        .await
        .expect("build behavioural testbench");
    run_rf(tb).await;
}

#[tokio::test]
async fn bundled_register_file_gate_level() {
    let tb = Testbench::gate_level_with_top(FIXTURE, "HsRfTop")
        .await
        .expect("build gate-level testbench");
    run_rf(tb).await;
}

/// Async blocks inside an async block step with it: one handshake updates the
/// parent's register and both children's accumulators together, from the
/// values before the step.
async fn run_cluster(mut tb: Testbench) {
    tb.set("x", 0u8).set("y", 0u8).set("go", 0u8);
    tb.clock(4).await;
    let (mut a, mut b, mut n) = (0u8, 0u8, 0u8);
    for (x, y) in [(3u8, 4u8), (200, 100), (17, 0), (255, 1), (90, 90), (1, 2)] {
        tb.set("x", x).set("y", y).set("go", 1u8);
        let mut cycles = 0;
        while tb.get_u32("done").await == 0 {
            tb.clock(1).await;
            cycles += 1;
            assert!(cycles < PHASE_LIMIT, "no acknowledge for x={x} y={y}");
        }
        let (a_old, b_old) = (a, b);
        a = a_old.wrapping_add(x);
        b = b_old.wrapping_add(a_old.wrapping_add(y));
        n = n.wrapping_add(1);
        assert_eq!(tb.get_u32("s").await, a as u32, "child A after x={x}");
        assert_eq!(tb.get_u32("t").await, b as u32, "child B after y={y}");
        assert_eq!(tb.get_u32("n").await, n as u32, "parent count");
        tb.set("go", 0u8);
        cycles = 0;
        while tb.get_u32("done").await != 0 {
            tb.clock(1).await;
            cycles += 1;
            assert!(cycles < PHASE_LIMIT, "acknowledge never fell");
        }
    }
}

#[tokio::test]
async fn bundled_nested_async_behavioural() {
    let tb = Testbench::behavioral_with_top(FIXTURE, "HsClusterTop")
        .await
        .expect("build behavioural testbench");
    run_cluster(tb).await;
}

#[tokio::test]
async fn bundled_nested_async_gate_level() {
    let tb = Testbench::gate_level_with_top(FIXTURE, "HsClusterTop")
        .await
        .expect("build gate-level testbench");
    run_cluster(tb).await;
}

/// Under `--async-state auto`, the blocks that can leave the clock network
/// when their registers become latches, least state first: not one without
/// state, nor one with a memory or coalesced children its strobe clocks
#[test]
fn latch_candidates_are_blocks_whose_strobe_would_clock_nothing() {
    let path = std::path::PathBuf::from(format!(
        "{}/tests/fixtures/ncl_latch_candidates.sk",
        env!("CARGO_MANIFEST_DIR")
    ));
    let ctx =
        skalp_frontend::parse_and_build_compilation_context_for_top(&path, None, "LcTop").unwrap();
    let mir = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .unwrap();
    let (hier, has_async) = skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir);
    assert!(has_async);

    let found: Vec<(String, u64)> =
        skalp_lir::latch_candidates(&hier, skalp_lir::AsyncState::PulseLatch)
            .into_iter()
            .map(|c| (c.path, c.latch_bits))
            .collect();
    // Each accumulator feeds its own state, so each bit is a pulsed latch
    // and a slave; so does each word of the register file (written in
    // place)
    let names: Vec<&str> = found
        .iter()
        .map(|(p, _)| p.rsplit('.').next().unwrap())
        .collect();
    assert_eq!(names, ["small", "wide", "rf"], "{found:?}");
    assert_eq!(found[0].1, 16, "{found:?}");
    assert_eq!(found[1].1, 32, "{found:?}");
    assert_eq!(found[2].1, 128, "{found:?}");

    assert!(skalp_lir::latch_candidates(&hier, skalp_lir::AsyncState::FlipFlop).is_empty());

    // Only the listed blocks change: the others keep flip-flops
    let small = found[0].0.clone();
    let latched = skalp_lir::apply_boundary_ncl_to_hierarchy(
        &hier,
        &skalp_lir::NclConfig {
            async_state: skalp_lir::AsyncState::PulseLatch,
            latch_blocks: Some([small.clone()].into_iter().collect()),
            ..Default::default()
        },
    );
    let has_latches = |path: &str| {
        latched.instances[path]
            .lir_result
            .lir
            .nodes
            .iter()
            .any(|n| matches!(n.op, skalp_lir::lir::LirOp::Latch { .. }))
    };
    assert!(has_latches(&small));
    assert!(!has_latches(&found[1].0));
}

/// Storage has no defined power-up value, so every async block under a
/// clocked parent takes a reset from it: the parent holds it high from
/// power-up (and its request low), the block's latches and flip-flops take
/// it to their start values
#[test]
fn async_blocks_reset_their_latches_and_flip_flops() {
    use skalp_lir::lir::LirOp;
    let path = std::path::PathBuf::from(format!(
        "{}/tests/fixtures/ncl_latch_candidates.sk",
        env!("CARGO_MANIFEST_DIR")
    ));
    let ctx =
        skalp_frontend::parse_and_build_compilation_context_for_top(&path, None, "LcTop").unwrap();
    let mir = skalp_mir::MirCompiler::new()
        .with_optimization_level(skalp_mir::OptimizationLevel::None)
        .compile_to_mir_with_modules(&ctx.main_hir, &ctx.module_hirs)
        .unwrap();
    let (hier, _) = skalp_lir::lower_mir_hierarchical_for_optimize_first(&mir);
    let candidates = skalp_lir::latch_candidates(&hier, skalp_lir::AsyncState::PulseLatch);
    let small = candidates[0].path.clone();
    let wide = candidates[1].path.clone();
    let expanded = skalp_lir::apply_boundary_ncl_to_hierarchy(
        &hier,
        &skalp_lir::NclConfig {
            async_state: skalp_lir::AsyncState::PulseLatch,
            latch_blocks: Some([small.clone()].into_iter().collect()),
            ..Default::default()
        },
    );

    // The parent drives each block's reset from a register that starts high
    let local = small.rsplit('.').next().unwrap();
    let parent = &expanded.instances[small.rsplit_once('.').map_or("top", |(p, _)| p)];
    let rst_name = format!("{local}__ncl_rst");
    let rst_signal = parent
        .lir_result
        .lir
        .get_signal(&rst_name)
        .unwrap_or_else(|| panic!("parent has no {rst_name}"));
    assert_eq!(rst_signal.initial, Some(1));
    assert!(matches!(
        expanded.instances[&small].port_connections.get(skalp_frontend::hir::NCL_RST_PORT),
        Some(skalp_lir::PortConnectionInfo::Signal(name)) if *name == rst_name
    ));

    // Every latch of the latched block takes the block's reset
    let lir = &expanded.instances[&small].lir_result.lir;
    let rst = lir
        .get_signal(skalp_frontend::hir::NCL_RST_PORT)
        .unwrap()
        .id;
    let latches: Vec<_> = lir
        .nodes
        .iter()
        .filter(|n| matches!(n.op, LirOp::Latch { .. }))
        .collect();
    assert!(!latches.is_empty());
    for latch in latches {
        assert_eq!(latch.inputs.get(2), Some(&rst), "{}", latch.path);
    }

    // The flip-flop block's registers reset asynchronously to their start
    // values
    let lir = &expanded.instances[&wide].lir_result.lir;
    let rst = lir
        .get_signal(skalp_frontend::hir::NCL_RST_PORT)
        .unwrap()
        .id;
    let go = lir.get_signal("__ncl_go").unwrap().id;
    let regs: Vec<_> = lir.nodes.iter().filter(|n| n.clock == Some(go)).collect();
    assert!(!regs.is_empty());
    for reg in regs {
        assert_eq!(reg.reset, Some(rst), "{}", reg.path);
        let LirOp::Reg {
            has_reset: true,
            async_reset: true,
            reset_value: Some(value),
            ..
        } = reg.op
        else {
            panic!("{} is not reset asynchronously: {:?}", reg.path, reg.op);
        };
        let start = lir.signals[reg.output.0 as usize].initial.unwrap_or(0);
        assert_eq!(value, start, "{}", reg.path);
    }
}
