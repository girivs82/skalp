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
