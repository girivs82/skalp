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
