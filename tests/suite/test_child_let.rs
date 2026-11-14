//! `let` bindings inside a child instance's clocked block reach the
//! registers that read them (they used to read 0 in behavioural simulation)

use skalp_testing::testbench::Testbench;

const FIXTURE: &str = "tests/fixtures/child_let.sk";

async fn run(mut tb: Testbench) {
    let mut total: u8 = 0;
    let mut odd: u8 = 0;
    for x in [3u8, 4, 7, 10, 255] {
        tb.set("x", x);
        tb.clock(1).await;
        total = total.wrapping_add(x.wrapping_add(1));
        if x & 1 == 1 {
            odd = odd.wrapping_add(x);
        }
    }
    tb.clock(1).await;
    // one more clock with the last input
    total = total.wrapping_add(0u8.wrapping_add(255).wrapping_add(1));
    odd = odd.wrapping_add(255);
    assert_eq!(tb.get_u32("total").await, total as u32);
    assert_eq!(tb.get_u32("odd_total").await, odd as u32);
}

#[tokio::test]
async fn child_let_behavioural() {
    let tb = Testbench::behavioral_with_top(FIXTURE, "ClTop")
        .await
        .expect("build behavioural testbench");
    run(tb).await;
}

#[tokio::test]
async fn child_let_gate_level() {
    let tb = Testbench::gate_level_with_top(FIXTURE, "ClTop")
        .await
        .expect("build gate-level testbench");
    run(tb).await;
}
