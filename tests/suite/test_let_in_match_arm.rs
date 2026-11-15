//! `let` bindings inside `match` arms of a clocked block, at the top and in
//! a child instance, behavioural and gate level.
//!
//! - At the top, the arm's binding was registered like state, so the arm's
//!   register write read a flip-flop the combinational pass never evaluated:
//!   0 in behavioural simulation.
//! - In a child, a name bound in two arms had no single binding, so nothing
//!   drove it and it read as 0.

use skalp_testing::testbench::Testbench;

const FIXTURE: &str = "tests/fixtures/let_in_match_arm.sk";

async fn check(mut tb: Testbench) {
    tb.set("a", 0x10u8).set("b", 0x20u8);
    // (sel, y): a + 1, (b + 2) ^ 0x0F, a + b, 0xFF
    for (sel, expected) in [(0u8, 0x11u32), (1, 0x2D), (2, 0x30), (3, 0xFF), (0, 0x11)] {
        tb.set("sel", sel);
        tb.clock(2).await;
        assert_eq!(tb.get_u32("y").await, expected, "sel = {sel}");
    }
}

#[tokio::test]
async fn match_arm_lets_behavioural() {
    check(
        Testbench::behavioral_with_top(FIXTURE, "ArmLet")
            .await
            .expect("build"),
    )
    .await;
}

#[tokio::test]
async fn match_arm_lets_in_a_child_behavioural() {
    check(
        Testbench::behavioral_with_top(FIXTURE, "ArmLetTop")
            .await
            .expect("build"),
    )
    .await;
}

#[tokio::test]
async fn match_arm_lets_gate_level() {
    check(
        Testbench::gate_level_with_top(FIXTURE, "ArmLet")
            .await
            .expect("build"),
    )
    .await;
}

#[tokio::test]
async fn match_arm_lets_in_a_child_gate_level() {
    check(
        Testbench::gate_level_with_top(FIXTURE, "ArmLetTop")
            .await
            .expect("build"),
    )
    .await;
}
