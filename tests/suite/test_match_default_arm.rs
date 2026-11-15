//! A sequential `match` lowered its default arm (`_`) to the arm's last
//! assignment of each target, unconditionally: every `if` inside the default
//! arm was dropped. A state machine that waits in its default arm for an
//! input left after one clock, and the input never reached the gates (found
//! as an ADC sequencer that never saw end-of-conversion on an FPGA).

use skalp_testing::Testbench;

fn fixture_path(name: &str) -> String {
    format!("{}/tests/fixtures/{}", env!("CARGO_MANIFEST_DIR"), name)
}

async fn check(mut tb: Testbench, level: &str) {
    // 0 -> 1 -> 2, then waits in the default arm while `go` is low
    tb.set("go", 0u64);
    tb.clock(10).await;
    assert_eq!(
        tb.get_u64("phase").await,
        2,
        "{level}: must wait in phase 2"
    );
    assert_eq!(
        tb.get_u64("done").await,
        0,
        "{level}: nothing done without go"
    );
    assert_eq!(
        tb.get_u64("waited").await,
        8,
        "{level}: 8 clocks in phase 2"
    );

    // go: one completion, back to phase 0
    tb.set("go", 1u64);
    tb.clock(1).await;
    assert_eq!(tb.get_u64("done").await, 1, "{level}: go completes once");
    assert_eq!(tb.get_u64("phase").await, 0, "{level}: back to phase 0");

    // the else branch's nested if: give up after 200 clocks without go
    tb.set("go", 0u64);
    tb.clock(2 + 200).await;
    assert_eq!(
        tb.get_u64("phase").await,
        2,
        "{level}: still waiting at 199"
    );
    tb.clock(1).await;
    assert_eq!(tb.get_u64("phase").await, 0, "{level}: gives up at 200");
    assert_eq!(
        tb.get_u64("done").await,
        1,
        "{level}: a timeout is not a completion"
    );
}

#[tokio::test]
async fn test_match_default_arm_if_behavioral() {
    let tb = Testbench::behavioral(&fixture_path("match_default_arm_if.sk"))
        .await
        .unwrap();
    check(tb, "behavioral").await;
}

#[tokio::test]
async fn test_match_default_arm_if_gate_level() {
    let tb = Testbench::gate_level(&fixture_path("match_default_arm_if.sk"))
        .await
        .unwrap();
    check(tb, "gate level").await;
}
