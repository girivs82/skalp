//! Self-timed async blocks connected by `stream<T>` channels, started and
//! stopped by a clocked parent, simulated behaviourally and at gate level.

use skalp_testing::testbench::Testbench;

const FIXTURE: &str = "tests/fixtures/ncl_streams.sk";

/// Most clock cycles the blocks may take to stop
const STOP_LIMIT: usize = 64;

/// Run the producer and consumer for `cycles` clocks, stop them and check
/// that every value sent was received once, in order
async fn run(mut tb: Testbench, cycles: usize) {
    tb.set("run", 0u8);
    tb.clock(4).await;
    assert_eq!(tb.get_u32("busy").await, 0, "busy at rest");
    assert_eq!(tb.get_u32("received").await, 0);

    tb.set("run", 1u8);
    tb.clock(cycles).await;
    tb.set("run", 0u8);
    let mut waited = 0;
    while tb.get_u32("busy").await != 0 {
        tb.clock(1).await;
        waited += 1;
        assert!(waited < STOP_LIMIT, "blocks never stopped");
    }
    tb.clock(2).await;

    let sent = tb.get_u32("sent").await;
    let received = tb.get_u32("received").await;
    let sum = tb.get_u32("sum").await;
    assert!(received > 0, "nothing received in {cycles} clocks");
    // the channel holds at most one value the consumer has not taken yet
    assert!(
        sent == received || sent == received + 1,
        "sent {sent}, received {received}"
    );
    // values 0, 1, 2, ... (8 bits) received in order, each once
    let expected: u32 = (0..received).map(|k| k & 0xff).sum::<u32>() & 0xffff;
    assert_eq!(sum, expected, "sum of the first {received} values");
}

#[tokio::test]
async fn streams_behavioural() {
    let tb = Testbench::behavioral_with_top(FIXTURE, "StTop")
        .await
        .expect("build behavioural testbench");
    run(tb, 40).await;
}

#[tokio::test]
async fn streams_gate_level() {
    let tb = Testbench::gate_level_with_top(FIXTURE, "StTop")
        .await
        .expect("build gate-level testbench");
    run(tb, 40).await;
}

/// Run two producers into the merging consumer for `cycles` clocks, stop
/// them, and check each stream's values arrived in order, once each
async fn run_merge(mut tb: Testbench, cycles: usize) {
    tb.set("run", 0u8);
    tb.clock(4).await;
    assert_eq!(tb.get_u32("busy").await, 0, "busy at rest");

    tb.set("run", 1u8);
    tb.clock(cycles).await;
    tb.set("run", 0u8);
    let mut waited = 0;
    while tb.get_u32("busy").await != 0 {
        tb.clock(1).await;
        waited += 1;
        assert!(waited < STOP_LIMIT, "blocks never stopped");
    }
    tb.clock(2).await;

    let mut total = 0;
    let mut counts = Vec::new();
    for (sent, n, sum) in [("sent_a", "na", "suma"), ("sent_b", "nb", "sumb")] {
        let sent = tb.get_u32(sent).await;
        let n = tb.get_u32(n).await;
        let sum_got = tb.get_u32(sum).await;
        assert!(n > 0, "{sum}: nothing received");
        assert!(
            sent == n || sent == n + 1,
            "{sum}: sent {sent}, received {n}"
        );
        let expected: u32 = (0..n).map(|k| k & 0xff).sum::<u32>() & 0xffff;
        assert_eq!(sum_got, expected, "{sum} after {n} values");
        total += n;
        counts.push(n);
    }
    // Both producers keep up with the consumer, so the arbiter sees both
    // requests waiting at every decision: a fair arbiter serves them in
    // turn, an unfair one lets one side through only now and then
    let (lo, hi) = (counts[0].min(counts[1]), counts[0].max(counts[1]));
    assert!(lo * 2 >= hi, "unfair arbitration: {counts:?}");
    // The `_ if` arm only wins when no value is waiting (the producers
    // here are usually faster), and never runs twice in a row
    let extra = tb.get_u32("extra").await;
    assert!(extra <= total, "{extra} extra steps after {total} values");
}

#[tokio::test]
async fn streams_merge_behavioural() {
    let tb = Testbench::behavioral_with_top(FIXTURE, "StMergeTop")
        .await
        .expect("build behavioural testbench");
    run_merge(tb, 60).await;
}

#[tokio::test]
async fn streams_merge_gate_level() {
    let tb = Testbench::gate_level_with_top(FIXTURE, "StMergeTop")
        .await
        .expect("build gate-level testbench");
    run_merge(tb, 60).await;
}
