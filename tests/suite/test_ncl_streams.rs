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

/// With a LUT library each block's step request is a single LUT latch
/// (its output fed back into it); the blocks must step and stop through it
/// just as through the gate-built latch
#[tokio::test]
async fn streams_gate_level_lut_latch() {
    let tb = Testbench::gate_level_with_top_and_library(FIXTURE, "StTop", "nexus")
        .await
        .expect("build gate-level testbench");
    run(tb, 40).await;
}

#[tokio::test]
async fn streams_merge_gate_level_lut_latch() {
    let tb = Testbench::gate_level_with_top_and_library(FIXTURE, "StMergeTop", "nexus")
        .await
        .expect("build gate-level testbench");
    run_merge(tb, 60).await;
}

const BAD_CONDITION: &str = "tests/fixtures/ncl_streams_bad_condition.sk";

/// The error building `top` from the bad-condition fixture at gate level
async fn condition_error(top: &str) -> String {
    match Testbench::gate_level_with_top(BAD_CONDITION, top).await {
        Ok(_) => panic!("{top}: built, but its send/recv conditions read an input"),
        Err(e) => format!("{e:#}"),
    }
}

#[tokio::test]
async fn step_condition_on_a_parent_input_is_refused() {
    let error = condition_error("BcGatedTop").await;
    assert!(error.contains("BcGated"), "{error}");
    assert!(error.contains("`enable`"), "{error}");
}

#[tokio::test]
async fn step_condition_on_received_data_is_refused() {
    let error = condition_error("BcFilterTop").await;
    assert!(error.contains("BcFilter"), "{error}");
    assert!(error.contains("from outside the block"), "{error}");
    // the blocks whose conditions are their own registers are fine
    assert!(!error.contains("BcSink"), "{error}");
    assert!(!error.contains("BcProducer"), "{error}");
}

/// Stream warnings for `source` (the deadlock check)
fn stream_warnings(source: &str) -> Vec<String> {
    let mut hir = skalp_frontend::parse_and_build_hir(source).expect("parse");
    let lowering = skalp_frontend::ncl_streams::lower_async_streams(&mut hir, &[]);
    assert!(lowering.errors.is_empty(), "{:?}", lowering.errors);
    lowering.warnings
}

/// A relay that receives, then sends what it received, in every step
const RELAY: &str = "
async entity Relay {
    in i: stream<bit[8]>
    out o: stream<bit[8]>
}
impl Relay {
    on() {
        let v: bit[8] = i.recv()
        o.send(v + 1)
    }
}
";

#[test]
fn a_ring_of_relays_warns_of_deadlock() {
    let source = format!(
        "{RELAY}
entity Ring {{
    in clk: clock
    in run: bit
}}
impl Ring {{
    signal req: bit = 0
    inst a = Relay {{ i: c.o, ncl_req: req }}
    inst b = Relay {{ i: a.o, ncl_req: req }}
    inst c = Relay {{ i: b.o, ncl_req: req }}
    on(clk.rise) {{
        req = run
    }}
}}
"
    );
    let warnings = stream_warnings(&source);
    assert_eq!(warnings.len(), 1, "{warnings:?}");
    let w = &warnings[0];
    assert!(w.contains("deadlock"), "{w}");
    for channel in ["a.o -> b.i", "b.o -> c.i", "c.o -> a.i"] {
        assert!(w.contains(channel), "{w}");
    }
}

#[test]
fn a_ring_with_a_block_that_sends_first_does_not_warn() {
    // The starter sends on its own steps (when `primed` is clear) without
    // receiving, then receives what comes back round
    let source = format!(
        "{RELAY}
async entity Starter {{
    in i: stream<bit[8]>
    out o: stream<bit[8]>
}}
impl Starter {{
    signal primed: bit = 0
    on() {{
        if primed == 0 {{
            o.send(8'h01)
            primed = 1
        }} else {{
            let v: bit[8] = i.recv()
            primed = 0
        }}
    }}
}}
entity Ring {{
    in clk: clock
    in run: bit
}}
impl Ring {{
    signal req: bit = 0
    inst a = Starter {{ i: b.o, ncl_req: req }}
    inst b = Relay {{ i: a.o, ncl_req: req }}
    on(clk.rise) {{
        req = run
    }}
}}
"
    );
    assert!(stream_warnings(&source).is_empty());
}

#[test]
fn a_pipeline_does_not_warn() {
    // Producer to consumer (the fixture's design): no cycle
    let source = std::fs::read_to_string(FIXTURE).expect("fixture");
    assert!(stream_warnings(&source).is_empty());
}

#[tokio::test]
async fn step_condition_on_a_coalesced_child_register_is_allowed() {
    let mut tb =
        Testbench::gate_level_with_top("tests/fixtures/ncl_streams_child_condition.sk", "CcTop")
            .await
            .expect("a condition on a coalesced child's register builds");
    tb.set("run", 1u8);
    tb.clock(40).await;
    assert!(tb.get_u32("received").await > 0, "nothing received");
}
