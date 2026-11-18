//! Behavioural simulation of `+`/`-` widths and array indices, against the
//! language's width-preserving arithmetic and the gate-level flow:
//!
//! - `+`/`-` into a wider target kept the carry or borrow without the
//!   stdlib (both flows) but not with it, where `ops.sk` routes them through
//!   a width-preserving `std_adder`; the spec makes them width-preserving
//! - SystemVerilog sizes `+`/`-` by context, so the emitted `a - b` kept
//!   the borrow into a wider target or comparison; it is now size-cast
//! - the C++ model kept a carry/borrow bit on every `a + b` / `a - b`, so
//!   `a - b == 31`, `(a - b) >> 1` and `arr[a - b]` saw a 6-bit value for
//!   5-bit operands; `arr[wp - d]` with `wp < d` read past the array into the
//!   next signal's storage
//! - an array index past the last element read and wrote outside the array
//! - the gate-level flow lowered an index's arithmetic at the width of the
//!   port it feeds, so `r[a - b]` with `a < b` selected past bit 31

use skalp_testing::testbench::Testbench;

fn systemverilog(path: &str) -> String {
    let source = std::fs::read_to_string(path).expect("read fixture");
    let hir = skalp_frontend::parse_and_build_hir(&source).expect("HIR");
    let mut engine = skalp_frontend::monomorphization::MonomorphizationEngine::new();
    let hir = engine.monomorphize(&hir);
    let mir = skalp_mir::lower_to_mir(&hir).expect("MIR");
    skalp_codegen::generate_systemverilog_from_mir(&mir).expect("SystemVerilog")
}

fn fixture() -> String {
    format!(
        "{}/tests/fixtures/sim_width/sub_width.sk",
        env!("CARGO_MANIFEST_DIR")
    )
}

async fn check_sub_width(mut tb: Testbench, mode: &str) {
    for i in 0..32u32 {
        tb.set("wa", i as u8)
            .set("we", 1u8)
            .set("d", (0x400 | i) as u16);
        tb.clock(1).await;
    }
    tb.set("we", 0u8).set("r", 0x8000_0001u32);
    for (ra, wa) in [
        (0u32, 1u32),
        (3, 5),
        (10, 4),
        (20, 20),
        (31, 0),
        (17, 30),
        (31, 31),
    ] {
        tb.set("ra", ra as u8).set("wa", wa as u8);
        tb.step().await;
        let d = ra.wrapping_sub(wa) & 31;
        let s = (ra + wa) & 31;
        let want = [
            ("eq", (d == 31) as u32),
            ("cat", d << 1),
            ("shr", d >> 1),
            ("lt", (d < 16) as u32),
            ("ix", (0x8000_0001u32 >> d) & 1),
            ("q", 0x400 | d),
            ("q_next", !(0x400 | s) & 0x7ff),
            ("wide_sum", (ra + wa) & 31),
            ("wide_diff", d),
            ("inc", (ra + 1) & 31),
            ("wsum", ra + wa),
        ];
        for (name, w) in want {
            assert_eq!(tb.get_u32(name).await, w, "{mode}: {name}, ra={ra} wa={wa}");
        }
    }
}

#[tokio::test]
async fn sub_and_add_keep_their_width_behavioural() {
    let tb = Testbench::behavioral_with_top(&fixture(), "SubWidth")
        .await
        .expect("build");
    check_sub_width(tb, "behavioural").await;
}

#[tokio::test]
async fn sub_and_add_keep_their_width_gate_level() {
    let tb = Testbench::gate_level_with_top(&fixture(), "SubWidth")
        .await
        .expect("build");
    check_sub_width(tb, "gate level").await;
}

/// Writes every 5-bit address of a 24-deep array, then reads the first 24
/// back. A write past the end must not land in the array (by wrapping or
/// aliasing) or in its neighbour `g`. What a read past the end returns is not
/// specified: the flows differ (0 in behavioural, an element in gate level).
async fn check_odd_depth(mut tb: Testbench, mode: &str) {
    for i in 0..32u32 {
        tb.set("wa", i as u8)
            .set("we", 1u8)
            .set("d", (0x40 + i) as u8);
        tb.clock(1).await;
    }
    tb.set("we", 0u8);
    for i in 0..24u32 {
        tb.set("ra", i as u8);
        tb.step().await;
        assert_eq!(tb.get_u32("q").await, 0x40 + i, "{mode}: m[{i}]");
        assert_eq!(tb.get_u32("guard").await, 0, "{mode}: g[{i}] never written");
    }
}

#[tokio::test]
async fn index_past_the_end_stays_in_the_array() {
    let beh = Testbench::behavioral_with_top(&fixture(), "OddDepth")
        .await
        .expect("build");
    check_odd_depth(beh, "behavioural").await;
    let gate = Testbench::gate_level_with_top(&fixture(), "OddDepth")
        .await
        .expect("build");
    check_odd_depth(gate, "gate level").await;
}

/// `+`/`-` come out size-cast to their own width, so SystemVerilog's
/// context sizing cannot give them the carry; `+:` keeps its extra bit.
#[test]
fn systemverilog_add_and_sub_keep_their_width() {
    let sv = systemverilog(&fixture());
    for line in [
        "assign wide_diff = 5'(ra - wa);",
        "assign wide_sum = 5'(ra + wa);",
        "assign inc = 5'(ra + 1);",
    ] {
        assert!(sv.contains(line), "missing `{line}` in:\n{sv}");
    }
    assert!(
        sv.contains("((5'(ra - wa) == 31)"),
        "comparison operand cast:\n{sv}"
    );
    let wsum = sv
        .lines()
        .find(|l| l.contains("assign wsum"))
        .expect("wsum");
    assert!(!wsum.contains("5'("), "`+:` is not cut to 5 bits: {wsum}");
}
