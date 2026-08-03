//! Regression tests for triage #32: behavioral C++ backend arithmetic on
//! values wider than 64 bits.
//!
//! 65-128-bit signals used to lower to `uint32_t[N]` arrays in the generated
//! C++, and any arithmetic/shift/compare on them emitted invalid C++ —
//! the simulation library failed to compile. They now lower to native
//! `unsigned __int128`, so ordinary expression emission stays valid.
//! (>128-bit values still use arrays and remain unsupported for arithmetic.)

use skalp_testing::testbench::*;

/// 80-bit concat, shift, and add — computed against u128 reference math.
#[tokio::test]
async fn test_triage32_wide_concat_shift_add() {
    let mut tb = Testbench::new("tests/fixtures/wide_ops.sk").await.unwrap();

    let a: u64 = 0xDEAD_BEEF_CAFE_F00D;
    let b: u64 = 0x0123_4567_89AB_CDEF;
    let sum: u128 = ((a as u128) << 16) + (b as u128);

    tb.set("a", a).set("b", b);
    tb.clock(1).await;
    tb.expect("lo", sum as u64).await;
    tb.expect("hi", ((sum >> 64) as u64) & 0xFFFF).await;

    // Second vector: carry propagation into the high bits
    let a2: u64 = u64::MAX;
    let b2: u64 = u64::MAX;
    let sum2: u128 = ((a2 as u128) << 16) + (b2 as u128);
    tb.set("a", a2).set("b", b2);
    tb.clock(1).await;
    tb.expect("lo", sum2 as u64).await;
    tb.expect("hi", ((sum2 >> 64) as u64) & 0xFFFF).await;
}

/// 32-bit multiply through the stdlib shift-add chain (65-bit internal adds)
/// — the exact shape that failed to compile before the fix.
#[tokio::test]
async fn test_triage32_mul32_through_stdlib_chain() {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        format!("{}/crates/skalp-stdlib", manifest_dir),
    );
    let mut tb = Testbench::new("tests/fixtures/wide_mul32.sk")
        .await
        .unwrap();

    tb.set("a", 6u32).set("b", 7u32);
    tb.clock(1).await;
    tb.expect("p", 42u32).await;

    tb.set("a", 0xFFFF_FFFFu32).set("b", 3u32);
    tb.clock(1).await;
    tb.expect("p", 0xFFFF_FFFDu32).await; // wraps to 32 bits
}
