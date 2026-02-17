//! Tests for the Partitionable Mantissa Adder
//!
//! Verifies correct operation in all three modes:
//! - Single (FP32): 1× 28-bit addition
//! - Dual (FP16×2): 2× 14-bit independent additions
//! - Quad (FP8×4): 4× 7-bit independent additions

use skalp_testing::Testbench;

/// Set up stdlib path for tests
fn setup_stdlib_path() {
    std::env::set_var(
        "SKALP_STDLIB_PATH",
        concat!(env!("CARGO_MANIFEST_DIR"), "/crates/skalp-stdlib"),
    );
}

fn fixture_path(name: &str) -> String {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    format!("{}/tests/fixtures/{}", manifest_dir, name)
}

/// Mode constants matching AdderMode enum
const MODE_SINGLE: u64 = 0; // FP32×1
const MODE_DUAL: u64 = 1; // FP16×2
const MODE_QUAD: u64 = 2; // FP8×4

#[tokio::test]
async fn test_single_mode_basic() {
    setup_stdlib_path();

    let mut tb = Testbench::with_top_module(&fixture_path("partitionable_adder_test.sk"), "TestPartitionableAdder")
        .await
        .expect("Failed to create testbench");

    // Test: 0x1000000 + 0x2000000 = 0x3000000 (basic addition)
    tb.set("a", 0x1000000u64)
        .set("b", 0x2000000u64)
        .set("cin", 0u64)
        .set("mode", MODE_SINGLE);
    tb.step().await;

    let sum: u64 = tb.get_as("sum").await;
    let cout: u64 = tb.get_as("cout").await;
    println!(
        "Single mode: 0x{:07X} + 0x{:07X} = 0x{:07X} (cout=0x{:X})",
        0x1000000u32, 0x2000000u32, sum, cout
    );
    assert_eq!(sum, 0x3000000, "Single mode basic addition failed");

    // Test with carry propagation across all sections (no overflow)
    // 0x0FFFFFF has only 24 bits of 1s, so adding 1 gives 0x1000000 (25 bits, fits in 28)
    tb.set("a", 0x0FFFFFF_u64)
        .set("b", 0x0000001_u64)
        .set("cin", 0u64)
        .set("mode", MODE_SINGLE);
    tb.step().await;

    let sum: u64 = tb.get_as("sum").await;
    let cout: u64 = tb.get_as("cout").await;
    println!(
        "Single mode carry propagation: 0x{:07X} + 0x{:07X} = 0x{:07X} (cout=0x{:X})",
        0x0FFFFFFu32, 0x0000001u32, sum, cout
    );
    // 0x0FFFFFF + 1 = 0x1000000 (25 bits, no overflow since we have 28 bits)
    assert_eq!(sum, 0x1000000, "Single mode carry propagation failed");
    assert_eq!(cout & 0x8, 0, "Single mode should have no final carry out");

    // Test with true 28-bit overflow
    // 0xFFFFFFF has all 28 bits set to 1
    tb.set("a", 0xFFFFFFF_u64) // All 1s in lower 28 bits
        .set("b", 0x0000001_u64)
        .set("cin", 0u64)
        .set("mode", MODE_SINGLE);
    tb.step().await;

    let sum: u64 = tb.get_as("sum").await;
    let cout: u64 = tb.get_as("cout").await;
    println!(
        "Single mode overflow: 0x{:07X} + 0x{:07X} = 0x{:07X} (cout=0x{:X})",
        0xFFFFFFFu32, 0x0000001u32, sum, cout
    );
    // 0xFFFFFFF + 1 = 0x10000000, but we only have 28 bits, so result is 0 with cout[3]=1
    assert_eq!(sum & 0x0FFFFFFF, 0, "Single mode overflow sum should be 0");
    assert_eq!(cout & 0x8, 0x8, "Single mode should have final carry out");

    println!("Single mode tests passed!");
    tb.export_waveform("build/test_single_mode_basic.skw.gz").ok();
}

#[tokio::test]
async fn test_dual_mode_independent() {
    setup_stdlib_path();

    let mut tb = Testbench::with_top_module(&fixture_path("partitionable_adder_test.sk"), "TestPartitionableAdder")
        .await
        .expect("Failed to create testbench");

    // In dual mode, we have two independent 14-bit adders:
    // - Low:  bits [13:0]
    // - High: bits [27:14]

    // Test: Low=0x1000+0x2000=0x3000, High=0x0100+0x0200=0x0300
    // a = {0x0100, 0x1000} = 0x0400000 + 0x1000 = 0x0401000
    // b = {0x0200, 0x2000} = 0x0800000 + 0x2000 = 0x0802000
    let a_lo: u64 = 0x1000;
    let a_hi: u64 = 0x0100;
    let b_lo: u64 = 0x2000;
    let b_hi: u64 = 0x0200;

    let a = (a_hi << 14) | a_lo;
    let b = (b_hi << 14) | b_lo;

    tb.set("a", a)
        .set("b", b)
        .set("cin", 0u64)
        .set("mode", MODE_DUAL);
    tb.step().await;

    let sum: u64 = tb.get_as("sum").await;
    let sum_lo = sum & 0x3FFF;
    let sum_hi = (sum >> 14) & 0x3FFF;

    println!(
        "Dual mode: lo(0x{:04X}+0x{:04X})=0x{:04X}, hi(0x{:04X}+0x{:04X})=0x{:04X}",
        a_lo, b_lo, sum_lo, a_hi, b_hi, sum_hi
    );

    assert_eq!(sum_lo, 0x3000, "Dual mode low half addition failed");
    assert_eq!(sum_hi, 0x0300, "Dual mode high half addition failed");

    // Test independence: overflow in low half should NOT affect high half
    // Low: 0x3FFF + 1 = 0x4000 (overflow in 14 bits → 0 with carry)
    // High: 0x0001 + 0x0001 = 0x0002 (should NOT be affected by low carry)
    let a_lo: u64 = 0x3FFF;
    let a_hi: u64 = 0x0001;
    let b_lo: u64 = 0x0001;
    let b_hi: u64 = 0x0001;

    let a = (a_hi << 14) | a_lo;
    let b = (b_hi << 14) | b_lo;

    tb.set("a", a)
        .set("b", b)
        .set("cin", 0u64)
        .set("mode", MODE_DUAL);
    tb.step().await;

    let sum: u64 = tb.get_as("sum").await;
    let cout: u64 = tb.get_as("cout").await;
    let sum_lo = sum & 0x3FFF;
    let sum_hi = (sum >> 14) & 0x3FFF;

    println!(
        "Dual mode independence: lo(0x{:04X}+0x{:04X})=0x{:04X}, hi(0x{:04X}+0x{:04X})=0x{:04X}, cout=0x{:X}",
        0x3FFFu32, 0x0001u32, sum_lo, 0x0001u32, 0x0001u32, sum_hi, cout
    );

    // Low should overflow: 0x3FFF + 1 = 0x4000 → 0x0000 with carry
    assert_eq!(sum_lo, 0x0000, "Dual mode low overflow should wrap to 0");
    // High should be independent: 1 + 1 = 2 (NOT 3 from carry)
    assert_eq!(
        sum_hi, 0x0002,
        "Dual mode high should be independent of low carry"
    );

    println!("Dual mode tests passed!");
    tb.export_waveform("build/test_dual_mode_independent.skw.gz").ok();
}

#[tokio::test]
async fn test_quad_mode_independent() {
    setup_stdlib_path();

    let mut tb = Testbench::with_top_module(&fixture_path("partitionable_adder_test.sk"), "TestPartitionableAdder")
        .await
        .expect("Failed to create testbench");

    // In quad mode, we have four independent 7-bit adders:
    // - sec0: bits [6:0]
    // - sec1: bits [13:7]
    // - sec2: bits [20:14]
    // - sec3: bits [27:21]

    // Test: Each section does 0x10 + 0x20 = 0x30
    let a = (0x10u64 << 21) | (0x10u64 << 14) | (0x10u64 << 7) | 0x10u64;
    let b = (0x20u64 << 21) | (0x20u64 << 14) | (0x20u64 << 7) | 0x20u64;

    tb.set("a", a)
        .set("b", b)
        .set("cin", 0u64)
        .set("mode", MODE_QUAD);
    tb.step().await;

    let sum: u64 = tb.get_as("sum").await;
    let s0 = sum & 0x7F;
    let s1 = (sum >> 7) & 0x7F;
    let s2 = (sum >> 14) & 0x7F;
    let s3 = (sum >> 21) & 0x7F;

    println!(
        "Quad mode: s0=0x{:02X}, s1=0x{:02X}, s2=0x{:02X}, s3=0x{:02X}",
        s0, s1, s2, s3
    );

    assert_eq!(s0, 0x30, "Quad mode section 0 failed");
    assert_eq!(s1, 0x30, "Quad mode section 1 failed");
    assert_eq!(s2, 0x30, "Quad mode section 2 failed");
    assert_eq!(s3, 0x30, "Quad mode section 3 failed");

    // Test independence: overflow in section 0 should NOT affect section 1
    // sec0: 0x7F + 1 = 0x80 → 0x00 with carry (but carry should be cut)
    // sec1: 0x01 + 0x01 = 0x02 (should NOT be affected)
    let a = (0x01u64 << 21) | (0x01u64 << 14) | (0x01u64 << 7) | 0x7Fu64;
    let b = (0x01u64 << 21) | (0x01u64 << 14) | (0x01u64 << 7) | 0x01u64;

    tb.set("a", a)
        .set("b", b)
        .set("cin", 0u64)
        .set("mode", MODE_QUAD);
    tb.step().await;

    let sum: u64 = tb.get_as("sum").await;
    let cout: u64 = tb.get_as("cout").await;
    let s0 = sum & 0x7F;
    let s1 = (sum >> 7) & 0x7F;
    let s2 = (sum >> 14) & 0x7F;
    let s3 = (sum >> 21) & 0x7F;

    println!(
        "Quad mode independence: s0=0x{:02X}, s1=0x{:02X}, s2=0x{:02X}, s3=0x{:02X}, cout=0x{:X}",
        s0, s1, s2, s3, cout
    );

    assert_eq!(s0, 0x00, "Quad mode section 0 overflow should wrap to 0");
    assert_eq!(
        s1, 0x02,
        "Quad mode section 1 should be independent (1+1=2, not 3)"
    );
    assert_eq!(s2, 0x02, "Quad mode section 2 should be 1+1=2");
    assert_eq!(s3, 0x02, "Quad mode section 3 should be 1+1=2");
    assert_eq!(cout & 0x1, 0x1, "Quad mode section 0 should have carry out");

    println!("Quad mode tests passed!");
    tb.export_waveform("build/test_quad_mode_independent.skw.gz").ok();
}

#[tokio::test]
async fn test_mode_switching() {
    setup_stdlib_path();

    let mut tb = Testbench::with_top_module(&fixture_path("partitionable_adder_test.sk"), "TestPartitionableAdder")
        .await
        .expect("Failed to create testbench");

    // Use inputs that will trigger overflow in section 1 to test carry cut behavior
    // We need section 1 to overflow (produce a carry) so that single vs quad mode differs
    // Section 1 needs a1 + b1 >= 128 to overflow
    // Let's use: a1 = 0x7F (127), b1 = 0x01 (1) -> 128 overflows!
    // a = (0x01 << 21) | (0x01 << 14) | (0x7F << 7) | 0x01 = section values: 0x01, 0x7F, 0x01, 0x01
    // b = (0x01 << 21) | (0x01 << 14) | (0x01 << 7) | 0x01 = section values: 0x01, 0x01, 0x01, 0x01
    let a: u64 = (0x01 << 21) | (0x01 << 14) | (0x7F << 7) | 0x01; // 0x02027F81
    let b: u64 = (0x01 << 21) | (0x01 << 14) | (0x01 << 7) | 0x01; // 0x02020281

    // Single mode: full 28-bit addition
    tb.set("a", a)
        .set("b", b)
        .set("cin", 0u64)
        .set("mode", MODE_SINGLE);
    tb.step().await;
    let sum_single: u64 = tb.get_as("sum").await;

    // Quad mode: four independent 7-bit additions
    tb.set("a", a)
        .set("b", b)
        .set("cin", 0u64)
        .set("mode", MODE_QUAD);
    tb.step().await;
    let sum_quad: u64 = tb.get_as("sum").await;

    // Debug: print section-by-section breakdown
    let single_s0 = sum_single & 0x7F;
    let single_s1 = (sum_single >> 7) & 0x7F;
    let single_s2 = (sum_single >> 14) & 0x7F;
    let single_s3 = (sum_single >> 21) & 0x7F;
    let quad_s0 = sum_quad & 0x7F;
    let quad_s1 = (sum_quad >> 7) & 0x7F;
    let quad_s2 = (sum_quad >> 14) & 0x7F;
    let quad_s3 = (sum_quad >> 21) & 0x7F;

    println!(
        "Mode comparison: single=0x{:07X}, quad=0x{:07X}",
        sum_single, sum_quad
    );
    println!(
        "  Single sections: s0=0x{:02X}, s1=0x{:02X}, s2=0x{:02X}, s3=0x{:02X}",
        single_s0, single_s1, single_s2, single_s3
    );
    println!(
        "  Quad sections: s0=0x{:02X}, s1=0x{:02X}, s2=0x{:02X}, s3=0x{:02X}",
        quad_s0, quad_s1, quad_s2, quad_s3
    );
    // Input sections for reference:
    println!(
        "  Input a: s0=0x{:02X}, s1=0x{:02X}, s2=0x{:02X}, s3=0x{:02X}",
        a & 0x7F,
        (a >> 7) & 0x7F,
        (a >> 14) & 0x7F,
        (a >> 21) & 0x7F
    );
    println!(
        "  Input b: s0=0x{:02X}, s1=0x{:02X}, s2=0x{:02X}, s3=0x{:02X}",
        b & 0x7F,
        (b >> 7) & 0x7F,
        (b >> 14) & 0x7F,
        (b >> 21) & 0x7F
    );

    // Section breakdown:
    // sec0: a0=0x01 + b0=0x01 = 0x02, no carry
    // sec1: a1=0x7F + b1=0x01 = 0x80 -> overflow! sum1=0x00, cout1=1
    // sec2: a2=0x01 + b2=0x01 + cin2 =
    //       Single mode: cin2 = cout1 = 1, sum2 = 0x03
    //       Quad mode: cin2 = cin[2] = 0, sum2 = 0x02
    // sec3: a3=0x01 + b3=0x01 + cin3 = 0x02 (no carry from sec2 in either mode)
    //
    // Expected:
    //   Single: sum = {0x02, 0x03, 0x00, 0x02} = 0x0406002
    //   Quad:   sum = {0x02, 0x02, 0x00, 0x02} = 0x0404002

    assert_ne!(
        sum_single, sum_quad,
        "Single and quad modes should produce different results due to carry propagation. \
         Section 1 overflows (0x7F+0x01=0x80), carry should propagate to section 2 in single mode only."
    );

    println!("Mode switching tests passed!");
    tb.export_waveform("build/test_mode_switching.skw.gz").ok();
}
