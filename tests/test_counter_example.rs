/// Example testbench demonstrating SKALP's current testbench API
///
/// This shows how to test hardware designs using Rust with the Testbench API
/// from skalp-testing. See examples/testbench_guide/README.md for full guide.
use skalp_testing::testbench::*;

#[tokio::test]
async fn test_counter_basic() {
    let mut tb = Testbench::new("examples/testbench_guide/counter.sk")
        .await
        .unwrap();

    // Test 1: Reset behavior
    tb.set("rst", 1u8).set("enable", 0u8);
    tb.clock(2).await;
    tb.expect("count", 0u8).await;

    // Test 2: Count with enable
    tb.set("rst", 0u8).set("enable", 1u8);
    tb.clock(1).await;
    tb.expect("count", 1u8).await;

    tb.clock(1).await;
    tb.expect("count", 2u8).await;

    tb.clock(5).await;
    tb.expect("count", 7u8).await;

    // Test 3: Disable stops counting
    tb.set("enable", 0u8);
    tb.clock(3).await;
    tb.expect("count", 7u8).await; // Still 7

    // Test 4: Re-enable continues
    tb.set("enable", 1u8);
    tb.clock(1).await;
    tb.expect("count", 8u8).await;

    tb.export_waveform("build/test_counter_basic.skw.gz").ok();
}

#[tokio::test]
async fn test_counter_reset_during_count() {
    let mut tb = Testbench::new("examples/testbench_guide/counter.sk")
        .await
        .unwrap();

    // Count to 5
    tb.set("rst", 0u8).set("enable", 1u8);
    tb.clock(5).await;
    tb.expect("count", 5u8).await;

    // Assert reset mid-count
    tb.set("rst", 1u8);
    tb.clock(1).await;
    tb.expect("count", 0u8).await;

    // Release reset and verify counting resumes from 0
    tb.set("rst", 0u8);
    tb.clock(1).await;
    tb.expect("count", 1u8).await;

    tb.export_waveform("build/test_counter_reset_during_count.skw.gz").ok();
}

#[tokio::test]
async fn test_counter_overflow() {
    let mut tb = Testbench::new("examples/testbench_guide/counter.sk")
        .await
        .unwrap();

    // Reset and enable
    tb.set("rst", 1u8).set("enable", 0u8);
    tb.clock(2).await;
    tb.set("rst", 0u8).set("enable", 1u8);

    // Count to near overflow (254)
    tb.clock(254).await;
    tb.expect("count", 254u8).await;

    // One more cycle should give 255
    tb.clock(1).await;
    tb.expect("count", 255u8).await;

    // Counter should wrap to 0
    tb.clock(1).await;
    tb.expect("count", 0u8).await;

    // And continue from 1
    tb.clock(1).await;
    tb.expect("count", 1u8).await;

    tb.export_waveform("build/test_counter_overflow.skw.gz").ok();
}

#[tokio::test]
async fn test_counter_enable_control() {
    let mut tb = Testbench::new("examples/testbench_guide/counter.sk")
        .await
        .unwrap();

    // Start counting
    tb.set("rst", 0u8).set("enable", 1u8);
    tb.clock(3).await;
    tb.expect("count", 3u8).await;

    // Toggle enable off and on
    tb.set("enable", 0u8);
    tb.clock(2).await;
    tb.expect("count", 3u8).await; // Frozen at 3

    tb.set("enable", 1u8);
    tb.clock(1).await;
    tb.expect("count", 4u8).await; // Resumes

    tb.set("enable", 0u8);
    tb.clock(5).await;
    tb.expect("count", 4u8).await; // Frozen at 4

    tb.set("enable", 1u8);
    tb.clock(2).await;
    tb.expect("count", 6u8).await; // Counts to 6

    tb.export_waveform("build/test_counter_enable_control.skw.gz").ok();
}
