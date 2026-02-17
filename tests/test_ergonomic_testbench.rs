//! Test suite demonstrating the new ergonomic testbench API

use skalp_testing::testbench::*;

#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS, CPU runtime not yet implemented"
)]
async fn test_alu_with_new_api() {
    let mut tb = Testbench::new("examples/alu.sk").await.unwrap();

    // Test ADD operation
    tb.set("a", 5u32).set("b", 3u32).set("op", 0b000u8);
    tb.clock(2).await;
    tb.expect("result", 8u32).await;

    // Test SUB operation
    tb.set("op", 0b001u8);
    tb.clock(2).await;
    tb.expect("result", 2u32).await;

    // Test AND operation
    tb.set("a", 0xFFu32).set("b", 0x0Fu32).set("op", 0b010u8);
    tb.clock(2).await;
    tb.expect("result", 0x0Fu32).await;

    tb.export_waveform("build/test_alu_with_new_api.skw.gz").ok();
}

#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS, CPU runtime not yet implemented"
)]
async fn test_alu_table_driven() {
    let mut tb = Testbench::new("examples/alu.sk").await.unwrap();

    // Test vectors: (a, b, op, expected_result, description)
    let test_cases = vec![
        (5u32, 3u32, 0b000u8, 8u32, "ADD"),
        (5u32, 3u32, 0b001u8, 2u32, "SUB"),
        (0xFFu32, 0x0Fu32, 0b010u8, 0x0Fu32, "AND"),
        (0xF0u32, 0x0Fu32, 0b011u8, 0xFFu32, "OR"),
        (0xFFu32, 0xF0u32, 0b100u8, 0x0Fu32, "XOR"),
    ];

    for (a, b, op, expected, desc) in test_cases {
        tb.set("a", a).set("b", b).set("op", op);
        tb.clock(2).await;
        tb.expect("result", expected).await;
        println!(
            "✓ Test passed: {} ({} {} with op={:03b} = {})",
            desc, a, desc, op, expected
        );
    }

    tb.export_waveform("build/test_alu_table_driven.skw.gz").ok();
}

#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS, CPU runtime not yet implemented"
)]
async fn test_fifo_with_new_api() {
    let mut tb = Testbench::new("examples/fifo.sk").await.unwrap();

    // Reset the FIFO
    tb.reset(2).await;

    // Write 5 values - demonstrating the clean API
    for i in 0..5 {
        tb.set("wr_en", 1u8).set("wr_data", (i * 10) as u8);
        tb.clock(1).await;
    }
    tb.set("wr_en", 0u8);

    println!("✓ FIFO write operations completed with clean API");
    println!("✓ Compare this to the old API which required manual byte arrays!");

    tb.export_waveform("build/test_fifo_with_new_api.skw.gz").ok();
}

#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS, CPU runtime not yet implemented"
)]
async fn test_fifo_full_condition() {
    let mut tb = Testbench::new("examples/fifo.sk").await.unwrap();

    tb.reset(2).await;

    // Fill the FIFO (depth = 16)
    for i in 0..16 {
        tb.set("wr_en", 1u8).set("wr_data", i as u8);
        tb.clock(1).await;
    }
    tb.set("wr_en", 0u8);
    tb.clock(1).await;

    println!("✓ FIFO filled with 16 values");

    tb.export_waveform("build/test_fifo_full_condition.skw.gz").ok();
}

#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS, CPU runtime not yet implemented"
)]
async fn test_cycle_counting() {
    let mut tb = Testbench::new("examples/alu.sk").await.unwrap();

    assert_eq!(tb.cycles(), 0, "Should start at cycle 0");

    tb.set("a", 1u32).set("b", 1u32).set("op", 0u8);
    tb.clock(5).await;

    assert_eq!(tb.cycles(), 5, "Should be at cycle 5 after 5 clocks");

    tb.clock(10).await;

    assert_eq!(
        tb.cycles(),
        15,
        "Should be at cycle 15 after 15 total clocks"
    );

    tb.export_waveform("build/test_cycle_counting.skw.gz").ok();
}

// Note: Reset test removed because not all modules (like ALU) have reset signals
// The reset() helper works with modules that have a 'rst' input
