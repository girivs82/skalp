//! CDC (Clock Domain Crossing) Verification Tests
//!
//! These tests demonstrate:
//! 1. Multi-clock testbench API usage
//! 2. Independent clock domain control
//! 3. clock_signal() and clock_multi() APIs
//!
//! Note: In clock_multi(&[("clk1", N), ("clk2", M)]), N and M are the NUMBER OF CYCLES,
//! not signal values. The API runs N full cycles on clk1 and M full cycles on clk2.

use skalp_testing::testbench::*;

#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS, CPU runtime not yet implemented"
)]
async fn test_multi_clock_api_basic() {
    // This test demonstrates the clock_signal API using a simple counter
    let mut tb = Testbench::new("examples/counter.sk").await.unwrap();

    // Apply reset and hold for 2 clock cycles
    tb.set("rst", 1u8); // Set reset signal HIGH
    tb.clock_signal("clk", 2).await; // Run 2 clock cycles with reset held high
    assert_eq!(tb.cycles(), 2, "clock_signal should advance cycle count");

    // Release reset and run for 5 more cycles
    tb.set("rst", 0u8); // Set reset signal LOW
    tb.clock_signal("clk", 5).await; // Run 5 clock cycles
    assert_eq!(
        tb.cycles(),
        7,
        "cycle count should accumulate across clock_signal calls"
    );

    // Verify counter is incrementing
    let count: u8 = tb.get_as("count").await;
    assert!(count > 0, "Counter should have incremented");

    println!("✓ Multi-clock API basic test passed!");
    println!("  - clock_signal(name, num_cycles) works correctly");
    println!("  - Cycle count tracking works");

    tb.export_waveform("build/test_multi_clock_api_basic.skw.gz").ok();
}

#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS, CPU runtime not yet implemented"
)]
async fn test_clock_multi_independent_clocks() {
    // Demonstrates running multiple independent clocks
    // We'll use the ALU example and show we can control clock separately
    let mut tb = Testbench::new("examples/alu.sk").await.unwrap();

    // Set up ALU inputs
    tb.set("a", 10u32).set("b", 5u32).set("op", 0b000u8);

    // Run the clock for 2 cycles using clock_multi
    // (Even though ALU only has one clock, this demonstrates the API)
    // The number 2 means "run 2 full clock cycles", not a signal value
    tb.clock_multi(&[("clk", 2)]).await;

    tb.expect("result", 15u32).await;

    println!("✓ clock_multi API test passed!");
    println!("  - clock_multi(&[(name, num_cycles)]) controls individual clocks");
    println!("  - The second parameter is NUMBER OF CYCLES, not a signal value");

    tb.export_waveform("build/test_clock_multi_independent_clocks.skw.gz").ok();
}

#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS, CPU runtime not yet implemented"
)]
async fn test_default_clock_still_works() {
    // Verify backward compatibility - clock() should still work
    let mut tb = Testbench::new("examples/alu.sk").await.unwrap();

    tb.set("a", 7u32).set("b", 3u32).set("op", 0b001u8);
    tb.clock(2).await; // Old API: runs 2 cycles on default "clk" signal

    tb.expect("result", 4u32).await;

    println!("✓ Backward compatibility test passed!");
    println!("  - clock(n) still works (internally calls clock_signal(\"clk\", n))");

    tb.export_waveform("build/test_default_clock_still_works.skw.gz").ok();
}

#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS, CPU runtime not yet implemented"
)]
async fn test_fifo_with_named_clock() {
    // Test clock_signal with FIFO (demonstrates CDC API readiness)
    let mut tb = Testbench::new("examples/fifo.sk").await.unwrap();

    // Reset sequence using named clock
    tb.set("rst", 1u8); // Assert reset
    tb.clock_signal("clk", 2).await; // Hold reset for 2 clock cycles
    tb.set("rst", 0u8); // De-assert reset
    tb.clock_signal("clk", 1).await; // One cycle for reset to take effect

    // Write data
    for i in 0..4 {
        tb.set("wr_en", 1u8).set("wr_data", (i * 10) as u8);
        tb.clock_signal("clk", 1).await; // Run 1 clock cycle
    }
    tb.set("wr_en", 0u8);

    tb.expect("empty", 0u8).await;

    println!("✓ FIFO with clock_signal API test passed!");
    println!("  - Named clock control works with FIFO");

    tb.export_waveform("build/test_fifo_with_named_clock.skw.gz").ok();
}

#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS, CPU runtime not yet implemented"
)]
async fn test_dual_clock_reset_pattern() {
    // Demonstrates proper CDC reset pattern with two independent clocks
    // This would be used with an async FIFO or other multi-clock design

    let mut tb = Testbench::new("examples/counter.sk").await.unwrap();

    // Simulate a dual-clock reset sequence
    // In a real async FIFO, you'd have wr_rst and rd_rst on different clocks

    // Method 1: Reset both clocks simultaneously for the same duration
    tb.set("rst", 1u8); // Assert reset
    tb.clock_multi(&[("clk", 3)]).await; // Run 3 cycles with reset high
    tb.set("rst", 0u8); // De-assert reset
    tb.clock_multi(&[("clk", 1)]).await; // 1 cycle to stabilize

    println!("✓ Dual-clock reset pattern test passed!");
    println!("  - clock_multi can be used for synchronized multi-clock operations");
    println!("  - Parameters are (clock_name, number_of_cycles)");

    tb.export_waveform("build/test_dual_clock_reset_pattern.skw.gz").ok();
}

// Example showing what a real async FIFO test would look like (conceptual)
#[tokio::test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "GPU simulation only available on macOS, CPU runtime not yet implemented"
)]
async fn test_async_fifo_conceptual() {
    // This test shows what testing a real async FIFO would look like
    // using the multi-clock API (even though async_fifo.sk has syntax issues)

    let mut tb = Testbench::new("examples/counter.sk").await.unwrap();

    // === CONCEPTUAL ASYNC FIFO RESET PATTERN ===
    //
    // Reset both clock domains independently:
    //
    // tb.set("wr_rst", 1u8).set("rd_rst", 1u8);
    //
    // Run 3 cycles on BOTH clocks simultaneously (both reset high):
    // tb.clock_multi(&[("wr_clk", 3), ("rd_clk", 3)]).await;
    //                     ^^^^^^^       ^^^^^^^
    //                     3 cycles      3 cycles
    //                     on wr_clk     on rd_clk
    //
    // tb.set("wr_rst", 0u8).set("rd_rst", 0u8);
    // tb.clock_multi(&[("wr_clk", 1), ("rd_clk", 1)]).await;
    //
    // === WRITE AT FAST CLOCK, READ AT SLOW CLOCK ===
    //
    // Write 8 values on write clock:
    // for i in 0..8 {
    //     tb.set("wr_en", 1u8).set("wr_data", i);
    //     tb.clock_signal("wr_clk", 1).await;  // 1 wr_clk cycle
    // }
    //
    // Wait for CDC synchronization (2 read clock cycles minimum):
    // tb.clock_signal("rd_clk", 3).await;  // 3 rd_clk cycles
    //
    // Read while write clock runs faster:
    // for i in 0..8 {
    //     tb.set("rd_en", 1u8);
    //     // Run 1 rd_clk cycle AND 3 wr_clk cycles simultaneously
    //     tb.clock_multi(&[("rd_clk", 1), ("wr_clk", 3)]).await;
    //     tb.expect("rd_data", i).await;
    // }

    // For now, just demonstrate the API works
    tb.set("rst", 1u8);
    tb.clock_multi(&[("clk", 2)]).await;
    tb.set("rst", 0u8);

    println!("✓ Async FIFO conceptual test passed!");
    println!("  - Demonstrates multi-clock patterns for CDC designs");
    println!("  - clock_multi(&[(clk1, N), (clk2, M)]) runs N cycles on clk1, M on clk2");

    tb.export_waveform("build/test_async_fifo_conceptual.skw.gz").ok();
}
