/// Tutorial verification tests.
///
/// These tests mirror the testbenches shown in the VHDL tutorial chapters
/// on mikaana.com. Each test uses the same VHDL example files and verifies
/// the same behaviors described in the tutorials.
use skalp_testing::testbench::*;

// ========================================================================
// Chapter 1: Getting Started — counter.vhd
// ========================================================================

#[tokio::test]
async fn tutorial_ch1_counter_counts() {
    let mut tb = Testbench::new("examples/vhdl/counter.vhd").await.unwrap();

    // Reset
    tb.set("rst", 1u8).set("en", 0u8);
    tb.clock(2).await;
    tb.expect("count", 0u8).await;

    // Release reset, enable counting
    tb.set("rst", 0u8).set("en", 1u8);
    for i in 1u8..=10 {
        tb.clock(1).await;
        tb.expect("count", i).await;
    }
}

#[tokio::test]
async fn tutorial_ch1_counter_overflow() {
    let mut tb = Testbench::new("examples/vhdl/counter.vhd").await.unwrap();

    // Reset
    tb.set("rst", 1u8).set("en", 0u8);
    tb.clock(2).await;

    // Enable and count to 255
    tb.set("rst", 0u8).set("en", 1u8);
    tb.clock(255).await;
    tb.expect("count", 255u8).await;

    // One more cycle — should wrap to 0
    tb.clock(1).await;
    tb.expect("count", 0u8).await;
}

#[tokio::test]
async fn tutorial_ch1_counter_disable() {
    let mut tb = Testbench::new("examples/vhdl/counter.vhd").await.unwrap();

    // Reset
    tb.set("rst", 1u8).set("en", 0u8);
    tb.clock(2).await;

    // Count up to 5
    tb.set("rst", 0u8).set("en", 1u8);
    tb.clock(5).await;
    tb.expect("count", 5u8).await;

    // Disable — counter should hold its value
    tb.set("en", 0u8);
    tb.clock(10).await;
    tb.expect("count", 5u8).await;

    // Re-enable — should resume from 5
    tb.set("en", 1u8);
    tb.clock(1).await;
    tb.expect("count", 6u8).await;
}

// ========================================================================
// Chapter 2: Combinational Logic — mux4.vhd
// ========================================================================

#[tokio::test]
async fn tutorial_ch2_mux4_selects() {
    let mut tb = Testbench::new("examples/vhdl/mux4.vhd").await.unwrap();

    // Set four input channels
    tb.set("a", 0x11u8)
        .set("b", 0x22u8)
        .set("c", 0x33u8)
        .set("d", 0x44u8);

    // sel=00 -> y=a
    tb.set("sel", 0b00u8);
    tb.clock(1).await;
    tb.expect("y", 0x11u8).await;

    // sel=01 -> y=b
    tb.set("sel", 0b01u8);
    tb.clock(1).await;
    tb.expect("y", 0x22u8).await;

    // sel=10 -> y=c
    tb.set("sel", 0b10u8);
    tb.clock(1).await;
    tb.expect("y", 0x33u8).await;

    // sel=11 -> y=d
    tb.set("sel", 0b11u8);
    tb.clock(1).await;
    tb.expect("y", 0x44u8).await;
}

// ========================================================================
// Chapter 3: Clocked Processes and FSMs — timer.vhd, i2c_fsm.vhd
// ========================================================================

#[tokio::test]
async fn tutorial_ch3_timer_counts() {
    let mut tb = Testbench::new("examples/vhdl/timer.vhd").await.unwrap();

    // Reset
    tb.set("rst", 1u8)
        .set("enable", 0u8)
        .set("prescaler", 0u8)
        .set("threshold", 0xFFu32);
    tb.clock(2).await;
    tb.expect("counter", 0u32).await;

    // Enable counting, prescaler=0 (tick every cycle)
    tb.set("rst", 0u8).set("enable", 1u8);

    // prescaler=0 means tick='1' always, but tick is registered,
    // so counter sees tick after 1-cycle delay
    tb.clock(1).await; // tick becomes '1', counter sees old tick=0
    for expected in 1u8..=5 {
        tb.clock(1).await;
        tb.expect("counter", expected).await;
    }
}

#[tokio::test]
async fn tutorial_ch3_timer_match() {
    let mut tb = Testbench::new("examples/vhdl/timer.vhd").await.unwrap();

    // Reset with threshold=5
    tb.set("rst", 1u8)
        .set("enable", 0u8)
        .set("prescaler", 0u8)
        .set("threshold", 5u32);
    tb.clock(2).await;

    // Enable counting
    tb.set("rst", 0u8).set("enable", 1u8);

    // Clock until match_out fires
    let mut match_seen = false;
    for _ in 0..20 {
        tb.clock(1).await;
        let m = tb.get_u32("match_out").await;
        if m == 1 {
            match_seen = true;
            break;
        }
    }
    assert!(
        match_seen,
        "match_out should fire when counter hits threshold"
    );

    // Counter should reset to 0 after match
    tb.expect("counter", 0u32).await;
}

#[tokio::test]
async fn tutorial_ch3_i2c_start_transfer() {
    let mut tb = Testbench::new("examples/vhdl/i2c_fsm.vhd").await.unwrap();

    // Reset
    tb.set("rst", 1u8)
        .set("start", 0u8)
        .set("stop", 0u8)
        .set("wr_data", 0u8)
        .set("sda_in", 1u8);
    tb.clock(3).await;

    // Release reset
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Should be idle
    tb.expect("busy", 0u32).await;

    // Start a transfer
    tb.set("wr_data", 0xA5u32).set("start", 1u8);
    tb.clock(1).await;
    tb.set("start", 0u8);

    // busy should be high
    tb.expect("busy", 1u32).await;

    // Wait for completion (loopback sda_out to sda_in)
    let mut done_seen = false;
    for _ in 0..700 {
        let sda = tb.get_u32("sda_out").await;
        tb.set("sda_in", sda as u8);
        tb.clock(1).await;
        if tb.get_u32("done").await == 1 {
            done_seen = true;
            break;
        }
    }
    assert!(done_seen, "FSM should complete transfer");
    tb.expect("busy", 0u32).await;
}

// ========================================================================
// Chapter 4: Generics, Records, and Arrays — gpio_ctrl.vhd
// ========================================================================

#[tokio::test]
async fn tutorial_ch4_gpio_register_write_read() {
    let mut tb = Testbench::new("examples/vhdl/gpio_ctrl.vhd").await.unwrap();

    // Reset
    tb.set("rst", 1u8)
        .set("addr", 0u8)
        .set("wdata", 0u8)
        .set("we", 0u8)
        .set("gpio_in", 0u8);
    tb.clock(2).await;
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Write 0xA5 to output register (addr=01)
    tb.set("addr", 0b01u8).set("wdata", 0xA5u32).set("we", 1u8);
    tb.clock(1).await;
    tb.set("we", 0u8);
    tb.clock(1).await;

    // Verify output register
    tb.expect("gpio_out", 0xA5u32).await;

    // Read back via mux — addr=01 should return out_reg
    tb.set("addr", 0b01u8);
    tb.clock(1).await;
    tb.expect("rdata", 0xA5u32).await;

    // Write 0x0F to direction register (addr=10)
    tb.set("addr", 0b10u8).set("wdata", 0x0Fu32).set("we", 1u8);
    tb.clock(1).await;
    tb.set("we", 0u8);
    tb.clock(1).await;

    tb.expect("gpio_dir", 0x0Fu32).await;
}

#[tokio::test]
async fn tutorial_ch4_gpio_edge_detect_irq() {
    let mut tb = Testbench::new("examples/vhdl/gpio_ctrl.vhd").await.unwrap();

    // Reset
    tb.set("rst", 1u8)
        .set("addr", 0u8)
        .set("wdata", 0u8)
        .set("we", 0u8)
        .set("gpio_in", 0u8);
    tb.clock(2).await;
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Enable IRQ on pin 0 (addr=11, bit 0)
    tb.set("addr", 0b11u8).set("wdata", 0x01u32).set("we", 1u8);
    tb.clock(1).await;
    tb.set("we", 0u8);
    tb.clock(1).await;

    // IRQ should be low before any edge
    tb.expect("irq", 0u32).await;

    // Rising edge on gpio_in[0]
    tb.set("gpio_in", 0x01u32);
    tb.clock(3).await; // double-flop sync + edge detect latency

    // IRQ should fire
    tb.expect("irq", 1u32).await;

    // IRQ stays latched even after input goes low
    tb.set("gpio_in", 0u8);
    tb.clock(2).await;
    tb.expect("irq", 1u32).await;
}

// ========================================================================
// Chapter 5: Hierarchical Design — bus_system.vhd
// ========================================================================

#[tokio::test]
async fn tutorial_ch5_bus_single_transfer() {
    let mut tb = Testbench::new("examples/vhdl/bus_system.vhd")
        .await
        .unwrap();

    // Reset
    tb.set("rst", 1u8).set("trigger", 0u8).set("tx_data", 0u8);
    tb.clock(2).await;

    // Release reset
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Transfer 0xA5
    tb.set("tx_data", 0xA5u32).set("trigger", 1u8);
    tb.clock(1).await;
    tb.expect("rx_data", 0xA5u32).await;
    tb.expect("rx_valid", 1u32).await;

    // Deassert trigger — rx_valid should drop
    tb.set("trigger", 0u8);
    tb.clock(1).await;
    tb.expect("rx_valid", 0u32).await;
}

#[tokio::test]
async fn tutorial_ch5_bus_back_to_back() {
    let mut tb = Testbench::new("examples/vhdl/bus_system.vhd")
        .await
        .unwrap();

    // Reset
    tb.set("rst", 1u8).set("trigger", 0u8).set("tx_data", 0u8);
    tb.clock(2).await;
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // First transfer
    tb.set("tx_data", 0xA5u32).set("trigger", 1u8);
    tb.clock(1).await;
    tb.expect("rx_data", 0xA5u32).await;

    // Second transfer (change data while trigger stays high)
    tb.set("tx_data", 0x3Cu32);
    tb.clock(1).await;
    tb.expect("rx_data", 0x3Cu32).await;
}

// ========================================================================
// Chapter 6: Testing VHDL with Rust — revisit counter, timer, i2c_fsm
// (These are covered by ch1-3 tests above; add waveform dump test)
// ========================================================================

#[tokio::test]
async fn tutorial_ch6_counter_with_waveform() {
    let mut tb = Testbench::new("examples/vhdl/counter.vhd").await.unwrap();

    // Reset
    tb.set("rst", 1u8).set("en", 0u8);
    tb.clock(2).await;

    // Count to 20
    tb.set("rst", 0u8).set("en", 1u8);
    tb.clock(20).await;
    tb.expect("count", 20u8).await;

    // Save waveform
    tb.export_waveform("build/tutorial_counter.skw.gz").ok();
}

// ========================================================================
// Chapter 7: skalp Integration — uart.vhd
// ========================================================================

#[tokio::test]
async fn tutorial_ch7_uart_tx() {
    let mut tb = Testbench::new("examples/vhdl/uart.vhd").await.unwrap();

    // Reset — UART uses "clock" not "clk"
    tb.set("reset", 1u8)
        .set("data_stream_in", 0u8)
        .set("data_stream_in_stb", 0u8)
        .set("rx", 1u8);
    tb.clock_signal("clock", 4).await;

    // Release reset
    tb.set("reset", 0u8);
    tb.clock_signal("clock", 5).await;

    // Send a byte
    tb.set("data_stream_in", 0x55u32)
        .set("data_stream_in_stb", 1u8);

    // Wait for acknowledgment
    let mut ack_seen = false;
    for _ in 0..30 {
        tb.clock_signal("clock", 1).await;
        if tb.get_u32("data_stream_in_ack").await == 1 {
            ack_seen = true;
            break;
        }
    }
    assert!(ack_seen, "data_stream_in_ack should pulse within 30 cycles");

    // Deassert strobe
    tb.set("data_stream_in_stb", 0u8);

    // Clock through TX frame and verify activity
    let mut tx_samples = Vec::new();
    for _ in 0..200 {
        tb.clock_signal("clock", 1).await;
        tx_samples.push(tb.get_u32("tx").await);
    }

    // Verify TX had both 0s and 1s (actual data transmission)
    assert!(tx_samples.contains(&0), "TX should go low for start bit");
    assert!(
        tx_samples.contains(&1),
        "TX should go high for stop/data bits"
    );

    // TX should return to idle high
    assert_eq!(*tx_samples.last().unwrap(), 1, "TX should return to idle");
}

#[tokio::test]
async fn tutorial_ch7_uart_rx_loopback() {
    let mut tb = Testbench::new("examples/vhdl/uart.vhd").await.unwrap();

    // With defaults: baud=9600, clock_frequency=153600
    // c_rx_div = 153600/(9600*16) = 1
    // RX bit period = 16 rx_baud_ticks * 2 cycles = 32 system cycles
    let rx_bit_cycles: usize = 32;

    // Reset
    tb.set("reset", 1u8)
        .set("data_stream_in", 0u8)
        .set("data_stream_in_stb", 0u8)
        .set("rx", 1u8);
    tb.clock_signal("clock", 4).await;
    tb.set("reset", 0u8);
    tb.clock_signal("clock", 4).await;

    // Build UART frame for 0x55 (LSB first)
    let byte_val: u8 = 0x55;
    let mut frame_bits: Vec<u8> = Vec::new();
    frame_bits.push(0); // start bit
    for i in 0..8 {
        frame_bits.push((byte_val >> i) & 1);
    }
    frame_bits.push(1); // stop bit

    // Feed the frame
    for &bit in &frame_bits {
        tb.set("rx", bit);
        tb.clock_signal("clock", rx_bit_cycles).await;
    }

    // Wait for processing
    tb.set("rx", 1u8);
    tb.clock_signal("clock", rx_bit_cycles * 3).await;

    // Verify received data
    let rx_data = tb.get_u32("data_stream_out").await;
    assert_eq!(
        rx_data, byte_val as u32,
        "RX data mismatch: expected 0x{:02X}, got 0x{:02X}",
        byte_val, rx_data
    );
}

// ========================================================================
// Chapter 8: VHDL-2019 Features — bus_system.vhd (with interfaces)
// (Same physical file as ch5 — bus_system.vhd uses VHDL-2019 interfaces)
// ========================================================================

#[tokio::test]
async fn tutorial_ch8_bus_with_interfaces() {
    let mut tb = Testbench::new("examples/vhdl/bus_system.vhd")
        .await
        .unwrap();

    // Reset
    tb.set("rst", 1u8).set("trigger", 0u8).set("tx_data", 0u8);
    tb.clock(2).await;
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Transfer data through interface-connected bus
    tb.set("tx_data", 0xDEu32).set("trigger", 1u8);
    tb.clock(1).await;
    tb.expect("rx_data", 0xDEu32).await;
    tb.expect("rx_valid", 1u32).await;
}

// ========================================================================
// Chapter 9: Real-World Project — spi_master.vhd
// ========================================================================

#[tokio::test]
async fn tutorial_ch9_spi_idle_state() {
    let mut tb = Testbench::new("examples/vhdl/spi_master.vhd")
        .await
        .unwrap();

    // Reset
    tb.set("rst", 1u8)
        .set("din", 0u32)
        .set("din_addr", 0u8)
        .set("din_last", 0u8)
        .set("din_vld", 0u8)
        .set("miso", 0u8);
    tb.clock(3).await;
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // After reset: ready, SCLK low, CS_N high
    tb.expect("din_rdy", 1u32).await;
    tb.expect("sclk", 0u32).await;
    tb.expect("cs_n", 1u8).await;
}

#[tokio::test]
async fn tutorial_ch9_spi_single_byte() {
    let mut tb = Testbench::new("examples/vhdl/spi_master.vhd")
        .await
        .unwrap();

    // Reset
    tb.set("rst", 1u8)
        .set("din", 0u32)
        .set("din_addr", 0u8)
        .set("din_last", 0u8)
        .set("din_vld", 0u8)
        .set("miso", 0u8);
    tb.clock(3).await;
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Load data
    tb.set("din", 0xA5u32)
        .set("din_addr", 0u8)
        .set("din_last", 1u8)
        .set("din_vld", 1u8);

    // Wait for master to accept
    for _ in 0..5 {
        tb.clock(1).await;
        if tb.get_u32("din_rdy").await == 0 {
            break;
        }
    }
    tb.set("din_vld", 0u8);

    // Wait for transfer to complete
    let mut dout_vld_seen = false;
    for _ in 0..500 {
        tb.clock(1).await;
        if tb.get_u32("dout_vld").await == 1 {
            dout_vld_seen = true;
            break;
        }
    }
    assert!(dout_vld_seen, "DOUT_VLD should fire after transfer");
}

#[tokio::test]
async fn tutorial_ch9_spi_loopback() {
    let mut tb = Testbench::new("examples/vhdl/spi_master.vhd")
        .await
        .unwrap();

    // Reset
    tb.set("rst", 1u8)
        .set("din", 0u32)
        .set("din_addr", 0u8)
        .set("din_last", 0u8)
        .set("din_vld", 0u8)
        .set("miso", 0u8);
    tb.clock(3).await;
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Send 0xA5
    tb.set("din", 0xA5u32)
        .set("din_last", 1u8)
        .set("din_vld", 1u8);

    for _ in 0..5 {
        tb.clock(1).await;
        if tb.get_u32("din_rdy").await == 0 {
            break;
        }
    }
    tb.set("din_vld", 0u8);

    // Loopback: feed MOSI back to MISO
    let mut dout_vld_seen = false;
    let mut dout_val = 0u32;
    for _ in 0..200 {
        let mosi = tb.get_u32("mosi").await;
        tb.set("miso", mosi as u8);
        tb.clock(1).await;

        if tb.get_u32("dout_vld").await == 1 {
            dout_val = tb.get_u32("dout").await;
            dout_vld_seen = true;
            break;
        }
    }

    assert!(dout_vld_seen, "DOUT_VLD should fire after loopback");
    assert_eq!(
        dout_val, 0xA5,
        "loopback DOUT should match DIN 0xA5, got 0x{:02X}",
        dout_val
    );
}

// ========================================================================
// SPI Master: verify MOSI transmits correct bits (MSB first)
// ========================================================================

#[tokio::test]
async fn tutorial_ch9_spi_mosi_bits() {
    let mut tb = Testbench::new("examples/vhdl/spi_master.vhd")
        .await
        .unwrap();

    // Reset
    tb.set("rst", 1u8)
        .set("din", 0u32)
        .set("din_addr", 0u8)
        .set("din_last", 0u8)
        .set("din_vld", 0u8)
        .set("miso", 0u8);
    tb.clock(3).await;
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Send 0xA5
    tb.set("din", 0xA5u32)
        .set("din_addr", 0u8)
        .set("din_last", 1u8)
        .set("din_vld", 1u8);

    for _ in 0..5 {
        tb.clock(1).await;
        if tb.get_u32("din_rdy").await == 0 {
            break;
        }
    }
    tb.set("din_vld", 0u8);

    // Sample MOSI bits on SCLK rising edges
    let mut mosi_bits: Vec<u32> = Vec::new();
    let mut prev_sclk = 0u32;

    for _ in 0..200 {
        tb.clock(1).await;
        let sclk = tb.get_u32("sclk").await;
        let cs_n = tb.get_u32("cs_n").await;

        if sclk == 1 && prev_sclk == 0 && cs_n == 0 {
            mosi_bits.push(tb.get_u32("mosi").await);
        }
        prev_sclk = sclk;

        if tb.get_u32("dout_vld").await == 1 {
            break;
        }
    }

    assert_eq!(
        mosi_bits.len(),
        8,
        "expected 8 MOSI bits, got {}: {:?}",
        mosi_bits.len(),
        mosi_bits
    );

    // Reconstruct byte (MSB first)
    let mut tx_byte: u32 = 0;
    for &bit in &mosi_bits {
        tx_byte = (tx_byte << 1) | bit;
    }
    assert_eq!(
        tx_byte, 0xA5,
        "MOSI should transmit 0xA5, got 0x{:02X}",
        tx_byte
    );
}
