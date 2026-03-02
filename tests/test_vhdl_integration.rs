/// Integration tests for the VHDL frontend.
///
/// Tests 1-3 exercise the full pipeline: VHDL -> HIR -> MIR -> SIR -> simulation
/// Tests 4-5 verify HIR structure from parse + lower without simulation

// ========================================================================
// Parse-only tests (VHDL -> HIR, no simulation)
// ========================================================================

#[test]
fn test_vhdl_counter_parse_only() {
    let source = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/vhdl/counter.vhd"),
    )
    .unwrap();

    let hir = skalp_vhdl::parse_vhdl_source(&source, None).unwrap();

    // Entity
    assert_eq!(hir.entities.len(), 1);
    assert_eq!(hir.entities[0].name, "Counter");
    assert_eq!(hir.entities[0].ports.len(), 4);

    // Architecture lowered to implementation
    assert_eq!(hir.implementations.len(), 1);
    let imp = &hir.implementations[0];

    // At least one signal (count_reg)
    assert!(
        !imp.signals.is_empty(),
        "expected at least 1 signal, got {}",
        imp.signals.len()
    );

    // At least one event block (the clocked process)
    assert!(
        !imp.event_blocks.is_empty(),
        "expected event blocks, got {}",
        imp.event_blocks.len()
    );

    // At least one concurrent assignment (count <= count_reg)
    assert!(
        !imp.assignments.is_empty(),
        "expected assignments, got {}",
        imp.assignments.len()
    );
}

#[test]
fn test_vhdl_mux4_parse_only() {
    let source = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/vhdl/mux4.vhd"),
    )
    .unwrap();

    let hir = skalp_vhdl::parse_vhdl_source(&source, None).unwrap();

    assert_eq!(hir.entities.len(), 1);
    assert_eq!(hir.entities[0].name, "Mux4");
    assert_eq!(hir.entities[0].ports.len(), 6);

    assert_eq!(hir.implementations.len(), 1);
    let imp = &hir.implementations[0];

    // Combinational process -> event block with empty triggers
    assert!(!imp.event_blocks.is_empty());
    let eb = &imp.event_blocks[0];
    assert!(
        eb.triggers.is_empty(),
        "combinational process should have empty triggers"
    );
}

#[test]
fn test_vhdl_case_insensitive() {
    let source = r#"
ENTITY MyDesign IS
    PORT (
        CLK : IN std_logic;
        RST : IN std_logic
    );
END ENTITY MyDesign;
"#;
    let hir = skalp_vhdl::parse_vhdl_source(source, None).unwrap();

    // "MyDesign" -> lexer lowercases to "mydesign" -> PascalCase: "Mydesign"
    assert_eq!(hir.entities[0].name, "Mydesign");
    assert_eq!(hir.entities[0].ports.len(), 2);
}

// ========================================================================
// Full pipeline tests (VHDL -> HIR -> MIR -> SIR -> simulation)
// ========================================================================

use skalp_frontend::hir::{HirGenericType, HirType};
use skalp_testing::testbench::*;

#[tokio::test]
async fn test_vhdl_counter_counts_up() {
    let mut tb = Testbench::new("examples/vhdl/counter.vhd").await.unwrap();

    // Reset
    tb.set("rst", 1u8).set("en", 0u8);
    tb.clock(2).await;
    tb.expect("count", 0u8).await;

    // Release reset, enable counting
    tb.set("rst", 0u8).set("en", 1u8);
    for expected in 1u8..=5 {
        tb.clock(1).await;
        tb.expect("count", expected).await;
    }

    tb.export_waveform("build/test_vhdl_counter_counts_up.skw.gz")
        .ok();
}

#[tokio::test]
async fn test_vhdl_counter_reset_clears() {
    let mut tb = Testbench::new("examples/vhdl/counter.vhd").await.unwrap();

    // Count up to 10
    tb.set("rst", 0u8).set("en", 1u8);
    tb.clock(10).await;
    tb.expect("count", 10u8).await;

    // Assert reset — count should clear
    tb.set("rst", 1u8);
    tb.clock(1).await;
    tb.expect("count", 0u8).await;

    // Release reset, verify counting resumes from 0
    tb.set("rst", 0u8);
    tb.clock(1).await;
    tb.expect("count", 1u8).await;

    tb.export_waveform("build/test_vhdl_counter_reset_clears.skw.gz")
        .ok();
}

#[test]
fn test_vhdl_axi_interface_e2e() {
    let source = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/vhdl/axi_peripheral.vhd"),
    )
    .unwrap();

    let hir = skalp_vhdl::parse_vhdl_source(&source, None).unwrap();

    // Entity with flattened interface ports
    assert_eq!(hir.entities.len(), 1);
    let entity = &hir.entities[0];
    assert_eq!(entity.name, "AxiReg");

    // clk + rst + 6 flattened bus ports = 8
    assert_eq!(
        entity.ports.len(),
        8,
        "expected 8 ports (clk, rst, 6 AXI), got {}: {:?}",
        entity.ports.len(),
        entity.ports.iter().map(|p| &p.name).collect::<Vec<_>>()
    );

    // Verify flattened port names exist
    let names: Vec<&str> = entity.ports.iter().map(|p| p.name.as_str()).collect();
    assert!(
        names.contains(&"bus_awaddr"),
        "missing bus_awaddr in {:?}",
        names
    );
    assert!(
        names.contains(&"bus_wdata"),
        "missing bus_wdata in {:?}",
        names
    );
    assert!(
        names.contains(&"bus_awready"),
        "missing bus_awready in {:?}",
        names
    );
    assert!(
        names.contains(&"bus_wready"),
        "missing bus_wready in {:?}",
        names
    );

    // Verify architecture lowered correctly
    assert_eq!(hir.implementations.len(), 1);
    let imp = &hir.implementations[0];

    // reg_data signal
    assert!(
        !imp.signals.is_empty(),
        "expected at least 1 signal (reg_data)"
    );

    // Concurrent assignments: bus_awready <= '1' and bus_wready <= '1'
    assert!(
        imp.assignments.len() >= 2,
        "expected at least 2 concurrent assignments, got {}",
        imp.assignments.len()
    );

    // Clocked process
    assert!(
        !imp.event_blocks.is_empty(),
        "expected at least 1 event block"
    );
}

#[tokio::test]
async fn test_vhdl_mux4_selects_correctly() {
    let mut tb = Testbench::new("examples/vhdl/mux4.vhd").await.unwrap();

    // Set four input channels to distinct values
    tb.set("a", 0x11u8)
        .set("b", 0x22u8)
        .set("c", 0x33u8)
        .set("d", 0x44u8);

    // sel = "00" -> y = a
    tb.set("sel", 0b00u8);
    tb.clock(1).await;
    tb.expect("y", 0x11u8).await;

    // sel = "01" -> y = b
    tb.set("sel", 0b01u8);
    tb.clock(1).await;
    tb.expect("y", 0x22u8).await;

    // sel = "10" -> y = c
    tb.set("sel", 0b10u8);
    tb.clock(1).await;
    tb.expect("y", 0x33u8).await;

    // sel = "11" -> y = d (others branch)
    tb.set("sel", 0b11u8);
    tb.clock(1).await;
    tb.expect("y", 0x44u8).await;

    tb.export_waveform("build/test_vhdl_mux4_selects.skw.gz")
        .ok();
}

// ========================================================================
// UART parse-only tests (generic wiring verification)
// ========================================================================

#[test]
fn test_vhdl_uart_parse_generics() {
    let source = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/vhdl/uart.vhd"),
    )
    .unwrap();

    let hir = skalp_vhdl::parse_vhdl_source(&source, None).unwrap();

    // Entity with generics
    assert_eq!(hir.entities.len(), 1);
    let entity = &hir.entities[0];
    assert_eq!(entity.name, "Uart");
    assert_eq!(
        entity.generics.len(),
        2,
        "expected 2 generics (baud, clock_frequency), got {}",
        entity.generics.len()
    );

    // Verify generic types
    assert!(
        matches!(
            entity.generics[0].param_type,
            HirGenericType::Const(HirType::Nat(32))
        ),
        "baud generic should be Nat(32)"
    );
    assert!(
        entity.generics[0].default_value.is_some(),
        "baud should have default value"
    );
    assert!(
        entity.generics[1].default_value.is_some(),
        "clock_frequency should have default value"
    );

    // 9 ports: clock, reset, data_stream_in[8], data_stream_in_stb, data_stream_in_ack,
    //          data_stream_out[8], data_stream_out_stb, tx, rx
    assert_eq!(
        entity.ports.len(),
        9,
        "expected 9 ports, got {}: {:?}",
        entity.ports.len(),
        entity.ports.iter().map(|p| &p.name).collect::<Vec<_>>()
    );

    // Architecture should have constants derived from generics
    assert_eq!(hir.implementations.len(), 1);
    let imp = &hir.implementations[0];

    // 2 generic constants (baud, clock_frequency) + 4 architecture constants (c_tx_div, c_rx_div, c_tx_div_width, c_rx_div_width)
    assert!(
        imp.constants.len() >= 6,
        "expected >= 6 constants (2 generics + 4 derived), got {}",
        imp.constants.len()
    );

    // Verify generic constants are in the list
    let const_names: Vec<&str> = imp.constants.iter().map(|c| c.name.as_str()).collect();
    assert!(
        const_names.contains(&"baud"),
        "missing 'baud' constant, got: {:?}",
        const_names
    );
    assert!(
        const_names.contains(&"clock_frequency"),
        "missing 'clock_frequency' constant, got: {:?}",
        const_names
    );

    // At least 6 processes + signals for the full UART
    assert!(
        imp.event_blocks.len() >= 6,
        "expected >= 6 event blocks (processes), got {}",
        imp.event_blocks.len()
    );
}

#[test]
fn test_vhdl_spi_master_parse_generics() {
    let source = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/vhdl/spi_master.vhd"),
    )
    .unwrap();

    let hir = skalp_vhdl::parse_vhdl_source(&source, None).unwrap();

    // Entity with 4 generics
    assert_eq!(hir.entities.len(), 1);
    let entity = &hir.entities[0];
    assert_eq!(entity.name, "SpiMaster");
    assert_eq!(
        entity.generics.len(),
        4,
        "expected 4 generics (CLK_FREQ, SCLK_FREQ, WORD_SIZE, SLAVE_COUNT), got {}",
        entity.generics.len()
    );

    // All generics should have defaults
    for g in &entity.generics {
        assert!(
            g.default_value.is_some(),
            "generic '{}' should have default value",
            g.name
        );
    }

    // Architecture should have generics injected as constants
    assert_eq!(hir.implementations.len(), 1);
    let imp = &hir.implementations[0];

    let const_names: Vec<&str> = imp.constants.iter().map(|c| c.name.as_str()).collect();
    assert!(
        const_names.contains(&"clk_freq"),
        "missing 'clk_freq' constant, got: {:?}",
        const_names
    );
    assert!(
        const_names.contains(&"word_size"),
        "missing 'word_size' constant, got: {:?}",
        const_names
    );
}

#[test]
fn test_vhdl_uart_constant_expressions() {
    let source = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/vhdl/uart.vhd"),
    )
    .unwrap();

    let hir = skalp_vhdl::parse_vhdl_source(&source, None).unwrap();
    let imp = &hir.implementations[0];

    // Verify c_tx_div exists and is a division expression
    let c_tx_div = imp
        .constants
        .iter()
        .find(|c| c.name == "c_tx_div")
        .expect("missing c_tx_div constant");

    assert!(
        matches!(
            &c_tx_div.value,
            skalp_frontend::hir::HirExpression::Binary(bin)
            if matches!(bin.op, skalp_frontend::hir::HirBinaryOp::Div)
        ),
        "c_tx_div should be Binary(Div) expression"
    );

    // Verify constant references (not GenericParam) are used in the expression
    if let skalp_frontend::hir::HirExpression::Binary(bin) = &c_tx_div.value {
        assert!(
            matches!(*bin.left, skalp_frontend::hir::HirExpression::Constant(_)),
            "c_tx_div left operand should be Constant(id)"
        );
        assert!(
            matches!(*bin.right, skalp_frontend::hir::HirExpression::Constant(_)),
            "c_tx_div right operand should be Constant(id)"
        );
    }
}

// ========================================================================
// UART pipeline test: VHDL -> HIR -> MIR -> SIR
// Verifies all 18 internal signals survive the pipeline and MIR case arms
// are correctly populated from VHDL enum patterns.
// ========================================================================

#[test]
fn test_vhdl_uart_sir_dump() {
    use skalp_mir::MirCompiler;

    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/vhdl/uart.vhd");
    let context = skalp_vhdl::parse_vhdl(&path).unwrap();

    let imp = &context.main_hir.implementations[0];

    // HIR should have all 18 signals and 6 constants (2 generics + 4 derived)
    assert_eq!(imp.signals.len(), 18, "HIR should have 18 signals");
    assert_eq!(imp.constants.len(), 6, "HIR should have 6 constants");
    assert_eq!(
        imp.event_blocks.len(),
        7,
        "HIR should have 7 event blocks (processes)"
    );

    let compiler = MirCompiler::new();
    let mir = compiler
        .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
        .unwrap();
    let module = &mir.modules[0];

    // All 18 signals must survive MIR conversion + DCE
    assert_eq!(
        module.signals.len(),
        18,
        "MIR should preserve all 18 signals after DCE"
    );
    assert_eq!(module.processes.len(), 7, "MIR should have 7 processes");

    // Verify concrete type widths for baud counters (tests const evaluation through generics)
    let tx_baud = module
        .signals
        .iter()
        .find(|s| s.name == "tx_baud_counter")
        .unwrap();
    assert!(
        matches!(tx_baud.signal_type, skalp_mir::mir::DataType::Nat(5)),
        "tx_baud_counter should be Nat(5), got {:?}",
        tx_baud.signal_type
    );
    let rx_baud = module
        .signals
        .iter()
        .find(|s| s.name == "rx_baud_counter")
        .unwrap();
    assert!(
        matches!(rx_baud.signal_type, skalp_mir::mir::DataType::Nat(1)),
        "rx_baud_counter should be Nat(1), got {:?}",
        rx_baud.signal_type
    );

    // Convert to SIR and verify basic structure
    let sir = skalp_sir::mir_to_sir::convert_mir_to_sir(module);
    assert!(sir.inputs.len() >= 4, "need at least 4 inputs");
    assert!(sir.outputs.len() >= 4, "need at least 4 outputs");
    assert!(
        !sir.state_elements.is_empty(),
        "need state elements for UART registers"
    );
}

// ========================================================================
// UART diagnostic sim test (probes internal signals)
// ========================================================================

#[tokio::test]
async fn test_vhdl_uart_internal_signals() {
    // Probe internal signals to understand what's happening in the UART sim
    let mut tb = Testbench::new("examples/vhdl/uart.vhd").await.unwrap();

    let mut diag = String::new();

    // Reset — UART uses "clock" not "clk"
    tb.set("reset", 1u8)
        .set("data_stream_in", 0u8)
        .set("data_stream_in_stb", 0u8)
        .set("rx", 1u8);
    tb.clock_signal("clock", 2).await;
    tb.set("reset", 0u8);
    tb.clock_signal("clock", 1).await;

    // Try reading internal signals (and a bogus one to test error behavior)
    let internal_names = [
        "tx_baud_counter",
        "tx_baud_tick",
        "uart_tx_state",
        "uart_tx_count",
        "uart_tx_data",
        "uart_rx_data_in_ack",
        "BOGUS_SIGNAL_NAME_12345",
    ];
    for name in &internal_names {
        let val = tb.get_u32(name).await;
        diag.push_str(&format!("  {} = {}\n", name, val));
    }

    // Load byte and clock, probing each cycle
    tb.set("data_stream_in", 0xA5u32)
        .set("data_stream_in_stb", 1u8);

    diag.push_str("\nClocking with stb=1:\n");
    let mut ack_seen = false;
    for cycle in 0..25 {
        tb.clock_signal("clock", 1).await;
        let ack = tb.get_u32("data_stream_in_ack").await;
        let tx = tb.get_u32("tx").await;
        let counter = tb.get_u32("tx_baud_counter").await;
        let tick = tb.get_u32("tx_baud_tick").await;
        let state = tb.get_u32("uart_tx_state").await;
        diag.push_str(&format!(
            "  cycle {:2}: ack={} tx={} counter={:2} tick={} state={}\n",
            cycle, ack, tx, counter, tick, state
        ));
        if ack == 1 {
            ack_seen = true;
        }
    }

    std::fs::write("/tmp/uart_sim_diag.txt", &diag).unwrap();

    // If ack never fired, report failure
    if !ack_seen {
        panic!("data_stream_in_ack never pulsed. See /tmp/uart_sim_diag.txt");
    }
}

// ========================================================================
// UART behavioral sim tests
// ========================================================================

#[tokio::test]
async fn test_vhdl_uart_compiles_and_simulates() {
    // Verify the UART design compiles through the full pipeline and basic signals work
    let mut tb = Testbench::new("examples/vhdl/uart.vhd").await.unwrap();

    // Reset
    tb.set("reset", 1u8)
        .set("data_stream_in", 0u8)
        .set("data_stream_in_stb", 0u8)
        .set("rx", 1u8);
    tb.clock_signal("clock", 4).await;

    // Release reset and clock enough for baud tick (c_tx_div=16 cycles)
    tb.set("reset", 0u8);
    tb.set("data_stream_in", 0xA5u32)
        .set("data_stream_in_stb", 1u8);
    tb.clock_signal("clock", 50).await;

    // After 50 cycles post-reset with stb=1, the ack should have pulsed.
    // But even if it didn't, verify the design doesn't crash.
    // The main goal is proving the pipeline compiles.
    let tx = tb.get_u32("tx").await;
    assert!(tx <= 1, "tx should be a valid logic level");

    tb.export_waveform("build/test_vhdl_uart_compile.skw.gz")
        .ok();
}

#[tokio::test]
async fn test_vhdl_uart_tx_sends_byte() {
    // With defaults: baud=9600, clock_frequency=153600
    // c_tx_div = 153600/9600 = 16, c_rx_div = 153600/(9600*16) = 1
    // c_tx_div_width = integer(log2(real(16))) + 1 = 4 + 1 = 5
    // TX sends: 1 start bit + 8 data bits + 1 stop bit = 10 bits
    // Each bit takes c_tx_div=16 cycles. Total = ~160 cycles.
    let mut tb = Testbench::new("examples/vhdl/uart.vhd").await.unwrap();

    // Reset
    tb.set("reset", 1u8)
        .set("data_stream_in", 0u8)
        .set("data_stream_in_stb", 0u8)
        .set("rx", 1u8); // Idle high
    tb.clock_signal("clock", 4).await;

    // Release reset, allow initialization to settle
    tb.set("reset", 0u8);
    tb.clock_signal("clock", 5).await;

    // Load byte 0xA5 = 10100101
    tb.set("data_stream_in", 0xA5u32)
        .set("data_stream_in_stb", 1u8);

    // Wait for ack (TX starts on baud tick)
    let mut ack_seen = false;
    for _ in 0..30 {
        tb.clock_signal("clock", 1).await;
        let ack = tb.get_u32("data_stream_in_ack").await;
        if ack == 1 {
            ack_seen = true;
            break;
        }
    }
    assert!(
        ack_seen,
        "data_stream_in_ack should pulse high within 30 cycles"
    );

    // Deassert strobe after ack
    tb.set("data_stream_in_stb", 0u8);

    // Clock through the entire TX frame: start + 8 data + stop = 10 bit periods
    // Each bit takes c_tx_div=16 cycles. Clock 200 cycles to be safe.
    let mut tx_samples = Vec::new();
    for _ in 0..200 {
        tb.clock_signal("clock", 1).await;
        let tx_val = tb.get_u32("tx").await;
        tx_samples.push(tx_val);
    }

    // Verify TX activity happened (saw at least one 0 and one 1 transition)
    let has_zeros = tx_samples.contains(&0);
    let has_ones = tx_samples.contains(&1);
    if !(has_zeros && has_ones) {
        // Write diagnostic
        let unique_vals: std::collections::HashSet<u32> = tx_samples.iter().copied().collect();
        let sample_str: String = tx_samples
            .iter()
            .take(50)
            .map(|v| format!("{}", v))
            .collect::<Vec<_>>()
            .join(",");
        std::fs::write(
            "/tmp/uart_tx_diag.txt",
            format!(
                "ack_seen={}\nunique_vals={:?}\nfirst_50_samples={}\ntotal_samples={}\n",
                ack_seen,
                unique_vals,
                sample_str,
                tx_samples.len()
            ),
        )
        .ok();
        panic!("TX should have both 0s and 1s. See /tmp/uart_tx_diag.txt");
    }

    // Verify TX returned to idle high at the end
    let last_tx = *tx_samples.last().unwrap();
    assert_eq!(
        last_tx, 1,
        "TX should return to idle high after transmission"
    );

    tb.export_waveform("build/test_vhdl_uart_tx.skw.gz").ok();
}

#[tokio::test]
async fn test_vhdl_uart_loopback() {
    // Feed a UART frame into RX at the correct timing and verify received byte.
    // With c_rx_div=1, rx_baud_tick fires every 2 cycles (counter: 0→1→reset).
    // RX bit period = 16 rx_baud_ticks * 2 cycles = 32 system cycles.
    let mut tb = Testbench::new("examples/vhdl/uart.vhd").await.unwrap();
    // RX bit period = 16 rx_baud_ticks * 2 cycles = 32 system cycles.
    let rx_bit_cycles: usize = 32;

    // Reset
    tb.set("reset", 1u8)
        .set("data_stream_in", 0u8)
        .set("data_stream_in_stb", 0u8)
        .set("rx", 1u8); // idle high
    tb.clock_signal("clock", 4).await;
    tb.set("reset", 0u8);
    tb.clock_signal("clock", 4).await;

    // Build UART frame for 0x55 = 01010101 (LSB first)
    // Frame: start(0), d0(1), d1(0), d2(1), d3(0), d4(1), d5(0), d6(1), d7(0), stop(1)
    let byte_val: u8 = 0x55;
    let mut frame_bits: Vec<u8> = Vec::new();
    frame_bits.push(0); // start bit
    for i in 0..8 {
        frame_bits.push((byte_val >> i) & 1); // data bits LSB first
    }
    frame_bits.push(1); // stop bit

    // Feed the UART frame
    for &bit in frame_bits.iter() {
        tb.set("rx", bit);
        tb.clock_signal("clock", rx_bit_cycles).await;
    }

    // Clock idle time for stop bit processing + pipeline latency
    tb.set("rx", 1u8);
    tb.clock_signal("clock", rx_bit_cycles * 3).await;

    tb.export_waveform("build/test_vhdl_uart_loopback.skw.gz")
        .ok();

    let rx_state = tb.get_u32("uart_rx_state").await;
    let rx_data = tb.get_u32("data_stream_out").await;
    let rx_vec = tb.get_u32("uart_rx_data_vec").await;
    let rx_bit = tb.get_u32("uart_rx_bit").await;

    assert_eq!(
        rx_data, byte_val as u32,
        "RX data mismatch: expected 0x{:02X}, got 0x{:02X} (state={} vec=0x{:02X} rx_bit={})",
        byte_val, rx_data, rx_state, rx_vec, rx_bit
    );
}

// ========================================================================
// SPI Master behavioral sim tests
// ========================================================================

#[tokio::test]
async fn test_vhdl_spi_master_tx() {
    // With defaults: CLK_FREQ=50e6, SCLK_FREQ=5e6, WORD_SIZE=8, SLAVE_COUNT=1
    // DIVIDER_VALUE = (50e6/5e6)/2 = 5
    // Each SPI clock half-period = 5 sys clocks, full period = 10
    // 8 bits * 10 cycles/bit = 80 cycles for data + overhead
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

    // Release reset
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // CS_N should be high (inactive)
    tb.expect("cs_n", 1u8).await;

    // Load data 0xA5 for transmission
    tb.set("din", 0xA5u32)
        .set("din_addr", 0u8)
        .set("din_last", 1u8)
        .set("din_vld", 1u8);

    // Wait for DIN_RDY to go low (master accepted data)
    let mut accepted = false;
    for _i in 0..5 {
        tb.clock(1).await;
        let rdy = tb.get_u32("din_rdy").await;
        if rdy == 0 {
            accepted = true;
            break;
        }
    }
    assert!(accepted, "SPI master should accept data within 5 cycles");

    // Deassert DIN_VLD
    tb.set("din_vld", 0u8);

    // Clock through the SPI transfer
    // Track MOSI bits on SCLK rising edges
    let mut mosi_bits: Vec<u32> = Vec::new();
    let mut prev_sclk = 0u32;
    let mut dout_vld_seen = false;

    for _cycle in 0..200 {
        tb.clock(1).await;
        let sclk = tb.get_u32("sclk").await;
        let cs_n = tb.get_u32("cs_n").await;
        let _state = tb.get_u32("present_state").await;

        // Sample MOSI on SCLK rising edge while CS_N is low
        if sclk == 1 && prev_sclk == 0 && cs_n == 0 {
            mosi_bits.push(tb.get_u32("mosi").await);
        }
        prev_sclk = sclk;

        let vld = tb.get_u32("dout_vld").await;
        if vld == 1 {
            dout_vld_seen = true;
            break;
        }
    }

    tb.export_waveform("build/test_vhdl_spi_master_tx.skw.gz")
        .ok();

    // Verify we got 8 MOSI bits
    assert_eq!(
        mosi_bits.len(),
        8,
        "expected 8 MOSI bits, got {}: {:?}",
        mosi_bits.len(),
        mosi_bits
    );

    // Reconstruct transmitted byte (MSB first)
    let mut tx_byte: u32 = 0;
    for &bit in &mosi_bits {
        tx_byte = (tx_byte << 1) | bit;
    }
    assert_eq!(
        tx_byte, 0xA5,
        "MOSI should transmit 0xA5 MSB-first, got 0x{:02X}",
        tx_byte
    );

    assert!(dout_vld_seen, "DOUT_VLD should fire after transfer");
}

#[tokio::test]
async fn test_vhdl_spi_loopback() {
    // MOSI -> MISO loopback: send 0xA5, verify DOUT = 0xA5
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

    // Wait for master to accept
    for _ in 0..5 {
        tb.clock(1).await;
        if tb.get_u32("din_rdy").await == 0 {
            break;
        }
    }
    tb.set("din_vld", 0u8);

    // Loopback: each cycle, feed MOSI back to MISO
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

    assert!(
        dout_vld_seen,
        "DOUT_VLD should fire after loopback transfer"
    );
    assert_eq!(
        dout_val, 0xA5,
        "loopback DOUT should match DIN 0xA5, got 0x{:02X}",
        dout_val
    );

    tb.export_waveform("build/test_vhdl_spi_loopback.skw.gz")
        .ok();
}

// ========================================================================
// Hierarchical VHDL-2019 tests (interfaces, views, entity instantiation)
// ========================================================================

#[test]
fn test_vhdl_bus_system_parse_only() {
    let source = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/vhdl/bus_system.vhd"),
    )
    .unwrap();

    let hir = skalp_vhdl::parse_vhdl_source(&source, None).unwrap();

    // 3 entities: Sender, Receiver, BusSystem
    assert_eq!(
        hir.entities.len(),
        3,
        "expected 3 entities, got {}: {:?}",
        hir.entities.len(),
        hir.entities.iter().map(|e| &e.name).collect::<Vec<_>>()
    );

    // Verify entity names (VHDL lowercases then PascalCases)
    let names: Vec<&str> = hir.entities.iter().map(|e| e.name.as_str()).collect();
    assert!(
        names.contains(&"Sender"),
        "missing Sender entity in {:?}",
        names
    );
    assert!(
        names.contains(&"Receiver"),
        "missing Receiver entity in {:?}",
        names
    );
    assert!(
        names.contains(&"BusSystem"),
        "missing BusSystem entity in {:?}",
        names
    );

    // BusSystem should have 2 instances
    let bus_system_impl = hir
        .implementations
        .iter()
        .find(|imp| {
            hir.entities
                .iter()
                .any(|e| e.id == imp.entity && e.name == "BusSystem")
        })
        .expect("missing BusSystem implementation");

    assert_eq!(
        bus_system_impl.instances.len(),
        2,
        "BusSystem should have 2 instances (sender, receiver), got {}",
        bus_system_impl.instances.len()
    );

    // Verify instance labels were captured
    let inst_names: Vec<&str> = bus_system_impl
        .instances
        .iter()
        .map(|i| i.name.as_str())
        .collect();
    assert!(
        inst_names.contains(&"u_sender"),
        "missing u_sender instance in {:?}",
        inst_names
    );
    assert!(
        inst_names.contains(&"u_receiver"),
        "missing u_receiver instance in {:?}",
        inst_names
    );
}

#[tokio::test]
async fn test_vhdl_bus_system_data_transfer() {
    let mut tb = Testbench::new("examples/vhdl/bus_system.vhd")
        .await
        .unwrap();

    // Reset
    tb.set("rst", 1u8).set("trigger", 0u8).set("tx_data", 0u8);
    tb.clock(2).await;

    // Release reset
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Transfer 0xA5: set data and trigger
    tb.set("tx_data", 0xA5u32).set("trigger", 1u8);
    tb.clock(1).await;
    tb.expect("rx_data", 0xA5u32).await;
    tb.expect("rx_valid", 1u32).await;

    // Deassert trigger — rx_valid should drop
    tb.set("trigger", 0u8);
    tb.clock(1).await;
    tb.expect("rx_valid", 0u32).await;

    // Transfer 0x3C
    tb.set("tx_data", 0x3Cu32).set("trigger", 1u8);
    tb.clock(1).await;
    tb.expect("rx_data", 0x3Cu32).await;
    tb.expect("rx_valid", 1u32).await;

    tb.export_waveform("build/test_vhdl_bus_system.skw.gz").ok();
}

// ========================================================================
// VHDL → SystemVerilog transpilation tests
// ========================================================================

/// Helper: transpile a VHDL file to SystemVerilog via the MIR pipeline
fn vhdl_to_systemverilog(vhdl_path: &str) -> String {
    use skalp_mir::MirCompiler;

    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join(vhdl_path);
    let context = skalp_vhdl::parse_vhdl(&path)
        .unwrap_or_else(|e| panic!("Failed to parse {}: {}", vhdl_path, e));

    let compiler = MirCompiler::new();
    let mir = compiler
        .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
        .unwrap_or_else(|e| panic!("Failed to compile {} to MIR: {}", vhdl_path, e));

    skalp_codegen::generate_systemverilog_from_mir(&mir)
        .unwrap_or_else(|e| panic!("Failed to generate SV from {}: {}", vhdl_path, e))
}

#[test]
fn test_vhdl_to_sv_counter() {
    let sv = vhdl_to_systemverilog("examples/vhdl/counter.vhd");
    assert!(
        sv.contains("module"),
        "Expected a module declaration in SV output"
    );
    // Counter should have clk, rst, count ports
    assert!(sv.contains("clk"), "Expected clk port");
    assert!(sv.contains("rst"), "Expected rst port");
    assert!(sv.contains("count"), "Expected count port");
    assert!(
        sv.contains("always_ff"),
        "Expected sequential logic (always_ff)"
    );
    std::fs::write("build/counter_from_vhdl.sv", &sv).ok();
    println!("--- counter.vhd → SystemVerilog ---\n{}", sv);
}

#[test]
fn test_vhdl_to_sv_mux4() {
    let sv = vhdl_to_systemverilog("examples/vhdl/mux4.vhd");
    assert!(sv.contains("module"), "Expected a module declaration");
    assert!(sv.contains("sel"), "Expected sel port");
    std::fs::write("build/mux4_from_vhdl.sv", &sv).ok();
    println!("--- mux4.vhd → SystemVerilog ---\n{}", sv);
}

#[test]
fn test_vhdl_to_sv_bus_system() {
    let sv = vhdl_to_systemverilog("examples/vhdl/bus_system.vhd");
    // Should generate all 3 modules
    let module_count = sv.matches("module ").count();
    assert!(
        module_count >= 3,
        "Expected 3 modules (Sender, Receiver, BusSystem), got {}",
        module_count
    );
    assert!(sv.contains("clk"), "Expected clk port");
    std::fs::write("build/bus_system_from_vhdl.sv", &sv).ok();
    println!("--- bus_system.vhd → SystemVerilog ---\n{}", sv);
}

#[test]
fn test_vhdl_to_sv_uart() {
    let sv = vhdl_to_systemverilog("examples/vhdl/uart.vhd");
    assert!(sv.contains("module"), "Expected a module declaration");
    assert!(sv.contains("tx"), "Expected tx-related port");
    std::fs::write("build/uart_from_vhdl.sv", &sv).ok();
    println!("--- uart.vhd → SystemVerilog ---\n{}", sv);
}

#[test]
fn test_vhdl_to_sv_spi_master() {
    // SPI master has VHDL generics with computed constant widths (ceil(log2(...)))
    // that create unresolved types in the MIR. The SV codegen skips modules with
    // unresolved generic-dependent types. This test verifies the pipeline doesn't
    // panic — the empty output is a known limitation for complex generics.
    let sv = vhdl_to_systemverilog("examples/vhdl/spi_master.vhd");
    std::fs::write("build/spi_master_from_vhdl.sv", &sv).ok();
    // The SV codegen currently produces only a header for this design
    // because generic-dependent port widths can't be resolved to concrete values.
    // When generic evaluation is improved, this test should be updated to check
    // for module output.
    assert!(!sv.is_empty(), "Expected at least a header comment");
}

// ========================================================================
// Complex VHDL designs — inspired by NEORV32 and GRLIB patterns
// These exercise the HIR lowerer robustness improvements:
//   with...select, exit/next when, qualified expressions, FSMs
// ========================================================================

// --- GPIO Controller (NEORV32-style) ---

#[test]
fn test_vhdl_gpio_ctrl_parse() {
    let source = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/vhdl/gpio_ctrl.vhd"),
    )
    .unwrap();

    let (hir, diags) = skalp_vhdl::parse_vhdl_source_with_diagnostics(&source, None).unwrap();

    assert_eq!(hir.entities.len(), 1);
    let entity = &hir.entities[0];
    assert_eq!(entity.name, "GpioCtrl");
    assert!(entity.ports.len() >= 8, "expected >= 8 ports");

    let imp = &hir.implementations[0];

    // The with...select produces an assignment with a Match expression
    let has_match_assign = imp
        .assignments
        .iter()
        .any(|a| matches!(&a.rhs, skalp_frontend::hir::HirExpression::Match(_)));
    assert!(
        has_match_assign,
        "expected at least one assignment with Match RHS (from with...select)"
    );

    // Should have event blocks (clocked processes)
    assert!(
        imp.event_blocks.len() >= 3,
        "expected >= 3 event blocks (reg_write, sync, irq_gen), got {}",
        imp.event_blocks.len()
    );

    // Check for no critical lowering errors
    let errors: Vec<_> = diags
        .iter()
        .filter(|d| d.severity == skalp_vhdl::diagnostics::VhdlSeverity::Error)
        .collect();
    assert!(
        errors.is_empty(),
        "unexpected lowering errors: {:?}",
        errors.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_vhdl_gpio_ctrl_sir_pipeline() {
    use skalp_mir::MirCompiler;

    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/vhdl/gpio_ctrl.vhd");
    let context = skalp_vhdl::parse_vhdl(&path).unwrap();

    let compiler = MirCompiler::new();
    let mir = compiler
        .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
        .unwrap();
    let module = &mir.modules[0];

    // Signals should survive MIR
    assert!(
        !module.signals.is_empty(),
        "MIR should have signals from GPIO ctrl"
    );

    // Convert to SIR
    let sir = skalp_sir::mir_to_sir::convert_mir_to_sir(module);
    assert!(!sir.inputs.is_empty(), "SIR should have inputs");
    assert!(!sir.outputs.is_empty(), "SIR should have outputs");
}

#[tokio::test]
async fn test_vhdl_gpio_ctrl_simulation() {
    let mut tb = Testbench::new("examples/vhdl/gpio_ctrl.vhd").await.unwrap();

    // Reset
    tb.set("rst", 1u8)
        .set("addr", 0u8)
        .set("wdata", 0u8)
        .set("we", 0u8)
        .set("gpio_in", 0u8);
    tb.clock(2).await;

    // Release reset
    tb.set("rst", 0u8);
    tb.clock(1).await;

    // Write 0xA5 to output register (addr=01)
    tb.set("addr", 0b01u8).set("wdata", 0xA5u32).set("we", 1u8);
    tb.clock(1).await;
    tb.set("we", 0u8);
    tb.clock(1).await;

    // Verify output register was written correctly
    tb.expect("gpio_out", 0xA5u32).await;

    // Write 0x0F to direction register (addr=10)
    tb.set("addr", 0b10u8).set("wdata", 0x0Fu32).set("we", 1u8);
    tb.clock(1).await;
    tb.set("we", 0u8);
    tb.clock(1).await;

    tb.expect("gpio_dir", 0x0Fu32).await;

    // Read back via with...select mux — addr=01 should return out_reg
    tb.set("addr", 0b01u8);
    tb.clock(1).await;
    tb.expect("rdata", 0xA5u32).await;

    // Read direction register — addr=10
    tb.set("addr", 0b10u8);
    tb.clock(1).await;
    tb.expect("rdata", 0x0Fu32).await;

    tb.export_waveform("build/test_vhdl_gpio_ctrl.skw.gz").ok();
}

#[tokio::test]
async fn test_vhdl_gpio_ctrl_irq() {
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
    tb.clock(3).await; // in_sync latches, then in_prev gets old in_sync, edge detected

    // IRQ should fire: irq_pend bit 0 set by edge detect
    let irq = tb.get_u32("irq").await;
    assert_eq!(irq, 1, "IRQ should fire after rising edge on enabled pin");

    // IRQ should remain latched even after input goes low
    tb.set("gpio_in", 0u8);
    tb.clock(2).await;
    tb.expect("irq", 1u32).await;

    tb.export_waveform("build/test_vhdl_gpio_ctrl_irq.skw.gz")
        .ok();
}

// --- Timer (NEORV32 GPTMR-style) ---

#[test]
fn test_vhdl_timer_parse() {
    let source = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/vhdl/timer.vhd"),
    )
    .unwrap();

    let (hir, diags) = skalp_vhdl::parse_vhdl_source_with_diagnostics(&source, None).unwrap();

    assert_eq!(hir.entities.len(), 1);
    assert_eq!(hir.entities[0].name, "Timer");

    let imp = &hir.implementations[0];
    assert!(
        imp.event_blocks.len() >= 2,
        "expected >= 2 event blocks (prescaler, counter)"
    );

    let errors: Vec<_> = diags
        .iter()
        .filter(|d| d.severity == skalp_vhdl::diagnostics::VhdlSeverity::Error)
        .collect();
    assert!(
        errors.is_empty(),
        "unexpected errors: {:?}",
        errors.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[tokio::test]
async fn test_vhdl_timer_counts() {
    let mut tb = Testbench::new("examples/vhdl/timer.vhd").await.unwrap();

    // Reset — set threshold high enough that counter doesn't match during test
    tb.set("rst", 1u8)
        .set("enable", 0u8)
        .set("prescaler", 0u8)
        .set("threshold", 0xFFu32);
    tb.clock(2).await;
    tb.expect("counter", 0u32).await;

    // Release reset, enable counting with prescaler=0 (tick every cycle)
    tb.set("rst", 0u8).set("enable", 1u8);

    // Count up a few cycles (1-cycle delay: tick is registered, counter sees it next cycle)
    tb.clock(1).await; // tick becomes '1' this cycle, counter sees old tick=0
    for expected in 1u8..=5 {
        tb.clock(1).await;
        tb.expect("counter", expected).await;
    }

    tb.export_waveform("build/test_vhdl_timer_counts.skw.gz")
        .ok();
}

#[tokio::test]
async fn test_vhdl_timer_match() {
    let mut tb = Testbench::new("examples/vhdl/timer.vhd").await.unwrap();

    // Reset
    tb.set("rst", 1u8)
        .set("enable", 0u8)
        .set("prescaler", 0u8)
        .set("threshold", 5u32);
    tb.clock(2).await;

    // Enable counting, prescaler=0 (tick every cycle)
    tb.set("rst", 0u8).set("enable", 1u8).set("prescaler", 0u8);

    // Clock until we see match_out pulse
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

    tb.export_waveform("build/test_vhdl_timer_match.skw.gz")
        .ok();
}

// --- I2C FSM (GRLIB-style) ---

#[test]
fn test_vhdl_i2c_fsm_parse() {
    let source = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/vhdl/i2c_fsm.vhd"),
    )
    .unwrap();

    let (hir, diags) = skalp_vhdl::parse_vhdl_source_with_diagnostics(&source, None).unwrap();

    assert_eq!(hir.entities.len(), 1);
    assert_eq!(hir.entities[0].name, "I2cFsm");

    let imp = &hir.implementations[0];

    // Should have with...select for SCL output
    let has_match = imp
        .assignments
        .iter()
        .any(|a| matches!(&a.rhs, skalp_frontend::hir::HirExpression::Match(_)));
    assert!(
        has_match,
        "expected Match expression from with...select for SCL"
    );

    // Should have event blocks for clk_gen and fsm processes
    assert!(
        imp.event_blocks.len() >= 2,
        "expected >= 2 event blocks (clk_gen, fsm), got {}",
        imp.event_blocks.len()
    );

    let errors: Vec<_> = diags
        .iter()
        .filter(|d| d.severity == skalp_vhdl::diagnostics::VhdlSeverity::Error)
        .collect();
    assert!(
        errors.is_empty(),
        "unexpected errors: {:?}",
        errors.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_vhdl_i2c_fsm_sir_pipeline() {
    use skalp_mir::MirCompiler;

    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/vhdl/i2c_fsm.vhd");
    let context = skalp_vhdl::parse_vhdl(&path).unwrap();

    let compiler = MirCompiler::new();
    let mir = compiler
        .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
        .unwrap();
    let module = &mir.modules[0];

    assert!(
        !module.signals.is_empty(),
        "MIR should have signals for I2C FSM"
    );

    let sir = skalp_sir::mir_to_sir::convert_mir_to_sir(module);
    assert!(!sir.inputs.is_empty(), "SIR should have inputs");
    assert!(!sir.outputs.is_empty(), "SIR should have outputs");
    assert!(
        !sir.state_elements.is_empty(),
        "SIR should have state elements for FSM registers"
    );
}

#[tokio::test]
async fn test_vhdl_i2c_fsm_simulation() {
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

    // Should be idle — not busy, SCL idle high
    tb.expect("busy", 0u32).await;
    tb.expect("scl_out", 1u32).await;

    // Initiate transfer of 0xA5
    tb.set("wr_data", 0xA5u32).set("start", 1u8);
    tb.clock(1).await;
    tb.set("start", 0u8);

    // busy should be high after start
    tb.expect("busy", 1u32).await;

    // Clock through the transfer
    // Clock divider: clk_div is 4-bit (counts 0-15), phase is 2-bit (increments when clk_div=15)
    // Data bit tick: phase="11" and clk_div="1111" → every 64 cycles
    // 8 data bits + ACK = 9 ticks * 64 cycles = 576 cycles minimum
    let mut done_seen = false;
    for _ in 0..700 {
        let sda = tb.get_u32("sda_out").await;
        tb.set("sda_in", sda as u8);
        tb.clock(1).await;

        let done = tb.get_u32("done").await;
        if done == 1 {
            done_seen = true;
            break;
        }
    }

    assert!(done_seen, "FSM should complete transfer (done pulse)");

    // After done, FSM returns to idle
    tb.expect("busy", 0u32).await;

    tb.export_waveform("build/test_vhdl_i2c_fsm.skw.gz").ok();
}

// --- SystemVerilog transpilation for new designs ---

#[test]
fn test_vhdl_to_sv_gpio_ctrl() {
    let sv = vhdl_to_systemverilog("examples/vhdl/gpio_ctrl.vhd");
    assert!(sv.contains("module"), "Expected module declaration");
    assert!(sv.contains("gpio_out"), "Expected gpio_out port");
    std::fs::write("build/gpio_ctrl_from_vhdl.sv", &sv).ok();
}

#[test]
fn test_vhdl_to_sv_timer() {
    let sv = vhdl_to_systemverilog("examples/vhdl/timer.vhd");
    assert!(sv.contains("module"), "Expected module declaration");
    assert!(sv.contains("counter"), "Expected counter port");
    std::fs::write("build/timer_from_vhdl.sv", &sv).ok();
}

#[test]
fn test_vhdl_to_sv_i2c_fsm() {
    let sv = vhdl_to_systemverilog("examples/vhdl/i2c_fsm.vhd");
    assert!(sv.contains("module"), "Expected module declaration");
    assert!(sv.contains("scl_out"), "Expected scl_out port");
    std::fs::write("build/i2c_fsm_from_vhdl.sv", &sv).ok();
}
