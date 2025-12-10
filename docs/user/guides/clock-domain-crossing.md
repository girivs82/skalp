# Clock Domain Crossing (CDC) Guide

This guide covers clock domain crossing in Skalp, including synchronizer types, domain annotation, and best practices for reliable multi-clock designs.

## Table of Contents

- [Overview](#overview)
- [CDC Fundamentals](#cdc-fundamentals)
- [Synchronizer Types](#synchronizer-types)
- [Domain Lifetime Syntax](#domain-lifetime-syntax)
- [Practical Examples](#practical-examples)
- [Common Patterns](#common-patterns)
- [CDC Verification](#cdc-verification)
- [Best Practices](#best-practices)

---

## Overview

Clock domain crossing (CDC) is required when signals pass between different clock domains. Without proper synchronization, metastability can cause unpredictable behavior.

Skalp provides:
- Automatic synchronizer generation
- Multiple synchronization strategies
- Domain annotation using lifetime syntax
- Compile-time CDC checking

---

## CDC Fundamentals

### What is Metastability?

When a signal changes close to a clock edge, the receiving flip-flop may enter an undefined state (metastability). This can propagate through logic, causing failures.

### Why Synchronize?

Synchronizers allow metastable states to resolve before the signal is used by downstream logic. Multiple flip-flop stages increase the mean time between failures (MTBF) exponentially.

### MTBF Calculation

```
MTBF = exp(tr / τ) / (f_clk × f_data × T_0)
```

Where:
- `tr` = resolution time (time between synchronizer stages)
- `τ` = flip-flop time constant
- `f_clk` = destination clock frequency
- `f_data` = data transition frequency
- `T_0` = metastability window

**Rule of thumb**: Each additional synchronizer stage increases MTBF by ~1000×

---

## Synchronizer Types

### Two-Flip-Flop Synchronizer (Default)

For single-bit signals that can change at any time:

```skalp
entity TwoFFExample {
    in clk_a: clock,
    in clk_b: clock,
    in async_signal: bit,
    out sync_signal: bit,

    // Default: 2-stage synchronizer
    #[cdc]
    signal synced: bit,

    // Explicit 2-stage
    #[cdc(sync_stages = 2)]
    signal synced_explicit: bit,
}

impl TwoFFExample {
    synced = async_signal;
    synced_explicit = async_signal;
    sync_signal = synced;
}
```

**Generated SystemVerilog:**
```systemverilog
// CDC Synchronizer: synced (2-stage)
(* ASYNC_REG = "TRUE" *)
reg synced_meta;
(* ASYNC_REG = "TRUE" *)
reg synced_sync;

always @(posedge clk_b or posedge reset) begin
    if (reset) begin
        synced_meta <= 1'b0;
        synced_sync <= 1'b0;
    end else begin
        synced_meta <= async_signal;
        synced_sync <= synced_meta;
    end
end
assign synced = synced_sync;
```

### Three-Stage Synchronizer

For higher reliability or extreme clock frequencies:

```skalp
entity ThreeStageExample {
    in fast_clk: clock,      // 500 MHz
    in slow_clk: clock,      // 100 MHz
    in async_in: bit,
    out sync_out: bit,

    // 3-stage for high-speed designs
    #[cdc(sync_stages = 3)]
    signal high_reliability: bit,
}
```

### Gray Code Synchronizer

For multi-bit values that change by only one bit at a time (counters, pointers):

```skalp
entity GrayCodeSync {
    in wr_clk: clock,
    in rd_clk: clock,
    in wr_ptr: bit[8],
    out rd_ptr_sync: bit[8],

    // Gray code synchronizer for FIFO pointers
    #[cdc(cdc_type = gray, sync_stages = 2)]
    signal wr_ptr_gray: bit[8],
}

impl GrayCodeSync {
    // Convert to Gray code before crossing
    wr_ptr_gray = wr_ptr ^ (wr_ptr >> 1);

    // After synchronization, convert back to binary
    rd_ptr_sync = gray_to_binary(wr_ptr_gray);
}
```

**Gray Code Properties:**
- Only one bit changes between consecutive values
- Safe for multi-bit CDC (no intermediate glitch states)
- Perfect for FIFO read/write pointers

### Pulse Synchronizer

For single-cycle pulses that may be missed by slow clocks:

```skalp
entity PulseSync {
    in clk_fast: clock,
    in clk_slow: clock,
    in pulse_in: bit,       // Single-cycle pulse in fast domain
    out pulse_out: bit,      // Pulse in slow domain

    #[cdc(cdc_type = pulse)]
    signal pulse_sync: bit,
}

impl PulseSync {
    pulse_sync = pulse_in;
    pulse_out = pulse_sync;
}
```

**How it works:**
1. Toggle flip-flop in source domain on each pulse
2. Synchronize toggle signal to destination domain
3. Edge detect in destination domain to recreate pulse

**Important**: Input pulses must be separated by at least 2 destination clock cycles.

### Handshake Synchronizer

For request/acknowledge protocols where data validity matters:

```skalp
entity HandshakeSync {
    in clk_src: clock,
    in clk_dst: clock,
    in req: bit,
    in data: bit[32],
    out ack: bit,
    out data_valid: bit,
    out data_out: bit[32],

    #[cdc(cdc_type = handshake)]
    signal req_sync: bit,

    signal data_captured: bit[32],
}

impl HandshakeSync {
    // Handshake protocol:
    // 1. Source asserts req with valid data
    // 2. req is synchronized to destination
    // 3. Destination captures data, asserts ack
    // 4. ack is synchronized back to source
    // 5. Source deasserts req
    // 6. Destination deasserts ack

    req_sync = req;

    if req_sync && !data_valid {
        data_captured = data;
        data_valid = 1;
    }

    data_out = data_captured;
}
```

### Async FIFO

For streaming data between clock domains:

```skalp
entity AsyncFifoExample {
    in wr_clk: clock,
    in rd_clk: clock,
    in wr_en: bit,
    in wr_data: bit[32],
    in rd_en: bit,
    out rd_data: bit[32],
    out full: bit,
    out empty: bit,

    #[cdc(cdc_type = async_fifo)]
    signal fifo_data: bit[32],

    // Internal FIFO storage
    #[memory(depth = 16, style = distributed)]
    signal mem: bit[32][16],

    // Gray-coded pointers
    #[cdc(cdc_type = gray)]
    signal wr_ptr: bit[5],

    #[cdc(cdc_type = gray)]
    signal rd_ptr: bit[5],
}
```

---

## Domain Lifetime Syntax

Skalp uses Rust-style lifetime annotations to explicitly mark clock domains:

### Declaring Clock Domains

```skalp
entity MultiClockDesign {
    // Clock inputs define domains
    in clk_fast: clock,     // Defines 'clk_fast domain
    in clk_slow: clock,     // Defines 'clk_slow domain

    // Signals with explicit domain
    signal fast_data: logic<'clk_fast>[32],
    signal slow_data: logic<'clk_slow>[32],
}
```

### Explicit CDC Annotation

```skalp
entity ExplicitCDC {
    in clk_100mhz: clock,
    in clk_250mhz: clock,
    in data_in: bit[16],
    out data_out: bit[16],

    // Signal in 100MHz domain
    signal src_data: logic<'clk_100mhz>[16],

    // CDC with explicit domains
    #[cdc(from = 'clk_100mhz, to = 'clk_250mhz, sync_stages = 2)]
    signal crossing: logic<'clk_250mhz>[16],
}

impl ExplicitCDC {
    src_data = data_in;
    crossing = src_data;  // CDC happens here
    data_out = crossing;
}
```

### Domain Checking

The compiler verifies CDC correctness:

```skalp
entity CDCCheck {
    in clk_a: clock,
    in clk_b: clock,

    signal sig_a: logic<'clk_a>[8],
    signal sig_b: logic<'clk_b>[8],
}

impl CDCCheck {
    // ERROR: Cross-domain assignment without #[cdc]
    // sig_b = sig_a;  // Compiler error!

    // CORRECT: With CDC annotation
    #[cdc(from = 'clk_a, to = 'clk_b)]
    signal temp: logic<'clk_b>[8],

    temp = sig_a;  // OK - CDC synchronizer inserted
    sig_b = temp;
}
```

---

## Practical Examples

### UART Clock Domain Bridge

```skalp
entity UartBridge {
    in sys_clk: clock,       // 100 MHz system clock
    in uart_clk: clock,      // 115200 baud (derived)
    in rx_data: bit[8],
    in rx_valid: bit,
    out tx_data: bit[8],
    out tx_valid: bit,

    // RX path: UART -> System
    #[cdc(from = 'uart_clk, to = 'sys_clk, cdc_type = handshake)]
    signal rx_sync_valid: bit,

    #[cdc(from = 'uart_clk, to = 'sys_clk)]
    signal rx_sync_data: bit[8],

    // TX path: System -> UART
    #[cdc(from = 'sys_clk, to = 'uart_clk, cdc_type = handshake)]
    signal tx_sync_valid: bit,

    signal tx_buffer: bit[8],
}

impl UartBridge {
    // RX synchronization
    if rx_valid {
        rx_sync_data = rx_data;
        rx_sync_valid = 1;
    }
    tx_data = rx_sync_data;
    tx_valid = rx_sync_valid;

    // TX synchronization
    tx_sync_valid = tx_valid;
}
```

### Async FIFO for Video Pipeline

```skalp
entity VideoFifo {
    in pixel_clk: clock,     // 148.5 MHz (1080p)
    in proc_clk: clock,      // 200 MHz processing
    in pixel_data: bit[24],
    in pixel_valid: bit,
    out proc_data: bit[24],
    out proc_valid: bit,

    // Write pointer (pixel clock domain)
    signal wr_ptr: bit[10],

    // Read pointer (processing clock domain)
    signal rd_ptr: bit[10],

    // Gray-coded pointer synchronization
    #[cdc(from = 'pixel_clk, to = 'proc_clk, cdc_type = gray)]
    signal wr_ptr_sync: bit[10],

    #[cdc(from = 'proc_clk, to = 'pixel_clk, cdc_type = gray)]
    signal rd_ptr_sync: bit[10],

    // FIFO memory
    #[memory(depth = 1024, width = 24, style = block)]
    signal fifo_mem: bit[24][1024],

    // Status signals
    signal full: bit,
    signal empty: bit,
}

impl VideoFifo {
    // Write side (pixel clock)
    if pixel_valid && !full {
        fifo_mem[wr_ptr[9:0]] = pixel_data;
        wr_ptr = wr_ptr + 1;
    }

    // Read side (processing clock)
    if !empty {
        proc_data = fifo_mem[rd_ptr[9:0]];
        proc_valid = 1;
        rd_ptr = rd_ptr + 1;
    } else {
        proc_valid = 0;
    }

    // Status (using synchronized pointers)
    full = (wr_ptr[9:0] == rd_ptr_sync[9:0]) &&
           (wr_ptr[10] != rd_ptr_sync[10]);
    empty = wr_ptr_sync == rd_ptr;
}
```

### Multi-Clock SoC Interconnect

```skalp
entity SoCInterconnect {
    in cpu_clk: clock,       // 1 GHz CPU
    in mem_clk: clock,       // 800 MHz DDR
    in peri_clk: clock,      // 100 MHz peripherals

    // CPU interface
    in cpu_req: bit,
    in cpu_addr: bit[32],
    in cpu_wdata: bit[64],
    out cpu_rdata: bit[64],
    out cpu_ack: bit,

    // Memory interface
    out mem_cmd: bit[4],
    out mem_addr: bit[32],
    out mem_wdata: bit[64],
    in mem_rdata: bit[64],
    in mem_ready: bit,

    // CPU -> Memory path
    #[cdc(from = 'cpu_clk, to = 'mem_clk, cdc_type = handshake)]
    signal cpu_to_mem_req: bit,

    #[cdc(from = 'cpu_clk, to = 'mem_clk)]
    signal cpu_to_mem_addr: bit[32],

    #[cdc(from = 'cpu_clk, to = 'mem_clk)]
    signal cpu_to_mem_wdata: bit[64],

    // Memory -> CPU path
    #[cdc(from = 'mem_clk, to = 'cpu_clk)]
    signal mem_to_cpu_rdata: bit[64],

    #[cdc(from = 'mem_clk, to = 'cpu_clk, cdc_type = pulse)]
    signal mem_to_cpu_ack: bit,
}
```

---

## Common Patterns

### Pattern 1: Reset Synchronization

Always synchronize async resets:

```skalp
entity ResetSync {
    in clk: clock,
    in async_reset_n: bit,
    out sync_reset_n: bit,

    // Synchronize async reset with de-assertion
    #[cdc(sync_stages = 3)]
    signal reset_sync: bit,
}

impl ResetSync {
    // Reset asserts immediately, de-asserts synchronously
    reset_sync = async_reset_n;
    sync_reset_n = reset_sync;
}
```

### Pattern 2: Bus Synchronization with Valid

For buses, use handshake or valid-based synchronization:

```skalp
entity BusSync {
    in src_clk: clock,
    in dst_clk: clock,
    in src_data: bit[32],
    in src_valid: bit,
    out dst_data: bit[32],
    out dst_valid: bit,

    // Synchronize valid signal
    #[cdc(from = 'src_clk, to = 'dst_clk)]
    signal valid_sync: bit,

    // Data registered when valid
    signal data_hold: bit[32],
}

impl BusSync {
    // Hold data stable while valid transfers
    if src_valid {
        data_hold = src_data;
    }

    valid_sync = src_valid;

    // Data is stable by the time valid arrives
    dst_valid = valid_sync;
    dst_data = data_hold;  // Safe - data was stable before valid
}
```

### Pattern 3: Level vs Edge Detection

Choose based on signal characteristics:

```skalp
entity LevelVsEdge {
    in clk_src: clock,
    in clk_dst: clock,

    // Level signal - use standard sync
    in enable: bit,
    out enable_sync: bit,

    // Pulse signal - use pulse sync
    in trigger: bit,
    out trigger_sync: bit,

    #[cdc]
    signal level_sync: bit,

    #[cdc(cdc_type = pulse)]
    signal edge_sync: bit,
}

impl LevelVsEdge {
    level_sync = enable;
    enable_sync = level_sync;

    edge_sync = trigger;
    trigger_sync = edge_sync;
}
```

---

## CDC Verification

### Static CDC Analysis

Skalp performs compile-time CDC checking:

```bash
# Check for CDC issues
skalp check --cdc-analysis design.sk

# Verbose CDC report
skalp check --cdc-analysis --verbose design.sk
```

### Common CDC Errors

```
error[CDC001]: Unsynchronized clock domain crossing
  --> design.sk:45:5
   |
45 |     dst_signal = src_signal;
   |     ^^^^^^^^^^^^^^^^^^^^^^^^
   |
   = note: src_signal is in domain 'clk_a
   = note: dst_signal is in domain 'clk_b
   = help: add #[cdc(from = 'clk_a, to = 'clk_b)]

warning[CDC002]: Multi-bit signal without Gray encoding
  --> design.sk:50:5
   |
50 |     #[cdc]
51 |     signal counter_sync: bit[8];
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   |
   = help: consider #[cdc(cdc_type = gray)] for multi-bit signals

error[CDC003]: Reconvergent clock domains
  --> design.sk:60:5
   |
60 |     result = sync_a & sync_b;
   |              ^^^^^^^^^^^^^^^
   |
   = note: sync_a and sync_b cross from same source
   = note: may arrive at different times
   = help: use single synchronizer for combined signal
```

---

## Best Practices

### 1. Synchronize Control, Not Data

```skalp
// BAD: Synchronizing wide data bus
#[cdc]
signal data_sync: bit[128];  // Risky - 128 synchronizers

// GOOD: Synchronize valid, use data hold
#[cdc]
signal valid_sync: bit;
signal data_hold: bit[128];  // Held stable, no sync needed
```

### 2. Use Appropriate Sync Stages

| Frequency | Recommended Stages |
|-----------|-------------------|
| < 100 MHz | 2 stages |
| 100-300 MHz | 2-3 stages |
| > 300 MHz | 3+ stages |

### 3. Avoid Reconvergent Paths

```skalp
// BAD: Same signal synchronized twice
#[cdc]
signal sync_a: bit,

#[cdc]
signal sync_b: bit,

// These may arrive at different times!
result = sync_a & sync_b;  // Potential glitch

// GOOD: Synchronize once, use result
#[cdc]
signal sync_combined: bit,

sync_combined = signal_a & signal_b;  // Combine before sync
```

### 4. Document CDC Assumptions

```skalp
entity DocumentedCDC {
    // CDC ASSUMPTIONS:
    // - cpu_clk and mem_clk are asynchronous
    // - cpu_req pulse width > 2 mem_clk cycles
    // - Data stable 1 cpu_clk before req assertion

    #[cdc(from = 'cpu_clk, to = 'mem_clk, cdc_type = pulse)]
    signal req_sync: bit,
}
```

### 5. Gray Code for Pointers

```skalp
// ALWAYS use Gray code for FIFO pointers
#[cdc(cdc_type = gray)]
signal wr_ptr_sync: bit[10],

#[cdc(cdc_type = gray)]
signal rd_ptr_sync: bit[10],
```

---

## See Also

- [Attributes Reference](../reference/attributes.md)
- [Power Intent Guide](power-intent.md)
- [Memory Synthesis Guide](memory-synthesis.md)
