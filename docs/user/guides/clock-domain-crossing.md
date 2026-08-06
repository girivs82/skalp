# Clock Domain Crossing (CDC) Guide

This guide covers clock domain crossing in Skalp: how the compiler's CDC analysis works, how to write synchronizers, and best practices for reliable multi-clock designs.

> **What the compiler does — and does not — do**
>
> Skalp performs real, compile-time CDC *analysis*: every cross-domain read is
> detected and reported with a severity (CRITICAL / WARNING / INFO), and an
> unsynchronized crossing that feeds logic **fails the build**. The `#[cdc]`
> attribute is a **verification annotation**: it marks a register you wrote as
> the first stage of a synchronizer, downgrades the report for that crossing,
> and emits an `(* ASYNC_REG = "TRUE" *)` placement attribute on it.
>
> The compiler does **not** insert synchronizer hardware for you. You write the
> flip-flop stages yourself; the analysis checks you did.

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
- Compile-time CDC analysis on every build — clock domains are keyed by the clock port that drives each process, so the analysis works even without any annotations
- Three report severities: **CRITICAL** (unsynchronized crossing into logic — the build fails), **WARNING** (a plain registered sample — builds, but you are told to check the chain), **INFO** (a crossing you have annotated with `#[cdc]`)
- The `#[cdc]` annotation for hand-written synchronizers, which emits `ASYNC_REG` placement attributes in the generated SystemVerilog
- Optional domain lifetime annotations (`<'domain>`) that name domains explicitly and flow into the analysis

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

All of the synchronizers below are written explicitly in Skalp. The `#[cdc]`
attribute goes on the first (metastable) stage; it tells the CDC analysis that
the crossing is intentional and marks the register `ASYNC_REG` in the output.

### Two-Flip-Flop Synchronizer

For single-bit signals that can change at any time:

```skalp
entity TwoFFSync {
    in clk_dst: clock;
    in async_in: bit;
    out sync_out: bit;
}

impl TwoFFSync {
    // You write the stages yourself; #[cdc] marks the crossing for analysis
    #[cdc]
    signal meta: bit = 0;
    signal sync: bit = 0;

    on(clk_dst.rise) {
        meta = async_in;
        sync = meta;
    }

    sync_out = sync;
}
```

**Generated SystemVerilog** (note the `ASYNC_REG` attribute emitted by `#[cdc]`):

```systemverilog
    // CDC: async -> sync, type=TwoFF, sync_stages=2
    (* ASYNC_REG = "TRUE" *)
    reg meta = 0;
    reg sync = 0;

    assign sync_out = sync;

    always_ff @(posedge clk_dst) begin
        meta <= async_in;
        sync <= meta;
    end
```

### Three-Stage Synchronizer

For higher reliability or extreme clock frequencies, add a third stage. The
`sync_stages` parameter of `#[cdc(...)]` is accepted and recorded as metadata
in the output, but it does not generate the stages — write all three:

```skalp
entity ThreeStageSync {
    in clk_dst: clock;
    in async_in: bit;
    out sync_out: bit;
}

impl ThreeStageSync {
    #[cdc(sync_stages = 3)]
    signal meta: bit = 0;
    signal sync1: bit = 0;
    signal sync2: bit = 0;

    on(clk_dst.rise) {
        meta = async_in;
        sync1 = meta;
        sync2 = sync1;
    }

    sync_out = sync2;
}
```

### Gray Code Synchronizer

For multi-bit values that change by only one bit at a time (counters, pointers):

```skalp
entity GrayCodeSync {
    in wr_clk: clock;
    in rd_clk: clock;
    in wr_ptr: bit[8];
    out rd_ptr_sync: bit[8];
}

impl GrayCodeSync {
    // Convert to Gray code before crossing (registered in the write domain)
    signal wr_ptr_gray: bit[8] = 0;

    // Explicit 2-stage synchronizer in the read domain
    #[cdc]
    signal gray_meta: bit[8] = 0;
    signal gray_sync: bit[8] = 0;

    on(wr_clk.rise) {
        wr_ptr_gray = wr_ptr ^ (wr_ptr >> 1);
    }

    on(rd_clk.rise) {
        gray_meta = wr_ptr_gray;
        gray_sync = gray_meta;
    }

    // Gray -> binary: XOR prefix fold
    rd_ptr_sync = gray_sync ^ (gray_sync >> 1) ^ (gray_sync >> 2) ^ (gray_sync >> 3)
                ^ (gray_sync >> 4) ^ (gray_sync >> 5) ^ (gray_sync >> 6) ^ (gray_sync >> 7);
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
    in clk_src: clock;
    in clk_dst: clock;
    in pulse_in: bit;
    out pulse_out: bit;
}

impl PulseSync {
    // 1. Toggle flip-flop in the source domain on each pulse
    signal toggle: bit = 0;

    // 2. Synchronize the toggle into the destination domain
    #[cdc]
    signal meta: bit = 0;
    signal sync: bit = 0;
    signal sync_d: bit = 0;

    on(clk_src.rise) {
        if pulse_in {
            toggle = toggle ^ 1;
        }
    }

    on(clk_dst.rise) {
        meta = toggle;
        sync = meta;
        sync_d = sync;
    }

    // 3. Edge-detect to recreate a single-cycle pulse
    pulse_out = sync ^ sync_d;
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
    in clk_src: clock;
    in clk_dst: clock;
    in req: bit;
    in data: bit[32];
    out ack: bit;
    out data_valid: bit;
    out data_out: bit[32];
}

impl HandshakeSync {
    // req synchronized into the destination domain
    #[cdc]
    signal req_meta: bit = 0;
    signal req_sync: bit = 0;

    // ack synchronized back into the source domain
    #[cdc]
    signal ack_meta: bit = 0;
    signal ack_sync: bit = 0;

    signal data_captured: bit[32] = 0;
    signal valid_reg: bit = 0;

    on(clk_dst.rise) {
        req_meta = req;
        req_sync = req_meta;

        if req_sync && !valid_reg {
            data_captured = data;
            valid_reg = 1;
        } else if !req_sync {
            valid_reg = 0;
        }
    }

    on(clk_src.rise) {
        ack_meta = valid_reg;
        ack_sync = ack_meta;
    }

    ack = ack_sync;
    data_valid = valid_reg;
    data_out = data_captured;
}
```

**The protocol:**
1. Source asserts `req` with valid data
2. `req` is synchronized to the destination
3. Destination captures data, asserts `ack`
4. `ack` is synchronized back to the source
5. Source deasserts `req`; destination then deasserts `ack`

### Async FIFO

For streaming data between clock domains, combine the Gray-code pointer
synchronizers above with a dual-port memory (see the
[Memory Synthesis Guide](memory-synthesis.md)). A complete async FIFO is a
composition of pieces you have already seen:

- one write pointer, Gray-coded and synchronized into the read domain
- one read pointer, Gray-coded and synchronized into the write domain
- a `#[memory]` array written in the write domain and read in the read domain
- full/empty flags computed from the local pointer and the synchronized
  opposite pointer

The [Async FIFO for Video Pipeline](#async-fifo-for-video-pipeline) example
below sketches the structure.

---

## Domain Lifetime Syntax

Skalp uses Rust-style lifetime annotations to explicitly name clock domains.
Named domains parse and flow into the CDC analysis. (Even without them, every
process already belongs to the domain of the clock that drives it.)

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

    // Annotated crossing signal in the 250MHz domain
    #[cdc(from = 'clk_100mhz, to = 'clk_250mhz, sync_stages = 2)]
    signal crossing: logic<'clk_250mhz>[16],
}

impl ExplicitCDC {
    src_data = data_in;
    crossing = src_data;  // the annotated crossing point
    data_out = crossing;
}
```

### Domain Checking

The compiler analyzes every cross-domain read. An unsynchronized crossing that
feeds logic is a CRITICAL violation and fails the build; a plain registered
sample builds with a WARNING; an annotated crossing is reported as INFO:

```skalp
entity CDCCheck {
    in clk_a: clock;
    in clk_b: clock;
    in d: bit[8];
    out q: bit[8];
}

impl CDCCheck {
    signal sig_a: bit[8] = 0;

    // CRITICAL (build fails): cross-domain read feeding logic, no synchronizer:
    // on(clk_b.rise) { sig_b = sig_a + 1; }

    // OK: explicit 2-FF sample, annotated with #[cdc] — reported as INFO
    #[cdc]
    signal meta: bit[8] = 0;
    signal sig_b: bit[8] = 0;

    on(clk_a.rise) {
        sig_a = d;
    }

    on(clk_b.rise) {
        meta = sig_a;
        sig_b = meta;
    }

    q = sig_b;
}
```

---

## Practical Examples

### UART Clock Domain Bridge

A valid+data bridge from a UART-rate domain into the system domain. Only the
valid flag is synchronized; the data bus is held stable while the flag
transfers (see [Best Practices](#best-practices)):

```skalp
entity UartBridge {
    in sys_clk: clock;
    in uart_clk: clock;
    in rx_data: bit[8];
    in rx_valid: bit;
    out sys_data: bit[8];
    out sys_valid: bit;
}

impl UartBridge {
    // Data held stable in the source domain while valid transfers
    signal rx_hold: bit[8] = 0;
    signal rx_flag: bit = 0;

    // valid flag synchronized into the system domain
    #[cdc]
    signal valid_meta: bit = 0;
    signal valid_sync: bit = 0;

    on(uart_clk.rise) {
        if rx_valid {
            rx_hold = rx_data;
            rx_flag = 1;
        }
    }

    on(sys_clk.rise) {
        valid_meta = rx_flag;
        valid_sync = valid_meta;
    }

    sys_valid = valid_sync;
    sys_data = rx_hold;  // stable by the time valid_sync asserts
}
```

### Async FIFO for Video Pipeline

Structural sketch of an async FIFO (ports and storage). The pointer
synchronizers are the Gray-code pattern from above, one in each direction:

```skalp
entity VideoFifo {
    in pixel_clk: clock,     // 148.5 MHz (1080p)
    in proc_clk: clock,      // 200 MHz processing
    in pixel_data: bit[24],
    in pixel_valid: bit,
    out proc_data: bit[24],
    out proc_valid: bit,

    // Write pointer (pixel clock domain), Gray-synchronized into proc_clk
    signal wr_ptr: bit[10],

    // Read pointer (proc clock domain), Gray-synchronized into pixel_clk
    signal rd_ptr: bit[10],

    // FIFO memory
    #[memory(depth = 1024, width = 24, style = block)]
    signal fifo_mem: bit[24][1024],
}
```

Full/empty are computed from the local pointer and the synchronized copy of
the opposite pointer — never from two unsynchronized pointers.

### Multi-Clock SoC Interconnect

Port map of a three-domain interconnect. Each arrow in the comments is one of
the synchronizer patterns above (handshake for request/ack, hold-and-flag for
the buses):

```skalp
entity SoCInterconnect {
    in cpu_clk: clock,       // 1 GHz CPU
    in mem_clk: clock,       // 800 MHz DDR
    in peri_clk: clock,      // 100 MHz peripherals

    // CPU interface (cpu_clk domain)
    in cpu_req: bit,
    in cpu_addr: bit[32],
    in cpu_wdata: bit[64],
    out cpu_rdata: bit[64],
    out cpu_ack: bit,

    // Memory interface (mem_clk domain)
    out mem_cmd: bit[4],
    out mem_addr: bit[32],
    out mem_wdata: bit[64],
    in mem_rdata: bit[64],
    in mem_ready: bit,

    // CPU -> Memory: handshake-synchronized request,
    //               addr/wdata held stable during the handshake
    // Memory -> CPU: pulse-synchronized ready, rdata held stable
}
```

---

## Common Patterns

### Pattern 1: Reset Synchronization

Always synchronize async reset de-assertion:

```skalp
entity ResetSync {
    in clk: clock;
    in async_reset_n: bit;
    out sync_reset_n: bit;
}

impl ResetSync {
    // Reset asserts immediately (async), de-asserts synchronously
    #[cdc(sync_stages = 3)]
    signal reset_meta: bit = 0;
    signal reset_s1: bit = 0;
    signal reset_s2: bit = 0;

    on(clk.rise) {
        reset_meta = async_reset_n;
        reset_s1 = reset_meta;
        reset_s2 = reset_s1;
    }

    sync_reset_n = reset_s2;
}
```

### Pattern 2: Bus Synchronization with Valid

For buses, synchronize a single valid flag and hold the data stable:

```skalp
entity BusSync {
    in src_clk: clock;
    in dst_clk: clock;
    in src_data: bit[32];
    in src_valid: bit;
    out dst_data: bit[32];
    out dst_valid: bit;
}

impl BusSync {
    // Hold data stable while valid transfers
    signal data_hold: bit[32] = 0;
    signal valid_flag: bit = 0;

    #[cdc]
    signal valid_meta: bit = 0;
    signal valid_sync: bit = 0;

    on(src_clk.rise) {
        if src_valid {
            data_hold = src_data;
            valid_flag = 1;
        }
    }

    on(dst_clk.rise) {
        valid_meta = valid_flag;
        valid_sync = valid_meta;
    }

    dst_valid = valid_sync;
    dst_data = data_hold;  // Safe - data was stable before valid
}
```

### Pattern 3: Level vs Edge Detection

Choose based on signal characteristics: a level (like an enable) can go
through a plain 2-FF synchronizer; a single-cycle pulse needs the
toggle-and-edge-detect pulse synchronizer:

```skalp
entity LevelVsEdge {
    in clk_src: clock;
    in clk_dst: clock;

    // Level signal - 2-FF sync is enough
    in enable: bit;
    out enable_sync: bit;

    // Pulse signal - toggle + edge detect
    in trigger: bit;
    out trigger_sync: bit;
}

impl LevelVsEdge {
    // Level: plain 2-FF
    #[cdc]
    signal en_meta: bit = 0;
    signal en_sync: bit = 0;

    // Pulse: toggle in source domain, edge-detect in destination
    signal tog: bit = 0;
    #[cdc]
    signal tog_meta: bit = 0;
    signal tog_sync: bit = 0;
    signal tog_d: bit = 0;

    on(clk_src.rise) {
        if trigger {
            tog = tog ^ 1;
        }
    }

    on(clk_dst.rise) {
        en_meta = enable;
        en_sync = en_meta;

        tog_meta = tog;
        tog_sync = tog_meta;
        tog_d = tog_sync;
    }

    enable_sync = en_sync;
    trigger_sync = tog_sync ^ tog_d;
}
```

---

## CDC Verification

### Static CDC Analysis

CDC analysis runs automatically as part of every build — there is no separate
command or flag:

```bash
skalp build design.sk -o build/
```

Every cross-domain read is classified and reported. A summary line gives the
counts:

```
CDC analysis: 0 critical, 1 warning(s), 0 info
```

### Report Severities

Actual compiler output for the three cases:

**CRITICAL — unsynchronized crossing into logic. The build fails:**

```
CDC CRITICAL: direct clock-domain crossing [CritCross]: assignment to `dst` reads a signal
from domain clock `clk_a` inside a process clocked in domain clock `clk_b` — needs a synchronizer
CDC analysis: 1 critical, 0 warning(s), 0 info
Error: Failed to compile HIR to MIR with CDC analysis: Compilation failed due to 1 critical CDC violation(s)
```

**WARNING — a bare registered sample. The build succeeds, but check your chain:**

```
CDC WARNING: direct clock-domain crossing [CrossRead]: assignment to `meta` reads a signal
from domain clock `clk_a` inside a process clocked in domain clock `clk_b` — registered sample —
ensure a >=2-stage chain or add #[cdc]
CDC analysis: 0 critical, 1 warning(s), 0 info
```

**INFO — a crossing annotated with `#[cdc]`:**

```
CDC INFO: direct clock-domain crossing [TwoFFSync]: assignment to `meta` reads a signal
from domain clock `clk_a` inside a process clocked in domain clock `clk_b` — #[cdc]-annotated synchronizer
CDC analysis: 0 critical, 0 warning(s), 1 info
```

---

## Best Practices

### 1. Synchronize Control, Not Data

```skalp
// BAD: synchronizing a wide data bus bit-by-bit — 128 independent
// 2-FF chains can each resolve on different cycles, tearing the word
#[cdc]
signal data_meta: bit[128];

// GOOD: synchronize a single valid flag, hold the data stable
#[cdc]
signal valid_meta: bit;
signal data_hold: bit[128];  // held stable in the source domain, no sync needed
```

### 2. Use Appropriate Sync Stages

| Frequency | Recommended Stages |
|-----------|-------------------|
| < 100 MHz | 2 stages |
| 100-300 MHz | 2-3 stages |
| > 300 MHz | 3+ stages |

### 3. Avoid Reconvergent Paths

```skalp
// BAD: the same source signal synchronized through two separate chains —
// the two copies may resolve on different destination cycles
#[cdc]
signal sync_a: bit,

#[cdc]
signal sync_b: bit,

// These may arrive at different times!
// result = sync_a & sync_b;  // Potential glitch

// GOOD: combine in the source domain, synchronize once
#[cdc]
signal sync_combined: bit,
```

### 4. Document CDC Assumptions

```skalp
entity DocumentedCDC {
    // CDC ASSUMPTIONS:
    // - cpu_clk and mem_clk are asynchronous
    // - cpu_req pulse width > 2 mem_clk cycles
    // - Data stable 1 cpu_clk before req assertion

    #[cdc(from = 'cpu_clk, to = 'mem_clk)]
    signal req_meta: bit,
}
```

### 5. Gray Code for Pointers

```skalp
// ALWAYS Gray-code FIFO pointers before crossing, and
// synchronize the Gray value through an explicit 2-FF chain
#[cdc]
signal wr_ptr_gray_meta: bit[10],
signal wr_ptr_gray_sync: bit[10],
```

---

## See Also

- [Attributes Reference](../reference/attributes.md)
- [Power Intent Guide](power-intent.md)
- [Memory Synthesis Guide](memory-synthesis.md)
