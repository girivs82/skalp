# Clock Domain Crossing (CDC) Support in SKALP

## Overview

SKALP provides comprehensive support for Clock Domain Crossing (CDC) verification and simulation, including:

1. **Static CDC Analysis** - Compile-time detection of unsafe clock domain crossings
2. **Multi-Clock Simulation** - Independent clock domain simulation with `ClockManager`
3. **Multi-Clock Testbench API** - Ergonomic API for testing CDC designs

## Architecture

### 1. Static CDC Analysis (`crates/skalp-mir/src/cdc_analysis.rs`)

The `CdcAnalyzer` performs compile-time analysis to detect unsafe clock domain crossings:

```rust
pub struct CdcAnalyzer {
    signal_domains: HashMap<SignalId, ClockDomainId>,
    port_domains: HashMap<PortId, ClockDomainId>,
}
```

**Violation Types Detected:**
- `DirectCrossing` - Direct assignment from one clock domain to another
- `CombinationalMixing` - Combinational logic mixing multiple clock domains
- `AsyncResetCrossing` - Asynchronous reset crossing clock domains
- `ArithmeticMixing` - Arithmetic operations on signals from different domains

**Severity Levels:**
- `Critical` - Will cause design failures
- `Warning` - Potentially unsafe patterns
- `Info` - Informational notices

### 2. Clock Manager (`crates/skalp-sim/src/clock_manager.rs`)

Supports multiple independent clocks with different periods:

```rust
pub struct ClockManager {
    clocks: HashMap<String, ClockInfo>,
    current_time_ps: u64,
    auto_toggle: bool,
}

pub struct ClockInfo {
    name: String,
    current_value: bool,
    previous_value: bool,
    period_ps: u64,
    duty_cycle: f32,
}
```

**Features:**
- Independent clock periods in picoseconds
- Edge detection (Rising/Falling/Both)
- Auto-toggle mode for time-based simulation
- Multiple clock domain support in both GPU and CPU runtimes

### 3. MIR Clock Domain Support

Signals and ports can be tagged with clock domains:

```rust
pub struct Signal {
    pub id: SignalId,
    pub name: String,
    pub signal_type: DataType,
    pub clock_domain: Option<ClockDomainId>,  // CDC support
}

pub enum DataType {
    Clock { domain: Option<ClockDomainId> },
    Reset { active_high: bool, domain: Option<ClockDomainId> },
    // ...
}

pub struct ClockDomain {
    pub id: ClockDomainId,
    pub name: String,
    pub clock_signal: Option<SignalId>,
    pub reset_signal: Option<SignalId>,
}
```

## Multi-Clock Testbench API

### Basic Usage

The testbench API now supports multiple independent clocks:

```rust
use skalp_testing::testbench::*;

#[tokio::test]
async fn test_dual_clock_design() {
    let mut tb = Testbench::new("examples/async_fifo.sk").await.unwrap();

    // === RESET SEQUENCE ===
    // Assert both reset signals (set to HIGH)
    tb.set("wr_rst", 1u8).set("rd_rst", 1u8);

    // Hold reset for 2 cycles on BOTH clocks
    // (2 means "2 clock cycles", not a signal value!)
    tb.clock_multi(&[("wr_clk", 2), ("rd_clk", 2)]).await;

    // Release both resets (set to LOW)
    tb.set("wr_rst", 0u8).set("rd_rst", 0u8);

    // One cycle to stabilize
    tb.clock_multi(&[("wr_clk", 1), ("rd_clk", 1)]).await;

    // === WRITE OPERATION ===
    // Write data on write clock
    tb.set("wr_en", 1u8).set("wr_data", 0xAA);
    tb.clock_signal("wr_clk", 1).await;  // 1 write clock cycle

    // === READ OPERATION ===
    // Read data on read clock
    tb.set("rd_en", 1u8);
    tb.clock_signal("rd_clk", 1).await;  // 1 read clock cycle
}
```

### API Methods

**Important Note**: The numeric parameters in these APIs are **NUMBER OF CYCLES**, not signal values!

#### `clock(cycles: usize)` - Default clock

Backward compatible - runs the default "clk" signal for N cycles:

```rust
tb.clock(5).await;  // Run 5 clock cycles (same as clock_signal("clk", 5))
```

#### `clock_signal(clock_name: &str, cycles: usize)` - Named clock

Run a specific named clock for N cycles:

```rust
// Run 3 full cycles on the write clock
tb.clock_signal("wr_clk", 3).await;  // 3 = number of cycles

// Run 5 full cycles on the read clock
tb.clock_signal("rd_clk", 5).await;  // 5 = number of cycles
```

Each cycle is: low → high → low (one complete clock period).

#### `clock_multi(clocks: &[(&str, usize)])` - Multiple clocks

Toggle multiple clocks independently in the same call:

```rust
// Run 1 cycle on wr_clk AND 2 cycles on rd_clk simultaneously
tb.clock_multi(&[("wr_clk", 1), ("rd_clk", 2)]).await;
//                           ^               ^
//                    1 = number of cycles    2 = number of cycles
```

This is critical for CDC testing as it allows simulating different clock frequencies.

**Common Pattern - Dual Clock Reset:**

```rust
// Assert both reset signals
tb.set("wr_rst", 1u8).set("rd_rst", 1u8);

// Hold reset for 3 cycles on BOTH clocks
tb.clock_multi(&[("wr_clk", 3), ("rd_clk", 3)]).await;
//                           ^               ^
//                      3 cycles        3 cycles

// Release both resets
tb.set("wr_rst", 0u8).set("rd_rst", 0u8);

// One cycle to stabilize
tb.clock_multi(&[("wr_clk", 1), ("rd_clk", 1)]).await;
```

## Example: Async FIFO (CDC Pattern)

```skalp
entity AsyncFifo {
    // Write domain
    in wr_clk: clock
    in wr_rst: reset(active_high)
    in wr_en: bit
    in wr_data: bit[8]
    out wr_full: bit

    // Read domain
    in rd_clk: clock
    in rd_rst: reset(active_high)
    in rd_en: bit
    out rd_data: bit[8]
    out rd_empty: bit
}

impl AsyncFifo {
    // Pointers in Gray code for CDC safety
    signal wr_ptr_gray: bit[5]
    signal rd_ptr_gray: bit[5]

    // 2-stage synchronizers (CDC best practice)
    signal rd_ptr_gray_sync1: bit[5]  // In wr_clk domain
    signal rd_ptr_gray_sync2: bit[5]  // In wr_clk domain
    signal wr_ptr_gray_sync1: bit[5]  // In rd_clk domain
    signal wr_ptr_gray_sync2: bit[5]  // In rd_clk domain

    // Write domain logic
    on(wr_clk.rise) {
        if !wr_rst {
            // Synchronize from read domain
            rd_ptr_gray_sync1 <= rd_ptr_gray
            rd_ptr_gray_sync2 <= rd_ptr_gray_sync1

            // Write logic...
        }
    }

    // Read domain logic
    on(rd_clk.rise) {
        if !rd_rst {
            // Synchronize from write domain
            wr_ptr_gray_sync1 <= wr_ptr_gray
            wr_ptr_gray_sync2 <= wr_ptr_gray_sync1

            // Read logic...
        }
    }
}
```

## CDC Best Practices

### 1. Use 2-Flop Synchronizers

Always use at least 2 flip-flops when crossing clock domains:

```skalp
// Stage 1: Capture (may be metastable)
signal sync1: bit

// Stage 2: Stable output
signal sync2: bit

on(dst_clk.rise) {
    sync1 <= src_signal  // From different clock domain
    sync2 <= sync1        // Stable synchronized value
}
```

### 2. Use Gray Code for Multi-Bit Values

Convert to Gray code before crossing domains:

```rust
// Binary to Gray: G = B XOR (B >> 1)
let gray = binary ^ (binary >> 1);

// Gray to Binary: Each bit is XOR of all higher bits
let b7 = g7;
let b6 = b7 ^ g6;
let b5 = b6 ^ g5;
// ...
```

### 3. Avoid Combinational Mixing

Don't mix signals from different clock domains in combinational logic without proper synchronization.

❌ **Bad:**
```skalp
output = signal_from_clk1 + signal_from_clk2  // CDC violation!
```

✅ **Good:**
```skalp
// Synchronize first
signal signal_clk2_synced: bit[8]

on(clk1.rise) {
    signal_clk2_synced <= signal_from_clk2  // Synchronize
}

output = signal_from_clk1 + signal_clk2_synced  // Safe
```

## Testing CDC Designs

### Example Test

```rust
#[tokio::test]
async fn test_async_fifo_different_rates() {
    let mut tb = Testbench::new("examples/async_fifo.sk").await.unwrap();

    // === RESET BOTH DOMAINS ===
    tb.set("wr_rst", 1u8).set("rd_rst", 1u8);

    // Run 2 cycles on both clocks with reset held high
    tb.clock_multi(&[("wr_clk", 2), ("rd_clk", 2)]).await;

    tb.set("wr_rst", 0u8).set("rd_rst", 0u8);

    // === WRITE AT FAST CLOCK ===
    // Write 8 values, 1 write clock cycle each
    for i in 0..8 {
        tb.set("wr_en", 1u8).set("wr_data", i as u8);
        tb.clock_signal("wr_clk", 1).await;  // 1 cycle
    }

    // === WAIT FOR CDC SYNCHRONIZATION ===
    // Run 3 read clock cycles to allow double-flop sync to complete
    tb.clock_signal("rd_clk", 3).await;  // 3 cycles

    // === READ AT SLOW CLOCK (while write clock runs 3x faster) ===
    for i in 0..8 {
        tb.set("rd_en", 1u8);

        // Read clock runs 1 cycle, write clock runs 3 cycles simultaneously
        // This simulates wr_clk being 3x faster than rd_clk
        tb.clock_multi(&[("rd_clk", 1), ("wr_clk", 3)]).await;
        //                           ^               ^
        //                     1 rd cycle      3 wr cycles

        tb.expect("rd_data", i as u8).await;
    }
}
```

## Current Limitations

1. **Syntax Limitations**: Complex CDC designs (like async FIFO with true arrays) are challenging with current SKALP syntax. See `examples/async_fifo.sk` for a working example using individual signals.

2. **Timing Analysis**: Static timing analysis across clock domains is not yet implemented.

3. **Metastability Modeling**: The simulator does not model metastability effects - it assumes synchronizers work perfectly.

## Future Work

1. **Automatic CDC Insertion**: Compiler could automatically insert synchronizers
2. **Formal Verification**: Integration with formal tools for CDC correctness proofs
3. **Timing Constraints**: Add timing constraint checking between clock domains
4. **Metastability Injection**: Simulation option to inject random metastability for robustness testing

## Summary

SKALP provides strong CDC support through:

✅ **Static analysis** - Detects violations at compile-time
✅ **ClockManager** - Multi-clock simulation infrastructure
✅ **Multi-clock testbench API** - `clock_signal()` and `clock_multi()`
✅ **MIR clock domains** - First-class support for clock domain tagging
✅ **Working examples** - CDC patterns documented and tested

The infrastructure is ready for realistic CDC designs. The main limitations are SKALP syntax features (arrays, generics), not CDC capabilities.
