# SKALP Testbench Guide

## Current Testbench Approach

SKALP currently uses a **two-language approach** for hardware verification:

1. **Hardware design** - Written in SKALP (`.sk` files) - synthesizable HDL
2. **Testbenches** - Written in Rust using the `Testbench` API - simulation framework

This approach provides:
- ✅ Type-safe stimulus and checking
- ✅ Modern async/await patterns
- ✅ Powerful Rust ecosystem (random testing, property-based testing, etc.)
- ✅ GPU-accelerated simulation on macOS via Metal
- ✅ Clean separation of synthesizable vs. simulation-only code

## Example: Counter with Testbench

### Step 1: Hardware Design (SKALP)

**File: `counter.sk`**

```skalp
// Simple 8-bit counter with enable and reset
entity Counter {
    in clk: clock
    in rst: reset(active_high)
    in enable: bit
    out count: bit[8]
}

impl Counter {
    signal count_reg: bit[8]

    on(clk.rise) {
        if rst {
            count_reg <= 0
        } else if enable {
            count_reg <= count_reg + 1
        }
    }

    count = count_reg
}
```

### Step 2: Testbench (Rust)

**File: `test_counter.rs`**

```rust
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

    // Test 5: Overflow behavior
    tb.set("enable", 1u8);
    for i in 8..255 {
        tb.clock(1).await;
        tb.expect("count", (i + 1) as u8).await;
    }

    // Counter should wrap to 0
    tb.clock(1).await;
    tb.expect("count", 0u8).await;
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
}
```

## Testbench API Reference

### Creating a Testbench

```rust
// Default configuration (GPU on macOS, CPU elsewhere)
let mut tb = Testbench::new("path/to/design.sk").await?;

// Custom configuration
let config = SimulationConfig {
    use_gpu: true,
    max_cycles: 1_000_000,
    timeout_ms: 60_000,
    capture_waveforms: true,
    parallel_threads: 4,
};
let mut tb = Testbench::with_config("path/to/design.sk", config).await?;
```

### Setting Inputs

```rust
// Chainable API
tb.set("clk", 1u8)
  .set("data", 0xDEADBEEFu32)
  .set("valid", 1u8);

// Inputs are buffered until clock() is called
```

### Advancing Time

```rust
// Default clock ("clk")
tb.clock(5).await;  // Run 5 clock cycles

// Specific clock signal
tb.clock_signal("rd_clk", 3).await;

// Multiple independent clocks (CDC testing)
tb.clock_multi(&[("wr_clk", 1), ("rd_clk", 2)]).await;
```

### Checking Outputs

```rust
// Expect specific value
tb.expect("result", 42u32).await;

// Get value for custom checking
let value: u32 = tb.get_as("result").await;
assert!(value > 100);

// Check multiple signals
tb.expect_many(&[
    ("x", 10u32),
    ("y", 20u32),
    ("z", 30u32),
]).await;
```

### Utility Methods

```rust
// Apply reset
tb.reset(5).await;  // 5 cycles of reset

// Wait for condition (with timeout)
tb.wait_until(|v| v[0] == 1, 100).await?;

// Get cycle count
let cycles = tb.cycles();
```

## Multi-Clock Domain Example

```rust
#[tokio::test]
async fn test_async_fifo() {
    let mut tb = Testbench::new("examples/async_fifo.sk").await?;

    // Setup
    tb.set("wr_rst", 1u8).set("rd_rst", 1u8);
    tb.clock_multi(&[("wr_clk", 5), ("rd_clk", 5)]).await;
    tb.set("wr_rst", 0u8).set("rd_rst", 0u8);

    // Write data on write clock domain
    for i in 0..8 {
        tb.set("write_data", i as u32).set("write_enable", 1u8);
        tb.clock_signal("wr_clk", 1).await;
    }
    tb.set("write_enable", 0u8);

    // Give CDC synchronizers time to propagate
    tb.clock_multi(&[("wr_clk", 3), ("rd_clk", 5)]).await;

    // Read data on read clock domain
    for i in 0..8 {
        tb.set("read_enable", 1u8);
        tb.clock_signal("rd_clk", 1).await;
        tb.expect("read_data", i as u32).await;
    }
}
```

## What IS Supported

### SKALP Language Features (Synthesizable)
- ✅ Entity declarations with ports (in/out)
- ✅ Signal declarations (in `impl` blocks)
- ✅ Sequential logic (`on(clk.rise)`)
- ✅ Combinational logic (continuous assignment)
- ✅ Conditional statements (`if/else`)
- ✅ Match expressions
- ✅ Arithmetic and logical operators
- ✅ Struct types
- ✅ Array types
- ✅ Generic entities with const parameters
- ✅ Module instantiation
- ✅ String type for documentation (non-synthesizable)

### Testbench API Features (Rust)
- ✅ Async/await based API
- ✅ Type-safe signal values (u8, u16, u32, u64, Vec<u8>)
- ✅ Single clock stepping
- ✅ Multi-clock domain testing
- ✅ Reset helpers
- ✅ Signal checking with assertions
- ✅ Chainable builder pattern
- ✅ GPU-accelerated simulation (macOS)
- ✅ Waveform capture (VCD)

## What is NOT Supported

### SystemVerilog Testbench Features (Not Implemented)
- ❌ `initial` blocks
- ❌ `always` blocks in testbenches
- ❌ Time delays (`#10ns`)
- ❌ Event control (`@(posedge clk)`)
- ❌ System tasks (`$display`, `$finish`, `$random`)
- ❌ Fork/join concurrency
- ❌ Behavioral loops in hardware context
- ❌ Coverage groups
- ❌ Assertions (SVA)
- ❌ Mutable variables in hardware

### Why Not?

These features require a **complete behavioral simulation language**, which would:
1. Significantly increase compiler complexity
2. Mix synthesizable and non-synthesizable code in the same file
3. Duplicate functionality already provided by Rust

The current approach keeps synthesizable hardware clean and leverages Rust's powerful testing ecosystem.

## Future Enhancements

Potential future additions (not currently implemented):

1. **Property-based testing integration**
   - Use `proptest` or `quickcheck` for random stimulus generation

2. **Coverage collection**
   - Track signal toggles, branch coverage, FSM states

3. **Waveform analysis**
   - Programmatic VCD parsing and checking

4. **Constraint random verification**
   - Rust-based constraint solver for stimulus generation

5. **UVM-like testbench components**
   - Drivers, monitors, scoreboards as Rust traits

## Best Practices

### 1. One Test Per Scenario
```rust
#[tokio::test]
async fn test_reset() { /* ... */ }

#[tokio::test]
async fn test_enable() { /* ... */ }

#[tokio::test]
async fn test_overflow() { /* ... */ }
```

### 2. Use Helper Functions
```rust
async fn write_fifo(tb: &mut Testbench, data: u32) {
    tb.set("write_data", data).set("write_enable", 1u8);
    tb.clock_signal("wr_clk", 1).await;
    tb.set("write_enable", 0u8);
}
```

### 3. Document Test Intent
```rust
// Test that counter wraps correctly at max value
#[tokio::test]
async fn test_counter_wraparound() {
    // ...
}
```

### 4. Use String Signals for Documentation
```skalp
entity MyTestbench {
    in clk: clock
    in data: bit[8]
    out result: bit[8]

    signal test_description: string
}

impl MyTestbench {
    // Hardware logic...

    test_description = "Tests data processing pipeline"
}
```

## Running Tests

```bash
# Run all tests
cargo test --all-features

# Run specific test
cargo test test_counter_basic

# Run with output
cargo test test_counter_basic -- --nocapture

# Run GPU tests (macOS only)
cargo test --all-features --test test_graphics_pipeline_functional
```

## See Also

- `crates/skalp-testing/src/testbench.rs` - Testbench API implementation
- `tests/test_graphics_pipeline_functional.rs` - Advanced CDC examples
- `examples/string_testbench_showcase.sk` - String type usage
- Working examples in `tests/fixtures/`
