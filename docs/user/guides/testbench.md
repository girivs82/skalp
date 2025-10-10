# Writing Testbenches for SKALP Designs

**Learn how to test your SKALP hardware designs using the integrated testbench API.**

---

## Overview

SKALP includes a built-in testbench API that lets you write hardware tests in Rust. Tests are:
- ✅ **Type-safe** - Automatic conversions between Rust types and signals
- ✅ **Concise** - 5-10x less code than raw API
- ✅ **Fast** - GPU-accelerated simulation
- ✅ **Integrated** - No external tools needed

---

## Quick Start

### 1. Create a Test File

In your SKALP project, create `tests/counter_test.rs`:

```rust
use skalp_testbench::Testbench;

#[tokio::test]
async fn test_counter() {
    // Load your design
    let mut tb = Testbench::new("src/main.sk").await.unwrap();

    // Reset for 2 cycles
    tb.reset(2).await;

    // Run for 10 clock cycles
    tb.clock(10).await;

    // Check output
    tb.expect("count", 10u8).await;
}
```

### 2. Add Dependencies

In your project's `Cargo.toml`:

```toml
[dev-dependencies]
skalp-testbench = { path = "../skalp/crates/skalp-testbench" }
tokio = { version = "1", features = ["full"] }
```

### 3. Run Tests

```bash
cargo test
```

---

## Core API

### Creating a Testbench

```rust
// From source file
let mut tb = Testbench::new("examples/counter.sk").await.unwrap();

// With custom configuration (future)
let config = SimulationConfig {
    use_gpu: true,
    duration: Some(1000),
};
let mut tb = Testbench::with_config("design.sk", config).await.unwrap();
```

### Setting Input Values

```rust
// Single input
tb.set("enable", 1u8);

// Multiple inputs (chainable)
tb.set("a", 5u32)
  .set("b", 3u32)
  .set("op", 0b000u8);

// Different types
tb.set("byte_val", 0xAAu8);
tb.set("word_val", 0x12345678u32);
tb.set("flag", 1u8);  // Boolean as u8
```

**Supported types:** `u8`, `u16`, `u32`, `u64`, `Vec<u8>`, `&[u8]`

### Clock Cycles

```rust
// Run N clock cycles
tb.clock(1).await;    // 1 cycle
tb.clock(10).await;   // 10 cycles
tb.clock(100).await;  // 100 cycles
```

**What happens:** Automatically toggles `clk` signal: `0 → 1 → 0 → 1 → ...`

### Reset

```rust
// Assert reset for N cycles
tb.reset(2).await;
```

**What happens:**
1. Sets `rst = 1`
2. Runs N clock cycles
3. Sets `rst = 0`

### Reading Outputs

```rust
// Get raw bytes
let data: Vec<u8> = tb.get("result").await;

// Get as typed value
let result: u32 = tb.get_as("result").await;
let flag: u8 = tb.get_as("ready").await;
```

### Assertions

```rust
// Assert output equals expected value
tb.expect("result", 8u32).await;
tb.expect("zero", 1u8).await;
tb.expect("ready", 0u8).await;
```

**On failure:** Panics with clear error message showing expected vs actual.

---

## Complete Examples

### Counter Test

```rust
#[tokio::test]
async fn test_counter_basic() {
    let mut tb = Testbench::new("examples/counter.sk").await.unwrap();

    // Reset
    tb.reset(2).await;

    // Count should start at 0
    tb.expect("count", 0u8).await;

    // After 5 cycles, should be 5
    tb.clock(5).await;
    tb.expect("count", 5u8).await;

    // After 10 more cycles, should be 15
    tb.clock(10).await;
    tb.expect("count", 15u8).await;

    // After 256 cycles total, should wrap to 0
    tb.clock(241).await;  // 15 + 241 = 256
    tb.expect("count", 0u8).await;
}
```

### ALU Test

```rust
#[tokio::test]
async fn test_alu_operations() {
    let mut tb = Testbench::new("examples/alu.sk").await.unwrap();

    // Test ADD (op = 0b000)
    tb.set("a", 5u32).set("b", 3u32).set("op", 0b000u8);
    tb.clock(2).await;  // Give time for registered output
    tb.expect("result", 8u32).await;
    tb.expect("zero", 0u8).await;

    // Test SUB (op = 0b001)
    tb.set("op", 0b001u8);
    tb.clock(2).await;
    tb.expect("result", 2u32).await;

    // Test AND (op = 0b010)
    tb.set("a", 0xFFu32).set("b", 0x0Fu32).set("op", 0b010u8);
    tb.clock(2).await;
    tb.expect("result", 0x0Fu32).await;

    // Test zero flag
    tb.set("a", 5u32).set("b", 5u32).set("op", 0b001u8);  // 5 - 5 = 0
    tb.clock(2).await;
    tb.expect("result", 0u32).await;
    tb.expect("zero", 1u8).await;
}
```

### FIFO Test

```rust
#[tokio::test]
async fn test_fifo_basic() {
    let mut tb = Testbench::new("examples/fifo.sk").await.unwrap();

    tb.reset(2).await;

    // Initially empty
    tb.expect("empty", 1u8).await;
    tb.expect("full", 0u8).await;

    // Write 4 values
    for i in 0..4 {
        tb.set("wr_en", 1u8).set("wr_data", (i * 10) as u8);
        tb.clock(1).await;
    }
    tb.set("wr_en", 0u8);

    // Should not be empty, might be full (if DEPTH=4)
    tb.expect("empty", 0u8).await;

    // Read back in order
    for i in 0..4 {
        tb.set("rd_en", 1u8);
        tb.clock(1).await;
        tb.expect("rd_data", (i * 10) as u8).await;
    }
    tb.set("rd_en", 0u8);

    // Should be empty again
    tb.expect("empty", 1u8).await;
}
```

---

## Advanced Patterns

### Table-Driven Testing

```rust
#[tokio::test]
async fn test_alu_table_driven() {
    let mut tb = Testbench::new("examples/alu.sk").await.unwrap();

    // (a, b, op, expected_result, expected_zero)
    let test_cases = vec![
        (5u32, 3u32, 0b000u8, 8u32, 0u8),      // ADD
        (5u32, 3u32, 0b001u8, 2u32, 0u8),      // SUB
        (10u32, 10u32, 0b001u8, 0u32, 1u8),    // SUB (zero)
        (0xFFu32, 0x0Fu32, 0b010u8, 0x0Fu32, 0u8),  // AND
        (0xF0u32, 0x0Fu32, 0b011u8, 0xFFu32, 0u8),  // OR
        (0xFFu32, 0xFFu32, 0b100u8, 0u32, 1u8),     // XOR (zero)
    ];

    for (a, b, op, expected_result, expected_zero) in test_cases {
        tb.set("a", a).set("b", b).set("op", op);
        tb.clock(2).await;
        tb.expect("result", expected_result).await;
        tb.expect("zero", expected_zero).await;
    }
}
```

### State Machine Testing

```rust
#[tokio::test]
async fn test_uart_tx_fsm() {
    let mut tb = Testbench::new("examples/uart_tx.sk").await.unwrap();

    tb.reset(2).await;

    // Should start in IDLE state
    tb.expect("busy", 0u8).await;

    // Load data and start transmission
    tb.set("data_in", 0x55u8).set("start", 1u8);
    tb.clock(1).await;
    tb.set("start", 0u8);

    // Should now be busy
    tb.expect("busy", 1u8).await;

    // Wait for transmission to complete (start + 8 data + stop)
    tb.clock(10).await;

    // Should be done
    tb.expect("busy", 0u8).await;
}
```

### Loop-Based Testing

```rust
#[tokio::test]
async fn test_counter_with_load() {
    let mut tb = Testbench::new("examples/counter_load.sk").await.unwrap();

    tb.reset(2).await;

    // Test loading different values
    for load_val in [10u8, 50u8, 100u8, 200u8] {
        tb.set("load", 1u8).set("load_value", load_val);
        tb.clock(1).await;
        tb.set("load", 0u8);

        tb.expect("count", load_val).await;

        // Count up a few cycles
        tb.clock(5).await;
        tb.expect("count", load_val.wrapping_add(5)).await;
    }
}
```

### Checking Cycle-Accurate Behavior

```rust
#[tokio::test]
async fn test_pipeline_timing() {
    let mut tb = Testbench::new("examples/pipeline.sk").await.unwrap();

    tb.reset(2).await;

    // Input at cycle 0
    tb.set("data_in", 0x12u8).set("valid_in", 1u8);
    tb.clock(1).await;
    tb.set("valid_in", 0u8);

    // Output should appear after 3 cycle pipeline delay
    tb.clock(2).await;
    tb.expect("valid_out", 0u8).await;  // Not yet

    tb.clock(1).await;
    tb.expect("valid_out", 1u8).await;  // Now!
    tb.expect("data_out", 0x12u8).await;
}
```

---

## Multi-Clock Designs (CDC Testing)

For designs with multiple clocks:

```rust
#[tokio::test]
async fn test_async_fifo() {
    let mut tb = Testbench::new("examples/async_fifo.sk").await.unwrap();

    // Reset both clock domains
    tb.reset_multi(&[("wr_rst", 2), ("rd_rst", 2)]).await;

    // Clock domains independently
    for i in 0..10 {
        tb.set("wr_en", 1u8).set("wr_data", i as u8);
        tb.clock_multi(&[("wr_clk", 1)]).await;  // Write clock only
    }

    tb.set("wr_en", 0u8);

    // Read from different clock domain
    for i in 0..10 {
        tb.set("rd_en", 1u8);
        tb.clock_multi(&[("rd_clk", 1)]).await;  // Read clock only
        tb.expect("rd_data", i as u8).await;
    }
}
```

---

## Helper Functions

Organize tests with helper functions:

```rust
// Helper: Write to FIFO
async fn fifo_write(tb: &mut Testbench, data: &[u8]) {
    for &byte in data {
        tb.set("wr_en", 1u8).set("wr_data", byte);
        tb.clock(1).await;
    }
    tb.set("wr_en", 0u8);
}

// Helper: Read from FIFO
async fn fifo_read(tb: &mut Testbench, count: usize) -> Vec<u8> {
    let mut result = Vec::new();
    for _ in 0..count {
        tb.set("rd_en", 1u8);
        tb.clock(1).await;
        let data: u8 = tb.get_as("rd_data").await;
        result.push(data);
    }
    tb.set("rd_en", 0u8);
    result
}

// Use helpers in tests
#[tokio::test]
async fn test_fifo_with_helpers() {
    let mut tb = Testbench::new("examples/fifo.sk").await.unwrap();
    tb.reset(2).await;

    let input = vec![0x11, 0x22, 0x33, 0x44];
    fifo_write(&mut tb, &input).await;

    let output = fifo_read(&mut tb, 4).await;
    assert_eq!(input, output);
}
```

---

## Common Patterns

### Pattern: Setup and Teardown

```rust
async fn setup() -> Testbench {
    let mut tb = Testbench::new("design.sk").await.unwrap();
    tb.reset(2).await;
    tb
}

#[tokio::test]
async fn test_something() {
    let mut tb = setup().await;
    // Test body...
}
```

### Pattern: Parameterized Tests

```rust
async fn test_counter_with_width(width: usize, max_count: u64) {
    let source = format!("entity Counter<const WIDTH: nat = {}> {{ ... }}", width);
    // ... test implementation
}

#[tokio::test]
async fn test_8bit_counter() {
    test_counter_with_width(8, 256).await;
}

#[tokio::test]
async fn test_16bit_counter() {
    test_counter_with_width(16, 65536).await;
}
```

### Pattern: Wait for Condition

```rust
// Wait until signal equals value (with timeout)
async fn wait_for(
    tb: &mut Testbench,
    signal: &str,
    value: u8,
    max_cycles: usize
) -> Result<(), &'static str> {
    for _ in 0..max_cycles {
        let current: u8 = tb.get_as(signal).await;
        if current == value {
            return Ok(());
        }
        tb.clock(1).await;
    }
    Err("Timeout waiting for signal")
}

// Usage
#[tokio::test]
async fn test_with_wait() {
    let mut tb = setup().await;
    tb.set("start", 1u8);
    wait_for(&mut tb, "done", 1u8, 100).await.unwrap();
}
```

---

## Debugging Tests

### Print Signal Values

```rust
#[tokio::test]
async fn debug_test() {
    let mut tb = Testbench::new("design.sk").await.unwrap();

    for i in 0..10 {
        let count: u8 = tb.get_as("count").await;
        println!("Cycle {}: count = {}", i, count);
        tb.clock(1).await;
    }
}
```

### Dump Waveforms (Future)

```rust
#[tokio::test]
async fn test_with_waveform() {
    let mut tb = Testbench::new("design.sk").await.unwrap();
    tb.enable_vcd("test.vcd");  // Future feature

    // Run test...

    // Waveform automatically saved to test.vcd
}
```

---

## Performance Tips

### 1. Batch Clock Cycles

```rust
// Slow: Many small clock calls
for _ in 0..100 {
    tb.clock(1).await;
}

// Fast: One large clock call
tb.clock(100).await;
```

### 2. Minimize Assertions in Loops

```rust
// Slow: Check every cycle
for i in 0..1000 {
    tb.clock(1).await;
    tb.expect("count", i as u8).await;
}

// Fast: Check key points only
tb.clock(100).await;
tb.expect("count", 100u8).await;
tb.clock(900).await;
tb.expect("count", 232u8).await;  // Wrapped: (100 + 900) % 256
```

---

## Troubleshooting

### Error: "Signal not found"

```
Error: Signal 'countr' not found
```
→ Check signal name spelling in your SKALP design

### Error: "Type mismatch"

```
Error: Expected 4 bytes for u32, got 1 byte
```
→ Use correct type: `tb.expect("result", 8u8)` not `8u32` for 8-bit signal

### Test Times Out

```
Test did not complete in 60s
```
→ Check for infinite loops in design or missing clock

### Assertion Fails

```
Assertion failed: expected 8, got 0
```
→ Check clock cycles (may need more time for sequential logic)
→ Verify reset sequence
→ Check if output is registered (needs extra clock cycle)

---

## See Also

- [Syntax Reference](../reference/syntax.md) - SKALP language syntax
- [CLI Reference](../reference/cli.md) - Build and simulation commands
- [Examples](../examples/) - Complete designs with tests
- [Testing Patterns](../cookbook/testing.md) - Common test patterns

---

## Quick Reference

```rust
// Setup
let mut tb = Testbench::new("design.sk").await.unwrap();

// Reset
tb.reset(2).await;

// Set inputs
tb.set("in", 42u8);
tb.set("a", 1000u32).set("b", 2000u32);

// Clock cycles
tb.clock(10).await;

// Read outputs
let out: u8 = tb.get_as("result").await;

// Assert
tb.expect("result", 42u8).await;

// Multi-clock
tb.clock_multi(&[("clk1", 2), ("clk2", 3)]).await;
```

---

**Happy testing! 🧪**
