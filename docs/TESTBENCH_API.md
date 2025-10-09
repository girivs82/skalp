# Ergonomic Testbench API for SKALP

## Problem Statement

The current testbench API is too verbose and low-level, making test writing tedious and error-prone:

```rust
// Current API - Lots of boilerplate!
sim.set_input("a", vec![0x05, 0x00, 0x00, 0x00]).await.unwrap();
sim.set_input("b", vec![0x03, 0x00, 0x00, 0x00]).await.unwrap();
sim.set_input("op", vec![0b000]).await.unwrap();
sim.set_input("clk", vec![0]).await.unwrap();
sim.step_simulation().await.unwrap();
sim.set_input("clk", vec![1]).await.unwrap();
sim.step_simulation().await.unwrap();
let result = sim.get_output("result").await.unwrap();
let result_val = u32::from_le_bytes([result[0], result[1], result[2], result[3]]);
assert_eq!(result_val, 8);
```

## Proposed Solution

A new high-level API that is:
- **Concise**: 5-10x less code
- **Type-safe**: Automatic conversions between Rust types and signal values
- **Readable**: Clear test intent
- **Chainable**: Fluent interface for common patterns

```rust
// New API - Clean and declarative!
let mut tb = Testbench::new("examples/alu.sk").await?;
tb.set("a", 5u32).set("b", 3u32).set("op", 0b000u8);
tb.clock(2).await;
tb.expect("result", 8u32).await;
```

## Key Features

### 1. Automatic Clock Handling

```rust
// Old: Manual clock toggling
sim.set_input("clk", vec![0]).await.unwrap();
sim.step_simulation().await.unwrap();
sim.set_input("clk", vec![1]).await.unwrap();
sim.step_simulation().await.unwrap();

// New: Automatic
tb.clock(1).await;
```

### 2. Type-Safe Value Conversions

```rust
// Old: Manual byte array construction
sim.set_input("a", vec![0x05, 0x00, 0x00, 0x00]).await.unwrap();

// New: Direct Rust values
tb.set("a", 5u32);
```

Supported types: `u8`, `u16`, `u32`, `u64`, `Vec<u8>`, `&[u8]`

### 3. Chainable Methods

```rust
// Old: Multiple statements
sim.set_input("a", vec![5, 0, 0, 0]).await.unwrap();
sim.set_input("b", vec![3, 0, 0, 0]).await.unwrap();
sim.set_input("op", vec![0]).await.unwrap();

// New: Chained
tb.set("a", 5u32).set("b", 3u32).set("op", 0u8);
```

### 4. Built-in Reset Helper

```rust
// Old: Manual reset sequence
sim.set_input("rst", vec![1]).await.unwrap();
sim.set_input("clk", vec![0]).await.unwrap();
sim.step_simulation().await.unwrap();
sim.set_input("clk", vec![1]).await.unwrap();
sim.step_simulation().await.unwrap();
sim.set_input("rst", vec![0]).await.unwrap();

// New: Single call
tb.reset(2).await;
```

### 5. Typed Output Reading

```rust
// Old: Manual parsing
let result = sim.get_output("result").await.unwrap();
let result_val = u32::from_le_bytes([result[0], result[1], result[2], result[3]]);

// New: Type inference
let result: u32 = tb.get_as("result").await;
```

## Complete Examples

### ALU Test

```rust
#[tokio::test]
async fn test_alu() {
    let mut tb = Testbench::new("examples/alu.sk").await.unwrap();

    // Test all operations
    tb.set("a", 5u32).set("b", 3u32).set("op", 0b000u8);
    tb.clock(2).await;
    tb.expect("result", 8u32).await;  // ADD

    tb.set("op", 0b001u8);
    tb.clock(2).await;
    tb.expect("result", 2u32).await;  // SUB

    tb.set("op", 0b010u8);
    tb.clock(2).await;
    tb.expect("result", 1u32).await;  // AND
}
```

### FIFO Test

```rust
#[tokio::test]
async fn test_fifo() {
    let mut tb = Testbench::new("examples/fifo.sk").await.unwrap();

    tb.reset(2).await;

    // Write 5 values
    for i in 0..5 {
        tb.set("wr_en", 1u8).set("wr_data", (i * 10) as u8);
        tb.clock(1).await;
    }
    tb.set("wr_en", 0u8);

    tb.expect("empty", 0u8).await;  // Not empty

    // Read back
    for i in 0..5 {
        tb.set("rd_en", 1u8);
        tb.clock(1).await;
        tb.expect("rd_data", (i * 10) as u8).await;
    }

    tb.expect("empty", 1u8).await;  // Now empty
}
```

### Table-Driven Testing

```rust
#[tokio::test]
async fn test_alu_comprehensive() {
    let mut tb = Testbench::new("examples/alu.sk").await.unwrap();

    let test_cases = vec![
        (5u32, 3u32, 0b000u8, 8u32),   // ADD
        (5u32, 3u32, 0b001u8, 2u32),   // SUB
        (0xFFu32, 0x0Fu32, 0b010u8, 0x0Fu32), // AND
        (0xF0u32, 0x0Fu32, 0b011u8, 0xFFu32), // OR
    ];

    for (a, b, op, expected) in test_cases {
        tb.set("a", a).set("b", b).set("op", op);
        tb.clock(2).await;
        tb.expect("result", expected).await;
    }
}
```

### Reusable Test Sequences

```rust
#[tokio::test]
async fn test_with_sequence() {
    let mut tb = Testbench::new("examples/counter.sk").await.unwrap();

    let seq = TestSequence::new()
        .reset(2)
        .clock(10)
        .expect("count", 10u8)
        .clock(5)
        .expect("count", 15u8);

    seq.run(&mut tb).await;
}
```

### Cycle-Accurate Waveform Checking

```rust
#[tokio::test]
async fn test_waveform() {
    let mut tb = Testbench::new("examples/fsm.sk").await.unwrap();

    tb.reset(1).await;

    // Check state at specific cycles
    let waveform = vec![
        (0, "state", 0u8),
        (2, "state", 1u8),
        (5, "state", 2u8),
        (10, "state", 3u8),
    ];

    for (cycle, signal, expected) in waveform {
        while tb.cycles() < cycle {
            tb.clock(1).await;
        }
        tb.expect(signal, expected).await;
    }
}
```

## API Reference

### Testbench

```rust
impl Testbench {
    /// Create from source file
    pub async fn new(path: &str) -> Result<Self>;

    /// Create with custom config
    pub async fn with_config(path: &str, config: SimulationConfig) -> Result<Self>;

    /// Set input signal (chainable)
    pub fn set(&mut self, signal: &str, value: impl IntoSignalValue) -> &mut Self;

    /// Run N clock cycles
    pub async fn clock(&mut self, cycles: usize) -> &mut Self;

    /// Assert output equals expected value
    pub async fn expect(&mut self, signal: &str, expected: impl IntoSignalValue) -> &mut Self;

    /// Get output value
    pub async fn get(&mut self, signal: &str) -> Vec<u8>;

    /// Get output as typed value
    pub async fn get_as<T: FromSignalValue>(&mut self, signal: &str) -> T;

    /// Apply reset for N cycles
    pub async fn reset(&mut self, cycles: usize) -> &mut Self;

    /// Get current cycle count
    pub fn cycles(&self) -> u64;
}
```

### TestSequence

```rust
impl TestSequence {
    pub fn new() -> Self;
    pub fn set(self, signal: &str, value: impl IntoSignalValue) -> Self;
    pub fn clock(self, cycles: usize) -> Self;
    pub fn expect(self, signal: &str, value: impl IntoSignalValue) -> Self;
    pub fn reset(self, cycles: usize) -> Self;
    pub async fn run(self, tb: &mut Testbench);
}
```

## Benefits Summary

| Aspect | Old API | New API | Improvement |
|--------|---------|---------|-------------|
| Lines of code | ~15 per test | ~3 per test | **5x reduction** |
| Type safety | Manual byte arrays | Automatic | **Type-safe** |
| Readability | Low (boilerplate) | High (declarative) | **Much clearer** |
| Error handling | .await.unwrap() everywhere | Automatic | **Less noise** |
| Clock handling | Manual toggle | Automatic | **Easier** |
| Reusability | Copy-paste | TestSequence | **Composable** |

## Migration Guide

### Step 1: Replace simulator setup

```rust
// Before
let source = fs::read_to_string("examples/alu.sk").unwrap();
let mut sim = setup_simulator(&source, true).await;

// After
let mut tb = Testbench::new("examples/alu.sk").await.unwrap();
```

### Step 2: Replace input setting

```rust
// Before
sim.set_input("a", vec![5, 0, 0, 0]).await.unwrap();

// After
tb.set("a", 5u32);
```

### Step 3: Replace clock cycles

```rust
// Before
sim.set_input("clk", vec![0]).await.unwrap();
sim.step_simulation().await.unwrap();
sim.set_input("clk", vec![1]).await.unwrap();
sim.step_simulation().await.unwrap();

// After
tb.clock(1).await;
```

### Step 4: Replace output checking

```rust
// Before
let result = sim.get_output("result").await.unwrap();
let result_val = u32::from_le_bytes([result[0], result[1], result[2], result[3]]);
assert_eq!(result_val, 8);

// After
tb.expect("result", 8u32).await;
```

## Implementation Status

- [x] Core API design
- [x] Type conversion traits
- [x] Testbench builder
- [x] Clock automation
- [x] Reset helper
- [x] TestSequence support
- [ ] Integration with existing Simulator
- [ ] Documentation
- [ ] Examples migration
- [ ] Test coverage

## Future Enhancements

1. **Waveform dumping**: `tb.dump_waveform("test.vcd")`
2. **Timeout support**: `tb.wait_until(|v| v[0] == 1, 100).await`
3. **Multi-signal assertions**: `tb.expect_many(&[...])`
4. **Coverage integration**: `tb.coverage_report()`
5. **Randomization**: `tb.randomize("data", 0..100)`

## Conclusion

The new testbench API makes verification **5-10x more concise** while improving:
- **Readability**: Clear test intent
- **Safety**: Type-checked values
- **Productivity**: Less boilerplate
- **Maintainability**: Reusable sequences

This brings SKALP's testbench experience closer to SystemVerilog's UVM or Python's cocotb, but with Rust's safety guarantees.
