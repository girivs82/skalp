# SKALP Language Tutorial

Learn SKALP step by step through practical examples.

## Chapter 1: Basic Concepts

### Entities and Signals

SKALP designs start with **entities** - the basic building blocks of hardware:

```skalp
entity led_blinker {
    in clk: clock;           // Clock input
    in rst: reset;           // Reset input  
    out led: bit;            // LED output
}
```

Inside entities, you define **signals** - wires that carry data:

```skalp
impl led_blinker {
    signal counter: bit<24>;    // 24-bit counter
    signal blink_rate: bit<24> = 12_000_000;  // 12MHz
    
    // Implementation goes here...
}
```

### Clock Events

Hardware operates on clock edges. Use `on()` blocks for sequential logic:

```skalp
impl led_blinker {
    signal counter: bit<24>;
    
    on(clk.rise) {
        if rst.active {
            counter = 0;
        } else {
            counter = counter + 1;
        }
    }
    
    // Combinational assignment
    led = (counter >= blink_rate);
}
```

## Chapter 2: Data Types

### Bit Vectors

```skalp
signal data8: bit<8>;       // 8-bit unsigned
signal data16: bit<16>;     // 16-bit unsigned
signal single_bit: bit;     // Single bit (same as bit<1>)
```

### Structured Types

```skalp
struct Packet {
    header: bit<32>;
    payload: bit<64>;
    checksum: bit<8>;
}

signal rx_packet: Packet;
signal header_data: bit<32> = rx_packet.header;
```

### Enumerations

```skalp
enum State {
    Idle,
    Receiving,
    Processing,
    Transmitting
}

signal current_state: State;
```

## Chapter 3: Control Flow

### Conditional Logic

```skalp
on(clk.rise) {
    if enable {
        if mode == Mode::Fast {
            counter = counter + 4;
        } else {
            counter = counter + 1;
        }
    }
}
```

### Pattern Matching

```skalp
signal result: bit<8>;

result = match opcode {
    OpCode::Add => operand_a + operand_b,
    OpCode::Sub => operand_a - operand_b,
    OpCode::Xor => operand_a ^ operand_b,
    _ => 0
};
```

## Chapter 4: Protocols and Interfaces

### Defining Protocols

```skalp
protocol AXI4Lite {
    // Address Write Channel
    out awaddr: bit<32>;
    out awvalid: bit;
    in awready: bit;
    
    // Write Data Channel
    out wdata: bit<32>;
    out wstrb: bit<4>;
    out wvalid: bit;
    in wready: bit;
    
    // Write Response Channel
    in bresp: bit<2>;
    in bvalid: bit;
    out bready: bit;
}
```

### Using Protocols

> **Status:** protocol *declarations* parse today, but protocol-typed entity
> ports (`master axi: AXI4Lite` / `slave axi: AXI4Lite`) are **not yet
> implemented** — using them in an entity is a compile error. Until they land,
> declare the channel signals as ordinary ports:

```skalp
entity axi_write_master {
    in clk: clock;
    in rst: reset;
    in start_write: bit;
    in target_address: bit<32>;
    in awready: bit;
    out awaddr: bit<32>;
    out awvalid: bit;
}

impl axi_write_master {
    signal addr_reg: bit<32> = 0;
    signal valid_reg: bit = 0;

    on(clk.rise) {
        if rst {
            valid_reg = 0;
        } else if start_write {
            addr_reg = target_address;
            valid_reg = 1;
        } else if awready {
            valid_reg = 0;
        }
    }

    awaddr = addr_reg;
    awvalid = valid_reg;
}
```

## Chapter 5: Clock Domains

### Clock Domain Safety

SKALP detects clock domain crossing (CDC) errors at compile time. Every clock
input port defines a clock domain, and every `on(clk.rise)` process belongs to
the domain of its clock — no annotations required. An unsynchronized
cross-domain read that feeds logic is a critical violation and fails the build.

```skalp
entity dual_clock_fifo {
    in clk_write: clock;   // defines the write clock domain
    in clk_read: clock;    // defines the read clock domain
    in rst: reset;

    in write_data: bit<32>;
    in write_enable: bit;

    out read_data: bit<32>;
    in read_enable: bit;
}
```

Signals can also name their domain explicitly with a lifetime annotation, e.g.
`signal src: logic<'clk_write>[32]`.

### Safe CDC Crossing

You write synchronizers explicitly; the `#[cdc]` attribute marks your
hand-written synchronizer register so the CDC analysis knows the crossing is
intentional (the compiler does not insert synchronizers for you). A Gray-coded
pointer crossing looks like this:

```skalp
entity ptr_sync {
    in clk_write: clock;
    in clk_read: clock;
    in write_ptr: bit<4>;
    out read_side_ptr: bit<4>;
}

impl ptr_sync {
    // Gray-code the pointer in the write domain
    signal write_ptr_gray: bit<4> = 0;

    // Explicit 2-stage synchronizer in the read domain
    #[cdc]
    signal gray_meta: bit<4> = 0;
    signal gray_sync: bit<4> = 0;

    on(clk_write.rise) {
        write_ptr_gray = write_ptr ^ (write_ptr >> 1);
    }

    on(clk_read.rise) {
        gray_meta = write_ptr_gray;
        gray_sync = gray_meta;
    }

    // Gray -> binary
    read_side_ptr = gray_sync ^ (gray_sync >> 1) ^ (gray_sync >> 2) ^ (gray_sync >> 3);
}
```

See the [CDC Guide](user/guides/clock-domain-crossing.md) for all the
synchronizer patterns and the analysis severities.

## Chapter 6: Verification

### Assertions

`assert property (expr)` states a boolean invariant over the design's signals:

```skalp
impl fifo {
    // Safety property: never write while full
    assert property (!(write_enable && full));
}
```

Temporal operators (implication `|=>`, `eventually`, explicit `@(posedge ...)`
clocking) are **not yet supported** — properties are boolean expressions over
current signal values.

For end-to-end verification, `skalp ec design.sk` runs formal equivalence
checking (SAT-based) between the RTL and the synthesized gate-level netlist.

### Coverage

> **Status: planned, not yet implemented.** `covergroup` is a reserved
> construct; the syntax below is the design sketch and does not compile today.

```text
covergroup fifo_coverage @(posedge clk) {
    fill_level: coverpoint ptr_diff {
        bins empty = {0};
        bins partial = {[1:14]};
        bins full = {15};
    }

    operations: coverpoint {write_enable, read_enable} {
        bins write_only = {2'b10};
        bins read_only = {2'b01};
        bins simultaneous = {2'b11};
    }

    level_ops: cross fill_level, operations;
}
```

## Chapter 7: Advanced Features

### Generics and Parameters

```skalp
entity parameterized_fifo<const WIDTH: usize, const DEPTH: usize> {
    in clk: clock;
    in rst: reset;
    in write_data: bit<WIDTH>;
    out read_data: bit<WIDTH>;
    in write_enable: bit;
    in read_enable: bit;
    out full: bit;
    out empty: bit;
}

impl parameterized_fifo {
    signal memory: Array<bit<WIDTH>, DEPTH>;
    signal write_ptr: bit<log2(DEPTH)>;
    signal read_ptr: bit<log2(DEPTH)>;
}
```

### Traits and Implementations

```skalp
trait Serializable {
    fn serialize(self) -> bit<Self::BITS>;
    fn deserialize(data: bit<Self::BITS>) -> Self;
    const BITS: usize;
}

impl Serializable for Packet {
    const BITS: usize = 104;  // 32 + 64 + 8
    
    fn serialize(self) -> bit<104> {
        {self.header, self.payload, self.checksum}
    }
    
    fn deserialize(data: bit<104>) -> Packet {
        Packet {
            header: data[103:72],
            payload: data[71:8],
            checksum: data[7:0]
        }
    }
}
```

## Chapter 8: Performance and Optimization

### Intent Attributes

Synthesis intent is expressed with attributes. The intents the compiler acts
on today are `mux_style`, `pipeline_style`, `impl_style`, and `#[unroll]`:

```skalp
entity alu {
    in clk: clock;
    in sel: bit[2];
    in a: bit<32>;
    in b: bit<32>;
    out result: bit<32>;
}

impl alu {
    // Ask for a parallel (one-hot) mux rather than a priority chain
    #[mux_style::parallel]
    result = match sel {
        0b00 => a + b,
        0b01 => a - b,
        0b10 => a & b,
        _ => a | b
    };
}
```

### Design Optimization

Pipelining is written explicitly with registered assignments — each `=` inside
`on(clk.rise)` infers a register stage:

```skalp
entity pipelined_add {
    in clk: clock;
    in operand_a: bit<32>;
    in operand_b: bit<32>;
    out result: bit<32>;
}

impl pipelined_add {
    signal stage1: bit<32> = 0;
    signal stage2: bit<32> = 0;

    on(clk.rise) {
        // Stage 1: compute and register
        stage1 = operand_a + operand_b;

        // Stage 2: output register
        stage2 = stage1;
    }

    result = stage2;
}
```

## Next Steps

- Explore [Examples](examples/) for complete designs
- Read the [Syntax Reference](user/reference/syntax.md) for complete syntax
- Try [GPU Simulation](GPU_SIMULATION.md) for high-performance testing
- Learn about the [Testbench API](user/guides/testbench.md) for testing your designs

## Exercise: Build a UART

Try implementing a UART transmitter using the concepts you've learned:

1. Define entity with clock, reset, data, and control signals
2. Use a state machine with enum for TX states
3. Add a baud rate generator with configurable parameters
4. Include assertions for protocol correctness
5. Add coverage for different data patterns

```skalp
entity uart_tx<const BAUD_RATE: usize> {
    in clk: clock;
    in rst: reset;
    in data: bit<8>;
    in start: bit;
    out tx: bit;
    out busy: bit;
}

// Your implementation here...
```
