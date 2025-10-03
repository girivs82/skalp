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
            counter <= 0;
        } else {
            counter <= counter + 1;
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
            counter <= counter + 4;
        } else {
            counter <= counter + 1;
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

```skalp
entity axi_master {
    in clk: clock;
    in rst: reset;
    master axi: AXI4Lite;  // Protocol instance
}

impl axi_master {
    on(clk.rise) {
        if start_write {
            axi.awaddr <= target_address;
            axi.awvalid <= 1;
        }
    }
}
```

## Chapter 5: Clock Domains

### Clock Domain Safety

SKALP prevents clock domain crossing (CDC) errors at compile time:

```skalp
entity dual_clock_fifo<'clk_w, 'clk_r> {
    in clk_write<'clk_w>: clock;
    in clk_read<'clk_r>: clock;
    in rst: reset;
    
    in write_data<'clk_w>: bit<32>;
    in write_enable<'clk_w>: bit;
    
    out read_data<'clk_r>: bit<32>;
    in read_enable<'clk_r>: bit;
}
```

### Safe CDC Crossing

```skalp
impl dual_clock_fifo {
    // Write domain signals
    signal write_ptr<'clk_w>: bit<4>;
    signal memory<'clk_w>: Array<bit<32>, 16>;
    
    // Read domain signals  
    signal read_ptr<'clk_r>: bit<4>;
    
    // Safe crossing with gray code
    signal write_ptr_gray<'clk_w>: bit<4>;
    signal write_ptr_sync<'clk_r>: bit<4>;
    
    on(clk_write.rise) {
        if write_enable && !full {
            memory[write_ptr] <= write_data;
            write_ptr <= write_ptr + 1;
            write_ptr_gray <= binary_to_gray(write_ptr + 1);
        }
    }
    
    on(clk_read.rise) {
        write_ptr_sync <= synchronize(write_ptr_gray);
    }
}
```

## Chapter 6: Verification

### Assertions

```skalp
impl fifo {
    // Safety property: never overflow
    assert property (write_enable -> !full)
        @(posedge clk_write);
    
    // Liveness property: data eventually flows
    assert property (write_enable |=> eventually read_enable)
        @(posedge clk_write);
}
```

### Coverage

```skalp
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

### Intent Declarations

```skalp
@intent("Low latency arithmetic unit")
entity alu {
    @intent("Pipeline for high frequency")
    signal pipeline_stage1: bit<32>;
    signal pipeline_stage2: bit<32>;
    
    @intent("Optimize for area")
    signal temp_storage: bit<128>;
}
```

### Design Optimization

```skalp
impl alu {
    // Pipeline for performance
    on(clk.rise) {
        // Stage 1: Input registration
        pipeline_stage1 <= operand_a + operand_b;
        
        // Stage 2: Output registration  
        result <= pipeline_stage1;
    }
    
    // Parallel execution for throughput
    match operation {
        Op::Add => result = adder_unit(operand_a, operand_b),
        Op::Mul => result = multiplier_unit(operand_a, operand_b),
        Op::Div => result = divider_unit(operand_a, operand_b)
    }
}
```

## Next Steps

- Explore [Examples](examples/) for complete designs
- Read the [Language Specification](language-spec.md) for complete syntax
- Try [GPU Simulation](gpu-simulation.md) for high-performance testing
- Learn about [Safety Features](safety.md) for mission-critical designs

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
