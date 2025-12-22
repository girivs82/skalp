# Hierarchical Design Patterns

**Module composition and instantiation in SKALP.**

Hierarchical design is essential for building complex systems from reusable components.

---

## Module Instantiation Syntax

SKALP uses a clean, type-safe syntax for instantiating sub-modules:

```skalp
let instance_name = ModuleName<generics> {
    port_name: signal_expr,
    port_name2: signal_expr2,
    ...
}
```

**Key features:**
- `let` introduces an instance
- Curly braces `{}` for port connections
- Named port mapping (order independent)
- Type-checked connections

---

## Basic Instantiation

### Simple Module Instantiation

**Child module:**
```skalp
entity Adder {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[8]
}

impl Adder {
    sum = a + b
}
```

**Parent module:**
```skalp
entity Calculator {
    in x: bit[8]
    in y: bit[8]
    out result: bit[8]
}

impl Calculator {
    signal internal_sum: bit[8]

    // Instantiate the adder
    let my_adder = Adder {
        a: x,
        b: y,
        sum: internal_sum
    }

    result = internal_sum
}
```

**Generated SystemVerilog:**
```systemverilog
module Calculator (
    input [7:0] x,
    input [7:0] y,
    output [7:0] result
);
    wire [7:0] internal_sum;

    // Instantiate adder
    Adder my_adder (
        .a(x),
        .b(y),
        .sum(internal_sum)
    );

    assign result = internal_sum;
endmodule
```

---

## Generic Module Instantiation

### Parameterized Modules

**Generic child:**
```skalp
entity Register<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset
    in data_in: bit[WIDTH]
    out data_out: bit[WIDTH]
}

impl Register {
    signal reg: bit[WIDTH] = 0

    on(clk.rise) {
        if (rst) {
            reg <= 0
        } else {
            reg <= data_in
        }
    }

    data_out = reg
}
```

**Parent with multiple widths:**
```skalp
entity DataPath {
    in clk: clock
    in rst: reset
    in byte_in: bit[8]
    in word_in: bit[32]
    out byte_out: bit[8]
    out word_out: bit[32]
}

impl DataPath {
    signal byte_reg_out: bit[8]
    signal word_reg_out: bit[32]

    // 8-bit register
    let byte_reg = Register<WIDTH = 8> {
        clk: clk,
        rst: rst,
        data_in: byte_in,
        data_out: byte_reg_out
    }

    // 32-bit register
    let word_reg = Register<WIDTH = 32> {
        clk: clk,
        rst: rst,
        data_in: word_in,
        data_out: word_reg_out
    }

    byte_out = byte_reg_out
    word_out = word_reg_out
}
```

---

## Multiple Instances

### Instantiating Multiple Copies

**Pipeline with multiple stages:**
```skalp
entity Pipeline3Stage<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset
    in data_in: bit[WIDTH]
    out data_out: bit[WIDTH]
}

impl Pipeline3Stage {
    signal stage1_out: bit[WIDTH]
    signal stage2_out: bit[WIDTH]
    signal stage3_out: bit[WIDTH]

    let stage1 = Register<WIDTH> {
        clk: clk,
        rst: rst,
        data_in: data_in,
        data_out: stage1_out
    }

    let stage2 = Register<WIDTH> {
        clk: clk,
        rst: rst,
        data_in: stage1_out,
        data_out: stage2_out
    }

    let stage3 = Register<WIDTH> {
        clk: clk,
        rst: rst,
        data_in: stage2_out,
        data_out: stage3_out
    }

    data_out = stage3_out
}
```

---

## Hierarchical Example: ALU with Components

### Complete ALU from Components

**Component modules:**
```skalp
// Adder module
entity Adder<const WIDTH: nat = 32> {
    in a: bit[WIDTH]
    in b: bit[WIDTH]
    out sum: bit[WIDTH]
    out carry: bit
}

impl Adder {
    signal result: bit[WIDTH+1] = a + b
    sum = result[WIDTH-1:0]
    carry = result[WIDTH]
}

// Comparator module
entity Comparator<const WIDTH: nat = 32> {
    in a: bit[WIDTH]
    in b: bit[WIDTH]
    out lt: bit  // a < b
    out eq: bit  // a == b
}

impl Comparator {
    lt = if a < b { 1 } else { 0 }
    eq = if a == b { 1 } else { 0 }
}

// Shifter module
entity Shifter<const WIDTH: nat = 32> {
    in data: bit[WIDTH]
    in shift_amt: bit[5]
    in left: bit  // 1 = left, 0 = right
    out result: bit[WIDTH]
}

impl Shifter {
    result = if left {
        data << shift_amt
    } else {
        data >> shift_amt
    }
}
```

**Top-level ALU:**
```skalp
entity ALU<const WIDTH: nat = 32> {
    in clk: clock
    in a: bit[WIDTH]
    in b: bit[WIDTH]
    in op: bit[3]
    out result: bit[WIDTH]
    out zero: bit
}

impl ALU {
    // Internal signals
    signal add_sum: bit[WIDTH]
    signal add_carry: bit
    signal cmp_lt: bit
    signal cmp_eq: bit
    signal shift_result: bit[WIDTH]
    signal result_comb: bit[WIDTH]

    // Instantiate components
    let adder = Adder<WIDTH> {
        a: a,
        b: b,
        sum: add_sum,
        carry: add_carry
    }

    let comparator = Comparator<WIDTH> {
        a: a,
        b: b,
        lt: cmp_lt,
        eq: cmp_eq
    }

    let shifter = Shifter<WIDTH> {
        data: a,
        shift_amt: b[4:0],
        left: op[0],
        result: shift_result
    }

    // Select result based on operation
    result_comb = match op {
        0b000 => add_sum,           // ADD
        0b001 => a - b,             // SUB (inline)
        0b010 => a & b,             // AND (inline)
        0b011 => a | b,             // OR (inline)
        0b100 => a ^ b,             // XOR (inline)
        0b101 => shift_result,      // SHIFT (use shifter)
        0b110 => if cmp_lt { 1 } else { 0 },  // SLT (use comparator)
        0b111 => if cmp_eq { 1 } else { 0 },  // SEQ (use comparator)
        _ => 0
    }

    // Register output
    on(clk.rise) {
        result <= result_comb
        zero <= if result_comb == 0 { 1 } else { 0 }
    }
}
```

---

## Port Connection Patterns

### Direct Connection
```skalp
let instance = Module {
    port: signal
}
```

### Expression Connection
```skalp
let instance = Module {
    port: signal1 + signal2,
    enable: !reset && valid
}
```

### Constant Connection
```skalp
let instance = Module {
    mode: 0b10,
    size: 16
}
```

### Bit Slicing Connection
```skalp
let instance = Module {
    upper: data[31:16],
    lower: data[15:0]
}
```

---

## Complete Example: Synchronous FIFO with Dual-Port RAM

### Building Block: Dual-Port RAM

```skalp
entity DualPortRAM<const WIDTH: nat = 8, const DEPTH: nat = 16> {
    in clk: clock
    in we: bit
    in waddr: nat[clog2(DEPTH)]
    in wdata: bit[WIDTH]
    in raddr: nat[clog2(DEPTH)]
    out rdata: bit[WIDTH]
}

impl DualPortRAM {
    signal memory: [bit[WIDTH]; DEPTH]
    signal rdata_reg: bit[WIDTH] = 0

    on(clk.rise) {
        if (we) {
            memory[waddr] <= wdata
        }
        rdata_reg <= memory[raddr]
    }

    rdata = rdata_reg
}
```

### Composite FIFO

```skalp
entity FIFO<const WIDTH: nat = 8, const DEPTH: nat = 16> {
    in clk: clock
    in rst: reset
    in wr_en: bit
    in wr_data: bit[WIDTH]
    out full: bit
    in rd_en: bit
    out rd_data: bit[WIDTH]
    out empty: bit
}

impl FIFO {
    // Control signals
    signal wr_ptr: nat[clog2(DEPTH)] = 0
    signal rd_ptr: nat[clog2(DEPTH)] = 0
    signal count: nat[clog2(DEPTH+1)] = 0

    // Status flags
    empty = (count == 0)
    full = (count == DEPTH)

    // Instantiate dual-port RAM for storage
    let ram = DualPortRAM<WIDTH, DEPTH> {
        clk: clk,
        we: wr_en && !full,
        waddr: wr_ptr,
        wdata: wr_data,
        raddr: rd_ptr,
        rdata: rd_data
    }

    // Pointer and count management
    on(clk.rise) {
        if (rst) {
            wr_ptr <= 0
            rd_ptr <= 0
            count <= 0
        } else {
            signal wr_ok: bit = wr_en && !full
            signal rd_ok: bit = rd_en && !empty

            if (wr_ok) {
                wr_ptr <= (wr_ptr + 1) % DEPTH
            }

            if (rd_ok) {
                rd_ptr <= (rd_ptr + 1) % DEPTH
            }

            if (wr_ok && !rd_ok) {
                count <= count + 1
            } else if (!wr_ok && rd_ok) {
                count <= count - 1
            }
        }
    }
}
```

---

## Complete Example: CPU Datapath

### CPU with Hierarchical Components

```skalp
// Register file component
entity RegisterFile<const WIDTH: nat = 32, const REGS: nat = 32> {
    in clk: clock
    in we: bit
    in waddr: nat[clog2(REGS)]
    in wdata: bit[WIDTH]
    in raddr1: nat[clog2(REGS)]
    out rdata1: bit[WIDTH]
    in raddr2: nat[clog2(REGS)]
    out rdata2: bit[WIDTH]
}

impl RegisterFile {
    signal regs: [bit[WIDTH]; REGS]

    on(clk.rise) {
        if (we && waddr != 0) {  // R0 is hardwired to 0
            regs[waddr] <= wdata
        }
    }

    rdata1 = if raddr1 == 0 { 0 } else { regs[raddr1] }
    rdata2 = if raddr2 == 0 { 0 } else { regs[raddr2] }
}

// Simple CPU datapath
entity CPU {
    in clk: clock
    in rst: reset
    in instruction: bit[32]
    out result: bit[32]
}

impl CPU {
    // Decoded instruction fields
    signal rs: nat[5] = instruction[25:21]
    signal rt: nat[5] = instruction[20:16]
    signal rd: nat[5] = instruction[15:11]
    signal opcode: bit[6] = instruction[31:26]

    // Internal signals
    signal reg_rd1: bit[32]
    signal reg_rd2: bit[32]
    signal alu_result: bit[32]
    signal alu_zero: bit
    signal reg_write_en: bit

    // Instantiate register file
    let regfile = RegisterFile<WIDTH = 32, REGS = 32> {
        clk: clk,
        we: reg_write_en,
        waddr: rd,
        wdata: alu_result,
        raddr1: rs,
        rdata1: reg_rd1,
        raddr2: rt,
        rdata2: reg_rd2
    }

    // Instantiate ALU
    let alu = ALU<WIDTH = 32> {
        clk: clk,
        a: reg_rd1,
        b: reg_rd2,
        op: opcode[2:0],
        result: alu_result,
        zero: alu_zero
    }

    // Control logic
    reg_write_en = if opcode[5:3] == 0b000 { 1 } else { 0 }

    result = alu_result
}
```

---

## Best Practices

### 1. Clear Naming

```skalp
// Good: Descriptive instance names
let input_buffer = FIFO<WIDTH = 8, DEPTH = 16> { ... }
let output_buffer = FIFO<WIDTH = 8, DEPTH = 16> { ... }

// Avoid: Generic names
let fifo1 = FIFO<WIDTH = 8, DEPTH = 16> { ... }
let fifo2 = FIFO<WIDTH = 8, DEPTH = 16> { ... }
```

### 2. Group Related Signals

```skalp
// Declare connection signals near instantiation
signal alu_a: bit[32]
signal alu_b: bit[32]
signal alu_result: bit[32]

let alu = ALU {
    a: alu_a,
    b: alu_b,
    result: alu_result
}
```

### 3. Use Named Connections

```skalp
// Good: All connections explicit
let adder = Adder {
    a: input_a,
    b: input_b,
    sum: output_sum
}

// No positional connection in SKALP - always named!
```

### 4. Clock Distribution

```skalp
// Pass clock to all sub-modules
impl TopLevel {
    let module1 = Module1 {
        clk: clk,  // Same clock
        ...
    }

    let module2 = Module2 {
        clk: clk,  // Same clock
        ...
    }
}
```

### 5. Reset Distribution

```skalp
// Distribute reset properly
impl TopLevel {
    let module1 = Module1 {
        clk: clk,
        rst: rst,  // Synchronous reset
        ...
    }
}
```

---

## Common Patterns

### Pattern: Datapath + Control

**Separate datapath and control:**
```skalp
entity System {
    in clk: clock
    in rst: reset
    in start: bit
    out done: bit
}

impl System {
    signal control_enable: bit
    signal datapath_done: bit

    let controller = Controller {
        clk: clk,
        rst: rst,
        start: start,
        enable: control_enable,
        done: datapath_done
    }

    let datapath = Datapath {
        clk: clk,
        rst: rst,
        enable: control_enable,
        done: datapath_done
    }

    done = datapath_done
}
```

### Pattern: Pipeline Stages

**Chain of processing stages:**
```skalp
impl Pipeline {
    signal s0_to_s1: bit[WIDTH]
    signal s1_to_s2: bit[WIDTH]
    signal s2_to_s3: bit[WIDTH]

    let stage0 = Stage { in: input, out: s0_to_s1 }
    let stage1 = Stage { in: s0_to_s1, out: s1_to_s2 }
    let stage2 = Stage { in: s1_to_s2, out: s2_to_s3 }
    let stage3 = Stage { in: s2_to_s3, out: output }
}
```

### Pattern: Arbiter + Clients

**Multiple clients, one arbiter:**
```skalp
impl System {
    signal client0_req: bit
    signal client0_grant: bit
    signal client1_req: bit
    signal client1_grant: bit

    let arbiter = Arbiter {
        req0: client0_req,
        req1: client1_req,
        grant0: client0_grant,
        grant1: client1_grant
    }

    let client0 = Client { grant: client0_grant, req: client0_req }
    let client1 = Client { grant: client1_grant, req: client1_req }
}
```

---

## Debugging Hierarchical Designs

### Signal Naming

Internal signals automatically get hierarchical names:
```
TopLevel.sub_module.internal_signal
```

### Testbench Access

Access sub-module signals in simulation:
```rust
// In testbench
tb.set("input", 42u8);
let sub_output = tb.get_as::<u8>("my_instance.output").await;
```

---

## Gate-Level Hierarchical Synthesis

When targeting gate-level output (`--target gates`), SKALP automatically detects hierarchical designs and optimizes them with per-instance specialization.

### How It Works

1. **Auto-Detection**: Multi-module designs trigger hierarchical synthesis
2. **Per-Instance Optimization**: Each instance is synthesized independently
3. **Port Stitching**: All connection types are properly handled
4. **Cross-Boundary Cleanup**: DCE and constant propagation after flattening

### Supported Connection Types

All connection patterns work with gate-level synthesis:

```skalp
let shifter = Shifter {
    data: a,              // Signal connection
    shift_amt: b[4:0],    // Range slice connection
    shift_left: op[0],    // Bit-select connection
    enable: 1,            // Constant connection
    result: shift_out
}
```

### Usage

```bash
# Build with gate-level target (hierarchical auto-detected)
skalp build -s design.sk --target gates -o output/

# Example output:
# [STITCH] Instance 'top.shifter' has 4 port connections
# [STITCH]   ✓ top.shifter.shift_left <-> top.op[0]
# [STITCH]   ✓ top.shifter.shift_amt <-> top.b[4:0] (range: 5 bits)
```

---

## See Also

- [Combinational Patterns](combinational.md) - Basic building blocks
- [Sequential Patterns](sequential.md) - Registers and counters
- [Memory Patterns](memories.md) - RAMs and FIFOs
- [Syntax Reference](../reference/syntax.md) - Language syntax
- [What's New](../../WHATS_NEW.md) - Latest features including hierarchical synthesis

---

**Key Takeaways:**
- Use `let instance = Module { ... }` syntax
- Always use named port connections
- Pass clocks and resets explicitly
- Build complex systems from simple, reusable components
- Use hierarchical design for maintainability and reusability
- Gate-level synthesis fully supports all connection types including range slices and bit-selects
