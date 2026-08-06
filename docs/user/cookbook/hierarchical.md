# Hierarchical Design Patterns

**Module composition and instantiation in SKALP.**

Hierarchical design is essential for building complex systems from reusable components.

---

## Module Instantiation Syntax

SKALP uses a clean, type-safe syntax for instantiating sub-modules:

```skalp
inst instance_name = ModuleName<generics> {
    input_port: signal_expr,
    input_port2: signal_expr2,
    ...
}

// Outputs are read with dot access:
some_signal = instance_name.output_port
```

**Key features:**
- `inst` introduces an instance
- Curly braces `{}` connect **inputs only** (named, order independent)
- Outputs are read with dot access: `instance.output_port`
- Type-checked connections

---

## Basic Instantiation

### Simple Module Instantiation

**Child and parent modules:**
```skalp
entity Adder {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[8]
}

impl Adder {
    sum = a + b
}

entity Calculator {
    in x: bit[8]
    in y: bit[8]
    out result: bit[8]
}

impl Calculator {
    // Instantiate the adder: braces bind the inputs...
    inst my_adder = Adder {
        a: x,
        b: y
    }

    // ...and the output is read with dot access
    result = my_adder.sum
}
```

**Generated SystemVerilog:**
```systemverilog
module Calculator (
    input [7:0] x,
    input [7:0] y,
    output [7:0] result
);
    wire [7:0] my_adder_sum;

    // Instantiate adder
    Adder my_adder (
        .a(x),
        .b(y),
        .sum(my_adder_sum)
    );

    assign result = my_adder_sum;
endmodule
```

---

## Generic Module Instantiation

### Parameterized Modules

Generic arguments are passed **positionally** in angle brackets: `Register<8>`,
`Fifo<8, 16>`.

**Generic child with parents at two widths:**
```skalp
entity Register<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset
    in data_in: bit[WIDTH]
    out data_out: bit[WIDTH]
}

impl Register {
    signal value: bit[WIDTH] = 0

    on(clk.rise) {
        if (rst) {
            value = 0
        } else {
            value = data_in
        }
    }

    data_out = value
}

entity DataPath {
    in clk: clock
    in rst: reset
    in byte_in: bit[8]
    in word_in: bit[32]
    out byte_out: bit[8]
    out word_out: bit[32]
}

impl DataPath {
    // 8-bit register
    inst byte_reg = Register<8> {
        clk: clk,
        rst: rst,
        data_in: byte_in
    }

    // 32-bit register
    inst word_reg = Register<32> {
        clk: clk,
        rst: rst,
        data_in: word_in
    }

    byte_out = byte_reg.data_out
    word_out = word_reg.data_out
}
```

---

## Multiple Instances

### Instantiating Multiple Copies

A parent generic can be passed straight through to child instances
(`Register<WIDTH>` below). Reading `stageN.data_out` chains the stages —
no intermediate signals needed.

**Pipeline with multiple stages:**
```skalp
entity Register<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset
    in data_in: bit[WIDTH]
    out data_out: bit[WIDTH]
}

impl Register {
    signal value: bit[WIDTH] = 0

    on(clk.rise) {
        if (rst) {
            value = 0
        } else {
            value = data_in
        }
    }

    data_out = value
}

entity Pipeline3Stage<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset
    in data_in: bit[WIDTH]
    out data_out: bit[WIDTH]
}

impl Pipeline3Stage {
    inst stage1 = Register<WIDTH> {
        clk: clk,
        rst: rst,
        data_in: data_in
    }

    inst stage2 = Register<WIDTH> {
        clk: clk,
        rst: rst,
        data_in: stage1.data_out
    }

    inst stage3 = Register<WIDTH> {
        clk: clk,
        rst: rst,
        data_in: stage2.data_out
    }

    data_out = stage3.data_out
}
```

---

## Hierarchical Example: ALU with Components

### Complete ALU from Components

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

// Top-level ALU
entity ALU<const WIDTH: nat = 32> {
    in clk: clock
    in a: bit[WIDTH]
    in b: bit[WIDTH]
    in op: bit[3]
    out result: bit[WIDTH]
    out zero: bit
}

impl ALU {
    signal result_comb: bit[WIDTH]

    // Instantiate components (inputs only in the braces)
    inst adder = Adder<WIDTH> {
        a: a,
        b: b
    }

    inst comparator = Comparator<WIDTH> {
        a: a,
        b: b
    }

    inst shifter = Shifter<WIDTH> {
        data: a,
        shift_amt: b[4:0],
        left: op[0]
    }

    // Select result based on operation; component outputs
    // are read with dot access
    result_comb = match op {
        0b000 => adder.sum,          // ADD
        0b001 => a - b,              // SUB (inline)
        0b010 => a & b,              // AND (inline)
        0b011 => a | b,              // OR (inline)
        0b100 => a ^ b,              // XOR (inline)
        0b101 => shifter.result,     // SHIFT (use shifter)
        0b110 => if comparator.lt { 1 } else { 0 },  // SLT (use comparator)
        0b111 => if comparator.eq { 1 } else { 0 }   // SEQ (use comparator)
    }

    // Register output: inside on(clk.rise), `=` is a registered assignment
    on(clk.rise) {
        result = result_comb
        zero = if result_comb == 0 { 1 } else { 0 }
    }
}
```

---

## Port Connection Patterns

### Direct Connection
```skalp
inst instance = Module {
    port: signal
}
```

### Expression Connection
```skalp
inst instance = Module {
    port: signal1 + signal2,
    enable: !reset && valid
}
```

### Constant Connection
```skalp
inst instance = Module {
    mode: 0b10,
    size: 16
}
```

### Bit Slicing Connection
```skalp
inst instance = Module {
    upper: data[31:16],
    lower: data[15:0]
}
```

### Reading Outputs
```skalp
// Outputs are never listed in the braces — read them with dot access
signal total: bit[8] = instance.sum
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
            memory[waddr] = wdata
        }
        rdata_reg = memory[raddr]
    }

    rdata = rdata_reg
}
```

### Composite FIFO

(The RAM definition is repeated so the example is self-contained.)

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
            memory[waddr] = wdata
        }
        rdata_reg = memory[raddr]
    }

    rdata = rdata_reg
}

entity Fifo<const WIDTH: nat = 8, const DEPTH: nat = 16> {
    in clk: clock
    in rst: reset
    in wr_en: bit
    in wr_data: bit[WIDTH]
    out full: bit
    in rd_en: bit
    out rd_data: bit[WIDTH]
    out empty: bit
}

impl Fifo {
    // Control signals
    signal wr_ptr: nat[clog2(DEPTH)] = 0
    signal rd_ptr: nat[clog2(DEPTH)] = 0
    signal count: nat[clog2(DEPTH+1)] = 0

    // Status flags
    empty = (count == 0)
    full = (count == DEPTH)

    // Instantiate dual-port RAM for storage
    inst ram = DualPortRAM<WIDTH, DEPTH> {
        clk: clk,
        we: wr_en && !full,
        waddr: wr_ptr,
        wdata: wr_data,
        raddr: rd_ptr
    }

    rd_data = ram.rdata

    // Pointer and count management
    on(clk.rise) {
        if (rst) {
            wr_ptr = 0
            rd_ptr = 0
            count = 0
        } else {
            let wr_ok = wr_en && !full
            let rd_ok = rd_en && !empty

            if (wr_ok) {
                wr_ptr = (wr_ptr + 1) % DEPTH
            }

            if (rd_ok) {
                rd_ptr = (rd_ptr + 1) % DEPTH
            }

            if (wr_ok && !rd_ok) {
                count = count + 1
            } else if (!wr_ok && rd_ok) {
                count = count - 1
            }
        }
    }
}
```

---

## Complete Example: CPU Datapath

### CPU with Hierarchical Components

Note that instances may reference each other's outputs freely — `regfile`
reads `alu.result` even though the ALU is instantiated later in the file.

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
            regs[waddr] = wdata
        }
    }

    rdata1 = if raddr1 == 0 { 0 } else { regs[raddr1] }
    rdata2 = if raddr2 == 0 { 0 } else { regs[raddr2] }
}

// Combinational ALU component
entity SimpleALU<const WIDTH: nat = 32> {
    in a: bit[WIDTH]
    in b: bit[WIDTH]
    in op: bit[3]
    out result: bit[WIDTH]
    out zero: bit
}

impl SimpleALU {
    result = match op {
        0b000 => a + b,
        0b001 => a - b,
        0b010 => a & b,
        0b011 => a | b,
        0b100 => a ^ b,
        _ => 0
    }
    zero = if result == 0 { 1 } else { 0 }
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

    signal reg_write_en: bit

    // Instantiate register file
    inst regfile = RegisterFile<32, 32> {
        clk: clk,
        we: reg_write_en,
        waddr: rd,
        wdata: alu.result,
        raddr1: rs,
        raddr2: rt
    }

    // Instantiate ALU
    inst alu = SimpleALU<32> {
        a: regfile.rdata1,
        b: regfile.rdata2,
        op: opcode[2:0]
    }

    // Control logic
    reg_write_en = if opcode[5:3] == 0b000 { 1 } else { 0 }

    result = alu.result
}
```

---

## Best Practices

### 1. Clear Naming

```skalp
// Good: Descriptive instance names
inst input_buffer = Fifo<8, 16> { ... }
inst output_buffer = Fifo<8, 16> { ... }

// Avoid: Generic names
inst fifo1 = Fifo<8, 16> { ... }
inst fifo2 = Fifo<8, 16> { ... }
```

### 2. Read Outputs Where You Use Them

```skalp
// Dot access means no boilerplate connection signals
inst alu = ALU {
    a: operand_a,
    b: operand_b
}

result = alu.result
overflow_flag = alu.overflow
```

### 3. Use Named Connections

```skalp
// Good: All input connections explicit
inst adder = Adder {
    a: input_a,
    b: input_b
}

// No positional connection in SKALP - always named!
```

### 4. Clock Distribution

```skalp
// Pass clock to all sub-modules
impl TopLevel {
    inst module1 = Module1 {
        clk: clk,  // Same clock
        ...
    }

    inst module2 = Module2 {
        clk: clk,  // Same clock
        ...
    }
}
```

### 5. Reset Distribution

```skalp
// Distribute reset properly
impl TopLevel {
    inst module1 = Module1 {
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
entity Controller {
    in clk: clock
    in rst: reset
    in start: bit
    in done: bit
    out enable: bit
}

impl Controller {
    signal running: bit = 0

    on(clk.rise) {
        if (rst) {
            running = 0
        } else if (start) {
            running = 1
        } else if (done) {
            running = 0
        }
    }

    enable = running
}

entity Datapath {
    in clk: clock
    in rst: reset
    in enable: bit
    out done: bit
}

impl Datapath {
    signal counter: bit[4] = 0

    on(clk.rise) {
        if (rst) {
            counter = 0
        } else if (enable) {
            counter = counter + 1
        }
    }

    done = (counter == 15)
}

entity System {
    in clk: clock
    in rst: reset
    in start: bit
    out done: bit
}

impl System {
    inst controller = Controller {
        clk: clk,
        rst: rst,
        start: start,
        done: datapath.done
    }

    inst datapath = Datapath {
        clk: clk,
        rst: rst,
        enable: controller.enable
    }

    done = datapath.done
}
```

### Pattern: Pipeline Stages

**Chain of processing stages:**
```skalp
impl Pipeline {
    inst stage0 = Stage { d: input_data }
    inst stage1 = Stage { d: stage0.q }
    inst stage2 = Stage { d: stage1.q }
    inst stage3 = Stage { d: stage2.q }

    output_data = stage3.q
}
```

### Pattern: Arbiter + Clients

**Multiple clients, one arbiter:**
```skalp
impl System {
    inst arbiter = Arbiter {
        req0: client0.req,
        req1: client1.req
    }

    inst client0 = Client { grant: arbiter.grant0 }
    inst client1 = Client { grant: arbiter.grant1 }
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
inst shifter = Shifter {
    data: a,              // Signal connection
    shift_amt: b[4:0],    // Range slice connection
    shift_left: op[0],    // Bit-select connection
    enable: 1             // Constant connection
}

shift_out = shifter.result
```

### Usage

```bash
# Build with gate-level target (hierarchical auto-detected)
skalp build design.sk --target gates -o output/

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

---

**Key Takeaways:**
- Use `inst instance = Module { ... }` syntax; braces bind inputs only
- Read outputs with dot access: `instance.output_port`
- Generic arguments are positional: `Fifo<8, 16>`
- Pass clocks and resets explicitly
- Build complex systems from simple, reusable components
- Use hierarchical design for maintainability and reusability
- Gate-level synthesis fully supports all connection types including range slices and bit-selects
