# SystemVerilog to SKALP Migration Guide

**For hardware engineers who know SystemVerilog and want to learn SKALP.**

This guide shows you how to translate SystemVerilog patterns to SKALP idioms.

---

## Quick Comparison Table

| Concept | SystemVerilog | SKALP | Notes |
|---------|---------------|-------|-------|
| **Module** | `module Counter` | `entity Counter` | Entity = module interface |
| **Implementation** | Inside module | `impl Counter { }` | Separates interface from implementation |
| **Clock input** | `input wire clk` | `in clk: clock` | Dedicated `clock` type |
| **Reset input** | `input wire rst` | `in rst: reset` | Dedicated `reset` type |
| **Input port** | `input wire [7:0] data` | `in data: bit[8]` | Type-safe, no `wire` keyword |
| **Output port** | `output wire [7:0] out` | `out out: bit[8]` | Type-safe |
| **Register** | `reg [7:0] counter` | `signal counter: bit[8]` | Always called `signal` |
| **Wire** | `wire [7:0] sum` | `signal sum: bit[8]` | No distinction in SKALP |
| **Sequential** | `always_ff @(posedge clk)` | `on(clk.rise)` | Cleaner syntax |
| **Combinational** | `assign out = in` | `out = in` | No `assign` keyword |
| **Non-blocking** | `counter <= counter + 1` | `counter <= counter + 1` | Same! |
| **Blocking** | `temp = a + b` | Not in sequential blocks | Use signals instead |
| **Case** | `case (op) ... endcase` | `match op { ... }` | Expression-based |
| **Ternary** | `out = sel ? a : b` | `out = if sel { a } else { b }` | If-expressions |
| **Width** | `[7:0]` or `[WIDTH-1:0]` | `bit[8]` or `bit[WIDTH]` | No `-1` needed! |
| **Parameter** | `parameter WIDTH = 8` | `const WIDTH: nat = 8` | In entity generics |

---

## Example 1: Simple Counter

### SystemVerilog Version
```systemverilog
module Counter #(
    parameter WIDTH = 8
)(
    input wire clk,
    input wire rst,
    output wire [WIDTH-1:0] count
);
    reg [WIDTH-1:0] counter;

    always_ff @(posedge clk) begin
        if (rst) begin
            counter <= '0;
        end else begin
            counter <= counter + 1'b1;
        end
    end

    assign count = counter;
endmodule
```

### SKALP Version
```skalp
entity Counter<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset
    out count: bit[WIDTH]
}

impl Counter {
    signal counter: bit[WIDTH] = 0

    on(clk.rise) {
        if (rst) {
            counter <= 0
        } else {
            counter <= counter + 1
        }
    }

    count = counter
}
```

### Key Differences:
1. **No `wire`/`reg` distinction** - Everything is `signal`
2. **No `[WIDTH-1:0]`** - Just `bit[WIDTH]`
3. **No `assign`** - Direct assignment for combinational
4. **`entity`/`impl` separation** - Clearer structure
5. **Type-safe clock/reset** - `clock` and `reset` types

---

## Example 2: Combinational Logic (Adder)

### SystemVerilog Version
```systemverilog
module Adder #(
    parameter WIDTH = 8
)(
    input wire [WIDTH-1:0] a,
    input wire [WIDTH-1:0] b,
    output wire [WIDTH-1:0] sum,
    output wire carry
);
    wire [WIDTH:0] full_sum;

    assign full_sum = a + b;
    assign sum = full_sum[WIDTH-1:0];
    assign carry = full_sum[WIDTH];
endmodule
```

### SKALP Version
```skalp
entity Adder<const WIDTH: nat = 8> {
    in a: bit[WIDTH]
    in b: bit[WIDTH]
    out sum: bit[WIDTH]
    out carry: bit
}

impl Adder {
    signal full_sum: bit[WIDTH+1] = a + b
    sum = full_sum[WIDTH-1:0]
    carry = full_sum[WIDTH]
}
```

### Key Differences:
1. **No `assign` keyword** - Cleaner
2. **Width arithmetic works** - `bit[WIDTH+1]` is valid
3. **Type inference** - `full_sum` type can be inferred
4. **No wire declarations** - Signals inferred from usage

---

## Example 3: Multiplexer

### SystemVerilog Version
```systemverilog
module Mux4 #(
    parameter WIDTH = 8
)(
    input wire [WIDTH-1:0] in0,
    input wire [WIDTH-1:0] in1,
    input wire [WIDTH-1:0] in2,
    input wire [WIDTH-1:0] in3,
    input wire [1:0] sel,
    output wire [WIDTH-1:0] out
);
    assign out = (sel == 2'b00) ? in0 :
                 (sel == 2'b01) ? in1 :
                 (sel == 2'b10) ? in2 : in3;
endmodule
```

### SKALP Version
```skalp
entity Mux4<const WIDTH: nat = 8> {
    in in0: bit[WIDTH]
    in in1: bit[WIDTH]
    in in2: bit[WIDTH]
    in in3: bit[WIDTH]
    in sel: bit[2]
    out out: bit[WIDTH]
}

impl Mux4 {
    out = match sel {
        0b00 => in0,
        0b01 => in1,
        0b10 => in2,
        0b11 => in3,
        _ => 0  // Default case
    }
}
```

### Key Differences:
1. **Pattern matching** - Much cleaner than nested ternaries
2. **Expression-based** - `match` returns a value
3. **Exhaustiveness checking** - Compiler warns if you miss a case
4. **Binary literals** - `0b00` instead of `2'b00`

---

## Example 4: State Machine (UART Transmitter)

### SystemVerilog Version
```systemverilog
module UART_TX (
    input wire clk,
    input wire rst,
    input wire [7:0] data_in,
    input wire start,
    output reg tx,
    output reg busy
);
    typedef enum logic [1:0] {
        IDLE = 2'b00,
        START = 2'b01,
        DATA = 2'b10,
        STOP = 2'b11
    } state_t;

    state_t state, next_state;
    reg [2:0] bit_index;

    always_ff @(posedge clk) begin
        if (rst) begin
            state <= IDLE;
        end else begin
            state <= next_state;
        end
    end

    always_comb begin
        next_state = state;
        case (state)
            IDLE: if (start) next_state = START;
            START: next_state = DATA;
            DATA: if (bit_index == 7) next_state = STOP;
            STOP: next_state = IDLE;
        endcase
    end

    // ... rest of logic
endmodule
```

### SKALP Version
```skalp
entity UART_TX {
    in clk: clock
    in rst: reset
    in data_in: bit[8]
    in start: bit
    out tx: bit
    out busy: bit
}

enum State {
    Idle,
    Start,
    Data,
    Stop
}

impl UART_TX {
    signal state: State = State::Idle
    signal bit_index: nat[3]

    on(clk.rise) {
        if (rst) {
            state <= State::Idle
        } else {
            state <= match state {
                State::Idle => if start { State::Start } else { State::Idle },
                State::Start => State::Data,
                State::Data => if bit_index == 7 { State::Stop } else { State::Data },
                State::Stop => State::Idle
            }
        }
    }

    // ... rest of logic
}
```

### Key Differences:
1. **Enum definition** - Outside impl block
2. **No `typedef enum`** - Just `enum State`
3. **Match for state transitions** - More concise
4. **Scoped enum values** - `State::Idle` not just `Idle`

---

## Example 5: Registered ALU

### SystemVerilog Version
```systemverilog
module ALU (
    input wire clk,
    input wire [31:0] a,
    input wire [31:0] b,
    input wire [2:0] op,
    output reg [31:0] result,
    output reg zero
);
    wire [31:0] result_comb;

    always_comb begin
        case (op)
            3'b000: result_comb = a + b;
            3'b001: result_comb = a - b;
            3'b010: result_comb = a & b;
            3'b011: result_comb = a | b;
            3'b100: result_comb = a ^ b;
            3'b101: result_comb = a << b[4:0];
            3'b110: result_comb = a >> b[4:0];
            3'b111: result_comb = (a < b) ? 32'd1 : 32'd0;
            default: result_comb = 32'd0;
        endcase
    end

    always_ff @(posedge clk) begin
        result <= result_comb;
        zero <= (result_comb == 0);
    end
endmodule
```

### SKALP Version
```skalp
entity ALU {
    in clk: clock
    in a: bit[32]
    in b: bit[32]
    in op: bit[3]
    out result: bit[32]
    out zero: bit
}

impl ALU {
    signal result_comb: bit[32]

    result_comb = match op {
        0b000 => a + b,
        0b001 => a - b,
        0b010 => a & b,
        0b011 => a | b,
        0b100 => a ^ b,
        0b101 => a << b[4:0],
        0b110 => a >> b[4:0],
        0b111 => if a < b { 1 } else { 0 },
        _ => 0
    }

    on(clk.rise) {
        result <= result_comb
        zero <= if result_comb == 0 { 1 } else { 0 }
    }
}
```

### Key Differences:
1. **Match expression** - Returns value directly
2. **If expression** - `if a < b { 1 } else { 0 }`
3. **No width suffixes** - `1` instead of `32'd1`
4. **Cleaner syntax** - No `begin/end`, no `case/endcase`

---

## Example 6: FIFO

### SystemVerilog Version (Simplified)
```systemverilog
module FIFO #(
    parameter WIDTH = 8,
    parameter DEPTH = 16,
    localparam ADDR_WIDTH = $clog2(DEPTH)
)(
    input wire clk,
    input wire rst,
    input wire wr_en,
    input wire [WIDTH-1:0] wr_data,
    output wire full,
    input wire rd_en,
    output wire [WIDTH-1:0] rd_data,
    output wire empty
);
    reg [WIDTH-1:0] memory [0:DEPTH-1];
    reg [ADDR_WIDTH-1:0] wr_ptr;
    reg [ADDR_WIDTH-1:0] rd_ptr;
    reg [ADDR_WIDTH:0] count;

    assign empty = (count == 0);
    assign full = (count == DEPTH);
    assign rd_data = memory[rd_ptr];

    always_ff @(posedge clk) begin
        if (rst) begin
            wr_ptr <= '0;
            rd_ptr <= '0;
            count <= '0;
        end else begin
            if (wr_en && !full) begin
                memory[wr_ptr] <= wr_data;
                wr_ptr <= (wr_ptr + 1) % DEPTH;
            end
            if (rd_en && !empty) begin
                rd_ptr <= (rd_ptr + 1) % DEPTH;
            end
            // Count update logic...
        end
    end
endmodule
```

### SKALP Version
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
    signal memory: [bit[WIDTH]; DEPTH]
    signal wr_ptr: nat[clog2(DEPTH)]      // Width auto-computed!
    signal rd_ptr: nat[clog2(DEPTH)]
    signal count: nat[clog2(DEPTH+1)]

    empty = (count == 0)
    full = (count == DEPTH)
    rd_data = memory[rd_ptr]

    on(clk.rise) {
        if (rst) {
            wr_ptr <= 0
            rd_ptr <= 0
            count <= 0
        } else {
            if (wr_en && !full) {
                memory[wr_ptr] <= wr_data
                wr_ptr <= (wr_ptr + 1) % DEPTH
            }
            if (rd_en && !empty) {
                rd_ptr <= (rd_ptr + 1) % DEPTH
            }
            // Count update logic...
        }
    }
}
```

### Key Differences:
1. **Automatic width calculation** - `nat[clog2(DEPTH)]` computed by compiler
2. **No localparam needed** - Width inference does it
3. **Array syntax** - `[bit[WIDTH]; DEPTH]` instead of `[WIDTH-1:0] memory [0:DEPTH-1]`
4. **No manual pointer width** - Type-safe and automatic

---

## Translation Patterns

### Module Declaration
```systemverilog
// SystemVerilog
module MyModule #(
    parameter WIDTH = 8
)(
    input wire clk,
    input wire [WIDTH-1:0] in,
    output wire [WIDTH-1:0] out
);
```
```skalp
// SKALP
entity MyModule<const WIDTH: nat = 8> {
    in clk: clock
    in in: bit[WIDTH]
    out out: bit[WIDTH]
}
```

### Register/Wire Declaration
```systemverilog
// SystemVerilog
reg [7:0] counter;
wire [7:0] sum;
```
```skalp
// SKALP
signal counter: bit[8]
signal sum: bit[8]
```

### Sequential Logic
```systemverilog
// SystemVerilog
always_ff @(posedge clk) begin
    if (rst) begin
        counter <= '0;
    end else begin
        counter <= counter + 1;
    end
end
```
```skalp
// SKALP
on(clk.rise) {
    if (rst) {
        counter <= 0
    } else {
        counter <= counter + 1
    }
}
```

### Combinational Assignment
```systemverilog
// SystemVerilog
assign out = a + b;
```
```skalp
// SKALP
out = a + b
```

### Case Statement
```systemverilog
// SystemVerilog
always_comb begin
    case (op)
        2'b00: out = a;
        2'b01: out = b;
        2'b10: out = c;
        default: out = 0;
    endcase
end
```
```skalp
// SKALP
out = match op {
    0b00 => a,
    0b01 => b,
    0b10 => c,
    _ => 0
}
```

---

## Common Gotchas

### 1. No `[WIDTH-1:0]`
**SystemVerilog:** `wire [WIDTH-1:0] data`
**SKALP:** `signal data: bit[WIDTH]` (no `-1`!)

### 2. No `assign` Keyword
**SystemVerilog:** `assign out = in;`
**SKALP:** `out = in` (just `=`)

### 3. Enum Scoping
**SystemVerilog:** `state = IDLE;`
**SKALP:** `state = State::Idle` (scoped!)

### 4. Match is Expression
**SystemVerilog:** `case` is statement
**SKALP:** `match` returns value

### 5. Width Inference
**SystemVerilog:** Must specify all widths
**SKALP:** Can infer from context

---

## Features SKALP Has That SystemVerilog Doesn't

1. **Automatic width calculation** - `nat[clog2(DEPTH)]`
2. **Expression-based syntax** - `match`, `if` return values
3. **Exhaustiveness checking** - Compiler warns on incomplete match
4. **Struct support** - First-class structured types
5. **Type-safe clock/reset** - `clock` and `reset` types
6. **CDC analysis** - Built-in at compile time
7. **No wire/reg confusion** - Everything is `signal`

---

## Features SystemVerilog Has That SKALP Doesn't (Yet)

1. **Interfaces** - Coming soon
2. **Classes/OOP** - Not planned (hardware-specific)
3. **Assertions (SVA)** - Planned
4. **Coverage** - Planned
5. **DPI** - Not planned initially

---

## Step-by-Step Migration Process

### 1. Start with Module Interface
Translate the module header to `entity`:
```systemverilog
module MyModule #(...)(...)
```
→
```skalp
entity MyModule<...> { ... }
```

### 2. Create `impl` Block
```skalp
impl MyModule {
    // All signals and logic go here
}
```

### 3. Translate Signals
- All `reg` and `wire` become `signal`
- Remove `[WIDTH-1:0]`, use `bit[WIDTH]`

### 4. Translate Sequential Logic
- `always_ff @(posedge clk)` → `on(clk.rise)`
- Keep `<=` for non-blocking

### 5. Translate Combinational Logic
- Remove `assign`
- Consider using `match` instead of `case`

### 6. Test and Iterate
- Compile with `skalp build`
- Check generated SystemVerilog
- Fix any type errors

---

## Next Steps

- **Try it:** Translate one of your own modules
- **Learn more:** [Tutorial](../tutorial/01-first-design.md)
- **Examples:** [FIFO](../examples/intermediate/fifo/), [UART](../examples/intermediate/uart/)
- **Reference:** [Syntax Guide](../reference/syntax.md)

---

**Ready to migrate?** Start with a simple counter, then move to more complex designs.
