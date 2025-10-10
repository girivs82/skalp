# SKALP Syntax Reference

**Quick lookup reference for SKALP syntax. For in-depth explanations, see the [Tutorial](../tutorial/).**

---

## Entity Declaration

Define a hardware module's interface.

### Basic Syntax
```skalp
entity <Name> {
    <port_declarations>
}
```

### Examples

**Simple entity:**
```skalp
entity Counter {
    in clk: clock
    in rst: reset
    out count: bit[8]
}
```

**With parameters (generics):**
```skalp
entity FIFO<const WIDTH: nat = 8, const DEPTH: nat = 16> {
    in clk: clock
    in rst: reset
    in wr_en: bit
    in wr_data: bit[WIDTH]
    out full: bit
}
```

### Port Directions
- `in` - Input port
- `out` - Output port
- `inout` - Bidirectional port (future)

---

## Implementation Block

Define the implementation of an entity.

### Syntax
```skalp
impl <EntityName> {
    <signal_declarations>
    <combinational_assignments>
    <sequential_blocks>
}
```

### Example
```skalp
impl Counter {
    signal count_reg: bit[8] = 0

    count = count_reg

    on(clk.rise) {
        if (rst) {
            count_reg <= 0
        } else {
            count_reg <= count_reg + 1
        }
    }
}
```

---

## Signal Declaration

Declare internal signals (wires and registers).

### Syntax
```skalp
signal <name>: <type> [= <initial_value>]
```

### Examples

**Basic signal:**
```skalp
signal data: bit[8]
```

**With initialization:**
```skalp
signal counter: bit[8] = 0
signal state: State = State::Idle
```

**Multiple signals:**
```skalp
signal a: bit[32]
signal b: bit[32]
signal sum: bit[33]
```

---

## Type Declarations

### Bit Vectors
```skalp
bit          // Single bit
bit[8]       // 8-bit vector
bit[WIDTH]   // Parameterized width
```

### Natural Numbers (Unsigned)
```skalp
nat[8]       // 8-bit unsigned
nat[WIDTH]   // Parameterized width
nat[clog2(DEPTH)]  // Width from expression
```

### Clock and Reset
```skalp
clock                  // Clock signal
reset                  // Reset signal (default active_high)
reset(active_high)     // Explicit polarity
reset(active_low)      // Active low reset
```

### Structs
```skalp
struct PacketHeader {
    src_addr: bit[32]
    dst_addr: bit[32]
    length: bit[16]
    checksum: bit[16]
}

// Usage
signal header: PacketHeader
signal src: bit[32] = header.src_addr
```

### Enums
```skalp
enum State {
    Idle,
    Processing,
    Done
}

// Usage
signal current_state: State = State::Idle
```

### Arrays
```skalp
[bit[8]; 16]         // Array of 16 x 8-bit elements
[bit[WIDTH]; DEPTH]  // Parameterized array
```

---

## Combinational Logic

Continuous assignments (combinational logic).

### Syntax
```skalp
<output> = <expression>
```

### Examples

**Simple assignment:**
```skalp
out = in
```

**Arithmetic:**
```skalp
sum = a + b
difference = a - b
```

**With intermediate signals:**
```skalp
signal temp: bit[9] = a + b
sum = temp[7:0]
carry = temp[8]
```

**Conditional (if-expression):**
```skalp
out = if sel { a } else { b }
```

**Pattern matching:**
```skalp
result = match op {
    0b00 => a + b,
    0b01 => a - b,
    0b10 => a & b,
    0b11 => a | b,
    _ => 0
}
```

---

## Sequential Logic

Clocked logic (registers, state machines).

### Syntax
```skalp
on(<clock>.<edge>) {
    <statements>
}
```

### Clock Edges
- `clk.rise` - Rising edge
- `clk.fall` - Falling edge

### Examples

**Simple register:**
```skalp
on(clk.rise) {
    if (rst) {
        counter <= 0
    } else {
        counter <= counter + 1
    }
}
```

**State machine:**
```skalp
on(clk.rise) {
    if (rst) {
        state <= State::Idle
    } else {
        state <= match state {
            State::Idle => if start { State::Running } else { State::Idle },
            State::Running => if done { State::Done } else { State::Running },
            State::Done => State::Idle
        }
    }
}
```

**Multiple signals:**
```skalp
on(clk.rise) {
    if (rst) {
        reg_a <= 0
        reg_b <= 0
    } else {
        reg_a <= in_a
        reg_b <= in_b
    }
}
```

---

## Operators

### Arithmetic Operators
| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition | `a + b` |
| `-` | Subtraction | `a - b` |
| `*` | Multiplication | `a * b` |
| `/` | Division | `a / b` |
| `%` | Modulo | `a % 16` |

### Bitwise Operators
| Operator | Description | Example |
|----------|-------------|---------|
| `&` | Bitwise AND | `a & b` |
| `\|` | Bitwise OR | `a \| b` |
| `^` | Bitwise XOR | `a ^ b` |
| `~` | Bitwise NOT | `~a` |
| `<<` | Left shift | `a << 2` |
| `>>` | Right shift | `a >> 2` |

### Comparison Operators
| Operator | Description | Example |
|----------|-------------|---------|
| `==` | Equal | `a == b` |
| `!=` | Not equal | `a != b` |
| `<` | Less than | `a < b` |
| `<=` | Less or equal | `a <= b` |
| `>` | Greater than | `a > b` |
| `>=` | Greater or equal | `a >= b` |

### Logical Operators
| Operator | Description | Example |
|----------|-------------|---------|
| `&&` | Logical AND | `enable && valid` |
| `\|\|` | Logical OR | `error \|\| timeout` |
| `!` | Logical NOT | `!ready` |

### Assignment Operators
| Operator | Context | Meaning |
|----------|---------|---------|
| `=` | Outside `on()` | Combinational assignment |
| `<=` | Inside `on()` | Non-blocking assignment (register) |

---

## Bit Slicing and Indexing

### Single Bit Access
```skalp
signal data: bit[8]
signal msb: bit = data[7]      // MSB
signal lsb: bit = data[0]      // LSB
```

### Bit Range (Slice)
```skalp
signal data: bit[16]
signal upper: bit[8] = data[15:8]   // Upper byte
signal lower: bit[8] = data[7:0]    // Lower byte
```

### Array Indexing
```skalp
signal memory: [bit[8]; 16]
signal value: bit[8] = memory[5]    // 6th element
```

**Important:** Slicing is `[high:low]` inclusive on both ends.

---

## Control Flow

### If Statements (Sequential)
```skalp
on(clk.rise) {
    if (condition) {
        // then branch
    } else {
        // else branch
    }
}
```

### If Expressions (Combinational)
```skalp
out = if condition { true_value } else { false_value }
```

### Match Expressions
```skalp
result = match selector {
    pattern1 => value1,
    pattern2 => value2,
    _ => default_value
}
```

**Match patterns:**
- Literal values: `0b00`, `5`, `0xFF`
- Wildcards: `_`
- Enum variants: `State::Idle`

---

## Pattern Matching

### Basic Match
```skalp
out = match opcode {
    0b00 => a,
    0b01 => b,
    0b10 => c,
    0b11 => d,
    _ => 0
}
```

### Match with Expressions
```skalp
result = match op {
    0b000 => a + b,
    0b001 => a - b,
    0b010 => a & b,
    0b011 => a | b,
    _ => 0
}
```

### Match with Nested If
```skalp
next_state = match current_state {
    State::Idle => if start { State::Running } else { State::Idle },
    State::Running => if done { State::Done } else { State::Running },
    State::Done => State::Idle
}
```

---

## Literals

### Binary Literals
```skalp
0b0
0b1
0b1010
0b11111111
```

### Hexadecimal Literals
```skalp
0x0
0xA
0xFF
0xDEADBEEF
```

### Decimal Literals
```skalp
0
1
42
1000
```

### Underscore Separators (for readability)
```skalp
0b1111_1111      // 255
0x00FF_00FF      // 16711935
1_000_000        // 1 million
```

---

## Comments

### Single-line Comments
```skalp
// This is a single-line comment
signal data: bit[8]  // Comment at end of line
```

### Multi-line Comments
```skalp
/*
 * This is a multi-line comment
 * It can span multiple lines
 */
signal data: bit[8]
```

---

## Built-in Functions

### Width Calculation
```skalp
clog2(n)         // Ceiling log base 2
```

**Example:**
```skalp
signal ptr: nat[clog2(16)]    // 4 bits (clog2(16) = 4)
signal count: nat[clog2(17)]  // 5 bits (clog2(17) = 5)
```

### Future Built-ins (Planned)
```skalp
width(signal)    // Get width of signal
signed(value)    // Convert to signed
unsigned(value)  // Convert to unsigned
```

---

## Complete Examples

### Counter with Load
```skalp
entity Counter<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset
    in load: bit
    in load_value: bit[WIDTH]
    in enable: bit
    out count: bit[WIDTH]
}

impl Counter {
    signal count_reg: bit[WIDTH] = 0

    count = count_reg

    on(clk.rise) {
        if (rst) {
            count_reg <= 0
        } else if (load) {
            count_reg <= load_value
        } else if (enable) {
            count_reg <= count_reg + 1
        }
    }
}
```

### ALU (Arithmetic Logic Unit)
```skalp
entity ALU {
    in a: bit[32]
    in b: bit[32]
    in op: bit[3]
    out result: bit[32]
}

impl ALU {
    result = match op {
        0b000 => a + b,           // ADD
        0b001 => a - b,           // SUB
        0b010 => a & b,           // AND
        0b011 => a | b,           // OR
        0b100 => a ^ b,           // XOR
        0b101 => a << b[4:0],     // SHL
        0b110 => a >> b[4:0],     // SHR
        0b111 => if a < b { 1 } else { 0 },  // SLT
        _ => 0
    }
}
```

### Simple FIFO
```skalp
entity FIFO<const WIDTH: nat = 8, const DEPTH: nat = 4> {
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
    signal wr_ptr: nat[clog2(DEPTH)] = 0
    signal rd_ptr: nat[clog2(DEPTH)] = 0
    signal count: nat[clog2(DEPTH+1)] = 0

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

            if (wr_en && !rd_en && !full) {
                count <= count + 1
            } else if (!wr_en && rd_en && !empty) {
                count <= count - 1
            }
        }
    }
}
```

---

## Syntax Summary Table

| Construct | Syntax | Example |
|-----------|--------|---------|
| **Entity** | `entity Name { ports }` | `entity Counter { in clk: clock }` |
| **Impl** | `impl Name { body }` | `impl Counter { signal c: bit[8] }` |
| **Generic** | `entity Name<const P: nat>` | `entity FIFO<const WIDTH: nat>` |
| **Signal** | `signal name: type` | `signal data: bit[8]` |
| **Combinational** | `out = expr` | `sum = a + b` |
| **Sequential** | `on(clk.rise) { stmts }` | `on(clk.rise) { r <= r + 1 }` |
| **If (stmt)** | `if (cond) { } else { }` | `if (rst) { r <= 0 }` |
| **If (expr)** | `if cond { a } else { b }` | `out = if sel { 1 } else { 0 }` |
| **Match** | `match x { p => v }` | `match op { 0 => a, _ => b }` |
| **Struct** | `struct Name { fields }` | `struct Pkt { addr: bit[32] }` |
| **Enum** | `enum Name { variants }` | `enum State { Idle, Run }` |
| **Array** | `[type; size]` | `[bit[8]; 16]` |
| **Slice** | `signal[high:low]` | `data[7:0]` |
| **Index** | `array[index]` | `memory[5]` |

---

## Common Patterns

### Mux (2-to-1)
```skalp
out = if sel { in1 } else { in0 }
```

### Mux (4-to-1)
```skalp
out = match sel {
    0b00 => in0,
    0b01 => in1,
    0b10 => in2,
    0b11 => in3,
    _ => 0
}
```

### Register with Enable
```skalp
on(clk.rise) {
    if (rst) {
        reg <= 0
    } else if (enable) {
        reg <= data_in
    }
}
```

### Shift Register
```skalp
on(clk.rise) {
    if (rst) {
        shift_reg <= 0
    } else {
        shift_reg <= (shift_reg << 1) | data_in
    }
}
```

---

## See Also

- [Type System Reference](types.md) - Detailed type information
- [Operators Reference](operators.md) - Operator precedence and details
- [Built-in Functions](builtins.md) - All built-in functions
- [Tutorial](../tutorial/01-first-design.md) - Learn by example
- [Examples](../examples/) - Complete working designs

---

**Quick Tips:**
- Use `=` for combinational, `<=` for sequential
- `match` is an expression (returns value)
- Slicing is `[high:low]` inclusive
- No semicolons needed at end of statements
- Clock edges: `clk.rise` or `clk.fall`
