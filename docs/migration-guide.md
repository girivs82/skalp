# Migration Guide: From Verilog/VHDL to SKALP

This guide helps experienced Verilog and VHDL designers transition to SKALP, highlighting key differences and providing side-by-side comparisons.

## Table of Contents
1. [Core Concepts](#core-concepts)
2. [Module/Entity Declaration](#moduleentity-declaration)
3. [Sequential Logic](#sequential-logic)
4. [Combinational Logic](#combinational-logic)
5. [Clock Domain Crossing](#clock-domain-crossing)
6. [Verification](#verification)
7. [Advanced Features](#advanced-features)

## Core Concepts

### Key Differences

| Feature | Verilog | VHDL | SKALP |
|---------|---------|------|-------|
| Module Definition | `module` | `entity`/`architecture` | `entity`/`impl` |
| Clock Events | `@(posedge clk)` | `rising_edge(clk)` | `on(clk.rise)` |
| Non-blocking Assignment | `<=` | Signal assignment | `<=` (sequential only) |
| Blocking Assignment | `=` | Variable assignment | `:=` (variables), `=` (combinational) |
| Type System | Weak | Strong | Strong with inference |
| Clock Domain Safety | Manual | Manual | **Automatic** |
| Generics/Parameters | `parameter` | `generic` | `const` generics |

## Module/Entity Declaration

### Verilog
```verilog
module counter #(
    parameter WIDTH = 8
) (
    input wire clk,
    input wire rst,
    input wire enable,
    output reg [WIDTH-1:0] count
);
    // Implementation
endmodule
```

### VHDL
```vhdl
entity counter is
    generic (
        WIDTH : integer := 8
    );
    port (
        clk    : in  std_logic;
        rst    : in  std_logic;
        enable : in  std_logic;
        count  : out std_logic_vector(WIDTH-1 downto 0)
    );
end counter;

architecture rtl of counter is
begin
    -- Implementation
end rtl;
```

### SKALP
```skalp
entity Counter<const WIDTH: nat = 8> {
    in clk: clock;
    in rst: reset(active_high);
    in enable: bit;
    out count: bit<WIDTH>;
}

impl Counter<const WIDTH: nat> {
    // Implementation
}
```

**Key Improvements in SKALP:**
- Cleaner syntax with type inference
- Built-in `clock` and `reset` types with polarity
- Generic parameters are strongly typed
- Separation of interface (`entity`) and implementation (`impl`)

## Sequential Logic

### Verilog
```verilog
always @(posedge clk or posedge rst) begin
    if (rst) begin
        count <= 8'b0;
        state <= IDLE;
    end else begin
        if (enable) begin
            count <= count + 1;
            case (state)
                IDLE: state <= ACTIVE;
                ACTIVE: state <= DONE;
                default: state <= IDLE;
            endcase
        end
    end
end
```

### VHDL
```vhdl
process(clk, rst)
begin
    if rst = '1' then
        count <= (others => '0');
        state <= IDLE;
    elsif rising_edge(clk) then
        if enable = '1' then
            count <= count + 1;
            case state is
                when IDLE => state <= ACTIVE;
                when ACTIVE => state <= DONE;
                when others => state <= IDLE;
            end case;
        end if;
    end if;
end process;
```

### SKALP
```skalp
on(rst.active) {
    count <= 0;
    state <= State::IDLE;
}

on(clk.rise) {
    if enable {
        count <= count + 1;
        match state {
            State::IDLE => state <= State::ACTIVE;
            State::ACTIVE => state <= State::DONE;
            _ => state <= State::IDLE;
        }
    }
}
```

**Key Improvements in SKALP:**
- Clearer event-driven syntax with `on()`
- Automatic reset handling - no need for nested if-else
- Pattern matching with `match` instead of case statements
- Type-safe enum values

## Combinational Logic

### Verilog
```verilog
always @(*) begin
    case (opcode)
        2'b00: result = a + b;
        2'b01: result = a - b;
        2'b10: result = a & b;
        2'b11: result = a | b;
    endcase
end

assign carry_out = (a[7] & b[7]) | (result[7] & (a[7] | b[7]));
```

### VHDL
```vhdl
process(opcode, a, b)
begin
    case opcode is
        when "00" => result <= a + b;
        when "01" => result <= a - b;
        when "10" => result <= a and b;
        when "11" => result <= a or b;
        when others => result <= (others => '0');
    end case;
end process;

carry_out <= (a(7) and b(7)) or (result(7) and (a(7) or b(7)));
```

### SKALP
```skalp
// Combinational assignments use '='
result = match opcode {
    0b00 => a + b;
    0b01 => a - b;
    0b10 => a & b;
    0b11 => a | b;
};

carry_out = (a[7] & b[7]) | (result[7] & (a[7] | b[7]));
```

**Key Improvements in SKALP:**
- Expression-based `match` for cleaner combinational logic
- Clear distinction: `=` for combinational, `<=` for sequential
- No sensitivity list needed - automatically inferred

## Clock Domain Crossing

### Verilog (Error-Prone)
```verilog
// CDC Bug - No synchronization!
always @(posedge clk1) begin
    data_clk1 <= input_data;
end

always @(posedge clk2) begin
    data_clk2 <= data_clk1;  // Unsafe CDC!
end
```

### VHDL (Manual Synchronization)
```vhdl
-- Manual 2-FF synchronizer
signal sync_ff1, sync_ff2 : std_logic_vector(7 downto 0);

process(clk2)
begin
    if rising_edge(clk2) then
        sync_ff1 <= data_clk1;
        sync_ff2 <= sync_ff1;
        data_clk2 <= sync_ff2;
    end if;
end process;
```

### SKALP (Compile-Time Safety)
```skalp
entity CDCSafe<'clk1, 'clk2> {
    in clk1: clock<'clk1>;
    in clk2: clock<'clk2>;
    in data_in<'clk1>: bit<8>;
    out data_out<'clk2>: bit<8>;
}

impl CDCSafe<'clk1, 'clk2> {
    // Compiler ERROR if you try direct assignment:
    // data_out <= data_in;  // Error: Clock domain mismatch!

    // Must use explicit synchronizer:
    sync #(.STAGES(2)) cdc_sync {
        .in_clk(clk1),
        .out_clk(clk2),
        .in(data_in),
        .out(data_out)
    };
}
```

**Key Improvements in SKALP:**
- **Compile-time CDC detection** - impossible to create CDC bugs
- Clock domains as generic lifetime parameters
- Built-in synchronizer primitives
- Automatic metastability analysis

## Verification

### SystemVerilog
```systemverilog
property req_ack;
    @(posedge clk) req |-> ##[1:3] ack;
endproperty

assert property(req_ack);
cover property(req_ack);

initial begin
    #10 rst = 1;
    #10 rst = 0;
    #10 data = 8'hAA;
    #10 assert(result == 8'h55);
end
```

### VHDL (with OSVVM/UVVM)
```vhdl
-- Complex setup required for assertions
assert (req = '1')
    report "Request not received"
    severity ERROR;

-- Testbench
process
begin
    wait for 10 ns;
    rst <= '1';
    wait for 10 ns;
    rst <= '0';
    wait for 10 ns;
    data <= x"AA";
    wait for 10 ns;
    assert result = x"55";
end process;
```

### SKALP
```skalp
// Built-in assertions
assert property (req |-> ##[1:3] ack);
cover property (state == State::ACTIVE);

// Async testbench
testbench MyTest {
    async test "basic_test" {
        await delay(10);
        rst = 1;
        await clk.rise;
        rst = 0;
        await clk.rise;
        data = 0xAA;
        await clk.rise;
        assert result == 0x55;
    }
}
```

**Key Improvements in SKALP:**
- Built-in assertion and coverage support
- Modern async/await testbench syntax
- Integrated verification - no separate language needed

## Advanced Features

### 1. Generics and Traits

**Verilog:** Limited parameter system
```verilog
module fifo #(parameter WIDTH=8, DEPTH=16) (...);
```

**SKALP:** Full generic system with traits
```skalp
trait Arithmetic<T> {
    fn add(a: T, b: T) -> T;
    fn sub(a: T, b: T) -> T;
}

entity ALU<T: Arithmetic> {
    in a: T;
    in b: T;
    out result: T;
}
```

### 2. Pattern Matching

**Verilog:** Basic case statements
```verilog
case(state)
    IDLE: next = START;
    START: next = RUN;
endcase
```

**SKALP:** Full pattern matching
```skalp
next_state = match (state, input) {
    (State::IDLE, Input::Go) => State::START;
    (State::START, _) => State::RUN;
    (_, Input::Reset) => State::IDLE;
    _ => state;  // Stay in current state
};
```

### 3. Flow Blocks (Pipeline)

**Verilog:** Manual pipeline stages
```verilog
always @(posedge clk) begin
    stage1 <= input_data;
    stage2 <= stage1 + 1;
    stage3 <= stage2 * 2;
    output_data <= stage3;
end
```

**SKALP:** Automatic pipelining
```skalp
flow process_pipeline {
    input_data
    |> (x => x + 1)      // Stage 1
    |> (x => x * 2)      // Stage 2
    |> (x => x & 0xFF)   // Stage 3
    |> output_data;
}
```

### 4. Safety Features

**Verilog/VHDL:** No built-in safety
```verilog
// Manual safety implementation required
```

**SKALP:** ISO 26262 support
```skalp
@safety(asil = "ASIL-D")
@requirement("REQ-SAFE-001: Dual redundancy")
entity SafetyCore {
    // Automatic FMEA generation
    // Built-in safety mechanism support
}
```

## Common Migration Patterns

### 1. FSM Migration

**Verilog FSM:**
```verilog
reg [1:0] state, next_state;
parameter IDLE = 2'b00, RUN = 2'b01, DONE = 2'b10;

always @(posedge clk or posedge rst) begin
    if (rst)
        state <= IDLE;
    else
        state <= next_state;
end

always @(*) begin
    case(state)
        IDLE: next_state = start ? RUN : IDLE;
        RUN:  next_state = complete ? DONE : RUN;
        DONE: next_state = IDLE;
        default: next_state = IDLE;
    endcase
end
```

**SKALP FSM:**
```skalp
enum State { IDLE, RUN, DONE }

entity FSM {
    in clk: clock;
    in rst: reset(active_high);
    in start: bit;
    in complete: bit;
    out state: State;
}

impl FSM {
    on(rst.active) {
        state <= State::IDLE;
    }

    on(clk.rise) {
        state <= match (state, start, complete) {
            (State::IDLE, true, _) => State::RUN;
            (State::RUN, _, true) => State::DONE;
            (State::DONE, _, _) => State::IDLE;
            _ => state;
        };
    }
}
```

### 2. Memory/RAM Migration

**Verilog RAM:**
```verilog
reg [7:0] ram [0:255];

always @(posedge clk) begin
    if (we)
        ram[addr] <= wdata;
    rdata <= ram[addr];
end
```

**SKALP RAM:**
```skalp
signal ram: array<bit<8>, 256>;

on(clk.rise) {
    if we {
        ram[addr] <= wdata;
    }
    rdata <= ram[addr];
}
```

## Migration Checklist

When migrating from Verilog/VHDL to SKALP:

- [ ] **Replace module/entity declarations** with SKALP `entity`/`impl`
- [ ] **Convert always blocks** to `on()` event blocks
- [ ] **Update type declarations** to SKALP's type system
- [ ] **Add clock domain annotations** where needed
- [ ] **Replace case statements** with `match` expressions
- [ ] **Convert assertions** to SKALP's built-in syntax
- [ ] **Update testbenches** to use async/await
- [ ] **Add safety annotations** for critical modules
- [ ] **Leverage generics** for reusable components
- [ ] **Use traits** for polymorphic designs

## Tool Mapping

| Verilog/VHDL Tool | SKALP Equivalent |
|-------------------|------------------|
| ModelSim/QuestaSim | `skalp sim` |
| Vivado/Quartus | `skalp synth --target fpga` |
| Design Compiler | `skalp synth --target asic` |
| Verilator | Built-in simulation |
| VCS | GPU-accelerated simulation |
| Formal tools | `skalp verify` |

## Getting Help

- **Documentation:** [docs.skalp-lang.org](https://docs.skalp-lang.org)
- **Examples:** See `examples/` directory
- **Community:** Discord and forums
- **Migration tool:** `skalp convert --from verilog input.v`

## Summary

SKALP provides a modern, safer alternative to Verilog and VHDL with:
- **Compile-time safety** for clock domains
- **Modern language features** (pattern matching, traits, async/await)
- **Built-in verification** capabilities
- **GPU-accelerated simulation**
- **ISO 26262 compliance** support

The transition from Verilog/VHDL to SKALP eliminates entire classes of bugs while providing more expressive and maintainable hardware descriptions.