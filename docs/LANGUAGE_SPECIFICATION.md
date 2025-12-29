# SKALP Language Specification
*Sankalpana (संकल्पना) - Intent-Driven Hardware Synthesis*

## Table of Contents

1. [Introduction](#introduction)
2. [Lexical Structure](#lexical-structure)
3. [Type System](#type-system)
4. [Clock Domains](#clock-domains)
5. [Entities and Implementation](#entities-and-implementation)
6. [Signals and Variables](#signals-and-variables)
7. [Protocols](#protocols)
8. [Intent Specifications](#intent-specifications)
9. [Flow Blocks and Dataflow](#flow-blocks-and-dataflow)
10. [Sequential Logic and Clock Events](#sequential-logic-and-clock-events)
11. [Pattern Matching](#pattern-matching)
12. [Traits and Generics](#traits-and-generics)
13. [Verification](#verification)
14. [Standard Library](#standard-library)
15. [Examples](#examples)
16. [Attributes](#attributes)

## 1. Introduction

SKALP (derived from Sanskrit 'Sankalpana' meaning 'conception with purpose') is a strongly-typed hardware description language that combines the correctness of VHDL with modern language features and a focus on design intent. This specification defines the SKALP language version 1.0.

### 1.1 Design Goals

- **Safety**: Eliminate entire classes of hardware bugs at compile time
- **Expressiveness**: From high-level algorithms to cycle-accurate descriptions
- **Intent Preservation**: Design decisions are explicit and maintained
- **Composability**: Build complex systems from verified components

### 1.2 Design Philosophy: Learning from History

SKALP combines the best practices from VHDL and modern HDLs while avoiding common pitfalls:

**From VHDL (Strengths We Keep):**
- Strong typing with no implicit conversions
- Clear signal vs variable semantics (signals with `<=`, variables with `:=`)
- Clean package/library system
- No ambiguous edge specifications

**Avoiding SystemVerilog's Inconsistencies:**
- ONE way to declare hardware: just `process`, not `always`/`always_ff`/`always_comb`
- Clear assignment operators: `=` for continuous, `<=` for signals, `:=` for variables
- No `reg`/`wire`/`logic` confusion - just `signal` and `var`
- No blocking/non-blocking confusion in wrong contexts
- No weak typing with dangerous implicit conversions

**Improvements Over Both:**
- Clock domains as first-class types (compile-time CDC checking)
- Protocols instead of interfaces/modports
- Intent declarations guide synthesis
- Progressive refinement from algorithm to gates

### 1.3 Notation

This specification uses the following notation:
- `monospace` for code and keywords
- *italic* for metavariables
- [brackets] for optional elements
- {braces} for zero or more repetitions
- (parens|pipes) for alternatives

## 2. Lexical Structure

### 2.1 Keywords

SKALP uses a minimal set of 43 keywords, each with a specific purpose:

```
// Core Hardware Description (13)
entity impl signal var const
in out inout on if else
async barrier

// Type System (9)
bit bool clock reset type stream
struct enum ncl

// Boolean Literals (2)
true false

// Traits and Generics (5)
trait protocol where self Self

// Event Control (2)
rise fall

// Control Flow (2)
match for

// Design Intent (3)
intent flow requirement

// Testbench Only (4)
await fn return let

// Type Conversion (1)
as

// Module System (4)
use mod pub with

// Verification (1)
assert
```

#### Design Rationale for Keyword Choices:

**Excluded Keywords and Why:**
- `logic` - SKALP uses 2-state (`bit`) only for performance. X/Z states are modeled as randomized 0/1
- `edge` - Redundant; use `rise | fall` for both edges
- `elif` - Use `else if` instead
- `loop`, `while`, `break`, `continue` - Not synthesizable; use bounded `for` loops
- `mut`, `ref`, `static` - Memory management concepts not applicable to hardware
- `package`, `import`, `export`, `priv` - Use simpler `mod`/`use` system
- `always`, `eventually` - Temporal operators belong in assertions, not as keywords
- `assume`, `cover`, `prove` - Start with just `assert`; add others if needed

**Key Distinctions:**
- `signal` vs `var`: Signals are hardware wires/registers, vars are procedural variables
- `let` vs `var` (testbench): `let` for immutable bindings, `var` for mutable
- `assert` vs `requirement`: Assertions are runtime checks, requirements are formal contracts
- `async entity` vs `entity`: Async entities are clockless NCL circuits
- `await`: Only for testbenches, never in synthesizable hardware

**NCL (Null Convention Logic) Keywords:**
- `async`: Modifier for clockless asynchronous entity declarations
- `barrier`: Pipeline stage boundary with completion detection
- `ncl<N>`: Explicit dual-rail type for N logical bits (2N physical wires)

### 2.2 Identifiers

Identifiers start with a letter or underscore, followed by letters, digits, or underscores:

```
identifier = [a-zA-Z_][a-zA-Z0-9_]*
```

### 2.3 Comments

```rust
// Line comment
/* Block comment */
/// Documentation comment
```

### 2.4 Literals

```rust
// Boolean literals
true        // Boolean true value
false       // Boolean false value

// Numeric literals
42          // Decimal
0x2A        // Hexadecimal
0o52        // Octal
0b101010    // Binary
4'b1010     // Sized binary
16'hDEAD    // Sized hexadecimal

// String literals
"hello"     // String
'a'         // Character

// Time literals
10ns        // Time unit
100MHz      // Frequency
```

## 3. Type System

### 3.1 Primitive Types

```rust
// Boolean type (for control flow and logic)
bool        // Boolean value (true or false)
// Note: Distinct from bit type. Used for conditional expressions,
//       control flow, and logical operations. Cannot be used with
//       bitwise operations (use bit type for that).

// Bit types (2-state only for performance)
bit         // Single bit (0 or 1)
bit[N]      // N-bit vector
// Note: No 4-state logic. X is modeled as randomized 0/1,
//       Z (tristate) is modeled as what receivers would see

// Integer types (legacy syntax - see 3.1.1 for parametric types)
int[N]      // N-bit signed integer
nat[N]      // N-bit unsigned integer

// Special hardware types
clock       // Clock signal (enables edge detection)
reset       // Reset signal (active high/low)

// Stream type for flow control
stream<T>   // Stream with implicit handshaking

// Type Conversions
// bool -> bit: Requires explicit cast (true as bit -> 1'b1, false as bit -> 1'b0)
// bit -> bool: Requires explicit cast (1'b0 -> false, 1'b1 -> true)
```

### 3.1.1 Parametric Numeric Types

SKALP provides a unified, parametric numeric type system that eliminates code duplication and enables compile-time specialization.

#### Floating-Point Types

```rust
// Parametric floating-point type
type fp<const F: FloatFormat> = bit[F.total_bits]

// Float format descriptor
struct FloatFormat {
    total_bits: nat,
    exponent_bits: nat,
    mantissa_bits: nat,
    bias: int,
    name: &str,
}

// Standard IEEE 754 formats
const IEEE754_16: FloatFormat = FloatFormat {
    total_bits: 16, exponent_bits: 5, mantissa_bits: 10,
    bias: 15, name: "IEEE754-half"
}

const IEEE754_32: FloatFormat = FloatFormat {
    total_bits: 32, exponent_bits: 8, mantissa_bits: 23,
    bias: 127, name: "IEEE754-single"
}

const IEEE754_64: FloatFormat = FloatFormat {
    total_bits: 64, exponent_bits: 11, mantissa_bits: 52,
    bias: 1023, name: "IEEE754-double"
}

// Alternative formats
const BFLOAT16: FloatFormat = FloatFormat {
    total_bits: 16, exponent_bits: 8, mantissa_bits: 7,
    bias: 127, name: "BFloat16"
}

const TFLOAT32: FloatFormat = FloatFormat {
    total_bits: 19, exponent_bits: 8, mantissa_bits: 10,
    bias: 127, name: "TensorFloat32"
}

// Convenient aliases
type fp16 = fp<IEEE754_16>
type fp32 = fp<IEEE754_32>
type fp64 = fp<IEEE754_64>
type bf16 = fp<BFLOAT16>
type tf32 = fp<TFLOAT32>

// Component access
impl<const F: FloatFormat> fp<F> {
    fn sign(self) -> bit {
        self[F.total_bits - 1]
    }

    fn exponent(self) -> bit[F.exponent_bits] {
        self[F.mantissa_bits .. F.mantissa_bits + F.exponent_bits]
    }

    fn mantissa(self) -> bit[F.mantissa_bits] {
        self[0 .. F.mantissa_bits]
    }

    fn is_nan(self) -> bit { /* ... */ }
    fn is_inf(self) -> bit { /* ... */ }
    fn is_zero(self) -> bit { /* ... */ }
}
```

#### Fixed-Point Types

```rust
// Parametric fixed-point type
type fixed<
    const WIDTH: nat,
    const FRAC: nat,
    const SIGNED: bool = true
> = bit[WIDTH]
where
    FRAC <= WIDTH

// Common Q-notation formats
type q16_16 = fixed<32, 16, true>   // 16.16 signed fixed
type q8_8   = fixed<16, 8, true>    // 8.8 signed fixed
type uq16_16 = fixed<32, 16, false> // Unsigned 16.16

// Custom formats
type q12_20 = fixed<32, 20, true>   // 12 integer, 20 fractional bits
```

#### Integer Types (as Degenerate Fixed-Point)

```rust
// Integer as fixed-point with FRAC=0
type int<const WIDTH: nat, const SIGNED: bool = true> =
    fixed<WIDTH, 0, SIGNED>

// Standard aliases
type i8   = int<8, true>
type i16  = int<16, true>
type i32  = int<32, true>
type i64  = int<64, true>
type u8   = int<8, false>
type u16  = int<16, false>
type u32  = int<32, false>
type u64  = int<64, false>
```

#### Unified Numeric Trait

All numeric types implement the `Numeric` trait:

```rust
trait Numeric {
    const TOTAL_BITS: nat;
    const IS_SIGNED: bool;
    const IS_FLOATING: bool;
    const IS_FIXED: bool;

    fn add(self, other: Self) -> Self;
    fn sub(self, other: Self) -> Self;
    fn mul(self, other: Self) -> Self;
    fn div(self, other: Self) -> Self;
    fn lt(self, other: Self) -> bit;
    fn eq(self, other: Self) -> bit;
}

// Example: Generic operation works for ANY numeric type
entity Add<T, intent I: Intent = Intent::default()>
where
    T: Numeric
{
    in a: T
    in b: T
    out result: T
}
```

#### Parametric Vector Types

```rust
// Generic N-dimensional vector
type vec<T, const N: nat> = [T; N]
where
    T: Synthesizable

// Common aliases
type vec2<T> = vec<T, 2>
type vec3<T> = vec<T, 3>
type vec4<T> = vec<T, 4>

// Named component access for small vectors
impl<T> vec<T, 2> {
    fn x(self) -> T { self[0] }
    fn y(self) -> T { self[1] }
}

impl<T> vec<T, 3> {
    fn x(self) -> T { self[0] }
    fn y(self) -> T { self[1] }
    fn z(self) -> T { self[2] }
}

impl<T> vec<T, 4> {
    fn x(self) -> T { self[0] }
    fn y(self) -> T { self[1] }
    fn z(self) -> T { self[2] }
    fn w(self) -> T { self[3] }
}

// Generic vector operations work for ANY element type and dimension
entity VecAdd<T, const N: nat, intent I: Intent = Intent::default()>
where
    T: Numeric
{
    in a: vec<T, N>
    in b: vec<T, N>
    out result: vec<T, N>
}
```

#### Custom Formats

Users can define custom numeric formats:

```rust
// Custom 24-bit floating-point
const CUSTOM_FP24: FloatFormat = FloatFormat {
    total_bits: 24,
    exponent_bits: 7,
    mantissa_bits: 16,
    bias: 63,
    name: "Custom24"
}

type fp24 = fp<CUSTOM_FP24>

// All stdlib operations automatically work!
signal x: fp24 = 3.14
signal y: fp24 = sqrt(x)         // Uses generic sqrt
signal v: vec3<fp24> = {1.0, 2.0, 3.0}
signal n: vec3<fp24> = normalize(v)  // Uses generic normalize
```

### 3.2 Composite Types

#### Structures (Data Grouping)
Structures group related data. When used in entity ports, ALL fields inherit the port's direction.

```rust
struct Instruction {
    opcode: bit[6],
    rs: bit[5],
    rt: bit[5],
    rd: bit[5],
    shamt: bit[5],
    funct: bit[6]
}

entity Decoder {
    in insn: Instruction,    // ALL fields are inputs
    out alu_op: bit[4]
}
```

#### Enumerations (Named Values)
Enumerations provide named constants for state machines and opcodes.

```rust
enum State {
    IDLE = 0b00,
    READ = 0b01,
    WRITE = 0b10,
    ERROR = 0b11
}

entity FSM {
    in clk: clock
    out busy: bit
}

impl FSM {
    signal state: State = State::IDLE

    on(clk.rise) {
        match state {
            State::IDLE => { /* ... */ }
            State::READ => { /* ... */ }
        }
    }

    busy = (state != State::IDLE)
}
```

#### Type Aliases
Type aliases improve readability without creating new types.

```rust
type Word = bit[32];
type Address = bit[32];
type CacheLine = struct {
    tag: bit[20],
    data: bit[512],
    valid: bit,
    dirty: bit
}
```

### 3.3 Protocols (Bidirectional Interfaces)

Protocols define interfaces with mixed signal directions. Unlike structs, protocols specify the direction for each signal.

```rust
protocol AXIStream {
    out data: bit[32],
    out valid: bit,
    in ready: bit,
    out last: bit
}
```

#### Protocol Flipping

The `~` operator flips ALL signal directions in a protocol. This allows defining a protocol from one perspective and using it from both sides.

```rust
entity Producer {
    port axi: AXIStream      // Uses protocol as defined
}

entity Consumer {
    port axi: ~AXIStream     // Flips all directions
    // data, valid, last become inputs
    // ready becomes output
}
```

**Key Design Principle:** Protocols are defined from ONE perspective (chosen by the protocol author). Users apply `~` when they need the opposite perspective. There is no "standard" perspective - the protocol author decides what's most natural for their documentation.

// Algebraic Data Types
enum Instruction {
    Load { addr: bit[32], reg: bit[5] },
    Store { addr: bit[32], reg: bit[5], data: bit[32] },
    Branch { target: bit[32], condition: Condition },
    Alu { op: Operation, rd: bit[5], rs1: bit[5], rs2: bit[5] }
}
```

### 3.3 Type Aliases

```rust
type Word = bit[32]
type Address = nat[0..0xFFFF]
type Coefficient = fixed[1.15]
```

### 3.4 Option and Result Types

```rust
// Option type for optional values
enum Option<T> {
    Some(T),
    None
}

// Result type for error handling
enum Result<T, E> {
    Ok(T),
    Err(E)
}
```

## 4. Clock Domains and Timing Specifications

### 4.1 Clock Domain Lifetimes

Clock domains are expressed using lifetime annotations:

```rust
// Signal in clock domain 'a
signal data: logic<'a>[32]

// Signal in different domain requires synchronization
signal sync_data: logic<'b>[32]

// Function generic over clock domain
fn process<'clk>(input: logic<'clk>[32]) -> logic<'clk>[32] {
    input + 1  // Same domain in and out
}
```

### 4.2 Clock Domain Crossing

```rust
entity CDC {
    in data: logic<'src>[32]
    out sync: logic<'dst>[32]
}

impl CDC {
    // Automatic CDC insertion
    sync = synchronize(data)  // Compiler inserts appropriate CDC

    // Or explicit multi-stage synchronizer
    signal stage1: logic<'dst>[32]
    signal stage2: logic<'dst>[32]

    on(clock<'dst>.rise) {
        stage1 <= data
        stage2 <= stage1
        sync <= stage2
    }
}
```

### 4.3 Clock Specifications

```rust
// Clock with frequency and constraints
clock sys_clk: clock<'sys, 100MHz> {
    uncertainty: 0.1ns,
    duty_cycle: 50% ± 5%,
    jitter: < 50ps
}

// Clock with period
clock slow_clk: clock<'slow, period=100ns>

// Derived clocks with relationships
clock div_clk: clock<'div> = sys_clk / 4

// DDR clock with phase shift
clock ddr_clk: clock<'ddr, 800MHz> {
    phase_shift: 90deg,
    relationship_to: sys_clk * 8
}
```

### 4.4 Clock Groups and Relationships

```rust
// Synchronous clock group
clock_group system_clocks {
    clk_100: clock<100MHz>,
    clk_200: clock<200MHz> = clk_100 * 2,
    clk_50: clock<50MHz> = clk_100 / 2
} with {
    synchronous: true,
    common_source: pll_0
}

// Asynchronous clock group
clock_group async_clocks {
    usb_clk: clock<48MHz>,
    eth_clk: clock<125MHz>
} with {
    asynchronous_to: system_clocks,
    no_timing_relationship: true
}
```

### 4.5 Integrated Timing Constraints

Unlike traditional HDLs where timing constraints are in separate files, SKALP integrates timing specifications directly into the design:

```rust
// Input timing constraints with the port
entity Design {
    in data: bit[32] @ clk {
        setup_time: 2ns,
        hold_time: 0.5ns,
        input_delay: 1ns  // Board trace delay
    }

    // Output timing constraints
    out result: bit[32] @ clk {
        clock_to_out: max 3ns,
        output_delay: 1ns,
        slew_rate: fast,
        drive_strength: 12mA
    }

    // Bidirectional I/O timing
    inout dq: bit[32] @ ddr_clk {
        input: { setup: 0.3ns, hold: 0.3ns },
        output: { min: -0.5ns, max: 0.5ns }
    }
}
```

### 4.6 Path Specifications

```rust
// False paths for CDC
path(async_signal -> synced_signal) {
    false_path: true,
    reason: "CDC through synchronizer"
}

// Multi-cycle paths
path(slow_ctrl -> status_reg) {
    multicycle: 3,
    reason: "Control path has 3 cycles to settle"
}

// Maximum delay constraints
path(critical_input -> critical_output) {
    max_delay: 5ns,
    reason: "Critical path for system performance"
}

// Point-to-point constraints
path(from: reg_a, to: reg_b) {
    min_delay: 1ns,
    max_delay: 3ns
}
```

### 4.7 Timing Assertions

```rust
entity Pipeline {
    timing {
        // Latency specifications
        latency(input -> output) == 3  // Exactly 3 cycles

        // Throughput requirements
        throughput >= 1_per_cycle

        // Critical path constraints
        critical_path < clock_period * 0.8  // 20% slack
    }

    // Timing assertions that are checked
    assert timing {
        always (request implies eventually(response, within: 10))
    }
}
```

### 4.8 Adaptive and Conditional Timing

```rust
entity AdaptiveDesign {
    in power_mode: enum { LOW, NORMAL, TURBO }

    // Adaptive clock based on mode
    clock sys_clk: adaptive_clock {
        when power_mode == LOW: 50MHz,
        when power_mode == NORMAL: 100MHz,
        when power_mode == TURBO: 200MHz
    }

    // Conditional timing constraints
    timing when power_mode == TURBO {
        critical_path < 4ns
    }
}
```

### 4.9 Physical-Aware Timing

```rust
entity ChipTop {
    // Placement regions affect timing
    region io_region: { x: 0..10, y: 0..100 }
    region core_region: { x: 10..90, y: 0..100 }

    // Module placement with timing impact
    module io_block @ io_region {
        out data @ clk {
            // Estimated routing delay to core
            routing_delay_to(core_region): 2ns
        }
    }

    module core @ core_region {
        // Timing budget allocation
        timing_budget: 60%  // 60% of clock period
    }
}
```

### 4.10 Timing Budget Management

```rust
entity DataPath {
    timing_budget total: 10ns

    // Distribute timing budget across pipeline stages
    stage1: Decoder {
        timing_budget: 3ns  // 30% of total
    }

    stage2: Execute {
        timing_budget: 4ns  // 40% of total
    }

    stage3: Writeback {
        timing_budget: 3ns  // 30% of total
    }

    // Compiler verifies: sum(budgets) <= total
    assert sum_timing_budgets() <= total
}
```

### 4.11 Physical Constraints

SKALP extends the philosophy of inline timing constraints (section 4.5) to physical constraints. Unlike traditional HDLs where physical constraints live in separate vendor-specific files (PCF, XDC, SDC), SKALP allows constraints to be specified inline with port declarations while maintaining compatibility with external constraint files.

#### 4.11.1 Design Philosophy

**Rationale:**
- **Single Source of Truth**: Physical constraints are design intent and should travel with the code
- **Type Safety**: Compiler validates pin names against device database, catching errors at compile time
- **Version Control**: Constraints are versioned with the design, not separate files
- **Tool Independence**: Same syntax generates PCF, XDC, or SDC as needed

**Compatibility:**
- Inline constraints are the primary mechanism (recommended)
- External constraint files supported for legacy designs and board-level constraints
- Inline constraints override external constraints when both are present

#### 4.11.2 Inline Pin Constraints

Physical constraints attach to entity ports using the `@` syntax with a constraint block:

```rust
entity LedBlinker {
    // Single-pin constraint
    in clk: clock @ {
        pin: "A1",
        io_standard: "LVCMOS33",
        frequency: 100MHz
    }

    // Pin with electrical characteristics
    in rst: reset @ {
        pin: "B2",
        io_standard: "LVCMOS33",
        pull: up,
        schmitt: true
    }

    // Multi-pin bus (pins auto-assigned in order)
    out leds: bit[8] @ {
        pins: ["C1", "C2", "C3", "C4", "D1", "D2", "D3", "D4"],
        io_standard: "LVCMOS33",
        drive: 8mA,
        slew: fast
    }

    // Differential pair
    inout lvds_data: bit @ {
        pin_p: "E1",    // Positive
        pin_n: "E2",    // Negative
        io_standard: "LVDS_25",
        diff_term: 100  // 100Ω termination
    }
}
```

#### 4.11.3 Constraint Block Syntax

**Grammar:**

```ebnf
physical_constraint_block ::= '{' constraint_pair { ',' constraint_pair } '}'

constraint_pair ::=
    | 'pin' ':' pin_location
    | 'pins' ':' pin_array
    | 'pin_p' ':' string_literal     // Differential positive
    | 'pin_n' ':' string_literal     // Differential negative
    | 'io_standard' ':' io_standard_name
    | 'drive' ':' drive_strength
    | 'slew' ':' slew_rate
    | 'pull' ':' termination
    | 'diff_term' ':' integer        // Differential termination (Ω)
    | 'schmitt' ':' bool
    | 'frequency' ':' frequency_value

pin_location ::= string_literal          // e.g., "A1", "PIN_12"
pin_array ::= '[' string_literal { ',' string_literal } ']'
io_standard_name ::= identifier          // e.g., LVCMOS33, LVDS_25
drive_strength ::= integer 'mA'          // e.g., 4mA, 8mA, 12mA, 16mA
slew_rate ::= 'fast' | 'slow' | 'medium'
termination ::= 'up' | 'down' | 'none' | 'keeper'
```

#### 4.11.4 Device-Specific I/O Standards

**iCE40 (Lattice):**
```rust
// Common iCE40 I/O standards
in signal: bit @ {
    pin: "A1",
    io_standard: "SB_LVCMOS"  // or SB_LVDS_INPUT, SB_LVCMOS33, etc.
}
```

**Xilinx 7-Series:**
```rust
// Xilinx I/O standards
in ddr_data: bit[32] @ {
    pins: ["AA1", "AA2", /* ... */],
    io_standard: "SSTL15",      // DDR3
    slew: fast,
    drive: 16mA
}

in lvds_clk: bit @ {
    pin_p: "AB1",
    pin_n: "AB2",
    io_standard: "LVDS_25"
}
```

**Intel Stratix/Cyclone:**
```rust
// Intel I/O standards
inout dram_dq: bit[16] @ {
    pins: ["PIN_A1", "PIN_A2", /* ... */],
    io_standard: "SSTL-15 CLASS I",
    drive: 16mA,
    on_chip_termination: "RZQ/6"
}
```

#### 4.11.5 Global Constraint Blocks

For device selection, floorplanning, and design-wide constraints:

```rust
// Device and floorplan constraints
constraint physical {
    // Target device selection
    device: "iCE40HX8K-CT256"

    // Floorplan regions
    floorplan {
        // Define placement regions
        region "fast_logic" {
            area: (x1: 10, y1: 10, x2: 30, y2: 30),
            instances: [adder, multiplier, alu]
        }

        region "io_ring" {
            area: (x1: 0, y1: 0, x2: 50, y2: 50),
            boundary: true,
            instances: [uart_tx, uart_rx, spi_interface]
        }

        // Keep groups together
        group "crypto_core" {
            instances: [aes_encrypt, aes_decrypt, key_schedule],
            keep_together: true,
            preferred_region: "fast_logic"
        }
    }

    // Global I/O defaults
    io_defaults {
        io_standard: "LVCMOS33",
        drive: 8mA,
        slew: slow  // Conservative default
    }
}
```

#### 4.11.6 Bank and Voltage Constraints

For multi-bank FPGAs with different I/O voltages:

```rust
constraint physical {
    // Define I/O banks and voltages
    bank 0 {
        voltage: 3.3,
        io_standard: "LVCMOS33"
    }

    bank 1 {
        voltage: 1.8,
        io_standard: "LVCMOS18"
    }

    bank 2 {
        voltage: 2.5,
        io_standard: "LVDS_25"
    }
}

entity MultiVoltageDesign {
    // Pins assigned to specific banks
    in low_voltage_input: bit @ {
        pin: "A1",
        bank: 1,  // Must match bank voltage
        io_standard: "LVCMOS18"
    }

    in diff_input: bit @ {
        pin_p: "C1",
        pin_n: "C2",
        bank: 2,
        io_standard: "LVDS_25"
    }
}
```

#### 4.11.7 Validation Rules

**Compile-Time Checks:**

1. **Pin Existence**: Pin names validated against device database
   ```rust
   // Error: Pin "Z99" does not exist on iCE40HX8K-CT256
   in clk: clock @ { pin: "Z99" }
   ```

2. **Pin Conflicts**: Multiple signals cannot use same pin
   ```rust
   // Error: Pin "A1" used by both clk and data
   in clk: clock @ { pin: "A1" }
   in data: bit @ { pin: "A1" }  // CONFLICT
   ```

3. **I/O Standard Compatibility**: Standard must match device capabilities
   ```rust
   // Error: LVDS_25 not supported on iCE40 HX series
   in signal: bit @ {
       pin: "A1",
       io_standard: "LVDS_25"  // iCE40 uses SB_LVDS_INPUT
   }
   ```

4. **Bank Voltage Compatibility**: I/O standard must match bank voltage
   ```rust
   constraint physical {
       bank 0 { voltage: 3.3 }
   }

   entity Design {
       // Error: LVCMOS18 requires 1.8V, but bank 0 is 3.3V
       in signal: bit @ {
           pin: "A1",  // Bank 0
           io_standard: "LVCMOS18"
       }
   }
   ```

5. **Differential Pair Validity**: Differential pins must be adjacent/valid pairs
   ```rust
   // Error: A1 and Z99 are not a valid differential pair
   in lvds: bit @ {
       pin_p: "A1",
       pin_n: "Z99"  // INVALID PAIR
   }
   ```

6. **Bus Width Matching**: Number of pins must match signal width
   ```rust
   // Error: 8-bit signal requires 8 pins, only 4 provided
   out data: bit[8] @ {
       pins: ["A1", "A2", "A3", "A4"]  // Need 4 more pins
   }
   ```

#### 4.11.8 External Constraint Files

For compatibility and board-level constraints:

**Loading External Constraints:**

```bash
# CLI support for external constraint files
skalp build design.sk --constraints board.pcf --device iCE40HX8K

# Multiple constraint files (merged in order)
skalp build design.sk \
    --constraints board.pcf \
    --constraints timing.sdc \
    --device iCE40HX8K
```

**Precedence Rules:**

1. Inline constraints in source code (highest priority)
2. Command-line constraint files (in order specified)
3. Global defaults in `constraint physical` blocks
4. Tool defaults (lowest priority)

**Example PCF (iCE40):**
```tcl
# board.pcf - iCE40 Physical Constraints File
set_io clk A1
set_io rst B2
set_io led[0] C1
set_io led[1] C2
```

**Example XDC (Xilinx):**
```tcl
# board.xdc - Xilinx Design Constraints
set_property PACKAGE_PIN A1 [get_ports clk]
set_property IOSTANDARD LVCMOS33 [get_ports clk]
set_property PACKAGE_PIN B2 [get_ports rst]
set_property PULLUP true [get_ports rst]
```

**Constraint File Merging:**

```rust
// design.sk - has inline constraints
entity Design {
    in clk: clock @ { pin: "A1" }  // Inline
    in rst: reset                   // No constraint
    out led: bit[4]                // No constraint
}

// board.pcf - external constraints
// set_io rst B2        # Applied (rst has no inline constraint)
// set_io clk C1        # IGNORED (clk has inline constraint at A1)
// set_io led[0] D1     # Applied
// set_io led[1] D2     # Applied
// set_io led[2] D3     # Applied
// set_io led[3] D4     # Applied

// Result: clk=A1 (inline), rst=B2 (external), led=D1-D4 (external)
```

#### 4.11.9 Advanced Features

**Conditional Constraints by Build Target:**

```rust
entity PortableDesign {
    // Constraints vary by target device
    in clk: clock @ {
        pin: if target == "iCE40HX8K" then "A1"
             else if target == "Artix7" then "E3"
             else "PIN_N14",  // Intel default
        io_standard: if target.vendor == "Lattice" then "SB_LVCMOS"
                     else "LVCMOS33"
    }
}
```

**Auto-Generated Constraint Documentation:**

```rust
// Compiler can generate constraint documentation
// skalp build design.sk --emit-constraints-doc constraints.md

entity Design {
    in clk: clock @ {
        pin: "A1",
        io_standard: "LVCMOS33"
        /* Generates documentation:
         * Signal: clk
         * Direction: Input
         * Type: clock
         * Pin: A1 (Bank 0, Position: Top-Left)
         * I/O Standard: LVCMOS33 (3.3V CMOS)
         * Board Connection: Crystal oscillator output
         */
    }
}
```

#### 4.11.10 Integration with Synthesis Flow

**Native Place & Route:**

When using SKALP's native place and route tools:

```rust
// Physical constraints directly guide placer
entity Design {
    in clk: clock @ { pin: "A1" }  // Fixed I/O location
}

// Native placer:
// 1. Reads inline constraints from HIR/MIR
// 2. Pre-assigns fixed I/O locations before placement
// 3. Uses electrical characteristics during routing
// 4. Generates bitstream with correct I/O configuration
```

**External Tool Integration:**

When using vendor tools (Vivado, Quartus, NextPNR):

```rust
// Compilation flow:
// 1. Parse inline constraints from SKALP source
// 2. Generate tool-specific constraint file (XDC/PCF/SDC)
// 3. Invoke synthesis tool with generated constraints
// 4. Validate results against original constraints

// Example: iCE40 flow
skalp build led.sk --device iCE40HX8K --native=false

// Internally:
// 1. Extracts pin constraints from led.sk
// 2. Generates /tmp/build/led.pcf
// 3. Calls: yosys led.v -o led.json
// 4. Calls: nextpnr-ice40 --pcf /tmp/build/led.pcf ...
// 5. Calls: icepack led.asc led.bin
```

#### 4.11.11 Complete Example

```rust
// Board constraint definition
constraint physical {
    device: "iCE40HX8K-CT256"

    bank 0 { voltage: 3.3 }
    bank 1 { voltage: 3.3 }
    bank 2 { voltage: 1.8 }

    io_defaults {
        io_standard: "LVCMOS33",
        slew: slow
    }

    floorplan {
        region "cpu_core" {
            area: (10, 10, 40, 40),
            instances: [cpu, ram, rom]
        }
    }
}

// UART communication module
entity UartTx {
    // 100MHz system clock - high-speed pin
    in clk: clock<100MHz> @ {
        pin: "J3",
        bank: 0,
        io_standard: "LVCMOS33",
        frequency: 100MHz
    }

    // Active-high reset with pull-up
    in rst: reset(active_high) @ {
        pin: "K3",
        bank: 0,
        io_standard: "LVCMOS33",
        pull: up
    }

    // UART transmit pin - medium drive
    out tx: bit @ {
        pin: "A2",
        bank: 1,
        io_standard: "LVCMOS33",
        drive: 8mA,
        slew: medium
    }

    // Optional flow control
    in cts: bit @ {
        pin: "A3",
        bank: 1,
        io_standard: "LVCMOS33",
        pull: up,
        schmitt: true  // Noise immunity
    }

    // 8-bit data input (internal, no pins)
    in data: bit[8]
    in valid: bit
    out ready: bit
}

impl UartTx {
    // Implementation here...
}
```

This example demonstrates:
- Device selection
- Bank voltage management
- Per-pin I/O standards
- Electrical characteristics (drive, slew, pull)
- Mixed internal/external signals
- Clean integration with clock domain specifications

## 5. Entities and Implementation

### 5.1 Entity Declaration

```rust
entity Adder[Width: nat = 8] {
    // Ports
    in a, b: bit[Width]
    out sum: bit[Width + 1]
    out overflow: bit

    // Optional clock and reset
    in clock: event
    in reset: bit
}
```

### 5.2 Entity Implementation

```rust
impl Adder {
    // Combinational logic
    let full_sum = a.extend() + b.extend()
    sum = full_sum[Width:0]
    overflow = full_sum[Width]

    // Or with sequential logic
    on(clock.rise) {
        if reset {
            sum <= 0
            overflow <= 0
        } else {
            sum <= full_sum[Width:0]
            overflow <= full_sum[Width]
        }
    }
}
```

### 5.3 Async Entity Declaration (NCL)

NCL (Null Convention Logic) entities are declared with the `async` keyword and synthesize to clockless asynchronous circuits using dual-rail encoding and threshold gates.

```rust
// Async (NCL) entity - no clock, self-timed
async entity NclAdder {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[8]
    out carry: bit
}

impl NclAdder {
    // Logic is automatically transformed to dual-rail NCL
    let result = a +: b
    sum = result[7:0]
    carry = result[8]
}
```

#### Barrier Statement (Pipeline Stages)

The `barrier` statement marks pipeline stage boundaries with completion detection:

```rust
async entity NclPipeline {
    in data: bit[32]
    out result: bit[32]
}

impl NclPipeline {
    // Stage 1
    let stage1 = data * 2

    barrier  // Completion detection inserted here

    // Stage 2
    let stage2 = stage1 + 1

    barrier  // Completion detection inserted here

    // Stage 3
    result = stage2 & 0xFFFF
}
```

#### Explicit NCL Types

Use `ncl<N>` for explicit dual-rail type declarations:

```rust
async entity NclMux {
    in sel: ncl<1>      // 1 logical bit = 2 wires
    in a: ncl<8>        // 8 logical bits = 16 wires
    in b: ncl<8>
    out y: ncl<8>
}
```

#### NCL Synthesis

| SKALP Operation | NCL Implementation |
|-----------------|-------------------|
| `a & b` | TH22(a_t,b_t) / TH12(a_f,b_f) |
| `a \| b` | TH12(a_t,b_t) / TH22(a_f,b_f) |
| `~a` | a_f / a_t (rail swap) |
| `a + b` | NCL full-adder chain |
| `a * b` | NCL AND array + adder tree |

See [NCL Documentation](NCL_ASYNC_CIRCUITS.md) for complete details.

### 5.4 Entity Instantiation and Hierarchical Design

#### Basic Instantiation

```rust
entity Adder {
    in a, b: bit[16]
    out sum: bit[17]
    out carry: bit
}

entity Top {
    in x, y: bit[16]
    out result: bit[17]
}

impl Top {
    // Method 1: Positional port mapping
    let adder1 = Adder(x, y)  // Maps x→a, y→b by position

    // Method 2: Named port mapping (preferred for clarity)
    let adder2 = Adder {
        a: x[15:0],
        b: y[15:0]
    }

    // Method 3: Partial mapping with defaults
    let adder3 = Adder {
        a: x,
        b: y,
        // Outputs are implicitly created as signals
    }

    // Access sub-entity outputs
    result = adder2.sum
}
```

#### Hierarchical Composition

```rust
entity ALU {
    in op: bit[3]
    in a, b: bit[32]
    out result: bit[32]
    out flags: bit[4]
}

entity RegisterFile {
    in clk: clock
    in read_addr1, read_addr2: bit[5]
    in write_addr: bit[5]
    in write_data: bit[32]
    in write_en: bit
    out read_data1, read_data2: bit[32]
}

entity CPU {
    in clk: clock
    in reset: bit
    in inst: bit[32]
    out data_out: bit[32]
}

impl CPU {
    // Instantiate sub-components
    let alu = ALU {
        op: inst[31:28],
        a: rf.read_data1,
        b: rf.read_data2
    }

    let rf = RegisterFile {
        clk: clk,
        read_addr1: inst[25:21],
        read_addr2: inst[20:16],
        write_addr: inst[15:11],
        write_data: alu.result,
        write_en: inst[0]
    }

    // Internal connections
    data_out = alu.result
}
```

#### Array of Instances

```rust
entity ProcessingElement {
    in a, b: bit[8]
    out result: bit[16]
}

entity SystolicArray {
    in data: bit[8][16]  // 16 8-bit inputs
    out results: bit[16][16]  // 16 16-bit outputs
}

impl SystolicArray {
    // Generate array of instances
    for i in 0..16 {
        let pe[i] = ProcessingElement {
            a: data[i],
            b: if i > 0 { pe[i-1].result[7:0] } else { 0 }
        }
        results[i] = pe[i].result
    }
}
```

#### Conditional Instantiation

```rust
entity Processor {
    in config: bit[2]
    in data: bit[32]
    out result: bit[32]
} with generic {
    INCLUDE_FPU: bool = true,
    CACHE_SIZE: nat = 1024
}

impl Processor {
    // Conditional instantiation based on generics
    if INCLUDE_FPU {
        let fpu = FloatingPointUnit {
            operand: data,
            mode: config
        }
        result = fpu.output
    } else {
        result = data  // Pass through
    }

    // Parameterized instantiation
    let cache = Cache[CACHE_SIZE] {
        addr: data[19:0],
        data_in: data
    }
}
```

#### Port Slicing and Concatenation

```rust
entity Router {
    in data_in: bit[64]
    out data_out: bit[4][16]  // 4 channels, 16 bits each
}

impl Router {
    // Multiple sub-entities with sliced connections
    for i in 0..4 {
        let channel[i] = Channel {
            in_data: data_in[i*16 +: 16],  // Bit slicing
            enable: data_in[63]
        }
        data_out[i] = channel[i].out_data
    }

    // Concatenation example
    let merged = Merger {
        inputs: [channel[0].status, channel[1].status,
                 channel[2].status, channel[3].status]
    }
}
```

### 5.4 Generate Blocks and Parameterized Design

```rust
entity ParametricDesign[N: nat = 8, USE_PIPELINE: bool = false] {
    in data: bit[N][16]
    out results: bit[N][16]
}

impl ParametricDesign {
    // Generate for loop - creates hardware
    generate for i in 0..N {
        let proc[i] = Processor {
            input: data[i]
        }

        // Conditional generate
        generate if USE_PIPELINE {
            let pipe[i] = PipelineReg {
                d: proc[i].output
            }
            results[i] = pipe[i].q
        } else {
            results[i] = proc[i].output
        }
    }

    // Generate case for different configurations
    generate match N {
        8 => {
            let optimizer = SmallOptimizer { ... }
        },
        16 => {
            let optimizer = MediumOptimizer { ... }
        },
        _ => {
            let optimizer = GenericOptimizer { ... }
        }
    }
}
```

### 5.5 Bidirectional Ports and Tristate

```rust
entity MemoryInterface {
    inout data_bus: logic[16]  // Bidirectional port
    in read_en: bit
    in write_en: bit
    in write_data: bit[16]
    out read_data: bit[16]
}

impl MemoryInterface {
    // Tristate driver
    data_bus = write_en ? write_data : 'z

    // Read from bidirectional port
    read_data = data_bus when read_en else 0

    // Alternative tristate syntax
    driver data_driver {
        enable: write_en,
        value: write_data,
        bus: data_bus
    }
}
```

### 5.7 Clock and Reset Handling

```rust
entity Resetable {
    in clk: clock
    in rst_n: reset<active_low>  // Active-low reset
    in rst: reset<active_high>   // Active-high reset
    in data: bit[8]
    out q: bit[8]
}

impl Resetable {
    // Synchronous reset
    on(clk.rise) {
        if (!rst_n) {
            q <= 0
        } else {
            q <= data
        }
    }

    // Asynchronous reset (using OR notation)
    on(clk.rise | rst.rise) {
        if (rst) {
            q <= 0
        } else {
            q <= data
        }
    }

    // Reset synchronizer
    signal rst_sync: bit[2]
    on(clk.rise) {
        rst_sync <= [rst_sync[0], rst]
    }
}
```

### 5.8 Memory and RAM/ROM Instantiation

```rust
entity MemorySystem {
    in clk: clock
    in addr: bit[10]
    in write_data: bit[32]
    in write_en: bit
    out read_data: bit[32]
}

impl MemorySystem {
    // Inferred RAM
    signal ram: bit[32][1024]  // 1K x 32-bit RAM

    on(clk.rise) {
        if (write_en) {
            ram[addr] <= write_data
        }
        read_data <= ram[addr]
    }

    // ROM initialization
    const rom: bit[8][256] = include_binary("rom_data.hex")

    // Dual-port RAM
    memory dpram: DualPortRAM[1024, 32] {
        port_a: { clk: clk, addr: addr_a, ... },
        port_b: { clk: clk, addr: addr_b, ... }
    }

    // Memory with initialization
    signal mem: bit[16][512] = init {
        0: 0x1234,
        1: 0x5678,
        default: 0xDEAD
    }
}
```

### 5.9 FSM (Finite State Machine) Pattern

```rust
enum State: bit[2] {
    Idle = 0,
    Active = 1,
    Wait = 2,
    Done = 3
}

entity FSM {
    in clk: clock
    in start: bit
    in done: bit
    out busy: bit
}

impl FSM {
    signal state: State = State::Idle
    signal next_state: State

    // State register
    on(clk.rise) {
        state <= next_state
    }

    // Next state logic (combinational)
    next_state = match state {
        State::Idle => if (start) { State::Active } else { State::Idle },
        State::Active => if (done) { State::Done } else { State::Wait },
        State::Wait => if (done) { State::Done } else { State::Wait },
        State::Done => State::Idle
    }

    // Output logic
    busy = (state != State::Idle)
}
```

### 5.10 Timing Intent in Entities

```rust
entity TimedModule {
    in clk: clock<200MHz>
    in data: bit[32] @ clk
    out result: bit[32] @ clk
} with timing {
    input_to_output: 5ns,
    register_to_register: 4ns,
    setup_slack: > 0.5ns,
    hold_slack: > 0.2ns
}

impl TimedModule {
    // Implementation must meet timing requirements
    // Compiler verifies timing intent is achievable
}
```

## 6. Signals and Variables

### 6.1 Signal Declaration

Signals update at the end of the delta cycle:

```rust
signal counter: nat[8] = 0  // Initial value
signal data: logic<'clk>[32]  // Clock domain specified
signal array: bit[8][16]  // Array of signals

// Packed vs unpacked arrays
signal packed: bit[7:0]     // Packed - treated as single vector
signal unpacked: bit[8]      // Unpacked - array of bits
signal mixed: bit[4][7:0]    // Unpacked array of packed vectors
```

### 6.2 Variable Declaration

Variables update immediately:

```rust
var temp: int[32] = 0
var state: State = State::Idle
```

### 6.3 Constants and Parameters

```rust
const SIZE = 256
const COEFFICIENTS: fixed[1.15][8] = [0.1, 0.2, ...]

parameter Width: nat = 32  // Generic parameter
localparam DEPTH = Width * 2  // Local parameter (computed)
```

### 6.4 Signal and Variable Assignment

SKALP uses consistent, VHDL-inspired assignment operators:

```rust
// Continuous assignment (outside process)
output = input + 1           // Combinational logic

// Inside sequential blocks:
on(clock.rise) {
    // Signals use <= (update at end of delta cycle)
    counter <= counter + 1

    // Variables use := (immediate update)
    temp := temp + 1
}

// Key difference from SystemVerilog:
// - No blocking (=) vs non-blocking (<=) confusion
// - Clear signal vs variable distinction
// - = is ONLY for combinational/continuous assignment

// Bit manipulation (works with = outside on blocks, <= inside)
signal word: bit[32]
on(clk.rise) {
    word[7:0] <= 0xFF           // Bit slice assignment
    word[31:28] <= nibble       // Upper nibble
    word[15 +: 8] <= byte_val   // Starting at 15, 8 bits up
    word[23 -: 8] <= byte_val   // Starting at 23, 8 bits down
}
```

### 6.5 Clock Domain Crossings

```rust
entity CDCExample {
    in data_a: bit[8]<'clk_a>     // Clock domain 'clk_a
    in valid_a: bit<'clk_a>
    out data_b: bit[8]<'clk_b>    // Clock domain 'clk_b
    out valid_b: bit<'clk_b>
}

impl CDCExample {
    // Synchronizer for single bit
    cdc sync_valid: Synchronizer<'clk_a, 'clk_b> {
        in: valid_a,
        out: valid_b
    }

    // Asynchronous FIFO for data
    cdc fifo: AsyncFIFO<'clk_a, 'clk_b>[8, 16] {
        write_data: data_a,
        write_valid: valid_a,
        read_data: data_b,
        read_ready: ready_b
    }

    // Gray code counter for CDC
    signal gray_cnt<'clk_a>: bit[4]
    signal gray_sync<'clk_b>: bit[4]

    cdc gray_cdc: GrayCodeSync<'clk_a, 'clk_b> {
        gray_in: gray_cnt,
        gray_out: gray_sync
    }

    // Handshake synchronizer
    cdc handshake: HandshakeSync<'clk_a, 'clk_b> {
        req_a: request_a,
        ack_a: acknowledge_a,
        req_b: request_b,
        ack_b: acknowledge_b
    }
}
```

### 6.6 Assertions and Properties

```rust
entity ProtocolChecker {
    in clk: clock
    in valid: bit
    in ready: bit
    in data: bit[32]
}

impl ProtocolChecker {
    // Immediate assertion
    assert(!(valid && !ready && $changed(data)),
           "Data changed while not ready!")

    // Concurrent assertion
    property valid_stable {
        @(posedge clk)
        valid && !ready |-> ##1 valid
    }
    assert(valid_stable, "Valid must stay high until ready")

    // Coverage
    cover(valid && ready, "Transaction completed")

    // Assumption for formal
    assume(ready |-> ##[1:5] !ready, "Ready pulses for 1-5 cycles")

    // Sequence definition
    sequence req_ack_seq {
        req ##1 !ack ##[1:3] ack
    }
    assert(@(posedge clk) req |-> req_ack_seq)
}
```

### 6.7 Structs and Unions

```rust
// Packed struct (synthesizable)
struct packed Command {
    opcode: bit[4],
    addr: bit[12],
    data: bit[16]
}

// Unpacked struct
struct DebugInfo {
    pc: bit[32],
    registers: bit[32][32],
    flags: StatusFlags
}

// Union for type punning
union packed FloatInt {
    f: float32,
    i: bit[32],
    parts: struct packed {
        mantissa: bit[23],
        exponent: bit[8],
        sign: bit
    }
}

// Usage
signal cmd: Command
cmd.opcode = 0x5
let addr_high = cmd.addr[11:8]
```

## 7. Protocols (Replacing Interfaces)

### 7.1 Protocol Definition

Protocols define reusable communication contracts with built-in directionality:

```rust
protocol Valid_Ready[T: type] {
    // Signal definitions with direction
    signals {
        data: T ->       // -> means master drives, slave receives
        valid: bit ->    // From master to slave
        ready: bit <-    // <- means slave drives, master receives
    }

    // Transaction semantics
    transaction Transfer {
        when (valid && ready) {
            capture data
        }
    }

    // Protocol properties (compiler-enforced)
    properties {
        data stable_when (valid && !ready)
        no_combinational_loop
    }
}
```

### 7.2 Protocol Usage (No Modports Needed!)

```rust
// Master entity - drives data and valid, receives ready
entity Producer {
    port out: Valid_Ready[bit[32]].master
}

// Slave entity - receives data and valid, drives ready
entity Consumer {
    port in: Valid_Ready[bit[32]].slave
}

// Connect them
entity System {
    let prod = Producer { }
    let cons = Consumer { }

    // Direct connection - protocol ensures correct directionality
    connect(prod.out, cons.in)
}
```

### 7.3 Complex Protocol Example (AXI4)

```rust
protocol AXI4 {
    // Write address channel
    signals write_addr {
        awaddr: bit[32] ->
        awlen: bit[8] ->
        awsize: bit[3] ->
        awvalid: bit ->
        awready: bit <-
    }

    // Write data channel
    signals write_data {
        wdata: bit[32] ->
        wstrb: bit[4] ->
        wlast: bit ->
        wvalid: bit ->
        wready: bit <-
    }

    // Write response channel
    signals write_resp {
        bresp: bit[2] <-
        bvalid: bit <-
        bready: bit ->
    }

    // Protocol ensures: master drives ->, slave drives <-
    // No need for modports!
}

entity AXIMaster {
    port axi: AXI4.master  // Gets correct signal directions
}

entity AXISlave {
    port axi: AXI4.slave   // Automatically inverted directions
}
```

### 7.4 Protocol Composition

```rust
protocol AXI4Stream extends Valid_Ready[bit[32]] {
    // Add streaming-specific signals
    signals {
        last: bit ->          // End of packet
        keep: bit[4] ->       // Byte enables
        id: bit[8] ->         // Stream ID
        user: bit[16] ->      // User-defined
    }

    // Additional semantics
    semantics {
        ordered_per_id        // Ordering per stream ID
        burst_capable        // Supports bursts
    }
}

// Can be used wherever Valid_Ready is expected
entity StreamProcessor {
    port in: AXI4Stream.slave
    port out: AXI4Stream.master
}
```

### 7.5 Protocol Adapters

```rust
// Automatic protocol conversion
entity ProtocolBridge {
    port axi_in: AXI4Stream.slave
    port vr_out: Valid_Ready[bit[32]].master
}

impl ProtocolBridge {
    // Protocols ensure type safety and correct connections
    vr_out.data = axi_in.data
    vr_out.valid = axi_in.valid
    axi_in.ready = vr_out.ready
    // last, keep, id are handled/dropped as needed
}
```

## 8. Intent Specifications

SKALP provides a comprehensive intent system that allows designers to specify synthesis constraints, optimization goals, and implementation strategies. Intents can be specified as annotations or used as first-class types for compile-time metaprogramming.

### 8.1 Intent as a First-Class Type

Intent can be used as a type parameter, enabling compile-time specialization:

```rust
// Intent type definition
type Intent = {
    latency: Cycles | auto,
    throughput: PerCycle | auto,
    accuracy: Accuracy | auto,
    area: LUT | auto,
    power: Milliwatts | auto,
    optimize: Optimize,
    overflow: OverflowMode | auto,
    rounding: RoundingMode | auto,
    // ... extensible
}

enum Optimize {
    Latency,
    Throughput,
    Area,
    Power,
    Balanced
}

enum Accuracy {
    Low,      // ~12 bits
    Medium,   // ~18 bits
    High      // Full precision
}

enum OverflowMode {
    Wrap,
    Saturate,
    Error
}

enum RoundingMode {
    Truncate,
    RoundNearest,
    RoundUp,
    RoundDown
}
```

#### Intent as Generic Parameter

Entities can be parametrized by intent, enabling single implementations with compile-time specialization:

```rust
// Generic sqrt that specializes based on intent
entity Sqrt<const F: FloatFormat, intent I: Intent = Intent::default()> {
    in x: fp<F>
    out result: fp<F>
}

impl<const F: FloatFormat, intent I> Sqrt<F, I> {
    // Compile-time branching based on intent
    result = if I.latency < 4 {
        // Fast LUT-based implementation
        lut_sqrt::<F>(x)
    } else if I.latency < 8 {
        // Medium-speed Newton-Raphson (1 iteration)
        newton_raphson::<F>(x, iterations: 1)
    } else if I.accuracy == Accuracy::High {
        // High-accuracy Newton-Raphson (2+ iterations)
        newton_raphson::<F>(x, iterations: 2)
    } else {
        // Balanced default
        newton_raphson::<F>(x, iterations: 1)
    }
}

// Usage - intent flows through type system
@intent(latency: 2_cycles)
impl FastProcessor {
    // Automatically selects lut_sqrt implementation
    signal r: fp32 = sqrt(x)  // Sqrt<IEEE754_32, Intent{latency:2}>
}

@intent(accuracy: High)
impl PreciseProcessor {
    // Automatically selects high-accuracy implementation
    signal r: fp32 = sqrt(x)  // Sqrt<IEEE754_32, Intent{accuracy:High}>
}
```

#### Intent Propagation

Intents propagate hierarchically through the module tree:

```rust
// Parent specifies top-level intent
@intent(optimize: Latency, latency: max(10_cycles))
impl VideoProcessor {
    flow {
        // Child operations inherit intent
        let blurred = pixels
            |> gaussian_blur()  // Inherits: optimize=Latency, latency<=10

        // Override for specific operations
        @intent(accuracy: High, latency: 16_cycles)
        let edges = blurred
            |> sobel_operator()  // Uses high-accuracy, relaxed latency
    }
}
```

#### Intent Constraints

Entities can specify intent requirements:

```rust
entity HighPerformanceModule<intent I: Intent>
where
    I.throughput >= 1_per_cycle,
    I.latency <= 10_cycles
{
    in data: stream<bit<32>>
    out result: stream<bit<32>>
}

// Compile-time validation
@intent(latency: 20_cycles)  // ❌ ERROR: violates constraint
impl Example {
    let hp = HighPerformanceModule<CurrentIntent> { }
}
```

#### Intent Composition

Intents can be merged and transformed:

```rust
// Define base intent
const BASE_INTENT: Intent = Intent {
    optimize: Optimize::Balanced,
    area: auto,
    latency: auto
}

// Compose intents
const FAST_INTENT: Intent = BASE_INTENT + Intent {
    optimize: Optimize::Latency,
    latency: 4_cycles
}

const SMALL_INTENT: Intent = BASE_INTENT + Intent {
    optimize: Optimize::Area,
    area: 500_lut
}

// Use in design
impl FastPath {
    @intent(FAST_INTENT)
    signal result = process(input)
}
```

### 8.2 Traditional Intent Annotations

Intent can also be specified using attribute-style annotations:

#### Entity Intent

```rust
entity Multiplier {
    in a, b: int[16]
    out product: int[32]
} with intent {
    architecture: booth_radix4,
    pipeline_stages: 2,
    optimization: balanced(speed: 0.7, area: 0.3),
    formal_verify: true
}
```

#### Implementation Intent

```rust
@intent(dsp_mapping: true)
impl Multiplier {
    product = a * b  // Maps to DSP blocks
}
```

#### Signal-Level Intent

```rust
impl Example {
    // Resource sharing
    @intent(share: sqrt)
    signal a: fp32 = sqrt(x)

    @intent(share: sqrt)
    signal b: fp32 = sqrt(y)

    // Pipeline scheduling
    @intent(pipeline: auto, latency: 8_cycles)
    signal c: fp32 = complex_op(z)
}
```

### 8.3 Intent Categories

#### Performance Intent

```rust
with intent {
    throughput: 1_sample_per_cycle,
    latency: 3_cycles,
    frequency: 200MHz,
    initiation_interval: 1_cycle
}
```

#### Resource Intent

```rust
with intent {
    dsp_blocks: 4,
    bram_usage: minimal,
    lut_usage: < 1000,
    resource_sharing: aggressive
}
```

#### Memory Intent

```rust
@intent(memory: {
    buffer: {
        banking: 8,              // 8-way banking
        mode: cyclic,            // Cyclic address mapping
        impl: bram,              // Block RAM
        partition: complete      // Fully partitioned
    }
})
signal buffer: [bit<32>; 1024]
```

#### Loop Intent

```rust
@intent(loop: {
    unroll: complete,            // Fully unroll
    pipeline: {ii: 1, latency: auto},
    tile: {i: 32, j: 32}        // Loop tiling
})
for i in 0..SIZE {
    // ...
}
```

#### Dataflow Intent

```rust
@intent(dataflow: {
    mode: pipeline,              // Pipeline dataflow
    channel_depth: 4,            // FIFO depth between stages
    blocking: auto               // Auto backpressure
})
flow {
    result = input
        |> stage1()
        |> stage2()
        |> stage3()
}
```

#### Power Intent

```rust
@intent(power: {
    clock_gating: auto,          // Automatic clock gating
    voltage: 0.9V,               // Operating voltage
    power_gating: {
        domains: [compute_array],
        retention: true
    }
})
impl PowerAwareDesign { }
```

#### Interface Intent

```rust
@intent(interface: {
    axi: {
        protocol: axi4,
        burst: incr,
        max_burst_length: 256,
        outstanding_transactions: 4
    }
})
entity DMAEngine {
    out axi: ~AXI4
}
```

#### Verification Intent

```rust
with intent {
    formal_verify: bounded_model_check,
    coverage: > 95%,
    assertions: enabled,
    protocol_check: axi4
}
```

### 8.4 Intent Profiles

Pre-defined intent combinations for common scenarios:

```rust
// Built-in profiles
profile "high_performance" {
    optimize: Latency,
    throughput: max,
    area: relaxed,
    power: relaxed
}

profile "low_power" {
    optimize: Power,
    power: minimal,
    clock_gating: true,
    latency: relaxed
}

profile "minimal_area" {
    optimize: Area,
    area: minimal,
    resource_sharing: aggressive,
    latency: relaxed
}

// Use profile
@intent(profile: "high_performance")
impl FastProcessor {
    // All operations use high_performance settings
}

// Override specific intent from profile
@intent(profile: "low_power")
impl MixedDesign {
    // Most operations use low_power

    @intent(override_profile, latency: 1_cycle)
    signal critical = fast_op(x)  // Override for critical path
}
```

### 8.5 Intent Resolution and Validation

The compiler validates and resolves intents:

```rust
// Conflict detection
@intent(latency: max(4_cycles))
@intent(accuracy: High)
impl ConflictingIntent {
    // ❌ ERROR: No sqrt satisfies both latency<=4 AND accuracy=High
    signal r: fp32 = sqrt(x)
}

// Helpful error message:
// error: no implementation satisfies all intents
//   --> example.sk:10:27
//    |
// 10 |     signal r: fp32 = sqrt(x)
//    |                      ^^^^^^^ cannot find sqrt implementation
//    |
// note: conflicting requirements:
//   - latency: max(4_cycles)  [from @intent on line 1]
//   - accuracy: High          [from @intent on line 2]
//    |
// note: available implementations:
//   - FP32SqrtFast: latency=4, accuracy=Medium ✗ (accuracy too low)
//   - FP32Sqrt: latency=8, accuracy=High ✗ (latency too high)
//    |
// help: relax one of the constraints
```

## 9. Flow Blocks and Dataflow

### 9.1 Basic Flow

```rust
entity Filter {
    in samples: stream[int[16]]
    out filtered: stream[int[16]]
}

impl Filter {
    flow {
        filtered = samples
            |> window(8)           // Create sliding window
            |> map(|w| w * coeff)  // Element-wise multiply
            |> sum()               // Reduce
            |> saturate(16)        // Saturate to 16 bits
    }
}
```

### 9.2 Stream Operations

```rust
flow {
    // Map operation
    let doubled = input.map(|x| x * 2)

    // Filter operation
    let positive = input.filter(|x| x > 0)

    // Reduce operation
    let sum = input.reduce(0, |acc, x| acc + x)

    // Window operation
    let windowed = input.window(4)  // [t-3, t-2, t-1, t]

    // Zip operation
    let combined = stream1.zip(stream2)
}
```

### 9.3 Parallel Flow

```rust
flow {
    // Fork into parallel paths
    let (path1, path2) = input.fork()

    // Process in parallel
    let result1 = path1 |> process1()
    let result2 = path2 |> process2()

    // Join results
    output = join(result1, result2)
}
```

## 10. Sequential Logic and Clock Events

### 10.1 SKALP's Synchronous Simulation Model

SKALP uses a purely synchronous, GPU-accelerated simulation model:

- **No sensitivity lists** - GPU evaluates everything in parallel each cycle
- **No event queue** - Fixed time-step simulation
- **Semantic syntax** - `on(clock.rise)` not abstract "process"
- **Two types of logic**:
  1. Sequential: `on` blocks triggered by clock edges
  2. Combinational: Continuous assignments with `=`

### 10.2 Sequential Logic (Event-Triggered Blocks)

The `on()` syntax explicitly lists ALL events that trigger the hardware block. The event list directly corresponds to the hardware's sensitivity - these are the exact events the hardware responds to.

#### Synchronous vs Asynchronous Reset - The Semantic Difference

```rust
// SYNCHRONOUS RESET - Only responds to clock edge
on(clock.rise) {
    if (reset) {        // Reset is sampled AT clock edge
        q <= 0          // Reset happens synchronously with clock
    } else {
        q <= data
    }
}
// Hardware: Reset signal goes to D input logic of flip-flop

// ASYNCHRONOUS RESET - Responds to clock OR reset
on(clock.rise | reset.rise) {
    if (reset) {        // Reset takes effect IMMEDIATELY on reset edge
        q <= 0          // Reset happens independent of clock
    } else {
        q <= data       // Normal operation on clock edge
    }
}
// Hardware: Reset signal goes to async clear pin of flip-flop
```

The key insight: **The OR notation (`|`) makes async behavior explicit**. Without `| reset.rise`, reset is synchronous. With it, reset is asynchronous. This perfectly matches the hardware implementation.

#### Common Event Patterns

```rust
// Single clock domain (most common)
on(clock.rise) {
    counter <= counter + 1
}

// Dual-edge clocking (DDR interfaces)
on(clock.rise | clock.fall) {
    ddr_data <= input    // Triggered on both edges
}

// Multiple async controls
on(clock.rise | reset.rise | preset.rise) {
    if (reset) {
        q <= 0          // Async reset to 0
    } else if (preset) {
        q <= 1          // Async preset to 1
    } else {
        q <= data       // Normal clocked operation
    }
}

// Different clocks (multi-clock design)
on(fast_clock.rise) {
    fast_counter <= fast_counter + 1
}

on(slow_clock.rise) {
    slow_counter <= slow_counter + 1
}

// Falling edge triggered
on(clock.fall) {
    negedge_data <= input
}
```

#### Why This Design?

1. **Explicit Triggers**: The event list shows EXACTLY what triggers the block
2. **No Hidden Behavior**: Synchronous vs asynchronous is visible in the trigger list
3. **Matches Hardware**: Directly corresponds to flip-flop input pins (clock, async reset, async preset)
4. **Prevents Bugs**: Can't accidentally create async logic when you meant sync
5. **Semantic Clarity**: `on(X | Y)` reads as "on X or Y", which is exactly the hardware behavior

```rust
// CLEAR: This responds to two events
on(clock.rise | reset.rise) { ... }

// WRONG: This would only respond to clock (sync reset)
on(clock.rise) {
    if (reset.rise) { ... }  // Compiler error! Can't check edges inside
}
```

### 10.3 Combinational Logic (No Process Needed!)

```rust
// In SKALP, combinational logic is just continuous assignment
// No process or sensitivity list needed
result = match opcode {
    ADD => a + b,
    SUB => a - b,
    AND => a & b,
    OR  => a | b,
    _   => 0
}

// Complex combinational logic
valid = ready && (count > 0) && !error
next_state = if reset { State::Idle } else { compute_next_state(current_state, input) }
```

### 10.4 Multiple Clock Domains

```rust
on(fast_clock.rise) {
    // Fast clock domain logic
    fast_counter <= fast_counter + 1
}

on(slow_clock.rise) {
    // Slow clock domain logic
    // CDC handled automatically for cross-domain signals
    slow_data <= synchronize(fast_counter)
}
```

### 10.5 For Loops

SKALP supports bounded `for` loops that iterate over compile-time known ranges. Unlike software languages, SKALP does not support unbounded iteration (`while`, `loop`) as these cannot be directly synthesized to hardware.

#### 10.5.1 Basic Syntax

```rust
// Exclusive range (0 to 7)
for i in 0..8 {
    result = result ^ ((input >> i) & 1);
}

// Inclusive range (0 to 7, same as above)
for i in 0..=7 {
    data[i] = 0;
}
```

#### 10.5.2 Loop Unrolling with `#[unroll]`

The `#[unroll]` attribute expands loops at compile time, generating parallel hardware for each iteration. This is essential for synthesizable hardware patterns.

```rust
// Full unroll - each iteration becomes parallel hardware
#[unroll]
for i in 0..8 {
    result[i] = input[7 - i];  // Generates 8 parallel assignments
}

// Partial unroll - unroll by factor N
#[unroll(4)]
for i in 0..32 {
    acc = acc + data[i];  // Unrolls 4 iterations per loop
}
```

**Unroll Modes:**
- `#[unroll]` - Full unroll: completely expands all iterations at compile time
- `#[unroll(N)]` - Partial unroll: expands N iterations per loop iteration, generating N parallel operations

#### 10.5.3 Common Loop Patterns

```rust
// XOR reduction (parity calculation)
#[unroll]
for i in 0..8 {
    result = result ^ ((input >> i) & 1) as bit[1];
}

// Bit reversal
#[unroll]
for i in 0..8 {
    result = result | ((input >> i) & 1) << (7 - i);
}

// Array initialization
#[unroll]
for i in 0..16 {
    memory[i] = 0;
}

// FIR filter accumulation
#[unroll]
for i in 0..TAPS {
    acc = acc + coefficients[i] * delay_line[i];
}
```

#### 10.5.4 Generate For (Hardware Instantiation)

For generating arrays of hardware instances, use `generate for`:

```rust
generate for i in 0..N {
    let proc[i] = Processor {
        input: data[i]
    }
}
```

## 11. Pattern Matching

### 11.1 Basic Patterns

```rust
match value {
    0 => output = 1,
    1..10 => output = 2,
    15 | 16 | 17 => output = 3,
    _ => output = 0
}
```

### 11.2 Bit Pattern Matching

```rust
match instruction {
    0b0000_xxxx => // Any value with upper nibble 0
    0b1xxx_0000 => // Upper bit 1, lower nibble 0
    0b1010_1010 => // Exact match
    _ => // Default
}
```

### 11.3 Structural Pattern Matching

```rust
match packet {
    Packet { header: 0xFF, payload, .. } => {
        // Match specific header, bind payload
        on(payload)
    }
    Packet { header, payload: [first, ..rest], crc } => {
        // Destructure array
        process_first(first)
    }
    _ => error()
}
```

### 11.4 Guard Conditions

```rust
match (state, input) {
    (Idle, x) if x > threshold => state <= Active,
    (Active, x) if x == 0 => state <= Idle,
    (Active, x) => counter <= counter + x,
    _ => // Do nothing
}
```

## 12. Traits and Generics

### 12.1 Trait Definition

```rust
trait Arithmetic {
    type Width: nat

    fn add(self, other: Self) -> Self
    fn multiply(self, other: Self) -> Self

    // Default implementation
    fn double(self) -> Self {
        self.add(self)
    }
}
```

### 12.2 Trait Implementation

```rust
impl Arithmetic for fixed[16.16] {
    type Width = 32

    fn add(self, other: Self) -> Self {
        // Fixed-point addition
        self + other
    }

    fn multiply(self, other: Self) -> Self {
        // Fixed-point multiplication with scaling
        (self * other) >> 16
    }
}
```

### 12.3 Generic Entities

```rust
entity Processor<T: Arithmetic> {
    in a, b: T
    out result: T
}

impl<T: Arithmetic> Processor<T> {
    result = a.add(b).double()
}

// Instantiation
let int_processor = Processor<int[32]>
let fixed_processor = Processor<fixed[16.16]>
```

### 12.4 Trait Bounds

```rust
entity Sorter<T: Comparable + Synthesizable, const N: nat> {
    in data: T[N]
    out sorted: T[N]
}

impl<T, N> Sorter<T, N>
where
    T: Comparable + Synthesizable,
    N: > 0
{
    flow {
        sorted = bitonic_sort(data)
    }
}
```

## 13. Verification and Testbenches

### 13.1 Context Separation

SKALP clearly separates synthesizable hardware from testbench code:

```rust
// HARDWARE CONTEXT (Synthesizable)
entity Processor {
    in clk: clock
    in reset: bit
    in data: bit[32]
    out result: bit[32]
}

impl Processor {
    on(clk.rise | reset.rise) {  // Hardware: OR notation for async reset
        if (reset) {
            result <= 0
        } else {
            result <= process_data(data)
        }
    }
}

// TESTBENCH CONTEXT (Non-synthesizable, runs on CPU)
#[testbench]
mod tests {
    // Async functions for testing - runs on CPU with Tokio
    async fn test_processor() {
        let mut dut = ProcessorSim::new();

        // Reset sequence
        dut.reset = 1;
        await clock_cycles(5);  // Wait asynchronously
        dut.reset = 0;

        // Test data processing
        dut.data = 0x42;
        await clock_cycle();

        // Check result
        assert_eq!(await dut.result.read(), 0x84);
    }

    // Can await on conditions
    async fn wait_for_ready(dut: &mut ProcessorSim) {
        await dut.ready.becomes_high();
    }
}
```

Key distinction:
- **Hardware**: Uses `on(clock.rise)`, no async/await
- **Testbench**: Uses `async fn` and `await` for test orchestration

### 13.2 Hardware Assertions

```rust
entity SafeDivider {
    in dividend, divisor: nat[32]
    out quotient: nat[32]

    // Immediate assertions
    assert divisor != 0 : "Division by zero"

    // Concurrent assertions
    assert always {
        divisor != 0 implies quotient == dividend / divisor
    }

    // Temporal assertions
    assert always {
        valid implies eventually ready
    }
}
```

### 13.2 Assumptions

```rust
entity Cache {
    in addr: bit[32]

    // Assumptions for formal verification
    assume addr[31:28] == 0  // Upper nibble always 0
    assume stable(addr, 3)    // Address stable for 3 cycles
}
```

### 13.3 Coverage

```rust
entity Controller {
    var state: State

    // Coverage points
    cover state == State::Idle : "Idle state reached"
    cover state == State::Active : "Active state reached"

    // Cross coverage
    cover cross(state, input) : "State-input combinations"
}
```

### 13.4 Formal Properties

```rust
entity Arbiter {
    in request: bit[4]
    out grant: bit[4]

    // Mutual exclusion
    prove mutual_exclusion {
        always count_ones(grant) <= 1
    }

    // Fairness
    prove fairness {
        always (request[i] implies eventually grant[i])
    }

    // Deadlock freedom
    prove no_deadlock {
        always (exists request implies eventually exists grant)
    }
}
```

## 14. Standard Library

### 14.1 Synchronization

```rust
use std::sync::{synchronize, handshake, gray_encode}

// Clock domain crossing
let synced = synchronize(async_signal)

// Handshake synchronization
let (req_sync, ack_sync) = handshake(req, ack)
```

### 14.2 Arithmetic

```rust
use std::math::{sqrt, sin, cos, log2}
use std::fixed::{multiply, divide, saturate}

// Fixed-point operations
let result = multiply<16,16>(a, b)
let sat = saturate<16>(extended_result)
```

### 14.3 Interfaces

```rust
use std::interfaces::{axi4, axi4_stream, avalon, wishbone}

entity Memory {
    port axi: axi4::slave<addr_width=32, data_width=64>
}
```

### 14.4 Data Structures

```rust
use std::structures::{fifo, lifo, cam}

// Instantiate FIFO
let buffer = fifo<bit[32], depth=16>

// Use FIFO
buffer.push(data)
let item = buffer.pop()
```

## 15. Examples

### 15.1 Complete UART Transceiver

```rust
entity UART {
    in clock: clock<'sys, 100MHz>
    in reset: bit
    in tx_data: bit[8]
    in tx_valid: bit
    out tx_ready: bit
    out rx_data: bit[8]
    out rx_valid: bit
    out uart_tx: bit
    in uart_rx: bit
} with intent {
    baud_rate: 115200,
    parity: none,
    stop_bits: 1
}

impl UART {
    const CLKS_PER_BIT = 100_000_000 / 115200

    // Transmitter FSM
    enum TxState { Idle, Start, Data(nat[3]), Stop }
    var tx_state: TxState = TxState::Idle
    var tx_shift: bit[8]
    var tx_count: nat[0..CLKS_PER_BIT]

    on(clock.rise) {
        if reset {
            tx_state := TxState::Idle
            uart_tx <= 1
        } else {
            match tx_state {
                TxState::Idle => {
                    uart_tx <= 1
                    tx_ready <= 1
                    if tx_valid {
                        tx_shift := tx_data
                        tx_state := TxState::Start
                        tx_ready <= 0
                    }
                }
                TxState::Start => {
                    uart_tx <= 0  // Start bit
                    if tx_count == CLKS_PER_BIT - 1 {
                        tx_count := 0
                        tx_state := TxState::Data(0)
                    } else {
                        tx_count := tx_count + 1
                    }
                }
                TxState::Data(bit_idx) => {
                    uart_tx <= tx_shift[bit_idx]
                    if tx_count == CLKS_PER_BIT - 1 {
                        tx_count := 0
                        if bit_idx == 7 {
                            tx_state := TxState::Stop
                        } else {
                            tx_state := TxState::Data(bit_idx + 1)
                        }
                    } else {
                        tx_count := tx_count + 1
                    }
                }
                TxState::Stop => {
                    uart_tx <= 1  // Stop bit
                    if tx_count == CLKS_PER_BIT - 1 {
                        tx_count := 0
                        tx_state := TxState::Idle
                    } else {
                        tx_count := tx_count + 1
                    }
                }
            }
        }
    }

    // Receiver implementation...
}
```

### 15.2 Pipelined Matrix Multiplier

```rust
entity MatrixMultiplier[N: nat] {
    in a, b: fixed[8.8][N][N]
    out c: fixed[16.16][N][N]
} with intent {
    architecture: systolic_array,
    throughput: N_cycles_per_matrix
}

impl MatrixMultiplier {
    // Processing element
    entity PE {
        in a_in, b_in: fixed[8.8]
        out a_out, b_out: fixed[8.8]
        in c_in: fixed[16.16]
        out c_out: fixed[16.16]
    }

    impl PE {
        on(clock.rise) {
            a_out <= a_in  // Pass through
            b_out <= b_in  // Pass through
            c_out <= c_in + (a_in * b_in)  // MAC
        }
    }

    // Systolic array of PEs
    signal pe_array: PE[N][N]

    // Connect PEs in systolic pattern
    generate for i in 0..N {
        generate for j in 0..N {
            pe_array[i][j] = PE {
                a_in: if j == 0 then a[i][*] else pe_array[i][j-1].a_out,
                b_in: if i == 0 then b[*][j] else pe_array[i-1][j].b_out,
                c_in: if i == 0 then 0 else pe_array[i-1][j].c_out
            }
        }
    }

    // Extract results
    for i in 0..N {
        c[N-1][i] = pe_array[N-1][i].c_out
    }
}
```

### 15.3 Protocol Bridge with CDC

```rust
entity ProtocolBridge {
    in axi_port: AXI4.slave<'bus_clk>
    out wishbone_port: Wishbone.master<'periph_clk>
} with intent {
    cdc_method: async_fifo,
    buffering: minimal
}

impl ProtocolBridge {
    // CDC FIFOs
    let cmd_fifo = AsyncFifo<Command, 4> {
        write_clk: 'bus_clk,
        read_clk: 'periph_clk
    }

    let resp_fifo = AsyncFifo<Response, 4> {
        write_clk: 'periph_clk,
        read_clk: 'bus_clk
    }

    // AXI side (bus_clk domain)
    on(clock<'bus_clk>.rise) {
        on axi_port.read_request as req {
            let cmd = Command {
                addr: req.addr,
                size: req.size,
                id: req.id
            }
            cmd_fifo.push(cmd)
        }

        if let Some(resp) = resp_fifo.pop() {
            axi_port.send_response(resp)
        }
    }

    // Wishbone side (periph_clk domain)
    on(clock<'periph_clk>.rise) {
        if let Some(cmd) = cmd_fifo.pop() {
            wishbone_port.cyc <= 1
            wishbone_port.stb <= 1
            wishbone_port.adr <= cmd.addr

            when wishbone_port.ack {
                let resp = Response {
                    data: wishbone_port.dat,
                    id: cmd.id
                }
                resp_fifo.push(resp)
            }
        }
    }
}
```

---

## 14. Requirements and Safety Features

### 14.1 Requirements Declaration

Requirements are first-class citizens in SKALP, supporting both functional and safety requirements:

```rust
// Functional requirement
requirement REQ_PERF_001 {
    id: "SYS-PERF-001",
    title: "Inference Throughput",
    description: "AI accelerator shall achieve 1 TOPS at 2W",
    type: functional,
    category: performance,
    measurable: {
        metric: throughput,
        target: 1_TOPS,
        conditions: { power: <= 2W },
    },
}

// Safety requirement (no separate keyword needed)
requirement SREQ_CPU_001 {
    id: "SAF-CPU-001",
    title: "CPU Lockstep Detection",
    type: safety,
    asil_level: ASIL_D,
    mechanism_type: PSM,  // Primary Safety Mechanism
    diagnostic_coverage: 99%,
}

// Import requirements from external systems
import requirements from "DOORS" {
    connection: "https://requirements.company.com/api",
    project: "PROJ-2024-001",
    baseline: "v1.0",
}
```

### 14.2 Entity Requirements and Safety

Entities use the existing `with` clause to declare requirements, safety mechanisms, and FMEA intent:

```rust
entity AIAccelerator {
    in data: stream<'clk>[512]
    out result: stream<'clk>[256]
} with {
    // Requirements satisfaction
    satisfies: [REQ_PERF_001, REQ_POWER_001],

    // Evidence of satisfaction
    evidence: {
        REQ_PERF_001: {
            throughput_achieved: 1.1_TOPS,
            power_measured: 1.8W,
            report: "validation/perf_report.html",
        },
    },

    // Regular intent (existing feature)
    intent: {
        architecture: systolic_array,
        optimization: balanced(speed: 0.7, area: 0.3),
    },
}

// Safety-critical entity with PSM
entity LockstepComparator {
    in core1: logic<'clk>[32]
    in core2: logic<'clk>[32]
    out error: logic<'clk>
} with {
    // Declare as safety mechanism
    safety_mechanism: {
        type: PSM,
        implements: SREQ_CPU_001,
        diagnostic_coverage: 99.2%,
    },

    // Safety properties
    safety_properties: {
        detection_latency: 1_cycle,
        false_positive_rate: < 0.001%,
    },
}

// LSM protecting a PSM
entity LockstepSelfTest {
    out comparator: LockstepComparator
    out test_result: logic<'clk>
} with {
    safety_mechanism: {
        type: LSM,
        implements: SREQ_CPU_002,
        protects: LockstepComparator,  // References the PSM
        test_interval: 100ms,
    },
}
```

### 14.3 FMEA Through Intent

FMEA annotations use the existing intent mechanism:

```rust
entity BrakeController {
    in pedal_input: logic<'clk>
    out brake_output: logic<'clk>
} with {
    // Requirements
    satisfies: [REQ_BRAKE_001],

    // FMEA through intent
    intent: {
        // Safety hazards
        hazards: {
            unintended_braking: "Brake applied without pedal input",
            loss_of_braking: "No brake when pedal pressed",
        },

        // Severity classification
        failure_impact: {
            unintended_braking: catastrophic,  // Maps to S3
            loss_of_braking: critical,         // Maps to S3
        },

        // Detection guidance for compiler
        detection_hints: {
            unintended_braking: "Monitor brake_output vs pedal_input",
            loss_of_braking: "Watchdog on brake actuation path",
        },
    },
}
```

### 14.4 Hierarchical Requirements

Requirements flow through hierarchy:

```rust
entity System {
    accelerator: AIAccelerator;  // Inherits REQ_PERF_001
    power_mgr: PowerManager;     // Partial REQ_POWER_001
} with {
    satisfies: [REQ_PERF_001, REQ_POWER_001],

    evidence: {
        REQ_PERF_001: {
            satisfied_by: accelerator,
            system_validation: "validation/system_perf.log",
        },
        REQ_POWER_001: {
            satisfied_by: [power_mgr, "external_pmic"],
            power_analysis: "power/full_system.rpt",
        },
    },
}
```

## 16. Attributes

Attributes provide compile-time directives for synthesis, simulation, debug, and design intent. They use the `#[attribute]` syntax similar to Rust.

### 16.1 Debug Attributes

```skalp
// Breakpoint - pause simulation on signal change
#[breakpoint]
signal error_flag: bit

// Conditional breakpoint
#[breakpoint(condition = "counter > 100")]
signal overflow_counter: bit[8]

// Error breakpoint - stop simulation immediately
#[breakpoint(is_error = true, name = "FATAL", message = "Critical error detected")]
signal critical_error: bit

// Signal tracing - export to waveform
#[trace]
signal debug_bus: bit[32]

// Grouped tracing with display options
#[trace(group = "pipeline", radix = hex, display_name = "Stage 1 Data")]
signal pipe1_data: bit[64]
```

### 16.2 Clock Domain Crossing (CDC) Attributes

```skalp
// Basic 2-stage synchronizer (default)
#[cdc]
signal async_input: bit

// Configurable synchronizer
#[cdc(sync_stages = 3)]
signal critical_sync: bit

// CDC types: two_ff, gray, pulse, handshake, async_fifo
#[cdc(cdc_type = gray, sync_stages = 2)]
signal fifo_ptr: bit[8]

#[cdc(cdc_type = pulse)]
signal trigger: bit

// Explicit domain crossing
#[cdc(from = 'clk_fast, to = 'clk_slow)]
signal cross_domain: bit[16]
```

### 16.3 Power Intent Attributes

Power intent as a first-class language feature, eliminating separate UPF files.

```skalp
// Retention - preserve state during power-down
#[retention]
signal saved_state: bit[32]

// Retention with explicit strategy
#[retention(strategy = balloon_latch)]
signal critical_reg: bit[16]

// Isolation - clamp to known value when domain powered off
#[isolation(clamp = low)]
signal isolated_output: bit[32]

#[isolation(clamp = high, enable = "iso_en")]
signal active_low_sig: bit

// Level shifter for voltage domain crossing
#[level_shift(from = "VDD_CORE", to = "VDD_IO")]
signal voltage_crossing: bit[16]

// Combined power domain crossing
#[pdc(from = 'core, to = 'io, isolation = clamp_low)]
signal power_domain_cross: bit[32]
```

**Generated SystemVerilog for retention:**
```systemverilog
(* RETAIN = "TRUE" *)
(* preserve = "true" *)
(* DONT_TOUCH = "TRUE" *)
reg [31:0] saved_state;
```

### 16.4 Memory Attributes

```skalp
// Block RAM inference
#[memory(depth = 1024, width = 64, style = block)]
signal bram: bit[64][1024]

// Distributed RAM (LUT-based)
#[memory(depth = 64, style = distributed)]
signal lutram: bit[16][64]

// UltraRAM (Xilinx)
#[memory(depth = 65536, style = ultra)]
signal uram: bit[72][65536]

// Register file
#[memory(depth = 32, style = register)]
signal regfile: bit[64][32]

// Dual-port with read latency
#[memory(depth = 512, ports = 2, read_latency = 2)]
signal dual_mem: bit[32][512]

// ROM inference
#[memory(depth = 256, read_only = true)]
signal lut: bit[8][256]
```

**Memory styles:** `auto`, `block`, `distributed`, `ultra`, `register`

### 16.5 Vendor IP Attributes

```skalp
// Xilinx IP wrapper
#[xilinx_ip(name = "xpm_fifo_sync", library = "xpm")]
entity SyncFifo<DEPTH: 512, WIDTH: 32> {
    in wr_clk: clock,
    in wr_en: bit,
    in din: bit[WIDTH],
    out full: bit,
    out empty: bit,
}

// Intel/Altera IP
#[intel_ip("altsyncram")]
entity DualPortRam { ... }

// Generic vendor IP
#[vendor_ip(name = "custom_dsp", vendor = generic, black_box = true)]
entity BlackBoxDSP { ... }
```

### 16.6 Synthesis Hints

```skalp
// Pipeline insertion
#[pipeline(stages = 4)]
entity PipelinedMultiplier { ... }

// Loop unrolling (see Section 10.5.2)
#[unroll]
for i in 0..8 { ... }

#[unroll(4)]  // Partial unroll by factor 4
for i in 0..32 { ... }

// Parallel execution hint
#[parallel]
impl ParallelAccumulator { ... }
```

### 16.7 Attribute Stacking

Multiple attributes can be applied to the same declaration:

```skalp
// Register file with retention and memory configuration
#[retention]
#[memory(depth = 8, width = 256, style = register)]
signal registers: bit[256][8]

// Signal with both trace and CDC
#[trace(group = "cdc", radix = hex)]
#[cdc(sync_stages = 2)]
signal sync_data: bit[32]
```

### 16.8 Attribute Grammar

```ebnf
attribute = "#[" attribute_name [ "(" attribute_params ")" ] "]"
attribute_name = ident
attribute_params = attribute_param { "," attribute_param }
attribute_param = ident "=" value | value
value = literal | ident | lifetime
```

## Appendix A: Grammar Summary

```ebnf
program = { package_decl | use_decl | entity_decl | impl_decl |
            type_decl | requirement_decl }

requirement_decl = "requirement" ident "{" requirement_body "}"

requirement_body = { requirement_field }

requirement_field = ident ":" value

entity_decl = "entity" ident [generic_params] "{"
              { port_decl }
              "}" [with_clause]

impl_decl = "impl" [generic_params] ident [where_clause] "{"
            { statement | process | flow_block }
            "}"

with_clause = "with" (with_single | with_block)

with_single = "intent" intent_block

with_block = "{" { with_item } "}"

with_item = "satisfies" ":" req_list |
            "evidence" ":" evidence_block |
            "safety_mechanism" ":" safety_block |
            "intent" ":" intent_block |
            "safety_properties" ":" properties_block

port_decl = ("in" | "out" | "inout") ident_list ":" type

type = primitive_type | array_type | struct_type | enum_type |
       generic_type | lifetime_type

process = "process" "(" sensitivity_list ")" "{" { statement } "}"

flow_block = "flow" "{" { flow_statement } "}"
```

## Appendix B: Operator Precedence

| Priority | Operators | Associativity |
|----------|-----------|---------------|
| 1 | `()` `[]` `.` | Left |
| 2 | `!` `~` `-` (unary) | Right |
| 3 | `*` `/` `%` | Left |
| 4 | `+` `-` | Left |
| 5 | `<<` `>>` | Left |
| 6 | `<` `<=` `>` `>=` | Left |
| 7 | `==` `!=` | Left |
| 8 | `&` | Left |
| 9 | `^` | Left |
| 10 | `\|` | Left |
| 11 | `&&` | Left |
| 12 | `\|\|` | Left |
| 13 | `? :` | Right |
| 14 | `=` `<=` `:=` | Right |

---

*End of SKALP Language Specification v1.0*

*संकल्पना - From conception to silicon*