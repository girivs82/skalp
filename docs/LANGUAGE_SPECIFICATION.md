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
// Core Hardware Description (11)
entity impl signal var const
in out inout on if else

// Type System (8)
bit bool clock reset type stream
struct enum

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

// Testbench Only (5)
async await fn return let

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
- `async`/`await`: Only for testbenches, never in synthesizable hardware

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

// Integer types
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

### 5.3 Entity Instantiation and Hierarchical Design

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

### 8.1 Entity Intent

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

### 8.2 Implementation Intent

```rust
impl Multiplier with intent(dsp_mapping: true) {
    product = a * b  // Maps to DSP blocks
}
```

### 8.3 Intent Categories

```rust
// Performance intent
with intent {
    throughput: 1_sample_per_cycle,
    latency: 3_cycles,
    frequency: 200MHz
}

// Resource intent
with intent {
    dsp_blocks: 4,
    bram_usage: minimal,
    lut_usage: < 1000
}

// Verification intent
with intent {
    formal_verify: bounded_model_check,
    coverage: > 95%,
    assertions: enabled
}
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