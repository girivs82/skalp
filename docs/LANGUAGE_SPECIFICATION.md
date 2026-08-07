# SKALP Language Specification
*Sankalpana (संकल्पना) - Intent-Driven Hardware Synthesis*

This document specifies the SKALP language **as implemented by the SKALP
compiler**. The compiler is the source of truth: every syntax form described
here is either exercised by the example corpus (`examples/`, `tests/`) or has
been verified to compile with `skalp build`. Constructs that are planned but
not implemented are collected in [Section 21](#21-future--not-implemented)
and are clearly marked; they must not be used in designs.

The formal grammar lives in [GRAMMAR.ebnf](GRAMMAR.ebnf).

## Table of Contents

1. [Introduction](#1-introduction)
2. [Lexical Structure](#2-lexical-structure)
3. [Type System](#3-type-system)
4. [Clock Domains](#4-clock-domains)
5. [Entities](#5-entities)
6. [Implementations, Signals, and Assignment](#6-implementations-signals-and-assignment)
7. [Sequential Logic: on-Blocks](#7-sequential-logic-on-blocks)
8. [Instantiation and Hierarchy](#8-instantiation-and-hierarchy)
9. [Expressions and Operators](#9-expressions-and-operators)
10. [Pattern Matching](#10-pattern-matching)
11. [Loops and Generate Constructs](#11-loops-and-generate-constructs)
12. [Functions and Tuples](#12-functions-and-tuples)
13. [Traits and Generics](#13-traits-and-generics)
14. [Numeric Types and the Standard Library](#14-numeric-types-and-the-standard-library)
15. [The Intent System](#15-the-intent-system)
16. [Attributes](#16-attributes)
17. [Physical Constraints](#17-physical-constraints)
18. [Power Domains](#18-power-domains)
19. [Asynchronous (NCL) Entities](#19-asynchronous-ncl-entities)
20. [Safety and Verification](#20-safety-and-verification)
21. [Future / Not Implemented](#21-future--not-implemented)

Appendix A: [Operator Precedence](#appendix-a-operator-precedence)
Appendix B: [Name Mangling of Flattened Composites](#appendix-b-name-mangling-of-flattened-composites)

## 1. Introduction

SKALP is a strongly-typed hardware description language with a modern type
system (generics, traits, pattern matching, tuples), compile-time clock-domain
tracking, and design-intent annotations that guide synthesis.

### 1.1 Design Goals

- **Safety**: Eliminate entire classes of hardware bugs at compile time
- **Expressiveness**: From high-level algorithms to cycle-accurate descriptions
- **Intent Preservation**: Design decisions are explicit and maintained
- **Composability**: Build complex systems from verified components

### 1.2 One Assignment Operator

SKALP deliberately has **one** assignment operator: `=`. Its hardware
semantics are inferred from context:

| Context | Meaning of `=` |
|---------|----------------|
| Inside an `on(event)` block | Registered (non-blocking) assignment — the target becomes a register |
| At `impl` level (outside `on`) | Continuous (combinational) assignment |
| `let name = expr` | Immutable combinational binding |

There is **no** `<=` assignment and **no** `:=` assignment. `<=` is always
the less-than-or-equal comparison. See
[Section 6.4](#64-the--comparison-pitfall) for the pitfall this implies for
users arriving from Verilog or VHDL.

### 1.3 Notation

- `monospace` for code and keywords
- [brackets] for optional elements in prose grammar sketches
- Complete examples in this document compile with the current compiler
  (they are checked by `tools/doc_snippet_check.py`)

## 2. Lexical Structure

### 2.1 Reserved Keywords

The following words are reserved by the lexer and cannot be used as ordinary
identifiers (they *can* still be used as **port names**, which the parser
accepts specially — e.g. `in reset: reset`):

```text
Core:          entity impl signal var const in input out output inout port
               on if else assign let inst mut fn return
Types:         bit bool string nat int logic clock reset type stream
               struct enum union distinct
Literals:      true false
Traits:        trait protocol where self Self
Events:        rise fall active inactive
Control flow:  match for while generate step as
Intent/flow:   intent flow requirement with
Async/NCL:     async await barrier
Modules:       use mod pub
Verification:  assert assume cover expect property sequence always
               eventually until strong weak throughout covergroup
               coverpoint bins ignore_bins illegal_bins cross invariant
               safety liveness bounded formal prove
Safety (ISO 26262): asil safety_req safety_goal safety_entity safety_trait
               fmea fmeda fmeda_library psm lsm hsr dhsr hsi covers ftti
               traces_to implements decomposes verification detection_time
               dc lc interval spfm lfm pmhf failure_mode severity component
               library exclude max_latency power_domain isolation
               diagnostic_coverage
Physical:      constraint physical pins pin_p pin_n io_standard slew pull
               diff_term schmitt floorplan instances boundary keep_together
               preferred_region io_defaults medium none keeper
```

### 2.2 Contextual Keywords

Many words that look like keywords lex as **plain identifiers** and are
recognized by text only in the contexts that need them. This keeps everyday
RTL names usable. Contextual (non-reserved) words include:

`area`, `bank`, `pin`, `voltage`, `target`, `fast`, `slow`, `up`, `down`,
`drive`, `frequency`, `region`, `device`, `open`, `active_high`,
`active_low`, `mux_style`, `pipeline_style`, `impl_style`, and the
power-domain declaration words `external`, `regulated`, `switched`,
`states`, `macro`, `on_when`, `ack_on` (recognized only inside a
`power_domain` declaration — see [Section 18](#18-power-domains)).

For example, `signal area: bit[16]` and `in up: bit` are legal.

`fp16`, `fp32`, and `fp64` are **not** keywords either — they are ordinary
types defined by the standard library (see [Section 14](#14-numeric-types-and-the-standard-library)).

### 2.3 Identifiers

```text
identifier = [a-zA-Z_][a-zA-Z0-9_]*
```

### 2.4 Comments

```text
// Line comment
/* Block comment */
/// Documentation comment (a line comment; picked up by tooling)
```

### 2.5 Literals

```text
// Boolean
true false

// Integers (underscores allowed as separators; there is NO octal form)
42            // Decimal
1_000_000     // Decimal with separators
0x2A          // Hexadecimal
0b1010_1010   // Binary

// Verilog-style sized literals (bases b, h, d)
4'b1010       // 4-bit binary
16'hDEAD      // 16-bit hex
4'd15         // 4-bit decimal

// Floating point (optional suffix: f, f16, f32, f64, fp16, fp32, fp64)
1.0  3.14  1.5e-10  2.5fp32

// Strings (escapes: \" \\ \n \t)
"hello\n"
```

There are no character literals and no time/frequency literals in
expressions. (A frequency such as `100MHz` appears only as a bound on a
clock-domain generic parameter, `<'clk: 100MHz>` — see Section 4.)

### 2.6 Clock-Domain Lifetimes

A lifetime token names a clock domain: `'a`, `'clk`, `'src`. Lifetimes
appear in generic parameter lists and as domain qualifiers on types
(`bit[8]<'a>`).

## 3. Type System

### 3.1 Primitive Types

```text
bit          // Single bit (2-state: 0 or 1)
bit[N]       // N-bit vector
nat[N]       // N-bit unsigned integer
int[N]       // N-bit signed integer
logic[N]     // Accepted as a synonym vector type
bool         // Boolean; for control flow and comparisons
string       // Simulation/testbench only; not synthesizable state
clock        // Clock; enables .rise/.fall event triggers
clock<'d>    // Clock in named domain 'd
reset        // Reset signal
reset(active_high) | reset(active_low)   // Explicit polarity
```

Notes:

- SKALP is 2-state; there is no `X`/`Z` in the value model.
- Width expressions may be any const-evaluable expression:
  `bit[WIDTH+1]`, `nat[clog2(DEPTH)]`.
- An angle-bracket width form also parses: `bit<WIDTH>`.
- `bool` and `bit` are distinct; convert explicitly with `as`.

### 3.2 Arrays

```text
[T; N]       // Array of N elements of T   e.g. signal mem: [bit[8]; 256]
T[N]         // Postfix form               e.g. signal mem: bit[8][256]
```

Both forms are accepted. Indexing is `mem[addr]`; arrays of registers used
inside `on` blocks infer memories.

### 3.3 Tuples

```text
(bit[8], bit)            // Tuple type
(a, b)                   // Tuple expression
let (q, r) = divmod(x);  // Destructuring
```

See [Section 12](#12-functions-and-tuples).

### 3.4 Structs

Structs group related data. When used as an entity port type, all fields
inherit the port's direction, and the port is flattened to per-field scalars
in the generated netlist (see [Appendix B](#appendix-b-name-mangling-of-flattened-composites)).

```skalp
struct Pair {
    hi: bit[4],
    lo: bit[4]
}

entity UsesPair {
    in d: Pair
    out q: bit[4]
}

impl UsesPair {
    q = d.hi
}
```

Struct values are built with struct literals: `Pair { hi: a, lo: b }`.

### 3.5 Enums

Enumerations provide named constants for state machines and opcodes.
Variants may carry explicit values.

```skalp
enum Op { Add = 0, Sub = 1 }

entity MiniAlu {
    in op: Op
    in a: bit[8]
    in b: bit[8]
    out q: bit[8]
}

impl MiniAlu {
    q = match op {
        Op::Add => a + b,
        Op::Sub => a - b
    }
}
```

Matching on an enum must cover every variant (or use `_`); see
[Section 10](#10-pattern-matching).

### 3.6 Unions, Type Aliases, Distinct Types

```text
union Raw { f: fp32, i: bit[32] }     // Type punning over one storage
type Word = bit[32]                    // Alias (no new type)
distinct type Celsius = bit[16]        // New nominal type over bit[16]
```

Inline (anonymous) `struct { ... }`, `enum { ... }`, and `union { ... }`
types are accepted in type position.

### 3.7 `stream<T>` Is Not Implemented

`stream<T>` parses (the keyword is reserved) but is **rejected** with a hard
error at compile time:

```text
port `d` of entity `S`: `stream<T>` is not implemented — no valid/ready
handshaking is generated; declare explicit `data`/`valid`/`ready` ports instead
```

Write explicit handshake ports instead. `stream` remains reserved for a
future flow-control feature.

## 4. Clock Domains

### 4.1 Domain Lifetimes on Types

Clock domains are expressed as lifetimes. A signal or port type may carry a
domain qualifier after its width:

```skalp
entity DomainDemo {
    in clk_a: clock<'a>
    in clk_b: clock<'b>
    in d: bit[8]<'a>
    out q: bit[8]<'b>
}

impl DomainDemo {
    signal s1: bit[8]<'b> = 0
    signal s2: bit[8]<'b> = 0

    on(clk_b.rise) {
        s1 = d
        s2 = s1
    }

    q = s2
}
```

Named domains participate in the compiler's CDC analysis: assignments that
cross from one named domain to another are detected, and the analysis
reports unsynchronized crossings. A two-stage synchronizer written as above
(two registers in the destination domain) is the canonical single-bit
crossing.

### 4.2 Domain Parameters on Entities

Entities may take domain lifetimes as generic parameters, optionally with a
frequency bound:

```text
entity Fifo<'wr, 'rd> { ... }
entity Serdes<'clk: 100MHz> { ... }
```

The frequency bound is parsed and recorded; it does not generate timing
constraints today.

### 4.3 The `#[cdc]` Attribute

`#[cdc]` on a signal declaration is a **verification annotation**: it marks
a signal as an intentional clock-domain crossing for the CDC analysis. It
does **not** synthesize a synchronizer — write the synchronizer registers
yourself:

```skalp
entity CdcMark {
    in clk: clock
    in d: bit
    out q: bit
}

impl CdcMark {
    #[cdc]
    signal s: bit = 0

    on(clk.rise) { s = d }
    q = s
}
```

Accepted arguments (`#[cdc(sync_stages = 3)]`, `#[cdc(cdc_type = gray)]`)
are recorded for the analysis but generate no hardware.

### 4.4 Design Decision: Domain Lifetimes Are Clock-Only

**Decided 2026-08-07.** The lifetime syntax (`bit[8]<'a>`) is reserved for
clock domains and will not be extended to other domain systems (power
domains in particular). This is a deliberate decision, not an accident of
implementation order.

The tick syntax pays for itself when a property is **value-granular,
propagates through expressions, and is mostly inferred**. Clock domain is
exactly that shape: it varies signal-by-signal within a single entity (a
synchronizer inherently holds values from two domains), the hazard lives at
expression joins (`a + b` across domains is the bug CDC analysis exists to
catch), and the compiler infers domains from drivers with annotations
needed only at boundaries. This is the same algebra as Rust borrows — a tag
riding on values, checked where values meet — which is why the borrowed
syntax reads correctly.

Power domain has a different algebra: it is a **regional containment
property**. A rail powers a block of logic; essentially every signal in an
instance inherits the instance's domain, and the event that matters is a
*net crossing an island boundary*, checkable at instance ports from
structural binding alone. A type-level tag that 99% of signals inherit
unchanged is noise, "power domain of an expression" has no meaningful
answer, and IEEE 1801 (UPF) — which the power model must ultimately emit —
is itself containment-based, so signal-level tags would have to be
collapsed back into regions anyway.

Overloading `<'...>` with a second tag kind would also cost real
readability: two tick-kinds distinguishable only by naming convention,
ordering/default questions, and two inference systems sharing one
syntactic position. Rust's lifetimes stay readable because there is exactly
one kind of them; this specification preserves that property. Power
domains bind structurally via attributes instead — see Section 18 for the
implemented model and Section 21.1 for the parts that remain future
work. Signal-level power *exceptions* (retention registers,
always-on straps in a gated block) are expressed as attributes on the
specific declarations, which is the natural syntax for exceptions to a
regional default.

## 5. Entities

### 5.1 Entity Declaration

An entity declares an interface: a name, optional generics, and a port list.

```skalp
entity Fifo<const WIDTH: nat = 8, const DEPTH: nat = 16> {
    in clk: clock
    in rst: reset(active_high)
    in wr_en: bit
    in wr_data: bit[WIDTH]
    out full: bit
    in rd_en: bit
    out rd_data: bit[WIDTH]
    out empty: bit
}

impl Fifo {
    signal memory: [bit[WIDTH]; DEPTH]
    signal wr_ptr: nat[clog2(DEPTH)]
    signal rd_ptr: nat[clog2(DEPTH)]
    signal count: nat[clog2(DEPTH+1)]

    empty = (count == 0)
    full = (count == DEPTH)
    rd_data = memory[rd_ptr]

    on(clk.rise) {
        if rst {
            wr_ptr = 0
            rd_ptr = 0
            count = 0
        } else {
            if wr_en && !full {
                memory[wr_ptr] = wr_data
                wr_ptr = (wr_ptr + 1) % DEPTH
            }
            if rd_en && !empty {
                rd_ptr = (rd_ptr + 1) % DEPTH
            }
            if wr_en && !rd_en && !full {
                count = count + 1
            } else if !wr_en && rd_en && !empty {
                count = count - 1
            }
        }
    }
}
```

Port directions are `in`, `out`, and `inout`. Separators between ports are
optional (newline, comma, or semicolon all work). One name per declaration:
write `in a: bit[8]` and `in b: bit[8]` on separate lines.

Keywords may be used as port names: `in reset: reset` is legal.

### 5.2 Generic Parameters

The generic parameter list accepts, in any order:

| Form | Meaning |
|------|---------|
| `const N: nat = 8` | Const value parameter with optional default |
| `T: SomeTrait` | Type parameter with trait bound |
| `'clk` / `'clk: 100MHz` | Clock-domain lifetime (optional frequency bound) |
| `intent I: Intent = DEFAULT_INTENT` | Intent parameter (used by the stdlib for implementation selection) |

A `where` clause may follow the parameter list.

### 5.3 Async Entities

`async entity` declares a clockless NCL entity — see
[Section 19](#19-asynchronous-ncl-entities).

## 6. Implementations, Signals, and Assignment

### 6.1 Impl Blocks

Every entity gets its behavior from an `impl` block. Items allowed in an
impl body:

- `signal` / `var` / `const` declarations
- `let` bindings (immutable combinational values, including tuple
  destructuring)
- continuous assignments (`port_or_signal = expr`)
- `on(event) { ... }` blocks
- `inst` entity instantiations
- `fn` / `const fn` function definitions
- local `struct` / `enum` / `union` declarations
- `if` / `match` / `for` / `generate` statements
- `with intent::name { ... }` blocks
- verification statements (`assert`, `assume`, `cover`, `covergroup`,
  `formal`, `invariant`, `prove`)
- anonymous `{ ... }` scope blocks for grouping related logic

### 6.2 Signals, Variables, Constants

```skalp
entity Decl {
    in clk: clock
    out q: bit[8]
}

impl Decl {
    signal counter: bit[8] = 0      // Hardware state (register or wire)
    const LIMIT: nat[8] = 200       // Compile-time constant
    let doubled = counter << 1      // Immutable combinational binding

    on(clk.rise) {
        counter = if counter == LIMIT { 0 } else { counter + 1 }
    }

    q = doubled
}
```

- **`signal`** declares hardware. A signal assigned inside an `on` block
  becomes a register; a signal assigned at impl level is combinational.
  The optional initializer is the register's reset/initial value.
- **`let`** binds a name to an expression. Bindings are immutable and
  combinational, and may carry a type annotation: `let r: bit[9] = a +: b`.
- **`var`** declares a procedural variable (mutable within statement
  context). Prefer `signal` and `let` in synthesizable code.
- **`const`** declares a compile-time constant, usable in widths, ranges,
  and generate conditions.

### 6.3 Assignment Semantics

The single `=` operator is context-inferred:

```skalp
entity Ctx {
    in clk: clock
    in d: bit[8]
    out q: bit[8]
    out comb: bit[8]
}

impl Ctx {
    signal r: bit[8] = 0

    // Continuous assignment at impl level: combinational
    comb = d + 1

    // Registered assignment inside on(): non-blocking, register semantics
    on(clk.rise) {
        r = d
    }

    q = r
}
```

Within one `on` block, all `=` assignments have non-blocking (registered)
semantics — reads see the *previous* cycle's value, and the last write to a
given target wins, exactly as in well-formed Verilog `always_ff` blocks.

Bit- and slice-targets are allowed on the left-hand side:
`word[7:0] = lo`, `mem[addr] = wr_data`, `s.field = x`.

The `assign` keyword form (`assign q = d`) is accepted as an explicit
continuous assignment; the bare form is idiomatic.

### 6.4 The `<=` Comparison Pitfall

`<=` is **only** the less-or-equal comparison. It is *never* an assignment.

Because an expression is a legal statement, Verilog-style code like this
**compiles silently and does nothing**:

```text
// WRONG — this is a comparison expression, evaluated and discarded.
// r keeps its initial value; the always_ff block is empty.
on(clk.rise) {
    r <= d
}
```

Write `r = d`. The documentation snippet checker
(`tools/doc_snippet_check.py`) and the linter flag `<=`-as-assignment
patterns for exactly this reason.

There is also no `:=` operator in the language.

## 7. Sequential Logic: on-Blocks

### 7.1 Event Triggers

An `on` block lists the events that trigger it:

```text
on(clk.rise)              { ... }   // Rising edge
on(clk.fall)              { ... }   // Falling edge
on(rst.active)            { ... }   // Reset asserted (respects polarity)
on(rst.inactive)          { ... }   // Reset deasserted
on(clk.rise | rst.active) { ... }   // Multiple triggers (| or ,)
on()                      { ... }   // Async/NCL entities only
```

Edge selectors are `.rise`, `.fall`, `.active`, `.inactive`. `.active` /
`.inactive` are for `reset`-typed signals and respect the declared polarity
(`reset(active_low)` asserts at 0).

### 7.2 Synchronous Reset

Sample the reset inside the clocked block:

```skalp
entity SyncReset {
    in clk: clock
    in rst: reset
    out count: nat[8]
}

impl SyncReset {
    signal counter: nat[8] = 0

    on(clk.rise) {
        if (rst) {
            counter = 0
        } else {
            counter = counter + 1
        }
    }

    count = counter
}
```

### 7.3 Asynchronous Reset

Give the reset its own `on` block:

```skalp
entity AsyncReset {
    in clk: clock
    in rst: reset
    in en: bit
    out count: bit[8]
}

impl AsyncReset {
    signal r: bit[8] = 0

    on(clk.rise) {
        if (en) { r = r + 1 }
    }

    on(rst.active) {
        r = 0
    }

    count = r
}
```

### 7.4 Multiple Clock Domains

Each `on` block belongs to the domain of its triggering clock; a design may
contain any number of independently-clocked blocks. Cross-domain reads are
subject to CDC analysis (Section 4).

### 7.5 Memories

An array signal written under a clock and indexed by a signal infers a
memory:

```skalp
entity Ram {
    in clk: clock
    in addr: bit[4]
    in wd: bit[8]
    in we: bit
    out rd: bit[8]
}

impl Ram {
    signal mem: bit[8][16]

    on(clk.rise) {
        if (we) { mem[addr] = wd }
    }

    rd = mem[addr]
}
```

## 8. Instantiation and Hierarchy

### 8.1 The `inst` Statement

Entities are instantiated with `inst`. The port map connects **inputs
only**; outputs are read with dot access on the instance name.

```skalp
entity Adder {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[8]
    out carry: bit
}

impl Adder {
    signal wide: bit[9]
    wide = a +: b
    sum = wide[7:0]
    carry = wide[8]
}

entity Top {
    in x: bit[8]
    in y: bit[8]
    out s: bit[8]
}

impl Top {
    inst add0 = Adder {
        a: x,
        b: y,
    }

    s = add0.sum        // Read an instance output with dot access
}
```

Rules:

- **Inputs must all be connected.** A missing input is a compile error:
  `input port 'en' of entity 'A' is not connected in 'inst a'`.
- **Outputs never appear in the port map** — read them via `name.port`.
  To document an intentionally unused output, you may list it connected to
  `open` (or `_`), which simply omits the connection:

  ```text
  inst a = A { x: x, unused_out: open }
  ```
- Generic arguments use angle brackets, with optional turbofish:
  `inst f = Fifo<16, 32> { ... }` or `inst f = Fifo::<16, 32> { ... }`.
- Connection expressions may be arbitrary expressions
  (`b: ~b`, `a: x[7:4]`).

### 8.2 `let` Instantiation Is Removed

The historical `let name = Entity { ... }` form is a **hard error**:

```text
entity instantiation with `let` was removed — use
`inst a = A { inputs... }` and read outputs via `a.<port>`
```

### 8.3 Instance Outputs and Auto-Wires

Reading `add0.sum` creates an auto-wire named `add0_sum` in the netlist;
see [Appendix B](#appendix-b-name-mangling-of-flattened-composites) for the
normative naming rules.

## 9. Expressions and Operators

### 9.1 Operator Set

```text
Arithmetic:   +  -  *  /  %        (width-preserving)
Widening add: a +: b               (bit[N] +: bit[N] -> bit[N+1])
Comparison:   ==  !=  <  >  <=  >= (result: bool/bit)
Logical:      &&  ||  !
Bitwise:      &  |  ^  ~  <<  >>
Reduction:    &x (AND-reduce)  ^x (XOR-reduce; parity)   [unary prefix]
Ternary:      cond ? a : b
Cast:         expr as Type
Concatenation: {a, b, c}   or   a ++ b ++ c   (same construct)
Replication:  {N{x}}
Index/slice:  v[i]   v[hi:lo]
Field/method: s.field   x.sqrt()
```

`a ++ b` lowers to exactly the same node as `{a, b}`; use whichever reads
better. `++` binds loosest of all operators.

There is no `**` power operator, no `<<<`/`>>>`, and no `|x` OR-reduction
(write `x != 0`).

### 9.2 Worked Example

```skalp
entity Ops {
    in a: bit[4]
    in b: bit[4]
    in d: bit[8]
    out cat1: bit[8]
    out cat2: bit[8]
    out rep: bit[8]
    out allb: bit
    out par: bit
    out sized: bit[8]
}

impl Ops {
    cat1 = {a, b}          // Concatenation, brace form
    cat2 = a ++ b          // Concatenation, operator form (same hardware)
    rep = {2{a}}           // Replication
    allb = &d              // AND-reduction
    par = ^d               // XOR-reduction (parity)
    sized = 8'hFF          // Sized literal
}
```

### 9.3 If and Match as Expressions

`if`/`else` and `match` are expressions and are the idiomatic mux forms:

```skalp
entity Mux {
    in op: bit[3]
    in a: bit[32]
    in b: bit[32]
    out result: bit[32]
    out zero: bit
}

impl Mux {
    result = match op {
        0b000 => a + b,
        0b001 => a - b,
        0b010 => a & b,
        0b011 => a | b,
        0b100 => a ^ b,
        0b101 => a << b[4:0],
        0b110 => a >> b[4:0],
        0b111 => if a < b { 1 } else { 0 },
        _ => 0
    };

    zero = if result == 0 { 1 } else { 0 };
}
```

Block bodies in expression position yield their final expression:
`match v { 0 => 0, _ => { let (x, y) = split(v); x } }`.

### 9.4 Casts

Conversions are explicit: `true as bit`, `q as bit[32]`, `x as fp32`.
Casting between `bool` and `bit`, and between differently-sized vectors,
requires `as`.

## 10. Pattern Matching

### 10.1 Patterns

| Pattern | Example |
|---------|---------|
| Literal | `0`, `0b101`, `8'hFF`, `true` |
| Wildcard | `_` |
| Enum path | `State::Idle` |
| Tuple-struct | `State::Data(n)` |
| Tuple | `(0, x)` |
| Binding | `n` (binds the scrutinee) |

Arms use `=>` (preferred; `->` is also accepted). Arms may have guards:
`n if n > 3 => ...`.

### 10.2 Exhaustiveness Is Required

Every `match` must cover its scrutinee completely:

- An N-bit scrutinee must cover all 2^N values or include a `_` arm.
- An enum scrutinee must cover every variant or include a `_` arm.
- **Guarded arms do not count toward coverage** — a guard can fail, so an
  arm with `if` cannot complete a match.

A non-exhaustive match is a compile error:

```text
non-exhaustive match on a 2-bit value: 3 of 4 value(s) covered —
add the missing arm(s) or a `_` arm
```

## 11. Loops and Generate Constructs

### 11.1 `for` Loops and `#[unroll]`

`for` iterates over a compile-time range (`0..8` exclusive, `0..=7`
inclusive). The `#[unroll]` attribute expands the loop into parallel
hardware; `#[unroll(N)]` unrolls by factor N:

```skalp
entity Parity {
    in d: bit[8]
    out p: bit
}

impl Parity {
    signal parity: bit = 0

    #[unroll]
    for i in 0..8 {
        parity = parity ^ d[i]
    }

    p = parity
}
```

There is no `while`/`loop`/`break`/`continue` iteration in synthesizable
code.

### 11.2 Generate Constructs

`generate if`, `generate for`, and `generate match` elaborate hardware at
compile time. They are valid at impl level **and inside `on` blocks**.
Conditions, ranges, and scrutinees must be const-evaluable — const generics
and `const` declarations resolve.

```skalp
entity GenDemo<const N: nat = 4> {
    in clk: clock
    in d: bit[8]
    out q: bit[8]
}

impl GenDemo {
    signal acc: bit[8] = 0
    signal comb: bit[8] = 0

    generate if N > 2 {
        comb = d + 1
    } else {
        comb = d
    }

    on(clk.rise) {
        generate for i in 0..N {
            acc = acc + d
        }
    }

    q = comb
}
```

`generate for` supports an optional step: `generate for i in 0..16 step 2`.
Generate bodies may declare per-iteration `signal`s and `const`s.

`generate match` selects an implementation from a const value:

```skalp
entity GenMatch<const MODE: nat = 2> {
    in a: bit[8]
    in b: bit[8]
    out q: bit[8]
}

impl GenMatch {
    signal r: bit[8] = 0

    generate match MODE {
        0 => { r = a }
        1 => { r = b }
        _ => { r = a + b }
    }

    q = r
}
```

**Current restriction:** drive an internal `signal` inside generate bodies
and assign the output port outside (as above). Driving an output port
directly from inside a generate branch is not currently recognized by the
undriven-output check.

## 12. Functions and Tuples

### 12.1 Functions

Functions are defined with `fn` (top-level or inside an impl), take typed
parameters, and may return values with `return`:

```text
fn clamp(x: bit[8], lo: bit[8], hi: bit[8]) -> bit[8] {
    if x < lo { return lo; }
    if x > hi { return hi; }
    return x;
}
```

Functions are combinational descriptions. Small functions are inlined at
call sites; functions whose bodies contain many nested calls are
automatically synthesized as separate modules and instantiated — this is an
implementation detail and does not change semantics. `const fn` declares a
compile-time-evaluable function.

### 12.2 Tuple Returns and Destructuring

Functions may return tuples; `let` destructures them, including at impl
level:

```skalp
entity Split {
    in a: bit[8]
    out hi: bit[4]
    out lo: bit[4]
}

impl Split {
    fn split(x: bit[8]) -> (bit[4], bit[4]) {
        return (x[7:4], x[3:0]);
    }

    let (h, l) = split(a);
    hi = h
    lo = l
}
```

Tuple types annotate bindings: `let t: (bit[32], bit[8]) = (x, y);`.

## 13. Traits and Generics

### 13.1 Trait Definitions

Traits declare interfaces of functions, associated types, and associated
constants:

```text
pub trait Add {
    fn add(a: Self, b: Self) -> Self;
}

pub trait PartialOrd {
    fn lt(a: Self, b: Self) -> bit;
    fn le(a: Self, b: Self) -> bit;
    fn gt(a: Self, b: Self) -> bit;
    fn ge(a: Self, b: Self) -> bit;
}
```

### 13.2 Trait Implementations and Operator Resolution

`impl Trait for Type { ... }` provides an implementation. The standard
library implements the operator traits `Add`, `Sub`, `Mul`, `Div`, `Neg`,
`Sqrt`, `Abs`, `PartialOrd`, `PartialEq` for the floating-point types; this
is what makes `a + b` work on `fp32` operands (Section 14). Trait method
bodies may instantiate entities — the stdlib's `Add for fp32` instantiates
the `FpAdd<IEEE754_32>` entity.

### 13.3 Generic Entities with Trait Bounds

Type parameters may carry trait bounds, checked at monomorphization:

```text
entity Accum<T: Add, const N: nat> {
    in xs: [T; N]
    out sum: T
}
```

Trait-based generic programming is primarily exercised through the standard
library's numeric layer; user-defined trait hierarchies beyond that pattern
are supported but less traveled.

## 14. Numeric Types and the Standard Library

### 14.1 Floating Point

Floating-point support comes from the standard library, not from keywords:

```skalp
use skalp::numeric::fp::*;

entity FpDemo {
    in a: fp32
    in b: fp32
    out s: fp32
    out d: fp32
    out m: fp32
}

impl FpDemo {
    s = a + b
    d = a - b
    m = a * b
}
```

- `fp16`, `fp32`, `fp64` are stdlib types (IEEE 754 half/single/double).
- The generic type is `fp<F>` where `F` is a `FloatFormat` constant
  (`IEEE754_16`, `IEEE754_32`, `IEEE754_64`); `fp32` is `fp<IEEE754_32>`.
- Operators `+ - * / == != < <= > >=` and unary `-` resolve through the
  operator traits to hardware entities (`FpAdd`, `FpMul`, ...).
- Square root is the `.sqrt()` method (`Sqrt` trait); absolute value is
  `.abs()`:

```text
fn fp_sqrt(x: fp32) -> fp32 {
    return x.sqrt()
}
```

- Float literals may be cast to fp types: `let four: fp32 = 4.0 as fp32;`.

The stdlib fp entities take an `intent` generic parameter and select among
implementations (fast / small / balanced) based on it; `DEFAULT_INTENT`
applies when unspecified.

### 14.2 Other Stdlib Components

`crates/skalp-stdlib` also provides reusable components (adders,
multipliers, barrel shifters, shift registers, bit operations) as ordinary
SKALP entities, plus `clog2` and similar const helpers usable in width
expressions. Consult the stdlib sources for the current inventory; the
stdlib is compiled with every build (set `SKALP_STDLIB_PATH` when running a
non-installed compiler).

## 15. The Intent System

Design intent influences *how* logic is implemented without changing *what*
it computes.

### 15.1 What Is Implemented

The implemented intent properties are:

| Property | Values | Effect |
|----------|--------|--------|
| `mux_style` | `parallel`, `priority` | Mux tree shape for match/select logic |
| `pipeline_style` | implementation-defined set | Pipelining strategy |
| `impl_style` | `auto`, `parallel`, `tree`, `sequential` | Operator implementation shape |

plus the `#[unroll]` / `#[unroll(N)]` loop attribute (Section 11.1).

Anything beyond this — intent as a first-class type, hierarchical intent
propagation, intent profiles, latency/area/power constraint solving — is
**future work** (Section 21). Unknown intent property names parse but have
no synthesis effect.

### 15.2 Intent Declarations

```text
intent fast = mux_style::parallel;                  // Single-line form
intent low_power { mux_style: priority }            // Block form
intent fast_decode = fast + critical;               // Composition
```

### 15.3 Applying Intents

`with intent::name` attaches an intent to a `let`-bound expression, an
entity declaration, or a block of impl items:

```skalp
intent fast = mux_style::parallel;

entity IntentDemo {
    in s: bit[2]
    in a: bit[8]
    in b: bit[8]
    out q: bit[8]
}

impl IntentDemo {
    let tmp = match s {
        0 => a,
        1 => b,
        _ => 0
    } with intent::fast;

    q = tmp
}
```

On an entity header:

```text
entity Decoder with intent::fast { ... }
```

Intents compose with `+`: `expr with intent::fast + intent::critical`.
An intent declaration may also be referenced as an attribute
(`#[fast] entity Decoder { ... }`).

### 15.4 Intent Generic Parameters

Entities may declare `intent I: Intent = DEFAULT_INTENT` generic
parameters. The standard library uses these to select among
implementations of the same operation (e.g. fast vs. small square root).
This mechanism is currently a stdlib-facing feature; the queryable-intent
surface available to user code is limited to what Section 15.1 lists.

## 16. Attributes

Attributes use Rust-like `#[...]` syntax and may precede entities, ports,
signals, functions, and `for` loops. Attributes with defined semantics:

| Attribute | Where | Effect |
|-----------|-------|--------|
| `#[unroll]`, `#[unroll(N)]` | `for` loops | Full / partial loop unrolling (Section 11.1) |
| `#[cdc(...)]` | signals | CDC verification annotation; no hardware (Section 4.3) |
| `#[safety_mechanism(...)]` | entities | Marks a safety mechanism for FMEA/FMEDA (Section 20) |
| `#[implements(GoalPath)]` | entities | Links a mechanism to a safety goal/requirement |
| `#[power_domain(name)]` | entities | Binds the entity's instance subtree to a declared power domain; checked reference when any `power_domain` declarations exist (Section 18) |
| `#[power_domain(name, allow_shared_supply)]` | entities | Same binding; downgrades the dependent-failure error to a documented warning (Section 18.4) |
| `#[detection_signal]` | output ports | Marks a fault-detection output for diagnostic-coverage measurement |
| `#[intent_name]` | entities, functions | Applies a declared intent (Section 15.3) |

The compiler accepts other attribute names (they parse and are carried as
annotations), but only the attributes above have specified behavior. Do not
rely on undocumented attributes for synthesis results.

## 17. Physical Constraints

Pin and I/O constraints attach inline to ports with `@ { ... }`:

```skalp
entity Blink {
    in clk: clock @ { pin: "A1", io_standard: "LVCMOS33" }
    out led: bit @ { pin: "B2", io_standard: "LVCMOS33" }
}

impl Blink {
    signal c: bit = 0
    on(clk.rise) { c = !c }
    led = c
}
```

Recognized constraint keys include `pin`, `pins` (array form for buses),
`pin_p`/`pin_n` (differential pairs), `io_standard`, `pull`, `slew`,
`drive`, `schmitt`, `diff_term`, and `bank`. Constraints flow into the
iCE40 synthesis and place-and-route flow (`skalp synth`).

A global `constraint physical { ... }` block (device selection, bank
voltages, `io_defaults`, floorplan regions) is parsed at top level.

There is no CLI flag for merging external constraint files (PCF/XDC);
inline constraints are the mechanism.

## 18. Power Domains

Power domains are modeled **in-language**: the supply tree is declared at
top level, entities bind to domains by attribute, and the compiler checks
the result on every `skalp build` — the ISO 26262 dependent-failure
(supply-independence) check and a domain-crossing check run on the
instance tree, and IEEE 1801 (UPF) power intent is emitted from the
checked model. UPF is an *export backend* of the in-compiler model; the
native flow consumes the model directly and never re-imports the UPF.

See [Section 4.4](#44-design-decision-domain-lifetimes-are-clock-only) for
why power domains bind structurally via attributes instead of the clock
lifetime syntax, and [Section 21.1](#211-power-domains-remaining-future-work)
for the parts of the recorded design that are **not** yet implemented
(control-cone checks, pin-level supply compatibility, port-granular
isolation/level-shifter strategies, `#[retention]` semantics, power-fault
injection, the PST legality table, and the FPGA leg).

### 18.1 Supply-Tree Declarations

A top-level `power_domain` declaration names a rail and records **how it
is derived** — independence for the safety checks is a property of the
supply tree, so naming a domain is not enough:

```text
power_domain NAME : external [, states = { ... }] ;
power_domain NAME = regulated( PARENT [, macro = IDENT]
                               [, states = { ... }] ) ;
power_domain NAME = switched( PARENT [, on_when = [!] PATH]
                              [, ack_on = [!] PATH]
                              [, states = { ... }] ) ;
```

- `external` — a chip supply port. Regulation is the board/PMIC's concern
  and out of scope.
- `regulated(parent, macro = ...)` — a new voltage level produced by an
  on-die analog block (LDO, buck). The regulator is a **black-box
  hard-macro instance** referenced by name; the language never models its
  analog behavior.
- `switched(parent, on_when = ..., ack_on = ...)` — the same voltage,
  gated by power-switch cells. `on_when` (switch enable) and `ack_on`
  (acknowledge) each take an **optionally-negated hierarchical path**
  (`!pmu_gpu_sleep`, `pmu.gpu_ack`): active-low polarity is expressed by
  the `!`, never by a separate flag.

`states = { ... }` lists the domain's supply states: a state is a name
with an optional voltage, e.g. `on: 0.9V`, `ret: 600mV`, `off`. Voltages
accept both volt (`0.9V`) and millivolt (`900mV`) forms and are stored in
millivolts. `on` is accepted as a state name even though it is a keyword
elsewhere.

`external`, `regulated`, `switched`, `states`, `macro`, `on_when`, and
`ack_on` are **contextual identifiers, not keywords**
([Section 2.2](#22-contextual-keywords)): `in external: bit` and
`signal states: bit[4]` remain legal everywhere else.

Declarations are validated at build time. Each of the following is a hard
error: a duplicate domain name, an unknown parent domain, a cycle in the
supply tree, and a duplicate state name within a domain.

### 18.2 Binding by Containment

`#[power_domain(name)]` on an entity binds that entity — and its **whole
instance subtree** — to the named domain. A child entity carrying its own
`#[power_domain]` attribute rebinds its subtree; everything else
inherits. (Rebinding an individual *instance* at the `inst` site is not
yet implemented — see Section 21.1.)

When any `power_domain` declarations exist in the design, the attribute
argument is a **checked reference**: naming an undeclared domain fails
the build with a fix-it:

```text
error: entity `Foo`: #[power_domain(vdd_cor)] references an undeclared
power domain — declare it with `power_domain vdd_cor: ...;`
```

The legacy string form `#[power_domain("name")]` still parses and is
checked against the declarations in exactly the same way. In a design
with **no** `power_domain` declarations, both forms degrade to the legacy
annotation-only behavior: no checking, no UPF emission.

`#[power_domain(name, allow_shared_supply)]` is the justified
shared-supply escape hatch for the dependent-failure check
(Section 18.4).

### 18.3 A Complete Example

The following design (see `examples/power_domains.sk`) declares a supply
tree and binds a safety mechanism to an independent rail:

```skalp
// power_domains.sk
power_domain vreg_main: external;
power_domain vdd_core = regulated(vreg_main, macro = u_ldo_core,
                                  states = { on: 0.9V, ret: 0.6V, off });
power_domain vdd_gpu  = switched(vdd_core, on_when = !pmu_gpu_sleep,
                                 ack_on = pmu_gpu_ack,
                                 states = { on: 0.9V, off });
power_domain vdd_mon: external;

#[power_domain(vdd_mon)]
#[safety_mechanism(type = watchdog)]
entity Watchdog {
    in clk: clock
    in kick: bit
    out timeout: bit
}

impl Watchdog {
    signal cnt: bit[8] = 0
    on(clk.rise) {
        if kick {
            cnt = 0
        } else {
            cnt = cnt + 1
        }
    }
    timeout = cnt == 255
}

#[power_domain(vdd_core)]
entity Controller {
    in clk: clock
    in kick: bit
    out wd_timeout: bit
}

impl Controller {
    inst wd = Watchdog { clk: clk, kick: kick }
    wd_timeout = wd.timeout
}
```

`Watchdog` is bound to `vdd_mon` (a separate supply path), so the
dependent-failure check passes. The `vdd_core` → `vdd_mon` instantiation
edge draws the domain-crossing warning of Section 18.5 unless one side
declares an `#[isolation]` signal.

### 18.4 The Dependent-Failure (CCF) Check

ISO 26262 dependent-failure analysis requires a safety mechanism to be
*supply-independent* of the element it monitors — a mechanism sharing a
rail with its monitored function dies with it (common-cause failure).
Because the supply tree is in-language, this check runs in the build
pipeline instead of degenerating to manual UPF review.

**Independence is ancestry**: two domains are independent iff their
supply-ancestor chains are disjoint. Two rails regulated off one
`vreg_main` are **not** independent of each other — an LDO filters its
parent, it does not survive it — and derivation kind does not affect
independence.

The check: a `#[safety_mechanism]` entity whose effective domain shares a
supply ancestor with its instantiating context's domain **fails the
build**. Binding `Watchdog` above to a domain under `vreg_main` produces:

```text
Error: Failed to compile HIR to MIR with CDC analysis: power-domain
dependent-failure check failed with 1 error(s):
  safety mechanism `Watchdog` (instance `wd` in `Controller`) is in power
  domain `vdd_periph`, which shares a supply ancestor with its context's
  domain `vdd_core` — not supply-independent from the logic it monitors
  (common-cause failure) — bind it to an independent supply, or justify
  with #[power_domain(vdd_periph, allow_shared_supply)]
```

Where the standard permits a justified shared supply,
`#[power_domain(name, allow_shared_supply)]` downgrades the error to a
documented warning:

```text
PDC warning: safety mechanism `Watchdog` (instance `wd` in `Controller`)
is in power domain `vdd_periph`, which shares a supply ancestor with its
context's domain `vdd_core` — not supply-independent from the logic it
monitors (common-cause failure) [downgraded: allow_shared_supply]
```

### 18.5 The Domain-Crossing Warning

A net crossing an instantiation edge between two different domains needs
an isolation strategy at the boundary. The current check is **coarse**:
if neither the instantiating entity nor the instantiated entity declares
any `#[isolation]` signal, the build emits a warning (it never fails the
build):

```text
PDC warning: nets cross power domains `vdd_core` -> `vdd_mon` (instance
`wd` of `Watchdog` in `Controller`) with no #[isolation] strategy
declared on either side
```

Declaring an `#[isolation(...)]` signal on either side of the edge
records that the crossing is handled and suppresses the warning.
Port-granular isolation/level-shifter strategies and inference are future
work (Section 21.1).

### 18.6 UPF Emission

Whenever `power_domain` declarations exist, `skalp build` writes
`design.upf` next to `design.sv` (reported as `📄 Power intent: ...`).
The file contains, per declaration and binding:

- `create_supply_port` / `create_supply_net` (ports only for `external`
  rails; a `regulated` net is annotated with its driving macro instance),
- `create_power_domain` with `-elements` listing the **bound instance
  roots** only — containment makes listing the subtree redundant,
- `create_supply_set` per domain,
- `create_power_switch` for each `switched` domain, with `on_state` /
  `off_state` Boolean expressions derived from `on_when` and an
  `-ack_port` from `ack_on`,
- `add_power_state` per declared supply state (voltages from the
  declaration, `OFF` for voltage-less `off` states).

For the example above, the switch and state-table portion is:

```text
create_power_switch SW_vdd_gpu -domain PD_vdd_gpu\
    -input_supply_port {sw_in VDD_CORE}\
    -output_supply_port {sw_out VDD_GPU}\
    -control_port {sw_ctrl pmu_gpu_sleep}\
    -on_state {on_s sw_in {!pmu_gpu_sleep}}\
    -off_state {off_s {pmu_gpu_sleep}}\
    -ack_port {sw_ack pmu_gpu_ack {pmu_gpu_ack}}

add_power_state SS_vdd_core -state on {-supply_expr {power == `{FULL_ON, 0.90}`}}
add_power_state SS_vdd_core -state ret {-supply_expr {power == `{FULL_ON, 0.60}`}}
add_power_state SS_vdd_core -state off {-supply_expr {power == `{OFF}`}}
```

The emitted UPF carries a header marking it generated; the checked
in-language model is authoritative and the file must not be hand-edited.

### 18.7 FPGA Targets: Bank/VCCIO Check and Power Stubbing

Commodity FPGA fabric has no user-partitionable power islands — VCCINT is
one grid. The FPGA leg of the power model therefore consists of the two
things that ARE real on FPGAs:

**Bank/VCCIO compatibility.** Each I/O bank has its own rail. When a
`constraint physical` block declares bank voltages and a port names a bank,
the port's `io_standard` must match the bank's rail — checked at compile
time for every build:

```skalp
constraint physical {
    bank 0 { voltage: 1.8, io_standard: "LVCMOS18" }
    bank 1 { voltage: 3.3, io_standard: "LVCMOS33" }
}

entity Blink {
    in clk: clock @ { pin: "A1", io_standard: "LVCMOS18", bank: 0 }
    out led: bit @ { pin: "B2", io_standard: "LVCMOS33", bank: 1 }
}

impl Blink {
    signal c: bit = 0
    on(clk.rise) { c = !c }
    led = c
}
```

An `LVCMOS33` pin on the 1.8 V bank fails the build:

```text
bank/VCCIO compatibility check failed with 1 error(s):
  port `clk` of `Blink`: io_standard LVCMOS33 requires a 3.3 V rail, but bank 0 declares 1.8 V (VCCIO mismatch)
```

Known standards cover the LVCMOS/LVTTL/SSTL/HSTL/LVDS families; unknown
standards are skipped rather than flagged.

**Power stubbing for ASIC prototyping.** `switched(...)` and
`regulated(...)` domains are unimplementable in fabric. `skalp synth` on an
FPGA device refuses such designs by default:

```text
Error: design declares power domains that FPGA fabric cannot implement:
`vdd_core` (regulated), `vdd_gpu` (switched). ... re-run with --power-stub
```

`--power-stub` requests prototyping semantics, and every stubbed element is
reported — never silent:

```text
⚡ FPGA power-stub report (ASIC power intent prototyped as always-on):
   `vdd_core`: regulator `u_ldo_core` on `vreg_main` NOT instantiated — rail assumed externally supplied at its `on` voltage
   `vdd_gpu`: power switch from `vdd_core` STUBBED — always-on; no isolation clamps, no retention loss, off/retention states unreachable
   NOTE: all fabric logic shares VCCINT — safety mechanisms on this FPGA are NOT supply-independent from what they monitor, regardless of declared domains.
```

The NOTE is the dependent-failure reality on FPGAs: a same-fabric safety
mechanism can claim other independence classes (timing, design diversity),
but never supply independence. External-only supply trees need no stubbing.

### 18.8 Control-Cone Checks for Switched Domains

The `on_when` / `ack_on` paths of a `switched(...)` domain are resolved
against the hierarchy root: the first path segment names an instance in
the top entity (the control's domain is that instance's effective domain)
or a signal of the top itself. Two checks run on the resolved domain:

- **No-self-power (error).** The control/ack driver must not live in the
  switched domain or any of its descendants — a domain cannot switch its
  own supply back on:

  ```text
  power_domain `vdd_gpu`: on_when control `!pmu.gpu_sleep` is driven from domain `vdd_gpu`, which is `vdd_gpu` itself or a descendant — a domain cannot switch its own supply (no-self-power)
  ```

- **Controller liveness (warning, simplified).** The controller should be
  always-on. If the resolved domain is itself switched or declares an
  `off` state, the build warns that the controller may be down exactly
  when the target needs switching, and recommends an always-on domain.
  (Per-state PST-liveness analysis is future work — Section 21.1.)

A control path that does not resolve to an instance or signal of the top
entity produces a warning: the cone cannot be checked, but the design may
resolve at a different top.

### 18.9 The System Power State Table

A `power_states { ... }` block declares the LEGAL cross-domain state
combinations — the power state table (PST):

```text
power_states {
    run   = { vdd_aon: on, vdd_core: on,  vdd_gpu: on },
    idle  = { vdd_aon: on, vdd_core: on,  vdd_gpu: off },
    sleep = { vdd_aon: on, vdd_core: ret, vdd_gpu: off },
};
```

Validation (build errors): referenced domains and states must be declared;
every domain that declares states must be assigned in **every** system
state; duplicate system-state names are rejected. Two semantic checks run
on a declared PST:

- **Ancestry legality.** In every system state, a powered domain (any
  state but `off`) must not have a supply ancestor assigned `off`:

  ```text
  system power state `sleep`: domain `vdd_gpu` is `on` while its supply ancestor `vdd_core` is `off` — a rail cannot be up while its source is down
  ```

- **Full PST-liveness.** With a PST declared, the simplified controller
  warning of Section 18.8 is replaced by a precise, build-failing check:
  a switched domain's resolved controller domain must be `on` in every
  declared system state (no transition graph is modeled, so the
  conservative rule applies — a switch may need to operate entering or
  leaving any state):

  ```text
  power_domain `vdd_gpu`: on_when control `!pmu.gpu_sleep` is driven from domain `vdd_core`, which is `ret` in system power state `sleep` — the controller must be on in every declared state (PST-liveness)
  ```

The table is exported to UPF as a legacy PST alongside the per-domain
`add_power_state` entries:

```text
create_pst SYSTEM_PST -supplies {VDD_AON VDD_CORE VDD_GPU}
add_pst_state run -pst SYSTEM_PST -state {on on on}
add_pst_state sleep -pst SYSTEM_PST -state {on ret off}
```

## 19. Asynchronous (NCL) Entities

`async entity` declares a clockless Null Convention Logic circuit. Logic is
synthesized to dual-rail encoding with threshold gates and is
delay-insensitive.

```skalp
async entity NclStage {
    in a: bit[8]
    in b: bit[8]
    out y: bit[8]
}

impl NclStage {
    let s1 = a & b

    barrier      // Pipeline stage boundary with completion detection

    y = s1 | a
}
```

- `barrier` marks an NCL pipeline stage boundary; completion-detection
  logic is inserted there.
- `on()` with an empty trigger list is permitted inside async entities.
- NCL synthesis maps operations to threshold-gate networks (e.g. `a & b` →
  TH22/TH12 pairs, `~a` → rail swap); arithmetic lowers to NCL adder
  structures. See `examples/ncl/` and NCL_ASYNC_CIRCUITS.md.
- Async timing analysis runs during build (disable with `--no-async-sta`).

## 20. Safety and Verification

### 20.1 Safety Annotations and FMEA

Safety mechanisms are ordinary entities marked with attributes; the safety
flow (`skalp build --safety`, `skalp safety`) uses them for fault-injection
FMEA/FMEDA and diagnostic-coverage measurement:

```skalp
#[safety_mechanism(type=tmr)]
entity TmrVoter {
    in a: bit[8]
    in b: bit[8]
    in c: bit[8]
    out voted: bit[8]
    #[detection_signal]
    out fault_detected: bit
}

impl TmrVoter {
    signal ab: bit = if a == b { 1 } else { 0 }
    signal bc: bit = if b == c { 1 } else { 0 }
    signal ac: bit = if a == c { 1 } else { 0 }

    voted = if ab { a } else if bc { b } else { a }
    fault_detected = if ab && bc && ac { 0 } else { 1 }
}
```

`#[implements(SG001::TmrVoting)]` links a mechanism to a declared safety
goal. `requirement NAME { key: value, ... }` blocks declare requirements
and safety goals; see `examples/safety/` for complete, compiling designs
and the ISO 26262 work-product generation options on `skalp build`.

### 20.2 Assertions

Immediate assertions compile in designs and are checked in simulation /
formal flows:

```skalp
entity Checked {
    in clk: clock
    in v: bit
    in r: bit
    out q: bit
}

impl Checked {
    signal s: bit = 0
    on(clk.rise) { s = v && r }
    q = s

    assert(!(v && !r), "protocol violation");
}
```

`assume(...)` and `cover(...)` take the same form.

### 20.3 Temporal and Formal Layer

The parser accepts an SVA-style temporal layer — `property`, `sequence`,
`##N` delays, `[*N]` repetition, `|->` / `|=>` implication, `covergroup` /
`coverpoint` / `bins`, and `formal { invariant ... prove ... }` blocks —
used for SVA generation and the SAT-based formal backend (`skalp ec`,
bounded model checking). The synthesizable subset of the language is
unaffected by these constructs. Consult `docs/` verification guides and
`tests/` for currently supported forms before relying on a specific
temporal idiom.

### 20.4 Testbenches

Testbenches are written in **Rust** against the `skalp-testing` `Testbench`
API (two-language approach): SKALP for synthesizable hardware, Rust
async/await for stimulus and checking, with CPU or GPU (Metal) simulation.
See `examples/testbench_guide/`. There is no SKALP-language `#[testbench]`
construct.

## 21. Future / Not Implemented

The following appear in older drafts of this specification or remain
reserved in the grammar, but are **not implemented**. Using them is either
a hard error or has no effect. They must not appear in designs.

| Construct | Status |
|-----------|--------|
| `stream<T>` ports | Reserved keyword; **hard compile error** (Section 3.7) |
| `<=` / `:=` assignment operators | Removed; `=` only. `<=` is comparison (Section 6.4) |
| `let x = Entity { ... }` instantiation | Removed; **hard error** — use `inst` (Section 8.2) |
| `protocol` definitions (`.master`/`.slave`, direction arrows, `~Protocol` flipping) | Keyword reserved; no protocol semantics. Declare explicit ports |
| `flow { }` pipeline blocks and the `|>` operator | Parsed; no dataflow synthesis. Write explicit stages |
| Intent as a first-class type; intent propagation, profiles, and constraint solving | Future work; implemented subset is Section 15.1 |
| Clock frequency types (`clock<100MHz>`), `clock_group`, derived/adaptive clocks | Not implemented (lifetime frequency *bounds* parse; see Section 4.2) |
| Inline timing constraint blocks (`with timing`, `path(...)`, `timing_budget`) | Not implemented |
| External constraint file merging (`--constraints board.pcf`) | No such CLI option; use inline constraints (Section 17) |
| `process` / `always` blocks, sensitivity lists | Never part of this dialect; use `on(event)` |
| `ncl<N>` explicit dual-rail types | Not implemented; `async entity` handles encoding (Section 19) |
| Memory/debug attribute semantics (`#[memory]`, `#[trace]`, `#[breakpoint]`, vendor-IP attributes) | Parse-accepted annotations only; no defined synthesis behavior |
| `#[retention]` / `#[isolation]` semantics | Parse-accepted; `#[retention]` emits synthesis attributes (`RETAIN`/`DONT_TOUCH`) and `#[isolation]` presence feeds the coarse domain-crossing check (Section 18.5), but retention sequencing, isolation-cell insertion, and port-granular strategies are future (Section 21.1) |
| `while` loops in synthesizable code | Not synthesizable; use bounded `for` |
| Entity aliases (`entity Fast = Foo::<N>;`) | Parsed and stored, but an alias cannot yet be instantiated (`unknown entity in instantiation`) |
| Octal literals (`0o52`) | Not lexed; use decimal/hex/binary |

### 21.1 Power Domains: Remaining Future Work

The core of the recorded power-domain design (decision 2026-08-07) is now
implemented and specified in [Section 18](#18-power-domains): supply-tree
declarations with derivation kinds and states, checked containment
binding, the dependent-failure (CCF) check with `allow_shared_supply`,
the coarse domain-crossing warning, and UPF emission from the checked
model. The following recorded pieces remain **future work** and must not
be relied on:

1. *Deep control-path resolution.* No-self-power, controller liveness,
   and full per-state PST-liveness are implemented (Sections 18.8,
   18.9). Still future: resolving control paths deeper than the top
   entity's immediate instances, and modeling state TRANSITIONS (the
   liveness rule is conservatively per-state, not per-edge).

2. *Pin-level supply compatibility.* Comparing a control driver's domain
   voltage against each switch/macro pin's related supply (Liberty
   `related_power_pin`) in every shared power state. Dependency: the
   `.sklib` technology-library format does not yet carry per-pin
   related-supply data.

3. *Port-granular isolation and level-shifter strategies and their
   inference.* The implemented crossing check is coarse
   (Section 18.5): it only tests for the *presence* of `#[isolation]`
   signals on either side of an edge. Per-port strategies, clamp-value
   checking, and level-shifter insertion driven by the PST are future.

4. *`#[retention]` semantics* — retention strategy selection,
   save/restore sequencing, and PST-driven retention inference. Today
   the attribute emits synthesis attributes (`RETAIN`, `DONT_TOUCH`) on
   the register and nothing more.

5. *Power-fault injection classes* — whole-domain loss, switch
   stuck-off/stuck-on, regulator output collapse, overvoltage —
   evidencing the FMEDA's independence claim beyond per-gate stuck-ats.

6. *PST transition modeling.* The legality layer itself is implemented
   (Section 18.9: `power_states` blocks, ancestry legality, precise
   liveness, `create_pst` export). Still future: a transition graph
   between system states (which transitions are legal, sequencing
   order), which would sharpen liveness to per-edge analysis.

7. *Instance-level rebinding.* Binding is per-entity; rebinding one
   `inst` of an entity to a different domain is not implemented.

8. *FPGA bank/domain linkage.* The bank/VCCIO compatibility check and
   `--power-stub` prototyping report are implemented (Section 18.7).
   Still future: naming a declared power domain as a bank's rail (today
   bank voltages are literals), and modeling device supply trees
   (VCCINT as an implicit external domain for fabric logic).

**Vocabulary note.** `intent` declarations (Section 15) are *advisory*
to synthesis. `#[cdc]`, the safety attributes, and power-domain binding
are *checked constraints* that can fail the build. Both use the same
attribute surface syntax, but they belong to different enforcement
classes, and documentation should not conflate them.

## Appendix A: Operator Precedence

From loosest to tightest binding:

| Level | Operators | Associativity |
|-------|-----------|---------------|
| 1 | `++` (concatenation) | Left |
| 2 | `? :` (ternary) | Right |
| 3 | `\|\|` | Left |
| 4 | `&&` | Left |
| 5 | `==` `!=` | Left |
| 6 | `<` `<=` `>` `>=` (comparisons) | Left |
| 7 | `\|` | Left |
| 8 | `^` | Left |
| 9 | `&` | Left |
| 10 | `<<` `>>` | Left |
| 11 | `+` `-` `+:` | Left |
| 12 | `*` `/` `%` | Left |
| 13 | unary `!` `~` `-` `&` `^` | Right |
| 14 | `as` (cast) | — |
| 15 | call `()`, index/slice `[]`, field/method `.` | Left |

A postfix `with intent::name` may follow a complete expression (Section 15.3).

## Appendix B: Name Mangling of Flattened Composites

Struct-typed ports and signals are flattened to per-field scalars in the
generated netlist. The mangling rules (normative — testbenches and external
tooling may rely on them):

- **Struct fields** join with a DOUBLE underscore: `insn.opcode` becomes
  `insn__opcode`; nested fields chain (`vertex.pos.x` → `vertex__pos__x`).
  The double underscore keeps the mapping unambiguous even when field or
  signal names themselves contain underscores.
- **Instance output auto-wires** are named `{instance}_{port}` (single
  underscore): `inst pwm = Pwm {...}` with output `counter` creates
  `pwm_counter`. If that name collides with an existing port, signal, or
  variable, the compiler renames the auto-wire to `{instance}__{port}`
  (all internal references follow the rename).
- A struct-typed instance output combines both rules:
  `geometry.output`'s field `x` lives on the wire `geometry_output__x`.

---

*End of SKALP Language Specification*

*संकल्पना - From conception to silicon*
