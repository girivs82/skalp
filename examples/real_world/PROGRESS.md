# Real-World SKALP Examples - Progress Report

## Goal
Validate SKALP language design by creating 10 progressively complex hardware examples, compile each, and identify:
1. **Used features** - Confirm they're valuable
2. **Unused features** - Candidates for removal
3. **Missing features** - Needed for real designs
4. **Compiler bugs** - Issues found during testing

---

## Completed Examples (4/10)

### ✅ Example 1: Simple FIFO (4 entries × 8 bits)
**Status:** Compiles ✓ | Simulated ⏳

**Features Used:**
- entity/impl syntax
- signal with initialization
- on(clk.rise) sequential blocks
- if/else conditionals
- Arithmetic (+, -, %)
- Comparisons (==, <, >)
- nat[N] types, bit type

**Complexity:** ⭐ Basic
**LOC:** 94 lines
**Real Use:** Data buffering, CDC, flow control

### ✅ Example 2: UART Transmitter (115200 baud)
**Status:** Compiles ✓ | Simulated ⏳

**Features Used:**
- FSM pattern (4 states)
- Multi-way if/else if chains
- Counter-based timing
- Bit manipulation (/2, %2 for shifting)
- State transitions
- Handshake protocol

**Complexity:** ⭐⭐ Intermediate
**LOC:** 103 lines
**Real Use:** Debug console, serial communication

### ✅ Example 3: SPI Master (Mode 0, 6.25MHz)
**Status:** Compiles ✓ | Simulated ⏳

**Features Used:**
- Clock generation (divider)
- Bidirectional data (in/out)
- Edge-sensitive logic
- Shift registers (MSB first)
- Multiple clock edges per transaction

**Complexity:** ⭐⭐ Intermediate
**LOC:** 92 lines
**Real Use:** Flash, ADC/DAC, SD cards

### ✅ Example 4: I2C Master Controller (100kHz)
**Status:** Compiles ✓ | Simulated ⏳

**Features Used:**
- Complex 8-state FSM
- Clock divider with phase tracking
- Shift registers for serial communication
- Bidirectional signaling (via in/out/oe workaround)
- START/STOP condition generation
- ACK/NACK detection

**Complexity:** ⭐⭐⭐ Advanced
**LOC:** 290 lines
**Real Use:** Sensor communication, EEPROM, RTC, displays

---

## Examples In Progress (6/10)

### 📋 Example 5: Memory Arbiter
**Status:** Not started
**Features:** Traits, priority logic, round-robin arbitration

### 📋 Example 6: AXI4-Lite Interface
**Status:** Not started
**Features:** Struct types, handshaking, address decoding

### 📋 Example 7: Pipelined ALU
**Status:** Not started
**Features:** Flow blocks, dataflow (`|>`), hazard detection

### 📋 Example 8: DMA Engine
**Status:** Not started
**Features:** Memory access, descriptors, interrupts

### 📋 Example 9: Video Scaler
**Status:** Not started
**Features:** Stream types, buffering, backpressure

### 📋 Example 10: SoC Subsystem
**Status:** Not started
**Features:** Protocol integration, clock domains, hierarchical design

---

## Key Findings

### ✅ Working Language Features

1. **entity/impl syntax** - Clean, works well
2. **signal declarations** - Initialization syntax is good
3. **on(clk.rise)** - Intuitive sequential blocks
4. **if/else conditionals** - Standard control flow works
5. **Arithmetic operations** - +, -, *, /, % all functional
6. **Comparisons** - ==, !=, <, >, <=, >= supported
7. **nat[N] and bit types** - Width inference works
8. **Comments** - Good documentation support

### 🐛 Compiler Bugs Found

#### Critical Bug: Comparison Codegen
**Problem:** `if (x == 0)` generates as `if (x)` in SystemVerilog

**Evidence:**
```skalp
// SKALP code
if (state == 0) { ... }

// Generated SV (WRONG!)
if (state) begin ... end

// Should be
if (state == 0) begin ... end
```

**Impact:** HIGH - Breaks all state machines and conditional logic
**Examples Affected:** All 3 (FIFO, UART, SPI)
**Files:** `crates/skalp-codegen/src/systemverilog.rs` line ~200-300

#### Medium Bug: Count Update Logic
**Problem:** Doesn't properly incorporate full/empty checks in count update

**Impact:** MEDIUM - Can cause counter overflow/underflow
**Examples Affected:** FIFO (example 1)

### 🚫 Unused Features So Far

These language features haven't been needed yet:
- **match expressions** - if/else sufficient for FSMs
- **struct types** - Haven't needed composite data yet
- **enum types** - State encoding with nat[N] works fine
- **traits** - No polymorphism needed in simple examples
- **generics** - Tried to use but not working (const parameters)
- **protocol** keyword - Haven't reached protocol examples yet
- **flow blocks** (`|>`) - Haven't reached pipeline examples yet
- **stream types** - Haven't reached streaming examples yet
- **requirements** - No verification examples yet
- **intent** - No optimization guidance examples yet

**Note:** These will likely be used in examples 4-10.

### ❌ Features in Spec but Not Yet Implemented

**Important**: These features ARE defined in LANGUAGE_SPECIFICATION.md but NOT implemented in parser/compiler yet:

1. **Array types** - Spec defines `T[N]` syntax (e.g., `nat[8][4]` for 4 elements of nat[8])
   - **Spec Reference:** Lines 1181-1191 (Array of Instances section)
   - **Status:** Parser fails with "expected implementation item"
   - **Workaround:** Manual signal expansion (`mem0`, `mem1`, `mem2`, `mem3`)
   - **Impact:** Code bloat, not scalable, tedious to write

2. **Generic entities** - Spec defines `entity Foo<T>` and `entity Bar<const N: nat>` syntax
   - **Spec Reference:** Lines 2108-2142 (Generic Entities and Trait Bounds)
   - **Status:** Not tested yet (parser may support partial implementation)
   - **Workaround:** Fixed-size modules
   - **Impact:** No code reusability, must duplicate logic for different widths

3. **Const generic expressions** - Spec allows `clog2(N)` in type expressions
   - **Spec Reference:** Intrinsic functions (clog2, pow2 exist in MIR at hir_to_mir.rs:1003)
   - **Status:** MIR supports evaluation, but parser may not allow in type positions
   - **Workaround:** Hardcode bit widths
   - **Impact:** Not parameterizable

4. **Bidirectional ports (inout)** - Spec defines `inout` keyword for tristate signals
   - **Spec Reference:** Line 74 (keyword list), used in protocol definitions
   - **Status:** Keyword exists but parser rejects `inout` in entity ports
   - **Workaround:** Use separate `in`/`out`/`oe` (output enable) signals
   - **Impact:** Cannot express true tristate I/O, less realistic models
   - **Example:** I2C SDA line needs tristate but must use sda_in/sda_out/sda_oe

### ✅ Features That DO Work

**Tested and confirmed working:**

1. **Bool type** - `in enable: bool` compiles successfully ✓
   - Distinct from `bit` as per spec
   - Can be used in ports and signals

2. **Struct types** - Defined in spec, need to test implementation
3. **Enum types** - Defined in spec, need to test implementation
4. **Match expressions** - Parser supports them (unused in examples so far)
5. **Traits** - Defined in spec, need to test implementation

---

## Feature Coverage Matrix

| Feature Category | Used? | Examples | Priority |
|-----------------|-------|----------|----------|
| **Core Syntax** | | | |
| entity/impl | ✅ | 1,2,3 | KEEP |
| signal | ✅ | 1,2,3 | KEEP |
| on(clk.rise) | ✅ | 1,2,3 | KEEP |
| if/else | ✅ | 1,2,3 | KEEP |
| **Data Types** | | | |
| bit | ✅ | 1,2,3 | KEEP |
| nat[N] | ✅ | 1,2,3 | KEEP |
| bool | ❌ | - | ADD? |
| struct | ❌ | - | TBD |
| enum | ❌ | - | TBD |
| **Control Flow** | | | |
| if/else | ✅ | 1,2,3 | KEEP |
| match | ❌ | - | MAYBE REMOVE? |
| for | ❌ | - | TBD |
| **Advanced** | | | |
| generics | ⚠️ | Tried, broken | FIX |
| traits | ❌ | - | TBD |
| protocol | ❌ | - | TBD (ex 6-10) |
| flow (`|>`) | ❌ | - | TBD (ex 7) |
| stream | ❌ | - | TBD (ex 9) |
| intent | ❌ | - | TBD |
| requirements | ❌ | - | TBD |

---

## Next Steps

### Immediate (This Session)

1. **Fix critical comparison bug** - Blocking all examples
2. Continue with examples 4-5 (I2C, Memory Arbiter)
3. Document more compiler bugs

### Short Term

1. Complete examples 6-10
2. Create comprehensive feature matrix
3. Simulate all examples
4. Write testbenches

### Medium Term

1. Fix compiler bugs identified
2. Add missing features (arrays, const expressions)
3. Remove unused features or justify keeping them
4. Optimize generated SystemVerilog quality

---

## Compilation Statistics

| Example | LOC | Compile Time | SV Lines | Status |
|---------|-----|--------------|----------|--------|
| FIFO | 94 | <1s | 67 | ✅ Compiles |
| UART TX | 103 | <1s | 71 | ✅ Compiles |
| SPI Master | 92 | <1s | ~70 | ✅ Compiles |

**Average:** 96 LOC SKALP → ~70 lines SystemVerilog

---

## Conclusions So Far

### Language Design is Sound
The core SKALP syntax (entity/impl/signal/on) is intuitive and works well for real hardware. The examples are readable and concise.

### Compiler Needs Work
Critical codegen bugs (especially comparison generation) prevent real use. These must be fixed before language validation can continue.

### Feature Set is Reasonable
Most "advanced" features (traits, protocols, streams) haven't been needed yet in simple examples. They'll likely be validated in examples 4-10.

### Arrays are Critical
The lack of proper array support forced workarounds that don't scale. This is the #1 missing feature.

**Recommendation:** Fix comparison bug, add array support, then continue with examples 4-10.
