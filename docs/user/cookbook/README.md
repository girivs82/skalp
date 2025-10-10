# SKALP Design Patterns Cookbook

**Proven hardware design patterns with copy-paste SKALP code.**

This cookbook provides ready-to-use solutions for common hardware design problems. Each pattern includes:
- **Problem description** - What you're trying to solve
- **Complete working code** - Copy-paste ready
- **Explanation** - How it works
- **Use cases** - When to use it

---

## 📚 Available Pattern Collections

### ⚡ [Combinational Logic](combinational.md)
Pure combinational circuits (no registers).

**Patterns:**
- Multiplexers (2-to-1, 4-to-1, N-to-1)
- Encoders and Decoders
- Priority Encoders
- Adders (half, full, ripple-carry)
- Incrementers and Subtractors
- Comparators (equality, magnitude)
- Bit Counters (population count)
- Leading Zero Counter
- Bit Reversal
- Parity Generators
- Barrel Shifters
- Min/Max Circuits
- Gray Code Converters

**Total:** 20+ patterns

---

### 🔄 [Sequential Logic](sequential.md)
Registered logic (flip-flops, state).

**Patterns:**
- Registers (basic, with enable, with load)
- Counters (up, down, up/down, modulo, with load)
- Shift Registers (left, right, parallel load, LFSR)
- Edge Detectors (rising, falling, any)
- Debouncers
- Pulse Generators (one-shot, width)
- Delay Lines
- Synchronizers (2-FF, 3-FF)
- Pipeline Registers
- Accumulators (basic, with saturation)

**Total:** 20+ patterns

---

### 💾 [Memory Patterns](memories.md)
RAMs, ROMs, FIFOs, and memory controllers.

**Patterns:**
- Single-Port RAM (basic, with byte enable)
- Dual-Port RAM (true dual, simple dual)
- ROM (synchronous, asynchronous)
- Synchronous FIFO (basic, with thresholds)
- Asynchronous FIFO (CDC-safe with Gray code)
- Register Files (basic, RISC-style with R0=0)
- Content-Addressable Memory (CAM)

**Total:** 12+ patterns

---

### 🎯 State Machines (Coming Soon)
Finite state machine patterns.

**Planned Patterns:**
- Moore FSM
- Mealy FSM
- One-Hot Encoding
- Binary Encoding
- Gray Code Encoding
- State Machine with Outputs
- Timeout Handling
- Multi-State Protocols

---

### 🔌 Protocol Patterns (Coming Soon)
Communication protocol implementations.

**Planned Patterns:**
- UART (TX, RX)
- SPI (Master, Slave)
- I2C (Master, Slave)
- AXI4-Lite (Master, Slave)
- Wishbone
- Handshake Protocols

---

### 🌉 CDC Patterns (Coming Soon)
Clock domain crossing patterns.

**Planned Patterns:**
- Single-Bit Synchronizer
- Multi-Bit Handshake
- Pulse Synchronizer
- Gray Code FIFO
- MCP FIFO
- Domain Crossing Register

---

### ➗ Arithmetic Patterns (Coming Soon)
Mathematical operations.

**Planned Patterns:**
- Multipliers (combinational, pipelined)
- Dividers (restoring, non-restoring)
- Square Root
- CORDIC
- Fixed-Point Math
- Saturating Arithmetic

---

### 🧪 Testing Patterns (Coming Soon)
Testbench and verification patterns.

**Planned Patterns:**
- Basic Testbench Template
- Table-Driven Tests
- Self-Checking Testbenches
- Coverage-Driven Tests
- Constrained Random
- Bus Functional Models

---

## 🎯 Quick Pattern Finder

### "I need to..."

**"...select between inputs"**
→ [Multiplexers](combinational.md#multiplexers)

**"...count events"**
→ [Counters](sequential.md#counters)

**"...store data temporarily"**
→ [FIFOs](memories.md#fifos)

**"...buffer between clock domains"**
→ [Async FIFO](memories.md#asynchronous-fifo-cdc-safe)

**"...implement a state machine"**
→ State Machines (coming soon)

**"...detect a button press"**
→ [Debouncer](sequential.md#debouncer)

**"...shift data serially"**
→ [Shift Registers](sequential.md#shift-registers)

**"...compare two values"**
→ [Comparators](combinational.md#comparators)

**"...add two numbers"**
→ [Adders](combinational.md#adders-and-arithmetic)

**"...store array of values"**
→ [RAMs](memories.md#single-port-ram)

**"...lookup table"**
→ [ROM](memories.md#rom)

**"...delay a signal"**
→ [Delay Lines](sequential.md#delay-lines)

**"...cross clock domains safely"**
→ [Synchronizers](sequential.md#synchronizers)

**"...generate random numbers"**
→ [LFSR](sequential.md#lfsr-linear-feedback-shift-register)

**"...encode/decode values"**
→ [Encoders and Decoders](combinational.md#encoders-and-decoders)

---

## 📖 How to Use This Cookbook

### 1. Find Your Pattern
- Browse by category (combinational, sequential, memory)
- Use the Quick Pattern Finder above
- Search within each category page

### 2. Copy the Code
All patterns are complete and ready to use:
```skalp
// Just copy and paste!
entity Counter<const WIDTH: nat = 8> {
    in clk: clock
    in rst: reset
    out count: bit[WIDTH]
}
// ... implementation
```

### 3. Customize for Your Needs
- Change generic parameters (`WIDTH`, `DEPTH`, etc.)
- Add/remove features
- Combine multiple patterns

### 4. Test Your Design
See [Testbench Guide](../guides/testbench.md) for testing patterns.

---

## 🎓 Learning Path

**Beginner:**
1. Start with [Combinational Patterns](combinational.md)
2. Learn basic [Sequential Patterns](sequential.md) (registers, counters)
3. Try simple [Memory Patterns](memories.md) (ROM, single-port RAM)

**Intermediate:**
1. Study [Shift Registers](sequential.md#shift-registers)
2. Implement [FIFOs](memories.md#fifos)
3. Learn [State Machines](state-machines.md) (coming soon)

**Advanced:**
1. Master [Async FIFOs](memories.md#asynchronous-fifo-cdc-safe)
2. Implement [Protocol Patterns](protocols.md) (coming soon)
3. Use [CDC Patterns](cdc.md) (coming soon)

---

## 💡 Design Tips

### Combinational Logic
- Use `=` for assignments
- Keep logic depth reasonable
- Use `match` for multi-way logic
- Pipeline long combinational paths

### Sequential Logic
- Always use `<=` in `on(clk.rise)` blocks
- Always include reset handling
- Use enable signals for conditional updates
- Consider power-of-2 for modulo counters

### Memory Patterns
- Use `clog2()` for address width
- Initialize in reset or declaration
- Be careful with read-after-write behavior
- Consider registered vs combinational outputs

### Clock Domain Crossing
- **Never** cross multi-bit signals directly
- Use Gray code for counters/pointers
- Always use 2-FF (or 3-FF) synchronizers
- Test CDC thoroughly

---

## 🔗 Related Documentation

- [Syntax Reference](../reference/syntax.md) - Language syntax
- [Testbench Guide](../guides/testbench.md) - How to test designs
- [Examples](../examples/) - Complete working designs
- [Tutorial](../tutorial/) - Step-by-step learning

---

## 📊 Pattern Statistics

| Category | Patterns | Status |
|----------|----------|--------|
| Combinational | 20+ | ✅ Complete |
| Sequential | 20+ | ✅ Complete |
| Memories | 12+ | ✅ Complete |
| State Machines | 8+ | 🚧 Coming Soon |
| Protocols | 10+ | 🚧 Coming Soon |
| CDC | 6+ | 🚧 Coming Soon |
| Arithmetic | 8+ | 🚧 Coming Soon |
| Testing | 6+ | 🚧 Coming Soon |
| **Total** | **90+** | **40% Complete** |

---

## 🤝 Contributing Patterns

Have a useful pattern to share? We welcome contributions!

**Pattern Template:**
```markdown
### Pattern Name

**Problem:** Brief description of what problem this solves.

**Solution:**
\`\`\`skalp
// Complete, working code
entity MyPattern { ... }
impl MyPattern { ... }
\`\`\`

**Use cases:** When to use this pattern.
```

See [Contributing Guide](../../developer/contributing/workflow.md) for details.

---

## 🎯 Popular Patterns (Most Used)

1. [Counter with Enable](sequential.md#counter-with-enable)
2. [Synchronous FIFO](memories.md#synchronous-fifo)
3. [2-to-1 Mux](combinational.md#2-to-1-multiplexer)
4. [Single-Port RAM](memories.md#basic-single-port-ram)
5. [Register with Enable](sequential.md#register-with-enable)
6. [Edge Detector](sequential.md#rising-edge-detector)
7. [4-to-1 Mux](combinational.md#4-to-1-multiplexer)
8. [Async FIFO](memories.md#asynchronous-fifo-cdc-safe)
9. [Magnitude Comparator](combinational.md#magnitude-comparator)
10. [2-FF Synchronizer](sequential.md#2-ff-synchronizer)

---

**Happy designing! 🚀**

Browse patterns: [Combinational](combinational.md) | [Sequential](sequential.md) | [Memories](memories.md)
