# SKALP User Documentation

Welcome to SKALP! This documentation will help you get started, learn the language, and build real hardware designs.

---

## 🚀 Getting Started (5 minutes)

**New to SKALP?** Start here:
- [**Quick Start**](quick-start.md) - Install and compile your first design in 5 minutes

---

## 📚 Learning SKALP

### Tutorials (Step-by-Step)
Progressive tutorials that teach you SKALP from the ground up:

1. [**Your First Design**](tutorial/01-first-design.md) - Counter, basic syntax
2. [**Combinational Logic**](tutorial/02-combinational.md) - Adders, muxes, ALUs
3. [**Sequential Logic**](tutorial/03-sequential.md) - Registers, FIFOs, shift registers
4. [**State Machines**](tutorial/04-state-machines.md) - FSMs, UART, protocols
5. [**Types and Structures**](tutorial/05-types.md) - Structs, enums, arrays
6. [**Testing Your Designs**](tutorial/06-testing.md) - Testbench API, simulation
7. [**Multi-Clock Designs**](tutorial/07-multi-clock.md) - CDC, async FIFOs
8. [**Verification**](tutorial/08-verification.md) - Formal verification (future)

**Recommended path:** Read tutorials 1-6 in order.

---

## 📖 Reference Manual

Quick lookup for language features:

- [**Syntax Reference**](reference/syntax.md) - All language constructs
- [**Type System**](reference/types.md) - bit, nat, structs, enums, arrays
- [**Operators**](reference/operators.md) - All operators with precedence
- [**Built-in Functions**](reference/builtins.md) - clog2, width inference
- [**CLI Commands**](reference/cli.md) - All `skalp` commands

**Use this when:** You know what you want to do, just need the syntax.

---

## 👨‍🍳 Cookbook (Design Patterns)

Common hardware patterns with copy-paste code:

- [**Combinational Patterns**](cookbook/combinational.md) - Muxes, encoders, decoders
- [**Sequential Patterns**](cookbook/sequential.md) - Counters, shift registers
- [**State Machine Patterns**](cookbook/state-machines.md) - Moore, Mealy, one-hot
- [**Memory Patterns**](cookbook/memories.md) - RAMs, ROMs, FIFOs
- [**CDC Patterns**](cookbook/cdc.md) - Synchronizers, gray code, handshake
- [**Protocol Patterns**](cookbook/protocols.md) - UART, SPI, I2C, AXI
- [**Arithmetic Patterns**](cookbook/arithmetic.md) - Adders, multipliers, pipelines
- [**Testing Patterns**](cookbook/testing.md) - Testbench recipes

**Use this when:** You need a proven pattern for a common problem.

---

## 🔄 Migration Guides

Already know another HDL? Learn SKALP quickly:

- [**From SystemVerilog**](migration/from-systemverilog.md) - Translate SV to SKALP
- [**From VHDL**](migration/from-vhdl.md) - VHDL → SKALP patterns
- [**From Chisel**](migration/from-chisel.md) - Chisel → SKALP guide
- [**Syntax Comparison Table**](migration/comparison-table.md) - Side-by-side

**Use this when:** You want to translate existing knowledge to SKALP.

---

## 📘 How-To Guides

Practical guides for specific tasks:

- [**Writing Testbenches**](guides/testbench.md) - Test your designs
- [**Running Simulations**](guides/simulation.md) - Simulate with GPU/CPU
- [**Synthesizing Designs**](guides/synthesis.md) - FPGA/ASIC synthesis
- [**Debugging**](guides/debugging.md) - Find and fix errors
- [**Performance Optimization**](guides/performance.md) - Make designs faster/smaller
- [**Troubleshooting**](guides/troubleshooting.md) - Common errors and fixes

**Use this when:** You have a specific task to accomplish.

---

## 💡 Complete Examples

Real-world designs you can learn from:

### Basic Examples
- [Counter](examples/basic/counter/) - Simple up-counter
- [Adder](examples/basic/adder/) - Multi-bit adder
- [Multiplexer](examples/basic/mux/) - N-way mux
- [Register](examples/basic/register/) - Loadable register

### Intermediate Examples
- [FIFO](examples/intermediate/fifo/) - Circular buffer with full/empty detection
- [UART](examples/intermediate/uart/) - Serial transmitter/receiver
- [SPI Master](examples/intermediate/spi/) - SPI controller
- [ALU](examples/intermediate/alu/) - Arithmetic logic unit

### Advanced Examples
- [Async FIFO](examples/advanced/async-fifo/) - Clock domain crossing FIFO
- [AXI4-Lite](examples/advanced/axi4-lite/) - Bus interface
- [DDR Controller](examples/advanced/ddr-controller/) - Memory controller
- [Pipelined CPU](examples/advanced/pipelined-cpu/) - Simple processor

**All examples include:**
- ✅ Complete SKALP source
- ✅ README with explanation
- ✅ Testbench (where applicable)
- ✅ Generated SystemVerilog

---

## 🎯 Quick Links by Use Case

**"I want to evaluate SKALP"**
→ [Quick Start (5 min)](quick-start.md)

**"I'm learning hardware design"**
→ [Tutorial: Your First Design](tutorial/01-first-design.md)

**"I know SystemVerilog"**
→ [Migration Guide](migration/from-systemverilog.md)

**"I need to build a FIFO"**
→ [Cookbook: Memory Patterns](cookbook/memories.md)

**"How do I test my design?"**
→ [Guide: Writing Testbenches](guides/testbench.md)

**"I'm getting a compile error"**
→ [Troubleshooting Guide](guides/troubleshooting.md)

**"I want to see real examples"**
→ [Examples Directory](examples/README.md)

**"What's the syntax for X?"**
→ [Syntax Reference](reference/syntax.md)

---

## 🆚 Why SKALP?

**Coming from SystemVerilog?**
- Read: [Why SKALP vs SystemVerilog](../../comparison/why-skalp.md)
- Read: [SKALP vs Veryl Comparison](../../comparison/skalp-vs-veryl.md)

**Key advantages:**
- ✅ **30-50% less code** - More concise syntax
- ✅ **Type safety** - Catch errors at compile time
- ✅ **Built-in CDC analysis** - No external tools needed
- ✅ **Modern syntax** - Rust-like, expression-based
- ✅ **Integrated testing** - Fast testbench development
- ✅ **Clean output** - Readable SystemVerilog generation

---

## 📊 Documentation Map

```
docs/user/
├── quick-start.md              ⭐ START HERE
│
├── tutorial/                   📚 LEARN STEP-BY-STEP
│   ├── 01-first-design.md
│   ├── 02-combinational.md
│   ├── 03-sequential.md
│   ├── 04-state-machines.md
│   ├── 05-types.md
│   ├── 06-testing.md
│   ├── 07-multi-clock.md
│   └── 08-verification.md
│
├── reference/                  📖 QUICK LOOKUP
│   ├── syntax.md
│   ├── types.md
│   ├── operators.md
│   ├── builtins.md
│   └── cli.md
│
├── cookbook/                   👨‍🍳 DESIGN PATTERNS
│   ├── combinational.md
│   ├── sequential.md
│   ├── state-machines.md
│   ├── memories.md
│   ├── cdc.md
│   ├── protocols.md
│   └── arithmetic.md
│
├── migration/                  🔄 FROM OTHER HDLS
│   ├── from-systemverilog.md
│   ├── from-vhdl.md
│   └── comparison-table.md
│
├── guides/                     📘 HOW-TO GUIDES
│   ├── testbench.md
│   ├── simulation.md
│   ├── synthesis.md
│   ├── debugging.md
│   └── troubleshooting.md
│
└── examples/                   💡 COMPLETE DESIGNS
    ├── basic/
    ├── intermediate/
    └── advanced/
```

---

## 🤝 Get Help

- **Found a bug?** - [Report an issue](https://github.com/skalp-lang/skalp/issues)
- **Have a question?** - [Discussions](https://github.com/skalp-lang/skalp/discussions)
- **Want to contribute?** - See [Developer Docs](../../developer/contributing/setup.md)

---

**Ready to start?** → [Quick Start Guide](quick-start.md)
