# SKALP User Documentation

Welcome to SKALP! This documentation will help you get started, learn the language, and build real hardware designs.

---

## 🚀 Getting Started (5 minutes)

**New to SKALP?** Start here:
- [**Quick Start**](quick-start.md) - Install and compile your first design in 5 minutes
- [**Getting Started**](../getting-started.md) - Installation, first design, IDE setup
- [**Language Tutorial**](../tutorial.md) - SKALP concepts chapter by chapter

---

## 📖 Reference Manual

Quick lookup for language features:

- [**Syntax Reference**](reference/syntax.md) - All language constructs
- [**Attributes Reference**](reference/attributes.md) - `#[...]` attributes: debug, CDC, power, memory, synthesis hints
- [**CLI Commands**](reference/cli.md) - All `skalp` commands

**Use this when:** You know what you want to do, just need the syntax.

---

## 👨‍🍳 Cookbook (Design Patterns)

Common hardware patterns with copy-paste code:

- [**Combinational Patterns**](cookbook/combinational.md) - Muxes, encoders, decoders
- [**Sequential Patterns**](cookbook/sequential.md) - Counters, shift registers
- [**Memory Patterns**](cookbook/memories.md) - RAMs, ROMs, FIFOs
- [**Hierarchical Design**](cookbook/hierarchical.md) - Entity instantiation and composition

**Use this when:** You need a proven pattern for a common problem.

---

## 🔄 Migration Guides

Already know another HDL? Learn SKALP quickly:

- [**From SystemVerilog**](migration/from-systemverilog.md) - Translate SV to SKALP

**Use this when:** You want to translate existing knowledge to SKALP.

---

## 📘 How-To Guides

Practical guides for specific tasks:

- [**Writing Testbenches**](guides/testbench.md) - Test your designs with the Rust testbench API
- [**Debug and Simulation**](guides/debug-simulation.md) - Breakpoints, signal tracing, waveforms
- [**Clock Domain Crossing**](guides/clock-domain-crossing.md) - CDC analysis and synchronizer patterns
- [**Memory Synthesis**](guides/memory-synthesis.md) - Inferring RAMs and ROMs
- [**Power Intent**](guides/power-intent.md) - Retention, isolation, and power domains
- [**Linter**](guides/linter.md) - Static design checks

**Use this when:** You have a specific task to accomplish.

---

## 💡 Complete Examples

Real-world designs you can learn from live in the repository's
[`examples/`](../../examples/) directory, and the standard library sources in
[`crates/skalp-stdlib/`](../../crates/skalp-stdlib/) are themselves idiomatic
SKALP (adders, multipliers, comparators, FIFOs, and more).

---

## 🎯 Quick Links by Use Case

**"I want to evaluate SKALP"**
→ [Quick Start (5 min)](quick-start.md)

**"I'm learning hardware design"**
→ [Language Tutorial](../tutorial.md)

**"I know SystemVerilog"**
→ [Migration Guide](migration/from-systemverilog.md)

**"I need to build a FIFO"**
→ [Cookbook: Memory Patterns](cookbook/memories.md)

**"How do I test my design?"**
→ [Guide: Writing Testbenches](guides/testbench.md)

**"What's the syntax for X?"**
→ [Syntax Reference](reference/syntax.md)

---

## 🆚 Why SKALP?

**Key advantages:**
- ✅ **30-50% less code** - More concise syntax
- ✅ **Type safety** - Catch errors at compile time
- ✅ **Built-in CDC analysis** - Every build checks clock domain crossings
- ✅ **Modern syntax** - Rust-like, expression-based
- ✅ **Integrated testing** - Fast testbench development in Rust
- ✅ **Clean output** - Readable SystemVerilog generation
- ✅ **Formal equivalence checking** - `skalp ec` verifies RTL against gates

---

## 📊 Documentation Map

```
docs/
├── getting-started.md          ⭐ INSTALL & FIRST DESIGN
├── tutorial.md                 📚 LANGUAGE TUTORIAL
│
└── user/
    ├── quick-start.md          ⭐ 5-MINUTE START
    │
    ├── reference/              📖 QUICK LOOKUP
    │   ├── syntax.md
    │   ├── attributes.md
    │   └── cli.md
    │
    ├── cookbook/               👨‍🍳 DESIGN PATTERNS
    │   ├── combinational.md
    │   ├── sequential.md
    │   ├── memories.md
    │   └── hierarchical.md
    │
    ├── migration/              🔄 FROM OTHER HDLS
    │   └── from-systemverilog.md
    │
    └── guides/                 📘 HOW-TO GUIDES
        ├── testbench.md
        ├── debug-simulation.md
        ├── clock-domain-crossing.md
        ├── memory-synthesis.md
        ├── power-intent.md
        └── linter.md
```

---

## 🤝 Get Help

- **Found a bug?** - [Report an issue](https://github.com/skalp-lang/skalp/issues)
- **Have a question?** - [Discussions](https://github.com/skalp-lang/skalp/discussions)

---

**Ready to start?** → [Quick Start Guide](quick-start.md)
