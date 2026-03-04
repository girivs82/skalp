# SKALP Documentation

**Complete documentation for the SKALP Hardware Description Language**

---

## 🚀 New to SKALP?

**Start here:** [5-Minute Quick Start →](user/quick-start.md)

Or dive into the [User Documentation Hub →](user/README.md)

---

## 📚 Documentation Sections

### 👥 User Documentation
**For hardware designers using SKALP**

- [**Quick Start**](user/quick-start.md) - Get started in 5 minutes
- [**Tutorials**](user/tutorial/) - Step-by-step learning path
- [**Reference Manual**](user/reference/) - Complete syntax and API reference
- [**Cookbook**](user/cookbook/) - Design patterns and recipes
- [**Migration Guides**](user/migration/) - Coming from SystemVerilog/VHDL/Chisel
- [**How-To Guides**](user/guides/) - Testbenches, simulation, synthesis
- [**Examples**](user/examples/) - Complete working designs

→ [**Browse User Docs**](user/README.md)

---

### 💻 Developer Documentation
**For contributors extending SKALP**

- [**Architecture**](developer/architecture/) - Compiler internals
- [**API Reference**](developer/api/) - Programmatic interfaces
- [**Contributing**](developer/contributing/) - Development workflow

---

### 🆚 Comparisons
**Understanding SKALP's position in the HDL landscape**

- [**Why SKALP?**](WHY_SKALP.md) - SKALP vs SystemVerilog
- [**SKALP vs Veryl**](SKALP_VS_VERYL.md) - Feature comparison with Veryl HDL
- [**Feature Matrix**](comparison/feature-matrix.md) - Compare all HDLs (coming soon)

---

### 📖 Formal Specifications
**Language specification and formal definitions**

- [**Language Specification**](LANGUAGE_SPECIFICATION.md) - Complete formal spec
- [**Grammar**](GRAMMAR.ebnf) - EBNF grammar
- [**Type System**](specification/type-system.md) - Formal type system (coming soon)

---

## 🎯 Quick Links by Role

### "I'm evaluating SKALP"
1. [5-Minute Quick Start](user/quick-start.md)
2. [Why SKALP vs SystemVerilog](WHY_SKALP.md)
3. [SKALP vs Veryl Comparison](SKALP_VS_VERYL.md)

### "I'm learning SKALP"
1. [Quick Start](user/quick-start.md)
2. [Tutorial: First Design](user/tutorial/01-first-design.md)
3. [Examples](user/examples/)

### "I know SystemVerilog"
1. [Quick Start](user/quick-start.md)
2. [Migration Guide: SystemVerilog → SKALP](user/migration/from-systemverilog.md)
3. [Syntax Comparison](user/migration/comparison-table.md)

### "I'm building a specific design"
1. [Cookbook](user/cookbook/) - Design patterns
2. [Examples](user/examples/) - Working code
3. [Reference](user/reference/) - Syntax lookup

### "I'm testing my design"
1. [Testbench Guide](user/guides/testbench.md)
2. [Simulation Guide](user/guides/simulation.md)
3. [Testing Patterns](user/cookbook/testing.md)

### "I have existing VHDL designs"
1. [VHDL Frontend Architecture](vhdl-frontend-architecture.md)
2. [Migration Guide: VHDL → SKALP](user/migration/from-vhdl.md)
3. [Quick Start](user/quick-start.md) — `skalp build design.vhd` works out of the box

### "I'm contributing to SKALP"
1. [Developer Setup](developer/contributing/setup.md)
2. [Architecture Overview](developer/architecture/overview.md)
3. [Contributing Guidelines](developer/contributing/workflow.md)

---

## 📂 Documentation Structure

```
docs/
├── README.md                          ← YOU ARE HERE
│
├── user/                              👥 USER DOCUMENTATION
│   ├── README.md                      Documentation hub
│   ├── quick-start.md                 5-minute getting started
│   ├── tutorial/                      Step-by-step tutorials
│   ├── reference/                     Syntax & API reference
│   ├── cookbook/                      Design patterns
│   ├── migration/                     From other HDLs
│   ├── guides/                        How-to guides
│   └── examples/                      Complete examples
│
├── developer/                         💻 DEVELOPER DOCUMENTATION
│   ├── architecture/                  Compiler internals
│   │   ├── overview.md
│   │   ├── frontend.md                HIR (High-level IR)
│   │   ├── mir.md                     MIR (Mid-level IR)
│   │   ├── sir.md                     SIR (Structural IR)
│   │   ├── codegen.md                 Code generation
│   │   ├── simulation.md              GPU simulation
│   │   └── verification.md            CDC & formal
│   ├── api/                           API documentation
│   └── contributing/                  Contribution guides
│
├── comparison/                        🆚 COMPETITIVE ANALYSIS
│   ├── why-skalp.md                   → WHY_SKALP.md
│   ├── skalp-vs-veryl.md              → SKALP_VS_VERYL.md
│   └── feature-matrix.md              (coming soon)
│
└── specification/                     📖 FORMAL SPECS
    ├── language-spec.md               → LANGUAGE_SPECIFICATION.md
    ├── grammar.ebnf                   → GRAMMAR.ebnf
    └── type-system.md                 (coming soon)
```

---

## 🔬 Advanced Features

### Asynchronous Circuits (NCL)

SKALP supports **Null Convention Logic (NCL)** for clockless asynchronous circuit design:

- [**NCL Async Circuits**](NCL_ASYNC_CIRCUITS.md) - Complete NCL documentation
- [**NCL Examples**](../examples/ncl/) - Working async circuit examples

**Key features:**
- `async entity` - Declare clockless modules
- `barrier` - Pipeline stage boundaries with completion detection
- Dual-rail encoding with THmn threshold gates
- Wavefront-based simulation

```skalp
async entity NclAdder {
    in a: bit[8]
    in b: bit[8]
    out sum: bit[8]
}

impl NclAdder {
    sum = a + b  // Automatically synthesized to NCL gates
}
```

### VHDL Frontend

SKALP accepts **VHDL-2008/2019** as a first-class input language, compiling through the same backend pipeline:

- [**VHDL Frontend Architecture**](vhdl-frontend-architecture.md) - Complete VHDL frontend documentation

**Key features:**
- Full VHDL-2008/2019 lexer and parser (synthesizable subset)
- VHDL-2019 interfaces and mode views
- Generic type parameters and generic package instantiation
- Synthesizable subset enforcement at parse time

```bash
# Compile VHDL directly
skalp build counter.vhd
```

### Other Advanced Features

- [**Clock Domain Crossing**](CDC_SUPPORT.md) - Safe multi-clock designs
- [**Safety Analysis**](implementation/AUTOMATED_SAFETY_ANALYSIS.md) - ISO 26262 FMEDA
- [**Pipeline Annotations**](PIPELINE_ANNOTATIONS.md) - Retiming and optimization
- [**GPU Simulation**](GPU_SIMULATION.md) - Hardware-accelerated simulation

---

## 📊 Documentation Status

### ✅ Complete
- Quick Start Guide
- SystemVerilog Migration Guide
- VHDL Frontend Architecture
- VHDL Migration Guide
- User Documentation Hub
- Why SKALP comparison
- SKALP vs Veryl comparison
- Language Specification
- EBNF Grammar
- Architecture documentation

### 🚧 In Progress
- Tutorial series
- Reference manual
- Cookbook
- CLI reference
- Examples with tests

### 📋 Planned
- Chisel migration guide
- Advanced patterns
- Video tutorials
- Interactive examples

---

## 🤝 Contributing to Documentation

Documentation improvements are welcome! See [Contributing Guide](developer/contributing/setup.md).

**Documentation principles:**
- ✅ Example-driven - Show, don't just tell
- ✅ Copy-pasteable - All examples must compile
- ✅ Progressive - Simple to complex
- ✅ Practical - Real-world use cases
- ✅ Honest - Acknowledge limitations

---

## 🔗 External Resources

- **Repository:** https://github.com/skalp-lang/skalp
- **Issues:** https://github.com/skalp-lang/skalp/issues
- **Discussions:** https://github.com/skalp-lang/skalp/discussions

---

**Ready to start?** → [Quick Start Guide](user/quick-start.md)
