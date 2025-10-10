# SKALP Documentation Improvement Plan

**Goal:** Create world-class documentation that makes SKALP accessible to both hardware engineers and software developers.

---

## Current State Assessment

### ✅ Strengths
1. **Excellent architecture docs** - 160KB+ of compiler internals
2. **Strong comparison docs** - WHY_SKALP.md and SKALP_VS_VERYL.md
3. **Real-world examples** - 8 working designs with READMEs
4. **Language spec** - 62KB formal specification

### ❌ Weaknesses
1. **No quick start** - Takes too long to get first result
2. **Outdated tutorials** - References unimplemented features
3. **Missing testbench guide** - Users can't test their designs
4. **No reference manual** - Hard to look up syntax
5. **Architecture mixed with user docs** - Confusing organization
6. **No migration guides** - Hard for SV/VHDL users to learn
7. **Examples lack tests** - Can't verify they work

---

## Target Audiences

### 1. **Quick Evaluators** (5 minutes)
*"I want to see if SKALP is worth my time"*
- **Needs:** One-page quick start, side-by-side comparison, running example
- **Success:** Compiles and simulates counter in 5 minutes

### 2. **New Users** (1-2 hours)
*"I'm learning SKALP from scratch"*
- **Needs:** Tutorial, examples, reference docs
- **Success:** Can write FIFO, ALU, simple state machine

### 3. **SystemVerilog Migrants** (30 minutes)
*"I know Verilog/VHDL, teach me SKALP idioms"*
- **Needs:** Migration guide, comparison table, pattern translation
- **Success:** Can translate existing SV design to SKALP

### 4. **Advanced Users** (ongoing)
- **Needs:** Reference manual, cookbook, advanced patterns
- **Success:** Can implement complex designs (CDC, protocols, verification)

### 5. **Contributors** (developers extending SKALP)
- **Needs:** Architecture docs, API reference, compiler internals
- **Success:** Can add features, fix bugs, write backends

---

## Documentation Structure (New Organization)

```
docs/
├── README.md                          # Documentation hub with clear navigation
│
├── user/                              # USER-FACING DOCUMENTATION
│   ├── quick-start.md                 # 5-minute getting started
│   ├── installation.md                # Detailed installation guide
│   ├── tutorial/                      # Step-by-step tutorials
│   │   ├── 01-first-design.md         # Hello World counter
│   │   ├── 02-combinational.md        # ALU, adders
│   │   ├── 03-sequential.md           # Registers, FIFOs
│   │   ├── 04-state-machines.md       # FSMs (UART, SPI)
│   │   ├── 05-types.md                # Structs, enums, arrays
│   │   ├── 06-testing.md              # Testbench API, simulation
│   │   ├── 07-multi-clock.md          # CDC, async FIFOs
│   │   └── 08-verification.md         # Formal verification (future)
│   │
│   ├── reference/                     # REFERENCE MANUAL
│   │   ├── syntax.md                  # Complete syntax reference
│   │   ├── types.md                   # Type system reference
│   │   ├── operators.md               # All operators with examples
│   │   ├── builtins.md                # Built-in functions (clog2, etc)
│   │   ├── attributes.md              # Attributes and annotations
│   │   └── cli.md                     # CLI command reference
│   │
│   ├── cookbook/                      # DESIGN PATTERNS & RECIPES
│   │   ├── README.md                  # Index of patterns
│   │   ├── combinational.md           # Muxes, decoders, ALUs
│   │   ├── sequential.md              # Counters, shift registers
│   │   ├── state-machines.md          # FSM patterns
│   │   ├── memories.md                # RAMs, ROMs, FIFOs
│   │   ├── cdc.md                     # Clock domain crossing patterns
│   │   ├── protocols.md               # SPI, I2C, UART, AXI
│   │   ├── arithmetic.md              # Adders, multipliers, pipelines
│   │   └── testing.md                 # Testbench patterns
│   │
│   ├── migration/                     # MIGRATION GUIDES
│   │   ├── from-systemverilog.md      # SV → SKALP translation
│   │   ├── from-vhdl.md               # VHDL → SKALP translation
│   │   ├── from-chisel.md             # Chisel → SKALP translation
│   │   └── comparison-table.md        # Side-by-side syntax comparison
│   │
│   ├── guides/                        # HOW-TO GUIDES
│   │   ├── testbench.md               # Writing testbenches
│   │   ├── simulation.md              # Running simulations
│   │   ├── synthesis.md               # Synthesizing to FPGA/ASIC
│   │   ├── debugging.md               # Debugging techniques
│   │   ├── performance.md             # Optimization tips
│   │   └── troubleshooting.md         # Common errors and fixes
│   │
│   └── examples/                      # COMPLETE EXAMPLES
│       ├── README.md                  # Index of examples
│       ├── basic/                     # Simple examples
│       │   ├── counter/
│       │   ├── adder/
│       │   ├── mux/
│       │   └── register/
│       ├── intermediate/              # Medium complexity
│       │   ├── fifo/
│       │   ├── uart/
│       │   ├── spi/
│       │   └── alu/
│       └── advanced/                  # Complex designs
│           ├── async-fifo/
│           ├── axi4-lite/
│           ├── ddr-controller/
│           └── pipelined-cpu/
│
├── developer/                         # DEVELOPER DOCUMENTATION
│   ├── architecture/                  # Compiler internals
│   │   ├── overview.md
│   │   ├── frontend.md                # Lexer, parser, HIR
│   │   ├── mir.md                     # Mid-level IR
│   │   ├── sir.md                     # Structural IR
│   │   ├── lir.md                     # Low-level IR
│   │   ├── codegen.md                 # Code generation
│   │   ├── simulation.md              # GPU simulation
│   │   └── verification.md            # CDC analysis, formal
│   │
│   ├── api/                           # API DOCUMENTATION
│   │   ├── testbench-api.md           # Testbench Rust API
│   │   ├── compiler-api.md            # Programmatic compilation
│   │   └── plugin-api.md              # Extending SKALP
│   │
│   └── contributing/                  # CONTRIBUTION GUIDES
│       ├── setup.md                   # Dev environment setup
│       ├── workflow.md                # Git workflow, CI
│       ├── testing.md                 # Writing tests
│       ├── adding-features.md         # How to add language features
│       └── style-guide.md             # Code style
│
├── comparison/                        # COMPETITIVE ANALYSIS
│   ├── why-skalp.md                   # Why SKALP vs SystemVerilog
│   ├── skalp-vs-veryl.md              # SKALP vs Veryl
│   ├── skalp-vs-chisel.md             # SKALP vs Chisel (future)
│   └── feature-matrix.md              # All HDLs comparison
│
└── specification/                     # FORMAL SPECIFICATIONS
    ├── language-spec.md               # Complete language spec
    ├── grammar.ebnf                   # EBNF grammar
    ├── type-system.md                 # Formal type system
    └── semantics.md                   # Formal semantics
```

---

## Phase 1: Essential User Documentation (Week 1)

### Priority 1: Quick Start (Day 1) ⭐⭐⭐
**File:** `docs/user/quick-start.md`

**Content:**
1. One-command installation
2. "Hello World" counter (5 lines)
3. Compile to SystemVerilog
4. Simulate (if possible)
5. What to read next

**Success Metric:** User writes working counter in 5 minutes

---

### Priority 2: Fix Tutorial (Day 2) ⭐⭐⭐
**File:** `docs/user/tutorial/01-first-design.md` through `08-verification.md`

**Changes:**
- Remove unimplemented features (protocols, traits, clock lifetimes)
- Use only features that work TODAY
- Add "Coming Soon" sections for planned features
- Include testbench examples for each chapter
- Verify every code example compiles

**Success Metric:** Every tutorial example compiles and runs

---

### Priority 3: Testbench Guide (Day 3) ⭐⭐⭐
**File:** `docs/user/guides/testbench.md`

**Content:**
1. Setting up Rust testbench
2. Basic API (`set`, `clock`, `expect`)
3. Multi-clock designs
4. Reading outputs
5. VCD waveforms
6. Complete FIFO test example

**Success Metric:** User can test their first design

---

### Priority 4: Reference Manual (Day 4-5) ⭐⭐
**Files:** `docs/user/reference/*.md`

**Content:**
- **syntax.md** - Every language construct with examples
- **types.md** - bit, nat, structs, enums, arrays
- **operators.md** - All operators (+, -, &, |, etc.) with precedence
- **builtins.md** - clog2, width inference
- **cli.md** - Every CLI command and flag

**Format:** Quick lookup, searchable, copy-paste examples

**Success Metric:** User can find any syntax in <30 seconds

---

## Phase 2: Migration & Patterns (Week 2)

### Priority 5: SystemVerilog Migration Guide ⭐⭐
**File:** `docs/user/migration/from-systemverilog.md`

**Content:**
| SystemVerilog | SKALP | Notes |
|---------------|-------|-------|
| `module Counter` | `entity Counter` | Entity declaration |
| `input wire clk` | `in clk: clock` | Dedicated clock type |
| `reg [7:0] data` | `signal data: bit[8]` | Signal declaration |
| `always_ff @(posedge clk)` | `on(clk.rise)` | Sequential logic |
| `assign out = in` | `out = in` | Combinational |
| `case (op)` | `match op { ... }` | Pattern matching |

**Success Metric:** SV engineer can translate basic module in 10 minutes

---

### Priority 6: Cookbook with Patterns ⭐⭐
**File:** `docs/user/cookbook/*.md`

**Examples:**
- **Combinational:** N-input mux, priority encoder, one-hot decoder
- **Sequential:** Loadable counter, shift register, delay line
- **State Machines:** Moore, Mealy, one-hot encoding
- **Memories:** Single-port RAM, dual-port RAM, ROM
- **CDC:** 2-FF synchronizer, gray code FIFO, handshake
- **Protocols:** UART TX/RX, SPI master/slave, I2C controller

**Format:** Problem → Solution → Code → Explanation

**Success Metric:** User finds pattern for common design in <2 minutes

---

## Phase 3: Advanced & Developer Docs (Week 3)

### Priority 7: Reorganize Architecture Docs ⭐
**Move:** `docs/COMPILER_ARCHITECTURE.md` → `docs/developer/architecture/`

**Split into:**
- overview.md (high-level flow)
- frontend.md (lexer, parser, HIR)
- mir.md (MIR passes, CDC analysis)
- codegen.md (SystemVerilog generation)

**Success Metric:** Clear separation of user vs developer docs

---

### Priority 8: API Documentation ⭐
**File:** `docs/developer/api/testbench-api.md`

**Content:**
- Full API reference for `Testbench`
- Every method with examples
- Type conversions
- Multi-clock API
- Error handling

**Success Metric:** Developer can use testbench API without reading source

---

### Priority 9: Example Tests ⭐⭐
**Action:** Add testbenches to all real-world examples

**For each example:**
1. Create `test.rs` in example directory
2. Write comprehensive test
3. Document expected behavior
4. Include waveform screenshot (if possible)

**Success Metric:** Every example has passing test

---

## Phase 4: Polish & Completeness (Week 4)

### Priority 10: Troubleshooting Guide
**File:** `docs/user/guides/troubleshooting.md`

**Content:**
- Common compile errors (with fixes)
- Type mismatch errors
- CDC violations
- Width inference failures
- Simulation issues
- Known bugs and workarounds

---

### Priority 11: Complete Examples
**Add missing examples:**
- DDR controller
- Pipelined CPU
- AHB/APB bridges
- Ethernet MAC (simplified)
- DMA controller

---

### Priority 12: Video Tutorials (Future)
**Platform:** YouTube or docs site

**Videos:**
1. "SKALP in 5 minutes"
2. "Building a UART from scratch"
3. "CDC-safe async FIFO"
4. "Testing with the SKALP testbench API"

---

## Documentation Quality Standards

### Every Document Must Have:
1. **Clear objective** - What will reader learn?
2. **Prerequisites** - What should they know first?
3. **Working code examples** - Must compile
4. **Expected output** - Show what success looks like
5. **Next steps** - Where to go next
6. **Last updated date** - Track freshness

### Code Examples Must:
1. ✅ Compile successfully
2. ✅ Include comments
3. ✅ Show complete context (not fragments)
4. ✅ Be copy-pasteable
5. ✅ Include expected output

### Writing Style:
- **Concise** - Respect reader's time
- **Example-driven** - Show, don't just tell
- **Progressive** - Simple → complex
- **Practical** - Real-world use cases
- **Honest** - Acknowledge limitations

---

## Metrics for Success

### User Metrics:
- ⏱️ Time to first compile: **< 5 minutes**
- ⏱️ Time to first simulation: **< 15 minutes**
- ⏱️ Time to write FIFO: **< 30 minutes**
- 📖 Tutorial completion rate: **> 70%**
- 🔍 Reference lookup time: **< 30 seconds**

### Content Metrics:
- ✅ 100% of examples compile
- ✅ 100% of examples have tests
- ✅ 100% of code snippets compile
- ✅ Zero broken links
- 📅 Docs updated within 7 days of feature changes

---

## Tools & Infrastructure

### Documentation Generation:
- **mdBook** - Generate beautiful docs site
- **rustdoc** - API documentation
- **cargo test --doc** - Test code in docs

### CI/CD:
- **Doc tests** - Verify all code examples compile
- **Link checker** - Find broken links
- **Spell checker** - Professional appearance
- **Auto-deploy** - Docs site updates on commit

### Templates:
- Tutorial template
- Reference page template
- Example project template
- Cookbook pattern template

---

## Timeline Summary

| Week | Focus | Deliverables |
|------|-------|--------------|
| **Week 1** | Essential user docs | Quick start, fixed tutorial, testbench guide, reference manual |
| **Week 2** | Migration & patterns | SV migration guide, cookbook with 20+ patterns |
| **Week 3** | Advanced & developer | Architecture reorganization, API docs, example tests |
| **Week 4** | Polish & completeness | Troubleshooting, advanced examples, doc site |

---

## Success Criteria

### For Users:
✅ Can evaluate SKALP in 5 minutes
✅ Can write first design in 15 minutes
✅ Can find any syntax reference in 30 seconds
✅ Can migrate SV module to SKALP in 30 minutes
✅ Can test their design with clear examples

### For Project:
✅ Documentation as good as Rust or Go
✅ Lower barrier to entry than SystemVerilog
✅ Competitive with Veryl's documentation
✅ Searchable, browsable, beautiful docs site
✅ Maintained and up-to-date

---

## Next Steps

1. **Approve this plan** - Review and adjust priorities
2. **Create document templates** - Standardize format
3. **Set up doc infrastructure** - mdBook, CI, hosting
4. **Start with Phase 1** - Quick start is critical
5. **Iterate based on feedback** - Track what users struggle with

---

**Documentation is the UI of your language. Let's make it world-class.**
