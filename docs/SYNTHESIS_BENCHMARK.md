# SKALP Synthesis Engine Benchmark Report

## Overview

This report summarizes the synthesis quality of SKALP's native Rust logic synthesis engine across different optimization presets.

## Benchmark Results

### Core Designs

| Design | Quick | Balanced | Full | Reduction |
|--------|------:|----------|------|-----------|
| Counter (8-bit) | 67 | 67 | 67 | 0% |
| ALU (32-bit) | 2,071 | 1,177 | 711 | **65.7%** |
| SPI Master | 19 | 19 | 19 | 0% |
| FIFO (sync) | 11 | 11 | 11 | 0% |
| CDC Synchronizer | 8 | 8 | 8 | 0% |

### Real-World Designs

| Design | Quick | Balanced | Full | Reduction |
|--------|------:|----------|------|-----------|
| UART TX | 7 | 7 | 7 | 0% |
| I2C Master | 15 | 15 | 15 | 0% |
| TMR Counter | 17 | 17 | 17 | 0% |
| Async FIFO | 62 | 62 | 62 | 0% |

## Optimization Presets

### Quick
- Pass sequence: `strash, const_prop, dce`
- Technology mapping: Fast mode (k=4, max_cuts=8)
- Use case: Fast iteration during development

### Balanced
- Pass sequence: `strash, rewrite, const_prop, dce, balance, rewrite`
- Technology mapping: Quality mode (k=6, max_cuts=16, area recovery)
- Use case: Good balance between compile time and optimization quality

### Full
- Pass sequence: `strash, rewrite, const_prop, dce, balance, rewrite, refactor, rewrite, balance, fraig`
- Technology mapping: Quality mode with choices
- Use case: Production synthesis requiring best QoR

### Timing
- Pass sequence: Full + `retiming, buffer`
- Technology mapping: Timing-aware with buffer insertion
- Use case: Timing-critical designs

## Key Findings

### 1. ALU Optimization
The 32-bit ALU shows significant optimization potential:
- **65.7% cell reduction** from Quick to Full preset
- 2,071 cells (Quick) -> 1,177 cells (Balanced) -> 711 cells (Full)
- Demonstrates effectiveness of:
  - AIG rewriting passes
  - FRAIG (SAT-based equivalence checking)
  - Area recovery during technology mapping

### 2. Small Designs
Simple designs (Counter, SPI Master, CDC Sync) show minimal reduction because:
- Already close to optimal representation
- Limited opportunities for sharing/factoring
- Register-dominated (flip-flops are not reduced)

### 3. Optimization Passes Impact

| Pass | Primary Effect |
|------|---------------|
| strash | Structural hashing - merge identical subgraphs |
| rewrite | 4-cut replacement with smaller equivalents |
| balance | Reduce logic depth through tree balancing |
| refactor | Larger cone optimization |
| fraig | SAT sweeping - merge functionally equivalent nodes |
| retiming | Move registers for timing optimization |
| buffer | Fanout management for drive strength |

## Comparison with Industry Tools

SKALP is designed for safety-critical hardware synthesis with these unique features:

1. **Safety Preservation**: FIT rates and ISO 26262 classifications maintained through optimization
2. **Traceability**: Source-to-gate mapping for safety audits
3. **Native Rust**: No external tool dependencies

### Expected Comparison (when Yosys/ABC available)

| Metric | SKALP | Yosys+ABC | Notes |
|--------|-------|-----------|-------|
| Gate reduction | 60-75% | 70-80% | ABC has more mature rewriting |
| Compile time | Fast | Medium | Rust native implementation |
| Safety tracing | Full | None | Unique to SKALP |
| Technology libs | Basic | Extensive | SKALP uses generic cells |

## Running Benchmarks

```bash
# Run the benchmark suite
./scripts/benchmark_synth.sh

# Run individual design
SKALP_STDLIB_PATH=./crates/skalp-stdlib \
  ./target/release/skalp build \
  -s examples/alu.sk \
  -o /tmp/alu_output \
  --target gates \
  --optimize full
```

## Cell Library

Current cell library includes standard cells:

| Cell Type | Area | Delay (ps) |
|-----------|------|------------|
| INV_X1 | 1.0 | 15 |
| AND2_X1 | 2.0 | 25 |
| OR2_X1 | 2.0 | 22 |
| NAND2_X1 | 1.5 | 18 |
| NOR2_X1 | 1.5 | 20 |
| XOR2_X1 | 3.0 | 35 |
| AND3_X1 | 2.5 | 30 |
| NAND3_X1 | 2.0 | 22 |
| AOI21_X1 | 2.0 | 20 |
| OAI21_X1 | 2.0 | 20 |
| MUX2_X1 | 3.0 | 30 |
| DFFR_X1 | 6.0 | 50 |

## Future Improvements

1. **ML-Guided Synthesis**: Pass ordering using reinforcement learning
2. **Datapath Optimization**: Kogge-Stone adders, Booth multipliers
3. **Liberty Support**: Load real technology libraries
4. **Formal Verification**: Equivalence checking between IR stages

---

*Generated: 2025-12-20*
*SKALP Version: master (commit 781b084)*
