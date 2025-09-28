# SKALP Performance Benchmarks

This directory contains performance benchmarks for the SKALP compiler and runtime.

## Running Benchmarks

```bash
# Run all benchmarks
cargo bench

# Run specific benchmark
cargo bench --bench compilation

# Run with detailed output
cargo bench -- --verbose
```

## Benchmark Categories

### 1. Compilation Performance
- Lexing and parsing speed
- HIR generation time
- MIR transformation time
- Code generation speed
- End-to-end compilation time

### 2. Optimization Performance
- Constant folding efficiency
- Dead code elimination speed
- Boolean simplification performance
- CSE (Common Subexpression Elimination) impact

### 3. GPU Simulation Performance
- CPU vs GPU simulation speed
- Shader compilation time
- Data transfer overhead
- Scalability with design size

### 4. Memory Usage
- Peak memory during compilation
- Memory efficiency per module
- Cache utilization

## Results

Benchmark results are stored in `target/criterion/` after running.