# GPU Testbench User Guide

## Overview

The GPU testbench provides a high-level interface for running hardware simulations on GPU with comprehensive performance analysis, waveform generation, and result validation.

## Quick Start

### Basic Testbench Setup

```rust
use skalp_sim::testbench::*;
use skalp_mir::*;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create testbench
    let mut testbench = GpuTestbench::new("my_counter".to_string());

    // Load design from MIR
    let mir = load_design_from_file("counter.skalp").await?;
    testbench.load_design(mir).await?;

    // Run simulation
    let result = testbench.run().await?;

    println!("Simulation completed in {:?}", result.gpu_result.wall_time);
    Ok(())
}
```

### Configuration Options

```rust
let config = TestbenchConfig {
    max_cycles: 10000,                    // Simulation length
    clock_period_ns: 10.0,                // 100 MHz clock
    enable_waveforms: true,               // Generate VCD output
    waveform_file: Some("waves.vcd".to_string()),
    enable_cpu_comparison: true,          // Validate against CPU
    validation_tolerance: 1e-9,           // Floating point tolerance
};

testbench.set_config(config);
```

## Design Loading

### From MIR

```rust
// Load existing MIR
let mir = Mir::new("my_design".to_string());
testbench.load_design(mir).await?;
```

### From File

```rust
// Load SKALP source file (when file loader is implemented)
testbench.load_design_from_file("design.skalp").await?;
```

## Test Vector Management

### Adding Stimulus

```rust
// Create test vector
let mut vector = TestVector {
    time: 0,
    signals: HashMap::new(),
};

// Set signal values
vector.signals.insert("clk".to_string(), 0);
vector.signals.insert("rst".to_string(), 1);
vector.signals.insert("enable".to_string(), 1);

testbench.add_test_vector(vector);
```

### Expected Outputs

```rust
// Define expected results for validation
let mut expected = TestVector {
    time: 100,
    signals: HashMap::new(),
};

expected.signals.insert("counter".to_string(), 50); // Expected count
testbench.add_expected_output(expected);
```

### Loading from JSON

```rust
// Load test vectors from JSON file
testbench.load_test_vectors("test_vectors.json").await?;
```

Example JSON format:
```json
[
    {
        "time": 0,
        "signals": {
            "clk": 0,
            "rst": 1,
            "data_in": 42
        }
    },
    {
        "time": 10,
        "signals": {
            "clk": 1,
            "rst": 0,
            "data_in": 100
        }
    }
]
```

## Running Simulations

### Basic Execution

```rust
let result = testbench.run().await?;

println!("Cycles simulated: {}", result.gpu_result.cycles_simulated);
println!("Wall time: {:?}", result.gpu_result.wall_time);
```

### With Performance Comparison

```rust
// Enable CPU comparison for validation and speedup measurement
let mut config = TestbenchConfig::default();
config.enable_cpu_comparison = true;
testbench.set_config(config);

let result = testbench.run().await?;

if let Some(speedup) = result.performance.speedup {
    println!("GPU Speedup: {:.2}x", speedup);
}
```

## Result Analysis

### Performance Metrics

```rust
let metrics = &result.performance;

println!("GPU Performance:");
println!("  Cycles/sec: {:.2}", metrics.gpu_cps);
println!("  Utilization: {:.1}%", metrics.gpu_utilization);
println!("  Memory BW: {:.2} MB/s", metrics.memory_bandwidth / 1e6);

if let Some(cpu_cps) = metrics.cpu_cps {
    println!("CPU Performance:");
    println!("  Cycles/sec: {:.2}", cpu_cps);
}
```

### Validation Results

```rust
let validation = &result.validation;

if validation.passed {
    println!("✅ All {} test vectors passed", validation.vectors_tested);
} else {
    println!("❌ {}/{} vectors failed",
             validation.vectors_tested - validation.vectors_passed,
             validation.vectors_tested);

    // Print validation errors
    for error in &validation.errors {
        println!("Error at time {}: {} expected {}, got {}",
                error.time, error.signal_name, error.expected, error.actual);
    }
}
```

## Waveform Generation

### Enabling Waveforms

```rust
let mut config = TestbenchConfig::default();
config.enable_waveforms = true;
config.waveform_file = Some("simulation.vcd".to_string());
testbench.set_config(config);
```

### VCD Output Format

The generated VCD file includes:
- All signals with proper hierarchical names
- Accurate timing information
- Value change events
- Clock and reset signals

### Viewing Waveforms

Use standard waveform viewers:
- **GTKWave**: Open source waveform viewer
- **ModelSim**: Commercial simulator with viewer
- **Vivado**: Xilinx design suite viewer

```bash
# View with GTKWave
gtkwave simulation.vcd
```

## Advanced Usage

### Custom Testbench Logic

```rust
// Custom testbench with complex stimulus generation
pub struct CustomTestbench {
    gpu_testbench: GpuTestbench,
    // Custom state
}

impl CustomTestbench {
    pub async fn run_custom_test(&mut self) -> Result<(), TestbenchError> {
        // Generate complex test patterns
        for cycle in 0..1000 {
            let vector = self.generate_test_vector(cycle);
            self.gpu_testbench.add_test_vector(vector);
        }

        // Run simulation
        let result = self.gpu_testbench.run().await?;

        // Custom analysis
        self.analyze_results(&result);

        Ok(())
    }

    fn generate_test_vector(&self, cycle: u64) -> TestVector {
        // Custom test pattern generation
        let mut vector = TestVector {
            time: cycle,
            signals: HashMap::new(),
        };

        // Generate clock
        vector.signals.insert("clk".to_string(), cycle % 2);

        // Generate data pattern
        vector.signals.insert("data".to_string(), cycle * 7 % 256);

        vector
    }
}
```

### Performance Optimization

```rust
// Optimize for different design characteristics
let mut config = TestbenchConfig::default();

match design_type {
    DesignType::SmallCombinational => {
        config.max_cycles = 100000;  // Many short cycles
        config.enable_cpu_comparison = true;
    }
    DesignType::LargeParallel => {
        config.max_cycles = 1000;    // Fewer long cycles
        config.enable_cpu_comparison = false; // GPU expected to win
    }
    DesignType::MemoryIntensive => {
        config.validation_tolerance = 1e-6; // Looser tolerance
    }
}
```

### Error Handling

```rust
match testbench.run().await {
    Ok(result) => {
        // Handle successful simulation
        process_results(result);
    }
    Err(TestbenchError::NoDesignLoaded) => {
        eprintln!("Error: No design loaded. Call load_design() first.");
    }
    Err(TestbenchError::SimulationFailed(sim_err)) => {
        eprintln!("Simulation failed: {}", sim_err);
        // Check GPU availability, memory constraints, etc.
    }
    Err(TestbenchError::ValidationFailed(msg)) => {
        eprintln!("Validation failed: {}", msg);
        // Review test vectors and expected outputs
    }
    Err(e) => {
        eprintln!("Unexpected error: {}", e);
    }
}
```

## Best Practices

### Test Vector Design

1. **Start Simple**: Begin with basic reset and clock patterns
2. **Edge Cases**: Test boundary conditions and corner cases
3. **Random Patterns**: Include randomized stimulus for thorough coverage
4. **Gradual Complexity**: Build up from simple to complex scenarios

### Performance Testing

1. **Baseline First**: Establish CPU performance baseline
2. **Scale Testing**: Test with increasing design complexity
3. **Resource Monitoring**: Watch GPU memory and utilization
4. **Comparative Analysis**: Compare different design approaches

### Debugging

1. **Enable Waveforms**: Always generate VCD for debugging
2. **Incremental Testing**: Start with short simulations
3. **Validation**: Use expected outputs to catch errors early
4. **Logging**: Enable detailed logging for complex issues

## Configuration Reference

### TestbenchConfig Fields

```rust
pub struct TestbenchConfig {
    /// Maximum simulation cycles
    pub max_cycles: u64,

    /// Clock period in nanoseconds
    pub clock_period_ns: f64,

    /// Enable waveform generation
    pub enable_waveforms: bool,

    /// Waveform output file
    pub waveform_file: Option<String>,

    /// Enable CPU vs GPU comparison
    pub enable_cpu_comparison: bool,

    /// Validation tolerance for floating point
    pub validation_tolerance: f64,
}
```

### Default Values

```rust
impl Default for TestbenchConfig {
    fn default() -> Self {
        Self {
            max_cycles: 1000,
            clock_period_ns: 10.0,        // 100 MHz
            enable_waveforms: false,
            waveform_file: None,
            enable_cpu_comparison: true,
            validation_tolerance: 1e-9,
        }
    }
}
```

## Examples

### Complete Counter Test

```rust
use skalp_sim::testbench::*;
use tokio;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create testbench
    let mut testbench = GpuTestbench::new("counter_test".to_string());

    // Load design (assuming we have a counter MIR)
    let mir = create_counter_mir(8); // 8-bit counter
    testbench.load_design(mir).await?;

    // Configure simulation
    let config = TestbenchConfig {
        max_cycles: 300,
        enable_waveforms: true,
        waveform_file: Some("counter.vcd".to_string()),
        enable_cpu_comparison: true,
        ..Default::default()
    };
    testbench.set_config(config);

    // Add reset sequence
    let reset_vector = TestVector {
        time: 0,
        signals: [
            ("clk".to_string(), 0),
            ("rst".to_string(), 1),
            ("enable".to_string(), 0),
        ].into_iter().collect(),
    };
    testbench.add_test_vector(reset_vector);

    // Add enable sequence
    let enable_vector = TestVector {
        time: 10,
        signals: [
            ("rst".to_string(), 0),
            ("enable".to_string(), 1),
        ].into_iter().collect(),
    };
    testbench.add_test_vector(enable_vector);

    // Add expected results
    let expected = TestVector {
        time: 100,
        signals: [
            ("counter".to_string(), 45), // Expected count after 90 cycles
        ].into_iter().collect(),
    };
    testbench.add_expected_output(expected);

    // Run simulation
    let result = testbench.run().await?;

    // Print results
    if result.validation.passed {
        println!("✅ Counter test PASSED");
        if let Some(speedup) = result.performance.speedup {
            println!("🚀 GPU Speedup: {:.2}x", speedup);
        }
    } else {
        println!("❌ Counter test FAILED");
    }

    Ok(())
}
```

## Troubleshooting

### Common Issues

1. **No GPU Available**: Ensure Metal is available on macOS
2. **Memory Errors**: Large designs may exceed GPU memory
3. **Performance Regression**: Check for inefficient cone generation
4. **Validation Failures**: Verify test vectors and expected outputs

### Debug Steps

1. **Check Logs**: Enable verbose logging
2. **Reduce Complexity**: Test with smaller designs first
3. **Verify Design**: Ensure MIR is valid
4. **Check Resources**: Monitor system resources

### Performance Tips

1. **Optimal Design Size**: 1K-100K gates for best GPU utilization
2. **Memory Bandwidth**: Minimize signal width when possible
3. **Cone Optimization**: Review combinational cone extraction
4. **Workgroup Tuning**: Adjust GPU workgroup sizes

## See Also

- [GPU Simulation Architecture](gpu-simulation-architecture.md)
- [SIR Format Reference](sir-format.md)
- [Performance Optimization Guide](gpu-performance.md)