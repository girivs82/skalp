# Pipeline Annotations

Pipeline annotations track latency changes introduced during synthesis optimization,
enabling behavioral simulation to match gate-level timing.

## Overview

When synthesis performs register retiming to meet timing constraints, the gate-level
implementation may have different latency than the original behavioral description.
Pipeline annotations capture these changes and allow the simulator to adjust
behavioral simulation to match.

### The Problem

```
Behavioral:  input → [combinational logic] → output  (1 cycle)
Gate-level:  input → [logic] → [REG] → [logic] → output  (2 cycles after retiming)
```

Without annotations, the same testbench produces different results at behavioral
vs gate-level, making verification difficult.

### The Solution

1. **Synthesis** records pipeline stages added during retiming
2. **Annotations** are written to `pipeline_annotations.toml`
3. **Simulator** reads annotations and delays outputs accordingly

## File Format

Pipeline annotations use TOML format:

```toml
[metadata]
target_frequency_mhz = 500.0
original_critical_path_ps = 3200.0
final_critical_path_ps = 1800.0
timing_met = true
timestamp = "1735400000"
skalp_version = "0.1.0"

[[modules]]
name = "MyALU"
original_latency_cycles = 1
final_latency_cycles = 3

[[modules.pipeline_stages]]
location = "multiply_output"
signal = "mult_result"
cycles_added = 1
reason = "Critical path: 2.3ns exceeds 2.0ns budget"
direction = "forward"
original_arrival_ps = 2300.0

[[modules.pipeline_stages]]
location = "adder_output"
signal = "add_result"
cycles_added = 1
reason = "Critical path: 2.1ns exceeds 2.0ns budget"
direction = "forward"
original_arrival_ps = 2100.0

[[modules.path_latencies]]
from_signal = "a"
to_signal = "result"
cycles = 3
delay_ps = 1800.0
```

## CLI Usage

Pipeline annotations are automatically generated when synthesis performs retiming:

```bash
# Build with timing-focused optimization (includes retiming)
skalp build -s src/main.sk -o build --target gates --optimize timing

# Check if annotations were generated
cat build/pipeline_annotations.toml
```

Output includes:
- `design_gates.v` - Gate-level Verilog
- `design_gates.json` - Gate netlist JSON
- `pipeline_annotations.toml` - Latency annotations (if retiming occurred)

## Simulator API

### Loading Annotations

```rust
use skalp_sim::{UnifiedSimulator, UnifiedSimConfig, SimLevel, HwAccel};

let config = UnifiedSimConfig {
    level: SimLevel::GateLevel,
    hw_accel: HwAccel::Auto,
    max_cycles: 1000,
    capture_waveforms: true,
};

let mut sim = UnifiedSimulator::new(config)?;

// Load annotations from synthesis output
sim.load_pipeline_annotations("build/pipeline_annotations.toml")?;

// Load the design
sim.load_gate_level(&sir)?;
```

### Manual Latency Override

For testing or when annotations are not available:

```rust
// Add 2-cycle delay to "count" output
sim.set_output_latency("count", 2);

// Check total adjustment
println!("Total latency: {} cycles", sim.total_latency_adjustment());
```

### Reading Outputs

```rust
// Returns delayed value (with latency adjustment)
let count = sim.get_output("count");

// Returns raw value (bypasses delay for debugging)
let count_raw = sim.get_output_raw("count");
```

### Checking Annotation Status

```rust
if sim.has_pipeline_annotations() {
    println!("Pipeline annotations loaded");
    println!("Total adjustment: {} cycles", sim.total_latency_adjustment());
}
```

## Programmatic Access

### Reading Annotations

```rust
use skalp_lir::synth::PipelineAnnotations;
use std::path::Path;

let annotations = PipelineAnnotations::read_toml(Path::new("build/pipeline_annotations.toml"))?;

// Check summary
println!("{}", annotations.summary());
// Output: "Pipeline annotations: 1 modules, 2 stages, 2 cycles added, timing met"

// Get latency adjustment for a specific module
let adjustment = annotations.get_module_latency_adjustment("MyALU");
println!("MyALU latency adjustment: {} cycles", adjustment);

// Iterate through modules
for module in &annotations.modules {
    println!("Module: {}", module.name);
    println!("  Original latency: {} cycles", module.original_latency_cycles);
    println!("  Final latency: {} cycles", module.final_latency_cycles);

    for stage in &module.pipeline_stages {
        println!("  Stage: {} (+{} cycles)", stage.signal, stage.cycles_added);
        println!("    Reason: {}", stage.reason);
        println!("    Direction: {}", stage.direction);
    }
}
```

### Creating Annotations Programmatically

```rust
use skalp_lir::synth::{
    PipelineAnnotations, ModuleAnnotations, PipelineStage, PathLatency
};

let mut annotations = PipelineAnnotations::with_metadata(500.0); // 500 MHz target

let mut module = ModuleAnnotations::new("MyModule".to_string());
module.original_latency_cycles = 1;

module.add_stage(PipelineStage::with_details(
    "mult_out".to_string(),
    "Critical path exceeded target".to_string(),
    1,       // cycles added
    "forward",
    2300.0,  // arrival time in ps
));

annotations.add_module(module);

// Write to file
annotations.write_toml(Path::new("annotations.toml"))?;
```

## Integration with Testbenches

### Behavioral vs Gate-Level Comparison

```rust
use skalp_sim::{UnifiedSimulator, UnifiedSimConfig, SimLevel};

// Run behavioral simulation
let mut behavioral_sim = UnifiedSimulator::new(UnifiedSimConfig {
    level: SimLevel::Behavioral,
    ..Default::default()
})?;
behavioral_sim.load_behavioral(&sir_module)?;

// Run gate-level simulation with annotations
let mut gate_sim = UnifiedSimulator::new(UnifiedSimConfig {
    level: SimLevel::GateLevel,
    ..Default::default()
})?;
gate_sim.load_pipeline_annotations("build/pipeline_annotations.toml")?;
gate_sim.load_gate_level(&gate_sir)?;

// Apply same inputs
behavioral_sim.set_input("a", 42);
gate_sim.set_input("a", 42);

// Step both
behavioral_sim.step();
gate_sim.step();

// Compare outputs (should match with annotations applied)
let behavioral_out = behavioral_sim.get_output("result");
let gate_out = gate_sim.get_output("result"); // Uses delayed value

assert_eq!(behavioral_out, gate_out);
```

## Synthesis Configuration

Retiming is triggered by timing-focused optimization:

```bash
# Timing preset includes retiming
skalp build --optimize timing

# Custom passes including retiming
skalp build --passes "strash,balance,retiming,dce"

# High-frequency retiming (more aggressive, 500MHz target)
skalp build --passes "strash,balance,retiming_hf,dce"
```

### Retiming Configuration (Internal)

The retiming pass uses these default parameters:

| Parameter | Default | High-Frequency |
|-----------|---------|----------------|
| Target period | 10,000 ps (100 MHz) | 2,000 ps (500 MHz) |
| Gate delay | 50 ps | 30 ps |
| Setup time | 100 ps | 50 ps |
| Max iterations | 10 | 20 |
| Allow backward | true | true |

## Data Structures

### PipelineAnnotations

Top-level container for all annotations:

```rust
pub struct PipelineAnnotations {
    pub metadata: SynthesisMetadata,
    pub modules: Vec<ModuleAnnotations>,
}
```

### SynthesisMetadata

Synthesis configuration and results:

```rust
pub struct SynthesisMetadata {
    pub target_frequency_mhz: f64,
    pub original_critical_path_ps: f64,
    pub final_critical_path_ps: f64,
    pub timing_met: bool,
    pub timestamp: String,
    pub skalp_version: String,
}
```

### ModuleAnnotations

Per-module latency information:

```rust
pub struct ModuleAnnotations {
    pub name: String,
    pub original_latency_cycles: u32,
    pub final_latency_cycles: u32,
    pub pipeline_stages: Vec<PipelineStage>,
    pub path_latencies: Vec<PathLatency>,
}
```

### PipelineStage

Individual pipeline stage added during retiming:

```rust
pub struct PipelineStage {
    pub location: String,
    pub signal: String,
    pub cycles_added: u32,
    pub reason: String,
    pub source_line: Option<u32>,
    pub original_arrival_ps: Option<f64>,
    pub direction: String,  // "forward" or "backward"
}
```

### PathLatency

Path-specific timing information:

```rust
pub struct PathLatency {
    pub from_signal: String,
    pub to_signal: String,
    pub cycles: u32,
    pub delay_ps: f64,
}
```

## Limitations

1. **Module-level granularity**: Current implementation tracks latency per module,
   not per individual output signal.

2. **Simple delay model**: Uses a FIFO delay buffer; doesn't model complex
   pipeline hazards or dependencies.

3. **Forward/backward only**: Tracks forward and backward retiming; doesn't
   handle more complex restructuring.

## Future Enhancements

- Per-signal latency tracking
- Integration with formal verification
- Automatic testbench latency adjustment
- Waveform annotation for debugging
