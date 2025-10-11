# Implicit Instantiation with Synthesis Intents

## Problem Statement

When using implicit instantiation syntax like `result = sqrt(x)`, we need to address:

1. **Resource Sharing**: Multiple calls to the same operation - should they create duplicate hardware or share a single instance?
2. **Pipeline Scheduling**: Operations have latency - how do we specify/schedule multi-cycle operations?

## Solution: Intent System

SKALP already has an `@intent` / `with intent` system (see LANGUAGE_SPECIFICATION.md §8). We extend it to control implicit instantiation behavior.

## Issue 1: Resource Sharing vs Duplication

### The Problem

```skalp
entity Example {
    in x: fp32
    in y: fp32
    out result1: fp32
    out result2: fp32
}

impl Example {
    result1 = sqrt(x)  // Instantiate FP32Sqrt?
    result2 = sqrt(y)  // Instantiate another FP32Sqrt?
}
```

**Question:** Should this create:
- **Option A:** Two separate sqrt instances (parallel execution, more area)
- **Option B:** One shared sqrt instance (sequential execution via muxing, less area)

### Default Behavior: Duplicate Instances

By default, each implicit call creates a separate instance (combinational assumption):

```skalp
impl Example {
    result1 = sqrt(x)  // Creates instance sqrt_0
    result2 = sqrt(y)  // Creates instance sqrt_1
}
```

**Generated hardware:**
```
inst sqrt_0: FP32Sqrt { x = x, result => result1 }
inst sqrt_1: FP32Sqrt { x = y, result => result2 }
```

**Rationale:** Matches user expectation - two separate computations happen in parallel.

### Explicit Sharing: Intent Directive

To share resources, use `@intent(share)`:

```skalp
use std::fp::math::sqrt;

@intent(share: sqrt)
impl Example {
    result1 = sqrt(x)  // } Both share single
    result2 = sqrt(y)  // } sqrt instance
}
```

**Generated hardware:**
```skalp
// Compiler generates:
signal sqrt_input: fp32
signal sqrt_output: fp32
signal sqrt_select: bit  // Which input to process

inst shared_sqrt: FP32Sqrt {
    x = sqrt_input,
    result => sqrt_output
}

// Muxing logic
sqrt_input = if sqrt_select { y } else { x }
result1 = if sqrt_select { 0.0 } else { sqrt_output }  // Valid when select=0
result2 = if sqrt_select { sqrt_output } else { 0.0 }  // Valid when select=1
```

### Fine-Grained Control

Specify exactly which calls to share:

```skalp
impl Pipeline {
    // These share one instance
    @intent(share: group1)
    signal a: fp32 = sqrt(x1)

    @intent(share: group1)
    signal b: fp32 = sqrt(x2)

    // This gets its own instance
    signal c: fp32 = sqrt(x3)
}
```

### Intent Syntax Summary

```skalp
// Share all sqrt calls in this impl
@intent(share: sqrt)
impl MyDesign { ... }

// Share specific group
@intent(share: group_name)
signal result1: fp32 = sqrt(x)

@intent(share: group_name)
signal result2: fp32 = sqrt(y)

// Force duplication (override default)
@intent(duplicate)
signal result: fp32 = sqrt(x)
```

## Issue 2: Pipeline Scheduling and Latency

### The Problem

```skalp
impl Example {
    result = sqrt(x)  // This takes 8 cycles - how to schedule?
}
```

**Questions:**
1. Is this combinational or pipelined?
2. If pipelined, what's the latency?
3. How do we schedule dependent operations?

### Default Behavior: Combinational

By default, implicit instantiation is combinational:

```skalp
result = sqrt(x)  // Assumed combinational, completes in same cycle
```

If the entity is actually pipelined, this may cause synthesis errors or incorrect timing.

### Declaring Latency: Entity Metadata

Stdlib entities can declare their latency:

```skalp
// In stdlib fp32_sqrt.sk
entity FP32Sqrt {
    in x: fp32
    out result: fp32
} with intent {
    latency: 8_cycles,
    pipeline: true,
    throughput: 1_per_cycle
}
```

### Using Pipelined Operations

When calling a pipelined operation, specify scheduling intent:

```skalp
impl Example {
    in x: fp32
    out result: fp32

    // Automatic pipeline - compiler schedules
    @intent(pipeline: auto)
    result = sqrt(x)
}
```

**Generated hardware:**
```skalp
// Compiler generates 8-cycle pipeline
on(clk.rise) {
    stage_0 <= x
    stage_1 <= sqrt_inst.result  // after 8 cycles
    // ... intermediate stages
    result <= stage_7
}

inst sqrt_inst: FP32Sqrt {
    x = stage_0,
    result => stage_1
}
```

### Explicit Latency Specification

Override entity default:

```skalp
@intent(latency: 4_cycles)  // Use fast sqrt variant
signal result: fp32 = sqrt(x)
```

Compiler resolves to `FP32SqrtFast` if available, or errors if no 4-cycle variant exists.

### Scheduling Dependent Operations

```skalp
impl VectorLength {
    in v: vec3<fp32>
    out length: fp32

    @intent(pipeline: auto)
    signal x_sq: fp32 = v.x * v.x    // Cycle 0-4 (multiply)

    @intent(pipeline: auto)
    signal y_sq: fp32 = v.y * v.y    // Cycle 0-4 (parallel)

    @intent(pipeline: auto)
    signal z_sq: fp32 = v.z * v.z    // Cycle 0-4 (parallel)

    @intent(pipeline: auto)
    signal sum_xy: fp32 = x_sq + y_sq  // Cycle 4-5

    @intent(pipeline: auto)
    signal sum: fp32 = sum_xy + z_sq   // Cycle 5-6

    @intent(pipeline: auto)
    length = sqrt(sum)                 // Cycle 6-14 (8 cycle sqrt)
}
```

**Compiler generates:**
- Detects dependencies via dataflow analysis
- Schedules operations respecting latencies
- Total latency: 14 cycles
- Inserts pipeline registers automatically

### Manual Scheduling

For precise control:

```skalp
@intent(schedule: manual)
impl VectorLength {
    // Cycle 0: Input
    signal stage0_x: fp32
    signal stage0_y: fp32
    signal stage0_z: fp32

    // Cycle 1-4: Multiplies (4-cycle latency)
    @intent(latency: 4_cycles, start: cycle_1)
    signal stage4_x_sq: fp32 = stage0_x * stage0_x

    @intent(latency: 4_cycles, start: cycle_1)
    signal stage4_y_sq: fp32 = stage0_y * stage0_y

    @intent(latency: 4_cycles, start: cycle_1)
    signal stage4_z_sq: fp32 = stage0_z * stage0_z

    // Cycle 5: Add
    @intent(start: cycle_5)
    signal stage5_sum: fp32 = stage4_x_sq + stage4_y_sq + stage4_z_sq

    // Cycle 6-13: Sqrt (8-cycle latency)
    @intent(latency: 8_cycles, start: cycle_6)
    signal stage13_result: fp32 = sqrt(stage5_sum)

    on(clk.rise) {
        // Manual pipeline registers
        stage0_x <= v.x
        stage0_y <= v.y
        stage0_z <= v.z
        length <= stage13_result
    }
}
```

## Intent Categories for Implicit Instantiation

### Resource Intents

```skalp
@intent(share: operation_name)        // Share instances
@intent(share: group_identifier)      // Share group
@intent(duplicate)                    // Force separate instance
@intent(resource: minimal)            // Prefer sharing
@intent(resource: performance)        // Prefer duplication
```

### Pipeline Intents

```skalp
@intent(pipeline: auto)               // Automatic scheduling
@intent(pipeline: manual)             // Manual control
@intent(latency: N_cycles)           // Override entity latency
@intent(throughput: X_per_cycle)     // Initiation interval
@intent(start: cycle_N)              // Schedule at specific cycle
```

### Optimization Intents

```skalp
@intent(optimize: latency)           // Minimize latency
@intent(optimize: throughput)        // Maximize throughput
@intent(optimize: area)              // Minimize area
@intent(optimize: power)             // Minimize power
@intent(optimize: balanced)          // Balance all
```

### Mapping Intents

```skalp
@intent(map: dsp)                    // Use DSP blocks
@intent(map: lut)                    // Use LUTs
@intent(map: bram)                   // Use block RAM
```

## Complete Example: Phong Shading with Intents

```skalp
use std::fp::math::{sqrt, min, max};
use std::vec::geometry::{normalize, dot};

entity PhongShader {
    in clk: clock
    in position: vec3<fp32>
    in normal: vec3<fp32>
    in light_pos: vec3<fp32>
    out color: vec3<fp32>
}

// Share normalize and sqrt instances to save area
@intent(share: normalize)
@intent(share: sqrt)
@intent(pipeline: auto)
@intent(optimize: area)
impl PhongShader {
    // Light direction (requires sqrt via normalize)
    signal light_vec: vec3<fp32> = light_pos - position
    signal light_dir: vec3<fp32> = normalize(light_vec)  // Shared normalize #1

    // Normal normalization (shares normalize instance)
    signal norm: vec3<fp32> = normalize(normal)  // Shared normalize #2

    // Diffuse
    signal n_dot_l: fp32 = dot(norm, light_dir)
    signal diffuse: fp32 = max(n_dot_l, 0.0)

    // Specular (shares normalize instance)
    signal reflect_dir: vec3<fp32> = reflect(light_dir, norm)
    signal view_normalized: vec3<fp32> = normalize(view_dir)  // Shared normalize #3
    signal spec_dot: fp32 = dot(reflect_dir, view_normalized)
    signal specular: fp32 = max(spec_dot, 0.0)

    // Final color
    color = material_color * (diffuse + specular)
}
```

**Generated hardware:**
- **1 shared FP32Sqrt instance** (used by all 3 normalize calls)
- **1 shared Vec3Normalize instance** (time-multiplexed 3 ways)
- **Automatic pipeline scheduling** for 3× normalize → sqrt → arithmetic
- **Total latency:** ~20 cycles (vs ~60 if all operations duplicated)
- **Area savings:** ~60% (3 normalizes → 1, 3 sqrts → 1)

## Implementation in Compiler

### Phase 1: Parse Intents

```rust
// In HIR builder
struct FunctionCallSite {
    name: String,
    args: Vec<Type>,
    intent: Option<Intent>,
}

struct Intent {
    share: Option<ShareIntent>,
    pipeline: Option<PipelineIntent>,
    latency: Option<u32>,
    // ...
}
```

### Phase 2: Resource Sharing Analysis

```rust
fn analyze_resource_sharing(impl_block: &HirImpl) -> ResourceMap {
    let mut groups: HashMap<String, Vec<FunctionCallSite>> = HashMap::new();

    // Group calls by share intent
    for call in impl_block.function_calls() {
        if let Some(share_group) = call.intent.share {
            groups.entry(share_group).or_default().push(call);
        }
    }

    // Generate shared instances
    for (group, calls) in groups {
        generate_shared_instance(group, calls);
    }
}
```

### Phase 3: Pipeline Scheduling

```rust
fn schedule_pipeline(impl_block: &HirImpl) -> Schedule {
    let mut scheduler = PipelineScheduler::new();

    for call in impl_block.function_calls() {
        let entity_latency = lookup_entity_latency(&call.name);
        let intent_latency = call.intent.latency;

        let latency = intent_latency.unwrap_or(entity_latency);
        scheduler.add_operation(call, latency);
    }

    scheduler.compute_schedule()  // ASAP, ALAP, or user-specified
}
```

### Phase 4: Generate Hardware

```rust
fn generate_shared_instance(group: &str, calls: &[FunctionCallSite]) {
    // Create mux for inputs
    let mux_width = calls.len();
    let select_bits = (mux_width as f32).log2().ceil() as u32;

    // Generate single instance
    emit_inst(&format!("shared_{}", group), &calls[0].entity);

    // Generate mux and demux logic
    emit_input_mux(calls, select_bits);
    emit_output_demux(calls, select_bits);

    // Generate scheduling FSM if pipelined
    if has_pipeline_intent(calls) {
        emit_scheduling_fsm(calls);
    }
}
```

## Comparison with HLS Tools

### Vivado HLS

```c++
#pragma HLS PIPELINE II=1
#pragma HLS RESOURCE variable=sqrt core=DSP48

float result = sqrtf(x);
```

**SKALP equivalent:**
```skalp
@intent(throughput: 1_per_cycle)
@intent(map: dsp)
signal result: fp32 = sqrt(x)
```

### Intel HLS

```c++
[[intel::fpga_register]]
[[intel::max_concurrency(1)]]
float result = sqrtf(x);
```

**SKALP equivalent:**
```skalp
@intent(share: sqrt)
@intent(throughput: 1_per_cycle)
signal result: fp32 = sqrt(x)
```

### Catapult HLS

```c++
#pragma hls_design pipeline
#pragma hls_resource sqrt_impl core=RESOURCE

result = ac_sqrt(x);
```

**SKALP equivalent:**
```skalp
@intent(pipeline: auto)
signal result: fp32 = sqrt(x)
```

## Key Differences from Traditional HLS

1. **Explicit over Implicit**
   - HLS: Pragmas hint, compiler decides
   - SKALP: Intents are contracts, guaranteed behavior

2. **Type-Driven**
   - HLS: Infer from C++ types (often wrong)
   - SKALP: Strong types (fp32, vec3<fp32>) determine exact entity

3. **Composable**
   - HLS: Pragmas scattered in code
   - SKALP: Intents compose hierarchically

4. **Verifiable**
   - HLS: "Did compiler honor my pragma?" (unclear)
   - SKALP: Intent violations are compile errors

## Summary

**Two orthogonal questions, two intent categories:**

1. **Resource Sharing**: Use `@intent(share: ...)` or `@intent(duplicate)`
2. **Pipeline Scheduling**: Use `@intent(pipeline: auto|manual)` and `@intent(latency: N_cycles)`

**Default behaviors:**
- Each call creates separate instance (parallel execution)
- Operations assumed combinational unless entity declares latency

**Philosophy:**
- Explicit is better than implicit
- User controls hardware mapping
- Compiler enforces correctness
- Intents are contracts, not hints

**Integration with implicit instantiation:**
- `use` brings names into scope
- Function syntax calls operations
- Intents control synthesis behavior

This gives SKALP users **complete control** over hardware generation while maintaining **high-level ergonomics**.
