# Intent Propagation System

**Hierarchical Intent Flow for Composable HLS**

## Problem Statement

When a parent module specifies an intent (e.g., `@intent(latency: 4_cycles)`), child entities/stdlib operations should automatically select the appropriate implementation variant that satisfies that intent.

### Example Scenario

```skalp
// Stdlib has multiple sqrt implementations
entity FP32Sqrt {          // 8-cycle accurate version
    in x: fp32
    out result: fp32
} with intent {
    latency: 8_cycles,
    accuracy: high
}

entity FP32SqrtFast {      // 4-cycle fast version
    in x: fp32
    out result: fp32
} with intent {
    latency: 4_cycles,
    accuracy: medium
}

// User specifies latency intent
@intent(latency: max(4_cycles))
impl VectorNormalize {
    signal len_sq: fp32 = x*x + y*y + z*z

    // Which sqrt gets instantiated?
    // Should automatically select FP32SqrtFast!
    signal len: fp32 = sqrt(len_sq)
}
```

**Current Problem:** Compiler doesn't know which `sqrt` to use.

**Solution:** Intent propagation system that selects appropriate implementation based on parent intent.

---

## 1. Intent Propagation Mechanisms

### 1.1 Automatic Propagation (Top-Down)

Parent intents flow down to child instantiations:

```skalp
@intent(latency: max(10_cycles))
@intent(optimize: area)
impl FastProcessor {
    // These inherit latency and optimization intents
    signal a: fp32 = sqrt(x)      // → FP32SqrtFast (4 cycles < 10 max)
    signal b: fp32 = div(y, z)    // → FP32DivFast (3 cycles < 10 max)
    signal c: fp32 = sin(theta)   // → SinLUT (1 cycle, area-optimized)
}
```

**Propagation Rules:**
1. Child inherits parent intents unless overridden
2. Stricter child intent overrides looser parent intent
3. Conflicts generate compile errors

### 1.2 Explicit Intent Binding

Explicitly pass intent to specific operation:

```skalp
impl MixedLatency {
    // This one needs high accuracy (8 cycles)
    @intent(latency: 8_cycles, accuracy: high)
    signal accurate: fp32 = sqrt(x)

    // This one can be fast (4 cycles)
    @intent(latency: 4_cycles, accuracy: medium)
    signal fast: fp32 = sqrt(y)
}
```

### 1.3 Intent Constraints (Bottom-Up)

Child can specify requirements that parent must satisfy:

```skalp
entity CriticalModule {
    in x: fp32
    out result: fp32
} with intent {
    latency: min(8_cycles),    // REQUIRES at least 8 cycles
    accuracy: high             // REQUIRES high accuracy
}

@intent(latency: max(4_cycles))  // ❌ ERROR: Conflict!
impl Parent {
    let critical = CriticalModule {  // Requires 8, parent allows max 4
        x = input,
        result => output
    }
}
```

**Compile Error:**
```
error: intent conflict - latency constraint violation
  --> parent.sk:5:10
   |
5  |     let critical = CriticalModule {
   |          ^^^^^^^^ requires min(8_cycles)
   |
note: parent intent is max(4_cycles)
  --> parent.sk:1:1
   |
1  | @intent(latency: max(4_cycles))
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ parent allows maximum 4 cycles
   |
help: relax parent constraint or use different implementation
```

---

## 2. Implementation Selection

### 2.1 Intent-Based Overload Resolution

Compiler selects implementation based on intent match:

```skalp
// Stdlib registry with intent metadata
[
    StdlibFunction {
        name: "sqrt",
        entity: "FP32Sqrt",
        signature: (fp32) → fp32,
        intent: {
            latency: 8_cycles,
            accuracy: high,
            area: medium
        }
    },
    StdlibFunction {
        name: "sqrt",
        entity: "FP32SqrtFast",
        signature: (fp32) → fp32,
        intent: {
            latency: 4_cycles,
            accuracy: medium,
            area: small
        }
    },
    StdlibFunction {
        name: "sqrt",
        entity: "FP32SqrtLUT",
        signature: (fp32) → fp32,
        intent: {
            latency: 1_cycle,
            accuracy: low,
            area: large
        }
    }
]
```

**Selection Algorithm:**

```rust
fn select_implementation(
    call: &FunctionCall,
    parent_intent: &Intent,
    candidates: &[StdlibFunction]
) -> Result<&StdlibFunction> {
    // Filter candidates that satisfy parent intent
    let compatible: Vec<_> = candidates.iter()
        .filter(|c| c.intent.satisfies(parent_intent))
        .collect();

    if compatible.is_empty() {
        return Err("No implementation satisfies intent");
    }

    // Rank by how well they match intent priority
    let best = compatible.iter()
        .max_by_key(|c| match_score(c, parent_intent))
        .unwrap();

    Ok(best)
}

fn match_score(candidate: &StdlibFunction, intent: &Intent) -> i32 {
    let mut score = 0;

    // Match optimization priority
    match intent.optimize {
        Optimize::Latency => {
            score += 1000 / candidate.intent.latency;  // Prefer lower latency
        }
        Optimize::Area => {
            score += 1000 / candidate.intent.area;     // Prefer smaller area
        }
        Optimize::Throughput => {
            score += candidate.intent.throughput;      // Prefer higher throughput
        }
    }

    // Bonus for meeting constraints exactly
    if candidate.intent.latency == intent.latency {
        score += 500;
    }

    score
}
```

### 2.2 Example: Automatic Selection

```skalp
@intent(optimize: latency)
impl FastPath {
    // Selects FP32SqrtLUT (1 cycle) - best for latency
    signal result: fp32 = sqrt(x)
}

@intent(optimize: area)
impl SmallPath {
    // Selects FP32SqrtFast (4 cycles, small area) - best area/latency trade-off
    signal result: fp32 = sqrt(x)
}

@intent(optimize: accuracy)
impl PrecisePath {
    // Selects FP32Sqrt (8 cycles, high accuracy) - most accurate
    signal result: fp32 = sqrt(x)
}
```

### 2.3 Constraint Satisfaction

```skalp
@intent(latency: max(5_cycles), accuracy: min(medium))
impl ConstrainedPath {
    // Candidates:
    // - FP32Sqrt: 8 cycles ❌ (exceeds latency)
    // - FP32SqrtFast: 4 cycles, medium accuracy ✅
    // - FP32SqrtLUT: 1 cycle, low accuracy ❌ (below accuracy threshold)

    signal result: fp32 = sqrt(x)  // Selects FP32SqrtFast
}
```

---

## 3. Intent Declaration in Entities

### 3.1 Entity Intent Metadata

Entities declare their characteristics:

```skalp
entity FP32Sqrt {
    in x: fp32
    out result: fp32
} with intent {
    latency: 8_cycles,
    throughput: 1_per_cycle,
    accuracy: high,
    area: 250_lut,
    power: 50_mw,
    resources: {
        dsp: 2,
        bram: 0,
        ff: 400
    }
}

impl FP32Sqrt {
    // Newton-Raphson with 2 iterations
    // ... implementation
}
```

### 3.2 Parameterized Intent

Entities can have configurable intent:

```skalp
entity FP32SqrtParameterized<const ITERATIONS: nat = 2> {
    in x: fp32
    out result: fp32
} with intent {
    latency: 4 * ITERATIONS,           // Scales with iterations
    accuracy: accuracy_model(ITERATIONS),
    area: 150_lut + 50_lut * ITERATIONS
}

impl FP32SqrtParameterized {
    // ITERATIONS determines accuracy vs latency trade-off
    signal result: fp32 = newton_raphson_sqrt(x, iterations: ITERATIONS)
}
```

**Usage:**

```skalp
@intent(latency: max(8_cycles))
impl Example {
    // Compiler selects FP32SqrtParameterized<ITERATIONS=2> (8 cycles)
    signal a: fp32 = sqrt(x)
}

@intent(latency: max(16_cycles), accuracy: high)
impl HighAccuracy {
    // Compiler selects FP32SqrtParameterized<ITERATIONS=4> (16 cycles, higher accuracy)
    signal b: fp32 = sqrt(y)
}
```

### 3.3 Intent Variants

Multiple implementations via traits/variants:

```skalp
// Define sqrt operation interface
trait SqrtOp {
    fn sqrt(x: fp32) -> fp32;
}

// Fast variant
entity FP32SqrtFast {
    in x: fp32
    out result: fp32
} with intent {
    latency: 4_cycles,
    variant: "fast"
}

impl SqrtOp for FP32SqrtFast {
    // Fast implementation
}

// Accurate variant
entity FP32SqrtAccurate {
    in x: fp32
    out result: fp32
} with intent {
    latency: 8_cycles,
    variant: "accurate"
}

impl SqrtOp for FP32SqrtAccurate {
    // Accurate implementation
}

// Compiler selects variant based on intent
@intent(variant: "fast")
impl UserDesign {
    signal result: fp32 = sqrt(x)  // Uses FP32SqrtFast
}
```

---

## 4. Propagation Hierarchy

### 4.1 Multi-Level Propagation

Intents propagate through multiple levels:

```skalp
@intent(optimize: latency)
impl TopLevel {
    let middle = MiddleLevel {
        input = data
    }
}

@intent(optimize: latency)  // Inherited from TopLevel
impl MiddleLevel {
    let bottom = BottomLevel {  // Propagates further
        input = input
    }
}

impl BottomLevel {
    // This inherits optimize: latency from two levels up
    signal result: fp32 = sqrt(x)  // Selects FP32SqrtLUT (1 cycle)
}
```

### 4.2 Intent Composition

Multiple intents combine:

```skalp
@intent(optimize: latency)
@intent(power: low)
impl PowerAwareFastPath {
    // Must satisfy BOTH latency AND power constraints
    // Selects implementation that's fast BUT also low power
    signal result: fp32 = sqrt(x)

    // Possible selection:
    // - FP32SqrtLUT: 1 cycle but high power ❌
    // - FP32SqrtFast: 4 cycles, medium power ✅
    // - FP32Sqrt: 8 cycles, low power ❌
}
```

**Selection with multiple constraints:**

```rust
fn satisfies_all_intents(
    candidate: &StdlibFunction,
    intents: &[Intent]
) -> bool {
    intents.iter().all(|intent| candidate.satisfies(intent))
}
```

### 4.3 Intent Override

Child can override parent intent:

```skalp
@intent(optimize: area)  // Parent wants small area
impl Parent {
    // Most operations optimize for area
    signal a: fp32 = add(x, y)     // Uses small adder
    signal b: fp32 = mul(x, y)     // Uses small multiplier

    // But this one needs speed (override)
    @intent(optimize: latency)
    signal critical: fp32 = sqrt(z)  // Uses FP32SqrtLUT (fast)
}
```

---

## 5. Intent Negotiation

### 5.1 Conflict Resolution

When intents conflict, compiler negotiates:

```skalp
@intent(latency: max(4_cycles))
@intent(accuracy: high)
impl Conflicting {
    // No sqrt implementation satisfies BOTH:
    // - FP32SqrtFast: 4 cycles but medium accuracy
    // - FP32Sqrt: high accuracy but 8 cycles

    signal result: fp32 = sqrt(x)  // ❌ ERROR
}
```

**Error Message:**
```
error: no implementation satisfies all intents
  --> example.sk:10:27
   |
10 |     signal result: fp32 = sqrt(x)
   |                           ^^^^^^^ cannot find sqrt implementation
   |
note: conflicting requirements:
  - latency: max(4_cycles)  [from @intent on line 1]
  - accuracy: high          [from @intent on line 2]
   |
note: available implementations:
  - FP32SqrtFast: latency=4, accuracy=medium ✗ (accuracy too low)
  - FP32Sqrt: latency=8, accuracy=high ✗ (latency too high)
   |
help: relax one of the constraints or provide explicit implementation
   |
10 |     @intent(accuracy: medium)  // Accept medium accuracy
   |     signal result: fp32 = sqrt(x)
```

### 5.2 Priority-Based Selection

Specify intent priority:

```skalp
@intent(optimize: latency, priority: 1)    // Highest priority
@intent(optimize: area, priority: 2)       // Secondary
@intent(optimize: power, priority: 3)      // Tertiary
impl PrioritizedDesign {
    // Selects implementation that best satisfies latency,
    // then among those, best area, then best power
    signal result: fp32 = sqrt(x)
}
```

### 5.3 Relaxation Strategies

Allow automatic constraint relaxation:

```skalp
@intent(latency: max(4_cycles), relax: auto)
impl AutoRelax {
    // If no 4-cycle sqrt exists, compiler tries:
    // 1. 4 cycles (not found)
    // 2. 5 cycles (not found)
    // 3. 6 cycles (not found)
    // 4. ... up to 8 cycles (found: FP32Sqrt)

    signal result: fp32 = sqrt(x)
}

// Warning issued:
// warning: intent relaxed from max(4_cycles) to 8_cycles
//   --> example.sk:5:27
```

---

## 6. Stdlib Intent Patterns

### 6.1 Standard Intent Profiles

Stdlib defines standard profiles:

```skalp
// In stdlib metadata
profile "low_latency" {
    optimize: latency,
    latency: max(2_cycles),
    accuracy: medium,
    area: relaxed
}

profile "high_accuracy" {
    optimize: accuracy,
    latency: relaxed,
    accuracy: high,
    area: relaxed
}

profile "low_power" {
    optimize: power,
    latency: relaxed,
    accuracy: medium,
    power: low,
    clock_gating: true
}

profile "minimal_area" {
    optimize: area,
    latency: relaxed,
    accuracy: medium,
    resource_sharing: aggressive
}
```

**Usage:**

```skalp
@intent(profile: "low_latency")
impl FastProcessor {
    // All stdlib calls use low_latency variants
    signal a: fp32 = sqrt(x)   // → FP32SqrtLUT
    signal b: fp32 = sin(y)    // → SinLUT
    signal c: fp32 = div(z, w) // → DivFast
}

@intent(profile: "high_accuracy")
impl PreciseProcessor {
    // All stdlib calls use high_accuracy variants
    signal a: fp32 = sqrt(x)   // → FP32Sqrt (Newton-Raphson)
    signal b: fp32 = sin(y)    // → SinCORDIC
    signal c: fp32 = div(z, w) // → DivIterative
}
```

### 6.2 Per-Operation Intent Override

Override profile for specific operations:

```skalp
@intent(profile: "minimal_area")
impl MixedDesign {
    // Most operations use minimal_area
    signal a: fp32 = add(x, y)
    signal b: fp32 = mul(x, y)

    // Critical path needs speed
    @intent(override_profile, latency: 1_cycle)
    signal critical: fp32 = sqrt(z)  // Uses SqrtLUT despite area profile
}
```

---

## 7. Implementation in Compiler

### 7.1 Intent Resolution Pipeline

```rust
// Phase 1: Collect intents
struct IntentContext {
    entity_intents: HashMap<EntityId, Intent>,
    impl_intents: HashMap<ImplId, Intent>,
    signal_intents: HashMap<SignalId, Intent>,
}

// Phase 2: Propagate intents
fn propagate_intents(ctx: &mut IntentContext, hierarchy: &ModuleHierarchy) {
    for module in hierarchy.top_down() {
        let parent_intent = ctx.get_parent_intent(module);
        let module_intent = ctx.get_module_intent(module);

        // Merge parent and module intents
        let effective_intent = merge_intents(parent_intent, module_intent)?;

        // Propagate to children
        for child in module.children() {
            ctx.set_parent_intent(child, effective_intent.clone());
        }
    }
}

// Phase 3: Resolve implementations
fn resolve_stdlib_call(
    call: &FunctionCall,
    intent: &Intent,
    registry: &StdlibRegistry
) -> Result<EntityId> {
    let candidates = registry.find_functions(&call.name);

    // Filter by type signature
    let type_matches: Vec<_> = candidates.iter()
        .filter(|c| c.signature.matches(&call.arg_types))
        .collect();

    // Filter by intent constraints
    let intent_matches: Vec<_> = type_matches.iter()
        .filter(|c| c.intent.satisfies(intent))
        .collect();

    if intent_matches.is_empty() {
        return Err(IntentError::NoMatchingImplementation {
            call,
            intent,
            available: type_matches
        });
    }

    // Rank by intent match quality
    let best = intent_matches.iter()
        .max_by_key(|c| intent_match_score(c, intent))
        .unwrap();

    Ok(best.entity_id)
}
```

### 7.2 Intent Validation

```rust
fn validate_intent_hierarchy(ctx: &IntentContext) -> Result<()> {
    for (child_id, child_intent) in &ctx.signal_intents {
        let parent_intent = ctx.get_parent_intent(child_id);

        // Check for conflicts
        if let Some(conflict) = find_conflict(&child_intent, &parent_intent) {
            return Err(IntentError::Conflict {
                child: child_id,
                child_intent,
                parent_intent,
                conflict
            });
        }
    }
    Ok(())
}

fn find_conflict(child: &Intent, parent: &Intent) -> Option<IntentConflict> {
    // Check latency conflict
    if let (Some(child_lat), Some(parent_lat)) = (&child.latency, &parent.latency) {
        match (child_lat, parent_lat) {
            (Latency::Min(c), Latency::Max(p)) if c > p => {
                return Some(IntentConflict::Latency {
                    child_requires: c,
                    parent_allows: p
                });
            }
            _ => {}
        }
    }

    // Check other conflicts...
    None
}
```

### 7.3 Intent Metadata Format

```yaml
# stdlib/fp/fp32_sqrt.yaml
entity: FP32Sqrt
signature: (fp32) -> fp32
intent:
  latency: 8_cycles
  throughput: 1_per_cycle
  accuracy:
    ulp: 0.5          # Unit in Last Place error
    bits: 23          # Accurate to 23 bits
  resources:
    dsp: 2
    lut: 250
    ff: 400
    bram: 0
  power: 50_mw
  tags: [accurate, iterative, newton_raphson]

---
entity: FP32SqrtFast
signature: (fp32) -> fp32
intent:
  latency: 4_cycles
  throughput: 1_per_cycle
  accuracy:
    ulp: 2.0
    bits: 18
  resources:
    dsp: 1
    lut: 150
    ff: 200
    bram: 0
  power: 30_mw
  tags: [fast, approximate]

---
entity: FP32SqrtLUT
signature: (fp32) -> fp32
intent:
  latency: 1_cycle
  throughput: 1_per_cycle
  accuracy:
    ulp: 4.0
    bits: 12
  resources:
    dsp: 0
    lut: 50
    ff: 50
    bram: 1
  power: 80_mw       # Higher due to BRAM access
  tags: [lut, combinational, fast]
```

---

## 8. Advanced Patterns

### 8.1 Adaptive Implementation Selection

Select implementation based on runtime profiling:

```skalp
@intent(adaptive: {
    profile: true,                    // Enable profiling
    metrics: [latency, throughput],
    switch_threshold: 10_percent      // Switch if >10% improvement
})
impl AdaptiveProcessor {
    // Compiler generates multiple implementations
    // Runtime selects based on profiling data
    signal result: fp32 = sqrt(x)
}
```

### 8.2 Multi-Objective Optimization

Pareto-optimal selection:

```skalp
@intent(optimize: pareto(latency, power))
impl ParetoOptimal {
    // Select implementation on Pareto frontier
    // No other implementation is better in BOTH latency AND power
    signal result: fp32 = sqrt(x)
}
```

### 8.3 Intent Inheritance via Traits

```skalp
trait FastOp with intent(latency: max(4_cycles)) {
    // All implementations must satisfy latency constraint
}

impl FastOp for CustomSqrt {
    // Must be <= 4 cycles
} with intent {
    latency: 3_cycles  // ✓ Satisfies trait constraint
}

impl FastOp for SlowSqrt {  // ❌ ERROR: violates trait intent
} with intent {
    latency: 8_cycles  // ✗ Exceeds trait constraint
}
```

---

## 9. Complete Example: Video Pipeline

```skalp
use std::fp::math::sqrt;
use std::vec::geometry::normalize;

entity VideoProcessor {
    in clk: clock
    in pixels: stream<rgb888>
    out processed: stream<rgb888>
}

// Top-level intent: real-time processing
@intent(profile: "low_latency")
@intent(throughput: 148_500_000_pixels_per_sec)  // 1080p60
@intent(latency: max(10_cycles))
impl VideoProcessor {
    flow {
        // Fast color conversion (1 cycle)
        let yuv = pixels |> rgb_to_yuv()  // Uses LUT variant

        // Fast blur (4 cycles) - inherits low_latency
        let blurred = yuv |> gaussian_blur()

        // This stage needs high accuracy despite latency profile
        @intent(override_profile, accuracy: high)
        let edges = blurred |> sobel_operator()  // Uses accurate variant

        processed = edges
    }
}

// Gaussian blur with intent-driven implementation selection
entity GaussianBlur {
    in pixel: rgb888
    out result: rgb888
}

// Fast variant for low-latency profile
entity GaussianBlurFast {
    in pixel: rgb888
    out result: rgb888
} with intent {
    latency: 4_cycles,
    accuracy: medium,
    variant: "separable_fast"
}

impl GaussianBlurFast {
    // Separable filter with reduced precision
    signal h_filtered = conv1d_fast(pixel, horizontal_kernel)
    result = conv1d_fast(h_filtered, vertical_kernel)
}

// Accurate variant for high-accuracy profile
entity GaussianBlurAccurate {
    in pixel: rgb888
    out result: rgb888
} with intent {
    latency: 16_cycles,
    accuracy: high,
    variant: "2d_direct"
}

impl GaussianBlurAccurate {
    // Direct 2D convolution with full precision
    result = conv2d(pixel, gaussian_kernel_5x5)
}

// Compiler selects GaussianBlurFast for VideoProcessor
// because it inherits low_latency profile
```

---

## Summary

### Key Features

1. **Automatic Propagation** - Parent intents flow to children
2. **Constraint Satisfaction** - Compiler validates intent hierarchies
3. **Implementation Selection** - Best match based on intent scores
4. **Conflict Resolution** - Clear errors with helpful messages
5. **Profile System** - Reusable intent configurations
6. **Override Mechanism** - Fine-grained control when needed

### Benefits

✅ **Composable** - Intents work through module hierarchy
✅ **Flexible** - Mix automatic and explicit selection
✅ **Safe** - Conflicts detected at compile time
✅ **Portable** - Change performance characteristics via intents
✅ **Maintainable** - Intent profiles for consistent design
✅ **Predictable** - Explicit selection rules, no surprises

### Next Steps

1. Extend stdlib with intent metadata for all operations
2. Implement intent propagation in HIR builder
3. Add intent-based overload resolution
4. Create standard intent profiles
5. Implement intent validation and conflict detection
6. Generate helpful error messages for intent violations

**With intent propagation, SKALP becomes a truly composable HLS system!** 🎯
