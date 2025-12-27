# Intent as a First-Class Type

**Compile-Time Metaprogramming for Hardware Generation**

## Core Idea

Make `Intent` a first-class type that can be:
- Passed as parameters
- Queried at compile-time
- Used in conditional compilation (`if intent.latency < 8`)
- Composed and transformed

This enables **single implementation with compile-time specialization** instead of multiple variant entities.

---

## 1. Intent as Type Parameter

### 1.1 Basic Syntax

```skalp
entity FP32Sqrt<intent I: Intent> {
    in x: fp32
    out result: fp32
}

impl<intent I> FP32Sqrt<I> {
    // Compile-time branching based on intent
    result = if I.latency < 4 {
        // Fast LUT-based implementation
        lut_sqrt(x)
    } else if I.latency < 8 {
        // Medium-speed iterative
        newton_raphson_sqrt(x, iterations: 1)
    } else {
        // High-accuracy iterative
        newton_raphson_sqrt(x, iterations: 2)
    }
}
```

### 1.2 Intent Type Definition

```skalp
// Built-in Intent type
type Intent = {
    latency: Cycles | auto,
    throughput: PerCycle | auto,
    accuracy: Accuracy | auto,
    area: LUT | auto,
    power: Milliwatts | auto,
    optimize: Optimize,
    // ... extensible
}

// Optimization target enum
enum Optimize {
    Latency,
    Throughput,
    Area,
    Power,
    Balanced
}

// Accuracy enum
enum Accuracy {
    Low,      // ~12 bits
    Medium,   // ~18 bits
    High      // ~23 bits (full FP32)
}
```

### 1.3 Usage Example

```skalp
// User specifies intent at call site
@intent(latency: 2_cycles)
impl VideoProcessor {
    // Intent propagates to FP32Sqrt
    // Compiler instantiates: FP32Sqrt<Intent{latency: 2}>
    // Which selects lut_sqrt(x) branch
    signal result: fp32 = sqrt(x)
}
```

---

## 2. Compile-Time Intent Queries

### 2.1 Intent Introspection

```skalp
impl<intent I> FP32Sqrt<I> {
    // Query intent properties at compile-time
    signal implementation: Implementation =
        if I.optimize == Optimize::Latency {
            Implementation::LUT
        } else if I.optimize == Optimize::Area {
            Implementation::Iterative
        } else if I.optimize == Optimize::Accuracy {
            Implementation::HighPrecision
        } else {
            Implementation::Balanced
        }

    result = match implementation {
        Implementation::LUT => lut_sqrt(x),
        Implementation::Iterative => newton_raphson_sqrt(x, iterations: 1),
        Implementation::HighPrecision => newton_raphson_sqrt(x, iterations: 2),
        Implementation::Balanced => newton_raphson_sqrt(x, iterations: 1)
    }
}
```

### 2.2 Multi-Dimensional Intent

```skalp
entity Vec3Normalize<intent I: Intent> {
    in v: vec3<fp32>
    out normalized: vec3<fp32>
}

impl<intent I> Vec3Normalize<I> {
    // Choose algorithm based on multiple intent dimensions
    normalized = if I.latency < 5 && I.area > 1000_lut {
        // Fast but large: use RSQRT LUT + multiply
        let inv_len = rsqrt_lut(dot(v, v))
        v * inv_len

    } else if I.accuracy == Accuracy::High {
        // Accurate: full sqrt + divide
        let len = sqrt_accurate(dot(v, v))
        v / len

    } else {
        // Balanced: fast rsqrt + multiply
        let inv_len = rsqrt_fast(dot(v, v))
        v * inv_len
    }
}
```

### 2.3 Intent Constraints

```skalp
entity FIRFilter<const TAPS: nat, intent I: Intent>
where
    // Compile-time constraints on intent
    I.latency >= TAPS,              // Must have enough cycles
    I.throughput >= 1_per_cycle,    // Minimum throughput
{
    in sample: fp32
    out result: fp32
}

impl<const TAPS: nat, intent I> FIRFilter<TAPS, I> {
    result = if I.throughput == 1_per_cycle {
        // Fully unrolled - all taps in parallel
        @unroll
        for i in 0..TAPS {
            sum += coeffs[i] * delay_line[i]
        }

    } else {
        // Time-multiplexed - sequential processing
        @sequential
        for i in 0..TAPS {
            sum += coeffs[i] * delay_line[i]
        }
    }
}
```

---

## 3. Intent Composition and Inheritance

### 3.1 Intent Merging

```skalp
// Merge intents using + operator
const BaseIntent: Intent = Intent {
    latency: auto,
    area: auto,
    optimize: Optimize::Balanced
}

const FastIntent: Intent = BaseIntent + Intent {
    latency: 4_cycles,
    optimize: Optimize::Latency
}
// Result: {latency: 4_cycles, area: auto, optimize: Latency}

const SmallIntent: Intent = BaseIntent + Intent {
    area: 500_lut,
    optimize: Optimize::Area
}
// Result: {latency: auto, area: 500_lut, optimize: Area}
```

### 3.2 Intent Propagation via Type System

```skalp
entity ParentModule<intent I: Intent> {
    in data: stream<fp32>
    out result: stream<fp32>
}

impl<intent I> ParentModule<I> {
    // Child inherits parent's intent automatically
    let sqrt_unit = FP32Sqrt<I> {
        x = data.next(),
        result => intermediate
    }

    // Override with modified intent
    let normalize_unit = Vec3Normalize<I + Intent{accuracy: High}> {
        v = vector_data,
        normalized => result
    }
}
```

### 3.3 Intent Refinement

```skalp
impl<intent I> ComplexPipeline<I> {
    // Refine intent for critical path
    const CriticalIntent: Intent = I + Intent {
        latency: min(I.latency * 0.3),  // Critical path gets 30% of budget
        optimize: Optimize::Latency
    }

    const NonCriticalIntent: Intent = I + Intent {
        latency: min(I.latency * 0.7),  // Non-critical gets 70%
        optimize: Optimize::Area
    }

    // Use refined intents
    let critical = CriticalOp<CriticalIntent> { }
    let non_critical = NonCriticalOp<NonCriticalIntent> { }
}
```

---

## 4. Compile-Time Computation

### 4.1 const fn with Intent

```skalp
// Compile-time function to compute resource usage
const fn compute_resources(intent: Intent, taps: nat) -> Resources {
    if intent.optimize == Optimize::Latency {
        // Fully parallel - need 'taps' multipliers
        Resources {
            dsp: taps,
            lut: 100 * taps,
            ff: 50 * taps
        }
    } else if intent.optimize == Optimize::Area {
        // Sequential - need 1 multiplier
        Resources {
            dsp: 1,
            lut: 200 + 10 * taps,  // Control overhead
            ff: 100
        }
    } else {
        // Balanced - partial unroll
        const UNROLL: nat = min(4, taps)
        Resources {
            dsp: UNROLL,
            lut: 150 * UNROLL,
            ff: 75 * UNROLL
        }
    }
}

entity FIR<const TAPS: nat, intent I: Intent> {
    in sample: fp32
    out result: fp32
} with resources {
    // Declare resources based on intent
    required: compute_resources(I, TAPS)
}
```

### 4.2 Intent-Driven Loop Transformation

```skalp
impl<const N: nat, intent I> MatrixMultiply<N, I> {
    // Compute unroll factor based on intent
    const UNROLL_FACTOR: nat = if I.optimize == Optimize::Latency {
        N  // Fully unroll
    } else if I.optimize == Optimize::Area {
        1  // No unroll
    } else {
        // Balanced - unroll to fit available DSPs
        min(N, I.area / 100)  // Heuristic
    }

    @unroll(factor: UNROLL_FACTOR)
    for i in 0..N {
        for j in 0..N {
            c[i][j] = dot_product(a[i], b_column(j))
        }
    }
}
```

### 4.3 Static Assertions on Intent

```skalp
entity HighPerformanceModule<intent I: Intent> {
    in data: stream<bit<32>>
    out result: stream<bit<32>>
}

impl<intent I> HighPerformanceModule<I> {
    // Compile-time assertion
    static_assert!(I.throughput >= 1_per_cycle,
        "HighPerformanceModule requires throughput >= 1/cycle");

    static_assert!(I.latency <= 10_cycles,
        "HighPerformanceModule cannot meet latency > 10 cycles");

    // Implementation guaranteed to satisfy constraints
    // ...
}
```

---

## 5. Intent Pattern Matching

### 5.1 Match on Intent Properties

```skalp
impl<intent I> AdaptiveSqrt<I> {
    result = match I {
        Intent { optimize: Optimize::Latency, .. } => {
            lut_sqrt(x)
        },
        Intent { optimize: Optimize::Area, accuracy: Accuracy::Low, .. } => {
            shared_sqrt(x)  // Time-multiplexed
        },
        Intent { accuracy: Accuracy::High, .. } => {
            newton_raphson_sqrt(x, iterations: 2)
        },
        _ => {
            newton_raphson_sqrt(x, iterations: 1)
        }
    }
}
```

### 5.2 Intent Guards

```skalp
impl<intent I> FlexibleFFT<I> {
    signal implementation = match I {
        Intent { latency: l, .. } if l < 10 => {
            Implementation::Parallel  // Fast parallel FFT
        },
        Intent { area: a, .. } if a < 1000_lut => {
            Implementation::Sequential  // Small sequential FFT
        },
        Intent { optimize: Optimize::Power, .. } => {
            Implementation::LowPower  // Clock-gated FFT
        },
        _ => {
            Implementation::Balanced
        }
    }

    result = execute_fft(input, implementation)
}
```

---

## 6. Complete Stdlib Example

### 6.1 Unified FP32Sqrt Implementation

```skalp
use std::fp::intrinsics::{lut_sqrt, newton_raphson, cordic_sqrt};

entity FP32Sqrt<intent I: Intent = Intent::default()> {
    in x: fp32
    out result: fp32
}

impl<intent I> FP32Sqrt<I> {
    // Single implementation - compiler generates specialized variants
    result = match I {
        // Ultra-low latency: LUT-based
        Intent { latency: l, .. } if l <= 2 => {
            lut_sqrt(x, precision: I.accuracy)
        },

        // Low latency: Single Newton-Raphson iteration
        Intent { latency: l, .. } if l <= 5 => {
            let guess = fast_rsqrt_guess(x)
            newton_raphson(x, guess, iterations: 1)
        },

        // Medium latency: Two iterations for better accuracy
        Intent { latency: l, .. } if l <= 10 => {
            let guess = fast_rsqrt_guess(x)
            newton_raphson(x, guess, iterations: 2)
        },

        // High accuracy: CORDIC with variable iterations
        Intent { accuracy: Accuracy::High, .. } => {
            cordic_sqrt(x, iterations: 16)
        },

        // Area-optimized: Minimal resources
        Intent { optimize: Optimize::Area, .. } => {
            // Shared sequential implementation
            sequential_sqrt(x)
        },

        // Default balanced
        _ => {
            let guess = fast_rsqrt_guess(x)
            newton_raphson(x, guess, iterations: 1)
        }
    }
}
```

**Generated Code for Different Intents:**

```skalp
// User code 1: Ultra-fast
@intent(latency: 1_cycle)
impl Fast {
    signal r: fp32 = sqrt(x)
}
// Generates: FP32Sqrt<Intent{latency:1}> with lut_sqrt path

// User code 2: Balanced
@intent(optimize: Balanced)
impl Medium {
    signal r: fp32 = sqrt(x)
}
// Generates: FP32Sqrt<Intent{optimize:Balanced}> with 1-iteration NR

// User code 3: High accuracy
@intent(accuracy: High)
impl Precise {
    signal r: fp32 = sqrt(x)
}
// Generates: FP32Sqrt<Intent{accuracy:High}> with CORDIC
```

### 6.2 Unified Vec3Normalize

```skalp
entity Vec3Normalize<intent I: Intent = Intent::default()> {
    in v: vec3<fp32>
    out normalized: vec3<fp32>
}

impl<intent I> Vec3Normalize<I> {
    // Compute dot product first
    signal len_sq: fp32 = dot(v, v)

    // Choose sqrt method based on intent
    signal len: fp32 = if I.latency < 5 {
        // Fast path: rsqrt approximation
        let inv_len = rsqrt_fast<I>(len_sq)
        // Return length = 1/inv_len, but we can skip this
        // and use inv_len directly for normalization
        signal _unused: fp32 = 1.0 / inv_len  // Not used
        1.0 / inv_len
    } else {
        // Accurate path: full sqrt
        sqrt<I>(len_sq)
    }

    // Normalize
    normalized = if I.latency < 5 {
        // Fast path: already have inv_len, multiply directly
        let inv_len = rsqrt_fast<I>(len_sq)
        v * inv_len
    } else {
        // Accurate path: divide by length
        v / len
    }
}
```

### 6.3 Intent-Aware FIFO

```skalp
entity FIFO<
    const DEPTH: nat,
    const WIDTH: nat,
    intent I: Intent = Intent::default()
> {
    in clk: clock
    in wr_en: bit
    in wr_data: bit[WIDTH]
    out full: bit
    in rd_en: bit
    out rd_data: bit[WIDTH]
    out empty: bit
}

impl<const DEPTH, const WIDTH, intent I> FIFO<DEPTH, WIDTH, I> {
    // Choose memory implementation based on intent
    @intent_driven_memory
    signal memory: [bit[WIDTH]; DEPTH] = if I.optimize == Optimize::Area {
        // Use distributed RAM (LUTs)
        @impl(lutram)
        [bit[WIDTH]; DEPTH]
    } else if I.latency <= 2 {
        // Use registers for ultra-low latency
        @impl(registers)
        [bit[WIDTH]; DEPTH]
    } else {
        // Use block RAM for balanced approach
        @impl(bram)
        [bit[WIDTH]; DEPTH]
    }

    // Pointer width based on depth
    const PTR_WIDTH: nat = clog2(DEPTH)
    signal wr_ptr: nat[PTR_WIDTH]
    signal rd_ptr: nat[PTR_WIDTH]
    signal count: nat[clog2(DEPTH+1)]

    // Gray code for CDC if crossing clock domains
    const USE_GRAY: bool = I.has_cdc_constraint

    signal wr_ptr_next = if USE_GRAY {
        binary_to_gray(wr_ptr + 1)
    } else {
        wr_ptr + 1
    }

    // Implementation...
}
```

---

## 7. Advanced Intent Metaprogramming

### 7.1 Intent-Driven Code Generation

```skalp
entity ConvolutionLayer<
    const KERNEL_SIZE: nat,
    const CHANNELS: nat,
    intent I: Intent
> {
    in feature_map: [[fp32; CHANNELS]; IMAGE_SIZE]
    out output: [[fp32; CHANNELS]; IMAGE_SIZE]
}

impl<const KERNEL_SIZE, const CHANNELS, intent I>
    ConvolutionLayer<KERNEL_SIZE, CHANNELS, I>
{
    // Compute optimal parallelization strategy
    const STRATEGY = compute_strategy(I, KERNEL_SIZE, CHANNELS)

    signal result = match STRATEGY {
        Strategy::FullParallel => {
            // Unroll everything - maximum throughput
            @unroll(factor: CHANNELS)
            @unroll(factor: KERNEL_SIZE)
            for ch in 0..CHANNELS {
                for ky in 0..KERNEL_SIZE {
                    for kx in 0..KERNEL_SIZE {
                        output[ch] += input[ch] * weights[ch][ky][kx]
                    }
                }
            }
        },

        Strategy::ChannelParallel => {
            // Parallelize over channels only
            @unroll(factor: CHANNELS)
            for ch in 0..CHANNELS {
                @pipeline(ii: 1)
                for ky in 0..KERNEL_SIZE {
                    for kx in 0..KERNEL_SIZE {
                        output[ch] += input[ch] * weights[ch][ky][kx]
                    }
                }
            }
        },

        Strategy::Sequential => {
            // Minimal area - fully sequential
            @sequential
            for ch in 0..CHANNELS {
                for ky in 0..KERNEL_SIZE {
                    for kx in 0..KERNEL_SIZE {
                        output[ch] += input[ch] * weights[ch][ky][kx]
                    }
                }
            }
        }
    }
}

// Compile-time function to choose strategy
const fn compute_strategy(
    intent: Intent,
    kernel_size: nat,
    channels: nat
) -> Strategy {
    const total_ops = kernel_size * kernel_size * channels

    if intent.optimize == Optimize::Latency && intent.area > total_ops * 100 {
        Strategy::FullParallel
    } else if intent.optimize == Optimize::Throughput && intent.area > channels * 100 {
        Strategy::ChannelParallel
    } else {
        Strategy::Sequential
    }
}
```

### 7.2 Intent Reflection

```skalp
impl<intent I> DebugModule<I> {
    // Reflect on intent at compile-time for debug output
    const INTENT_INFO: &str = format!(
        "Compiled with: latency={}, area={}, opt={}",
        I.latency,
        I.area,
        I.optimize
    )

    // Embed as constant in generated code
    @synthesis_attribute("intent_info", INTENT_INFO)
    signal debug_marker: bit
}
```

### 7.3 Intent Transformation Functions

```skalp
// Transform intent for subcircuits
const fn split_intent(parent: Intent, stages: nat) -> [Intent; stages] {
    const latency_per_stage = parent.latency / stages
    const area_per_stage = parent.area / stages

    [
        parent + Intent { latency: latency_per_stage, area: area_per_stage };
        stages
    ]
}

impl<const STAGES: nat, intent I> Pipeline<STAGES, I> {
    const STAGE_INTENTS = split_intent(I, STAGES)

    @unroll
    for i in 0..STAGES {
        inst stage[i]: PipelineStage<STAGE_INTENTS[i]> {
            input = prev_output
        }
    }
}
```

---

## 8. Comparison: Before and After

### 8.1 Before (Multiple Entities)

```skalp
// Separate entity for each variant - code duplication!
entity FP32SqrtFast {
    in x: fp32
    out result: fp32
}

impl FP32SqrtFast {
    signal guess = rsqrt_guess(x)
    result = newton_raphson(x, guess, iterations: 1)
}

entity FP32SqrtAccurate {
    in x: fp32
    out result: fp32
}

impl FP32SqrtAccurate {
    signal guess = rsqrt_guess(x)
    result = newton_raphson(x, guess, iterations: 2)
}

entity FP32SqrtLUT {
    in x: fp32
    out result: fp32
}

impl FP32SqrtLUT {
    result = lut_sqrt(x)
}

// Need registry to select correct variant
// Need manual maintenance when adding variants
```

### 8.2 After (Single Parameterized Entity)

```skalp
// Single entity - DRY principle!
entity FP32Sqrt<intent I: Intent = Intent::default()> {
    in x: fp32
    out result: fp32
}

impl<intent I> FP32Sqrt<I> {
    // Compile-time selection
    result = if I.latency <= 2 {
        lut_sqrt(x)
    } else if I.latency <= 5 {
        let guess = rsqrt_guess(x)
        newton_raphson(x, guess, iterations: 1)
    } else {
        let guess = rsqrt_guess(x)
        newton_raphson(x, guess, iterations: 2)
    }
}

// Automatic selection via type system
// Adding new variants is just adding new branches
```

---

## 9. Type System Integration

### 9.1 Intent as Generic Bound

```skalp
// Trait that requires specific intent properties
trait LowLatency {
    const REQUIRED_INTENT: Intent = Intent {
        latency: max(5_cycles),
        optimize: Optimize::Latency
    }
}

// Entity that implements the trait
entity FastProcessor<intent I: Intent>
where
    I.satisfies(LowLatency::REQUIRED_INTENT)
{
    // Only accepts intents with latency <= 5 cycles
}

// Usage
@intent(latency: 3_cycles)
impl Example {
    let fast = FastProcessor<CurrentIntent> { }  // ✓ OK

    @intent(latency: 10_cycles)
    let slow = FastProcessor<CurrentIntent> { }  // ❌ ERROR: intent violation
}
```

### 9.2 Intent Inference

```skalp
// Infer intent from context
impl Parent {
    // Explicit intent
    @intent(latency: 4_cycles)
    signal a: fp32 = sqrt(x)

    // Inferred intent from usage
    signal b: fp32 = sqrt(y)  // Uses default intent

    // Inferred from type requirements
    signal critical: CriticalValue<Intent{latency:2}> = process(z)
    // process() infers it needs latency <= 2
}
```

### 9.3 Default Intent

```skalp
// System-wide default intent
const DEFAULT_INTENT: Intent = Intent {
    latency: auto,
    throughput: auto,
    accuracy: Accuracy::Medium,
    area: auto,
    power: auto,
    optimize: Optimize::Balanced
}

// Entity with default intent parameter
entity MyModule<intent I: Intent = DEFAULT_INTENT> {
    // Can be used without specifying intent
}

// Usage
impl Example {
    let m1 = MyModule { }              // Uses DEFAULT_INTENT
    let m2 = MyModule<FastIntent> { }  // Uses custom intent
}
```

---

## 10. Implementation Strategy

### 10.1 Compiler Phases

```rust
// Phase 1: Parse intent type parameters
struct EntityDef {
    name: Ident,
    generics: Vec<GenericParam>,  // Includes `intent I: Intent`
    intent_params: Vec<IntentParam>,
}

// Phase 2: Resolve intent values
fn resolve_intent<'a>(
    context: &Context,
    intent_param: &IntentParam
) -> ResolvedIntent {
    // Look up parent intent
    let parent = context.get_parent_intent();

    // Merge with local intent
    let local = context.get_local_intent();

    merge_intents(parent, local)
}

// Phase 3: Evaluate compile-time conditionals
fn evaluate_intent_conditional(
    cond: &IntentCondition,
    intent: &ResolvedIntent
) -> bool {
    match cond {
        IntentCondition::LessThan(field, value) => {
            intent.get_field(field) < *value
        }
        IntentCondition::Equal(field, value) => {
            intent.get_field(field) == *value
        }
        // ...
    }
}

// Phase 4: Monomorphization
fn monomorphize_entity(
    entity: &EntityDef,
    intent: &ResolvedIntent
) -> MonomorphizedEntity {
    // Generate specialized code based on intent
    let mut specialized = entity.clone();

    // Evaluate all intent-based conditionals
    specialized.body = evaluate_conditionals(
        &entity.body,
        intent
    );

    specialized
}
```

### 10.2 Intent Type Representation

```rust
// Runtime representation (for compile-time computation)
#[derive(Clone, Debug)]
pub struct Intent {
    pub latency: Option<Cycles>,
    pub throughput: Option<PerCycle>,
    pub accuracy: Option<Accuracy>,
    pub area: Option<LUT>,
    pub power: Option<Milliwatts>,
    pub optimize: Optimize,
}

impl Intent {
    pub fn merge(&self, other: &Intent) -> Intent {
        Intent {
            latency: other.latency.or(self.latency),
            throughput: other.throughput.or(self.throughput),
            accuracy: other.accuracy.or(self.accuracy),
            area: other.area.or(self.area),
            power: other.power.or(self.power),
            optimize: other.optimize,
        }
    }

    pub fn satisfies(&self, constraint: &Intent) -> bool {
        // Check if this intent satisfies constraint
        if let (Some(lat), Some(max_lat)) = (self.latency, constraint.latency) {
            if lat > max_lat {
                return false;
            }
        }
        // ... check other fields
        true
    }
}
```

---

## Summary

### Key Benefits

1. **DRY Principle** - Single implementation, not N variants
2. **Compile-Time Specialization** - Zero runtime overhead
3. **Type Safety** - Intent constraints checked by compiler
4. **Composability** - Intents flow through type parameters
5. **Readability** - Clear conditional logic: `if I.latency < 8`
6. **Maintainability** - Add variants by adding branches, not entities
7. **Flexibility** - Mix static and dynamic intent handling

### Comparison Table

| Aspect | Multiple Entities | Intent as Type |
|--------|------------------|----------------|
| **Code Duplication** | High (N variants) | None (1 impl) |
| **Selection** | Registry lookup | Type system |
| **Type Safety** | Manual | Automatic |
| **Extensibility** | Add new entity | Add new branch |
| **Composition** | Manual propagation | Type parameter |
| **Compile-Time** | Limited | Full metaprogramming |

### Example Impact

**Before:** 3 sqrt entities × 3 normalize entities × 3 div entities = **27 combinations**

**After:** 3 parameterized entities = **3 total** (compiler generates needed combinations)

### Next Steps

1. **Extend Type System** - Add `intent` as keyword and type
2. **Parser Support** - Parse `<intent I: Intent>` syntax
3. **Intent Evaluation** - Compile-time intent expression evaluation
4. **Monomorphization** - Generate specialized code per intent
5. **Stdlib Migration** - Convert multi-variant entities to parameterized
6. **Documentation** - Intent metaprogramming guide

**Intent as a first-class type makes SKALP the most powerful HLS system!** 🚀💡
