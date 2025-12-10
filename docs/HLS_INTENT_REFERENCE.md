# SKALP HLS Intent System - Complete Reference

**Transforming SKALP from RTL Replacement to Full HLS Tool**

## Table of Contents

1. [Introduction](#1-introduction)
2. [Memory Optimization Intents](#2-memory-optimization-intents)
3. [Loop Transformation Intents](#3-loop-transformation-intents)
4. [Dataflow and Streaming Intents](#4-dataflow-and-streaming-intents)
5. [Interface Protocol Intents](#5-interface-protocol-intents)
6. [Resource Binding Intents](#6-resource-binding-intents)
7. [Power Optimization Intents](#7-power-optimization-intents)
8. [Clock Domain Intents](#8-clock-domain-intents)
9. [Verification Intents](#9-verification-intents)
10. [Synthesis Strategy Intents](#10-synthesis-strategy-intents)
11. [Complete Examples](#11-complete-examples)

---

## 1. Introduction

### What Makes SKALP a True HLS Tool?

Traditional HLS (Vivado HLS, Intel HLS, Catapult) transform C++ to RTL with compiler directives. SKALP is **hardware-native** but with **HLS-level intent control**.

**The SKALP Advantage:**
```skalp
// Looks like high-level code
result = sqrt(x) + sqrt(y)

// With HLS intents, becomes optimized hardware
@intent(share: sqrt)        // Resource sharing
@intent(pipeline: auto)     // Automatic pipelining
@intent(map: dsp)          // Hardware mapping
result = sqrt(x) + sqrt(y)
```

### Intent Philosophy

1. **Explicit over Implicit** - Intents are contracts, not hints
2. **Composable** - Intents work together hierarchically
3. **Verifiable** - Violations are compile errors
4. **Portable** - Same code, different targets via intent changes

### Intent Syntax

```skalp
// Entity-level intents
entity MyDesign {
    // ...
} with intent {
    throughput: 1_per_cycle,
    latency: max(10_cycles)
}

// Implementation-level intents
@intent(pipeline: auto)
impl MyDesign {
    // ...
}

// Signal/operation-level intents
@intent(share: sqrt)
signal result: fp32 = sqrt(x)
```

---

## 2. Memory Optimization Intents

### 2.1 Memory Banking

**Problem:** Wide arrays accessed in parallel need multiple physical memories.

```skalp
entity ParallelFilter<const TAPS: nat = 8> {
    in clk: clock
    in samples: bit<16>
    out result: bit<32>
}

@intent(memory: {
    coeffs: {
        banking: TAPS,         // Split into 8 banks
        mode: cyclic           // Cyclic address mapping
    }
})
impl ParallelFilter {
    // Single logical array
    signal coeffs: [bit<16>; TAPS]

    // Parallel access - compiler generates banked memories
    signal products: [bit<32>; TAPS]
    for i in 0..TAPS {
        @intent(parallel: unroll)
        products[i] = samples * coeffs[i]  // Each bank accessed simultaneously
    }

    result = products.sum()
}
```

**Generated Hardware:**
- 8 separate block RAMs (one per bank)
- Parallel access without conflicts
- Automatic address mapping

**Intent Options:**
```skalp
@intent(memory: {
    name: {
        banking: N,                    // Number of banks
        mode: cyclic | block,          // Address mapping
        impl: bram | lutram | uram,    // Physical implementation
        depth: auto | N,               // Override depth
        width: auto | N,               // Override width
        ports: 1 | 2                   // Single or dual-port
    }
})
```

### 2.2 Memory Partitioning

```skalp
entity MatrixMultiply<const SIZE: nat = 16> {
    in clk: clock
    in a: [[bit<16>; SIZE]; SIZE]
    in b: [[bit<16>; SIZE]; SIZE]
    out c: [[bit<32>; SIZE]; SIZE]
}

@intent(memory: {
    a: { partition: row, factor: SIZE },    // Each row in separate memory
    b: { partition: column, factor: SIZE }, // Each column in separate memory
    c: { partition: complete }              // Fully partitioned (registers)
})
impl MatrixMultiply {
    // Parallel matrix multiply
    for i in 0..SIZE {
        for j in 0..SIZE {
            @intent(parallel: unroll)
            c[i][j] = dot(a[i], b_col(j))  // All accesses conflict-free
        }
    }
}
```

**Partition Modes:**
- `complete` - Full partitioning into registers
- `row` - Partition by rows (first dimension)
- `column` - Partition by columns (second dimension)
- `block(factor: N)` - Partition into N blocks
- `cyclic(factor: N)` - Cyclic partitioning

### 2.3 Memory Access Patterns

```skalp
@intent(memory: {
    buffer: {
        access_pattern: sequential,    // Hint for optimization
        burst_length: 16,             // Expected burst size
        read_latency: 2_cycles,       // External memory latency
        write_latency: 1_cycle
    }
})
impl StreamProcessor {
    signal buffer: [bit<32>; 1024]

    // Compiler generates burst controller
    @intent(burst: auto)
    for i in 0..1024 {
        buffer[i] = input_stream.next()
    }
}
```

**Access Pattern Hints:**
- `sequential` - Consecutive addresses (enables bursting)
- `random` - Unpredictable (use registers)
- `strided(N)` - Fixed stride access
- `windowed(N)` - Sliding window pattern

### 2.4 Cache and Buffer Management

```skalp
entity ImageProcessor {
    in pixels: stream<bit<24>>
    out filtered: stream<bit<24>>
}

@intent(buffer: {
    line_buffer: {
        impl: shift_register,      // Use shift register for FIFO
        depth: 1920,               // Full HD width
        prefetch: 2                // Prefetch 2 lines
    }
})
impl ImageProcessor {
    // 3-line buffer for 3x3 kernel
    signal line_buffer: [bit<24>; 1920 * 3]

    // Compiler generates efficient line buffer with shift register
    filtered = convolve_3x3(line_buffer)
}
```

---

## 3. Loop Transformation Intents

### 3.1 Loop Unrolling

**Complete Unrolling:**
```skalp
entity VectorAdd<const SIZE: nat = 4> {
    in a: [fp32; SIZE]
    in b: [fp32; SIZE]
    out c: [fp32; SIZE]
}

impl VectorAdd {
    // Unroll completely - all operations in parallel
    @intent(parallel: unroll)
    for i in 0..SIZE {
        c[i] = a[i] + b[i]
    }
}
```

**Generated Hardware:**
```skalp
// No loop - 4 parallel adders
c[0] = a[0] + b[0]
c[1] = a[1] + b[1]
c[2] = a[2] + b[2]
c[3] = a[3] + b[3]
```

**Partial Unrolling:**
```skalp
@intent(parallel: unroll(factor: 4))
for i in 0..256 {
    result[i] = compute(data[i])
}
```

**Generated Hardware:**
- 4 parallel compute units
- Loop executes in 256/4 = 64 iterations
- 4× throughput improvement

### 3.2 Loop Pipelining

```skalp
entity FIR<const TAPS: nat = 64> {
    in clk: clock
    in sample: bit<16>
    out result: bit<32>
}

impl FIR {
    signal delay_line: [bit<16>; TAPS]
    signal coeffs: [bit<16>; TAPS]

    on(clk.rise) {
        // Pipeline with II=1 (initiation interval)
        @intent(pipeline: {
            ii: 1_cycle,           // Accept new input every cycle
            latency: auto          // Compiler determines latency
        })
        for i in 0..TAPS {
            result += delay_line[i] * coeffs[i]
        }
    }
}
```

**Pipelining Options:**
```skalp
@intent(pipeline: {
    ii: N_cycles,              // Initiation interval (throughput)
    latency: auto | N_cycles,  // Total latency
    rewind: true | false,      // Allow overlapping iterations
    flush: auto | manual       // Pipeline flush control
})
```

### 3.3 Loop Merging and Flattening

**Loop Merging:**
```skalp
@intent(loop: merge)
impl Example {
    // Two sequential loops
    for i in 0..N {
        a[i] = input[i] * 2
    }

    for i in 0..N {
        b[i] = input[i] + 1
    }
}
```

**Generated:** Single merged loop with both operations.

**Loop Flattening:**
```skalp
@intent(loop: flatten)
for i in 0..M {
    for j in 0..N {
        result[i][j] = compute(i, j)
    }
}
```

**Generated:** Single loop with `M*N` iterations (enables better pipelining).

### 3.4 Loop Tiling

```skalp
entity MatrixMultiply<const SIZE: nat = 1024> {
    in a: [[fp32; SIZE]; SIZE]
    in b: [[fp32; SIZE]; SIZE]
    out c: [[fp32; SIZE]; SIZE]
}

@intent(loop: {
    tile: {
        i: 32,    // Tile rows by 32
        j: 32,    // Tile columns by 32
        k: 32     // Tile depth by 32
    }
})
impl MatrixMultiply {
    for i in 0..SIZE {
        for j in 0..SIZE {
            for k in 0..SIZE {
                c[i][j] += a[i][k] * b[k][j]
            }
        }
    }
}
```

**Benefits:**
- Better cache locality
- Reduced memory bandwidth
- Improved DSP utilization

---

## 4. Dataflow and Streaming Intents

### 4.1 Dataflow Channels

```skalp
entity ImagePipeline {
    in pixels: stream<rgb888>
    out processed: stream<rgb888>
}

@intent(dataflow: {
    mode: pipeline,           // Pipeline dataflow
    channel_depth: 4,         // FIFO depth between stages
    blocking: auto            // Auto-insert backpressure
})
impl ImagePipeline {
    flow {
        let grayscale = pixels
            |> rgb_to_gray()

        let blurred = grayscale
            |> gaussian_blur()

        let edges = blurred
            |> sobel_edges()

        processed = edges
    }
}
```

**Dataflow Modes:**
- `pipeline` - Stages execute in pipeline (best throughput)
- `sequential` - Stages execute in sequence (minimal area)
- `parallel` - Stages execute in parallel (best latency)

### 4.2 Stream Buffering

```skalp
@intent(stream: {
    input_buffer: 16,         // 16-deep input buffer
    output_buffer: 16,        // 16-deep output buffer
    flow_control: ready_valid // Handshaking protocol
})
entity Processor {
    in data: stream<bit<32>>
    out result: stream<bit<32>>
}
```

### 4.3 Burst Transfer Optimization

```skalp
@intent(burst: {
    read_length: 64,          // Burst read 64 elements
    write_length: 64,         // Burst write 64 elements
    alignment: 64_bytes       // Align to cache line
})
impl DMAController {
    for i in 0..1024 step 64 {
        // Compiler generates burst logic
        local_buffer[0..64] = ddr_read(base_addr + i, length: 64)
        process(local_buffer)
        ddr_write(dest_addr + i, local_buffer, length: 64)
    }
}
```

### 4.4 Producer-Consumer Patterns

```skalp
@intent(dataflow: producer_consumer)
impl VideoCodec {
    // Producer: Read frames
    @intent(task: producer)
    flow produce_frames {
        for frame in video_stream {
            yield frame_buffer.push(frame)
        }
    }

    // Consumer: Encode frames
    @intent(task: consumer)
    flow encode_frames {
        loop {
            let frame = frame_buffer.pop()
            let encoded = h264_encode(frame)
            yield output.push(encoded)
        }
    }
}
```

---

## 5. Interface Protocol Intents

### 5.1 AXI4 Interface Generation

```skalp
entity DMAEngine {
    in clk: clock
    out axi: ~AXI4
}

@intent(interface: {
    axi: {
        protocol: axi4,
        burst: incr,                  // Incrementing burst
        max_burst_length: 256,
        outstanding_transactions: 4,
        reorder: false
    }
})
impl DMAEngine {
    // Compiler generates full AXI4 interface logic
    axi.read_burst(addr, length: 256)
}
```

### 5.2 Streaming Interface

```skalp
@intent(interface: {
    input: {
        protocol: axis,              // AXI-Stream
        tkeep: true,                // Include TKEEP signal
        tlast: true,                // Include TLAST signal
        tuser_width: 8              // 8-bit TUSER
    }
})
entity StreamProcessor {
    in data: stream<bit<32>>
    out result: stream<bit<32>>
}
```

### 5.3 Handshake Protocol

```skalp
@intent(handshake: {
    input: {
        mode: ready_valid,          // Ready/valid handshake
        registered: true,           // Register outputs
        timeout: 1000_cycles        // Timeout threshold
    }
})
entity AsyncFIFO {
    in wr_data: bit<32>
    in wr_valid: bit
    out wr_ready: bit
}
```

### 5.4 Memory-Mapped Interface

```skalp
entity Accelerator {
    in clk: clock
    out bus: ~MemoryMapped<addr_width: 32, data_width: 64>
}

@intent(interface: {
    bus: {
        protocol: axi4_lite,
        registers: {
            control: 0x0000,        // Auto-generate CSRs
            status: 0x0004,
            config: 0x0008
        },
        access_latency: 1_cycle
    }
})
impl Accelerator {
    // Compiler generates register file + bus interface
}
```

---

## 6. Resource Binding Intents

### 6.1 DSP Block Mapping

```skalp
entity MACArray<const SIZE: nat = 16> {
    in a: [bit<18>; SIZE]
    in b: [bit<18>; SIZE]
    out result: [bit<48>; SIZE]
}

@intent(resource: {
    multiply: {
        impl: dsp48,               // Use DSP48 blocks
        mode: mac,                 // Multiply-accumulate mode
        pipeline_stages: 3         // 3-stage pipeline
    }
})
impl MACArray {
    for i in 0..SIZE {
        @intent(map: dsp)
        result[i] = a[i] * b[i] + result[i]  // MAP to DSP48
    }
}
```

### 6.2 Block RAM Utilization

```skalp
@intent(resource: {
    buffer: {
        impl: bram,                // Use block RAM
        primitive: simple_dual_port,
        read_latency: 2_cycles,
        write_mode: read_first
    }
})
impl FrameBuffer {
    signal buffer: [rgb888; 1920 * 1080]
}
```

**Block RAM Modes:**
- `simple_dual_port` - One read, one write port
- `true_dual_port` - Two read/write ports
- `single_port` - One port for read/write
- `rom` - Read-only memory

### 6.3 LUT-Based Logic

```skalp
@intent(resource: {
    lut_mapping: true,            // Force LUT implementation
    lut_size: 6,                  // Target LUT-6
    cascade: false                // No LUT cascading
})
impl SmallLogic {
    result = complex_boolean_function(inputs)
}
```

### 6.4 URAM Usage (UltraScale+)

```skalp
@intent(resource: {
    large_buffer: {
        impl: uram,                // UltraRAM (288Kb blocks)
        cascade: 4,                // Cascade 4 URAMs
        pipeline: true             // Add pipeline registers
    }
})
impl LargeMemory {
    signal large_buffer: [bit<64>; 1_000_000]
}
```

---

## 7. Power Optimization Intents

SKALP provides two complementary approaches for power intent specification:
1. **`@intent(power: {...})`** - High-level intent blocks for entity/module-level power configuration
2. **`#[attribute]`** - Signal-level attributes for fine-grained power control (NEW in Dec 2025)

### 7.1 Signal-Level Power Attributes (NEW)

**Retention** - Preserve state during power-down:
```skalp
// Basic retention
#[retention]
signal saved_state: bit[32]

// With explicit strategy
#[retention(strategy = balloon_latch)]
signal critical_reg: bit[16]

#[retention(strategy = shadow_register)]
signal config_backup: bit[64]
```

**Isolation** - Control signals when power domain is off:
```skalp
// Clamp output to 0 when domain powered off
#[isolation(clamp = low)]
signal isolated_output: bit[32]

// Clamp to 1
#[isolation(clamp = high, enable = "iso_en")]
signal active_low_sig: bit

// Hold last value
#[isolation(clamp = latch)]
signal hold_signal: bit[16]
```

**Level Shifters** - Voltage domain crossing:
```skalp
#[level_shift(from = "VDD_CORE", to = "VDD_IO")]
signal voltage_crossing: bit[16]

#[level_shift(shifter_type = low_to_high)]
signal core_to_io_signal: bit
```

**Power Domain Crossing**:
```skalp
#[pdc(from = 'core, to = 'io, isolation = clamp_low)]
signal power_domain_cross: bit[32]
```

**Generated SystemVerilog:**
```systemverilog
// From #[retention]
(* RETAIN = "TRUE" *)
(* preserve = "true" *)
(* DONT_TOUCH = "TRUE" *)
reg [31:0] saved_state;

// From #[isolation(clamp = low)]
(* DONT_TOUCH = "TRUE" *)
wire [31:0] isolated_output_isolated;
assign isolated_output_isolated = iso_en ? 32'b0 : isolated_output;
```

### 7.2 Clock Gating

```skalp
@intent(power: {
    clock_gating: auto,           // Automatic clock gating
    gate_threshold: 0.3           // Gate if <30% utilization
})
entity PowerAwareProcessor {
    in clk: clock
    in enable: bit
}

impl PowerAwareProcessor {
    // Compiler inserts clock gating cells
    on(clk.rise) {
        if enable {
            // Active logic
        }
        // Clock gated when enable = 0
    }
}
```

### 7.3 Multi-Voltage Domains

```skalp
@intent(power: {
    voltage_scaling: {
        fast_path: 1.0V,          // Critical path at nominal voltage
        slow_path: 0.8V           // Non-critical at low voltage
    }
})
impl MixedVoltageDomain {
    @intent(voltage: 1.0V)
    signal critical: bit<32> = fast_computation()

    @intent(voltage: 0.8V)
    signal non_critical: bit<32> = slow_computation()
}
```

### 7.4 Power Gating (Entity-Level)

For entity-level power gating configuration:
```skalp
@intent(power: {
    power_gating: {
        domains: [compute_array, memory_bank],
        retention: true,          // Retain state during power-down
        wakeup_latency: 10_cycles
    }
})
impl PowerManagedAccelerator {
    signal compute_array: ComputeArray
    signal memory_bank: [bit<32>; 1024]
}
```

Combined with signal-level attributes:
```skalp
impl PowerManagedAccelerator {
    // These signals have retention during power gating
    #[retention]
    signal compute_array_state: bit[32]

    #[retention(strategy = shadow_register)]
    signal memory_bank_ptr: bit[16]

    // Isolation for outputs crossing power domains
    #[isolation(clamp = low)]
    signal domain_output: bit[64]
}
```

### 7.5 Dynamic Frequency Scaling

```skalp
@intent(power: {
    dvfs: {
        modes: [
            {name: "high_perf", freq: 500MHz, voltage: 1.0V},
            {name: "balanced",  freq: 250MHz, voltage: 0.9V},
            {name: "low_power", freq: 100MHz, voltage: 0.8V}
        ],
        transition_latency: 100_cycles
    }
})
entity AdaptiveProcessor {
    in perf_mode: PerformanceMode
}
```

### 7.6 Complete Power-Aware Design Example

```skalp
entity PowerAwareSubsystem {
    in clk: clock
    in rst: reset
    in power_enable: bit
    in data_in: bit[32]
    out data_out: bit[32]
}

@intent(power: {
    clock_gating: auto,
    power_gating: {
        retention: true,
        wakeup_latency: 5_cycles
    }
})
impl PowerAwareSubsystem {
    // Register file with retention - preserved during power-down
    #[retention]
    #[memory(depth = 8, width = 32, style = register)]
    signal reg_file: bit[32][8]

    // Configuration registers - must survive power cycles
    #[retention(strategy = balloon_latch)]
    signal config_active: bit[8]

    // Output isolation - clamp to 0 when domain is off
    #[isolation(clamp = low, enable = "power_enable")]
    signal internal_result: bit[32]

    // CDC synchronized input from always-on domain
    #[cdc(sync_stages = 2)]
    signal power_enable_sync: bit

    on(clk.rise) {
        if (rst) {
            reg_file <= {0, 0, 0, 0, 0, 0, 0, 0}
        } else if (power_enable_sync) {
            // Normal operation
            internal_result <= compute(data_in, reg_file)
        }
    }

    data_out = internal_result
}
```

---

## 8. Clock Domain Intents

### 8.1 Clock Domain Crossing

```skalp
@intent(cdc: {
    synchronizer: two_ff,         // Two-FF synchronizer
    metastability_mtbf: 1e12,    // Mean time between failures
    constraint: max_delay(5ns)
})
signal sync_signal: bit = async_input
```

### 8.2 Asynchronous FIFO

```skalp
@intent(cdc: {
    fifo: {
        depth: 16,
        gray_code: true,          // Use Gray code for pointers
        almost_full: 12,          // Assert almost_full at 12 entries
        almost_empty: 4
    }
})
entity AsyncFIFO {
    in wr_clk: clock
    in rd_clk: clock
    in wr_data: bit<32>
    out rd_data: bit<32>
}
```

### 8.3 Clock Multiplexing

```skalp
@intent(clock: {
    mux: {
        sources: [clk_100mhz, clk_200mhz],
        glitch_free: true,        // Glitch-free switching
        hold_cycles: 4            // Hold select for 4 cycles
    }
})
signal system_clk: clock = if high_speed { clk_200mhz } else { clk_100mhz }
```

### 8.4 Clock Domain Isolation

```skalp
@intent(clock: {
    domain_isolation: true,       // Isolate clock domains
    verify: formal                // Formal verification of CDC
})
impl MultiClockSystem {
    @intent(clock_domain: domain_a)
    signal data_a: bit<32>

    @intent(clock_domain: domain_b)
    signal data_b: bit<32>

    // Compiler checks CDC safety
    data_b = sync(data_a)  // Requires explicit synchronizer
}
```

---

## 9. Verification Intents

### 9.1 Assertion Binding

```skalp
@intent(verify: {
    assertions: enabled,
    coverage: statement | branch | toggle,
    formal: bounded_model_check(depth: 20)
})
impl SafeProcessor {
    assert!(valid → ready, "Ready must follow valid")

    @intent(cover)
    signal corner_case: bit = (a == max_value) && (b == 0)
}
```

### 9.2 Protocol Checking

```skalp
@intent(verify: {
    protocol: axi4,
    check: [
        "no_outstanding_overflow",
        "burst_alignment",
        "response_ordering"
    ]
})
entity AXIController {
    out axi: ~AXI4
}
```

### 9.3 Formal Properties

```skalp
@intent(verify: {
    formal: {
        tool: sby,                // SymbiYosys
        engine: smtbmc,
        depth: 50
    }
})
impl FormallyVerified {
    requirement!(always(valid → eventually(ready)),
        "Valid request must eventually get ready")

    requirement!(mutex(state_a, state_b),
        "States A and B are mutually exclusive")
}
```

---

## 10. Synthesis Strategy Intents

### 10.1 Optimization Goals

```skalp
@intent(optimize: {
    primary: throughput,          // Optimize for throughput
    secondary: power,             // Then minimize power
    constraints: {
        area: < 50_klut,         // Hard area constraint
        frequency: > 200MHz       // Hard frequency constraint
    }
})
entity Accelerator { }
```

**Optimization Targets:**
- `latency` - Minimize cycle count
- `throughput` - Maximize samples/sec
- `area` - Minimize resource usage
- `power` - Minimize dynamic power
- `frequency` - Maximize clock frequency
- `balanced` - Balance all metrics

### 10.2 Retiming

```skalp
@intent(synthesis: {
    retiming: {
        enabled: true,
        forward: true,            // Forward retiming
        backward: true,           // Backward retiming
        goal: min_period          // Minimize clock period
    }
})
impl DeepPipeline {
    result = stage1 → stage2 → stage3 → stage4
}
```

### 10.3 Resource Sharing

```skalp
@intent(synthesis: {
    sharing: {
        arithmetic: true,         // Share adders, multipliers
        memory: true,             // Share memory blocks
        control: false            // Don't share FSMs
    }
})
impl SharedResources {
    // Two paths share hardware when not active simultaneously
    signal path1: bit<32> = compute_a(input_a)
    signal path2: bit<32> = compute_a(input_b)  // Shares compute_a logic
}
```

### 10.4 Technology Mapping

```skalp
@intent(synthesis: {
    target: {
        vendor: xilinx,
        family: ultrascale_plus,
        device: zu9eg,
        primitives: [dsp48e2, uram288, lutram]
    }
})
entity TargetSpecific { }
```

### 10.5 Pipeline Style Control

Control how pipelining is handled at the module or flow block level:

```skalp
@intent(pipeline_style: auto)         // Compiler decides based on timing analysis
@intent(pipeline_style: combinational) // Fully combinational - no pipeline registers
@intent(pipeline_style: manual)       // Explicit manual stages via |> operator
@intent(pipeline_style: retimed)      // Auto-retiming - compiler inserts registers
```

**Pipeline Style Options:**

| Style | Description | Use Case |
|-------|-------------|----------|
| `auto` | Compiler decides based on timing analysis | General purpose, let tools optimize |
| `combinational` | Fully combinational, no pipeline registers | Single-cycle operations, low latency |
| `manual` | Explicit pipeline stages via `\|>` operator | Precise control over pipeline structure |
| `retimed` | Automatic register insertion for target frequency | High-frequency designs, complex logic |

**Example: Manual Pipeline with Flow Blocks**
```skalp
@intent(pipeline_style: manual)
impl DataProcessor {
    on(clk.rise) {
        flow {
            input_data
            |> stage1_compute()   // Pipeline register inserted after stage1
            |> stage2_compute()   // Pipeline register inserted after stage2
            |> output
        }
    }
}
```

**Example: Auto-Retiming for High Frequency**
```skalp
@intent(pipeline_style: retimed)
@intent(optimize: {constraints: {frequency: > 500MHz}})
impl HighSpeedALU {
    // Compiler automatically inserts pipeline stages to meet timing
    result = (a * b) + (c * d)
}
```

---

## 11. Complete Examples

### 11.1 High-Throughput FFT Engine

```skalp
use std::complex::Complex;
use std::fp::math::sqrt;

entity FFT<const N: nat = 1024> {
    in clk: clock
    in rst: reset
    in samples: stream<Complex<fp32>>
    out spectrum: stream<Complex<fp32>>
}

@intent(optimize: throughput)
@intent(dataflow: pipeline)
@intent(power: {clock_gating: auto})
impl FFT {
    // Twiddle factors in BRAM
    @intent(memory: {
        twiddles: {
            impl: bram,
            init: precompute_twiddles(N),
            ports: 2              // Dual-port for parallel access
        }
    })
    signal twiddles: [Complex<fp32>; N/2]

    // Banking for parallel butterfly access
    @intent(memory: {
        buffer: {
            banking: 8,           // 8-way banking
            mode: cyclic
        }
    })
    signal buffer: [Complex<fp32>; N]

    flow {
        // Pipelined FFT stages
        let stage_out = samples

        @intent(loop: unroll)
        for stage in 0..log2(N) {
            @intent(pipeline: {ii: 1, latency: auto})
            stage_out = stage_out
                |> butterfly_stage(twiddles, stage)
                |> reorder()
        }

        spectrum = stage_out
    }
}

// Butterfly computation with DSP mapping
@intent(resource: {multiply: {impl: dsp48, pipeline_stages: 3}})
@intent(share: multiply)
entity ButterflyStage {
    in a: Complex<fp32>
    in b: Complex<fp32>
    in twiddle: Complex<fp32>
    out x: Complex<fp32>
    out y: Complex<fp32>
}

impl ButterflyStage {
    signal b_rotated: Complex<fp32> = b * twiddle  // DSP48 multiply

    x = a + b_rotated
    y = a - b_rotated
}
```

### 11.2 Video Processing Pipeline

```skalp
entity VideoProcessor {
    in clk: clock
    in pixel_stream: stream<rgb888>
    out processed_stream: stream<rgb888>
}

@intent(dataflow: {
    mode: pipeline,
    channel_depth: 1920,      // One line buffer between stages
    blocking: auto
})
@intent(optimize: {
    primary: throughput,
    constraints: {
        frequency: 150MHz,    // 1080p @ 60fps
        power: < 5W
    }
})
impl VideoProcessor {
    flow {
        // Stage 1: Color space conversion
        @intent(parallel: unroll)
        @intent(map: dsp)
        let yuv = pixel_stream
            |> rgb_to_yuv()

        // Stage 2: Gaussian blur (memory-intensive)
        @intent(memory: {
            line_buffer: {
                impl: uram,
                depth: 1920 * 3   // 3-line buffer
            }
        })
        @intent(loop: {tile: {x: 16, y: 16}})
        let blurred = yuv
            |> gaussian_blur_5x5()

        // Stage 3: Edge detection
        @intent(pipeline: {ii: 1})
        @intent(power: {clock_gating: auto})
        let edges = blurred
            |> sobel_operator()

        // Stage 4: Color grading
        @intent(resource: {lut_table: {impl: lutram, depth: 256}})
        processed_stream = edges
            |> apply_lut()
    }
}
```

### 11.3 Machine Learning Inference Engine

```skalp
entity ConvLayer<
    const IN_CH: nat = 64,
    const OUT_CH: nat = 128,
    const KERNEL: nat = 3
> {
    in clk: clock
    in feature_map: stream<[fp16; IN_CH]>
    out output_map: stream<[fp16; OUT_CH]>
}

@intent(optimize: {
    primary: throughput,
    secondary: power,
    constraints: {area: < 200_dsp}
})
@intent(dataflow: pipeline)
impl ConvLayer {
    // Weights in block RAM with partitioning
    @intent(memory: {
        weights: {
            partition: column,    // Partition by output channel
            factor: OUT_CH,
            impl: bram,
            init: load_weights("conv_weights.bin")
        }
    })
    signal weights: [[[fp16; KERNEL]; KERNEL]; IN_CH * OUT_CH]

    // Parallel convolution with DSP blocks
    @intent(parallel: unroll(factor: 16))   // 16-way parallelism
    @intent(pipeline: {ii: 1})
    for out_ch in 0..OUT_CH {
        @intent(resource: {mac: {impl: dsp48, mode: fp16_mult_add}})
        @intent(loop: flatten)
        signal acc: fp16 = 0

        for in_ch in 0..IN_CH {
            for ky in 0..KERNEL {
                for kx in 0..KERNEL {
                    acc += feature_map[in_ch] * weights[out_ch][in_ch][ky][kx]
                }
            }
        }

        output_map[out_ch] = activation(acc)
    }
}

// Activation function with resource sharing
@intent(share: exp)
@intent(resource: {exp: {impl: lut_approximation, accuracy: 0.01}})
entity Activation {
    in x: fp16
    out y: fp16
}

impl Activation {
    // Sigmoid: 1 / (1 + exp(-x))
    y = 1.0 / (1.0 + exp(-x))
}
```

### 11.4 AXI DMA Controller

```skalp
entity DMAController {
    in clk: clock
    in rst: reset

    // AXI Master interface
    out axi_master: ~AXI4

    // Control interface
    in start: bit
    in src_addr: bit<32>
    in dst_addr: bit<32>
    in length: bit<32>
    out done: bit
}

@intent(interface: {
    axi_master: {
        protocol: axi4,
        data_width: 512,           // 64-byte wide bus
        burst: incr,
        max_burst_length: 256,
        outstanding_reads: 4,
        outstanding_writes: 4
    }
})
@intent(optimize: throughput)
impl DMAController {
    // Burst optimization
    @intent(burst: {
        read_length: 256,          // Full burst
        write_length: 256,
        alignment: 64_bytes,       // Cache line aligned
        overlap: true              // Overlap read/write
    })

    // Double buffering for continuous transfer
    @intent(memory: {
        read_buffer: {impl: uram, double_buffer: true},
        write_buffer: {impl: uram, double_buffer: true}
    })
    signal read_buffer: [bit<512>; 256]
    signal write_buffer: [bit<512>; 256]

    // Dataflow with overlapped operations
    @intent(dataflow: {
        mode: parallel,
        overlap: [read, write]
    })
    flow {
        // Read burst
        @intent(task: reader)
        signal read_data = axi_master.read_burst(
            addr: src_addr,
            length: min(length, 256 * 64)
        )

        read_buffer[0..256] = read_data

        // Write burst (parallel with read)
        @intent(task: writer)
        axi_master.write_burst(
            addr: dst_addr,
            data: write_buffer[0..256],
            length: min(length, 256 * 64)
        )
    }

    done = (bytes_transferred == length)
}
```

---

## Summary: SKALP as a Complete HLS Tool

### What Makes This HLS, Not Just RTL?

| Feature | Traditional RTL | Traditional HLS | SKALP with Intents |
|---------|----------------|-----------------|---------------------|
| **Abstraction** | Register-level | Behavioral C/C++ | Hardware-native + HLS intents |
| **Memory Opt** | Manual banking | Automatic (opaque) | Explicit intent control |
| **Pipelining** | Manual | Pragma-based | Intent-based contracts |
| **Resource Bind** | Inference/manual | Automatic (limited) | Explicit mapping control |
| **Verification** | Separate flow | Limited | Integrated with intents |
| **Predictability** | High | Low (surprises) | High (explicit contracts) |

### The SKALP Philosophy

1. **Hardware-First** - You write hardware, not C++
2. **Intent-Driven** - Optimization is explicit, not inferred
3. **Composable** - Intents work together hierarchically
4. **Verifiable** - Compiler enforces intent contracts
5. **Portable** - Same code, different optimizations via intents

### Key Advantages Over Traditional HLS

✅ **No Surprises** - Intents are contracts, violations are errors
✅ **Full Control** - Explicit resource binding, not inference
✅ **Type Safety** - Hardware types (fp32, vec3<T>), not C++ float
✅ **Composable** - Intents at entity/impl/signal level
✅ **Verifiable** - Integrated verification intents
✅ **Debuggable** - Direct mapping to hardware, not multi-level IR

### When to Use What

**Use Traditional RTL when:**
- You need cycle-exact control
- Design is small and manageable
- Existing IP integration

**Use Traditional HLS when:**
- Algorithm exploration from C++
- No hardware expertise on team
- Willing to accept unpredictability

**Use SKALP with Intents when:**
- You want HLS productivity with RTL predictability
- You need both abstraction and control
- You want verifiable, composable optimization
- You're building complex, high-performance systems

---

## Next Steps

1. **Implement Intent Parser** - Extend SKALP parser to handle all intent forms
2. **Intent Validation** - Compile-time checking of intent contracts
3. **Optimization Passes** - Implement intent-driven transformations in MIR/LIR
4. **Target Backends** - Generate optimized code per vendor (Xilinx, Intel, ASIC)
5. **Intent Profiler** - Tool to suggest intents based on synthesis results
6. **Intent Library** - Pre-defined intent sets for common patterns

**SKALP is now a complete HLS tool - hardware-native with high-level intent control!** 🚀
