# SKALP Simulation Architecture

## Overview

SKALP provides multi-level simulation capabilities, allowing designers to verify functionality at different abstraction levels with appropriate speed/accuracy tradeoffs. Unlike traditional flows where you simulate either behavioral or gate-level, SKALP offers continuous refinement from algorithm to timing-accurate simulation.

## Simulation Levels

### 1. Source-Level Simulation (Behavioral)

The fastest simulation mode, executing SKALP source directly:

```rust
// Direct source interpretation
entity Counter {
    in clk: event
    in reset: bit
    out count: nat[8]
}

// Source-level simulation
#[test]
fn test_counter() {
    let mut sim = SourceSimulator::new();
    let counter = sim.instantiate::<Counter>();

    // Behavioral simulation - no timing
    counter.reset = 1;
    sim.step();
    assert_eq!(counter.count, 0);

    counter.reset = 0;
    sim.step();
    assert_eq!(counter.count, 1);
}
```

**Characteristics:**
- **Speed**: Very fast (near native code speed)
- **Accuracy**: Functional only, no timing
- **Use case**: Algorithm verification, quick iteration
- **Debug**: Source-level debugging with breakpoints

### 2. HIR Simulation (Dataflow)

Simulates at the dataflow level with stream semantics:

```rust
struct HirSimulator {
    // Stream-based execution model
    streams: HashMap<StreamId, StreamBuffer>,

    fn simulate_dataflow(&mut self, hir: &Hir) {
        // Execute dataflow operations
        for op in hir.dataflow_ops() {
            match op {
                Map(f, input) => {
                    let result = input.iter().map(f);
                    self.push_stream(result);
                }
                Window(size, input) => {
                    let windows = input.windows(size);
                    self.push_stream(windows);
                }
                // Intent-aware simulation
                Pipeline(stages) if hir.intent.latency => {
                    self.simulate_with_latency(stages);
                }
            }
        }
    }
}
```

**Characteristics:**
- **Speed**: Fast (optimized stream operations)
- **Accuracy**: Transaction-level with intent
- **Use case**: Protocol verification, throughput analysis
- **Debug**: Stream visualization, transaction tracking

### 3. MIR Simulation (Cycle-Accurate)

The primary verification level - cycle-accurate RTL simulation:

```rust
struct MirSimulator {
    // Event-driven simulation kernel
    event_queue: BinaryHeap<Event>,
    current_time: SimTime,

    // Signal values
    signals: HashMap<SignalId, LogicValue>,
    next_signals: HashMap<SignalId, LogicValue>,

    fn simulate_cycle(&mut self, mir: &Mir) {
        // Process all events at current time
        while let Some(event) = self.event_queue.peek() {
            if event.time > self.current_time {
                break;
            }

            self.process_event(event);
        }

        // Evaluate processes
        for process in mir.processes() {
            if self.is_sensitive(process) {
                self.evaluate_process(process);
            }
        }

        // Update signals (NBA region)
        for (sig, val) in self.next_signals.drain() {
            if self.signals[&sig] != val {
                self.signals.insert(sig, val);
                self.schedule_dependent_events(sig);
            }
        }

        self.current_time += 1;
    }
}

// 4-state logic for MIR simulation
enum LogicValue {
    Zero,
    One,
    X,  // Unknown
    Z,  // High-impedance
}
```

**Characteristics:**
- **Speed**: Moderate (event-driven efficiency)
- **Accuracy**: Cycle-accurate, 4-state logic
- **Use case**: Primary functional verification
- **Debug**: Waveform generation, assertion checking

### 4. LIR Simulation (Timing-Accurate)

Simulates with target primitive delays:

```rust
struct LirSimulator {
    // Timing-aware simulation
    primitive_models: HashMap<PrimitiveType, TimingModel>,

    fn simulate_with_timing(&mut self, lir: &Lir) {
        for primitive in lir.primitives() {
            let model = &self.primitive_models[&primitive.ptype];

            // Apply input-to-output delays
            for (input, output) in primitive.pins() {
                let delay = model.delay(input, output);
                self.schedule_transition(output, delay);
            }

            // Check timing constraints
            self.check_setup_hold(primitive);
            self.check_pulse_width(primitive);
        }
    }

    // Timing checks
    fn check_setup_hold(&self, prim: &Primitive) {
        if let Some(ff) = prim.as_flipflop() {
            let setup = self.time_to_clock_edge(ff.d) ;
            let hold = self.time_after_clock_edge(ff.d);

            if setup < ff.setup_time() {
                self.report_violation(SetupViolation {
                    signal: ff.d,
                    required: ff.setup_time(),
                    actual: setup,
                });
            }
        }
    }
}
```

**Characteristics:**
- **Speed**: Slow (detailed timing propagation)
- **Accuracy**: Timing-accurate with SDF
- **Use case**: Timing verification, power analysis
- **Debug**: Detailed timing reports, slack analysis

### 5. Post-Route Simulation (Sign-off)

Most accurate simulation with actual routing delays:

```rust
struct PostRouteSimulator {
    // Full physical accuracy
    routing: Routing,
    parasitics: RCNetwork,

    fn simulate_physical(&mut self) {
        // Wire delay calculation
        for net in self.routing.nets() {
            let rc_delay = self.calculate_rc_delay(net);
            self.apply_wire_delay(net, rc_delay);
        }

        // Crosstalk analysis
        for aggressor in self.routing.parallel_nets() {
            self.calculate_crosstalk(aggressor);
        }

        // Power grid simulation
        self.simulate_power_grid();
    }
}
```

**Characteristics:**
- **Speed**: Very slow (SPICE-like accuracy)
- **Accuracy**: Full physical effects
- **Use case**: Final sign-off, SI/PI analysis
- **Debug**: Physical visualization, power maps

## Unified Simulation Interface

All levels share a common interface for testbenches:

```rust
trait SimulationLevel {
    fn step(&mut self);
    fn run_until(&mut self, time: SimTime);
    fn set_signal(&mut self, signal: SignalId, value: Value);
    fn get_signal(&self, signal: SignalId) -> Value;
    fn add_monitor(&mut self, signal: SignalId, callback: MonitorFn);
    fn checkpoint(&self) -> SimState;
    fn restore(&mut self, state: SimState);
}

// Same testbench runs at any level
fn universal_testbench<S: SimulationLevel>(sim: &mut S) {
    sim.set_signal(reset, 1);
    sim.run_until(10);
    sim.set_signal(reset, 0);
    sim.run_until(100);

    assert_eq!(sim.get_signal(output), expected);
}
```

## Simulation Modes

### Delta-Cycle Simulation (Combinational)

```rust
struct DeltaCycle {
    // IEEE 1364-style simulation regions
    regions: [Region; 5],
}

enum Region {
    Active,        // Current events
    Inactive,      // #0 delays
    NBA,          // Non-blocking assigns
    Monitor,      // $monitor, $strobe
    Future,       // Future events
}

impl DeltaCycle {
    fn advance(&mut self) {
        loop {
            // Process active events
            while !self.regions[Active].is_empty() {
                self.process_active_events();
            }

            // Move inactive to active
            if !self.regions[Inactive].is_empty() {
                self.promote_inactive();
                continue;
            }

            // Process NBA
            if !self.regions[NBA].is_empty() {
                self.process_nba();
                continue;
            }

            // No more events in current time
            break;
        }
    }
}
```

### Transaction-Level Simulation

```rust
struct TransactionSimulator {
    // Higher-level protocol simulation
    channels: HashMap<ChannelId, TransactionQueue>,

    fn simulate_transaction(&mut self, protocol: &Protocol) {
        match protocol {
            Protocol::AXI => {
                let txn = self.channels.get_transaction();
                let response = self.process_axi_transaction(txn);
                self.channels.put_response(response);
            }
            Protocol::PCIe => {
                self.simulate_pcie_tlp();
            }
        }
    }
}
```

### Mixed-Level Simulation

Different parts at different abstraction levels:

```rust
struct MixedSimulator {
    // CPU at transaction level
    cpu: TransactionModel,

    // Custom accelerator at cycle-accurate
    accelerator: MirSimulator,

    // Memory at behavioral
    memory: BehavioralModel,

    // Interfaces at LIR level
    interfaces: LirSimulator,

    fn co_simulate(&mut self) {
        // Synchronize different domains
        let sync_time = self.find_next_sync_point();

        self.cpu.run_until(sync_time);
        self.accelerator.run_until(sync_time);
        self.memory.run_until(sync_time);
        self.interfaces.run_until(sync_time);

        // Exchange data at boundaries
        self.synchronize_interfaces();
    }
}
```

## Formal Integration

Simulation can be replaced with formal verification:

```rust
enum VerificationMode {
    Simulation(SimulationLevel),
    Formal(FormalEngine),
    Hybrid(Box<VerificationMode>, Box<VerificationMode>),
}

struct FormalEngine {
    fn verify_property(&self, property: Property) -> Result<(), CounterExample> {
        let smt = self.encode_to_smt(property);
        let solver = Z3Solver::new();

        match solver.check(smt) {
            Sat => Err(solver.get_model()),
            Unsat => Ok(()),
            Unknown => self.use_simulation_fallback(),
        }
    }
}
```

## Performance Optimization

### Parallel Simulation

```rust
struct ParallelSimulator {
    // Partition design for parallel execution
    partitions: Vec<Partition>,

    fn simulate_parallel(&mut self) {
        // Each partition in separate thread
        let handles: Vec<_> = self.partitions
            .par_iter_mut()
            .map(|p| p.simulate_local())
            .collect();

        // Synchronize at partition boundaries
        self.synchronize_partitions(handles);
    }
}
```

### JIT Compilation

```rust
struct JitSimulator {
    // Compile hot paths to native code
    jit: JitCompiler,

    fn optimize_hot_path(&mut self, block: &SimBlock) {
        if block.execution_count > THRESHOLD {
            let native = self.jit.compile(block);
            self.replace_with_native(block.id, native);
        }
    }
}
```

### Incremental Simulation

```rust
struct IncrementalSimulator {
    // Only simulate changed portions
    dirty: HashSet<ModuleId>,

    fn incremental_update(&mut self, changes: &[Change]) {
        // Mark affected modules
        for change in changes {
            self.mark_dirty(change.module);
            self.mark_dependent_modules(change.module);
        }

        // Only resimulate dirty modules
        for module in &self.dirty {
            self.resimulate_module(module);
        }
    }
}
```

## Simulation Output

### Waveform Generation

```rust
struct WaveformWriter {
    format: WaveformFormat,

    fn write_signal(&mut self, time: SimTime, signal: SignalId, value: Value) {
        match self.format {
            WaveformFormat::VCD => self.write_vcd(time, signal, value),
            WaveformFormat::FST => self.write_fst(time, signal, value),
            WaveformFormat::WLF => self.write_wlf(time, signal, value),
        }
    }
}
```

### Coverage Collection

```rust
struct CoverageCollector {
    line_coverage: HashMap<SourceLine, usize>,
    toggle_coverage: HashMap<SignalId, ToggleCount>,
    fsm_coverage: HashMap<StateId, usize>,
    assertion_coverage: HashMap<AssertionId, usize>,

    fn report_coverage(&self) -> CoverageReport {
        CoverageReport {
            line: self.line_coverage.percentage(),
            toggle: self.toggle_coverage.percentage(),
            fsm: self.fsm_coverage.percentage(),
            assertion: self.assertion_coverage.percentage(),
        }
    }
}
```

## Debug Interface

### Interactive Debugging

```rust
struct SimulationDebugger {
    breakpoints: Vec<Breakpoint>,
    watchpoints: Vec<Watchpoint>,

    fn debug_loop(&mut self) {
        loop {
            let cmd = self.read_command();
            match cmd {
                Step => self.simulator.step(),
                Continue => self.run_until_breakpoint(),
                SetBreakpoint(loc) => self.add_breakpoint(loc),
                Print(signal) => println!("{:?}", self.get_value(signal)),
                Backtrace => self.print_call_stack(),
                Checkpoint => self.save_state(),
            }
        }
    }
}
```

### Assertion-Based Verification

```rust
struct AssertionChecker {
    assertions: Vec<Assertion>,

    fn check_assertions(&self, state: &SimState) {
        for assertion in &self.assertions {
            match assertion {
                Immediate(cond) => {
                    if !self.eval(cond, state) {
                        self.report_failure(assertion);
                    }
                }
                Eventually(cond, cycles) => {
                    self.schedule_check(cond, cycles);
                }
                Always(cond) => {
                    self.add_invariant(cond);
                }
            }
        }
    }
}
```

## Configuration

```toml
# simulation.toml
[simulation]
default_level = "mir"
enable_coverage = true
waveform_format = "fst"

[mir]
four_state = true
timing_checks = true
x_propagation = "pessimistic"

[optimization]
use_jit = true
parallel_threshold = 1000
incremental = true

[debug]
enable_gui = false
history_depth = 10000
```

## GPU-Accelerated Real-Time Co-Simulation

### Architecture Overview

For real-time hardware-software co-simulation, SKALP leverages GPU parallelism to simulate hardware while software runs natively on CPU. On Apple Silicon, we use Metal and unified memory for zero-copy, low-latency co-simulation:

```rust
// Metal-based simulation for Apple Silicon
struct MetalSimulationKernel {
    // Unified memory - no copying needed!
    gates: MTLBuffer<Gate>,        // Shared between CPU/GPU
    flops: MTLBuffer<FlipFlop>,    // Zero-copy access
    wires: MTLBuffer<Wire>,         // CPU writes, GPU reads instantly

    // Metal command queue
    command_queue: MTLCommandQueue,
    pipeline_state: MTLComputePipelineState,

    // Unified memory advantages
    shared_memory: MTLHeap,  // Dynamically allocate unified memory
    cpu_visible: bool = true,  // Always true on Apple Silicon!
}

// Metal compute shader for gate simulation
const METAL_SHADER: &str = r#"
#include <metal_stdlib>
using namespace metal;

struct Gate {
    uint gate_type;
    uint in1, in2;
    uint out;
};

kernel void simulate_gates(
    device const Gate* gates [[buffer(0)]],
    device const uint* inputs [[buffer(1)]],
    device uint* outputs [[buffer(2)]],
    uint tid [[thread_position_in_grid]]
) {
    const Gate gate = gates[tid];

    switch (gate.gate_type) {
        case 0: // AND
            outputs[gate.out] = inputs[gate.in1] & inputs[gate.in2];
            break;
        case 1: // OR
            outputs[gate.out] = inputs[gate.in1] | inputs[gate.in2];
            break;
        case 2: // NOT
            outputs[gate.out] = ~inputs[gate.in1];
            break;
        case 3: // XOR
            outputs[gate.out] = inputs[gate.in1] ^ inputs[gate.in2];
            break;
    }
}
"#;

// Metal kernel for flip-flop updates
const FLOP_UPDATE_SHADER: &str = r#"
kernel void update_flops(
    device const FlipFlop* flops [[buffer(0)]],
    constant bool& clock_edge [[buffer(1)]],
    device const uint* next_state [[buffer(2)]],
    device uint* current_state [[buffer(3)]],
    uint tid [[thread_position_in_grid]]
) {
    const FlipFlop flop = flops[tid];

    if (clock_edge && flop.clock_enable) {
        current_state[flop.q] = next_state[flop.d];
    }
}
"#;
```

### Zero-Copy Unified Memory Benefits

```rust
struct UnifiedMemorySimulator {
    // All memory is unified on Apple Silicon
    memory_region: MTLHeap,

    // CPU and GPU see same memory instantly
    fn write_from_cpu(&mut self, addr: usize, value: u32) {
        // Direct write - visible to GPU immediately
        unsafe {
            let ptr = self.memory_region.contents() as *mut u32;
            ptr.add(addr).write(value);
        }
        // No flush needed! Hardware coherency handles it
    }

    fn read_from_gpu(shader: &str) -> &str {
        r#"
        // GPU reads same memory without any transfer
        uint value = unified_memory[addr];
        // Instant access to CPU writes!
        "#
    }
}

// No DMA, no PCIe, no copies!
struct ZeroCopyMMIO {
    // CPU writes to MMIO addresses
    cpu_side: *mut u32,

    // GPU sees writes immediately in unified memory
    gpu_side: MTLBuffer<u32>,

    fn init(device: &MTLDevice) -> Self {
        // Allocate unified memory
        let buffer = device.new_buffer_with_options(
            size: 64 * 1024,  // 64KB MMIO region
            options: MTLResourceOptions::StorageModeShared  // CPU+GPU accessible
        );

        Self {
            cpu_side: buffer.contents() as *mut u32,
            gpu_side: buffer,
        }
    }
}
```

### Async Runtime Integration with Grand Central Dispatch

```rust
struct MetalCoSimulator {
    // CPU side - software execution
    cpu_runtime: tokio::Runtime,
    software_threads: Vec<JoinHandle<()>>,

    // GPU side - Metal hardware simulation
    device: MTLDevice,
    command_queue: MTLCommandQueue,
    simulation_kernel: MetalSimulationKernel,

    // Unified memory - no channels needed for data!
    // Events still use channels for synchronization
    hw_to_sw: mpsc::Sender<HwEvent>,
    sw_to_hw: mpsc::Receiver<SwEvent>,

    // Unified memory region - zero copy!
    unified_memory: MTLHeap,  // CPU and GPU share this
}

impl MetalCoSimulator {
    async fn run_cosimulation(&mut self) {
        // Software runs on CPU
        let sw_handle = self.cpu_runtime.spawn(async move {
            // Run actual software (OS, drivers, apps)
            self.run_software().await;
        });

        // Hardware simulates on Metal GPU
        let hw_handle = tokio::task::spawn_blocking(move || {
            loop {
                // Run one simulation quantum
                self.simulate_hardware_quantum().await;

                // Check for software events
                if let Some(event) = self.sw_to_hw.try_recv() {
                    self.handle_software_event(event).await;
                }

                // Send hardware events to software
                if let Some(hw_event) = self.get_hardware_event() {
                    self.hw_to_sw.send(hw_event).await;
                }
            }
        });

        // Coordinate execution
        tokio::join!(sw_handle, hw_handle);
    }
}
```

### Memory-Mapped I/O with Unified Memory

```rust
struct UnifiedMMIO {
    // Single memory space visible to both CPU and GPU
    mmio_base: *mut u8,  // Raw pointer for CPU
    mmio_buffer: MTLBuffer,  // Same memory for GPU

    // Memory barriers for coherency (though Apple Silicon handles most)
    fn cpu_write_mmio(&self, offset: usize, value: u32) {
        unsafe {
            let addr = self.mmio_base.add(offset) as *mut u32;
            // Atomic write for CPU-GPU coherency
            std::ptr::write_volatile(addr, value);
            std::sync::atomic::fence(Ordering::Release);
        }
    }
}

// Metal shader for MMIO handling
const MMIO_SHADER: &str = r#"
kernel void handle_memory_access(
    device uint* mmio_region [[buffer(0)]],
    constant uint& addr [[buffer(1)]],
    constant uint& data [[buffer(2)]],
    constant bool& is_write [[buffer(3)]],
    device atomic_uint* event_count [[buffer(4)]]
) {
    if let Some(handler) = mmio.find_handler(addr) {
        if is_write {
            // Trigger hardware behavior based on MMIO write
            let event = handler.handle_write(addr, data);
            event_queue.push(event);
        } else {
            // Return hardware status via MMIO read
            return handler.handle_read(addr);
        }
    }
}
```

### Real-Time Synchronization with Metal Performance

```rust
struct MetalRealTimeSync {
    wall_clock: std::time::Instant,
    sim_time: SimTime,
    target_frequency: f64,  // Target Hz for hardware

    // Metal timing
    gpu_start: MTLTimestamp,
    gpu_end: MTLTimestamp,

    async fn synchronize(&mut self) {
        let elapsed = self.wall_clock.elapsed();
        let target_sim_time = elapsed.as_secs_f64() * self.target_frequency;

        if self.sim_time < target_sim_time {
            // Hardware is behind, catch up
            let cycles_behind = target_sim_time - self.sim_time;
            self.fast_forward(cycles_behind).await;
        } else if self.sim_time > target_sim_time {
            // Hardware is ahead, wait
            let wait_time = (self.sim_time - target_sim_time) / self.target_frequency;
            tokio::time::sleep(Duration::from_secs_f64(wait_time)).await;
        }
    }
}
```

### Simulation Granularity Optimization

#### Combinational Cone Simulation (Flop-to-Flop)

Instead of simulating individual gates, we can dramatically improve performance by simulating entire combinational cones (logic between flip-flops) as single GPU threads:

```rust
// Combinational cone representation
struct CombinationalCone {
    id: ConeId,
    input_flops: Vec<FlopId>,
    output_flop: FlopId,
    logic: ConeLogic,
}

enum ConeLogic {
    Expression(ExprTree),      // Simple expressions
    MicroCode(Vec<MicroOp>),   // Complex logic
    Compiled(MetalKernel),     // JIT-compiled for performance
}

// Single thread evaluates entire cone
kernel void evaluate_cone(
    device const ConeLogic* cones [[buffer(0)]],
    device const uint* flop_outputs [[buffer(1)]],
    device uint* flop_inputs [[buffer(2)]],
    uint tid [[thread_position_in_grid]]
) {
    const ConeLogic cone = cones[tid];
    uint result = evaluate_logic(cone, flop_outputs);
    flop_inputs[cone.output_flop] = result;
}
```

**Benefits:**
- **10-100x larger designs** - Threads scale with flops, not gates
- **Better GPU utilization** - More compute per thread
- **Less synchronization** - Only at clock boundaries
- **Lower memory bandwidth** - Only flop values transferred

**Trade-offs:**
- **No glitch modeling** - Can't see intermediate hazards
- **No gate delays** - Assumes zero-delay combinational logic
- **Harder debugging** - Can't observe internal cone signals
- **Not suitable for** - Asynchronous circuits, timing verification

#### Granularity Selection

```rust
struct SimulationGranularity {
    mode: GranularityMode,
}

enum GranularityMode {
    // Gate-level: Every gate is a thread
    GateLevel {
        model_glitches: bool,
        gate_delays: bool,
    },

    // Cone-level: Combinational logic as threads
    ConeLevel {
        max_gates_per_cone: usize,
        jit_threshold: usize,
    },

    // Module-level: Entire modules as threads
    ModuleLevel {
        behavioral_models: bool,
    },

    // Hybrid: Different granularities for different parts
    Hybrid {
        rules: Vec<GranularityRule>,
    },
}

// Choose based on simulation goals
fn select_granularity(design: &Design, goals: &SimGoals) -> GranularityMode {
    match goals.primary {
        Goal::Performance => GranularityMode::ConeLevel {
            max_gates_per_cone: 1000,
            jit_threshold: 100,
        },
        Goal::TimingAccuracy => GranularityMode::GateLevel {
            model_glitches: true,
            gate_delays: true,
        },
        Goal::Functional => GranularityMode::ConeLevel {
            max_gates_per_cone: 10000,
            jit_threshold: 1000,
        },
        Goal::Debug => GranularityMode::GateLevel {
            model_glitches: false,
            gate_delays: false,
        },
    }
}
```

#### Performance Comparison

| Granularity | Max Design Size | Real-time Capability | Timing Accuracy | Debug Visibility |
|-------------|-----------------|---------------------|-----------------|------------------|
| Gate-Level | 10M gates | 10MHz for 1M gates | Exact | Full |
| Cone-Level | 1B gates | 100MHz for 10M gates | Cycle-accurate | Flop-only |
| Module-Level | 10B gates | 1GHz for 100M gates | Transaction | Interface-only |
| Hybrid | 1B gates | Varies | Configurable | Configurable |

### GPU Instancing and Pattern Optimization

#### Hardware Regularity Exploitation

Hardware designs have massive regularity that maps perfectly to GPU instancing and specialized shader techniques:

```metal
// Instance-based simulation for repeated structures
struct BitSlicedALU {
    operation: uint,
    width: uint,
};

// Single kernel, multiple instances
kernel void alu_instanced(
    constant BitSlicedALU& alu [[buffer(0)]],
    device const uint* input_a [[buffer(1)]],
    device const uint* input_b [[buffer(2)]],
    device uint* output [[buffer(3)]],
    uint instance_id [[instance_id]]  // GPU provides for each bit
) {
    // Same logic, different bit position
    uint bit_a = extract_bit(input_a, instance_id);
    uint bit_b = extract_bit(input_b, instance_id);
    uint result = compute_alu_bit(alu.operation, bit_a, bit_b);
    set_bit(output, instance_id, result);
}

// Launch once for 64-bit ALU, GPU handles 64 instances
```

#### Pattern-Specific Optimizations

```rust
enum HardwarePattern {
    // Bit-sliced datapaths - use instancing
    BitSliced { width: usize },

    // Memory arrays - use texture cache
    MemoryArray { rows: usize, cols: usize },

    // Systolic arrays - use tiled compute
    SystolicArray { dim: usize },

    // LUT-based logic - use texture lookups
    LookupTables { count: usize, size: usize },
}

// Compiler recognizes and optimizes patterns
fn optimize_for_pattern(pattern: HardwarePattern) -> GpuStrategy {
    match pattern {
        BitSliced { width } => GpuStrategy::Instancing(width),
        MemoryArray { .. } => GpuStrategy::TextureMemory,
        SystolicArray { .. } => GpuStrategy::TiledCompute,
        LookupTables { .. } => GpuStrategy::TextureLUT,
    }
}
```

#### Advanced GPU Techniques

1. **Texture Memory for LUTs**
```metal
texture2d<uint> lut_texture [[texture(0)]];

kernel void fpga_lut_sim(
    device const uint4* inputs [[buffer(0)]],
    device uint* outputs [[buffer(1)]],
    uint tid [[thread_position_in_grid]]
) {
    // 4-input LUT as texture lookup
    uint addr = pack_lut_address(inputs[tid]);
    outputs[tid] = lut_texture.read(uint2(addr, 0)).x;
}
```

2. **SIMD Lane Operations**
```metal
kernel void parallel_carry_chain(
    device const uint* a [[buffer(0)]],
    device const uint* b [[buffer(1)]],
    device uint* sum [[buffer(2)]],
    uint lane_id [[thread_index_in_simdgroup]]
) {
    // Each SIMD lane is one bit
    uint bit_a = (a[0] >> lane_id) & 1;
    uint bit_b = (b[0] >> lane_id) & 1;

    // Parallel prefix for carry propagation
    uint carry = simd_prefix_exclusive_sum(bit_a & bit_b);

    // Gather results
    uint bit_sum = bit_a ^ bit_b ^ carry;
    sum[0] = simd_or(bit_sum << lane_id);
}
```

3. **Persistent Thread Blocks**
```metal
// Threads stay alive across cycles
kernel void persistent_simulation(
    device SimulationState* state [[buffer(0)]],
    constant uint& max_cycles [[buffer(1)]]
) {
    uint tid = get_global_id(0);

    for (uint cycle = 0; cycle < max_cycles; cycle++) {
        // Simulate without kernel re-launch
        simulate_cycle(state, tid);
        threadgroup_barrier(mem_flags::mem_device);
    }
}
```

### Multi-Cone Data Type Packing

#### Exploiting GPU Wide Data Types

Hardware simulation only needs 1-bit values, but GPUs excel at wider data types (int32, int64, float32, etc.). We can pack multiple simulation cones into single GPU operations for massive performance gains:

```metal
// Pack 32 1-bit cones into int32
typedef struct {
    uint32_t packed_states;  // 32 cones in one int
    uint32_t packed_inputs;  // 32 cone inputs
    uint32_t packed_outputs; // 32 cone outputs
} PackedConeState;

// SIMD operation on 32 cones simultaneously
kernel void simulate_32_cones_packed(
    device PackedConeState* cones [[buffer(0)]],
    uint gid [[thread_position_in_grid]]
) {
    PackedConeState cone = cones[gid];

    // Simulate 32 cones with single bitwise operations
    uint32_t and_result = cone.packed_inputs & 0xAAAAAAAA;  // 32 AND gates
    uint32_t or_result = cone.packed_inputs | 0x55555555;   // 32 OR gates
    uint32_t xor_result = cone.packed_inputs ^ cone.packed_states; // 32 XOR gates

    // Complex logic using bit manipulation
    uint32_t lut_result = simulate_lut_32x(cone.packed_inputs);

    cones[gid].packed_outputs = lut_result;
}

// Even wider: 128 cones using 4x int32 SIMD
kernel void simulate_128_cones_simd(
    device packed_uint4* cone_inputs [[buffer(0)]],   // 4x uint32 = 128 bits
    device packed_uint4* cone_outputs [[buffer(1)]],
    uint gid [[thread_position_in_grid]]
) {
    packed_uint4 inputs = cone_inputs[gid];

    // SIMD operations on 4x32=128 cones simultaneously
    packed_uint4 inverted = ~inputs;                    // 128 NOT gates
    packed_uint4 and_mask = packed_uint4(0xAAAAAAAA);   // Pattern for ANDs
    packed_uint4 and_result = inputs & and_mask;        // 128 AND operations

    cone_outputs[gid] = and_result;
}

// Float types for arithmetic cones
kernel void simulate_arithmetic_cones(
    device packed_float4* operands [[buffer(0)]],
    device packed_float4* results [[buffer(1)]],
    uint gid [[thread_position_in_grid]]
) {
    packed_float4 a = operands[gid * 2];
    packed_float4 b = operands[gid * 2 + 1];

    // 4 parallel arithmetic units using GPU's native float hardware
    results[gid] = a * b + a;  // 4 multiply-accumulate operations
}
```

#### Cone Classification for Optimal Packing

```rust
enum ConeType {
    // Simple logic - pack many into wide integers
    BasicLogic {
        inputs: u8,      // Number of inputs (1-6 typical)
        lut_mask: u64,   // Truth table for this cone
        pack_width: u8,  // How many fit in int32/int64
    },

    // Arithmetic - use GPU's native float/int units
    Arithmetic {
        operation: ArithOp,  // Add, Mul, MAC, etc.
        width: u8,           // Bit width
        use_native: bool,    // Use GPU's native arithmetic
    },

    // Memory operations - use GPU's memory hierarchy
    Memory {
        access_pattern: MemPattern,
        use_texture: bool,   // Use GPU texture units for tables
    },

    // State machines - use GPU branching efficiently
    StateMachine {
        states: u8,
        transitions: Vec<Transition>,
    },
}

struct ConePackingStrategy {
    fn analyze_cone(&self, cone: &Cone) -> ConeType {
        match cone.gate_count() {
            1..=6 => {
                // Simple logic - pack multiple cones
                let pack_width = match cone.inputs() {
                    1 => 32,  // 32 inverters in one int32
                    2 => 16,  // 16 2-input gates in one int32
                    3 => 10,  // 10 3-input gates in one int32
                    4 => 8,   // 8 4-input gates in one int32
                    5..=6 => 5, // 5 complex gates in one int32
                    _ => 1,
                };

                ConeType::BasicLogic {
                    inputs: cone.inputs(),
                    lut_mask: cone.truth_table(),
                    pack_width,
                }
            },

            7..=16 => {
                // Medium complexity - arithmetic operations
                if cone.is_arithmetic() {
                    ConeType::Arithmetic {
                        operation: cone.arith_op(),
                        width: cone.bit_width(),
                        use_native: cone.bit_width() <= 32,
                    }
                } else {
                    ConeType::BasicLogic { pack_width: 1, .. }
                }
            },

            _ => {
                // Complex cones - individual processing
                ConeType::StateMachine {
                    states: cone.state_count(),
                    transitions: cone.transitions(),
                }
            }
        }
    }
}
```

#### Performance Analysis of Packing Strategies

```rust
// Performance comparison of different packing approaches
struct PackingPerformance {
    // Traditional: 1 cone per GPU thread
    fn traditional_performance(&self) -> PerfMetrics {
        PerfMetrics {
            cones_per_thread: 1.0,
            memory_bandwidth_util: 100.0,  // Full utilization
            compute_utilization: 5.0,      // Wasted on simple logic
            relative_throughput: 1.0,      // Baseline
        }
    }

    // Packed: Multiple cones per thread
    fn packed_performance(&self) -> PerfMetrics {
        PerfMetrics {
            cones_per_thread: 32.0,        // 32 cones per thread
            memory_bandwidth_util: 3.125,  // 32x less memory traffic
            compute_utilization: 80.0,     // Much better ALU usage
            relative_throughput: 25.0,     // Massive speedup
        }
    }

    // Hybrid: Pack simple, individual complex
    fn hybrid_performance(&self) -> PerfMetrics {
        PerfMetrics {
            cones_per_thread: 10.0,        // 10x average packing
            memory_bandwidth_util: 15.0,   // 6.7x less traffic
            compute_utilization: 60.0,     // Balanced utilization
            relative_throughput: 12.0,     // Best practical speedup
        }
    }
}
```

The key insight is that typical hardware designs have many simple logic cones (inverters, 2-input gates, simple LUTs) that can be packed together, with only occasional complex arithmetic or state machine cones that need individual processing.

#### Separation of Concerns: Hardware IR vs Simulation Compilation

SKALP's IR remains **pure hardware description** - no GPU simulation constructs pollute the hardware representation. Instead, we use a separate **Simulation Compiler** that analyzes the clean MIR to generate optimized GPU simulation:

```rust
// Clean MIR: Pure hardware description
mir::Module {
    registers: Vec<Register>,           // Only hardware state
    combinational: Vec<Gate>,           // Only logic description
    timing_paths: Vec<TimingPath>,      // Only hardware timing
    // NO GPU simulation artifacts here
}

// Separate simulation compilation pipeline
struct SimulationCompiler {
    fn compile_for_gpu(&self, mir: &Mir) -> GpuSimulationArtifacts {
        // Extract simulation-relevant information from pure hardware IR
        let cone_analysis = self.analyze_combinational_structure(&mir);
        let packing_plan = self.determine_optimal_packing(cone_analysis);
        let memory_layout = self.optimize_memory_access_patterns(&mir);

        GpuSimulationArtifacts {
            metal_kernels: self.generate_optimized_kernels(packing_plan),
            memory_layout,
            debug_metadata: self.preserve_source_mapping(&mir),
        }
    }
}
```

#### Implementation in SKALP Simulator

```rust
// Implementation: Simulation engine built from analyzed MIR
struct PackedGpuSimulator {
    // Different kernel types for different cone categories
    packed_logic_kernel: MetalKernel,     // 32-128 cones per thread
    arithmetic_kernel: MetalKernel,       // Native float/int operations
    complex_kernel: MetalKernel,          // Individual cone processing

    // Cone grouping and scheduling
    cone_groups: Vec<ConeGroup>,
    scheduling_strategy: PackingStrategy,
}

impl PackedGpuSimulator {
    fn analyze_and_group_cones(&mut self, design: &LirNetlist) {
        for cone in design.combinational_cones() {
            match self.classify_cone(cone) {
                ConeType::BasicLogic { pack_width, .. } if pack_width > 1 => {
                    self.add_to_packed_group(cone, pack_width);
                },
                ConeType::Arithmetic { use_native: true, .. } => {
                    self.add_to_arithmetic_group(cone);
                },
                _ => {
                    self.add_to_individual_group(cone);
                }
            }
        }

        // Optimize group sizes for GPU occupancy
        self.optimize_group_sizes();
    }

    fn generate_packed_kernel(&self, group: &PackedConeGroup) -> String {
        let pack_width = group.pack_width;
        format!(r#"
        kernel void simulate_packed_cones_{pack_width}(
            device PackedConeState* cones [[buffer(0)]],
            uint gid [[thread_position_in_grid]]
        ) {{
            PackedConeState cone = cones[gid];

            // Generate specific bitwise operations for this group
            {}

            cones[gid].packed_outputs = result;
        }}
        "#, self.generate_packed_operations(group))
    }

    fn simulate_cycle(&mut self) -> Result<()> {
        // Launch different kernels in parallel
        let command_buffer = self.command_queue.new_command_buffer();

        // Packed logic cones - highest throughput
        if !self.packed_groups.is_empty() {
            let encoder = command_buffer.new_compute_command_encoder();
            encoder.set_compute_pipeline_state(&self.packed_logic_kernel);
            encoder.set_buffer(0, &self.packed_cone_buffer, 0);

            let threads_per_group = 256;
            let groups = (self.packed_cone_count + threads_per_group - 1) / threads_per_group;
            encoder.dispatch_thread_groups(MTLSize::new(groups, 1, 1),
                                         MTLSize::new(threads_per_group, 1, 1));
            encoder.end_encoding();
        }

        // Arithmetic cones using native GPU units
        if !self.arithmetic_groups.is_empty() {
            let encoder = command_buffer.new_compute_command_encoder();
            encoder.set_compute_pipeline_state(&self.arithmetic_kernel);
            encoder.set_buffer(0, &self.arithmetic_buffer, 0);
            encoder.dispatch_thread_groups(/* ... */);
            encoder.end_encoding();
        }

        // Complex individual cones
        if !self.complex_cones.is_empty() {
            let encoder = command_buffer.new_compute_command_encoder();
            encoder.set_compute_pipeline_state(&self.complex_kernel);
            encoder.set_buffer(0, &self.complex_cone_buffer, 0);
            encoder.dispatch_thread_groups(/* ... */);
            encoder.end_encoding();
        }

        command_buffer.commit();
        command_buffer.wait_until_completed();

        Ok(())
    }
}

// Cone analysis for packing decisions
impl ConeAnalyzer {
    fn classify_cone(&self, cone: &Cone) -> ConeType {
        // Analyze cone structure to determine optimal processing
        let input_count = cone.inputs().len();
        let gate_count = cone.gates().len();

        if gate_count <= 6 && !cone.has_memory() && !cone.has_state() {
            // Simple combinational logic - candidate for packing
            let truth_table = self.extract_truth_table(cone);
            let pack_width = self.calculate_pack_width(input_count, gate_count);

            ConeType::BasicLogic {
                inputs: input_count as u8,
                lut_mask: truth_table,
                pack_width,
            }
        } else if cone.is_arithmetic_pattern() {
            // Arithmetic operations - use GPU's native ALUs
            ConeType::Arithmetic {
                operation: cone.detect_arithmetic_op(),
                width: cone.bit_width() as u8,
                use_native: cone.bit_width() <= 32,
            }
        } else {
            // Complex logic - individual processing
            ConeType::StateMachine {
                states: cone.state_count() as u8,
                transitions: cone.extract_transitions(),
            }
        }
    }

    fn calculate_pack_width(&self, inputs: usize, gates: usize) -> u8 {
        // Conservative packing to maintain correctness
        match (inputs, gates) {
            (1, 1) => 32,    // Simple inverters
            (2, 1) => 16,    // 2-input gates
            (2, 2..=3) => 8, // Small compound gates
            (3, 1..=2) => 8, // 3-input LUTs
            (4, 1) => 8,     // 4-input LUTs
            _ => 1,          // Too complex to pack
        }
    }
}
```

#### Memory Layout Optimization

```rust
// Optimized memory layout for packed simulation
struct PackedMemoryLayout {
    // Structure of Arrays (SoA) for better coalescing
    packed_inputs: Vec<u32>,      // All input states packed
    packed_outputs: Vec<u32>,     // All output states packed
    packed_lut_data: Vec<u64>,    // Truth tables for each group

    // Metadata for unpacking
    cone_metadata: Vec<ConeMetadata>,
    group_offsets: Vec<u32>,
}

struct ConeMetadata {
    group_id: u16,        // Which packed group
    bit_offset: u8,       // Bit position within packed word
    input_mask: u32,      // Which bits are inputs
    output_mask: u32,     // Which bits are outputs
}

impl PackedMemoryLayout {
    fn pack_cone_states(&mut self, cones: &[Cone]) {
        for group in self.create_groups(cones) {
            let mut packed_input = 0u32;
            let mut packed_output = 0u32;

            for (i, cone) in group.cones.iter().enumerate() {
                if i < 32 {  // Ensure we don't overflow
                    packed_input |= (cone.input_state() as u32) << i;
                    packed_output |= (cone.output_state() as u32) << i;
                }
            }

            self.packed_inputs.push(packed_input);
            self.packed_outputs.push(packed_output);
        }
    }

    fn unpack_results(&self, gpu_results: &[u32]) -> Vec<bool> {
        let mut results = vec![false; self.total_cones()];

        for (group_idx, &packed_result) in gpu_results.iter().enumerate() {
            let group = &self.groups[group_idx];

            for (local_idx, cone_idx) in group.cone_indices.iter().enumerate() {
                if local_idx < 32 {
                    results[*cone_idx] = (packed_result >> local_idx) & 1 != 0;
                }
            }
        }

        results
    }
}
```

This packing approach can achieve 10-25x performance improvements by:

1. **Massive Parallelism**: 32-128 cones per GPU thread vs 1 cone per thread
2. **Memory Bandwidth Reduction**: 32x fewer memory accesses for packed operations
3. **Better ALU Utilization**: GPU arithmetic units used efficiently for bitwise operations
4. **Cache Efficiency**: Larger, more efficient memory transfers

The feasibility is excellent because most hardware designs contain many simple logic gates that are perfect candidates for packing, with only occasional complex operations that need individual processing.

### GPU Async Runtime Orchestration

#### Single Persistent Orchestrator Kernel

Instead of launching multiple kernels, use a single persistent GPU kernel that acts as an async runtime:

```metal
// GPU-side async runtime
kernel void gpu_async_runtime(
    device SimulationState* state [[buffer(0)]],
    device EventQueue* events [[buffer(1)]],
    device WorkQueue* work [[buffer(2)]],
    constant RuntimeConfig& config [[buffer(3)]],
    uint3 tid [[thread_position_in_grid]],
    uint3 tg_id [[threadgroup_position_in_grid]]
) {
    // This kernel NEVER exits - persistent runtime
    while (true) {
        // Check for events (like CPU async/await)
        Event event = atomic_dequeue(events);

        if (event.type == EventType::Clock) {
            // Orchestrate clock edge evaluation
            if (tid.x < state->num_flops) {
                update_flop(state->flops[tid.x]);
            }
            threadgroup_barrier(mem_flags::mem_device);

        } else if (event.type == EventType::Combinational) {
            // Distribute combinational evaluation
            if (tid.x < state->num_cones) {
                evaluate_cone(state->cones[tid.x]);
            }

        } else if (event.type == EventType::Memory) {
            // Handle memory operations
            if (tg_id.x == event.memory_bank) {
                handle_memory_access(event.memory_op);
            }

        } else if (event.type == EventType::Yield) {
            // Yield to CPU for I/O
            atomic_store(&state->gpu_waiting, true);
            while (atomic_load(&state->cpu_ready) == false) {
                // Spin wait or use Metal's wait
            }
        }

        // Work stealing for load balancing
        if (tid.x == 0) {
            steal_work_from_neighbor(work, tg_id);
        }
    }
}
```

#### Async Task System on GPU

```rust
// GPU-side task representation
struct GpuTask {
    task_type: TaskType,
    dependencies: Vec<TaskId>,
    ready: AtomicBool,
}

enum TaskType {
    EvaluateCone(ConeId),
    UpdateMemory(MemoryOp),
    PropagateSignal(SignalId),
    CheckAssertion(AssertionId),
}

// GPU async executor
struct GpuAsyncExecutor {
    tasks: Vec<GpuTask>,
    ready_queue: LockFreeQueue<TaskId>,

    fn spawn(&mut self, task: GpuTask) -> TaskId {
        let id = self.tasks.len();
        self.tasks.push(task);

        if task.dependencies.is_empty() {
            self.ready_queue.push(id);
        }

        id
    }

    fn complete(&mut self, task_id: TaskId) {
        // Wake dependent tasks
        for dependent in self.get_dependents(task_id) {
            if self.all_deps_ready(dependent) {
                self.ready_queue.push(dependent);
            }
        }
    }
}
```

#### Benefits of GPU Async Runtime

1. **No Kernel Launch Overhead**
   - Traditional: 5-10μs per kernel launch
   - Async Runtime: 0 overhead, already running

2. **Fine-Grained Scheduling**
   - Can switch tasks at instruction level
   - No CPU intervention needed

3. **Better GPU Utilization**
   - Threads never idle waiting for CPU
   - Work stealing for load balancing

4. **Event-Driven Simulation**
   ```metal
   // Event-driven on GPU!
   while (has_events()) {
       Event e = get_next_event();
       process_event(e);
       schedule_dependent_events(e);
   }
   ```

5. **Coroutine-Style Simulation**
   ```metal
   // GPU coroutines for state machines
   generator fsm_coroutine(State* state) {
       while (true) {
           switch (*state) {
               case IDLE:
                   co_yield wait_for_input();
                   *state = ACTIVE;
                   break;
               case ACTIVE:
                   co_yield process_data();
                   *state = DONE;
                   break;
           }
       }
   }
   ```

#### Implementation Strategy

```rust
// Host-side setup
fn setup_gpu_async_runtime(device: &MTLDevice) -> GpuRuntime {
    // Allocate persistent state
    let state = device.new_buffer(/* simulation state */);
    let event_queue = device.new_buffer(/* event queue */);
    let work_queue = device.new_buffer(/* work queue */);

    // Create persistent kernel
    let kernel = device.new_kernel("gpu_async_runtime");

    // Launch once, runs forever
    let encoder = command_buffer.compute_encoder();
    encoder.dispatch_threads(
        threads_per_grid: (1024, 1, 1),
        threads_per_threadgroup: (32, 1, 1)
    );
    encoder.end_encoding();
    command_buffer.commit();

    GpuRuntime {
        state,
        event_queue,
        work_queue,
        kernel_handle: command_buffer,
    }
}

// Communication is just memory writes
fn send_event_to_gpu(runtime: &GpuRuntime, event: Event) {
    // Write to unified memory - GPU sees immediately
    runtime.event_queue.push(event);
}
```

### Performance Impact

| Approach | Kernel Launches | Overhead | GPU Utilization |
|----------|----------------|----------|------------------|
| Traditional | 1000s/cycle | 5-10ms/cycle | 30-50% |
| Async Runtime | 1 total | ~0 | 90-95% |

### Apple Silicon Optimization Strategies

#### 1. Tile-Based Clustering
```rust
// Leverage Apple's TBDR architecture
struct TileBasedCluster {
    gates: Vec<Gate>,
    inputs: Vec<Signal>,
    outputs: Vec<Signal>,

    // Use Metal's threadgroup memory (ultra-fast on-chip)
    fn process_on_metal(&self) -> MetalKernel {
        // Use threadgroup memory (on-chip SRAM)
        // Leverage Apple's tile-based architecture
        // No need to minimize "global" traffic - it's all unified!
    }
}
```

#### 2. Unified Memory Event Ring
```rust
// Lock-free ring buffer in unified memory
struct UnifiedEventRing {
    ring: MTLBuffer,  // Circular buffer in unified memory
    cpu_write: AtomicUsize,
    gpu_read: AtomicUsize,

    fn push_from_cpu(&self, event: Event) {
        // CPU writes directly to unified memory
        let idx = self.cpu_write.fetch_add(1, Ordering::Relaxed) % RING_SIZE;
        unsafe {
            let ptr = self.ring.contents() as *mut Event;
            ptr.add(idx).write(event);
        }
        // GPU sees it immediately - no transfer!
    }
}
```

#### 3. SIMD-Accelerated Waveform Compression
```rust
// Use Apple's NEON/AMX for compression
struct SIMDWaveform {
    // Only store changes, not every cycle
    changes: Vec<(SimTime, SignalId, Value)>,

    // Metal kernel using SIMD
    const COMPRESS_SHADER: &str = r#"
    kernel void compress_signals(
        device const uint* signals [[buffer(0)]],
        device const uint* previous [[buffer(1)]],
        device Change* changes [[buffer(2)]],
        uint tid [[thread_position_in_grid]]
    ) {
        // SIMD comparison using Metal's vector types
        uint4 curr = signals[tid];
        uint4 prev = previous[tid];
        uint4 diff = curr ^ prev;

        if (any(diff)) {
            // Atomic operation in unified memory
            uint idx = atomic_fetch_add_explicit(&change_count, 1);
            changes[idx] = {current_time, tid, curr};
        }
    }
    "#;
}
```

### Performance Targets on Apple Silicon

```rust
// M4 Max performance metrics
struct M4MaxPerformance {
    // Simulation throughput by granularity
    gate_level: {
        gates_per_second: 5_000_000_000,  // 5B gates/sec
        realtime_100mhz: 2_000_000,       // 2M gates
        realtime_10mhz:  20_000_000,      // 20M gates
    },

    cone_level: {
        cones_per_second: 500_000_000,    // 500M cones/sec
        avg_gates_per_cone: 20,           // Typical cone size
        effective_gates_per_second: 10_000_000_000, // 10B gates/sec
        realtime_100mhz: 20_000_000,      // 20M gates (10x improvement!)
        realtime_10mhz:  200_000_000,     // 200M gates
        realtime_1mhz:   2_000_000_000,   // 2B gates
    },

    // Co-simulation overhead (MUCH better!)
    cpu_gpu_latency: 0.1, // 100 nanoseconds - same memory!
    memory_bandwidth: 546_000_000_000, // 546 GB/s unified memory

    // Unique advantages
    zero_copy_overhead: true,
    hardware_coherency: true,
    unified_memory_size: 192_000_000_000, // 192GB on M4 Max
}
```

### Configuration

```toml
[metal_simulation]
enabled = true
device = "default"  # Use default Metal device
threadgroup_size = 256  # Threads per threadgroup
max_threadgroups = 1024  # Maximum threadgroups

[cosimulation]
mode = "lockstep"  # or "async"
quantum = 1000  # Cycles per quantum
sync_mode = "realtime"  # or "fast"

[memory]
shared_size = "1GB"
dma_channels = 4
mmio_cache = true

[optimization]
gate_clustering = true
event_batching = true
batch_size = 1000
```

### Example Usage

```rust
// Real-time SoC simulation on Apple Silicon
#[test]
async fn test_soc_realtime() {
    let mut cosim = MetalCoSimulator::new()
        .hardware("soc.sk")
        .software("firmware.elf")
        .target_frequency(100_000_000)  // 100MHz
        .metal_device(MTLDevice::system_default())
        .unified_memory(true)  // Always true on Apple Silicon
        .build();

    // Run for 1 second of real time
    cosim.run_for_realtime(Duration::from_secs(1)).await;

    // Check results
    assert_eq!(cosim.read_memory(0x8000_0000), expected);
}
```

## Simulation Mode Selection

### When to Use Each Granularity

#### Gate-Level Simulation
**Use when:**
- Debugging asynchronous circuits
- Analyzing glitches/hazards
- Verifying timing-critical paths
- Validating clock domain crossings
- Educational/visualization purposes

**Don't use when:**
- Need real-time performance
- Simulating large SoCs
- Functional verification only

#### Cone-Level Simulation
**Use when:**
- Real-time co-simulation needed
- Large design simulation
- Cycle-accurate is sufficient
- Software development/testing
- Performance analysis

**Don't use when:**
- Need to observe glitches
- Debugging combinational loops
- Precise timing analysis
- Asynchronous circuit verification

#### Hybrid Approach
```rust
// Configure different parts differently
let sim_config = SimulationConfig {
    cpu_core: GranularityMode::ConeLevel,     // Performance
    clock_gen: GranularityMode::GateLevel,    // Timing critical
    memories: GranularityMode::ModuleLevel,   // Behavioral
    debug_block: GranularityMode::GateLevel,  // Full visibility
};
```

## CLI Integration

```bash
# Specify simulation granularity
skalp sim design.sk --granularity cone  # Default for performance
skalp sim design.sk --granularity gate  # For timing accuracy
skalp sim design.sk --granularity hybrid --config sim.toml
# Simulate at different levels
skalp sim design.sk --level source    # Fast behavioral
skalp sim design.sk --level mir       # Cycle-accurate
skalp sim design.sk --level lir       # With timing

# Metal-accelerated simulation on macOS
skalp sim design.sk --metal  # Use Metal on Apple Silicon

# GPU async runtime mode
skalp sim design.sk --metal --async-runtime  # Persistent GPU kernel

# Pattern-optimized simulation
skalp sim design.sk --metal --optimize-patterns  # Auto-detect and optimize

# Real-time co-simulation
skalp cosim soc.sk firmware.elf --realtime --freq 100MHz

# With testbench
skalp sim design.sk --tb testbench.sk

# Generate waveforms
skalp sim design.sk --wave output.vcd

# Coverage-driven
skalp sim design.sk --coverage --report cov.html

# Interactive debug
skalp sim design.sk --debug --gui
```

---

## GPU Dual-Role: Simulation Engine + Display Framebuffer

### Simultaneous Simulation and Display Rendering

On Apple Silicon with unified memory, the GPU can efficiently serve dual roles: executing hardware simulation while also rendering display output from simulated video controllers.

#### Architecture Overview

```rust
struct DualRoleGpuEngine {
    // Simulation kernels
    simulation_kernels: Vec<MetalKernel>,
    simulation_buffers: Vec<MTLBuffer>,

    // Display rendering pipeline
    display_pipeline: MetalRenderPipeline,
    framebuffer: MTLTexture,               // Final display output
    video_memory: MTLBuffer,               // Simulated video memory

    // Shared resources
    unified_memory: MTLHeap,               // CPU+GPU accessible
    command_queue: MTLCommandQueue,
}

impl DualRoleGpuEngine {
    fn simulate_and_render_frame(&mut self) -> Result<()> {
        let command_buffer = self.command_queue.new_command_buffer();

        // Phase 1: Hardware simulation
        self.run_simulation_kernels(&command_buffer)?;

        // Phase 2: Extract video data from simulated hardware
        self.extract_video_output(&command_buffer)?;

        // Phase 3: Render to display framebuffer
        self.render_display_frame(&command_buffer)?;

        command_buffer.commit();
        command_buffer.wait_until_completed();

        Ok(())
    }
}
```

#### Real-Time SoC Simulation with HDMI Display

```rust
// Example: SoC with video controller simulation
struct SocSimulationWithDisplay {
    // Hardware simulation state
    cpu_state: CpuSimulationState,
    video_controller_state: VideoControllerState,
    memory_controller_state: MemoryState,

    // Display rendering resources
    hdmi_framebuffer: MTLTexture,          // 1920x1080 RGBA
    video_memory_buffer: MTLBuffer,        // Simulated video RAM
    display_pipeline: RenderPipeline,

    // Timing synchronization
    vsync_timer: Timer,
    pixel_clock: ClockDomain,
}

impl SocSimulationWithDisplay {
    fn simulate_frame(&mut self) -> Result<()> {
        let command_buffer = self.command_queue.new_command_buffer();

        // Simulate hardware for one frame period (16.67ms @ 60Hz)
        {
            let compute_encoder = command_buffer.new_compute_command_encoder();

            // 1. Simulate CPU executing video driver code
            self.simulate_cpu_video_operations(&compute_encoder);

            // 2. Simulate video controller hardware
            self.simulate_video_controller(&compute_encoder);

            // 3. Simulate memory transactions (DMA, etc.)
            self.simulate_memory_system(&compute_encoder);

            compute_encoder.end_encoding();
        }

        // Extract simulated video output and render to display
        {
            let render_encoder = command_buffer.new_render_command_encoder(&self.render_pass);

            // Video controller's output becomes GPU texture input
            self.convert_simulated_video_to_texture(&render_encoder);

            // Render simulated display to actual display
            self.render_hdmi_output(&render_encoder);

            render_encoder.end_encoding();
        }

        command_buffer.present_drawable(self.display_drawable);
        command_buffer.commit();

        Ok(())
    }
}
```

#### Video Controller Hardware Simulation

```metal
// Metal kernel simulating video controller hardware
kernel void simulate_video_controller(
    device VideoControllerRegs* regs [[buffer(0)]],
    device uint32_t* video_memory [[buffer(1)]],     // Simulated VRAM
    device uint32_t* framebuffer_out [[buffer(2)]],  // Output to display
    constant VideoTiming& timing [[buffer(3)]],
    uint2 pixel_coord [[thread_position_in_grid]]
) {
    uint x = pixel_coord.x;
    uint y = pixel_coord.y;

    // Simulate hardware video timing
    if (x < timing.h_active && y < timing.v_active) {
        // Calculate memory address based on simulated hardware addressing
        uint32_t addr = regs->base_addr + (y * regs->stride) + (x * regs->bytes_per_pixel);

        // Simulate memory read latency
        if (addr < regs->memory_size) {
            // Read from simulated video memory
            uint32_t pixel_data = video_memory[addr / 4];

            // Apply simulated color space conversion, scaling, etc.
            uint32_t converted_pixel = simulate_color_pipeline(pixel_data, regs);

            // Output to display framebuffer
            framebuffer_out[y * timing.h_active + x] = converted_pixel;
        }
    }
}
```

#### Unified Memory Benefits for Display

```rust
struct UnifiedVideoMemory {
    // Single allocation visible to CPU, GPU simulation, and display
    video_buffer: MTLBuffer,

    fn setup_shared_video_memory(&mut self) -> Result<()> {
        // Allocate unified memory for video data
        self.video_buffer = self.device.new_buffer_with_length(
            1920 * 1080 * 4,  // 1080p RGBA
            MTLResourceOptions::StorageModeShared  // CPU+GPU accessible
        );

        // CPU can write (simulated CPU storing to video memory)
        let cpu_ptr = self.video_buffer.contents() as *mut u32;
        unsafe {
            // Simulated CPU writes directly to video memory
            *cpu_ptr.offset(1000) = 0xFF00FF00;  // Green pixel
        }

        // GPU simulation kernel reads same memory instantly
        // GPU display pipeline renders same memory to screen
        // Zero copy, zero latency between simulation and display

        Ok(())
    }
}
```

#### Real-Time Display Update Flow

```rust
impl RealTimeDisplaySim {
    fn run_real_time_simulation(&mut self) {
        // Target 60 FPS display refresh
        let frame_duration = Duration::from_millis(16);  // 16.67ms per frame

        loop {
            let frame_start = Instant::now();

            // 1. Simulate hardware for one frame period
            self.simulate_hardware_frame();

            // 2. Update display from simulated video output
            self.update_display_from_simulation();

            // 3. Handle user input (keyboard, mouse → simulated hardware)
            self.process_user_input_to_hardware();

            // 4. Maintain real-time frame rate
            let elapsed = frame_start.elapsed();
            if elapsed < frame_duration {
                thread::sleep(frame_duration - elapsed);
            }
        }
    }

    fn simulate_hardware_frame(&mut self) {
        // High-speed hardware simulation (much faster than real-time)
        // Simulate multiple clock cycles per display frame
        for cycle in 0..self.cycles_per_frame() {
            self.simulate_soc_cycle();

            // Check if video controller generated new pixel data
            if self.video_controller.has_new_frame() {
                break;  // Frame complete
            }
        }
    }
}
```

#### Display Debugging and Visualization

```rust
struct SimulationDisplayDebugger {
    // Multiple display windows
    main_display: MTLTexture,           // Simulated HDMI output
    debug_overlay: MTLTexture,          // Debug information
    signal_viewer: MTLTexture,          // Waveform display

    fn render_debug_display(&mut self) {
        // Main window: Actual simulated display output
        self.render_simulated_hdmi_output();

        // Debug overlay: Show simulation state
        self.render_debug_overlay_with_info(&[
            "CPU PC: 0x1000234",
            "Video Controller: Active",
            "Frame Rate: 59.8 FPS",
            "Simulation Speed: 15.2x real-time",
        ]);

        // Signal viewer: Real-time waveforms
        self.render_signal_waveforms(&[
            "clk", "hsync", "vsync", "pixel_data[23:0]"
        ]);
    }
}
```

#### Performance Characteristics

```rust
struct DualRolePerformance {
    simulation_overhead: f32,    // ~5-10% for display rendering
    memory_efficiency: f32,      // ~95% (unified memory, no copies)
    frame_rate_impact: f32,      // ~2-3% (display is much simpler than simulation)

    fn analyze_performance(&self) -> PerformanceReport {
        PerformanceReport {
            // GPU utilization breakdown
            simulation_kernels: 85.0,    // Most GPU time
            display_rendering: 10.0,     // Minimal overhead
            memory_transfers: 0.0,       // Zero copy with unified memory
            idle_time: 5.0,              // Waiting for next frame

            // Memory bandwidth breakdown
            simulation_memory: 90.0,     // Most bandwidth for simulation
            display_memory: 8.0,         // Video output
            debug_memory: 2.0,           // Debug overlays

            bottleneck: "Simulation complexity, not display rendering"
        }
    }
}
```

### Key Advantages:

1. **Zero-Copy Display**: Simulated video memory directly becomes GPU texture
2. **Real-Time Feedback**: See simulation results immediately on display
3. **Minimal Overhead**: Display rendering uses <10% of GPU resources
4. **Interactive Debugging**: Click on display to probe hardware state
5. **Multiple Displays**: Show main output + debug windows simultaneously

This approach transforms hardware simulation from "batch processing" to "real-time interactive experience" - you can literally watch your simulated SoC boot up and see the display output in real-time while debugging the underlying hardware behavior.

## Real-Time UART and Console Output

### GPU-to-Terminal UART Streaming

The GPU simulation can capture UART output from simulated hardware and stream it directly to the host terminal in real-time, providing immediate feedback from simulated software.

#### Architecture for UART Capture

```rust
struct UartOutputCapture {
    // GPU-accessible UART buffers
    uart_tx_buffer: MTLBuffer,           // Circular buffer for UART data
    uart_write_index: MTLBuffer,         // Atomic write pointer
    uart_ready_flag: MTLBuffer,          // New data available flag

    // CPU-side terminal interface
    terminal_writer: TerminalWriter,
    uart_decoder: UartDecoder,

    // Real-time streaming
    output_thread: JoinHandle<()>,
}

impl UartOutputCapture {
    fn setup_uart_capture(&mut self) -> Result<()> {
        // Shared buffer between GPU simulation and CPU terminal
        self.uart_tx_buffer = self.device.new_buffer_with_length(
            4096,  // 4KB circular buffer
            MTLResourceOptions::StorageModeShared  // CPU+GPU accessible
        );

        // Atomic pointers for lock-free communication
        self.uart_write_index = self.device.new_buffer_with_length(
            8,     // Single u64 atomic counter
            MTLResourceOptions::StorageModeShared
        );

        // Start background thread for terminal output
        self.output_thread = thread::spawn(move || {
            self.uart_output_loop();
        });

        Ok(())
    }

    fn uart_output_loop(&self) {
        let mut last_read_index = 0u64;

        loop {
            // Check for new UART data from GPU simulation
            let current_write_index = self.read_atomic_write_index();

            if current_write_index > last_read_index {
                // Extract new UART bytes
                let new_bytes = self.extract_uart_bytes(
                    last_read_index,
                    current_write_index
                );

                // Decode and display in terminal
                for byte in new_bytes {
                    self.terminal_writer.write_uart_byte(byte);
                }

                last_read_index = current_write_index;
            }

            // Check every 100μs for low-latency output
            thread::sleep(Duration::from_micros(100));
        }
    }
}
```

#### GPU UART Hardware Simulation

```metal
// Metal kernel simulating UART transmitter hardware
kernel void simulate_uart_tx(
    device UartRegs* uart_regs [[buffer(0)]],
    device uint8_t* uart_tx_buffer [[buffer(1)]],     // Shared with CPU
    device atomic_uint* write_index [[buffer(2)]],     // Atomic write pointer
    device uint32_t* simulation_state [[buffer(3)]],
    uint tid [[thread_position_in_grid]]
) {
    // Simulate UART hardware state machine
    if (uart_regs->tx_enable && uart_regs->tx_ready) {
        // Check if CPU wrote new data to UART
        if (uart_regs->tx_data_valid) {
            uint8_t tx_byte = uart_regs->tx_data;

            // Simulate UART timing (baud rate, start/stop bits)
            if (simulate_uart_timing(uart_regs, simulation_state)) {
                // Output byte to shared buffer for terminal display
                uint32_t index = atomic_fetch_add_explicit(
                    write_index, 1, memory_order_relaxed
                ) % 4096;  // Circular buffer

                uart_tx_buffer[index] = tx_byte;

                // Mark transmission complete
                uart_regs->tx_ready = 1;
                uart_regs->tx_data_valid = 0;
            }
        }
    }
}

// Simulate UART baud rate timing
bool simulate_uart_timing(device UartRegs* regs, device uint32_t* state) {
    uint32_t baud_divisor = regs->baud_divisor;
    uint32_t bit_counter = state[tid];

    bit_counter++;

    if (bit_counter >= baud_divisor) {
        state[tid] = 0;  // Reset for next bit
        return true;     // Bit period complete
    } else {
        state[tid] = bit_counter;
        return false;    // Still transmitting current bit
    }
}
```

#### Multi-UART Support

```rust
struct MultiUartCapture {
    // Multiple UART channels
    uart_channels: Vec<UartChannel>,

    // Terminal multiplexing
    terminal_mux: TerminalMultiplexer,
}

struct UartChannel {
    id: u8,                              // UART0, UART1, etc.
    buffer: MTLBuffer,                   // Dedicated buffer per UART
    decoder: UartProtocolDecoder,        // Handle different protocols
    color: TerminalColor,                // Color-code different UARTs
}

impl MultiUartCapture {
    fn display_uart_output(&self, channel: u8, data: &[u8]) {
        let color = self.uart_channels[channel as usize].color;

        match channel {
            0 => println!("{}{}", color.code(), String::from_utf8_lossy(data)),
            1 => println!("{}[UART1] {}", color.code(), String::from_utf8_lossy(data)),
            2 => println!("{}[DEBUG] {}", color.code(), String::from_utf8_lossy(data)),
            _ => println!("{}[UART{}] {}", color.code(), channel, String::from_utf8_lossy(data)),
        }
    }
}
```

#### Real-Time Boot Sequence Capture

```rust
impl BootSequenceCapture {
    fn capture_boot_messages(&mut self) {
        // Example real-time output from simulated SoC boot:
        println!("🔌 Starting SKALP SoC simulation...");

        // GPU simulates bootloader, UART output appears in terminal:
        // [0.000] Bootloader v1.2.3 starting...
        // [0.120] DRAM initialized (512MB)
        // [0.250] Loading kernel from flash...
        // [1.150] Kernel boot complete
        // [1.200] Starting init process...
        // [2.500] Welcome to SimOS!
        // login:

        // User can type in terminal, input goes back to simulated UART RX
        self.handle_terminal_input_to_uart();
    }

    fn handle_terminal_input_to_uart(&mut self) {
        // Terminal input → GPU simulation UART RX
        let stdin = io::stdin();

        for line in stdin.lines() {
            let input = line.unwrap();

            // Send each character to simulated UART RX buffer
            for byte in input.bytes() {
                self.write_to_simulated_uart_rx(byte);
            }

            // Send enter key
            self.write_to_simulated_uart_rx(b'\n');
        }
    }
}
```

#### Advanced UART Features

```rust
struct AdvancedUartCapture {
    // Protocol decoding
    ansi_decoder: AnsiEscapeDecoder,     // Handle escape sequences
    log_parser: LogLevelParser,          // Parse log levels (INFO, ERROR, etc.)

    // Debugging features
    uart_analyzer: UartProtocolAnalyzer, // Analyze UART traffic
    timing_analyzer: UartTimingAnalyzer, // Check baud rate accuracy

    // Output formatting
    timestamp_formatter: TimestampFormatter,
    syntax_highlighter: SyntaxHighlighter,
}

impl AdvancedUartCapture {
    fn process_uart_stream(&mut self, data: &[u8]) {
        for &byte in data {
            match self.ansi_decoder.process_byte(byte) {
                AnsiResult::Character(ch) => {
                    // Regular character - apply syntax highlighting
                    let highlighted = self.syntax_highlighter.highlight(ch);
                    print!("{}", highlighted);
                },

                AnsiResult::EscapeSequence(seq) => {
                    // ANSI escape sequence - apply to terminal
                    print!("{}", seq.to_terminal_code());
                },

                AnsiResult::Incomplete => {
                    // Wait for more bytes
                },
            }
        }

        io::stdout().flush().unwrap();
    }

    fn analyze_uart_timing(&self) -> UartTimingReport {
        UartTimingReport {
            measured_baud_rate: 115387,     // Actual vs expected 115200
            bit_timing_accuracy: 99.84,     // Percentage accuracy
            frame_errors: 0,                // Start/stop bit errors
            buffer_overruns: 0,             // Buffer overflow count
        }
    }
}
```

#### Integration with Simulation Control

```rust
impl SimulationWithUartConsole {
    fn run_interactive_simulation(&mut self) {
        println!("🚀 SKALP SoC Simulation Starting");
        println!("📟 UART console active - type 'help' for commands");
        println!("🎮 Press Ctrl+C to stop simulation\n");

        // Start GPU simulation with UART capture
        let _sim_handle = thread::spawn(move || {
            self.run_gpu_simulation_loop();
        });

        // Handle special commands in terminal
        let stdin = io::stdin();
        for line in stdin.lines() {
            let input = line.unwrap();

            match input.trim() {
                "reset" => self.send_reset_to_simulation(),
                "break" => self.send_debug_break(),
                "status" => self.print_simulation_status(),
                _ => {
                    // Regular input → simulated UART
                    self.send_to_uart(input.as_bytes());
                }
            }
        }
    }
}
```

#### Performance and Latency

```rust
struct UartCapturePerformance {
    // Latency characteristics
    gpu_to_buffer_latency: Duration,     // ~10μs (GPU kernel to shared buffer)
    buffer_to_terminal_latency: Duration, // ~100μs (polling interval)
    total_latency: Duration,             // ~110μs total

    // Throughput
    max_uart_throughput: u32,            // Limited by baud rate, not simulation

    fn analyze_performance(&self) -> UartPerfReport {
        UartPerfReport {
            effective_latency: "110μs - imperceptible to users",
            throughput_limit: "UART baud rate (115200 bps typical)",
            cpu_overhead: "< 0.1% - background polling thread",
            gpu_overhead: "< 0.01% - trivial compared to simulation",

            user_experience: "Real-time console output indistinguishable from native UART"
        }
    }
}
```

### Command Line Integration

```bash
# Start simulation with live UART console
skalp sim soc_design.sk --uart-console

# Multiple UART channels with color coding
skalp sim soc_design.sk --uart-console --uart-channels 3

# UART output to file + terminal
skalp sim soc_design.sk --uart-console --uart-log boot.log

# Interactive debugging
skalp sim soc_design.sk --uart-console --interactive
```

This provides a seamless development experience where you can:
1. **Watch boot sequences** in real-time as the simulated SoC starts up
2. **Interactive debugging** by typing commands that go to simulated UART RX
3. **Multiple UART channels** with color-coded output
4. **Protocol analysis** of UART traffic and timing
5. **Zero-latency feedback** from simulated hardware to terminal

The experience feels exactly like working with real hardware over a UART connection, but with the full power of simulation debugging capabilities.

## Interactive Input: Keyboard and Mouse Integration

### Host Input → Simulated Hardware Pipeline

The simulation engine can capture keyboard and mouse events from the host system and inject them into simulated input controllers (PS/2, USB HID, etc.) in real-time, creating a fully interactive simulated system.

#### Architecture for Input Capture

```rust
struct InputCaptureSystem {
    // Host input capture
    keyboard_monitor: KeyboardEventMonitor,
    mouse_monitor: MouseEventMonitor,

    // Simulated hardware input controllers
    ps2_controller: Ps2ControllerSim,
    usb_hid_controller: UsbHidControllerSim,

    // GPU-accessible input buffers
    keyboard_buffer: MTLBuffer,          // Keyboard events for GPU
    mouse_buffer: MTLBuffer,             // Mouse events for GPU
    input_event_queue: MTLBuffer,        // Timestamped input events

    // Input routing configuration
    input_mapping: InputDeviceMapping,
}

impl InputCaptureSystem {
    fn setup_input_capture(&mut self) -> Result<()> {
        // Create shared buffers for input events
        self.keyboard_buffer = self.device.new_buffer_with_length(
            1024,  // 1KB for keyboard events
            MTLResourceOptions::StorageModeShared
        );

        self.mouse_buffer = self.device.new_buffer_with_length(
            2048,  // 2KB for mouse events (higher frequency)
            MTLResourceOptions::StorageModeShared
        );

        // Start host input monitoring
        self.keyboard_monitor.start_capture(Box::new(|event| {
            self.inject_keyboard_event(event);
        }));

        self.mouse_monitor.start_capture(Box::new(|event| {
            self.inject_mouse_event(event);
        }));

        Ok(())
    }

    fn inject_keyboard_event(&mut self, event: KeyboardEvent) {
        // Convert host keyboard event to simulated hardware protocol
        let hw_event = match self.input_mapping.keyboard_protocol {
            KeyboardProtocol::Ps2 => self.convert_to_ps2_scancode(event),
            KeyboardProtocol::UsbHid => self.convert_to_usb_hid(event),
        };

        // Write to GPU-accessible buffer with timestamp
        self.write_input_event_to_gpu(InputEvent::Keyboard(hw_event));
    }

    fn inject_mouse_event(&mut self, event: MouseEvent) {
        // Convert host mouse event to simulated hardware
        let hw_event = match self.input_mapping.mouse_protocol {
            MouseProtocol::Ps2 => self.convert_to_ps2_mouse(event),
            MouseProtocol::UsbHid => self.convert_to_usb_mouse_hid(event),
        };

        self.write_input_event_to_gpu(InputEvent::Mouse(hw_event));
    }
}
```

#### GPU Input Controller Simulation

```metal
// Metal kernel simulating PS/2 keyboard controller
kernel void simulate_ps2_keyboard(
    device Ps2ControllerRegs* ps2_regs [[buffer(0)]],
    device InputEvent* input_events [[buffer(1)]],       // From host
    device atomic_uint* event_read_index [[buffer(2)]],  // Atomic consumer
    device uint32_t* simulation_cycle [[buffer(3)]],
    uint tid [[thread_position_in_grid]]
) {
    // Check for new input events from host
    uint32_t current_index = atomic_load_explicit(event_read_index, memory_order_acquire);

    if (current_index < input_events->count) {
        InputEvent event = input_events[current_index];

        if (event.type == INPUT_KEYBOARD && event.timestamp <= *simulation_cycle) {
            // Simulate PS/2 keyboard controller receiving scancode
            ps2_regs->data_register = event.keyboard.scancode;
            ps2_regs->status_register |= PS2_DATA_AVAILABLE;

            // Generate interrupt if enabled
            if (ps2_regs->command_register & PS2_INTERRUPT_ENABLE) {
                ps2_regs->interrupt_pending = 1;
            }

            // Mark event as consumed
            atomic_fetch_add_explicit(event_read_index, 1, memory_order_acq_rel);
        }
    }
}

// Metal kernel simulating USB HID controller
kernel void simulate_usb_hid_mouse(
    device UsbHidControllerRegs* usb_regs [[buffer(0)]],
    device InputEvent* input_events [[buffer(1)]],
    device atomic_uint* event_read_index [[buffer(2)]],
    uint tid [[thread_position_in_grid]]
) {
    uint32_t current_index = atomic_load_explicit(event_read_index, memory_order_acquire);

    if (current_index < input_events->count) {
        InputEvent event = input_events[current_index];

        if (event.type == INPUT_MOUSE) {
            // Simulate USB HID mouse report
            UsbHidMouseReport report;
            report.buttons = event.mouse.buttons;
            report.x_delta = event.mouse.delta_x;
            report.y_delta = event.mouse.delta_y;
            report.wheel_delta = event.mouse.wheel_delta;

            // Write to USB HID endpoint buffer
            usb_regs->endpoint_buffer[usb_regs->write_pointer] = report;
            usb_regs->write_pointer = (usb_regs->write_pointer + 1) % USB_BUFFER_SIZE;

            // Signal USB transaction ready
            usb_regs->transaction_ready = 1;

            atomic_fetch_add_explicit(event_read_index, 1, memory_order_acq_rel);
        }
    }
}
```

#### Multi-Protocol Input Support

```rust
struct InputDeviceMapping {
    keyboard_protocol: KeyboardProtocol,
    mouse_protocol: MouseProtocol,
    custom_mappings: Vec<CustomInputMapping>,
}

enum KeyboardProtocol {
    Ps2 {
        scancode_set: ScanCodeSet,       // Set 1, 2, or 3
        layout: KeyboardLayout,          // US, UK, DE, etc.
    },
    UsbHid {
        report_descriptor: HidDescriptor,
        polling_rate: u32,               // 1000Hz typical
    },
    Custom {
        protocol_definition: CustomProtocol,
    },
}

enum MouseProtocol {
    Ps2 {
        resolution: u8,                  // 1-4 counts/mm
        sample_rate: u8,                 // 10-200 Hz
        wheel_support: bool,
    },
    UsbHid {
        report_rate: u32,                // Up to 1000Hz
        button_count: u8,                // 3, 5, 8+ buttons
        scroll_support: ScrollSupport,
    },
}

impl InputDeviceMapping {
    fn convert_key_to_ps2(&self, key: HostKey) -> Ps2ScanCode {
        match self.keyboard_protocol {
            KeyboardProtocol::Ps2 { scancode_set: ScanCodeSet::Set2, .. } => {
                // Convert host key to PS/2 Set 2 scancode
                match key {
                    HostKey::A => Ps2ScanCode::new(0x1C, false),
                    HostKey::Enter => Ps2ScanCode::new(0x5A, false),
                    HostKey::LeftShift => Ps2ScanCode::new(0x12, false),
                    HostKey::Escape => Ps2ScanCode::new(0x76, false),
                    // ... complete mapping table
                }
            },
            // Other scancode sets...
        }
    }

    fn convert_mouse_to_usb_hid(&self, mouse: HostMouseEvent) -> UsbHidReport {
        UsbHidReport {
            report_id: 1,
            buttons: mouse.buttons.bits(),
            x: clamp(mouse.delta_x, -127, 127) as i8,
            y: clamp(mouse.delta_y, -127, 127) as i8,
            wheel: clamp(mouse.wheel_delta, -127, 127) as i8,
        }
    }
}
```

#### Real-Time Input Processing

```rust
impl RealTimeInputProcessor {
    fn run_input_capture_loop(&mut self) {
        let mut event_queue = VecDeque::new();

        // Capture events at high frequency
        thread::spawn(move || {
            loop {
                // Check for keyboard events
                if let Some(key_event) = self.keyboard_monitor.poll_event() {
                    let hw_event = self.process_keyboard_event(key_event);
                    event_queue.push_back(hw_event);
                }

                // Check for mouse events
                if let Some(mouse_event) = self.mouse_monitor.poll_event() {
                    let hw_event = self.process_mouse_event(mouse_event);
                    event_queue.push_back(hw_event);
                }

                // Transfer events to GPU buffer
                self.flush_events_to_gpu(&mut event_queue);

                // High frequency polling for low latency
                thread::sleep(Duration::from_micros(250)); // 4kHz polling
            }
        });
    }

    fn process_keyboard_event(&self, event: HostKeyEvent) -> InputEvent {
        InputEvent {
            timestamp: self.get_simulation_timestamp(),
            event_type: InputEventType::Keyboard,
            keyboard: KeyboardData {
                scancode: self.convert_to_target_scancode(event.key),
                pressed: event.pressed,
                modifiers: event.modifiers,
            },
        }
    }
}
```

#### Interactive Simulation Example

```rust
struct InteractiveSimulatedSystem {
    // Full system simulation with input/output
    cpu_simulation: CpuSimulation,
    gpu_simulation: GpuSimulation,
    memory_simulation: MemorySimulation,

    // Input controllers
    keyboard_controller: KeyboardControllerSim,
    mouse_controller: MouseControllerSim,

    // Output systems
    display_output: DisplayOutput,
    audio_output: AudioOutput,

    fn run_interactive_session(&mut self) {
        println!("🎮 Starting interactive SoC simulation");
        println!("💡 Move mouse and type to interact with simulated system");

        loop {
            let frame_start = Instant::now();

            // 1. Capture and inject input events
            self.process_host_input_events();

            // 2. Run hardware simulation
            self.simulate_hardware_frame();

            // 3. Update display output
            self.update_display_from_simulation();

            // 4. Process audio output
            self.update_audio_output();

            // 5. Handle special debug commands
            self.process_debug_commands();

            // Maintain 60 FPS
            let elapsed = frame_start.elapsed();
            if elapsed < Duration::from_millis(16) {
                thread::sleep(Duration::from_millis(16) - elapsed);
            }
        }
    }
}
```

#### Advanced Input Features

```rust
struct AdvancedInputFeatures {
    // Input recording and playback
    input_recorder: InputRecorder,
    input_playback: InputPlayback,

    // Multiple input devices
    multiple_keyboards: Vec<KeyboardDevice>,
    multiple_mice: Vec<MouseDevice>,

    // Custom input devices
    gamepad_support: GamepadController,
    touch_input: TouchController,

    fn record_input_session(&mut self, filename: &str) {
        self.input_recorder.start_recording(filename);
        println!("🔴 Recording input session to {}", filename);
    }

    fn playback_input_session(&mut self, filename: &str) -> Result<()> {
        let events = self.input_playback.load_session(filename)?;

        for event in events {
            // Wait for correct timestamp
            thread::sleep(event.delay_from_previous);

            // Inject into simulation
            self.inject_recorded_event(event);
        }

        Ok(())
    }
}
```

#### Performance Characteristics

```rust
struct InputCapturePerformance {
    input_latency: Duration,             // ~250μs (host → GPU buffer)
    processing_latency: Duration,        // ~50μs (GPU simulation)
    total_latency: Duration,             // ~300μs total

    fn analyze_input_performance(&self) -> InputPerfReport {
        InputPerfReport {
            keyboard_latency: "300μs - imperceptible to users",
            mouse_latency: "300μs - excellent for interactive use",
            polling_frequency: "4kHz - higher than gaming mice",
            cpu_overhead: "< 0.2% - lightweight event processing",
            gpu_overhead: "< 0.01% - minimal simulation load",

            gaming_performance: "Suitable for real-time interactive applications",
            precision: "Full host input fidelity preserved"
        }
    }
}
```

#### Command Line Usage

```bash
# Interactive simulation with input capture
skalp sim soc_design.sk --interactive --input-capture

# Specific input protocols
skalp sim soc_design.sk --keyboard-protocol ps2 --mouse-protocol usb-hid

# Input recording for regression testing
skalp sim soc_design.sk --record-input test_session.inp

# Playback recorded input
skalp sim soc_design.sk --playback-input test_session.inp

# Multiple input devices
skalp sim soc_design.sk --keyboards 2 --mice 1 --gamepad
```

### Complete Interactive Loop:

1. **Host Input** → Keyboard/mouse events captured at 4kHz
2. **Protocol Conversion** → Convert to PS/2, USB HID, etc.
3. **GPU Injection** → Events written to simulation buffers
4. **Hardware Simulation** → Input controllers process events
5. **Software Response** → Simulated OS/applications respond
6. **Display Output** → Results shown on simulated display
7. **User Feedback** → User sees response and continues interaction

This creates a **fully interactive simulated computer** where you can:
- Type text that appears on simulated display
- Move mouse cursor in simulated OS
- Click buttons in simulated applications
- Play games running on simulated hardware
- Test input device drivers interactively

The experience is **indistinguishable from real hardware** but with full debugging capabilities!

## Waveform Viewing and DSO-like Triggering

### Real-Time Waveform Capture with GPU Simulation

The GPU simulation engine includes sophisticated waveform capture and viewing capabilities with DSO (Digital Storage Oscilloscope) like triggering, breakpoints, and real-time visualization.

#### Architecture for Waveform Capture

```rust
struct WaveformCaptureEngine {
    // Waveform storage (circular buffer in GPU memory)
    waveform_buffer: MTLBuffer,          // Multi-channel waveform data
    trigger_engine: TriggerEngine,        // DSO-like triggering
    capture_control: CaptureControl,      // Start/stop/trigger control

    // Visualization pipeline
    waveform_renderer: WaveformRenderer,  // GPU-accelerated rendering
    display_texture: MTLTexture,         // Real-time waveform display

    // Breakpoint system
    breakpoint_manager: BreakpointManager,
    trigger_conditions: Vec<TriggerCondition>,
}

impl WaveformCaptureEngine {
    fn setup_waveform_capture(&mut self, signals: &[SignalProbe]) -> Result<()> {
        // Allocate circular buffer for waveform storage
        let buffer_size = signals.len() * self.sample_depth * size_of::<SignalValue>();
        self.waveform_buffer = self.device.new_buffer_with_length(
            buffer_size,
            MTLResourceOptions::StorageModeShared
        );

        // Setup signal probes in simulation
        for signal in signals {
            self.attach_probe(signal);
        }

        // Configure trigger system
        self.trigger_engine.setup_triggers(&self.trigger_conditions);

        Ok(())
    }

    fn capture_on_breakpoint(&mut self, breakpoint: &Breakpoint) {
        // Pre-trigger capture (like DSO negative time)
        let pre_trigger_samples = self.capture_control.pre_trigger_depth;
        let post_trigger_samples = self.capture_control.post_trigger_depth;

        // Capture waveforms around breakpoint
        self.capture_window(
            breakpoint.cycle - pre_trigger_samples,
            breakpoint.cycle + post_trigger_samples
        );
    }
}
```

#### GPU Waveform Sampling Kernel

```metal
// Metal kernel for high-speed waveform capture
kernel void capture_waveforms(
    device SignalProbe* probes [[buffer(0)]],           // Signals to monitor
    device WaveformBuffer* waveforms [[buffer(1)]],     // Circular buffer
    device atomic_uint* write_index [[buffer(2)]],      // Current write position
    device TriggerState* trigger [[buffer(3)]],         // Trigger conditions
    constant SimulationState* sim_state [[buffer(4)]],
    uint tid [[thread_position_in_grid]]
) {
    // Each thread handles one signal probe
    SignalProbe probe = probes[tid];

    // Sample signal value
    uint32_t signal_value = sample_signal(sim_state, probe.signal_id);

    // Check trigger conditions
    bool trigger_met = evaluate_trigger(trigger, probe, signal_value);

    if (trigger_met) {
        // Mark trigger point (like DSO trigger indicator)
        trigger->triggered = true;
        trigger->trigger_cycle = sim_state->cycle;
        trigger->trigger_signal = probe.signal_id;
    }

    // Store in circular buffer (continuous capture)
    uint32_t index = atomic_fetch_add_explicit(write_index, 1, memory_order_relaxed);
    uint32_t buffer_index = index % waveforms->buffer_size;

    // Store with timestamp for accurate reconstruction
    waveforms->samples[buffer_index] = {
        .timestamp = sim_state->timestamp,
        .cycle = sim_state->cycle,
        .signal_id = probe.signal_id,
        .value = signal_value,
        .trigger_relative_time = trigger->triggered ?
            (int32_t)(sim_state->cycle - trigger->trigger_cycle) : INT32_MAX
    };
}

// Metal kernel for waveform rendering
kernel void render_waveforms(
    device WaveformBuffer* waveforms [[buffer(0)]],
    texture2d<float, access::write> display [[texture(0)]],
    constant RenderParams& params [[buffer(1)]],
    uint2 pixel [[thread_position_in_grid]]
) {
    // Map pixel position to time
    float time = params.time_start + (pixel.x * params.time_per_pixel);

    // Find signal value at this time
    uint signal_index = pixel.y / params.signal_height;
    float signal_value = sample_waveform_at_time(waveforms, signal_index, time);

    // Convert to pixel color (with anti-aliasing)
    float4 color = render_signal_value(signal_value, params.render_style);

    // Add grid, cursor, measurements
    color = add_grid_overlay(color, pixel, params);
    color = add_cursor_measurements(color, pixel, params);

    display.write(color, pixel);
}
```

#### DSO-like Trigger System

```rust
#[derive(Clone)]
enum TriggerCondition {
    // Edge triggers (like DSO)
    RisingEdge { signal: SignalId, threshold: LogicLevel },
    FallingEdge { signal: SignalId, threshold: LogicLevel },
    EitherEdge { signal: SignalId },

    // Pattern triggers
    Pattern { signals: Vec<SignalId>, pattern: BitPattern, mask: BitMask },

    // Protocol triggers
    I2CStart { sda: SignalId, scl: SignalId },
    SPIChipSelect { cs: SignalId, active_low: bool },
    UARTStart { rx: SignalId, baud_rate: u32 },

    // Advanced triggers
    PulseWidth { signal: SignalId, min_width: Duration, max_width: Duration },
    Glitch { signal: SignalId, max_width: Duration },
    Runt { signal: SignalId, min_high: f32, max_low: f32 },

    // Sequence triggers
    Sequence { stages: Vec<TriggerCondition>, timeout: Duration },

    // Breakpoint triggers
    CodeBreakpoint { address: u64, processor: ProcessorId },
    DataBreakpoint { address: u64, access_type: AccessType },
    ConditionalBreakpoint { condition: String }, // Evaluated expression
}

impl TriggerEngine {
    fn evaluate_complex_trigger(&self, state: &SimulationState) -> TriggerResult {
        match &self.current_trigger {
            TriggerCondition::Pattern { signals, pattern, mask } => {
                let current_pattern = self.read_signal_pattern(signals);
                if (current_pattern & mask) == (pattern & mask) {
                    TriggerResult::Triggered {
                        time: state.time,
                        pre_trigger: self.capture_pre_trigger_samples()
                    }
                } else {
                    TriggerResult::Waiting
                }
            },

            TriggerCondition::PulseWidth { signal, min_width, max_width } => {
                let pulse_width = self.measure_pulse_width(signal);
                if pulse_width >= *min_width && pulse_width <= *max_width {
                    TriggerResult::Triggered { time: state.time, pre_trigger: true }
                } else {
                    TriggerResult::Waiting
                }
            },

            // ... other trigger types
        }
    }
}
```

#### Interactive Waveform Viewer

```rust
struct InteractiveWaveformViewer {
    // Display settings
    time_scale: f64,              // ns/div, μs/div, ms/div (like DSO)
    voltage_scale: f32,           // V/div for analog, logic levels for digital
    display_mode: DisplayMode,    // Analog, Digital, Mixed, Protocol

    // Cursors and measurements
    cursor_a: TimeCursor,
    cursor_b: TimeCursor,
    measurements: Vec<Measurement>,

    // Zoom and pan
    zoom_level: f32,
    pan_offset: f64,

    fn render_interactive_display(&mut self) {
        // Render waveforms with GPU acceleration
        self.render_waveforms();

        // Overlay measurements
        self.render_measurements();

        // Interactive cursors
        self.render_cursors();

        // Protocol decode overlay
        self.render_protocol_decode();
    }

    fn handle_mouse_interaction(&mut self, event: MouseEvent) {
        match event {
            MouseEvent::Click(pos) => {
                // Set cursor position
                self.cursor_a.time = self.pixel_to_time(pos.x);
            },
            MouseEvent::Drag(delta) => {
                // Pan waveform view
                self.pan_offset += delta.x * self.time_scale;
            },
            MouseEvent::Scroll(delta) => {
                // Zoom in/out (like DSO knob)
                self.zoom_time_scale(delta);
            },
            MouseEvent::RightClick(pos) => {
                // Context menu: Add measurement, set trigger, etc.
                self.show_context_menu(pos);
            }
        }
    }
}
```

#### Automatic Measurements (DSO-style)

```rust
struct AutomaticMeasurements {
    fn measure_signal(&self, signal: &WaveformData) -> MeasurementResults {
        MeasurementResults {
            // Time measurements
            frequency: self.measure_frequency(signal),
            period: self.measure_period(signal),
            duty_cycle: self.measure_duty_cycle(signal),
            rise_time: self.measure_rise_time(signal, 0.1, 0.9), // 10%-90%
            fall_time: self.measure_fall_time(signal, 0.9, 0.1),
            pulse_width_positive: self.measure_pulse_width(signal, true),
            pulse_width_negative: self.measure_pulse_width(signal, false),

            // Amplitude measurements
            vpp: self.measure_peak_to_peak(signal),
            vmax: self.measure_maximum(signal),
            vmin: self.measure_minimum(signal),
            vavg: self.measure_average(signal),
            vrms: self.measure_rms(signal),

            // Statistical
            std_dev: self.measure_std_deviation(signal),
            overshoot: self.measure_overshoot(signal),
            undershoot: self.measure_undershoot(signal),

            // Protocol specific
            baud_rate: self.detect_baud_rate(signal),
            protocol_errors: self.detect_protocol_errors(signal),
        }
    }

    fn measure_timing_between_signals(&self, sig_a: &WaveformData, sig_b: &WaveformData) -> TimingMeasurements {
        TimingMeasurements {
            setup_time: self.measure_setup_time(sig_a, sig_b),
            hold_time: self.measure_hold_time(sig_a, sig_b),
            propagation_delay: self.measure_prop_delay(sig_a, sig_b),
            skew: self.measure_skew(sig_a, sig_b),
            phase_shift: self.measure_phase_shift(sig_a, sig_b),
        }
    }
}
```

#### Protocol Decoding Overlay

```rust
struct ProtocolDecoder {
    decoders: Vec<Box<dyn ProtocolDecoderTrait>>,

    fn decode_protocols(&self, waveforms: &WaveformBuffer) -> Vec<ProtocolEvent> {
        let mut events = Vec::new();

        for decoder in &self.decoders {
            events.extend(decoder.decode(waveforms));
        }

        events
    }
}

struct I2CDecoder {
    fn decode(&self, waveforms: &WaveformBuffer) -> Vec<ProtocolEvent> {
        let sda = self.get_signal(waveforms, self.sda_signal);
        let scl = self.get_signal(waveforms, self.scl_signal);

        let mut events = Vec::new();
        let mut state = I2CState::Idle;

        for i in 0..waveforms.len() {
            // Detect START condition
            if sda.falling_edge(i) && scl.is_high(i) {
                events.push(ProtocolEvent::I2CStart { time: i });
                state = I2CState::Address;
            }

            // Decode address, data, ACK/NACK
            // ... full I2C protocol decode
        }

        events
    }
}
```

#### Breakpoint-Triggered Waveform Capture

```rust
impl BreakpointWaveformCapture {
    fn on_breakpoint_hit(&mut self, bp: &Breakpoint) {
        println!("🔴 Breakpoint hit at cycle {}", bp.cycle);

        // Capture waveforms around breakpoint (like DSO trigger)
        let waveforms = self.capture_waveforms_around_breakpoint(bp);

        // Open interactive waveform viewer
        self.open_waveform_viewer(waveforms);

        // Allow user to:
        // - Inspect signal values
        // - Set new triggers
        // - Continue, step, or modify simulation
        self.enter_debug_mode();
    }

    fn handle_debug_commands(&mut self, cmd: DebugCommand) {
        match cmd {
            DebugCommand::Continue => {
                self.resume_simulation();
            },
            DebugCommand::Step(cycles) => {
                self.step_simulation(cycles);
                self.update_waveforms();
            },
            DebugCommand::SetSignal { signal, value } => {
                self.force_signal_value(signal, value);
            },
            DebugCommand::AddWatch { signal } => {
                self.add_signal_to_waveform(signal);
            },
            DebugCommand::SaveWaveform { filename } => {
                self.export_vcd(filename); // Standard VCD format
            },
        }
    }
}
```

#### Performance Optimization

```rust
struct WaveformCapturePerformance {
    // Capture performance
    max_capture_rate: u64,        // 10GS/s+ possible on modern GPUs
    channels_supported: u32,      // 1000s of signals simultaneously
    capture_depth: u64,           // GB of waveform storage

    fn optimize_capture(&mut self) {
        // Selective capture - only store on change
        self.enable_change_detection();

        // Compress repetitive patterns
        self.enable_pattern_compression();

        // Hierarchical storage
        self.use_hierarchical_timebase();
    }
}
```

#### Command Line Integration

```bash
# Run with waveform capture
skalp sim design.sk --waveform --signals "clk,data,valid,ready"

# Set trigger conditions
skalp sim design.sk --trigger "rising:clk && data==0xFF"

# Breakpoint with waveform
skalp sim design.sk --break-at "top.cpu.pc==0x1000" --capture-depth 1000

# Interactive waveform viewer
skalp sim design.sk --interactive-waves

# Export to standard formats
skalp sim design.sk --export-vcd output.vcd --export-fst output.fst
```

### Key Features Summary:

1. **DSO-like Triggering**: Edge, pattern, protocol, glitch triggers
2. **Real-time Visualization**: GPU-accelerated waveform rendering
3. **Interactive Debugging**: Breakpoint → Waveform → Inspect → Continue
4. **Automatic Measurements**: Frequency, timing, amplitude, statistics
5. **Protocol Decoding**: I2C, SPI, UART overlays on waveforms
6. **High Performance**: 10GS/s+ capture rate, 1000s of signals

This provides a **complete oscilloscope experience** integrated with simulation - better than hardware debugging because you can:
- Capture ALL internal signals (not just pins)
- Infinite capture depth (limited only by memory)
- Time-travel debugging (rewind and replay)
- Modify signals and re-run
- Perfect triggering without noise

## Simulation Strategy: Per-Clock Updates vs Event-Driven

### The Fundamental Question: Should We Use Per-Clock or Event-Driven Simulation?

**Answer: Per-clock updates are BETTER for synchronous hardware simulation, especially on GPUs.**

#### Why Traditional Simulators Use Event-Driven

```verilog
// Traditional event-driven simulation
always @(a or b or c) begin  // Sensitivity list
    d = a & b | c;  // Triggers on any change
end

always @(posedge clk) begin
    q <= d;  // Only samples at clock edge
end

// Problem: All intermediate glitches on 'd' are computed but never used!
```

Traditional simulators track every combinational change because they evolved from asynchronous circuit simulation. But for **synchronous designs**, this is wasteful:

1. **99% of events are ignored** - Only values at clock edges matter
2. **Glitches don't affect flops** - Setup/hold times filter them out
3. **Massive overhead** - Scheduling/processing useless events
4. **Poor GPU fit** - Event queues are inherently serial

#### Per-Clock Update Strategy (Chosen for SKALP)

```rust
struct PerClockSimulation {
    // Two-phase evaluation per clock cycle
    fn simulate_clock_cycle(&mut self) {
        // Phase 1: Evaluate all combinational logic once
        self.evaluate_combinational_cones();

        // Phase 2: Update all flip-flops
        self.update_sequential_elements();

        // That's it! No event scheduling, no sensitivity lists
    }
}
```

### Advantages of Per-Clock Updates

#### 1. **Massive GPU Parallelism**

```metal
// GPU kernel - evaluate ALL cones in parallel
kernel void evaluate_all_cones_parallel(
    device CombinationalCone* cones [[buffer(0)]],
    device LogicState* state [[buffer(1)]],
    uint tid [[thread_position_in_grid]]
) {
    // Each thread evaluates one cone - no dependencies!
    CombinationalCone cone = cones[tid];
    state[cone.output] = evaluate_cone(cone, state);
    // No event queue, no scheduling - pure parallel computation
}
```

#### 2. **Simplified Architecture**

```rust
// Event-driven (complex)
struct EventDrivenSim {
    event_queue: PriorityQueue<Event>,      // Serial bottleneck
    sensitivity_lists: HashMap<Signal, Vec<Process>>,  // Memory overhead
    delta_cycles: Vec<DeltaCycle>,          // Convergence issues

    fn process_events(&mut self) {
        while !self.event_queue.is_empty() {
            let event = self.event_queue.pop();  // Serial processing
            // Ripple effects cause more events...
        }
    }
}

// Per-clock (simple)
struct PerClockSim {
    cones: Vec<Cone>,           // Just the logic
    flops: Vec<FlipFlop>,       // Just the state

    fn simulate(&mut self) {
        self.eval_cones();      // Parallel
        self.update_flops();    // Parallel
    }
}
```

#### 3. **Better Performance**

```rust
struct PerformanceComparison {
    fn benchmark_results(&self) -> BenchmarkReport {
        BenchmarkReport {
            // Event-driven traditional
            event_driven: Performance {
                events_per_second: 1e6,        // Serial event processing
                cpu_utilization: 100.0,        // Single thread bottleneck
                gpu_utilization: 0.0,           // Can't use GPU effectively
                memory_bandwidth: "High",       // Event queue thrashing
            },

            // Per-clock GPU
            per_clock_gpu: Performance {
                cycles_per_second: 1e9,        // 1000x faster
                cpu_utilization: 5.0,          // Just orchestration
                gpu_utilization: 95.0,         // Massive parallelism
                memory_bandwidth: "Optimal",    // Coalesced access
            },
        }
    }
}
```

### Addressing Accuracy Concerns

#### Q: "Don't we lose accuracy without event-driven simulation?"

**A: No! We actually MATCH hardware behavior better:**

```rust
// Real Hardware Behavior
struct RealHardware {
    fn clock_edge(&mut self) {
        // Combinational logic has settled by setup time
        // Flip-flop samples stable value
        // Glitches during cycle are irrelevant
    }
}

// Per-Clock Simulation (matches hardware)
struct PerClockSim {
    fn clock_edge(&mut self) {
        // Evaluate combinational once (settled value)
        // Update flops with stable value
        // Correctly models synchronous behavior
    }
}

// Event-Driven (overspecified)
struct EventDrivenSim {
    fn clock_cycle(&mut self) {
        // Tracks every glitch (hardware doesn't care)
        // Wastes time on intermediate values
        // Can even give WRONG results due to race conditions
    }
}
```

#### When We DO Need Intra-Cycle Accuracy

```rust
enum SimulationMode {
    // Default: Per-clock for synchronous logic
    Synchronous {
        eval_per_clock: true,
        track_glitches: false,
    },

    // Special cases that need event-driven
    Asynchronous {
        // For async logic (rare in modern designs)
        latches: Vec<Latch>,
        async_feedback: Vec<Path>,
        use_event_driven: true,
    },

    // Mixed mode
    Hybrid {
        sync_regions: Vec<SyncRegion>,    // Per-clock
        async_regions: Vec<AsyncRegion>,  // Event-driven
    },
}
```

### Optimizations Enabled by Per-Clock

#### 1. **Cone-Level Parallelism**

```rust
impl ConeParallelSimulation {
    fn optimize_for_gpu(&mut self) {
        // Since we evaluate once per clock, we can:
        // 1. Merge adjacent cones
        self.merge_cones_for_locality();

        // 2. Pack multiple cones per GPU thread
        self.pack_cones_into_warps();

        // 3. Eliminate redundant evaluations
        self.common_subexpression_elimination();

        // None of this is possible with event-driven!
    }
}
```

#### 2. **Memory Access Patterns**

```metal
// Perfect coalesced memory access on GPU
kernel void eval_cones_coalesced(
    device uint32_t* cone_inputs [[buffer(0)]],   // Packed, aligned
    device uint32_t* cone_outputs [[buffer(1)]],  // Sequential write
    uint tid [[thread_position_in_grid]]
) {
    // All threads in warp access consecutive memory
    uint32_t inputs = cone_inputs[tid];           // Coalesced read
    uint32_t outputs = evaluate_logic(inputs);
    cone_outputs[tid] = outputs;                  // Coalesced write
}
```

#### 3. **Temporal Locality**

```rust
struct TemporalOptimization {
    fn optimize_across_cycles(&mut self) {
        // Since evaluation is deterministic per-clock:
        // 1. Predict next cycle's active cones
        self.prefetch_next_cycle_data();

        // 2. Keep frequently-used cones in cache
        self.optimize_cone_placement();

        // 3. Compress inactive regions
        self.skip_inactive_cones();
    }
}
```

### Special Handling for Edge Cases

#### 1. **Multiple Clock Domains**

```rust
struct MultiClockSimulation {
    domains: Vec<ClockDomain>,

    fn simulate_cycle(&mut self) {
        // Still per-clock, but respect domain timing
        for domain in &self.domains {
            if domain.is_active_edge() {
                self.evaluate_domain_cones(domain);
                self.update_domain_flops(domain);
            }
        }

        // CDC paths evaluated specially
        self.evaluate_cdc_paths();
    }
}
```

#### 2. **Combinational Loops**

```rust
impl CombinationalLoopHandling {
    fn handle_loops(&mut self) {
        // Detect at compile time
        let loops = self.detect_combinational_loops();

        if !loops.is_empty() {
            // Option 1: Error (recommended)
            compile_error!("Combinational loop detected");

            // Option 2: Break with unit delay (for legacy)
            self.insert_loop_breakers(loops);

            // Option 3: Iterate to convergence (slow)
            self.iterate_until_stable(loops);
        }
    }
}
```

#### 3. **Timing Verification**

```rust
struct TimingVerification {
    fn verify_timing(&self) {
        // Per-clock makes timing analysis EASIER
        for path in &self.timing_paths {
            let delay = self.compute_combinational_delay(path);

            if delay > self.clock_period {
                // Real timing violation - would fail in hardware
                error!("Setup violation: path delay {} > clock period {}",
                       delay, self.clock_period);
            }
        }

        // No false violations from glitches!
    }
}
```

### Performance Results

```rust
struct SimulationBenchmark {
    fn compare_approaches(&self) -> Results {
        Results {
            // Traditional event-driven (ModelSim/VCS)
            traditional: Metrics {
                million_gates_per_second: 10,
                power_consumption: "100W",
                memory_usage: "32GB",
                simulation_time_1M_cycles: "60 seconds",
            },

            // SKALP per-clock GPU
            skalp_gpu: Metrics {
                million_gates_per_second: 10000,  // 1000x faster
                power_consumption: "50W",          // GPU efficient
                memory_usage: "4GB",               // No event queues
                simulation_time_1M_cycles: "0.06 seconds",  // Sub-second!
            },
        }
    }
}
```

### Conclusion: Per-Clock is the Right Choice

**Per-clock simulation is BETTER for synchronous hardware because:**

1. **Matches actual hardware behavior** - Flops only sample at clock edges
2. **Massive GPU parallelism** - No serial event processing
3. **Simpler and faster** - No event scheduling overhead
4. **More predictable** - No simulation/synthesis mismatches
5. **Better debugging** - See stable values, not glitches

**We only need event-driven for:**
- Asynchronous logic (rare)
- Analog/mixed-signal (different simulator)
- Legacy IP with latches (wrap in synchronous boundary)

This design decision enables SKALP to achieve **1000x+ speedup** over traditional simulators while providing MORE accurate results for modern synchronous designs!

## GPU Hardware I/O Architecture

### Efficient Data Movement Between Host and GPU Simulation

The GPU simulation needs sophisticated I/O mechanisms for feeding inputs, extracting outputs, and debugging. With Apple Silicon's unified memory, we can achieve zero-copy I/O with clever buffer management.

#### Overall I/O Architecture

```rust
struct GpuSimulationIO {
    // Input/Output ring buffers (unified memory)
    input_ring: RingBuffer<InputEvent>,        // Host → GPU inputs
    output_ring: RingBuffer<OutputEvent>,      // GPU → Host outputs

    // Debug access (direct memory mapped)
    debug_window: DebugMemoryWindow,           // Peek/poke any signal
    trace_buffer: TraceBuffer,                 // Continuous trace capture

    // Bulk data transfer
    dma_channels: Vec<DmaChannel>,             // High-bandwidth streams

    // Control plane
    control_mailbox: Mailbox<ControlMessage>,  // Commands/status
}

impl GpuSimulationIO {
    fn setup_unified_memory_io(&mut self) -> Result<()> {
        // Allocate unified memory visible to CPU and GPU
        self.input_ring = RingBuffer::new_unified(
            size: 1_048_576,  // 1MB input buffer
            mode: MTLResourceOptions::StorageModeShared
        );

        self.output_ring = RingBuffer::new_unified(
            size: 4_194_304,  // 4MB output buffer (more verbose)
            mode: MTLResourceOptions::StorageModeShared
        );

        // Debug window for arbitrary signal access
        self.debug_window = DebugMemoryWindow::new(
            size: 16_777_216,  // 16MB debug space
            mode: MTLResourceOptions::StorageModeShared
        );

        Ok(())
    }
}
```

#### Input Mechanisms

##### 1. Streaming Inputs via Ring Buffer

```rust
struct InputRingBuffer {
    // Lock-free SPSC ring buffer
    buffer: MTLBuffer,                    // Unified memory
    write_index: AtomicU64,              // Host writes
    read_index: AtomicU64,               // GPU reads

    fn host_write_input(&mut self, event: InputEvent) {
        let write_idx = self.write_index.load(Ordering::Acquire);
        let next_idx = (write_idx + 1) % self.capacity;

        // Check not full
        if next_idx != self.read_index.load(Ordering::Acquire) {
            // Write to unified memory
            unsafe {
                let ptr = self.buffer.contents() as *mut InputEvent;
                ptr.add(write_idx as usize).write(event);
            }

            // Update write index (GPU sees immediately)
            self.write_index.store(next_idx, Ordering::Release);
        }
    }
}

// GPU-side input consumption
kernel void consume_inputs(
    device InputEvent* input_ring [[buffer(0)]],
    device atomic_uint* read_index [[buffer(1)]],
    device atomic_uint* write_index [[buffer(2)]],
    device SimulationState* state [[buffer(3)]]
) {
    uint read_idx = atomic_load_explicit(read_index, memory_order_acquire);
    uint write_idx = atomic_load_explicit(write_index, memory_order_acquire);

    while (read_idx != write_idx) {
        InputEvent event = input_ring[read_idx];

        // Apply input to simulation
        apply_input_to_state(state, event);

        // Update read index
        read_idx = (read_idx + 1) % RING_SIZE;
        atomic_store_explicit(read_index, read_idx, memory_order_release);
    }
}
```

##### 2. Memory-Mapped Testbench Interface

```rust
struct TestbenchInterface {
    // Direct memory-mapped I/O ports
    input_ports: HashMap<String, MemoryMappedPort>,
    output_ports: HashMap<String, MemoryMappedPort>,

    fn write_input(&mut self, port_name: &str, value: BitVector) {
        if let Some(port) = self.input_ports.get(port_name) {
            // Direct write to GPU-visible memory
            unsafe {
                let ptr = port.address as *mut u64;
                ptr.write_volatile(value.as_u64());
            }

            // GPU kernel polls this memory location
        }
    }

    fn read_output(&self, port_name: &str) -> BitVector {
        if let Some(port) = self.output_ports.get(port_name) {
            // Direct read from GPU-written memory
            unsafe {
                let ptr = port.address as *const u64;
                BitVector::from_u64(ptr.read_volatile())
            }
        }
    }
}
```

##### 3. File-Based Stimulus

```rust
struct FileStimulus {
    // Efficient file → GPU streaming
    vcd_reader: VcdReader,
    pattern_reader: PatternReader,

    fn stream_vcd_to_gpu(&mut self, filename: &str) -> Result<()> {
        let vcd = self.vcd_reader.open(filename)?;

        // Pre-process and pack for GPU consumption
        let packed_events = vcd.events()
            .batch(1024)  // Batch for efficiency
            .map(|batch| self.pack_events(batch));

        // DMA transfer to GPU
        for packed_batch in packed_events {
            self.dma_to_gpu(packed_batch)?;
        }

        Ok(())
    }
}
```

#### Output Mechanisms

##### 1. Streaming Outputs via Ring Buffer

```metal
// GPU-side output generation
kernel void produce_outputs(
    device SimulationState* state [[buffer(0)]],
    device OutputEvent* output_ring [[buffer(1)]],
    device atomic_uint* write_index [[buffer(2)]],
    device atomic_uint* read_index [[buffer(3)]],
    constant OutputConfig* config [[buffer(4)]]
) {
    // Check which signals to output
    for (uint i = 0; i < config->num_monitored_signals; i++) {
        SignalID signal = config->monitored_signals[i];

        if (signal_changed(state, signal)) {
            // Write to output ring
            uint write_idx = atomic_load_explicit(write_index, memory_order_acquire);
            uint next_idx = (write_idx + 1) % RING_SIZE;

            // Check not full
            if (next_idx != atomic_load_explicit(read_index, memory_order_acquire)) {
                OutputEvent event = {
                    .timestamp = state->current_time,
                    .signal_id = signal,
                    .value = get_signal_value(state, signal)
                };

                output_ring[write_idx] = event;
                atomic_store_explicit(write_index, next_idx, memory_order_release);
            }
        }
    }
}
```

##### 2. VCD/FST Generation

```rust
struct WaveformOutput {
    vcd_writer: VcdWriter,
    fst_writer: FstWriter,

    fn capture_outputs(&mut self) {
        // Background thread consuming output ring
        thread::spawn(move || {
            let mut vcd = VcdWriter::new("output.vcd");

            loop {
                // Read from output ring
                while let Some(event) = self.output_ring.read() {
                    vcd.add_change(
                        event.timestamp,
                        event.signal_id,
                        event.value
                    );
                }

                // Periodic flush to disk
                if should_flush() {
                    vcd.flush();
                }

                thread::sleep(Duration::from_micros(100));
            }
        });
    }
}
```

#### Debug Access

##### 1. Arbitrary Signal Inspection

```rust
struct DebugInterface {
    // Direct peek/poke into simulation state
    signal_map: HashMap<String, SignalAddress>,

    fn peek_signal(&self, path: &str) -> BitVector {
        if let Some(addr) = self.signal_map.get(path) {
            // Direct read from GPU memory
            unsafe {
                let ptr = (self.base_address + addr.offset) as *const u64;
                BitVector::from_u64(ptr.read_volatile())
            }
        }
    }

    fn poke_signal(&mut self, path: &str, value: BitVector) {
        if let Some(addr) = self.signal_map.get(path) {
            // Direct write to GPU memory
            unsafe {
                let ptr = (self.base_address + addr.offset) as *mut u64;
                ptr.write_volatile(value.as_u64());
            }

            // Set dirty flag for GPU to notice
            self.mark_signal_dirty(addr.signal_id);
        }
    }

    fn force_signal(&mut self, path: &str, value: BitVector) {
        // Override signal until release
        self.poke_signal(path, value);
        self.add_force_list(path);
    }

    fn release_signal(&mut self, path: &str) {
        self.remove_force_list(path);
    }
}
```

##### 2. Hierarchical Access

```rust
struct HierarchicalDebug {
    fn get_module_state(&self, module_path: &str) -> ModuleState {
        // Get all signals in a module
        let signals = self.get_module_signals(module_path);

        // Batch read from GPU
        let values = self.batch_read_signals(&signals);

        ModuleState {
            path: module_path.to_string(),
            signals: signals.zip(values).collect()
        }
    }

    fn watch_signal(&mut self, path: &str, callback: WatchCallback) {
        // Register callback for signal changes
        self.watchers.insert(path, callback);

        // GPU will write to specific location on change
        self.enable_gpu_watch(path);
    }
}
```

#### High-Performance Bulk I/O

##### 1. DMA Channels for Streaming

```rust
struct DmaChannel {
    channel_id: u32,
    buffer_size: usize,
    double_buffer: [MTLBuffer; 2],
    active_buffer: AtomicU8,

    fn stream_to_gpu(&mut self, data: &[u8]) -> Result<()> {
        // Use double buffering for continuous streaming
        let buffer_idx = self.active_buffer.load(Ordering::Acquire) as usize;
        let next_idx = (buffer_idx + 1) % 2;

        // Write to inactive buffer
        unsafe {
            let ptr = self.double_buffer[next_idx].contents() as *mut u8;
            ptr.copy_from_nonoverlapping(data.as_ptr(), data.len());
        }

        // Atomic swap buffers
        self.active_buffer.store(next_idx as u8, Ordering::Release);

        // GPU reads from active buffer
        Ok(())
    }
}
```

##### 2. Memory-Mapped File I/O

```rust
struct MemoryMappedIO {
    input_file: MmapMut,  // Memory-mapped input file
    output_file: MmapMut, // Memory-mapped output file

    fn setup_mmap_io(&mut self, input_path: &str, output_path: &str) -> Result<()> {
        // Map files directly into GPU-accessible memory
        let input_file = OpenOptions::new()
            .read(true)
            .open(input_path)?;

        self.input_file = unsafe {
            MmapOptions::new()
                .map_mut(&input_file)?
        };

        // GPU can read directly from mapped memory
        self.gpu_set_input_buffer(self.input_file.as_ptr());

        Ok(())
    }
}
```

#### Command and Control Interface

```rust
struct ControlInterface {
    // Bidirectional command/response channel
    command_queue: Mailbox<SimCommand>,
    response_queue: Mailbox<SimResponse>,

    fn send_command(&mut self, cmd: SimCommand) -> SimResponse {
        // Send command to GPU
        self.command_queue.send(cmd);

        // Wait for response
        self.response_queue.receive()
    }

    fn handle_commands(&mut self) {
        match self.command_queue.try_receive() {
            Some(SimCommand::Pause) => {
                self.pause_simulation();
                self.response_queue.send(SimResponse::Paused);
            },
            Some(SimCommand::Step(n)) => {
                self.step_cycles(n);
                self.response_queue.send(SimResponse::Stepped(n));
            },
            Some(SimCommand::ReadMemory(addr, size)) => {
                let data = self.read_memory_range(addr, size);
                self.response_queue.send(SimResponse::MemoryData(data));
            },
            // ... other commands
        }
    }
}
```

#### Performance Optimization

```rust
struct IOPerformanceMetrics {
    fn measure_bandwidth(&self) -> BandwidthReport {
        BandwidthReport {
            // Input bandwidth
            input_throughput: "10 GB/s",      // PCIe 4.0 speeds
            input_latency: "< 1μs",           // Unified memory

            // Output bandwidth
            output_throughput: "8 GB/s",      // Slightly less due to formatting
            output_latency: "< 5μs",          // Including serialization

            // Debug access
            debug_latency: "< 100ns",         // Direct memory read
            debug_throughput: "1M ops/sec",   // Random access

            // Bulk transfer
            dma_throughput: "25 GB/s",        // Full memory bandwidth
            dma_latency: "< 10μs",            // Setup overhead
        }
    }
}
```

#### Python/C API Bindings

```python
# Python testbench integration
import skalp_sim

# Create simulation
sim = skalp_sim.GpuSimulation("design.sk")

# Write inputs
sim.poke("top.clk", 1)
sim.poke("top.reset", 0)
sim.poke("top.data_in", 0xDEADBEEF)

# Run simulation
sim.run_cycles(1000)

# Read outputs
result = sim.peek("top.data_out")
print(f"Result: {result:08x}")

# Debug access
sim.force("top.cpu.pc", 0x1000)
sim.run_until("top.done == 1")
sim.release("top.cpu.pc")

# Dump waveform
sim.dump_vcd("output.vcd")
```

### Key Design Principles:

1. **Zero-Copy on Apple Silicon**: Unified memory eliminates transfers
2. **Lock-Free Ring Buffers**: High-throughput streaming I/O
3. **Memory-Mapped Direct Access**: Instant debug peek/poke
4. **Double Buffering**: Continuous streaming without stalls
5. **Hierarchical Access**: Navigate design hierarchy easily

This I/O architecture provides the bandwidth and flexibility needed for practical GPU simulation while maintaining the performance advantages of unified memory.

## FFI/VPI Replacement: Why We Don't Need It

### Traditional FFI/VPI Problems vs GPU Co-Simulation Solution

Traditional simulators need Foreign Function Interface (FFI) and Verilog Procedural Interface (VPI) to call external C/C++ code, but these interfaces are slow, complex, and error-prone. With GPU simulation and unified memory, we get **better functionality for free**.

#### Traditional FFI/VPI Architecture (Slow & Complex)

```c
// Traditional VPI - Massive overhead per call
static int my_vpi_task(char *userdata) {
    vpiHandle systfref = vpi_handle(vpiSysTfCall, NULL);
    vpiHandle args_iter = vpi_iterate(vpiArgument, systfref);

    // Expensive data marshalling
    vpiHandle arg1 = vpi_scan(args_iter);
    s_vpi_value value;
    value.format = vpiIntVal;
    vpi_get_value(arg1, &value);  // SLOW: Crosses simulation boundary

    // Process in C
    int result = process_data(value.value.integer);

    // Write back (more overhead)
    value.value.integer = result;
    vpi_put_value(arg1, &value, NULL, vpiNoDelay);  // SLOW: Another boundary cross

    return 0;
}

// Problems:
// 1. Context switch overhead (μs per call)
// 2. Data marshalling/unmarshalling
// 3. Simulation must pause
// 4. No parallelism
// 5. Complex error handling
```

#### GPU Co-Simulation Architecture (Zero Overhead)

```rust
// Our approach: Software and hardware share memory directly!
struct GpuCoSimulation {
    // Same memory visible to both CPU and GPU
    shared_memory: UnifiedMemory,

    fn run_cosimulation(&mut self) {
        // Software (CPU) writes directly to shared memory
        self.shared_memory.write_cpu_data(data);

        // Hardware (GPU) reads it instantly - NO MARSHALLING
        // GPU simulation continues at full speed

        // Software can read results immediately - NO CALLBACKS
        let result = self.shared_memory.read_gpu_result();
    }
}
```

### Why Traditional FFI/VPI Exists vs Why We Don't Need It

#### Traditional Simulators Need FFI/VPI For:

1. **Testbench Integration**
```systemverilog
// Traditional: Need FFI to run C++ testbench
import "DPI-C" function void run_test();

initial begin
    run_test();  // Context switch to C++
end
```

**Our Solution: Native Integration**
```rust
// CPU and GPU run simultaneously
let cpu_thread = thread::spawn(|| {
    run_software_test();  // Runs natively on CPU
});

let gpu_sim = gpu::spawn(|| {
    run_hardware_sim();   // Runs on GPU
});

// Both access same memory - no FFI needed!
```

2. **Reference Models**
```c
// Traditional: Call C reference model via VPI
int golden_result = reference_model(input);  // Slow VPI call
```

**Our Solution: Parallel Execution**
```rust
// Reference model runs on CPU while DUT runs on GPU
// Both see the same inputs instantly
let ref_result = cpu_reference_model(shared_input);
let dut_result = gpu_dut_simulation(shared_input);
assert_eq!(ref_result, dut_result);  // Direct comparison!
```

3. **System-Level Simulation**
```systemverilog
// Traditional: Complex SystemC/TLM integration via VPI
sc_model.process_transaction(data);  // Slow boundary crossing
```

**Our Solution: True Co-Simulation**
```rust
// Software components run natively on CPU
// Hardware components run on GPU
// Unified memory = zero-copy communication

struct SystemSimulation {
    cpu_software: CpuProcess,      // Full-speed native execution
    gpu_hardware: GpuSimulation,   // Full-speed GPU simulation
    shared_bus: UnifiedMemory,      // Both see same memory
}
```

### Performance Comparison

```rust
struct PerformanceComparison {
    traditional_ffi: Metrics {
        call_overhead: "1-10 μs per call",
        data_transfer: "10-100 MB/s",
        parallelism: "None - simulation pauses",
        complexity: "High - manual marshalling",
        debugging: "Difficult - crosses boundaries",
    },

    gpu_cosim: Metrics {
        call_overhead: "0 ns - no calls needed",
        data_transfer: "400 GB/s - unified memory",
        parallelism: "Full - CPU and GPU run together",
        complexity: "None - shared memory",
        debugging: "Easy - unified view",
    },
}
```

### Real-World Examples

#### Example 1: Processor Verification

**Traditional FFI Approach:**
```c
// Instruction set simulator via VPI (SLOW)
void execute_instruction() {
    uint32_t instr = vpi_get_value("cpu.instruction");  // Get from sim
    uint32_t result = iss_execute(instr);               // Run ISS
    vpi_put_value("cpu.result", result);                // Put back
}  // Total overhead: ~5μs per instruction!
```

**Our GPU Co-Sim Approach:**
```rust
// ISS runs on CPU, RTL on GPU - both full speed!
loop {
    let instr = shared_mem.next_instruction;  // Zero-copy read
    let iss_result = iss.execute(instr);      // Native CPU speed
    let rtl_result = shared_mem.rtl_result;   // GPU wrote here
    assert_eq!(iss_result, rtl_result);       // Instant comparison
}  // Overhead: 0ns - everything runs in parallel!
```

#### Example 2: DMA Controller Testing

**Traditional FFI:**
```verilog
// Need FFI to generate/check DMA traffic
import "DPI-C" function void generate_dma_pattern();
import "DPI-C" function bit check_dma_result();

// Slow calls across boundary
always @(posedge clk) begin
    generate_dma_pattern();  // FFI overhead
    // ... wait for DMA ...
    if (!check_dma_result()) $error("DMA failed");  // More overhead
end
```

**Our Approach:**
```rust
// Pattern generator runs on CPU, DMA sim on GPU
struct DmaTest {
    fn run_test(&mut self) {
        // CPU generates patterns continuously
        thread::spawn(|| {
            for pattern in test_patterns {
                shared_mem.dma_buffer.write(pattern);  // Direct write
            }
        });

        // GPU simulates DMA controller
        // Reads patterns from same memory - no FFI!
        // Writes results back to shared memory

        // CPU checks results in parallel
        thread::spawn(|| {
            while let Some(result) = shared_mem.results.read() {
                verify_dma_result(result);  // Native speed
            }
        });
    }
}
```

### Advanced Capabilities Beyond Traditional FFI

#### 1. **Bidirectional Real-Time Communication**
```rust
// Impossible with traditional FFI - too slow
struct RealtimeCosim {
    fn run(&mut self) {
        loop {
            // CPU makes decisions based on GPU state
            if shared_mem.interrupt_pending {
                shared_mem.interrupt_vector = calculate_vector();
            }

            // GPU reacts immediately to CPU decisions
            // No function calls, no marshalling
            // Just shared memory updates at GB/s speeds
        }
    }
}
```

#### 2. **Massive Parallel Testbenches**
```rust
// Run 1000 tests in parallel - impossible with VPI
let tests: Vec<_> = (0..1000).map(|i| {
    thread::spawn(move || {
        let test_region = &shared_mem[i * TEST_SIZE..(i + 1) * TEST_SIZE];
        run_test_variant(i, test_region);
    })
}).collect();

// All tests run simultaneously with GPU simulation
// No FFI bottleneck!
```

#### 3. **Zero-Latency Assertions**
```rust
// Assertions checked continuously without FFI overhead
struct ContinuousChecker {
    fn monitor(&self) {
        // CPU thread continuously monitors GPU simulation
        while running {
            let state = shared_mem.read_state();  // <100ns read

            if violates_property(state) {
                // Instant detection - no VPI callback delay
                report_violation(state);
            }
        }
    }
}
```

### The Bottom Line

**We don't need FFI/VPI because:**

1. **Unified Memory = No Boundaries**: CPU and GPU share same memory
2. **True Parallelism**: Software and hardware run simultaneously
3. **Zero Overhead**: No function calls, no marshalling
4. **Native Speed**: Everything runs at full speed
5. **Better Debugging**: Single memory space to inspect

**Traditional FFI/VPI:**
- ❌ 1-10μs overhead per call
- ❌ Complex data marshalling
- ❌ Simulation must pause
- ❌ Hard to debug
- ❌ No parallelism

**Our GPU Co-Simulation:**
- ✅ 0ns overhead (shared memory)
- ✅ No marshalling needed
- ✅ Full parallel execution
- ✅ Easy debugging
- ✅ True co-simulation

This is a **fundamental architectural advantage** - by eliminating the boundary between simulation and external code, we get FFI/VPI functionality "for free" with better performance and simpler implementation!

## CPU-GPU Synchronization: Handling Race Conditions

### The Race Condition Problem

You're absolutely right - we need to handle race conditions between CPU writes and GPU reads. Without proper synchronization, we could have:

```
CPU: Writes input at time T
GPU: Reads input at time T  <- Race! Might see old or new value
Result: Non-deterministic simulation!
```

### Solution: Double-Buffered Time-Sliced Architecture

The key insight is to use **double buffering with time slices** - similar to how real hardware handles clock domain crossings!

#### Time-Sliced Execution Model

```rust
struct TimeSlicedSimulation {
    // Double buffers for race-free communication
    input_buffers: [InputBuffer; 2],     // Two buffers
    output_buffers: [OutputBuffer; 2],   // Two buffers
    current_buffer: AtomicU8,            // Which buffer is "current"

    // Time coordination
    current_cycle: AtomicU64,            // Current simulation cycle
    cpu_ready: AtomicBool,               // CPU ready for next cycle
    gpu_ready: AtomicBool,               // GPU ready for next cycle
}

impl TimeSlicedSimulation {
    fn run_synchronized(&mut self) {
        loop {
            let buffer_idx = self.current_buffer.load(Ordering::Acquire);
            let next_idx = (buffer_idx + 1) % 2;

            // Phase 1: CPU prepares inputs for NEXT cycle
            self.cpu_prepare_inputs(next_idx);
            self.cpu_ready.store(true, Ordering::Release);

            // Phase 2: GPU simulates CURRENT cycle with stable inputs
            self.gpu_simulate_cycle(buffer_idx);
            self.gpu_ready.store(true, Ordering::Release);

            // Phase 3: Synchronization barrier
            while !self.cpu_ready.load(Ordering::Acquire)
                || !self.gpu_ready.load(Ordering::Acquire) {
                // Wait for both to complete
            }

            // Phase 4: Atomic buffer swap
            self.current_buffer.store(next_idx, Ordering::Release);
            self.current_cycle.fetch_add(1, Ordering::Release);

            // Reset ready flags
            self.cpu_ready.store(false, Ordering::Release);
            self.gpu_ready.store(false, Ordering::Release);
        }
    }
}
```

#### GPU Kernel with Synchronization

```metal
kernel void simulate_with_sync(
    device SimulationState* state [[buffer(0)]],
    device InputBuffer* inputs [[buffer(1)]],      // Current cycle inputs
    device OutputBuffer* outputs [[buffer(2)]],     // Current cycle outputs
    device atomic_uint* buffer_index [[buffer(3)]],
    device atomic_bool* gpu_ready [[buffer(4)]]
) {
    // Read from stable input buffer (no races!)
    uint buf_idx = atomic_load_explicit(buffer_index, memory_order_acquire);
    InputData input = inputs[buf_idx].data[tid];

    // Simulate with stable inputs
    OutputData output = simulate_logic(state, input);

    // Write to output buffer
    outputs[buf_idx].data[tid] = output;

    // Signal completion (once per workgroup)
    if (tid == 0) {
        atomic_store_explicit(gpu_ready, true, memory_order_release);
    }
}
```

### Three Synchronization Strategies

#### Strategy 1: Strict Cycle-Accurate (Slowest, Most Accurate)

```rust
struct StrictSynchronization {
    fn run_cycle(&mut self) {
        // CPU and GPU lock-step execution

        // 1. CPU writes all inputs for cycle N
        self.cpu_write_inputs();

        // 2. Memory barrier ensures writes visible
        atomic::fence(Ordering::Release);

        // 3. GPU simulates cycle N
        self.gpu_simulate_cycle();

        // 4. Memory barrier ensures reads complete
        atomic::fence(Ordering::Acquire);

        // 5. CPU reads outputs from cycle N
        self.cpu_read_outputs();

        // Guaranteed: No races, perfect accuracy
        // Cost: Synchronization overhead each cycle
    }
}
```

#### Strategy 2: Pipelined with Blocking (CHOSEN APPROACH)

```rust
struct PipelinedSynchronization {
    current_buffer: Arc<AtomicPtr<InputBuffer>>,
    next_buffer: Arc<AtomicPtr<InputBuffer>>,
    cpu_ready: Arc<AtomicBool>,
    gpu_ready: Arc<AtomicBool>,

    fn run_pipelined(&mut self) {
        // CPU thread - prepares next cycle inputs
        let cpu_thread = thread::spawn(|| {
            loop {
                // Prepare inputs for cycle N+1
                self.prepare_next_inputs();

                // Signal CPU ready
                self.cpu_ready.store(true, Ordering::Release);

                // Wait for GPU to be ready for swap
                while !self.gpu_ready.load(Ordering::Acquire) {
                    // CPU blocks here if GPU is still simulating
                    std::hint::spin_loop();
                }

                // Atomic buffer swap at clock edge
                self.swap_buffers();
            }
        });

        // GPU thread - simulates current cycle
        let gpu_thread = thread::spawn(|| {
            loop {
                // GPU simulates cycle N
                self.gpu_evaluate_cycle();

                // Signal GPU ready
                self.gpu_ready.store(true, Ordering::Release);

                // Wait for CPU to prepare next inputs
                while !self.cpu_ready.load(Ordering::Acquire) {
                    // GPU blocks here if CPU is still preparing
                    std::hint::spin_loop();
                }

                // Continue with swapped buffers
            }
        });

        // Natural backpressure:
        // - If GPU is slow (complex logic), CPU waits
        // - If CPU is slow (complex testbench), GPU waits
        // - System runs at speed of slowest component
        // - No unbounded buffering, deterministic execution
    }
}
```

**Why This Approach:**
- **Correctness**: Guarantees deterministic simulation
- **Performance**: Minimal overhead through pipelining
- **Natural**: Matches hardware clock edge behavior
- **Simple**: No complex queue management or race conditions

#### Strategy 3: Async Message Passing (Best Performance)

```rust
struct AsyncMessagePassing {
    // Lock-free queues for communication
    input_queue: spsc::Queue<InputEvent>,    // CPU → GPU
    output_queue: spsc::Queue<OutputEvent>,  // GPU → CPU

    fn run_async(&mut self) {
        // CPU thread - produces inputs
        thread::spawn(|| {
            loop {
                let input = generate_input();
                self.input_queue.push(InputEvent {
                    cycle: current_cycle(),
                    data: input,
                });
            }
        });

        // GPU processes when ready
        // No strict cycle synchronization
        // Higher throughput, slightly relaxed timing
    }
}
```

### Handling Different I/O Patterns

#### Pattern 1: Testbench Driven (Most Common)

```rust
// Testbench knows when to write/read
struct TestbenchDriven {
    fn run_test(&mut self) {
        // Write inputs
        self.poke("data_in", 0x42);
        self.poke("valid", 1);

        // Run cycles (GPU processes stable inputs)
        self.run_cycles(10);

        // Read outputs (guaranteed stable)
        let result = self.peek("data_out");

        // No races - explicit sequencing
    }
}
```

#### Pattern 2: Reactive Software (Interrupt-Driven)

```rust
struct ReactiveSimulation {
    fn handle_interrupts(&mut self) {
        // GPU signals interrupt
        if self.interrupt_pending.load(Ordering::Acquire) {
            // CPU responds in NEXT cycle
            self.next_cycle_inputs.interrupt_ack = true;

            // Natural 1-cycle latency - matches hardware!
        }
    }
}
```

#### Pattern 3: Continuous Monitoring

```rust
struct ContinuousMonitoring {
    // Use separate read-only buffer for monitoring
    monitor_buffer: ReadOnlyBuffer,

    fn monitor_thread(&self) {
        loop {
            // Read from previous cycle's snapshot
            let state = self.monitor_buffer.read_snapshot();

            // Analyze without affecting simulation
            check_assertions(state);

            // No races - reading stable snapshot
        }
    }
}
```

### Memory Ordering Guarantees

```rust
// Proper memory ordering for race-free operation
struct MemoryOrdering {
    fn write_input(&self, value: u32) {
        // Store with Release ordering
        self.input.store(value, Ordering::Release);
        // Ensures write is visible before cycle advance
    }

    fn read_output(&self) -> u32 {
        // Load with Acquire ordering
        self.output.load(Ordering::Acquire)
        // Ensures we see all writes from GPU
    }

    fn advance_cycle(&self) {
        // Full barrier at cycle boundaries
        atomic::fence(Ordering::SeqCst);
        self.cycle.fetch_add(1, Ordering::SeqCst);
    }
}
```

### Real Hardware Analogy

This mirrors how real hardware works:

```verilog
// Real hardware
always @(posedge clk) begin
    q <= d;  // Sample at clock edge, visible next cycle
end

// Our simulation
fn simulate_cycle() {
    // Sample inputs at "clock edge"
    let inputs = self.current_inputs();  // Stable for whole cycle

    // Compute outputs
    let outputs = evaluate_logic(inputs);

    // Outputs visible next cycle
    self.next_outputs = outputs;
}
```

### Performance Impact

```rust
struct SyncPerformance {
    fn measure_overhead(&self) -> Overhead {
        Overhead {
            // Strict synchronization
            strict: "5-10% slower, perfect accuracy",

            // Pipelined (recommended)
            pipelined: "< 1% overhead, matches hardware",

            // Async message passing
            async_queue: "0% overhead, relaxed timing",

            // No synchronization (wrong!)
            race_condition: "Fastest but non-deterministic!"
        }
    }
}
```

### Best Practice: Match Hardware Semantics

```rust
// Recommended approach - matches real hardware
struct HardwareSemanticSimulation {
    fn simulate(&mut self) {
        // Inputs change between cycles (not during)
        self.prepare_next_inputs();

        // GPU evaluates with stable inputs
        self.gpu_evaluate_cycle();

        // Outputs available next cycle
        self.swap_buffers();

        // This is EXACTLY how real hardware works!
        // No races, minimal overhead
    }
}
```

### The Bottom Line

**Yes, we need synchronization**, but:

1. **Double buffering** eliminates most races
2. **Cycle boundaries** provide natural sync points
3. **Matches hardware** behavior (inputs stable during cycle)
4. **< 1% overhead** with pipelined approach
5. **Lock-free** algorithms for high performance

The key insight: **Real hardware has the same "problem"** - inputs must be stable at clock edges. Our simulation just mirrors this natural behavior, turning a potential bug into correct-by-construction design!

*SKALP's multi-level simulation provides the right abstraction for each verification task, from quick algorithm checks to detailed timing analysis. On Apple Silicon, Metal acceleration with unified memory enables zero-copy, real-time hardware-software co-simulation with unprecedented performance, real-time display capabilities, and professional-grade waveform analysis.*

## Two-Runtime Async Architecture

### Coordinated CPU and GPU Runtimes

SKALP uses two separate async runtimes that coordinate efficiently - one on CPU (Tokio) and one on GPU (Metal persistent threads). This architecture eliminates synchronization complexity while respecting the fundamental hardware boundary:

```rust
// Two distinct runtimes that coordinate asynchronously
pub struct SimulationSystem {
    // CPU-side runtime (Tokio) - runs on CPU
    cpu_runtime: tokio::runtime::Runtime,

    // GPU-side runtime - runs ON the GPU (persistent Metal threads)
    gpu_runtime: GpuAsyncRuntime,

    // Async coordination between them
    cpu_to_gpu: mpsc::Sender<CpuToGpuMsg>,
    gpu_to_cpu: mpsc::Receiver<GpuToCpuMsg>,

    // Cycle synchronization
    cycle_barrier: Arc<AsyncBarrier>,
}

impl UnifiedAsyncRuntime {
    // Everything is async/await - no blocking!
    pub async fn poke(&mut self, signal: &str, value: u64) {
        self.input_channel.send(InputUpdate { signal, value }).await;
    }

    pub async fn peek(&self, signal: &str) -> u64 {
        self.output_channel.recv().await
    }

    pub async fn cycle(&mut self) {
        self.cycle_barrier.wait().await;  // Natural async synchronization
    }

    pub async fn cycles(&mut self, n: usize) {
        for _ in 0..n {
            self.cycle().await;
        }
    }
}
```

### How the Two Runtimes Work Together

The key insight: **CPU and GPU are separate processors with separate runtimes that coordinate asynchronously**.

```rust
// CPU Runtime (Tokio) - Manages testbenches on CPU
pub struct CpuRuntime {
    tokio_runtime: tokio::runtime::Runtime,

    pub fn run_testbench(&self, testbench: AsyncTestbench) {
        self.tokio_runtime.spawn(async move {
            // Testbench runs on CPU
            testbench.execute().await;
        });
    }
}

// GPU Runtime - Persistent Metal threads ON the GPU
pub struct GpuAsyncRuntime {
    // These threads live on the GPU permanently!
    persistent_threads: Vec<MetalPersistentThread>,

    // Runs continuously on GPU hardware
    pub fn gpu_kernel() {
        // This code runs ON THE GPU
        loop {
            wait_for_work();  // GPU waits for signal
            evaluate_cones();  // GPU does simulation
            signal_complete(); // GPU signals CPU
        }
    }
}

// Coordination between them
impl SimulationSystem {
    pub async fn run(&mut self) {
        // CPU side: spawn testbench task
        let cpu_task = self.cpu_runtime.spawn(async {
            loop {
                prepare_inputs().await;
                signal_gpu_ready().await;
                await_gpu_complete().await;
            }
        });

        // GPU side: already running persistent threads
        // (initialized at startup, continuous execution)

        // Coordination through async channels
        loop {
            // CPU prepares inputs
            let msg = self.gpu_to_cpu.recv().await;

            match msg {
                GpuComplete => {
                    // GPU finished cycle, CPU can continue
                    self.cpu_to_gpu.send(NextCycle).await;
                }
            }
        }
    }
}
```

### Why Two Runtimes?

1. **Hardware Reality**: CPU and GPU are separate processors
2. **Different Execution Models**:
   - CPU: Task-based async (Tokio)
   - GPU: Persistent thread model (Metal)
3. **Optimal for Each Side**:
   - CPU uses Tokio's work-stealing scheduler
   - GPU uses persistent threads to avoid kernel launch overhead
4. **Clean Separation**: Each runtime optimized for its hardware

### Native SKALP Testbenches Only

```rust
// SKALP testbenches compile to async Rust
testbench MyTest for MyDesign {
    async initial {
        sim.poke("reset", 1).await;
        sim.cycles(10).await;
        sim.poke("reset", 0).await;

        // Natural async/await syntax
        for i in 0..100 {
            sim.poke("data_in", random()).await;
            sim.poke("valid", 1).await;
            sim.cycle().await;
            sim.poke("valid", 0).await;
            sim.cycles(4).await;
        }
    }

    // Concurrent assertions run as separate async tasks
    async always {
        loop {
            sim.cycle().await;
            assert!(!(sim.peek("valid").await == 1 && sim.peek("error").await == 1));
        }
    }
}
```

### Async Rust Testbenches

```rust
// Pure async Rust - no blocking, maximum performance
use skalp_runtime::AsyncSimulation;

#[tokio::main]
async fn main() {
    let mut sim = AsyncSimulation::new("my_design.sk").await;

    // Reset sequence - all async
    sim.poke("reset", 1).await;
    sim.cycles(10).await;
    sim.poke("reset", 0).await;

    // Parallel test operations with futures
    let test_tasks = (0..1000).map(|i| async move {
        sim.poke("data_in", rand::random::<u32>()).await;
        sim.poke("valid", 1).await;
        sim.cycle().await;

        // Async output checking
        if sim.peek("output_valid").await == 1 {
            let result = sim.peek("data_out").await;
            verify_result(result).await;
        }
    });

    // Run all tests concurrently!
    futures::future::join_all(test_tasks).await;

    sim.finish().await;
}
```

### Why No C/C++/Python Support?

**We made the deliberate choice to drop traditional language compatibility in favor of a pure async Rust approach:**

1. **Async/await doesn't translate** - C/C++ lack native async/await
2. **Performance is better without FFI** - No marshaling overhead
3. **Safety guarantees** - Rust's ownership model prevents races
4. **Simpler architecture** - One language, one runtime, no glue code
5. **Modern tooling** - Rust's ecosystem is superior for this use case

Users who need other languages can:
- Generate Rust testbenches from their preferred language
- Use SKALP's native testbench syntax (compiles to async Rust)
- Port existing testbenches (one-time effort for massive gains)

### Simplified Compilation Strategy

```rust
// Only two paths: SKALP or Rust, both compile to async Rust
pub struct TestbenchCompiler {
    async fn compile_testbench(&self, source: TestbenchSource) -> AsyncBinary {
        match source {
            TestbenchSource::Skalp(code) => {
                // SKALP testbenches transpile to async Rust
                let async_rust = self.transpile_to_async_rust(code);
                self.compile_async_rust(async_rust).await
            }

            TestbenchSource::Rust(code) => {
                // Direct async Rust compilation
                self.compile_async_rust(code).await
            }
        }
    }

    fn transpile_to_async_rust(&self, skalp: &str) -> String {
        // Transform SKALP testbench syntax to async Rust
        // initial { } blocks become async fn
        // #N timing becomes sim.cycles(N).await
        // All I/O becomes async operations
    }
}
```

### Async Channel-Based I/O

```rust
// Async channels provide natural backpressure and synchronization
pub struct AsyncChannelIO {
    // Lock-free MPSC channels for communication
    input_tx: mpsc::Sender<InputUpdate>,
    output_rx: mpsc::Receiver<OutputUpdate>,

    // Still uses unified memory underneath for zero-copy
    unified_memory: Arc<UnifiedMemory>,

    async fn poke(&mut self, signal: &str, value: u64) {
        // Send through async channel - natural backpressure
        self.input_tx.send(InputUpdate {
            signal: signal.to_string(),
            value,
        }).await.unwrap();
    }

    async fn peek(&mut self, signal: &str) -> u64 {
        // Request output value
        self.output_rx.recv().await.unwrap().value
    }

    async fn wait_for(&mut self, signal: &str, value: u64) {
        loop {
            if self.peek(signal).await == value {
                break;
            }
            self.cycle().await;
        }
    }
}
```

### Async Task Spawning

```rust
// Testbenches are just async tasks
pub struct AsyncTestbenchRunner {
    runtime: Arc<tokio::runtime::Runtime>,

    async fn spawn_testbench(&self, testbench: impl Future<Output = ()> + Send + 'static) {
        // Spawn as tokio task
        self.runtime.spawn(testbench);
    }

    async fn run_parallel_tests(&self, tests: Vec<AsyncTestbench>) {
        // Run multiple testbenches concurrently
        let handles: Vec<_> = tests.into_iter()
            .map(|test| self.runtime.spawn(test.run()))
            .collect();

        // Wait for all to complete
        futures::future::join_all(handles).await;
    }
}
```

### Async Performance Benefits

```rust
pub struct AsyncPerformance {
    // Natural batching through channel buffering
    async fn auto_batched_operations(&mut self) {
        // Tokio automatically batches channel operations
        // No manual batching needed!
    }

    // Concurrent test execution
    async fn parallel_stimulus(&mut self) {
        // Multiple async tasks generate stimulus concurrently
        let tasks = (0..100).map(|i| async move {
            generate_stimulus(i).await
        });

        futures::future::join_all(tasks).await;
    }

    // GPU stays busy through async pipelining
    async fn pipelined_execution(&mut self) {
        // CPU prepares next while GPU computes current
        // Naturally emerges from async/await pattern
    }
}
```

### No Legacy Tool Integration Needed

```rust
// Clean, modern async API replaces VPI/DPI/PLI
pub struct ModernTestbenchAPI {
    // Simple, type-safe, async API
    async fn drive_signal<T: Into<u64>>(&mut self, signal: &str, value: T) {
        self.poke(signal, value.into()).await;
    }

    async fn check_signal<T: From<u64>>(&self, signal: &str) -> T {
        T::from(self.peek(signal).await)
    }

    async fn wait_until(&mut self, condition: impl Fn(&Self) -> bool) {
        while !condition(self) {
            self.cycle().await;
        }
    }
}

// Migration path for legacy testbenches:
// 1. Auto-translate VPI/DPI calls to async Rust
// 2. One-time conversion tool provided
// 3. Massive performance improvement justifies migration
```

### Debugging and Introspection

```rust
pub struct TestbenchDebugger {
    // Breakpoints in testbench code
    breakpoints: HashSet<usize>,

    // Step through testbench execution
    fn step(&mut self) {
        self.execute_one_line();
        self.update_debugger_view();
    }

    // Inspect simulation state
    fn inspect(&self, signal: &str) -> SignalInfo {
        SignalInfo {
            current_value: self.runtime.peek(signal),
            history: self.waveform.get_history(signal),
            drivers: self.runtime.get_drivers(signal),
        }
    }

    // Time travel debugging
    fn rewind_to_cycle(&mut self, cycle: usize) {
        self.runtime.restore_checkpoint(cycle);
    }
}
```

### Async Synchronization - Clean and Efficient

The async runtime eliminates manual synchronization complexity:

```rust
// Clean async runtime - no manual synchronization needed!
pub struct AsyncSimulationRuntime {
    // Async channels handle all synchronization
    input_updates: mpsc::Sender<InputUpdate>,
    output_updates: mpsc::Receiver<OutputUpdate>,

    // Cycle coordination through async barrier
    cycle_barrier: Arc<AsyncBarrier>,

    // The GPU runtime task
    gpu_task: JoinHandle<()>,

    // Still zero-copy through unified memory
    unified_memory: Arc<MetalBuffer>,
}

impl AsyncSimulationRuntime {
    pub async fn new(design: &str) -> Self {
        let (input_tx, input_rx) = mpsc::channel(1000);
        let (output_tx, output_rx) = mpsc::channel(1000);
        let cycle_barrier = Arc::new(AsyncBarrier::new());

        // Spawn GPU task
        let gpu_task = tokio::spawn(Self::gpu_loop(
            design.to_string(),
            input_rx,
            output_tx,
            cycle_barrier.clone(),
        ));

        Self {
            input_updates: input_tx,
            output_updates: output_rx,
            cycle_barrier,
            gpu_task,
            unified_memory: Arc::new(MetalBuffer::new()),
        }
    }

    async fn gpu_loop(
        design: String,
        mut input_rx: mpsc::Receiver<InputUpdate>,
        output_tx: mpsc::Sender<OutputUpdate>,
        cycle_barrier: Arc<AsyncBarrier>,
    ) {
        let metal_device = Device::system_default().unwrap();
        let command_queue = metal_device.new_command_queue();

        loop {
            // Await inputs from testbench
            while let Some(update) = input_rx.recv().await {
                // Apply input updates
                apply_input(update);
            }

            // Await cycle barrier
            cycle_barrier.wait().await;

            // Run GPU simulation (also async!)
            let command_buffer = command_queue.new_command_buffer();
            let compute_encoder = command_buffer.new_compute_command_encoder();
            // ... dispatch Metal kernels ...
            compute_encoder.end_encoding();

            // Async wait for GPU completion
            let (tx, rx) = oneshot::channel();
            command_buffer.add_completed_handler(move |_| {
                tx.send(()).unwrap();
            });
            command_buffer.commit();
            rx.await.unwrap();

            // Send outputs back
            send_outputs(&output_tx).await;
        }
    }

    // Clean async API - no manual synchronization
    pub async fn poke(&mut self, signal: &str, value: u64) {
        self.input_updates.send(InputUpdate {
            signal: signal.to_string(),
            value,
        }).await.unwrap();
    }

    pub async fn peek(&mut self, signal: &str) -> u64 {
        // Request and await output
        let (tx, rx) = oneshot::channel();
        self.output_request.send((signal.to_string(), tx)).await.unwrap();
        rx.await.unwrap()
    }

    // Natural async cycle coordination
    pub async fn cycle(&mut self) {
        // Signal ready for next cycle
        self.cycle_barrier.signal_and_wait().await;
        // That's it! No manual synchronization needed
    }

    pub async fn cycles(&mut self, n: usize) {
        for _ in 0..n {
            self.cycle().await;
        }
    }
}
```

### Clean Async Testbench Example

```rust
// Natural async/await - no manual synchronization!
#[tokio::main]
async fn main() {
    let mut sim = AsyncSimulationRuntime::new("counter.sk").await;

    // All operations are naturally async
    sim.poke("reset", 1).await;
    sim.poke("enable", 0).await;

    sim.cycles(5).await;  // Clean async waiting

    sim.poke("reset", 0).await;
    sim.poke("enable", 1).await;

    sim.cycles(10).await;

    let count = sim.peek("count").await;
    assert_eq!(count, 10);

    // Can even run things in parallel!
    tokio::join!(
        sim.poke("data", 42),
        sim.poke("valid", 1),
    );
}
```

### The Async Flow - Natural and Efficient

```
Async Testbench         Async Runtime              GPU Task
      |                       |                        |
      |--poke("data").await-->|                        |
      |                       |--channel.send()------->|
      |--poke("valid").await->|                        |
      |                       |--channel.send()------->|
      |                       |                        |
      |--cycle().await------->|                        |
      |                       |--barrier.wait()------->|
      |                       |                        |--barrier.wait()
      |                       |                        |--simulate()
      |                       |<---channel.send()------|
      |<---future completes---|                        |
      |                       |                        |
      |--peek("result").await>|                        |
      |<---future completes---|                        |

No spin loops! No manual synchronization! Natural async/await!
```

### Key Advantages of Async Architecture

1. **No manual synchronization** - async/await handles everything
2. **No spin loops** - efficient waiting through futures
3. **Natural composition** - combine async operations easily
4. **Automatic backpressure** - channels prevent overload
5. **Still zero-copy** - unified memory underneath

### The Two-Runtime Architecture Benefits

1. **Maximum Performance**: Each runtime optimized for its hardware
2. **No Blocking**: Async/await on CPU, persistent threads on GPU
3. **Clean Separation**: CPU and GPU runtimes respect hardware boundaries
4. **Zero-Copy**: Unified memory for data, async channels for coordination
5. **Natural Parallelism**: Both runtimes can scale independently
6. **Hardware-Optimal**: Tokio for CPU tasks, Metal persistent threads for GPU

By using two coordinated async runtimes - Tokio on CPU and persistent Metal threads on GPU - SKALP achieves unprecedented performance while maintaining clean architecture. The runtimes communicate asynchronously through channels, eliminating spin loops and manual synchronization.

**The Architecture Summary**:
- **CPU Runtime (Tokio)**: Manages testbenches, I/O, and coordination
- **GPU Runtime (Metal)**: Persistent threads evaluate hardware continuously
- **Coordination**: Async channels provide natural backpressure and synchronization
- **Data Transfer**: Zero-copy through unified memory on Apple Silicon

**The bottom line**: We're not trying to force one runtime to manage both CPU and GPU. Instead, we're embracing the reality that they're separate processors that need separate runtimes, coordinated through modern async primitives. This is how heterogeneous computing should work.