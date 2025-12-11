# Gate-Level Simulation for Fault Injection

## Overview

To support fault simulation for ISO 26262 safety analysis, we need gate-level simulation capability. Instead of requiring external synthesized netlists (which are proprietary and process-specific), we extend our existing infrastructure:

1. **Extend LIR** with technology-independent primitives
2. **Add RTL-to-gate elaboration** pass
3. **Enhance simulators** (CPU/GPU) with gate-level mode and fault injection

## Architecture

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│    HIR      │────▶│    MIR      │────▶│    LIR      │────▶│    SIR      │
│  (source)   │     │  (typed)    │     │ (gate-level)│     │ (sim-ready) │
└─────────────┘     └─────────────┘     └─────────────┘     └─────────────┘
                                              │
                                              ▼
                                    ┌─────────────────────┐
                                    │  Gate-Level Netlist │
                                    │  (technology-indep) │
                                    └─────────────────────┘
                                              │
                          ┌───────────────────┼───────────────────┐
                          ▼                   ▼                   ▼
                    ┌──────────┐        ┌──────────┐        ┌──────────┐
                    │ RTL Sim  │        │ Gate Sim │        │Fault Sim │
                    │ (normal) │        │(gate-lvl)│        │(inject)  │
                    └──────────┘        └──────────┘        └──────────┘
```

## Phase 1: LIR Primitive Extensions

### Current GateType (already exists)
```rust
pub enum GateType {
    And, Or, Not, Nand, Nor, Xor, Xnor, Buffer, DFF, Latch,
}
```

### Extended Primitives for Safety Analysis

```rust
/// Extended gate types for technology-independent synthesis
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PrimitiveType {
    // === Combinational Logic ===
    /// N-input AND gate
    And { inputs: u8 },
    /// N-input OR gate
    Or { inputs: u8 },
    /// N-input NAND gate
    Nand { inputs: u8 },
    /// N-input NOR gate
    Nor { inputs: u8 },
    /// 2-input XOR gate
    Xor,
    /// 2-input XNOR gate
    Xnor,
    /// Inverter
    Inv,
    /// Buffer (non-inverting)
    Buf,
    /// Tri-state buffer
    Tribuf { enable_active_high: bool },
    /// 2:1 Multiplexer
    Mux2,
    /// 4:1 Multiplexer
    Mux4,
    /// N:1 Multiplexer (general)
    MuxN { select_bits: u8 },

    // === Sequential Logic ===
    /// D Flip-Flop (rising edge, active-high reset)
    DffP,
    /// D Flip-Flop (rising edge, active-low reset)
    DffN,
    /// D Flip-Flop (falling edge)
    DffNeg,
    /// D Flip-Flop with enable
    DffE,
    /// D Flip-Flop with async reset
    DffAR,
    /// D Flip-Flop with async set
    DffAS,
    /// D Flip-Flop with scan
    DffScan,
    /// D Latch (level-sensitive)
    Dlatch,
    /// SR Latch
    SRlatch,

    // === Arithmetic (for FIT estimation) ===
    /// Half adder
    HalfAdder,
    /// Full adder
    FullAdder,
    /// Comparator bit
    CompBit,

    // === Memory Elements ===
    /// Single-bit memory cell (for SRAM modeling)
    MemCell,
    /// Register file cell
    RegCell,
}

/// Primitive instance with hierarchical path for traceability
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Primitive {
    /// Unique ID within the design
    pub id: PrimitiveId,
    /// Primitive type
    pub ptype: PrimitiveType,
    /// Hierarchical path (e.g., "top.cpu.alu.adder_bit3")
    pub path: String,
    /// Input net IDs
    pub inputs: Vec<NetId>,
    /// Output net IDs
    pub outputs: Vec<NetId>,
    /// Clock net (for sequential elements)
    pub clock: Option<NetId>,
    /// Reset net (for sequential elements)
    pub reset: Option<NetId>,
    /// Enable net (for gated elements)
    pub enable: Option<NetId>,
    /// Bit width (for multi-bit primitives)
    pub width: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PrimitiveId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct NetId(pub u32);
```

### Gate-Level Netlist Structure

```rust
/// Technology-independent gate-level netlist
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GateNetlist {
    /// Design name
    pub name: String,
    /// Original module hierarchy (for traceability)
    pub hierarchy: Vec<HierarchyNode>,
    /// All primitives in flattened design
    pub primitives: Vec<Primitive>,
    /// All nets in flattened design
    pub nets: Vec<GateNet>,
    /// Primary inputs
    pub inputs: Vec<NetId>,
    /// Primary outputs
    pub outputs: Vec<NetId>,
    /// Clock nets
    pub clocks: Vec<NetId>,
    /// Statistics
    pub stats: NetlistStats,
}

/// Hierarchy node for traceability back to RTL
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HierarchyNode {
    /// Instance path
    pub path: String,
    /// Original module name
    pub module: String,
    /// Primitive ID range [start, end)
    pub primitive_range: (u32, u32),
}

/// Net in gate-level netlist
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GateNet {
    /// Net ID
    pub id: NetId,
    /// Name (for debugging)
    pub name: String,
    /// Driver primitive (None for primary inputs)
    pub driver: Option<(PrimitiveId, u8)>,  // (primitive, output_pin)
    /// Load primitives
    pub loads: Vec<(PrimitiveId, u8)>,  // (primitive, input_pin)
    /// Is this a primary input?
    pub is_pi: bool,
    /// Is this a primary output?
    pub is_po: bool,
    /// Is this a state element output?
    pub is_state: bool,
}

/// Netlist statistics for reporting
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct NetlistStats {
    pub total_primitives: u64,
    pub comb_gates: u64,
    pub flip_flops: u64,
    pub latches: u64,
    pub muxes: u64,
    pub total_nets: u64,
    pub max_fanout: u32,
    pub logic_depth: u32,
}
```

## Phase 2: RTL-to-Gate Elaboration

### Elaboration Rules

| RTL Construct | Gate-Level Mapping |
|--------------|-------------------|
| `signal a: bit = b & c` | AND2 gate |
| `signal a: bit = b \| c` | OR2 gate |
| `signal a: bit = !b` | INV gate |
| `signal a: bit = b ^ c` | XOR2 gate |
| `signal a: bit[8] = b + c` | 8x FullAdder chain |
| `signal a: bit = sel ? b : c` | MUX2 gate |
| `on(clk.rise) { reg <= val }` | DFF per bit |
| `signal a: bit[8] = b[3:0] ++ c[3:0]` | Wire routing only |
| `match sel { 0 => a, 1 => b, ... }` | MuxN or decoder+AND |

### Elaboration Pass Structure

```rust
/// RTL to gate-level elaboration
pub struct GateElaborator {
    /// Current primitive ID counter
    next_primitive_id: u32,
    /// Current net ID counter
    next_net_id: u32,
    /// Primitives being built
    primitives: Vec<Primitive>,
    /// Nets being built
    nets: Vec<GateNet>,
    /// Net name to ID mapping
    net_map: HashMap<String, NetId>,
    /// Current hierarchy path
    current_path: String,
}

impl GateElaborator {
    /// Elaborate MIR module to gate netlist
    pub fn elaborate(&mut self, mir: &MirModule) -> GateNetlist;

    /// Elaborate a single expression
    fn elaborate_expr(&mut self, expr: &MirExpr, output: NetId) -> Vec<Primitive>;

    /// Elaborate binary operation
    fn elaborate_binop(&mut self, op: BinOp, left: NetId, right: NetId, out: NetId) -> Primitive;

    /// Elaborate register assignment
    fn elaborate_register(&mut self, target: &str, clock: NetId, reset: Option<NetId>, value: NetId) -> Vec<Primitive>;

    /// Elaborate adder chain
    fn elaborate_adder(&mut self, width: u8, a: &[NetId], b: &[NetId], out: &[NetId]) -> Vec<Primitive>;
}
```

## Phase 3: Gate-Level Simulator

### Simulation Mode Selection

```rust
/// Simulation mode configuration
#[derive(Debug, Clone)]
pub enum SimulationMode {
    /// RTL-level simulation (fast, behavioral)
    Rtl,
    /// Gate-level simulation (accurate, for timing/fault analysis)
    GateLevel {
        /// Enable timing annotation
        timing: bool,
        /// Fault injection configuration
        fault_injection: Option<FaultInjectionConfig>,
    },
}

/// Fault injection configuration
#[derive(Debug, Clone)]
pub struct FaultInjectionConfig {
    /// Fault to inject
    pub fault: FaultSpec,
    /// Primitive to inject at
    pub target: PrimitiveId,
    /// Injection cycle
    pub inject_at_cycle: u64,
    /// Duration (None = permanent)
    pub duration: Option<u64>,
}

/// Fault specification
#[derive(Debug, Clone)]
pub enum FaultSpec {
    /// Output stuck at 0
    StuckAt0 { output_pin: u8 },
    /// Output stuck at 1
    StuckAt1 { output_pin: u8 },
    /// Single-event transient (bit flip for one cycle)
    SET { output_pin: u8 },
    /// Single-event upset (flip-flop bit flip, persistent)
    SEU,
    /// Bridging fault (short between two nets)
    Bridge { net_a: NetId, net_b: NetId },
}
```

### Gate Evaluation Functions

```rust
/// Evaluate a primitive given input values
pub fn evaluate_primitive(
    ptype: &PrimitiveType,
    inputs: &[BitValue],
    state: Option<BitValue>,  // For sequential elements
    clock_edge: Option<bool>, // true = rising, false = falling
) -> Vec<BitValue> {
    match ptype {
        PrimitiveType::And { inputs: n } => {
            vec![inputs.iter().all(|&b| b == BitValue::One).into()]
        }
        PrimitiveType::Or { inputs: n } => {
            vec![inputs.iter().any(|&b| b == BitValue::One).into()]
        }
        PrimitiveType::Inv => {
            vec![(!inputs[0]).into()]
        }
        PrimitiveType::Mux2 => {
            // inputs: [sel, d0, d1]
            if inputs[0] == BitValue::One { vec![inputs[2]] }
            else { vec![inputs[1]] }
        }
        PrimitiveType::DffP => {
            // Rising edge D flip-flop
            if clock_edge == Some(true) {
                vec![inputs[0]]  // D input captured
            } else {
                vec![state.unwrap_or(BitValue::X)]  // Hold state
            }
        }
        // ... etc
    }
}

/// Evaluate with fault injection
pub fn evaluate_with_fault(
    ptype: &PrimitiveType,
    inputs: &[BitValue],
    state: Option<BitValue>,
    clock_edge: Option<bool>,
    fault: Option<&FaultSpec>,
) -> Vec<BitValue> {
    let mut result = evaluate_primitive(ptype, inputs, state, clock_edge);

    if let Some(fault) = fault {
        match fault {
            FaultSpec::StuckAt0 { output_pin } => {
                result[*output_pin as usize] = BitValue::Zero;
            }
            FaultSpec::StuckAt1 { output_pin } => {
                result[*output_pin as usize] = BitValue::One;
            }
            FaultSpec::SET { output_pin } => {
                result[*output_pin as usize] = !result[*output_pin as usize];
            }
            FaultSpec::SEU => {
                // Flip the stored state
                result[0] = !result[0];
            }
            _ => {}
        }
    }

    result
}
```

### Integration with Existing Simulators

```rust
// In cpu_runtime.rs
impl CpuRuntime {
    /// Run one cycle in gate-level mode
    pub fn step_gate_level(
        &mut self,
        netlist: &GateNetlist,
        fault: Option<&FaultInjectionConfig>,
    ) -> SimulationResult<()> {
        // 1. Evaluate all combinational primitives in topological order
        for prim_id in &self.comb_order {
            let prim = &netlist.primitives[prim_id.0 as usize];
            let inputs = self.gather_inputs(prim, netlist);
            let fault_here = fault.filter(|f| f.target == *prim_id && f.is_active(self.cycle));
            let outputs = evaluate_with_fault(&prim.ptype, &inputs, None, None, fault_here.as_ref().map(|f| &f.fault));
            self.apply_outputs(prim, &outputs);
        }

        // 2. Capture register inputs on clock edge
        for prim_id in &self.seq_elements {
            let prim = &netlist.primitives[prim_id.0 as usize];
            let clock_val = self.get_net_value(prim.clock.unwrap());
            let clock_edge = self.detect_edge(prim.clock.unwrap(), clock_val);

            if clock_edge.is_some() {
                let inputs = self.gather_inputs(prim, netlist);
                let state = self.get_state(*prim_id);
                let fault_here = fault.filter(|f| f.target == *prim_id && f.is_active(self.cycle));
                let outputs = evaluate_with_fault(&prim.ptype, &inputs, state, clock_edge, fault_here.as_ref().map(|f| &f.fault));
                self.set_state(*prim_id, outputs[0]);
            }
        }

        self.cycle += 1;
        Ok(())
    }
}
```

## Phase 4: Fault Simulation Campaign

### Campaign Orchestration

```rust
/// Fault simulation campaign
pub struct FaultCampaign {
    /// Gate netlist
    netlist: GateNetlist,
    /// Safety goal with failure effects
    safety_goal: SafetyGoalSimSpec,
    /// Test vectors
    test_vectors: Vec<TestVector>,
    /// Configuration
    config: SimulationCampaignConfig,
}

impl FaultCampaign {
    /// Run complete fault simulation campaign
    pub fn run(&self) -> SimulationCampaignResults {
        let fault_universe = self.enumerate_faults();
        let mut results = SimulationCampaignResults::new(&self.safety_goal.name, "");

        // Parallel fault simulation
        let fault_results: Vec<_> = fault_universe
            .par_iter()  // Rayon parallel iterator
            .map(|fault| self.simulate_fault(fault))
            .collect();

        // Aggregate results
        for (fault, effect, detected, detector) in fault_results {
            results.record_fault_result(fault, effect, detected, detector);
        }

        results.update_metrics();
        results
    }

    /// Enumerate all fault sites
    fn enumerate_faults(&self) -> Vec<FaultSpec> {
        let mut faults = Vec::new();

        for prim in &self.netlist.primitives {
            // Stuck-at faults on each output
            for pin in 0..prim.outputs.len() {
                faults.push(FaultSpec::StuckAt0 { output_pin: pin as u8 });
                faults.push(FaultSpec::StuckAt1 { output_pin: pin as u8 });
            }

            // SEU for sequential elements
            if prim.ptype.is_sequential() {
                faults.push(FaultSpec::SEU);
            }
        }

        faults
    }

    /// Simulate design with single fault
    fn simulate_fault(&self, fault: &FaultSpec) -> FaultSimulationResult {
        let mut runtime = CpuRuntime::new_gate_level(&self.netlist);
        let fault_config = FaultInjectionConfig {
            fault: fault.clone(),
            target: /* ... */,
            inject_at_cycle: 10,
            duration: None,
        };

        // Run simulation with fault
        for vector in &self.test_vectors {
            runtime.apply_inputs(vector);
            runtime.step_gate_level(&self.netlist, Some(&fault_config));

            // Check failure effects
            for effect in &self.safety_goal.effects {
                if effect.evaluate(&runtime) {
                    // Effect triggered - check if detected
                    let detected = self.check_detection(&runtime);
                    return FaultSimulationResult { /* ... */ };
                }
            }
        }

        FaultSimulationResult { /* fault caused no effect */ }
    }
}
```

## FIT Estimation Without PDK

Since we don't have foundry-specific FIT data, we use technology-independent estimation:

```rust
/// Technology-independent FIT estimation
pub fn estimate_fit(ptype: &PrimitiveType) -> f64 {
    // Base FIT rates from industry averages (per billion hours)
    // These are representative values, can be overridden via config
    match ptype {
        // Combinational logic - low FIT, mainly SETs
        PrimitiveType::And { .. } | PrimitiveType::Or { .. } |
        PrimitiveType::Nand { .. } | PrimitiveType::Nor { .. } => 0.1,
        PrimitiveType::Xor | PrimitiveType::Xnor => 0.15,
        PrimitiveType::Inv | PrimitiveType::Buf => 0.05,
        PrimitiveType::Mux2 => 0.2,
        PrimitiveType::MuxN { select_bits } => 0.2 * (1 << *select_bits) as f64,

        // Sequential logic - higher FIT due to SEUs
        PrimitiveType::DffP | PrimitiveType::DffN | PrimitiveType::DffNeg => 1.0,
        PrimitiveType::DffE | PrimitiveType::DffAR | PrimitiveType::DffAS => 1.2,
        PrimitiveType::DffScan => 1.5,
        PrimitiveType::Dlatch | PrimitiveType::SRlatch => 0.8,

        // Arithmetic - moderate FIT
        PrimitiveType::HalfAdder => 0.2,
        PrimitiveType::FullAdder => 0.3,
        PrimitiveType::CompBit => 0.15,

        // Memory - highest FIT per bit
        PrimitiveType::MemCell => 2.0,
        PrimitiveType::RegCell => 1.5,

        _ => 0.5,  // Default
    }
}

/// Allow user override via configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FitOverrides {
    /// Override FIT for specific primitive types
    pub primitive_overrides: HashMap<String, f64>,
    /// Scaling factor for entire design (e.g., 0.7 for hardened process)
    pub global_scale: f64,
    /// Temperature derating factor
    pub temperature_factor: f64,
}
```

## Implementation Plan

### Phase 1: LIR Extensions (Week 1)
1. Add `PrimitiveType` enum to `lir.rs`
2. Add `Primitive`, `GateNet`, `GateNetlist` structs
3. Add `NetlistStats` for reporting
4. Add unit tests

### Phase 2: Gate Elaboration (Week 2)
1. Create `gate_elaborate.rs` in skalp-lir
2. Implement expression-to-gate mapping
3. Implement register elaboration
4. Implement adder/comparator synthesis
5. Add hierarchy tracking for traceability

### Phase 3: Simulator Integration (Week 3)
1. Add `SimulationMode` enum to skalp-sim
2. Implement `evaluate_primitive()` function
3. Implement `evaluate_with_fault()` function
4. Add `step_gate_level()` to CpuRuntime
5. Add topological sort for combinational evaluation

### Phase 4: Fault Campaign (Week 4)
1. Create `fault_campaign.rs` in skalp-safety
2. Implement fault enumeration
3. Implement parallel fault simulation
4. Integrate with existing `SimulationCampaignResults`
5. Add effect checking and DC calculation

## CLI Integration

```bash
# Run gate-level simulation
skalp sim --mode=gate-level design.sk

# Run fault simulation for safety analysis
skalp build --safety --gate-level design.sk

# Quick fault sampling (1% of faults)
skalp build --safety --gate-level --sampling=0.01 design.sk

# Full exhaustive fault simulation
skalp build --safety --gate-level --exhaustive design.sk
```

## Benefits

1. **No external dependencies** - No need for Synopsys DC, proprietary PDKs
2. **Reproducible** - Same results across environments
3. **Fast iteration** - Integrated in compile loop
4. **Traceable** - Hierarchy preserved for RTL-to-gate mapping
5. **Configurable** - FIT rates can be overridden for specific processes
6. **Parallelizable** - Works with existing GPU/CPU infrastructure
