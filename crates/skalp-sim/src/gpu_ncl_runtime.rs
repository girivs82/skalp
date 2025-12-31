//! GPU-Accelerated NCL (Null Convention Logic) Runtime
//!
//! Provides GPU acceleration for NCL async circuit simulation with proper
//! THmn threshold gate evaluation including hysteresis (state-holding behavior).
//!
//! # Architecture
//!
//! Unlike synchronous simulation, NCL simulation:
//! - Uses THmn gates that hold their output when inputs are in transition
//! - Requires per-gate state storage for hysteresis behavior
//! - Iterates until stable (no signal changes) instead of clock edges
//! - Operates on dual-rail encoded signals (true rail + false rail per bit)
//!
//! # GPU Buffers
//!
//! - `signal_buffer`: Current signal values (double-buffered for read/write)
//! - `gate_state_buffer`: Per-THmn gate state for hysteresis
//! - `change_buffer`: Atomic counter for detecting when simulation is stable
//!
//! # Usage
//!
//! ```ignore
//! let runtime = GpuNclRuntime::new(&gate_netlist)?;
//! runtime.set_dual_rail_input("a", 5, 8);  // a = 5, 8-bit
//! let iterations = runtime.run_until_stable(10000);
//! let result = runtime.get_dual_rail_output("y", 8);
//! ```

use skalp_lir::gate_netlist::{CellId, GateNetId, GateNetlist};
use std::collections::HashMap;

#[cfg(target_os = "macos")]
use metal::{
    Buffer, CommandQueue, CompileOptions, ComputePipelineState, Device, MTLResourceOptions, MTLSize,
};

/// GPU-accelerated NCL runtime
#[cfg(target_os = "macos")]
pub struct GpuNclRuntime {
    /// Metal device
    device: Device,
    /// Command queue
    command_queue: CommandQueue,
    /// The GateNetlist being simulated
    netlist: GateNetlist,
    /// Signal name to net IDs mapping (base name -> [net_id...])
    signal_name_to_nets: HashMap<String, Vec<GateNetId>>,
    /// Net ID to value index mapping
    net_to_index: HashMap<u32, usize>,
    /// Cell ID to state index mapping (for THmn gates)
    cell_to_state_index: HashMap<u32, usize>,
    /// Number of nets
    num_nets: usize,
    /// Number of THmn gates (gates needing state)
    num_stateful_gates: usize,
    /// All cells in topological order
    cells: Vec<NclCellInfo>,

    // GPU buffers
    /// Cell definitions buffer
    cell_buffer: Option<Buffer>,
    /// Signal values buffer A (double-buffered)
    signal_buffer_a: Option<Buffer>,
    /// Signal values buffer B (double-buffered)
    signal_buffer_b: Option<Buffer>,
    /// Which buffer is current (true = A, false = B)
    current_buffer_is_a: bool,
    /// THmn gate state buffer (for hysteresis)
    gate_state_buffer: Option<Buffer>,
    /// Change counter buffer (atomic, for stability detection)
    change_buffer: Option<Buffer>,

    // Compute pipelines
    /// Pipeline for NCL evaluation
    ncl_pipeline: Option<ComputePipelineState>,

    /// Whether to use GPU (falls back to CPU if compilation fails)
    use_gpu: bool,

    /// CPU fallback: net values
    cpu_net_values: Vec<bool>,
    /// CPU fallback: gate states
    cpu_gate_states: Vec<bool>,
    /// Whether last run_until_stable converged (changes == 0)
    is_stable: bool,
}

/// Information about an NCL cell for GPU simulation
#[derive(Debug, Clone)]
struct NclCellInfo {
    id: CellId,
    /// Primitive type
    ptype: NclPrimitiveType,
    /// Input net indices
    inputs: Vec<usize>,
    /// Output net indices
    outputs: Vec<usize>,
    /// State index (for THmn gates, -1 if stateless)
    state_index: i32,
}

/// NCL primitive types for GPU encoding
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
enum NclPrimitiveType {
    // Standard combinational (stateless)
    And2 = 0,
    And3 = 1,
    And4 = 2,
    Or2 = 3,
    Or3 = 4,
    Or4 = 5,
    Xor = 6,
    Inv = 7,
    Buf = 8,
    Mux2 = 9,
    Const0 = 10,
    Const1 = 11,
    // Arithmetic cells
    HalfAdder = 12,
    FullAdder = 13,

    // NCL threshold gates (stateful - need hysteresis)
    Th12 = 20,
    Th22 = 21,
    Th13 = 22,
    Th23 = 23,
    Th33 = 24,
    Th14 = 25,
    Th24 = 26,
    Th34 = 27,
    Th44 = 28,
    Thmn = 29, // Generic THmn (m stored in inputs[4], n in inputs[5])

    // NCL completion detection
    NclCompletion = 30,
}

/// GPU-compatible cell representation
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
struct GpuNclCell {
    /// Primitive type
    ptype: u32,
    /// Input net indices (up to 8 for THmn with large n)
    inputs: [u32; 8],
    /// Number of inputs
    num_inputs: u32,
    /// Primary output net index (sum for adders)
    output: u32,
    /// State index (-1 if no state needed)
    state_index: i32,
    /// For generic THmn: m threshold
    threshold_m: u32,
    /// For generic THmn: n inputs
    threshold_n: u32,
    /// Secondary output net index (carry for adders, 0xFFFFFFFF if unused)
    output2: u32,
}

#[cfg(target_os = "macos")]
impl GpuNclRuntime {
    /// Create a new GPU NCL runtime from a GateNetlist
    pub fn new(netlist: GateNetlist) -> Result<Self, String> {
        let device = Device::system_default().ok_or_else(|| "No Metal device found".to_string())?;
        let command_queue = device.new_command_queue();

        let num_nets = netlist.nets.len();

        let mut runtime = Self {
            device,
            command_queue,
            netlist,
            signal_name_to_nets: HashMap::new(),
            net_to_index: HashMap::new(),
            cell_to_state_index: HashMap::new(),
            num_nets,
            num_stateful_gates: 0,
            cells: Vec::new(),
            cell_buffer: None,
            signal_buffer_a: None,
            signal_buffer_b: None,
            current_buffer_is_a: true,
            gate_state_buffer: None,
            change_buffer: None,
            ncl_pipeline: None,
            use_gpu: true,
            cpu_net_values: vec![false; num_nets],
            cpu_gate_states: Vec::new(),
            is_stable: false,
        };

        runtime.initialize()?;

        // Try to compile GPU shaders
        if let Err(e) = runtime.compile_shaders() {
            eprintln!(
                "GPU NCL shader compilation failed, falling back to CPU: {}",
                e
            );
            runtime.use_gpu = false;
        }

        Ok(runtime)
    }

    /// Initialize the runtime from the GateNetlist
    fn initialize(&mut self) -> Result<(), String> {
        // Build net mappings
        for (i, net) in self.netlist.nets.iter().enumerate() {
            self.net_to_index.insert(net.id.0, i);

            // Group nets by base name (strip bit suffix like [0], _t0, etc.)
            let base_name = strip_bit_suffix(&net.name);
            self.signal_name_to_nets
                .entry(base_name)
                .or_default()
                .push(net.id);
        }

        // Build cell info and count stateful gates
        let mut state_index = 0i32;
        for cell in &self.netlist.cells {
            let ptype = self.classify_cell(&cell.cell_type);
            let is_stateful = self.is_stateful_gate(ptype);

            let inputs: Vec<usize> = cell
                .inputs
                .iter()
                .filter_map(|net_id| self.net_to_index.get(&net_id.0).copied())
                .collect();

            let outputs: Vec<usize> = cell
                .outputs
                .iter()
                .filter_map(|net_id| self.net_to_index.get(&net_id.0).copied())
                .collect();

            let cell_state_index = if is_stateful {
                let idx = state_index;
                self.cell_to_state_index.insert(cell.id.0, idx as usize);
                state_index += 1;
                idx
            } else {
                -1
            };

            self.cells.push(NclCellInfo {
                id: cell.id,
                ptype,
                inputs,
                outputs,
                state_index: cell_state_index,
            });
        }

        self.num_stateful_gates = state_index as usize;
        self.cpu_gate_states = vec![false; self.num_stateful_gates];

        // Allocate GPU buffers
        self.allocate_buffers()?;

        Ok(())
    }

    /// Classify a cell type string to NclPrimitiveType
    fn classify_cell(&self, cell_type: &str) -> NclPrimitiveType {
        let upper = cell_type.to_uppercase();

        // Check for THmn patterns first
        if upper.starts_with("TH") {
            if let Some(digits) = upper.strip_prefix("TH") {
                if digits.len() == 2 {
                    let m = digits.chars().next().and_then(|c| c.to_digit(10));
                    let n = digits.chars().nth(1).and_then(|c| c.to_digit(10));
                    if let (Some(m), Some(n)) = (m, n) {
                        return match (m, n) {
                            (1, 2) => NclPrimitiveType::Th12,
                            (2, 2) => NclPrimitiveType::Th22,
                            (1, 3) => NclPrimitiveType::Th13,
                            (2, 3) => NclPrimitiveType::Th23,
                            (3, 3) => NclPrimitiveType::Th33,
                            (1, 4) => NclPrimitiveType::Th14,
                            (2, 4) => NclPrimitiveType::Th24,
                            (3, 4) => NclPrimitiveType::Th34,
                            (4, 4) => NclPrimitiveType::Th44,
                            _ => NclPrimitiveType::Thmn,
                        };
                    }
                }
            }
        }

        // NCL completion
        if upper.starts_with("NCL_COMPLETE") {
            return NclPrimitiveType::NclCompletion;
        }

        // Strip library suffix (e.g., "AND2_X1" -> "AND2")
        let base_type = upper.split('_').next().unwrap_or(&upper).to_string();

        // Standard gates
        match base_type.as_str() {
            "AND2" => NclPrimitiveType::And2,
            "AND3" => NclPrimitiveType::And3,
            "AND4" => NclPrimitiveType::And4,
            "OR2" => NclPrimitiveType::Or2,
            "OR3" => NclPrimitiveType::Or3,
            "OR4" => NclPrimitiveType::Or4,
            "XOR" | "XOR2" => NclPrimitiveType::Xor,
            "INV" | "NOT" => NclPrimitiveType::Inv,
            "BUF" | "BUFFER" => NclPrimitiveType::Buf,
            "MUX2" => NclPrimitiveType::Mux2,
            "TIE" | "TIEH" | "VDD" if upper.contains("HIGH") => NclPrimitiveType::Const1,
            "TIE" | "TIEL" | "GND" | "VSS" if !upper.contains("HIGH") => NclPrimitiveType::Const0,
            // Arithmetic cells
            "HA" | "HALFADDER" | "HALF" => NclPrimitiveType::HalfAdder,
            "FA" | "FULLADDER" | "FULL" => NclPrimitiveType::FullAdder,
            _ => {
                // Check for full name match for tie cells
                if upper.starts_with("TIE_HIGH") || upper == "TIEH" || upper == "VDD" {
                    NclPrimitiveType::Const1
                } else if upper.starts_with("TIE_LOW")
                    || upper == "TIEL"
                    || upper == "GND"
                    || upper == "VSS"
                {
                    NclPrimitiveType::Const0
                } else {
                    NclPrimitiveType::Buf // Default
                }
            }
        }
    }

    /// Check if a primitive type needs state tracking
    fn is_stateful_gate(&self, ptype: NclPrimitiveType) -> bool {
        matches!(
            ptype,
            NclPrimitiveType::Th12
                | NclPrimitiveType::Th22
                | NclPrimitiveType::Th13
                | NclPrimitiveType::Th23
                | NclPrimitiveType::Th33
                | NclPrimitiveType::Th14
                | NclPrimitiveType::Th24
                | NclPrimitiveType::Th34
                | NclPrimitiveType::Th44
                | NclPrimitiveType::Thmn
        )
    }

    /// Allocate GPU buffers
    fn allocate_buffers(&mut self) -> Result<(), String> {
        let num_cells = self.cells.len();

        // Cell definitions buffer
        let cell_size = std::mem::size_of::<GpuNclCell>();
        let cells_buffer_size = num_cells * cell_size;
        self.cell_buffer = Some(self.device.new_buffer(
            cells_buffer_size as u64,
            MTLResourceOptions::StorageModeShared,
        ));

        // Fill cell buffer
        if let Some(ref buf) = self.cell_buffer {
            let ptr = buf.contents() as *mut GpuNclCell;
            for (i, cell) in self.cells.iter().enumerate() {
                // Build inputs array
                let mut inputs = [0u32; 8];
                for (j, &input_idx) in cell.inputs.iter().enumerate().take(8) {
                    inputs[j] = input_idx as u32;
                }

                // Get threshold values for THmn gates
                let (threshold_m, threshold_n) = if cell.ptype == NclPrimitiveType::Thmn {
                    let cell_type = &self.netlist.cells[i].cell_type;
                    parse_thmn(cell_type)
                        .map(|(m, n)| (m as u32, n as u32))
                        .unwrap_or((0, 0))
                } else {
                    (0, 0)
                };

                // Get secondary output for multi-output cells (HA/FA carry)
                let output2 = cell.outputs.get(1).map(|&o| o as u32).unwrap_or(0xFFFFFFFF);

                let gpu_cell = GpuNclCell {
                    ptype: cell.ptype as u32,
                    inputs,
                    num_inputs: cell.inputs.len() as u32,
                    output: cell.outputs.first().copied().unwrap_or(0) as u32,
                    state_index: cell.state_index,
                    threshold_m,
                    threshold_n,
                    output2,
                };

                unsafe {
                    *ptr.add(i) = gpu_cell;
                }
            }
        }

        // Signal buffers (double-buffered)
        let signal_buffer_size = (self.num_nets * std::mem::size_of::<u32>()) as u64;
        self.signal_buffer_a = Some(
            self.device
                .new_buffer(signal_buffer_size, MTLResourceOptions::StorageModeShared),
        );
        self.signal_buffer_b = Some(
            self.device
                .new_buffer(signal_buffer_size, MTLResourceOptions::StorageModeShared),
        );

        // Gate state buffer
        if self.num_stateful_gates > 0 {
            let state_buffer_size = (self.num_stateful_gates * std::mem::size_of::<u32>()) as u64;
            self.gate_state_buffer = Some(
                self.device
                    .new_buffer(state_buffer_size, MTLResourceOptions::StorageModeShared),
            );
        }

        // Change counter buffer
        self.change_buffer = Some(self.device.new_buffer(
            std::mem::size_of::<u32>() as u64,
            MTLResourceOptions::StorageModeShared,
        ));

        Ok(())
    }

    /// Compile Metal shaders
    fn compile_shaders(&mut self) -> Result<(), String> {
        let shader_source = self.generate_ncl_shader();
        let options = CompileOptions::new();
        let library = self
            .device
            .new_library_with_source(&shader_source, &options)
            .map_err(|e| format!("Shader compilation failed: {}", e))?;

        let ncl_func = library
            .get_function("eval_ncl", None)
            .map_err(|e| format!("Function 'eval_ncl' not found: {}", e))?;

        self.ncl_pipeline = Some(
            self.device
                .new_compute_pipeline_state_with_function(&ncl_func)
                .map_err(|e| format!("Pipeline creation failed: {}", e))?,
        );

        Ok(())
    }

    /// Generate Metal shader source for NCL evaluation
    fn generate_ncl_shader(&self) -> String {
        r#"
#include <metal_stdlib>
using namespace metal;

// NCL Primitive types
constant uint PTYPE_AND2 = 0;
constant uint PTYPE_AND3 = 1;
constant uint PTYPE_AND4 = 2;
constant uint PTYPE_OR2 = 3;
constant uint PTYPE_OR3 = 4;
constant uint PTYPE_OR4 = 5;
constant uint PTYPE_XOR = 6;
constant uint PTYPE_INV = 7;
constant uint PTYPE_BUF = 8;
constant uint PTYPE_MUX2 = 9;
constant uint PTYPE_CONST0 = 10;
constant uint PTYPE_CONST1 = 11;
// Arithmetic cells
constant uint PTYPE_HALFADDER = 12;
constant uint PTYPE_FULLADDER = 13;

// THmn threshold gates (stateful)
constant uint PTYPE_TH12 = 20;
constant uint PTYPE_TH22 = 21;
constant uint PTYPE_TH13 = 22;
constant uint PTYPE_TH23 = 23;
constant uint PTYPE_TH33 = 24;
constant uint PTYPE_TH14 = 25;
constant uint PTYPE_TH24 = 26;
constant uint PTYPE_TH34 = 27;
constant uint PTYPE_TH44 = 28;
constant uint PTYPE_THMN = 29;

constant uint PTYPE_NCL_COMPLETION = 30;

struct NclCell {
    uint ptype;
    uint inputs[8];
    uint num_inputs;
    uint output;
    int state_index;
    uint threshold_m;
    uint threshold_n;
    uint output2;  // Secondary output for multi-output cells (carry for adders)
};

// Evaluate THmn gate with hysteresis
// Returns: new output value
// Updates: gate_states[state_index] if state_index >= 0
uint eval_thmn(
    uint m,
    uint n,
    uint in0, uint in1, uint in2, uint in3,
    uint in4, uint in5, uint in6, uint in7,
    uint num_inputs,
    uint prev_state
) {
    // Count high inputs
    uint count = 0;
    if (num_inputs > 0 && in0 != 0) count++;
    if (num_inputs > 1 && in1 != 0) count++;
    if (num_inputs > 2 && in2 != 0) count++;
    if (num_inputs > 3 && in3 != 0) count++;
    if (num_inputs > 4 && in4 != 0) count++;
    if (num_inputs > 5 && in5 != 0) count++;
    if (num_inputs > 6 && in6 != 0) count++;
    if (num_inputs > 7 && in7 != 0) count++;

    // Check if all inputs are low
    bool all_low = (count == 0);

    // THmn behavior:
    // - Output = 1 when count >= m (threshold met)
    // - Output = 0 when all inputs are 0
    // - Output = previous state otherwise (hysteresis)
    if (count >= m) {
        return 1;
    } else if (all_low) {
        return 0;
    } else {
        return prev_state;
    }
}

// Evaluate a combinational (stateless) gate
uint eval_comb_gate(uint ptype, uint in0, uint in1, uint in2, uint in3) {
    switch (ptype) {
        case PTYPE_AND2: return in0 & in1;
        case PTYPE_AND3: return in0 & in1 & in2;
        case PTYPE_AND4: return in0 & in1 & in2 & in3;
        case PTYPE_OR2: return in0 | in1;
        case PTYPE_OR3: return in0 | in1 | in2;
        case PTYPE_OR4: return in0 | in1 | in2 | in3;
        case PTYPE_XOR: return in0 ^ in1;
        case PTYPE_INV: return (~in0) & 1;
        case PTYPE_BUF: return in0;
        case PTYPE_MUX2: return (in2 == 0) ? in0 : in1;
        case PTYPE_CONST0: return 0;
        case PTYPE_CONST1: return 1;
        // Arithmetic: HalfAdder sum = a XOR b
        case PTYPE_HALFADDER: return in0 ^ in1;
        // Arithmetic: FullAdder sum = a XOR b XOR cin
        case PTYPE_FULLADDER: return in0 ^ in1 ^ in2;
        default: return 0;
    }
}

// Kernel: Evaluate all NCL cells
// Each thread evaluates one cell
kernel void eval_ncl(
    device const NclCell* cells [[buffer(0)]],
    device const uint* signals_in [[buffer(1)]],
    device uint* signals_out [[buffer(2)]],
    device uint* gate_states [[buffer(3)]],
    device atomic_uint* change_count [[buffer(4)]],
    constant uint& num_cells [[buffer(5)]],
    uint tid [[thread_position_in_grid]]
) {
    if (tid >= num_cells) return;

    NclCell cell = cells[tid];

    // Get input values
    uint in0 = signals_in[cell.inputs[0]];
    uint in1 = (cell.num_inputs > 1) ? signals_in[cell.inputs[1]] : 0;
    uint in2 = (cell.num_inputs > 2) ? signals_in[cell.inputs[2]] : 0;
    uint in3 = (cell.num_inputs > 3) ? signals_in[cell.inputs[3]] : 0;
    uint in4 = (cell.num_inputs > 4) ? signals_in[cell.inputs[4]] : 0;
    uint in5 = (cell.num_inputs > 5) ? signals_in[cell.inputs[5]] : 0;
    uint in6 = (cell.num_inputs > 6) ? signals_in[cell.inputs[6]] : 0;
    uint in7 = (cell.num_inputs > 7) ? signals_in[cell.inputs[7]] : 0;

    uint result = 0;
    uint ptype = cell.ptype;

    // Handle THmn gates (stateful)
    if (ptype >= PTYPE_TH12 && ptype <= PTYPE_THMN) {
        uint m, n;

        // Determine m and n based on primitive type
        switch (ptype) {
            case PTYPE_TH12: m = 1; n = 2; break;
            case PTYPE_TH22: m = 2; n = 2; break;
            case PTYPE_TH13: m = 1; n = 3; break;
            case PTYPE_TH23: m = 2; n = 3; break;
            case PTYPE_TH33: m = 3; n = 3; break;
            case PTYPE_TH14: m = 1; n = 4; break;
            case PTYPE_TH24: m = 2; n = 4; break;
            case PTYPE_TH34: m = 3; n = 4; break;
            case PTYPE_TH44: m = 4; n = 4; break;
            case PTYPE_THMN:
                m = cell.threshold_m;
                n = cell.threshold_n;
                break;
            default: m = 1; n = 2; break;
        }

        // Get previous state
        uint prev_state = 0;
        if (cell.state_index >= 0) {
            prev_state = gate_states[cell.state_index];
        }

        // Evaluate with hysteresis
        result = eval_thmn(m, n, in0, in1, in2, in3, in4, in5, in6, in7, cell.num_inputs, prev_state);

        // Update state
        if (cell.state_index >= 0) {
            gate_states[cell.state_index] = result;
        }
    }
    // Handle NCL completion detector
    else if (ptype == PTYPE_NCL_COMPLETION) {
        // Completion: all dual-rail pairs must be in same phase
        // Assumes inputs are: t0, f0, t1, f1, ...
        uint width = cell.num_inputs / 2;
        bool all_data = true;
        bool all_null = true;

        for (uint i = 0; i < width && i < 4; i++) {
            uint t = 0, f = 0;
            switch (i) {
                case 0: t = in0; f = in1; break;
                case 1: t = in2; f = in3; break;
                case 2: t = in4; f = in5; break;
                case 3: t = in6; f = in7; break;
            }
            bool is_data = (t ^ f) != 0;  // Exactly one high
            bool is_null = (t == 0) && (f == 0);  // Both low
            all_data = all_data && is_data;
            all_null = all_null && is_null;
        }

        result = (all_data || all_null) ? 1 : 0;
    }
    // Handle standard combinational gates
    else {
        result = eval_comb_gate(ptype, in0, in1, in2, in3);
    }

    // Write primary output and check for change
    uint old_value = signals_out[cell.output];
    signals_out[cell.output] = result;

    if (old_value != result) {
        atomic_fetch_add_explicit(change_count, 1, memory_order_relaxed);
    }

    // Handle secondary output for multi-output cells (HalfAdder, FullAdder)
    if (cell.output2 != 0xFFFFFFFF) {
        uint carry = 0;
        if (ptype == PTYPE_HALFADDER) {
            // HalfAdder carry = a AND b
            carry = in0 & in1;
        } else if (ptype == PTYPE_FULLADDER) {
            // FullAdder carry = (a AND b) OR (cin AND (a XOR b))
            carry = (in0 & in1) | (in2 & (in0 ^ in1));
        }

        uint old_carry = signals_out[cell.output2];
        signals_out[cell.output2] = carry;

        if (old_carry != carry) {
            atomic_fetch_add_explicit(change_count, 1, memory_order_relaxed);
        }
    }
}
"#
        .to_string()
    }

    /// Get device name
    pub fn device_name(&self) -> String {
        self.device.name().to_string()
    }

    /// Set a net value
    pub fn set_net(&mut self, net_id: GateNetId, value: bool) {
        if let Some(&idx) = self.net_to_index.get(&net_id.0) {
            self.cpu_net_values[idx] = value;

            if self.use_gpu {
                let buffer = if self.current_buffer_is_a {
                    &self.signal_buffer_a
                } else {
                    &self.signal_buffer_b
                };
                if let Some(buf) = buffer {
                    unsafe {
                        let ptr = buf.contents() as *mut u32;
                        *ptr.add(idx) = value as u32;
                    }
                }
            }
        }
    }

    /// Get a net value
    pub fn get_net(&self, net_id: GateNetId) -> bool {
        if let Some(&idx) = self.net_to_index.get(&net_id.0) {
            if self.use_gpu {
                let buffer = if self.current_buffer_is_a {
                    &self.signal_buffer_a
                } else {
                    &self.signal_buffer_b
                };
                if let Some(buf) = buffer {
                    unsafe {
                        let ptr = buf.contents() as *const u32;
                        return *ptr.add(idx) != 0;
                    }
                }
            }
            self.cpu_net_values[idx]
        } else {
            false
        }
    }

    /// Set a dual-rail input value
    ///
    /// The layout is: t rails first [0..width), then f rails [width..2*width)
    pub fn set_dual_rail_value(&mut self, name: &str, value: u64, width: usize) {
        if let Some(nets) = self.signal_name_to_nets.get(name).cloned() {
            // Determine actual width from signal nets (t rails + f rails)
            let actual_width = nets.len() / 2;
            let width = width.min(actual_width);

            for bit in 0..width {
                // For bits beyond u64 range, value is 0
                let bit_value = if bit < 64 {
                    (value >> bit) & 1 != 0
                } else {
                    false
                };
                // Layout: [t0, t1, ..., tN-1, f0, f1, ..., fN-1]
                let t_idx = bit;
                let f_idx = actual_width + bit;

                if let Some(&t_net) = nets.get(t_idx) {
                    self.set_net(t_net, bit_value);
                }
                if let Some(&f_net) = nets.get(f_idx) {
                    self.set_net(f_net, !bit_value);
                }
            }
        }
    }

    /// Set all inputs to NULL (both rails low)
    ///
    /// The layout is: t rails first [0..width), then f rails [width..2*width)
    pub fn set_null(&mut self, name: &str, width: usize) {
        if let Some(nets) = self.signal_name_to_nets.get(name).cloned() {
            // Determine actual width from signal nets
            let actual_width = nets.len() / 2;
            let width = width.min(actual_width);

            for bit in 0..width {
                // Layout: [t0, t1, ..., tN-1, f0, f1, ..., fN-1]
                let t_idx = bit;
                let f_idx = actual_width + bit;

                if let Some(&t_net) = nets.get(t_idx) {
                    self.set_net(t_net, false);
                }
                if let Some(&f_net) = nets.get(f_idx) {
                    self.set_net(f_net, false);
                }
            }
        }
    }

    /// Get a dual-rail output value
    ///
    /// The layout is: t rails first [0..width), then f rails [width..2*width)
    pub fn get_dual_rail_value(&self, name: &str, width: usize) -> Option<u64> {
        let nets = self.signal_name_to_nets.get(name)?;
        let mut result = 0u64;

        // Determine actual width from signal nets
        let actual_width = nets.len() / 2;
        let width = width.min(actual_width);

        for bit in 0..width {
            // Layout: [t0, t1, ..., tN-1, f0, f1, ..., fN-1]
            let t_idx = bit;
            let f_idx = actual_width + bit;

            let t = nets.get(t_idx).map(|&n| self.get_net(n)).unwrap_or(false);
            let f = nets.get(f_idx).map(|&n| self.get_net(n)).unwrap_or(false);

            // Check for valid DATA (exactly one rail high)
            if t && !f {
                // Only set bits within u64 range
                if bit < 64 {
                    result |= 1u64 << bit;
                }
            } else if !t && f {
                // DATA_FALSE, bit stays 0
            } else {
                // NULL or Invalid
                return None;
            }
        }

        Some(result)
    }

    /// Run one iteration of NCL evaluation
    /// Returns the number of signals that changed
    pub fn iterate(&mut self) -> u32 {
        if self.use_gpu {
            self.iterate_gpu()
        } else {
            self.iterate_cpu()
        }
    }

    /// GPU iteration
    fn iterate_gpu(&mut self) -> u32 {
        // Reset change counter
        if let Some(ref buf) = self.change_buffer {
            unsafe {
                let ptr = buf.contents() as *mut u32;
                *ptr = 0;
            }
        }

        // Swap buffers
        self.current_buffer_is_a = !self.current_buffer_is_a;

        // Copy current to output buffer
        let (src_buf, dst_buf) = if self.current_buffer_is_a {
            (&self.signal_buffer_b, &self.signal_buffer_a)
        } else {
            (&self.signal_buffer_a, &self.signal_buffer_b)
        };

        if let (Some(src), Some(dst)) = (src_buf, dst_buf) {
            unsafe {
                std::ptr::copy_nonoverlapping(
                    src.contents() as *const u8,
                    dst.contents() as *mut u8,
                    self.num_nets * std::mem::size_of::<u32>(),
                );
            }
        }

        // Create command buffer
        let command_buffer = self.command_queue.new_command_buffer();
        let encoder = command_buffer.new_compute_command_encoder();

        if let Some(ref pipeline) = self.ncl_pipeline {
            encoder.set_compute_pipeline_state(pipeline);

            // Set buffers
            if let Some(ref buf) = self.cell_buffer {
                encoder.set_buffer(0, Some(buf), 0);
            }

            let src_buffer = if self.current_buffer_is_a {
                &self.signal_buffer_b
            } else {
                &self.signal_buffer_a
            };
            if let Some(ref buf) = src_buffer {
                encoder.set_buffer(1, Some(buf), 0);
            }

            let dst_buffer = if self.current_buffer_is_a {
                &self.signal_buffer_a
            } else {
                &self.signal_buffer_b
            };
            if let Some(ref buf) = dst_buffer {
                encoder.set_buffer(2, Some(buf), 0);
            }

            if let Some(ref buf) = self.gate_state_buffer {
                encoder.set_buffer(3, Some(buf), 0);
            } else {
                // Create dummy buffer if no stateful gates
                let dummy = self
                    .device
                    .new_buffer(4, MTLResourceOptions::StorageModeShared);
                encoder.set_buffer(3, Some(&dummy), 0);
            }

            if let Some(ref buf) = self.change_buffer {
                encoder.set_buffer(4, Some(buf), 0);
            }

            let num_cells = self.cells.len() as u32;
            encoder.set_bytes(
                5,
                std::mem::size_of::<u32>() as u64,
                &num_cells as *const u32 as *const _,
            );

            // Dispatch
            let thread_group_size = MTLSize::new(64, 1, 1);
            let grid_size = MTLSize::new(self.cells.len() as u64, 1, 1);
            encoder.dispatch_threads(grid_size, thread_group_size);
        }

        encoder.end_encoding();
        command_buffer.commit();
        command_buffer.wait_until_completed();

        // Read change count
        if let Some(ref buf) = self.change_buffer {
            unsafe {
                let ptr = buf.contents() as *const u32;
                *ptr
            }
        } else {
            0
        }
    }

    /// CPU iteration (fallback)
    fn iterate_cpu(&mut self) -> u32 {
        let mut changes = 0u32;

        for cell in &self.cells {
            let inputs: Vec<bool> = cell
                .inputs
                .iter()
                .map(|&idx| self.cpu_net_values.get(idx).copied().unwrap_or(false))
                .collect();

            let result = match cell.ptype {
                NclPrimitiveType::And2 => {
                    inputs.first().copied().unwrap_or(false)
                        && inputs.get(1).copied().unwrap_or(false)
                }
                NclPrimitiveType::And3 => inputs.iter().take(3).all(|&x| x),
                NclPrimitiveType::And4 => inputs.iter().take(4).all(|&x| x),
                NclPrimitiveType::Or2 => {
                    inputs.first().copied().unwrap_or(false)
                        || inputs.get(1).copied().unwrap_or(false)
                }
                NclPrimitiveType::Or3 => inputs.iter().take(3).any(|&x| x),
                NclPrimitiveType::Or4 => inputs.iter().take(4).any(|&x| x),
                NclPrimitiveType::Xor => {
                    inputs.first().copied().unwrap_or(false)
                        ^ inputs.get(1).copied().unwrap_or(false)
                }
                NclPrimitiveType::Inv => !inputs.first().copied().unwrap_or(false),
                NclPrimitiveType::Buf => inputs.first().copied().unwrap_or(false),
                NclPrimitiveType::Mux2 => {
                    let sel = inputs.get(2).copied().unwrap_or(false);
                    if sel {
                        inputs.get(1).copied().unwrap_or(false)
                    } else {
                        inputs.first().copied().unwrap_or(false)
                    }
                }
                NclPrimitiveType::Const0 => false,
                NclPrimitiveType::Const1 => true,
                NclPrimitiveType::HalfAdder => {
                    // HalfAdder: sum = a XOR b, carry = a AND b
                    // For single output, we return sum (first output)
                    // Carry is handled via second output below
                    let a = inputs.first().copied().unwrap_or(false);
                    let b = inputs.get(1).copied().unwrap_or(false);
                    a ^ b // sum
                }
                NclPrimitiveType::FullAdder => {
                    // FullAdder: sum = a XOR b XOR cin, cout = (a AND b) OR (cin AND (a XOR b))
                    // For single output, we return sum (first output)
                    // Carry is handled via second output below
                    let a = inputs.first().copied().unwrap_or(false);
                    let b = inputs.get(1).copied().unwrap_or(false);
                    let cin = inputs.get(2).copied().unwrap_or(false);
                    a ^ b ^ cin // sum
                }
                NclPrimitiveType::Th12
                | NclPrimitiveType::Th22
                | NclPrimitiveType::Th13
                | NclPrimitiveType::Th23
                | NclPrimitiveType::Th33
                | NclPrimitiveType::Th14
                | NclPrimitiveType::Th24
                | NclPrimitiveType::Th34
                | NclPrimitiveType::Th44
                | NclPrimitiveType::Thmn => {
                    let (m, n) = match cell.ptype {
                        NclPrimitiveType::Th12 => (1, 2),
                        NclPrimitiveType::Th22 => (2, 2),
                        NclPrimitiveType::Th13 => (1, 3),
                        NclPrimitiveType::Th23 => (2, 3),
                        NclPrimitiveType::Th33 => (3, 3),
                        NclPrimitiveType::Th14 => (1, 4),
                        NclPrimitiveType::Th24 => (2, 4),
                        NclPrimitiveType::Th34 => (3, 4),
                        NclPrimitiveType::Th44 => (4, 4),
                        _ => {
                            // Parse from netlist
                            let cell_idx =
                                self.cells.iter().position(|c| c.id == cell.id).unwrap_or(0);
                            parse_thmn(&self.netlist.cells[cell_idx].cell_type).unwrap_or((1, 2))
                        }
                    };

                    let count: usize = inputs.iter().take(n).filter(|&&x| x).count();
                    let all_low = count == 0;
                    let prev_state = if cell.state_index >= 0 {
                        self.cpu_gate_states
                            .get(cell.state_index as usize)
                            .copied()
                            .unwrap_or(false)
                    } else {
                        false
                    };

                    let new_state = if count >= m {
                        true
                    } else if all_low {
                        false
                    } else {
                        prev_state
                    };

                    if cell.state_index >= 0 {
                        if let Some(state) = self.cpu_gate_states.get_mut(cell.state_index as usize)
                        {
                            *state = new_state;
                        }
                    }

                    new_state
                }
                NclPrimitiveType::NclCompletion => {
                    let width = inputs.len() / 2;
                    let mut all_data = true;
                    let mut all_null = true;

                    for i in 0..width {
                        let t = inputs.get(i * 2).copied().unwrap_or(false);
                        let f = inputs.get(i * 2 + 1).copied().unwrap_or(false);
                        let is_data = t ^ f;
                        let is_null = !t && !f;
                        all_data = all_data && is_data;
                        all_null = all_null && is_null;
                    }

                    all_data || all_null
                }
            };

            // Write outputs
            if let Some(&out_idx) = cell.outputs.first() {
                let old_value = self.cpu_net_values.get(out_idx).copied().unwrap_or(false);
                if old_value != result {
                    changes += 1;
                }
                if let Some(val) = self.cpu_net_values.get_mut(out_idx) {
                    *val = result;
                }
            }

            // Handle multi-output cells (HalfAdder, FullAdder have carry as second output)
            if cell.outputs.len() > 1 {
                let carry = match cell.ptype {
                    NclPrimitiveType::HalfAdder => {
                        let a = inputs.first().copied().unwrap_or(false);
                        let b = inputs.get(1).copied().unwrap_or(false);
                        a && b // carry
                    }
                    NclPrimitiveType::FullAdder => {
                        let a = inputs.first().copied().unwrap_or(false);
                        let b = inputs.get(1).copied().unwrap_or(false);
                        let cin = inputs.get(2).copied().unwrap_or(false);
                        (a && b) || (cin && (a ^ b)) // carry out
                    }
                    _ => false, // Other multi-output cells don't need special handling
                };

                if let Some(&carry_idx) = cell.outputs.get(1) {
                    let old_carry = self.cpu_net_values.get(carry_idx).copied().unwrap_or(false);
                    if old_carry != carry {
                        changes += 1;
                    }
                    if let Some(val) = self.cpu_net_values.get_mut(carry_idx) {
                        *val = carry;
                    }
                }
            }
        }

        changes
    }

    /// Run until stable (no signal changes)
    pub fn run_until_stable(&mut self, max_iterations: u32) -> u32 {
        let mut iterations = 0u32;
        let mut last_changes = 0;

        loop {
            let changes = self.iterate();
            iterations += 1;
            last_changes = changes;

            if changes == 0 {
                break;
            }

            if iterations >= max_iterations {
                eprintln!(
                    "[GPU_NCL] Warning: Max iterations ({}) reached with {} changes",
                    max_iterations, changes
                );
                break;
            }
        }

        self.is_stable = last_changes == 0;
        iterations
    }

    /// Check if the last simulation run converged
    pub fn is_stable(&self) -> bool {
        self.is_stable
    }

    /// Identify cells that changed frequently (oscillating) during simulation
    /// Returns (cell_path, cell_type, change_count)
    pub fn identify_oscillating_cells(&mut self, iterations: u32) -> Vec<(String, String, u32)> {
        let mut change_counts = vec![0u32; self.cells.len()];
        let mut current_nets = self.cpu_net_values.clone();
        let mut current_states = self.cpu_gate_states.clone();

        for _ in 0..iterations {
            let mut next_nets = current_nets.clone();
            let mut next_states = current_states.clone();

            for (cell_idx, cell) in self.cells.iter().enumerate() {
                let inputs: Vec<bool> = cell
                    .inputs
                    .iter()
                    .map(|&idx| current_nets.get(idx).copied().unwrap_or(false))
                    .collect();

                let (result, new_state) =
                    self.evaluate_cell_with_state(cell, &inputs, &current_states);

                if let Some(&out_idx) = cell.outputs.first() {
                    let old_value = current_nets.get(out_idx).copied().unwrap_or(false);
                    if old_value != result {
                        change_counts[cell_idx] += 1;
                    }
                    if out_idx < next_nets.len() {
                        next_nets[out_idx] = result;
                    }
                }

                if cell.state_index >= 0 {
                    let state_idx = cell.state_index as usize;
                    if state_idx < next_states.len() {
                        next_states[state_idx] = new_state;
                    }
                }
            }

            current_nets = next_nets;
            current_states = next_states;
        }

        // Collect cells that changed more than expected
        let mut oscillating: Vec<(String, String, u32)> = change_counts
            .iter()
            .enumerate()
            .filter(|(_, &count)| count > 2)
            .map(|(idx, &count)| {
                let cell = &self.cells[idx];
                let path = self
                    .netlist
                    .cells
                    .get(idx)
                    .map(|c| c.path.clone())
                    .unwrap_or_default();
                let cell_type = self
                    .netlist
                    .cells
                    .get(idx)
                    .map(|c| c.cell_type.clone())
                    .unwrap_or_default();
                (path, cell_type, count)
            })
            .collect();

        oscillating.sort_by(|a, b| b.2.cmp(&a.2));
        oscillating
    }

    /// Helper to evaluate a single cell with state tracking
    fn evaluate_cell_with_state(
        &self,
        cell: &NclCellInfo,
        inputs: &[bool],
        gate_states: &[bool],
    ) -> (bool, bool) {
        match cell.ptype {
            NclPrimitiveType::And2 => {
                let result = inputs.first().copied().unwrap_or(false)
                    && inputs.get(1).copied().unwrap_or(false);
                (result, false)
            }
            NclPrimitiveType::And3 => (inputs.iter().take(3).all(|&x| x), false),
            NclPrimitiveType::And4 => (inputs.iter().take(4).all(|&x| x), false),
            NclPrimitiveType::Or2 => {
                let result = inputs.first().copied().unwrap_or(false)
                    || inputs.get(1).copied().unwrap_or(false);
                (result, false)
            }
            NclPrimitiveType::Or3 => (inputs.iter().take(3).any(|&x| x), false),
            NclPrimitiveType::Or4 => (inputs.iter().take(4).any(|&x| x), false),
            NclPrimitiveType::Xor => {
                let result = inputs.iter().fold(false, |acc, &x| acc ^ x);
                (result, false)
            }
            NclPrimitiveType::Inv => (!inputs.first().copied().unwrap_or(false), false),
            NclPrimitiveType::Buf => (inputs.first().copied().unwrap_or(false), false),
            NclPrimitiveType::Const0 => (false, false),
            NclPrimitiveType::Const1 => (true, false),
            NclPrimitiveType::Th12 => {
                // TH12 is OR with hysteresis
                let any_high = inputs.iter().any(|&x| x);
                let all_low = inputs.iter().all(|&x| !x);
                let prev = gate_states
                    .get(cell.state_index as usize)
                    .copied()
                    .unwrap_or(false);
                let result = if any_high {
                    true
                } else if all_low {
                    false
                } else {
                    prev
                };
                (result, result)
            }
            NclPrimitiveType::Th22 | NclPrimitiveType::Th33 | NclPrimitiveType::Th44 => {
                // C-element: all high -> 1, all low -> 0, else hold
                let all_high = inputs.iter().all(|&x| x);
                let all_low = inputs.iter().all(|&x| !x);
                let prev = gate_states
                    .get(cell.state_index as usize)
                    .copied()
                    .unwrap_or(false);
                let result = if all_high {
                    true
                } else if all_low {
                    false
                } else {
                    prev
                };
                (result, result)
            }
            _ => (inputs.first().copied().unwrap_or(false), false),
        }
    }

    /// Reset all state
    pub fn reset(&mut self) {
        self.cpu_net_values.fill(false);
        self.cpu_gate_states.fill(false);

        if self.use_gpu {
            // Clear signal buffers
            for b in [&self.signal_buffer_a, &self.signal_buffer_b]
                .into_iter()
                .flatten()
            {
                unsafe {
                    let ptr = b.contents() as *mut u8;
                    std::ptr::write_bytes(ptr, 0, self.num_nets * std::mem::size_of::<u32>());
                }
            }

            // Clear gate state buffer
            if let Some(ref buf) = self.gate_state_buffer {
                unsafe {
                    let ptr = buf.contents() as *mut u8;
                    std::ptr::write_bytes(
                        ptr,
                        0,
                        self.num_stateful_gates * std::mem::size_of::<u32>(),
                    );
                }
            }
        }

        self.current_buffer_is_a = true;
    }

    /// Check if GPU is being used
    pub fn is_using_gpu(&self) -> bool {
        self.use_gpu
    }
}

/// Strip bit suffix from signal name
///
/// For multi-bit dual-rail signals, we need to handle patterns like:
/// - `signal[0]` → `signal`
/// - `signal_t0` → `signal`
/// - `signal_t[0]` → `signal` (both suffixes)
fn strip_bit_suffix(name: &str) -> String {
    let mut result = name.to_string();

    // Step 1: Strip [N] suffix if present
    if let Some(bracket_pos) = result.find('[') {
        result = result[..bracket_pos].to_string();
    }

    // Step 2: Strip _t or _f suffix for dual-rail signals
    // Handle both `_t` and `_tN` patterns
    if let Some(underscore_pos) = result.rfind('_') {
        let suffix = &result[underscore_pos + 1..];
        // Check for bare _t or _f
        if suffix == "t" || suffix == "f" {
            result = result[..underscore_pos].to_string();
        }
        // Check for _tN or _fN where N is digits
        else if (suffix.starts_with('t') || suffix.starts_with('f'))
            && suffix[1..].chars().all(|c| c.is_ascii_digit())
        {
            result = result[..underscore_pos].to_string();
        }
    }

    result
}

/// Parse THmn pattern from cell type string
fn parse_thmn(cell_type: &str) -> Option<(usize, usize)> {
    let upper = cell_type.to_uppercase();
    if let Some(digits) = upper.strip_prefix("TH") {
        if digits.len() >= 2 {
            let m = digits.chars().next()?.to_digit(10)? as usize;
            let n = digits.chars().nth(1)?.to_digit(10)? as usize;
            return Some((m, n));
        }
    }
    None
}

// Stub implementation for non-macOS platforms
#[cfg(not(target_os = "macos"))]
pub struct GpuNclRuntime;

#[cfg(not(target_os = "macos"))]
impl GpuNclRuntime {
    pub fn new(_netlist: GateNetlist) -> Result<Self, String> {
        Err("GPU NCL simulation is only available on macOS".to_string())
    }

    pub fn is_stable(&self) -> bool {
        false
    }

    pub fn identify_oscillating_cells(&mut self, _iterations: u32) -> Vec<(String, String, u32)> {
        Vec::new()
    }
}
