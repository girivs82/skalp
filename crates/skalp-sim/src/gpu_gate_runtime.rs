//! GPU-Accelerated Gate-Level Runtime
//!
//! Provides GPU acceleration for regular gate-level simulation (without fault injection).
//! Uses Metal compute shaders for parallel gate evaluation.
//!
//! # Architecture
//!
//! Unlike the fault simulator which runs one fault per thread, this runtime:
//! - Evaluates all combinational gates in parallel in each cycle
//! - Updates sequential elements on clock edges
//! - Maintains signal state in GPU buffers
//!
//! # Usage
//!
//! ```ignore
//! let runtime = GpuGateRuntime::new(&sir)?;
//! runtime.set_input("a", 5);
//! runtime.step();
//! let result = runtime.get_output("y");
//! ```

use crate::gate_eval::evaluate_primitive;
use crate::sir::{
    PrimitiveId, PrimitiveType, Sir, SirOperation, SirPortDirection, SirSignalId, SirSignalType,
};
use std::collections::HashMap;

#[cfg(target_os = "macos")]
use metal::{
    Buffer, CommandQueue, CompileOptions, ComputePipelineState, Device, MTLResourceOptions, MTLSize,
};

/// GPU-accelerated gate-level runtime
#[cfg(target_os = "macos")]
pub struct GpuGateRuntime {
    /// Metal device
    device: Device,
    /// Command queue
    command_queue: CommandQueue,
    /// The SIR being simulated
    sir: Sir,
    /// Signal name to ID mapping
    signal_name_to_id: HashMap<String, SirSignalId>,
    /// Signal ID to name mapping
    signal_id_to_name: HashMap<u32, String>,
    /// Signal widths
    signal_widths: HashMap<u32, usize>,
    /// Input port IDs
    input_ports: Vec<SirSignalId>,
    /// Output port IDs
    output_ports: Vec<SirSignalId>,
    /// Clock signal IDs
    clock_signals: Vec<SirSignalId>,
    /// All primitives in topological order
    primitives: Vec<PrimitiveInfo>,
    /// Number of signals
    num_signals: usize,
    /// Current cycle
    current_cycle: u64,
    /// Previous clock values for edge detection
    prev_clocks: HashMap<u32, bool>,

    // GPU buffers
    /// Primitive definitions buffer
    primitive_buffer: Option<Buffer>,
    /// Signal values buffer (double-buffered for read/write)
    signal_buffer_a: Option<Buffer>,
    signal_buffer_b: Option<Buffer>,
    /// Which buffer is current (true = A, false = B)
    current_buffer_is_a: bool,

    // Compute pipelines
    /// Pipeline for combinational evaluation
    comb_pipeline: Option<ComputePipelineState>,
    /// Pipeline for sequential update
    seq_pipeline: Option<ComputePipelineState>,

    /// Whether to use GPU (falls back to CPU if compilation fails)
    use_gpu: bool,

    /// CPU fallback state (signal values)
    cpu_signals: HashMap<u32, Vec<bool>>,
}

/// Information about a primitive for GPU simulation
#[derive(Debug, Clone)]
struct PrimitiveInfo {
    id: PrimitiveId,
    ptype: PrimitiveType,
    inputs: Vec<SirSignalId>,
    outputs: Vec<SirSignalId>,
    is_sequential: bool,
    clock: Option<SirSignalId>,
}

/// GPU-compatible primitive representation
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
struct GpuPrimitive {
    /// Primitive type encoded as u32
    ptype: u32,
    /// Input signal indices (up to 4)
    inputs: [u32; 4],
    /// Number of inputs
    num_inputs: u32,
    /// Output signal index
    output: u32,
    /// Is sequential (DFF)?
    is_sequential: u32,
    /// Clock signal index (for sequential)
    clock: u32,
}

#[cfg(target_os = "macos")]
impl GpuGateRuntime {
    /// Create a new GPU gate runtime from SIR
    pub fn new(sir: &Sir) -> Result<Self, String> {
        let device = Device::system_default().ok_or_else(|| "No Metal device found".to_string())?;
        let command_queue = device.new_command_queue();

        let mut runtime = Self {
            device,
            command_queue,
            sir: sir.clone(),
            signal_name_to_id: HashMap::new(),
            signal_id_to_name: HashMap::new(),
            signal_widths: HashMap::new(),
            input_ports: Vec::new(),
            output_ports: Vec::new(),
            clock_signals: Vec::new(),
            primitives: Vec::new(),
            num_signals: 0,
            current_cycle: 0,
            prev_clocks: HashMap::new(),
            primitive_buffer: None,
            signal_buffer_a: None,
            signal_buffer_b: None,
            current_buffer_is_a: true,
            comb_pipeline: None,
            seq_pipeline: None,
            use_gpu: true,
            cpu_signals: HashMap::new(),
        };

        runtime.initialize()?;

        // Try to compile GPU shaders, fall back to CPU if it fails
        if let Err(e) = runtime.compile_shaders() {
            eprintln!("GPU shader compilation failed, falling back to CPU: {}", e);
            runtime.use_gpu = false;
        }

        Ok(runtime)
    }

    /// Initialize the runtime from SIR
    fn initialize(&mut self) -> Result<(), String> {
        let module = &self.sir.top_module;

        // Build signal mappings
        for signal in &module.signals {
            self.signal_name_to_id
                .insert(signal.name.clone(), signal.id);
            self.signal_id_to_name
                .insert(signal.id.0, signal.name.clone());
            self.signal_widths.insert(signal.id.0, signal.width);

            // Initialize CPU fallback signals
            self.cpu_signals
                .insert(signal.id.0, vec![false; signal.width]);

            // Categorize signals
            match &signal.signal_type {
                SirSignalType::Port { direction } => match direction {
                    SirPortDirection::Input => {
                        self.input_ports.push(signal.id);
                        if signal.name.contains("clk") || signal.name.contains("clock") {
                            self.clock_signals.push(signal.id);
                            self.prev_clocks.insert(signal.id.0, false);
                        }
                    }
                    SirPortDirection::Output => {
                        self.output_ports.push(signal.id);
                    }
                    SirPortDirection::InOut => {
                        self.input_ports.push(signal.id);
                        self.output_ports.push(signal.id);
                    }
                },
                SirSignalType::Register { clock, .. } => {
                    self.prev_clocks.insert(clock.0, false);
                }
                SirSignalType::Wire => {}
            }
        }

        self.num_signals = module.signals.len();

        // Collect primitives from combinational blocks
        for block in &module.comb_blocks {
            for op in &block.operations {
                if let SirOperation::Primitive {
                    id,
                    ptype,
                    inputs,
                    outputs,
                    ..
                } = op
                {
                    self.primitives.push(PrimitiveInfo {
                        id: *id,
                        ptype: ptype.clone(),
                        inputs: inputs.clone(),
                        outputs: outputs.clone(),
                        is_sequential: false,
                        clock: None,
                    });
                }
            }
        }

        // Collect primitives from sequential blocks
        for block in &module.seq_blocks {
            for op in &block.operations {
                if let SirOperation::Primitive {
                    id,
                    ptype,
                    inputs,
                    outputs,
                    ..
                } = op
                {
                    self.primitives.push(PrimitiveInfo {
                        id: *id,
                        ptype: ptype.clone(),
                        inputs: inputs.clone(),
                        outputs: outputs.clone(),
                        is_sequential: true,
                        clock: Some(block.clock),
                    });
                }
            }
        }

        // Create GPU buffers
        self.create_buffers()?;

        Ok(())
    }

    /// Create GPU buffers
    fn create_buffers(&mut self) -> Result<(), String> {
        // Primitive buffer
        let gpu_prims: Vec<GpuPrimitive> = self
            .primitives
            .iter()
            .map(|p| {
                let mut inputs = [0u32; 4];
                for (i, inp) in p.inputs.iter().take(4).enumerate() {
                    inputs[i] = inp.0;
                }
                GpuPrimitive {
                    ptype: encode_ptype(&p.ptype),
                    inputs,
                    num_inputs: p.inputs.len() as u32,
                    output: p.outputs.first().map(|o| o.0).unwrap_or(0),
                    is_sequential: if p.is_sequential { 1 } else { 0 },
                    clock: p.clock.map(|c| c.0).unwrap_or(0),
                }
            })
            .collect();

        if !gpu_prims.is_empty() {
            let size = std::mem::size_of::<GpuPrimitive>() * gpu_prims.len();
            let buffer = self
                .device
                .new_buffer(size as u64, MTLResourceOptions::StorageModeShared);

            unsafe {
                let ptr = buffer.contents() as *mut GpuPrimitive;
                std::ptr::copy_nonoverlapping(gpu_prims.as_ptr(), ptr, gpu_prims.len());
            }
            self.primitive_buffer = Some(buffer);
        }

        // Signal buffers (double-buffered)
        let signal_size = std::mem::size_of::<u32>() * self.num_signals.max(1);
        self.signal_buffer_a = Some(
            self.device
                .new_buffer(signal_size as u64, MTLResourceOptions::StorageModeShared),
        );
        self.signal_buffer_b = Some(
            self.device
                .new_buffer(signal_size as u64, MTLResourceOptions::StorageModeShared),
        );

        // Initialize signal buffers to 0
        if let Some(buf) = &self.signal_buffer_a {
            unsafe {
                let ptr = buf.contents() as *mut u32;
                std::ptr::write_bytes(ptr, 0, self.num_signals.max(1));
            }
        }
        if let Some(buf) = &self.signal_buffer_b {
            unsafe {
                let ptr = buf.contents() as *mut u32;
                std::ptr::write_bytes(ptr, 0, self.num_signals.max(1));
            }
        }

        Ok(())
    }

    /// Compile Metal shaders
    fn compile_shaders(&mut self) -> Result<(), String> {
        let shader_source = self.generate_shader();

        // Write shader to temp file for debugging
        let _ = std::fs::write("/tmp/skalp_gate_runtime.metal", &shader_source);

        let options = CompileOptions::new();
        let library = self
            .device
            .new_library_with_source(&shader_source, &options)
            .map_err(|e| format!("Failed to compile shader: {:?}", e))?;

        // Combinational evaluation pipeline
        let comb_fn = library
            .get_function("eval_combinational", None)
            .map_err(|e| format!("Failed to get eval_combinational: {:?}", e))?;
        self.comb_pipeline = Some(
            self.device
                .new_compute_pipeline_state_with_function(&comb_fn)
                .map_err(|e| format!("Failed to create comb pipeline: {:?}", e))?,
        );

        // Sequential update pipeline
        let seq_fn = library
            .get_function("eval_sequential", None)
            .map_err(|e| format!("Failed to get eval_sequential: {:?}", e))?;
        self.seq_pipeline = Some(
            self.device
                .new_compute_pipeline_state_with_function(&seq_fn)
                .map_err(|e| format!("Failed to create seq pipeline: {:?}", e))?,
        );

        Ok(())
    }

    /// Generate Metal shader source
    fn generate_shader(&self) -> String {
        r#"
#include <metal_stdlib>
using namespace metal;

// Primitive types
constant uint PTYPE_AND = 0;
constant uint PTYPE_OR = 1;
constant uint PTYPE_XOR = 2;
constant uint PTYPE_NAND = 3;
constant uint PTYPE_NOR = 4;
constant uint PTYPE_XNOR = 5;
constant uint PTYPE_INV = 6;
constant uint PTYPE_BUF = 7;
constant uint PTYPE_MUX2 = 8;
constant uint PTYPE_MUX4 = 9;
constant uint PTYPE_DFF_P = 10;
constant uint PTYPE_DFF_N = 11;
constant uint PTYPE_HALF_ADDER = 12;
constant uint PTYPE_FULL_ADDER = 13;
constant uint PTYPE_CONST = 14;

struct Primitive {
    uint ptype;
    uint inputs[4];
    uint num_inputs;
    uint output;
    uint is_sequential;
    uint clock;
};

// Evaluate a combinational gate
uint eval_gate(uint ptype, uint in0, uint in1, uint in2, uint in3, uint num_inputs) {
    switch (ptype) {
        case PTYPE_AND:
            if (num_inputs == 2) return in0 & in1;
            if (num_inputs == 3) return in0 & in1 & in2;
            return in0 & in1 & in2 & in3;
        case PTYPE_OR:
            if (num_inputs == 2) return in0 | in1;
            if (num_inputs == 3) return in0 | in1 | in2;
            return in0 | in1 | in2 | in3;
        case PTYPE_XOR:
            return in0 ^ in1;
        case PTYPE_NAND:
            if (num_inputs == 2) return ~(in0 & in1) & 1;
            return ~(in0 & in1 & in2 & in3) & 1;
        case PTYPE_NOR:
            if (num_inputs == 2) return ~(in0 | in1) & 1;
            return ~(in0 | in1 | in2 | in3) & 1;
        case PTYPE_XNOR:
            return ~(in0 ^ in1) & 1;
        case PTYPE_INV:
            return ~in0 & 1;
        case PTYPE_BUF:
            return in0;
        case PTYPE_MUX2:
            return (in2 == 0) ? in0 : in1;  // sel=in2
        case PTYPE_HALF_ADDER:
            // Returns sum in bit 0, carry in bit 1
            return (in0 ^ in1) | ((in0 & in1) << 1);
        case PTYPE_FULL_ADDER:
            // Returns sum in bit 0, carry in bit 1
            {
                uint sum = in0 ^ in1 ^ in2;
                uint carry = (in0 & in1) | (in1 & in2) | (in0 & in2);
                return sum | (carry << 1);
            }
        case PTYPE_CONST:
            return in0;
        default:
            return 0;
    }
}

// Kernel: Evaluate all combinational primitives
// Each thread evaluates one primitive
kernel void eval_combinational(
    device const Primitive* primitives [[buffer(0)]],
    device const uint* signals_in [[buffer(1)]],
    device uint* signals_out [[buffer(2)]],
    constant uint& num_primitives [[buffer(3)]],
    uint tid [[thread_position_in_grid]]
) {
    if (tid >= num_primitives) return;

    Primitive prim = primitives[tid];

    // Skip sequential primitives in this kernel
    if (prim.is_sequential != 0) return;

    // Get input values
    uint in0 = signals_in[prim.inputs[0]];
    uint in1 = (prim.num_inputs > 1) ? signals_in[prim.inputs[1]] : 0;
    uint in2 = (prim.num_inputs > 2) ? signals_in[prim.inputs[2]] : 0;
    uint in3 = (prim.num_inputs > 3) ? signals_in[prim.inputs[3]] : 0;

    // Evaluate gate
    uint result = eval_gate(prim.ptype, in0, in1, in2, in3, prim.num_inputs);

    // Store result
    signals_out[prim.output] = result;
}

// Kernel: Evaluate sequential primitives on clock edge
kernel void eval_sequential(
    device const Primitive* primitives [[buffer(0)]],
    device const uint* signals_in [[buffer(1)]],
    device uint* signals_out [[buffer(2)]],
    constant uint& num_primitives [[buffer(3)]],
    constant uint& clock_mask [[buffer(4)]],  // Bitmask of clocks with rising edge
    uint tid [[thread_position_in_grid]]
) {
    if (tid >= num_primitives) return;

    Primitive prim = primitives[tid];

    // Only process sequential primitives
    if (prim.is_sequential == 0) return;

    // Check if our clock has a rising edge
    if (((clock_mask >> prim.clock) & 1) == 0) return;

    // DFF: output = D input (inputs[1] is D, inputs[0] is clk)
    if (prim.ptype == PTYPE_DFF_P || prim.ptype == PTYPE_DFF_N) {
        uint d_input = signals_in[prim.inputs[1]];
        signals_out[prim.output] = d_input;
    }
}
"#
        .to_string()
    }

    /// Set an input signal value
    pub fn set_input(&mut self, name: &str, value: &[bool]) {
        if let Some(id) = self.signal_name_to_id.get(name) {
            let width = self.signal_widths.get(&id.0).copied().unwrap_or(1);
            let mut padded = value.to_vec();
            padded.resize(width, false);

            // Update CPU state
            self.cpu_signals.insert(id.0, padded.clone());

            // Update GPU buffer
            if self.use_gpu {
                let val = padded.first().copied().unwrap_or(false) as u32;
                let buffer = if self.current_buffer_is_a {
                    &self.signal_buffer_a
                } else {
                    &self.signal_buffer_b
                };
                if let Some(buf) = buffer {
                    unsafe {
                        let ptr = buf.contents() as *mut u32;
                        *ptr.add(id.0 as usize) = val;
                    }
                }
            }
        }
    }

    /// Set an input signal value from u64
    pub fn set_input_u64(&mut self, name: &str, value: u64) {
        // Try direct lookup
        if let Some(id) = self.signal_name_to_id.get(name) {
            let width = self.signal_widths.get(&id.0).copied().unwrap_or(1);
            let bits: Vec<bool> = (0..width).map(|i| (value >> i) & 1 == 1).collect();
            self.cpu_signals.insert(id.0, bits.clone());

            if self.use_gpu {
                let val = bits.first().copied().unwrap_or(false) as u32;
                let buffer = if self.current_buffer_is_a {
                    &self.signal_buffer_a
                } else {
                    &self.signal_buffer_b
                };
                if let Some(buf) = buffer {
                    unsafe {
                        let ptr = buf.contents() as *mut u32;
                        *ptr.add(id.0 as usize) = val;
                    }
                }
            }
            return;
        }

        // Try bit-indexed signals
        let mut bit_idx = 0;
        while let Some(id) = self
            .signal_name_to_id
            .get(&format!("{}[{}]", name, bit_idx))
        {
            let bit_value = (value >> bit_idx) & 1 == 1;
            self.cpu_signals.insert(id.0, vec![bit_value]);

            if self.use_gpu {
                let buffer = if self.current_buffer_is_a {
                    &self.signal_buffer_a
                } else {
                    &self.signal_buffer_b
                };
                if let Some(buf) = buffer {
                    unsafe {
                        let ptr = buf.contents() as *mut u32;
                        *ptr.add(id.0 as usize) = bit_value as u32;
                    }
                }
            }
            bit_idx += 1;
        }
    }

    /// Get an output signal value
    pub fn get_output(&self, name: &str) -> Option<Vec<bool>> {
        if self.use_gpu {
            self.get_signal_from_gpu(name)
        } else {
            self.signal_name_to_id
                .get(name)
                .and_then(|id| self.cpu_signals.get(&id.0).cloned())
        }
    }

    /// Get signal value from GPU buffer
    fn get_signal_from_gpu(&self, name: &str) -> Option<Vec<bool>> {
        let id = self.signal_name_to_id.get(name)?;
        let buffer = if self.current_buffer_is_a {
            &self.signal_buffer_a
        } else {
            &self.signal_buffer_b
        };
        buffer.as_ref().map(|buf| {
            let val = unsafe {
                let ptr = buf.contents() as *const u32;
                *ptr.add(id.0 as usize)
            };
            vec![val != 0]
        })
    }

    /// Get an output signal value as u64
    pub fn get_output_u64(&self, name: &str) -> Option<u64> {
        // Try direct lookup
        if let Some(bits) = self.get_output(name) {
            return Some(
                bits.iter()
                    .enumerate()
                    .fold(0u64, |acc, (i, &b)| acc | ((b as u64) << i)),
            );
        }

        // Try bit-indexed signals
        let mut result = 0u64;
        let mut bit_idx = 0;
        let mut found_any = false;

        while self
            .signal_name_to_id
            .contains_key(&format!("{}[{}]", name, bit_idx))
        {
            found_any = true;
            if let Some(bits) = self.get_output(&format!("{}[{}]", name, bit_idx)) {
                if bits.first().copied().unwrap_or(false) {
                    result |= 1u64 << bit_idx;
                }
            }
            bit_idx += 1;
        }

        if found_any {
            Some(result)
        } else {
            None
        }
    }

    /// Get any signal value by name
    pub fn get_signal(&self, name: &str) -> Option<Vec<bool>> {
        self.get_output(name)
    }

    /// Step simulation by one cycle
    pub fn step(&mut self) {
        if self.use_gpu {
            self.step_gpu();
        } else {
            self.step_cpu();
        }
        self.current_cycle += 1;
    }

    /// Step simulation on GPU
    fn step_gpu(&mut self) {
        let (src_buf, dst_buf) = if self.current_buffer_is_a {
            (&self.signal_buffer_a, &self.signal_buffer_b)
        } else {
            (&self.signal_buffer_b, &self.signal_buffer_a)
        };

        let Some(prim_buf) = &self.primitive_buffer else {
            return;
        };
        let Some(src) = src_buf else { return };
        let Some(dst) = dst_buf else { return };

        // Copy current state to destination (for signals not modified by primitives)
        unsafe {
            let src_ptr = src.contents() as *const u32;
            let dst_ptr = dst.contents() as *mut u32;
            std::ptr::copy_nonoverlapping(src_ptr, dst_ptr, self.num_signals.max(1));
        }

        // Detect clock edges
        let clock_mask = self.detect_clock_edges_gpu();

        // Evaluate combinational logic
        if let Some(pipeline) = &self.comb_pipeline {
            let command_buffer = self.command_queue.new_command_buffer();
            let encoder = command_buffer.new_compute_command_encoder();

            encoder.set_compute_pipeline_state(pipeline);
            encoder.set_buffer(0, Some(prim_buf), 0);
            encoder.set_buffer(1, Some(src), 0);
            encoder.set_buffer(2, Some(dst), 0);

            let num_prims = self.primitives.len() as u32;
            encoder.set_bytes(
                3,
                std::mem::size_of::<u32>() as u64,
                &num_prims as *const u32 as _,
            );

            let thread_group_size = MTLSize::new(64, 1, 1);
            let grid_size = MTLSize::new(self.primitives.len().max(1) as u64, 1, 1);

            encoder.dispatch_threads(grid_size, thread_group_size);
            encoder.end_encoding();

            command_buffer.commit();
            command_buffer.wait_until_completed();
        }

        // Evaluate sequential logic on clock edges
        if clock_mask != 0 {
            if let Some(pipeline) = &self.seq_pipeline {
                let command_buffer = self.command_queue.new_command_buffer();
                let encoder = command_buffer.new_compute_command_encoder();

                encoder.set_compute_pipeline_state(pipeline);
                encoder.set_buffer(0, Some(prim_buf), 0);
                encoder.set_buffer(1, Some(dst), 0); // Read from dst (post-comb)
                encoder.set_buffer(2, Some(dst), 0); // Write to same dst

                let num_prims = self.primitives.len() as u32;
                encoder.set_bytes(
                    3,
                    std::mem::size_of::<u32>() as u64,
                    &num_prims as *const u32 as _,
                );
                encoder.set_bytes(
                    4,
                    std::mem::size_of::<u32>() as u64,
                    &clock_mask as *const u32 as _,
                );

                let thread_group_size = MTLSize::new(64, 1, 1);
                let grid_size = MTLSize::new(self.primitives.len().max(1) as u64, 1, 1);

                encoder.dispatch_threads(grid_size, thread_group_size);
                encoder.end_encoding();

                command_buffer.commit();
                command_buffer.wait_until_completed();
            }
        }

        // Swap buffers
        self.current_buffer_is_a = !self.current_buffer_is_a;

        // Update previous clock values
        self.update_prev_clocks_gpu();
    }

    /// Detect clock edges (returns bitmask)
    fn detect_clock_edges_gpu(&self) -> u32 {
        let mut mask = 0u32;
        let buffer = if self.current_buffer_is_a {
            &self.signal_buffer_a
        } else {
            &self.signal_buffer_b
        };

        if let Some(buf) = buffer {
            for clock_id in &self.clock_signals {
                let prev = self.prev_clocks.get(&clock_id.0).copied().unwrap_or(false);
                let curr = unsafe {
                    let ptr = buf.contents() as *const u32;
                    *ptr.add(clock_id.0 as usize) != 0
                };

                // Rising edge
                if !prev && curr {
                    mask |= 1 << clock_id.0;
                }
            }
        }

        mask
    }

    /// Update previous clock values from GPU buffer
    fn update_prev_clocks_gpu(&mut self) {
        let buffer = if self.current_buffer_is_a {
            &self.signal_buffer_a
        } else {
            &self.signal_buffer_b
        };

        if let Some(buf) = buffer {
            for clock_id in &self.clock_signals {
                let curr = unsafe {
                    let ptr = buf.contents() as *const u32;
                    *ptr.add(clock_id.0 as usize) != 0
                };
                self.prev_clocks.insert(clock_id.0, curr);
            }
        }
    }

    /// Step simulation on CPU (fallback)
    fn step_cpu(&mut self) {
        // Detect clock edges
        let mut rising_edges = Vec::new();
        for clock_id in &self.clock_signals {
            let prev = self.prev_clocks.get(&clock_id.0).copied().unwrap_or(false);
            let curr = self
                .cpu_signals
                .get(&clock_id.0)
                .and_then(|v| v.first().copied())
                .unwrap_or(false);

            if !prev && curr {
                rising_edges.push(*clock_id);
            }
        }

        // Evaluate combinational primitives
        for prim in &self.primitives.clone() {
            if prim.is_sequential {
                continue;
            }

            let input_values: Vec<bool> = prim
                .inputs
                .iter()
                .filter_map(|sig_id| {
                    self.cpu_signals
                        .get(&sig_id.0)
                        .and_then(|v| v.first().copied())
                })
                .collect();

            let output_values = evaluate_primitive(&prim.ptype, &input_values);

            for (i, out_id) in prim.outputs.iter().enumerate() {
                if let Some(&value) = output_values.get(i) {
                    let width = self.signal_widths.get(&out_id.0).copied().unwrap_or(1);
                    let mut bits = vec![false; width];
                    bits[0] = value;
                    self.cpu_signals.insert(out_id.0, bits);
                }
            }
        }

        // Evaluate sequential primitives on clock edges
        for prim in &self.primitives.clone() {
            if !prim.is_sequential {
                continue;
            }

            let clock_id = match prim.clock {
                Some(c) => c,
                None => continue,
            };

            if !rising_edges.contains(&clock_id) {
                continue;
            }

            let input_values: Vec<bool> = prim
                .inputs
                .iter()
                .filter_map(|sig_id| {
                    self.cpu_signals
                        .get(&sig_id.0)
                        .and_then(|v| v.first().copied())
                })
                .collect();

            let output_values = evaluate_primitive(&prim.ptype, &input_values);

            for (i, out_id) in prim.outputs.iter().enumerate() {
                if let Some(&value) = output_values.get(i) {
                    let width = self.signal_widths.get(&out_id.0).copied().unwrap_or(1);
                    let mut bits = vec![false; width];
                    bits[0] = value;
                    self.cpu_signals.insert(out_id.0, bits);
                }
            }
        }

        // Update previous clock values
        for clock_id in &self.clock_signals {
            if let Some(bits) = self.cpu_signals.get(&clock_id.0) {
                let curr = bits.first().copied().unwrap_or(false);
                self.prev_clocks.insert(clock_id.0, curr);
            }
        }
    }

    /// Reset the runtime
    pub fn reset(&mut self) {
        self.current_cycle = 0;
        self.prev_clocks.clear();

        // Reset CPU signals
        for signal in &self.sir.top_module.signals {
            self.cpu_signals
                .insert(signal.id.0, vec![false; signal.width]);
        }

        // Reset GPU buffers
        if let Some(buf) = &self.signal_buffer_a {
            unsafe {
                let ptr = buf.contents() as *mut u32;
                std::ptr::write_bytes(ptr, 0, self.num_signals.max(1));
            }
        }
        if let Some(buf) = &self.signal_buffer_b {
            unsafe {
                let ptr = buf.contents() as *mut u32;
                std::ptr::write_bytes(ptr, 0, self.num_signals.max(1));
            }
        }

        // Reset clock state
        for clock_id in &self.clock_signals {
            self.prev_clocks.insert(clock_id.0, false);
        }
    }

    /// Get current cycle
    pub fn cycle(&self) -> u64 {
        self.current_cycle
    }

    /// Check if GPU is being used
    pub fn is_using_gpu(&self) -> bool {
        self.use_gpu
    }

    /// Get device info
    pub fn device_info(&self) -> String {
        format!(
            "Metal device: {} (GPU enabled: {})",
            self.device.name(),
            self.use_gpu
        )
    }

    /// Get device name
    pub fn device_name(&self) -> String {
        self.device.name().to_string()
    }

    /// Get names of all input ports
    pub fn get_input_names(&self) -> Vec<String> {
        self.input_ports
            .iter()
            .filter_map(|id| self.signal_id_to_name.get(&id.0).cloned())
            .collect()
    }

    /// Get names of all output ports
    pub fn get_output_names(&self) -> Vec<String> {
        self.output_ports
            .iter()
            .filter_map(|id| self.signal_id_to_name.get(&id.0).cloned())
            .collect()
    }

    /// Dump all signals (for debugging)
    pub fn dump_signals(&self) -> Vec<(String, Vec<bool>)> {
        let mut result: Vec<_> = self
            .signal_name_to_id
            .iter()
            .filter_map(|(name, id)| self.get_output(name).map(|v| (name.clone(), v)))
            .collect();
        result.sort_by(|a, b| a.0.cmp(&b.0));
        result
    }
}

/// Encode primitive type for GPU
fn encode_ptype(ptype: &PrimitiveType) -> u32 {
    match ptype {
        PrimitiveType::And { .. } => 0,
        PrimitiveType::Or { .. } => 1,
        PrimitiveType::Xor => 2,
        PrimitiveType::Nand { .. } => 3,
        PrimitiveType::Nor { .. } => 4,
        PrimitiveType::Xnor => 5,
        PrimitiveType::Inv => 6,
        PrimitiveType::Buf => 7,
        PrimitiveType::Mux2 => 8,
        PrimitiveType::Mux4 => 9,
        PrimitiveType::DffP => 10,
        PrimitiveType::DffN => 11,
        PrimitiveType::HalfAdder => 12,
        PrimitiveType::FullAdder => 13,
        PrimitiveType::Constant { .. } => 14,
        _ => 15,
    }
}

// Stub for non-macOS platforms
#[cfg(not(target_os = "macos"))]
pub struct GpuGateRuntime;

#[cfg(not(target_os = "macos"))]
impl GpuGateRuntime {
    pub fn new(_sir: &crate::sir::Sir) -> Result<Self, String> {
        Err("GPU gate runtime requires macOS with Metal".to_string())
    }
}

#[cfg(test)]
mod tests {
    // Tests temporarily disabled - they used legacy LIR types (Lir, LirNet, Primitive)
    // that have been removed during the GateNetlist migration.
    #![allow(dead_code, unused_imports)]
    use super::*;

    /*
    #[cfg(target_os = "macos")]
    #[test]
    fn test_gpu_gate_runtime_creation() {
        use crate::lir_to_sir::convert_lir_to_sir;
        use skalp_lir::lir::{Lir, LirNet, NetId, Primitive};

        let mut lir = Lir::new("test".to_string());

        let net_a = lir.add_net(LirNet {
            id: NetId(0),
            name: "a".to_string(),
            driver: None,
            loads: vec![],
            is_primary_input: true,
            is_primary_output: false,
            is_state_output: false,
            is_detection: false,
            detection_config: None,
            width: 1,
        });

        let net_b = lir.add_net(LirNet {
            id: NetId(1),
            name: "b".to_string(),
            driver: None,
            loads: vec![],
            is_primary_input: true,
            is_primary_output: false,
            is_state_output: false,
            is_detection: false,
            detection_config: None,
            width: 1,
        });

        let net_y = lir.add_net(LirNet {
            id: NetId(2),
            name: "y".to_string(),
            driver: None,
            loads: vec![],
            is_primary_input: false,
            is_primary_output: true,
            is_state_output: false,
            is_detection: false,
            detection_config: None,
            width: 1,
        });

        lir.add_primitive(Primitive {
            id: PrimitiveId(0),
            ptype: PrimitiveType::And { inputs: 2 },
            path: "and_0".to_string(),
            inputs: vec![net_a, net_b],
            outputs: vec![net_y],
            clock: None,
            reset: None,
            enable: None,
            bit_index: None,
            safety_info: None,
            power_domain: None,
        });

        let sir_result = convert_lir_to_sir(&lir);
        let runtime = GpuGateRuntime::new(&sir_result.sir);

        assert!(runtime.is_ok());
        let rt = runtime.unwrap();
        println!("Device info: {}", rt.device_info());
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn test_gpu_gate_runtime_and_gate() {
        use crate::lir_to_sir::convert_lir_to_sir;
        use skalp_lir::lir::{Lir, LirNet, NetId, Primitive};

        let mut lir = Lir::new("test".to_string());

        let net_a = lir.add_net(LirNet {
            id: NetId(0),
            name: "a".to_string(),
            driver: None,
            loads: vec![],
            is_primary_input: true,
            is_primary_output: false,
            is_state_output: false,
            is_detection: false,
            detection_config: None,
            width: 1,
        });

        let net_b = lir.add_net(LirNet {
            id: NetId(1),
            name: "b".to_string(),
            driver: None,
            loads: vec![],
            is_primary_input: true,
            is_primary_output: false,
            is_state_output: false,
            is_detection: false,
            detection_config: None,
            width: 1,
        });

        let net_y = lir.add_net(LirNet {
            id: NetId(2),
            name: "y".to_string(),
            driver: None,
            loads: vec![],
            is_primary_input: false,
            is_primary_output: true,
            is_state_output: false,
            is_detection: false,
            detection_config: None,
            width: 1,
        });

        lir.add_primitive(Primitive {
            id: PrimitiveId(0),
            ptype: PrimitiveType::And { inputs: 2 },
            path: "and_0".to_string(),
            inputs: vec![net_a, net_b],
            outputs: vec![net_y],
            clock: None,
            reset: None,
            enable: None,
            bit_index: None,
            safety_info: None,
            power_domain: None,
        });

        let sir_result = convert_lir_to_sir(&lir);
        let mut rt = GpuGateRuntime::new(&sir_result.sir).unwrap();

        // Test AND(1,1) = 1
        rt.set_input("a", &[true]);
        rt.set_input("b", &[true]);
        rt.step();
        assert_eq!(rt.get_output("y"), Some(vec![true]));

        // Test AND(1,0) = 0
        rt.set_input("a", &[true]);
        rt.set_input("b", &[false]);
        rt.step();
        assert_eq!(rt.get_output("y"), Some(vec![false]));
    }
    */
}
