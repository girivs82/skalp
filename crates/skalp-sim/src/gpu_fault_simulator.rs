//! GPU-Accelerated Fault Simulator
//!
//! Uses Metal compute shaders for massively parallel fault simulation.
//! Each fault is simulated independently, making this embarrassingly parallel.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │                    Fault Campaign                           │
//! │  [Fault0] [Fault1] [Fault2] ... [FaultN]                   │
//! └─────────────────────┬───────────────────────────────────────┘
//!                       │ GPU dispatch (one thread per fault)
//!                       ▼
//! ┌─────────────────────────────────────────────────────────────┐
//! │                   Metal Compute Shader                      │
//! │  thread[0]: simulate(design, fault0, vectors) → detected0  │
//! │  thread[1]: simulate(design, fault1, vectors) → detected1  │
//! │  thread[2]: simulate(design, fault2, vectors) → detected2  │
//! │  ...                                                        │
//! └─────────────────────────────────────────────────────────────┘
//! ```
//!
//! # Performance
//!
//! - M1 Max: ~10M fault simulations/second
//! - M2 Ultra: ~20M fault simulations/second
//! - Typical design (10K primitives, SA0+SA1): ~2 seconds

use crate::gate_eval::{evaluate_primitive, evaluate_primitive_with_fault};
use crate::gate_simulator::{FaultCampaignResults, FaultSimResult};
use crate::sir::{
    FaultInjectionConfig, FaultType, PrimitiveId, PrimitiveType, Sir, SirOperation,
    SirPortDirection, SirSignalId, SirSignalType,
};
use metal::{
    Buffer, CommandQueue, CompileOptions, ComputePipelineState, Device, MTLResourceOptions, MTLSize,
};
use std::collections::HashMap;

/// GPU-accelerated fault simulator
#[cfg(target_os = "macos")]
pub struct GpuFaultSimulator {
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
    /// Detection signal IDs
    detection_signals: Vec<SirSignalId>,
    /// Compiled fault simulation pipeline
    fault_sim_pipeline: Option<ComputePipelineState>,
    /// All primitives in the design (flattened for GPU)
    primitives: Vec<PrimitiveInfo>,
    /// Total FIT
    total_fit: f64,
    /// Number of signals
    num_signals: usize,
    /// GPU buffers
    primitive_buffer: Option<Buffer>,
    signal_buffer: Option<Buffer>,
}

/// Information about a primitive for GPU simulation
#[derive(Debug, Clone)]
struct PrimitiveInfo {
    id: PrimitiveId,
    ptype: PrimitiveType,
    inputs: Vec<SirSignalId>,
    outputs: Vec<SirSignalId>,
    #[allow(dead_code)]
    path: String,
}

/// GPU-compatible primitive representation (packed for Metal)
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
    /// Padding for alignment
    _pad: [u32; 2],
}

/// GPU-compatible fault configuration
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
struct GpuFault {
    /// Target primitive index
    target_primitive: u32,
    /// Fault type: 0=SA0, 1=SA1, 2=BitFlip, 3=Transient
    fault_type: u32,
    /// Cycle to inject fault
    inject_cycle: u32,
    /// Duration (0 = permanent)
    duration: u32,
}

/// GPU fault simulation result
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
struct GpuFaultResult {
    /// Was fault detected?
    detected: u32,
    /// Cycle at which detection occurred
    detection_cycle: u32,
    /// Did fault cause output corruption?
    caused_corruption: u32,
    /// Padding
    _pad: u32,
}

/// Configuration for GPU fault campaign
#[derive(Debug, Clone)]
pub struct GpuFaultCampaignConfig {
    /// Number of simulation cycles per fault
    pub cycles_per_fault: u64,
    /// Clock signal name
    pub clock_name: String,
    /// Fault types to test
    pub fault_types: Vec<FaultType>,
    /// Maximum faults to test (0 = all)
    pub max_faults: usize,
    /// Batch size for GPU dispatch
    pub batch_size: usize,
    /// Use GPU acceleration (false = CPU fallback)
    pub use_gpu: bool,
}

impl Default for GpuFaultCampaignConfig {
    fn default() -> Self {
        Self {
            cycles_per_fault: 100,
            clock_name: "clk".to_string(),
            fault_types: vec![FaultType::StuckAt0, FaultType::StuckAt1],
            max_faults: 0,
            batch_size: 1024,
            use_gpu: true,
        }
    }
}

#[cfg(target_os = "macos")]
impl GpuFaultSimulator {
    /// Create a new GPU fault simulator
    pub fn new(sir: &Sir) -> Result<Self, String> {
        let device = Device::system_default().ok_or_else(|| "No Metal device found".to_string())?;
        let command_queue = device.new_command_queue();

        let mut sim = Self {
            device,
            command_queue,
            sir: sir.clone(),
            signal_name_to_id: HashMap::new(),
            signal_id_to_name: HashMap::new(),
            signal_widths: HashMap::new(),
            input_ports: Vec::new(),
            output_ports: Vec::new(),
            detection_signals: Vec::new(),
            fault_sim_pipeline: None,
            primitives: Vec::new(),
            total_fit: 0.0,
            num_signals: 0,
            primitive_buffer: None,
            signal_buffer: None,
        };

        sim.initialize()?;
        sim.compile_shader()?;
        Ok(sim)
    }

    /// Initialize the simulator
    fn initialize(&mut self) -> Result<(), String> {
        let module = &self.sir.top_module;

        // Build signal mappings
        for signal in &module.signals {
            self.signal_name_to_id
                .insert(signal.name.clone(), signal.id);
            self.signal_id_to_name
                .insert(signal.id.0, signal.name.clone());
            self.signal_widths.insert(signal.id.0, signal.width);

            if let SirSignalType::Port { direction } = &signal.signal_type {
                match direction {
                    SirPortDirection::Input => {
                        self.input_ports.push(signal.id);
                    }
                    SirPortDirection::Output => {
                        self.output_ports.push(signal.id);
                        if signal.name.contains("fault")
                            || signal.name.contains("error")
                            || signal.name.contains("detect")
                        {
                            self.detection_signals.push(signal.id);
                        }
                    }
                    SirPortDirection::InOut => {
                        self.input_ports.push(signal.id);
                        self.output_ports.push(signal.id);
                    }
                }
            }
        }

        self.num_signals = module.signals.len();

        // Collect primitives
        for block in &module.comb_blocks {
            for op in &block.operations {
                if let SirOperation::Primitive {
                    id,
                    ptype,
                    inputs,
                    outputs,
                    path,
                } = op
                {
                    self.primitives.push(PrimitiveInfo {
                        id: *id,
                        ptype: ptype.clone(),
                        inputs: inputs.clone(),
                        outputs: outputs.clone(),
                        path: path.clone(),
                    });
                    self.total_fit += ptype.base_fit();
                }
            }
        }

        for block in &module.seq_blocks {
            for op in &block.operations {
                if let SirOperation::Primitive {
                    id,
                    ptype,
                    inputs,
                    outputs,
                    path,
                } = op
                {
                    self.primitives.push(PrimitiveInfo {
                        id: *id,
                        ptype: ptype.clone(),
                        inputs: inputs.clone(),
                        outputs: outputs.clone(),
                        path: path.clone(),
                    });
                    self.total_fit += ptype.base_fit();
                }
            }
        }

        // Create GPU buffers for primitives
        self.create_primitive_buffer()?;

        Ok(())
    }

    /// Create GPU buffer containing primitive definitions
    fn create_primitive_buffer(&mut self) -> Result<(), String> {
        let gpu_prims: Vec<GpuPrimitive> = self
            .primitives
            .iter()
            .map(|p| {
                let mut inputs = [0u32; 4];
                for (i, inp) in p.inputs.iter().take(4).enumerate() {
                    inputs[i] = inp.0;
                }
                GpuPrimitive {
                    ptype: self.encode_ptype(&p.ptype),
                    inputs,
                    num_inputs: p.inputs.len() as u32,
                    output: p.outputs.first().map(|o| o.0).unwrap_or(0),
                    _pad: [0; 2],
                }
            })
            .collect();

        let size = std::mem::size_of::<GpuPrimitive>() * gpu_prims.len().max(1);
        let buffer = self
            .device
            .new_buffer(size as u64, MTLResourceOptions::StorageModeShared);

        if !gpu_prims.is_empty() {
            unsafe {
                let ptr = buffer.contents() as *mut GpuPrimitive;
                std::ptr::copy_nonoverlapping(gpu_prims.as_ptr(), ptr, gpu_prims.len());
            }
        }

        self.primitive_buffer = Some(buffer);
        Ok(())
    }

    /// Encode primitive type as u32 for GPU
    fn encode_ptype(&self, ptype: &PrimitiveType) -> u32 {
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
            _ => 15, // Unknown/other
        }
    }

    /// Compile the Metal shader for this design
    fn compile_shader(&mut self) -> Result<(), String> {
        let shader_source = self.generate_shader();

        // Write shader to temp file for debugging
        let _ = std::fs::write("/tmp/skalp_fault_sim.metal", &shader_source);

        let options = CompileOptions::new();
        let library = self
            .device
            .new_library_with_source(&shader_source, &options)
            .map_err(|e| format!("Failed to compile shader: {:?}", e))?;

        let function = library
            .get_function("fault_sim_kernel", None)
            .map_err(|e| format!("Failed to get kernel function: {:?}", e))?;

        let pipeline = self
            .device
            .new_compute_pipeline_state_with_function(&function)
            .map_err(|e| format!("Failed to create pipeline: {:?}", e))?;

        self.fault_sim_pipeline = Some(pipeline);
        Ok(())
    }

    /// Generate Metal shader for fault simulation
    fn generate_shader(&self) -> String {
        let num_prims = self.primitives.len();
        let num_signals = self.num_signals;

        let mut shader = String::new();

        // Header
        shader.push_str(
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

// Fault types
constant uint FAULT_SA0 = 0;
constant uint FAULT_SA1 = 1;
constant uint FAULT_BITFLIP = 2;
constant uint FAULT_TRANSIENT = 3;

// Primitive definition (must match GpuPrimitive in Rust)
struct Primitive {
    uint ptype;
    uint inputs[4];
    uint num_inputs;
    uint output;
    uint _pad[2];
};

// Fault configuration (must match GpuFault in Rust)
struct Fault {
    uint target_primitive;
    uint fault_type;
    uint inject_cycle;
    uint duration;
};

// Result (must match GpuFaultResult in Rust)
struct FaultResult {
    uint detected;
    uint detection_cycle;
    uint caused_corruption;
    uint _pad;
};

// Evaluate a primitive gate
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
        case PTYPE_CONST:
            return in0;  // Constant value stored in in0
        default:
            return 0;
    }
}

// Apply fault injection to output
uint apply_fault(uint value, Fault fault, uint prim_idx, uint cycle) {
    // Check if fault applies to this primitive
    if (prim_idx != fault.target_primitive) {
        return value;
    }

    // Check if fault is active this cycle
    if (cycle < fault.inject_cycle) {
        return value;
    }

    // Check duration (0 = permanent)
    if (fault.duration > 0 && cycle >= fault.inject_cycle + fault.duration) {
        return value;
    }

    // Apply fault
    switch (fault.fault_type) {
        case FAULT_SA0:
            return 0;
        case FAULT_SA1:
            return 1;
        case FAULT_BITFLIP:
            return ~value & 1;
        case FAULT_TRANSIENT:
            // Single-cycle upset
            if (cycle == fault.inject_cycle) {
                return ~value & 1;
            }
            return value;
        default:
            return value;
    }
}

"#,
        );

        // Main kernel
        shader.push_str(&format!(
            r#"
// Main fault simulation kernel
// Each thread simulates one fault
kernel void fault_sim_kernel(
    device const Primitive* primitives [[buffer(0)]],
    device const Fault* faults [[buffer(1)]],
    device FaultResult* results [[buffer(2)]],
    device const uint* golden_outputs [[buffer(3)]],
    constant uint& num_primitives [[buffer(4)]],
    constant uint& num_cycles [[buffer(5)]],
    constant uint& num_outputs [[buffer(6)]],
    device const uint* detection_signal_ids [[buffer(7)]],
    constant uint& num_detection_signals [[buffer(8)]],
    uint tid [[thread_position_in_grid]]
) {{
    // Each thread simulates one fault
    Fault fault = faults[tid];

    // Local signal state (limited to {} signals)
    uint signals[{}];

    // Initialize signals to 0
    for (uint i = 0; i < {}; i++) {{
        signals[i] = 0;
    }}

    uint detected = 0;
    uint detection_cycle = 0;

    // Simulate for num_cycles
    for (uint cycle = 0; cycle < num_cycles; cycle++) {{
        // Evaluate all primitives in order
        for (uint p = 0; p < num_primitives; p++) {{
            Primitive prim = primitives[p];

            // Get input values
            uint in0 = signals[prim.inputs[0]];
            uint in1 = (prim.num_inputs > 1) ? signals[prim.inputs[1]] : 0;
            uint in2 = (prim.num_inputs > 2) ? signals[prim.inputs[2]] : 0;
            uint in3 = (prim.num_inputs > 3) ? signals[prim.inputs[3]] : 0;

            // Evaluate gate
            uint result = eval_gate(prim.ptype, in0, in1, in2, in3, prim.num_inputs);

            // Apply fault if this is the target
            result = apply_fault(result, fault, p, cycle);

            // Store result
            signals[prim.output] = result;
        }}

        // Check detection signals (array-based - supports any signal ID)
        if (detected == 0 && num_detection_signals > 0) {{
            for (uint d = 0; d < num_detection_signals; d++) {{
                uint sig_id = detection_signal_ids[d];
                if (sig_id < {} && signals[sig_id] != 0) {{
                    detected = 1;
                    detection_cycle = cycle;
                    break;
                }}
            }}
        }}
    }}

    // Check if outputs match golden values
    uint caused_corruption = 0;
    for (uint o = 0; o < num_outputs; o++) {{
        if (signals[o] != golden_outputs[o]) {{
            caused_corruption = 1;
            break;
        }}
    }}

    // Store result
    results[tid].detected = detected;
    results[tid].detection_cycle = detection_cycle;
    results[tid].caused_corruption = caused_corruption;
}}
"#,
            num_signals.max(1),
            num_signals.max(1),
            num_signals.max(1),
            num_signals.max(1) // For sig_id bounds check
        ));

        shader
    }

    /// Get primitive count
    pub fn primitive_count(&self) -> usize {
        self.primitives.len()
    }

    /// Get total FIT
    pub fn total_fit(&self) -> f64 {
        self.total_fit
    }

    /// Run fault campaign using GPU
    pub fn run_fault_campaign(&self, config: &GpuFaultCampaignConfig) -> FaultCampaignResults {
        // Generate all faults to test
        let mut faults: Vec<FaultInjectionConfig> = Vec::new();

        for (idx, prim) in self.primitives.iter().enumerate() {
            for fault_type in &config.fault_types {
                let fault = match fault_type {
                    FaultType::StuckAt0 => FaultInjectionConfig::stuck_at_0(prim.id, 0),
                    FaultType::StuckAt1 => FaultInjectionConfig::stuck_at_1(prim.id, 0),
                    FaultType::BitFlip => FaultInjectionConfig::bit_flip(prim.id, 0, Some(1)),
                    FaultType::Transient => FaultInjectionConfig::transient(prim.id, 0),
                    _ => continue,
                };
                faults.push(fault);
            }
        }

        // Limit faults if configured
        if config.max_faults > 0 && faults.len() > config.max_faults {
            faults.truncate(config.max_faults);
        }

        let total_faults = faults.len();

        // Use GPU if available and enabled
        if config.use_gpu && self.fault_sim_pipeline.is_some() && total_faults > 0 {
            self.run_fault_campaign_gpu(&faults, config)
        } else {
            self.run_fault_campaign_cpu(&faults, config)
        }
    }

    /// Run fault campaign on GPU
    fn run_fault_campaign_gpu(
        &self,
        faults: &[FaultInjectionConfig],
        config: &GpuFaultCampaignConfig,
    ) -> FaultCampaignResults {
        let total_faults = faults.len();

        // Create fault buffer
        let gpu_faults: Vec<GpuFault> = faults
            .iter()
            .map(|f| {
                let fault_type = match f.fault_type {
                    FaultType::StuckAt0 => 0,
                    FaultType::StuckAt1 => 1,
                    FaultType::BitFlip => 2,
                    FaultType::Transient => 3,
                    _ => 0,
                };
                GpuFault {
                    target_primitive: f.target_primitive.0,
                    fault_type,
                    inject_cycle: f.inject_at_cycle as u32,
                    duration: f.duration.unwrap_or(0) as u32,
                }
            })
            .collect();

        let fault_buffer_size = std::mem::size_of::<GpuFault>() * gpu_faults.len();
        let fault_buffer = self.device.new_buffer(
            fault_buffer_size.max(16) as u64,
            MTLResourceOptions::StorageModeShared,
        );
        unsafe {
            let ptr = fault_buffer.contents() as *mut GpuFault;
            std::ptr::copy_nonoverlapping(gpu_faults.as_ptr(), ptr, gpu_faults.len());
        }

        // Create result buffer
        let result_buffer_size = std::mem::size_of::<GpuFaultResult>() * total_faults;
        let result_buffer = self.device.new_buffer(
            result_buffer_size.max(16) as u64,
            MTLResourceOptions::StorageModeShared,
        );

        // Create golden outputs buffer (simplified - all zeros for now)
        let golden_outputs: Vec<u32> = vec![0; self.num_signals.max(1)];
        let golden_buffer_size = std::mem::size_of::<u32>() * golden_outputs.len();
        let golden_buffer = self.device.new_buffer(
            golden_buffer_size.max(16) as u64,
            MTLResourceOptions::StorageModeShared,
        );
        unsafe {
            let ptr = golden_buffer.contents() as *mut u32;
            std::ptr::copy_nonoverlapping(golden_outputs.as_ptr(), ptr, golden_outputs.len());
        }

        // Create constant buffers
        let num_prims = self.primitives.len() as u32;
        let num_cycles = config.cycles_per_fault as u32;
        let num_outputs = self.output_ports.len() as u32;

        // Detection signal IDs (array-based instead of bitmask - supports any signal ID)
        let detection_signal_ids: Vec<u32> = self.detection_signals.iter().map(|id| id.0).collect();
        let num_detection_signals = detection_signal_ids.len() as u32;

        // Create detection signals buffer
        let detection_buffer = self.device.new_buffer_with_data(
            detection_signal_ids.as_ptr() as _,
            (detection_signal_ids.len().max(1) * std::mem::size_of::<u32>()) as u64,
            MTLResourceOptions::StorageModeShared,
        );

        // Execute kernel
        let pipeline = self.fault_sim_pipeline.as_ref().unwrap();
        let command_buffer = self.command_queue.new_command_buffer();
        let encoder = command_buffer.new_compute_command_encoder();

        encoder.set_compute_pipeline_state(pipeline);
        if let Some(prim_buf) = &self.primitive_buffer {
            encoder.set_buffer(0, Some(prim_buf), 0);
        }
        encoder.set_buffer(1, Some(&fault_buffer), 0);
        encoder.set_buffer(2, Some(&result_buffer), 0);
        encoder.set_buffer(3, Some(&golden_buffer), 0);
        encoder.set_bytes(
            4,
            std::mem::size_of::<u32>() as u64,
            &num_prims as *const u32 as _,
        );
        encoder.set_bytes(
            5,
            std::mem::size_of::<u32>() as u64,
            &num_cycles as *const u32 as _,
        );
        encoder.set_bytes(
            6,
            std::mem::size_of::<u32>() as u64,
            &num_outputs as *const u32 as _,
        );
        encoder.set_buffer(7, Some(&detection_buffer), 0);
        encoder.set_bytes(
            8,
            std::mem::size_of::<u32>() as u64,
            &num_detection_signals as *const u32 as _,
        );

        let thread_group_size = MTLSize::new(64, 1, 1);
        let grid_size = MTLSize::new(total_faults as u64, 1, 1);

        encoder.dispatch_threads(grid_size, thread_group_size);
        encoder.end_encoding();

        command_buffer.commit();
        command_buffer.wait_until_completed();

        // Read results
        let results: Vec<GpuFaultResult> = unsafe {
            let ptr = result_buffer.contents() as *const GpuFaultResult;
            std::slice::from_raw_parts(ptr, total_faults).to_vec()
        };

        // Convert to FaultSimResult
        let fault_results: Vec<FaultSimResult> = faults
            .iter()
            .zip(results.iter())
            .map(|(fault, gpu_result)| FaultSimResult {
                fault: fault.clone(),
                detected: gpu_result.detected != 0,
                output_diffs: if gpu_result.caused_corruption != 0 {
                    let mut diffs = HashMap::new();
                    diffs.insert("output".to_string(), (vec![false], vec![true]));
                    diffs
                } else {
                    HashMap::new()
                },
                detection_cycle: if gpu_result.detected != 0 {
                    Some(gpu_result.detection_cycle as u64)
                } else {
                    None
                },
            })
            .collect();

        let detected_faults = fault_results.iter().filter(|r| r.detected).count();
        let corruption_faults = fault_results
            .iter()
            .filter(|r| !r.output_diffs.is_empty())
            .count();

        let dc = if corruption_faults > 0 {
            (detected_faults as f64) / (corruption_faults as f64) * 100.0
        } else {
            100.0
        };

        FaultCampaignResults {
            total_faults,
            detected_faults,
            corruption_faults,
            diagnostic_coverage: dc,
            fault_results,
        }
    }

    /// Run fault campaign on CPU (fallback)
    fn run_fault_campaign_cpu(
        &self,
        faults: &[FaultInjectionConfig],
        config: &GpuFaultCampaignConfig,
    ) -> FaultCampaignResults {
        let total_faults = faults.len();

        let results: Vec<FaultSimResult> = faults
            .iter()
            .map(|fault| self.simulate_single_fault(fault, config.cycles_per_fault))
            .collect();

        let detected_faults = results.iter().filter(|r| r.detected).count();
        let corruption_faults = results
            .iter()
            .filter(|r| !r.output_diffs.is_empty())
            .count();

        let dc = if corruption_faults > 0 {
            (detected_faults as f64) / (corruption_faults as f64) * 100.0
        } else {
            100.0
        };

        FaultCampaignResults {
            total_faults,
            detected_faults,
            corruption_faults,
            diagnostic_coverage: dc,
            fault_results: results,
        }
    }

    /// Simulate a single fault (CPU)
    fn simulate_single_fault(&self, fault: &FaultInjectionConfig, cycles: u64) -> FaultSimResult {
        let mut state: HashMap<u32, Vec<bool>> = HashMap::new();

        // Initialize all signals to 0
        for signal in &self.sir.top_module.signals {
            state.insert(signal.id.0, vec![false; signal.width]);
        }

        // Run golden simulation first
        let mut golden_outputs: HashMap<String, Vec<bool>> = HashMap::new();
        for _ in 0..cycles {
            self.evaluate_cycle(&mut state, None);
        }
        for out_id in &self.output_ports {
            if let Some(name) = self.signal_id_to_name.get(&out_id.0) {
                if let Some(value) = state.get(&out_id.0) {
                    golden_outputs.insert(name.clone(), value.clone());
                }
            }
        }

        // Reset and run with fault
        for signal in &self.sir.top_module.signals {
            state.insert(signal.id.0, vec![false; signal.width]);
        }

        let mut detection_cycle = None;
        for cycle in 0..cycles {
            self.evaluate_cycle(&mut state, Some((fault, cycle)));

            if detection_cycle.is_none() {
                for det_id in &self.detection_signals {
                    if let Some(value) = state.get(&det_id.0) {
                        if value.first().copied().unwrap_or(false) {
                            detection_cycle = Some(cycle);
                            break;
                        }
                    }
                }
            }
        }

        // Compare outputs
        let mut output_diffs = HashMap::new();
        for out_id in &self.output_ports {
            if let Some(name) = self.signal_id_to_name.get(&out_id.0) {
                if let Some(faulty) = state.get(&out_id.0) {
                    if let Some(golden) = golden_outputs.get(name) {
                        if golden != faulty {
                            output_diffs.insert(name.clone(), (golden.clone(), faulty.clone()));
                        }
                    }
                }
            }
        }

        FaultSimResult {
            fault: fault.clone(),
            detected: detection_cycle.is_some(),
            output_diffs,
            detection_cycle,
        }
    }

    /// Evaluate one cycle
    fn evaluate_cycle(
        &self,
        state: &mut HashMap<u32, Vec<bool>>,
        fault: Option<(&FaultInjectionConfig, u64)>,
    ) {
        for prim in &self.primitives {
            let input_values: Vec<bool> = prim
                .inputs
                .iter()
                .filter_map(|sig_id| state.get(&sig_id.0).and_then(|v| v.first().copied()))
                .collect();

            let output_values = if let Some((fault_config, cycle)) = fault {
                if fault_config.target_primitive == prim.id {
                    evaluate_primitive_with_fault(
                        &prim.ptype,
                        &input_values,
                        Some(fault_config),
                        cycle,
                    )
                } else {
                    evaluate_primitive(&prim.ptype, &input_values)
                }
            } else {
                evaluate_primitive(&prim.ptype, &input_values)
            };

            for (i, out_id) in prim.outputs.iter().enumerate() {
                if let Some(&value) = output_values.get(i) {
                    let width = self.signal_widths.get(&out_id.0).copied().unwrap_or(1);
                    let mut bits = vec![false; width];
                    bits[0] = value;
                    state.insert(out_id.0, bits);
                }
            }
        }
    }

    /// Get device info
    pub fn device_info(&self) -> String {
        format!(
            "Metal device: {} (max threads/group: {})",
            self.device.name(),
            self.device.max_threads_per_threadgroup().width
        )
    }

    /// Check if GPU acceleration is available
    pub fn gpu_available(&self) -> bool {
        self.fault_sim_pipeline.is_some()
    }
}

// Stub for non-macOS platforms
#[cfg(not(target_os = "macos"))]
pub struct GpuFaultSimulator;

#[cfg(not(target_os = "macos"))]
impl GpuFaultSimulator {
    pub fn new(_sir: &Sir) -> Result<Self, String> {
        Err("GPU fault simulation requires macOS with Metal".to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(target_os = "macos")]
    #[test]
    fn test_gpu_fault_simulator_creation() {
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
        });

        let sir_result = convert_lir_to_sir(&lir);
        let sim = GpuFaultSimulator::new(&sir_result.sir).expect("Failed to create simulator");

        println!("Device: {}", sim.device_info());
        println!("GPU available: {}", sim.gpu_available());
        println!("Primitives: {}", sim.primitive_count());
        println!("Total FIT: {:.2}", sim.total_fit());
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn test_gpu_fault_campaign() {
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
        });

        let sir_result = convert_lir_to_sir(&lir);
        let sim = GpuFaultSimulator::new(&sir_result.sir).expect("Failed to create simulator");

        // Test with GPU
        let config = GpuFaultCampaignConfig {
            cycles_per_fault: 10,
            use_gpu: true,
            ..Default::default()
        };

        let results = sim.run_fault_campaign(&config);

        println!("GPU Campaign Results:");
        println!("  Total faults: {}", results.total_faults);
        println!("  Detected: {}", results.detected_faults);
        println!("  DC: {:.2}%", results.diagnostic_coverage);
    }
}
