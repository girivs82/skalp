//! GPU-Accelerated AIG Cone Simulator
//!
//! Uses Metal compute shaders to evaluate AIG cones with random patterns.
//! Each thread evaluates one random input/latch assignment on the cone.
//!
//! # Architecture
//!
//! ```text
//! ┌──────────────────────────────────────────────────────┐
//! │              AIG Cone Evaluation                      │
//! │  thread[0]: random inputs → evaluate → check diff    │
//! │  thread[1]: random inputs → evaluate → check diff    │
//! │  ...                                                  │
//! │  thread[N]: random inputs → evaluate → check diff    │
//! └──────────────────────────────────────────────────────┘
//! ```
//!
//! Used by the EC pipeline to verify SAT-unresolved diff gates (typically
//! multiplier/accumulator cones that are SAT-hard).

/// A cone of logic extracted from a miter AIG for a single diff gate.
///
/// Contains only the AND gates, inputs, and latches reachable from the diff gate.
/// All node IDs are remapped to a compact [0..N) range for efficient GPU evaluation.
#[derive(Debug, Clone)]
pub struct AigCone {
    /// Name of the diff gate this cone was extracted for
    pub name: String,

    /// AND gates in topological order.
    /// Each entry: (output_node, left_node, left_inverted, right_node, right_inverted)
    pub and_gates: Vec<(u32, u32, bool, u32, bool)>,

    /// Primary input node IDs in the cone (compact IDs)
    pub input_indices: Vec<u32>,

    /// Latch node IDs in the cone (compact IDs)
    pub latch_indices: Vec<u32>,

    /// State input → latch pairs in the cone (state_input_compact_id, latch_compact_id)
    pub state_input_map: Vec<(u32, u32)>,

    /// Diff gate output node (compact ID)
    pub output_node: u32,

    /// Whether the diff gate output is inverted
    pub output_inverted: bool,

    /// Total number of nodes in the compacted cone (for signal array sizing)
    pub num_nodes: u32,
}

#[cfg(target_os = "macos")]
mod gpu_impl {
    use super::AigCone;
    use metal::{
        CommandQueue, CompileOptions, Device, MTLResourceOptions, MTLSize,
    };

    /// GPU-accelerated AIG cone evaluator using Metal compute shaders.
    pub struct GpuAigConeSim {
        device: Device,
        command_queue: CommandQueue,
    }

    impl GpuAigConeSim {
        /// Create a new GPU cone simulator.
        pub fn new() -> Result<Self, String> {
            let device =
                Device::system_default().ok_or_else(|| "No Metal device found".to_string())?;
            let command_queue = device.new_command_queue();
            Ok(Self {
                device,
                command_queue,
            })
        }

        /// Generate the Metal shader source for a given cone.
        fn generate_shader(cone: &AigCone) -> String {
            let num_nodes = cone.num_nodes;
            let num_and_gates = cone.and_gates.len();
            let num_inputs = cone.input_indices.len();
            let num_latches = cone.latch_indices.len();
            let num_state_pairs = cone.state_input_map.len();
            let output_node = cone.output_node;
            let output_inverted = if cone.output_inverted { 1 } else { 0 };

            let mut shader = String::with_capacity(4096);

            shader.push_str(
                r#"#include <metal_stdlib>
using namespace metal;

kernel void eval_aig_cone(
    device uint* results [[buffer(0)]],
    constant uint& seed [[buffer(1)]],
    uint tid [[thread_position_in_grid]]
) {
    // Per-thread PRNG (LCG)
    uint rng = tid * 1103515245u + seed;
"#,
            );

            // Declare local signal array
            shader.push_str(&format!(
                "    // {} nodes in cone\n    uint signals[{}];\n    signals[0] = 0;\n\n",
                num_nodes, num_nodes
            ));

            // Randomize inputs
            shader.push_str("    // Randomize inputs\n");
            for &input_id in &cone.input_indices {
                shader.push_str(&format!(
                    "    rng = rng * 1103515245u + 12345u; signals[{}] = (rng >> 16u) & 1u;\n",
                    input_id
                ));
            }

            // Randomize latches
            shader.push_str("\n    // Randomize latches\n");
            for &latch_id in &cone.latch_indices {
                shader.push_str(&format!(
                    "    rng = rng * 1103515245u + 12345u; signals[{}] = (rng >> 16u) & 1u;\n",
                    latch_id
                ));
            }

            // Copy latch values to state inputs
            if !cone.state_input_map.is_empty() {
                shader.push_str("\n    // Copy latch values to state inputs\n");
                for &(si, latch) in &cone.state_input_map {
                    shader.push_str(&format!(
                        "    signals[{}] = signals[{}];\n",
                        si, latch
                    ));
                }
            }

            // Evaluate AND gates in topological order (inlined for max perf)
            shader.push_str("\n    // Evaluate AND gates\n");
            for &(out, left, l_inv, right, r_inv) in &cone.and_gates {
                let l_xor = if l_inv { " ^ 1u" } else { "" };
                let r_xor = if r_inv { " ^ 1u" } else { "" };
                shader.push_str(&format!(
                    "    signals[{}] = (signals[{}]{}) & (signals[{}]{});\n",
                    out, left, l_xor, right, r_xor
                ));
            }

            // Check diff gate output
            let out_xor = if cone.output_inverted { " ^ 1u" } else { "" };
            shader.push_str(&format!(
                "\n    // Check diff gate\n    results[tid] = signals[{}]{};\n}}\n",
                output_node, out_xor
            ));

            shader
        }

        /// Evaluate a cone with random patterns on the GPU.
        ///
        /// Returns `true` if all patterns pass (no counterexample found).
        /// Returns `false` if any pattern finds a counterexample (diff gate = 1).
        pub fn evaluate_cone(&self, cone: &AigCone, num_patterns: u32) -> Result<bool, String> {
            if cone.and_gates.is_empty() && cone.input_indices.is_empty() {
                // Trivial cone — evaluate directly
                return Ok(!cone.output_inverted || cone.output_node == 0);
            }

            // Generate and compile shader
            let shader_src = Self::generate_shader(cone);
            let options = CompileOptions::new();
            let library = self
                .device
                .new_library_with_source(&shader_src, &options)
                .map_err(|e| format!("Metal shader compile error: {}", e))?;
            let function = library
                .get_function("eval_aig_cone", None)
                .map_err(|e| format!("Metal function not found: {}", e))?;
            let pipeline = self
                .device
                .new_compute_pipeline_state_with_function(&function)
                .map_err(|e| format!("Metal pipeline error: {}", e))?;

            // Create results buffer
            let result_size = (num_patterns as u64) * std::mem::size_of::<u32>() as u64;
            let result_buffer = self
                .device
                .new_buffer(result_size, MTLResourceOptions::StorageModeShared);

            // Seed from system time for variety across calls
            let seed: u32 = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .subsec_nanos();

            // Dispatch
            let command_buffer = self.command_queue.new_command_buffer();
            let encoder = command_buffer.new_compute_command_encoder();
            encoder.set_compute_pipeline_state(&pipeline);
            encoder.set_buffer(0, Some(&result_buffer), 0);
            encoder.set_bytes(
                1,
                std::mem::size_of::<u32>() as u64,
                &seed as *const u32 as *const _,
            );

            let thread_group_size = MTLSize::new(64, 1, 1);
            let grid_size = MTLSize::new(num_patterns as u64, 1, 1);
            encoder.dispatch_threads(grid_size, thread_group_size);
            encoder.end_encoding();

            command_buffer.commit();
            command_buffer.wait_until_completed();

            // Read back results
            let has_counterexample = unsafe {
                let ptr = result_buffer.contents() as *const u32;
                let results = std::slice::from_raw_parts(ptr, num_patterns as usize);
                results.iter().any(|&v| v != 0)
            };

            Ok(!has_counterexample)
        }

        /// Evaluate multiple cones, returning names of cones that found counterexamples.
        pub fn evaluate_cones(
            &self,
            cones: &[AigCone],
            num_patterns: u32,
        ) -> Result<Vec<String>, String> {
            let mut failures = Vec::new();
            for cone in cones {
                match self.evaluate_cone(cone, num_patterns) {
                    Ok(true) => {
                        // All patterns passed for this cone
                    }
                    Ok(false) => {
                        failures.push(cone.name.clone());
                    }
                    Err(e) => {
                        return Err(format!(
                            "GPU cone sim error on '{}': {}",
                            cone.name, e
                        ));
                    }
                }
            }
            Ok(failures)
        }
    }
}

#[cfg(target_os = "macos")]
pub use gpu_impl::GpuAigConeSim;

/// CPU fallback for non-macOS platforms or when GPU is unavailable.
/// Uses the same random pattern evaluation but on CPU.
pub fn evaluate_cone_cpu(cone: &AigCone, num_patterns: u32) -> bool {
    let num_nodes = cone.num_nodes as usize;
    let mut signals = vec![0u32; num_nodes];

    // Simple LCG PRNG
    let base_seed: u32 = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .subsec_nanos();

    for pattern in 0..num_patterns {
        let mut rng = pattern.wrapping_mul(1103515245).wrapping_add(base_seed);

        // Node 0 = constant false
        signals[0] = 0;

        // Randomize inputs
        for &input_id in &cone.input_indices {
            rng = rng.wrapping_mul(1103515245).wrapping_add(12345);
            signals[input_id as usize] = (rng >> 16) & 1;
        }

        // Randomize latches
        for &latch_id in &cone.latch_indices {
            rng = rng.wrapping_mul(1103515245).wrapping_add(12345);
            signals[latch_id as usize] = (rng >> 16) & 1;
        }

        // Copy latch values to state inputs
        for &(si, latch) in &cone.state_input_map {
            signals[si as usize] = signals[latch as usize];
        }

        // Evaluate AND gates
        for &(out, left, l_inv, right, r_inv) in &cone.and_gates {
            let l = signals[left as usize] ^ (l_inv as u32);
            let r = signals[right as usize] ^ (r_inv as u32);
            signals[out as usize] = l & r;
        }

        // Check diff gate
        let diff = signals[cone.output_node as usize] ^ (cone.output_inverted as u32);
        if diff != 0 {
            return false; // Counterexample found
        }
    }

    true // All patterns passed
}
