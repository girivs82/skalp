//! Metal Shading Language Backend
//!
//! This module provides a thin wrapper around SharedCodegen that adds Metal-specific
//! headers, kernel signatures, and address space qualifiers.
//!
//! # Metal-Specific Syntax
//!
//! - Address spaces: `device`, `constant`, `thread`
//! - Kernel entry: `kernel void func_name(...) [[buffer(N)]]`
//! - Thread ID: `uint tid [[thread_position_in_grid]]`
//! - Types: `uint`, `uint2`, `uint4`, `half`, `float`

use crate::sir::SirModule;

use super::shared::SharedCodegen;
use super::types::BackendTarget;

/// Metal backend for GPU shader generation
pub struct MetalBackend<'a> {
    shared: SharedCodegen<'a>,
}

impl<'a> MetalBackend<'a> {
    /// Create a new Metal backend for the given SIR module
    pub fn new(module: &'a SirModule) -> Self {
        Self {
            shared: SharedCodegen::new(module, BackendTarget::Metal),
        }
    }

    /// Generate complete Metal shader source
    pub fn generate(module: &SirModule) -> String {
        let mut backend = MetalBackend::new(module);
        backend.generate_shader()
    }

    /// Generate the complete Metal shader
    fn generate_shader(&mut self) -> String {
        let mut output = String::new();

        // Metal header
        output.push_str("#include <metal_stdlib>\n");
        output.push_str("#include <metal_compute>\n");
        output.push_str("using namespace metal;\n\n");

        // Generate struct definitions using shared codegen
        self.shared.generate_inputs_struct();
        self.shared.generate_registers_struct();
        self.shared.generate_signals_struct();
        output.push_str(&self.shared.take_output());

        // Generate combinational kernel
        output.push_str(&self.generate_combinational_kernel());

        // Generate sequential kernel
        output.push_str(&self.generate_sequential_kernel());

        // Generate batched simulation kernel
        output.push_str(&self.generate_batched_kernel());

        output
    }

    /// Generate the combinational cone kernel
    fn generate_combinational_kernel(&mut self) -> String {
        let mut output = String::new();

        output.push_str("kernel void combinational_cone_0(\n");
        output.push_str("    device const Inputs* inputs [[buffer(0)]],\n");
        output.push_str("    device const Registers* registers [[buffer(1)]],\n");
        output.push_str("    device Signals* signals [[buffer(2)]],\n");
        output.push_str("    uint tid [[thread_position_in_grid]]\n");
        output.push_str(") {\n");

        // Generate combinational body using shared codegen
        self.shared.indent();
        self.shared.generate_combinational_body();
        output.push_str(&self.shared.take_output());

        output.push_str("}\n\n");
        output
    }

    /// Generate the sequential update kernel
    fn generate_sequential_kernel(&mut self) -> String {
        let mut output = String::new();

        output.push_str("kernel void sequential_update(\n");
        output.push_str("    device const Inputs* inputs [[buffer(0)]],\n");
        output.push_str("    device const Registers* current_registers [[buffer(1)]],\n");
        output.push_str("    device const Signals* signals [[buffer(2)]],\n");
        output.push_str("    device Registers* next_registers [[buffer(3)]],\n");
        output.push_str("    uint tid [[thread_position_in_grid]]\n");
        output.push_str(") {\n");

        // Generate sequential body using shared codegen
        self.shared.indent();
        self.shared
            .generate_sequential_body("current_registers", "next_registers");
        output.push_str(&self.shared.take_output());

        output.push_str("}\n\n");
        output
    }

    /// Generate the batched simulation kernel for multi-cycle simulation
    fn generate_batched_kernel(&mut self) -> String {
        let mut output = String::new();

        output.push_str("kernel void batched_simulation(\n");
        output.push_str("    device Inputs* inputs [[buffer(0)]],\n");
        output.push_str("    device Registers* registers [[buffer(1)]],\n");
        output.push_str("    device Signals* signals [[buffer(2)]],\n");
        output.push_str("    constant uint& num_cycles [[buffer(3)]],\n");
        output.push_str("    uint tid [[thread_position_in_grid]]\n");
        output.push_str(") {\n");

        // Generate local variables for scalar state elements
        self.shared.indent();
        self.generate_local_state_copies(&mut output);

        // Main simulation loop
        output.push_str("    for (uint cycle = 0; cycle < num_cycles; cycle++) {\n");

        // Combinational evaluation with local registers
        self.shared.set_batched_mode(true);
        self.shared.indent();
        self.shared.generate_combinational_body();
        output.push_str(&self.shared.take_output());

        // Sequential updates
        self.shared
            .generate_sequential_body("registers", "registers");
        output.push_str(&self.shared.take_output());
        self.shared.set_batched_mode(false);

        output.push_str("    }\n");

        // Copy local state back to registers
        self.generate_local_state_writeback(&mut output);

        // Final combinational pass: propagate updated register values to output signals
        // Without this, output signals are stale by one cycle (reflect pre-update state)
        output.push_str(
            "\n    // Final combinational pass (FWFT: outputs reflect latest register state)\n",
        );
        self.shared.generate_combinational_body();
        output.push_str(&self.shared.take_output());

        self.shared.dedent();
        output.push_str("}\n");
        output
    }

    /// Generate local copies of scalar state elements for batched mode
    fn generate_local_state_copies(&self, output: &mut String) {
        use crate::sir::{SirNodeKind, SirType};
        use std::collections::HashSet;

        let mut sorted_states: Vec<_> = self.shared.module.state_elements.iter().collect();
        sorted_states.sort_by_key(|(name, _)| *name);

        let mut declared_locals: HashSet<String> = HashSet::new();

        for (name, elem) in &sorted_states {
            // Skip array-type state elements (use sir_type or width-based detection)
            let signal_width = self.shared.get_signal_width(name);
            let is_array = if let Some(ref sir_type) = elem.sir_type {
                matches!(sir_type, SirType::Array(_, _))
            } else {
                // Fall back to width-based check
                let (_, array_size) = self.shared.type_mapper.get_type_for_width(signal_width);
                array_size.is_some()
            };

            if is_array {
                continue; // Arrays don't get local copies
            }

            // Only create local copies for scalar registers (≤128 bits)
            if signal_width <= 128 {
                let sanitized = self.shared.sanitize_name(name);
                let (base_type, array_size) =
                    self.shared.type_mapper.get_type_for_width(signal_width);

                if array_size.is_none() {
                    output.push_str(&format!(
                        "    {} local_{} = registers->{};\n",
                        base_type, sanitized, sanitized
                    ));
                    declared_locals.insert(sanitized);
                }
            }
        }

        // BUG #254 FIX: Also declare local variables for FF outputs not in state_elements
        for node in &self.shared.module.sequential_nodes {
            if let SirNodeKind::FlipFlop { .. } = &node.kind {
                for ff_output in &node.outputs {
                    if !self
                        .shared
                        .module
                        .state_elements
                        .contains_key(&ff_output.signal_id)
                    {
                        let sanitized = self.shared.sanitize_name(&ff_output.signal_id);
                        if declared_locals.contains(&sanitized) {
                            continue;
                        }
                        let width = self.shared.get_signal_width(&ff_output.signal_id);
                        if width <= 128 {
                            let (base_type, array_size) =
                                self.shared.type_mapper.get_type_for_width(width);
                            if array_size.is_none() {
                                output.push_str(&format!(
                                    "    {} local_{} = registers->{};\n",
                                    base_type, sanitized, sanitized
                                ));
                                declared_locals.insert(sanitized);
                            }
                        }
                    }
                }
            }
        }

        output.push('\n');
    }

    /// Generate writeback of local state to registers after batched simulation
    fn generate_local_state_writeback(&self, output: &mut String) {
        use crate::sir::{SirNodeKind, SirType};
        use std::collections::HashSet;

        let mut sorted_states: Vec<_> = self.shared.module.state_elements.iter().collect();
        sorted_states.sort_by_key(|(name, _)| *name);

        let mut written_back: HashSet<String> = HashSet::new();

        output.push_str("\n    // Write back local state to registers\n");
        for (name, elem) in &sorted_states {
            // Skip array-type state elements (use sir_type to detect)
            let is_array = if let Some(ref sir_type) = elem.sir_type {
                matches!(sir_type, SirType::Array(_, _))
            } else {
                false
            };

            if is_array {
                continue; // Arrays don't have local copies
            }

            if elem.width <= 128 {
                let sanitized = self.shared.sanitize_name(name);
                let (_, array_size) = self.shared.type_mapper.get_type_for_width(elem.width);

                if array_size.is_none() {
                    output.push_str(&format!(
                        "    registers->{} = local_{};\n",
                        sanitized, sanitized
                    ));
                    written_back.insert(sanitized);
                }
            }
        }

        // BUG #254 FIX: Also write back FF outputs not in state_elements
        for node in &self.shared.module.sequential_nodes {
            if let SirNodeKind::FlipFlop { .. } = &node.kind {
                for ff_output in &node.outputs {
                    if !self
                        .shared
                        .module
                        .state_elements
                        .contains_key(&ff_output.signal_id)
                    {
                        let sanitized = self.shared.sanitize_name(&ff_output.signal_id);
                        if written_back.contains(&sanitized) {
                            continue;
                        }
                        let width = self.shared.get_signal_width(&ff_output.signal_id);
                        if width <= 128 {
                            let (_, array_size) = self.shared.type_mapper.get_type_for_width(width);
                            if array_size.is_none() {
                                output.push_str(&format!(
                                    "    registers->{} = local_{};\n",
                                    sanitized, sanitized
                                ));
                                written_back.insert(sanitized);
                            }
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_metal_backend_creation() {
        // Create a minimal SIR module for testing
        let module = SirModule {
            name: "test".to_string(),
            inputs: vec![],
            outputs: vec![],
            signals: vec![],
            combinational_nodes: vec![],
            sequential_nodes: vec![],
            state_elements: Default::default(),
            clock_domains: Default::default(),
            sorted_combinational_node_ids: vec![],
            pipeline_config: None,
            span: None,
            name_registry: Default::default(),
        };

        let shader = MetalBackend::generate(&module);
        assert!(shader.contains("#include <metal_stdlib>"));
        assert!(shader.contains("kernel void combinational_cone_0"));
        assert!(shader.contains("kernel void sequential_update"));
        assert!(shader.contains("kernel void batched_simulation"));
    }
}
