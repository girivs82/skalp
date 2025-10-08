use crate::simulator::{SimulationError, SimulationResult, SimulationRuntime, SimulationState};
use async_trait::async_trait;
use skalp_sir::SirModule;
use std::collections::HashMap;

pub struct CpuRuntime {
    module: Option<SirModule>,
    inputs: HashMap<String, Vec<u8>>, // Input port values (preserved across steps)
    state: HashMap<String, Vec<u8>>,  // State elements (registers)
    next_state: HashMap<String, Vec<u8>>, // Next state for double-buffering
    outputs: HashMap<String, Vec<u8>>, // Output port values
    signals: HashMap<String, Vec<u8>>, // Internal signal values
    signal_widths: HashMap<String, usize>, // Width in bits for each signal
    prev_clock_values: HashMap<String, u8>, // Previous clock values for edge detection
    current_cycle: u64,
}

impl Default for CpuRuntime {
    fn default() -> Self {
        Self::new()
    }
}

impl CpuRuntime {
    pub fn new() -> Self {
        CpuRuntime {
            module: None,
            inputs: HashMap::new(),
            state: HashMap::new(),
            next_state: HashMap::new(),
            outputs: HashMap::new(),
            signals: HashMap::new(),
            signal_widths: HashMap::new(),
            prev_clock_values: HashMap::new(),
            current_cycle: 0,
        }
    }

    /// Truncate a value to the specified bit width
    fn truncate_to_width(value: &[u8], width_bits: usize) -> Vec<u8> {
        let byte_size = width_bits.div_ceil(8);
        if value.len() <= byte_size {
            // Value already fits within width
            return value.to_vec();
        }

        // Truncate to correct byte size
        let mut result = value[..byte_size].to_vec();

        // Mask off excess bits in the last byte
        let excess_bits = byte_size * 8 - width_bits;
        if excess_bits > 0 && !result.is_empty() {
            let mask = (1u8 << (8 - excess_bits)) - 1;
            *result.last_mut().unwrap() &= mask;
        }

        result
    }

    fn evaluate_combinational(&mut self) -> Result<(), SimulationError> {
        // Clone combinational nodes and outputs to avoid borrow checker issues
        let nodes = if let Some(module) = &self.module {
            module.combinational_nodes.clone()
        } else {
            return Ok(());
        };

        let outputs = if let Some(module) = &self.module {
            module.outputs.clone()
        } else {
            return Ok(());
        };

        // Evaluate all combinational nodes in topological order
        for node in &nodes {
            self.evaluate_node(node)?;
        }

        // Update outputs based on their drivers, truncating to correct width
        for output in &outputs {
            if let Some(value) = self.signals.get(&output.name) {
                // Truncate to the output's declared width
                let byte_size = output.width.div_ceil(8);
                let truncated = if value.len() > byte_size {
                    value[..byte_size].to_vec()
                } else {
                    value.clone()
                };
                self.outputs.insert(output.name.clone(), truncated);
            }
        }

        Ok(())
    }

    fn evaluate_node(&mut self, node: &skalp_sir::SirNode) -> Result<(), SimulationError> {
        use skalp_sir::SirNodeKind;

        // Get input values
        let input_values: Vec<Vec<u8>> = node
            .inputs
            .iter()
            .map(|input| {
                self.signals
                    .get(&input.signal_id)
                    .or_else(|| self.inputs.get(&input.signal_id))
                    .or_else(|| self.state.get(&input.signal_id))
                    .cloned()
                    .unwrap_or_else(|| vec![0u8])
            })
            .collect();

        // Compute output based on node kind
        let output_value = match &node.kind {
            SirNodeKind::Constant { value, width } => {
                let byte_size = width.div_ceil(8);
                let mut bytes = vec![0u8; byte_size];
                for (i, byte) in bytes.iter_mut().enumerate().take(byte_size) {
                    *byte = ((value >> (i * 8)) & 0xFF) as u8;
                }
                bytes
            }

            SirNodeKind::SignalRef { signal } => {
                // SignalRef reads directly from the named signal, not from inputs list
                // First check signals map, then inputs, then state
                self.signals
                    .get(signal)
                    .or_else(|| self.inputs.get(signal))
                    .or_else(|| self.state.get(signal))
                    .cloned()
                    .unwrap_or_else(|| {
                        eprintln!("WARNING: SignalRef couldn't find signal '{}', defaulting to 0", signal);
                        vec![0u8]
                    })
            }

            SirNodeKind::BinaryOp(op) => {
                if input_values.len() >= 2 {
                    Self::evaluate_binary_op(op, &input_values[0], &input_values[1])?
                } else {
                    vec![0u8]
                }
            }

            SirNodeKind::UnaryOp(op) => {
                if !input_values.is_empty() {
                    Self::evaluate_unary_op(op, &input_values[0])?
                } else {
                    vec![0u8]
                }
            }

            SirNodeKind::Mux => {
                // Mux: inputs = [condition, true_value, false_value]
                if input_values.len() >= 3 {
                    let cond = input_values[0][0] != 0;
                    if cond {
                        input_values[1].clone()
                    } else {
                        input_values[2].clone()
                    }
                } else {
                    vec![0u8]
                }
            }

            SirNodeKind::Slice { start, end } => {
                if !input_values.is_empty() {
                    Self::extract_slice(&input_values[0], *start, *end)
                } else {
                    vec![0u8]
                }
            }

            SirNodeKind::Concat => {
                // Concatenate all inputs
                let total_bits: usize = input_values.iter().map(|v| v.len() * 8).sum();
                let byte_size = total_bits.div_ceil(8);
                let mut result = vec![0u8; byte_size];

                let mut bit_offset = 0;
                for value in &input_values {
                    for (byte_idx, &byte_val) in value.iter().enumerate() {
                        let target_byte = (bit_offset + byte_idx * 8) / 8;
                        let target_bit_in_byte = (bit_offset + byte_idx * 8) % 8;

                        if target_byte < result.len() {
                            result[target_byte] |= byte_val << target_bit_in_byte;
                            if target_bit_in_byte > 0 && target_byte + 1 < result.len() {
                                result[target_byte + 1] |= byte_val >> (8 - target_bit_in_byte);
                            }
                        }
                    }
                    bit_offset += value.len() * 8;
                }
                result
            }

            SirNodeKind::ArrayRead => {
                // ArrayRead: inputs = [array_signal, index]
                if input_values.len() >= 2 {
                    // For now, just return zero (array operations need more complex handling)
                    vec![0u8]
                } else {
                    vec![0u8]
                }
            }

            _ => {
                // For other node types (FlipFlop, etc.), don't evaluate here
                vec![0u8]
            }
        };

        // Store output value(s) - a node can have multiple outputs
        // Truncate to correct width based on signal declaration
        for output in &node.outputs {
            let truncated_value = if let Some(&width) = self.signal_widths.get(&output.signal_id) {
                Self::truncate_to_width(&output_value, width)
            } else {
                output_value.clone()
            };
            self.signals
                .insert(output.signal_id.clone(), truncated_value);
        }

        Ok(())
    }

    fn evaluate_binary_op(
        op: &skalp_sir::BinaryOperation,
        left: &[u8],
        right: &[u8],
    ) -> Result<Vec<u8>, SimulationError> {
        use skalp_sir::BinaryOperation;

        // Convert bytes to u64 for computation (little-endian)
        let left_val = Self::bytes_to_u64(left);
        let right_val = Self::bytes_to_u64(right);

        let result_val = match op {
            BinaryOperation::Add => left_val.wrapping_add(right_val),
            BinaryOperation::Sub => left_val.wrapping_sub(right_val),
            BinaryOperation::Mul => left_val.wrapping_mul(right_val),
            BinaryOperation::Div => {
                if right_val == 0 {
                    0
                } else {
                    left_val / right_val
                }
            }
            BinaryOperation::Mod => {
                if right_val == 0 {
                    0
                } else {
                    left_val % right_val
                }
            }
            BinaryOperation::And => left_val & right_val,
            BinaryOperation::Or => left_val | right_val,
            BinaryOperation::Xor => left_val ^ right_val,
            BinaryOperation::Eq => {
                if left_val == right_val {
                    1
                } else {
                    0
                }
            }
            BinaryOperation::Neq => {
                if left_val != right_val {
                    1
                } else {
                    0
                }
            }
            BinaryOperation::Lt => {
                if left_val < right_val {
                    1
                } else {
                    0
                }
            }
            BinaryOperation::Lte => {
                if left_val <= right_val {
                    1
                } else {
                    0
                }
            }
            BinaryOperation::Gt => {
                if left_val > right_val {
                    1
                } else {
                    0
                }
            }
            BinaryOperation::Gte => {
                if left_val >= right_val {
                    1
                } else {
                    0
                }
            }
            BinaryOperation::Shl => left_val << (right_val & 0x3F), // Limit shift amount
            BinaryOperation::Shr => left_val >> (right_val & 0x3F),
        };

        // Determine output size (max of input sizes)
        let output_size = left.len().max(right.len());
        Ok(Self::u64_to_bytes(result_val, output_size))
    }

    fn evaluate_unary_op(
        op: &skalp_sir::UnaryOperation,
        operand: &[u8],
    ) -> Result<Vec<u8>, SimulationError> {
        use skalp_sir::UnaryOperation;

        let operand_val = Self::bytes_to_u64(operand);

        let result_val = match op {
            UnaryOperation::Not => !operand_val,
            UnaryOperation::Neg => operand_val.wrapping_neg(),
            UnaryOperation::RedAnd => {
                // Reduction AND: result is 1 if all bits are 1
                let mask = (1u64 << (operand.len() * 8)) - 1;
                if (operand_val & mask) == mask {
                    1
                } else {
                    0
                }
            }
            UnaryOperation::RedOr => {
                // Reduction OR: result is 1 if any bit is 1
                if operand_val != 0 {
                    1
                } else {
                    0
                }
            }
            UnaryOperation::RedXor => {
                // Reduction XOR: result is parity of all bits
                let mut result = 0;
                let mut val = operand_val;
                while val != 0 {
                    result ^= val & 1;
                    val >>= 1;
                }
                result
            }
        };

        Ok(Self::u64_to_bytes(result_val, operand.len()))
    }

    fn extract_slice(value: &[u8], start: usize, end: usize) -> Vec<u8> {
        let width = end - start + 1;
        let byte_size = width.div_ceil(8);
        let mut result = vec![0u8; byte_size];

        let value_as_u64 = Self::bytes_to_u64(value);
        let mask = if width >= 64 {
            u64::MAX
        } else {
            (1u64 << width) - 1
        };
        let sliced = (value_as_u64 >> start) & mask;

        for (i, byte) in result.iter_mut().enumerate() {
            *byte = ((sliced >> (i * 8)) & 0xFF) as u8;
        }

        result
    }

    fn bytes_to_u64(bytes: &[u8]) -> u64 {
        let mut result = 0u64;
        for (i, &byte) in bytes.iter().enumerate().take(8) {
            result |= (byte as u64) << (i * 8);
        }
        result
    }

    fn u64_to_bytes(value: u64, byte_count: usize) -> Vec<u8> {
        let mut bytes = vec![0u8; byte_count];
        for (i, byte) in bytes.iter_mut().enumerate().take(byte_count) {
            *byte = ((value >> (i * 8)) & 0xFF) as u8;
        }
        bytes
    }

    fn evaluate_sequential(&mut self) -> Result<(), SimulationError> {
        use skalp_sir::ClockEdge;

        // Clone nodes to avoid borrow checker issues
        let seq_nodes = if let Some(module) = &self.module {
            module.sequential_nodes.clone()
        } else {
            return Ok(());
        };

        // Evaluate flip-flops: check for clock edges and update on edge
        for node in &seq_nodes {
            if let skalp_sir::SirNodeKind::FlipFlop { clock_edge } = &node.kind {
                // First input is clock, second is data (D input)
                if node.inputs.len() < 2 {
                    continue;
                }

                let clock_signal = &node.inputs[0].signal_id;
                let d_input = &node.inputs[1].signal_id;

                // Get current clock value
                let current_clock = self
                    .signals
                    .get(clock_signal)
                    .or_else(|| self.inputs.get(clock_signal))
                    .and_then(|v| v.first())
                    .copied()
                    .unwrap_or(0);

                // Get previous clock value
                let prev_clock = self.prev_clock_values.get(clock_signal).copied().unwrap_or(0);

                // Check for the specified edge
                let edge_detected = match clock_edge {
                    ClockEdge::Rising => prev_clock == 0 && current_clock != 0,
                    ClockEdge::Falling => prev_clock != 0 && current_clock == 0,
                    ClockEdge::Both => {
                        (prev_clock == 0 && current_clock != 0) || (prev_clock != 0 && current_clock == 0)
                    }
                };

                // Update previous clock value
                self.prev_clock_values.insert(clock_signal.clone(), current_clock);

                if edge_detected {
                    // Get the D input value
                    let d_value = self
                        .signals
                        .get(d_input)
                        .or_else(|| self.inputs.get(d_input))
                        .or_else(|| self.state.get(d_input))
                        .cloned()
                        .unwrap_or_else(|| vec![0u8]);

                    // Write to the output (which is the register)
                    // Truncate to the correct width
                    if let Some(output) = node.outputs.first() {
                        let truncated = if let Some(&width) = self.signal_widths.get(&output.signal_id) {
                            Self::truncate_to_width(&d_value, width)
                        } else {
                            d_value.clone()
                        };
                        self.next_state.insert(output.signal_id.clone(), truncated);
                    }
                }
            }
        }

        // Swap state buffers
        std::mem::swap(&mut self.state, &mut self.next_state);
        self.next_state.clear();

        // Update signals map with new state values so combinational logic can see them
        for (name, value) in &self.state {
            self.signals.insert(name.clone(), value.clone());
        }

        Ok(())
    }

    fn extract_state(&self) -> SimulationState {
        let mut signals = HashMap::new();
        let mut registers = HashMap::new();

        if let Some(module) = &self.module {
            // Build set of output names to avoid reading them twice (matching GPU behavior)
            let output_names: std::collections::HashSet<_> =
                module.outputs.iter().map(|o| &o.name).collect();

            // Add outputs to signals (matching GPU behavior)
            for output in &module.outputs {
                if let Some(value) = self.outputs.get(&output.name) {
                    signals.insert(output.name.clone(), value.clone());
                }
            }

            // Add non-state intermediate signals, skipping outputs (matching GPU behavior)
            // For inputs that aren't in self.signals (because they're not driven by nodes),
            // add them with zero values to match GPU's uninitialized signal_buffer behavior
            for signal in &module.signals {
                if !signal.is_state && !output_names.contains(&signal.name) {
                    let value = self.signals.get(&signal.name).cloned().unwrap_or_else(|| {
                        // Input signals not driven by nodes get zero value (matching GPU)
                        vec![0u8; signal.width.div_ceil(8)]
                    });
                    signals.insert(signal.name.clone(), value);
                }
            }

            // Add state elements to registers (matching GPU behavior)
            // Only state elements go in registers, not inputs or outputs
            for (name, _) in &module.state_elements {
                if let Some(value) = self.state.get(name) {
                    registers.insert(name.clone(), value.clone());
                }
            }
        }

        SimulationState {
            cycle: self.current_cycle,
            signals,
            registers,
        }
    }

    fn initialize_signals(&mut self, module: &SirModule) {
        // Build set of input names for quick lookup
        let input_names: std::collections::HashSet<_> = module.inputs.iter().map(|i| &i.name).collect();

        // Initialize non-input signals to zero and record their widths
        // Inputs are handled separately and should not be in the signals map
        for signal in &module.signals {
            if !input_names.contains(&signal.name) {
                let byte_size = signal.width.div_ceil(8);
                self.signals
                    .insert(signal.name.clone(), vec![0u8; byte_size]);
            }
            self.signal_widths.insert(signal.name.clone(), signal.width);
        }

        // Initialize input ports (separate from signals)
        for input in &module.inputs {
            let byte_size = input.width.div_ceil(8);
            self.inputs.insert(input.name.clone(), vec![0u8; byte_size]);
            self.signal_widths.insert(input.name.clone(), input.width);
        }

        // Initialize output ports (separate from state)
        for output in &module.outputs {
            let byte_size = output.width.div_ceil(8);
            self.outputs
                .insert(output.name.clone(), vec![0u8; byte_size]);
            self.signal_widths.insert(output.name.clone(), output.width);
        }

        // Initialize state elements (registers only)
        for (name, element) in &module.state_elements {
            let byte_size = element.width.div_ceil(8);
            let initial_value = if let Some(reset_val) = element.reset_value {
                let mut bytes = vec![0u8; byte_size];
                // Convert reset value to bytes
                for (i, byte) in bytes.iter_mut().enumerate().take(byte_size) {
                    *byte = ((reset_val >> (i * 8)) & 0xFF) as u8;
                }
                bytes
            } else {
                vec![0u8; byte_size]
            };
            self.state.insert(name.clone(), initial_value);
            self.signal_widths.insert(name.clone(), element.width);
        }
    }
}

#[async_trait]
impl SimulationRuntime for CpuRuntime {
    async fn initialize(&mut self, module: &SirModule) -> SimulationResult<()> {
        self.module = Some(module.clone());
        self.initialize_signals(module);
        Ok(())
    }

    async fn step(&mut self) -> SimulationResult<SimulationState> {
        // CRITICAL THREE-PHASE EXECUTION (matching GPU runtime):

        // Phase 1: Copy old state to signals, then evaluate combinational logic
        // This ensures sequential logic sees old register values (non-blocking semantics)
        // Note: Inputs are NOT copied to signals - SignalRef nodes read directly from self.inputs
        for (name, value) in &self.state {
            self.signals.insert(name.clone(), value.clone());
        }
        self.evaluate_combinational()?;

        // Phase 2: Sequential logic updates registers on clock edges
        // This samples the D inputs computed in phase 1 and updates state
        self.evaluate_sequential()?;

        // Phase 3: Re-execute combinational logic with NEW register state
        // This ensures outputs reflect current (not old) register values
        for (name, value) in &self.state {
            self.signals.insert(name.clone(), value.clone());
        }
        self.evaluate_combinational()?;

        self.current_cycle += 1;

        // Extract and return current state (matching GPU behavior)
        Ok(self.extract_state())
    }

    async fn run(&mut self, cycles: u64) -> SimulationResult<Vec<SimulationState>> {
        let mut states = Vec::new();

        for _ in 0..cycles {
            let state = self.step().await?;
            states.push(state);

            // Yield occasionally to prevent blocking
            if states.len() % 100 == 0 {
                tokio::task::yield_now().await;
            }
        }

        Ok(states)
    }

    async fn reset(&mut self) -> SimulationResult<()> {
        self.current_cycle = 0;

        // Reset all state elements to initial values
        if let Some(module) = &self.module {
            for (name, element) in &module.state_elements {
                let byte_size = element.width.div_ceil(8);
                let reset_value = if let Some(val) = element.reset_value {
                    let mut bytes = vec![0u8; byte_size];
                    for (i, byte) in bytes.iter_mut().enumerate().take(byte_size) {
                        *byte = ((val >> (i * 8)) & 0xFF) as u8;
                    }
                    bytes
                } else {
                    vec![0u8; byte_size]
                };
                self.state.insert(name.clone(), reset_value);
            }
        }

        // Clear signals
        for (_, signal_data) in self.signals.iter_mut() {
            signal_data.fill(0);
        }

        Ok(())
    }

    async fn set_input(&mut self, name: &str, value: &[u8]) -> SimulationResult<()> {
        if self.inputs.contains_key(name) {
            self.inputs.insert(name.to_string(), value.to_vec());
            Ok(())
        } else {
            Err(SimulationError::InvalidInput(format!(
                "Input {} not found",
                name
            )))
        }
    }

    async fn get_output(&self, name: &str) -> SimulationResult<Vec<u8>> {
        self.outputs
            .get(name)
            .cloned()
            .ok_or_else(|| SimulationError::InvalidInput(format!("Output {} not found", name)))
    }
}
