use crate::simulator::{SimulationError, SimulationResult, SimulationRuntime, SimulationState};
use async_trait::async_trait;
use indexmap::IndexMap;
use skalp_sir::SirModule;

pub struct CpuRuntime {
    module: Option<SirModule>,
    inputs: IndexMap<String, Vec<u8>>, // Input port values (preserved across steps)
    state: IndexMap<String, Vec<u8>>,  // State elements (registers)
    next_state: IndexMap<String, Vec<u8>>, // Next state for double-buffering
    outputs: IndexMap<String, Vec<u8>>, // Output port values (from BEFORE sequential update)
    current_outputs: IndexMap<String, Vec<u8>>, // Temporary outputs during evaluation
    signals: IndexMap<String, Vec<u8>>, // Internal signal values
    signal_widths: IndexMap<String, usize>, // Width in bits for each signal
    prev_clock_values: IndexMap<String, u8>, // Previous clock values for edge detection
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
            inputs: IndexMap::new(),
            state: IndexMap::new(),
            next_state: IndexMap::new(),
            outputs: IndexMap::new(),
            current_outputs: IndexMap::new(),
            signals: IndexMap::new(),
            signal_widths: IndexMap::new(),
            prev_clock_values: IndexMap::new(),
            current_cycle: 0,
        }
    }

    /// Truncate or zero-extend a value to the specified bit width
    fn truncate_to_width(value: &[u8], width_bits: usize) -> Vec<u8> {
        let byte_size = width_bits.div_ceil(8);

        if value.len() < byte_size {
            // BUG FIX #89: Zero-extend value to target width (was just returning original)
            // This is critical for signal propagation where narrower values feed wider outputs
            let mut result = vec![0u8; byte_size];
            result[..value.len()].copy_from_slice(value);
            return result;
        }

        if value.len() == byte_size {
            // Value is exact size, just clone
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
        // Get pre-sorted node IDs and node map from SIR (computed once at SIR construction)
        let (sorted_ids, nodes, outputs) = if let Some(module) = &self.module {
            (
                module.sorted_combinational_node_ids.clone(),
                module.combinational_nodes.clone(),
                module.outputs.clone(),
            )
        } else {
            return Ok(());
        };

        // Build a map from node_id to node for efficient lookup
        let node_map: IndexMap<usize, &skalp_sir::SirNode> =
            nodes.iter().map(|n| (n.id, n)).collect();

        // Evaluate all combinational nodes in pre-computed topological order
        for node_id in &sorted_ids {
            if let Some(node) = node_map.get(node_id) {
                self.evaluate_node(node)?;
            }
        }

        // Update current_outputs based on their drivers, truncating to correct width
        for output in &outputs {
            if let Some(value) = self.signals.get(&output.name) {
                // Truncate to the output's declared width
                let byte_size = output.width.div_ceil(8);
                let truncated = if value.len() > byte_size {
                    value[..byte_size].to_vec()
                } else {
                    value.clone()
                };
                self.current_outputs.insert(output.name.clone(), truncated);
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
                // Only extract bytes from the u64 value for indices where the shift is valid
                // For i*8 >= 64, the byte remains 0 (already initialized)
                for (i, byte) in bytes.iter_mut().enumerate().take(byte_size.min(8)) {
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
                        eprintln!(
                            "WARNING: SignalRef couldn't find signal '{}', defaulting to 0",
                            signal
                        );
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
                // BUG FIX #90: In HDL {A, B}, A is high bits and B is low bits
                // Inputs are ordered [high_bits, ..., low_bits], so we iterate in reverse
                // to place the last input (low bits) at bit_offset=0
                let total_bits: usize = input_values.iter().map(|v| v.len() * 8).sum();
                let byte_size = total_bits.div_ceil(8);
                let mut result = vec![0u8; byte_size];

                let mut bit_offset = 0;
                // Iterate in reverse to match HDL semantics (last input = lowest bits)
                for value in input_values.iter().rev() {
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
                // Array is stored as contiguous bytes: [elem0, elem1, elem2, ...]
                // We need to extract element at given index
                if input_values.len() >= 2 {
                    let array_bytes = &input_values[0];
                    let index = Self::bytes_to_u64(&input_values[1]) as usize;

                    // Get the output width to determine element size
                    let elem_size = if let Some(output) = node.outputs.first() {
                        self.signal_widths
                            .get(&output.signal_id)
                            .map(|&w| w.div_ceil(8))
                            .unwrap_or(1)
                    } else {
                        1
                    };

                    let offset = index * elem_size;

                    // Extract element from array
                    if offset + elem_size <= array_bytes.len() {
                        array_bytes[offset..offset + elem_size].to_vec()
                    } else {
                        // Out of bounds - return zero
                        vec![0u8; elem_size]
                    }
                } else {
                    vec![0u8]
                }
            }

            SirNodeKind::ArrayWrite => {
                // ArrayWrite: inputs = [old_array, index, value]
                // Creates new array with updated element
                if input_values.len() >= 3 {
                    let old_array = &input_values[0];
                    let index = Self::bytes_to_u64(&input_values[1]) as usize;
                    let value = &input_values[2];

                    // Create a copy of the old array
                    let mut new_array = old_array.clone();

                    let elem_size = value.len();
                    let offset = index * elem_size;

                    // Update element if within bounds
                    if offset + elem_size <= new_array.len() {
                        new_array[offset..offset + elem_size].copy_from_slice(value);
                    }

                    new_array
                } else {
                    vec![0u8]
                }
            }

            SirNodeKind::Latch { .. } => {
                // TODO: Implement latch support (level-sensitive storage)
                // Latches are transparent when enable is high, hold value when low
                vec![0u8]
            }

            SirNodeKind::Memory { .. } => {
                // TODO: Implement memory block support (for block RAMs)
                // Memory blocks have read/write ports with address decoding
                vec![0u8]
            }

            SirNodeKind::ClockGate => {
                // TODO: Implement clock gating support
                // Clock gating conditionally stops the clock for power savings
                vec![0u8]
            }

            SirNodeKind::Reset => {
                // TODO: Implement reset node support
                // Currently reset is handled via signal values
                vec![0u8]
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
            // Floating-point operations - perform proper IEEE 754 arithmetic
            BinaryOperation::FAdd => Self::eval_fp_binary(left, right, |a, b| a + b),
            BinaryOperation::FSub => Self::eval_fp_binary(left, right, |a, b| a - b),
            BinaryOperation::FMul => Self::eval_fp_binary(left, right, |a, b| a * b),
            BinaryOperation::FDiv => Self::eval_fp_binary(left, right, |a, b| a / b),
            BinaryOperation::FMod => Self::eval_fp_binary(left, right, |a, b| a % b),
            BinaryOperation::FEq => Self::eval_fp_compare(left, right, |a, b| a == b),
            BinaryOperation::FNeq => Self::eval_fp_compare(left, right, |a, b| a != b),
            BinaryOperation::FLt => Self::eval_fp_compare(left, right, |a, b| a < b),
            BinaryOperation::FLte => Self::eval_fp_compare(left, right, |a, b| a <= b),
            BinaryOperation::FGt => Self::eval_fp_compare(left, right, |a, b| a > b),
            BinaryOperation::FGte => Self::eval_fp_compare(left, right, |a, b| a >= b),
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
            UnaryOperation::Not => {
                // For proper NOT semantics, we need to mask to the actual bit width
                // Since we only have byte count, we mask to byte-aligned width
                // Special case: for 1-byte values that are 0 or 1 (boolean/1-bit signals),
                // use logical NOT to avoid 0xFE result from bitwise NOT of 1
                if operand.len() == 1 && operand_val <= 1 {
                    // Boolean value - logical NOT
                    if operand_val == 0 { 1 } else { 0 }
                } else {
                    // Multi-bit value - bitwise NOT with proper masking
                    let mask = if operand.len() >= 8 {
                        u64::MAX
                    } else {
                        (1u64 << (operand.len() * 8)) - 1
                    };
                    (!operand_val) & mask
                }
            }
            UnaryOperation::Neg => {
                // Check if this is likely an FP type based on size (2, 4, or 8 bytes)
                // For FP types, perform proper IEEE 754 negation
                match operand.len() {
                    2 | 4 | 8 => Self::eval_fp_unary(operand, |a| -a),
                    _ => operand_val.wrapping_neg(), // Integer negation for other sizes
                }
            }
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
            // Floating-point operations - perform proper IEEE 754 arithmetic
            UnaryOperation::FNeg => Self::eval_fp_unary(operand, |a| -a),
            UnaryOperation::FAbs => Self::eval_fp_unary(operand, |a| a.abs()),
            UnaryOperation::FSqrt => Self::eval_fp_unary(operand, |a| a.sqrt()),
        };

        Ok(Self::u64_to_bytes(result_val, operand.len()))
    }

    fn extract_slice(value: &[u8], start: usize, end: usize) -> Vec<u8> {
        // For HDL range [high:low], start=high, end=low
        // Width = high - low + 1, shift = low
        let (high, low) = if start >= end {
            (start, end)
        } else {
            (end, start)
        };
        let width = high - low + 1;
        let byte_size = width.div_ceil(8);
        let mut result = vec![0u8; byte_size];

        // For values > 64 bits, work with byte arrays directly
        if low < 64 && value.len() <= 8 {
            // Fast path for u64 values
            let value_as_u64 = Self::bytes_to_u64(value);
            let mask = if width >= 64 {
                u64::MAX
            } else {
                (1u64 << width) - 1
            };
            let sliced = (value_as_u64 >> low) & mask;

            for (i, byte) in result.iter_mut().enumerate() {
                *byte = ((sliced >> (i * 8)) & 0xFF) as u8;
            }
        } else {
            // Slow path for values > 64 bits or high bit positions
            // Extract bit-by-bit
            for bit_idx in 0..width {
                let src_bit = low + bit_idx;
                let src_byte = src_bit / 8;
                let src_bit_in_byte = src_bit % 8;

                let dst_byte = bit_idx / 8;
                let dst_bit_in_byte = bit_idx % 8;

                if src_byte < value.len() {
                    let bit_val = (value[src_byte] >> src_bit_in_byte) & 1;
                    result[dst_byte] |= bit_val << dst_bit_in_byte;
                }
            }
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
        // Only extract up to 8 bytes (64 bits) from the u64 value
        let extract_count = byte_count.min(8);
        for i in 0..extract_count {
            bytes[i] = ((value >> (i * 8)) & 0xFF) as u8;
        }
        // Remaining bytes (if byte_count > 8) stay as 0
        bytes
    }

    /// Evaluate floating-point binary operation
    fn eval_fp_binary<F>(left: &[u8], right: &[u8], op: F) -> u64
    where
        F: Fn(f64, f64) -> f64,
    {
        // Determine FP type based on byte length
        match left.len() {
            2 => {
                // fp16 - convert via f32 (Rust doesn't have native f16)
                let left_f32 = f32::from_bits(u32::from_le_bytes([
                    left[0],
                    left.get(1).copied().unwrap_or(0),
                    0,
                    0,
                ]));
                let right_f32 = f32::from_bits(u32::from_le_bytes([
                    right[0],
                    right.get(1).copied().unwrap_or(0),
                    0,
                    0,
                ]));
                let result = op(left_f32 as f64, right_f32 as f64) as f32;
                let result_bits = result.to_bits();
                (result_bits & 0xFFFF) as u64
            }
            4 => {
                // fp32
                let left_f32 = f32::from_le_bytes([
                    left[0],
                    left.get(1).copied().unwrap_or(0),
                    left.get(2).copied().unwrap_or(0),
                    left.get(3).copied().unwrap_or(0),
                ]);
                let right_f32 = f32::from_le_bytes([
                    right[0],
                    right.get(1).copied().unwrap_or(0),
                    right.get(2).copied().unwrap_or(0),
                    right.get(3).copied().unwrap_or(0),
                ]);
                let result = op(left_f32 as f64, right_f32 as f64) as f32;
                result.to_bits() as u64
            }
            8 => {
                // fp64
                let left_f64 = f64::from_le_bytes([
                    left[0],
                    left.get(1).copied().unwrap_or(0),
                    left.get(2).copied().unwrap_or(0),
                    left.get(3).copied().unwrap_or(0),
                    left.get(4).copied().unwrap_or(0),
                    left.get(5).copied().unwrap_or(0),
                    left.get(6).copied().unwrap_or(0),
                    left.get(7).copied().unwrap_or(0),
                ]);
                let right_f64 = f64::from_le_bytes([
                    right[0],
                    right.get(1).copied().unwrap_or(0),
                    right.get(2).copied().unwrap_or(0),
                    right.get(3).copied().unwrap_or(0),
                    right.get(4).copied().unwrap_or(0),
                    right.get(5).copied().unwrap_or(0),
                    right.get(6).copied().unwrap_or(0),
                    right.get(7).copied().unwrap_or(0),
                ]);
                let result = op(left_f64, right_f64);
                result.to_bits()
            }
            _ => 0, // Unsupported FP size
        }
    }

    /// Evaluate floating-point comparison operation
    fn eval_fp_compare<F>(left: &[u8], right: &[u8], op: F) -> u64
    where
        F: Fn(f64, f64) -> bool,
    {
        // Determine FP type based on byte length
        let result = match left.len() {
            2 => {
                // fp16
                let left_f32 = f32::from_bits(u32::from_le_bytes([
                    left[0],
                    left.get(1).copied().unwrap_or(0),
                    0,
                    0,
                ]));
                let right_f32 = f32::from_bits(u32::from_le_bytes([
                    right[0],
                    right.get(1).copied().unwrap_or(0),
                    0,
                    0,
                ]));
                op(left_f32 as f64, right_f32 as f64)
            }
            4 => {
                // fp32
                let left_f32 = f32::from_le_bytes([
                    left[0],
                    left.get(1).copied().unwrap_or(0),
                    left.get(2).copied().unwrap_or(0),
                    left.get(3).copied().unwrap_or(0),
                ]);
                let right_f32 = f32::from_le_bytes([
                    right[0],
                    right.get(1).copied().unwrap_or(0),
                    right.get(2).copied().unwrap_or(0),
                    right.get(3).copied().unwrap_or(0),
                ]);
                op(left_f32 as f64, right_f32 as f64)
            }
            8 => {
                // fp64
                let left_f64 = f64::from_le_bytes([
                    left[0],
                    left.get(1).copied().unwrap_or(0),
                    left.get(2).copied().unwrap_or(0),
                    left.get(3).copied().unwrap_or(0),
                    left.get(4).copied().unwrap_or(0),
                    left.get(5).copied().unwrap_or(0),
                    left.get(6).copied().unwrap_or(0),
                    left.get(7).copied().unwrap_or(0),
                ]);
                let right_f64 = f64::from_le_bytes([
                    right[0],
                    right.get(1).copied().unwrap_or(0),
                    right.get(2).copied().unwrap_or(0),
                    right.get(3).copied().unwrap_or(0),
                    right.get(4).copied().unwrap_or(0),
                    right.get(5).copied().unwrap_or(0),
                    right.get(6).copied().unwrap_or(0),
                    right.get(7).copied().unwrap_or(0),
                ]);
                op(left_f64, right_f64)
            }
            _ => false, // Unsupported FP size
        };
        if result {
            1
        } else {
            0
        }
    }

    /// Evaluate floating-point unary operation
    fn eval_fp_unary<F>(operand: &[u8], op: F) -> u64
    where
        F: Fn(f64) -> f64,
    {
        // Determine FP type based on byte length
        match operand.len() {
            2 => {
                // fp16
                let operand_f32 = f32::from_bits(u32::from_le_bytes([
                    operand[0],
                    operand.get(1).copied().unwrap_or(0),
                    0,
                    0,
                ]));
                let result = op(operand_f32 as f64) as f32;
                let result_bits = result.to_bits();
                (result_bits & 0xFFFF) as u64
            }
            4 => {
                // fp32
                let operand_f32 = f32::from_le_bytes([
                    operand[0],
                    operand.get(1).copied().unwrap_or(0),
                    operand.get(2).copied().unwrap_or(0),
                    operand.get(3).copied().unwrap_or(0),
                ]);
                let result = op(operand_f32 as f64) as f32;
                result.to_bits() as u64
            }
            8 => {
                // fp64
                let operand_f64 = f64::from_le_bytes([
                    operand[0],
                    operand.get(1).copied().unwrap_or(0),
                    operand.get(2).copied().unwrap_or(0),
                    operand.get(3).copied().unwrap_or(0),
                    operand.get(4).copied().unwrap_or(0),
                    operand.get(5).copied().unwrap_or(0),
                    operand.get(6).copied().unwrap_or(0),
                    operand.get(7).copied().unwrap_or(0),
                ]);
                let result = op(operand_f64);
                result.to_bits()
            }
            _ => 0, // Unsupported FP size
        }
    }

    fn evaluate_sequential(&mut self) -> Result<(), SimulationError> {
        use skalp_sir::ClockEdge;

        // Clone nodes to avoid borrow checker issues
        let seq_nodes = if let Some(module) = &self.module {
            module.sequential_nodes.clone()
        } else {
            return Ok(());
        };

        // Track clock values to update after processing all flip-flops
        let mut clock_updates = IndexMap::new();

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
                let prev_clock = self
                    .prev_clock_values
                    .get(clock_signal)
                    .copied()
                    .unwrap_or(0);

                // Check for the specified edge
                let edge_detected = match clock_edge {
                    ClockEdge::Rising => prev_clock == 0 && current_clock != 0,
                    ClockEdge::Falling => prev_clock != 0 && current_clock == 0,
                    ClockEdge::Both => {
                        (prev_clock == 0 && current_clock != 0)
                            || (prev_clock != 0 && current_clock == 0)
                    }
                };

                // Track this clock for later update (don't update yet!)
                clock_updates.insert(clock_signal.clone(), current_clock);

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
                        let truncated =
                            if let Some(&width) = self.signal_widths.get(&output.signal_id) {
                                Self::truncate_to_width(&d_value, width)
                            } else {
                                d_value.clone()
                            };
                        self.next_state.insert(output.signal_id.clone(), truncated);
                    }
                }
            }
        }

        // Now update all clock values (after processing all flip-flops)
        for (clock_signal, clock_value) in clock_updates {
            self.prev_clock_values.insert(clock_signal, clock_value);
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
        let mut signals = IndexMap::new();
        let mut registers = IndexMap::new();

        if let Some(module) = &self.module {
            // Build set of output names to avoid reading them twice (matching GPU behavior)
            let output_names: std::collections::HashSet<_> =
                module.outputs.iter().map(|o| &o.name).collect();

            // BUG FIX #87: Build set of input port names to exclude (matching GPU behavior)
            // Input ports are in the Inputs struct, not Signals, so they shouldn't appear in signal output
            let input_names: std::collections::HashSet<_> =
                module.inputs.iter().map(|i| &i.name).collect();

            // Add outputs to signals (matching GPU behavior)
            for output in &module.outputs {
                if let Some(value) = self.outputs.get(&output.name) {
                    signals.insert(output.name.clone(), value.clone());
                }
            }

            // Add non-state intermediate signals, skipping outputs and input ports (matching GPU behavior)
            for signal in &module.signals {
                if !signal.is_state
                    && !output_names.contains(&signal.name)
                    && !input_names.contains(&signal.name)
                {
                    let value = self.signals.get(&signal.name).cloned().unwrap_or_else(|| {
                        // Signals not driven by nodes get zero value (matching GPU)
                        vec![0u8; signal.width.div_ceil(8)]
                    });
                    signals.insert(signal.name.clone(), value);
                }
            }

            // Add state elements to registers (matching GPU behavior)
            // Only state elements go in registers, not inputs or outputs
            for name in module.state_elements.keys() {
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
        let input_names: std::collections::HashSet<_> =
            module.inputs.iter().map(|i| &i.name).collect();

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
            self.current_outputs
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
        // This updates signals and outputs to reflect the post-clock state
        for (name, value) in &self.state {
            self.signals.insert(name.clone(), value.clone());
        }
        self.evaluate_combinational()?;

        // BUG FIX #87: Capture outputs AFTER phase 3 (matching GPU runtime)
        // This provides correct FIFO/FWFT semantics where outputs reflect the post-clock state
        // Before this fix, CPU captured outputs BEFORE phase 2, causing 1-cycle timing difference with GPU
        for (name, value) in &self.current_outputs {
            self.outputs.insert(name.clone(), value.clone());
        }

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
        // BUG FIX #212: Also expose internal signals and inputs for debugging
        // First check declared outputs, then internal signals, then state, then inputs
        // This allows testbenches to inspect internal state during simulation
        self.outputs
            .get(name)
            .or_else(|| self.signals.get(name))
            .or_else(|| self.state.get(name))
            .or_else(|| self.inputs.get(name))
            .cloned()
            .ok_or_else(|| SimulationError::InvalidInput(format!("Signal '{}' not found in outputs, signals, state, or inputs", name)))
    }
}
