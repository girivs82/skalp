//! Metal GPU shader generation for hardware simulation
//!
//! Generates Metal compute shaders from SIR combinational blocks to enable
//! parallel execution of digital logic on GPU.

use crate::sir::{SirExpression, SirOperation, BinaryOp, UnaryOp, ReduceOp, CombinationalBlock, SirSignalId};
use crate::cone::CombinationalCone;
use indexmap::IndexMap;
use std::fmt::Write;

/// Metal shader generation result
#[derive(Debug)]
pub struct ShaderGenerationResult {
    /// Generated Metal shader source code
    pub source: String,
    /// Shader entry point function name
    pub entry_point: String,
    /// Input buffer layout (signal_id -> byte_offset)
    pub input_layout: IndexMap<SirSignalId, usize>,
    /// Output buffer layout (signal_id -> byte_offset)
    pub output_layout: IndexMap<SirSignalId, usize>,
    /// Total input buffer size in bytes
    pub input_buffer_size: usize,
    /// Total output buffer size in bytes
    pub output_buffer_size: usize,
}

/// Metal shader generator for combinational logic
pub struct MetalShaderGenerator {
    /// Next temporary variable ID
    next_temp_id: u32,
    /// Signal width information (signal_id -> width_in_bits)
    signal_widths: IndexMap<SirSignalId, usize>,
}

impl MetalShaderGenerator {
    /// Create a new Metal shader generator
    pub fn new() -> Self {
        Self {
            next_temp_id: 0,
            signal_widths: IndexMap::new(),
        }
    }

    /// Generate Metal compute shader for a combinational cone
    pub fn generate_cone_shader(
        &mut self,
        cone: &CombinationalCone,
        blocks: &[CombinationalBlock],
        signal_widths: &IndexMap<SirSignalId, usize>,
    ) -> Result<ShaderGenerationResult, ShaderGenerationError> {
        self.signal_widths = signal_widths.clone();
        self.next_temp_id = 0;

        let mut shader_source = String::new();

        // Generate Metal shader header
        self.generate_header(&mut shader_source)?;

        // Generate buffer layout structs
        let (input_layout, output_layout, input_size, output_size) =
            self.generate_buffer_layouts(&mut shader_source, cone)?;

        // Generate the compute kernel function
        let entry_point = format!("cone_{}_kernel", cone.id.0);
        self.generate_kernel_function(&mut shader_source, &entry_point, cone, blocks)?;

        Ok(ShaderGenerationResult {
            source: shader_source,
            entry_point,
            input_layout,
            output_layout,
            input_buffer_size: input_size,
            output_buffer_size: output_size,
        })
    }

    /// Generate Metal shader header
    fn generate_header(&self, source: &mut String) -> Result<(), ShaderGenerationError> {
        writeln!(source, "#include <metal_stdlib>")?;
        writeln!(source, "using namespace metal;")?;
        writeln!(source)?;
        writeln!(source, "// SKALP GPU Simulation Shader")?;
        writeln!(source, "// Generated automatically - DO NOT EDIT")?;
        writeln!(source)?;
        Ok(())
    }

    /// Generate buffer layout structs and calculate layouts
    fn generate_buffer_layouts(
        &self,
        source: &mut String,
        cone: &CombinationalCone,
    ) -> Result<(IndexMap<SirSignalId, usize>, IndexMap<SirSignalId, usize>, usize, usize), ShaderGenerationError> {
        let mut input_layout = IndexMap::new();
        let mut output_layout = IndexMap::new();
        let mut input_offset = 0;
        let mut output_offset = 0;

        // Generate input buffer struct
        writeln!(source, "struct InputBuffer {{")?;
        for &signal_id in &cone.inputs {
            let width = self.signal_widths.get(&signal_id).unwrap_or(&32);
            let metal_type = self.get_metal_type(*width);
            writeln!(source, "    {} signal_{};", metal_type, signal_id.0)?;

            input_layout.insert(signal_id, input_offset);
            input_offset += self.get_type_size(*width);
        }
        writeln!(source, "}};")?;
        writeln!(source)?;

        // Generate output buffer struct
        writeln!(source, "struct OutputBuffer {{")?;
        for &signal_id in &cone.outputs {
            let width = self.signal_widths.get(&signal_id).unwrap_or(&32);
            let metal_type = self.get_metal_type(*width);
            writeln!(source, "    {} signal_{};", metal_type, signal_id.0)?;

            output_layout.insert(signal_id, output_offset);
            output_offset += self.get_type_size(*width);
        }
        writeln!(source, "}};")?;
        writeln!(source)?;

        Ok((input_layout, output_layout, input_offset, output_offset))
    }

    /// Generate the main compute kernel function
    fn generate_kernel_function(
        &mut self,
        source: &mut String,
        entry_point: &str,
        cone: &CombinationalCone,
        blocks: &[CombinationalBlock],
    ) -> Result<(), ShaderGenerationError> {
        writeln!(source, "kernel void {}(", entry_point)?;
        writeln!(source, "    device const InputBuffer* input_buffer [[buffer(0)]],")?;
        writeln!(source, "    device OutputBuffer* output_buffer [[buffer(1)]],")?;
        writeln!(source, "    uint gid [[thread_position_in_grid]]")?;
        writeln!(source, ") {{")?;
        writeln!(source, "    // Check bounds")?;
        writeln!(source, "    if (gid >= 1) return;")?;
        writeln!(source)?;

        // Load inputs into local variables
        writeln!(source, "    // Load inputs")?;
        for &signal_id in &cone.inputs {
            let width = self.signal_widths.get(&signal_id).unwrap_or(&32);
            let metal_type = self.get_metal_type(*width);
            writeln!(source, "    {} signal_{} = input_buffer->signal_{};",
                    metal_type, signal_id.0, signal_id.0)?;
        }
        writeln!(source)?;

        // Generate logic for each block in the cone
        let block_map: IndexMap<_, _> = blocks.iter().map(|b| (b.id, b)).collect();
        for &block_id in &cone.blocks {
            if let Some(block) = block_map.get(&block_id) {
                writeln!(source, "    // Block {}", block_id.0)?;
                self.generate_block_logic(source, block)?;
                writeln!(source)?;
            }
        }

        // Store outputs
        writeln!(source, "    // Store outputs")?;
        for &signal_id in &cone.outputs {
            writeln!(source, "    output_buffer->signal_{} = signal_{};",
                    signal_id.0, signal_id.0)?;
        }

        writeln!(source, "}}")?;
        Ok(())
    }

    /// Generate logic for a single combinational block
    fn generate_block_logic(
        &mut self,
        source: &mut String,
        block: &CombinationalBlock,
    ) -> Result<(), ShaderGenerationError> {
        for operation in &block.operations {
            self.generate_operation(source, operation)?;
        }
        Ok(())
    }

    /// Generate code for a single operation
    fn generate_operation(
        &mut self,
        source: &mut String,
        operation: &SirOperation,
    ) -> Result<(), ShaderGenerationError> {
        match operation {
            SirOperation::Assign { target, source: expr } => {
                let expr_code = self.generate_expression(expr)?;
                writeln!(source, "    signal_{} = {};", target.0, expr_code)?;
            }
            SirOperation::ConditionalAssign { condition, target, source: expr } => {
                let cond_code = self.generate_expression(condition)?;
                let expr_code = self.generate_expression(expr)?;
                writeln!(source, "    if ({}) {{ signal_{} = {}; }}",
                        cond_code, target.0, expr_code)?;
            }
            SirOperation::Case { selector, cases, default } => {
                let sel_code = self.generate_expression(selector)?;
                writeln!(source, "    switch ({}) {{", sel_code)?;

                for case in cases {
                    for value in &case.values {
                        let val_code = self.generate_expression(value)?;
                        writeln!(source, "    case {}:", val_code)?;
                    }
                    for op in &case.operations {
                        write!(source, "    ")?;
                        self.generate_operation(source, op)?;
                    }
                    writeln!(source, "        break;")?;
                }

                if let Some(default_ops) = default {
                    writeln!(source, "    default:")?;
                    for op in default_ops {
                        write!(source, "    ")?;
                        self.generate_operation(source, op)?;
                    }
                    writeln!(source, "        break;")?;
                }

                writeln!(source, "    }}")?;
            }
        }
        Ok(())
    }

    /// Generate code for an expression
    fn generate_expression(&mut self, expr: &SirExpression) -> Result<String, ShaderGenerationError> {
        match expr {
            SirExpression::Signal(signal_id) => {
                Ok(format!("signal_{}", signal_id.0))
            }
            SirExpression::Constant(bit_vec) => {
                // Convert BitVec to appropriate constant
                // For now, treat as integer
                let value = self.bitvec_to_u64(bit_vec);
                Ok(value.to_string())
            }
            SirExpression::Binary { op, left, right } => {
                let left_code = self.generate_expression(left)?;
                let right_code = self.generate_expression(right)?;
                let op_str = self.binary_op_to_metal(op);
                Ok(format!("({} {} {})", left_code, op_str, right_code))
            }
            SirExpression::Unary { op, operand } => {
                let operand_code = self.generate_expression(operand)?;
                match op {
                    UnaryOp::Not => Ok(format!("(!{})", operand_code)),
                    UnaryOp::BitwiseNot => Ok(format!("(~{})", operand_code)),
                    UnaryOp::Negate => Ok(format!("(-{})", operand_code)),
                    UnaryOp::Reduce(reduce_op) => {
                        self.generate_reduction(reduce_op, &operand_code)
                    }
                }
            }
            SirExpression::BitSelect { signal, index } => {
                let signal_code = self.generate_expression(&SirExpression::Signal(*signal))?;
                let index_code = self.generate_expression(index)?;
                Ok(format!("(({} >> {}) & 1)", signal_code, index_code))
            }
            SirExpression::RangeSelect { signal, high, low } => {
                let signal_code = self.generate_expression(&SirExpression::Signal(*signal))?;
                let high_code = self.generate_expression(high)?;
                let low_code = self.generate_expression(low)?;
                Ok(format!("(({} >> {}) & ((1u << ({} - {} + 1)) - 1))",
                          signal_code, low_code, high_code, low_code))
            }
            SirExpression::Concat(exprs) => {
                let mut result = String::new();
                let mut shift = 0;
                for (i, expr) in exprs.iter().enumerate() {
                    let expr_code = self.generate_expression(expr)?;
                    if i == 0 {
                        result = expr_code;
                    } else {
                        // Estimate width for shift (simplified)
                        shift += 8; // Default to 8-bit shift
                        result = format!("({} | ({} << {}))", result, expr_code, shift);
                    }
                }
                Ok(result)
            }
            SirExpression::Replicate { count, value } => {
                let count_code = self.generate_expression(count)?;
                let value_code = self.generate_expression(value)?;
                // Simplified replication - would need proper width calculation
                Ok(format!("({} * ((1u << {}) - 1) / ((1u << 1) - 1))",
                          value_code, count_code))
            }
        }
    }

    /// Generate reduction operation code
    fn generate_reduction(&self, op: &ReduceOp, operand: &str) -> Result<String, ShaderGenerationError> {
        match op {
            ReduceOp::And => Ok(format!("(popcount(~{}) == 0)", operand)),
            ReduceOp::Or => Ok(format!("({} != 0)", operand)),
            ReduceOp::Xor => Ok(format!("(popcount({}) & 1)", operand)),
            ReduceOp::Nand => Ok(format!("(popcount(~{}) != 0)", operand)),
            ReduceOp::Nor => Ok(format!("({} == 0)", operand)),
            ReduceOp::Xnor => Ok(format!("((popcount({}) & 1) == 0)", operand)),
        }
    }

    /// Convert binary operation to Metal operator
    fn binary_op_to_metal(&self, op: &BinaryOp) -> &'static str {
        match op {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
            BinaryOp::Xor => "^^", // Note: Metal doesn't have ^^ for logical XOR
            BinaryOp::BitwiseAnd => "&",
            BinaryOp::BitwiseOr => "|",
            BinaryOp::BitwiseXor => "^",
            BinaryOp::Equal => "==",
            BinaryOp::NotEqual => "!=",
            BinaryOp::Less => "<",
            BinaryOp::LessEqual => "<=",
            BinaryOp::Greater => ">",
            BinaryOp::GreaterEqual => ">=",
            BinaryOp::LeftShift => "<<",
            BinaryOp::RightShift => ">>",
        }
    }

    /// Get Metal type for given bit width
    fn get_metal_type(&self, width: usize) -> &'static str {
        match width {
            1 => "bool",
            2..=8 => "uint8_t",
            9..=16 => "uint16_t",
            17..=32 => "uint32_t",
            33..=64 => "uint64_t",
            _ => "uint64_t", // Default to largest type
        }
    }

    /// Get size in bytes for given bit width
    fn get_type_size(&self, width: usize) -> usize {
        match width {
            1 => 1,
            2..=8 => 1,
            9..=16 => 2,
            17..=32 => 4,
            33..=64 => 8,
            _ => 8,
        }
    }

    /// Convert BitVec to u64 (simplified)
    fn bitvec_to_u64(&self, _bit_vec: &bitvec::prelude::BitVec) -> u64 {
        // Simplified conversion - would need proper BitVec handling
        0
    }
}

impl Default for MetalShaderGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Error type for shader generation
#[derive(Debug, thiserror::Error)]
pub enum ShaderGenerationError {
    #[error("Format error: {0}")]
    Format(#[from] std::fmt::Error),
    #[error("Invalid operation: {0}")]
    InvalidOperation(String),
    #[error("Unsupported feature: {0}")]
    UnsupportedFeature(String),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sir::*;
    use crate::cone::*;
    use bitvec::prelude::*;

    #[test]
    fn test_metal_shader_generation() {
        let mut generator = MetalShaderGenerator::new();

        // Create a simple cone with one block
        let cone = CombinationalCone {
            id: ConeId(0),
            blocks: vec![CombBlockId(0)],
            inputs: vec![SirSignalId(0), SirSignalId(1)],
            outputs: vec![SirSignalId(2)],
            workgroup_size: 64,
            logic_depth: 1,
        };

        // Create a simple addition block
        let block = CombinationalBlock {
            id: CombBlockId(0),
            inputs: vec![SirSignalId(0), SirSignalId(1)],
            outputs: vec![SirSignalId(2)],
            operations: vec![
                SirOperation::Assign {
                    target: SirSignalId(2),
                    source: SirExpression::Binary {
                        op: BinaryOp::Add,
                        left: Box::new(SirExpression::Signal(SirSignalId(0))),
                        right: Box::new(SirExpression::Signal(SirSignalId(1))),
                    }
                }
            ],
            workgroup_size_hint: Some(64),
        };

        let mut signal_widths = IndexMap::new();
        signal_widths.insert(SirSignalId(0), 8);
        signal_widths.insert(SirSignalId(1), 8);
        signal_widths.insert(SirSignalId(2), 9);

        let result = generator.generate_cone_shader(&cone, &[block], &signal_widths);

        assert!(result.is_ok());
        let shader = result.unwrap();

        assert!(shader.source.contains("#include <metal_stdlib>"));
        assert!(shader.source.contains("cone_0_kernel"));
        assert!(shader.input_layout.len() == 2);
        assert!(shader.output_layout.len() == 1);
    }
}