//! Shared Code Generation Core
//!
//! This module contains the shared code generation logic that produces
//! nearly identical C++ code for both Metal and CPU backends. The expressions,
//! struct layouts, and computation bodies are identical - only the wrapper
//! code differs between backends.

use crate::sir::*;
use std::collections::{HashMap, HashSet};

use super::types::{BackendTarget, TypeMapper};

/// Shared codegen core that generates backend-agnostic C++ expressions
pub struct SharedCodegen<'a> {
    /// Reference to the SIR module being compiled
    pub module: &'a SirModule,
    /// Type mapper for the target backend
    pub type_mapper: TypeMapper,
    /// Output buffer for generated code
    output: String,
    /// Current indentation level
    indent: usize,
    /// Track recursion depth for concat generation
    concat_recursion_depth: usize,
    /// Whether we're generating code for batched simulation mode
    in_batched_mode: bool,
}

impl<'a> SharedCodegen<'a> {
    /// Create a new shared codegen instance
    pub fn new(module: &'a SirModule, target: BackendTarget) -> Self {
        Self {
            module,
            type_mapper: TypeMapper::new(target),
            output: String::new(),
            indent: 0,
            concat_recursion_depth: 0,
            in_batched_mode: false,
        }
    }

    /// Set batched mode flag
    pub fn set_batched_mode(&mut self, batched: bool) {
        self.in_batched_mode = batched;
    }

    /// Get the generated output
    pub fn take_output(&mut self) -> String {
        std::mem::take(&mut self.output)
    }

    /// Write indented text to output
    pub fn write_indented(&mut self, text: &str) {
        for _ in 0..self.indent {
            self.output.push_str("    ");
        }
        self.output.push_str(text);
    }

    /// Write text without indentation
    pub fn write(&mut self, text: &str) {
        self.output.push_str(text);
    }

    /// Write a line with newline
    pub fn writeln(&mut self, text: &str) {
        self.output.push_str(text);
        self.output.push('\n');
    }

    /// Increase indentation
    pub fn indent(&mut self) {
        self.indent += 1;
    }

    /// Decrease indentation
    pub fn dedent(&mut self) {
        if self.indent > 0 {
            self.indent -= 1;
        }
    }

    /// Sanitize signal names (delegate to type mapper)
    pub fn sanitize_name(&self, name: &str) -> String {
        self.type_mapper.sanitize_name(name)
    }

    /// Get signal width from SIR module (delegate to type mapper)
    pub fn get_signal_width(&self, signal_name: &str) -> usize {
        self.type_mapper.get_signal_width(self.module, signal_name)
    }

    /// Get signal SIR type from module (delegate to type mapper)
    pub fn get_signal_type(&self, signal_name: &str) -> Option<SirType> {
        self.type_mapper.get_signal_type(self.module, signal_name)
    }

    /// Check if a given bit width uses array storage for the current backend
    /// C++: > 64 bits uses uint32_t[N] arrays
    /// Metal: > 128 bits uses uint[N] arrays (65-128 uses vector types like uint2/uint4)
    pub fn uses_array_storage(&self, width: usize) -> bool {
        match self.type_mapper.target {
            BackendTarget::Cpp => width > 64,
            BackendTarget::Metal => width > 128,
        }
    }

    /// Get the number of 32-bit array elements for a given width
    pub fn get_array_size(&self, width: usize) -> usize {
        width.div_ceil(32)
    }

    /// Get the accessor string for a register
    /// In batched mode, returns `local_X`, otherwise returns `registers->X`
    pub fn get_register_accessor(&self, name: &str) -> String {
        let sanitized = self.sanitize_name(name);
        if self.in_batched_mode {
            format!("local_{}", sanitized)
        } else {
            format!("registers->{}", sanitized)
        }
    }

    /// Generate struct field definition for a signal
    /// Returns true if the signal was decomposed
    pub fn generate_signal_field(&mut self, name: &str, sir_type: &SirType) -> bool {
        let width = sir_type.width();
        let sanitized_name = self.sanitize_name(name);

        // Check if this is an array type - arrays should not be decomposed
        let is_array_type = matches!(sir_type, SirType::Array(_, _));

        if width > 256 && !is_array_type {
            // Decompose into multiple 256-bit chunks
            let num_parts = width.div_ceil(256);
            let last_part_width = width - (num_parts - 1) * 256;

            // Track this decomposition in the type mapper
            // Note: We can't mutate type_mapper here, but the decomposition info
            // should be pre-computed. For now, just generate the parts.

            for part_idx in 0..num_parts {
                let part_width = if part_idx == num_parts - 1 {
                    last_part_width
                } else {
                    256
                };

                let (base_type, array_opt) = self.type_mapper.get_type_for_width(part_width);
                let part_name = format!("{}_part{}", sanitized_name, part_idx);

                if let Some(array_size) = array_opt {
                    self.write_indented(&format!("{} {}[{}];\n", base_type, part_name, array_size));
                } else {
                    self.write_indented(&format!("{} {};\n", base_type, part_name));
                }
            }
            true
        } else {
            // Normal case: single field
            let (base_type, array_suffix) = self.type_mapper.get_struct_field_parts(sir_type);
            self.write_indented(&format!("{} {}{};\n", base_type, sanitized_name, array_suffix));
            false
        }
    }

    /// Generate the Inputs struct definition
    pub fn generate_inputs_struct(&mut self) {
        self.writeln("// Input buffer");
        self.writeln("struct Inputs {");
        self.indent();

        for input in &self.module.inputs {
            let (base_type, array_suffix) =
                self.type_mapper.get_struct_field_parts(&input.sir_type);
            self.write_indented(&format!(
                "{} {}{};\n",
                base_type,
                self.sanitize_name(&input.name),
                array_suffix
            ));
        }

        self.dedent();
        self.writeln("};\n");
    }

    /// Generate the Registers struct definition
    pub fn generate_registers_struct(&mut self) {
        self.writeln("// Register buffer (flip-flop outputs only)");
        self.writeln("struct Registers {");
        self.indent();

        // Sort state elements by name for consistent ordering
        let mut sorted_states: Vec<_> = self.module.state_elements.iter().collect();
        sorted_states.sort_by_key(|(name, _)| *name);

        for (name, elem) in sorted_states.iter() {
            // Look up the signal to get its type
            let default_type = SirType::Bits(elem.width);
            let found_signal = self.module.signals.iter().find(|s| &s.name == *name);
            let sir_type = found_signal.map(|s| &s.sir_type).unwrap_or(&default_type);

            let (base_type, array_suffix) = self.type_mapper.get_struct_field_parts(sir_type);
            self.write_indented(&format!(
                "{} {}{};\n",
                base_type,
                self.sanitize_name(name),
                array_suffix
            ));
        }

        // Also add flip-flop outputs that aren't state elements
        let mut ff_output_names: HashSet<String> = HashSet::new();
        for node in &self.module.sequential_nodes {
            if let SirNodeKind::FlipFlop { .. } = &node.kind {
                if let Some(output) = node.outputs.first() {
                    if !self.module.state_elements.contains_key(&output.signal_id) {
                        ff_output_names.insert(output.signal_id.clone());
                    }
                }
            }
        }

        let mut sorted_ff_outputs: Vec<_> = ff_output_names.iter().collect();
        sorted_ff_outputs.sort();

        for name in sorted_ff_outputs {
            let width = self.get_signal_width(name);
            let sir_type = SirType::Bits(width);
            let (base_type, array_suffix) = self.type_mapper.get_struct_field_parts(&sir_type);
            self.write_indented(&format!(
                "{} {}{};\n",
                base_type,
                self.sanitize_name(name),
                array_suffix
            ));
        }

        self.dedent();
        self.writeln("};\n");
    }

    /// Generate the Signals struct definition
    pub fn generate_signals_struct(&mut self) {
        self.writeln("// Signal buffer (all computed values)");
        self.writeln("struct Signals {");
        self.indent();

        // Keep track of names already added
        let mut added_names: HashSet<String> = HashSet::new();
        let input_names: HashSet<String> =
            self.module.inputs.iter().map(|i| i.name.clone()).collect();

        // Add outputs first (skip state elements)
        for output in &self.module.outputs {
            if self.module.state_elements.contains_key(&output.name) {
                continue;
            }
            let sanitized_name = self.sanitize_name(&output.name);
            if added_names.insert(sanitized_name.clone()) {
                self.generate_signal_field(&output.name, &output.sir_type);
            }
        }

        // Add intermediate signals
        for signal in &self.module.signals {
            let sanitized_name = self.sanitize_name(&signal.name);
            if !signal.is_state
                && !input_names.contains(&signal.name)
                && added_names.insert(sanitized_name.clone())
            {
                self.generate_signal_field(&signal.name, &signal.sir_type);
            }
        }

        // Collect signals from combinational node outputs
        let signal_widths: HashMap<String, usize> = self
            .module
            .signals
            .iter()
            .map(|s| (s.name.clone(), s.width))
            .collect();

        for node in &self.module.combinational_nodes {
            for output in &node.outputs {
                let signal_id = &output.signal_id;
                let sanitized_name = self.sanitize_name(signal_id);

                if !added_names.insert(sanitized_name.clone()) {
                    continue;
                }

                if input_names.contains(signal_id)
                    || self.module.state_elements.contains_key(signal_id)
                {
                    continue;
                }

                let width = signal_widths.get(signal_id).copied().unwrap_or(32);
                let sir_type = SirType::Bits(width);
                self.generate_signal_field(signal_id, &sir_type);
            }
        }

        // Collect signals from node inputs
        for node in &self.module.combinational_nodes {
            for input in &node.inputs {
                let signal_id = &input.signal_id;
                let sanitized_name = self.sanitize_name(signal_id);

                if !added_names.insert(sanitized_name.clone()) {
                    continue;
                }

                if input_names.contains(signal_id)
                    || self.module.state_elements.contains_key(signal_id)
                {
                    continue;
                }

                let width = signal_widths.get(signal_id).copied().unwrap_or(32);
                let sir_type = SirType::Bits(width);
                self.generate_signal_field(signal_id, &sir_type);
            }
        }

        // Check sequential nodes too
        for node in &self.module.sequential_nodes {
            for input in &node.inputs {
                let signal_id = &input.signal_id;
                let sanitized_name = self.sanitize_name(signal_id);

                if !added_names.insert(sanitized_name.clone()) {
                    continue;
                }

                if input_names.contains(signal_id)
                    || self.module.state_elements.contains_key(signal_id)
                {
                    continue;
                }

                let width = signal_widths.get(signal_id).copied().unwrap_or(32);
                let sir_type = SirType::Bits(width);
                self.generate_signal_field(signal_id, &sir_type);
            }
        }

        self.dedent();
        self.writeln("};\n");
    }

    /// Generate binary operation expression
    pub fn generate_binary_op(&mut self, node: &SirNode, op: &BinaryOperation) {
        if node.inputs.len() < 2 || node.outputs.is_empty() {
            return;
        }

        let left = &node.inputs[0].signal_id;
        let right = &node.inputs[1].signal_id;
        let output = &node.outputs[0].signal_id;

        let left_width = self.get_signal_width(left);
        let right_width = self.get_signal_width(right);
        let output_width = self.get_signal_width(output);

        // Determine operator string
        let op_str = self.get_binary_op_str(op, left_width, right_width);

        // Check for wide bit operations that need element-wise handling
        if output_width > 128 {
            self.generate_wide_binary_op(node, op, &op_str);
            return;
        }

        // For 33-64 bit add/sub, use uint64_t arithmetic for proper carry
        let is_add_sub = matches!(op, BinaryOperation::Add | BinaryOperation::Sub);
        if is_add_sub && output_width > 32 && output_width <= 64 {
            self.generate_64bit_add_sub(node, &op_str, left_width, right_width);
            return;
        }

        // Standard scalar operation
        self.write_indented(&format!(
            "signals->{} = signals->{} {} signals->{};\n",
            self.sanitize_name(output),
            self.sanitize_name(left),
            op_str,
            self.sanitize_name(right)
        ));
    }

    /// Get binary operation string
    fn get_binary_op_str(&self, op: &BinaryOperation, left_width: usize, right_width: usize) -> &'static str {
        let is_boolean_context = left_width == 1 && right_width == 1;

        match op {
            BinaryOperation::Add => "+",
            BinaryOperation::Sub => "-",
            BinaryOperation::Mul | BinaryOperation::SMul => "*",
            BinaryOperation::Div | BinaryOperation::SDiv => "/",
            BinaryOperation::Mod | BinaryOperation::SMod => "%",
            BinaryOperation::And => if is_boolean_context { "&&" } else { "&" },
            BinaryOperation::Or => if is_boolean_context { "||" } else { "|" },
            BinaryOperation::Xor => "^",
            BinaryOperation::Eq => "==",
            BinaryOperation::Neq => "!=",
            BinaryOperation::Lt | BinaryOperation::Slt => "<",
            BinaryOperation::Lte | BinaryOperation::Slte => "<=",
            BinaryOperation::Gt | BinaryOperation::Sgt => ">",
            BinaryOperation::Gte | BinaryOperation::Sgte => ">=",
            BinaryOperation::Shl => "<<",
            BinaryOperation::Shr | BinaryOperation::Sar => ">>",
            BinaryOperation::FAdd => "+",
            BinaryOperation::FSub => "-",
            BinaryOperation::FMul => "*",
            BinaryOperation::FDiv => "/",
            BinaryOperation::FMod => "%",
            BinaryOperation::FEq => "==",
            BinaryOperation::FNeq => "!=",
            BinaryOperation::FLt => "<",
            BinaryOperation::FLte => "<=",
            BinaryOperation::FGt => ">",
            BinaryOperation::FGte => ">=",
        }
    }

    /// Generate wide binary operation (>128 bits) with element-wise handling
    fn generate_wide_binary_op(&mut self, node: &SirNode, op: &BinaryOperation, op_str: &str) {
        let left = &node.inputs[0].signal_id;
        let right = &node.inputs[1].signal_id;
        let output = &node.outputs[0].signal_id;

        let left_width = self.get_signal_width(left);
        let right_width = self.get_signal_width(right);
        let output_width = self.get_signal_width(output);

        let is_shift_op = matches!(op, BinaryOperation::Shl | BinaryOperation::Shr | BinaryOperation::Sar);
        let left_is_scalar = left_width <= 32;
        let right_is_scalar = right_width <= 32;

        let array_size = output_width.div_ceil(32);
        self.write_indented(&format!(
            "// Element-wise {} for {}-bit output\n",
            op_str, output_width
        ));
        self.write_indented(&format!("for (uint32_t i = 0; i < {}; i++) {{\n", array_size));
        self.indent();

        // Handle scalar/vector/array operand combinations
        let left_access = if left_is_scalar {
            format!("(i == 0 ? signals->{} : 0)", self.sanitize_name(left))
        } else {
            format!("signals->{}[i]", self.sanitize_name(left))
        };

        let right_access = if is_shift_op && right_is_scalar {
            format!("signals->{}", self.sanitize_name(right))
        } else if right_is_scalar {
            format!("(i == 0 ? signals->{} : 0)", self.sanitize_name(right))
        } else {
            format!("signals->{}[i]", self.sanitize_name(right))
        };

        self.write_indented(&format!(
            "signals->{}[i] = {} {} {};\n",
            self.sanitize_name(output),
            left_access,
            op_str,
            right_access
        ));

        self.dedent();
        self.write_indented("}\n");
    }

    /// Generate 64-bit add/sub with proper carry propagation
    fn generate_64bit_add_sub(&mut self, node: &SirNode, op_str: &str, left_width: usize, right_width: usize) {
        let left = &node.inputs[0].signal_id;
        let right = &node.inputs[1].signal_id;
        let output = &node.outputs[0].signal_id;

        // Convert operands to uint64_t
        let left_expr = if left_width <= 32 {
            format!("(uint64_t)signals->{}", self.sanitize_name(left))
        } else {
            format!(
                "((uint64_t)signals->{}[0] | ((uint64_t)signals->{}[1] << 32))",
                self.sanitize_name(left),
                self.sanitize_name(left)
            )
        };

        let right_expr = if right_width <= 32 {
            format!("(uint64_t)signals->{}", self.sanitize_name(right))
        } else {
            format!(
                "((uint64_t)signals->{}[0] | ((uint64_t)signals->{}[1] << 32))",
                self.sanitize_name(right),
                self.sanitize_name(right)
            )
        };

        self.write_indented(&format!(
            "{{ uint64_t _tmp = {} {} {}; signals->{}[0] = (uint32_t)_tmp; signals->{}[1] = (uint32_t)(_tmp >> 32); }}\n",
            left_expr,
            op_str,
            right_expr,
            self.sanitize_name(output),
            self.sanitize_name(output)
        ));
    }

    /// Generate unary operation
    pub fn generate_unary_op(&mut self, node: &SirNode, op: &UnaryOperation) {
        if node.inputs.is_empty() || node.outputs.is_empty() {
            return;
        }

        let input = &node.inputs[0].signal_id;
        let output = &node.outputs[0].signal_id;

        let input_width = self.get_signal_width(input);

        let op_str = match op {
            UnaryOperation::Not => {
                if input_width == 1 {
                    "!" // Logical NOT for booleans
                } else {
                    "~" // Bitwise NOT
                }
            }
            UnaryOperation::Neg | UnaryOperation::FNeg => "-",
            UnaryOperation::RedAnd => "&",
            UnaryOperation::RedOr => "|",
            UnaryOperation::RedXor => "^",
            UnaryOperation::FAbs => "fabs",
            UnaryOperation::FSqrt => "sqrt",
        };

        let is_function = matches!(op, UnaryOperation::FAbs | UnaryOperation::FSqrt);

        if is_function {
            self.write_indented(&format!(
                "signals->{} = {}(signals->{});\n",
                self.sanitize_name(output),
                op_str,
                self.sanitize_name(input)
            ));
        } else {
            self.write_indented(&format!(
                "signals->{} = {}signals->{};\n",
                self.sanitize_name(output),
                op_str,
                self.sanitize_name(input)
            ));
        }
    }

    /// Generate constant assignment
    pub fn generate_constant(&mut self, node: &SirNode, value: u64, width: usize) {
        if node.outputs.is_empty() {
            return;
        }

        let output = &node.outputs[0].signal_id;
        let (base_type, array_size) = self.type_mapper.get_type_for_width(width);

        if let Some(size) = array_size {
            // Wide constant - initialize array
            self.write_indented(&format!("// Constant: {}-bit = {}\n", width, value));
            for i in 0..size {
                let elem_val = if i == 0 {
                    value as u32
                } else if i == 1 && width > 32 {
                    (value >> 32) as u32
                } else {
                    0
                };
                self.write_indented(&format!(
                    "signals->{}[{}] = {};\n",
                    self.sanitize_name(output),
                    i,
                    elem_val
                ));
            }
        } else {
            self.write_indented(&format!(
                "signals->{} = {}({});\n",
                self.sanitize_name(output),
                base_type,
                value
            ));
        }
    }

    /// Generate mux (conditional select) operation
    pub fn generate_mux(&mut self, node: &SirNode) {
        if node.inputs.len() < 3 || node.outputs.is_empty() {
            return;
        }

        let sel = &node.inputs[0].signal_id;
        let true_val = &node.inputs[1].signal_id;
        let false_val = &node.inputs[2].signal_id;
        let output = &node.outputs[0].signal_id;

        let output_width = self.get_signal_width(output);

        if self.uses_array_storage(output_width) {
            // Array mux - element-wise
            let array_size = self.get_array_size(output_width);
            self.write_indented(&format!("for (uint32_t i = 0; i < {}; i++) {{\n", array_size));
            self.indent();
            self.write_indented(&format!(
                "signals->{}[i] = signals->{} ? signals->{}[i] : signals->{}[i];\n",
                self.sanitize_name(output),
                self.sanitize_name(sel),
                self.sanitize_name(true_val),
                self.sanitize_name(false_val)
            ));
            self.dedent();
            self.write_indented("}\n");
        } else {
            // Scalar/vector mux
            self.write_indented(&format!(
                "signals->{} = signals->{} ? signals->{} : signals->{};\n",
                self.sanitize_name(output),
                self.sanitize_name(sel),
                self.sanitize_name(true_val),
                self.sanitize_name(false_val)
            ));
        }
    }

    /// Generate slice (bit extraction) operation
    pub fn generate_slice(&mut self, node: &SirNode, start: usize, end: usize) {
        if node.inputs.is_empty() || node.outputs.is_empty() {
            return;
        }

        let input = &node.inputs[0].signal_id;
        let output = &node.outputs[0].signal_id;

        let (high, low) = if start >= end {
            (start, end)
        } else {
            (end, start)
        };
        let width = high - low + 1;
        let shift = low;

        let mask = if width >= 64 {
            u64::MAX
        } else {
            (1u64 << width) - 1
        };

        // Check if input is a state element
        let is_state = self.module.state_elements.contains_key(input);
        let input_width = self.get_signal_width(input);
        let input_uses_array = self.uses_array_storage(input_width);

        if input_uses_array {
            // For array inputs, extract from the appropriate element
            // Each element is 32 bits, so element_idx = low / 32
            let element_idx = low / 32;
            let bit_in_element = low % 32;

            let input_base = if is_state {
                self.get_register_accessor(input)
            } else {
                format!("signals->{}", self.sanitize_name(input))
            };

            if width <= 32 && bit_in_element + width <= 32 {
                // Slice fits in single element
                let elem_mask = if width >= 32 { 0xFFFFFFFF } else { (1u32 << width) - 1 };
                self.write_indented(&format!(
                    "signals->{} = ({}[{}] >> {}) & 0x{:X};\n",
                    self.sanitize_name(output),
                    input_base,
                    element_idx,
                    bit_in_element,
                    elem_mask
                ));
            } else if width <= 32 {
                // Slice spans two elements
                let bits_from_first = 32 - bit_in_element;
                let bits_from_second = width - bits_from_first;
                let first_mask = (1u32 << bits_from_first) - 1;
                let second_mask = (1u32 << bits_from_second) - 1;
                self.write_indented(&format!(
                    "signals->{} = (({}[{}] >> {}) & 0x{:X}) | (({}[{}] & 0x{:X}) << {});\n",
                    self.sanitize_name(output),
                    input_base,
                    element_idx,
                    bit_in_element,
                    first_mask,
                    input_base,
                    element_idx + 1,
                    second_mask,
                    bits_from_first
                ));
            } else {
                // Multi-element extraction - just extract the first element for now
                // (this handles the common case of extracting a 32-bit chunk)
                self.write_indented(&format!(
                    "signals->{} = {}[{}];\n",
                    self.sanitize_name(output),
                    input_base,
                    element_idx
                ));
            }
        } else {
            // Scalar input - use simple shift and mask
            let input_ref = if is_state {
                self.get_register_accessor(input)
            } else {
                format!("signals->{}", self.sanitize_name(input))
            };

            self.write_indented(&format!(
                "signals->{} = ({} >> {}) & 0x{:X};\n",
                self.sanitize_name(output),
                input_ref,
                shift,
                mask
            ));
        }
    }

    /// Generate concatenation operation
    pub fn generate_concat(&mut self, node: &SirNode) {
        self.concat_recursion_depth += 1;
        if self.concat_recursion_depth > 100 {
            panic!("Concat recursion depth exceeded 100");
        }

        if node.outputs.is_empty() {
            self.concat_recursion_depth -= 1;
            return;
        }

        let output = &node.outputs[0].signal_id;
        let output_width = self.get_signal_width(output);

        // Collect input widths
        let mut input_widths = Vec::new();
        for input in &node.inputs {
            let width = self.get_signal_width(&input.signal_id);
            input_widths.push((input.signal_id.clone(), width));
        }

        if output_width > 128 {
            // Wide concat - use array
            let array_size = output_width.div_ceil(32);
            self.write_indented(&format!("// Concat: {} inputs -> {}-bit output\n", node.inputs.len(), output_width));

            // Initialize to zero
            for i in 0..array_size {
                self.write_indented(&format!("signals->{}[{}] = 0;\n", self.sanitize_name(output), i));
            }

            // Pack inputs (LSB to MSB - reverse order)
            let mut bit_offset = 0;
            for (input_name, width) in input_widths.iter().rev() {
                let element_idx = bit_offset / 32;
                let bit_in_element = bit_offset % 32;

                if *width <= 32 && bit_in_element == 0 {
                    self.write_indented(&format!(
                        "signals->{}[{}] = signals->{};\n",
                        self.sanitize_name(output),
                        element_idx,
                        self.sanitize_name(input_name)
                    ));
                } else if *width <= 32 {
                    self.write_indented(&format!(
                        "signals->{}[{}] |= (signals->{} << {});\n",
                        self.sanitize_name(output),
                        element_idx,
                        self.sanitize_name(input_name),
                        bit_in_element
                    ));
                } else {
                    // Wide input - copy element by element
                    let input_array_size = width.div_ceil(32);
                    for i in 0..input_array_size {
                        let dest_elem = (bit_offset + i * 32) / 32;
                        self.write_indented(&format!(
                            "signals->{}[{}] = signals->{}[{}];\n",
                            self.sanitize_name(output),
                            dest_elem,
                            self.sanitize_name(input_name),
                            i
                        ));
                    }
                }

                bit_offset += width;
            }
        } else if output_width > 64 {
            // Wide vector concat (65-128 bits) - use array access
            // For both C++ and Metal, this range uses arrays (uint32_t[N] or uint4)
            let num_elements = output_width.div_ceil(32);
            let mut components = vec!["0".to_string(); num_elements];
            let mut bit_offset = 0;

            for (input_name, width) in input_widths.iter().rev() {
                let component_idx = bit_offset / 32;
                let bit_in_component = bit_offset % 32;

                if component_idx < num_elements {
                    let input_ref = format!("signals->{}", self.sanitize_name(input_name));
                    if bit_in_component == 0 && *width <= 32 {
                        components[component_idx] = input_ref;
                    } else if *width <= 32 {
                        components[component_idx] = format!("({} | ({} << {}))", components[component_idx], input_ref, bit_in_component);
                    }
                }
                bit_offset += width;
            }

            // Generate array assignment
            for (i, comp) in components.iter().enumerate() {
                self.write_indented(&format!(
                    "signals->{}[{}] = {};\n",
                    self.sanitize_name(output),
                    i,
                    comp
                ));
            }
        } else if output_width > 32 {
            // Medium vector concat (33-64 bits)
            // C++ uses uint64_t (scalar), Metal uses uint2 (indexable vector)
            let is_cpp = matches!(self.type_mapper.target, BackendTarget::Cpp);

            if is_cpp {
                // For C++: Pack into uint64_t using bit shifts
                let mut concat_expr = String::new();
                let mut shift = 0;

                for (input_name, width) in input_widths.iter().rev() {
                    if shift >= output_width {
                        break;
                    }

                    let input_ref = format!("(uint64_t)signals->{}", self.sanitize_name(input_name));

                    if !concat_expr.is_empty() {
                        concat_expr.push_str(" | ");
                    }

                    if shift > 0 {
                        concat_expr.push_str(&format!("({} << {})", input_ref, shift));
                    } else {
                        concat_expr.push_str(&input_ref);
                    }
                    shift += width;
                }

                self.write_indented(&format!(
                    "signals->{} = {};\n",
                    self.sanitize_name(output),
                    concat_expr
                ));
            } else {
                // For Metal: Use vector component access (uint2[0], uint2[1])
                let num_elements = output_width.div_ceil(32);
                let mut components = vec!["0".to_string(); num_elements];
                let mut bit_offset = 0;

                for (input_name, width) in input_widths.iter().rev() {
                    let component_idx = bit_offset / 32;
                    let bit_in_component = bit_offset % 32;

                    if component_idx < num_elements {
                        let input_ref = format!("signals->{}", self.sanitize_name(input_name));
                        if bit_in_component == 0 && *width <= 32 {
                            components[component_idx] = input_ref;
                        } else if *width <= 32 {
                            components[component_idx] = format!("({} | ({} << {}))", components[component_idx], input_ref, bit_in_component);
                        }
                    }
                    bit_offset += width;
                }

                // Generate vector component assignment
                for (i, comp) in components.iter().enumerate() {
                    self.write_indented(&format!(
                        "signals->{}[{}] = {};\n",
                        self.sanitize_name(output),
                        i,
                        comp
                    ));
                }
            }
        } else {
            // Scalar concat (1-32 bits)
            let mut concat_expr = String::new();
            let mut shift = 0;

            for (input_name, width) in input_widths.iter().rev() {
                if shift >= output_width {
                    break;
                }

                let input_ref = format!("signals->{}", self.sanitize_name(input_name));

                if !concat_expr.is_empty() {
                    concat_expr.push_str(" | ");
                }

                if shift > 0 {
                    concat_expr.push_str(&format!("({} << {})", input_ref, shift));
                } else {
                    concat_expr.push_str(&input_ref);
                }
                shift += width;
            }

            self.write_indented(&format!(
                "signals->{} = {};\n",
                self.sanitize_name(output),
                concat_expr
            ));
        }

        self.concat_recursion_depth -= 1;
    }

    /// Generate signal reference (copy from one location to another)
    pub fn generate_signal_ref(&mut self, node: &SirNode, signal: &str) {
        if node.outputs.is_empty() {
            return;
        }

        let output = &node.outputs[0].signal_id;

        // Skip if output is a state element
        if self.module.state_elements.contains_key(output) {
            return;
        }

        let is_input = self.module.inputs.iter().any(|i| i.name == signal);
        let is_state = self.module.state_elements.contains_key(signal);

        let source_location = if is_input {
            format!("inputs->{}", self.sanitize_name(signal))
        } else if is_state {
            self.get_register_accessor(signal)
        } else {
            format!("signals->{}", self.sanitize_name(signal))
        };

        let source_width = self.get_signal_width(signal);
        let output_width = self.get_signal_width(output);
        let source_uses_array = self.uses_array_storage(source_width);
        let output_uses_array = self.uses_array_storage(output_width);

        if source_uses_array && output_uses_array {
            // Array copy
            let array_size = self.get_array_size(output_width);
            self.write_indented(&format!("for (uint32_t i = 0; i < {}; i++) {{\n", array_size));
            self.indent();
            self.write_indented(&format!(
                "signals->{}[i] = {}[i];\n",
                self.sanitize_name(output),
                source_location
            ));
            self.dedent();
            self.write_indented("}\n");
        } else {
            // Scalar/vector copy
            self.write_indented(&format!(
                "signals->{} = {};\n",
                self.sanitize_name(output),
                source_location
            ));
        }
    }

    /// Generate array read operation
    pub fn generate_array_read(&mut self, node: &SirNode) {
        if node.inputs.len() < 2 || node.outputs.is_empty() {
            return;
        }

        let array_signal = &node.inputs[0].signal_id;
        let index_signal = &node.inputs[1].signal_id;
        let output = &node.outputs[0].signal_id;

        let array_location = if self.module.state_elements.contains_key(array_signal) {
            self.get_register_accessor(array_signal)
        } else {
            format!("signals->{}", self.sanitize_name(array_signal))
        };

        self.write_indented(&format!(
            "signals->{} = {}[signals->{}];\n",
            self.sanitize_name(output),
            array_location,
            self.sanitize_name(index_signal)
        ));
    }

    /// Generate array write operation
    pub fn generate_array_write(&mut self, node: &SirNode) {
        if node.inputs.len() < 3 || node.outputs.is_empty() {
            return;
        }

        let old_array = &node.inputs[0].signal_id;
        let index_signal = &node.inputs[1].signal_id;
        let value_signal = &node.inputs[2].signal_id;
        let output = &node.outputs[0].signal_id;

        let array_type = self.get_signal_type(old_array);

        if let Some(SirType::Array(_, size)) = array_type {
            // Copy old array to output
            self.write_indented(&format!("for (uint32_t i = 0; i < {}; i++) {{\n", size));
            self.indent();
            self.write_indented(&format!(
                "signals->{}[i] = signals->{}[i];\n",
                self.sanitize_name(output),
                self.sanitize_name(old_array)
            ));
            self.dedent();
            self.write_indented("}\n");

            // Update the specific element
            self.write_indented(&format!(
                "signals->{}[signals->{}] = signals->{};\n",
                self.sanitize_name(output),
                self.sanitize_name(index_signal),
                self.sanitize_name(value_signal)
            ));
        }
    }

    /// Generate node computation
    pub fn generate_node(&mut self, node: &SirNode) {
        match &node.kind {
            SirNodeKind::BinaryOp(op) => self.generate_binary_op(node, op),
            SirNodeKind::UnaryOp(op) => self.generate_unary_op(node, op),
            SirNodeKind::Constant { value, width } => self.generate_constant(node, *value, *width),
            SirNodeKind::Mux => self.generate_mux(node),
            SirNodeKind::ParallelMux { .. } => self.generate_mux(node),
            SirNodeKind::Slice { start, end } => self.generate_slice(node, *start, *end),
            SirNodeKind::Concat => self.generate_concat(node),
            SirNodeKind::SignalRef { signal } => self.generate_signal_ref(node, signal),
            SirNodeKind::ArrayRead => self.generate_array_read(node),
            SirNodeKind::ArrayWrite => self.generate_array_write(node),
            SirNodeKind::FlipFlop { .. } => {} // Sequential, handled separately
            SirNodeKind::Latch { .. } => {}
            SirNodeKind::Memory { .. } => {}
            SirNodeKind::ClockGate => {}
            SirNodeKind::Reset => {}
        }

        // Handle nodes with multiple outputs
        if node.outputs.len() > 1 {
            let first_output = &node.outputs[0].signal_id;
            let first_width = self.get_signal_width(first_output);
            let uses_array = self.uses_array_storage(first_width);

            for additional_output in &node.outputs[1..] {
                if self.module.state_elements.contains_key(&additional_output.signal_id) {
                    continue;
                }
                if uses_array {
                    // Array copy - element-wise
                    let array_size = self.get_array_size(first_width);
                    self.write_indented(&format!(
                        "for (uint32_t i = 0; i < {}; i++) signals->{}[i] = signals->{}[i];\n",
                        array_size,
                        self.sanitize_name(&additional_output.signal_id),
                        self.sanitize_name(first_output)
                    ));
                } else {
                    // Scalar copy
                    self.write_indented(&format!(
                        "signals->{} = signals->{};\n",
                        self.sanitize_name(&additional_output.signal_id),
                        self.sanitize_name(first_output)
                    ));
                }
            }
        }
    }

    /// Generate combinational evaluation body
    pub fn generate_combinational_body(&mut self) {
        // Use pre-computed topological order
        for node_id in &self.module.sorted_combinational_node_ids.clone() {
            if let Some(node) = self.module.combinational_nodes.iter().find(|n| n.id == *node_id) {
                self.generate_node(node);
            }
        }
    }

    /// Generate sequential (flip-flop) update body
    pub fn generate_sequential_body(&mut self, current_reg: &str, next_reg: &str) {
        // Sort state elements for consistent ordering
        let mut sorted_states: Vec<_> = self.module.state_elements.iter().collect();
        sorted_states.sort_by_key(|(name, _)| *name);

        // Initialize next registers from current
        self.write_indented(&format!("// Initialize {} from {}\n", next_reg, current_reg));
        for (state_name, _) in &sorted_states {
            let sanitized = self.sanitize_name(state_name);
            self.write_indented(&format!(
                "{}->{} = {}->{};\n",
                next_reg, sanitized, current_reg, sanitized
            ));
        }

        // Sort sequential nodes by output name
        let mut sorted_seq_nodes: Vec<_> = self.module.sequential_nodes.iter().collect();
        sorted_seq_nodes.sort_by(|a, b| {
            let a_name = a.outputs.first().map(|o| o.signal_id.as_str()).unwrap_or("");
            let b_name = b.outputs.first().map(|o| o.signal_id.as_str()).unwrap_or("");
            a_name.cmp(b_name)
        });

        // Generate flip-flop updates
        for node in sorted_seq_nodes {
            if let SirNodeKind::FlipFlop { .. } = &node.kind {
                self.generate_flipflop_update(node, current_reg, next_reg);
            }
        }
    }

    /// Generate flip-flop update
    fn generate_flipflop_update(&mut self, node: &SirNode, _current_reg: &str, next_reg: &str) {
        if node.inputs.len() < 2 || node.outputs.is_empty() {
            return;
        }

        let data_signal = &node.inputs[1].signal_id;
        let output_signal = &node.outputs[0].signal_id;

        let sanitized_output = self.sanitize_name(output_signal);
        let sanitized_data = self.sanitize_name(data_signal);

        let output_width = self.get_signal_width(output_signal);

        // Generate width mask if needed
        let mask = if output_width < 32 {
            format!(" & 0x{:X}", (1u64 << output_width) - 1)
        } else {
            String::new()
        };

        self.write_indented(&format!(
            "{}->{} = signals->{}{};\n",
            next_reg,
            sanitized_output,
            sanitized_data,
            mask
        ));
    }
}
