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
    /// Pre-computed signal widths (including intermediate node outputs)
    /// This is the authoritative source for all signal widths - never guess or default to 32
    signal_width_cache: HashMap<String, usize>,
}

impl<'a> SharedCodegen<'a> {
    /// Create a new shared codegen instance
    pub fn new(module: &'a SirModule, target: BackendTarget) -> Self {
        let mut codegen = Self {
            module,
            type_mapper: TypeMapper::new(target),
            output: String::new(),
            indent: 0,
            concat_recursion_depth: 0,
            in_batched_mode: false,
            signal_width_cache: HashMap::new(),
        };
        codegen.build_signal_width_cache();
        codegen
    }

    /// Build the signal width cache by collecting widths from all sources
    /// This must be called before any code generation to ensure accurate widths
    fn build_signal_width_cache(&mut self) {
        // 1. Add widths from declared signals
        for signal in &self.module.signals {
            self.signal_width_cache
                .insert(signal.name.clone(), signal.width);
        }

        // 2. Add widths from inputs
        for input in &self.module.inputs {
            self.signal_width_cache
                .insert(input.name.clone(), input.sir_type.width());
        }

        // 3. Add widths from outputs
        for output in &self.module.outputs {
            self.signal_width_cache
                .insert(output.name.clone(), output.sir_type.width());
        }

        // 4. Add widths from state elements
        // For array types, use sir_type.width() to get the total width,
        // not elem.width which may be just the element width
        for (name, elem) in &self.module.state_elements {
            let width = if let Some(ref sir_type) = elem.sir_type {
                sir_type.width()
            } else {
                elem.width
            };
            self.signal_width_cache.insert(name.clone(), width);
        }

        // 5. Compute widths from combinational nodes (in topological order)
        // Nodes should already be in topological order in the SIR
        for node in &self.module.combinational_nodes {
            let computed_width = self.compute_node_output_width_from_cache(node);
            for output in &node.outputs {
                self.signal_width_cache
                    .insert(output.signal_id.clone(), computed_width);
            }
        }

        // 6. Compute widths from sequential nodes
        // For FlipFlop/Latch outputs that are state elements, keep the state element width
        // (which is the declared width) rather than the computed width from the data input.
        // The data path may be wider, but the flip-flop should truncate to the declared width.
        for node in &self.module.sequential_nodes {
            let computed_width = self.compute_node_output_width_from_cache(node);
            for output in &node.outputs {
                // Only insert if not already present (state elements already have correct widths)
                if !self.signal_width_cache.contains_key(&output.signal_id) {
                    self.signal_width_cache
                        .insert(output.signal_id.clone(), computed_width);
                }
            }
        }
    }

    /// Compute node output width using the cache for input widths
    /// This version uses the cache instead of get_signal_width to avoid issues
    /// during cache building when not all widths are known yet
    fn compute_node_output_width_from_cache(&self, node: &SirNode) -> usize {
        let get_input_width = |idx: usize| -> usize {
            if idx < node.inputs.len() {
                let signal_id = &node.inputs[idx].signal_id;
                // First try the cache, then fall back to type_mapper
                self.signal_width_cache
                    .get(signal_id)
                    .copied()
                    .unwrap_or_else(|| self.type_mapper.get_signal_width(self.module, signal_id))
            } else {
                32 // Malformed node
            }
        };

        match &node.kind {
            SirNodeKind::BinaryOp(op) => {
                let left_width = get_input_width(0);
                let right_width = get_input_width(1);

                match op {
                    BinaryOperation::SMul | BinaryOperation::Mul => left_width + right_width,
                    BinaryOperation::Sar | BinaryOperation::SDiv | BinaryOperation::SMod => {
                        left_width
                    }
                    BinaryOperation::Shl | BinaryOperation::Shr => left_width,
                    BinaryOperation::Div | BinaryOperation::Mod => left_width,
                    BinaryOperation::Add | BinaryOperation::Sub => {
                        std::cmp::max(left_width, right_width)
                    }
                    BinaryOperation::And | BinaryOperation::Or | BinaryOperation::Xor => {
                        std::cmp::max(left_width, right_width)
                    }
                    BinaryOperation::Eq
                    | BinaryOperation::Neq
                    | BinaryOperation::Lt
                    | BinaryOperation::Lte
                    | BinaryOperation::Gt
                    | BinaryOperation::Gte
                    | BinaryOperation::Slt
                    | BinaryOperation::Slte
                    | BinaryOperation::Sgt
                    | BinaryOperation::Sgte => 1,
                    BinaryOperation::FAdd
                    | BinaryOperation::FSub
                    | BinaryOperation::FMul
                    | BinaryOperation::FDiv
                    | BinaryOperation::FMod => std::cmp::max(left_width, right_width),
                    BinaryOperation::FEq
                    | BinaryOperation::FNeq
                    | BinaryOperation::FLt
                    | BinaryOperation::FLte
                    | BinaryOperation::FGt
                    | BinaryOperation::FGte => 1,
                }
            }
            SirNodeKind::UnaryOp(_) => get_input_width(0),
            SirNodeKind::Mux => get_input_width(1),
            SirNodeKind::ParallelMux { result_width, .. } => *result_width,
            SirNodeKind::Concat => node
                .inputs
                .iter()
                .map(|input| {
                    self.signal_width_cache
                        .get(&input.signal_id)
                        .copied()
                        .unwrap_or_else(|| {
                            self.type_mapper
                                .get_signal_width(self.module, &input.signal_id)
                        })
                })
                .sum(),
            SirNodeKind::Slice { start, end } => {
                // Handle both orderings: [start:end] where start can be > or < end
                if *end >= *start {
                    end - start + 1
                } else {
                    start - end + 1
                }
            }
            SirNodeKind::Constant { width, .. } => *width,
            SirNodeKind::SignalRef { signal } => self
                .signal_width_cache
                .get(signal)
                .copied()
                .unwrap_or_else(|| self.type_mapper.get_signal_width(self.module, signal)),
            // FlipFlop inputs: [clock, data], so data is at index 1
            // Latch inputs: [enable, data], so data is at index 1
            SirNodeKind::FlipFlop { .. } | SirNodeKind::Latch { .. } => get_input_width(1),
            SirNodeKind::Memory { width, .. } => *width,
            SirNodeKind::ArrayRead => {
                // ArrayRead output is the ELEMENT width, not the total array width
                // Get the array signal from input 0
                if !node.inputs.is_empty() {
                    let array_signal = &node.inputs[0].signal_id;
                    // Check if this is an array state element
                    if let Some(state_elem) = self.module.state_elements.get(array_signal) {
                        if let Some(SirType::Array(elem_type, _)) = &state_elem.sir_type {
                            return elem_type.width();
                        }
                    }
                    // Check if the signal comes from a SignalRef to an array state element
                    for comb_node in &self.module.combinational_nodes {
                        if let SirNodeKind::SignalRef { signal } = &comb_node.kind {
                            if comb_node
                                .outputs
                                .iter()
                                .any(|o| o.signal_id == *array_signal)
                            {
                                if let Some(state_elem) = self.module.state_elements.get(signal) {
                                    if let Some(SirType::Array(elem_type, _)) = &state_elem.sir_type
                                    {
                                        return elem_type.width();
                                    }
                                }
                            }
                        }
                    }
                }
                // Fallback to input width (may be incorrect for arrays)
                get_input_width(0)
            }
            SirNodeKind::ArrayWrite => get_input_width(0),
            SirNodeKind::ClockGate | SirNodeKind::Reset => 1,
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

    /// Get signal width - uses pre-computed cache for accurate widths
    /// This is the authoritative source for all signal widths during code generation
    pub fn get_signal_width(&self, signal_name: &str) -> usize {
        // First check the pre-computed cache (includes intermediate node outputs)
        if let Some(&width) = self.signal_width_cache.get(signal_name) {
            return width;
        }
        // Fall back to type_mapper for signals not in cache (shouldn't happen normally)
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

    /// Create the appropriate SirType for a given width, using array types for wide signals
    /// This ensures signals that require array storage are declared with array types in structs.
    pub fn create_sir_type_for_width(&self, width: usize) -> SirType {
        if self.uses_array_storage(width) {
            // Wide signal - needs array storage
            let array_size = width.div_ceil(32);
            SirType::Array(Box::new(SirType::Bits(32)), array_size)
        } else {
            SirType::Bits(width)
        }
    }

    /// Check if a given bit width uses vector storage (Metal only: uint2/uint4)
    /// For C++, always returns false (no vector types used)
    /// For Metal: 33-64 bits uses uint2, 65-128 bits uses uint4
    pub fn uses_vector_storage(&self, width: usize) -> bool {
        match self.type_mapper.target {
            BackendTarget::Cpp => false,
            BackendTarget::Metal => width > 32 && width <= 128,
        }
    }

    /// Get the number of 32-bit components in a Metal vector type
    /// Returns 1 for scalar (<=32 bits), 2 for uint2, 4 for uint4, or array size for >128 bits
    pub fn get_vector_size(&self, width: usize) -> usize {
        match self.type_mapper.target {
            BackendTarget::Cpp => 1, // C++ doesn't use vector types
            BackendTarget::Metal => {
                if width <= 32 {
                    1
                } else if width <= 64 {
                    2
                } else if width <= 128 {
                    4
                } else {
                    width.div_ceil(32) // Array
                }
            }
        }
    }

    /// Get the component accessor for a Metal vector index (0='x', 1='y', 2='z', 3='w')
    pub fn get_vector_component(&self, idx: usize) -> &'static str {
        match idx {
            0 => ".x",
            1 => ".y",
            2 => ".z",
            3 => ".w",
            _ => panic!("Invalid vector component index: {}", idx),
        }
    }

    /// Get the number of 32-bit array elements for a given width
    pub fn get_array_size(&self, width: usize) -> usize {
        width.div_ceil(32)
    }

    /// Check if a signal is stored as an array of vectors in Metal.
    /// This happens when we have Array(Bits(N), M) where 32 < N <= 128.
    /// In Metal, each element becomes uint2 (for 33-64 bits) or uint4 (for 65-128 bits).
    /// Returns (is_array_of_vectors, element_bits, vector_size) where:
    /// - element_bits is the width of each array element
    /// - vector_size is 2 for uint2, 4 for uint4
    pub fn is_array_of_vectors(&self, signal: &str) -> (bool, usize, usize) {
        // Only relevant for Metal
        if self.type_mapper.target != BackendTarget::Metal {
            return (false, 0, 0);
        }

        // Check if this signal comes from an array-type state element
        if let Some(state_elem) = self.module.state_elements.get(signal) {
            if let Some(SirType::Array(elem_type, _)) = &state_elem.sir_type {
                let elem_width = elem_type.width();
                // Check if element uses vector storage (33-128 bits in Metal)
                if elem_width > 32 && elem_width <= 128 {
                    let vector_size = if elem_width <= 64 { 2 } else { 4 };
                    return (true, elem_width, vector_size);
                }
            }
        }

        // Also check if this is a signal that was copied from an array of vectors
        // We can detect this by checking the signal width and if it's a multiple of vector element size
        // For now, use a heuristic: if signal name starts with "node_" and ends with "_out",
        // check if there's a SignalRef that points to an array state element
        // This is complex, so we'll rely on the state element check above for now

        (false, 0, 0)
    }

    /// Resolve a signal to its source array if it came from a SignalRef to an array state element.
    /// Returns the source signal name if found, otherwise the original signal name.
    pub fn resolve_to_array_source(&self, signal: &str) -> Option<String> {
        // Look for a SignalRef node that outputs this signal
        for node in &self.module.combinational_nodes {
            if let SirNodeKind::SignalRef { signal: source } = &node.kind {
                if node.outputs.iter().any(|o| o.signal_id == signal) {
                    // Check if source is an array state element
                    if let Some(state_elem) = self.module.state_elements.get(source) {
                        if let Some(SirType::Array(_, _)) = &state_elem.sir_type {
                            return Some(source.clone());
                        }
                    }
                }
            }
        }
        None
    }

    /// Get the accessor string for a register
    /// In batched mode, returns `local_X`, otherwise returns `registers->X`
    pub fn get_register_accessor(&self, name: &str) -> String {
        let sanitized = self.sanitize_name(name);
        if self.in_batched_mode {
            // Check if this is an array type - array registers don't have local copies
            // First try sir_type, but ALWAYS fall back to width-based check for non-array types
            // because sir_type might be Bits(N) for wide signals that need array storage
            let width = self.get_signal_width(name);
            let (_, array_size_by_width) = self.type_mapper.get_type_for_width(width);
            let width_needs_array = array_size_by_width.is_some();

            let is_array = if let Some(state_elem) = self.module.state_elements.get(name) {
                if let Some(ref sir_type) = state_elem.sir_type {
                    // If sir_type says Array, it's definitely an array
                    // If sir_type is NOT Array but width requires array storage, treat as array
                    matches!(sir_type, SirType::Array(_, _)) || width_needs_array
                } else {
                    // No sir_type, use width-based check
                    width_needs_array
                }
            } else {
                // Not a state element, use width-based check
                width_needs_array
            };

            if is_array {
                // Array type - access directly from registers buffer
                format!("registers->{}", sanitized)
            } else {
                // Scalar type - use local variable
                format!("local_{}", sanitized)
            }
        } else {
            format!("registers->{}", sanitized)
        }
    }

    /// Resolve array source through SignalRef chains
    /// If a signal comes from a SignalRef to an array state element, return the state element name
    fn resolve_array_source(&self, signal: &str) -> String {
        // Check if this signal is produced by a SignalRef node pointing to an array state element
        for node in &self.module.combinational_nodes {
            if let SirNodeKind::SignalRef { signal: source } = &node.kind {
                // Check if this node outputs to our signal
                if node.outputs.iter().any(|o| o.signal_id == signal) {
                    // Check if source is an array-type state element
                    if let Some(state_elem) = self.module.state_elements.get(source) {
                        if let Some(ref sir_type) = state_elem.sir_type {
                            if matches!(sir_type, SirType::Array(_, _)) {
                                return source.clone();
                            }
                        }
                    }
                }
            }
        }
        signal.to_string()
    }

    /// Get the array type for a signal, resolving through SignalRef chains if needed
    /// Returns Some(SirType::Array) if the signal is an array, None otherwise
    fn get_array_type_for_signal(&self, signal: &str) -> Option<SirType> {
        // Direct state element lookup
        if let Some(state_elem) = self.module.state_elements.get(signal) {
            if let Some(ref sir_type) = state_elem.sir_type {
                if matches!(sir_type, SirType::Array(_, _)) {
                    return Some(sir_type.clone());
                }
            }
        }

        // Check if signal comes from a SignalRef to an array state element
        for node in &self.module.combinational_nodes {
            if let SirNodeKind::SignalRef { signal: source } = &node.kind {
                if node.outputs.iter().any(|o| o.signal_id == signal) {
                    if let Some(state_elem) = self.module.state_elements.get(source) {
                        if let Some(ref sir_type) = state_elem.sir_type {
                            if matches!(sir_type, SirType::Array(_, _)) {
                                return Some(sir_type.clone());
                            }
                        }
                    }
                }
            }
        }

        None
    }

    /// Check if a signal (by output name) is produced by an array-related node
    /// Returns Some(array_type) if it's from SignalRef to array or ArrayWrite
    fn get_array_type_for_signal_output(&self, output_name: &str) -> Option<SirType> {
        // Find the node that produces this output
        for node in &self.module.combinational_nodes {
            for output in &node.outputs {
                if output.signal_id == output_name {
                    match &node.kind {
                        // SignalRef to array state element
                        SirNodeKind::SignalRef { signal } => {
                            if let Some(state_elem) = self.module.state_elements.get(signal) {
                                if let Some(ref sir_type) = state_elem.sir_type {
                                    if matches!(sir_type, SirType::Array(_, _)) {
                                        return Some(sir_type.clone());
                                    }
                                }
                            }
                        }
                        // ArrayWrite outputs same type as input array
                        SirNodeKind::ArrayWrite => {
                            if !node.inputs.is_empty() {
                                let array_signal = &node.inputs[0].signal_id;
                                return self.get_array_type_for_signal(array_signal);
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
        None
    }

    /// Check if a signal is used as the array source (first input) for any ArrayRead node
    fn is_array_read_source(&self, signal: &str) -> bool {
        for node in &self.module.combinational_nodes {
            if matches!(node.kind, SirNodeKind::ArrayRead)
                && !node.inputs.is_empty()
                && node.inputs[0].signal_id == signal
            {
                return true;
            }
        }
        false
    }

    /// Get the array size for a signal that is used as ArrayRead source
    /// Returns the array size from the ArrayRead operation's context, or None if not found
    fn get_array_read_source_size(&self, signal: &str) -> Option<usize> {
        for node in &self.module.combinational_nodes {
            if matches!(node.kind, SirNodeKind::ArrayRead)
                && !node.inputs.is_empty()
                && node.inputs[0].signal_id == signal
            {
                // Try to get array size from the source signal's type
                if let Some(SirType::Array(_, size)) = self.get_array_type_for_signal(signal) {
                    return Some(size);
                }
                // Fall back to checking the source of this signal
                // via SignalRef chain
                let resolved = self.resolve_array_source(signal);
                if let Some(state_elem) = self.module.state_elements.get(&resolved) {
                    if let Some(SirType::Array(_, size)) = &state_elem.sir_type {
                        return Some(*size);
                    }
                }
            }
        }
        None
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
            self.write_indented(&format!(
                "{} {}{};\n",
                base_type, sanitized_name, array_suffix
            ));
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
            // Use sir_type from state element if available (for arrays), else look up signal
            let default_type = SirType::Bits(elem.width);
            let sir_type = if let Some(ref t) = elem.sir_type {
                t
            } else {
                let found_signal = self.module.signals.iter().find(|s| &s.name == *name);
                found_signal.map(|s| &s.sir_type).unwrap_or(&default_type)
            };

            let (base_type, array_suffix) = self.type_mapper.get_struct_field_parts(sir_type);
            self.write_indented(&format!(
                "{} {}{};\n",
                base_type,
                self.sanitize_name(name),
                array_suffix
            ));
        }

        // Also add flip-flop outputs that aren't state elements
        // BUG #254 FIX: Iterate over ALL outputs of flip-flops, not just first()
        let mut ff_output_names: HashSet<String> = HashSet::new();
        for node in &self.module.sequential_nodes {
            if let SirNodeKind::FlipFlop { .. } = &node.kind {
                for output in &node.outputs {
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
        // Use cached width to ensure consistency with node output widths
        for output in &self.module.outputs {
            if self.module.state_elements.contains_key(&output.name) {
                continue;
            }
            let sanitized_name = self.sanitize_name(&output.name);
            if added_names.insert(sanitized_name.clone()) {
                // Use cache width which may differ from declared width for node outputs
                let width = self.get_signal_width(&output.name);
                let sir_type = SirType::Bits(width);
                self.generate_signal_field(&output.name, &sir_type);
            }
        }

        // Pre-collect signals that are used as array sources in ArrayRead nodes
        // These MUST be declared as arrays regardless of width
        let mut array_source_signals: HashSet<String> = HashSet::new();
        for node in &self.module.combinational_nodes {
            if matches!(node.kind, SirNodeKind::ArrayRead) && !node.inputs.is_empty() {
                let array_signal = &node.inputs[0].signal_id;
                // Only track if not a state element (state elements are in Registers)
                if !self.module.state_elements.contains_key(array_signal)
                    && !input_names.contains(array_signal)
                {
                    array_source_signals.insert(array_signal.clone());
                }
            }
        }

        // Add intermediate signals
        // Note: Use cached width instead of declared sir_type to ensure consistency
        // with how nodes compute their output widths (especially for concat nodes)
        // However, for array-producing nodes, preserve the array type
        for signal in &self.module.signals {
            let sanitized_name = self.sanitize_name(&signal.name);
            if !signal.is_state
                && !input_names.contains(&signal.name)
                && added_names.insert(sanitized_name.clone())
            {
                // Check if this signal is used as an array source - force array type
                let is_array_source = array_source_signals.contains(&signal.name);

                let sir_type = if is_array_source {
                    // This signal is used as an array source - must be array type
                    self.get_array_type_for_signal(&signal.name)
                        .unwrap_or_else(|| {
                            // Create a minimum array if we can't determine the type
                            SirType::Array(Box::new(SirType::Bits(32)), 16)
                        })
                } else {
                    // Check if this signal is produced by an array-related node
                    // Fall back to create_sir_type_for_width for proper array type handling
                    self.get_array_type_for_signal_output(&signal.name)
                        .unwrap_or_else(|| {
                            let width = self.get_signal_width(&signal.name);
                            self.create_sir_type_for_width(width)
                        })
                };
                self.generate_signal_field(&signal.name, &sir_type);
            }
        }

        // Generate struct fields for combinational node outputs
        // Widths are retrieved from the pre-computed signal_width_cache
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

                // For array-related nodes, preserve the array type
                // All fallback cases use create_sir_type_for_width for proper array handling
                // Also check if this output is used as an array source in ArrayRead
                let is_array_source = array_source_signals.contains(signal_id);

                let sir_type = if is_array_source {
                    // This signal is used as an array source - must be array type
                    self.get_array_type_for_signal(signal_id)
                        .unwrap_or_else(|| {
                            // Create a minimum array if we can't determine the type
                            SirType::Array(Box::new(SirType::Bits(32)), 16)
                        })
                } else {
                    match &node.kind {
                        // ArrayWrite outputs should be array type (same as input array)
                        SirNodeKind::ArrayWrite => {
                            if !node.inputs.is_empty() {
                                let array_signal = &node.inputs[0].signal_id;
                                self.get_array_type_for_signal(array_signal)
                                    .unwrap_or_else(|| {
                                        self.create_sir_type_for_width(
                                            self.get_signal_width(signal_id),
                                        )
                                    })
                            } else {
                                self.create_sir_type_for_width(self.get_signal_width(signal_id))
                            }
                        }
                        // SignalRef to array state element should preserve array type
                        SirNodeKind::SignalRef { signal } => {
                            if let Some(state_elem) = self.module.state_elements.get(signal) {
                                if let Some(ref sir_type) = state_elem.sir_type {
                                    if matches!(sir_type, SirType::Array(_, _)) {
                                        sir_type.clone()
                                    } else {
                                        self.create_sir_type_for_width(
                                            self.get_signal_width(signal_id),
                                        )
                                    }
                                } else {
                                    self.create_sir_type_for_width(self.get_signal_width(signal_id))
                                }
                            } else {
                                // Not a state element - fall back to width-based type
                                // This handles intermediate signals that reference other wide signals
                                self.create_sir_type_for_width(self.get_signal_width(signal_id))
                            }
                        }
                        // Default: use cached width, with array type for wide signals
                        _ => {
                            let width = self.get_signal_width(signal_id);
                            self.create_sir_type_for_width(width)
                        }
                    }
                };
                self.generate_signal_field(signal_id, &sir_type);
            }
        }

        // Collect signals from node inputs (these reference other node outputs)
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

                // Check if this signal is used as an array source - if so, force array type
                let sir_type = if array_source_signals.contains(signal_id) {
                    // This signal is used as an array source - get its array type
                    // Try to find the array type from the producer node or state element
                    self.get_array_type_for_signal(signal_id)
                        .unwrap_or_else(|| {
                            // Fall back to creating array based on width
                            // Use a minimum array size that can be subscripted
                            let width = self.get_signal_width(signal_id);
                            // If width is small, we still need an array - use the width as element count
                            if width <= 32 {
                                // Can't subscript a scalar - this is likely an error in the SIR
                                // For now, create a small array
                                SirType::Array(Box::new(SirType::Bits(32)), 16)
                            } else {
                                self.create_sir_type_for_width(width)
                            }
                        })
                } else {
                    // Use cached width from the producing node
                    // Use array type for wide signals that require array storage
                    let width = self.get_signal_width(signal_id);
                    self.create_sir_type_for_width(width)
                };
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

                // Use cached width from the producing node
                // Use array type for wide signals that require array storage
                let width = self.get_signal_width(signal_id);
                let sir_type = self.create_sir_type_for_width(width);
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
            self.generate_wide_binary_op(node, op, op_str);
            return;
        }

        // Check for widening multiplication where output needs different storage than inputs
        // This happens when Mul produces a result wider than either operand (e.g., 32x32->64, 64x64->128)
        let is_mul = matches!(op, BinaryOperation::Mul);
        let max_input_width = std::cmp::max(left_width, right_width);
        if is_mul && output_width > max_input_width {
            self.generate_widening_multiply(node, left_width, right_width, output_width);
            return;
        }

        // For 33-64 bit add/sub, use uint64_t arithmetic for proper carry
        let is_add_sub = matches!(op, BinaryOperation::Add | BinaryOperation::Sub);
        if is_add_sub && output_width > 32 && output_width <= 64 {
            self.generate_64bit_add_sub(node, op_str, left_width, right_width);
            return;
        }

        // Check for signed comparison operations - need to cast to signed types
        let is_signed_comparison = matches!(
            op,
            BinaryOperation::Slt
                | BinaryOperation::Slte
                | BinaryOperation::Sgt
                | BinaryOperation::Sgte
        );

        if is_signed_comparison {
            self.generate_signed_comparison(node, op_str, left_width, right_width);
            return;
        }

        // Check for signed arithmetic operations - need to cast to signed types
        let is_signed_arithmetic = matches!(
            op,
            BinaryOperation::SMul
                | BinaryOperation::SDiv
                | BinaryOperation::SMod
                | BinaryOperation::Sar
        );

        if is_signed_arithmetic {
            self.generate_signed_arithmetic(node, op, left_width, right_width, output_width);
            return;
        }

        // Check for floating-point operations - need to cast bit patterns to float types
        let is_fp_arithmetic = matches!(
            op,
            BinaryOperation::FAdd
                | BinaryOperation::FSub
                | BinaryOperation::FMul
                | BinaryOperation::FDiv
                | BinaryOperation::FMod
        );

        if is_fp_arithmetic {
            self.generate_fp_binary_op(node, op, left_width, right_width, output_width);
            return;
        }

        // Check for floating-point comparisons
        let is_fp_comparison = matches!(
            op,
            BinaryOperation::FEq
                | BinaryOperation::FNeq
                | BinaryOperation::FLt
                | BinaryOperation::FLte
                | BinaryOperation::FGt
                | BinaryOperation::FGte
        );

        if is_fp_comparison {
            self.generate_fp_comparison(node, op, left_width, right_width);
            return;
        }

        // Check if either operand uses vector storage (Metal uint2/uint4)
        // Vector comparisons produce vector bool results, so we need to compare as scalars
        let left_uses_vector = self.uses_vector_storage(left_width);
        let right_uses_vector = self.uses_vector_storage(right_width);
        let is_comparison = matches!(
            op,
            BinaryOperation::Eq
                | BinaryOperation::Neq
                | BinaryOperation::Lt
                | BinaryOperation::Lte
                | BinaryOperation::Gt
                | BinaryOperation::Gte
        );

        if is_comparison && (left_uses_vector || right_uses_vector) {
            // For comparisons involving vector types, convert to 64-bit scalars first
            let left_expr = if left_uses_vector {
                format!(
                    "((uint64_t)signals->{}.x | ((uint64_t)signals->{}.y << 32))",
                    self.sanitize_name(left),
                    self.sanitize_name(left)
                )
            } else {
                format!("(uint64_t)signals->{}", self.sanitize_name(left))
            };
            let right_expr = if right_uses_vector {
                format!(
                    "((uint64_t)signals->{}.x | ((uint64_t)signals->{}.y << 32))",
                    self.sanitize_name(right),
                    self.sanitize_name(right)
                )
            } else {
                format!("(uint64_t)signals->{}", self.sanitize_name(right))
            };
            self.write_indented(&format!(
                "signals->{} = {} {} {};\n",
                self.sanitize_name(output),
                left_expr,
                op_str,
                right_expr
            ));
            return;
        }

        // Guard unsigned division/modulo against division by zero (C++ UB)
        let is_div_mod = matches!(op, BinaryOperation::Div | BinaryOperation::Mod);
        if is_div_mod {
            if output_width < 32 {
                let mask = (1u64 << output_width) - 1;
                self.write_indented(&format!(
                    "signals->{} = ((signals->{} != 0) ? (signals->{} {} signals->{}) : 0) & 0x{:X};\n",
                    self.sanitize_name(output),
                    self.sanitize_name(right),
                    self.sanitize_name(left),
                    op_str,
                    self.sanitize_name(right),
                    mask
                ));
            } else {
                self.write_indented(&format!(
                    "signals->{} = (signals->{} != 0) ? (signals->{} {} signals->{}) : 0;\n",
                    self.sanitize_name(output),
                    self.sanitize_name(right),
                    self.sanitize_name(left),
                    op_str,
                    self.sanitize_name(right)
                ));
            }
            return;
        }

        // Standard scalar operation
        // BUG #275 FIX: Mask arithmetic results to output_width to prevent 32-bit overflow/underflow
        // from poisoning subsequent comparisons. In hardware, all signals have fixed widths and
        // arithmetic naturally wraps. But in C++ with uint32_t storage, sub/add/shl can produce
        // values wider than the signal's actual width.
        let needs_mask = output_width < 32 && !is_comparison;
        if needs_mask {
            let mask = (1u64 << output_width) - 1;
            self.write_indented(&format!(
                "signals->{} = (signals->{} {} signals->{}) & 0x{:X};\n",
                self.sanitize_name(output),
                self.sanitize_name(left),
                op_str,
                self.sanitize_name(right),
                mask
            ));
        } else {
            self.write_indented(&format!(
                "signals->{} = signals->{} {} signals->{};\n",
                self.sanitize_name(output),
                self.sanitize_name(left),
                op_str,
                self.sanitize_name(right)
            ));
        }
    }

    /// Generate signed comparison with proper type casting
    fn generate_signed_comparison(
        &mut self,
        node: &SirNode,
        op_str: &str,
        left_width: usize,
        right_width: usize,
    ) {
        let left = &node.inputs[0].signal_id;
        let right = &node.inputs[1].signal_id;
        let output = &node.outputs[0].signal_id;

        let left_sanitized = self.sanitize_name(left);
        let right_sanitized = self.sanitize_name(right);

        // Check array storage for each operand
        let left_uses_array = self.uses_array_storage(left_width);
        let right_uses_array = self.uses_array_storage(right_width);

        // Generate signed expressions for comparison
        // Using needs_64bit=true for comparisons involving 33-64 bit values
        let needs_64bit = left_width > 32 || right_width > 32;
        let left_expr = self.generate_signed_operand_expr(
            &left_sanitized,
            left_width,
            left_uses_array,
            needs_64bit,
        );
        let right_expr = self.generate_signed_operand_expr(
            &right_sanitized,
            right_width,
            right_uses_array,
            needs_64bit,
        );

        self.write_indented(&format!(
            "signals->{} = {} {} {};\n",
            self.sanitize_name(output),
            left_expr,
            op_str,
            right_expr
        ));
    }

    /// Generate signed arithmetic operation with proper type casting
    /// Handles SMul, SDiv, SMod, and Sar (arithmetic shift right)
    fn generate_signed_arithmetic(
        &mut self,
        node: &SirNode,
        op: &BinaryOperation,
        left_width: usize,
        right_width: usize,
        output_width: usize,
    ) {
        let left = &node.inputs[0].signal_id;
        let right = &node.inputs[1].signal_id;
        let output = &node.outputs[0].signal_id;

        let left_sanitized = self.sanitize_name(left);
        let right_sanitized = self.sanitize_name(right);
        let output_sanitized = self.sanitize_name(output);

        // For signed multiply, the result can be up to left_width + right_width bits
        // For SMul of 16-bit x 32-bit, we need 48-bit result, use int64_t
        let needs_64bit = matches!(op, BinaryOperation::SMul) && (left_width + right_width > 32);

        // Calculate the actual output width for SMul (may differ from stored signal width)
        // SMul output is left_width + right_width bits
        // For Sar, the output width is the same as the left operand width
        // For SDiv/SMod, output width is typically the left operand width
        let actual_output_width = match op {
            BinaryOperation::SMul => left_width + right_width,
            BinaryOperation::Sar | BinaryOperation::SDiv | BinaryOperation::SMod => left_width,
            _ => output_width,
        };

        // Determine the operator string
        let op_str = match op {
            BinaryOperation::SMul => "*",
            BinaryOperation::SDiv => "/",
            BinaryOperation::SMod => "%",
            BinaryOperation::Sar => ">>",
            _ => unreachable!(),
        };

        // Check array storage for each operand
        // For C++: width > 64 uses arrays, 33-64 uses uint64_t scalar, <= 32 uses uint32_t
        // For Metal: width > 128 uses arrays
        let left_uses_array = self.uses_array_storage(left_width);
        let right_uses_array = self.uses_array_storage(right_width);
        let output_uses_array = self.uses_array_storage(actual_output_width);

        // Generate signed expression for left operand
        let left_expr = self.generate_signed_operand_expr(
            &left_sanitized,
            left_width,
            left_uses_array,
            needs_64bit,
        );

        // For shift operations, the shift amount doesn't need sign extension
        let right_expr = if matches!(op, BinaryOperation::Sar) {
            format!("signals->{}", right_sanitized)
        } else {
            self.generate_signed_operand_expr(
                &right_sanitized,
                right_width,
                right_uses_array,
                needs_64bit,
            )
        };

        // Guard signed division/modulo against division by zero (C++ UB)
        let is_signed_div_mod = matches!(op, BinaryOperation::SDiv | BinaryOperation::SMod);
        if is_signed_div_mod {
            // Check the raw right operand signal for zero before performing the operation
            self.write_indented(&format!(
                "signals->{} = (signals->{} != 0) ? (uint32_t)({} {} {}) : 0;\n",
                output_sanitized, right_sanitized, left_expr, op_str, right_expr
            ));
            return;
        }

        // Generate the operation with proper casting
        if output_uses_array {
            // Output uses array storage (> 64 bits for C++, > 128 bits for Metal)
            self.write_indented(&format!(
                "{{ int64_t _tmp = {} {} {}; signals->{}[0] = (uint32_t)_tmp; signals->{}[1] = (uint32_t)((uint64_t)_tmp >> 32); }}\n",
                left_expr,
                op_str,
                right_expr,
                output_sanitized,
                output_sanitized
            ));
        } else if actual_output_width > 32 && actual_output_width <= 64 {
            // 33-64 bit output: stored as uint64_t scalar (C++) or uint2 vector (Metal)
            let is_metal = matches!(self.type_mapper.target, BackendTarget::Metal);

            if is_metal {
                // Metal: Pack 64-bit result into uint2 (.x = low 32 bits, .y = high 32 bits)
                let mask = if actual_output_width == 64 {
                    "".to_string()
                } else {
                    format!(" & 0x{:X}ULL", (1u64 << actual_output_width) - 1)
                };
                self.write_indented(&format!(
                    "{{ uint64_t _tmp = (uint64_t)({} {} {}){}; signals->{}.x = (uint)_tmp; signals->{}.y = (uint)(_tmp >> 32); }}\n",
                    left_expr, op_str, right_expr, mask,
                    output_sanitized, output_sanitized
                ));
            } else if actual_output_width == 64 {
                // C++: Direct assignment to uint64_t
                self.write_indented(&format!(
                    "signals->{} = (uint64_t)({} {} {});\n",
                    output_sanitized, left_expr, op_str, right_expr
                ));
            } else {
                // C++: Mask to actual width to ensure upper bits don't affect sign extension
                let mask = (1u64 << actual_output_width) - 1;
                self.write_indented(&format!(
                    "signals->{} = (uint64_t)({} {} {}) & 0x{:X}ULL;\n",
                    output_sanitized, left_expr, op_str, right_expr, mask
                ));
            }
        } else if actual_output_width < 32 {
            // Sub-32-bit output: mask to prevent overflow from polluting upper bits
            let mask = (1u64 << actual_output_width) - 1;
            self.write_indented(&format!(
                "signals->{} = (uint32_t)({} {} {}) & 0x{:X};\n",
                output_sanitized, left_expr, op_str, right_expr, mask
            ));
        } else {
            // 32-bit output
            self.write_indented(&format!(
                "signals->{} = (uint32_t)({} {} {});\n",
                output_sanitized, left_expr, op_str, right_expr
            ));
        }
    }

    /// Generate a signed operand expression for reading a signal with proper sign extension
    fn generate_signed_operand_expr(
        &self,
        sanitized: &str,
        width: usize,
        uses_array: bool,
        needs_64bit: bool,
    ) -> String {
        if uses_array {
            // > 64 bits (C++) or > 128 bits (Metal): stored as uint32_t[N]
            // Combine low 64 bits into int64_t for signed operations
            format!(
                "(int64_t)((uint64_t)signals->{}[0] | ((uint64_t)signals->{}[1] << 32))",
                sanitized, sanitized
            )
        } else if self.uses_vector_storage(width) {
            // Metal vector types (uint2/uint4): need to extract components
            if width <= 64 {
                // uint2: combine .x and .y into 64-bit value for signed operations
                let shift = 64 - width;
                format!(
                    "((int64_t)(((uint64_t)signals->{}.x | ((uint64_t)signals->{}.y << 32)) << {}) >> {})",
                    sanitized, sanitized, shift, shift
                )
            } else {
                // uint4: combine first two components for 64-bit signed operation
                format!(
                    "(int64_t)((uint64_t)signals->{}.x | ((uint64_t)signals->{}.y << 32))",
                    sanitized, sanitized
                )
            }
        } else if width == 64 {
            // Exactly 64 bits: just cast to int64_t
            format!("(int64_t)signals->{}", sanitized)
        } else if width > 32 && width < 64 {
            // 33-63 bits: stored as uint64_t scalar (C++ only, Metal uses vectors)
            // Left shift to put sign bit at position 63, then arithmetic right shift to sign-extend
            let shift = 64 - width;
            format!(
                "((int64_t)(signals->{} << {}) >> {})",
                sanitized, shift, shift
            )
        } else if width == 32 {
            // 32-bit: cast to int32_t (or int64_t if needed for multiply)
            if needs_64bit {
                format!("(int64_t)(int32_t)signals->{}", sanitized)
            } else {
                format!("(int32_t)signals->{}", sanitized)
            }
        } else {
            // Narrower than 32 bits: sign extend from actual width
            let shift = 32 - width;
            if needs_64bit {
                format!(
                    "(int64_t)((int32_t)(signals->{} << {}) >> {})",
                    sanitized, shift, shift
                )
            } else {
                format!(
                    "((int32_t)(signals->{} << {}) >> {})",
                    sanitized, shift, shift
                )
            }
        }
    }

    /// Generate floating-point binary operation with proper type casting
    /// FP operations require reinterpreting bit patterns as float/half types
    fn generate_fp_binary_op(
        &mut self,
        node: &SirNode,
        op: &BinaryOperation,
        _left_width: usize,
        _right_width: usize,
        output_width: usize,
    ) {
        let left = &node.inputs[0].signal_id;
        let right = &node.inputs[1].signal_id;
        let output = &node.outputs[0].signal_id;

        let left_sanitized = self.sanitize_name(left);
        let right_sanitized = self.sanitize_name(right);
        let output_sanitized = self.sanitize_name(output);

        let is_metal = matches!(self.type_mapper.target, BackendTarget::Metal);

        // Determine the operator string
        let op_str = match op {
            BinaryOperation::FAdd => "+",
            BinaryOperation::FSub => "-",
            BinaryOperation::FMul => "*",
            BinaryOperation::FDiv => "/",
            BinaryOperation::FMod => "%", // Note: fmod for C++
            _ => unreachable!(),
        };

        // Determine float type based on OUTPUT width (semantic type)
        // FP16 = 16-bit half, FP32 = 32-bit float
        // We use output_width because SIR may widen operands to 32-bit for storage
        // but the semantic FP width is preserved in the output
        let fp_width = output_width;

        if is_metal {
            // Metal Shading Language: use as_type<> for bit reinterpretation
            if fp_width <= 16 {
                // FP16: reinterpret ushort as half, operate, reinterpret back
                self.write_indented(&format!(
                    "signals->{} = as_type<ushort>(as_type<half>((ushort)signals->{}) {} as_type<half>((ushort)signals->{}));\n",
                    output_sanitized,
                    left_sanitized,
                    op_str,
                    right_sanitized
                ));
            } else {
                // FP32: reinterpret uint as float, operate, reinterpret back
                self.write_indented(&format!(
                    "signals->{} = as_type<uint>(as_type<float>(signals->{}) {} as_type<float>(signals->{}));\n",
                    output_sanitized,
                    left_sanitized,
                    op_str,
                    right_sanitized
                ));
            }
        } else {
            // C++: use union-based reinterpretation or memcpy
            // For portability, we use a simple cast approach that most compilers optimize well
            if fp_width <= 16 {
                // FP16 in C++ - need to convert to float, operate, convert back
                // Most C++ compilers don't have native half type, so we use fp16 library functions
                // For now, use the __fp16 type if available (clang/gcc), otherwise fall back to float
                self.write_indented("{\n");
                self.indent();
                self.write_indented("// FP16 operation: convert to float, operate, convert back\n");
                self.write_indented(&format!("uint32_t _a = signals->{};\n", left_sanitized));
                self.write_indented(&format!("uint32_t _b = signals->{};\n", right_sanitized));
                // Use the fp16 to fp32 conversion formula
                self.write_indented("float _fa = _fp16_to_fp32(_a);\n");
                self.write_indented("float _fb = _fp16_to_fp32(_b);\n");
                self.write_indented(&format!("float _fr = _fa {} _fb;\n", op_str));
                self.write_indented(&format!(
                    "signals->{} = _fp32_to_fp16(_fr);\n",
                    output_sanitized
                ));
                self.dedent();
                self.write_indented("}\n");
            } else {
                // FP32 in C++: use union for type punning
                self.write_indented("{\n");
                self.indent();
                self.write_indented("union { uint32_t u; float f; } _a, _b, _r;\n");
                self.write_indented(&format!("_a.u = signals->{};\n", left_sanitized));
                self.write_indented(&format!("_b.u = signals->{};\n", right_sanitized));
                self.write_indented(&format!("_r.f = _a.f {} _b.f;\n", op_str));
                self.write_indented(&format!("signals->{} = _r.u;\n", output_sanitized));
                self.dedent();
                self.write_indented("}\n");
            }
        }
    }

    /// Generate floating-point comparison operation
    fn generate_fp_comparison(
        &mut self,
        node: &SirNode,
        op: &BinaryOperation,
        left_width: usize,
        right_width: usize,
    ) {
        let left = &node.inputs[0].signal_id;
        let right = &node.inputs[1].signal_id;
        let output = &node.outputs[0].signal_id;

        let left_sanitized = self.sanitize_name(left);
        let right_sanitized = self.sanitize_name(right);
        let output_sanitized = self.sanitize_name(output);

        let is_metal = matches!(self.type_mapper.target, BackendTarget::Metal);

        // Determine the comparison operator
        let op_str = match op {
            BinaryOperation::FEq => "==",
            BinaryOperation::FNeq => "!=",
            BinaryOperation::FLt => "<",
            BinaryOperation::FLte => "<=",
            BinaryOperation::FGt => ">",
            BinaryOperation::FGte => ">=",
            _ => unreachable!(),
        };

        let max_width = std::cmp::max(left_width, right_width);

        if is_metal {
            if max_width <= 16 {
                self.write_indented(&format!(
                    "signals->{} = as_type<half>((ushort)signals->{}) {} as_type<half>((ushort)signals->{}) ? 1u : 0u;\n",
                    output_sanitized,
                    left_sanitized,
                    op_str,
                    right_sanitized
                ));
            } else {
                self.write_indented(&format!(
                    "signals->{} = as_type<float>(signals->{}) {} as_type<float>(signals->{}) ? 1u : 0u;\n",
                    output_sanitized,
                    left_sanitized,
                    op_str,
                    right_sanitized
                ));
            }
        } else {
            // C++
            if max_width <= 16 {
                self.write_indented(&format!(
                    "signals->{} = (_fp16_to_fp32(signals->{}) {} _fp16_to_fp32(signals->{})) ? 1u : 0u;\n",
                    output_sanitized,
                    left_sanitized,
                    op_str,
                    right_sanitized
                ));
            } else {
                self.write_indented("{\n");
                self.indent();
                self.write_indented("union { uint32_t u; float f; } _a, _b;\n");
                self.write_indented(&format!("_a.u = signals->{};\n", left_sanitized));
                self.write_indented(&format!("_b.u = signals->{};\n", right_sanitized));
                self.write_indented(&format!(
                    "signals->{} = (_a.f {} _b.f) ? 1u : 0u;\n",
                    output_sanitized, op_str
                ));
                self.dedent();
                self.write_indented("}\n");
            }
        }
    }

    /// Get binary operation string
    fn get_binary_op_str(
        &self,
        op: &BinaryOperation,
        left_width: usize,
        right_width: usize,
    ) -> &'static str {
        let is_boolean_context = left_width == 1 && right_width == 1;

        match op {
            BinaryOperation::Add => "+",
            BinaryOperation::Sub => "-",
            BinaryOperation::Mul | BinaryOperation::SMul => "*",
            BinaryOperation::Div | BinaryOperation::SDiv => "/",
            BinaryOperation::Mod | BinaryOperation::SMod => "%",
            BinaryOperation::And => {
                if is_boolean_context {
                    "&&"
                } else {
                    "&"
                }
            }
            BinaryOperation::Or => {
                if is_boolean_context {
                    "||"
                } else {
                    "|"
                }
            }
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

        let is_shift_op = matches!(
            op,
            BinaryOperation::Shl | BinaryOperation::Shr | BinaryOperation::Sar
        );
        let left_is_scalar = left_width <= 32;
        let right_is_scalar = right_width <= 32;

        let array_size = output_width.div_ceil(32);
        self.write_indented(&format!(
            "// Element-wise {} for {}-bit output\n",
            op_str, output_width
        ));
        self.write_indented(&format!(
            "for (uint32_t i = 0; i < {}; i++) {{\n",
            array_size
        ));
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

    /// Generate widening multiplication where output is wider than inputs
    /// This handles cases like 32*32->64, 64*64->128, etc.
    fn generate_widening_multiply(
        &mut self,
        node: &SirNode,
        left_width: usize,
        right_width: usize,
        output_width: usize,
    ) {
        let left = &node.inputs[0].signal_id;
        let right = &node.inputs[1].signal_id;
        let output = &node.outputs[0].signal_id;

        let left_sanitized = self.sanitize_name(left);
        let right_sanitized = self.sanitize_name(right);
        let output_sanitized = self.sanitize_name(output);

        let is_metal = matches!(self.type_mapper.target, BackendTarget::Metal);

        self.write_indented(&format!(
            "// Widening multiply: {}x{} -> {}\n",
            left_width, right_width, output_width
        ));

        // Determine how the output signal is actually stored
        let output_uses_array = self.uses_array_storage(output_width);
        let output_uses_vector = self.uses_vector_storage(output_width);

        // For outputs up to 64 bits, we can use uint64_t arithmetic
        if output_width <= 64 {
            // Convert both operands to uint64_t and multiply
            let left_expr = self.get_64bit_operand_expr(&left_sanitized, left_width, is_metal);
            let right_expr = self.get_64bit_operand_expr(&right_sanitized, right_width, is_metal);

            self.write_indented("{\n");
            self.indent();
            self.write_indented(&format!(
                "uint64_t _result = {} * {};\n",
                left_expr, right_expr
            ));

            if output_width <= 32 {
                // Output fits in a scalar - just take low bits
                let mask = if output_width == 32 {
                    "".to_string()
                } else {
                    format!(" & 0x{:X}", (1u64 << output_width) - 1)
                };
                self.write_indented(&format!(
                    "signals->{} = (uint32_t)_result{};\n",
                    output_sanitized, mask
                ));
            } else if is_metal && output_uses_vector {
                // Metal: output is uint2
                self.write_indented(&format!(
                    "signals->{} = uint2((uint)_result, (uint)(_result >> 32));\n",
                    output_sanitized
                ));
            } else if output_uses_array {
                // C++: output is uint32_t[2] array (width > 64)
                self.write_indented(&format!(
                    "signals->{}[0] = (uint32_t)_result;\n",
                    output_sanitized
                ));
                self.write_indented(&format!(
                    "signals->{}[1] = (uint32_t)(_result >> 32);\n",
                    output_sanitized
                ));
            } else {
                // C++: output is uint64_t scalar
                self.write_indented(&format!("signals->{} = _result;\n", output_sanitized));
            }

            self.dedent();
            self.write_indented("}\n");
        } else if output_width <= 128 {
            // For 65-128 bit outputs, we need to do 64-bit * 64-bit -> 128-bit
            // This requires decomposition into 32-bit parts
            self.write_indented("{\n");
            self.indent();

            // Get the low 32 bits and high 32 bits of each operand
            let (left_lo, left_hi) =
                self.get_32bit_parts_expr(&left_sanitized, left_width, is_metal);
            let (right_lo, right_hi) =
                self.get_32bit_parts_expr(&right_sanitized, right_width, is_metal);

            // Schoolbook multiplication:
            // (a_hi * 2^32 + a_lo) * (b_hi * 2^32 + b_lo)
            // = a_hi * b_hi * 2^64 + (a_hi * b_lo + a_lo * b_hi) * 2^32 + a_lo * b_lo
            self.write_indented(&format!(
                "uint64_t _ll = (uint64_t){} * (uint64_t){};\n",
                left_lo, right_lo
            ));
            self.write_indented(&format!(
                "uint64_t _lh = (uint64_t){} * (uint64_t){};\n",
                left_lo, right_hi
            ));
            self.write_indented(&format!(
                "uint64_t _hl = (uint64_t){} * (uint64_t){};\n",
                left_hi, right_lo
            ));
            self.write_indented(&format!(
                "uint64_t _hh = (uint64_t){} * (uint64_t){};\n",
                left_hi, right_hi
            ));

            // Combine: result_lo64 = _ll + ((_lh + _hl) << 32)
            //          result_hi64 = _hh + ((_lh + _hl) >> 32) + carry
            self.write_indented("uint64_t _mid = _lh + _hl;\n");
            self.write_indented("uint64_t _result_lo = _ll + (_mid << 32);\n");
            self.write_indented("uint64_t _carry = (_result_lo < _ll) ? 1 : 0;\n");
            self.write_indented("uint64_t _result_hi = _hh + (_mid >> 32) + _carry;\n");

            // Write output based on actual array size
            let num_elements = output_width.div_ceil(32);

            if is_metal && num_elements == 4 {
                // Metal: output is uint4
                self.write_indented(&format!(
                    "signals->{} = uint4((uint)_result_lo, (uint)(_result_lo >> 32), (uint)_result_hi, (uint)(_result_hi >> 32));\n",
                    output_sanitized
                ));
            } else {
                // C++: output is uint32_t[N] array - only write elements that exist
                self.write_indented(&format!(
                    "signals->{}[0] = (uint32_t)_result_lo;\n",
                    output_sanitized
                ));
                if num_elements > 1 {
                    self.write_indented(&format!(
                        "signals->{}[1] = (uint32_t)(_result_lo >> 32);\n",
                        output_sanitized
                    ));
                }
                if num_elements > 2 {
                    self.write_indented(&format!(
                        "signals->{}[2] = (uint32_t)_result_hi;\n",
                        output_sanitized
                    ));
                }
                if num_elements > 3 {
                    self.write_indented(&format!(
                        "signals->{}[3] = (uint32_t)(_result_hi >> 32);\n",
                        output_sanitized
                    ));
                }
            }

            self.dedent();
            self.write_indented("}\n");
        } else {
            // For outputs > 128 bits, fall back to element-wise (which won't be mathematically correct
            // for full widening multiply, but handles the common case of wide bitwise ops)
            self.generate_wide_binary_op(node, &BinaryOperation::Mul, "*");
        }
    }

    /// Get a 64-bit expression for an operand, handling different storage types
    fn get_64bit_operand_expr(&self, name: &str, width: usize, is_metal: bool) -> String {
        if width <= 32 {
            format!("(uint64_t)signals->{}", name)
        } else if is_metal {
            // Metal: uint2 with .x and .y components
            format!(
                "((uint64_t)signals->{}.x | ((uint64_t)signals->{}.y << 32))",
                name, name
            )
        } else if width <= 64 {
            // C++: uint64_t scalar
            format!("signals->{}", name)
        } else {
            // C++: uint32_t array - take first 64 bits
            format!(
                "((uint64_t)signals->{}[0] | ((uint64_t)signals->{}[1] << 32))",
                name, name
            )
        }
    }

    /// Get the low and high 32-bit parts of an operand
    fn get_32bit_parts_expr(&self, name: &str, width: usize, is_metal: bool) -> (String, String) {
        if width <= 32 {
            (format!("signals->{}", name), "0".to_string())
        } else if is_metal {
            // Metal: uint2 with .x and .y components
            (
                format!("signals->{}.x", name),
                format!("signals->{}.y", name),
            )
        } else if width <= 64 {
            // C++: uint64_t scalar
            (
                format!("(uint32_t)signals->{}", name),
                format!("(uint32_t)(signals->{} >> 32)", name),
            )
        } else {
            // C++: uint32_t array
            (
                format!("signals->{}[0]", name),
                format!("signals->{}[1]", name),
            )
        }
    }

    /// Generate 64-bit add/sub with proper carry propagation
    fn generate_64bit_add_sub(
        &mut self,
        node: &SirNode,
        op_str: &str,
        left_width: usize,
        right_width: usize,
    ) {
        let left = &node.inputs[0].signal_id;
        let right = &node.inputs[1].signal_id;
        let output = &node.outputs[0].signal_id;

        let is_metal = matches!(self.type_mapper.target, BackendTarget::Metal);
        let output_width = std::cmp::max(left_width, right_width);

        // Convert operands to uint64_t
        // For C++: 33-64 bits use uint64_t scalar, >64 bits use uint32_t[N] array
        let left_expr = if left_width <= 32 {
            format!("(uint64_t)signals->{}", self.sanitize_name(left))
        } else if is_metal {
            // Metal: uint2 with .x and .y components
            format!(
                "((uint64_t)signals->{}.x | ((uint64_t)signals->{}.y << 32))",
                self.sanitize_name(left),
                self.sanitize_name(left)
            )
        } else if left_width <= 64 {
            // C++: 33-64 bits stored as uint64_t scalar
            format!("signals->{}", self.sanitize_name(left))
        } else {
            // C++: > 64 bits stored as uint32_t[N] array
            format!(
                "((uint64_t)signals->{}[0] | ((uint64_t)signals->{}[1] << 32))",
                self.sanitize_name(left),
                self.sanitize_name(left)
            )
        };

        let right_expr = if right_width <= 32 {
            format!("(uint64_t)signals->{}", self.sanitize_name(right))
        } else if is_metal {
            // Metal: uint2 with .x and .y components
            format!(
                "((uint64_t)signals->{}.x | ((uint64_t)signals->{}.y << 32))",
                self.sanitize_name(right),
                self.sanitize_name(right)
            )
        } else if right_width <= 64 {
            // C++: 33-64 bits stored as uint64_t scalar
            format!("signals->{}", self.sanitize_name(right))
        } else {
            // C++: > 64 bits stored as uint32_t[N] array
            format!(
                "((uint64_t)signals->{}[0] | ((uint64_t)signals->{}[1] << 32))",
                self.sanitize_name(right),
                self.sanitize_name(right)
            )
        };

        if is_metal {
            // Metal: Pack result into uint2 (.x = low 32 bits, .y = high 32 bits)
            self.write_indented(&format!(
                "{{ uint64_t _tmp = {} {} {}; signals->{}.x = (uint)_tmp; signals->{}.y = (uint)(_tmp >> 32); }}\n",
                left_expr, op_str, right_expr,
                self.sanitize_name(output),
                self.sanitize_name(output)
            ));
        } else if output_width <= 64 {
            // C++: 33-64 bits stored as uint64_t scalar
            self.write_indented(&format!(
                "signals->{} = {} {} {};\n",
                self.sanitize_name(output),
                left_expr,
                op_str,
                right_expr
            ));
        } else {
            // C++: > 64 bits stored as uint32_t[N] array
            self.write_indented(&format!(
                "{{ uint64_t _tmp = {} {} {}; signals->{}[0] = (uint32_t)_tmp; signals->{}[1] = (uint32_t)(_tmp >> 32); }}\n",
                left_expr, op_str, right_expr,
                self.sanitize_name(output),
                self.sanitize_name(output)
            ));
        }
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

        // FNeg on fp32: flip the IEEE754 sign bit (XOR with 0x80000000)
        // NOT integer negation which gives wrong results
        if matches!(op, UnaryOperation::FNeg) {
            let is_metal = matches!(self.type_mapper.target, BackendTarget::Metal);
            if is_metal {
                // Metal: use as_type for proper bit manipulation
                self.write_indented(&format!(
                    "signals->{} = as_type<uint>(-(as_type<float>(signals->{})));\n",
                    self.sanitize_name(output),
                    self.sanitize_name(input)
                ));
            } else {
                // C++: XOR sign bit for both fp32 and fp16
                if input_width <= 16 {
                    self.write_indented(&format!(
                        "signals->{} = signals->{} ^ 0x8000;\n",
                        self.sanitize_name(output),
                        self.sanitize_name(input)
                    ));
                } else {
                    self.write_indented(&format!(
                        "signals->{} = signals->{} ^ 0x80000000;\n",
                        self.sanitize_name(output),
                        self.sanitize_name(input)
                    ));
                }
            }
            return;
        }

        // FAbs on fp: clear the IEEE754 sign bit
        if matches!(op, UnaryOperation::FAbs) {
            let is_metal = matches!(self.type_mapper.target, BackendTarget::Metal);
            if is_metal {
                self.write_indented(&format!(
                    "signals->{} = as_type<uint>(fabs(as_type<float>(signals->{})));\n",
                    self.sanitize_name(output),
                    self.sanitize_name(input)
                ));
            } else if input_width <= 16 {
                self.write_indented(&format!(
                    "signals->{} = signals->{} & 0x7FFF;\n",
                    self.sanitize_name(output),
                    self.sanitize_name(input)
                ));
            } else {
                self.write_indented(&format!(
                    "signals->{} = signals->{} & 0x7FFFFFFF;\n",
                    self.sanitize_name(output),
                    self.sanitize_name(input)
                ));
            }
            return;
        }

        // FSqrt on fp: need explicit float bitcast
        let is_function = matches!(op, UnaryOperation::FSqrt);

        if is_function {
            let is_metal = matches!(self.type_mapper.target, BackendTarget::Metal);
            if is_metal {
                self.write_indented(&format!(
                    "signals->{} = as_type<uint>(sqrt(as_type<float>(signals->{})));\n",
                    self.sanitize_name(output),
                    self.sanitize_name(input)
                ));
            } else {
                // C++: use union for bitcast
                self.write_indented("{\n");
                self.indent();
                self.write_indented("union { uint32_t u; float f; } _in, _out;\n");
                self.write_indented(&format!(
                    "_in.u = signals->{};\n",
                    self.sanitize_name(input)
                ));
                self.write_indented("_out.f = sqrtf(_in.f);\n");
                self.write_indented(&format!(
                    "signals->{} = _out.u;\n",
                    self.sanitize_name(output)
                ));
                self.dedent();
                self.write_indented("}\n");
            }
        } else {
            let output_width = self.get_signal_width(output);
            let needs_mask =
                output_width < 32 && matches!(op, UnaryOperation::Not | UnaryOperation::Neg);
            if needs_mask {
                let mask = (1u64 << output_width) - 1;
                self.write_indented(&format!(
                    "signals->{} = ({}signals->{}) & 0x{:X};\n",
                    self.sanitize_name(output),
                    op_str,
                    self.sanitize_name(input),
                    mask
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
            self.write_indented(&format!(
                "for (uint32_t i = 0; i < {}; i++) {{\n",
                array_size
            ));
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

        // Check if input/output are state elements or use arrays
        let is_state = self.module.state_elements.contains_key(input);
        let input_width = self.get_signal_width(input);
        let output_width = self.get_signal_width(output);
        let input_uses_array = self.uses_array_storage(input_width);
        let output_uses_array = self.uses_array_storage(output_width);

        let input_base = if is_state {
            self.get_register_accessor(input)
        } else {
            format!("signals->{}", self.sanitize_name(input))
        };

        if output_uses_array {
            // Output is an array - need element-wise copy
            let output_array_size = self.get_array_size(output_width);
            let start_element = low / 32;
            let bit_in_element = low % 32;

            if input_uses_array {
                // Both input and output are arrays - copy elements with offset
                if bit_in_element == 0 {
                    // Aligned copy - just copy elements directly
                    for i in 0..output_array_size {
                        self.write_indented(&format!(
                            "signals->{}[{}] = {}[{}];\n",
                            self.sanitize_name(output),
                            i,
                            input_base,
                            start_element + i
                        ));
                    }
                } else {
                    // Unaligned copy - need to shift and combine
                    for i in 0..output_array_size {
                        let src_idx = start_element + i;
                        self.write_indented(&format!(
                            "signals->{}[{}] = ({}[{}] >> {}) | ({}[{}] << {});\n",
                            self.sanitize_name(output),
                            i,
                            input_base,
                            src_idx,
                            bit_in_element,
                            input_base,
                            src_idx + 1,
                            32 - bit_in_element
                        ));
                    }
                }
            } else {
                // Input is scalar, output is array - unusual case, zero-fill extra elements
                // BUG FIX: If shift exceeds input width, the result is 0 (not undefined behavior)
                if shift >= input_width {
                    // Shift exceeds input width - all bits are 0
                    for i in 0..output_array_size {
                        self.write_indented(&format!(
                            "signals->{}[{}] = 0;\n",
                            self.sanitize_name(output),
                            i
                        ));
                    }
                } else {
                    self.write_indented(&format!(
                        "signals->{}[0] = ({} >> {}) & 0xFFFFFFFF;\n",
                        self.sanitize_name(output),
                        input_base,
                        shift
                    ));
                    for i in 1..output_array_size {
                        self.write_indented(&format!(
                            "signals->{}[{}] = 0;\n",
                            self.sanitize_name(output),
                            i
                        ));
                    }
                }
            }
        } else if input_uses_array {
            // Input is array, output is scalar - extract from array elements
            // Check if this is an array-of-vectors in Metal (e.g., uint2[8] for Array(Bits(64), 8))
            let source_signal = self
                .resolve_to_array_source(input)
                .unwrap_or_else(|| input.to_string());
            let (is_array_of_vectors, elem_bits, vector_size) =
                self.is_array_of_vectors(&source_signal);

            if is_array_of_vectors && self.type_mapper.target == BackendTarget::Metal {
                // Metal array of vectors: need to access array[i].x/y/z/w
                let array_idx = low / elem_bits;
                let bit_in_elem = low % elem_bits;
                let component_idx = bit_in_elem / 32;
                let bit_in_component = bit_in_elem % 32;

                if width <= 32 && bit_in_component + width <= 32 {
                    // Slice fits in single component
                    let comp = self.get_vector_component(component_idx);
                    let mask = if width >= 32 {
                        0xFFFFFFFF_u32
                    } else {
                        (1u32 << width) - 1
                    };
                    self.write_indented(&format!(
                        "signals->{} = ({}[{}]{} >> {}) & 0x{:X};\n",
                        self.sanitize_name(output),
                        input_base,
                        array_idx,
                        comp,
                        bit_in_component,
                        mask
                    ));
                } else if width <= 32 && component_idx + 1 < vector_size {
                    // Slice spans two components within same vector element
                    let comp1 = self.get_vector_component(component_idx);
                    let comp2 = self.get_vector_component(component_idx + 1);
                    let bits_from_first = 32 - bit_in_component;
                    let bits_from_second = width - bits_from_first;
                    let first_mask = (1u32 << bits_from_first) - 1;
                    let second_mask = (1u32 << bits_from_second) - 1;
                    self.write_indented(&format!(
                        "signals->{} = (({}[{}]{} >> {}) & 0x{:X}) | (({}[{}]{} & 0x{:X}) << {});\n",
                        self.sanitize_name(output),
                        input_base, array_idx, comp1, bit_in_component, first_mask,
                        input_base, array_idx, comp2, second_mask, bits_from_first
                    ));
                } else {
                    // Fallback: just use first component
                    let comp = self.get_vector_component(component_idx.min(vector_size - 1));
                    let mask = if width >= 32 {
                        0xFFFFFFFF_u32
                    } else {
                        (1u32 << width.min(32)) - 1
                    };
                    self.write_indented(&format!(
                        "signals->{} = ({}[{}]{} >> {}) & 0x{:X};\n",
                        self.sanitize_name(output),
                        input_base,
                        array_idx,
                        comp,
                        bit_in_component,
                        mask
                    ));
                }
            } else {
                // Regular array with 32-bit elements (C++ or Metal with >128-bit elements)
                let element_idx = low / 32;
                let bit_in_element = low % 32;

                if width <= 32 && bit_in_element + width <= 32 {
                    // Slice fits in single element
                    let elem_mask = if width >= 32 {
                        0xFFFFFFFF
                    } else {
                        (1u32 << width) - 1
                    };
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
                } else if width <= 64 {
                    // 33-64 bit slice from array - combine two elements
                    self.write_indented(&format!(
                        "signals->{} = ((uint64_t){}[{}] >> {}) | ((uint64_t){}[{}] << {});\n",
                        self.sanitize_name(output),
                        input_base,
                        element_idx,
                        bit_in_element,
                        input_base,
                        element_idx + 1,
                        32 - bit_in_element
                    ));
                } else {
                    // Wide slice to scalar (shouldn't happen often)
                    self.write_indented(&format!(
                        "signals->{} = {}[{}];\n",
                        self.sanitize_name(output),
                        input_base,
                        element_idx
                    ));
                }
            }
        } else if self.uses_vector_storage(input_width) {
            // Input is a Metal vector type (uint2/uint4), output is scalar
            // Need to extract from the appropriate vector component(s)
            let component_idx = low / 32;
            let bit_in_component = low % 32;
            let input_vec_size = self.get_vector_size(input_width);

            if component_idx >= input_vec_size {
                // Shift exceeds input width - result is 0
                self.write_indented(&format!("signals->{} = 0;\n", self.sanitize_name(output)));
            } else if width <= 32 && bit_in_component + width <= 32 {
                // Slice fits in single component
                let comp = self.get_vector_component(component_idx);
                let mask = if width >= 32 {
                    0xFFFFFFFF_u32
                } else {
                    (1u32 << width) - 1
                };
                self.write_indented(&format!(
                    "signals->{} = ({}{} >> {}) & 0x{:X};\n",
                    self.sanitize_name(output),
                    input_base,
                    comp,
                    bit_in_component,
                    mask
                ));
            } else if width <= 32 && component_idx + 1 < input_vec_size {
                // Slice spans two components
                let comp1 = self.get_vector_component(component_idx);
                let comp2 = self.get_vector_component(component_idx + 1);
                let bits_from_first = 32 - bit_in_component;
                let bits_from_second = width - bits_from_first;
                let first_mask = (1u32 << bits_from_first) - 1;
                let second_mask = (1u32 << bits_from_second) - 1;
                self.write_indented(&format!(
                    "signals->{} = (({}{} >> {}) & 0x{:X}) | (({}{} & 0x{:X}) << {});\n",
                    self.sanitize_name(output),
                    input_base,
                    comp1,
                    bit_in_component,
                    first_mask,
                    input_base,
                    comp2,
                    second_mask,
                    bits_from_first
                ));
            } else {
                // Extract from first matching component only (fallback)
                let comp = self.get_vector_component(component_idx.min(input_vec_size - 1));
                let mask = if width >= 32 {
                    0xFFFFFFFF_u32
                } else {
                    (1u32 << width.min(32)) - 1
                };
                self.write_indented(&format!(
                    "signals->{} = ({}{} >> {}) & 0x{:X};\n",
                    self.sanitize_name(output),
                    input_base,
                    comp,
                    bit_in_component,
                    mask
                ));
            }
        } else {
            // Both input and output are scalar - simple shift and mask
            // BUG FIX: If shift exceeds input width, the result is 0
            if shift >= input_width {
                self.write_indented(&format!("signals->{} = 0;\n", self.sanitize_name(output)));
            } else {
                let mask = if width >= 64 {
                    u64::MAX
                } else {
                    (1u64 << width) - 1
                };

                self.write_indented(&format!(
                    "signals->{} = ({} >> {}) & 0x{:X};\n",
                    self.sanitize_name(output),
                    input_base,
                    shift,
                    mask
                ));
            }
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
            self.write_indented(&format!(
                "// Concat: {} inputs -> {}-bit output\n",
                node.inputs.len(),
                output_width
            ));

            // Initialize to zero
            for i in 0..array_size {
                self.write_indented(&format!(
                    "signals->{}[{}] = 0;\n",
                    self.sanitize_name(output),
                    i
                ));
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
                } else if *width <= 64 {
                    // 33-64 bit input: stored as uint64_t scalar in C++, uint2 in Metal
                    // Copy low 32 bits to first element, high 32 bits to second element
                    let dest_elem_lo = bit_offset / 32;
                    let dest_elem_hi = (bit_offset + 32) / 32;
                    let is_metal = matches!(self.type_mapper.target, BackendTarget::Metal);
                    if is_metal {
                        // Metal: use .x and .y components
                        self.write_indented(&format!(
                            "signals->{}[{}] = signals->{}.x;\n",
                            self.sanitize_name(output),
                            dest_elem_lo,
                            self.sanitize_name(input_name)
                        ));
                        if dest_elem_hi < output_width.div_ceil(32) {
                            self.write_indented(&format!(
                                "signals->{}[{}] = signals->{}.y;\n",
                                self.sanitize_name(output),
                                dest_elem_hi,
                                self.sanitize_name(input_name)
                            ));
                        }
                    } else {
                        // C++: use cast and shift
                        self.write_indented(&format!(
                            "signals->{}[{}] = (uint32_t)signals->{};\n",
                            self.sanitize_name(output),
                            dest_elem_lo,
                            self.sanitize_name(input_name)
                        ));
                        if dest_elem_hi < output_width.div_ceil(32) {
                            self.write_indented(&format!(
                                "signals->{}[{}] = (uint32_t)(signals->{} >> 32);\n",
                                self.sanitize_name(output),
                                dest_elem_hi,
                                self.sanitize_name(input_name)
                            ));
                        }
                    }
                } else {
                    // Wide input (>64 bits) - copy element by element from array
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
            // Wide vector concat (65-128 bits)
            // C++ uses uint32_t[N] arrays, Metal uses uint4 with .x/.y/.z/.w components
            let num_elements = output_width.div_ceil(32);
            let mut components = vec!["0".to_string(); num_elements];
            let mut bit_offset = 0;
            let is_cpp = matches!(self.type_mapper.target, BackendTarget::Cpp);

            for (input_name, width) in input_widths.iter().rev() {
                let component_idx = bit_offset / 32;
                let bit_in_component = bit_offset % 32;

                if component_idx < num_elements {
                    let input_ref = format!("signals->{}", self.sanitize_name(input_name));
                    if bit_in_component == 0 && *width <= 32 {
                        components[component_idx] = input_ref;
                    } else if *width <= 32 {
                        components[component_idx] = format!(
                            "({} | ({} << {}))",
                            components[component_idx], input_ref, bit_in_component
                        );
                    } else if *width <= 64 && is_cpp {
                        // 33-64 bit input stored as uint64_t scalar in C++
                        // Split into low and high 32-bit parts
                        if bit_in_component == 0 {
                            components[component_idx] = format!("(uint32_t){}", input_ref);
                            if component_idx + 1 < num_elements {
                                components[component_idx + 1] =
                                    format!("(uint32_t)({} >> 32)", input_ref);
                            }
                        }
                    }
                }
                bit_offset += width;
            }

            // Generate component/array assignment based on backend
            for (i, comp) in components.iter().enumerate() {
                if is_cpp {
                    self.write_indented(&format!(
                        "signals->{}[{}] = {};\n",
                        self.sanitize_name(output),
                        i,
                        comp
                    ));
                } else {
                    // Metal: use vector component accessors
                    let component = self.get_vector_component(i);
                    self.write_indented(&format!(
                        "signals->{}{} = {};\n",
                        self.sanitize_name(output),
                        component,
                        comp
                    ));
                }
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

                    let input_ref =
                        format!("(uint64_t)signals->{}", self.sanitize_name(input_name));

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
            } else if self.uses_vector_storage(output_width) {
                // For Metal: Use vector component access (uint2.x, uint2.y)
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
                            components[component_idx] = format!(
                                "({} | ({} << {}))",
                                components[component_idx], input_ref, bit_in_component
                            );
                        }
                    }
                    bit_offset += width;
                }

                // Generate Metal vector component assignment (.x, .y)
                for (i, comp) in components.iter().enumerate() {
                    let vec_comp = self.get_vector_component(i);
                    self.write_indented(&format!(
                        "signals->{}{} = {};\n",
                        self.sanitize_name(output),
                        vec_comp,
                        comp
                    ));
                }
            } else {
                // Metal: output_width > 32 but not using vector storage (shouldn't happen normally)
                // Fall back to scalar expression for safety
                let mut concat_expr = String::new();
                let mut shift = 0;

                for (input_name, width) in input_widths.iter().rev() {
                    if shift >= 32 {
                        break; // Only take first 32 bits for scalar
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

        // Check if source is an array-type state element
        // If so, generate array copy code
        if let Some(state_elem) = self.module.state_elements.get(signal) {
            if let Some(SirType::Array(elem_type, outer_size)) = &state_elem.sir_type {
                let output_name = self.sanitize_name(output);
                let source_accessor = self.get_register_accessor(signal);

                // Check if this is effectively a 2D array:
                // Case 1: Explicit 2D - Array(Array(...), N)
                // Case 2: Width-based 2D - Array(Bits(N), M) where N > 64 (C++) or N > 128 (Metal)
                let elem_width = elem_type.width();
                let (_, elem_array_size) = self.type_mapper.get_type_for_width(elem_width);
                let is_effectively_2d =
                    matches!(elem_type.as_ref(), SirType::Array(_, _)) || elem_array_size.is_some();

                if is_effectively_2d {
                    // Get inner size either from explicit Array or from width-based storage
                    let inner_size = if let SirType::Array(_, inner_sz) = elem_type.as_ref() {
                        *inner_sz
                    } else {
                        elem_array_size.unwrap_or(8)
                    };

                    // 2D array - use memcpy for C++, nested loops for Metal
                    if self.type_mapper.target == BackendTarget::Cpp {
                        self.write_indented(&format!(
                            "memcpy(signals->{}, {}, sizeof(signals->{}));\n",
                            output_name, source_accessor, output_name
                        ));
                    } else {
                        // Metal: nested loops
                        self.write_indented(&format!(
                            "for (uint i = 0; i < {}; i++) {{\n",
                            outer_size
                        ));
                        self.indent();
                        self.write_indented(&format!(
                            "for (uint j = 0; j < {}; j++) {{\n",
                            inner_size
                        ));
                        self.indent();
                        self.write_indented(&format!(
                            "signals->{}[i][j] = {}[i][j];\n",
                            output_name, source_accessor
                        ));
                        self.dedent();
                        self.write_indented("}\n");
                        self.dedent();
                        self.write_indented("}\n");
                    }
                } else {
                    // True 1D array - simple loop
                    self.write_indented(&format!(
                        "for (uint32_t i = 0; i < {}; i++) {{\n",
                        outer_size
                    ));
                    self.indent();
                    self.write_indented(&format!(
                        "signals->{}[i] = {}[i];\n",
                        output_name, source_accessor
                    ));
                    self.dedent();
                    self.write_indented("}\n");
                }
                return;
            }
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
        let output_uses_vector = self.uses_vector_storage(output_width);

        // Check if output is declared as array in Signals struct (e.g., used as ArrayRead source)
        // even if width doesn't require array storage
        let output_is_array_source = self.is_array_read_source(output);

        if source_uses_array && (output_uses_array || output_is_array_source) {
            // Both are arrays - element-wise copy
            let array_size = self.get_array_size(output_width);
            self.write_indented(&format!(
                "for (uint32_t i = 0; i < {}; i++) {{\n",
                array_size
            ));
            self.indent();
            self.write_indented(&format!(
                "signals->{}[i] = {}[i];\n",
                self.sanitize_name(output),
                source_location
            ));
            self.dedent();
            self.write_indented("}\n");
        } else if source_uses_array && output_uses_vector {
            // Metal-specific: source is array (uint[N]), output is vector (uint2/uint4)
            // Copy first elements from array to construct the vector
            let output_sanitized = self.sanitize_name(output);
            if output_width <= 64 {
                // uint2 - copy 2 elements
                self.write_indented(&format!(
                    "signals->{} = uint2({}[0], {}[1]);\n",
                    output_sanitized, source_location, source_location
                ));
            } else {
                // uint4 - copy 4 elements
                self.write_indented(&format!(
                    "signals->{} = uint4({}[0], {}[1], {}[2], {}[3]);\n",
                    output_sanitized,
                    source_location,
                    source_location,
                    source_location,
                    source_location
                ));
            }
        } else if source_uses_array {
            // Source is array but output is scalar - copy first element
            self.write_indented(&format!(
                "signals->{} = {}[0];\n",
                self.sanitize_name(output),
                source_location
            ));
        } else if output_is_array_source && !source_uses_array {
            // Output is forced to be array (ArrayRead source) but source is scalar
            // Initialize the array with the scalar value in the first element, zeros elsewhere
            // Get the array size from the ArrayRead source info or use default
            let array_size = self.get_array_read_source_size(output).unwrap_or(16);
            self.write_indented(&format!(
                "signals->{}[0] = {};\n",
                self.sanitize_name(output),
                source_location
            ));
            if array_size > 1 {
                self.write_indented(&format!(
                    "for (uint32_t i = 1; i < {}; i++) signals->{}[i] = 0;\n",
                    array_size,
                    self.sanitize_name(output)
                ));
            }
        } else {
            // Scalar/vector copy (no arrays involved)
            // On Metal, FP inputs are declared as float/half/double in the Inputs struct,
            // but signals use uint storage. Use as_type<> to preserve IEEE 754 bit patterns.
            let is_metal = matches!(self.type_mapper.target, BackendTarget::Metal);
            let input_fp_cast = if is_metal && is_input {
                self.module
                    .inputs
                    .iter()
                    .find(|i| i.name == signal)
                    .and_then(|i| match &i.sir_type {
                        SirType::Float16 => Some("ushort"),
                        SirType::Float32 => Some("uint"),
                        SirType::Float64 => Some("ulong"),
                        _ => None,
                    })
            } else {
                None
            };
            if let Some(cast_type) = input_fp_cast {
                self.write_indented(&format!(
                    "signals->{} = as_type<{}>({});\n",
                    self.sanitize_name(output),
                    cast_type,
                    source_location
                ));
            } else {
                self.write_indented(&format!(
                    "signals->{} = {};\n",
                    self.sanitize_name(output),
                    source_location
                ));
            }
        }
    }

    /// Check if a state element is a 2D array (array of arrays) or
    /// is effectively stored as a 2D array due to element width.
    /// For example, Array(Bits(256), 8) is stored as uint32_t[8][8] in C++.
    fn is_2d_array(&self, signal: &str) -> bool {
        if let Some(state_elem) = self.module.state_elements.get(signal) {
            if let Some(SirType::Array(elem_type, _)) = &state_elem.sir_type {
                // Case 1: Explicit 2D array - Array(Array(...), N)
                if matches!(elem_type.as_ref(), SirType::Array(_, _)) {
                    return true;
                }
                // Case 2: Array of wide elements - Array(Bits(N), M) where N requires array storage
                // This is stored as uint32_t[M][K] where K = ceil(N/32)
                let elem_width = elem_type.width();
                let (_, elem_array_size) = self.type_mapper.get_type_for_width(elem_width);
                return elem_array_size.is_some();
            }
        }
        false
    }

    /// Get the inner array size for a 2D array (either explicit or width-based)
    #[allow(dead_code)]
    fn get_2d_inner_size(&self, signal: &str) -> Option<usize> {
        if let Some(state_elem) = self.module.state_elements.get(signal) {
            if let Some(SirType::Array(elem_type, _)) = &state_elem.sir_type {
                // Case 1: Explicit 2D array
                if let SirType::Array(_, inner_size) = elem_type.as_ref() {
                    return Some(*inner_size);
                }
                // Case 2: Width-based 2D - inner size from element width
                let elem_width = elem_type.width();
                let (_, elem_array_size) = self.type_mapper.get_type_for_width(elem_width);
                return elem_array_size;
            }
        }
        None
    }

    /// Get the outer array size for a 2D array
    #[allow(dead_code)]
    fn get_2d_outer_size(&self, signal: &str) -> Option<usize> {
        if let Some(state_elem) = self.module.state_elements.get(signal) {
            if let Some(SirType::Array(_, outer_size)) = &state_elem.sir_type {
                return Some(*outer_size);
            }
        }
        None
    }

    /// Generate array read operation
    pub fn generate_array_read(&mut self, node: &SirNode) {
        if node.inputs.len() < 2 || node.outputs.is_empty() {
            return;
        }

        let array_signal = &node.inputs[0].signal_id;
        let index_signal = &node.inputs[1].signal_id;
        let output = &node.outputs[0].signal_id;

        let output_width = self.get_signal_width(output);
        let (_, output_array_size) = self.type_mapper.get_type_for_width(output_width);

        // Check if array_signal is the output of a SignalRef to an array state element
        // (which we skipped generating). If so, use the state element directly.
        let resolved_signal = if self.module.state_elements.contains_key(array_signal) {
            array_signal.clone()
        } else {
            self.resolve_array_source(array_signal)
        };

        let array_location = if self.module.state_elements.contains_key(&resolved_signal) {
            self.get_register_accessor(&resolved_signal)
        } else {
            format!("signals->{}", self.sanitize_name(array_signal))
        };

        // Check if the source array is a 2D array (array of arrays)
        let is_2d = self.is_2d_array(&resolved_signal);

        if let Some(size) = output_array_size {
            // Output is an array type (multi-word value)
            // This means each array element spans multiple words
            let sanitized_output = self.sanitize_name(output);
            let sanitized_index = self.sanitize_name(index_signal);

            if is_2d {
                // For 2D arrays, use 2D indexing: array[outer_index][inner_index]
                self.write_indented(&format!(
                    "for (uint32_t i = 0; i < {}; i++) signals->{}[i] = {}[signals->{}][i];\n",
                    size, sanitized_output, array_location, sanitized_index
                ));
            } else {
                // 1D array with linear indexing
                self.write_indented(&format!(
                    "for (uint32_t i = 0; i < {}; i++) signals->{}[i] = {}[signals->{} * {} + i];\n",
                    size, sanitized_output, array_location, sanitized_index, size
                ));
            }
        } else {
            // Simple scalar index - single word result
            self.write_indented(&format!(
                "signals->{} = {}[signals->{}];\n",
                self.sanitize_name(output),
                array_location,
                self.sanitize_name(index_signal)
            ));
        }
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

        if let Some(SirType::Array(elem_type, outer_size)) = array_type {
            let output_name = self.sanitize_name(output);
            let old_name = self.sanitize_name(old_array);
            let index_name = self.sanitize_name(index_signal);
            let value_name = self.sanitize_name(value_signal);

            // Check if this is effectively a 2D array:
            // Case 1: Explicit 2D - Array(Array(...), N)
            // Case 2: Width-based 2D - Array(Bits(N), M) where N > 64 (C++) or N > 128 (Metal)
            let elem_width = elem_type.width();
            let (_, elem_array_size) = self.type_mapper.get_type_for_width(elem_width);
            let is_explicit_2d = matches!(elem_type.as_ref(), SirType::Array(_, _));
            let is_width_based_2d = elem_array_size.is_some();
            let is_effectively_2d = is_explicit_2d || is_width_based_2d;

            if is_effectively_2d {
                // Get inner size from explicit Array or from width-based storage
                let inner_size = if let SirType::Array(_, sz) = elem_type.as_ref() {
                    *sz
                } else {
                    elem_array_size.unwrap_or(8)
                };

                // 2D array - use memcpy for C++, nested loops for Metal
                if self.type_mapper.target == BackendTarget::Cpp {
                    // C++: memcpy for bulk copy
                    self.write_indented(&format!(
                        "memcpy(signals->{}, signals->{}, sizeof(signals->{}));\n",
                        output_name, old_name, output_name
                    ));
                    // Update the specific row by copying value array
                    self.write_indented(&format!(
                        "for (uint32_t i = 0; i < {}; i++) signals->{}[signals->{}][i] = signals->{}[i];\n",
                        inner_size, output_name, index_name, value_name
                    ));
                } else {
                    // Metal: nested loops for copy
                    self.write_indented(&format!("for (uint i = 0; i < {}; i++) {{\n", outer_size));
                    self.indent();
                    self.write_indented(&format!("for (uint j = 0; j < {}; j++) {{\n", inner_size));
                    self.indent();
                    self.write_indented(&format!(
                        "signals->{}[i][j] = signals->{}[i][j];\n",
                        output_name, old_name
                    ));
                    self.dedent();
                    self.write_indented("}\n");
                    self.dedent();
                    self.write_indented("}\n");
                    // Update specific row
                    self.write_indented(&format!(
                        "for (uint j = 0; j < {}; j++) signals->{}[signals->{}][j] = signals->{}[j];\n",
                        inner_size, output_name, index_name, value_name
                    ));
                }
            } else {
                // True 1D array - simple loop
                self.write_indented(&format!(
                    "for (uint32_t i = 0; i < {}; i++) {{\n",
                    outer_size
                ));
                self.indent();
                self.write_indented(&format!(
                    "signals->{}[i] = signals->{}[i];\n",
                    output_name, old_name
                ));
                self.dedent();
                self.write_indented("}\n");

                // Update the specific element
                self.write_indented(&format!(
                    "signals->{}[signals->{}] = signals->{};\n",
                    output_name, index_name, value_name
                ));
            }
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
                if self
                    .module
                    .state_elements
                    .contains_key(&additional_output.signal_id)
                {
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
            if let Some(node) = self
                .module
                .combinational_nodes
                .iter()
                .find(|n| n.id == *node_id)
            {
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
        // In batched mode with local variables, skip this - locals already have current values
        if !self.in_batched_mode {
            self.write_indented(&format!(
                "// Initialize {} from {}\n",
                next_reg, current_reg
            ));
            for (state_name, elem) in &sorted_states {
                let sanitized = self.sanitize_name(state_name);

                // Check if this is an array type using sir_type with width-based fallback
                // IMPORTANT: Always check width-based array requirement even when sir_type exists
                // because sir_type might be Bits(256) which requires uint32_t[8] storage
                let width = self.get_signal_width(state_name);
                let (_, width_based_array_size) = self.type_mapper.get_type_for_width(width);
                let width_needs_array = width_based_array_size.is_some();

                let is_array = if let Some(ref sir_type) = elem.sir_type {
                    // If sir_type says Array, it's definitely an array
                    // If sir_type is NOT Array but width requires array storage, treat as array
                    matches!(sir_type, SirType::Array(_, _)) || width_needs_array
                } else {
                    width_needs_array
                };

                if is_array {
                    // Array type - check for 2D arrays
                    if let Some(SirType::Array(elem_type, outer_size)) = &elem.sir_type {
                        // Check if inner element is also an array (2D array)
                        if let SirType::Array(_, inner_size) = elem_type.as_ref() {
                            // 2D array
                            if self.type_mapper.target == BackendTarget::Cpp {
                                self.write_indented(&format!(
                                    "memcpy({}->{}, {}->{}, sizeof({}->{}));\n",
                                    next_reg,
                                    sanitized,
                                    current_reg,
                                    sanitized,
                                    current_reg,
                                    sanitized
                                ));
                            } else {
                                // Metal: nested loops
                                self.write_indented(&format!(
                                    "for (uint i = 0; i < {}; i++) {{\n",
                                    outer_size
                                ));
                                self.indent();
                                self.write_indented(&format!(
                                    "for (uint j = 0; j < {}; j++) {{\n",
                                    inner_size
                                ));
                                self.indent();
                                self.write_indented(&format!(
                                    "{}->{}[i][j] = {}->{}[i][j];\n",
                                    next_reg, sanitized, current_reg, sanitized
                                ));
                                self.dedent();
                                self.write_indented("}\n");
                                self.dedent();
                                self.write_indented("}\n");
                            }
                        } else {
                            // Check if element type requires array storage (width-based 2D)
                            let elem_width = elem_type.width();
                            let (_, elem_array_size) =
                                self.type_mapper.get_type_for_width(elem_width);
                            let is_width_based_2d = elem_array_size.is_some();

                            if self.type_mapper.target == BackendTarget::Cpp {
                                // C++: memcpy works for both 1D and 2D
                                self.write_indented(&format!(
                                    "memcpy({}->{}, {}->{}, sizeof({}->{}));\n",
                                    next_reg,
                                    sanitized,
                                    current_reg,
                                    sanitized,
                                    current_reg,
                                    sanitized
                                ));
                            } else if is_width_based_2d {
                                // Metal width-based 2D: nested loops
                                let inner_size = elem_array_size.unwrap_or(8);
                                self.write_indented(&format!(
                                    "for (uint i = 0; i < {}; i++) {{\n",
                                    outer_size
                                ));
                                self.indent();
                                self.write_indented(&format!(
                                    "for (uint j = 0; j < {}; j++) {{\n",
                                    inner_size
                                ));
                                self.indent();
                                self.write_indented(&format!(
                                    "{}->{}[i][j] = {}->{}[i][j];\n",
                                    next_reg, sanitized, current_reg, sanitized
                                ));
                                self.dedent();
                                self.write_indented("}\n");
                                self.dedent();
                                self.write_indented("}\n");
                            } else {
                                // Metal 1D array: element-wise copy
                                self.write_indented(&format!(
                                    "for (uint i = 0; i < {}; i++) {}->{}[i] = {}->{}[i];\n",
                                    outer_size, next_reg, sanitized, current_reg, sanitized
                                ));
                            }
                        }
                    } else {
                        // Width-based array (sir_type is Bits(N) but N requires array storage)
                        let array_size = width_based_array_size.unwrap_or(8);
                        if self.type_mapper.target == BackendTarget::Cpp {
                            self.write_indented(&format!(
                                "memcpy({}->{}, {}->{}, sizeof({}->{}));\n",
                                next_reg, sanitized, current_reg, sanitized, current_reg, sanitized
                            ));
                        } else {
                            // Metal: element-wise copy
                            self.write_indented(&format!(
                                "for (uint i = 0; i < {}; i++) {}->{}[i] = {}->{}[i];\n",
                                array_size, next_reg, sanitized, current_reg, sanitized
                            ));
                        }
                    }
                } else {
                    // Scalar type - direct assignment
                    self.write_indented(&format!(
                        "{}->{} = {}->{};\n",
                        next_reg, sanitized, current_reg, sanitized
                    ));
                }
            }
        }

        // Sort sequential nodes by output name
        let mut sorted_seq_nodes: Vec<_> = self.module.sequential_nodes.iter().collect();
        sorted_seq_nodes.sort_by(|a, b| {
            let a_name = a
                .outputs
                .first()
                .map(|o| o.signal_id.as_str())
                .unwrap_or("");
            let b_name = b
                .outputs
                .first()
                .map(|o| o.signal_id.as_str())
                .unwrap_or("");
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

        // Check if this is an array type using state element's sir_type with width-based fallback
        // IMPORTANT: Always check width-based array requirement even when sir_type exists
        // because sir_type might be Bits(256) which requires uint32_t[8] storage
        let (_, width_based_array_size) = self.type_mapper.get_type_for_width(output_width);
        let width_needs_array = width_based_array_size.is_some();

        let is_array = if let Some(state_elem) = self.module.state_elements.get(output_signal) {
            if let Some(ref sir_type) = state_elem.sir_type {
                // If sir_type says Array, it's definitely an array
                // If sir_type is NOT Array but width requires array storage, treat as array
                matches!(sir_type, SirType::Array(_, _)) || width_needs_array
            } else {
                width_needs_array
            }
        } else {
            width_needs_array
        };

        let array_size = if is_array {
            if let Some(state_elem) = self.module.state_elements.get(output_signal) {
                if let Some(SirType::Array(_, size)) = &state_elem.sir_type {
                    *size
                } else {
                    self.get_array_size(output_width)
                }
            } else {
                self.get_array_size(output_width)
            }
        } else {
            0
        };

        if is_array && array_size > 0 {
            // Array type - check for 2D arrays (explicit or width-based)
            let (is_2d, inner_size) = if let Some(state_elem) =
                self.module.state_elements.get(output_signal)
            {
                if let Some(SirType::Array(elem_type, _)) = &state_elem.sir_type {
                    // Case 1: Explicit 2D array
                    if let SirType::Array(_, inner_sz) = elem_type.as_ref() {
                        (true, *inner_sz)
                    } else {
                        // Case 2: Width-based 2D - element width requires array storage
                        let elem_width = elem_type.width();
                        let (_, elem_array_size) = self.type_mapper.get_type_for_width(elem_width);
                        if let Some(inner_sz) = elem_array_size {
                            (true, inner_sz)
                        } else {
                            (false, 1)
                        }
                    }
                } else {
                    (false, 1)
                }
            } else {
                (false, 1)
            };

            let target = format!("{}->{}", next_reg, sanitized_output);
            if self.type_mapper.target == BackendTarget::Cpp {
                // C++: use memcpy which works for both 1D and 2D arrays
                self.write_indented(&format!(
                    "memcpy({}, signals->{}, sizeof({}));\n",
                    target, sanitized_data, target
                ));
            } else if is_2d {
                // Metal 2D array: nested loops
                self.write_indented(&format!("for (uint i = 0; i < {}; i++) {{\n", array_size));
                self.indent();
                self.write_indented(&format!(
                    "for (uint j = 0; j < {}; j++) {}[i][j] = signals->{}[i][j];\n",
                    inner_size, target, sanitized_data
                ));
                self.dedent();
                self.write_indented("}\n");
            } else {
                // Metal 1D array: simple loop
                self.write_indented(&format!(
                    "for (uint i = 0; i < {}; i++) {}[i] = signals->{}[i];\n",
                    array_size, target, sanitized_data
                ));
            }
        } else {
            // Scalar type
            let target = if self.in_batched_mode && output_width <= 64 {
                format!("local_{}", sanitized_output)
            } else {
                format!("{}->{}", next_reg, sanitized_output)
            };

            self.write_indented(&format!(
                "{} = signals->{}{};\n",
                target, sanitized_data, mask
            ));
        }
    }
}
