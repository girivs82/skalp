use crate::sir::*;
use std::fmt::Write;

pub fn generate_metal_shader(sir_module: &SirModule) -> String {
    let mut shader = String::new();
    let mut generator = MetalShaderGenerator::new(&mut shader);

    generator.generate_header();
    generator.generate_struct_definitions(sir_module);
    generator.generate_combinational_kernels(sir_module);
    generator.generate_sequential_kernel(sir_module);

    eprintln!(
        "\n========== GENERATED METAL SHADER ==========\n{}\n========== END SHADER ==========\n",
        shader
    );
    shader
}

struct MetalShaderGenerator<'a> {
    output: &'a mut String,
    indent: usize,
}

impl<'a> MetalShaderGenerator<'a> {
    fn new(output: &'a mut String) -> Self {
        MetalShaderGenerator { output, indent: 0 }
    }

    /// Sanitize signal names for Metal by replacing dots with underscores
    /// This handles hierarchical signal names like "stage1.reg" -> "stage1_reg"
    fn sanitize_name(&self, name: &str) -> String {
        name.replace('.', "_")
    }

    fn generate_header(&mut self) {
        writeln!(self.output, "#include <metal_stdlib>").unwrap();
        writeln!(self.output, "#include <metal_compute>").unwrap();
        writeln!(self.output, "using namespace metal;\n").unwrap();
    }

    fn generate_struct_definitions(&mut self, sir: &SirModule) {
        // Input buffer structure
        writeln!(self.output, "// Input buffer").unwrap();
        writeln!(self.output, "struct Inputs {{").unwrap();
        self.indent += 1;
        for input in &sir.inputs {
            let (base_type, array_suffix) = self.get_metal_type_parts(&input.sir_type);
            self.write_indented(&format!(
                "{} {}{};\n",
                base_type,
                self.sanitize_name(&input.name),
                array_suffix
            ));
        }
        self.indent -= 1;
        writeln!(self.output, "}};\n").unwrap();

        // Register buffer (only flip-flop state)
        writeln!(self.output, "// Register buffer (flip-flop outputs only)").unwrap();
        writeln!(self.output, "struct Registers {{").unwrap();
        self.indent += 1;
        // Sort state elements by name for consistent ordering
        let mut sorted_states: Vec<_> = sir.state_elements.iter().collect();
        sorted_states.sort_by_key(|(name, _)| *name);
        for (i, (name, _elem)) in sorted_states.iter().enumerate() {
            // Look up the signal to get its type
            let default_type = SirType::Bits(_elem.width);
            let sir_type = sir
                .signals
                .iter()
                .find(|s| &s.name == *name)
                .map(|s| &s.sir_type)
                .unwrap_or(&default_type);
            let (base_type, array_suffix) = self.get_metal_type_parts(sir_type);
            eprintln!(
                "🔧 REGISTER[{}]: {} (type={} {})",
                i, name, base_type, array_suffix
            );
            self.write_indented(&format!(
                "{} {}{};\n",
                base_type,
                self.sanitize_name(name),
                array_suffix
            ));
        }
        self.indent -= 1;
        writeln!(self.output, "}};\n").unwrap();

        // Signal buffer (all computed values including outputs)
        writeln!(self.output, "// Signal buffer (all computed values)").unwrap();
        writeln!(self.output, "struct Signals {{").unwrap();
        self.indent += 1;

        // Keep track of names already added to avoid duplicates
        let mut added_names = std::collections::HashSet::new();

        // Outputs are computed signals too
        for output in &sir.outputs {
            let sanitized_name = self.sanitize_name(&output.name);
            if added_names.insert(sanitized_name.clone()) {
                let (base_type, array_suffix) = self.get_metal_type_parts(&output.sir_type);
                self.write_indented(&format!(
                    "{} {}{};\n",
                    base_type, sanitized_name, array_suffix
                ));
            }
        }

        // All intermediate signals (avoiding duplicates)
        // IMPORTANT: Skip signals that are input ports - they're already in the Inputs struct
        let input_names: std::collections::HashSet<String> =
            sir.inputs.iter().map(|i| i.name.clone()).collect();
        for signal in &sir.signals {
            let sanitized_name = self.sanitize_name(&signal.name);
            // Skip input port signals - they belong in Inputs struct, not Signals
            if !signal.is_state
                && !input_names.contains(&signal.name)
                && added_names.insert(sanitized_name.clone())
            {
                let (base_type, array_suffix) = self.get_metal_type_parts(&signal.sir_type);
                self.write_indented(&format!(
                    "{} {}{};\n",
                    base_type, sanitized_name, array_suffix
                ));
            }
        }
        self.indent -= 1;
        writeln!(self.output, "}};\n").unwrap();
    }

    fn generate_combinational_kernels(&mut self, sir: &SirModule) {
        let cones = sir.extract_combinational_cones();

        if cones.is_empty() {
            // Generate an empty kernel if there are no combinational cones
            // This prevents runtime errors when trying to load non-existent kernels
            self.generate_empty_combinational_kernel();
        } else {
            let total_cones = cones.len();
            for (i, cone) in cones.iter().enumerate() {
                self.generate_combinational_cone_kernel(sir, cone, i, total_cones);
            }
        }
    }

    fn generate_empty_combinational_kernel(&mut self) {
        writeln!(self.output, "// No combinational logic - empty kernel").unwrap();
        writeln!(self.output, "kernel void combinational_cone_0(").unwrap();
        self.indent += 1;
        self.write_indented("device const Inputs* inputs [[buffer(0)]],\n");
        self.write_indented("device const Registers* registers [[buffer(1)]],\n");
        self.write_indented("device Signals* signals [[buffer(2)]],\n");
        self.write_indented("uint tid [[thread_position_in_grid]]\n");
        self.indent -= 1;
        writeln!(self.output, ") {{").unwrap();
        self.indent += 1;
        self.write_indented("// No combinational logic to evaluate\n");
        self.indent -= 1;
        writeln!(self.output, "}}\n").unwrap();
    }

    fn generate_combinational_cone_kernel(
        &mut self,
        sir: &SirModule,
        cone: &CombinationalCone,
        index: usize,
        total_cones: usize,
    ) {
        writeln!(self.output, "kernel void combinational_cone_{}(", index).unwrap();
        self.indent += 1;

        self.write_indented("device const Inputs* inputs [[buffer(0)]],\n");
        self.write_indented("device const Registers* registers [[buffer(1)]],\n");
        self.write_indented("device Signals* signals [[buffer(2)]],\n");
        self.write_indented("uint tid [[thread_position_in_grid]]\n");

        self.indent -= 1;
        writeln!(self.output, ") {{").unwrap();
        self.indent += 1;

        self.write_indented("// Combinational logic evaluation\n");

        // Sort nodes in dependency order
        let sorted_nodes = self.topological_sort_nodes(sir, &cone.nodes);

        eprintln!("🔍 Cone {} has {} nodes", index, sorted_nodes.len());

        // WORKAROUND: Include ALL combinational nodes, not just those in the cone
        // This ensures Concat nodes (which may not be properly traced) are generated
        let all_comb_ids: std::collections::HashSet<usize> =
            sir.combinational_nodes.iter().map(|n| n.id).collect();
        let cone_ids: std::collections::HashSet<usize> = sorted_nodes.iter().copied().collect();
        let missing_ids: Vec<usize> = all_comb_ids.difference(&cone_ids).copied().collect();

        if !missing_ids.is_empty() && index == 0 {
            eprintln!(
                "⚠️  {} combinational nodes NOT in any cone (will add to cone 0)",
                missing_ids.len()
            );
            for &id in &missing_ids {
                if let Some(node) = sir.combinational_nodes.iter().find(|n| n.id == id) {
                    if matches!(node.kind, SirNodeKind::Concat) {
                        eprintln!("    - Concat node {}", id);
                    }
                }
            }
        }

        // Process cone nodes
        for node_id in sorted_nodes {
            if let Some(node) = sir.combinational_nodes.iter().find(|n| n.id == node_id) {
                if matches!(node.kind, SirNodeKind::Concat) {
                    eprintln!("  ✅ Processing Concat node {} (in cone)", node_id);
                }
                self.generate_node_computation_v2(sir, node);
            }
        }

        // WORKAROUND: In cone 0, also process nodes that weren't in any cone
        if index == 0 {
            for node_id in missing_ids {
                if let Some(node) = sir.combinational_nodes.iter().find(|n| n.id == node_id) {
                    if matches!(node.kind, SirNodeKind::Concat) {
                        eprintln!(
                            "  ✅ Processing Concat node {} (MISSING from cones)",
                            node_id
                        );
                    }
                    self.generate_node_computation_v2(sir, node);
                }
            }
        }

        // Only assign outputs in the last cone to avoid duplication
        // TODO: This is a temporary fix - should be based on actual signal dependencies
        if index == total_cones - 1 {
            // Assign outputs based on their drivers
            for output in &sir.outputs {
                let output_width = self.get_signal_width_from_sir(sir, &output.name);
                let is_wide = output_width > 128;

                // Check if this output is a state element (sequential output)
                if sir.state_elements.contains_key(&output.name) {
                    // Output is driven by a register - read from register buffer
                    if is_wide {
                        // Wide bit type - use element-wise copy
                        let array_size = output_width.div_ceil(32);
                        self.write_indented(&format!(
                            "// Element-wise copy for {}-bit output from register (uint[{}])\n",
                            output_width, array_size
                        ));
                        self.write_indented(&format!(
                            "for (uint i = 0; i < {}; i++) {{\n",
                            array_size
                        ));
                        self.indent += 1;
                        self.write_indented(&format!(
                            "signals->{}[i] = registers->{}[i];\n",
                            self.sanitize_name(&output.name),
                            self.sanitize_name(&output.name)
                        ));
                        self.indent -= 1;
                        self.write_indented("}\n");
                    } else {
                        // Scalar - direct assignment
                        self.write_indented(&format!(
                            "signals->{} = registers->{};\n",
                            self.sanitize_name(&output.name),
                            self.sanitize_name(&output.name)
                        ));
                    }
                    eprintln!(
                        "🔗 OUTPUT (STATE): {} = registers->{}",
                        output.name, output.name
                    );
                } else {
                    // Output is driven by combinational logic
                    // Find the driver node for this output signal
                    if let Some(signal) = sir.signals.iter().find(|s| s.name == output.name) {
                        if let Some(driver_node_id) = signal.driver_node {
                            // Find the driver node and connect its output to this signal
                            for comb_node in &sir.combinational_nodes {
                                if comb_node.id == driver_node_id {
                                    if let Some(node_output) = comb_node.outputs.first() {
                                        // Check BOTH output width AND source width
                                        let source_width = self
                                            .get_signal_width_from_sir(sir, &node_output.signal_id);
                                        let both_wide = output_width > 128 && source_width > 128;

                                        if both_wide {
                                            // Both are wide bit types - use element-wise copy
                                            let array_size = output_width.div_ceil(32);
                                            self.write_indented(&format!(
                                                "// Element-wise copy for {}-bit output from {}-bit source (uint[{}])\n",
                                                output_width, source_width, array_size
                                            ));
                                            self.write_indented(&format!(
                                                "for (uint i = 0; i < {}; i++) {{\n",
                                                array_size
                                            ));
                                            self.indent += 1;
                                            self.write_indented(&format!(
                                                "signals->{}[i] = signals->{}[i];\n",
                                                self.sanitize_name(&output.name),
                                                self.sanitize_name(&node_output.signal_id)
                                            ));
                                            self.indent -= 1;
                                            self.write_indented("}\n");
                                        } else if source_width > 128 && output_width <= 128 {
                                            // Source is wide but destination is scalar - extract first element
                                            self.write_indented(&format!(
                                                "// Wide to scalar: extract element 0 from {}-bit source to {}-bit output\n",
                                                source_width, output_width
                                            ));
                                            self.write_indented(&format!(
                                                "signals->{} = signals->{}[0];\n",
                                                self.sanitize_name(&output.name),
                                                self.sanitize_name(&node_output.signal_id)
                                            ));
                                        } else {
                                            // Both scalar - direct assignment
                                            self.write_indented(&format!(
                                                "signals->{} = signals->{};\n",
                                                self.sanitize_name(&output.name),
                                                self.sanitize_name(&node_output.signal_id)
                                            ));
                                        }
                                        eprintln!(
                                            "🔗 OUTPUT (COMB): {} = signals->{}",
                                            output.name, node_output.signal_id
                                        );
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        self.indent -= 1;
        writeln!(self.output, "}}\n").unwrap();
    }

    fn generate_sequential_kernel(&mut self, sir: &SirModule) {
        writeln!(self.output, "kernel void sequential_update(").unwrap();
        self.indent += 1;

        self.write_indented("device const Inputs* inputs [[buffer(0)]],\n");
        self.write_indented("device Registers* registers [[buffer(1)]],\n");
        self.write_indented("device const Signals* signals [[buffer(2)]],\n");
        self.write_indented("uint tid [[thread_position_in_grid]]\n");

        self.indent -= 1;
        writeln!(self.output, ") {{").unwrap();
        self.indent += 1;

        self.write_indented("// Update registers on clock edges\n");

        // Add debug to verify kernel execution
        self.write_indented("// DEBUG: Sequential update kernel executing\n");

        // Save old register values for proper simultaneous update semantics
        // Sort state elements by name for consistent ordering
        let mut sorted_states: Vec<_> = sir.state_elements.iter().collect();
        sorted_states.sort_by_key(|(name, _)| *name);
        for (state_name, state_elem) in sorted_states {
            let sanitized = self.sanitize_name(state_name);

            // Check if this is an array - arrays don't need old value capture
            let state_sir_type = self.get_signal_sir_type(sir, state_name);
            let is_array = matches!(state_sir_type, Some(SirType::Array(_, _)));
            // Also skip wide bit types (>128 bits) as they're represented as arrays
            let is_wide = state_elem.width > 128;

            if !is_array && !is_wide {
                // Only scalar types (<=128 bits) can use direct initialization
                let (base_type, _) = self.get_metal_type_for_wide_bits(state_elem.width);
                self.write_indented(&format!(
                    "{} old_{} = registers->{};\n",
                    base_type, sanitized, sanitized
                ));
            }
        }

        // Check clock edges and update registers
        // Sort sequential nodes by their output register name for consistent ordering
        let mut sorted_seq_nodes: Vec<_> = sir.sequential_nodes.iter().collect();
        sorted_seq_nodes.sort_by(|a, b| {
            // Get the output register names from these flip-flops
            let a_name = a
                .outputs
                .first()
                .map(|out| out.signal_id.as_str())
                .unwrap_or("");
            let b_name = b
                .outputs
                .first()
                .map(|out| out.signal_id.as_str())
                .unwrap_or("");
            a_name.cmp(b_name)
        });

        eprintln!("DEBUG Metal gen: Sorted FF order:");
        for (i, node) in sorted_seq_nodes.iter().enumerate() {
            if let Some(output) = node.outputs.first() {
                eprintln!(
                    "  [{}] Node {}: output '{}', inputs: {:?}",
                    i,
                    node.id,
                    output.signal_id,
                    node.inputs
                        .iter()
                        .map(|inp| &inp.signal_id)
                        .collect::<Vec<_>>()
                );
            }
        }

        for node in sorted_seq_nodes {
            if let SirNodeKind::FlipFlop { clock_edge } = &node.kind {
                self.generate_flipflop_update_v2(sir, node, clock_edge);
            }
        }

        self.indent -= 1;
        writeln!(self.output, "}}\n").unwrap();
    }
    fn generate_binary_op(&mut self, sir: &SirModule, node: &SirNode, op: &BinaryOperation) {
        if node.inputs.len() >= 2 && !node.outputs.is_empty() {
            let left = &node.inputs[0].signal_id;
            let right = &node.inputs[1].signal_id;
            let output = &node.outputs[0].signal_id;

            let op_str = match op {
                BinaryOperation::Add => "+",
                BinaryOperation::Sub => "-",
                BinaryOperation::Mul => "*",
                BinaryOperation::Div => "/",
                BinaryOperation::Mod => "%",
                BinaryOperation::And => "&",
                BinaryOperation::Or => "|",
                BinaryOperation::Xor => "^",
                BinaryOperation::Eq => "==",
                BinaryOperation::Neq => "!=",
                BinaryOperation::Lt => "<",
                BinaryOperation::Lte => "<=",
                BinaryOperation::Gt => ">",
                BinaryOperation::Gte => ">=",
                BinaryOperation::Shl => "<<",
                BinaryOperation::Shr => ">>",
                // Floating-point operations
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
            };

            // Check if we need element-wise operations for wide bit types (> 128 bits)
            // Get the output signal width from the SIR module
            let output_width = self.get_signal_width_from_sir(sir, output);

            if output_width > 128 {
                // Wide bit type - use element-wise operations
                let array_size = output_width.div_ceil(32); // Ceil division
                self.write_indented(&format!(
                    "// Element-wise {} for {}-bit operands (uint[{}])\n",
                    op_str, output_width, array_size
                ));
                self.write_indented(&format!("for (uint i = 0; i < {}; i++) {{\n", array_size));
                self.indent += 1;
                self.write_indented(&format!(
                    "signals->{}[i] = signals->{}[i] {} signals->{}[i];\n",
                    self.sanitize_name(output),
                    self.sanitize_name(left),
                    op_str,
                    self.sanitize_name(right)
                ));
                self.indent -= 1;
                self.write_indented("}\n");
            } else {
                // Scalar operation - direct assignment
                // Check if this is a floating-point operation
                let is_fp_op = matches!(
                    op,
                    BinaryOperation::FAdd
                        | BinaryOperation::FSub
                        | BinaryOperation::FMul
                        | BinaryOperation::FDiv
                        | BinaryOperation::FMod
                        | BinaryOperation::FEq
                        | BinaryOperation::FNeq
                        | BinaryOperation::FLt
                        | BinaryOperation::FLte
                        | BinaryOperation::FGt
                        | BinaryOperation::FGte
                );

                if is_fp_op {
                    // For FP operations, cast to float, operate, cast back to bits
                    // Determine float type based on width
                    let float_type = if output_width == 16 { "half" } else { "float" };

                    self.write_indented(&format!(
                        "signals->{} = as_type<uint>(as_type<{}>( signals->{}) {} as_type<{}>(signals->{}));\n",
                        self.sanitize_name(output),
                        float_type,
                        self.sanitize_name(left),
                        op_str,
                        float_type,
                        self.sanitize_name(right)
                    ));
                } else {
                    // Regular integer operation
                    self.write_indented(&format!(
                        "signals->{} = signals->{} {} signals->{};\n",
                        self.sanitize_name(output),
                        self.sanitize_name(left),
                        op_str,
                        self.sanitize_name(right)
                    ));
                }
            }
        }
    }

    fn generate_unary_op(&mut self, sir: &SirModule, node: &SirNode, op: &UnaryOperation) {
        if !node.inputs.is_empty() && !node.outputs.is_empty() {
            let input = &node.inputs[0].signal_id;
            let output = &node.outputs[0].signal_id;

            let op_str = match op {
                UnaryOperation::Not => "~",
                UnaryOperation::Neg => "-",
                UnaryOperation::RedAnd => "&",
                UnaryOperation::RedOr => "|",
                UnaryOperation::RedXor => "^",
                // Floating-point operations use Metal math functions
                UnaryOperation::FNeg => "-",
                UnaryOperation::FAbs => "abs",
                UnaryOperation::FSqrt => "sqrt",
            };

            // Check if this is a function call (abs, sqrt) or a prefix operator (-, ~)
            let is_function = matches!(op, UnaryOperation::FAbs | UnaryOperation::FSqrt);

            // Check if we need element-wise operations for wide bit types (> 128 bits)
            let output_width = self.get_signal_width_from_sir(sir, output);

            if output_width > 128 {
                // Wide bit type - use element-wise operations
                let array_size = output_width.div_ceil(32); // Ceil division
                self.write_indented(&format!(
                    "// Element-wise {} for {}-bit operand (uint[{}])\n",
                    if is_function { op_str } else { "unary op" },
                    output_width,
                    array_size
                ));
                self.write_indented(&format!("for (uint i = 0; i < {}; i++) {{\n", array_size));
                self.indent += 1;
                if is_function {
                    self.write_indented(&format!(
                        "signals->{}[i] = {}(signals->{}[i]);\n",
                        self.sanitize_name(output),
                        op_str,
                        self.sanitize_name(input)
                    ));
                } else {
                    self.write_indented(&format!(
                        "signals->{}[i] = {}signals->{}[i];\n",
                        self.sanitize_name(output),
                        op_str,
                        self.sanitize_name(input)
                    ));
                }
                self.indent -= 1;
                self.write_indented("}\n");
                return;
            }

            // Scalar operations
            // Check if this is a floating-point operation
            let is_fp_op = matches!(
                op,
                UnaryOperation::FNeg | UnaryOperation::FAbs | UnaryOperation::FSqrt
            );

            if is_fp_op {
                // For FP operations, cast to float, operate, cast back to bits
                let float_type = if output_width == 16 { "half" } else { "float" };

                if is_function {
                    self.write_indented(&format!(
                        "signals->{} = as_type<uint>({}(as_type<{}>(signals->{})));\n",
                        self.sanitize_name(output),
                        op_str,
                        float_type,
                        self.sanitize_name(input)
                    ));
                } else {
                    self.write_indented(&format!(
                        "signals->{} = as_type<uint>({}as_type<{}>(signals->{}));\n",
                        self.sanitize_name(output),
                        op_str,
                        float_type,
                        self.sanitize_name(input)
                    ));
                }
            } else {
                // Regular integer operation
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
        }
    }

    fn generate_constant(&mut self, node: &SirNode, value: u64, width: usize) {
        if !node.outputs.is_empty() {
            let output = &node.outputs[0].signal_id;
            let (base_type, array_size) = self.get_metal_type_for_wide_bits(width);

            if let Some(size) = array_size {
                // For arrays, initialize element by element
                eprintln!(
                    "🔢 Metal codegen: node {} = {}[{}] with value {}",
                    node.id, base_type, size, value
                );
                self.write_indented(&format!(
                    "{}[{}] temp_const_{} = {{",
                    base_type,
                    size,
                    self.sanitize_name(output)
                ));
                for i in 0..size {
                    if i > 0 {
                        write!(self.output, ", ").unwrap();
                    }
                    // Extract the appropriate 32 bits from the value
                    let elem_val = if width <= 64 {
                        if i == 0 {
                            value as u32
                        } else {
                            0
                        }
                    } else {
                        0 // For 256-bit, value is typically 0
                    };
                    write!(self.output, "{}", elem_val).unwrap();
                }
                writeln!(self.output, "}};").unwrap();
                self.write_indented(&format!(
                    "signals->{} = temp_const_{};\n",
                    self.sanitize_name(output),
                    self.sanitize_name(output)
                ));
            } else {
                eprintln!(
                    "🔢 Metal codegen: node {} = {}({})",
                    node.id, base_type, value
                );
                self.write_indented(&format!(
                    "signals->{} = {}({});\n",
                    self.sanitize_name(output),
                    base_type,
                    value
                ));
            }
        }
    }

    fn generate_mux(&mut self, sir: &SirModule, node: &SirNode) {
        if node.inputs.len() >= 3 && !node.outputs.is_empty() {
            let sel = &node.inputs[0].signal_id;
            let true_val = &node.inputs[1].signal_id;
            let false_val = &node.inputs[2].signal_id;
            let output = &node.outputs[0].signal_id;

            // Check if we need element-wise mux for wide bit types (> 128 bits)
            let output_width = self.get_signal_width_from_sir(sir, output);
            let true_val_width = self.get_signal_width_from_sir(sir, true_val);
            let false_val_width = self.get_signal_width_from_sir(sir, false_val);

            if output_width > 128 {
                // Wide bit type - use element-wise mux
                let array_size = output_width.div_ceil(32); // Ceil division
                self.write_indented(&format!(
                    "// Element-wise mux for {}-bit operands (uint[{}])\n",
                    output_width, array_size
                ));
                self.write_indented(&format!("for (uint i = 0; i < {}; i++) {{\n", array_size));
                self.indent += 1;

                // Handle cases where one operand might be a scalar and needs broadcasting
                let true_access = if true_val_width > 128 {
                    format!("signals->{}[i]", self.sanitize_name(true_val))
                } else {
                    // Scalar - broadcast to all elements (only element 0 gets the value, rest get 0)
                    format!("(i == 0 ? signals->{} : 0)", self.sanitize_name(true_val))
                };

                let false_access = if false_val_width > 128 {
                    format!("signals->{}[i]", self.sanitize_name(false_val))
                } else {
                    // Scalar - broadcast to all elements (only element 0 gets the value, rest get 0)
                    format!("(i == 0 ? signals->{} : 0)", self.sanitize_name(false_val))
                };

                self.write_indented(&format!(
                    "signals->{}[i] = signals->{} ? {} : {};\n",
                    self.sanitize_name(output),
                    self.sanitize_name(sel),
                    true_access,
                    false_access
                ));
                self.indent -= 1;
                self.write_indented("}\n");
            } else {
                // Scalar mux - direct assignment
                self.write_indented(&format!(
                    "signals->{} = signals->{} ? signals->{} : signals->{};\n",
                    self.sanitize_name(output),
                    self.sanitize_name(sel),
                    self.sanitize_name(true_val),
                    self.sanitize_name(false_val)
                ));
            }
        }
    }

    fn generate_slice(&mut self, sir: &SirModule, node: &SirNode, start: usize, end: usize) {
        if !node.inputs.is_empty() && !node.outputs.is_empty() {
            let input = &node.inputs[0].signal_id;
            let output = &node.outputs[0].signal_id;
            eprintln!(
                "🔧 SLICE: input='{}', output='{}', state_elements={:?}",
                input,
                output,
                sir.state_elements.keys().collect::<Vec<_>>()
            );

            // For HDL range [high:low], start=high, end=low
            // Width = high - low + 1, shift = low
            let (high, low) = if start >= end {
                (start, end)
            } else {
                (end, start)
            };
            let width = high - low + 1;
            let shift = low;

            // Get the input signal's type to determine if it's an array, vector, or bit vector
            let input_type = self.get_signal_sir_type(sir, input);

            // BUG #36 FIX: Check if this is an array type - if so, this is an array element read, not a bit slice
            if let Some(SirType::Array(elem_type, _size)) = &input_type {
                // This is a constant index array read disguised as a slice
                // The slice is trying to extract one element from the array
                // For arrays, the "shift" is actually the array index
                let array_index = shift / elem_type.width();

                let array_location = if sir.state_elements.contains_key(input) {
                    format!("registers->{}", self.sanitize_name(input))
                } else {
                    format!("signals->{}", self.sanitize_name(input))
                };

                eprintln!(
                    "   🎯 ARRAY ELEMENT READ: {}[{}] (elem_width={}, shift={}, index={})",
                    input,
                    array_index,
                    elem_type.width(),
                    shift,
                    array_index
                );

                self.write_indented(&format!(
                    "signals->{} = {}[{}];\n",
                    self.sanitize_name(output),
                    array_location,
                    array_index
                ));
                return;
            }

            // Map signal names to register names for flip-flop outputs
            let input_ref = if sir.state_elements.contains_key(input) {
                // Direct register reference
                format!("registers->{}", self.sanitize_name(input))
            } else if input.starts_with("node_") && input.ends_with("_out") {
                // This might be a flip-flop output signal, check if it corresponds to a register
                let mut mapped_register = None;

                // Extract node ID from signal name like "node_6_out"
                if let Some(node_id_str) = input
                    .strip_prefix("node_")
                    .and_then(|s| s.strip_suffix("_out"))
                {
                    if let Ok(node_id) = node_id_str.parse::<usize>() {
                        // Find which register this node drives
                        for reg_name in sir.state_elements.keys() {
                            if let Some(signal) = sir.signals.iter().find(|s| s.name == *reg_name) {
                                if signal.driver_node == Some(node_id) {
                                    eprintln!("   🎯 MAPPED: {} -> registers->{}", input, reg_name);
                                    mapped_register = Some(reg_name.clone());
                                    break;
                                }
                            }
                        }
                    }
                }

                if let Some(reg_name) = mapped_register {
                    format!("registers->{}", self.sanitize_name(&reg_name))
                } else {
                    format!("signals->{}", self.sanitize_name(input))
                }
            } else {
                format!("signals->{}", self.sanitize_name(input))
            };

            // Check if this is a vector component access
            if let Some(ref input_type_val) = input_type {
                if input_type_val.is_vector() {
                    // For vectors, use component accessors (.x, .y, .z, .w)
                    // Determine which component based on the slice range
                    let elem_width = input_type_val.elem_type().map(|t| t.width()).unwrap_or(32);
                    let component_index = low / elem_width;
                    let component = match component_index {
                        0 => "x",
                        1 => "y",
                        2 => "z",
                        3 => "w",
                        _ => "x", // fallback
                    };

                    eprintln!(
                        "   🎯 VECTOR SLICE: {} -> .{} (elem_width={}, low={}, component={})",
                        input, component, elem_width, low, component_index
                    );

                    self.write_indented(&format!(
                        "signals->{} = {}.{};\n",
                        self.sanitize_name(output),
                        input_ref,
                        component
                    ));
                    return;
                }
            }

            // For non-vector types, check if it's a wide Bits type requiring element access
            if let Some(SirType::Bits(input_width)) = input_type {
                if input_width > 128 {
                    // Wide Bits type stored as array - need element access
                    let element_idx = shift / 32; // Which array element
                    let bit_offset = shift % 32; // Bit offset within that element
                    let mask = (1u64 << width) - 1;

                    self.write_indented(&format!(
                        "signals->{} = ({}[{}] >> {}) & 0x{:X};\n",
                        self.sanitize_name(output),
                        input_ref,
                        element_idx,
                        bit_offset,
                        mask
                    ));
                    return;
                } else if input_width > 64 {
                    // uint4 type (65-128 bits) - use component access
                    let component_index = shift / 32;
                    let component = match component_index {
                        0 => "x",
                        1 => "y",
                        2 => "z",
                        3 => "w",
                        _ => "x",
                    };

                    eprintln!(
                        "   🎯 SLICE from uint4: {} -> .{} (width={}, shift={})",
                        input, component, input_width, shift
                    );

                    self.write_indented(&format!(
                        "signals->{} = {}.{};\n",
                        self.sanitize_name(output),
                        input_ref,
                        component
                    ));
                    return;
                } else if input_width > 32 {
                    // uint2 type (33-64 bits) - use component access
                    let component_index = shift / 32;
                    let component = if component_index == 0 { "x" } else { "y" };

                    eprintln!(
                        "   🎯 SLICE from uint2: {} -> .{} (width={}, shift={})",
                        input, component, input_width, shift
                    );

                    self.write_indented(&format!(
                        "signals->{} = {}.{};\n",
                        self.sanitize_name(output),
                        input_ref,
                        component
                    ));
                    return;
                }
            }

            // For scalar types (<= 32 bits), use bit shifts and masks
            let mask = (1u64 << width) - 1;
            self.write_indented(&format!(
                "signals->{} = ({} >> {}) & 0x{:X};\n",
                self.sanitize_name(output),
                input_ref,
                shift,
                mask
            ));
        }
    }

    fn generate_array_read(&mut self, sir: &SirModule, node: &SirNode) {
        // ArrayRead: inputs=[array_signal, index], outputs=[value]
        if node.inputs.len() >= 2 && !node.outputs.is_empty() {
            let array_signal_name = &node.inputs[0].signal_id;
            let index_signal = &node.inputs[1].signal_id;
            let output = &node.outputs[0].signal_id;

            // Check if this is a proper array type
            let array_type = self.get_signal_sir_type(sir, array_signal_name);

            if matches!(array_type, Some(SirType::Array(_, _))) {
                // Proper array - use direct indexing
                let array_location = if sir.state_elements.contains_key(array_signal_name) {
                    format!("registers->{}", self.sanitize_name(array_signal_name))
                } else {
                    format!("signals->{}", self.sanitize_name(array_signal_name))
                };

                self.write_indented(&format!(
                    "signals->{} = {}[signals->{}];\n",
                    self.sanitize_name(output),
                    array_location,
                    self.sanitize_name(index_signal)
                ));
            } else {
                // Fallback for packed bit vectors (legacy)
                let elem_width = 8;
                let array_width = self.get_signal_width_from_sir(sir, array_signal_name);

                if array_width > 32 {
                    // Multi-word packed array
                    let (base_type, array_size) = self.get_metal_type_for_wide_bits(array_width);
                    let array_type = if let Some(size) = array_size {
                        format!("{}[{}]", base_type, size)
                    } else {
                        base_type
                    };
                    self.write_indented(&format!(
                        "{} array_val_{} = signals->{};\n",
                        array_type,
                        self.sanitize_name(output),
                        self.sanitize_name(array_signal_name)
                    ));

                    let san_output = self.sanitize_name(output);
                    let san_index = self.sanitize_name(index_signal);
                    self.write_indented(&format!(
                        "uint elem_idx_{} = signals->{};\n",
                        san_output, san_index
                    ));
                    self.write_indented(&format!(
                        "uint word_idx_{} = elem_idx_{} / 4;\n",
                        san_output, san_output
                    ));
                    self.write_indented(&format!(
                        "uint byte_in_word_{} = elem_idx_{} % 4;\n",
                        san_output, san_output
                    ));
                    self.write_indented(&format!(
                        "signals->{} = (array_val_{}[word_idx_{}] >> (byte_in_word_{} * 8)) & 0xFF;\n",
                        san_output, san_output, san_output, san_output
                    ));
                } else {
                    // Single-word packed array
                    self.write_indented(&format!(
                        "signals->{} = (signals->{} >> (signals->{} * {})) & 0xFF;\n",
                        self.sanitize_name(output),
                        self.sanitize_name(array_signal_name),
                        self.sanitize_name(index_signal),
                        elem_width
                    ));
                }
            }
        }
    }

    fn generate_array_write(&mut self, sir: &SirModule, node: &SirNode) {
        // ArrayWrite: inputs=[old_array, index, value], outputs=[new_array]
        if node.inputs.len() >= 3 && !node.outputs.is_empty() {
            let old_array_name = &node.inputs[0].signal_id;
            let index_signal = &node.inputs[1].signal_id;
            let value_signal = &node.inputs[2].signal_id;
            let output = &node.outputs[0].signal_id;

            self.write_indented(&format!(
                "// Array write: {}[{}] = {}\n",
                old_array_name, index_signal, value_signal
            ));

            // Check if this is a proper array type
            let array_type = self.get_signal_sir_type(sir, old_array_name);

            if let Some(SirType::Array(_, size)) = array_type {
                // Proper array - copy old array to output, then update one element
                self.write_indented(&format!("for (uint i = 0; i < {}; i++) {{\n", size));
                self.indent += 1;
                self.write_indented(&format!(
                    "signals->{}[i] = signals->{}[i];\n",
                    self.sanitize_name(output),
                    self.sanitize_name(old_array_name)
                ));
                self.indent -= 1;
                self.write_indented("}\n");
                self.write_indented(&format!(
                    "signals->{}[signals->{}] = signals->{};\n",
                    self.sanitize_name(output),
                    self.sanitize_name(index_signal),
                    self.sanitize_name(value_signal)
                ));
            } else {
                // Fallback for packed bit vectors (legacy)
                let elem_width = 8;
                let array_width = self.get_signal_width_from_sir(sir, old_array_name);
                let (base_type, array_size) = self.get_metal_type_for_wide_bits(array_width);
                let array_type_name = if let Some(size) = array_size {
                    format!("{}[{}]", base_type, size)
                } else {
                    base_type
                };

                if array_width > 32 {
                    // Multi-word packed array
                    let san_output = self.sanitize_name(output);
                    let san_old_array = self.sanitize_name(old_array_name);
                    let san_index = self.sanitize_name(index_signal);
                    let san_value = self.sanitize_name(value_signal);
                    self.write_indented(&format!(
                        "{} new_array_{} = signals->{};\n",
                        array_type_name, san_output, san_old_array
                    ));
                    self.write_indented(&format!(
                        "uint elem_idx_{} = signals->{};\n",
                        san_output, san_index
                    ));
                    self.write_indented(&format!(
                        "uint word_idx_{} = elem_idx_{} / 4;\n",
                        san_output, san_output
                    ));
                    self.write_indented(&format!(
                        "uint byte_in_word_{} = elem_idx_{} % 4;\n",
                        san_output, san_output
                    ));
                    self.write_indented(&format!(
                        "uint shift_{} = byte_in_word_{} * 8;\n",
                        san_output, san_output
                    ));
                    self.write_indented(&format!(
                        "uint mask_{} = ~(0xFFu << shift_{});\n",
                        san_output, san_output
                    ));
                    self.write_indented(&format!(
                        "new_array_{}[word_idx_{}] = (new_array_{}[word_idx_{}] & mask_{}) | ((signals->{} & 0xFF) << shift_{});\n",
                        san_output, san_output, san_output, san_output, san_output, san_value, san_output
                    ));
                    self.write_indented(&format!(
                        "signals->{} = new_array_{};\n",
                        san_output, san_output
                    ));
                } else {
                    // Single-word packed array
                    let san_output = self.sanitize_name(output);
                    let san_index = self.sanitize_name(index_signal);
                    let san_old_array = self.sanitize_name(old_array_name);
                    let san_value = self.sanitize_name(value_signal);
                    self.write_indented(&format!(
                        "uint32_t shift_{} = signals->{} * {};\n",
                        san_output, san_index, elem_width
                    ));
                    self.write_indented(&format!(
                        "uint32_t mask_{} = ~(0xFFu << shift_{});\n",
                        san_output, san_output
                    ));
                    self.write_indented(&format!(
                        "signals->{} = (signals->{} & mask_{}) | ((signals->{} & 0xFF) << shift_{});\n",
                        san_output, san_old_array, san_output, san_value, san_output
                    ));
                }
            }
        }
    }

    fn get_signal_width_from_sir(&self, sir: &SirModule, signal_name: &str) -> usize {
        // Check signals - prefer sir_type over width field
        if let Some(sig) = sir.signals.iter().find(|s| s.name == signal_name) {
            match &sig.sir_type {
                SirType::Bits(w) => return *w,
                SirType::Array(elem_type, size) => {
                    if let SirType::Bits(elem_w) = **elem_type {
                        return elem_w * size;
                    }
                }
                _ => {}
            }
            return sig.width;
        }
        // Check inputs
        if let Some(input) = sir.inputs.iter().find(|i| i.name == signal_name) {
            if let SirType::Bits(w) = input.sir_type {
                return w;
            }
        }
        // Check outputs
        if let Some(output) = sir.outputs.iter().find(|o| o.name == signal_name) {
            if let SirType::Bits(w) = output.sir_type {
                return w;
            }
        }
        // Check state elements
        if let Some(state) = sir.state_elements.get(signal_name) {
            return state.width;
        }
        // Default
        32
    }

    #[allow(dead_code)]
    fn generate_signal_ref(&mut self, node: &SirNode, signal: &str) {
        if !node.outputs.is_empty() && !node.inputs.is_empty() {
            let output = &node.outputs[0].signal_id;

            // Copy from state to signals
            self.write_indented(&format!(
                "signals->{} = state->{};\n",
                self.sanitize_name(output),
                self.sanitize_name(signal)
            ));
        }
    }

    fn generate_concat(&mut self, sir: &SirModule, node: &SirNode) {
        eprintln!("🔧 CONCAT: node {}, {} inputs", node.id, node.inputs.len());
        if node.outputs.is_empty() {
            return;
        }

        let output = &node.outputs[0].signal_id;
        let output_width = self.get_signal_width_from_sir(sir, output);
        eprintln!("   output='{}', width={} bits", output, output_width);

        // Get input widths
        let mut input_widths = Vec::new();
        for input in &node.inputs {
            let width = self.get_signal_width_from_sir(sir, &input.signal_id);
            eprintln!("   input='{}', width={} bits", input.signal_id, width);
            input_widths.push((input.signal_id.clone(), width));
        }

        // BUG FIX #48: Metal concat expressions must use output width, not element count
        // For wide output types (>128 bits), ALWAYS generate uint[N] array concat
        // This fixes the issue where {0, var_2} was being treated as uint2 instead of uint[8]
        if output_width > 128 {
            let array_size = output_width.div_ceil(32);
            self.write_indented(&format!(
                "// Concat: pack inputs into {}-bit output (uint[{}])\n",
                output_width, array_size
            ));

            // Initialize output array to zeros first
            self.write_indented(&format!("for (uint i = 0; i < {}; i++) {{\n", array_size));
            self.indent += 1;
            self.write_indented(&format!(
                "signals->{}[i] = 0;\n",
                self.sanitize_name(output)
            ));
            self.indent -= 1;
            self.write_indented("}\n");

            // Concatenation packs from MSB to LSB
            // Inputs are ordered: [high_bits, ..., low_bits]
            // We need to pack them into array elements
            let mut bit_offset = 0;
            for (input_name, width) in input_widths.iter().rev() {
                let element_idx = bit_offset / 32;
                let bit_in_element = bit_offset % 32;

                if *width <= 32 && bit_in_element == 0 {
                    // Simple case: scalar input aligned to element boundary
                    self.write_indented(&format!(
                        "signals->{}[{}] = signals->{};\n",
                        self.sanitize_name(output),
                        element_idx,
                        self.sanitize_name(input_name)
                    ));
                } else if *width <= 32 {
                    // Scalar input not aligned - need bit manipulation
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
            // Output is 65-128 bits -> uint4
            // Construct uint4 from 32-bit components
            self.write_indented(&format!(
                "// Concat: pack inputs into {}-bit output (uint4)\n",
                output_width
            ));

            let mut components = vec!["0".to_string(); 4];
            let mut bit_offset = 0;

            for (input_name, width) in input_widths.iter().rev() {
                let component_idx = bit_offset / 32;
                if component_idx < 4 {
                    components[component_idx] =
                        format!("signals->{}", self.sanitize_name(input_name));
                }
                bit_offset += width;
            }

            self.write_indented(&format!(
                "signals->{} = uint4({}, {}, {}, {});\n",
                self.sanitize_name(output),
                components[0],
                components[1],
                components[2],
                components[3]
            ));
        } else if output_width > 32 {
            // Output is 33-64 bits -> uint2
            // Construct uint2 from 32-bit components
            self.write_indented(&format!(
                "// Concat: pack inputs into {}-bit output (uint2)\n",
                output_width
            ));

            let mut components = vec!["0".to_string(); 2];
            let mut bit_offset = 0;

            for (input_name, width) in input_widths.iter().rev() {
                let component_idx = bit_offset / 32;
                if component_idx < 2 {
                    components[component_idx] =
                        format!("signals->{}", self.sanitize_name(input_name));
                }
                bit_offset += width;
            }

            self.write_indented(&format!(
                "signals->{} = uint2({}, {});\n",
                self.sanitize_name(output),
                components[0],
                components[1]
            ));
        } else {
            // Output is 1-32 bits -> uint
            // Use bit shifts (safe because all fits in 32 bits)
            let mut shift = 0;
            let mut concat_expr = String::new();
            for (input_name, width) in input_widths.iter().rev() {
                if !concat_expr.is_empty() {
                    concat_expr.push_str(" | ");
                }
                if shift > 0 {
                    concat_expr.push_str(&format!(
                        "(signals->{} << {})",
                        self.sanitize_name(input_name),
                        shift
                    ));
                } else {
                    concat_expr.push_str(&format!("signals->{}", self.sanitize_name(input_name)));
                }
                shift += width;
            }

            self.write_indented(&format!(
                "signals->{} = {};\n",
                self.sanitize_name(output),
                concat_expr
            ));
        }
    }

    fn generate_node_computation_v2(&mut self, sir: &SirModule, node: &SirNode) {
        if matches!(node.kind, SirNodeKind::Concat) {
            eprintln!("🔧 MATCH CONCAT: node {}", node.id);
        }
        match &node.kind {
            SirNodeKind::BinaryOp(op) => self.generate_binary_op(sir, node, op),
            SirNodeKind::UnaryOp(op) => self.generate_unary_op(sir, node, op),
            SirNodeKind::Constant { value, width } => self.generate_constant(node, *value, *width),
            SirNodeKind::Mux => self.generate_mux(sir, node),
            SirNodeKind::Concat => self.generate_concat(sir, node),
            SirNodeKind::Slice { start, end } => self.generate_slice(sir, node, *start, *end),
            SirNodeKind::SignalRef { signal } => {
                // Check if it's reading from inputs or registers
                if !node.outputs.is_empty() {
                    let output = &node.outputs[0].signal_id;

                    // Skip if output is a state element (it's in Registers, not Signals)
                    if sir.state_elements.contains_key(output) {
                        return;
                    }

                    // Check if source is an array type OR wide Bits type - both need element-wise copy
                    let source_type = self.get_signal_sir_type(sir, signal);
                    let is_array = matches!(source_type, Some(SirType::Array(_, _)));

                    // Check if source is a wide Bits type (>128 bits)
                    let source_width = self.get_signal_width_from_sir(sir, signal);
                    let output_width = self.get_signal_width_from_sir(sir, output);
                    let is_wide_bits = source_width > 128 && output_width > 128;

                    // Debug: log widths for problematic signals
                    if output.contains("node") || output.contains("fu_result") {
                        eprintln!("🔍 SignalRef: {} <- {} | source_width={}, output_width={}, is_wide_bits={}",
                                  output, signal, source_width, output_width, is_wide_bits);
                    }

                    if is_array {
                        // Get array size for loop
                        if let Some(SirType::Array(_, size)) = source_type {
                            let source_location = if sir.inputs.iter().any(|i| i.name == *signal) {
                                format!("inputs->{}", self.sanitize_name(signal))
                            } else if sir.state_elements.contains_key(signal) {
                                format!("registers->{}", self.sanitize_name(signal))
                            } else {
                                format!("signals->{}", self.sanitize_name(signal))
                            };

                            self.write_indented(&format!(
                                "for (uint i = 0; i < {}; i++) {{\n",
                                size
                            ));
                            self.indent += 1;
                            self.write_indented(&format!(
                                "signals->{}[i] = {}[i];\n",
                                self.sanitize_name(output),
                                source_location
                            ));
                            self.indent -= 1;
                            self.write_indented("}\n");
                        }
                    } else if is_wide_bits {
                        // Wide Bits type (>128 bits) - use element-wise copy
                        let array_size = output_width.div_ceil(32); // Ceil division
                        let source_location = if sir.inputs.iter().any(|i| i.name == *signal) {
                            format!("inputs->{}", self.sanitize_name(signal))
                        } else if sir.state_elements.contains_key(signal) {
                            format!("registers->{}", self.sanitize_name(signal))
                        } else {
                            format!("signals->{}", self.sanitize_name(signal))
                        };

                        self.write_indented(&format!(
                            "// Element-wise copy for {}-bit signal (uint[{}])\n",
                            output_width, array_size
                        ));
                        self.write_indented(&format!(
                            "for (uint i = 0; i < {}; i++) {{\n",
                            array_size
                        ));
                        self.indent += 1;
                        self.write_indented(&format!(
                            "signals->{}[i] = {}[i];\n",
                            self.sanitize_name(output),
                            source_location
                        ));
                        self.indent -= 1;
                        self.write_indented("}\n");
                    } else {
                        // Scalar/vector types - check for mixed width assignments
                        let source_location = if sir.inputs.iter().any(|i| i.name == *signal) {
                            format!("inputs->{}", self.sanitize_name(signal))
                        } else if sir.state_elements.contains_key(signal) {
                            format!("registers->{}", self.sanitize_name(signal))
                        } else if sir.signals.iter().any(|s| s.name == *signal) {
                            format!("signals->{}", self.sanitize_name(signal))
                        } else {
                            return; // Unknown signal source
                        };

                        // Check for vector-to-scalar conversion (uint2/uint4 -> uint)
                        if source_width > 32 && output_width <= 32 {
                            // Source is uint2 or uint4, destination is uint - extract first component
                            eprintln!(
                                "   🎯 Vector->Scalar: {} ({} bits) -> {} ({} bits), extracting .x",
                                signal, source_width, output, output_width
                            );
                            self.write_indented(&format!(
                                "signals->{} = {}.x;\n",
                                self.sanitize_name(output),
                                source_location
                            ));
                        } else if source_width <= 32 && output_width > 32 && output_width <= 64 {
                            // Source is uint, destination is uint2 - construct vector
                            eprintln!(
                                "   🎯 Scalar->uint2: {} ({} bits) -> {} ({} bits)",
                                signal, source_width, output, output_width
                            );
                            self.write_indented(&format!(
                                "signals->{} = uint2({}, 0);\n",
                                self.sanitize_name(output),
                                source_location
                            ));
                        } else if source_width <= 32 && output_width > 64 && output_width <= 128 {
                            // Source is uint, destination is uint4 - construct vector
                            eprintln!(
                                "   🎯 Scalar->uint4: {} ({} bits) -> {} ({} bits)",
                                signal, source_width, output, output_width
                            );
                            self.write_indented(&format!(
                                "signals->{} = uint4({}, 0, 0, 0);\n",
                                self.sanitize_name(output),
                                source_location
                            ));
                        } else {
                            // Same type - direct assignment
                            self.write_indented(&format!(
                                "signals->{} = {};\n",
                                self.sanitize_name(output),
                                source_location
                            ));
                        }
                    }
                }
            }
            SirNodeKind::ArrayRead => self.generate_array_read(sir, node),
            SirNodeKind::ArrayWrite => self.generate_array_write(sir, node),
            _ => {}
        }

        // Handle nodes with multiple outputs: copy first output to all additional outputs
        // This handles cases like FIFO outputs where connect_node_to_signal adds extra outputs
        if node.outputs.len() > 1 {
            let first_output = &node.outputs[0].signal_id;
            for additional_output in &node.outputs[1..] {
                // Check if we need element-wise copy for wide bit types
                // IMPORTANT: Check BOTH source and destination widths
                let output_width =
                    self.get_signal_width_from_sir(sir, &additional_output.signal_id);
                let source_width = self.get_signal_width_from_sir(sir, first_output);
                let both_wide = output_width > 128 && source_width > 128;

                if both_wide {
                    // Both are wide bit types - use element-wise copy
                    let array_size = output_width.div_ceil(32);
                    self.write_indented(&format!(
                        "// Element-wise copy for {}-bit additional output from {}-bit source (uint[{}])\n",
                        output_width, source_width, array_size
                    ));
                    self.write_indented(&format!("for (uint i = 0; i < {}; i++) {{\n", array_size));
                    self.indent += 1;
                    self.write_indented(&format!(
                        "signals->{}[i] = signals->{}[i];\n",
                        self.sanitize_name(&additional_output.signal_id),
                        self.sanitize_name(first_output)
                    ));
                    self.indent -= 1;
                    self.write_indented("}\n");
                } else if output_width > 128 && source_width <= 128 {
                    // Destination is wide but source is scalar - assign to first element, zero rest
                    let array_size = output_width.div_ceil(32);
                    self.write_indented(&format!(
                        "// Scalar to wide: assign {}-bit source to element 0 of {}-bit output (uint[{}])\n",
                        source_width, output_width, array_size
                    ));
                    self.write_indented(&format!(
                        "signals->{}[0] = signals->{};\n",
                        self.sanitize_name(&additional_output.signal_id),
                        self.sanitize_name(first_output)
                    ));
                    // Zero out remaining elements
                    self.write_indented(&format!("for (uint i = 1; i < {}; i++) {{\n", array_size));
                    self.indent += 1;
                    self.write_indented(&format!(
                        "signals->{}[i] = 0;\n",
                        self.sanitize_name(&additional_output.signal_id)
                    ));
                    self.indent -= 1;
                    self.write_indented("}\n");
                } else if source_width > 128 && output_width <= 128 {
                    // Source is wide but destination is scalar - extract first element
                    self.write_indented(&format!(
                        "// Wide to scalar: extract element 0 from {}-bit source to {}-bit output\n",
                        source_width, output_width
                    ));
                    self.write_indented(&format!(
                        "signals->{} = signals->{}[0];\n",
                        self.sanitize_name(&additional_output.signal_id),
                        self.sanitize_name(first_output)
                    ));
                } else if source_width > 32 && output_width <= 32 {
                    // Source is uint2/uint4, destination is uint - extract first component
                    eprintln!(
                        "   🎯 Additional output vector->scalar: {} ({} bits) -> {} ({} bits)",
                        first_output, source_width, additional_output.signal_id, output_width
                    );
                    self.write_indented(&format!(
                        "signals->{} = signals->{}.x;\n",
                        self.sanitize_name(&additional_output.signal_id),
                        self.sanitize_name(first_output)
                    ));
                } else if source_width <= 32 && output_width > 32 && output_width <= 64 {
                    // Source is uint, destination is uint2 - construct vector
                    eprintln!(
                        "   🎯 Additional output scalar->uint2: {} ({} bits) -> {} ({} bits)",
                        first_output, source_width, additional_output.signal_id, output_width
                    );
                    self.write_indented(&format!(
                        "signals->{} = uint2(signals->{}, 0);\n",
                        self.sanitize_name(&additional_output.signal_id),
                        self.sanitize_name(first_output)
                    ));
                } else if source_width <= 32 && output_width > 64 && output_width <= 128 {
                    // Source is uint, destination is uint4 - construct vector
                    eprintln!(
                        "   🎯 Additional output scalar->uint4: {} ({} bits) -> {} ({} bits)",
                        first_output, source_width, additional_output.signal_id, output_width
                    );
                    self.write_indented(&format!(
                        "signals->{} = uint4(signals->{}, 0, 0, 0);\n",
                        self.sanitize_name(&additional_output.signal_id),
                        self.sanitize_name(first_output)
                    ));
                } else {
                    // Same type - direct assignment
                    self.write_indented(&format!(
                        "signals->{} = signals->{};\n",
                        self.sanitize_name(&additional_output.signal_id),
                        self.sanitize_name(first_output)
                    ));
                }
            }
        }
    }

    fn generate_flipflop_update_v2(&mut self, sir: &SirModule, node: &SirNode, edge: &ClockEdge) {
        if node.inputs.len() >= 2 && !node.outputs.is_empty() {
            let clock_signal = &node.inputs[0].signal_id;
            let data_signal = &node.inputs[1].signal_id;

            eprintln!(
                "DEBUG Metal gen: FF node {} reads from signal '{}' for clock '{}', outputs: {:?}",
                node.id,
                data_signal,
                clock_signal,
                node.outputs
                    .iter()
                    .map(|o| &o.signal_id)
                    .collect::<Vec<_>>()
            );

            // Find which clock input this is
            if let Some(clock_input) = sir.inputs.iter().find(|i| i.name == *clock_signal) {
                let edge_condition = match edge {
                    ClockEdge::Rising => {
                        // Check for rising edge: was 0, now 1
                        format!("inputs->{} == 1", self.sanitize_name(&clock_input.name))
                    }
                    ClockEdge::Falling => {
                        format!("inputs->{} == 0", self.sanitize_name(&clock_input.name))
                    }
                    ClockEdge::Both => "true".to_string(),
                };

                // FIXED: Check clock value to support multi-clock designs
                // Each flip-flop should only update when its specific clock has the right edge
                // This is critical for CDC (clock domain crossing) circuits like AsyncFifo
                self.write_indented(&format!("if ({}) {{\n", edge_condition));
                self.indent += 1;

                // Update all register outputs with the data input value from signals
                for output in &node.outputs {
                    eprintln!(
                        "DEBUG Metal gen: Looking up output.signal_id='{}' in state_elements",
                        output.signal_id
                    );
                    eprintln!(
                        "DEBUG Metal gen: state_elements keys: {:?}",
                        sir.state_elements.keys().collect::<Vec<_>>()
                    );
                    eprintln!(
                        "DEBUG Metal gen: Lookup result: {:?}",
                        sir.state_elements.get(&output.signal_id)
                    );

                    if let Some(state_elem) = sir.state_elements.get(&output.signal_id) {
                        // Check if this is an array type OR wide bit type (>128 bits)
                        let output_type = self.get_signal_sir_type(sir, &output.signal_id);
                        let is_array = matches!(output_type, Some(SirType::Array(_, _)));
                        // Check data signal width to see if we're assigning from a wide signal
                        let data_signal_width = self.get_signal_width_from_sir(sir, data_signal);
                        let register_is_wide = state_elem.width > 128;
                        let data_is_wide = data_signal_width > 128;

                        if is_array {
                            // Element-wise copy for arrays
                            if let Some(SirType::Array(_, size)) = output_type {
                                self.write_indented(&format!(
                                    "for (uint i = 0; i < {}; i++) {{\n",
                                    size
                                ));
                                self.indent += 1;
                                self.write_indented(&format!(
                                    "registers->{}[i] = signals->{}[i];\n",
                                    self.sanitize_name(&output.signal_id),
                                    self.sanitize_name(data_signal)
                                ));
                                self.indent -= 1;
                                self.write_indented("}\n");
                            }
                        } else if register_is_wide {
                            // Wide register (>128 bits) - use element-wise copy
                            let array_size = state_elem.width.div_ceil(32);
                            self.write_indented(&format!(
                                "// Element-wise copy for {}-bit register (uint[{}])\n",
                                state_elem.width, array_size
                            ));
                            self.write_indented(&format!(
                                "for (uint i = 0; i < {}; i++) {{\n",
                                array_size
                            ));
                            self.indent += 1;
                            if data_is_wide {
                                self.write_indented(&format!(
                                    "registers->{}[i] = signals->{}[i];\n",
                                    self.sanitize_name(&output.signal_id),
                                    self.sanitize_name(data_signal)
                                ));
                            } else {
                                // Data is scalar, only assign to element 0
                                self.write_indented(&format!(
                                    "registers->{}[i] = (i == 0) ? signals->{} : 0;\n",
                                    self.sanitize_name(&output.signal_id),
                                    self.sanitize_name(data_signal)
                                ));
                            }
                            self.indent -= 1;
                            self.write_indented("}\n");
                        } else {
                            // Register is narrow (≤128 bits)
                            // Extract LSBs if data is wide, otherwise use scalar assignment with mask
                            let width = state_elem.width;
                            let mask = if width < 32 {
                                format!("0x{:X}", (1u64 << width) - 1)
                            } else {
                                "0xFFFFFFFF".to_string()
                            };

                            let data_expr = if data_is_wide {
                                // Extract element[0] from wide data signal
                                format!("signals->{}[0]", self.sanitize_name(data_signal))
                            } else {
                                // Scalar data signal
                                format!("signals->{}", self.sanitize_name(data_signal))
                            };

                            let assignment = format!(
                                "registers->{} = {} & {};\n",
                                self.sanitize_name(&output.signal_id),
                                data_expr,
                                mask
                            );
                            eprintln!(
                                "DEBUG Metal gen: About to write: {} (width={}, mask={})",
                                assignment.trim(),
                                width,
                                mask
                            );
                            self.write_indented(&assignment);
                            eprintln!("DEBUG Metal gen: Wrote to output buffer");
                        }
                    }
                }

                self.indent -= 1;
                self.write_indented("}\n");
            }
        }
    }

    fn topological_sort_nodes(&self, sir: &SirModule, node_ids: &[usize]) -> Vec<usize> {
        use std::collections::{HashMap, VecDeque};

        // Build dependency graph
        let mut dependencies: HashMap<usize, Vec<usize>> = HashMap::new();
        let mut in_degree: HashMap<usize, usize> = HashMap::new();

        // Initialize all nodes with 0 in-degree
        for &id in node_ids {
            in_degree.insert(id, 0);
            dependencies.insert(id, Vec::new());
        }

        // Build dependency relationships
        for &node_id in node_ids {
            if let Some(node) = sir.combinational_nodes.iter().find(|n| n.id == node_id) {
                for input in &node.inputs {
                    // Find which node produces this input signal
                    for &other_id in node_ids {
                        if other_id != node_id {
                            if let Some(other_node) =
                                sir.combinational_nodes.iter().find(|n| n.id == other_id)
                            {
                                for output in &other_node.outputs {
                                    if output.signal_id == input.signal_id {
                                        // other_node -> node dependency
                                        dependencies.get_mut(&other_id).unwrap().push(node_id);
                                        *in_degree.get_mut(&node_id).unwrap() += 1;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Topological sort using Kahn's algorithm
        let mut queue = VecDeque::new();
        let mut sorted = Vec::new();

        // Start with nodes that have no dependencies
        for (&id, &degree) in &in_degree {
            if degree == 0 {
                queue.push_back(id);
            }
        }

        while let Some(node_id) = queue.pop_front() {
            sorted.push(node_id);

            if let Some(deps) = dependencies.get(&node_id) {
                for &dep in deps {
                    if let Some(degree) = in_degree.get_mut(&dep) {
                        *degree -= 1;
                        if *degree == 0 {
                            queue.push_back(dep);
                        }
                    }
                }
            }
        }

        // If we couldn't sort all nodes, just return them in original order
        if sorted.len() != node_ids.len() {
            node_ids.to_vec()
        } else {
            sorted
        }
    }

    #[allow(dead_code)]
    fn get_metal_type_name(&self, width: usize) -> &str {
        match width {
            1..=32 => "uint",
            33..=64 => "uint2",
            65..=128 => "uint4",
            _ => panic!("Unsupported bit width {} for Metal codegen (max 128 bits). Use arrays for wider types.", width),
        }
    }

    /// Get Metal type representation for wide bit types (> 128 bits)
    /// Returns (base_type, array_size) for array representation
    fn get_metal_type_for_wide_bits(&self, width: usize) -> (String, Option<usize>) {
        match width {
            1..=32 => ("uint".to_string(), None),
            33..=64 => ("uint2".to_string(), None),
            65..=128 => ("uint4".to_string(), None),
            129..=256 => ("uint".to_string(), Some(8)), // uint[8] for 256 bits
            _ => panic!(
                "Unsupported bit width {} for Metal codegen (max 256 bits)",
                width
            ),
        }
    }

    /// Returns (base_type, array_suffix) for Metal type declarations
    /// For example: Array(Bits(32), 16) returns ("uint", "[16]")
    /// Non-arrays return ("type", "")
    fn get_metal_type_parts(&self, sir_type: &SirType) -> (String, String) {
        match sir_type {
            SirType::Bits(w) => {
                let (base_type, array_size) = self.get_metal_type_for_wide_bits(*w);
                if let Some(size) = array_size {
                    (base_type, format!("[{}]", size))
                } else {
                    (base_type, String::new())
                }
            }
            SirType::Float16 => ("half".to_string(), String::new()),
            SirType::Float32 => ("float".to_string(), String::new()),
            SirType::Float64 => ("double".to_string(), String::new()),
            SirType::Vec2(elem) => {
                let (base, _) = self.get_metal_type_parts(elem);
                (format!("{}2", base), String::new())
            }
            SirType::Vec3(elem) => {
                let (base, _) = self.get_metal_type_parts(elem);
                (format!("{}3", base), String::new())
            }
            SirType::Vec4(elem) => {
                let (base, _) = self.get_metal_type_parts(elem);
                (format!("{}4", base), String::new())
            }
            SirType::Array(elem, size) => {
                let (base, suffix) = self.get_metal_type_parts(elem);
                // Metal array syntax: type name[size], not type[size] name
                (base, format!("{}[{}]", suffix, size))
            }
        }
    }

    fn get_signal_sir_type(&self, sir: &SirModule, signal_name: &str) -> Option<SirType> {
        // Check signals
        if let Some(signal) = sir.signals.iter().find(|s| s.name == signal_name) {
            return Some(signal.sir_type.clone());
        }
        // Check inputs
        if let Some(input) = sir.inputs.iter().find(|i| i.name == signal_name) {
            return Some(input.sir_type.clone());
        }
        // Check outputs
        if let Some(output) = sir.outputs.iter().find(|o| o.name == signal_name) {
            return Some(output.sir_type.clone());
        }
        // Check state elements (registers)
        if let Some(state_elem) = sir.state_elements.get(signal_name) {
            return Some(SirType::Bits(state_elem.width));
        }
        None
    }

    fn write_indented(&mut self, text: &str) {
        let indent_str = "    ".repeat(self.indent);
        write!(self.output, "{}{}", indent_str, text).unwrap();
    }
}
