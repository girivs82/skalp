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
    /// Track signals that need to be decomposed (width > 256 bits)
    /// Maps signal name to (total_width, num_parts, part_width)
    wide_signal_decomposition: std::collections::HashMap<String, (usize, usize, usize)>,
}

impl<'a> MetalShaderGenerator<'a> {
    fn new(output: &'a mut String) -> Self {
        MetalShaderGenerator {
            output,
            indent: 0,
            wide_signal_decomposition: std::collections::HashMap::new(),
        }
    }

    /// Sanitize signal names for Metal by replacing dots with underscores
    /// This handles hierarchical signal names like "stage1.reg" -> "stage1_reg"
    fn sanitize_name(&self, name: &str) -> String {
        name.replace('.', "_")
    }

    /// Generate code to write to an array element of a potentially decomposed signal
    /// BUG FIX #71: Handle wide signals (>256 bits) that are decomposed into parts
    fn write_to_decomposed_array(&mut self, signal_name: &str, element_idx: usize, value: &str) {
        let sanitized = self.sanitize_name(signal_name);

        if let Some((total_width, num_parts, _part_width)) =
            self.wide_signal_decomposition.get(&sanitized).cloned()
        {
            // Signal is decomposed - figure out which part and index within part
            let part_idx = element_idx / 8; // 8 elements (256 bits) per part
            let idx_in_part = element_idx % 8;
            let part_name = format!("{}_part{}", sanitized, part_idx);

            // BUG FIX #71e: Check if this is the last part and if it's a scalar (<64 bits)
            let is_last_part = part_idx == num_parts - 1;
            let last_part_width = if is_last_part {
                total_width - (num_parts - 1) * 256
            } else {
                256
            };
            let is_scalar = last_part_width <= 32;

            if is_last_part && is_scalar && idx_in_part == 0 {
                // Last part is a scalar (≤32 bits) - use direct assignment without array index
                eprintln!(
                    "  [BUG FIX #71e] Scalar last part: signals->{} = {} (width={} bits)",
                    part_name, value, last_part_width
                );
                self.write_indented(&format!(
                    "signals->{} = {};\n",
                    part_name, value
                ));
            } else if is_last_part && is_scalar && idx_in_part > 0 {
                // Trying to access beyond a scalar - this is an error, skip it
                eprintln!(
                    "  ⚠️ WARNING: Skipping invalid array access to scalar part: {}[{}]",
                    part_name, idx_in_part
                );
                // Don't generate any code - this shouldn't happen with correct widths
            } else {
                // Normal case: part is an array
                self.write_indented(&format!(
                    "signals->{}[{}] = {};\n",
                    part_name, idx_in_part, value
                ));
            }
        } else {
            // Normal case - not decomposed
            self.write_indented(&format!(
                "signals->{}[{}] = {};\n",
                sanitized, element_idx, value
            ));
        }
    }

    /// Generate field(s) for a signal, decomposing if width > 256 bits
    /// Returns true if the signal was decomposed
    fn generate_signal_field(&mut self, name: &str, sir_type: &SirType) -> bool {
        let width = sir_type.width();
        let sanitized_name = self.sanitize_name(name);

        if width > 256 {
            // Decompose into multiple 256-bit (or smaller) chunks
            let num_parts = width.div_ceil(256);
            let last_part_width = width - (num_parts - 1) * 256;

            eprintln!(
                "[METAL DECOMP] Signal '{}' ({} bits) decomposed into {} parts",
                name, width, num_parts
            );

            // Track this decomposition
            self.wide_signal_decomposition
                .insert(sanitized_name.clone(), (width, num_parts, 256));

            // Generate a field for each part
            for part_idx in 0..num_parts {
                let part_width = if part_idx == num_parts - 1 {
                    last_part_width
                } else {
                    256
                };

                let (base_type, array_opt) = self.get_metal_type_for_wide_bits(part_width);
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
            let (base_type, array_suffix) = self.get_metal_type_parts_for_struct(sir_type);
            self.write_indented(&format!(
                "{} {}{};\n",
                base_type, sanitized_name, array_suffix
            ));
            false
        }
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
            let (base_type, array_suffix) = self.get_metal_type_parts_for_struct(&input.sir_type);
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
        for (name, _elem) in sorted_states.iter() {
            // Look up the signal to get its type
            let default_type = SirType::Bits(_elem.width);
            let sir_type = sir
                .signals
                .iter()
                .find(|s| &s.name == *name)
                .map(|s| &s.sir_type)
                .unwrap_or(&default_type);
            let (base_type, array_suffix) = self.get_metal_type_parts_for_struct(sir_type);
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
                self.generate_signal_field(&output.name, &output.sir_type);
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
                // DEBUG: Log signal width for tuple-related signals OR wide signals
                let should_log = signal.name.contains("tuple")
                    || signal.name.contains("rx")
                    || signal.name.contains("ry")
                    || signal.name.contains("rz")
                    || signal.width > 128; // BUG #71: Also log wide signals

                if should_log {
                    eprintln!(
                        "🔍 METAL STRUCT: signal='{}', width={}, sir_type={:?}",
                        signal.name, signal.width, signal.sir_type
                    );
                }

                self.generate_signal_field(&signal.name, &signal.sir_type);
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
                                        } else if output_width > 128
                                            && source_width > 32
                                            && source_width <= 128
                                        {
                                            // BUG FIX #65 & Metal Backend: Vector/Array-to-array conversion for output signals
                                            let array_size = output_width.div_ceil(32);
                                            let vector_components = source_width.div_ceil(32);

                                            // Check if source is actually a vector type
                                            let source_sir_type = self
                                                .get_signal_sir_type(sir, &node_output.signal_id);
                                            let source_is_vector = matches!(
                                                source_sir_type,
                                                Some(SirType::Vec2(_))
                                                    | Some(SirType::Vec3(_))
                                                    | Some(SirType::Vec4(_))
                                            ) && (source_width == 64
                                                || source_width == 96
                                                || source_width == 128);

                                            if source_is_vector {
                                                self.write_indented(&format!(
                                                    "// BUG FIX #65: Unpack {}-bit vector ({} components) into {}-bit array (uint[{}])\n",
                                                    source_width, vector_components, output_width, array_size
                                                ));

                                                // Unpack vector components into array elements
                                                let component_names = ["x", "y", "z", "w"];
                                                #[allow(clippy::needless_range_loop)]
                                                for i in 0..vector_components.min(4) {
                                                    self.write_indented(&format!(
                                                        "signals->{}[{}] = signals->{}.{};\n",
                                                        self.sanitize_name(&output.name),
                                                        i,
                                                        self.sanitize_name(&node_output.signal_id),
                                                        component_names[i]
                                                    ));
                                                }
                                            } else {
                                                // Source is an array - use array indexing
                                                self.write_indented(&format!(
                                                    "// Metal Backend Fix: Copy {}-bit array to {}-bit array\n",
                                                    source_width, output_width
                                                ));

                                                let copy_elements =
                                                    vector_components.min(array_size);
                                                for i in 0..copy_elements {
                                                    self.write_indented(&format!(
                                                        "signals->{}[{}] = signals->{}[{}];\n",
                                                        self.sanitize_name(&output.name),
                                                        i,
                                                        self.sanitize_name(&node_output.signal_id),
                                                        i
                                                    ));
                                                }
                                            }

                                            // Zero out remaining array elements if output is wider than source
                                            if array_size > vector_components {
                                                self.write_indented(&format!(
                                                    "// Zero-pad remaining {} elements\n",
                                                    array_size - vector_components
                                                ));
                                                self.write_indented(&format!(
                                                    "for (uint i = {}; i < {}; i++) {{\n",
                                                    vector_components, array_size
                                                ));
                                                self.indent += 1;
                                                self.write_indented(&format!(
                                                    "signals->{}[i] = 0;\n",
                                                    self.sanitize_name(&output.name)
                                                ));
                                                self.indent -= 1;
                                                self.write_indented("}\n");
                                            }
                                        } else if output_width > 128 && source_width <= 32 {
                                            // Scalar to wide: source is scalar (≤32 bits), destination is array (>128 bits)
                                            let array_size = output_width.div_ceil(32);
                                            self.write_indented(&format!(
                                                "// Scalar to wide: assign {}-bit source to element 0 of {}-bit output (uint[{}])\n",
                                                source_width, output_width, array_size
                                            ));
                                            self.write_indented(&format!(
                                                "signals->{}[0] = signals->{};\n",
                                                self.sanitize_name(&output.name),
                                                self.sanitize_name(&node_output.signal_id)
                                            ));
                                            // Zero out remaining elements
                                            self.write_indented(&format!(
                                                "for (uint i = 1; i < {}; i++) {{\n",
                                                array_size
                                            ));
                                            self.indent += 1;
                                            self.write_indented(&format!(
                                                "signals->{}[i] = 0;\n",
                                                self.sanitize_name(&output.name)
                                            ));
                                            self.indent -= 1;
                                            self.write_indented("}\n");
                                        } else {
                                            // Both scalar - check for type reinterpretation
                                            // BUG FIX #62: Check if source and dest have different types (float <-> bits)
                                            let source_type = self
                                                .get_signal_sir_type(sir, &node_output.signal_id);
                                            let dest_type =
                                                self.get_signal_sir_type(sir, &output.name);

                                            let source_is_float = source_type
                                                .as_ref()
                                                .is_some_and(|st| st.is_float());
                                            let dest_is_float =
                                                dest_type.as_ref().is_some_and(|dt| dt.is_float());
                                            let needs_reinterpretation =
                                                source_is_float != dest_is_float;

                                            if needs_reinterpretation {
                                                // Need to reinterpret bits when converting between float and non-float
                                                // BUG FIX: Check width matching for as_type<> validity
                                                let source_width = self.get_signal_width_from_sir(
                                                    sir,
                                                    &node_output.signal_id,
                                                );

                                                let dest_metal_type = if dest_is_float {
                                                    match output_width {
                                                        16 => "half",
                                                        32 => "float",
                                                        64 => "double",
                                                        _ => "float",
                                                    }
                                                } else {
                                                    match output_width {
                                                        8 => "uchar",
                                                        16 => "ushort",
                                                        32 => "uint",
                                                        64 => "ulong",
                                                        _ => "uint",
                                                    }
                                                };

                                                // Metal as_type<> requires exact width match
                                                use std::cmp::Ordering;
                                                let source_location = format!(
                                                    "signals->{}",
                                                    self.sanitize_name(&node_output.signal_id)
                                                );

                                                // Debug for specific problematic signals
                                                if output.name.contains("_fp16")
                                                    || output.name.contains("a_fp16")
                                                    || output.name.contains("b_fp16")
                                                {
                                                    eprintln!("[OUTPUT ASSIGN] {} = {} | src_width={}, dst_width={}, src_float={}, dst_float={}",
                                                        output.name, node_output.signal_id, source_width, output_width, source_is_float, dest_is_float);
                                                }

                                                match source_width.cmp(&output_width) {
                                                    Ordering::Equal => {
                                                        // Same width - direct as_type<>
                                                        self.write_indented(&format!(
                                                            "signals->{} = as_type<{}>({});\n",
                                                            self.sanitize_name(&output.name),
                                                            dest_metal_type,
                                                            source_location
                                                        ));
                                                    }
                                                    Ordering::Greater => {
                                                        // Source wider: narrow first, then reinterpret
                                                        let intermediate_type = match output_width {
                                                            16 => "ushort",
                                                            32 => "uint",
                                                            64 => "ulong",
                                                            _ => "uint",
                                                        };
                                                        self.write_indented(&format!(
                                                            "signals->{} = as_type<{}>(({}){}); \n",
                                                            self.sanitize_name(&output.name),
                                                            dest_metal_type,
                                                            intermediate_type,
                                                            source_location
                                                        ));
                                                    }
                                                    Ordering::Less => {
                                                        // Source narrower: widen first, then reinterpret
                                                        let intermediate_type = match output_width {
                                                            16 => "ushort",
                                                            32 => "uint",
                                                            64 => "ulong",
                                                            _ => "uint",
                                                        };
                                                        self.write_indented(&format!(
                                                            "signals->{} = as_type<{}>(({}){}); \n",
                                                            self.sanitize_name(&output.name),
                                                            dest_metal_type,
                                                            intermediate_type,
                                                            source_location
                                                        ));
                                                    }
                                                }
                                            } else {
                                                // Same type - direct assignment
                                                self.write_indented(&format!(
                                                    "signals->{} = signals->{};\n",
                                                    self.sanitize_name(&output.name),
                                                    self.sanitize_name(&node_output.signal_id)
                                                ));
                                            }
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

            // BUG FIX #16: For And/Or operations on boolean (1-bit) values, use logical operators
            // Check if both inputs are 1-bit (boolean) to determine if we need logical vs bitwise ops
            let left_width = self.get_signal_width_from_sir(sir, left);
            let right_width = self.get_signal_width_from_sir(sir, right);
            let is_boolean_context = left_width == 1 && right_width == 1;

            let op_str = match op {
                BinaryOperation::Add => "+",
                BinaryOperation::Sub => "-",
                BinaryOperation::Mul => "*",
                BinaryOperation::Div => "/",
                BinaryOperation::Mod => "%",
                BinaryOperation::And => {
                    if is_boolean_context {
                        "&&" // Logical AND for booleans (prevents bitwise & on float comparison results)
                    } else {
                        "&" // Bitwise AND for integers
                    }
                }
                BinaryOperation::Or => {
                    if is_boolean_context {
                        "||" // Logical OR for booleans
                    } else {
                        "|" // Bitwise OR for integers
                    }
                }
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
                let is_fp_op = op.is_float_op();

                if is_fp_op {
                    // BUG FIX #54: Handle both Float-typed and Bits-typed signals correctly
                    // Check input and output signal types to generate correct Metal code
                    // Determine precision from input signal width
                    let left_width = self.get_signal_width_from_sir(sir, left);
                    let fp_precision = if left_width == 16 {
                        16
                    } else if left_width == 64 {
                        64
                    } else {
                        32
                    };

                    let (float_type, bit_type) = match fp_precision {
                        16 => ("half", "ushort"),
                        64 => ("double", "ulong"),
                        _ => ("float", "uint"),
                    };

                    // Check signal types
                    let left_sir_type = self.get_signal_sir_type(sir, left);
                    let right_sir_type = self.get_signal_sir_type(sir, right);
                    let output_sir_type = self.get_signal_sir_type(sir, output);

                    let left_is_float = left_sir_type
                        .as_ref()
                        .map(|t| t.is_float())
                        .unwrap_or(false);
                    let right_is_float = right_sir_type
                        .as_ref()
                        .map(|t| t.is_float())
                        .unwrap_or(false);
                    let output_is_float = output_sir_type
                        .as_ref()
                        .map(|t| t.is_float())
                        .unwrap_or(false);

                    // Convert inputs to float type if they're stored as bits
                    // BUG FIX: Check width matching for as_type<> casts
                    let left_actual_width = self.get_signal_width_from_sir(sir, left);
                    let right_actual_width = self.get_signal_width_from_sir(sir, right);

                    // BUG FIX #71b: Use format_signal_reference to handle vec3/vec4 extraction
                    let left_signal_ref = self.format_signal_reference(sir, left, Some(0));
                    let right_signal_ref = self.format_signal_reference(sir, right, Some(0));

                    let left_expr = if left_is_float {
                        left_signal_ref
                    } else {
                        // Need to cast from bits to float
                        if left_actual_width == fp_precision {
                            // Widths match - direct as_type cast
                            format!("as_type<{}>({})", float_type, left_signal_ref)
                        } else {
                            // Widths don't match - need intermediate cast
                            let intermediate_type = match fp_precision {
                                16 => "ushort",
                                64 => "ulong",
                                _ => "uint",
                            };
                            format!(
                                "as_type<{}>(({}){})",
                                float_type, intermediate_type, left_signal_ref
                            )
                        }
                    };

                    let right_expr = if right_is_float {
                        right_signal_ref
                    } else {
                        // Need to cast from bits to float
                        if right_actual_width == fp_precision {
                            // Widths match - direct as_type cast
                            format!("as_type<{}>({})", float_type, right_signal_ref)
                        } else {
                            // Widths don't match - need intermediate cast
                            let intermediate_type = match fp_precision {
                                16 => "ushort",
                                64 => "ulong",
                                _ => "uint",
                            };
                            format!(
                                "as_type<{}>(({}){})",
                                float_type, intermediate_type, right_signal_ref
                            )
                        }
                    };

                    // Perform the FP operation
                    let op_expr = format!("{} {} {}", left_expr, op_str, right_expr);

                    // Convert output to bits if output signal is Bits-typed
                    let result_expr = if output_is_float {
                        op_expr
                    } else {
                        format!("as_type<{}>({})", bit_type, op_expr)
                    };

                    self.write_indented(&format!(
                        "signals->{} = {};\n",
                        self.sanitize_name(output),
                        result_expr
                    ));
                } else {
                    // Regular integer operation - signals are always Bits now
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
                // Floating-point operations
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
            let is_fp_op = op.is_float_op();

            if is_fp_op {
                // Determine precision from input signal width
                let input_width = self.get_signal_width_from_sir(sir, input);
                let fp_precision = if input_width == 16 {
                    16
                } else if input_width == 64 {
                    64
                } else {
                    32
                };

                let (float_type, bit_type) = match fp_precision {
                    16 => ("half", "ushort"),
                    64 => ("double", "ulong"),
                    _ => ("float", "uint"),
                };

                // BUG FIX #64: Check input and output signal types to determine conversions needed
                let input_type = self.get_signal_type_from_sir(sir, input);
                let output_type = self.get_signal_type_from_sir(sir, output);

                let needs_input_conversion = !matches!(
                    input_type,
                    Some(SirType::Float16) | Some(SirType::Float32) | Some(SirType::Float64)
                );
                let needs_output_conversion = !matches!(
                    output_type,
                    Some(SirType::Float16) | Some(SirType::Float32) | Some(SirType::Float64)
                );

                if is_function {
                    let input_expr = if needs_input_conversion {
                        format!(
                            "as_type<{}>(signals->{})",
                            float_type,
                            self.sanitize_name(input)
                        )
                    } else {
                        format!("signals->{}", self.sanitize_name(input))
                    };

                    let op_expr = format!("{}({})", op_str, input_expr);

                    let output_expr = if needs_output_conversion {
                        format!("as_type<{}>({})", bit_type, op_expr)
                    } else {
                        op_expr
                    };

                    self.write_indented(&format!(
                        "signals->{} = {};\n",
                        self.sanitize_name(output),
                        output_expr
                    ));
                } else {
                    let input_expr = if needs_input_conversion {
                        format!(
                            "as_type<{}>(signals->{})",
                            float_type,
                            self.sanitize_name(input)
                        )
                    } else {
                        format!("signals->{}", self.sanitize_name(input))
                    };

                    let op_expr = format!("{}{}", op_str, input_expr);

                    let output_expr = if needs_output_conversion {
                        format!("as_type<{}>({})", bit_type, op_expr)
                    } else {
                        op_expr
                    };

                    self.write_indented(&format!(
                        "signals->{} = {};\n",
                        self.sanitize_name(output),
                        output_expr
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

    fn generate_constant(&mut self, sir: &SirModule, node: &SirNode, value: u64, width: usize) {
        if !node.outputs.is_empty() {
            let output = &node.outputs[0].signal_id;

            // BUG FIX #65: Check if output is a float type and use appropriate Metal type
            let output_sir_type = self.get_signal_sir_type(sir, output);
            let is_float_output = output_sir_type.as_ref().is_some_and(|t| t.is_float());

            let (base_type, array_size) = if is_float_output {
                // Float constant - use Metal float type and reinterpret value as float bits
                let float_type = match width {
                    16 => "half",
                    32 => "float",
                    64 => "double",
                    _ => "float",
                };
                (float_type.to_string(), None)
            } else {
                self.get_metal_type_for_wide_bits(width)
            };

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

                // BUG FIX #65: For float constants, reinterpret bits as float
                if is_float_output {
                    // Reinterpret the bit pattern as a float
                    // BUG FIX: Check width matching - uint literal is always 32 bits
                    let output_width = self.get_signal_width_from_sir(sir, output);
                    if output_width == 32 {
                        // 32-bit float - direct cast from uint
                        self.write_indented(&format!(
                            "signals->{} = as_type<{}>(uint({}));\n",
                            self.sanitize_name(output),
                            base_type,
                            value as u32
                        ));
                    } else if output_width == 16 {
                        // 16-bit half - need to narrow to ushort first
                        self.write_indented(&format!(
                            "signals->{} = as_type<{}>((ushort)uint({}));\n",
                            self.sanitize_name(output),
                            base_type,
                            value as u32
                        ));
                    } else {
                        // Other widths (64-bit double, etc.)
                        let intermediate_type = match output_width {
                            64 => "ulong",
                            _ => "uint",
                        };
                        self.write_indented(&format!(
                            "signals->{} = as_type<{}>(({})uint({}));\n",
                            self.sanitize_name(output),
                            base_type,
                            intermediate_type,
                            value as u32
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
                // Scalar mux - check for type reinterpretation
                // BUG FIX #62: If mux inputs/output have different types, add reinterpretation
                let output_type = self.get_signal_sir_type(sir, output);
                let true_type = self.get_signal_sir_type(sir, true_val);
                let false_type = self.get_signal_sir_type(sir, false_val);

                let output_is_float = output_type.as_ref().is_some_and(|t| t.is_float());
                let true_is_float = true_type.as_ref().is_some_and(|t| t.is_float());
                let false_is_float = false_type.as_ref().is_some_and(|t| t.is_float());

                // Determine Metal type for reinterpretation
                let output_metal_type = if output_is_float {
                    match output_width {
                        16 => "half",
                        32 => "float",
                        64 => "double",
                        _ => "float",
                    }
                } else {
                    match output_width {
                        8 => "uchar",
                        16 => "ushort",
                        32 => "uint",
                        64 => "ulong",
                        _ => "uint",
                    }
                };

                // BUG FIX #13: Format operands with proper type conversion
                // When converting float to different-width integer, must convert to bits first, then widen/narrow
                let true_width = true_type.as_ref().map(|t| t.width()).unwrap_or(32);
                let false_width = false_type.as_ref().map(|t| t.width()).unwrap_or(32);

                // BUG FIX #71b: Use format_signal_reference to handle vector component extraction
                // When extracting scalar from vector, we need .x accessor, not cast
                let true_signal_ref = self.format_signal_reference(sir, true_val, Some(0));
                let false_signal_ref = self.format_signal_reference(sir, false_val, Some(0));

                let true_expr = if true_is_float != output_is_float {
                    if true_is_float && true_width != output_width {
                        // Float to different-width integer: convert to bits first, then cast
                        // BUG FIX #71b: Use signal ref that may include .x accessor
                        format!(
                            "({})(as_type<uint>({}))",
                            output_metal_type, true_signal_ref
                        )
                    } else {
                        // Same width or integer-to-float: direct as_type
                        format!("as_type<{}>({})", output_metal_type, true_signal_ref)
                    }
                } else {
                    true_signal_ref
                };

                let false_expr = if false_is_float != output_is_float {
                    if false_is_float && false_width != output_width {
                        // Float to different-width integer: convert to bits first, then cast
                        // BUG FIX #71b: Use signal ref that may include .x accessor
                        format!(
                            "({})(as_type<uint>({}))",
                            output_metal_type, false_signal_ref
                        )
                    } else {
                        // Same width or integer-to-float: direct as_type
                        format!("as_type<{}>({})", output_metal_type, false_signal_ref)
                    }
                } else {
                    false_signal_ref
                };

                self.write_indented(&format!(
                    "signals->{} = signals->{} ? {} : {};\n",
                    self.sanitize_name(output),
                    self.sanitize_name(sel),
                    true_expr,
                    false_expr
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
                    // BUG FIX #10 & #71: Before using component access, verify the Metal representation supports it
                    // Some signals that are vectors in SIR may be stored as arrays in Metal
                    let input_width = input_type_val.width();
                    let (_, metal_array_size, is_decomposed) = self.get_metal_type_safe(input, input_width);
                    let is_metal_array = metal_array_size.is_some() || is_decomposed;

                    if is_metal_array {
                        // Metal stores this as an array - use array indexing
                        let elem_width =
                            input_type_val.elem_type().map(|t| t.width()).unwrap_or(32);

                        // BUG FIX #71d: Handle decomposed signals
                        let (actual_input_ref, adjusted_element_idx) = if is_decomposed {
                            let sanitized = self.sanitize_name(input);
                            if let Some((_total_width, _num_parts, part_width)) = self.wide_signal_decomposition.get(&sanitized) {
                                // Calculate which part and which element within that part
                                let part_index = low / part_width;
                                let bit_offset_in_part = low % part_width;
                                let element_idx_in_part = bit_offset_in_part / elem_width;

                                let part_ref = format!("signals->{}_part{}", sanitized, part_index);

                                eprintln!(
                                    "   🎯 VECTOR->ARRAY SLICE from DECOMPOSED: part={}, element={} (low={}, part_width={}, elem_width={})",
                                    part_index, element_idx_in_part, low, part_width, elem_width
                                );

                                (part_ref, element_idx_in_part)
                            } else {
                                // Shouldn't happen, but fallback
                                let element_idx = low / elem_width;
                                (input_ref.clone(), element_idx)
                            }
                        } else {
                            // Not decomposed - use original logic
                            let element_idx = low / elem_width;
                            (input_ref.clone(), element_idx)
                        };

                        self.write_indented(&format!(
                            "signals->{} = {}[{}];\n",
                            self.sanitize_name(output),
                            actual_input_ref,
                            adjusted_element_idx
                        ));
                        return;
                    }

                    // For vectors stored as vectors in Metal, use component accessors (.x, .y, .z, .w)
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
                // BUG FIX #10 & #71: Check if the Metal representation is an array before using component access
                // Some signals are stored as uint[N] arrays in Metal, not as vector types
                let (_, metal_array_size, is_decomposed) = self.get_metal_type_safe(input, input_width);
                let is_metal_array = metal_array_size.is_some() || is_decomposed;

                if input_width > 128 || is_metal_array {
                    // Wide Bits type stored as array - need element access

                    // BUG FIX #71d: Handle decomposed signals (>256 bits)
                    // For decomposed signals, calculate which part to reference
                    let (actual_input_ref, adjusted_element_idx, adjusted_bit_offset) = if is_decomposed {
                        let sanitized = self.sanitize_name(input);
                        if let Some((_total_width, _num_parts, part_width)) = self.wide_signal_decomposition.get(&sanitized) {
                            // Calculate which part and which element within that part
                            let part_index = shift / part_width;
                            let bit_offset_in_part = shift % part_width;
                            let element_idx_in_part = bit_offset_in_part / 32;
                            let bit_offset_in_element = bit_offset_in_part % 32;

                            let part_ref = format!("signals->{}_part{}", sanitized, part_index);

                            eprintln!(
                                "   🎯 SLICE from DECOMPOSED signal: part={}, element={}, bit_offset={} (total_shift={}, part_width={})",
                                part_index, element_idx_in_part, bit_offset_in_element, shift, part_width
                            );

                            (part_ref, element_idx_in_part, bit_offset_in_element)
                        } else {
                            // Shouldn't happen, but fallback
                            let element_idx = shift / 32;
                            let bit_offset = shift % 32;
                            (input_ref.clone(), element_idx, bit_offset)
                        }
                    } else {
                        // Not decomposed - use original logic
                        let element_idx = shift / 32;
                        let bit_offset = shift % 32;
                        (input_ref.clone(), element_idx, bit_offset)
                    };

                    let mask = (1u64 << width) - 1;

                    eprintln!(
                        "   🎯 SLICE from array: {} (width={}, shift={}, is_decomposed={})",
                        input, input_width, shift, is_decomposed
                    );

                    self.write_indented(&format!(
                        "signals->{} = ({}[{}] >> {}) & 0x{:X};\n",
                        self.sanitize_name(output),
                        actual_input_ref,
                        adjusted_element_idx,
                        adjusted_bit_offset,
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
        // BUG FIX #52: Use SirType::width() method to handle all types correctly
        // Previously only handled Bits explicitly, causing Float16/Float32 to fall back
        // to sig.width field which might not be correctly set

        // Check signals - use sir_type.width() for all types
        if let Some(sig) = sir.signals.iter().find(|s| s.name == signal_name) {
            return sig.sir_type.width();
        }
        // Check inputs
        if let Some(input) = sir.inputs.iter().find(|i| i.name == signal_name) {
            return input.sir_type.width();
        }
        // Check outputs
        if let Some(output) = sir.outputs.iter().find(|o| o.name == signal_name) {
            return output.sir_type.width();
        }
        // Check state elements
        if let Some(state) = sir.state_elements.get(signal_name) {
            return state.width;
        }
        // Default
        32
    }

    /// Get the SirType for a signal
    /// BUG FIX #52: Helper to get signal type for proper Float handling
    #[allow(dead_code)]
    fn get_signal_type_from_sir(&self, sir: &SirModule, signal_name: &str) -> Option<SirType> {
        // Check signals
        if let Some(sig) = sir.signals.iter().find(|s| s.name == signal_name) {
            return Some(sig.sir_type.clone());
        }
        // Check inputs
        if let Some(input) = sir.inputs.iter().find(|i| i.name == signal_name) {
            return Some(input.sir_type.clone());
        }
        // Check outputs
        if let Some(output) = sir.outputs.iter().find(|o| o.name == signal_name) {
            return Some(output.sir_type.clone());
        }
        None
    }

    /// Format a signal reference with appropriate type casting for Metal
    /// BUG FIX #52: Auto-cast Float types to bits when used in bitwise operations
    /// BUG FIX #61: Used in concat to prevent float bitwise operation errors
    fn format_signal_for_bitwise_op(&self, sir: &SirModule, signal_name: &str) -> String {
        let sanitized = self.sanitize_name(signal_name);

        // Check if this signal has a Float type - use the more comprehensive lookup
        // BUG FIX #12: Use get_signal_sir_type instead of get_signal_type_from_sir
        if let Some(sir_type) = self.get_signal_sir_type(sir, signal_name) {
            if sir_type.is_float() {
                // Float signals need to be cast to their bit representation for bitwise ops
                let bit_type = match sir_type {
                    SirType::Float16 => "ushort",
                    SirType::Float32 => "uint",
                    SirType::Float64 => "ulong",
                    _ => return format!("signals->{}", sanitized),
                };
                return format!("as_type<{}>(signals->{})", bit_type, sanitized);
            }
        }

        // 32-bit or smaller signals can be used directly
        format!("signals->{}", sanitized)
    }

    /// Format a signal reference for use in expressions, handling vector component extraction
    /// BUG FIX #71b: Handle vec3/vec4 types stored as Metal vectors
    fn format_signal_reference(
        &self,
        sir: &SirModule,
        signal_name: &str,
        component_index: Option<usize>,
    ) -> String {
        let sanitized = self.sanitize_name(signal_name);
        let base_ref = format!("signals->{}", sanitized);

        // Get the signal width to determine Metal representation
        let width = self.get_signal_width_from_sir(sir, signal_name);
        if width == 0 {
            return base_ref;
        }

        // Check what Metal type this signal uses
        let (metal_type, metal_array_size) = self.get_metal_type_for_wide_bits(width);

        // If stored as array (>256 bits after decomposition)
        if metal_array_size.is_some() {
            let index = component_index.unwrap_or(0);
            return format!("{}[{}]", base_ref, index);
        }

        // BUG FIX #71b: If signal is stored as Metal vector type (uint2, uint4),
        // extract component when we need scalar value
        // uint2: 33-64 bits, uint4: 65-128 bits
        let is_metal_vector = metal_type == "uint2" || metal_type == "uint4";

        if is_metal_vector {
            if let Some(idx) = component_index {
                let component = match idx {
                    0 => "x",
                    1 => "y",
                    2 => "z",
                    3 => "w",
                    _ => "x",
                };
                return format!("{}.{}", base_ref, component);
            }
        }

        base_ref
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
            // BUG FIX #71: Use helper for decomposed outputs
            for i in 0..array_size {
                self.write_to_decomposed_array(output, i, "0");
            }

            // Concatenation packs from MSB to LSB
            // Inputs are ordered: [high_bits, ..., low_bits]
            // We need to pack them into array elements
            let mut bit_offset = 0;
            for (input_name, width) in input_widths.iter().rev() {
                let element_idx = bit_offset / 32;
                let bit_in_element = bit_offset % 32;

                if *width <= 32 && bit_in_element == 0 {
                    // Simple case: scalar input aligned to element boundary
                    // BUG FIX #61: Use format_signal_for_bitwise_op to handle float types
                    let input_ref = self.format_signal_for_bitwise_op(sir, input_name);
                    // BUG FIX #71: Use helper for decomposed outputs
                    self.write_to_decomposed_array(output, element_idx, &input_ref);
                } else if *width <= 32 {
                    // Scalar input not aligned - need bit manipulation
                    // BUG FIX #61: Use format_signal_for_bitwise_op to handle float types
                    let input_ref = self.format_signal_for_bitwise_op(sir, input_name);
                    // BUG FIX #71: Need to handle decomposed outputs with |= operator
                    // Generate the compound assignment differently
                    let sanitized_out = self.sanitize_name(output);
                    if let Some((_total_width, _num_parts, _part_width)) =
                        self.wide_signal_decomposition.get(&sanitized_out)
                    {
                        // Decomposed - figure out which part
                        let part_idx = element_idx / 8;
                        let idx_in_part = element_idx % 8;
                        let part_name = format!("{}_part{}", sanitized_out, part_idx);
                        self.write_indented(&format!(
                            "signals->{}[{}] |= ({} << {});\n",
                            part_name, idx_in_part, input_ref, bit_in_element
                        ));
                    } else {
                        // Normal case
                        self.write_indented(&format!(
                            "signals->{}[{}] |= ({} << {});\n",
                            sanitized_out, element_idx, input_ref, bit_in_element
                        ));
                    }
                } else {
                    // Wide input - copy element by element
                    let input_array_size = width.div_ceil(32);
                    let sanitized_input = self.sanitize_name(input_name);

                    // BUG FIX #71: Check if input signal is decomposed (>256 bits)
                    if let Some((_total_width, num_parts, _part_width)) = self
                        .wide_signal_decomposition
                        .get(&sanitized_input)
                        .cloned()
                    {
                        // Signal is decomposed - copy from parts
                        eprintln!(
                            "[CONCAT DECOMP] Copying from decomposed signal '{}' ({} parts)",
                            input_name, num_parts
                        );

                        // Copy from each part
                        let mut elements_copied = 0;
                        for part_idx in 0..num_parts {
                            let part_name = format!("{}_part{}", sanitized_input, part_idx);

                            // Each part is up to 256 bits = 8 uint elements
                            let is_last_part = part_idx == num_parts - 1;
                            let part_elements = if is_last_part {
                                // Last part might be smaller
                                input_array_size - elements_copied
                            } else {
                                8 // 256 bits / 32 bits per element
                            };

                            // BUG FIX #71e: Check if last part is a scalar (<64 bits)
                            let last_part_width = *width - (num_parts - 1) * 256;
                            let is_scalar = is_last_part && last_part_width <= 32;

                            for i in 0..part_elements {
                                let dest_elem = (bit_offset + (elements_copied + i) * 32) / 32;
                                // BUG FIX #71e: Use scalar reference for small last parts
                                let value_expr = if is_scalar && i == 0 {
                                    // Last part is a scalar - reference it directly without indexing
                                    format!("signals->{}", part_name)
                                } else if is_scalar && i > 0 {
                                    // Trying to access beyond scalar - shouldn't happen, skip
                                    eprintln!(
                                        "  ⚠️ WARNING: Skipping read from scalar part {}[{}]",
                                        part_name, i
                                    );
                                    continue;
                                } else {
                                    // Normal case: array indexing
                                    format!("signals->{}[{}]", part_name, i)
                                };
                                self.write_to_decomposed_array(output, dest_elem, &value_expr);
                            }
                            elements_copied += part_elements;
                        }
                    } else {
                        // Normal case - signal not decomposed
                        for i in 0..input_array_size {
                            let dest_elem = (bit_offset + i * 32) / 32;
                            // BUG FIX #71: Use helper for decomposed outputs
                            let value_expr = format!("signals->{}[{}]", sanitized_input, i);
                            self.write_to_decomposed_array(output, dest_elem, &value_expr);
                        }
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

            let mut components = vec!["0u".to_string(); 4];
            let mut bit_offset = 0;

            // BUG FIX #15: Hardware concat {a, b, c, d} has a in MSB, d in LSB
            // SystemVerilog {a, b, c, d} = {a[127:96], b[95:64], c[63:32], d[31:0]}
            // Metal uint4(x, y, z, w) = {x[31:0], y[63:32], z[95:64], w[127:96]}
            // So {a, b, c, d} maps to uint4(d, c, b, a) - REVERSE order
            for (input_name, width) in input_widths.iter() {
                let component_idx = bit_offset / 32;
                if component_idx < 4 {
                    // BUG FIX #61: Use format_signal_for_bitwise_op to handle float types
                    let component_str = self.format_signal_for_bitwise_op(sir, input_name);

                    // BUG FIX #12: uint4 constructor requires all arguments to be exactly 'unsigned int'
                    // Handle different input widths:
                    // - Literals (0u) and as_type<> casts: use as-is
                    // - 32-bit or smaller non-floats: cast to (uint)
                    // - Wider than 32 bits: extract first 32-bit component (signal.x or signal[0])
                    // Note: format_signal_for_bitwise_op may already have done extraction for wide signals
                    if component_str == "0u" || component_str.contains("as_type") {
                        // Literal or already has as_type cast - use as-is
                        components[component_idx] = component_str;
                    } else if component_str.contains(".x") || component_str.contains("[0]") {
                        // Already has component extraction - just cast to uint if needed
                        components[component_idx] = format!("(uint)({})", component_str);
                    } else if *width <= 32 {
                        // 32-bit or smaller - cast to uint to ensure correct type
                        components[component_idx] = format!("(uint)({})", component_str);
                    } else {
                        // Wider than 32 bits - it's a vector (uint2/uint4) or array in Metal
                        // Extract the first 32-bit component
                        let (_, metal_array_size) = self.get_metal_type_for_wide_bits(*width);
                        if metal_array_size.is_some() {
                            // Array storage - use array indexing
                            components[component_idx] = format!("{}[0]", component_str);
                        } else {
                            // Vector storage (uint2/uint4) - use .x component
                            components[component_idx] = format!("{}.x", component_str);
                        }
                    }
                }
                bit_offset += width;
            }

            // REVERSE components: SystemVerilog MSB-first → Metal LSB-first
            self.write_indented(&format!(
                "signals->{} = uint4({}, {}, {}, {});\n",
                self.sanitize_name(output),
                components[3],
                components[2],
                components[1],
                components[0]
            ));
        } else if output_width > 32 {
            // Output is 33-64 bits -> uint2
            // Construct uint2 from 32-bit components
            self.write_indented(&format!(
                "// Concat: pack inputs into {}-bit output (uint2)\n",
                output_width
            ));

            let mut components = vec!["0u".to_string(); 2];
            let mut bit_offset = 0;

            // BUG FIX #15: Hardware concat {a, b} has a in MSB, b in LSB
            // SystemVerilog {a, b} = {a[63:32], b[31:0]}
            // Metal uint2(x, y) = {x[31:0], y[63:32]}
            // So {a, b} maps to uint2(b, a) - REVERSE order
            for (input_name, width) in input_widths.iter() {
                let component_idx = bit_offset / 32;
                if component_idx < 2 {
                    // BUG FIX #61: Use format_signal_for_bitwise_op to handle float types
                    let component_str = self.format_signal_for_bitwise_op(sir, input_name);

                    // BUG FIX #12: uint2 constructor requires all arguments to be exactly 'unsigned int'
                    // Note: format_signal_for_bitwise_op may already have done extraction for wide signals
                    if component_str == "0u" || component_str.contains("as_type") {
                        components[component_idx] = component_str;
                    } else if component_str.contains(".x") || component_str.contains("[0]") {
                        // Already has component extraction - just cast to uint if needed
                        components[component_idx] = format!("(uint)({})", component_str);
                    } else if *width <= 32 {
                        components[component_idx] = format!("(uint)({})", component_str);
                    } else {
                        // Wider than 32 bits - extract first 32-bit component
                        let (_, metal_array_size) = self.get_metal_type_for_wide_bits(*width);
                        if metal_array_size.is_some() {
                            components[component_idx] = format!("{}[0]", component_str);
                        } else {
                            components[component_idx] = format!("{}.x", component_str);
                        }
                    }
                }
                bit_offset += width;
            }

            // REVERSE components: SystemVerilog MSB-first → Metal LSB-first
            self.write_indented(&format!(
                "signals->{} = uint2({}, {});\n",
                self.sanitize_name(output),
                components[1],
                components[0]
            ));
        } else {
            // Output is 1-32 bits -> uint
            // Use bit shifts (safe because all fits in 32 bits)
            // BUG FIX #61: Use format_signal_for_bitwise_op to handle float types
            // BUG FIX #14: Extract lower 32 bits from wide inputs to avoid uint4 OR results
            let mut shift = 0;
            let mut concat_expr = String::new();
            for (input_name, width) in input_widths.iter().rev() {
                // Skip shifts that would overflow the output width
                // If shift >= output_width, this input won't contribute to the output
                if shift >= output_width {
                    shift += width;
                    continue;
                }

                let mut input_ref = self.format_signal_for_bitwise_op(sir, input_name);

                // If input is wide (>32 bits), extract lower 32 bits to avoid type mismatch
                // Wide inputs are stored as uint2/uint4 in Metal, and OR on uint4 produces uint4
                // which can't be assigned to uint output
                if *width > 32 {
                    let (_, metal_array_size) = self.get_metal_type_for_wide_bits(*width);
                    if metal_array_size.is_some() {
                        // Array storage - use array indexing
                        input_ref = format!("{}[0]", input_ref);
                    } else {
                        // Vector storage (uint2/uint4) - use .x component
                        input_ref = format!("{}.x", input_ref);
                    }
                }

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
    }

    fn generate_node_computation_v2(&mut self, sir: &SirModule, node: &SirNode) {
        if matches!(node.kind, SirNodeKind::Concat) {
            eprintln!("🔧 MATCH CONCAT: node {}", node.id);
        }
        match &node.kind {
            SirNodeKind::BinaryOp(op) => self.generate_binary_op(sir, node, op),
            SirNodeKind::UnaryOp(op) => self.generate_unary_op(sir, node, op),
            SirNodeKind::Constant { value, width } => {
                self.generate_constant(sir, node, *value, *width)
            }
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
                    if output.contains("node")
                        || output.contains("fu_result")
                        || output.contains("computed")
                    {
                        eprintln!("🔍 SignalRef: {} <- {} | source_width={}, output_width={}, is_array={}, is_wide_bits={}",
                                  output, signal, source_width, output_width, is_array, is_wide_bits);
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
                        // Wide Bits type (>128 bits) - check if decomposed
                        // BUG FIX #71: Handle decomposed signals (>256 bits)
                        let source_sanitized = self.sanitize_name(signal);
                        let output_sanitized = self.sanitize_name(output);

                        let source_is_decomposed = self.wide_signal_decomposition.contains_key(&source_sanitized);
                        let output_is_decomposed = self.wide_signal_decomposition.contains_key(&output_sanitized);

                        if source_is_decomposed || output_is_decomposed {
                            // At least one signal is decomposed - copy part by part
                            let source_parts = if let Some((_, num_parts, _)) = self.wide_signal_decomposition.get(&source_sanitized) {
                                *num_parts
                            } else {
                                1 // Source not decomposed, treat as single part
                            };

                            let output_parts = if let Some((_, num_parts, _)) = self.wide_signal_decomposition.get(&output_sanitized) {
                                *num_parts
                            } else {
                                1 // Output not decomposed, treat as single part
                            };

                            self.write_indented(&format!(
                                "// BUG FIX #71: Decomposed signal copy ({}-bit)\n",
                                output_width
                            ));

                            // Copy each part
                            let copy_parts = source_parts.min(output_parts);
                            for part_idx in 0..copy_parts {
                                let source_part = if source_is_decomposed {
                                    format!("signals->{}_part{}", source_sanitized, part_idx)
                                } else if sir.inputs.iter().any(|i| i.name == *signal) {
                                    format!("inputs->{}", source_sanitized)
                                } else {
                                    format!("signals->{}", source_sanitized)
                                };

                                let output_part = if output_is_decomposed {
                                    format!("signals->{}_part{}", output_sanitized, part_idx)
                                } else {
                                    format!("signals->{}", output_sanitized)
                                };

                                self.write_indented(&format!(
                                    "for (uint i = 0; i < 8; i++) {{ {}[i] = {}[i]; }}\n",
                                    output_part, source_part
                                ));
                            }
                        } else {
                            // Neither decomposed - regular element-wise copy
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
                        }
                    } else if source_width > 32 && source_width <= 128 && output_width > 128 {
                        // BUG FIX #65 & Metal Backend: Vector/Array-to-array conversion
                        // Source could be uint2/uint4 (vector) OR uint[N] (array)
                        // Need to check the actual source type to decide access pattern
                        let source_location = if sir.inputs.iter().any(|i| i.name == *signal) {
                            format!("inputs->{}", self.sanitize_name(signal))
                        } else if sir.state_elements.contains_key(signal) {
                            format!("registers->{}", self.sanitize_name(signal))
                        } else {
                            format!("signals->{}", self.sanitize_name(signal))
                        };

                        let array_size = output_width.div_ceil(32);
                        let vector_components = source_width.div_ceil(32);

                        // Check if source is actually a vector type (supports .x/.y/.z/.w) or an array
                        let source_sir_type = self.get_signal_sir_type(sir, signal);
                        let source_is_vector = match source_sir_type {
                            Some(SirType::Vec2(_))
                            | Some(SirType::Vec3(_))
                            | Some(SirType::Vec4(_)) => {
                                // Check if Metal type would be uint2/uint4 or float2/float3/float4
                                source_width == 64 || source_width == 96 || source_width == 128
                            }
                            _ => false, // Bits type or array - use array indexing
                        };

                        if source_is_vector {
                            eprintln!(
                                "   🎯 Vector->Array: {} ({} bits, {} components) -> {} ({} bits, uint[{}])",
                                signal, source_width, vector_components, output, output_width, array_size
                            );

                            self.write_indented(&format!(
                                "// BUG FIX #65: Unpack {}-bit vector into {}-bit array\n",
                                source_width, output_width
                            ));

                            // Unpack vector components into array elements
                            let component_names = ["x", "y", "z", "w"];
                            #[allow(clippy::needless_range_loop)]
                            for i in 0..vector_components.min(4) {
                                self.write_indented(&format!(
                                    "signals->{}[{}] = {}.{};\n",
                                    self.sanitize_name(output),
                                    i,
                                    source_location,
                                    component_names[i]
                                ));
                            }
                        } else {
                            // Source is an array - use array indexing
                            eprintln!(
                                "   🎯 Array->Array: {} ({} bits, uint[{}]) -> {} ({} bits, uint[{}])",
                                signal, source_width, vector_components, output, output_width, array_size
                            );

                            self.write_indented(&format!(
                                "// Metal Backend Fix: Copy {}-bit array to {}-bit array\n",
                                source_width, output_width
                            ));

                            // Copy array elements
                            let copy_elements = vector_components.min(array_size);
                            for i in 0..copy_elements {
                                self.write_indented(&format!(
                                    "signals->{}[{}] = {}[{}];\n",
                                    self.sanitize_name(output),
                                    i,
                                    source_location,
                                    i
                                ));
                            }
                        }

                        // Zero out remaining array elements if output is wider than source
                        if array_size > vector_components {
                            self.write_indented(&format!(
                                "// Zero-pad remaining {} elements\n",
                                array_size - vector_components
                            ));
                            self.write_indented(&format!(
                                "for (uint i = {}; i < {}; i++) {{\n",
                                vector_components, array_size
                            ));
                            self.indent += 1;
                            self.write_indented(&format!(
                                "signals->{}[i] = 0;\n",
                                self.sanitize_name(output)
                            ));
                            self.indent -= 1;
                            self.write_indented("}\n");
                        }
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
                            // BUG FIX #10 & #71: Check if source is actually stored as a vector before using .x
                            // Use safe version to handle decomposed signals
                            let (_, metal_array_size, source_decomposed) =
                                self.get_metal_type_safe(signal, source_width);
                            let is_metal_array = metal_array_size.is_some() || source_decomposed;

                            if is_metal_array {
                                // Source is stored as array - use array indexing
                                eprintln!(
                                    "   🎯 Array->Scalar: {} ({} bits, array) -> {} ({} bits), extracting [0]",
                                    signal, source_width, output, output_width
                                );
                                self.write_indented(&format!(
                                    "signals->{} = {}[0];\n",
                                    self.sanitize_name(output),
                                    source_location
                                ));
                            } else {
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
                            }
                        } else if source_width <= 32 && output_width > 32 && output_width <= 64 {
                            // Source is scalar, destination is uint2 - construct vector
                            // Metal Backend: Check if source is float and needs cast
                            let source_type = self.get_signal_sir_type(sir, signal);
                            let source_is_float =
                                source_type.as_ref().is_some_and(|st| st.is_float());
                            let source_expr = if source_is_float {
                                format!("as_type<uint>({})", source_location)
                            } else {
                                source_location.clone()
                            };

                            eprintln!(
                                "   🎯 Scalar->uint2: {} ({} bits, float={}) -> {} ({} bits)",
                                signal, source_width, source_is_float, output, output_width
                            );
                            self.write_indented(&format!(
                                "signals->{} = uint2({}, 0u);\n",
                                self.sanitize_name(output),
                                source_expr
                            ));
                        } else if source_width <= 32 && output_width > 64 && output_width <= 128 {
                            // Source is scalar, destination is uint4 - construct vector
                            // Metal Backend: Check if source is float and needs cast
                            let source_type = self.get_signal_sir_type(sir, signal);
                            let source_is_float =
                                source_type.as_ref().is_some_and(|st| st.is_float());
                            // BUG FIX #12: uint4 constructor requires all arguments to be exactly 'unsigned int'
                            // - Floats: use as_type<uint>() for bit reinterpretation
                            // - Other types: wrap with (uint) to ensure correct type
                            let source_expr = if source_is_float {
                                format!("as_type<uint>({})", source_location)
                            } else {
                                format!("(uint)({})", source_location)
                            };

                            eprintln!(
                                "   🎯 Scalar->uint4: {} ({} bits, float={}) -> {} ({} bits)",
                                signal, source_width, source_is_float, output, output_width
                            );
                            self.write_indented(&format!(
                                "signals->{} = uint4({}, 0u, 0u, 0u);\n",
                                self.sanitize_name(output),
                                source_expr
                            ));
                        } else {
                            // BUG FIX #58 & Metal Backend: Check if source and dest have different Metal types requiring reinterpretation
                            let source_type = self.get_signal_sir_type(sir, signal);
                            let dest_type = self.get_signal_sir_type(sir, output);

                            // Check if source or destination is a float type
                            let source_is_float =
                                source_type.as_ref().is_some_and(|st| st.is_float());
                            let dest_is_float = dest_type.as_ref().is_some_and(|dt| dt.is_float());

                            // Metal Backend Fix: Get actual Metal type names to detect float4 <-> uint4 mismatches
                            let source_metal_type = if let Some(ref st) = source_type {
                                let (base, _) = self.get_metal_type_parts(st);
                                base
                            } else {
                                // Fallback for unknown types based on width
                                match source_width {
                                    0..=32 => "uint".to_string(),
                                    64 => "uint2".to_string(),
                                    96 | 128 => "uint4".to_string(),
                                    _ => format!("uint[{}]", source_width.div_ceil(32)),
                                }
                            };

                            let dest_metal_type = if let Some(ref dt) = dest_type {
                                let (base, _) = self.get_metal_type_parts(dt);
                                base
                            } else {
                                // Fallback for unknown types based on width
                                match output_width {
                                    0..=32 => "uint".to_string(),
                                    64 => "uint2".to_string(),
                                    96 | 128 => "uint4".to_string(),
                                    _ => format!("uint[{}]", output_width.div_ceil(32)),
                                }
                            };

                            // Check if we need type reinterpretation
                            // Metal Backend: Need cast if Metal types differ (e.g., float4 vs uint4)
                            let needs_reinterpretation = source_metal_type != dest_metal_type;

                            // BUG DEBUG #62 & Metal Backend: Log type mismatch detection
                            if source_is_float != dest_is_float || needs_reinterpretation {
                                eprintln!(
                                    "🔧 Metal Backend: Type check: {} ({}:{}) -> {} ({}:{}), needs_cast={}",
                                    signal, source_metal_type, source_is_float, output, dest_metal_type, dest_is_float, needs_reinterpretation
                                );
                            }

                            if needs_reinterpretation {
                                // Metal Backend: Use the Metal type we already computed
                                let dest_type_name = dest_metal_type.clone();

                                // BUG FIX #59: Check width matching for as_type<> validity
                                // Metal's as_type<> requires EXACT size match

                                // Debug for specific problematic signals
                                if output.contains("_fp16")
                                    || output.contains("a_fp16")
                                    || output.contains("b_fp16")
                                {
                                    eprintln!(
                                        "[NODE ASSIGN] {} = {} | src_width={}, dst_width={}",
                                        output, signal, source_width, output_width
                                    );
                                }

                                use std::cmp::Ordering;
                                match source_width.cmp(&output_width) {
                                    Ordering::Equal => {
                                        // Same width, can use as_type<> directly
                                        eprintln!(
                                            "   🔄 Type reinterpretation: {} -> {} (as_type<{}>)",
                                            signal, output, dest_type_name
                                        );
                                        self.write_indented(&format!(
                                            "signals->{} = as_type<{}>({});\n",
                                            self.sanitize_name(output),
                                            dest_type_name,
                                            source_location
                                        ));
                                    }
                                    Ordering::Greater => {
                                        // Source wider: extract lower bits first, then reinterpret
                                        // Metal Backend: Only use as_type<> if dest is float, otherwise just cast
                                        let intermediate_type = match output_width {
                                            8 => "uchar",
                                            16 => "ushort",
                                            32 => "uint",
                                            64 => "ulong",
                                            _ => "uint",
                                        };

                                        let source_expr = if source_is_float {
                                            // Convert float to bits first, then narrow
                                            // e.g., float(32) -> half(16): (ushort)as_type<uint>(float_source)
                                            let source_intermediate = match source_width {
                                                64 => "ulong",
                                                _ => "uint",
                                            };
                                            format!(
                                                "as_type<{}>({})",
                                                source_intermediate, source_location
                                            )
                                        } else {
                                            source_location.clone()
                                        };

                                        if dest_is_float {
                                            // Destination is float - need as_type<> for bit reinterpretation
                                            // e.g., uint(32) -> half(16): as_type<half>((ushort)source)
                                            eprintln!(
                                                "   🔄 Narrow+Reinterpret to float: {} ({} bits, src_float={}) -> {} ({} bits): as_type<{}>(({}){}) ",
                                                signal, source_width, source_is_float, output, output_width, dest_type_name, intermediate_type, source_expr
                                            );
                                            self.write_indented(&format!(
                                                "signals->{} = as_type<{}>(({}){}); \n",
                                                self.sanitize_name(output),
                                                dest_type_name,
                                                intermediate_type,
                                                source_expr
                                            ));
                                        } else {
                                            // Destination is integer - just use C-style cast
                                            // e.g., uint(32) -> uchar(8): (uchar)source
                                            eprintln!(
                                                "   🔄 Narrow to int: {} ({} bits, src_float={}) -> {} ({} bits): ({}){} ",
                                                signal, source_width, source_is_float, output, output_width, intermediate_type, source_expr
                                            );
                                            self.write_indented(&format!(
                                                "signals->{} = ({}){};\n",
                                                self.sanitize_name(output),
                                                intermediate_type,
                                                source_expr
                                            ));
                                        }
                                    }
                                    Ordering::Less => {
                                        // Source narrower: widen source first, then reinterpret
                                        // Metal Backend: Only use as_type<> if dest is float, otherwise just cast
                                        let intermediate_type = match output_width {
                                            16 => "ushort",
                                            32 => "uint",
                                            64 => "ulong",
                                            _ => "uint",
                                        };

                                        let source_expr = if source_is_float {
                                            // Convert float to bits first, then widen
                                            // e.g., float(32) -> ulong(64): (ulong)as_type<uint>(float_source)
                                            format!("as_type<uint>({})", source_location)
                                        } else {
                                            source_location.clone()
                                        };

                                        if dest_is_float {
                                            // Destination is float - need as_type<> for bit reinterpretation
                                            // e.g., uchar(8) -> float(32): as_type<float>((uint)source)
                                            eprintln!(
                                                "   🔄 Widen+Reinterpret to float: {} ({} bits, src_float={}) -> {} ({} bits): as_type<{}>(({}){}) ",
                                                signal, source_width, source_is_float, output, output_width, dest_type_name, intermediate_type, source_expr
                                            );
                                            self.write_indented(&format!(
                                                "signals->{} = as_type<{}>(({}){}); \n",
                                                self.sanitize_name(output),
                                                dest_type_name,
                                                intermediate_type,
                                                source_expr
                                            ));
                                        } else {
                                            // Destination is integer - just use C-style cast
                                            // e.g., uchar(8) -> uint(32): (uint)source
                                            eprintln!(
                                                "   🔄 Widen to int: {} ({} bits, src_float={}) -> {} ({} bits): ({}){} ",
                                                signal, source_width, source_is_float, output, output_width, intermediate_type, source_expr
                                            );
                                            self.write_indented(&format!(
                                                "signals->{} = ({}){};\n",
                                                self.sanitize_name(output),
                                                intermediate_type,
                                                source_expr
                                            ));
                                        }
                                    }
                                }
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
                } else if output_width > 128 && source_width > 32 && source_width <= 128 {
                    // BUG FIX #65 & Metal Backend: Vector/Array-to-array conversion for additional outputs
                    let array_size = output_width.div_ceil(32);
                    let vector_components = source_width.div_ceil(32);

                    // Check if source is actually a vector type
                    let source_sir_type = self.get_signal_sir_type(sir, first_output);
                    let source_is_vector = matches!(
                        source_sir_type,
                        Some(SirType::Vec2(_)) | Some(SirType::Vec3(_)) | Some(SirType::Vec4(_))
                    ) && (source_width == 64
                        || source_width == 96
                        || source_width == 128);

                    if source_is_vector {
                        eprintln!(
                            "   🎯 Additional output Vector->Array: {} ({} bits, {} components) -> {} ({} bits, uint[{}])",
                            first_output, source_width, vector_components, additional_output.signal_id, output_width, array_size
                        );

                        self.write_indented(&format!(
                            "// BUG FIX #65: Unpack {}-bit vector into {}-bit array (additional output)\n",
                            source_width, output_width
                        ));

                        // Unpack vector components into array elements
                        let component_names = ["x", "y", "z", "w"];
                        #[allow(clippy::needless_range_loop)]
                        for i in 0..vector_components.min(4) {
                            self.write_indented(&format!(
                                "signals->{}[{}] = signals->{}.{};\n",
                                self.sanitize_name(&additional_output.signal_id),
                                i,
                                self.sanitize_name(first_output),
                                component_names[i]
                            ));
                        }
                    } else {
                        // Source is an array - use array indexing
                        eprintln!(
                            "   🎯 Additional output Array->Array: {} ({} bits, uint[{}]) -> {} ({} bits, uint[{}])",
                            first_output, source_width, vector_components, additional_output.signal_id, output_width, array_size
                        );

                        self.write_indented(&format!(
                            "// Metal Backend Fix: Copy {}-bit array to {}-bit array (additional output)\n",
                            source_width, output_width
                        ));

                        // Copy array elements
                        let copy_elements = vector_components.min(array_size);
                        for i in 0..copy_elements {
                            self.write_indented(&format!(
                                "signals->{}[{}] = signals->{}[{}];\n",
                                self.sanitize_name(&additional_output.signal_id),
                                i,
                                self.sanitize_name(first_output),
                                i
                            ));
                        }
                    }

                    // Zero out remaining array elements if output is wider than source
                    if array_size > vector_components {
                        self.write_indented(&format!(
                            "// Zero-pad remaining {} elements\n",
                            array_size - vector_components
                        ));
                        self.write_indented(&format!(
                            "for (uint i = {}; i < {}; i++) {{\n",
                            vector_components, array_size
                        ));
                        self.indent += 1;
                        self.write_indented(&format!(
                            "signals->{}[i] = 0;\n",
                            self.sanitize_name(&additional_output.signal_id)
                        ));
                        self.indent -= 1;
                        self.write_indented("}\n");
                    }
                } else if output_width > 128 && source_width <= 32 {
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
                    // BUG FIX #10: Check if source is actually stored as a vector before using .x
                    let (_, metal_array_size) = self.get_metal_type_for_wide_bits(source_width);
                    let is_metal_array = metal_array_size.is_some();

                    if is_metal_array {
                        // Source is stored as array - use array indexing
                        eprintln!(
                            "   🎯 Additional output array->scalar: {} ({} bits, array) -> {} ({} bits)",
                            first_output, source_width, additional_output.signal_id, output_width
                        );
                        self.write_indented(&format!(
                            "signals->{} = signals->{}[0];\n",
                            self.sanitize_name(&additional_output.signal_id),
                            self.sanitize_name(first_output)
                        ));
                    } else {
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
                    }
                } else if source_width <= 32 && output_width > 32 && output_width <= 64 {
                    // Source is scalar, destination is uint2 - construct vector
                    // Metal Backend: Check if source is float and needs cast
                    let source_type = self.get_signal_sir_type(sir, first_output);
                    let source_is_float = source_type.as_ref().is_some_and(|st| st.is_float());
                    let source_expr = if source_is_float {
                        format!(
                            "as_type<uint>(signals->{})",
                            self.sanitize_name(first_output)
                        )
                    } else {
                        format!("signals->{}", self.sanitize_name(first_output))
                    };

                    eprintln!(
                        "   🎯 Additional output scalar->uint2: {} ({} bits, float={}) -> {} ({} bits)",
                        first_output, source_width, source_is_float, additional_output.signal_id, output_width
                    );
                    self.write_indented(&format!(
                        "signals->{} = uint2({}, 0u);\n",
                        self.sanitize_name(&additional_output.signal_id),
                        source_expr
                    ));
                } else if source_width <= 32 && output_width > 64 && output_width <= 128 {
                    // Source is scalar, destination is uint4 - construct vector
                    // Metal Backend: Check if source is float and needs cast
                    let source_type = self.get_signal_sir_type(sir, first_output);
                    let source_is_float = source_type.as_ref().is_some_and(|st| st.is_float());
                    // BUG FIX #12: uint4 constructor requires all arguments to be exactly 'unsigned int'
                    // - Floats: use as_type<uint>() for bit reinterpretation
                    // - Other types: wrap with (uint) to ensure correct type
                    let source_expr = if source_is_float {
                        format!(
                            "as_type<uint>(signals->{})",
                            self.sanitize_name(first_output)
                        )
                    } else {
                        format!("(uint)(signals->{})", self.sanitize_name(first_output))
                    };

                    eprintln!(
                        "   🎯 Additional output scalar->uint4: {} ({} bits, float={}) -> {} ({} bits)",
                        first_output, source_width, source_is_float, additional_output.signal_id, output_width
                    );
                    self.write_indented(&format!(
                        "signals->{} = uint4({}, 0u, 0u, 0u);\n",
                        self.sanitize_name(&additional_output.signal_id),
                        source_expr
                    ));
                } else {
                    // Both scalar (≤128 bits) - check for type reinterpretation
                    // BUG FIX #62: Check if source and dest have different types (float <-> bits)
                    let source_type = self.get_signal_sir_type(sir, first_output);
                    let dest_type = self.get_signal_sir_type(sir, &additional_output.signal_id);

                    let source_is_float = source_type.as_ref().is_some_and(|st| st.is_float());
                    let dest_is_float = dest_type.as_ref().is_some_and(|dt| dt.is_float());
                    let needs_reinterpretation = source_is_float != dest_is_float;

                    if needs_reinterpretation {
                        // Need to reinterpret bits when converting between float and non-float
                        // BUG FIX: Check width matching for as_type<> validity
                        let source_width = self.get_signal_width_from_sir(sir, first_output);

                        let dest_metal_type = if dest_is_float {
                            match output_width {
                                16 => "half",
                                32 => "float",
                                64 => "double",
                                _ => "float",
                            }
                        } else {
                            match output_width {
                                8 => "uchar",
                                16 => "ushort",
                                32 => "uint",
                                64 => "ulong",
                                _ => "uint",
                            }
                        };
                        eprintln!(
                            "   🔄 Additional output reinterpretation: {} ({}) -> {} ({})",
                            first_output,
                            if source_is_float { "float" } else { "bits" },
                            additional_output.signal_id,
                            if dest_is_float { "float" } else { "bits" }
                        );

                        // Metal as_type<> requires exact width match
                        use std::cmp::Ordering;
                        let source_location =
                            format!("signals->{}", self.sanitize_name(first_output));
                        match source_width.cmp(&output_width) {
                            Ordering::Equal => {
                                // Same width - direct as_type<>
                                self.write_indented(&format!(
                                    "signals->{} = as_type<{}>({});\n",
                                    self.sanitize_name(&additional_output.signal_id),
                                    dest_metal_type,
                                    source_location
                                ));
                            }
                            Ordering::Greater => {
                                // Source wider: narrow first, then reinterpret
                                let intermediate_type = match output_width {
                                    16 => "ushort",
                                    32 => "uint",
                                    64 => "ulong",
                                    _ => "uint",
                                };
                                self.write_indented(&format!(
                                    "signals->{} = as_type<{}>(({}){}); \n",
                                    self.sanitize_name(&additional_output.signal_id),
                                    dest_metal_type,
                                    intermediate_type,
                                    source_location
                                ));
                            }
                            Ordering::Less => {
                                // Source narrower: widen first, then reinterpret
                                let intermediate_type = match output_width {
                                    16 => "ushort",
                                    32 => "uint",
                                    64 => "ulong",
                                    _ => "uint",
                                };
                                self.write_indented(&format!(
                                    "signals->{} = as_type<{}>(({}){}); \n",
                                    self.sanitize_name(&additional_output.signal_id),
                                    dest_metal_type,
                                    intermediate_type,
                                    source_location
                                ));
                            }
                        }
                    } else {
                        // BUG FIX #11: Check Metal type compatibility even when SIR types match
                        // float4 and uint4 are different types in Metal!
                        let output_type =
                            self.get_signal_sir_type(sir, &additional_output.signal_id);
                        let source_type = self.get_signal_sir_type(sir, first_output);

                        let output_metal =
                            output_type.as_ref().map(|t| self.get_metal_type_parts(t).0);
                        let source_metal =
                            source_type.as_ref().map(|t| self.get_metal_type_parts(t).0);

                        let needs_cast = output_metal.is_some()
                            && source_metal.is_some()
                            && output_metal != source_metal;

                        if needs_cast {
                            eprintln!(
                                "   🔄 Additional output Metal type mismatch: {} ({}) -> {} ({}), adding as_type cast",
                                first_output, source_metal.as_ref().unwrap(),
                                additional_output.signal_id, output_metal.as_ref().unwrap()
                            );
                            self.write_indented(&format!(
                                "signals->{} = as_type<{}>(signals->{});\n",
                                self.sanitize_name(&additional_output.signal_id),
                                output_metal.unwrap(),
                                self.sanitize_name(first_output)
                            ));
                        } else {
                            // Same Metal type - direct assignment
                            self.write_indented(&format!(
                                "signals->{} = signals->{};\n",
                                self.sanitize_name(&additional_output.signal_id),
                                self.sanitize_name(first_output)
                            ));
                        }
                    }
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

                            // BUG FIX #65 & Metal Backend: Check if data is actually a vector type
                            let data_sir_type = self.get_signal_sir_type(sir, data_signal);
                            let data_is_vector = matches!(
                                data_sir_type,
                                Some(SirType::Vec2(_))
                                    | Some(SirType::Vec3(_))
                                    | Some(SirType::Vec4(_))
                            ) && data_signal_width > 32
                                && data_signal_width <= 128;

                            if data_is_vector {
                                // Data is float2/float3/float4 or uint2/uint4 vector, unpack into array elements
                                let vector_components = data_signal_width.div_ceil(32);

                                eprintln!(
                                    "   🎯 Sequential Vector->Array: {} ({} bits, {} components) -> register {} ({} bits)",
                                    data_signal, data_signal_width, vector_components, output.signal_id, state_elem.width
                                );

                                self.write_indented(&format!(
                                    "// BUG FIX #65: Unpack {}-bit vector into {}-bit register array\n",
                                    data_signal_width, state_elem.width
                                ));

                                // Unpack vector components
                                let component_names = ["x", "y", "z", "w"];
                                #[allow(clippy::needless_range_loop)]
                                for i in 0..vector_components.min(4) {
                                    self.write_indented(&format!(
                                        "registers->{}[{}] = signals->{}.{};\n",
                                        self.sanitize_name(&output.signal_id),
                                        i,
                                        self.sanitize_name(data_signal),
                                        component_names[i]
                                    ));
                                }
                            } else if data_is_wide {
                                // Data is also a wide array - copy array elements
                                let data_array_size = data_signal_width.div_ceil(32);
                                let copy_elements = data_array_size.min(array_size);

                                eprintln!(
                                    "   🎯 Sequential Array->Array: {} ({} bits, uint[{}]) -> register {} ({} bits, uint[{}])",
                                    data_signal, data_signal_width, data_array_size, output.signal_id, state_elem.width, array_size
                                );

                                self.write_indented(&format!(
                                    "// Metal Backend Fix: Copy {}-bit array to {}-bit register array\n",
                                    data_signal_width, state_elem.width
                                ));

                                // Copy array elements
                                for i in 0..copy_elements {
                                    self.write_indented(&format!(
                                        "registers->{}[{}] = signals->{}[{}];\n",
                                        self.sanitize_name(&output.signal_id),
                                        i,
                                        self.sanitize_name(data_signal),
                                        i
                                    ));
                                }

                                // Zero out remaining elements if register is wider than data
                                if array_size > copy_elements {
                                    self.write_indented(&format!(
                                        "for (uint i = {}; i < {}; i++) {{\n",
                                        copy_elements, array_size
                                    ));
                                    self.indent += 1;
                                    self.write_indented(&format!(
                                        "registers->{}[i] = 0;\n",
                                        self.sanitize_name(&output.signal_id)
                                    ));
                                    self.indent -= 1;
                                    self.write_indented("}\n");
                                }
                            } else {
                                // Data is either array or scalar
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
                            }
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
        // BUG FIX #57: Use appropriate Metal types for different bit widths
        // This ensures Bits(16) maps to ushort, not uint
        if width > 256 {
            eprintln!(
                "❌ BUG #71 PANIC: Attempting to create Metal type for width={} (max 256 bits)",
                width
            );
            eprintln!("❌ BUG #71 PANIC: This indicates a 288-bit signal was created in SIR");
            eprintln!("❌ BUG #71 PANIC: Check stack trace to see which signal");
        }
        match width {
            1..=8 => ("uchar".to_string(), None),
            9..=16 => ("ushort".to_string(), None),
            17..=32 => ("uint".to_string(), None),
            33..=64 => ("uint2".to_string(), None),
            65..=128 => ("uint4".to_string(), None),
            129..=256 => ("uint".to_string(), Some(8)), // uint[8] for 256 bits
            _ => panic!(
                "Unsupported bit width {} for Metal codegen (max 256 bits)",
                width
            ),
        }
    }

    /// Safe version of get_metal_type_for_wide_bits that handles decomposed signals
    /// BUG FIX #71: For signals > 256 bits, returns info about first decomposed part
    /// Returns (base_type, array_size, is_decomposed)
    fn get_metal_type_safe(&self, signal_name: &str, width: usize) -> (String, Option<usize>, bool) {
        let sanitized = self.sanitize_name(signal_name);

        // Check if this signal is decomposed
        if let Some((total_width, _num_parts, part_width)) = self.wide_signal_decomposition.get(&sanitized) {
            eprintln!(
                "[METAL DECOMP SAFE] Signal '{}' is decomposed: total={} bits, part={} bits",
                signal_name, total_width, part_width
            );
            // Return type info for one part
            let (base_type, array_size) = self.get_metal_type_for_wide_bits(*part_width);
            return (base_type, array_size, true);
        }

        // Not decomposed, use regular logic
        if width <= 256 {
            let (base_type, array_size) = self.get_metal_type_for_wide_bits(width);
            (base_type, array_size, false)
        } else {
            // Signal should have been decomposed but wasn't found in HashMap
            // This shouldn't happen if generate_signal_field was called properly
            eprintln!(
                "⚠️ WARNING: Signal '{}' has width {} but not found in decomposition map",
                signal_name, width
            );
            // Fall back to treating as 256-bit array
            ("uint".to_string(), Some(8), false)
        }
    }

    /// Returns (base_type, array_suffix) for Metal type declarations
    /// For example: Array(Bits(32), 16) returns ("uint", "[16]")
    /// Non-arrays return ("type", "")
    fn get_metal_type_parts(&self, sir_type: &SirType) -> (String, String) {
        self.get_metal_type_parts_impl(sir_type, false)
    }

    /// Get Metal type with option to force 4-byte alignment for struct fields
    fn get_metal_type_parts_for_struct(&self, sir_type: &SirType) -> (String, String) {
        self.get_metal_type_parts_impl(sir_type, true)
    }

    fn get_metal_type_parts_impl(
        &self,
        sir_type: &SirType,
        force_4byte_align: bool,
    ) -> (String, String) {
        match sir_type {
            SirType::Bits(w) => {
                // BUG FIX #60: Use uint for all bit types ≤32 bits in struct definitions
                // to ensure 4-byte alignment and avoid struct padding mismatches between
                // Metal shader and CPU runtime
                let (base_type, array_size) = if force_4byte_align && *w <= 32 {
                    ("uint".to_string(), None)
                } else {
                    self.get_metal_type_for_wide_bits(*w)
                };
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
