use crate::sir::*;
use std::fmt::Write;

pub fn generate_metal_shader(sir_module: &SirModule) -> String {
    let mut shader = String::new();
    let mut generator = MetalShaderGenerator::new(&mut shader);

    generator.generate_header();
    generator.generate_struct_definitions(sir_module);
    generator.generate_combinational_kernels(sir_module);
    generator.generate_sequential_kernel(sir_module);

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
            self.write_indented(&format!(
                "{} {};\n",
                self.get_metal_type_name(input.width),
                input.name
            ));
        }
        self.indent -= 1;
        writeln!(self.output, "}};\n").unwrap();

        // Register buffer (only flip-flop state)
        writeln!(self.output, "// Register buffer (flip-flop outputs only)").unwrap();
        writeln!(self.output, "struct Registers {{").unwrap();
        self.indent += 1;
        for (i, (name, elem)) in sir.state_elements.iter().enumerate() {
            eprintln!("🔧 REGISTER[{}]: {}", i, name);
            self.write_indented(&format!(
                "{} {};\n",
                self.get_metal_type_name(elem.width),
                name
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
            if added_names.insert(output.name.clone()) {
                self.write_indented(&format!(
                    "{} {};\n",
                    self.get_metal_type_name(output.width),
                    output.name
                ));
            }
        }

        // All intermediate signals (avoiding duplicates)
        for signal in &sir.signals {
            if !signal.is_state && added_names.insert(signal.name.clone()) {
                self.write_indented(&format!(
                    "{} {};\n",
                    self.get_metal_type_name(signal.width),
                    signal.name
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

        for node_id in sorted_nodes {
            if let Some(node) = sir.combinational_nodes.iter().find(|n| n.id == node_id) {
                self.generate_node_computation_v2(sir, node);
            }
        }

        // Only assign outputs in the last cone to avoid duplication
        // TODO: This is a temporary fix - should be based on actual signal dependencies
        if index == total_cones - 1 {
            // Assign outputs based on their drivers
            for output in &sir.outputs {
                // Check if this output is a state element (sequential output)
                if sir.state_elements.contains_key(&output.name) {
                    // Output is driven by a register - read from register buffer
                    self.write_indented(&format!(
                        "signals->{} = registers->{};\n",
                        output.name, output.name
                    ));
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
                                        self.write_indented(&format!(
                                            "signals->{} = signals->{};\n",
                                            output.name, node_output.signal_id
                                        ));
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
        for state_name in sir.state_elements.keys() {
            self.write_indented(&format!(
                "uint old_{} = registers->{};\n",
                state_name, state_name
            ));
        }

        // Check clock edges and update registers
        for node in &sir.sequential_nodes {
            if let SirNodeKind::FlipFlop { clock_edge } = &node.kind {
                self.generate_flipflop_update_v2(sir, node, clock_edge);
            }
        }

        self.indent -= 1;
        writeln!(self.output, "}}\n").unwrap();
    }
    #[allow(dead_code)]
    #[allow(dead_code)]
    fn generate_node_computation(&mut self, node: &SirNode) {
        match &node.kind {
            SirNodeKind::BinaryOp(op) => {
                self.generate_binary_op(node, op);
            }
            SirNodeKind::UnaryOp(op) => {
                self.generate_unary_op(node, op);
            }
            SirNodeKind::Constant { value, width } => {
                self.generate_constant(node, *value, *width);
            }
            SirNodeKind::Mux => {
                self.generate_mux(node);
            }
            SirNodeKind::Slice { start, end } => {
                // Note: This function needs SIR access for proper slice generation
                // For now, generate a placeholder that reads from signals
                let input = &node.inputs[0].signal_id;
                let output = &node.outputs[0].signal_id;
                let (high, low) = if *start >= *end {
                    (*start, *end)
                } else {
                    (*end, *start)
                };
                let shift = low;
                let mask = (1u64 << (high - low + 1)) - 1;
                self.write_indented(&format!(
                    "signals->{} = (signals->{} >> {}) & 0x{:X};\n",
                    output, input, shift, mask
                ));
            }
            SirNodeKind::SignalRef { signal } => {
                self.generate_signal_ref(node, signal);
            }
            _ => {}
        }
    }

    fn generate_binary_op(&mut self, node: &SirNode, op: &BinaryOperation) {
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
            };

            self.write_indented(&format!(
                "signals->{} = signals->{} {} signals->{};\n",
                output, left, op_str, right
            ));
        }
    }

    fn generate_unary_op(&mut self, node: &SirNode, op: &UnaryOperation) {
        if !node.inputs.is_empty() && !node.outputs.is_empty() {
            let input = &node.inputs[0].signal_id;
            let output = &node.outputs[0].signal_id;

            let op_str = match op {
                UnaryOperation::Not => "~",
                UnaryOperation::Neg => "-",
                UnaryOperation::RedAnd => "&",
                UnaryOperation::RedOr => "|",
                UnaryOperation::RedXor => "^",
            };

            self.write_indented(&format!(
                "signals->{} = {}signals->{};\n",
                output, op_str, input
            ));
        }
    }

    fn generate_constant(&mut self, node: &SirNode, value: u64, width: usize) {
        if !node.outputs.is_empty() {
            let output = &node.outputs[0].signal_id;
            let metal_type = self.get_metal_type_name(width);
            self.write_indented(&format!(
                "signals->{} = {}({});\n",
                output, metal_type, value
            ));
        }
    }

    fn generate_mux(&mut self, node: &SirNode) {
        if node.inputs.len() >= 3 && !node.outputs.is_empty() {
            let sel = &node.inputs[0].signal_id;
            let true_val = &node.inputs[1].signal_id;
            let false_val = &node.inputs[2].signal_id;
            let output = &node.outputs[0].signal_id;

            self.write_indented(&format!(
                "signals->{} = signals->{} ? signals->{} : signals->{};\n",
                output, sel, true_val, false_val
            ));
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
            let mask = (1u64 << width) - 1;

            // Map signal names to register names for flip-flop outputs
            let input_ref = if sir.state_elements.contains_key(input) {
                // Direct register reference
                format!("registers->{}", input)
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
                    format!("registers->{}", reg_name)
                } else {
                    format!("signals->{}", input)
                }
            } else {
                format!("signals->{}", input)
            };

            self.write_indented(&format!(
                "signals->{} = ({} >> {}) & 0x{:X};\n",
                output, input_ref, shift, mask
            ));
        }
    }

    fn generate_signal_ref(&mut self, node: &SirNode, signal: &str) {
        if !node.outputs.is_empty() && !node.inputs.is_empty() {
            let output = &node.outputs[0].signal_id;

            // Copy from state to signals
            self.write_indented(&format!("signals->{} = state->{};\n", output, signal));
        }
    }

    fn generate_node_computation_v2(&mut self, sir: &SirModule, node: &SirNode) {
        match &node.kind {
            SirNodeKind::BinaryOp(op) => self.generate_binary_op(node, op),
            SirNodeKind::UnaryOp(op) => self.generate_unary_op(node, op),
            SirNodeKind::Constant { value, width } => self.generate_constant(node, *value, *width),
            SirNodeKind::Mux => self.generate_mux(node),
            SirNodeKind::Slice { start, end } => self.generate_slice(sir, node, *start, *end),
            SirNodeKind::SignalRef { signal } => {
                // Check if it's reading from inputs or registers
                if !node.outputs.is_empty() {
                    let output = &node.outputs[0].signal_id;
                    if sir.inputs.iter().any(|i| i.name == *signal) {
                        self.write_indented(&format!(
                            "signals->{} = inputs->{};\n",
                            output, signal
                        ));
                    } else if sir.state_elements.contains_key(signal) {
                        self.write_indented(&format!(
                            "signals->{} = registers->{};\n",
                            output, signal
                        ));
                    }
                }
            }
            _ => {}
        }
    }

    fn generate_flipflop_update_v2(&mut self, sir: &SirModule, node: &SirNode, edge: &ClockEdge) {
        if node.inputs.len() >= 2 && !node.outputs.is_empty() {
            let clock_signal = &node.inputs[0].signal_id;
            let data_signal = &node.inputs[1].signal_id;

            // Find which clock input this is
            if let Some(clock_input) = sir.inputs.iter().find(|i| i.name == *clock_signal) {
                let _edge_condition = match edge {
                    ClockEdge::Rising => {
                        // Check for rising edge: was 0, now 1
                        format!("inputs->{} == 1", clock_input.name)
                    }
                    ClockEdge::Falling => {
                        format!("inputs->{} == 0", clock_input.name)
                    }
                    ClockEdge::Both => "true".to_string(),
                };

                // Note: Don't check clock value here since GPU runtime already ensures this kernel
                // only executes on the correct clock edge
                // self.write_indented(&format!("if ({}) {{\n", edge_condition));
                // self.indent += 1;

                // Update all register outputs with the data input value from signals
                for output in &node.outputs {
                    if sir.state_elements.contains_key(&output.signal_id) {
                        // General case: read the computed value from signals
                        self.write_indented(&format!(
                            "registers->{} = signals->{};\n",
                            output.signal_id, data_signal
                        ));
                    }
                }

                // self.indent -= 1;
                // self.write_indented("}\n");
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

    fn get_metal_type_name(&self, width: usize) -> &str {
        match width {
            1..=32 => "uint",
            33..=64 => "uint2",
            _ => "uint4",
        }
    }

    fn write_indented(&mut self, text: &str) {
        let indent_str = "    ".repeat(self.indent);
        write!(self.output, "{}{}", indent_str, text).unwrap();
    }
}
