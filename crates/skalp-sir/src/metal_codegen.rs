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
        writeln!(self.output, "// State structure for module: {}", sir.name).unwrap();
        writeln!(self.output, "struct ModuleState {{").unwrap();
        self.indent += 1;

        for input in &sir.inputs {
            self.write_indented(&format!("uint{} {};\n",
                self.get_metal_type_width(input.width), input.name));
        }

        for output in &sir.outputs {
            self.write_indented(&format!("uint{} {};\n",
                self.get_metal_type_width(output.width), output.name));
        }

        for signal in &sir.signals {
            if signal.is_state {
                self.write_indented(&format!("uint{} {};\n",
                    self.get_metal_type_width(signal.width), signal.name));
            }
        }

        self.indent -= 1;
        writeln!(self.output, "}};\n").unwrap();

        writeln!(self.output, "// Intermediate signals structure").unwrap();
        writeln!(self.output, "struct Signals {{").unwrap();
        self.indent += 1;

        for signal in &sir.signals {
            if !signal.is_state {
                self.write_indented(&format!("uint{} {};\n",
                    self.get_metal_type_width(signal.width), signal.name));
            }
        }

        self.indent -= 1;
        writeln!(self.output, "}};\n").unwrap();
    }

    fn generate_combinational_kernels(&mut self, sir: &SirModule) {
        let cones = sir.extract_combinational_cones();

        for (i, cone) in cones.iter().enumerate() {
            self.generate_combinational_cone_kernel(sir, cone, i);
        }
    }

    fn generate_combinational_cone_kernel(&mut self, sir: &SirModule, cone: &CombinationalCone, index: usize) {
        writeln!(self.output, "kernel void combinational_cone_{}(", index).unwrap();
        self.indent += 1;

        self.write_indented("device ModuleState* state [[buffer(0)]],\n");
        self.write_indented("device Signals* signals [[buffer(1)]],\n");
        self.write_indented("uint tid [[thread_position_in_grid]]\n");

        self.indent -= 1;
        writeln!(self.output, ") {{").unwrap();
        self.indent += 1;

        self.write_indented("// Combinational logic evaluation\n");

        for &node_id in &cone.nodes {
            if let Some(node) = sir.combinational_nodes.iter().find(|n| n.id == node_id) {
                self.generate_node_computation(node);
            }
        }

        self.indent -= 1;
        writeln!(self.output, "}}\n").unwrap();
    }

    fn generate_sequential_kernel(&mut self, sir: &SirModule) {
        writeln!(self.output, "kernel void sequential_update(").unwrap();
        self.indent += 1;

        self.write_indented("device ModuleState* state [[buffer(0)]],\n");
        self.write_indented("device ModuleState* next_state [[buffer(1)]],\n");
        self.write_indented("device const uint* clock_edges [[buffer(2)]],\n");
        self.write_indented("uint tid [[thread_position_in_grid]]\n");

        self.indent -= 1;
        writeln!(self.output, ") {{").unwrap();
        self.indent += 1;

        for node in &sir.sequential_nodes {
            match &node.kind {
                SirNodeKind::FlipFlop { clock_edge } => {
                    self.generate_flipflop_update(node, clock_edge);
                }
                _ => {}
            }
        }

        self.indent -= 1;
        writeln!(self.output, "}}\n").unwrap();
    }

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
                self.generate_slice(node, *start, *end);
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

            self.write_indented(&format!("signals->{} = signals->{} {} signals->{};\n",
                output, left, op_str, right));
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

            self.write_indented(&format!("signals->{} = {}signals->{};\n",
                output, op_str, input));
        }
    }

    fn generate_constant(&mut self, node: &SirNode, value: u64, width: usize) {
        if !node.outputs.is_empty() {
            let output = &node.outputs[0].signal_id;
            let metal_type = self.get_metal_type_name(width);
            self.write_indented(&format!("signals->{} = {}({});\n",
                output, metal_type, value));
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

    fn generate_slice(&mut self, node: &SirNode, start: usize, end: usize) {
        if !node.inputs.is_empty() && !node.outputs.is_empty() {
            let input = &node.inputs[0].signal_id;
            let output = &node.outputs[0].signal_id;
            let width = end - start + 1;
            let mask = (1u64 << width) - 1;

            self.write_indented(&format!(
                "signals->{} = (signals->{} >> {}) & 0x{:X};\n",
                output, input, start, mask
            ));
        }
    }

    fn generate_flipflop_update(&mut self, node: &SirNode, edge: &ClockEdge) {
        if !node.outputs.is_empty() {
            let output = &node.outputs[0].signal_id;

            let edge_check = match edge {
                ClockEdge::Rising => "clock_edges[tid] == 1",
                ClockEdge::Falling => "clock_edges[tid] == 2",
                ClockEdge::Both => "clock_edges[tid] != 0",
            };

            self.write_indented(&format!("if ({}) {{\n", edge_check));
            self.indent += 1;
            self.write_indented(&format!("next_state->{} = state->{};\n", output, output));
            self.indent -= 1;
            self.write_indented("}\n");
        }
    }

    fn get_metal_type_width(&self, width: usize) -> &str {
        match width {
            1..=8 => "",
            9..=16 => "2",
            17..=32 => "4",
            _ => "4", // Default to 32-bit for larger widths
        }
    }

    fn get_metal_type_name(&self, width: usize) -> &str {
        match width {
            1..=8 => "uint",
            9..=16 => "uint2",
            17..=32 => "uint4",
            _ => "uint4",
        }
    }

    fn write_indented(&mut self, text: &str) {
        let indent_str = "    ".repeat(self.indent);
        write!(self.output, "{}{}", indent_str, text).unwrap();
    }
}