use crate::sir::*;
use skalp_mir::{Module, Statement, Expression, ProcessKind, EdgeType, DataType,
                SensitivityList, Value, LValue, IfStatement, Block};
use std::collections::HashMap;

pub fn convert_mir_to_sir(mir_module: &Module) -> SirModule {
    let mut sir = SirModule::new(mir_module.name.clone());
    let mut converter = MirToSirConverter::new(&mut sir, mir_module);

    converter.convert_ports();
    converter.convert_signals();
    converter.convert_logic();
    converter.extract_clock_domains();

    sir
}

struct MirToSirConverter<'a> {
    sir: &'a mut SirModule,
    mir: &'a Module,
    node_counter: usize,
    signal_map: HashMap<String, String>,
}

impl<'a> MirToSirConverter<'a> {
    fn new(sir: &'a mut SirModule, mir: &'a Module) -> Self {
        MirToSirConverter {
            sir,
            mir,
            node_counter: 0,
            signal_map: HashMap::new(),
        }
    }

    fn convert_ports(&mut self) {
        for port in &self.mir.ports {
            let width = self.get_width(&port.port_type);
            let direction = match port.direction {
                skalp_mir::PortDirection::Input => PortDirection::Input,
                skalp_mir::PortDirection::Output => PortDirection::Output,
                skalp_mir::PortDirection::InOut => PortDirection::Output,
            };

            let sir_port = SirPort {
                name: port.name.clone(),
                width,
                direction: direction.clone(),
                clock_domain: None,
            };

            if matches!(direction, PortDirection::Input) {
                self.sir.inputs.push(sir_port);
            } else {
                self.sir.outputs.push(sir_port);
            }

            self.sir.signals.push(SirSignal {
                name: port.name.clone(),
                width,
                driver_node: None,
                fanout_nodes: Vec::new(),
                is_state: false,
            });
        }
    }

    fn convert_signals(&mut self) {
        for signal in &self.mir.signals {
            let width = self.get_width(&signal.signal_type);

            // Determine if this is a register by checking if it's assigned in sequential blocks
            let is_register = self.is_signal_sequential(signal.id);

            self.sir.signals.push(SirSignal {
                name: signal.name.clone(),
                width,
                driver_node: None,
                fanout_nodes: Vec::new(),
                is_state: is_register,
            });

            if is_register {
                self.sir.state_elements.insert(
                    signal.name.clone(),
                    StateElement {
                        name: signal.name.clone(),
                        width,
                        reset_value: None,
                        clock: String::new(),
                        reset: None,
                    },
                );
            }
        }
    }

    fn is_signal_sequential(&self, signal_id: skalp_mir::SignalId) -> bool {
        // Check if this signal is assigned in any sequential process
        for process in &self.mir.processes {
            if process.kind == ProcessKind::Sequential {
                if self.is_signal_assigned_in_block(&process.body, signal_id) {
                    return true;
                }
            }
        }
        false
    }

    fn is_signal_assigned_in_block(&self, block: &skalp_mir::Block, signal_id: skalp_mir::SignalId) -> bool {
        for stmt in &block.statements {
            match stmt {
                Statement::Assignment(assign) => {
                    if self.lvalue_contains_signal(&assign.lhs, signal_id) {
                        return true;
                    }
                }
                Statement::Block(inner_block) => {
                    if self.is_signal_assigned_in_block(inner_block, signal_id) {
                        return true;
                    }
                }
                Statement::If(if_stmt) => {
                    if self.is_signal_assigned_in_block(&if_stmt.then_block, signal_id) {
                        return true;
                    }
                    if let Some(else_block) = &if_stmt.else_block {
                        if self.is_signal_assigned_in_block(else_block, signal_id) {
                            return true;
                        }
                    }
                }
                _ => {}
            }
        }
        false
    }

    fn lvalue_contains_signal(&self, lvalue: &LValue, signal_id: skalp_mir::SignalId) -> bool {
        match lvalue {
            LValue::Signal(id) => *id == signal_id,
            LValue::BitSelect { base, .. } | LValue::RangeSelect { base, .. } => {
                self.lvalue_contains_signal(base, signal_id)
            }
            LValue::Concat(parts) => {
                parts.iter().any(|part| self.lvalue_contains_signal(part, signal_id))
            }
            _ => false,
        }
    }

    fn convert_logic(&mut self) {
        // Convert processes
        for process in &self.mir.processes {
            match &process.kind {
                ProcessKind::Combinational => {
                    self.convert_combinational_block(&process.body.statements);
                }
                ProcessKind::Sequential => {
                    if let SensitivityList::Edge(edges) = &process.sensitivity {
                        if let Some(edge_sens) = edges.first() {
                            let edge = match edge_sens.edge {
                                EdgeType::Rising => ClockEdge::Rising,
                                EdgeType::Falling => ClockEdge::Falling,
                                EdgeType::Both => ClockEdge::Both,
                                _ => ClockEdge::Rising,
                            };
                            let signal_name = self.lvalue_to_string(&edge_sens.signal);
                            self.convert_sequential_block(&process.body.statements, &signal_name, edge);
                        }
                    }
                }
                _ => {}
            }
        }

        // Convert continuous assignments
        for assign in &self.mir.assignments {
            let target = self.lvalue_to_string(&assign.lhs);
            self.convert_continuous_assign(&target, &assign.rhs);
        }
    }

    fn convert_combinational_block(&mut self, statements: &[Statement]) {
        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let node_id = self.create_expression_node(&assign.rhs);
                    let target = self.lvalue_to_string(&assign.lhs);
                    self.connect_node_to_signal(node_id, &target);
                }
                Statement::Block(block) => {
                    self.convert_combinational_block(&block.statements);
                }
                Statement::If(if_stmt) => {
                    // Convert if statement to mux
                    self.convert_if_to_mux(if_stmt);
                }
                Statement::Case(case_stmt) => {
                    // Convert case statement to mux tree
                    self.convert_case_to_mux_tree(case_stmt);
                }
                _ => {}
            }
        }
    }

    fn convert_sequential_block(&mut self, statements: &[Statement], clock: &str, edge: ClockEdge) {
        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let input_node = self.create_expression_node(&assign.rhs);
                    let ff_node = self.create_flipflop_with_input(input_node, clock, edge.clone());
                    let target = self.lvalue_to_string(&assign.lhs);
                    self.connect_node_to_signal(ff_node, &target);
                }
                Statement::Block(block) => {
                    self.convert_sequential_block(&block.statements, clock, edge.clone());
                }
                Statement::If(if_stmt) => {
                    // Convert if statement to a mux in sequential context
                    // The condition determines which value gets clocked into the register
                    self.convert_if_in_sequential(if_stmt, clock, edge.clone());
                }
                _ => {}
            }
        }
    }

    fn convert_if_in_sequential(&mut self, if_stmt: &IfStatement, clock: &str, edge: ClockEdge) {
        // Create condition node
        let cond_node = self.create_expression_node(&if_stmt.condition);

        // Process then branch
        for stmt in &if_stmt.then_block.statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let target = self.lvalue_to_string(&assign.lhs);
                    let then_value = self.create_expression_node(&assign.rhs);

                    // Process else branch if it exists
                    let final_value = if let Some(else_block) = &if_stmt.else_block {
                        // Find assignment to same target in else block
                        let else_value = self.find_else_value(else_block, &target, then_value);

                        // Create mux: condition ? then_value : else_value
                        self.create_mux_node(cond_node, then_value, else_value)
                    } else {
                        then_value
                    };

                    // Create flip-flop with the muxed value
                    let ff_node = self.create_flipflop_with_input(final_value, clock, edge.clone());
                    self.connect_node_to_signal(ff_node, &target);
                }
                Statement::If(nested_if) => {
                    self.convert_if_in_sequential(nested_if, clock, edge.clone());
                }
                _ => {}
            }
        }
    }

    fn find_else_value(&mut self, else_block: &Block, target: &str, default: usize) -> usize {
        for stmt in &else_block.statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let assign_target = self.lvalue_to_string(&assign.lhs);
                    if assign_target == target {
                        return self.create_expression_node(&assign.rhs);
                    }
                }
                _ => {}
            }
        }
        default
    }

    fn convert_continuous_assign(&mut self, target: &str, value: &Expression) {
        let node_id = self.create_expression_node(value);
        self.connect_node_to_signal(node_id, target);
    }

    fn lvalue_to_string(&self, lvalue: &LValue) -> String {
        match lvalue {
            LValue::Port(port_id) => {
                self.mir.ports.iter()
                    .find(|p| p.id == *port_id)
                    .map(|p| p.name.clone())
                    .unwrap_or_else(|| format!("port_{}", port_id.0))
            }
            LValue::Signal(sig_id) => {
                self.mir.signals.iter()
                    .find(|s| s.id == *sig_id)
                    .map(|s| s.name.clone())
                    .unwrap_or_else(|| format!("signal_{}", sig_id.0))
            }
            LValue::Variable(var_id) => {
                self.mir.variables.iter()
                    .find(|v| v.id == *var_id)
                    .map(|v| v.name.clone())
                    .unwrap_or_else(|| format!("var_{}", var_id.0))
            }
            LValue::BitSelect { base, .. } => {
                // For bit select, use the base signal name
                self.lvalue_to_string(base)
            }
            LValue::RangeSelect { base, .. } => {
                // For range select, use the base signal name
                self.lvalue_to_string(base)
            }
            LValue::Concat(parts) => {
                // For concat, create a synthetic name
                if let Some(first) = parts.first() {
                    format!("concat_{}", self.lvalue_to_string(first))
                } else {
                    "concat".to_string()
                }
            }
        }
    }

    fn create_expression_node(&mut self, expr: &Expression) -> usize {
        match expr {
            Expression::Literal(value) => {
                self.create_literal_node(value)
            }
            Expression::Ref(lvalue) => {
                self.create_lvalue_ref_node(lvalue)
            }
            Expression::Binary { op, left, right } => {
                let left_node = self.create_expression_node(left);
                let right_node = self.create_expression_node(right);
                self.create_binary_op_node(op, left_node, right_node)
            }
            Expression::Unary { op, operand } => {
                let operand_node = self.create_expression_node(operand);
                self.create_unary_op_node(op, operand_node)
            }
            Expression::Conditional { cond, then_expr, else_expr } => {
                let cond_node = self.create_expression_node(cond);
                let then_node = self.create_expression_node(then_expr);
                let else_node = self.create_expression_node(else_expr);
                self.create_mux_node(cond_node, then_node, else_node)
            }
            Expression::Concat(parts) => {
                let part_nodes: Vec<usize> = parts.iter()
                    .map(|p| self.create_expression_node(p))
                    .collect();
                self.create_concat_node(part_nodes)
            }
            _ => {
                // For unsupported expressions, create a zero constant
                self.create_constant_node(0, 1)
            }
        }
    }

    fn create_literal_node(&mut self, value: &Value) -> usize {
        let (val, width) = match value {
            Value::Integer(i) => (*i as u64, 32),
            Value::BitVector { width, value } => (*value, *width),
            _ => (0, 1),
        };
        self.create_constant_node(val, width)
    }

    fn create_constant_node(&mut self, value: u64, width: usize) -> usize {
        let node_id = self.next_node_id();

        // Create output signal for this node
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };
        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::Constant { value, width },
            inputs: vec![],
            outputs: vec![output_signal],
            clock_domain: None,
        };

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn create_lvalue_ref_node(&mut self, lvalue: &LValue) -> usize {
        match lvalue {
            LValue::Signal(sig_id) => {
                let signal_name = self.mir.signals.iter()
                    .find(|s| s.id == *sig_id)
                    .map(|s| s.name.clone())
                    .unwrap_or_else(|| format!("signal_{}", sig_id.0));
                self.get_or_create_signal_driver(&signal_name)
            }
            LValue::Port(port_id) => {
                let port_name = self.mir.ports.iter()
                    .find(|p| p.id == *port_id)
                    .map(|p| p.name.clone())
                    .unwrap_or_else(|| format!("port_{}", port_id.0));
                self.get_or_create_signal_driver(&port_name)
            }
            LValue::Variable(var_id) => {
                let var_name = self.mir.variables.iter()
                    .find(|v| v.id == *var_id)
                    .map(|v| v.name.clone())
                    .unwrap_or_else(|| format!("var_{}", var_id.0));
                self.get_or_create_signal_driver(&var_name)
            }
            LValue::BitSelect { base, index } => {
                let base_node = self.create_lvalue_ref_node(base);
                let _index_node = self.create_expression_node(index);
                // Create a bit select node (simplified as slice with width 1)
                self.create_slice_node(base_node, 0, 0) // TODO: Use actual index
            }
            LValue::RangeSelect { base, high: _, low: _ } => {
                let base_node = self.create_lvalue_ref_node(base);
                // TODO: Evaluate high and low expressions to get constant values
                self.create_slice_node(base_node, 0, 7) // Placeholder
            }
            LValue::Concat(parts) => {
                let part_nodes: Vec<usize> = parts.iter()
                    .map(|p| self.create_lvalue_ref_node(p))
                    .collect();
                self.create_concat_node(part_nodes)
            }
        }
    }

    fn create_binary_op_node(&mut self, op: &skalp_mir::BinaryOp, left: usize, right: usize) -> usize {
        let node_id = self.next_node_id();
        let bin_op = self.convert_binary_op(op);

        let left_signal = self.node_to_signal_ref(left);
        let right_signal = self.node_to_signal_ref(right);

        // Determine width from input signals
        let width = self.get_signal_width(&left_signal.signal_id).max(
            self.get_signal_width(&right_signal.signal_id)
        );

        // Create output signal for this node
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };

        // Add the signal to the SIR module
        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::BinaryOp(bin_op),
            inputs: vec![left_signal, right_signal],
            outputs: vec![output_signal],
            clock_domain: None,
        };

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn create_unary_op_node(&mut self, op: &skalp_mir::UnaryOp, operand: usize) -> usize {
        let node_id = self.next_node_id();
        let unary_op = self.convert_unary_op(op);

        let operand_signal = self.node_to_signal_ref(operand);

        // Get width from operand
        let width = self.get_signal_width(&operand_signal.signal_id);

        // Create output signal for this node
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };

        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::UnaryOp(unary_op),
            inputs: vec![operand_signal],
            outputs: vec![output_signal],
            clock_domain: None,
        };

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn create_mux_node(&mut self, sel: usize, true_val: usize, false_val: usize) -> usize {
        let node_id = self.next_node_id();

        let sel_signal = self.node_to_signal_ref(sel);
        let true_signal = self.node_to_signal_ref(true_val);
        let false_signal = self.node_to_signal_ref(false_val);

        // Get width from true/false branches
        let width = self.get_signal_width(&true_signal.signal_id).max(
            self.get_signal_width(&false_signal.signal_id)
        );

        // Create output signal for this node
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };

        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::Mux,
            inputs: vec![sel_signal, true_signal, false_signal],
            outputs: vec![output_signal],
            clock_domain: None,
        };

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn create_concat_node(&mut self, parts: Vec<usize>) -> usize {
        let node_id = self.next_node_id();

        let part_signals: Vec<SignalRef> = parts.iter()
            .map(|&p| self.node_to_signal_ref(p))
            .collect();

        // Calculate total width as sum of input widths
        let width = part_signals.iter().map(|s| self.get_signal_width(&s.signal_id)).sum();

        // Create output signal for this node
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };
        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::Concat,
            inputs: part_signals,
            outputs: vec![output_signal],
            clock_domain: None,
        };

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn create_slice_node(&mut self, base: usize, start: usize, end: usize) -> usize {
        let node_id = self.next_node_id();

        let base_signal = self.node_to_signal_ref(base);

        // Create output signal for this node
        let output_width = end - start;
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };
        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width: output_width,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::Slice { start, end },
            inputs: vec![base_signal],
            outputs: vec![output_signal],
            clock_domain: None,
        };

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn create_flipflop_with_input(&mut self, input: usize, clock: &str, edge: ClockEdge) -> usize {
        let node_id = self.next_node_id();

        let input_signal = self.node_to_signal_ref(input);
        let clock_signal = SignalRef {
            signal_id: clock.to_string(),
            bit_range: None,
        };

        // Get width from input signal
        let width = self.get_signal_width(&input_signal.signal_id);

        // Create output signal for this flip-flop
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };
        self.sir.signals.push(SirSignal {
            name: output_signal_name,
            width,
            is_state: false, // This is just a temporary signal, not a state element
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
        });

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::FlipFlop { clock_edge: edge },
            inputs: vec![clock_signal, input_signal],
            outputs: vec![output_signal],
            clock_domain: Some(clock.to_string()),
        };

        self.sir.sequential_nodes.push(node);
        node_id
    }

    fn node_to_signal_ref(&mut self, node_id: usize) -> SignalRef {
        // Create a temporary signal for this node's output
        let signal_name = format!("node_{}_out", node_id);

        // Add signal if it doesn't exist
        if !self.sir.signals.iter().any(|s| s.name == signal_name) {
            self.sir.signals.push(SirSignal {
                name: signal_name.clone(),
                width: 8, // Default to 8 bits for counter example
                driver_node: Some(node_id),
                fanout_nodes: Vec::new(),
                is_state: false,
            });
        }

        SignalRef {
            signal_id: signal_name,
            bit_range: None,
        }
    }

    fn convert_if_to_mux(&mut self, if_stmt: &skalp_mir::IfStatement) {
        // Convert condition
        let _cond_node = self.create_expression_node(&if_stmt.condition);

        // Convert then block
        self.convert_combinational_block(&if_stmt.then_block.statements);

        // Convert else block if present
        if let Some(else_block) = &if_stmt.else_block {
            self.convert_combinational_block(&else_block.statements);
        }

        // TODO: Create actual mux nodes for assignments in branches
    }

    fn convert_case_to_mux_tree(&mut self, _case_stmt: &skalp_mir::CaseStatement) {
        // TODO: Implement case to mux tree conversion
    }

    fn connect_node_to_signal(&mut self, node_id: usize, signal_name: &str) {
        // Update signal to have this node as driver
        if let Some(signal) = self.sir.signals.iter_mut().find(|s| s.name == signal_name) {
            signal.driver_node = Some(node_id);
        }

        // Update node to output to this signal
        if let Some(node) = self.sir.combinational_nodes.iter_mut()
            .chain(self.sir.sequential_nodes.iter_mut())
            .find(|n| n.id == node_id) {
            node.outputs.push(SignalRef {
                signal_id: signal_name.to_string(),
                bit_range: None,
            });
        }
    }

    fn get_or_create_signal_driver(&mut self, name: &str) -> usize {
        if let Some(signal) = self.sir.signals.iter().find(|s| s.name == name) {
            if let Some(driver) = signal.driver_node {
                return driver;
            }
        }

        // Create a signal reader node that reads from state
        let node_id = self.next_node_id();

        // Create output signal for this node
        let output_signal_name = format!("node_{}_out", node_id);
        let output_signal = SignalRef {
            signal_id: output_signal_name.clone(),
            bit_range: None,
        };

        // Check if this is a state element or signal to get width
        let width = if let Some(state) = self.sir.state_elements.get(name) {
            state.width
        } else if let Some(signal) = self.sir.signals.iter().find(|s| s.name == name) {
            signal.width
        } else {
            8 // Default to 8 bits
        };

        self.sir.signals.push(SirSignal {
            name: output_signal_name.clone(),
            width,
            is_state: false,
            driver_node: Some(node_id),
            fanout_nodes: Vec::new(),
        });

        // Create a signal reader node
        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::SignalRef { signal: name.to_string() },
            inputs: vec![SignalRef { signal_id: name.to_string(), bit_range: None }],
            outputs: vec![output_signal],
            clock_domain: None,
        };

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn convert_binary_op(&self, op: &skalp_mir::BinaryOp) -> BinaryOperation {
        use skalp_mir::BinaryOp::*;
        match op {
            Add => BinaryOperation::Add,
            Sub => BinaryOperation::Sub,
            Mul => BinaryOperation::Mul,
            Div => BinaryOperation::Div,
            Mod => BinaryOperation::Mod,
            BitwiseAnd => BinaryOperation::And,
            BitwiseOr => BinaryOperation::Or,
            BitwiseXor => BinaryOperation::Xor,
            And => BinaryOperation::And, // Logical AND mapped to bitwise
            Or => BinaryOperation::Or,   // Logical OR mapped to bitwise
            Xor => BinaryOperation::Xor, // Logical XOR mapped to bitwise
            Equal => BinaryOperation::Eq,
            NotEqual => BinaryOperation::Neq,
            Less => BinaryOperation::Lt,
            LessEqual => BinaryOperation::Lte,
            Greater => BinaryOperation::Gt,
            GreaterEqual => BinaryOperation::Gte,
            LeftShift => BinaryOperation::Shl,
            RightShift => BinaryOperation::Shr,
            LogicalAnd => BinaryOperation::And, // Boolean AND
            LogicalOr => BinaryOperation::Or,   // Boolean OR
        }
    }

    fn convert_unary_op(&self, op: &skalp_mir::UnaryOp) -> UnaryOperation {
        use skalp_mir::UnaryOp::*;
        match op {
            Not => UnaryOperation::Not,
            BitwiseNot => UnaryOperation::Not,
            Negate => UnaryOperation::Neg,
            Reduce(_) => UnaryOperation::Not, // Map reduction to NOT for now
        }
    }

    fn get_width(&self, data_type: &DataType) -> usize {
        match data_type {
            DataType::Bit(w) | DataType::Logic(w) => *w,
            DataType::Int(w) | DataType::Nat(w) => *w,
            DataType::Clock { .. } | DataType::Reset { .. } => 1,
            DataType::Array(dt, size) => self.get_width(dt) * size,
            DataType::BitParam { default, .. } |
            DataType::LogicParam { default, .. } |
            DataType::IntParam { default, .. } |
            DataType::NatParam { default, .. } => *default,
            _ => 1,
        }
    }

    fn get_signal_width(&self, signal_name: &str) -> usize {
        // Check if it's a signal
        if let Some(signal) = self.sir.signals.iter().find(|s| s.name == signal_name) {
            return signal.width;
        }

        // Check if it's a state element
        if let Some(state) = self.sir.state_elements.get(signal_name) {
            return state.width;
        }

        // Default to 8 bits for the counter example
        8
    }

    fn extract_clock_domains(&mut self) {
        let mut domains: HashMap<String, ClockDomain> = HashMap::new();

        for process in &self.mir.processes {
            if process.kind == ProcessKind::Sequential {
                if let SensitivityList::Edge(edges) = &process.sensitivity {
                    if let Some(edge_sens) = edges.first() {
                        let clock_name = self.lvalue_to_string(&edge_sens.signal);
                        domains.entry(clock_name.clone()).or_insert_with(|| {
                            ClockDomain {
                                name: clock_name.clone(),
                                frequency_hz: None,
                                phase_offset: 0.0,
                                state_elements: Vec::new(),
                            }
                        });
                    }
                }
            }
        }

        // Associate state elements with clock domains
        for (name, state) in &self.sir.state_elements {
            if let Some(domain) = domains.values_mut().find(|d| d.name == state.clock) {
                domain.state_elements.push(name.clone());
            }
        }

        self.sir.clock_domains = domains;
    }

    fn next_node_id(&mut self) -> usize {
        let id = self.node_counter;
        self.node_counter += 1;
        id
    }
}