use crate::sir::*;
use skalp_mir::{Module, Statement, Expression, Process, ProcessKind, EdgeType, DataType,
                SensitivityList, Value, LValue, SignalId, VariableId, PortId};
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
            let is_register = false; // TODO: Determine from usage in sequential blocks

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

    fn convert_logic(&mut self) {
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

        for assign in &self.mir.assignments {
            let target = self.lvalue_to_string(&assign.lhs);
            self.convert_continuous_assign(&target, &assign.rhs);
        }
    }

    fn convert_combinational_block(&mut self, statements: &[Statement]) {
        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let node_id = self.create_combinational_node(&assign.rhs);
                    let target = self.lvalue_to_string(&assign.lhs);
                    self.connect_node_to_signal(node_id, &target);
                }
                Statement::Case(_case_stmt) => {
                    // TODO: Convert case statement
                }
                _ => {}
            }
        }
    }

    fn convert_sequential_block(&mut self, statements: &[Statement], clock: &str, edge: ClockEdge) {
        for stmt in statements {
            match stmt {
                Statement::Assignment(assign) => {
                    let ff_node = self.create_flipflop(&assign.rhs, clock, edge.clone());
                    let target = self.lvalue_to_string(&assign.lhs);
                    self.connect_node_to_signal(ff_node, &target);
                }
                _ => {}
            }
        }
    }

    fn convert_continuous_assign(&mut self, target: &str, value: &Expression) {
        let node_id = self.create_combinational_node(value);
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
            LValue::BitSlice { base, .. } => self.lvalue_to_string(base),
            LValue::Index { base, .. } => self.lvalue_to_string(base),
            LValue::Member { base, member } => {
                format!("{}_{}", self.lvalue_to_string(base), member)
            }
        }
    }

    fn create_combinational_node(&mut self, expr: &Expression) -> usize {
        let node_id = self.next_node_id();

        let (kind, inputs) = match expr {
            Expression::Binary { left, op, right } => {
                let left_ref = self.expr_to_signal_ref(left);
                let right_ref = self.expr_to_signal_ref(right);
                let bin_op = self.convert_binary_op(op);
                (
                    SirNodeKind::BinaryOp(bin_op),
                    vec![left_ref, right_ref]
                )
            }
            Expression::Unary { op, operand } => {
                let operand_ref = self.expr_to_signal_ref(operand);
                let unary_op = self.convert_unary_op(op);
                (
                    SirNodeKind::UnaryOp(unary_op),
                    vec![operand_ref]
                )
            }
            Expression::Value(value) => {
                match value {
                    Value::Port(port_id) => {
                        let name = self.mir.ports.iter()
                            .find(|p| p.id == *port_id)
                            .map(|p| p.name.clone())
                            .unwrap_or_else(|| format!("port_{}", port_id.0));
                        return self.get_or_create_signal_driver(&name);
                    }
                    Value::Signal(sig_id) => {
                        let name = self.mir.signals.iter()
                            .find(|s| s.id == *sig_id)
                            .map(|s| s.name.clone())
                            .unwrap_or_else(|| format!("signal_{}", sig_id.0));
                        return self.get_or_create_signal_driver(&name);
                    }
                    Value::Variable(var_id) => {
                        let name = self.mir.variables.iter()
                            .find(|v| v.id == *var_id)
                            .map(|v| v.name.clone())
                            .unwrap_or_else(|| format!("var_{}", var_id.0));
                        return self.get_or_create_signal_driver(&name);
                    }
                    Value::Literal(lit) => {
                        let val = 0; // TODO: Parse literal properly
                        (
                            SirNodeKind::Constant {
                                value: val,
                                width: 32
                            },
                            vec![]
                        )
                    }
                    _ => (SirNodeKind::Constant { value: 0, width: 1 }, vec![])
                }
            }
            Expression::BitSlice { base, start, end } => {
                let base_ref = self.expr_to_signal_ref(base);
                (
                    SirNodeKind::Slice {
                        start: *start,
                        end: *end
                    },
                    vec![base_ref]
                )
            }
            _ => (SirNodeKind::Constant { value: 0, width: 1 }, vec![])
        };

        let node = SirNode {
            id: node_id,
            kind,
            inputs,
            outputs: vec![],
            clock_domain: None,
        };

        self.sir.combinational_nodes.push(node);
        node_id
    }

    fn create_flipflop(&mut self, _input: &Expression, clock: &str, edge: ClockEdge) -> usize {
        let node_id = self.next_node_id();

        let node = SirNode {
            id: node_id,
            kind: SirNodeKind::FlipFlop { clock_edge: edge },
            inputs: vec![SignalRef {
                signal_id: clock.to_string(),
                bit_range: None
            }],
            outputs: vec![],
            clock_domain: Some(clock.to_string()),
        };

        self.sir.sequential_nodes.push(node);
        node_id
    }

    fn expr_to_signal_ref(&mut self, expr: &Expression) -> SignalRef {
        match expr {
            Expression::Value(value) => {
                let signal_id = match value {
                    Value::Port(port_id) => {
                        self.mir.ports.iter()
                            .find(|p| p.id == *port_id)
                            .map(|p| p.name.clone())
                            .unwrap_or_else(|| format!("port_{}", port_id.0))
                    }
                    Value::Signal(sig_id) => {
                        self.mir.signals.iter()
                            .find(|s| s.id == *sig_id)
                            .map(|s| s.name.clone())
                            .unwrap_or_else(|| format!("signal_{}", sig_id.0))
                    }
                    Value::Variable(var_id) => {
                        self.mir.variables.iter()
                            .find(|v| v.id == *var_id)
                            .map(|v| v.name.clone())
                            .unwrap_or_else(|| format!("var_{}", var_id.0))
                    }
                    _ => format!("tmp_{}", self.node_counter)
                };
                SignalRef {
                    signal_id,
                    bit_range: None,
                }
            }
            Expression::BitSlice { base, start, end } => {
                let base_ref = self.expr_to_signal_ref(base);
                SignalRef {
                    signal_id: base_ref.signal_id,
                    bit_range: Some((*start, *end)),
                }
            }
            _ => SignalRef {
                signal_id: format!("tmp_{}", self.node_counter),
                bit_range: None,
            }
        }
    }

    fn connect_node_to_signal(&mut self, node_id: usize, signal_name: &str) {
        if let Some(signal) = self.sir.signals.iter_mut().find(|s| s.name == signal_name) {
            signal.driver_node = Some(node_id);
        }

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

        let node_id = self.next_node_id();
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
            Equal => BinaryOperation::Eq,
            NotEqual => BinaryOperation::Neq,
            Less => BinaryOperation::Lt,
            LessEqual => BinaryOperation::Lte,
            Greater => BinaryOperation::Gt,
            GreaterEqual => BinaryOperation::Gte,
            ShiftLeft => BinaryOperation::Shl,
            ShiftRight => BinaryOperation::Shr,
            _ => BinaryOperation::Add,
        }
    }

    fn convert_unary_op(&self, op: &skalp_mir::UnaryOp) -> UnaryOperation {
        use skalp_mir::UnaryOp::*;
        match op {
            BitwiseNot => UnaryOperation::Not,
            LogicalNot => UnaryOperation::Not,
            Negate => UnaryOperation::Neg,
            _ => UnaryOperation::Not,
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