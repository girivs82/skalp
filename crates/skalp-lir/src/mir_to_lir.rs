//! MIR to LIR transformation
//!
//! Converts Mid-level IR to Low-level IR (gate-level representation)

use crate::lir::{Gate, GateType, Lir, Net};
use skalp_mir::mir::Module;
use std::collections::HashMap;

/// MIR to LIR transformer
pub struct MirToLirTransform {
    /// Current LIR being built
    lir: Lir,
    /// Net name generator counter
    net_counter: usize,
    /// Gate name generator counter
    gate_counter: usize,
    /// Map from MIR signals to LIR nets
    signal_map: HashMap<String, String>,
}

impl MirToLirTransform {
    /// Create a new transformer
    pub fn new(module_name: String) -> Self {
        Self {
            lir: Lir::new(module_name),
            net_counter: 0,
            gate_counter: 0,
            signal_map: HashMap::new(),
        }
    }

    /// Transform a MIR module to LIR
    pub fn transform(&mut self, module: &Module) -> Lir {
        // Create nets for all ports and map them by port ID
        for (i, port) in module.ports.iter().enumerate() {
            let net_name = match port.direction {
                skalp_mir::mir::PortDirection::Input => {
                    self.create_input_net(&port.name, 1) // Create input net
                }
                skalp_mir::mir::PortDirection::Output => {
                    self.create_output_net(&port.name, 1) // Create output net
                }
                _ => self.create_net(&port.name, 1), // Fallback for other directions
            };
            self.signal_map.insert(port.name.clone(), net_name.clone());
            // Also map by port ID for LValue resolution
            self.signal_map.insert(format!("port_{}", i), net_name);
        }

        // Create nets for all signals and map them by signal ID
        for (i, signal) in module.signals.iter().enumerate() {
            let net_name = self.create_net(&signal.name, 1); // Simplified: assume 1-bit
            self.signal_map
                .insert(signal.name.clone(), net_name.clone());
            // Also map by signal ID for LValue resolution
            self.signal_map.insert(format!("signal_{}", i), net_name);
        }

        // Transform continuous assignments into gates
        for assign in &module.assignments {
            self.transform_continuous_assign(assign);
        }

        // Transform processes (simplified for now)
        for process in &module.processes {
            self.transform_process(process);
        }

        // Return the completed LIR
        self.lir.clone()
    }

    /// Transform a continuous assignment
    fn transform_continuous_assign(&mut self, assign: &skalp_mir::mir::ContinuousAssign) {
        // Get the target net name from the LHS
        let target_net = self.get_lvalue_net(&assign.lhs);

        // Decompose the RHS expression into gates and get the output net
        let expr_output_net = self.decompose_expression(&assign.rhs);

        // Create a buffer gate to connect expression output to target
        let gate = Gate {
            id: self.create_gate_id("assign"),
            gate_type: GateType::Buffer,
            inputs: vec![expr_output_net],
            outputs: vec![target_net],
        };
        self.lir.gates.push(gate);
    }

    /// Transform a process
    fn transform_process(&mut self, process: &skalp_mir::mir::Process) {
        // For sequential processes, create flip-flops
        // For combinational processes, create logic gates
        // This is a simplified implementation
        match process.kind {
            skalp_mir::mir::ProcessKind::Sequential => {
                // Create DFF for sequential logic
                for statement in &process.body.statements {
                    self.transform_statement(statement);
                }
            }
            skalp_mir::mir::ProcessKind::Combinational => {
                // Create combinational logic
                for statement in &process.body.statements {
                    self.transform_statement(statement);
                }
            }
            skalp_mir::mir::ProcessKind::General => {
                // General process - handle as combinational for now
                for statement in &process.body.statements {
                    self.transform_statement(statement);
                }
            }
        }
    }

    /// Transform a statement
    fn transform_statement(&mut self, stmt: &skalp_mir::mir::Statement) {
        match stmt {
            skalp_mir::mir::Statement::Assignment(assign) => {
                // Create gates for the assignment
                let output_net = self.create_temp_net();
                let input_net = self.create_temp_net();

                let gate = Gate {
                    id: self.create_gate_id("stmt"),
                    gate_type: GateType::Buffer,
                    inputs: vec![input_net],
                    outputs: vec![output_net],
                };
                self.lir.gates.push(gate);
            }
            skalp_mir::mir::Statement::If { .. } => {
                // Create multiplexers for if statements
                let mux_gate = Gate {
                    id: self.create_gate_id("mux"),
                    gate_type: GateType::Buffer, // Simplified
                    inputs: vec![],
                    outputs: vec![],
                };
                self.lir.gates.push(mux_gate);
            }
            skalp_mir::mir::Statement::Case { .. } => {
                // Create decoder and mux tree for case statements
                // Simplified implementation
            }
            _ => {
                // Handle other statement types
            }
        }
    }

    /// Create a new net
    fn create_net(&mut self, base_name: &str, width: usize) -> String {
        let net_name = format!("{}_{}", base_name, self.net_counter);
        self.net_counter += 1;

        let net = Net {
            id: net_name.clone(),
            width,
            driver: None,
            loads: Vec::new(),
            is_output: false, // Default to false, will be set by specific methods
            is_input: false,  // Default to false, will be set by specific methods
        };
        self.lir.nets.push(net);

        net_name
    }

    /// Create a net for an output port
    fn create_output_net(&mut self, base_name: &str, width: usize) -> String {
        let net_name = format!("{}_{}", base_name, self.net_counter);
        self.net_counter += 1;

        let net = Net {
            id: net_name.clone(),
            width,
            driver: None,
            loads: Vec::new(),
            is_output: true, // Mark as output
            is_input: false,
        };
        self.lir.nets.push(net);

        net_name
    }

    /// Create a net for an input port
    fn create_input_net(&mut self, base_name: &str, width: usize) -> String {
        let net_name = format!("{}_{}", base_name, self.net_counter);
        self.net_counter += 1;

        let net = Net {
            id: net_name.clone(),
            width,
            driver: None,
            loads: Vec::new(),
            is_output: false,
            is_input: true, // Mark as input
        };
        self.lir.nets.push(net);

        net_name
    }

    /// Create a temporary net
    fn create_temp_net(&mut self) -> String {
        self.create_net("temp", 1)
    }

    /// Create a gate ID
    fn create_gate_id(&mut self, gate_type: &str) -> String {
        let id = format!("{}_{}", gate_type, self.gate_counter);
        self.gate_counter += 1;
        id
    }

    /// Get the net name for an LValue
    fn get_lvalue_net(&mut self, lvalue: &skalp_mir::mir::LValue) -> String {
        match lvalue {
            skalp_mir::mir::LValue::Port(port_id) => {
                // Look up the port net in our signal map
                let port_key = format!("port_{}", port_id.0);
                if let Some(net_name) = self.signal_map.get(&port_key) {
                    net_name.clone()
                } else {
                    // Fallback: create a new net
                    self.create_temp_net()
                }
            }
            skalp_mir::mir::LValue::Signal(signal_id) => {
                // Look up the signal net in our signal map
                let signal_key = format!("signal_{}", signal_id.0);
                if let Some(net_name) = self.signal_map.get(&signal_key) {
                    net_name.clone()
                } else {
                    // Fallback: create a new net
                    self.create_temp_net()
                }
            }
            skalp_mir::mir::LValue::Variable(var_id) => {
                // For variables, create a temporary net
                format!("var_{}", var_id.0)
            }
            skalp_mir::mir::LValue::BitSelect { base, index: _ } => {
                // For now, treat bit select like the base
                self.get_lvalue_net(base)
            }
            skalp_mir::mir::LValue::RangeSelect {
                base,
                high: _,
                low: _,
            } => {
                // For now, treat range select like the base
                self.get_lvalue_net(base)
            }
            skalp_mir::mir::LValue::Concat(_) => {
                // Create a temporary net for concatenation
                self.create_temp_net()
            }
        }
    }

    /// Decompose an expression into gates and return the output net
    fn decompose_expression(&mut self, expr: &skalp_mir::mir::Expression) -> String {
        match expr {
            skalp_mir::mir::Expression::Literal(value) => {
                // For literals, create a constant driver net

                // In a real implementation, we'd create tie-high/tie-low cells
                // For now, just return the net name
                self.create_temp_net()
            }
            skalp_mir::mir::Expression::Ref(lvalue) => {
                // Reference to a signal/port - return its net
                self.get_lvalue_net(lvalue)
            }
            skalp_mir::mir::Expression::Binary { op, left, right } => {
                // Decompose left and right operands
                let left_net = self.decompose_expression(left);
                let right_net = self.decompose_expression(right);

                // Create output net for this operation
                let output_net = self.create_temp_net();

                // Choose gate type based on operation
                let gate_type = self.binary_op_to_gate_type(op);

                // Create the gate
                let gate = Gate {
                    id: self.create_gate_id(&format!("{:?}", gate_type).to_lowercase()),
                    gate_type,
                    inputs: vec![left_net, right_net],
                    outputs: vec![output_net.clone()],
                };
                self.lir.gates.push(gate);

                output_net
            }
            skalp_mir::mir::Expression::Unary { op, operand } => {
                // Decompose operand
                let input_net = self.decompose_expression(operand);

                // Create output net
                let output_net = self.create_temp_net();

                // Choose gate type based on operation
                let gate_type = self.unary_op_to_gate_type(op);

                // Create the gate
                let gate = Gate {
                    id: self.create_gate_id(&format!("{:?}", gate_type).to_lowercase()),
                    gate_type,
                    inputs: vec![input_net],
                    outputs: vec![output_net.clone()],
                };
                self.lir.gates.push(gate);

                output_net
            }
            // For other expression types, create a placeholder for now
            _ => self.create_temp_net(),
        }
    }

    /// Convert binary operator to gate type
    fn binary_op_to_gate_type(&self, op: &skalp_mir::mir::BinaryOp) -> GateType {
        match op {
            skalp_mir::mir::BinaryOp::And | skalp_mir::mir::BinaryOp::LogicalAnd => GateType::And,
            skalp_mir::mir::BinaryOp::Or | skalp_mir::mir::BinaryOp::LogicalOr => GateType::Or,
            skalp_mir::mir::BinaryOp::Xor => GateType::Xor,
            skalp_mir::mir::BinaryOp::BitwiseAnd => GateType::And,
            skalp_mir::mir::BinaryOp::BitwiseOr => GateType::Or,
            skalp_mir::mir::BinaryOp::BitwiseXor => GateType::Xor,
            // For operations that don't map directly to basic gates, use buffer for now
            _ => GateType::Buffer,
        }
    }

    /// Convert unary operator to gate type
    fn unary_op_to_gate_type(&self, op: &skalp_mir::mir::UnaryOp) -> GateType {
        match op {
            skalp_mir::mir::UnaryOp::Not | skalp_mir::mir::UnaryOp::BitwiseNot => GateType::Not,
            // For other unary operations, use buffer for now
            _ => GateType::Buffer,
        }
    }
}

/// Public API for MIR to LIR transformation
pub fn transform_mir_to_lir(module: &Module) -> Lir {
    let mut transformer = MirToLirTransform::new(module.name.clone());
    transformer.transform(module)
}
