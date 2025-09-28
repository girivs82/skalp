//! MIR to SIR Transformation
//!
//! Converts MIR (Mid-level IR) to SIR (Simulation IR) for GPU-parallel execution.
//! This transformation optimizes for GPU simulation by:
//! - Separating combinational and sequential logic
//! - Extracting dependency graphs for parallel execution
//! - Flattening hierarchies for GPU workgroups

use crate::sir::*;
use skalp_mir::{Mir, Module, DataType, Process, Statement, Expression, LValue, Value};
use skalp_mir::{SensitivityList, EdgeType as MirEdgeType, BinaryOp as MirBinaryOp};
use skalp_mir::{UnaryOp as MirUnaryOp, ReduceOp as MirReduceOp, Block};
use std::collections::HashMap;
use bitvec::prelude::*;

/// Transformer from MIR to SIR
pub struct MirToSir {
    /// Current signal ID counter
    next_signal_id: u32,
    /// Current combinational block ID counter
    next_comb_block_id: u32,
    /// Current sequential block ID counter
    next_seq_block_id: u32,
    /// Current instance ID counter
    next_instance_id: u32,
    /// Mapping from MIR signal IDs to SIR signal IDs
    signal_map: HashMap<skalp_mir::SignalId, SirSignalId>,
    /// Mapping from MIR port IDs to SIR signal IDs
    port_map: HashMap<skalp_mir::PortId, SirSignalId>,
}

impl MirToSir {
    /// Create a new MIR to SIR transformer
    pub fn new() -> Self {
        Self {
            next_signal_id: 0,
            next_comb_block_id: 0,
            next_seq_block_id: 0,
            next_instance_id: 0,
            signal_map: HashMap::new(),
            port_map: HashMap::new(),
        }
    }

    /// Transform MIR to SIR
    pub fn transform(&mut self, mir: &Mir) -> Sir {
        let mut sir = Sir::new(mir.name.clone());

        // Transform the top-level module first
        if let Some(top_module) = mir.modules.first() {
            sir.top_module = self.transform_module(top_module);

            // Transform all modules
            for module in &mir.modules {
                let sir_module = self.transform_module(module);
                sir.modules.insert(sir_module.name.clone(), sir_module);
            }
        }

        sir
    }

    /// Transform a single module from MIR to SIR
    fn transform_module(&mut self, module: &Module) -> SirModule {
        let mut sir_module = SirModule::new(module.name.clone());

        // Clear mappings for this module
        self.signal_map.clear();
        self.port_map.clear();

        // Transform ports to signals first
        for port in &module.ports {
            let sir_signal = SirSignal {
                id: self.next_signal_id(),
                name: port.name.clone(),
                width: self.get_data_type_width(&port.port_type),
                signal_type: SirSignalType::Port {
                    direction: match port.direction {
                        skalp_mir::PortDirection::Input => SirPortDirection::Input,
                        skalp_mir::PortDirection::Output => SirPortDirection::Output,
                        skalp_mir::PortDirection::InOut => SirPortDirection::InOut,
                    },
                },
                initial_value: None,
            };
            self.port_map.insert(port.id, sir_signal.id);
            sir_module.signals.push(sir_signal);
        }

        // Transform internal signals
        for signal in &module.signals {
            let sir_signal = SirSignal {
                id: self.next_signal_id(),
                name: signal.name.clone(),
                width: self.get_data_type_width(&signal.signal_type),
                signal_type: SirSignalType::Wire,
                initial_value: signal.initial.as_ref().map(|val| self.convert_value_to_bitvec(val)),
            };
            self.signal_map.insert(signal.id, sir_signal.id);
            sir_module.signals.push(sir_signal);
        }

        // Transform variables (they become registers or wires depending on usage)
        for variable in &module.variables {
            let sir_signal = SirSignal {
                id: self.next_signal_id(),
                name: variable.name.clone(),
                width: self.get_data_type_width(&variable.var_type),
                signal_type: SirSignalType::Wire, // Will be updated if used in sequential block
                initial_value: variable.initial.as_ref().map(|val| self.convert_value_to_bitvec(val)),
            };
            // Note: Variables use the same ID space as signals in SIR
            let var_signal_id = skalp_mir::SignalId(variable.id.0);
            self.signal_map.insert(var_signal_id, sir_signal.id);
            sir_module.signals.push(sir_signal);
        }

        // Transform processes
        for process in &module.processes {
            match &process.sensitivity {
                SensitivityList::Always => {
                    // Combinational logic
                    let comb_block = self.transform_combinational_process(process);
                    sir_module.comb_blocks.push(comb_block);
                }
                SensitivityList::Edge(_) => {
                    // Sequential logic
                    let seq_block = self.transform_sequential_process(process);

                    // Update signal types to be registers before moving seq_block
                    self.update_signals_to_registers(&mut sir_module, &seq_block);

                    sir_module.seq_blocks.push(seq_block);
                }
                SensitivityList::Level(_) => {
                    // Level-sensitive (combinational)
                    let comb_block = self.transform_combinational_process(process);
                    sir_module.comb_blocks.push(comb_block);
                }
            }
        }

        // Transform continuous assignments to combinational blocks
        for assignment in &module.assignments {
            let comb_block = self.transform_continuous_assignment(assignment);
            sir_module.comb_blocks.push(comb_block);
        }

        // Transform module instances
        for instance in &module.instances {
            let sir_instance = SirInstance {
                name: instance.name.clone(),
                module_name: format!("module_{}", instance.module.0), // TODO: proper module name lookup
                id: self.next_instance_id(),
            };
            sir_module.instances.push(sir_instance);
        }

        sir_module
    }

    /// Transform a combinational process to a combinational block
    fn transform_combinational_process(&mut self, process: &Process) -> CombinationalBlock {
        let id = self.next_comb_block_id();
        let mut operations = Vec::new();
        let mut inputs = Vec::new();
        let mut outputs = Vec::new();

        // Convert all statements in the block
        self.transform_block_to_operations(&process.body, &mut operations, &mut inputs, &mut outputs);

        CombinationalBlock {
            id,
            inputs,
            outputs,
            operations,
            workgroup_size_hint: None, // Will be determined during optimization
        }
    }

    /// Transform a sequential process to a sequential block
    fn transform_sequential_process(&mut self, process: &Process) -> SequentialBlock {
        let id = self.next_seq_block_id();
        let mut operations = Vec::new();
        let mut inputs = Vec::new();
        let mut outputs = Vec::new();

        // Extract clock and reset from sensitivity list
        let (clock, reset) = self.extract_clock_reset_from_sensitivity(&process.sensitivity);

        // Convert all statements in the block
        self.transform_block_to_operations(&process.body, &mut operations, &mut inputs, &mut outputs);

        SequentialBlock {
            id,
            clock: clock.unwrap_or(SirSignalId(0)), // TODO: handle clock properly
            clock_edge: EdgeType::Rising, // TODO: extract from sensitivity
            reset,
            registers: outputs, // All outputs become registers
            operations,
        }
    }

    /// Transform a continuous assignment to a combinational block
    fn transform_continuous_assignment(&mut self, assignment: &skalp_mir::ContinuousAssign) -> CombinationalBlock {
        let id = self.next_comb_block_id();
        let mut inputs = Vec::new();
        let mut outputs = Vec::new();

        // Convert the assignment
        let target_signal = self.convert_lvalue(&assignment.lhs);
        let source_expr = self.convert_expression(&assignment.rhs, &mut inputs);

        outputs.push(target_signal);

        let operation = SirOperation::Assign {
            target: target_signal,
            source: source_expr,
        };

        CombinationalBlock {
            id,
            inputs,
            outputs,
            operations: vec![operation],
            workgroup_size_hint: None,
        }
    }

    /// Transform a block of statements to SIR operations
    fn transform_block_to_operations(
        &mut self,
        block: &Block,
        operations: &mut Vec<SirOperation>,
        inputs: &mut Vec<SirSignalId>,
        outputs: &mut Vec<SirSignalId>,
    ) {
        for statement in &block.statements {
            self.transform_statement_to_operations(statement, operations, inputs, outputs);
        }
    }

    /// Transform a single statement to SIR operations
    fn transform_statement_to_operations(
        &mut self,
        statement: &Statement,
        operations: &mut Vec<SirOperation>,
        inputs: &mut Vec<SirSignalId>,
        outputs: &mut Vec<SirSignalId>,
    ) {
        match statement {
            Statement::Assignment(assignment) => {
                let target_signal = self.convert_lvalue(&assignment.lhs);
                let source_expr = self.convert_expression(&assignment.rhs, inputs);

                if !outputs.contains(&target_signal) {
                    outputs.push(target_signal);
                }

                let operation = SirOperation::Assign {
                    target: target_signal,
                    source: source_expr,
                };
                operations.push(operation);
            }
            Statement::If(if_stmt) => {
                let condition = self.convert_expression(&if_stmt.condition, inputs);

                // Convert then block
                let mut then_ops = Vec::new();
                self.transform_block_to_operations(&if_stmt.then_block, &mut then_ops, inputs, outputs);

                // For now, convert to conditional assignments
                // TODO: Support proper if-else structure
                for op in then_ops {
                    if let SirOperation::Assign { target, source } = op {
                        let cond_assign = SirOperation::ConditionalAssign {
                            condition: condition.clone(),
                            target,
                            source,
                        };
                        operations.push(cond_assign);
                    }
                }
            }
            Statement::Case(_case_stmt) => {
                // TODO: Implement case statement transformation
                // For now, skip
            }
            Statement::Block(inner_block) => {
                self.transform_block_to_operations(inner_block, operations, inputs, outputs);
            }
            Statement::Loop(_) => {
                // TODO: Implement loop transformation
                // Loops are not directly supported in hardware simulation
            }
        }
    }

    /// Convert MIR LValue to SIR signal ID
    fn convert_lvalue(&self, lvalue: &LValue) -> SirSignalId {
        match lvalue {
            LValue::Signal(signal_id) => {
                self.signal_map.get(signal_id).copied()
                    .unwrap_or(SirSignalId(0)) // TODO: proper error handling
            }
            LValue::Variable(var_id) => {
                let signal_id = skalp_mir::SignalId(var_id.0);
                self.signal_map.get(&signal_id).copied()
                    .unwrap_or(SirSignalId(0))
            }
            LValue::Port(port_id) => {
                self.port_map.get(port_id).copied()
                    .unwrap_or(SirSignalId(0))
            }
            _ => {
                // TODO: Implement bit select, range select, concat
                SirSignalId(0)
            }
        }
    }

    /// Convert MIR Expression to SIR Expression
    fn convert_expression(&self, expr: &Expression, inputs: &mut Vec<SirSignalId>) -> SirExpression {
        match expr {
            Expression::Literal(value) => {
                SirExpression::Constant(self.convert_value_to_bitvec(value))
            }
            Expression::Ref(lvalue) => {
                let signal_id = self.convert_lvalue(lvalue);
                if !inputs.contains(&signal_id) {
                    inputs.push(signal_id);
                }
                SirExpression::Signal(signal_id)
            }
            Expression::Binary { op, left, right } => {
                let left_expr = self.convert_expression(left, inputs);
                let right_expr = self.convert_expression(right, inputs);
                SirExpression::Binary {
                    op: self.convert_binary_op(op),
                    left: Box::new(left_expr),
                    right: Box::new(right_expr),
                }
            }
            Expression::Unary { op, operand } => {
                let operand_expr = self.convert_expression(operand, inputs);
                SirExpression::Unary {
                    op: self.convert_unary_op(op),
                    operand: Box::new(operand_expr),
                }
            }
            Expression::Conditional { cond, then_expr, else_expr } => {
                // Convert ternary to binary with conditional logic
                // TODO: Implement proper conditional expression
                self.convert_expression(then_expr, inputs)
            }
            Expression::Concat(exprs) => {
                let sir_exprs: Vec<SirExpression> = exprs.iter()
                    .map(|e| self.convert_expression(e, inputs))
                    .collect();
                SirExpression::Concat(sir_exprs)
            }
            _ => {
                // TODO: Implement other expression types
                SirExpression::Constant(bitvec![0; 1])
            }
        }
    }

    /// Convert MIR BinaryOp to SIR BinaryOp
    fn convert_binary_op(&self, op: &MirBinaryOp) -> BinaryOp {
        match op {
            MirBinaryOp::Add => BinaryOp::Add,
            MirBinaryOp::Sub => BinaryOp::Sub,
            MirBinaryOp::Mul => BinaryOp::Mul,
            MirBinaryOp::Div => BinaryOp::Div,
            MirBinaryOp::Mod => BinaryOp::Mod,
            MirBinaryOp::And => BinaryOp::And,
            MirBinaryOp::Or => BinaryOp::Or,
            MirBinaryOp::Xor => BinaryOp::Xor,
            MirBinaryOp::BitwiseAnd => BinaryOp::BitwiseAnd,
            MirBinaryOp::BitwiseOr => BinaryOp::BitwiseOr,
            MirBinaryOp::BitwiseXor => BinaryOp::BitwiseXor,
            MirBinaryOp::LogicalAnd => BinaryOp::And,
            MirBinaryOp::LogicalOr => BinaryOp::Or,
            MirBinaryOp::Equal => BinaryOp::Equal,
            MirBinaryOp::NotEqual => BinaryOp::NotEqual,
            MirBinaryOp::Less => BinaryOp::Less,
            MirBinaryOp::LessEqual => BinaryOp::LessEqual,
            MirBinaryOp::Greater => BinaryOp::Greater,
            MirBinaryOp::GreaterEqual => BinaryOp::GreaterEqual,
            MirBinaryOp::LeftShift => BinaryOp::LeftShift,
            MirBinaryOp::RightShift => BinaryOp::RightShift,
        }
    }

    /// Convert MIR UnaryOp to SIR UnaryOp
    fn convert_unary_op(&self, op: &MirUnaryOp) -> UnaryOp {
        match op {
            MirUnaryOp::Not => UnaryOp::Not,
            MirUnaryOp::BitwiseNot => UnaryOp::BitwiseNot,
            MirUnaryOp::Negate => UnaryOp::Negate,
            MirUnaryOp::Reduce(reduce_op) => UnaryOp::Reduce(self.convert_reduce_op(reduce_op)),
        }
    }

    /// Convert MIR ReduceOp to SIR ReduceOp
    fn convert_reduce_op(&self, op: &MirReduceOp) -> ReduceOp {
        match op {
            MirReduceOp::And => ReduceOp::And,
            MirReduceOp::Or => ReduceOp::Or,
            MirReduceOp::Xor => ReduceOp::Xor,
            MirReduceOp::Nand => ReduceOp::Nand,
            MirReduceOp::Nor => ReduceOp::Nor,
            MirReduceOp::Xnor => ReduceOp::Xnor,
        }
    }

    /// Extract clock and reset signals from sensitivity list
    fn extract_clock_reset_from_sensitivity(&self, sensitivity: &SensitivityList) -> (Option<SirSignalId>, Option<ResetSpec>) {
        match sensitivity {
            SensitivityList::Edge(edges) => {
                let mut clock = None;
                let mut reset = None;

                for edge in edges {
                    let signal_id = self.convert_lvalue(&edge.signal);
                    match edge.edge {
                        MirEdgeType::Rising | MirEdgeType::Falling => {
                            if clock.is_none() {
                                clock = Some(signal_id);
                            } else {
                                // This might be a reset signal
                                reset = Some(ResetSpec {
                                    signal: signal_id,
                                    active_high: matches!(edge.edge, MirEdgeType::Rising),
                                    edge: Some(match edge.edge {
                                        MirEdgeType::Rising => EdgeType::Rising,
                                        MirEdgeType::Falling => EdgeType::Falling,
                                        _ => EdgeType::Rising,
                                    }),
                                });
                            }
                        }
                        _ => {}
                    }
                }

                (clock, reset)
            }
            _ => (None, None),
        }
    }

    /// Convert MIR Value to BitVec
    fn convert_value_to_bitvec(&self, value: &Value) -> BitVec {
        match value {
            Value::Integer(n) => {
                let mut bits = bitvec![0; 32];
                for i in 0..32 {
                    bits.set(i, (n >> i) & 1 == 1);
                }
                bits
            }
            Value::BitVector { width, value } => {
                let mut bits = bitvec![0; *width];
                for i in 0..*width {
                    bits.set(i, (value >> i) & 1 == 1);
                }
                bits
            }
            _ => bitvec![0; 1],
        }
    }

    /// Get the width of a data type in bits
    fn get_data_type_width(&self, data_type: &DataType) -> usize {
        match data_type {
            DataType::Bit(width) => *width,
            DataType::Logic(width) => *width,
            DataType::Int(width) => *width,
            DataType::Nat(width) => *width,
            DataType::Clock { .. } => 1,
            DataType::Reset { .. } => 1,
            DataType::Event => 1,
            _ => 1, // TODO: Handle struct, enum, union types
        }
    }

    /// Update signals to be registers based on sequential block usage
    fn update_signals_to_registers(&self, sir_module: &mut SirModule, seq_block: &SequentialBlock) {
        for &register_id in &seq_block.registers {
            if let Some(signal) = sir_module.signals.iter_mut().find(|s| s.id == register_id) {
                signal.signal_type = SirSignalType::Register {
                    clock: seq_block.clock,
                    reset: seq_block.reset.as_ref().map(|r| r.signal),
                    reset_active_high: seq_block.reset.as_ref().map_or(true, |r| r.active_high),
                };
            }
        }
    }

    /// Generate next signal ID
    fn next_signal_id(&mut self) -> SirSignalId {
        let id = SirSignalId(self.next_signal_id);
        self.next_signal_id += 1;
        id
    }

    /// Generate next combinational block ID
    fn next_comb_block_id(&mut self) -> CombBlockId {
        let id = CombBlockId(self.next_comb_block_id);
        self.next_comb_block_id += 1;
        id
    }

    /// Generate next sequential block ID
    fn next_seq_block_id(&mut self) -> SeqBlockId {
        let id = SeqBlockId(self.next_seq_block_id);
        self.next_seq_block_id += 1;
        id
    }

    /// Generate next instance ID
    fn next_instance_id(&mut self) -> InstanceId {
        let id = InstanceId(self.next_instance_id);
        self.next_instance_id += 1;
        id
    }
}

impl Default for MirToSir {
    fn default() -> Self {
        Self::new()
    }
}