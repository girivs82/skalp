//! HIR to MIR transformation
//!
//! This module converts the high-level HIR representation
//! to the mid-level MIR suitable for code generation

use crate::mir::*;
use skalp_frontend::hir::{self as hir, Hir};
use std::collections::HashMap;

/// HIR to MIR transformer
pub struct HirToMir {
    /// Next module ID
    next_module_id: u32,
    /// Next port ID
    next_port_id: u32,
    /// Next signal ID
    next_signal_id: u32,
    /// Next variable ID
    next_variable_id: u32,
    /// Next process ID
    next_process_id: u32,
    /// Next clock domain ID
    next_clock_domain_id: u32,
    /// Entity to module ID mapping
    entity_map: HashMap<hir::EntityId, ModuleId>,
    /// Port ID mapping (HIR to MIR)
    port_map: HashMap<hir::PortId, PortId>,
    /// Signal ID mapping (HIR to MIR)
    signal_map: HashMap<hir::SignalId, SignalId>,
    /// Variable ID mapping (HIR to MIR)
    variable_map: HashMap<hir::VariableId, VariableId>,
    /// Clock domain ID mapping (HIR to MIR)
    clock_domain_map: HashMap<hir::ClockDomainId, ClockDomainId>,
}

impl HirToMir {
    /// Create a new transformer
    pub fn new() -> Self {
        Self {
            next_module_id: 0,
            next_port_id: 0,
            next_signal_id: 0,
            next_variable_id: 0,
            next_process_id: 0,
            next_clock_domain_id: 0,
            entity_map: HashMap::new(),
            port_map: HashMap::new(),
            signal_map: HashMap::new(),
            variable_map: HashMap::new(),
            clock_domain_map: HashMap::new(),
        }
    }

    /// Transform HIR to MIR
    pub fn transform(&mut self, hir: &Hir) -> Mir {
        let mut mir = Mir::new(hir.name.clone());

        // First pass: create modules for entities
        for entity in &hir.entities {
            let module_id = self.next_module_id();
            self.entity_map.insert(entity.id, module_id);

            let mut module = Module::new(module_id, entity.name.clone());

            // Convert generic parameters
            for hir_generic in &entity.generics {
                let parameter = GenericParameter {
                    name: hir_generic.name.clone(),
                    param_type: self.convert_generic_type(&hir_generic.param_type),
                    default: None, // TODO: Handle default values
                };
                module.parameters.push(parameter);
            }

            // Convert ports
            for hir_port in &entity.ports {
                let port_id = self.next_port_id();
                self.port_map.insert(hir_port.id, port_id);

                let port = Port {
                    id: port_id,
                    name: hir_port.name.clone(),
                    direction: self.convert_port_direction(&hir_port.direction),
                    port_type: self.convert_type(&hir_port.port_type),
                };
                module.ports.push(port);
            }

            mir.add_module(module);
        }

        // Second pass: add implementations
        for impl_block in &hir.implementations {
            if let Some(&module_id) = self.entity_map.get(&impl_block.entity) {
                // Find the module
                if let Some(module) = mir.modules.iter_mut().find(|m| m.id == module_id) {
                    // Add signals
                    for hir_signal in &impl_block.signals {
                        let signal_id = self.next_signal_id();
                        self.signal_map.insert(hir_signal.id, signal_id);

                        let signal = Signal {
                            id: signal_id,
                            name: hir_signal.name.clone(),
                            signal_type: self.convert_type(&hir_signal.signal_type),
                            initial: hir_signal.initial_value.as_ref()
                                .and_then(|expr| self.convert_literal_expr(expr)),
                        };
                        module.signals.push(signal);
                    }

                    // Add variables
                    for hir_var in &impl_block.variables {
                        let var_id = self.next_variable_id();
                        self.variable_map.insert(hir_var.id, var_id);

                        let variable = Variable {
                            id: var_id,
                            name: hir_var.name.clone(),
                            var_type: self.convert_type(&hir_var.var_type),
                            initial: hir_var.initial_value.as_ref()
                                .and_then(|expr| self.convert_literal_expr(expr)),
                        };
                        module.variables.push(variable);
                    }

                    // Convert event blocks to processes
                    for event_block in &impl_block.event_blocks {
                        let process = self.convert_event_block(event_block);
                        module.processes.push(process);
                    }

                    // Convert continuous assignments
                    for hir_assign in &impl_block.assignments {
                        if let Some(assign) = self.convert_continuous_assignment(hir_assign) {
                            module.assignments.push(assign);
                        }
                    }

                    // Convert module instances
                    for hir_instance in &impl_block.instances {
                        if let Some(instance) = self.convert_instance(hir_instance) {
                            module.instances.push(instance);
                        }
                    }
                }
            }
        }

        mir
    }

    /// Convert event block to process
    fn convert_event_block(&mut self, block: &hir::HirEventBlock) -> Process {
        let id = self.next_process_id();

        // Determine process kind and sensitivity
        let (kind, sensitivity) = self.analyze_event_block(block);

        // Convert body
        let body = self.convert_statements(&block.statements);

        Process {
            id,
            kind,
            sensitivity,
            body,
        }
    }

    /// Analyze event block to determine process kind and sensitivity
    fn analyze_event_block(&self, block: &hir::HirEventBlock) -> (ProcessKind, SensitivityList) {
        if block.triggers.is_empty() {
            return (ProcessKind::Combinational, SensitivityList::Always);
        }

        let mut edges = Vec::new();
        let mut is_sequential = false;

        for trigger in &block.triggers {
            // Check if it's a clock edge or reset
            match &trigger.edge {
                hir::HirEdgeType::Rising | hir::HirEdgeType::Falling => {
                    is_sequential = true;
                    edges.push(EdgeSensitivity {
                        signal: self.convert_lvalue_from_signal(trigger.signal),
                        edge: self.convert_edge_type(&trigger.edge),
                    });
                }
                hir::HirEdgeType::Both => {
                    edges.push(EdgeSensitivity {
                        signal: self.convert_lvalue_from_signal(trigger.signal),
                        edge: EdgeType::Both,
                    });
                }
                hir::HirEdgeType::Active | hir::HirEdgeType::Inactive => {
                    // Reset events are typically asynchronous and make the process sequential
                    is_sequential = true;
                    edges.push(EdgeSensitivity {
                        signal: self.convert_lvalue_from_signal(trigger.signal),
                        edge: self.convert_edge_type(&trigger.edge),
                    });
                }
            }
        }

        let kind = if is_sequential {
            ProcessKind::Sequential
        } else {
            ProcessKind::Combinational
        };

        let sensitivity = if !edges.is_empty() {
            SensitivityList::Edge(edges)
        } else {
            SensitivityList::Always
        };

        (kind, sensitivity)
    }

    /// Convert HIR statements to MIR block
    fn convert_statements(&mut self, hir_stmts: &[hir::HirStatement]) -> Block {
        let mut statements = Vec::new();

        for hir_stmt in hir_stmts {
            if let Some(stmt) = self.convert_statement(hir_stmt) {
                statements.push(stmt);
            }
        }

        Block { statements }
    }

    /// Convert HIR statement to MIR statement
    fn convert_statement(&mut self, stmt: &hir::HirStatement) -> Option<Statement> {
        match stmt {
            hir::HirStatement::Assignment(assign) => {
                self.convert_assignment(assign).map(Statement::Assignment)
            }
            hir::HirStatement::If(if_stmt) => {
                self.convert_if_statement(if_stmt).map(Statement::If)
            }
            hir::HirStatement::Match(match_stmt) => {
                self.convert_match_statement(match_stmt).map(Statement::Case)
            }
            hir::HirStatement::Block(stmts) => {
                Some(Statement::Block(self.convert_statements(stmts)))
            }
            hir::HirStatement::Flow(_flow_stmt) => {
                // Flow statements represent pipeline stages
                // For now, treat them as sequential blocks
                // TODO: Implement proper pipeline transformation
                None
            }
        }
    }

    /// Convert HIR assignment to MIR assignment
    fn convert_assignment(&mut self, assign: &hir::HirAssignment) -> Option<Assignment> {
        let lhs = self.convert_lvalue(&assign.lhs)?;
        let rhs = self.convert_expression(&assign.rhs)?;
        let kind = match assign.assignment_type {
            hir::HirAssignmentType::NonBlocking => AssignmentKind::NonBlocking,
            hir::HirAssignmentType::Blocking => AssignmentKind::Blocking,
            hir::HirAssignmentType::Combinational => AssignmentKind::Blocking,
        };

        Some(Assignment { lhs, rhs, kind })
    }

    /// Convert continuous assignment
    fn convert_continuous_assignment(&mut self, assign: &hir::HirAssignment) -> Option<ContinuousAssign> {
        // Only combinational assignments become continuous assigns
        if !matches!(assign.assignment_type, hir::HirAssignmentType::Combinational) {
            return None;
        }

        let lhs = self.convert_lvalue(&assign.lhs)?;
        let rhs = self.convert_expression(&assign.rhs)?;

        Some(ContinuousAssign { lhs, rhs })
    }

    /// Convert module instance
    fn convert_instance(&mut self, instance: &hir::HirInstance) -> Option<ModuleInstance> {
        // Map entity ID to module ID
        let module_id = *self.entity_map.get(&instance.entity)?;

        // Convert connections
        let mut connections = std::collections::HashMap::new();
        for conn in &instance.connections {
            if let Some(expr) = self.convert_expression(&conn.expr) {
                connections.insert(conn.port.clone(), expr);
            }
        }

        Some(ModuleInstance {
            name: instance.name.clone(),
            module: module_id,
            connections,
            parameters: std::collections::HashMap::new(),
        })
    }

    /// Convert HIR if statement to MIR
    fn convert_if_statement(&mut self, if_stmt: &hir::HirIfStatement) -> Option<IfStatement> {
        let condition = self.convert_expression(&if_stmt.condition)?;
        let then_block = self.convert_statements(&if_stmt.then_statements);
        let else_block = if_stmt.else_statements.as_ref()
            .map(|stmts| self.convert_statements(stmts));

        Some(IfStatement {
            condition,
            then_block,
            else_block,
        })
    }

    /// Convert HIR match statement to MIR case statement
    fn convert_match_statement(&mut self, match_stmt: &hir::HirMatchStatement) -> Option<CaseStatement> {
        let expr = self.convert_expression(&match_stmt.expr)?;

        let mut items = Vec::new();
        let mut default = None;

        for arm in &match_stmt.arms {
            let block = self.convert_statements(&arm.statements);

            // Check if this is a default/wildcard pattern
            // TODO: Properly handle patterns - for now just create a default
            if let Some(expr_val) = self.convert_pattern_to_expr(&arm.pattern) {
                items.push(CaseItem {
                    values: vec![expr_val],
                    block: block.clone()
                });
            } else {
                default = Some(block);
            }
        }

        Some(CaseStatement { expr, items, default })
    }

    /// Convert pattern to expression (for case values)
    fn convert_pattern_to_expr(&mut self, _pattern: &hir::HirPattern) -> Option<Expression> {
        // TODO: Implement pattern conversion
        // For now, just return a placeholder
        Some(Expression::Literal(Value::Integer(0)))
    }

    /// Convert HIR lvalue to MIR
    fn convert_lvalue(&mut self, lval: &hir::HirLValue) -> Option<LValue> {
        match lval {
            hir::HirLValue::Signal(id) => {
                self.signal_map.get(id).map(|&id| LValue::Signal(id))
            }
            hir::HirLValue::Variable(id) => {
                self.variable_map.get(id).map(|&id| LValue::Variable(id))
            }
            hir::HirLValue::Index(base, index) => {
                let base = Box::new(self.convert_lvalue(base)?);
                let index = Box::new(self.convert_expression(index)?);
                Some(LValue::BitSelect { base, index })
            }
            hir::HirLValue::Range(base, high, low) => {
                let base = Box::new(self.convert_lvalue(base)?);
                let high = Box::new(self.convert_expression(high)?);
                let low = Box::new(self.convert_expression(low)?);
                Some(LValue::RangeSelect { base, high, low })
            }
        }
    }

    /// Convert signal ID to lvalue
    fn convert_lvalue_from_signal(&self, signal_id: hir::SignalId) -> LValue {
        if let Some(&mir_id) = self.signal_map.get(&signal_id) {
            LValue::Signal(mir_id)
        } else {
            // Fallback - shouldn't happen
            LValue::Signal(SignalId(0))
        }
    }

    /// Convert HIR expression to MIR
    fn convert_expression(&mut self, expr: &hir::HirExpression) -> Option<Expression> {
        match expr {
            hir::HirExpression::Literal(lit) => {
                self.convert_literal(lit).map(Expression::Literal)
            }
            hir::HirExpression::Signal(id) => {
                self.signal_map.get(id)
                    .map(|&id| Expression::Ref(LValue::Signal(id)))
            }
            hir::HirExpression::Variable(id) => {
                self.variable_map.get(id)
                    .map(|&id| Expression::Ref(LValue::Variable(id)))
            }
            hir::HirExpression::Constant(_id) => {
                // TODO: Handle constants
                Some(Expression::Literal(Value::Integer(0)))
            }
            hir::HirExpression::Binary(binary) => {
                let left = Box::new(self.convert_expression(&binary.left)?);
                let right = Box::new(self.convert_expression(&binary.right)?);
                let op = self.convert_binary_op(&binary.op);
                Some(Expression::Binary { op, left, right })
            }
            hir::HirExpression::Unary(unary) => {
                let operand = Box::new(self.convert_expression(&unary.operand)?);
                let op = self.convert_unary_op(&unary.op);
                Some(Expression::Unary { op, operand })
            }
            hir::HirExpression::Call(_call) => {
                // TODO: Handle function calls
                None
            }
            hir::HirExpression::Index(base, index) => {
                // Convert index expression to bit select
                let base_lval = self.expr_to_lvalue(base)?;
                let index_expr = self.convert_expression(index)?;
                Some(Expression::Ref(LValue::BitSelect {
                    base: Box::new(base_lval),
                    index: Box::new(index_expr),
                }))
            }
            hir::HirExpression::Range(base, high, low) => {
                // Convert range expression to range select
                let base_lval = self.expr_to_lvalue(base)?;
                let high_expr = self.convert_expression(high)?;
                let low_expr = self.convert_expression(low)?;
                Some(Expression::Ref(LValue::RangeSelect {
                    base: Box::new(base_lval),
                    high: Box::new(high_expr),
                    low: Box::new(low_expr),
                }))
            }
            hir::HirExpression::FieldAccess { base, field: _ } => {
                // Convert field access to base expression for now
                // TODO: Implement proper struct field access in MIR
                self.convert_expression(base)
            }
        }
    }

    /// Convert literal expression (for initial values)
    fn convert_literal_expr(&mut self, expr: &hir::HirExpression) -> Option<Value> {
        if let hir::HirExpression::Literal(lit) = expr {
            self.convert_literal(lit)
        } else {
            None
        }
    }

    /// Convert HIR literal to MIR value
    fn convert_literal(&mut self, lit: &hir::HirLiteral) -> Option<Value> {
        match lit {
            hir::HirLiteral::Integer(val) => Some(Value::Integer(*val as i64)),
            hir::HirLiteral::Boolean(b) => Some(Value::Integer(if *b { 1 } else { 0 })),
            hir::HirLiteral::String(s) => Some(Value::String(s.clone())),
            hir::HirLiteral::BitVector(bits) => {
                // Convert vector of bools to integer value
                let mut value = 0u64;
                for (i, &bit) in bits.iter().enumerate() {
                    if bit {
                        value |= 1u64 << i;
                    }
                }
                Some(Value::BitVector {
                    width: bits.len(),
                    value
                })
            }
        }
    }

    /// Helper to convert expression to lvalue (for index/range operations)
    fn expr_to_lvalue(&mut self, expr: &hir::HirExpression) -> Option<LValue> {
        match expr {
            hir::HirExpression::Signal(id) => {
                self.signal_map.get(id).map(|&id| LValue::Signal(id))
            }
            hir::HirExpression::Variable(id) => {
                self.variable_map.get(id).map(|&id| LValue::Variable(id))
            }
            _ => None,
        }
    }

    /// Convert HIR binary op to MIR
    fn convert_binary_op(&self, op: &hir::HirBinaryOp) -> BinaryOp {
        match op {
            hir::HirBinaryOp::Add => BinaryOp::Add,
            hir::HirBinaryOp::Sub => BinaryOp::Sub,
            hir::HirBinaryOp::Mul => BinaryOp::Mul,
            hir::HirBinaryOp::Div => BinaryOp::Div,
            hir::HirBinaryOp::Mod => BinaryOp::Mod,
            hir::HirBinaryOp::And => BinaryOp::BitwiseAnd,
            hir::HirBinaryOp::Or => BinaryOp::BitwiseOr,
            hir::HirBinaryOp::Xor => BinaryOp::BitwiseXor,
            hir::HirBinaryOp::Equal => BinaryOp::Equal,
            hir::HirBinaryOp::NotEqual => BinaryOp::NotEqual,
            hir::HirBinaryOp::Less => BinaryOp::Less,
            hir::HirBinaryOp::LessEqual => BinaryOp::LessEqual,
            hir::HirBinaryOp::Greater => BinaryOp::Greater,
            hir::HirBinaryOp::GreaterEqual => BinaryOp::GreaterEqual,
            hir::HirBinaryOp::LogicalAnd => BinaryOp::LogicalAnd,
            hir::HirBinaryOp::LogicalOr => BinaryOp::LogicalOr,
            hir::HirBinaryOp::LeftShift => BinaryOp::LeftShift,
            hir::HirBinaryOp::RightShift => BinaryOp::RightShift,
        }
    }

    /// Convert HIR unary op to MIR
    fn convert_unary_op(&self, op: &hir::HirUnaryOp) -> UnaryOp {
        match op {
            hir::HirUnaryOp::Not => UnaryOp::Not,
            hir::HirUnaryOp::BitwiseNot => UnaryOp::BitwiseNot,
            hir::HirUnaryOp::Negate => UnaryOp::Negate,
        }
    }

    /// Convert HIR type to MIR data type
    fn convert_type(&mut self, hir_type: &hir::HirType) -> DataType {
        match hir_type {
            hir::HirType::Bit(width) => DataType::Bit(*width as usize),
            hir::HirType::Logic(width) => DataType::Logic(*width as usize),
            hir::HirType::Int(width) => DataType::Int(*width as usize),
            hir::HirType::Nat(width) => DataType::Nat(*width as usize),
            hir::HirType::Clock(domain) => DataType::Clock {
                domain: domain.map(|id| ClockDomainId(id.0)),
            },
            hir::HirType::Reset(domain) => DataType::Reset {
                active_high: true,
                domain: domain.map(|id| ClockDomainId(id.0)),
            },
            hir::HirType::Event => DataType::Event,
            hir::HirType::Array(inner_type, _size) => {
                // TODO: Implement proper array support
                self.convert_type(inner_type)
            },
            hir::HirType::Custom(_name) => DataType::Bit(1), // TODO: Resolve custom types
            hir::HirType::Struct(struct_type) => DataType::Struct(Box::new(self.convert_struct_type(struct_type))),
            hir::HirType::Enum(enum_type) => DataType::Enum(Box::new(self.convert_enum_type(enum_type))),
            hir::HirType::Union(union_type) => DataType::Union(Box::new(self.convert_union_type(union_type))),
        }
    }

    /// Convert HIR port direction
    fn convert_port_direction(&self, dir: &hir::HirPortDirection) -> PortDirection {
        match dir {
            hir::HirPortDirection::Input => PortDirection::Input,
            hir::HirPortDirection::Output => PortDirection::Output,
            hir::HirPortDirection::Bidirectional => PortDirection::InOut,
        }
    }

    /// Convert HIR edge type
    fn convert_edge_type(&self, edge: &hir::HirEdgeType) -> EdgeType {
        match edge {
            hir::HirEdgeType::Rising => EdgeType::Rising,
            hir::HirEdgeType::Falling => EdgeType::Falling,
            hir::HirEdgeType::Both => EdgeType::Both,
            hir::HirEdgeType::Active => EdgeType::Active,
            hir::HirEdgeType::Inactive => EdgeType::Inactive,
        }
    }

    // ID generation methods
    /// Convert HIR generic type to MIR generic parameter type
    fn convert_generic_type(&mut self, hir_type: &hir::HirGenericType) -> GenericParameterType {
        match hir_type {
            hir::HirGenericType::Type => GenericParameterType::Type,
            hir::HirGenericType::Const(hir_data_type) => {
                GenericParameterType::Const(self.convert_type(hir_data_type))
            },
            hir::HirGenericType::Width => GenericParameterType::Width,
            hir::HirGenericType::ClockDomain => GenericParameterType::ClockDomain,
        }
    }

    /// Convert HIR struct type to MIR struct type
    fn convert_struct_type(&mut self, hir_struct: &hir::HirStructType) -> StructType {
        let fields = hir_struct.fields.iter()
            .map(|field| StructField {
                name: field.name.clone(),
                field_type: self.convert_type(&field.field_type),
            })
            .collect();

        StructType {
            name: hir_struct.name.clone(),
            fields,
            packed: hir_struct.packed,
        }
    }

    /// Convert HIR enum type to MIR enum type
    fn convert_enum_type(&mut self, hir_enum: &hir::HirEnumType) -> EnumType {
        let variants = hir_enum.variants.iter()
            .map(|variant| EnumVariant {
                name: variant.name.clone(),
                value: variant.value.as_ref().and_then(|expr| self.convert_literal_expr(expr)),
            })
            .collect();

        EnumType {
            name: hir_enum.name.clone(),
            variants,
            base_type: self.convert_type(&hir_enum.base_type),
        }
    }

    /// Convert HIR union type to MIR union type
    fn convert_union_type(&mut self, hir_union: &hir::HirUnionType) -> UnionType {
        let fields = hir_union.fields.iter()
            .map(|field| StructField {
                name: field.name.clone(),
                field_type: self.convert_type(&field.field_type),
            })
            .collect();

        UnionType {
            name: hir_union.name.clone(),
            fields,
            packed: hir_union.packed,
        }
    }

    fn next_module_id(&mut self) -> ModuleId {
        let id = ModuleId(self.next_module_id);
        self.next_module_id += 1;
        id
    }

    fn next_port_id(&mut self) -> PortId {
        let id = PortId(self.next_port_id);
        self.next_port_id += 1;
        id
    }

    fn next_signal_id(&mut self) -> SignalId {
        let id = SignalId(self.next_signal_id);
        self.next_signal_id += 1;
        id
    }

    fn next_variable_id(&mut self) -> VariableId {
        let id = VariableId(self.next_variable_id);
        self.next_variable_id += 1;
        id
    }

    fn next_process_id(&mut self) -> ProcessId {
        let id = ProcessId(self.next_process_id);
        self.next_process_id += 1;
        id
    }
}

impl Default for HirToMir {
    fn default() -> Self {
        Self::new()
    }
}