//! HIR builder for SKALP
//!
//! Transforms the syntax tree into HIR (High-level Intermediate Representation)

use crate::hir::*;
use crate::syntax::{SyntaxKind, SyntaxNode, SyntaxNodeExt, SyntaxToken};
use crate::types::{Type, Width};
use crate::typeck::TypeChecker;
use std::collections::HashMap;

/// HIR builder context
pub struct HirBuilderContext {
    /// Next IDs for various HIR elements
    next_entity_id: u32,
    next_port_id: u32,
    next_signal_id: u32,
    next_variable_id: u32,
    next_constant_id: u32,
    next_block_id: u32,
    next_assignment_id: u32,
    next_protocol_id: u32,
    next_intent_id: u32,
    next_requirement_id: u32,
    next_instance_id: u32,
    next_clock_domain_id: u32,

    /// Symbol table for name resolution
    symbols: SymbolTable,

    /// Type checker for type information
    type_checker: TypeChecker,

    /// Errors collected during HIR building
    errors: Vec<HirError>,

    /// Built entities (for accessing ports during impl building)
    built_entities: HashMap<String, HirEntity>,
}

/// Symbol table for name resolution
#[derive(Debug, Clone)]
struct SymbolTable {
    /// Maps names to their HIR IDs
    entities: HashMap<String, EntityId>,
    ports: HashMap<String, PortId>,
    signals: HashMap<String, SignalId>,
    variables: HashMap<String, VariableId>,
    constants: HashMap<String, ConstantId>,
    clock_domains: HashMap<String, ClockDomainId>,

    /// Current scope for nested lookups
    scopes: Vec<HashMap<String, SymbolId>>,
}

/// Generic symbol ID
#[derive(Debug, Clone, Copy)]
enum SymbolId {
    Entity(EntityId),
    Port(PortId),
    Signal(SignalId),
    Variable(VariableId),
    Constant(ConstantId),
}

/// HIR building errors
#[derive(Debug, Clone)]
pub struct HirError {
    pub message: String,
    pub location: Option<usize>,
}

impl HirBuilderContext {
    /// Create a new HIR builder context
    pub fn new() -> Self {
        Self {
            next_entity_id: 0,
            next_port_id: 0,
            next_signal_id: 0,
            next_variable_id: 0,
            next_constant_id: 0,
            next_block_id: 0,
            next_assignment_id: 0,
            next_protocol_id: 0,
            next_intent_id: 0,
            next_requirement_id: 0,
            next_instance_id: 0,
            next_clock_domain_id: 0,
            symbols: SymbolTable::new(),
            type_checker: TypeChecker::new(),
            errors: Vec::new(),
            built_entities: HashMap::new(),
        }
    }

    /// Build HIR from syntax tree
    pub fn build(&mut self, root: &SyntaxNode) -> Result<Hir, Vec<HirError>> {
        // First pass: type check
        if let Err(type_errors) = self.type_checker.check_source_file(root) {
            for err in type_errors {
                self.errors.push(HirError {
                    message: format!("Type error: {}", err.error),
                    location: None,
                });
            }
        }

        let mut hir = Hir::new("main".to_string());

        // Build HIR from source file
        for child in root.children() {
            match child.kind() {
                SyntaxKind::ENTITY_DECL => {
                    if let Some(entity) = self.build_entity(&child) {
                        // Store entity for later access by implementations
                        self.built_entities.insert(entity.name.clone(), entity.clone());
                        hir.entities.push(entity);
                    }
                }
                SyntaxKind::IMPL_BLOCK => {
                    if let Some(implementation) = self.build_implementation(&child) {
                        hir.implementations.push(implementation);
                    }
                }
                SyntaxKind::PROTOCOL_DECL => {
                    if let Some(protocol) = self.build_protocol(&child) {
                        hir.protocols.push(protocol);
                    }
                }
                SyntaxKind::INTENT_DECL => {
                    if let Some(intent) = self.build_intent(&child) {
                        hir.intents.push(intent);
                    }
                }
                SyntaxKind::REQUIREMENT_DECL => {
                    if let Some(requirement) = self.build_requirement(&child) {
                        hir.requirements.push(requirement);
                    }
                }
                _ => {}
            }
        }

        if self.errors.is_empty() {
            Ok(hir)
        } else {
            Err(self.errors.clone())
        }
    }

    /// Build entity from syntax node
    fn build_entity(&mut self, node: &SyntaxNode) -> Option<HirEntity> {
        let id = self.next_entity_id();
        let name = self.extract_name(node)?;

        // Register in symbol table
        self.symbols.entities.insert(name.clone(), id);

        // Build ports
        let mut ports = Vec::new();
        if let Some(port_list) = node.first_child_of_kind(SyntaxKind::PORT_LIST) {
            for port_node in port_list.children_of_kind(SyntaxKind::PORT_DECL) {
                if let Some(port) = self.build_port(&port_node) {
                    ports.push(port);
                }
            }
        }

        // Build generics (stub for now)
        let generics = Vec::new();

        Some(HirEntity {
            id,
            name,
            ports,
            generics,
            clock_domains: Vec::new(),  // TODO: Parse clock domain parameters
        })
    }

    /// Build port from syntax node
    fn build_port(&mut self, node: &SyntaxNode) -> Option<HirPort> {
        let id = self.next_port_id();
        let name = self.extract_name(node)?;

        // Get direction
        let direction = if let Some(dir_node) = node.first_child_of_kind(SyntaxKind::PORT_DIRECTION) {
            self.extract_port_direction(&dir_node)
        } else {
            HirPortDirection::Input // Default
        };

        // Get type
        let port_type = self.extract_hir_type(node);

        // Register in symbol table
        self.symbols.ports.insert(name.clone(), id);

        Some(HirPort {
            id,
            name,
            direction,
            port_type,
        })
    }

    /// Build implementation from syntax node
    fn build_implementation(&mut self, node: &SyntaxNode) -> Option<HirImplementation> {
        // Get target entity name
        let entity_name = self.extract_name(node)?;

        // Look up entity ID
        let entity = *self.symbols.entities.get(&entity_name)?;

        // Enter new scope and add ports to symbol table
        self.symbols.enter_scope();

        // Get the built entity and add its ports to the current scope
        if let Some(built_entity) = self.built_entities.get(&entity_name) {
            for port in &built_entity.ports {
                self.symbols.add_to_scope(&port.name, SymbolId::Port(port.id));
            }
        }

        let mut signals = Vec::new();
        let mut variables = Vec::new();
        let mut constants = Vec::new();
        let mut event_blocks = Vec::new();
        let mut assignments = Vec::new();

        // Build implementation items
        for child in node.children() {
            match child.kind() {
                SyntaxKind::SIGNAL_DECL => {
                    if let Some(signal) = self.build_signal(&child) {
                        signals.push(signal);
                    }
                }
                SyntaxKind::VARIABLE_DECL => {
                    if let Some(variable) = self.build_variable(&child) {
                        variables.push(variable);
                    }
                }
                SyntaxKind::CONSTANT_DECL => {
                    if let Some(constant) = self.build_constant(&child) {
                        constants.push(constant);
                    }
                }
                SyntaxKind::EVENT_BLOCK => {
                    if let Some(block) = self.build_event_block(&child) {
                        event_blocks.push(block);
                    }
                }
                SyntaxKind::ASSIGNMENT_STMT => {
                    if let Some(assignment) = self.build_assignment(&child, HirAssignmentType::Combinational) {
                        assignments.push(assignment);
                    }
                }
                _ => {}
            }
        }

        // Exit scope
        self.symbols.exit_scope();

        Some(HirImplementation {
            entity,
            signals,
            variables,
            constants,
            event_blocks,
            assignments,
            instances: vec![], // TODO: Parse instance declarations
        })
    }

    /// Build signal declaration
    fn build_signal(&mut self, node: &SyntaxNode) -> Option<HirSignal> {
        let id = self.next_signal_id();
        let name = self.extract_name(node)?;
        let signal_type = self.extract_hir_type(node);

        // Build initial value
        let initial_value = self.find_initial_value_expr(node);

        // Register in symbol table
        self.symbols.signals.insert(name.clone(), id);
        self.symbols.add_to_scope(&name, SymbolId::Signal(id));

        Some(HirSignal {
            id,
            name,
            signal_type,
            initial_value,
        })
    }

    /// Build variable declaration
    fn build_variable(&mut self, node: &SyntaxNode) -> Option<HirVariable> {
        let id = self.next_variable_id();
        let name = self.extract_name(node)?;
        let var_type = self.extract_hir_type(node);

        // Build initial value
        let initial_value = self.find_initial_value_expr(node);

        // Register in symbol table
        self.symbols.variables.insert(name.clone(), id);
        self.symbols.add_to_scope(&name, SymbolId::Variable(id));

        Some(HirVariable {
            id,
            name,
            var_type,
            initial_value,
        })
    }

    /// Build constant declaration
    fn build_constant(&mut self, node: &SyntaxNode) -> Option<HirConstant> {
        let id = self.next_constant_id();
        let name = self.extract_name(node)?;
        let const_type = self.extract_hir_type(node);

        // Constants must have a value
        let value = self.find_initial_value_expr(node)?;

        // Register in symbol table
        self.symbols.constants.insert(name.clone(), id);
        self.symbols.add_to_scope(&name, SymbolId::Constant(id));

        Some(HirConstant {
            id,
            name,
            const_type,
            value,
        })
    }

    /// Build event block
    fn build_event_block(&mut self, node: &SyntaxNode) -> Option<HirEventBlock> {
        let id = self.next_block_id();

        // Build triggers
        let mut triggers = Vec::new();
        if let Some(trigger_list) = node.first_child_of_kind(SyntaxKind::EVENT_TRIGGER_LIST) {
            for trigger_node in trigger_list.children_of_kind(SyntaxKind::EVENT_TRIGGER) {
                if let Some(trigger) = self.build_event_trigger(&trigger_node) {
                    triggers.push(trigger);
                }
            }
        }

        // Build statements
        let mut statements = Vec::new();
        if let Some(block) = node.first_child_of_kind(SyntaxKind::BLOCK_STMT) {
            statements = self.build_statements(&block);
        }

        Some(HirEventBlock {
            id,
            triggers,
            statements,
        })
    }

    /// Build event trigger
    fn build_event_trigger(&mut self, node: &SyntaxNode) -> Option<HirEventTrigger> {
        // Get signal name
        let signal_name = node.first_token_of_kind(SyntaxKind::IDENT)
            .map(|t| t.text().to_string())?;

        // Look up signal ID
        let signal = self.symbols.lookup(&signal_name)
            .and_then(|id| match id {
                SymbolId::Signal(s) => Some(*s),
                _ => None,
            })?;

        // Get edge type
        let edge = if let Some(edge_node) = node.first_child_of_kind(SyntaxKind::EDGE_TYPE) {
            self.extract_edge_type(&edge_node)
        } else {
            HirEdgeType::Rising // Default
        };

        Some(HirEventTrigger {
            signal,
            edge,
        })
    }

    /// Build statements from block
    fn build_statements(&mut self, node: &SyntaxNode) -> Vec<HirStatement> {
        let mut statements = Vec::new();

        for child in node.children() {
            match child.kind() {
                SyntaxKind::ASSIGNMENT_STMT => {
                    // Determine assignment type from operator
                    let assignment_type = self.determine_assignment_type(&child);
                    if let Some(assignment) = self.build_assignment(&child, assignment_type) {
                        statements.push(HirStatement::Assignment(assignment));
                    }
                }
                SyntaxKind::IF_STMT => {
                    if let Some(if_stmt) = self.build_if_statement(&child) {
                        statements.push(HirStatement::If(if_stmt));
                    }
                }
                SyntaxKind::MATCH_STMT => {
                    if let Some(match_stmt) = self.build_match_statement(&child) {
                        statements.push(HirStatement::Match(match_stmt));
                    }
                }
                SyntaxKind::BLOCK_STMT => {
                    let block_stmts = self.build_statements(&child);
                    statements.push(HirStatement::Block(block_stmts));
                }
                _ => {}
            }
        }

        statements
    }

    /// Build assignment
    fn build_assignment(&mut self, node: &SyntaxNode, assignment_type: HirAssignmentType) -> Option<HirAssignment> {
        let id = self.next_assignment_id();

        // Get LHS and RHS expressions
        let exprs: Vec<_> = node.children()
            .filter(|n| matches!(n.kind(),
                SyntaxKind::LITERAL_EXPR | SyntaxKind::IDENT_EXPR |
                SyntaxKind::BINARY_EXPR | SyntaxKind::UNARY_EXPR |
                SyntaxKind::FIELD_EXPR | SyntaxKind::INDEX_EXPR))
            .collect();

        if exprs.len() < 2 {
            return None;
        }

        let lhs = self.build_lvalue(&exprs[0])?;
        let rhs = self.build_expression(&exprs[1])?;

        Some(HirAssignment {
            id,
            lhs,
            assignment_type,
            rhs,
        })
    }

    /// Build if statement
    fn build_if_statement(&mut self, node: &SyntaxNode) -> Option<HirIfStatement> {
        // Get condition expression
        let condition = node.children()
            .find(|n| matches!(n.kind(),
                SyntaxKind::LITERAL_EXPR | SyntaxKind::IDENT_EXPR |
                SyntaxKind::BINARY_EXPR | SyntaxKind::UNARY_EXPR))
            .and_then(|n| self.build_expression(&n))?;

        // Get then and else blocks
        let blocks: Vec<_> = node.children_of_kind(SyntaxKind::BLOCK_STMT);

        let then_statements = if !blocks.is_empty() {
            self.build_statements(&blocks[0])
        } else {
            Vec::new()
        };

        let else_statements = if blocks.len() > 1 {
            Some(self.build_statements(&blocks[1]))
        } else {
            None
        };

        Some(HirIfStatement {
            condition,
            then_statements,
            else_statements,
        })
    }

    /// Build match statement
    fn build_match_statement(&mut self, node: &SyntaxNode) -> Option<HirMatchStatement> {
        // Get expression being matched
        let expr = node.children()
            .find(|n| matches!(n.kind(),
                SyntaxKind::LITERAL_EXPR | SyntaxKind::IDENT_EXPR |
                SyntaxKind::BINARY_EXPR | SyntaxKind::UNARY_EXPR))
            .and_then(|n| self.build_expression(&n))?;

        // Build match arms
        let arms = Vec::new(); // TODO: Implement match arm parsing

        Some(HirMatchStatement {
            expr,
            arms,
        })
    }

    /// Build L-value expression
    fn build_lvalue(&mut self, node: &SyntaxNode) -> Option<HirLValue> {
        match node.kind() {
            SyntaxKind::IDENT_EXPR => {
                let name = node.first_token_of_kind(SyntaxKind::IDENT)
                    .map(|t| t.text().to_string())?;

                // Look up symbol
                if let Some(symbol) = self.symbols.lookup(&name) {
                    match symbol {
                        SymbolId::Port(id) => {
                            // For ports, we create a special port L-value
                            // For now, treat it as a signal
                            Some(HirLValue::Signal(SignalId(id.0)))
                        }
                        SymbolId::Signal(id) => Some(HirLValue::Signal(*id)),
                        SymbolId::Variable(id) => Some(HirLValue::Variable(*id)),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            // TODO: Handle indexed and ranged L-values
            _ => None,
        }
    }

    /// Build expression
    fn build_expression(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        match node.kind() {
            SyntaxKind::LITERAL_EXPR => self.build_literal_expr(node),
            SyntaxKind::IDENT_EXPR => self.build_ident_expr(node),
            SyntaxKind::BINARY_EXPR => self.build_binary_expr(node),
            SyntaxKind::UNARY_EXPR => self.build_unary_expr(node),
            SyntaxKind::FIELD_EXPR => self.build_field_expr(node),
            SyntaxKind::INDEX_EXPR => self.build_index_expr(node),
            SyntaxKind::PAREN_EXPR => {
                // Unwrap parentheses
                node.children()
                    .find(|n| matches!(n.kind(),
                        SyntaxKind::LITERAL_EXPR | SyntaxKind::IDENT_EXPR |
                        SyntaxKind::BINARY_EXPR | SyntaxKind::UNARY_EXPR))
                    .and_then(|n| self.build_expression(&n))
            }
            _ => None,
        }
    }

    /// Build literal expression
    fn build_literal_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        if let Some(token) = node.first_child_or_token() {
            match token.kind() {
                SyntaxKind::INT_LITERAL => {
                    let text = token.as_token().map(|t| t.text())?;
                    let value = text.parse::<u64>().ok()?;
                    Some(HirExpression::Literal(HirLiteral::Integer(value)))
                }
                SyntaxKind::BIN_LITERAL => {
                    let text = token.as_token().map(|t| t.text())?;
                    let value = parse_bin_literal(text);
                    // Convert to bit vector
                    let bits = format!("{:b}", value)
                        .chars()
                        .map(|c| c == '1')
                        .collect();
                    Some(HirExpression::Literal(HirLiteral::BitVector(bits)))
                }
                SyntaxKind::HEX_LITERAL => {
                    let text = token.as_token().map(|t| t.text())?;
                    let value = parse_hex_literal(text);
                    Some(HirExpression::Literal(HirLiteral::Integer(value)))
                }
                SyntaxKind::STRING_LITERAL => {
                    let text = token.as_token().map(|t| t.text())?;
                    // Remove quotes
                    let s = text.trim_start_matches('"').trim_end_matches('"').to_string();
                    Some(HirExpression::Literal(HirLiteral::String(s)))
                }
                _ => None,
            }
        } else {
            None
        }
    }

    /// Build identifier expression
    fn build_ident_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        let name = node.first_token_of_kind(SyntaxKind::IDENT)
            .map(|t| t.text().to_string())?;

        // Look up symbol
        if let Some(symbol) = self.symbols.lookup(&name) {
            match symbol {
                SymbolId::Port(id) => {
                    // For ports in expressions, treat as signals
                    Some(HirExpression::Signal(SignalId(id.0)))
                }
                SymbolId::Signal(id) => Some(HirExpression::Signal(*id)),
                SymbolId::Variable(id) => Some(HirExpression::Variable(*id)),
                SymbolId::Constant(id) => Some(HirExpression::Constant(*id)),
                _ => None,
            }
        } else {
            self.errors.push(HirError {
                message: format!("Undefined symbol: {}", name),
                location: None,
            });
            None
        }
    }

    /// Build binary expression
    fn build_binary_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        let children: Vec<_> = node.children().collect();
        if children.len() < 2 {
            return None;
        }

        let left = Box::new(self.build_expression(&children[0])?);
        let right = Box::new(self.build_expression(&children[1])?);

        // Get operator
        let op = node.children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|t| t.kind().is_operator())
            .and_then(|t| self.token_to_binary_op(t.kind()))?;

        Some(HirExpression::Binary(HirBinaryExpr {
            left,
            op,
            right,
        }))
    }

    /// Build unary expression
    fn build_unary_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        let operand = node.children().next()
            .and_then(|n| self.build_expression(&n))?;

        // Get operator
        let op = node.children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|t| matches!(t.kind(), SyntaxKind::BANG | SyntaxKind::TILDE | SyntaxKind::MINUS))
            .and_then(|t| self.token_to_unary_op(t.kind()))?;

        Some(HirExpression::Unary(HirUnaryExpr {
            op,
            operand: Box::new(operand),
        }))
    }

    /// Build field expression
    fn build_field_expr(&mut self, _node: &SyntaxNode) -> Option<HirExpression> {
        // TODO: Implement field access
        None
    }

    /// Build index expression
    fn build_index_expr(&mut self, _node: &SyntaxNode) -> Option<HirExpression> {
        // TODO: Implement array indexing
        None
    }

    /// Build protocol (stub)
    fn build_protocol(&mut self, node: &SyntaxNode) -> Option<HirProtocol> {
        let id = self.next_protocol_id();
        let name = self.extract_name(node)?;

        Some(HirProtocol {
            id,
            name,
            signals: Vec::new(),
        })
    }

    /// Build intent (stub)
    fn build_intent(&mut self, node: &SyntaxNode) -> Option<HirIntent> {
        let id = self.next_intent_id();
        let name = self.extract_name(node)?;

        Some(HirIntent {
            id,
            name,
            description: String::new(),
            constraints: Vec::new(),
        })
    }

    /// Build requirement (stub)
    fn build_requirement(&mut self, node: &SyntaxNode) -> Option<HirRequirement> {
        let id = self.next_requirement_id();
        let name = self.extract_name(node)?;

        Some(HirRequirement {
            id,
            name,
            description: String::new(),
            verification: HirVerificationMethod::Simulation,
        })
    }

    // === Helper methods ===

    /// Extract name from node
    fn extract_name(&self, node: &SyntaxNode) -> Option<String> {
        node.children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|t| t.kind() == SyntaxKind::IDENT)
            .map(|t| t.text().to_string())
    }

    /// Extract port direction
    fn extract_port_direction(&self, node: &SyntaxNode) -> HirPortDirection {
        if node.first_token_of_kind(SyntaxKind::IN_KW).is_some() {
            HirPortDirection::Input
        } else if node.first_token_of_kind(SyntaxKind::OUT_KW).is_some() {
            HirPortDirection::Output
        } else if node.first_token_of_kind(SyntaxKind::INOUT_KW).is_some() {
            HirPortDirection::Bidirectional
        } else {
            HirPortDirection::Input
        }
    }

    /// Extract edge type
    fn extract_edge_type(&self, node: &SyntaxNode) -> HirEdgeType {
        if node.first_token_of_kind(SyntaxKind::RISE_KW).is_some() {
            HirEdgeType::Rising
        } else if node.first_token_of_kind(SyntaxKind::FALL_KW).is_some() {
            HirEdgeType::Falling
        } else if node.first_token_of_kind(SyntaxKind::EDGE_KW).is_some() {
            HirEdgeType::Both
        } else {
            HirEdgeType::Rising
        }
    }

    /// Extract HIR type from node
    fn extract_hir_type(&self, _node: &SyntaxNode) -> HirType {
        // TODO: Convert from Type to HirType
        HirType::Bit(8) // Placeholder
    }

    /// Find initial value expression
    fn find_initial_value_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // Look for expression after '='
        let mut found_assign = false;
        for child in node.children() {
            if found_assign {
                return self.build_expression(&child);
            }
            if child.children_with_tokens()
                .any(|e| e.kind() == SyntaxKind::ASSIGN) {
                found_assign = true;
            }
        }
        None
    }

    /// Determine assignment type from operator
    fn determine_assignment_type(&self, node: &SyntaxNode) -> HirAssignmentType {
        if node.children_with_tokens()
            .any(|e| e.kind() == SyntaxKind::NON_BLOCKING_ASSIGN) {
            HirAssignmentType::NonBlocking
        } else if node.children_with_tokens()
            .any(|e| e.kind() == SyntaxKind::BLOCKING_ASSIGN) {
            HirAssignmentType::Blocking
        } else {
            HirAssignmentType::Combinational
        }
    }

    /// Convert token to binary operator
    fn token_to_binary_op(&self, kind: SyntaxKind) -> Option<HirBinaryOp> {
        match kind {
            SyntaxKind::PLUS => Some(HirBinaryOp::Add),
            SyntaxKind::MINUS => Some(HirBinaryOp::Sub),
            SyntaxKind::STAR => Some(HirBinaryOp::Mul),
            SyntaxKind::SLASH => Some(HirBinaryOp::Div),
            SyntaxKind::PERCENT => Some(HirBinaryOp::Mod),
            SyntaxKind::AMP => Some(HirBinaryOp::And),
            SyntaxKind::PIPE => Some(HirBinaryOp::Or),
            SyntaxKind::CARET => Some(HirBinaryOp::Xor),
            SyntaxKind::EQ => Some(HirBinaryOp::Equal),
            SyntaxKind::NEQ => Some(HirBinaryOp::NotEqual),
            SyntaxKind::LT => Some(HirBinaryOp::Less),
            SyntaxKind::LE => Some(HirBinaryOp::LessEqual),
            SyntaxKind::GT => Some(HirBinaryOp::Greater),
            SyntaxKind::GE => Some(HirBinaryOp::GreaterEqual),
            SyntaxKind::AMP_AMP => Some(HirBinaryOp::LogicalAnd),
            SyntaxKind::PIPE_PIPE => Some(HirBinaryOp::LogicalOr),
            SyntaxKind::SHL => Some(HirBinaryOp::LeftShift),
            SyntaxKind::SHR => Some(HirBinaryOp::RightShift),
            _ => None,
        }
    }

    /// Convert token to unary operator
    fn token_to_unary_op(&self, kind: SyntaxKind) -> Option<HirUnaryOp> {
        match kind {
            SyntaxKind::BANG => Some(HirUnaryOp::Not),
            SyntaxKind::MINUS => Some(HirUnaryOp::Negate),
            SyntaxKind::TILDE => Some(HirUnaryOp::BitwiseNot),
            _ => None,
        }
    }

    // ID generation methods
    fn next_entity_id(&mut self) -> EntityId {
        let id = EntityId(self.next_entity_id);
        self.next_entity_id += 1;
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

    fn next_constant_id(&mut self) -> ConstantId {
        let id = ConstantId(self.next_constant_id);
        self.next_constant_id += 1;
        id
    }

    fn next_block_id(&mut self) -> BlockId {
        let id = BlockId(self.next_block_id);
        self.next_block_id += 1;
        id
    }

    fn next_assignment_id(&mut self) -> AssignmentId {
        let id = AssignmentId(self.next_assignment_id);
        self.next_assignment_id += 1;
        id
    }

    fn next_protocol_id(&mut self) -> ProtocolId {
        let id = ProtocolId(self.next_protocol_id);
        self.next_protocol_id += 1;
        id
    }

    fn next_intent_id(&mut self) -> IntentId {
        let id = IntentId(self.next_intent_id);
        self.next_intent_id += 1;
        id
    }

    fn next_requirement_id(&mut self) -> RequirementId {
        let id = RequirementId(self.next_requirement_id);
        self.next_requirement_id += 1;
        id
    }
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            entities: HashMap::new(),
            ports: HashMap::new(),
            signals: HashMap::new(),
            variables: HashMap::new(),
            constants: HashMap::new(),
            clock_domains: HashMap::new(),
            scopes: vec![HashMap::new()], // Start with global scope
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    fn add_to_scope(&mut self, name: &str, id: SymbolId) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), id);
        }
    }

    fn lookup(&self, name: &str) -> Option<&SymbolId> {
        // Search from innermost to outermost scope
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.get(name) {
                return Some(id);
            }
        }
        None
    }
}

/// Parse binary literal
fn parse_bin_literal(text: &str) -> u64 {
    let without_prefix = text.strip_prefix("0b").unwrap_or(text);
    u64::from_str_radix(without_prefix, 2).unwrap_or(0)
}

/// Parse hex literal
fn parse_hex_literal(text: &str) -> u64 {
    let without_prefix = text.strip_prefix("0x").unwrap_or(text);
    u64::from_str_radix(without_prefix, 16).unwrap_or(0)
}

impl Default for HirBuilderContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Build HIR from syntax tree
pub fn build_hir(root: &SyntaxNode) -> Result<Hir, Vec<HirError>> {
    let mut builder = HirBuilderContext::new();
    builder.build(root)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse;

    #[test]
    fn test_build_simple_entity() {
        let source = r#"
            entity Counter {
                in clk: clock
                out count: nat[8]
            }
        "#;

        let tree = parse(source);
        let result = build_hir(&tree);

        assert!(result.is_ok());
        let hir = result.unwrap();
        assert_eq!(hir.entities.len(), 1);
        assert_eq!(hir.entities[0].name, "Counter");
        assert_eq!(hir.entities[0].ports.len(), 2);
    }

    #[test]
    fn test_build_impl_with_signal() {
        let source = r#"
            entity Counter {
                in clk: clock
                out count: nat[8]
            }

            impl Counter {
                signal counter: nat[8] = 0

                on(clk.rise) {
                    counter <= counter + 1
                }

                count = counter
            }
        "#;

        let tree = parse(source);
        let result = build_hir(&tree);

        assert!(result.is_ok());
        let hir = result.unwrap();
        assert_eq!(hir.implementations.len(), 1);
        assert_eq!(hir.implementations[0].signals.len(), 1);
        assert_eq!(hir.implementations[0].event_blocks.len(), 1);
    }
}