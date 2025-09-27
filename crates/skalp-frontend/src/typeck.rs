//! Type checking for SKALP
//!
//! This module implements the type checker that operates on the syntax tree

use crate::syntax::{SyntaxKind, SyntaxNode, SyntaxNodeExt};
use crate::types::{Type, Width, TypeEnv, TypeInference, TypeError, TypeScheme, ResetPolarity};
use std::collections::HashMap;

/// Type checker for SKALP
pub struct TypeChecker {
    /// Type environment
    env: TypeEnv,

    /// Type inference engine
    inference: TypeInference,

    /// Collected errors
    errors: Vec<TypeCheckError>,

    /// Node type cache
    node_types: HashMap<usize, Type>,
}

/// Type checking error with location information
#[derive(Debug, Clone)]
pub struct TypeCheckError {
    /// Error details
    pub error: TypeError,

    /// Location in source (if available)
    pub location: Option<Location>,
}

/// Source location
#[derive(Debug, Clone)]
pub struct Location {
    /// Line number (1-based)
    pub line: usize,

    /// Column number (1-based)
    pub column: usize,
}

impl TypeChecker {
    /// Create a new type checker
    pub fn new() -> Self {
        Self {
            env: TypeEnv::new(),
            inference: TypeInference::new(),
            errors: Vec::new(),
            node_types: HashMap::new(),
        }
    }

    /// Type check a source file
    pub fn check_source_file(&mut self, root: &SyntaxNode) -> Result<(), Vec<TypeCheckError>> {
        assert_eq!(root.kind(), SyntaxKind::SOURCE_FILE);

        // Check all top-level items
        for child in root.children() {
            match child.kind() {
                SyntaxKind::ENTITY_DECL => self.check_entity_decl(&child),
                SyntaxKind::IMPL_BLOCK => self.check_impl_block(&child),
                _ => {} // Skip other nodes for now
            }
        }

        // Solve constraints
        if let Err(e) = self.inference.solve_constraints() {
            self.errors.push(TypeCheckError {
                error: e,
                location: None,
            });
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    /// Check entity declaration
    fn check_entity_decl(&mut self, node: &SyntaxNode) {
        // Extract entity name
        let name = self.get_entity_name(node);

        // Create a new scope for the entity
        let parent_env = self.env.clone();
        self.env = TypeEnv::child(parent_env.clone());

        // Check ports
        if let Some(port_list) = node.first_child_of_kind(SyntaxKind::PORT_LIST) {
            for port in port_list.children_of_kind(SyntaxKind::PORT_DECL) {
                self.check_port_decl(&port);
            }
        }

        // Restore parent environment
        self.env = parent_env.clone();

        // Register entity type
        // TODO: Create proper entity type
    }

    /// Check implementation block
    fn check_impl_block(&mut self, node: &SyntaxNode) {
        // Extract target entity name
        let entity_name = self.get_impl_entity_name(node);

        // Create a new scope for the implementation
        let parent_env = self.env.clone();
        self.env = TypeEnv::child(parent_env.clone());

        // Check implementation items
        for child in node.children() {
            match child.kind() {
                SyntaxKind::SIGNAL_DECL => self.check_signal_decl(&child),
                SyntaxKind::VARIABLE_DECL => self.check_variable_decl(&child),
                SyntaxKind::CONSTANT_DECL => self.check_constant_decl(&child),
                SyntaxKind::EVENT_BLOCK => self.check_event_block(&child),
                SyntaxKind::ASSIGNMENT_STMT => self.check_assignment_stmt(&child),
                _ => {}
            }
        }

        // Restore parent environment
        self.env = parent_env.clone();
    }

    /// Check port declaration
    fn check_port_decl(&mut self, node: &SyntaxNode) {
        // Get port name and type
        let name = self.get_port_name(node);
        let port_type = self.extract_type(node);

        // Add to environment
        let scheme = TypeScheme {
            type_params: vec![],
            width_params: vec![],
            ty: port_type.clone(),
        };
        self.env.bind(name.clone(), scheme);

        // Cache the type
        self.node_types.insert(node.text_range().start().into(), port_type);
    }

    /// Check signal declaration
    fn check_signal_decl(&mut self, node: &SyntaxNode) {
        let name = self.get_signal_name(node);
        let signal_type = self.extract_type(node);

        // Check initial value if present
        if let Some(init_expr) = self.find_initial_value(node) {
            let init_type = self.check_expression(&init_expr);

            // Check that initial value matches signal type
            if let Err(e) = self.inference.check_assignment(&signal_type, &init_type) {
                self.errors.push(TypeCheckError {
                    error: e,
                    location: None,
                });
            }
        }

        // Add to environment
        let scheme = TypeScheme {
            type_params: vec![],
            width_params: vec![],
            ty: signal_type.clone(),
        };
        self.env.bind(name.clone(), scheme);

        self.node_types.insert(node.text_range().start().into(), signal_type);
    }

    /// Check variable declaration
    fn check_variable_decl(&mut self, node: &SyntaxNode) {
        let name = self.get_variable_name(node);
        let var_type = self.extract_type(node);

        // Check initial value if present
        if let Some(init_expr) = self.find_initial_value(node) {
            let init_type = self.check_expression(&init_expr);

            if let Err(e) = self.inference.check_assignment(&var_type, &init_type) {
                self.errors.push(TypeCheckError {
                    error: e,
                    location: None,
                });
            }
        }

        // Add to environment
        let scheme = TypeScheme {
            type_params: vec![],
            width_params: vec![],
            ty: var_type.clone(),
        };
        self.env.bind(name.clone(), scheme);

        self.node_types.insert(node.text_range().start().into(), var_type);
    }

    /// Check constant declaration
    fn check_constant_decl(&mut self, node: &SyntaxNode) {
        let name = self.get_constant_name(node);
        let const_type = self.extract_type(node);

        // Constants must have an initial value
        if let Some(init_expr) = self.find_initial_value(node) {
            let init_type = self.check_expression(&init_expr);

            if let Err(e) = self.inference.check_assignment(&const_type, &init_type) {
                self.errors.push(TypeCheckError {
                    error: e,
                    location: None,
                });
            }
        } else {
            self.errors.push(TypeCheckError {
                error: TypeError::CannotInfer,
                location: None,
            });
        }

        // Add to environment
        let scheme = TypeScheme {
            type_params: vec![],
            width_params: vec![],
            ty: const_type.clone(),
        };
        self.env.bind(name.clone(), scheme);

        self.node_types.insert(node.text_range().start().into(), const_type);
    }

    /// Check event block
    fn check_event_block(&mut self, node: &SyntaxNode) {
        // Check event triggers
        if let Some(trigger_list) = node.first_child_of_kind(SyntaxKind::EVENT_TRIGGER_LIST) {
            for trigger in trigger_list.children_of_kind(SyntaxKind::EVENT_TRIGGER) {
                self.check_event_trigger(&trigger);
            }
        }

        // Check block body
        if let Some(block) = node.first_child_of_kind(SyntaxKind::BLOCK_STMT) {
            self.check_block_stmt(&block);
        }
    }

    /// Check event trigger
    fn check_event_trigger(&mut self, node: &SyntaxNode) {
        // Get signal name from identifier token
        if let Some(ident) = node.first_token_of_kind(SyntaxKind::IDENT) {
            let signal_name = ident.text();

            // Look up signal type
            if let Some(scheme) = self.env.lookup(signal_name) {
                let signal_type = &scheme.ty;

                // Check that it's a clock or event type
                if !signal_type.is_clock() && signal_type != &Type::Event {
                    self.errors.push(TypeCheckError {
                        error: TypeError::NotClock(signal_type.clone()),
                        location: None,
                    });
                }
            } else {
                self.errors.push(TypeCheckError {
                    error: TypeError::UndefinedVariable(signal_name.to_string()),
                    location: None,
                });
            }
        }
    }

    /// Check assignment statement
    fn check_assignment_stmt(&mut self, node: &SyntaxNode) {
        // Get LHS and RHS expressions
        let children: Vec<_> = node.children().collect();

        if children.len() >= 2 {
            let lhs_type = self.check_expression(&children[0]);
            let rhs_type = self.check_expression(&children[1]);

            // Check assignment compatibility
            if let Err(e) = self.inference.check_assignment(&lhs_type, &rhs_type) {
                self.errors.push(TypeCheckError {
                    error: e,
                    location: None,
                });
            }
        }
    }

    /// Check block statement
    fn check_block_stmt(&mut self, node: &SyntaxNode) {
        // Create new scope
        let parent_env = self.env.clone();
        self.env = TypeEnv::child(parent_env.clone());

        // Check all statements
        for child in node.children() {
            match child.kind() {
                SyntaxKind::ASSIGNMENT_STMT => self.check_assignment_stmt(&child),
                SyntaxKind::IF_STMT => self.check_if_stmt(&child),
                SyntaxKind::MATCH_STMT => self.check_match_stmt(&child),
                SyntaxKind::BLOCK_STMT => self.check_block_stmt(&child),
                _ => {}
            }
        }

        // Restore parent scope
        self.env = parent_env;
    }

    /// Check if statement
    fn check_if_stmt(&mut self, node: &SyntaxNode) {
        // Check condition is boolean/bit
        if let Some(cond_expr) = node.children().find(|n|
            matches!(n.kind(),
                SyntaxKind::LITERAL_EXPR |
                SyntaxKind::IDENT_EXPR |
                SyntaxKind::BINARY_EXPR |
                SyntaxKind::UNARY_EXPR)
        ) {
            let cond_type = self.check_expression(&cond_expr);

            // Condition should be bit[1] or boolean
            match &cond_type {
                Type::Bit(Width::Fixed(1)) => {} // OK
                _ => {
                    // Try to coerce to bit[1]
                    self.inference.add_constraint(
                        crate::types::TypeConstraint::Equal(
                            cond_type,
                            Type::Bit(Width::Fixed(1))
                        )
                    );
                }
            }
        }

        // Check then and else blocks
        for block in node.children_of_kind(SyntaxKind::BLOCK_STMT) {
            self.check_block_stmt(&block);
        }
    }

    /// Check match statement
    fn check_match_stmt(&mut self, node: &SyntaxNode) {
        // TODO: Implement match statement checking
    }

    /// Check expression and return its type
    fn check_expression(&mut self, node: &SyntaxNode) -> Type {
        match node.kind() {
            SyntaxKind::LITERAL_EXPR => self.check_literal_expr(node),
            SyntaxKind::IDENT_EXPR => self.check_ident_expr(node),
            SyntaxKind::BINARY_EXPR => self.check_binary_expr(node),
            SyntaxKind::UNARY_EXPR => self.check_unary_expr(node),
            SyntaxKind::PAREN_EXPR => {
                // Check inner expression
                if let Some(inner) = node.children().find(|n|
                    matches!(n.kind(),
                        SyntaxKind::LITERAL_EXPR |
                        SyntaxKind::IDENT_EXPR |
                        SyntaxKind::BINARY_EXPR |
                        SyntaxKind::UNARY_EXPR)
                ) {
                    self.check_expression(&inner)
                } else {
                    Type::Error
                }
            }
            _ => Type::Error,
        }
    }

    /// Check literal expression
    fn check_literal_expr(&mut self, node: &SyntaxNode) -> Type {
        if let Some(token) = node.first_child_or_token() {
            match token.kind() {
                SyntaxKind::INT_LITERAL => {
                    // Parse literal value
                    let text = token.as_token().map(|t| t.text()).unwrap_or("");
                    let value = text.parse::<u64>().unwrap_or(0);
                    self.inference.infer_int_literal(value)
                }
                SyntaxKind::BIN_LITERAL => {
                    let text = token.as_token().map(|t| t.text()).unwrap_or("");
                    let value = parse_bin_literal(text);
                    self.inference.infer_bin_literal(value, None)
                }
                SyntaxKind::HEX_LITERAL => {
                    let text = token.as_token().map(|t| t.text()).unwrap_or("");
                    let value = parse_hex_literal(text);
                    self.inference.infer_hex_literal(value, None)
                }
                SyntaxKind::STRING_LITERAL => {
                    // Strings are not really supported in hardware
                    Type::Error
                }
                _ => Type::Error,
            }
        } else {
            Type::Error
        }
    }

    /// Check identifier expression
    fn check_ident_expr(&mut self, node: &SyntaxNode) -> Type {
        if let Some(ident) = node.first_token_of_kind(SyntaxKind::IDENT) {
            let name = ident.text();

            if let Some(scheme) = self.env.lookup(name) {
                scheme.ty.clone()
            } else {
                self.errors.push(TypeCheckError {
                    error: TypeError::UndefinedVariable(name.to_string()),
                    location: None,
                });
                Type::Error
            }
        } else {
            Type::Error
        }
    }

    /// Check binary expression
    fn check_binary_expr(&mut self, node: &SyntaxNode) -> Type {
        let children: Vec<_> = node.children().collect();

        if children.len() >= 2 {
            let left_type = self.check_expression(&children[0]);
            let right_type = self.check_expression(&children[1]);

            // Get operator
            let op_token = node.children_with_tokens()
                .filter_map(|elem| elem.into_token())
                .find(|t| t.kind().is_operator());

            if let Some(op) = op_token {
                self.check_binary_op(op.kind(), left_type, right_type)
            } else {
                Type::Error
            }
        } else {
            Type::Error
        }
    }

    /// Check unary expression
    fn check_unary_expr(&mut self, node: &SyntaxNode) -> Type {
        if let Some(operand) = node.children().next() {
            let operand_type = self.check_expression(&operand);

            // Get operator
            let op_token = node.children_with_tokens()
                .filter_map(|elem| elem.into_token())
                .find(|t| matches!(t.kind(),
                    SyntaxKind::BANG | SyntaxKind::TILDE | SyntaxKind::MINUS));

            if let Some(op) = op_token {
                self.check_unary_op(op.kind(), operand_type)
            } else {
                Type::Error
            }
        } else {
            Type::Error
        }
    }

    /// Check binary operator type rules
    fn check_binary_op(&mut self, op: SyntaxKind, left: Type, right: Type) -> Type {
        match op {
            // Arithmetic operators
            SyntaxKind::PLUS | SyntaxKind::MINUS | SyntaxKind::STAR | SyntaxKind::SLASH => {
                // Both operands must be numeric and same type
                self.inference.add_constraint(crate::types::TypeConstraint::Numeric(left.clone()));
                self.inference.add_constraint(crate::types::TypeConstraint::Numeric(right.clone()));
                self.inference.add_constraint(crate::types::TypeConstraint::Equal(left.clone(), right.clone()));
                left // Result has same type as operands
            }

            // Comparison operators
            SyntaxKind::EQ | SyntaxKind::NEQ |
            SyntaxKind::LT | SyntaxKind::GT |
            SyntaxKind::LE | SyntaxKind::GE => {
                // Operands must match, result is bit[1]
                self.inference.add_constraint(crate::types::TypeConstraint::Equal(left, right));
                Type::Bit(Width::Fixed(1))
            }

            // Logical operators
            SyntaxKind::AMP_AMP | SyntaxKind::PIPE_PIPE => {
                // Operands must be bit[1], result is bit[1]
                self.inference.add_constraint(
                    crate::types::TypeConstraint::Equal(left, Type::Bit(Width::Fixed(1)))
                );
                self.inference.add_constraint(
                    crate::types::TypeConstraint::Equal(right, Type::Bit(Width::Fixed(1)))
                );
                Type::Bit(Width::Fixed(1))
            }

            // Bitwise operators
            SyntaxKind::AMP | SyntaxKind::PIPE | SyntaxKind::CARET => {
                // Both operands must be same bit/logic type
                self.inference.add_constraint(crate::types::TypeConstraint::Equal(left.clone(), right.clone()));
                left
            }

            // Shift operators
            SyntaxKind::SHL | SyntaxKind::SHR => {
                // Left operand determines result type
                // Right operand should be unsigned
                self.inference.add_constraint(crate::types::TypeConstraint::Numeric(right));
                left
            }

            _ => Type::Error
        }
    }

    /// Check unary operator type rules
    fn check_unary_op(&mut self, op: SyntaxKind, operand: Type) -> Type {
        match op {
            SyntaxKind::BANG => {
                // Logical not - operand must be bit[1]
                self.inference.add_constraint(
                    crate::types::TypeConstraint::Equal(operand, Type::Bit(Width::Fixed(1)))
                );
                Type::Bit(Width::Fixed(1))
            }

            SyntaxKind::TILDE => {
                // Bitwise not - preserves type
                self.inference.add_constraint(crate::types::TypeConstraint::Numeric(operand.clone()));
                operand
            }

            SyntaxKind::MINUS => {
                // Arithmetic negation
                self.inference.add_constraint(crate::types::TypeConstraint::Numeric(operand.clone()));
                operand
            }

            _ => Type::Error
        }
    }

    // === Helper methods ===

    /// Extract type from a type annotation node
    fn extract_type(&mut self, node: &SyntaxNode) -> Type {
        if let Some(type_node) = node.first_child_of_kind(SyntaxKind::TYPE_ANNOTATION) {
            self.parse_type_annotation(&type_node)
        } else {
            // Try to find type in children
            for child in node.children() {
                if matches!(child.kind(),
                    SyntaxKind::BIT_TYPE | SyntaxKind::LOGIC_TYPE |
                    SyntaxKind::INT_TYPE | SyntaxKind::NAT_TYPE |
                    SyntaxKind::CLOCK_TYPE | SyntaxKind::RESET_TYPE) {
                    return self.parse_type_node(&child);
                }
            }
            Type::Unknown
        }
    }

    /// Parse type annotation
    fn parse_type_annotation(&mut self, node: &SyntaxNode) -> Type {
        // Find the actual type node
        if let Some(type_node) = node.children().next() {
            self.parse_type_node(&type_node)
        } else {
            Type::Unknown
        }
    }

    /// Parse a type node
    fn parse_type_node(&mut self, node: &SyntaxNode) -> Type {
        match node.kind() {
            SyntaxKind::BIT_TYPE => {
                let width = self.extract_width(node);
                Type::Bit(width)
            }
            SyntaxKind::LOGIC_TYPE => {
                let width = self.extract_width(node);
                Type::Logic(width)
            }
            SyntaxKind::INT_TYPE => {
                let width = self.extract_width(node);
                Type::Int(width)
            }
            SyntaxKind::NAT_TYPE => {
                let width = self.extract_width(node);
                Type::Nat(width)
            }
            SyntaxKind::CLOCK_TYPE => Type::Clock(None),
            SyntaxKind::RESET_TYPE => Type::Reset(ResetPolarity::ActiveHigh),
            SyntaxKind::CUSTOM_TYPE => {
                // Get type name
                if let Some(ident) = node.first_token_of_kind(SyntaxKind::IDENT) {
                    let name = ident.text();
                    // Look up type definition
                    if let Some(ty) = self.env.lookup_type(name) {
                        ty.clone()
                    } else {
                        Type::Unknown
                    }
                } else {
                    Type::Unknown
                }
            }
            _ => Type::Unknown
        }
    }

    /// Extract width specification
    fn extract_width(&mut self, node: &SyntaxNode) -> Width {
        if let Some(width_spec) = node.first_child_of_kind(SyntaxKind::WIDTH_SPEC) {
            if let Some(literal) = width_spec.first_token_of_kind(SyntaxKind::INT_LITERAL) {
                let text = literal.text();
                if let Ok(n) = text.parse::<u32>() {
                    return Width::Fixed(n);
                }
            }
        }
        Width::Unknown
    }

    /// Find initial value expression
    fn find_initial_value(&self, node: &SyntaxNode) -> Option<SyntaxNode> {
        // Look for expression after assignment operator
        let mut found_assign = false;
        for child in node.children() {
            if found_assign {
                if matches!(child.kind(),
                    SyntaxKind::LITERAL_EXPR |
                    SyntaxKind::IDENT_EXPR |
                    SyntaxKind::BINARY_EXPR |
                    SyntaxKind::UNARY_EXPR) {
                    return Some(child);
                }
            }
            if child.kind() == SyntaxKind::ASSIGN {
                found_assign = true;
            }
        }
        None
    }

    /// Get entity name
    fn get_entity_name(&self, node: &SyntaxNode) -> String {
        node.children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|t| t.kind() == SyntaxKind::IDENT)
            .map(|t| t.text().to_string())
            .unwrap_or_else(|| "unknown".to_string())
    }

    /// Get implementation entity name
    fn get_impl_entity_name(&self, node: &SyntaxNode) -> String {
        self.get_entity_name(node)
    }

    /// Get port name
    fn get_port_name(&self, node: &SyntaxNode) -> String {
        self.get_entity_name(node)
    }

    /// Get signal name
    fn get_signal_name(&self, node: &SyntaxNode) -> String {
        self.get_entity_name(node)
    }

    /// Get variable name
    fn get_variable_name(&self, node: &SyntaxNode) -> String {
        self.get_entity_name(node)
    }

    /// Get constant name
    fn get_constant_name(&self, node: &SyntaxNode) -> String {
        self.get_entity_name(node)
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

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse;

    #[test]
    fn test_type_check_simple_entity() {
        let source = r#"
            entity Counter {
                in clk: clock
                out count: nat[8]
            }
        "#;

        let tree = parse(source);
        let mut checker = TypeChecker::new();
        let result = checker.check_source_file(&tree);

        assert!(result.is_ok());
    }

    #[test]
    fn test_type_check_signal_with_init() {
        let source = r#"
            impl Counter {
                signal counter: nat[8] = 0
            }
        "#;

        let tree = parse(source);
        let mut checker = TypeChecker::new();
        let result = checker.check_source_file(&tree);

        // Should pass - 0 can fit in nat[8]
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_check_width_mismatch() {
        let source = r#"
            impl Test {
                signal a: bit[8] = 0
                signal b: bit[16] = 0

                a = b
            }
        "#;

        let tree = parse(source);
        let mut checker = TypeChecker::new();
        let result = checker.check_source_file(&tree);

        // Should fail - width mismatch
        assert!(result.is_err());
    }
}