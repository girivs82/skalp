//! Type checking for SKALP
//!
//! This module implements the type checker that operates on the syntax tree

use crate::syntax::{SyntaxKind, SyntaxNode, SyntaxNodeExt};
use crate::types::{
    self, EnumType, EnumVariant, ResetPolarity, StructField, StructType, Type, TypeEnv, TypeError,
    TypeInference, TypeScheme, Width,
};
use indexmap::IndexMap;

/// Type checker for SKALP
pub struct TypeChecker {
    /// Type environment
    env: TypeEnv,

    /// Type inference engine
    inference: TypeInference,

    /// Collected errors
    errors: Vec<TypeCheckError>,

    /// Node type cache
    node_types: IndexMap<usize, Type>,

    /// Entity port definitions (entity name -> port environment)
    entity_ports: IndexMap<String, Vec<(String, Type)>>,
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
            node_types: IndexMap::new(),
            entity_ports: IndexMap::new(),
        }
    }

    /// Type check a source file
    pub fn check_source_file(&mut self, root: &SyntaxNode) -> Result<(), Vec<TypeCheckError>> {
        assert_eq!(root.kind(), SyntaxKind::SourceFile);

        // Check all top-level items
        for child in root.children() {
            match child.kind() {
                SyntaxKind::StructDecl => self.check_struct_decl(&child),
                SyntaxKind::EnumDecl => self.check_enum_decl(&child),
                SyntaxKind::EntityDecl => self.check_entity_decl(&child),
                SyntaxKind::ImplBlock => self.check_impl_block(&child),
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

    /// Check struct declaration
    fn check_struct_decl(&mut self, node: &SyntaxNode) {
        // Extract struct name
        let struct_name = self.get_struct_name(node);

        // Create struct type
        let mut fields = Vec::new();

        // Check each field
        if let Some(field_list) = node.first_child_of_kind(SyntaxKind::StructFieldList) {
            for field_node in field_list.children_of_kind(SyntaxKind::StructField) {
                let field_name = self.get_field_name(&field_node);
                let field_type = self.extract_type(&field_node);

                // Check for duplicate field names
                if fields.iter().any(|f: &StructField| f.name == field_name) {
                    self.errors.push(TypeCheckError {
                        error: TypeError::DuplicateDefinition(format!("field {}", field_name)),
                        location: None,
                    });
                } else {
                    fields.push(StructField {
                        name: field_name,
                        field_type,
                    });
                }
            }
        }

        let struct_type = StructType {
            name: struct_name.clone(),
            fields,
        };

        // Add struct type to environment
        self.env
            .define_type(struct_name.clone(), Type::Struct(struct_type));

        // Cache the type
        self.node_types.insert(
            node.text_range().start().into(),
            Type::Struct(StructType {
                name: struct_name,
                fields: vec![], // Simplified for caching
            }),
        );
    }

    /// Check enum declaration
    fn check_enum_decl(&mut self, node: &SyntaxNode) {
        // Extract enum name
        let enum_name = self.get_enum_name(node);

        // Check base type if specified
        let base_type =
            if let Some(base_type_node) = node.first_child_of_kind(SyntaxKind::TypeAnnotation) {
                self.extract_type(&base_type_node)
            } else {
                Type::Nat(Width::Fixed(32)) // Default to 32-bit nat
            };

        // Create enum type
        let mut variants = Vec::new();

        // Check each variant
        if let Some(variant_list) = node.first_child_of_kind(SyntaxKind::EnumVariantList) {
            for variant_node in variant_list.children_of_kind(SyntaxKind::EnumVariant) {
                let variant_name = self.get_variant_name(&variant_node);

                // Check for duplicate variant names
                if variants
                    .iter()
                    .any(|v: &EnumVariant| v.name == variant_name)
                {
                    self.errors.push(TypeCheckError {
                        error: TypeError::DuplicateDefinition(format!("variant {}", variant_name)),
                        location: None,
                    });
                } else {
                    // Check for payload type if specified
                    let payload = variant_node
                        .first_child_of_kind(SyntaxKind::TypeAnnotation)
                        .map(|payload_node| self.extract_type(&payload_node));

                    variants.push(EnumVariant {
                        name: variant_name,
                        payload,
                    });
                }
            }
        }

        let enum_type = EnumType {
            name: enum_name.clone(),
            variants,
        };

        // Add enum type to environment
        self.env
            .define_type(enum_name.clone(), Type::Enum(enum_type));

        // Cache the type
        self.node_types.insert(
            node.text_range().start().into(),
            Type::Enum(EnumType {
                name: enum_name,
                variants: vec![], // Simplified for caching
            }),
        );
    }

    /// Check entity declaration
    fn check_entity_decl(&mut self, node: &SyntaxNode) {
        // Extract entity name
        let name = self.get_entity_name(node);

        // Create a new scope for the entity
        let parent_env = self.env.clone();
        self.env = TypeEnv::child(parent_env.clone());

        // Collect ports for this entity
        let mut entity_ports = Vec::new();

        // Check ports
        if let Some(port_list) = node.first_child_of_kind(SyntaxKind::PortList) {
            for port in port_list.children_of_kind(SyntaxKind::PortDecl) {
                self.check_port_decl(&port);

                // Store port information
                let port_name = self.get_port_name(&port);
                let port_type = self.extract_type(&port);
                entity_ports.push((port_name, port_type));
            }
        }

        // Store the entity's ports for use in impl blocks
        self.entity_ports.insert(name.clone(), entity_ports.clone());

        // Restore parent environment
        self.env = parent_env.clone();

        // Register entity type
        let entity_name = self.get_entity_name(node);
        let mut entity_type = types::EntityType {
            name: entity_name.clone(),
            inputs: Vec::new(),
            outputs: Vec::new(),
            inouts: Vec::new(),
            type_params: Vec::new(),
            width_params: Vec::new(),
        };

        // Collect ports
        for port in &entity_ports {
            let port_type = types::PortType {
                name: port.0.clone(),
                port_type: port.1.clone(),
            };

            // Determine port direction (simplified - would need proper AST analysis)
            // For now, assume all are inputs
            entity_type.inputs.push(port_type);
        }

        // Register entity type in environment
        let entity_type = types::Type::Entity(entity_type);
        self.env.define_type(entity_name.clone(), entity_type);
    }

    /// Check implementation block
    fn check_impl_block(&mut self, node: &SyntaxNode) {
        // Extract target entity name
        let entity_name = self.get_impl_entity_name(node);

        // Create a new scope for the implementation
        let parent_env = self.env.clone();
        self.env = TypeEnv::child(parent_env.clone());

        // Add entity ports to the implementation scope
        if let Some(ports) = self.entity_ports.get(&entity_name).cloned() {
            for (port_name, port_type) in ports {
                let scheme = TypeScheme {
                    type_params: vec![],
                    width_params: vec![],
                    ty: port_type,
                };
                self.env.bind(port_name, scheme);
            }
        }

        // Check implementation items
        for child in node.children() {
            match child.kind() {
                SyntaxKind::SignalDecl => self.check_signal_decl(&child),
                SyntaxKind::VariableDecl => self.check_variable_decl(&child),
                SyntaxKind::ConstantDecl => self.check_constant_decl(&child),
                SyntaxKind::EventBlock => self.check_event_block(&child),
                SyntaxKind::AssignmentStmt => self.check_assignment_stmt(&child),
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
        self.node_types
            .insert(node.text_range().start().into(), port_type);
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

        self.node_types
            .insert(node.text_range().start().into(), signal_type);
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

        self.node_types
            .insert(node.text_range().start().into(), var_type);
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

        self.node_types
            .insert(node.text_range().start().into(), const_type);
    }

    /// Check event block
    fn check_event_block(&mut self, node: &SyntaxNode) {
        // Check event triggers
        if let Some(trigger_list) = node.first_child_of_kind(SyntaxKind::EventTriggerList) {
            for trigger in trigger_list.children_of_kind(SyntaxKind::EventTrigger) {
                self.check_event_trigger(&trigger);
            }
        }

        // Check block body
        if let Some(block) = node.first_child_of_kind(SyntaxKind::BlockStmt) {
            self.check_block_stmt(&block);
        }
    }

    /// Check event trigger
    fn check_event_trigger(&mut self, node: &SyntaxNode) {
        // Get signal name from identifier token
        if let Some(ident) = node.first_token_of_kind(SyntaxKind::Ident) {
            let signal_name = ident.text();

            // Look up signal type
            if let Some(scheme) = self.env.lookup(signal_name) {
                let signal_type = &scheme.ty;

                // Check that it's a clock or event type
                if !signal_type.is_clock() && signal_type != &Type::Event {
                    self.errors.push(TypeCheckError {
                        error: TypeError::NotClock(Box::new(signal_type.clone())),
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
                SyntaxKind::AssignmentStmt => self.check_assignment_stmt(&child),
                SyntaxKind::IfStmt => self.check_if_stmt(&child),
                SyntaxKind::MatchStmt => self.check_match_stmt(&child),
                SyntaxKind::BlockStmt => self.check_block_stmt(&child),
                _ => {}
            }
        }

        // Restore parent scope
        self.env = parent_env;
    }

    /// Check if statement
    fn check_if_stmt(&mut self, node: &SyntaxNode) {
        // Check condition is boolean/bit
        if let Some(cond_expr) = node.children().find(|n| {
            matches!(
                n.kind(),
                SyntaxKind::LiteralExpr
                    | SyntaxKind::IdentExpr
                    | SyntaxKind::BinaryExpr
                    | SyntaxKind::UnaryExpr
            )
        }) {
            let cond_type = self.check_expression(&cond_expr);

            // Condition should be bit[1], boolean, or reset (resets are implicitly boolean)
            match &cond_type {
                Type::Bit(Width::Fixed(1)) => {} // OK
                Type::Reset { .. } => {}         // OK - resets can be used as booleans
                Type::Clock { .. } => {} // OK - clocks can be used as booleans (for gated logic)
                _ => {
                    // Try to coerce to bit[1]
                    self.inference
                        .add_constraint(crate::types::TypeConstraint::Equal(
                            cond_type,
                            Type::Bit(Width::Fixed(1)),
                        ));
                }
            }
        }

        // Check then and else blocks
        for block in node.children_of_kind(SyntaxKind::BlockStmt) {
            self.check_block_stmt(&block);
        }
    }

    /// Check match statement
    fn check_match_stmt(&mut self, node: &SyntaxNode) {
        // Get expression being matched
        let expr_type = if let Some(expr_node) = node.children().find(|n| {
            matches!(
                n.kind(),
                SyntaxKind::LiteralExpr
                    | SyntaxKind::IdentExpr
                    | SyntaxKind::BinaryExpr
                    | SyntaxKind::UnaryExpr
            )
        }) {
            self.check_expression(&expr_node)
        } else {
            Type::Error
        };

        // Check match arms
        if let Some(arm_list) = node
            .children()
            .find(|n| n.kind() == SyntaxKind::MatchArmList)
        {
            for arm in arm_list
                .children()
                .filter(|n| n.kind() == SyntaxKind::MatchArm)
            {
                self.check_match_arm(&arm, &expr_type);
            }
        }
    }

    /// Check match arm
    fn check_match_arm(&mut self, node: &SyntaxNode, match_expr_type: &Type) {
        // Check pattern matches the expression type
        if let Some(pattern_node) = node.children().find(|n| {
            matches!(
                n.kind(),
                SyntaxKind::LiteralPattern
                    | SyntaxKind::IdentPattern
                    | SyntaxKind::WildcardPattern
                    | SyntaxKind::TuplePattern
            )
        }) {
            let pattern_type = self.check_pattern(&pattern_node);

            // Ensure pattern type matches expression type
            if let Err(e) = self
                .inference
                .check_assignment(match_expr_type, &pattern_type)
            {
                self.errors.push(TypeCheckError {
                    error: e,
                    location: None,
                });
            }
        }

        // Check arm body in new scope (for pattern variables)
        let parent_env = self.env.clone();
        self.env = TypeEnv::child(parent_env.clone());

        // Add pattern variables to scope
        if let Some(pattern_node) = node.children().find(|n| {
            matches!(
                n.kind(),
                SyntaxKind::LiteralPattern
                    | SyntaxKind::IdentPattern
                    | SyntaxKind::WildcardPattern
                    | SyntaxKind::TuplePattern
            )
        }) {
            self.bind_pattern_variables(&pattern_node, match_expr_type);
        }

        // Check statements in arm
        for child in node.children() {
            match child.kind() {
                SyntaxKind::AssignmentStmt => self.check_assignment_stmt(&child),
                SyntaxKind::IfStmt => self.check_if_stmt(&child),
                SyntaxKind::MatchStmt => self.check_match_stmt(&child),
                SyntaxKind::BlockStmt => self.check_block_stmt(&child),
                _ => {}
            }
        }

        // Restore parent scope
        self.env = parent_env;
    }

    /// Bind pattern variables to their types in the current scope
    fn bind_pattern_variables(&mut self, pattern: &SyntaxNode, matched_type: &Type) {
        match pattern.kind() {
            SyntaxKind::IdentPattern => {
                // Get the identifier name
                if let Some(ident_token) = pattern.first_child_or_token() {
                    if let Some(token) = ident_token.as_token() {
                        if token.kind() == SyntaxKind::Ident {
                            let var_name = token.text().to_string();
                            // Bind the variable to the matched type
                            let scheme = TypeScheme {
                                type_params: vec![],
                                width_params: vec![],
                                ty: matched_type.clone(),
                            };
                            self.env.bind(var_name, scheme);
                        }
                    }
                }
            }
            SyntaxKind::TuplePattern => {
                // For tuple patterns, recursively bind each element
                // Would need to destructure the matched type as well
                // For now, skip complex patterns
            }
            SyntaxKind::WildcardPattern | SyntaxKind::LiteralPattern => {
                // These don't bind any variables
            }
            _ => {}
        }
    }

    /// Check pattern and return its type
    fn check_pattern(&mut self, node: &SyntaxNode) -> Type {
        match node.kind() {
            SyntaxKind::LiteralPattern => {
                // Find the literal and check its type
                if let Some(literal_child) = node.children().find(|n| {
                    matches!(
                        n.kind(),
                        SyntaxKind::IntLiteral
                            | SyntaxKind::BinLiteral
                            | SyntaxKind::HexLiteral
                            | SyntaxKind::StringLiteral
                    )
                }) {
                    self.check_literal_expr(&literal_child)
                } else {
                    Type::Error
                }
            }
            SyntaxKind::IdentPattern => {
                // For now, assume it matches any type (pattern variable)
                // In a real implementation, we'd need to track pattern variables
                self.inference.fresh_type_var()
            }
            SyntaxKind::WildcardPattern => {
                // Wildcard matches any type
                self.inference.fresh_type_var()
            }
            SyntaxKind::TuplePattern => {
                // TODO: Implement tuple pattern checking
                Type::Error
            }
            _ => Type::Error,
        }
    }

    /// Check expression and return its type
    fn check_expression(&mut self, node: &SyntaxNode) -> Type {
        match node.kind() {
            SyntaxKind::LiteralExpr => self.check_literal_expr(node),
            SyntaxKind::IdentExpr => self.check_ident_expr(node),
            SyntaxKind::BinaryExpr => self.check_binary_expr(node),
            SyntaxKind::UnaryExpr => self.check_unary_expr(node),
            SyntaxKind::ParenExpr => {
                // Check inner expression
                if let Some(inner) = node.children().find(|n| {
                    matches!(
                        n.kind(),
                        SyntaxKind::LiteralExpr
                            | SyntaxKind::IdentExpr
                            | SyntaxKind::BinaryExpr
                            | SyntaxKind::UnaryExpr
                    )
                }) {
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
                SyntaxKind::IntLiteral => {
                    // Parse literal value
                    let text = token.as_token().map(|t| t.text()).unwrap_or("");
                    let value = text.parse::<u64>().unwrap_or(0);
                    self.inference.infer_int_literal(value)
                }
                SyntaxKind::BinLiteral => {
                    let text = token.as_token().map(|t| t.text()).unwrap_or("");
                    let value = parse_bin_literal(text);
                    self.inference.infer_bin_literal(value, None)
                }
                SyntaxKind::HexLiteral => {
                    let text = token.as_token().map(|t| t.text()).unwrap_or("");
                    let value = parse_hex_literal(text);
                    self.inference.infer_hex_literal(value, None)
                }
                SyntaxKind::StringLiteral => {
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
        if let Some(ident) = node.first_token_of_kind(SyntaxKind::Ident) {
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
            let op_token = node
                .children_with_tokens()
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
            let op_token = node
                .children_with_tokens()
                .filter_map(|elem| elem.into_token())
                .find(|t| {
                    matches!(
                        t.kind(),
                        SyntaxKind::Bang | SyntaxKind::Tilde | SyntaxKind::Minus
                    )
                });

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
            SyntaxKind::Plus | SyntaxKind::Minus | SyntaxKind::Star | SyntaxKind::Slash => {
                // Both operands must be numeric and same type
                self.inference
                    .add_constraint(crate::types::TypeConstraint::Numeric(left.clone()));
                self.inference
                    .add_constraint(crate::types::TypeConstraint::Numeric(right.clone()));
                self.inference
                    .add_constraint(crate::types::TypeConstraint::Equal(
                        left.clone(),
                        right.clone(),
                    ));
                left // Result has same type as operands
            }

            // Comparison operators
            SyntaxKind::Eq
            | SyntaxKind::Neq
            | SyntaxKind::Lt
            | SyntaxKind::Gt
            | SyntaxKind::Le
            | SyntaxKind::Ge => {
                // Operands must match, result is bit[1]
                self.inference
                    .add_constraint(crate::types::TypeConstraint::Equal(left, right));
                Type::Bit(Width::Fixed(1))
            }

            // Logical operators
            SyntaxKind::AmpAmp | SyntaxKind::PipePipe => {
                // Operands must be bit[1], result is bit[1]
                self.inference
                    .add_constraint(crate::types::TypeConstraint::Equal(
                        left,
                        Type::Bit(Width::Fixed(1)),
                    ));
                self.inference
                    .add_constraint(crate::types::TypeConstraint::Equal(
                        right,
                        Type::Bit(Width::Fixed(1)),
                    ));
                Type::Bit(Width::Fixed(1))
            }

            // Bitwise operators
            SyntaxKind::Amp | SyntaxKind::Pipe | SyntaxKind::Caret => {
                // Both operands must be same bit/logic type
                self.inference
                    .add_constraint(crate::types::TypeConstraint::Equal(
                        left.clone(),
                        right.clone(),
                    ));
                left
            }

            // Shift operators
            SyntaxKind::Shl | SyntaxKind::Shr => {
                // Left operand determines result type
                // Right operand should be unsigned
                self.inference
                    .add_constraint(crate::types::TypeConstraint::Numeric(right));
                left
            }

            _ => Type::Error,
        }
    }

    /// Check unary operator type rules
    fn check_unary_op(&mut self, op: SyntaxKind, operand: Type) -> Type {
        match op {
            SyntaxKind::Bang => {
                // Logical not - operand must be bit[1]
                self.inference
                    .add_constraint(crate::types::TypeConstraint::Equal(
                        operand,
                        Type::Bit(Width::Fixed(1)),
                    ));
                Type::Bit(Width::Fixed(1))
            }

            SyntaxKind::Tilde => {
                // Bitwise not - preserves type
                self.inference
                    .add_constraint(crate::types::TypeConstraint::Numeric(operand.clone()));
                operand
            }

            SyntaxKind::Minus => {
                // Arithmetic negation
                self.inference
                    .add_constraint(crate::types::TypeConstraint::Numeric(operand.clone()));
                operand
            }

            _ => Type::Error,
        }
    }

    // === Helper methods ===

    /// Extract type from a type annotation node
    fn extract_type(&mut self, node: &SyntaxNode) -> Type {
        if let Some(type_node) = node.first_child_of_kind(SyntaxKind::TypeAnnotation) {
            self.parse_type_annotation(&type_node)
        } else {
            // Try to find type in children
            for child in node.children() {
                if matches!(
                    child.kind(),
                    SyntaxKind::BitType
                        | SyntaxKind::LogicType
                        | SyntaxKind::IntType
                        | SyntaxKind::NatType
                        | SyntaxKind::ClockType
                        | SyntaxKind::ResetType
                ) {
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
            SyntaxKind::BitType => {
                let width = self.extract_width(node);
                Type::Bit(width)
            }
            SyntaxKind::BoolType => Type::Bool,
            SyntaxKind::LogicType => {
                let width = self.extract_width(node);
                Type::Logic(width)
            }
            SyntaxKind::IntType => {
                let width = self.extract_width(node);
                Type::Int(width)
            }
            SyntaxKind::NatType => {
                let width = self.extract_width(node);
                Type::Nat(width)
            }
            SyntaxKind::ClockType => Type::Clock(None),
            SyntaxKind::ResetType => Type::Reset(ResetPolarity::ActiveHigh),
            SyntaxKind::CustomType => {
                // Get type name
                if let Some(ident) = node.first_token_of_kind(SyntaxKind::Ident) {
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
            _ => Type::Unknown,
        }
    }

    /// Extract width specification
    fn extract_width(&mut self, node: &SyntaxNode) -> Width {
        if let Some(width_spec) = node.first_child_of_kind(SyntaxKind::WidthSpec) {
            if let Some(literal) = width_spec.first_token_of_kind(SyntaxKind::IntLiteral) {
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
            if found_assign
                && matches!(
                    child.kind(),
                    SyntaxKind::LiteralExpr
                        | SyntaxKind::IdentExpr
                        | SyntaxKind::BinaryExpr
                        | SyntaxKind::UnaryExpr
                )
            {
                return Some(child);
            }
            if child.kind() == SyntaxKind::Assign {
                found_assign = true;
            }
        }
        None
    }

    /// Get entity name
    fn get_entity_name(&self, node: &SyntaxNode) -> String {
        node.children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|t| t.kind() == SyntaxKind::Ident)
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

    /// Extract struct name from struct declaration
    fn get_struct_name(&self, node: &SyntaxNode) -> String {
        // Look for the first Ident token after the struct keyword
        node.children_with_tokens()
            .filter_map(|child| {
                if let Some(token) = child.as_token() {
                    if token.kind() == SyntaxKind::Ident {
                        return Some(token.text().to_string());
                    }
                }
                None
            })
            .next()
            .unwrap_or_else(|| "unknown_struct".to_string())
    }

    /// Extract enum name from enum declaration
    fn get_enum_name(&self, node: &SyntaxNode) -> String {
        // Look for the first Ident token after the enum keyword
        node.children_with_tokens()
            .filter_map(|child| {
                if let Some(token) = child.as_token() {
                    if token.kind() == SyntaxKind::Ident {
                        return Some(token.text().to_string());
                    }
                }
                None
            })
            .next()
            .unwrap_or_else(|| "unknown_enum".to_string())
    }

    /// Extract field name from struct field
    fn get_field_name(&self, node: &SyntaxNode) -> String {
        // Look for the first Ident token in the struct field
        node.children_with_tokens()
            .filter_map(|child| {
                if let Some(token) = child.as_token() {
                    if token.kind() == SyntaxKind::Ident {
                        return Some(token.text().to_string());
                    }
                }
                None
            })
            .next()
            .unwrap_or_else(|| "unknown_field".to_string())
    }

    /// Extract variant name from enum variant
    fn get_variant_name(&self, node: &SyntaxNode) -> String {
        // Look for the first Ident token in the enum variant
        node.children_with_tokens()
            .filter_map(|child| {
                if let Some(token) = child.as_token() {
                    if token.kind() == SyntaxKind::Ident {
                        return Some(token.text().to_string());
                    }
                }
                None
            })
            .next()
            .unwrap_or_else(|| "unknown_variant".to_string())
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

/// Additional helper methods for type checking
impl TypeChecker {
    /// Check function call expression
    fn check_call_expr(&mut self, node: &SyntaxNode) -> Type {
        // Get function name
        let func_name = if let Some(ident) = node.first_token_of_kind(SyntaxKind::Ident) {
            ident.text().to_string()
        } else {
            return Type::Error;
        };

        // Check if it's a built-in function
        match func_name.as_str() {
            "resize" => {
                // resize(value, new_width) - changes width of bit vector
                Type::Bit(Width::Unknown) // Result type depends on arguments
            }
            "concat" => {
                // concat(a, b) - concatenates bit vectors
                Type::Bit(Width::Unknown) // Result width is sum of operand widths
            }
            "replicate" => {
                // replicate(value, count) - replicates a value
                Type::Bit(Width::Unknown)
            }
            _ => {
                // Look up user-defined function
                if let Some(scheme) = self.env.lookup(&func_name) {
                    scheme.ty.clone()
                } else {
                    self.errors.push(TypeCheckError {
                        error: TypeError::UndefinedVariable(func_name),
                        location: None,
                    });
                    Type::Error
                }
            }
        }
    }

    /// Check array/struct field access
    fn check_field_access(&mut self, base_type: &Type, field_name: &str) -> Type {
        match base_type {
            Type::Struct(struct_type) => {
                // Look up field in struct
                for field in &struct_type.fields {
                    if field.name == field_name {
                        return field.field_type.clone();
                    }
                }
                self.errors.push(TypeCheckError {
                    error: TypeError::UndefinedVariable(format!("field {}", field_name)),
                    location: None,
                });
                Type::Error
            }
            Type::Enum(enum_type) => {
                // Look up variant in enum
                for variant in &enum_type.variants {
                    if variant.name == field_name {
                        // Return the enum type itself for variant access
                        return Type::Enum(enum_type.clone());
                    }
                }
                self.errors.push(TypeCheckError {
                    error: TypeError::UndefinedVariable(format!("variant {}", field_name)),
                    location: None,
                });
                Type::Error
            }
            Type::Protocol(protocol_name) => {
                // For protocols, field access might reference signals
                // This would need protocol definitions to be tracked
                Type::Unknown // Placeholder
            }
            _ => {
                self.errors.push(TypeCheckError {
                    error: TypeError::TypeMismatch {
                        expected: Box::new(Type::Struct(StructType {
                            name: "struct or enum".to_string(),
                            fields: vec![],
                        })),
                        found: Box::new(base_type.clone()),
                    },
                    location: None,
                });
                Type::Error
            }
        }
    }

    /// Check array indexing
    fn check_array_index(&mut self, base_type: &Type, index_type: &Type) -> Type {
        // Index must be an integer type
        if !index_type.is_numeric() {
            self.errors.push(TypeCheckError {
                error: TypeError::NotNumeric(Box::new(index_type.clone())),
                location: None,
            });
        }

        match base_type {
            Type::Array { element_type, .. } => (**element_type).clone(),
            Type::Bit(_) | Type::Logic(_) => {
                // Bit indexing returns a single bit
                Type::Bit(Width::Fixed(1))
            }
            _ => {
                self.errors.push(TypeCheckError {
                    error: TypeError::TypeMismatch {
                        expected: Box::new(Type::Array {
                            element_type: Box::new(Type::Unknown),
                            size: 0,
                        }),
                        found: Box::new(base_type.clone()),
                    },
                    location: None,
                });
                Type::Error
            }
        }
    }

    /// Check bit range access [high:low]
    fn check_bit_range(&mut self, base_type: &Type, high_type: &Type, low_type: &Type) -> Type {
        // Range indices must be integers
        if !high_type.is_numeric() || !low_type.is_numeric() {
            self.errors.push(TypeCheckError {
                error: TypeError::NotNumeric(Box::new(if !high_type.is_numeric() {
                    high_type.clone()
                } else {
                    low_type.clone()
                })),
                location: None,
            });
        }

        match base_type {
            Type::Bit(width) | Type::Logic(width) => {
                // Calculate result width (would need constant evaluation in real implementation)
                match width {
                    Width::Fixed(_) => Type::Bit(Width::Unknown), // Would need to calculate actual width
                    _ => Type::Bit(Width::Unknown),
                }
            }
            _ => {
                self.errors.push(TypeCheckError {
                    error: TypeError::TypeMismatch {
                        expected: Box::new(Type::Bit(Width::Unknown)),
                        found: Box::new(base_type.clone()),
                    },
                    location: None,
                });
                Type::Error
            }
        }
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

        // Width mismatches are now allowed (with implicit truncation/extension)
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_check_match_statement() {
        let source = r#"
            impl Test {
                signal value: nat[4] = 0
                signal output: bit[1] = 0

                match value {
                    0 => output = 0
                    _ => output = 1
                }
            }
        "#;

        let tree = parse(source);
        let mut checker = TypeChecker::new();
        let result = checker.check_source_file(&tree);

        // Should pass basic type checking
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_check_binary_operations() {
        let source = r#"
            impl Test {
                signal a: nat[8] = 10
                signal b: nat[8] = 20
                signal result: nat[8] = 0

                result = a + b
            }
        "#;

        let tree = parse(source);
        let mut checker = TypeChecker::new();
        let result = checker.check_source_file(&tree);

        assert!(result.is_ok());
    }

    #[test]
    fn test_type_check_clock_entity() {
        let source = r#"
            entity ClockTest {
                in clk: clock
                out data: bit[8]
            }
        "#;

        let tree = parse(source);
        let mut checker = TypeChecker::new();
        let result = checker.check_source_file(&tree);

        assert!(result.is_ok());
    }
}
