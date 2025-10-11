//! HIR builder for SKALP
//!
//! Transforms the syntax tree into HIR (High-level Intermediate Representation)

use crate::hir::*;
use crate::lexer::{parse_binary, parse_float, parse_hex};
use crate::syntax::{SyntaxKind, SyntaxNode, SyntaxNodeExt};
use crate::typeck::TypeChecker;
use std::collections::HashMap;

/// Maximum recursion depth to prevent stack overflow
const MAX_RECURSION_DEPTH: usize = 100;

/// List of builtin/intrinsic functions that don't require symbol resolution
const BUILTIN_FUNCTIONS: &[&str] = &["clog2", "pow2", "min", "max", "abs"];

/// HIR builder context
pub struct HirBuilderContext {
    /// Next IDs for various HIR elements
    next_entity_id: u32,
    next_port_id: u32,
    next_signal_id: u32,
    next_variable_id: u32,
    next_constant_id: u32,
    next_function_id: u32,
    next_block_id: u32,
    next_assignment_id: u32,
    next_protocol_id: u32,
    next_intent_id: u32,
    next_requirement_id: u32,
    next_instance_id: u32,
    next_clock_domain_id: u32,
    next_assertion_id: u32,
    next_property_id: u32,
    next_cover_id: u32,
    next_import_id: u32,

    /// Symbol table for name resolution
    symbols: SymbolTable,

    /// Type checker for type information
    type_checker: TypeChecker,

    /// Errors collected during HIR building
    errors: Vec<HirError>,

    /// Built entities (for accessing ports during impl building)
    built_entities: HashMap<String, HirEntity>,

    /// Recursion depth counter to prevent infinite loops
    recursion_depth: usize,
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

    /// User-defined types (struct, enum, union)
    user_types: HashMap<String, HirType>,

    /// Current scope for nested lookups
    scopes: Vec<HashMap<String, SymbolId>>,
}

/// Generic symbol ID
#[derive(Debug, Clone)]
enum SymbolId {
    Entity(EntityId),
    Port(PortId),
    Signal(SignalId),
    Variable(VariableId),
    Constant(ConstantId),
    Function(FunctionId),
    GenericParam(String), // Generic parameter name (e.g., WIDTH)
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
            next_function_id: 0,
            next_block_id: 0,
            next_assignment_id: 0,
            next_protocol_id: 0,
            next_intent_id: 0,
            next_requirement_id: 0,
            next_instance_id: 0,
            next_clock_domain_id: 0,
            next_assertion_id: 0,
            next_property_id: 0,
            next_cover_id: 0,
            next_import_id: 0,
            symbols: SymbolTable::new(),
            type_checker: TypeChecker::new(),
            errors: Vec::new(),
            built_entities: HashMap::new(),
            recursion_depth: 0,
        }
    }

    /// Build HIR from syntax tree
    pub fn build(&mut self, root: &SyntaxNode) -> Result<Hir, Vec<HirError>> {
        // Type checking is temporarily disabled during HIR building to avoid conflicts
        // with the existing type resolution system. The type checker enhancements
        // are available for standalone use.
        //
        // if let Err(type_errors) = self.type_checker.check_source_file(root) {
        //     for err in type_errors {
        //         self.errors.push(HirError {
        //             message: format!("Type error: {}", err.error),
        //             location: None,
        //         });
        //     }
        // }

        let mut hir = Hir::new("main".to_string());

        // Build HIR from source file
        for child in root.children() {
            match child.kind() {
                SyntaxKind::EntityDecl => {
                    if let Some(entity) = self.build_entity(&child) {
                        // Store entity for later access by implementations
                        self.built_entities
                            .insert(entity.name.clone(), entity.clone());
                        hir.entities.push(entity);
                    }
                }
                SyntaxKind::ImplBlock => {
                    if let Some(implementation) = self.build_implementation(&child) {
                        hir.implementations.push(implementation);
                    }
                }
                SyntaxKind::ProtocolDecl => {
                    if let Some(protocol) = self.build_protocol(&child) {
                        hir.protocols.push(protocol);
                    }
                }
                SyntaxKind::TraitDef => {
                    if let Some(trait_def) = self.build_trait_def(&child) {
                        hir.trait_definitions.push(trait_def);
                    }
                }
                SyntaxKind::TraitImpl => {
                    if let Some(trait_impl) = self.build_trait_impl(&child) {
                        hir.trait_implementations.push(trait_impl);
                    }
                }
                SyntaxKind::IntentDecl => {
                    if let Some(intent) = self.build_intent(&child) {
                        hir.intents.push(intent);
                    }
                }
                SyntaxKind::RequirementDecl => {
                    if let Some(requirement) = self.build_requirement(&child) {
                        hir.requirements.push(requirement);
                    }
                }
                SyntaxKind::StructDecl => {
                    if let Some(struct_type) = self.build_struct_type(&child) {
                        // Store struct type for later reference
                        self.symbols
                            .user_types
                            .insert(struct_type.name.clone(), HirType::Struct(struct_type));
                    }
                }
                SyntaxKind::EnumDecl => {
                    if let Some(enum_type) = self.build_enum_type(&child) {
                        // Store enum type for later reference
                        self.symbols
                            .user_types
                            .insert(enum_type.name.clone(), HirType::Enum(Box::new(enum_type)));
                    }
                }
                SyntaxKind::UnionDecl => {
                    if let Some(union_type) = self.build_union_type(&child) {
                        // Store union type for later reference
                        self.symbols
                            .user_types
                            .insert(union_type.name.clone(), HirType::Union(union_type));
                    }
                }
                SyntaxKind::FunctionDecl => {
                    if let Some(function) = self.build_function(&child) {
                        // Store function in HIR for later use
                        hir.functions.push(function);
                    }
                }
                SyntaxKind::UseDecl => {
                    if let Some(import) = self.build_import(&child) {
                        hir.imports.push(import);
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

        // Build generics first - this must come before ports since ports may reference generic parameters
        let generics =
            if let Some(generic_list) = node.first_child_of_kind(SyntaxKind::GenericParamList) {
                let params = self.parse_generic_params(&generic_list);

                // Register generic parameters in symbol table so ports can reference them
                for param in &params {
                    self.symbols
                        .add_to_scope(&param.name, SymbolId::GenericParam(param.name.clone()));
                }

                // Register clock domain lifetimes in symbol table
                for param in &params {
                    if let HirGenericType::ClockDomain = param.param_type {
                        let domain_id = ClockDomainId(self.symbols.clock_domains.len() as u32);
                        self.symbols
                            .clock_domains
                            .insert(param.name.clone(), domain_id);
                    }
                }

                params
            } else {
                Vec::new()
            };

        // Build ports after generics so they can reference generic clock domains
        let mut ports = Vec::new();
        if let Some(port_list) = node.first_child_of_kind(SyntaxKind::PortList) {
            for port_node in port_list.children_of_kind(SyntaxKind::PortDecl) {
                if let Some(port) = self.build_port(&port_node) {
                    ports.push(port);
                }
            }
        }

        // Build clock domains - look for lifetime-like annotations
        let mut clock_domains = Vec::new();

        // For now, extract any clock domain from port types
        // In the future, this should parse explicit clock domain parameters
        let mut seen_domains = std::collections::HashSet::new();
        for port in &ports {
            if let HirType::Clock(Some(domain_id)) | HirType::Reset(Some(domain_id)) =
                &port.port_type
            {
                if seen_domains.insert(*domain_id) {
                    clock_domains.push(HirClockDomain {
                        id: *domain_id,
                        name: format!("clk_{}", domain_id.0),
                    });
                }
            }
        }

        Some(HirEntity {
            id,
            name,
            visibility: crate::hir::HirVisibility::Private,
            ports,
            generics,
            clock_domains,
        })
    }

    /// Build port from syntax node
    fn build_port(&mut self, node: &SyntaxNode) -> Option<HirPort> {
        let id = self.next_port_id();
        let name = self.extract_name(node)?;

        // Get direction
        let direction = if let Some(dir_node) = node.first_child_of_kind(SyntaxKind::PortDirection)
        {
            self.extract_port_direction(&dir_node)
        } else {
            HirPortDirection::Input // Default
        };

        // Get type - look for TypeAnnotation first
        let port_type =
            if let Some(type_node) = node.first_child_of_kind(SyntaxKind::TypeAnnotation) {
                self.extract_hir_type(&type_node)
            } else {
                self.extract_hir_type(node)
            };

        // Extract physical constraints if present
        let physical_constraints = node
            .first_child_of_kind(SyntaxKind::PhysicalConstraintBlock)
            .and_then(|constraint_node| self.extract_physical_constraints(&constraint_node));

        // Register in symbol table
        self.symbols.ports.insert(name.clone(), id);
        // Also add to general scope for lookup
        self.symbols.add_to_scope(&name, SymbolId::Port(id));

        Some(HirPort {
            id,
            name,
            direction,
            port_type,
            physical_constraints,
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

        // Get the built entity and add its ports and generic parameters to the current scope
        let (ports, generics) = if let Some(built_entity) = self.built_entities.get(&entity_name) {
            (built_entity.ports.clone(), built_entity.generics.clone())
        } else {
            (Vec::new(), Vec::new())
        };

        for port in &ports {
            self.symbols
                .add_to_scope(&port.name, SymbolId::Port(port.id));
        }

        // Add generic parameters to scope so they can be referenced in the impl
        for generic in &generics {
            self.symbols
                .add_to_scope(&generic.name, SymbolId::GenericParam(generic.name.clone()));
        }

        let mut signals = Vec::new();
        let mut variables = Vec::new();
        let mut constants = Vec::new();
        let mut functions = Vec::new();
        let mut event_blocks = Vec::new();
        let mut assignments = Vec::new();

        // Build implementation items
        for child in node.children() {
            match child.kind() {
                SyntaxKind::SignalDecl => {
                    if let Some(signal) = self.build_signal(&child) {
                        signals.push(signal);
                    }
                }
                SyntaxKind::VariableDecl => {
                    if let Some(variable) = self.build_variable(&child) {
                        variables.push(variable);
                    }
                }
                SyntaxKind::ConstantDecl => {
                    if let Some(constant) = self.build_constant(&child) {
                        constants.push(constant);
                    }
                }
                SyntaxKind::FunctionDecl => {
                    if let Some(function) = self.build_function(&child) {
                        functions.push(function);
                    }
                }
                SyntaxKind::EventBlock => {
                    if let Some(block) = self.build_event_block(&child) {
                        event_blocks.push(block);
                    }
                }
                SyntaxKind::AssignmentStmt => {
                    if let Some(assignment) =
                        self.build_assignment(&child, HirAssignmentType::Combinational)
                    {
                        assignments.push(assignment);
                    }
                }
                SyntaxKind::LetStmt => {
                    // Let bindings in impl blocks are treated as variables with combinational assignments
                    if let Some(let_stmt) = self.build_let_statement(&child) {
                        // Create a variable for the let binding
                        let variable = HirVariable {
                            id: let_stmt.id,
                            name: let_stmt.name.clone(),
                            var_type: let_stmt.var_type.clone(),
                            initial_value: None,
                        };
                        variables.push(variable);

                        // Create a combinational assignment for the initialization
                        let assignment = HirAssignment {
                            id: self.next_assignment_id(),
                            lhs: HirLValue::Variable(let_stmt.id),
                            assignment_type: HirAssignmentType::Combinational,
                            rhs: let_stmt.value,
                        };
                        assignments.push(assignment);
                    }
                }
                _ => {}
            }
        }

        // Build instances BEFORE exiting scope so signals are still accessible
        let mut instances = Vec::new();
        for child in node.children() {
            if child.kind() == SyntaxKind::InstanceDecl {
                if let Some(instance) = self.build_instance(&child) {
                    instances.push(instance);
                }
            }
        }

        // Exit scope
        self.symbols.exit_scope();

        let mut implementation = HirImplementation {
            entity,
            signals,
            variables,
            constants,
            functions,
            event_blocks,
            assignments,
            covergroups: Vec::new(),   // TODO: Implement covergroup building
            formal_blocks: Vec::new(), // TODO: Implement formal verification building
            instances,
        };

        // Infer clock domains for signals based on event block assignments
        self.infer_clock_domains(&mut implementation);

        Some(implementation)
    }

    /// Build instance declaration
    fn build_instance(&mut self, node: &SyntaxNode) -> Option<HirInstance> {
        let id = InstanceId(0); // TODO: Add instance ID generation

        // Get instance name (first identifier after 'let')
        let tokens: Vec<_> = node
            .children_with_tokens()
            .filter_map(|element| element.into_token())
            .collect();

        let name = tokens
            .iter()
            .find(|token| token.kind() == SyntaxKind::Ident)
            .map(|t| t.text().to_string())?;

        // Get entity name (second identifier, after '=')
        let entity_name = tokens
            .iter()
            .filter(|token| token.kind() == SyntaxKind::Ident)
            .nth(1)
            .map(|t| t.text().to_string())?;

        // Look up entity ID
        let entity = *self.symbols.entities.get(&entity_name)?;

        // Extract generic arguments if present
        let mut generic_args = Vec::new();
        if let Some(arg_list) = node.first_child_of_kind(SyntaxKind::ArgList) {
            for arg_node in arg_list.children() {
                // Each generic argument is an expression (could be a literal, const, or type)
                if let Some(expr) = self.build_expression(&arg_node) {
                    generic_args.push(expr);
                }
            }
        }

        // Build connections
        let mut connections = Vec::new();
        if let Some(conn_list) = node.first_child_of_kind(SyntaxKind::ConnectionList) {
            for conn_node in conn_list.children_of_kind(SyntaxKind::Connection) {
                if let Some(connection) = self.build_connection(&conn_node) {
                    connections.push(connection);
                }
            }
        }

        Some(HirInstance {
            id,
            name,
            entity,
            generic_args,
            connections,
        })
    }

    /// Build connection
    fn build_connection(&mut self, node: &SyntaxNode) -> Option<HirConnection> {
        // Get port name (first identifier)
        let port_name = node
            .first_token_of_kind(SyntaxKind::Ident)
            .map(|t| t.text().to_string())?;

        // Get the expression (everything after the colon)
        let expr_node = node.children().find(|n| {
            matches!(
                n.kind(),
                SyntaxKind::LiteralExpr
                    | SyntaxKind::IdentExpr
                    | SyntaxKind::BinaryExpr
                    | SyntaxKind::UnaryExpr
                    | SyntaxKind::FieldExpr
                    | SyntaxKind::IndexExpr
            )
        })?;

        let expr = self.build_expression(&expr_node)?;

        Some(HirConnection {
            port: port_name,
            expr,
        })
    }

    /// Build signal declaration
    fn build_signal(&mut self, node: &SyntaxNode) -> Option<HirSignal> {
        let id = self.next_signal_id();
        let name = self.extract_name(node)?;

        // Get type - look for TypeAnnotation first
        let signal_type =
            if let Some(type_node) = node.first_child_of_kind(SyntaxKind::TypeAnnotation) {
                self.extract_hir_type(&type_node)
            } else {
                self.extract_hir_type(node)
            };

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
            clock_domain: None, // Will be inferred during clock domain analysis
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

    /// Build function declaration
    fn build_function(&mut self, node: &SyntaxNode) -> Option<HirFunction> {
        let id = self.next_function_id();

        // Check if this is a const function (has ConstKw token before FnKw)
        let is_const = node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .any(|t| t.kind() == SyntaxKind::ConstKw);

        // Extract function name
        let name = node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|t| t.kind() == SyntaxKind::Ident)
            .map(|t| t.text().to_string())?;

        // Build parameters
        let mut params = Vec::new();
        if let Some(param_list) = node.first_child_of_kind(SyntaxKind::ParameterList) {
            for param_node in param_list.children_of_kind(SyntaxKind::Parameter) {
                if let Some(param) = self.build_parameter(&param_node) {
                    params.push(param);
                }
            }
        }

        // Extract optional return type
        let return_type = node
            .children()
            .skip_while(|n| !matches!(n.kind(), SyntaxKind::Arrow))
            .skip(1) // Skip the Arrow itself
            .find(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::TypeAnnotation
                        | SyntaxKind::BitType
                        | SyntaxKind::LogicType
                        | SyntaxKind::IntType
                        | SyntaxKind::NatType
                        | SyntaxKind::CustomType
                        | SyntaxKind::ArrayType
                )
            })
            .map(|n| self.extract_hir_type(&n));

        // Build function body (statements from block)
        let body = if let Some(block) = node.first_child_of_kind(SyntaxKind::BlockStmt) {
            self.build_statements(&block)
        } else {
            Vec::new()
        };

        // Register function in symbol table
        self.symbols.add_to_scope(&name, SymbolId::Function(id));

        Some(HirFunction {
            id,
            is_const,
            name,
            params,
            return_type,
            body,
        })
    }

    /// Build event block
    fn build_event_block(&mut self, node: &SyntaxNode) -> Option<HirEventBlock> {
        let id = self.next_block_id();

        // Build triggers
        let mut triggers = Vec::new();
        if let Some(trigger_list) = node.first_child_of_kind(SyntaxKind::EventTriggerList) {
            for trigger_node in trigger_list.children_of_kind(SyntaxKind::EventTrigger) {
                if let Some(trigger) = self.build_event_trigger(&trigger_node) {
                    triggers.push(trigger);
                }
            }
        }

        // Build statements
        let mut statements = Vec::new();
        if let Some(block) = node.first_child_of_kind(SyntaxKind::BlockStmt) {
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
        let signal_name = node
            .first_token_of_kind(SyntaxKind::Ident)
            .map(|t| t.text().to_string())?;

        // Look up signal ID - check if it's a port or signal
        let signal = self.symbols.lookup(&signal_name).and_then(|id| match id {
            SymbolId::Signal(s) => Some(HirEventSignal::Signal(*s)),
            SymbolId::Port(p) => Some(HirEventSignal::Port(*p)),
            _ => None,
        })?;

        // Get edge type
        let edge = if let Some(edge_node) = node.first_child_of_kind(SyntaxKind::EdgeType) {
            self.extract_edge_type(&edge_node)
        } else {
            HirEdgeType::Rising // Default
        };

        Some(HirEventTrigger { signal, edge })
    }

    /// Build import (use statement) from syntax node
    fn build_import(&mut self, node: &SyntaxNode) -> Option<HirImport> {
        let id = self.next_import_id();

        // For now, visibility is always private (pub use not yet implemented)
        let visibility = HirVisibility::Private;

        // Build the import path
        let path = self.build_import_path(node)?;

        Some(HirImport {
            id,
            visibility,
            path,
        })
    }

    /// Build import path from use declaration
    fn build_import_path(&mut self, node: &SyntaxNode) -> Option<HirImportPath> {
        // Find the UsePath child
        let use_path = node.first_child_of_kind(SyntaxKind::UsePath)?;

        // Collect path segments
        let mut segments = Vec::new();
        for child in use_path.children() {
            if child.kind() == SyntaxKind::Ident {
                if let Some(token) = child.first_token() {
                    segments.push(token.text().to_string());
                }
            }
        }

        if segments.is_empty() {
            return None;
        }

        // Check for rename (as keyword)
        // TODO: Implement renamed imports

        // Check for glob (*)
        // TODO: Implement glob imports

        // Check for nested imports ({...})
        // TODO: Implement nested imports

        // For now, just support simple paths
        Some(HirImportPath::Simple { segments })
    }

    /// Build statements from block
    fn build_statements(&mut self, node: &SyntaxNode) -> Vec<HirStatement> {
        // Check recursion depth to prevent infinite loops
        if self.recursion_depth >= MAX_RECURSION_DEPTH {
            self.errors.push(HirError {
                message: format!(
                    "Maximum recursion depth ({}) exceeded while building statements",
                    MAX_RECURSION_DEPTH
                ),
                location: None,
            });
            return Vec::new();
        }

        self.recursion_depth += 1;
        let mut statements = Vec::new();

        for child in node.children() {
            match child.kind() {
                SyntaxKind::AssignmentStmt => {
                    // Determine assignment type from operator
                    let assignment_type = self.determine_assignment_type(&child);
                    if let Some(assignment) = self.build_assignment(&child, assignment_type) {
                        statements.push(HirStatement::Assignment(assignment));
                    }
                }
                SyntaxKind::IfStmt => {
                    if let Some(if_stmt) = self.build_if_statement(&child) {
                        statements.push(HirStatement::If(if_stmt));
                    }
                }
                SyntaxKind::MatchStmt => {
                    if let Some(match_stmt) = self.build_match_statement(&child) {
                        statements.push(HirStatement::Match(match_stmt));
                    }
                }
                SyntaxKind::FlowStmt => {
                    if let Some(flow_stmt) = self.build_flow_statement(&child) {
                        statements.push(HirStatement::Flow(flow_stmt));
                    }
                }
                SyntaxKind::AssertStmt => {
                    if let Some(assert_stmt) = self.build_assert_statement(&child) {
                        statements.push(HirStatement::Assert(assert_stmt));
                    }
                }
                SyntaxKind::PropertyStmt => {
                    if let Some(property_stmt) = self.build_property_statement(&child) {
                        statements.push(HirStatement::Property(property_stmt));
                    }
                }
                SyntaxKind::CoverStmt => {
                    if let Some(cover_stmt) = self.build_cover_statement(&child) {
                        statements.push(HirStatement::Cover(cover_stmt));
                    }
                }
                SyntaxKind::LetStmt => {
                    if let Some(let_stmt) = self.build_let_statement(&child) {
                        statements.push(HirStatement::Let(let_stmt));
                    }
                }
                SyntaxKind::BlockStmt => {
                    let block_stmts = self.build_statements(&child);
                    statements.push(HirStatement::Block(block_stmts));
                }
                SyntaxKind::ExprStmt => {
                    // Expression statement (for implicit returns in functions)
                    if let Some(stmt) = self.build_statement(&child) {
                        statements.push(stmt);
                    }
                }
                SyntaxKind::ReturnStmt => {
                    // Return statement
                    if let Some(stmt) = self.build_statement(&child) {
                        statements.push(stmt);
                    }
                }
                _ => {}
            }
        }

        self.recursion_depth -= 1;
        statements
    }

    /// Build single statement
    fn build_statement(&mut self, node: &SyntaxNode) -> Option<HirStatement> {
        // Check recursion depth to prevent infinite loops
        if self.recursion_depth >= MAX_RECURSION_DEPTH {
            self.errors.push(HirError {
                message: format!(
                    "Maximum recursion depth ({}) exceeded while building statement",
                    MAX_RECURSION_DEPTH
                ),
                location: None,
            });
            return None;
        }

        self.recursion_depth += 1;
        let result = match node.kind() {
            SyntaxKind::AssignmentStmt => {
                let assignment_type = self.determine_assignment_type(node);
                self.build_assignment(node, assignment_type)
                    .map(HirStatement::Assignment)
            }
            SyntaxKind::IfStmt => self.build_if_statement(node).map(HirStatement::If),
            SyntaxKind::MatchStmt => self.build_match_statement(node).map(HirStatement::Match),
            SyntaxKind::FlowStmt => self.build_flow_statement(node).map(HirStatement::Flow),
            SyntaxKind::AssertStmt => self.build_assert_statement(node).map(HirStatement::Assert),
            SyntaxKind::PropertyStmt => self
                .build_property_statement(node)
                .map(HirStatement::Property),
            SyntaxKind::CoverStmt => self.build_cover_statement(node).map(HirStatement::Cover),
            SyntaxKind::LetStmt => self.build_let_statement(node).map(HirStatement::Let),
            SyntaxKind::ReturnStmt => {
                // Return statement: return [expr]
                let expr = node
                    .children()
                    .find(|n| {
                        matches!(
                            n.kind(),
                            SyntaxKind::LiteralExpr
                                | SyntaxKind::IdentExpr
                                | SyntaxKind::BinaryExpr
                                | SyntaxKind::UnaryExpr
                                | SyntaxKind::FieldExpr
                                | SyntaxKind::IndexExpr
                                | SyntaxKind::PathExpr
                                | SyntaxKind::ParenExpr
                                | SyntaxKind::IfExpr
                                | SyntaxKind::MatchExpr
                                | SyntaxKind::CallExpr
                                | SyntaxKind::ArrayLiteral
                        )
                    })
                    .and_then(|n| self.build_expression(&n));
                Some(HirStatement::Return(expr))
            }
            SyntaxKind::ExprStmt => {
                // Expression statement (implicit return at end of function)
                // Check for function call pattern: IdentExpr followed by CallExpr
                let children: Vec<_> = node.children().collect();

                // Look for IdentExpr + CallExpr pattern (postfix function call)
                for i in 0..children.len().saturating_sub(1) {
                    if children[i].kind() == SyntaxKind::IdentExpr
                        && children[i + 1].kind() == SyntaxKind::CallExpr
                    {
                        // This is a function call - get function name from IdentExpr
                        if let Some(func_name) = children[i]
                            .first_token_of_kind(SyntaxKind::Ident)
                            .map(|t| t.text().to_string())
                        {
                            // Parse arguments from CallExpr
                            // Note: Parser may create intermediate nodes (e.g., IdentExpr) that are
                            // part of larger expressions (e.g., BinaryExpr). We need to identify
                            // the top-level argument expressions.
                            let call_children: Vec<_> = children[i + 1].children().collect();

                            let mut args = Vec::new();
                            let mut skip_next = false;

                            for (idx, arg_child) in call_children.iter().enumerate() {
                                if skip_next {
                                    skip_next = false;
                                    continue;
                                }

                                // Check for nested function call: IdentExpr followed by CallExpr
                                if arg_child.kind() == SyntaxKind::IdentExpr
                                    && idx + 1 < call_children.len()
                                {
                                    let next_kind = call_children[idx + 1].kind();

                                    if next_kind == SyntaxKind::CallExpr {
                                        // This is a nested function call
                                        if let Some(nested_func_name) = arg_child
                                            .first_token_of_kind(SyntaxKind::Ident)
                                            .map(|t| t.text().to_string())
                                        {
                                            // Parse arguments from the nested CallExpr
                                            let mut nested_args = Vec::new();
                                            for nested_arg_child in
                                                call_children[idx + 1].children()
                                            {
                                                if let Some(nested_arg_expr) =
                                                    self.build_expression(&nested_arg_child)
                                                {
                                                    nested_args.push(nested_arg_expr);
                                                }
                                            }

                                            let nested_call = HirExpression::Call(HirCallExpr {
                                                function: nested_func_name,
                                                args: nested_args,
                                            });
                                            args.push(nested_call);
                                            skip_next = true; // Skip the CallExpr in next iteration
                                            continue;
                                        }
                                    } else if matches!(
                                        next_kind,
                                        SyntaxKind::BinaryExpr | SyntaxKind::UnaryExpr
                                    ) {
                                        // IdentExpr is part of a binary/unary expression
                                        continue;
                                    }
                                }

                                if let Some(arg_expr) = self.build_expression(arg_child) {
                                    args.push(arg_expr);
                                }
                            }
                            let call_expr = HirExpression::Call(HirCallExpr {
                                function: func_name,
                                args,
                            });
                            return Some(HirStatement::Expression(call_expr));
                        }
                    }
                }

                // Fall back to original logic if no function call pattern found
                // Search children in reverse to prioritize complex expressions like BinaryExpr
                // over simple ones like IdentExpr (which may not be in scope yet)
                let expr = children
                    .into_iter()
                    .rev()
                    .find(|n| {
                        matches!(
                            n.kind(),
                            SyntaxKind::BinaryExpr
                                | SyntaxKind::UnaryExpr
                                | SyntaxKind::CallExpr
                                | SyntaxKind::IfExpr
                                | SyntaxKind::MatchExpr
                                | SyntaxKind::FieldExpr
                                | SyntaxKind::IndexExpr
                                | SyntaxKind::ParenExpr
                                | SyntaxKind::ArrayLiteral
                                | SyntaxKind::PathExpr
                                | SyntaxKind::LiteralExpr
                                | SyntaxKind::IdentExpr
                        )
                    })
                    .and_then(|n| self.build_expression(&n))?;
                Some(HirStatement::Expression(expr))
            }
            SyntaxKind::BlockStmt => {
                let block_stmts = self.build_statements(node);
                Some(HirStatement::Block(block_stmts))
            }
            _ => None,
        };
        self.recursion_depth -= 1;
        result
    }

    /// Build assignment
    fn build_assignment(
        &mut self,
        node: &SyntaxNode,
        assignment_type: HirAssignmentType,
    ) -> Option<HirAssignment> {
        let id = self.next_assignment_id();

        // Get LHS and RHS expressions
        let exprs: Vec<_> = node
            .children()
            .filter(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::LiteralExpr
                        | SyntaxKind::IdentExpr
                        | SyntaxKind::BinaryExpr
                        | SyntaxKind::UnaryExpr
                        | SyntaxKind::FieldExpr
                        | SyntaxKind::IndexExpr
                        | SyntaxKind::PathExpr
                        | SyntaxKind::ParenExpr
                        | SyntaxKind::IfExpr
                        | SyntaxKind::MatchExpr
                        | SyntaxKind::CallExpr
                        | SyntaxKind::ArrayLiteral
                )
            })
            .collect();

        if exprs.len() < 2 {
            return None;
        }

        // Handle array indexing/slicing: memory[index] <= value or state[3:1] <= value
        // Parser splits this into: IdentExpr("memory"), IndexExpr("[index]"), IdentExpr("value")
        let lhs = if exprs.len() == 3 && exprs[1].kind() == SyntaxKind::IndexExpr {
            // Combine base and index/range to form indexed lvalue
            let base = self.build_lvalue(&exprs[0])?;
            let index_node = &exprs[1];

            // Check if it's a range (has colon token) or single index
            let has_colon = index_node.children_with_tokens().any(|element| {
                element
                    .as_token()
                    .is_some_and(|t| t.kind() == SyntaxKind::Colon)
            });

            if has_colon {
                // Range indexing: signal[high:low]
                let index_exprs: Vec<_> = index_node
                    .children()
                    .filter(|n| {
                        matches!(
                            n.kind(),
                            SyntaxKind::IdentExpr
                                | SyntaxKind::LiteralExpr
                                | SyntaxKind::BinaryExpr
                        )
                    })
                    .collect();

                if index_exprs.len() >= 2 {
                    let high_expr = self.build_expression(&index_exprs[0])?;
                    let low_expr = self.build_expression(&index_exprs[1])?;
                    HirLValue::Range(Box::new(base), high_expr, low_expr)
                } else {
                    return None;
                }
            } else {
                // Single index: signal[index]
                let index_expr = index_node
                    .children()
                    .find(|n| {
                        matches!(
                            n.kind(),
                            SyntaxKind::IdentExpr
                                | SyntaxKind::LiteralExpr
                                | SyntaxKind::BinaryExpr
                        )
                    })
                    .and_then(|n| self.build_expression(&n))?;

                HirLValue::Index(Box::new(base), index_expr)
            }
        } else {
            self.build_lvalue(&exprs[0])?
        };

        // Handle RHS - if there are multiple expressions, we need to combine them
        let rhs = if exprs.len() == 2 {
            // Simple case: LHS op RHS
            self.build_expression(&exprs[1])?
        } else if exprs.len() == 3 {
            let second_expr = &exprs[1];
            let third_expr = &exprs[2];

            // Check if this is an indexed assignment (base[index] <= value)
            // In this case, exprs[1] is the IndexExpr which we already consumed in LHS
            if second_expr.kind() == SyntaxKind::IndexExpr {
                // The RHS is exprs[2]
                self.build_expression(&exprs[2])?
            } else if third_expr.kind() == SyntaxKind::FieldExpr {
                // Case: LHS op BASE_EXPR FIELD_EXPR
                // This happens with field access like "dst_addr <= header.dst"
                // EXPR1 is the base (header), EXPR2 is the field access (.dst)
                self.build_field_access_from_parts(&exprs[1], &exprs[2])?
            } else if third_expr.kind() == SyntaxKind::BinaryExpr {
                // Case: LHS op EXPR1 BINARYEXPR
                // This happens when we have something like "counter <= counter + 1"
                // We need to combine EXPR1 and BINARYEXPR
                let first_expr = self.build_expression(&exprs[1])?;
                let binary_expr = &exprs[2];

                // The binary expr should have the operator and second operand
                // We need to create a new binary expression with first_expr as left operand
                self.combine_expressions_with_binary(first_expr, binary_expr)?
            } else if third_expr.kind() == SyntaxKind::IndexExpr {
                // Case: LHS op BASE_EXPR INDEX_EXPR
                // This happens with range/index access like "decode_opcode <= fetch_instruction[15:12]"
                // EXPR1 is the base (fetch_instruction), EXPR2 is the index expression ([15:12])
                self.build_index_access_from_parts(&exprs[1], &exprs[2])?
            } else {
                // Fallback to last expression
                self.build_expression(exprs.last().unwrap())?
            }
        } else {
            // For 4+ expressions, we have a chain like: LHS = BASE FIELD BINARY or LHS = BASE INDEX FIELD BINARY
            // Build the RHS by chaining the expressions left-to-right
            self.build_chained_rhs_expression(&exprs[1..])?
        };

        Some(HirAssignment {
            id,
            lhs,
            assignment_type,
            rhs,
        })
    }

    /// Combine a left expression with a binary expression node
    fn combine_expressions_with_binary(
        &mut self,
        left_expr: HirExpression,
        binary_node: &SyntaxNode,
    ) -> Option<HirExpression> {
        // Get the operator and right operand from the binary node
        let binary_children: Vec<_> = binary_node
            .children()
            .filter(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::LiteralExpr
                        | SyntaxKind::IdentExpr
                        | SyntaxKind::BinaryExpr
                        | SyntaxKind::UnaryExpr
                        | SyntaxKind::FieldExpr
                        | SyntaxKind::IndexExpr
                        | SyntaxKind::PathExpr
                        | SyntaxKind::ParenExpr
                        | SyntaxKind::CallExpr
                        | SyntaxKind::ArrayLiteral
                )
            })
            .collect();

        if binary_children.is_empty() {
            return None;
        }

        // The binary node may contain multiple expression children if the right operand has postfix operations
        // For example: "a.x + b.x" parses as BinaryExpr containing [b (IdentExpr), .x (FieldExpr)]
        // We need to chain these into a single right operand: b.x
        let right_expr = if binary_children.len() == 1 {
            self.build_expression(&binary_children[0])?
        } else {
            // Chain the expressions
            self.build_chained_rhs_expression(&binary_children)?
        };

        // Get the operator token from the binary node
        let op_token = binary_node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|t| t.kind().is_operator())?;

        let op = self.token_to_binary_op(op_token.kind())?;

        Some(HirExpression::Binary(HirBinaryExpr {
            left: Box::new(left_expr),
            op,
            right: Box::new(right_expr),
        }))
    }

    /// Build chained RHS expression from multiple sibling nodes
    /// Parser creates flat structure: BASE FIELD BINARY etc
    /// We need to chain them: (BASE.FIELD) + ...
    fn build_chained_rhs_expression(&mut self, rhs_exprs: &[SyntaxNode]) -> Option<HirExpression> {
        if rhs_exprs.is_empty() {
            return None;
        }

        // Start with the first expression
        let mut result = self.build_expression(&rhs_exprs[0])?;

        // Chain the remaining expressions
        for current_node in rhs_exprs.iter().skip(1) {
            match current_node.kind() {
                SyntaxKind::FieldExpr => {
                    // Combine result with field access
                    let field_name = current_node
                        .children_with_tokens()
                        .filter_map(|elem| elem.into_token())
                        .find(|t| t.kind() == SyntaxKind::Ident)
                        .map(|t| t.text().to_string())?;

                    result = HirExpression::FieldAccess {
                        base: Box::new(result),
                        field: field_name,
                    };
                }
                SyntaxKind::IndexExpr => {
                    // Combine result with index access
                    result = self.build_index_with_base(result, current_node)?;
                }
                SyntaxKind::BinaryExpr => {
                    // Combine result with binary operation
                    result = self.combine_expressions_with_binary(result, current_node)?;
                }
                SyntaxKind::CallExpr => {
                    // Function call - just build it normally
                    // The parser should have created proper structure
                    result = self.build_expression(current_node)?;
                }
                _ => {
                    // Unknown expression type - skip it
                    continue;
                }
            }
        }

        Some(result)
    }

    /// Build index expression with a given base expression
    fn build_index_with_base(
        &mut self,
        base: HirExpression,
        index_node: &SyntaxNode,
    ) -> Option<HirExpression> {
        // Check if it's a range (has colon token) or single index
        let has_colon = index_node.children_with_tokens().any(|element| {
            element
                .as_token()
                .is_some_and(|t| t.kind() == SyntaxKind::Colon)
        });

        if has_colon {
            // Range indexing: base[high:low]
            let index_exprs: Vec<_> = index_node
                .children()
                .filter(|n| {
                    matches!(
                        n.kind(),
                        SyntaxKind::IdentExpr | SyntaxKind::LiteralExpr | SyntaxKind::BinaryExpr
                    )
                })
                .collect();

            if index_exprs.len() >= 2 {
                let high_expr = self.build_expression(&index_exprs[0])?;
                let low_expr = self.build_expression(&index_exprs[1])?;
                Some(HirExpression::Range(
                    Box::new(base),
                    Box::new(high_expr),
                    Box::new(low_expr),
                ))
            } else {
                None
            }
        } else {
            // Single index: base[index]
            let index_expr = index_node
                .children()
                .find(|n| {
                    matches!(
                        n.kind(),
                        SyntaxKind::IdentExpr | SyntaxKind::LiteralExpr | SyntaxKind::BinaryExpr
                    )
                })
                .and_then(|n| self.build_expression(&n))?;

            Some(HirExpression::Index(Box::new(base), Box::new(index_expr)))
        }
    }

    /// Build if statement
    fn build_if_statement(&mut self, node: &SyntaxNode) -> Option<HirIfStatement> {
        // Get condition expression - prefer complex expressions (Binary, Unary, Paren) over simple ones (Ident, Literal)
        // This avoids selecting the left operand of a binary expression instead of the full expression
        let condition = node
            .children()
            .find(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::BinaryExpr | SyntaxKind::UnaryExpr | SyntaxKind::ParenExpr
                )
            })
            .or_else(|| {
                node.children()
                    .find(|n| matches!(n.kind(), SyntaxKind::IdentExpr | SyntaxKind::LiteralExpr))
            })
            .and_then(|n| self.build_expression(&n))?;

        // Get then and else blocks
        let blocks: Vec<_> = node.children_of_kind(SyntaxKind::BlockStmt);

        let then_statements = if !blocks.is_empty() {
            self.build_statements(&blocks[0])
        } else {
            Vec::new()
        };

        // For else block, check if it's a regular block or an else-if (nested IfStmt)
        let else_statements = if blocks.len() > 1 {
            // Regular else with block: `else { ... }`
            Some(self.build_statements(&blocks[1]))
        } else {
            // Check for else-if pattern: `else if ...`
            // The parser creates: IfStmt -> Condition -> BlockStmt -> [IfStmt | BlockStmt]
            // So we look for an IfStmt child that appears after the first BlockStmt
            let mut found_then_block = false;
            let nested_if = node.children().find_map(|child| {
                if child.kind() == SyntaxKind::BlockStmt {
                    found_then_block = true;
                    None
                } else if found_then_block && child.kind() == SyntaxKind::IfStmt {
                    Some(child)
                } else {
                    None
                }
            });

            nested_if.and_then(|nested_if_node| {
                // Found a nested if statement - this is an else-if
                self.build_if_statement(&nested_if_node)
                    .map(|if_stmt| vec![HirStatement::If(if_stmt)])
            })
        };

        Some(HirIfStatement {
            condition,
            then_statements,
            else_statements,
        })
    }

    /// Build let statement
    fn build_let_statement(&mut self, node: &SyntaxNode) -> Option<HirLetStatement> {
        // Extract variable name using children_with_tokens to access tokens
        let name = node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|t| t.kind() == SyntaxKind::Ident)
            .map(|t| t.text().to_string())?;

        // Extract optional type annotation
        let explicit_type = node
            .children()
            .find(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::TypeAnnotation
                        | SyntaxKind::BitType
                        | SyntaxKind::LogicType
                        | SyntaxKind::IntType
                        | SyntaxKind::NatType
                        | SyntaxKind::CustomType
                        | SyntaxKind::ArrayType
                )
            })
            .map(|n| self.extract_hir_type(&n));

        // Extract initializer expression
        let value = node
            .children()
            .find(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::LiteralExpr
                        | SyntaxKind::IdentExpr
                        | SyntaxKind::BinaryExpr
                        | SyntaxKind::UnaryExpr
                        | SyntaxKind::CallExpr
                        | SyntaxKind::IndexExpr
                        | SyntaxKind::FieldExpr
                        | SyntaxKind::ParenExpr
                )
            })
            .and_then(|n| self.build_expression(&n))?;

        // Allocate a variable ID for this let binding
        let id = self.next_variable_id();

        // Use explicit type if provided, otherwise we'll need type inference
        // For now, use a placeholder type that will be inferred later
        let var_type = explicit_type.unwrap_or({
            // Default to Nat(32) as a placeholder - type inference will refine this
            HirType::Nat(32)
        });

        // Register in symbol table so the variable can be resolved
        self.symbols.variables.insert(name.clone(), id);
        self.symbols.add_to_scope(&name, SymbolId::Variable(id));

        Some(HirLetStatement {
            id,
            name,
            var_type,
            value,
        })
    }

    /// Build match statement
    fn build_match_statement(&mut self, node: &SyntaxNode) -> Option<HirMatchStatement> {
        // Get expression being matched
        let expr = node
            .children()
            .find(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::LiteralExpr
                        | SyntaxKind::IdentExpr
                        | SyntaxKind::BinaryExpr
                        | SyntaxKind::UnaryExpr
                )
            })
            .and_then(|n| self.build_expression(&n))?;

        // Build match arms from MATCH_ARM_LIST
        let mut arms = Vec::new();
        if let Some(arm_list) = node
            .children()
            .find(|n| n.kind() == SyntaxKind::MatchArmList)
        {
            for arm_node in arm_list
                .children()
                .filter(|n| n.kind() == SyntaxKind::MatchArm)
            {
                if let Some(arm) = self.build_match_arm(&arm_node) {
                    arms.push(arm);
                }
            }
        }

        Some(HirMatchStatement { expr, arms })
    }

    /// Build match arm
    fn build_match_arm(&mut self, node: &SyntaxNode) -> Option<HirMatchArm> {
        // Find pattern
        let pattern = node
            .children()
            .find(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::LiteralPattern
                        | SyntaxKind::IdentPattern
                        | SyntaxKind::WildcardPattern
                        | SyntaxKind::TuplePattern
                )
            })
            .and_then(|n| self.build_pattern(&n))?;

        // Find optional guard
        let guard = node
            .children()
            .find(|n| n.kind() == SyntaxKind::MatchGuard)
            .and_then(|guard_node| {
                // Find the expression inside the guard
                guard_node
                    .children()
                    .find(|n| {
                        matches!(
                            n.kind(),
                            SyntaxKind::LiteralExpr
                                | SyntaxKind::IdentExpr
                                | SyntaxKind::BinaryExpr
                                | SyntaxKind::UnaryExpr
                                | SyntaxKind::FieldExpr
                                | SyntaxKind::IndexExpr
                                | SyntaxKind::PathExpr
                                | SyntaxKind::ParenExpr
                                | SyntaxKind::IfExpr
                                | SyntaxKind::MatchExpr
                                | SyntaxKind::CallExpr
                                | SyntaxKind::ArrayLiteral
                        )
                    })
                    .and_then(|n| self.build_expression(&n))
            });

        // Find statements (after the arrow)
        let mut statements = Vec::new();
        for child in node.children() {
            match child.kind() {
                SyntaxKind::AssignmentStmt
                | SyntaxKind::IfStmt
                | SyntaxKind::MatchStmt
                | SyntaxKind::BlockStmt => {
                    if let Some(stmt) = self.build_statement(&child) {
                        statements.push(stmt);
                    }
                }
                _ => {}
            }
        }

        Some(HirMatchArm {
            pattern,
            guard,
            statements,
        })
    }

    /// Build flow statement
    fn build_flow_statement(&mut self, node: &SyntaxNode) -> Option<HirFlowStatement> {
        // Find the flow pipeline
        let pipeline = node
            .children()
            .find(|n| n.kind() == SyntaxKind::FlowPipeline)
            .and_then(|n| self.build_flow_pipeline(&n))?;

        Some(HirFlowStatement { pipeline })
    }

    /// Build flow pipeline
    fn build_flow_pipeline(&mut self, node: &SyntaxNode) -> Option<HirFlowPipeline> {
        // Find all pipeline stages
        let stage_nodes: Vec<_> = node
            .children()
            .filter(|n| n.kind() == SyntaxKind::PipelineStage)
            .collect();

        if stage_nodes.is_empty() {
            return None;
        }

        // Build the first stage
        let start = self.build_pipeline_stage(&stage_nodes[0])?;

        // Build remaining stages
        let mut stages = Vec::new();
        for stage_node in stage_nodes.iter().skip(1) {
            if let Some(stage) = self.build_pipeline_stage(stage_node) {
                stages.push(stage);
            }
        }

        Some(HirFlowPipeline { start, stages })
    }

    /// Build pipeline stage
    fn build_pipeline_stage(&mut self, node: &SyntaxNode) -> Option<HirPipelineStage> {
        for child in node.children() {
            match child.kind() {
                SyntaxKind::BlockStmt => {
                    let statements = self.build_statements(&child);
                    return Some(HirPipelineStage::Block(statements));
                }
                SyntaxKind::LiteralExpr
                | SyntaxKind::IdentExpr
                | SyntaxKind::BinaryExpr
                | SyntaxKind::UnaryExpr
                | SyntaxKind::FieldExpr
                | SyntaxKind::IndexExpr => {
                    if let Some(expr) = self.build_expression(&child) {
                        return Some(HirPipelineStage::Expression(expr));
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Build assert statement
    fn build_assert_statement(&mut self, node: &SyntaxNode) -> Option<HirAssertStatement> {
        let id = self.next_assertion_id();

        // Find the condition expression (first expression)
        let expressions: Vec<_> = node
            .children()
            .filter(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::LiteralExpr
                        | SyntaxKind::IdentExpr
                        | SyntaxKind::BinaryExpr
                        | SyntaxKind::UnaryExpr
                        | SyntaxKind::FieldExpr
                        | SyntaxKind::IndexExpr
                        | SyntaxKind::ParenExpr
                        | SyntaxKind::CallExpr
                        | SyntaxKind::IfExpr
                        | SyntaxKind::MatchExpr
                        | SyntaxKind::PathExpr
                )
            })
            .collect();

        let condition = if let Some(expr_node) = expressions.first() {
            self.build_expression(expr_node)?
        } else {
            // Default to a literal false if no condition is found
            HirExpression::Literal(HirLiteral::Boolean(false))
        };

        // Check for optional message (second expression)
        let message = if expressions.len() > 1 {
            if let Some(expr) = self.build_expression(&expressions[1]) {
                match expr {
                    HirExpression::Literal(HirLiteral::String(s)) => Some(s),
                    _ => None,
                }
            } else {
                None
            }
        } else {
            None
        };

        Some(HirAssertStatement {
            id,
            condition,
            message,
            severity: HirAssertionSeverity::Error, // Default severity
        })
    }

    /// Build property statement
    fn build_property_statement(&mut self, node: &SyntaxNode) -> Option<HirPropertyStatement> {
        let id = self.next_property_id();

        // Extract property name
        let name = node
            .children_with_tokens()
            .filter_map(|element| {
                element
                    .as_token()
                    .filter(|token| token.kind() == SyntaxKind::Ident)
                    .map(|token| token.text().to_string())
            })
            .next()
            .unwrap_or_else(|| "unnamed_property".to_string());

        // For now, parse the property body as a simple expression
        // This will be expanded to handle full SVA syntax in future milestones
        let property = if let Some(expr_node) = node.children().find(|n| {
            matches!(
                n.kind(),
                SyntaxKind::LiteralExpr
                    | SyntaxKind::IdentExpr
                    | SyntaxKind::BinaryExpr
                    | SyntaxKind::UnaryExpr
                    | SyntaxKind::FieldExpr
                    | SyntaxKind::IndexExpr
            )
        }) {
            let expr = self.build_expression(&expr_node)?;
            HirProperty::Expression(expr)
        } else {
            // Default to a literal true property
            HirProperty::Expression(HirExpression::Literal(HirLiteral::Boolean(true)))
        };

        Some(HirPropertyStatement {
            id,
            name,
            property,
            clock: None, // Will be parsed in future milestones
            disable: None,
        })
    }

    /// Build cover statement
    fn build_cover_statement(&mut self, node: &SyntaxNode) -> Option<HirCoverStatement> {
        let id = self.next_cover_id();

        // Find the property expression
        let property = if let Some(expr_node) = node.children().find(|n| {
            matches!(
                n.kind(),
                SyntaxKind::LiteralExpr
                    | SyntaxKind::IdentExpr
                    | SyntaxKind::BinaryExpr
                    | SyntaxKind::UnaryExpr
                    | SyntaxKind::FieldExpr
                    | SyntaxKind::IndexExpr
            )
        }) {
            let expr = self.build_expression(&expr_node)?;
            HirProperty::Expression(expr)
        } else {
            // Default to a literal true property
            HirProperty::Expression(HirExpression::Literal(HirLiteral::Boolean(true)))
        };

        Some(HirCoverStatement {
            id,
            property,
            name: None, // Optional cover name
        })
    }

    /// Build pattern
    #[allow(clippy::only_used_in_recursion, clippy::comparison_chain)]
    fn build_pattern(&mut self, node: &SyntaxNode) -> Option<HirPattern> {
        // Check recursion depth to prevent infinite loops
        if self.recursion_depth >= MAX_RECURSION_DEPTH {
            self.errors.push(HirError {
                message: format!(
                    "Maximum recursion depth ({}) exceeded while building pattern",
                    MAX_RECURSION_DEPTH
                ),
                location: None,
            });
            return None;
        }

        self.recursion_depth += 1;
        let result = match node.kind() {
            SyntaxKind::LiteralPattern => {
                // Find literal token (it's a token, not a node)
                let literal_token = node.children_with_tokens().find(|element| {
                    #[allow(unknown_lints, clippy::unnecessary_map_or)]
                    {
                        element.as_token().map_or(false, |t| {
                            matches!(
                                t.kind(),
                                SyntaxKind::IntLiteral
                                    | SyntaxKind::BinLiteral
                                    | SyntaxKind::HexLiteral
                                    | SyntaxKind::StringLiteral
                            )
                        })
                    }
                })?;

                if let Some(token) = literal_token.as_token() {
                    let literal = match token.kind() {
                        SyntaxKind::IntLiteral => {
                            let text = token.text();
                            let value = text.parse::<u64>().ok()?;
                            HirLiteral::Integer(value)
                        }
                        SyntaxKind::BinLiteral => {
                            let text = token.text();
                            let value = parse_binary(text)?;
                            // Convert to bit vector, preserving the original bit width from source
                            // Extract the binary digits after "0b" prefix, removing underscores
                            let bin_digits = text[2..].replace('_', "");
                            let bits: Vec<bool> =
                                bin_digits.chars().rev().map(|c| c == '1').collect();
                            HirLiteral::BitVector(bits)
                        }
                        SyntaxKind::HexLiteral => {
                            let text = token.text();
                            let value = parse_hex(text)?;
                            HirLiteral::Integer(value)
                        }
                        SyntaxKind::StringLiteral => {
                            let text = token.text();
                            // Remove quotes
                            let s = text
                                .trim_start_matches('"')
                                .trim_end_matches('"')
                                .to_string();
                            HirLiteral::String(s)
                        }
                        _ => return None,
                    };
                    Some(HirPattern::Literal(literal))
                } else {
                    None
                }
            }
            SyntaxKind::IdentPattern => {
                // Check if this is a path pattern (Enum::Variant) or just a variable
                let idents: Vec<_> = node
                    .children_with_tokens()
                    .filter_map(|element| {
                        element
                            .as_token()
                            .filter(|t| t.kind() == SyntaxKind::Ident)
                            .map(|t| t.text().to_string())
                    })
                    .collect();

                if idents.len() == 2 {
                    // Path pattern: Enum::Variant
                    Some(HirPattern::Path(idents[0].clone(), idents[1].clone()))
                } else if idents.len() == 1 {
                    // Variable pattern
                    Some(HirPattern::Variable(idents[0].clone()))
                } else {
                    // Fallback to first identifier
                    let name = node
                        .first_token_of_kind(SyntaxKind::Ident)
                        .map(|t| t.text().to_string())?;
                    Some(HirPattern::Variable(name))
                }
            }
            SyntaxKind::WildcardPattern => Some(HirPattern::Wildcard),
            SyntaxKind::TuplePattern => {
                // Build patterns for tuple elements
                let mut patterns = Vec::new();
                for child in node.children() {
                    if matches!(
                        child.kind(),
                        SyntaxKind::LiteralPattern
                            | SyntaxKind::IdentPattern
                            | SyntaxKind::WildcardPattern
                            | SyntaxKind::TuplePattern
                    ) {
                        if let Some(pattern) = self.build_pattern(&child) {
                            patterns.push(pattern);
                        }
                    }
                }
                Some(HirPattern::Tuple(patterns))
            }
            _ => None,
        };
        self.recursion_depth -= 1;
        result
    }

    /// Build literal for pattern matching
    fn build_literal_for_pattern(&mut self, node: &SyntaxNode) -> Option<HirLiteral> {
        if let Some(token) = node.first_child_or_token() {
            match token.kind() {
                SyntaxKind::IntLiteral => {
                    let text = token.as_token().map(|t| t.text())?;
                    let value = text.parse::<u64>().ok()?;
                    Some(HirLiteral::Integer(value))
                }
                SyntaxKind::BinLiteral => {
                    let text = token.as_token().map(|t| t.text())?;
                    let value = parse_binary(text)?;
                    // Convert to bit vector, preserving the original bit width from source
                    // Extract the binary digits after "0b" prefix, removing underscores
                    let bin_digits = text[2..].replace('_', "");
                    let bits: Vec<bool> = bin_digits.chars().rev().map(|c| c == '1').collect();
                    Some(HirLiteral::BitVector(bits))
                }
                SyntaxKind::HexLiteral => {
                    let text = token.as_token().map(|t| t.text())?;
                    let value = parse_hex(text)?;
                    Some(HirLiteral::Integer(value))
                }
                SyntaxKind::StringLiteral => {
                    let text = token.as_token().map(|t| t.text())?;
                    // Remove quotes
                    let s = text
                        .trim_start_matches('"')
                        .trim_end_matches('"')
                        .to_string();
                    Some(HirLiteral::String(s))
                }
                _ => None,
            }
        } else {
            None
        }
    }

    /// Build L-value expression
    fn build_lvalue(&mut self, node: &SyntaxNode) -> Option<HirLValue> {
        match node.kind() {
            SyntaxKind::IdentExpr => {
                let name = node
                    .first_token_of_kind(SyntaxKind::Ident)
                    .map(|t| t.text().to_string())?;

                // Look up symbol
                if let Some(symbol) = self.symbols.lookup(&name) {
                    match symbol {
                        SymbolId::Port(id) => {
                            // For ports, create a port L-value
                            Some(HirLValue::Port(*id))
                        }
                        SymbolId::Signal(id) => Some(HirLValue::Signal(*id)),
                        SymbolId::Variable(id) => Some(HirLValue::Variable(*id)),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            SyntaxKind::IndexExpr => {
                // Handle indexed L-values like signal[index] or signal[start:end]
                let children: Vec<_> = node.children().collect();

                if children.is_empty() {
                    return None;
                }

                // First child is the base L-value
                let base = self.build_lvalue(&children[0])?;

                // Look for index expressions after the base
                let index_exprs: Vec<_> = node
                    .children()
                    .filter(|n| {
                        matches!(
                            n.kind(),
                            SyntaxKind::LiteralExpr
                                | SyntaxKind::IdentExpr
                                | SyntaxKind::BinaryExpr
                                | SyntaxKind::UnaryExpr
                        )
                    })
                    .skip(1) // Skip the base
                    .collect();

                if index_exprs.is_empty() {
                    return None;
                }

                // Check if it's a range (has colon token between indices)
                let has_colon = node.children_with_tokens().any(|element| {
                    element
                        .as_token()
                        .is_some_and(|t| t.kind() == SyntaxKind::Colon)
                });

                if has_colon && index_exprs.len() >= 2 {
                    // Range indexing: signal[start:end]
                    let start_expr = self.build_expression(&index_exprs[0])?;
                    let end_expr = self.build_expression(&index_exprs[1])?;
                    Some(HirLValue::Range(Box::new(base), start_expr, end_expr))
                } else {
                    // Single indexing: signal[index]
                    let index_expr = self.build_expression(&index_exprs[0])?;
                    Some(HirLValue::Index(Box::new(base), index_expr))
                }
            }
            SyntaxKind::FieldExpr => {
                // Handle field access like struct.field
                // For now, treat as simple identifier
                // Look for the last identifier token
                let tokens: Vec<_> = node
                    .children_with_tokens()
                    .filter_map(|e| e.into_token())
                    .filter(|t| t.kind() == SyntaxKind::Ident)
                    .collect();

                let name = tokens.last().map(|t| t.text().to_string())?;

                // Look up as signal or variable
                if let Some(symbol) = self.symbols.lookup(&name) {
                    match symbol {
                        SymbolId::Signal(id) => Some(HirLValue::Signal(*id)),
                        SymbolId::Variable(id) => Some(HirLValue::Variable(*id)),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Build expression
    #[allow(clippy::comparison_chain)]
    fn build_expression(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // Check recursion depth to prevent infinite loops
        if self.recursion_depth >= MAX_RECURSION_DEPTH {
            self.errors.push(HirError {
                message: format!(
                    "Maximum recursion depth ({}) exceeded while building expression",
                    MAX_RECURSION_DEPTH
                ),
                location: None,
            });
            return None;
        }

        self.recursion_depth += 1;
        let result = match node.kind() {
            SyntaxKind::LiteralExpr => self.build_literal_expr(node),
            SyntaxKind::IdentExpr => self.build_ident_expr(node),
            SyntaxKind::BinaryExpr => self.build_binary_expr(node),
            SyntaxKind::UnaryExpr => self.build_unary_expr(node),
            SyntaxKind::CallExpr => self.build_call_expr(node),
            SyntaxKind::StructLiteral => self.build_struct_literal(node),
            SyntaxKind::FieldExpr => self.build_field_expr(node),
            SyntaxKind::IndexExpr => self.build_index_expr(node),
            SyntaxKind::PathExpr => self.build_path_expr(node),
            SyntaxKind::IfExpr => self.build_if_expr(node),
            SyntaxKind::MatchExpr => self.build_match_expr(node),
            SyntaxKind::CastExpr => self.build_cast_expr(node),
            SyntaxKind::ParenExpr => {
                // Unwrap parentheses - but handle parser bug where complex expressions
                // inside parens are represented as multiple sibling BinaryExpr nodes
                // For example: (a & b & c) becomes:
                //   ParenExpr
                //     IdentExpr(a)
                //     BinaryExpr(& b)
                //     BinaryExpr(& c)

                let children: Vec<_> = node.children().collect();
                let binary_count = children
                    .iter()
                    .filter(|n| n.kind() == SyntaxKind::BinaryExpr)
                    .count();

                if binary_count > 1 {
                    // Multiple binary expressions - need to combine them
                    // Treat the ParenExpr itself as a BinaryExpr for building
                    self.build_binary_expr(node)
                } else if binary_count == 1 {
                    // Single binary expression - just process it
                    let binary = children
                        .iter()
                        .find(|n| n.kind() == SyntaxKind::BinaryExpr)?;
                    self.build_expression(binary)
                } else {
                    // No binary expressions - just unwrap to the single expression inside
                    node.children()
                        .find(|n| {
                            matches!(
                                n.kind(),
                                SyntaxKind::LiteralExpr
                                    | SyntaxKind::IdentExpr
                                    | SyntaxKind::UnaryExpr
                                    | SyntaxKind::CallExpr
                                    | SyntaxKind::FieldExpr
                                    | SyntaxKind::IndexExpr
                                    | SyntaxKind::ArrayLiteral
                                    | SyntaxKind::PathExpr
                                    | SyntaxKind::ParenExpr
                                    | SyntaxKind::IfExpr
                                    | SyntaxKind::MatchExpr
                            )
                        })
                        .and_then(|n| self.build_expression(&n))
                }
            }
            _ => None,
        };
        self.recursion_depth -= 1;
        result
    }

    /// Build literal expression
    fn build_literal_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        if let Some(token) = node.first_child_or_token() {
            match token.kind() {
                SyntaxKind::IntLiteral => {
                    let text = token.as_token().map(|t| t.text())?;
                    let value = text.parse::<u64>().ok()?;
                    Some(HirExpression::Literal(HirLiteral::Integer(value)))
                }
                SyntaxKind::BinLiteral => {
                    let text = token.as_token().map(|t| t.text())?;
                    let value = parse_binary(text)?;
                    // Convert to bit vector, preserving the original bit width from source
                    // Extract the binary digits after "0b" prefix, removing underscores
                    let bin_digits = text[2..].replace('_', "");
                    let bits: Vec<bool> = bin_digits.chars().rev().map(|c| c == '1').collect();
                    Some(HirExpression::Literal(HirLiteral::BitVector(bits)))
                }
                SyntaxKind::HexLiteral => {
                    let text = token.as_token().map(|t| t.text())?;
                    let value = parse_hex(text)?;
                    Some(HirExpression::Literal(HirLiteral::Integer(value)))
                }
                SyntaxKind::FloatLiteral => {
                    let text = token.as_token().map(|t| t.text())?;
                    let value = parse_float(text)?;
                    Some(HirExpression::Literal(HirLiteral::Float(value)))
                }
                SyntaxKind::StringLiteral => {
                    let text = token.as_token().map(|t| t.text())?;
                    // Remove quotes
                    let s = text
                        .trim_start_matches('"')
                        .trim_end_matches('"')
                        .to_string();
                    Some(HirExpression::Literal(HirLiteral::String(s)))
                }
                SyntaxKind::TrueKw => Some(HirExpression::Literal(HirLiteral::Boolean(true))),
                SyntaxKind::FalseKw => Some(HirExpression::Literal(HirLiteral::Boolean(false))),
                _ => None,
            }
        } else {
            None
        }
    }

    /// Build identifier expression
    fn build_ident_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        let name = node
            .first_token_of_kind(SyntaxKind::Ident)
            .map(|t| t.text().to_string())?;

        // Check if this is a builtin function - don't resolve as symbol
        if BUILTIN_FUNCTIONS.contains(&name.as_str()) {
            // Builtin functions should only appear in call expressions
            // If we see them as plain identifiers, something is wrong with the parse tree
            // Return None to indicate this should be handled differently
            return None;
        }

        // Look up symbol
        if let Some(symbol) = self.symbols.lookup(&name) {
            match symbol {
                SymbolId::Port(id) => {
                    // For ports in expressions, use port reference
                    Some(HirExpression::Port(*id))
                }
                SymbolId::Signal(id) => Some(HirExpression::Signal(*id)),
                SymbolId::Variable(id) => Some(HirExpression::Variable(*id)),
                SymbolId::Constant(id) => Some(HirExpression::Constant(*id)),
                SymbolId::GenericParam(param_name) => {
                    // Generic parameters in expressions - create a Constant expression
                    // They will be resolved during monomorphization
                    // For now, look up the constant ID or create a placeholder
                    if let Some(const_id) = self.symbols.constants.get(param_name) {
                        Some(HirExpression::Constant(*const_id))
                    } else {
                        // Create a variable reference for the generic param
                        // This will be resolved during type checking/monomorphization
                        Some(HirExpression::GenericParam(param_name.clone()))
                    }
                }
                _ => None,
            }
        } else {
            // Treat unresolved identifiers as generic parameters or function parameters
            // This allows const function parameters to be referenced in function bodies
            // They will be bound during const evaluation
            Some(HirExpression::GenericParam(name))
        }
    }

    /// Build function call expression
    fn build_call_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // Get function name - try direct Ident token first, then IdentExpr child,
        // then check for preceding sibling IdentExpr (postfix call syntax)
        let function = if let Some(ident_token) = node.first_token_of_kind(SyntaxKind::Ident) {
            ident_token.text().to_string()
        } else if let Some(ident_expr) = node.first_child_of_kind(SyntaxKind::IdentExpr) {
            let name = ident_expr
                .first_token_of_kind(SyntaxKind::Ident)
                .map(|t| t.text().to_string())?;
            name
        } else if let Some(parent) = node.parent() {
            // Postfix call: IdentExpr and CallExpr are siblings
            // Look for preceding IdentExpr sibling
            let siblings: Vec<_> = parent.children().collect();
            let call_pos = siblings.iter().position(|n| n == node)?;

            if call_pos > 0 {
                if let Some(prev_sibling) = siblings.get(call_pos - 1) {
                    if prev_sibling.kind() == SyntaxKind::IdentExpr {
                        let name = prev_sibling
                            .first_token_of_kind(SyntaxKind::Ident)
                            .map(|t| t.text().to_string())?;
                        name
                    } else {
                        return None;
                    }
                } else {
                    return None;
                }
            } else {
                return None;
            }
        } else {
            return None;
        };

        // Parse arguments - skip the first child if it's the function name (IdentExpr)
        let mut args = Vec::new();
        let mut skip_first = node
            .first_child()
            .is_some_and(|c| c.kind() == SyntaxKind::IdentExpr);

        for child in node.children() {
            if skip_first {
                skip_first = false;
                continue; // Skip the IdentExpr that contains the function name
            }
            // All remaining children are expression arguments
            if let Some(expr) = self.build_expression(&child) {
                args.push(expr);
            }
        }

        Some(HirExpression::Call(HirCallExpr { function, args }))
    }

    /// Build struct literal expression
    fn build_struct_literal(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // The StructLiteral node should have:
        // - A type name token (first Ident)
        // - Multiple StructFieldInit children

        // Get the type name - it's the first identifier token in the StructLiteral node
        let type_name = node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|t| t.kind() == SyntaxKind::Ident)
            .map(|t| t.text().to_string())?;

        // Parse field initializations
        let mut fields = Vec::new();
        for child in node.children() {
            if child.kind() == SyntaxKind::StructFieldInit {
                if let Some(field_init) = self.build_struct_field_init(&child) {
                    fields.push(field_init);
                }
            }
        }

        Some(HirExpression::StructLiteral(HirStructLiteral {
            type_name,
            fields,
        }))
    }

    /// Build struct field initialization
    fn build_struct_field_init(&mut self, node: &SyntaxNode) -> Option<HirStructFieldInit> {
        // StructFieldInit has: field_name : expression
        let name = node
            .first_token_of_kind(SyntaxKind::Ident)
            .map(|t| t.text().to_string())?;

        // Find the expression child
        let value = node
            .children()
            .find(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::LiteralExpr
                        | SyntaxKind::IdentExpr
                        | SyntaxKind::BinaryExpr
                        | SyntaxKind::UnaryExpr
                        | SyntaxKind::CallExpr
                        | SyntaxKind::FieldExpr
                        | SyntaxKind::IndexExpr
                        | SyntaxKind::PathExpr
                        | SyntaxKind::ParenExpr
                        | SyntaxKind::IfExpr
                        | SyntaxKind::MatchExpr
                        | SyntaxKind::StructLiteral
                        | SyntaxKind::ArrayLiteral
                )
            })
            .and_then(|n| self.build_expression(&n))?;

        Some(HirStructFieldInit { name, value })
    }

    /// Build binary expression
    fn build_binary_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // Find all expression children (filter out tokens and other nodes)
        let mut expr_children: Vec<_> = node
            .children()
            .filter(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::LiteralExpr
                        | SyntaxKind::IdentExpr
                        | SyntaxKind::BinaryExpr
                        | SyntaxKind::UnaryExpr
                        | SyntaxKind::FieldExpr
                        | SyntaxKind::IndexExpr
                        | SyntaxKind::PathExpr
                        | SyntaxKind::ParenExpr
                        | SyntaxKind::IfExpr
                        | SyntaxKind::MatchExpr
                        | SyntaxKind::CallExpr
                        | SyntaxKind::ArrayLiteral
                )
            })
            .collect();

        // SPECIAL CASE: If we're being called on a ParenExpr with multiple BinaryExpr children,
        // we need to handle the flattened structure where each BinaryExpr only contains operator + right operand
        if node.kind() == SyntaxKind::ParenExpr {
            let binary_children: Vec<_> = expr_children
                .iter()
                .filter(|n| n.kind() == SyntaxKind::BinaryExpr)
                .collect();

            if binary_children.len() > 1 || (binary_children.len() == 1 && expr_children.len() > 1)
            {
                // First, clean up IdentExpr + IndexExpr pairs (parser bug)
                // For example: [(IdentExpr, "a"), (IndexExpr, "[31]")] should be treated as one IndexExpr
                let mut indices_to_skip = Vec::new();
                for i in 0..expr_children.len() {
                    if expr_children[i].kind() == SyntaxKind::IndexExpr && i > 0 {
                        let prev = &expr_children[i - 1];
                        if matches!(
                            prev.kind(),
                            SyntaxKind::IdentExpr | SyntaxKind::FieldExpr | SyntaxKind::PathExpr
                        ) {
                            // Check if they're adjacent
                            if let (Some(prev_parent), Some(index_parent)) =
                                (prev.parent(), expr_children[i].parent())
                            {
                                if prev_parent == index_parent {
                                    let siblings: Vec<_> = prev_parent.children().collect();
                                    if let (Some(prev_pos), Some(index_pos)) = (
                                        siblings.iter().position(|n| n == prev),
                                        siblings.iter().position(|n| n == &expr_children[i]),
                                    ) {
                                        if index_pos == prev_pos + 1 {
                                            // Mark the IdentExpr for skipping, keep the IndexExpr
                                            indices_to_skip.push(i - 1);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // We have a flattened expression like: first_operand BinaryExpr(op operand) BinaryExpr(op operand) ...
                // Build it left-associatively
                let mut result_expr = None;

                for (idx, child) in expr_children.iter().enumerate() {
                    // Skip IdentExprs that are part of IndexExprs
                    if indices_to_skip.contains(&idx) {
                        continue;
                    }

                    if child.kind() == SyntaxKind::BinaryExpr {
                        // This is an operator + operand
                        // Extract the operator
                        let op_token = child
                            .children_with_tokens()
                            .filter_map(|e| e.into_token())
                            .find(|t| {
                                matches!(
                                    t.kind(),
                                    SyntaxKind::Plus
                                        | SyntaxKind::Minus
                                        | SyntaxKind::Star
                                        | SyntaxKind::Slash
                                        | SyntaxKind::Percent
                                        | SyntaxKind::Amp
                                        | SyntaxKind::Pipe
                                        | SyntaxKind::Caret
                                        | SyntaxKind::Shl
                                        | SyntaxKind::Shr
                                        | SyntaxKind::Eq
                                        | SyntaxKind::Neq
                                        | SyntaxKind::Lt
                                        | SyntaxKind::Le
                                        | SyntaxKind::Gt
                                        | SyntaxKind::Ge
                                        | SyntaxKind::AmpAmp
                                        | SyntaxKind::PipePipe
                                )
                            });

                        // Extract the right operand from this BinaryExpr's children
                        let binary_expr_children: Vec<_> = child
                            .children()
                            .filter(|n| {
                                matches!(
                                    n.kind(),
                                    SyntaxKind::LiteralExpr
                                        | SyntaxKind::IdentExpr
                                        | SyntaxKind::BinaryExpr
                                        | SyntaxKind::UnaryExpr
                                        | SyntaxKind::FieldExpr
                                        | SyntaxKind::IndexExpr
                                        | SyntaxKind::PathExpr
                                        | SyntaxKind::ParenExpr
                                        | SyntaxKind::IfExpr
                                        | SyntaxKind::MatchExpr
                                        | SyntaxKind::CallExpr
                                        | SyntaxKind::ArrayLiteral
                                )
                            })
                            .collect();

                        // Parser bug: If we have [(IdentExpr, "x"), (IndexExpr, "[i]")], use the IndexExpr
                        let right_node = if binary_expr_children.len() >= 2 {
                            if binary_expr_children[1].kind() == SyntaxKind::IndexExpr
                                && matches!(
                                    binary_expr_children[0].kind(),
                                    SyntaxKind::IdentExpr
                                        | SyntaxKind::FieldExpr
                                        | SyntaxKind::PathExpr
                                )
                            {
                                &binary_expr_children[1]
                            } else {
                                binary_expr_children.first()?
                            }
                        } else {
                            binary_expr_children.first()?
                        };

                        if let Some(op_tok) = op_token {
                            let op = self.token_to_binary_op(op_tok.kind())?;
                            let right = Box::new(self.build_expression(right_node)?);

                            if let Some(left_expr) = result_expr {
                                // Chain: (previous result) op right
                                result_expr = Some(HirExpression::Binary(HirBinaryExpr {
                                    left: Box::new(left_expr),
                                    op,
                                    right,
                                }));
                            } else {
                                return None; // Error: found BinaryExpr but no left operand yet
                            }
                        }
                    } else {
                        // This is a regular operand
                        let operand = self.build_expression(child)?;
                        if result_expr.is_none() {
                            // First operand
                            result_expr = Some(operand);
                        } else {
                            // Unexpected: operand after we already have a result?
                            // This shouldn't happen in well-formed expressions
                            eprintln!("WARNING: Unexpected operand after result in ParenExpr");
                        }
                    }
                }

                return result_expr;
            }
        }

        // WORKAROUND FOR PARSER BUG: If we have an IndexExpr as a child, the base might also be a child
        // For example: "a << b[2:0]" might have children [IdentExpr(b), IndexExpr([2:0])]
        // We need to identify which IdentExprs are actually bases of IndexExprs (adjacent siblings)
        // and which are separate operands

        // Build a list of indices to remove (IdentExpr/FieldExpr that immediately precede IndexExpr in tree order)
        let mut indices_to_remove = Vec::new();

        for i in 0..expr_children.len() {
            if expr_children[i].kind() == SyntaxKind::IndexExpr && i > 0 {
                // Check if the previous child is likely the base
                let prev = &expr_children[i - 1];
                if matches!(
                    prev.kind(),
                    SyntaxKind::IdentExpr | SyntaxKind::FieldExpr | SyntaxKind::PathExpr
                ) {
                    // Check if they're adjacent in the CST (no nodes/tokens between them)
                    // We can check this by seeing if prev's next sibling is the IndexExpr
                    if let (Some(prev_parent), Some(index_parent)) =
                        (prev.parent(), expr_children[i].parent())
                    {
                        if prev_parent == index_parent {
                            // They share the same parent (this BinaryExpr node)
                            // Check if prev immediately precedes index in the sibling list
                            let siblings: Vec<_> = prev_parent.children().collect();
                            if let (Some(prev_pos), Some(index_pos)) = (
                                siblings.iter().position(|n| n == prev),
                                siblings.iter().position(|n| n == &expr_children[i]),
                            ) {
                                // Only remove if they're immediately adjacent (no other nodes between)
                                if index_pos == prev_pos + 1 {
                                    indices_to_remove.push(i - 1);
                                }
                            }
                        }
                    }
                }
            }
        }

        // Remove in reverse order to maintain correct indices
        for &idx in indices_to_remove.iter().rev() {
            expr_children.remove(idx);
        }

        // WORKAROUND FOR PARSER BUG: Due to how the parser implements Pratt parsing,
        // the left operand of a binary expression may be a SIBLING of the BinaryExpr node
        // instead of a child. If we only have 1 child, look for the left operand in the parent.
        if expr_children.len() == 1 {
            if let Some(parent) = node.parent() {
                // Find the previous sibling expression node
                let siblings: Vec<_> = parent.children().collect();
                if let Some(pos) = siblings.iter().position(|n| n == node) {
                    if pos > 0 {
                        // Check if the previous sibling is an expression
                        let prev = &siblings[pos - 1];
                        if matches!(
                            prev.kind(),
                            SyntaxKind::LiteralExpr
                                | SyntaxKind::IdentExpr
                                | SyntaxKind::BinaryExpr
                                | SyntaxKind::UnaryExpr
                                | SyntaxKind::FieldExpr
                                | SyntaxKind::IndexExpr
                                | SyntaxKind::PathExpr
                                | SyntaxKind::ParenExpr
                                | SyntaxKind::IfExpr
                                | SyntaxKind::MatchExpr
                                | SyntaxKind::CallExpr
                                | SyntaxKind::ArrayLiteral
                        ) {
                            // Insert the previous sibling as the left operand
                            expr_children.insert(0, prev.clone());
                        }
                    }
                }
            }
        }

        if expr_children.len() < 2 {
            return None;
        }

        let left = Box::new(self.build_expression(&expr_children[0])?);
        let right = Box::new(self.build_expression(&expr_children[1])?);

        // Get operator
        let tokens: Vec<_> = node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .collect();

        let op = tokens
            .iter()
            .find(|t| t.kind().is_operator())
            .and_then(|t| self.token_to_binary_op(t.kind()));

        let op = op?;

        Some(HirExpression::Binary(HirBinaryExpr { left, op, right }))
    }

    /// Build unary expression
    fn build_unary_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // WORKAROUND FOR PARSER BUG: Similar to binary expressions, if we have an IndexExpr,
        // the base might also be a child. For "~a[3]", we might have [IdentExpr(a), IndexExpr([3])]
        // We want to use IndexExpr, not IdentExpr
        let expr_children: Vec<_> = node
            .children()
            .filter(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::LiteralExpr
                        | SyntaxKind::IdentExpr
                        | SyntaxKind::BinaryExpr
                        | SyntaxKind::UnaryExpr
                        | SyntaxKind::FieldExpr
                        | SyntaxKind::IndexExpr
                        | SyntaxKind::PathExpr
                        | SyntaxKind::ParenExpr
                        | SyntaxKind::IfExpr
                        | SyntaxKind::MatchExpr
                        | SyntaxKind::CallExpr
                        | SyntaxKind::ArrayLiteral
                )
            })
            .collect();

        // If we have both IdentExpr and IndexExpr, use the IndexExpr (it's the complete expression)
        let operand_node = if expr_children.len() > 1 {
            // Check if last is IndexExpr and previous is IdentExpr/FieldExpr/PathExpr
            if expr_children
                .last()
                .is_some_and(|n| n.kind() == SyntaxKind::IndexExpr)
                && expr_children.len() >= 2
                && matches!(
                    expr_children[expr_children.len() - 2].kind(),
                    SyntaxKind::IdentExpr | SyntaxKind::FieldExpr | SyntaxKind::PathExpr
                )
            {
                expr_children.last()?
            } else {
                expr_children.first()?
            }
        } else {
            expr_children.first()?
        };

        let operand = self.build_expression(operand_node)?;

        // Get operator
        let op = node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|t| {
                matches!(
                    t.kind(),
                    SyntaxKind::Bang | SyntaxKind::Tilde | SyntaxKind::Minus
                )
            })
            .and_then(|t| self.token_to_unary_op(t.kind()))?;

        Some(HirExpression::Unary(HirUnaryExpr {
            op,
            operand: Box::new(operand),
        }))
    }

    /// Build field expression
    fn build_field_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // Get base expression and field name
        let children: Vec<_> = node.children().collect();
        if children.is_empty() {
            return None;
        }

        let base_expr = self.build_expression(&children[0]);
        let base = Box::new(base_expr?);

        // Find the field name (identifier after dot)
        let field_name = node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|t| t.kind() == SyntaxKind::Ident)
            .map(|t| t.text().to_string());
        let field_name = field_name?;

        Some(HirExpression::FieldAccess {
            base,
            field: field_name,
        })
    }

    /// Build field access from separate base and field expression nodes
    fn build_field_access_from_parts(
        &mut self,
        base_node: &SyntaxNode,
        field_node: &SyntaxNode,
    ) -> Option<HirExpression> {
        // Build the base expression from the base node
        let base = Box::new(self.build_expression(base_node)?);

        // Extract the field name from the field node
        let field_name = field_node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|t| t.kind() == SyntaxKind::Ident)
            .map(|t| t.text().to_string())?;

        Some(HirExpression::FieldAccess {
            base,
            field: field_name,
        })
    }

    /// Build index access from separate base and index expression nodes
    fn build_index_access_from_parts(
        &mut self,
        base_node: &SyntaxNode,
        index_node: &SyntaxNode,
    ) -> Option<HirExpression> {
        // Build the base expression from the base node
        let base = Box::new(self.build_expression(base_node)?);

        // Parse the index expression to get the indices
        let indices: Vec<_> = index_node
            .children()
            .filter(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::LiteralExpr
                        | SyntaxKind::IdentExpr
                        | SyntaxKind::BinaryExpr
                        | SyntaxKind::UnaryExpr
                )
            })
            .collect();

        if indices.is_empty() {
            return None;
        }

        // Check if it's a range access [start:end] by looking for colon token
        let has_colon = index_node.children_with_tokens().any(|element| {
            element
                .as_token()
                .is_some_and(|t| t.kind() == SyntaxKind::Colon)
        });

        if has_colon && indices.len() >= 2 {
            // Range access: base[start:end]
            let start = Box::new(self.build_expression(&indices[0])?);
            let end = Box::new(self.build_expression(&indices[1])?);
            Some(HirExpression::Range(base, start, end))
        } else {
            // Single index access: base[index]
            let index = Box::new(self.build_expression(&indices[0])?);
            Some(HirExpression::Index(base, index))
        }
    }

    /// Build path expression (e.g., State::Idle)
    fn build_path_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // Get the enum name and variant name from the tokens
        let mut idents = Vec::new();
        for elem in node.children_with_tokens() {
            if let Some(token) = elem.as_token() {
                if token.kind() == SyntaxKind::Ident {
                    idents.push(token.text().to_string());
                }
            }
        }

        if idents.len() >= 2 {
            let enum_name = idents[0].clone();
            let variant_name = idents[1].clone();

            // Return enum variant expression for code generation
            Some(HirExpression::EnumVariant {
                enum_type: enum_name,
                variant: variant_name,
            })
        } else {
            None
        }
    }

    /// Build if expression
    fn build_if_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // Parse structure: if <condition> { <then_expr> } else { <else_expr> } or else if ...
        // Due to parser bug, sub-expressions may appear as siblings.
        // We need to filter based on tokens to find the correct expressions.

        // Find expressions by looking at tokens:
        // Structure: if <cond> { <then> } else { <else> }

        let mut found_if = false;
        let mut found_lbrace1 = false;
        let mut found_rbrace1 = false;
        let mut found_else = false;
        let mut found_lbrace2 = false;

        let mut condition_expr = None;
        let mut then_expr = None;
        let mut else_expr = None;

        for element in node.children_with_tokens() {
            match element {
                rowan::NodeOrToken::Token(t) => match t.kind() {
                    SyntaxKind::IfKw => found_if = true,
                    SyntaxKind::LBrace if !found_lbrace1 => found_lbrace1 = true,
                    SyntaxKind::RBrace if !found_rbrace1 => found_rbrace1 = true,
                    SyntaxKind::ElseKw => found_else = true,
                    SyntaxKind::LBrace if found_else && !found_lbrace2 => found_lbrace2 = true,
                    _ => {}
                },
                rowan::NodeOrToken::Node(n) => {
                    if matches!(
                        n.kind(),
                        SyntaxKind::LiteralExpr
                            | SyntaxKind::IdentExpr
                            | SyntaxKind::BinaryExpr
                            | SyntaxKind::UnaryExpr
                            | SyntaxKind::CallExpr
                            | SyntaxKind::FieldExpr
                            | SyntaxKind::IndexExpr
                            | SyntaxKind::PathExpr
                            | SyntaxKind::ParenExpr
                            | SyntaxKind::IfExpr
                            | SyntaxKind::MatchExpr
                    ) {
                        if found_if && !found_lbrace1 {
                            // Take the LAST expression before the first brace (to handle sub-expressions)
                            condition_expr = Some(n);
                        } else if found_lbrace1 && !found_rbrace1 {
                            // Take the LAST expression between braces
                            then_expr = Some(n);
                        } else if found_else {
                            // Take the LAST expression after else (works for both { expr } and else if)
                            else_expr = Some(n);
                        }
                    }
                }
            }
        }

        // Now build the expressions, taking the LAST occurrence of each to handle sub-expressions
        // (similar to the match arm fix)
        let condition = self.build_expression(&condition_expr?)?;
        let then = self.build_expression(&then_expr?)?;
        let else_val = self.build_expression(&else_expr?)?;

        Some(HirExpression::If(HirIfExpr {
            condition: Box::new(condition),
            then_expr: Box::new(then),
            else_expr: Box::new(else_val),
        }))
    }

    /// Build match expression
    fn build_match_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // Parse structure: match <expr> { <arms> }

        // First child should be the expression being matched
        let expr = node
            .children()
            .find(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::LiteralExpr
                        | SyntaxKind::IdentExpr
                        | SyntaxKind::BinaryExpr
                        | SyntaxKind::UnaryExpr
                        | SyntaxKind::CallExpr
                        | SyntaxKind::FieldExpr
                        | SyntaxKind::IndexExpr
                        | SyntaxKind::PathExpr
                        | SyntaxKind::ParenExpr
                        | SyntaxKind::IfExpr
                        | SyntaxKind::MatchExpr
                )
            })
            .and_then(|n| self.build_expression(&n))?;

        // Build match arms from MATCH_ARM_LIST
        let mut arms = Vec::new();
        if let Some(arm_list) = node
            .children()
            .find(|n| n.kind() == SyntaxKind::MatchArmList)
        {
            for arm_node in arm_list
                .children()
                .filter(|n| n.kind() == SyntaxKind::MatchArm)
            {
                if let Some(arm) = self.build_match_arm_expr(&arm_node) {
                    arms.push(arm);
                }
            }
        }

        Some(HirExpression::Match(HirMatchExpr {
            expr: Box::new(expr),
            arms,
        }))
    }

    /// Build cast expression (expr as Type)
    fn build_cast_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // CastExpr structure from parser (manual tree building):
        //   ParenExpr (or other expression)
        //   TypeAnnotation
        // Find the expression (first child)
        let expr = node
            .children()
            .find(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::LiteralExpr
                        | SyntaxKind::IdentExpr
                        | SyntaxKind::BinaryExpr
                        | SyntaxKind::UnaryExpr
                        | SyntaxKind::CallExpr
                        | SyntaxKind::FieldExpr
                        | SyntaxKind::IndexExpr
                        | SyntaxKind::PathExpr
                        | SyntaxKind::ParenExpr
                        | SyntaxKind::IfExpr
                        | SyntaxKind::MatchExpr
                )
            })
            .and_then(|n| self.build_expression(&n))?;

        // For now, just return the expression unwrapped
        // TODO: Add type information to HIR when needed
        Some(expr)
    }

    /// Build match arm for match expressions
    fn build_match_arm_expr(&mut self, node: &SyntaxNode) -> Option<HirMatchArmExpr> {
        // Find pattern
        let pattern = node
            .children()
            .find(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::LiteralPattern
                        | SyntaxKind::IdentPattern
                        | SyntaxKind::WildcardPattern
                        | SyntaxKind::TuplePattern
                )
            })
            .and_then(|n| self.build_pattern(&n))?;

        // Find optional guard
        let guard = node
            .children()
            .find(|n| n.kind() == SyntaxKind::MatchGuard)
            .and_then(|guard_node| {
                guard_node
                    .children()
                    .find(|n| {
                        matches!(
                            n.kind(),
                            SyntaxKind::LiteralExpr
                                | SyntaxKind::IdentExpr
                                | SyntaxKind::BinaryExpr
                                | SyntaxKind::UnaryExpr
                                | SyntaxKind::FieldExpr
                                | SyntaxKind::IndexExpr
                                | SyntaxKind::PathExpr
                                | SyntaxKind::ParenExpr
                                | SyntaxKind::CallExpr
                                | SyntaxKind::ArrayLiteral
                                | SyntaxKind::IfExpr
                                | SyntaxKind::MatchExpr
                        )
                    })
                    .and_then(|n| self.build_expression(&n))
            });

        // Find the arm expression (after the arrow)
        // The parser structure is: MatchArm contains pattern, optional guard, and expression
        // Due to Rowan's CST, sub-expressions may appear as direct children
        // We need to find the ROOT expression node by looking at children_with_tokens
        // and finding the first expression node that comes AFTER the FatArrow (=>)

        // Find all expression nodes after the arrow
        // Take the LAST one, as it will be the outermost expression
        // (Earlier ones are sub-expressions that are also children of the outermost one)
        let expr_nodes_after_arrow: Vec<_> = node
            .children_with_tokens()
            .skip_while(|e| {
                #[allow(unknown_lints, clippy::unnecessary_map_or)]
                {
                    e.as_token()
                        .map_or(true, |t| t.kind() != SyntaxKind::FatArrow)
                }
            })
            .skip(1) // Skip the arrow itself
            .filter_map(|e| {
                e.as_node()
                    .filter(|n| {
                        matches!(
                            n.kind(),
                            SyntaxKind::LiteralExpr
                                | SyntaxKind::IdentExpr
                                | SyntaxKind::BinaryExpr
                                | SyntaxKind::UnaryExpr
                                | SyntaxKind::FieldExpr
                                | SyntaxKind::IndexExpr
                                | SyntaxKind::PathExpr
                                | SyntaxKind::ParenExpr
                                | SyntaxKind::CallExpr
                                | SyntaxKind::ArrayLiteral
                                | SyntaxKind::IfExpr
                                | SyntaxKind::MatchExpr
                        )
                    })
                    .cloned()
            })
            .collect();

        let expr_node = expr_nodes_after_arrow.last()?;
        let expr = self.build_expression(expr_node)?;

        Some(HirMatchArmExpr {
            pattern,
            guard,
            expr,
        })
    }

    /// Build index expression
    fn build_index_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        let children: Vec<_> = node.children().collect();

        // Determine if this is a range by checking for colon token
        let has_colon = node
            .children_with_tokens()
            .any(|e| e.as_token().is_some_and(|t| t.kind() == SyntaxKind::Colon));

        // WORKAROUND FOR PARSER BUG: The base expression is ALWAYS a sibling, not a child
        // Look for base in parent/siblings
        let mut base_expr = None;
        if let Some(parent) = node.parent() {
            let siblings: Vec<_> = parent.children().collect();
            if let Some(pos) = siblings.iter().position(|n| n == node) {
                if pos > 0 {
                    let prev = &siblings[pos - 1];
                    if matches!(
                        prev.kind(),
                        SyntaxKind::IdentExpr
                            | SyntaxKind::FieldExpr
                            | SyntaxKind::IndexExpr
                            | SyntaxKind::PathExpr
                    ) {
                        base_expr = Some(prev.clone());
                    }
                }
            }
        }

        let base_expr = base_expr?;

        if has_colon {
            // Range expression: base[high:low]
            // children should be [high, low]
            if children.len() < 2 {
                return None;
            }
            let base = Box::new(self.build_expression(&base_expr)?);
            let high = Box::new(self.build_expression(&children[0])?);
            let low = Box::new(self.build_expression(&children[1])?);
            Some(HirExpression::Range(base, high, low))
        } else {
            // Single index: base[index]
            // children should be [index]
            if children.is_empty() {
                return None;
            }
            let base = Box::new(self.build_expression(&base_expr)?);
            let index = Box::new(self.build_expression(&children[0])?);
            Some(HirExpression::Index(base, index))
        }
    }

    /// Build protocol (stub)
    fn build_protocol(&mut self, node: &SyntaxNode) -> Option<HirProtocol> {
        let id = self.next_protocol_id();
        let name = self.extract_name(node)?;

        // Build protocol signals
        let mut signals = Vec::new();
        if let Some(signal_list) = node.first_child_of_kind(SyntaxKind::ProtocolSignalList) {
            for signal_node in signal_list.children_of_kind(SyntaxKind::ProtocolSignal) {
                if let Some(signal) = self.build_protocol_signal(&signal_node) {
                    signals.push(signal);
                }
            }
        }

        Some(HirProtocol { id, name, signals })
    }

    /// Build protocol signal
    fn build_protocol_signal(&mut self, node: &SyntaxNode) -> Option<HirProtocolSignal> {
        // Extract signal name
        let name = node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|t| t.kind() == SyntaxKind::Ident)
            .map(|t| t.text().to_string())?;

        // Extract direction
        let direction =
            if let Some(direction_node) = node.first_child_of_kind(SyntaxKind::ProtocolDirection) {
                if direction_node
                    .first_token_of_kind(SyntaxKind::InKw)
                    .is_some()
                {
                    HirProtocolDirection::In
                } else if direction_node
                    .first_token_of_kind(SyntaxKind::OutKw)
                    .is_some()
                {
                    HirProtocolDirection::Out
                } else {
                    return None;
                }
            } else {
                return None;
            };

        // Extract type
        let signal_type =
            if let Some(type_annotation) = node.first_child_of_kind(SyntaxKind::TypeAnnotation) {
                self.extract_hir_type(&type_annotation)
            } else {
                HirType::Custom("unknown".to_string())
            };

        Some(HirProtocolSignal {
            name,
            direction,
            signal_type,
        })
    }

    /// Build intent with constraints
    fn build_intent(&mut self, node: &SyntaxNode) -> Option<HirIntent> {
        let id = self.next_intent_id();
        let name = self.extract_name(node)?;

        // Parse intent constraints
        let mut constraints = Vec::new();
        if let Some(constraint_list) = node.first_child_of_kind(SyntaxKind::IntentConstraintList) {
            for constraint_node in constraint_list.children() {
                if constraint_node.kind() == SyntaxKind::IntentConstraint {
                    if let Some(constraint) = self.build_intent_constraint(&constraint_node) {
                        constraints.push(constraint);
                    }
                }
            }
        }

        Some(HirIntent {
            id,
            name,
            description: String::new(),
            constraints,
        })
    }

    /// Build intent constraint from syntax node
    fn build_intent_constraint(&mut self, node: &SyntaxNode) -> Option<HirIntentConstraint> {
        // Extract constraint type identifier
        let constraint_token = node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|token| token.kind() == SyntaxKind::Ident)?;
        let constraint_type_name = constraint_token.text();

        // Map constraint type name to HirConstraintType
        let constraint_type = match constraint_type_name {
            "timing" => HirConstraintType::Timing,
            "power" => HirConstraintType::Power,
            "area" => HirConstraintType::Area,
            "performance" => HirConstraintType::Performance,
            _ => {
                // Unknown constraint type, default to Performance
                HirConstraintType::Performance
            }
        };

        // Parse constraint expression without symbol resolution
        // Intent constraints are optimization hints, not actual hardware signals
        let expr = node
            .children()
            .find(|child| {
                matches!(
                    child.kind(),
                    SyntaxKind::BinaryExpr
                        | SyntaxKind::UnaryExpr
                        | SyntaxKind::LiteralExpr
                        | SyntaxKind::IdentExpr
                )
            })
            .and_then(|expr_node| Self::build_constraint_expression(&expr_node))
            .unwrap_or({
                // Default expression if parsing fails
                HirExpression::Literal(HirLiteral::Integer(0))
            });

        Some(HirIntentConstraint {
            constraint_type,
            expr,
        })
    }

    /// Build constraint expression without symbol resolution
    /// Intent constraints contain keywords like "maximize", "minimize" which are not symbols
    fn build_constraint_expression(node: &SyntaxNode) -> Option<HirExpression> {
        match node.kind() {
            SyntaxKind::LiteralExpr => {
                // Parse numeric literals
                let token = node.first_token()?;
                let text = token.text();
                if let Ok(val) = text.parse::<u64>() {
                    Some(HirExpression::Literal(HirLiteral::Integer(val)))
                } else {
                    None
                }
            }
            SyntaxKind::IdentExpr => {
                // For constraint expressions, treat identifiers as string literals
                // This handles keywords like "maximize", "minimize", "MHz", "W", etc.
                let token = node.first_token()?;
                let text = token.text().to_string();
                Some(HirExpression::Literal(HirLiteral::String(text)))
            }
            SyntaxKind::BinaryExpr => {
                // Parse binary expressions like "< 1000"
                // For now, just recursively parse the left side
                let left = node
                    .first_child()
                    .and_then(|expr_node| Self::build_constraint_expression(&expr_node))?;
                Some(left)
            }
            _ => None,
        }
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
            .find(|t| t.kind() == SyntaxKind::Ident)
            .map(|t| t.text().to_string())
    }

    /// Extract port direction
    fn extract_port_direction(&self, node: &SyntaxNode) -> HirPortDirection {
        if node.first_token_of_kind(SyntaxKind::InKw).is_some() {
            HirPortDirection::Input
        } else if node.first_token_of_kind(SyntaxKind::OutKw).is_some() {
            HirPortDirection::Output
        } else if node.first_token_of_kind(SyntaxKind::InoutKw).is_some() {
            HirPortDirection::Bidirectional
        } else if node.first_token_of_kind(SyntaxKind::PortKw).is_some() {
            HirPortDirection::Protocol
        } else {
            HirPortDirection::Input
        }
    }

    /// Extract edge type
    fn extract_edge_type(&self, node: &SyntaxNode) -> HirEdgeType {
        if node.first_token_of_kind(SyntaxKind::RiseKw).is_some() {
            HirEdgeType::Rising
        } else if node.first_token_of_kind(SyntaxKind::FallKw).is_some() {
            HirEdgeType::Falling
        } else {
            // Default to both edges if not specified
            HirEdgeType::Both
        }
    }

    /// Extract physical constraints from constraint block
    fn extract_physical_constraints(&self, node: &SyntaxNode) -> Option<PhysicalConstraints> {
        let mut constraints = PhysicalConstraints {
            pin_location: None,
            io_standard: None,
            drive_strength: None,
            slew_rate: None,
            termination: None,
            schmitt_trigger: None,
            bank: None,
            diff_term: None,
        };

        for pair in node
            .children()
            .filter(|c| c.kind() == SyntaxKind::ConstraintPair)
        {
            // Get key - it's the first token (keyword or ident)
            let key_token = pair
                .children_with_tokens()
                .filter_map(|elem| elem.into_token())
                .next();

            if key_token.is_none() {
                continue;
            }

            let key_text = key_token.unwrap().text().to_string();

            // Find the value (third token) - may be string literal or keyword
            let value_token = pair
                .children_with_tokens()
                .filter_map(|elem| elem.into_token())
                .nth(2); // Skip key and colon, get value

            // Extract value text and kind if available
            let (value_text, value_kind) = if let Some(token) = value_token {
                (token.text().to_string(), Some(token.kind()))
            } else {
                (String::new(), None)
            };

            match key_text.as_str() {
                "pin" => {
                    // Remove quotes from string literal
                    constraints.pin_location = Some(PinLocation::Single(
                        value_text.trim_matches('"').to_string(),
                    ));
                }
                "pins" => {
                    // Handle pin array - need to look for PinArray child node
                    for child in pair.children() {
                        if child.kind() == SyntaxKind::PinArray {
                            if let Some(pins) = self.extract_pin_array(&child) {
                                constraints.pin_location = Some(PinLocation::Multiple(pins));
                            }
                        }
                    }
                }
                "pin_p" | "pin_n" => {
                    // Handle differential pairs
                    let pin = value_text.trim_matches('"').to_string();
                    if key_text == "pin_p" {
                        if let Some(negative) = self.find_differential_pair(node, "pin_n") {
                            constraints.pin_location = Some(PinLocation::Differential {
                                positive: pin,
                                negative,
                            });
                        }
                    }
                }
                "io_standard" => {
                    constraints.io_standard = Some(value_text.trim_matches('"').to_string());
                }
                "slew" => {
                    // Debug: see what children we have
                    for child in pair.children() {
                        eprintln!("  child kind: {:?}", child.kind());
                    }
                    eprintln!("  value_kind: {:?}", value_kind);

                    // Look for SlewRate child node or direct keyword
                    if let Some(slew_node) = pair.first_child_of_kind(SyntaxKind::SlewRate) {
                        eprintln!("  Found SlewRate node");
                        if slew_node.first_token_of_kind(SyntaxKind::FastKw).is_some() {
                            constraints.slew_rate = Some(SlewRate::Fast);
                        } else if slew_node.first_token_of_kind(SyntaxKind::SlowKw).is_some() {
                            constraints.slew_rate = Some(SlewRate::Slow);
                        } else if slew_node
                            .first_token_of_kind(SyntaxKind::MediumKw)
                            .is_some()
                        {
                            constraints.slew_rate = Some(SlewRate::Medium);
                        }
                    } else {
                        eprintln!("  No SlewRate node, trying direct keyword");
                        // Try direct keyword
                        match value_kind {
                            Some(SyntaxKind::FastKw) => {
                                eprintln!("  Found FastKw!");
                                constraints.slew_rate = Some(SlewRate::Fast);
                            }
                            Some(SyntaxKind::SlowKw) => {
                                constraints.slew_rate = Some(SlewRate::Slow)
                            }
                            Some(SyntaxKind::MediumKw) => {
                                constraints.slew_rate = Some(SlewRate::Medium)
                            }
                            _ => {
                                eprintln!("  No matching keyword");
                            }
                        }
                    }
                }
                "pull" => {
                    // Look for Termination child node or direct keyword
                    if let Some(term_node) = pair.first_child_of_kind(SyntaxKind::Termination) {
                        if term_node.first_token_of_kind(SyntaxKind::UpKw).is_some() {
                            constraints.termination = Some(Termination::PullUp);
                        } else if term_node.first_token_of_kind(SyntaxKind::DownKw).is_some() {
                            constraints.termination = Some(Termination::PullDown);
                        } else if term_node.first_token_of_kind(SyntaxKind::NoneKw).is_some() {
                            constraints.termination = Some(Termination::None);
                        } else if term_node
                            .first_token_of_kind(SyntaxKind::KeeperKw)
                            .is_some()
                        {
                            constraints.termination = Some(Termination::Keeper);
                        }
                    } else {
                        // Try direct keyword
                        match value_kind {
                            Some(SyntaxKind::UpKw) => {
                                constraints.termination = Some(Termination::PullUp)
                            }
                            Some(SyntaxKind::DownKw) => {
                                constraints.termination = Some(Termination::PullDown)
                            }
                            Some(SyntaxKind::NoneKw) => {
                                constraints.termination = Some(Termination::None)
                            }
                            Some(SyntaxKind::KeeperKw) => {
                                constraints.termination = Some(Termination::Keeper)
                            }
                            _ => {}
                        }
                    }
                }
                "schmitt" => match value_kind {
                    Some(SyntaxKind::TrueKw) => constraints.schmitt_trigger = Some(true),
                    Some(SyntaxKind::FalseKw) => constraints.schmitt_trigger = Some(false),
                    _ => {}
                },
                "bank" => {
                    if let Ok(bank_num) = value_text.parse::<u32>() {
                        constraints.bank = Some(bank_num);
                    }
                }
                "diff_term" => match value_kind {
                    Some(SyntaxKind::TrueKw) => constraints.diff_term = Some(true),
                    Some(SyntaxKind::FalseKw) => constraints.diff_term = Some(false),
                    _ => {}
                },
                _ => {}
            }
        }

        Some(constraints)
    }

    /// Extract string literal from node
    fn extract_string_literal(&self, node: &SyntaxNode) -> Option<String> {
        node.children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|t| t.kind() == SyntaxKind::StringLiteral)
            .map(|t| {
                // Remove quotes
                let text = t.text();
                text.trim_matches('"').to_string()
            })
    }

    /// Extract pin array from node
    fn extract_pin_array(&self, array_node: &SyntaxNode) -> Option<Vec<String>> {
        let mut pins = Vec::new();
        for token in array_node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
        {
            if token.kind() == SyntaxKind::StringLiteral {
                let pin = token.text().trim_matches('"').to_string();
                pins.push(pin);
            }
        }

        if pins.is_empty() {
            None
        } else {
            Some(pins)
        }
    }

    /// Find differential pair partner in constraint block
    fn find_differential_pair(
        &self,
        constraint_block: &SyntaxNode,
        key_name: &str,
    ) -> Option<String> {
        for pair in constraint_block
            .children()
            .filter(|c| c.kind() == SyntaxKind::ConstraintPair)
        {
            let key = pair
                .children_with_tokens()
                .filter_map(|elem| elem.into_token())
                .find(|t| t.kind() == SyntaxKind::Ident)
                .map(|t| t.text().to_string())?;

            if key == key_name {
                let value_node = pair.children().nth(1)?;
                return self.extract_string_literal(&value_node);
            }
        }
        None
    }

    /// Extract slew rate from node
    fn extract_slew_rate(&self, node: &SyntaxNode) -> Option<SlewRate> {
        let slew_node = node.first_child_of_kind(SyntaxKind::SlewRate)?;

        if slew_node.first_token_of_kind(SyntaxKind::FastKw).is_some() {
            Some(SlewRate::Fast)
        } else if slew_node.first_token_of_kind(SyntaxKind::SlowKw).is_some() {
            Some(SlewRate::Slow)
        } else if slew_node
            .first_token_of_kind(SyntaxKind::MediumKw)
            .is_some()
        {
            Some(SlewRate::Medium)
        } else {
            None
        }
    }

    /// Extract termination from node
    fn extract_termination(&self, node: &SyntaxNode) -> Option<Termination> {
        let term_node = node.first_child_of_kind(SyntaxKind::Termination)?;

        if term_node.first_token_of_kind(SyntaxKind::UpKw).is_some() {
            Some(Termination::PullUp)
        } else if term_node.first_token_of_kind(SyntaxKind::DownKw).is_some() {
            Some(Termination::PullDown)
        } else if term_node.first_token_of_kind(SyntaxKind::NoneKw).is_some() {
            Some(Termination::None)
        } else if term_node
            .first_token_of_kind(SyntaxKind::KeeperKw)
            .is_some()
        {
            Some(Termination::Keeper)
        } else {
            None
        }
    }

    /// Extract boolean from node
    fn extract_boolean(&self, node: &SyntaxNode) -> Option<bool> {
        if node.first_token_of_kind(SyntaxKind::TrueKw).is_some() {
            Some(true)
        } else if node.first_token_of_kind(SyntaxKind::FalseKw).is_some() {
            Some(false)
        } else {
            None
        }
    }

    /// Extract integer from node
    fn extract_integer(&self, node: &SyntaxNode) -> Option<i64> {
        node.children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|t| t.kind() == SyntaxKind::IntLiteral)
            .and_then(|t| t.text().parse().ok())
    }

    /// Extract HIR type from node
    fn extract_hir_type(&mut self, node: &SyntaxNode) -> HirType {
        // Look for type nodes in children
        if let Some(type_node) = node.first_child_of_kind(SyntaxKind::TypeExpr) {
            return self.build_hir_type(&type_node);
        }

        // Check for specific type nodes
        for child in node.children() {
            match child.kind() {
                SyntaxKind::BitType => {
                    return self.build_bit_type(&child);
                }
                SyntaxKind::NatType => {
                    return self.build_nat_type(&child);
                }
                SyntaxKind::IntType => {
                    return self.build_int_type(&child);
                }
                SyntaxKind::LogicType => {
                    return self.build_logic_type(&child);
                }
                SyntaxKind::ClockType => {
                    return self.build_clock_type(&child);
                }
                SyntaxKind::ResetType => {
                    return HirType::Reset(None); // TODO: Add domain support
                }
                SyntaxKind::StreamType => {
                    // Stream<T> type - extract inner type
                    if let Some(inner_type_node) = child.children().next() {
                        let inner = Box::new(self.extract_hir_type(&inner_type_node));
                        return HirType::Stream(inner);
                    } else {
                        // Default to Stream<bit[8]> if no inner type specified
                        return HirType::Stream(Box::new(HirType::Bit(8)));
                    }
                }
                SyntaxKind::Fp16Type => {
                    return HirType::Float16;
                }
                SyntaxKind::Fp32Type => {
                    return HirType::Float32;
                }
                SyntaxKind::Fp64Type => {
                    return HirType::Float64;
                }
                SyntaxKind::Vec2Type => {
                    // Extract element type from vec2<T>
                    let element_type = child
                        .children()
                        .map(|c| self.extract_hir_type(&c))
                        .next()
                        .unwrap_or(HirType::Float32); // Default to fp32 if not specified
                    return HirType::Vec2(Box::new(element_type));
                }
                SyntaxKind::Vec3Type => {
                    let element_type = child
                        .children()
                        .map(|c| self.extract_hir_type(&c))
                        .next()
                        .unwrap_or(HirType::Float32);
                    return HirType::Vec3(Box::new(element_type));
                }
                SyntaxKind::Vec4Type => {
                    let element_type = child
                        .children()
                        .map(|c| self.extract_hir_type(&c))
                        .next()
                        .unwrap_or(HirType::Float32);
                    return HirType::Vec4(Box::new(element_type));
                }
                SyntaxKind::ArrayType => {
                    return self.build_array_type(&child);
                }
                SyntaxKind::IdentType | SyntaxKind::CustomType => {
                    if let Some(name) = child.first_token_of_kind(SyntaxKind::Ident) {
                        let type_name = name.text().to_string();
                        // Check if this is a user-defined type (struct, enum, union)
                        if let Some(user_type) = self.symbols.user_types.get(&type_name) {
                            return user_type.clone();
                        }
                        return HirType::Custom(type_name);
                    }
                }
                _ => {}
            }
        }

        // Default to bit[8] if no type found
        HirType::Bit(8)
    }

    /// Build HIR type from type expression node
    fn build_hir_type(&mut self, node: &SyntaxNode) -> HirType {
        // Recursively extract from type expression
        for child in node.children() {
            match child.kind() {
                SyntaxKind::BitType => return self.build_bit_type(&child),
                SyntaxKind::ClockType => return self.build_clock_type(&child),
                SyntaxKind::ResetType => return HirType::Reset(None),
                SyntaxKind::StreamType => {
                    // Stream<T> type
                    if let Some(inner_type_node) = child.children().next() {
                        let inner = Box::new(self.extract_hir_type(&inner_type_node));
                        return HirType::Stream(inner);
                    } else {
                        return HirType::Stream(Box::new(HirType::Bit(8)));
                    }
                }
                _ => {}
            }
        }
        HirType::Bit(8) // Default
    }

    /// Build bit type with width
    fn build_bit_type(&mut self, node: &SyntaxNode) -> HirType {
        // Look for width specification [N] or <N>
        if let Some(width_node) = node.first_child_of_kind(SyntaxKind::WidthSpec) {
            // Look for expression inside width spec
            for child in width_node.children() {
                // Check if it's a simple identifier that might be a generic parameter
                if child.kind() == SyntaxKind::IdentExpr {
                    if let Some(ident_token) = child.first_token_of_kind(SyntaxKind::Ident) {
                        let name = ident_token.text().to_string();
                        // Check if this is a generic parameter
                        if let Some(SymbolId::GenericParam(param_name)) = self.symbols.lookup(&name)
                        {
                            return HirType::BitParam(param_name.clone());
                        }
                    }
                }

                // Try to build as expression - handles literals, identifiers, binary ops, function calls, etc.
                if let Some(expr) = self.build_expression(&child) {
                    // Check if it's a simple case that can be evaluated immediately
                    match &expr {
                        HirExpression::Literal(HirLiteral::Integer(val)) => {
                            // Concrete value
                            return HirType::Bit(*val as u32);
                        }
                        HirExpression::Variable(_) | HirExpression::Constant(_) => {
                            // Single parameter reference - treat as expression for now
                            return HirType::BitExpr(Box::new(expr));
                        }
                        _ => {
                            // Complex expression (binary op, function call, etc.)
                            return HirType::BitExpr(Box::new(expr));
                        }
                    }
                }
            }
        }
        HirType::Bit(1) // Default to single bit
    }

    /// Build nat type with width
    fn build_nat_type(&mut self, node: &SyntaxNode) -> HirType {
        // Look for width specification [N] or <N>
        if let Some(width_node) = node.first_child_of_kind(SyntaxKind::WidthSpec) {
            // Special case: function call is represented as IdentExpr + CallExpr siblings
            let has_ident = width_node
                .first_child_of_kind(SyntaxKind::IdentExpr)
                .is_some();
            let has_call = width_node
                .first_child_of_kind(SyntaxKind::CallExpr)
                .is_some();

            if has_ident && has_call {
                // This is a function call - combine IdentExpr and CallExpr
                if let Some(func_name) = width_node
                    .first_child_of_kind(SyntaxKind::IdentExpr)
                    .and_then(|n| n.first_token_of_kind(SyntaxKind::Ident))
                    .map(|t| t.text().to_string())
                {
                    if let Some(call_node) = width_node.first_child_of_kind(SyntaxKind::CallExpr) {
                        let mut args = Vec::new();
                        for arg_child in call_node.children() {
                            if let Some(arg_expr) = self.build_expression(&arg_child) {
                                args.push(arg_expr);
                            }
                        }

                        let call_expr = HirExpression::Call(HirCallExpr {
                            function: func_name,
                            args,
                        });
                        return HirType::NatExpr(Box::new(call_expr));
                    }
                }
            }

            // Normal case: try to build each child as an expression
            for child in width_node.children() {
                // Try to build as expression
                if let Some(expr) = self.build_expression(&child) {
                    match &expr {
                        HirExpression::Literal(HirLiteral::Integer(val)) => {
                            return HirType::Nat(*val as u32);
                        }
                        HirExpression::Variable(_) | HirExpression::Constant(_) => {
                            return HirType::NatExpr(Box::new(expr));
                        }
                        _ => {
                            return HirType::NatExpr(Box::new(expr));
                        }
                    }
                }
            }
        }
        HirType::Nat(32) // Default to 32-bit unsigned
    }

    /// Build int type with width
    fn build_int_type(&mut self, node: &SyntaxNode) -> HirType {
        // Look for width specification [N] or <N>
        if let Some(width_node) = node.first_child_of_kind(SyntaxKind::WidthSpec) {
            for child in width_node.children() {
                if let Some(expr) = self.build_expression(&child) {
                    match &expr {
                        HirExpression::Literal(HirLiteral::Integer(val)) => {
                            return HirType::Int(*val as u32);
                        }
                        HirExpression::Variable(_) | HirExpression::Constant(_) => {
                            return HirType::IntExpr(Box::new(expr));
                        }
                        _ => {
                            return HirType::IntExpr(Box::new(expr));
                        }
                    }
                }
            }
        }
        HirType::Int(32) // Default to 32-bit signed
    }

    /// Build logic type with width
    fn build_logic_type(&mut self, node: &SyntaxNode) -> HirType {
        // Look for width specification [N] or <N>
        if let Some(width_node) = node.first_child_of_kind(SyntaxKind::WidthSpec) {
            for child in width_node.children() {
                if let Some(expr) = self.build_expression(&child) {
                    match &expr {
                        HirExpression::Literal(HirLiteral::Integer(val)) => {
                            return HirType::Logic(*val as u32);
                        }
                        HirExpression::Variable(_) | HirExpression::Constant(_) => {
                            return HirType::LogicExpr(Box::new(expr));
                        }
                        _ => {
                            return HirType::LogicExpr(Box::new(expr));
                        }
                    }
                }
            }
        }
        HirType::Logic(1) // Default to single bit logic
    }

    /// Build clock type with optional lifetime domain
    fn build_clock_type(&self, node: &SyntaxNode) -> HirType {
        // Look for lifetime token in the clock type
        if let Some(lifetime_token) = node.first_token_of_kind(SyntaxKind::Lifetime) {
            let lifetime_name = lifetime_token.text().trim_start_matches('\'');
            // Look up the lifetime in the symbol table to get a ClockDomainId
            if let Some(domain_id) = self.symbols.clock_domains.get(lifetime_name) {
                return HirType::Clock(Some(*domain_id));
            } else {
                // Create a new clock domain for this lifetime
                // Note: In a full implementation, this would be handled during
                // generic parameter resolution, but for now we create it dynamically
                let domain_id = ClockDomainId(self.symbols.clock_domains.len() as u32);
                // Note: We can't mutate here, so we'll return None for now
                // This will be properly handled when we add generic parameter support
                return HirType::Clock(None);
            }
        } else if let Some(ident_token) = node.first_token_of_kind(SyntaxKind::Ident) {
            // Old syntax: clock(domain_name) - still supported for compatibility
            let domain_name = ident_token.text();
            if let Some(domain_id) = self.symbols.clock_domains.get(domain_name) {
                return HirType::Clock(Some(*domain_id));
            } else {
                return HirType::Clock(None);
            }
        }

        // No domain specified, return untyped clock
        HirType::Clock(None)
    }

    /// Build array type
    fn build_array_type(&mut self, node: &SyntaxNode) -> HirType {
        // Array syntax can be either:
        // 1. Rust-style: [Type; Size] - used by parse_array_type()
        // 2. Postfix style: Type[Size] - created by parse_type() with start_node_at()

        // Collect all children that are types or expressions
        let children: Vec<_> = node.children().collect();

        // For postfix style Type[Size], the structure is:
        // ArrayType
        //   ├─ TypeAnnotation (contains the element type like NatType[8])
        //   └─ LiteralExpr/Expression (the array size)

        // For Rust style [Type; Size], the structure is:
        // ArrayType
        //   ├─ TypeAnnotation/Type (element type)
        //   └─ Expression (array size)

        // Try to find element type and size
        let elem_type = if let Some(first) = children.first() {
            // The first child should be the element type or TypeAnnotation containing it
            Box::new(self.extract_hir_type(first))
        } else {
            Box::new(HirType::Bit(8))
        };

        // Second child (if exists) is the size
        if let Some(size_node) = children.get(1) {
            // Try to parse as literal first
            if let Some(lit) = size_node.first_token_of_kind(SyntaxKind::IntLiteral) {
                let size = lit.text().parse::<u32>().unwrap_or(1);
                return HirType::Array(elem_type, size);
            }

            // Otherwise, it's an expression (IdentExpr, BinaryExpr, CallExpr, etc.)
            if let Some(size_expr) = self.build_expression(size_node) {
                return HirType::ArrayExpr(elem_type, Box::new(size_expr));
            }
        }

        // Fallback: array of size 1
        HirType::Array(elem_type, 1)
    }

    /// Find initial value expression
    fn find_initial_value_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // Look for expression after '='
        let mut found_assign = false;
        for child in node.children() {
            if found_assign {
                return self.build_expression(&child);
            }
            if child
                .children_with_tokens()
                .any(|e| e.kind() == SyntaxKind::Assign)
            {
                found_assign = true;
            }
        }
        None
    }

    /// Determine assignment type from operator
    fn determine_assignment_type(&self, node: &SyntaxNode) -> HirAssignmentType {
        if node
            .children_with_tokens()
            .any(|e| e.kind() == SyntaxKind::NonBlockingAssign)
        {
            HirAssignmentType::NonBlocking
        } else if node
            .children_with_tokens()
            .any(|e| e.kind() == SyntaxKind::BlockingAssign)
        {
            HirAssignmentType::Blocking
        } else {
            HirAssignmentType::Combinational
        }
    }

    /// Convert token to binary operator
    fn token_to_binary_op(&self, kind: SyntaxKind) -> Option<HirBinaryOp> {
        match kind {
            SyntaxKind::Plus => Some(HirBinaryOp::Add),
            SyntaxKind::Minus => Some(HirBinaryOp::Sub),
            SyntaxKind::Star => Some(HirBinaryOp::Mul),
            SyntaxKind::Slash => Some(HirBinaryOp::Div),
            SyntaxKind::Percent => Some(HirBinaryOp::Mod),
            SyntaxKind::Amp => Some(HirBinaryOp::And),
            SyntaxKind::Pipe => Some(HirBinaryOp::Or),
            SyntaxKind::Caret => Some(HirBinaryOp::Xor),
            SyntaxKind::Eq => Some(HirBinaryOp::Equal),
            SyntaxKind::Neq => Some(HirBinaryOp::NotEqual),
            SyntaxKind::Lt => Some(HirBinaryOp::Less),
            SyntaxKind::Le => Some(HirBinaryOp::LessEqual),
            SyntaxKind::Gt => Some(HirBinaryOp::Greater),
            SyntaxKind::Ge => Some(HirBinaryOp::GreaterEqual),
            SyntaxKind::AmpAmp => Some(HirBinaryOp::LogicalAnd),
            SyntaxKind::PipePipe => Some(HirBinaryOp::LogicalOr),
            SyntaxKind::Shl => Some(HirBinaryOp::LeftShift),
            SyntaxKind::Shr => Some(HirBinaryOp::RightShift),
            _ => None,
        }
    }

    /// Convert token to unary operator
    fn token_to_unary_op(&self, kind: SyntaxKind) -> Option<HirUnaryOp> {
        match kind {
            SyntaxKind::Bang => Some(HirUnaryOp::Not),
            SyntaxKind::Minus => Some(HirUnaryOp::Negate),
            SyntaxKind::Tilde => Some(HirUnaryOp::BitwiseNot),
            _ => None,
        }
    }

    /// Build struct type from syntax node
    fn build_struct_type(&mut self, node: &SyntaxNode) -> Option<HirStructType> {
        let name = self.extract_name(node)?;
        let mut fields = Vec::new();

        // Find the field list
        if let Some(field_list) = node.first_child_of_kind(SyntaxKind::StructFieldList) {
            for field_node in field_list.children_of_kind(SyntaxKind::StructField) {
                if let Some(field) = self.build_struct_field(&field_node) {
                    fields.push(field);
                }
            }
        }

        Some(HirStructType {
            name,
            fields,
            packed: false, // TODO: Check for packed attribute
        })
    }

    /// Build type from annotation (for struct/union fields)
    fn build_type_from_annotation(&mut self, node: &SyntaxNode) -> Option<HirType> {
        // Look for TypeAnnotation node
        if let Some(type_node) = node.first_child_of_kind(SyntaxKind::TypeAnnotation) {
            Some(self.extract_hir_type(&type_node))
        } else {
            // Try to extract from the node itself
            Some(self.extract_hir_type(node))
        }
    }

    /// Build struct field from syntax node
    fn build_struct_field(&mut self, node: &SyntaxNode) -> Option<HirStructField> {
        let name = self.extract_name(node)?;
        let field_type = self.build_type_from_annotation(node)?;

        Some(HirStructField { name, field_type })
    }

    /// Build enum type from syntax node
    fn build_enum_type(&mut self, node: &SyntaxNode) -> Option<HirEnumType> {
        let name = self.extract_name(node)?;
        let mut variants = Vec::new();

        // Find the variant list
        if let Some(variant_list) = node.first_child_of_kind(SyntaxKind::EnumVariantList) {
            for variant_node in variant_list.children_of_kind(SyntaxKind::EnumVariant) {
                if let Some(variant) = self.build_enum_variant(&variant_node) {
                    variants.push(variant);
                }
            }
        }

        // Default base type is nat[32]
        let base_type = Box::new(HirType::Nat(32));

        Some(HirEnumType {
            name,
            variants,
            base_type,
        })
    }

    /// Build enum variant from syntax node
    fn build_enum_variant(&mut self, node: &SyntaxNode) -> Option<HirEnumVariant> {
        let name = self.extract_name(node)?;

        // TODO: Parse explicit values for enum variants
        // For now, values will be auto-assigned
        let value = None;

        Some(HirEnumVariant { name, value })
    }

    /// Build union type from syntax node
    fn build_union_type(&mut self, node: &SyntaxNode) -> Option<HirUnionType> {
        let name = self.extract_name(node)?;
        let mut fields = Vec::new();

        // Find the field list
        if let Some(field_list) = node.first_child_of_kind(SyntaxKind::UnionFieldList) {
            for field_node in field_list.children_of_kind(SyntaxKind::UnionField) {
                // Union fields are the same as struct fields
                if let Some(field) = self.build_struct_field(&field_node) {
                    fields.push(field);
                }
            }
        }

        Some(HirUnionType {
            name,
            fields,
            packed: false, // TODO: Check for packed attribute
        })
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

    fn next_function_id(&mut self) -> FunctionId {
        let id = FunctionId(self.next_function_id);
        self.next_function_id += 1;
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

    fn next_assertion_id(&mut self) -> AssertionId {
        let id = AssertionId(self.next_assertion_id);
        self.next_assertion_id += 1;
        id
    }

    fn next_property_id(&mut self) -> PropertyId {
        let id = PropertyId(self.next_property_id);
        self.next_property_id += 1;
        id
    }

    fn next_cover_id(&mut self) -> CoverId {
        let id = CoverId(self.next_cover_id);
        self.next_cover_id += 1;
        id
    }

    fn next_import_id(&mut self) -> ImportId {
        let id = ImportId(self.next_import_id);
        self.next_import_id += 1;
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
            user_types: HashMap::new(),
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

impl HirBuilderContext {
    /// Build trait definition
    fn build_trait_def(&mut self, node: &SyntaxNode) -> Option<HirTraitDefinition> {
        let name = self.extract_name(node)?;

        // Parse generic parameters
        let generics =
            if let Some(generic_list) = node.first_child_of_kind(SyntaxKind::GenericParamList) {
                self.parse_generic_params(&generic_list)
            } else {
                Vec::new()
            };

        // Parse trait items
        let mut methods = Vec::new();
        let mut associated_types = Vec::new();
        let mut associated_constants = Vec::new();

        if let Some(trait_items) = node.first_child_of_kind(SyntaxKind::TraitItemList) {
            for item in trait_items.children() {
                match item.kind() {
                    SyntaxKind::TraitMethod => {
                        if let Some(method) = self.build_trait_method(&item) {
                            methods.push(method);
                        }
                    }
                    SyntaxKind::TraitType => {
                        if let Some(assoc_type) = self.build_trait_associated_type(&item) {
                            associated_types.push(assoc_type);
                        }
                    }
                    SyntaxKind::TraitConst => {
                        if let Some(assoc_const) = self.build_trait_associated_const(&item) {
                            associated_constants.push(assoc_const);
                        }
                    }
                    _ => {}
                }
            }
        }

        Some(HirTraitDefinition {
            name,
            generics,
            methods,
            associated_types,
            associated_constants,
        })
    }

    /// Build trait implementation
    fn build_trait_impl(&mut self, node: &SyntaxNode) -> Option<HirTraitImplementation> {
        // Extract trait name and target type from tokens
        let tokens: Vec<_> = node
            .children_with_tokens()
            .filter_map(|element| element.into_token())
            .collect();

        let ident_tokens: Vec<_> = tokens
            .iter()
            .filter(|token| token.kind() == SyntaxKind::Ident)
            .collect();

        if ident_tokens.len() < 2 {
            return None;
        }

        let trait_name = ident_tokens[0].text().to_string();
        let target_type = ident_tokens[1].text().to_string();

        // Look up target entity
        let target_entity = *self.symbols.entities.get(&target_type)?;

        // Parse implementation items
        let mut method_implementations = Vec::new();
        let mut type_implementations = Vec::new();
        let mut const_implementations = Vec::new();

        if let Some(trait_items) = node.first_child_of_kind(SyntaxKind::TraitItemList) {
            for item in trait_items.children() {
                match item.kind() {
                    SyntaxKind::TraitMethod => {
                        if let Some(method_impl) = self.build_trait_method_impl(&item) {
                            method_implementations.push(method_impl);
                        }
                    }
                    SyntaxKind::TraitType => {
                        if let Some(type_impl) = self.build_trait_type_impl(&item) {
                            type_implementations.push(type_impl);
                        }
                    }
                    SyntaxKind::TraitConst => {
                        if let Some(const_impl) = self.build_trait_const_impl(&item) {
                            const_implementations.push(const_impl);
                        }
                    }
                    _ => {}
                }
            }
        }

        Some(HirTraitImplementation {
            trait_name,
            target_entity,
            method_implementations,
            type_implementations,
            const_implementations,
        })
    }

    /// Parse generic parameters from list
    fn parse_generic_params(&mut self, node: &SyntaxNode) -> Vec<HirGeneric> {
        let mut generics = Vec::new();

        for param_node in node.children_of_kind(SyntaxKind::GenericParam) {
            if let Some(generic) = self.build_generic_param(&param_node) {
                generics.push(generic);
            }
        }

        generics
    }

    /// Build generic parameter
    fn build_generic_param(&mut self, node: &SyntaxNode) -> Option<HirGeneric> {
        // Check if it's a lifetime parameter ('clk)
        if let Some(lifetime_token) = node.first_token_of_kind(SyntaxKind::Lifetime) {
            // 'clk style lifetime parameter with new token
            // Strip the leading apostrophe from the lifetime name
            let name = lifetime_token.text().trim_start_matches('\'').to_string();

            Some(HirGeneric {
                name,
                param_type: HirGenericType::ClockDomain,
                default_value: None,
            })
        }
        // Legacy support for apostrophe + ident
        else if node.first_token_of_kind(SyntaxKind::Apostrophe).is_some() {
            // 'clk style lifetime parameter (legacy)
            let name = node
                .children_with_tokens()
                .filter_map(|elem| elem.into_token())
                .find(|t| t.kind() == SyntaxKind::Ident)
                .map(|t| t.text().to_string())?;

            Some(HirGeneric {
                name,
                param_type: HirGenericType::ClockDomain,
                default_value: None,
            })
        }
        // Check if it's an intent parameter (intent I: Intent)
        else if node.first_token_of_kind(SyntaxKind::IntentKw).is_some() {
            // intent I: Intent style parameter
            let name = node
                .children_with_tokens()
                .filter_map(|elem| elem.into_token())
                .find(|t| t.kind() == SyntaxKind::Ident)
                .map(|t| t.text().to_string())?;

            // Extract default value if present (e.g., intent I: Intent = Intent::default())
            let default_value = node.children().nth(1).and_then(|child| {
                if child.kind() == SyntaxKind::LiteralExpr {
                    self.build_expression(&child)
                } else {
                    None
                }
            });

            Some(HirGeneric {
                name,
                param_type: HirGenericType::Intent,
                default_value,
            })
        }
        // Check if it's a const parameter
        else if node.first_token_of_kind(SyntaxKind::ConstKw).is_some() {
            // const N: nat[32] style parameter
            let name = node
                .children_with_tokens()
                .filter_map(|elem| elem.into_token())
                .find(|t| t.kind() == SyntaxKind::Ident)
                .map(|t| t.text().to_string())?;

            let param_type = self.extract_hir_type(node);

            // Extract default value if present (e.g., const WIDTH: nat = 8)
            // The parser structure is: GenericParam -> TypeAnnotation, LiteralExpr
            // If there's a second child, it's the default value
            let default_value = node.children().nth(1).and_then(|child| {
                if child.kind() == SyntaxKind::LiteralExpr {
                    self.build_expression(&child)
                } else {
                    None
                }
            });

            Some(HirGeneric {
                name,
                param_type: HirGenericType::Const(param_type),
                default_value,
            })
        } else {
            // Regular type parameter (e.g., WIDTH: nat = 8)
            let name = node
                .first_token_of_kind(SyntaxKind::Ident)
                .map(|t| t.text().to_string())?;

            // Check for default value (= expression after type)
            let default_value = self.find_initial_value_expr(node);

            Some(HirGeneric {
                name,
                param_type: HirGenericType::Type,
                default_value,
            })
        }
    }

    /// Build trait method
    fn build_trait_method(&mut self, node: &SyntaxNode) -> Option<HirTraitMethod> {
        let name = self.extract_name(node)?;

        // Parse parameters
        let parameters = self.parse_method_parameters(node);

        // Parse return type
        let return_type = if node.first_token_of_kind(SyntaxKind::Arrow).is_some() {
            Some(self.extract_hir_type(node))
        } else {
            None
        };

        // Check if it has a default implementation
        let default_implementation = node
            .first_child_of_kind(SyntaxKind::BlockStmt)
            .map(|block| self.build_statements(&block));

        Some(HirTraitMethod {
            name,
            parameters,
            return_type,
            default_implementation,
        })
    }

    /// Parse method parameters
    fn parse_method_parameters(&mut self, node: &SyntaxNode) -> Vec<HirParameter> {
        let mut parameters = Vec::new();

        // Look for Parameter nodes within the method node
        for child in node.children() {
            if child.kind() == SyntaxKind::Parameter {
                if let Some(param) = self.build_parameter(&child) {
                    parameters.push(param);
                }
            }
        }

        parameters
    }

    /// Build a single parameter from AST
    fn build_parameter(&mut self, node: &SyntaxNode) -> Option<HirParameter> {
        // Extract parameter name
        let name = self.extract_name(node)?;

        // Extract parameter type
        let param_type = self.extract_hir_type(node);

        // Look for default value (rare in trait methods but possible in implementations)
        let default_value = None; // Default values are not typically supported in parameters

        Some(HirParameter {
            name,
            param_type,
            default_value,
        })
    }

    /// Build trait associated type
    fn build_trait_associated_type(&mut self, node: &SyntaxNode) -> Option<HirTraitAssociatedType> {
        let name = self.extract_name(node)?;

        // Parse bounds and default type
        let bounds = self.extract_trait_bounds(node);
        let default_type = self.extract_default_type(node);

        Some(HirTraitAssociatedType {
            name,
            bounds,
            default_type,
        })
    }

    /// Extract trait bounds from a node
    fn extract_trait_bounds(&mut self, node: &SyntaxNode) -> Vec<String> {
        let mut bounds = Vec::new();

        // Look for TraitBoundList child
        for child in node.children() {
            if child.kind() == SyntaxKind::TraitBoundList {
                // Iterate through TraitBound children
                for bound_node in child.children() {
                    if bound_node.kind() == SyntaxKind::TraitBound {
                        // Extract the trait name from the bound
                        for token in bound_node.children_with_tokens() {
                            if let Some(ident_token) = token.as_token() {
                                if ident_token.kind() == SyntaxKind::Ident {
                                    bounds.push(ident_token.text().to_string());
                                    break; // Only take the first identifier (trait name)
                                }
                            }
                        }
                    }
                }
            }
        }

        bounds
    }

    /// Extract default type from associated type declaration
    fn extract_default_type(&mut self, node: &SyntaxNode) -> Option<HirType> {
        // Look for an assignment followed by a type
        let mut found_assign = false;
        for child in node.children_with_tokens() {
            if let Some(token) = child.as_token() {
                if token.kind() == SyntaxKind::Assign {
                    found_assign = true;
                }
            } else if let Some(node) = child.as_node() {
                if found_assign && node.kind() == SyntaxKind::TypeAnnotation {
                    return Some(self.extract_hir_type(node));
                }
            }
        }
        None
    }

    /// Build trait associated constant
    fn build_trait_associated_const(
        &mut self,
        node: &SyntaxNode,
    ) -> Option<HirTraitAssociatedConst> {
        let name = self.extract_name(node)?;
        let const_type = self.extract_hir_type(node);

        // Parse default value if present
        let default_value = self.find_initial_value_expr(node);

        Some(HirTraitAssociatedConst {
            name,
            const_type,
            default_value,
        })
    }

    /// Build trait method implementation
    fn build_trait_method_impl(&mut self, node: &SyntaxNode) -> Option<HirTraitMethodImpl> {
        let name = self.extract_name(node)?;
        let body = if let Some(block) = node.first_child_of_kind(SyntaxKind::BlockStmt) {
            self.build_statements(&block)
        } else {
            Vec::new()
        };

        Some(HirTraitMethodImpl { name, body })
    }

    /// Build trait type implementation
    fn build_trait_type_impl(&mut self, node: &SyntaxNode) -> Option<HirTraitTypeImpl> {
        let name = self.extract_name(node)?;
        let implementation = self.extract_hir_type(node);

        Some(HirTraitTypeImpl {
            name,
            implementation,
        })
    }

    /// Build trait const implementation
    fn build_trait_const_impl(&mut self, node: &SyntaxNode) -> Option<HirTraitConstImpl> {
        let name = self.extract_name(node)?;
        let value = self.find_initial_value_expr(node)?;

        Some(HirTraitConstImpl { name, value })
    }

    /// Infer clock domains for signals based on event block assignments
    fn infer_clock_domains(&mut self, implementation: &mut HirImplementation) {
        // Create a map to track which signals are assigned in which clock domains
        let mut signal_domains: std::collections::HashMap<SignalId, ClockDomainId> =
            std::collections::HashMap::new();

        // Go through each event block and track assignments
        for event_block in &implementation.event_blocks {
            // Determine the clock domain for this event block based on its triggers
            if let Some(clock_domain) = self.get_event_block_clock_domain(event_block) {
                // Collect all signals assigned in this event block
                let assigned_signals = self.collect_assigned_signals(&event_block.statements);

                // Associate these signals with this clock domain
                for signal_id in assigned_signals {
                    signal_domains.insert(signal_id, clock_domain);
                }
            }
        }

        // Update signal clock domains
        for signal in &mut implementation.signals {
            if let Some(domain_id) = signal_domains.get(&signal.id) {
                signal.clock_domain = Some(*domain_id);
            }
        }
    }

    /// Get the clock domain for an event block based on its triggers
    fn get_event_block_clock_domain(&self, event_block: &HirEventBlock) -> Option<ClockDomainId> {
        // For now, create a simple mapping - each unique clock signal gets its own domain
        // In a more sophisticated implementation, this would use the generic parameters
        for trigger in &event_block.triggers {
            match &trigger.signal {
                HirEventSignal::Port(port_id) => {
                    // For clock ports, create/reuse a clock domain
                    // This is a simplified approach - in reality we'd look up the port type
                    return Some(ClockDomainId(port_id.0));
                }
                HirEventSignal::Signal(_) => {
                    // For signal triggers, would need more complex analysis
                    continue;
                }
            }
        }
        None
    }

    /// Collect all signals that are assigned to in a list of statements
    #[allow(clippy::only_used_in_recursion)]
    fn collect_assigned_signals(&self, statements: &[HirStatement]) -> Vec<SignalId> {
        let mut signals = Vec::new();

        for statement in statements {
            match statement {
                HirStatement::Assignment(assignment) => {
                    if let HirLValue::Signal(signal_id) = &assignment.lhs {
                        signals.push(*signal_id);
                    }
                }
                HirStatement::If(if_stmt) => {
                    // Recursively collect from if/else branches
                    signals.extend(self.collect_assigned_signals(&if_stmt.then_statements));
                    if let Some(else_stmts) = &if_stmt.else_statements {
                        signals.extend(self.collect_assigned_signals(else_stmts));
                    }
                }
                HirStatement::Match(match_stmt) => {
                    // Recursively collect from match arms
                    for arm in &match_stmt.arms {
                        signals.extend(self.collect_assigned_signals(&arm.statements));
                    }
                }
                HirStatement::Block(block_stmts) => {
                    // Recursively collect from block statements
                    signals.extend(self.collect_assigned_signals(block_stmts));
                }
                _ => {} // Other statement types don't assign to signals
            }
        }

        signals
    }
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
