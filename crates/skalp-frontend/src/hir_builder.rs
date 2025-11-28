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
const BUILTIN_FUNCTIONS: &[&str] = &[
    // Logarithmic & Exponential
    "clog2",
    "pow2",
    "pow",
    // Bit Manipulation
    "popcount",
    "clz",
    "ctz",
    "reverse_bits",
    "is_power_of_2",
    // Arithmetic
    "min",
    "max",
    "abs",
    "gcd",
    "lcm",
    // Gray Code
    "gray_encode",
    "gray_decode",
];

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

    /// Intent definitions: maps intent name to its MuxStyle constraint
    /// Built from `intent parallel = mux_style::parallel;` declarations
    intent_mux_styles: HashMap<String, MuxStyle>,

    /// Pending MuxStyle hint from most recent attribute
    /// Set when we see `#[parallel]` before a statement
    pending_mux_style: Option<MuxStyle>,
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

    /// Types of let-bound variables (for type inference in tuple destructuring)
    variable_types: HashMap<VariableId, HirType>,

    /// BUG FIX #5: Signal types for type inference
    signal_types: HashMap<SignalId, HirType>,

    /// BUG FIX #5: Port types for type inference
    port_types: HashMap<PortId, HirType>,

    /// Function return types (for type inference of function call expressions)
    /// BUG FIX #67: Track function return types to properly infer tuple types
    function_return_types: HashMap<String, HirType>,

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
            intent_mux_styles: HashMap::new(),
            pending_mux_style: None,
        }
    }

    /// Pre-register entities from merged HIR (for handling imports)
    pub fn preregister_entity(&mut self, entity: &HirEntity) {
        self.symbols.entities.insert(entity.name.clone(), entity.id);
        self.built_entities
            .insert(entity.name.clone(), entity.clone());
    }

    /// Pre-register function return types from merged HIR (for handling imports)
    /// BUG FIX #67: Register imported function return types for type inference
    pub fn preregister_function(&mut self, function: &HirFunction) {
        if let Some(ref return_type) = function.return_type {
            eprintln!(
                "\u{1f4e6} BUG #67 FIX: Pre-registering imported function '{}' with return type: {:?}",
                function.name, return_type
            );
            self.symbols
                .function_return_types
                .insert(function.name.clone(), return_type.clone());
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
                        let name = struct_type.name.clone();
                        let type_def = HirType::Struct(struct_type);

                        // Extract visibility
                        let visibility = if child.children_with_tokens().any(|c| {
                            c.as_token()
                                .map(|t| t.kind() == SyntaxKind::PubKw)
                                .unwrap_or(false)
                        }) {
                            HirVisibility::Public
                        } else {
                            HirVisibility::Private
                        };

                        // Store struct type for later reference in symbol table
                        self.symbols
                            .user_types
                            .insert(name.clone(), type_def.clone());

                        // Add to HIR for export
                        hir.user_defined_types.push(HirUserDefinedType {
                            name,
                            visibility,
                            type_def,
                        });
                    }
                }
                SyntaxKind::EnumDecl => {
                    if let Some(enum_type) = self.build_enum_type(&child) {
                        let name = enum_type.name.clone();
                        let type_def = HirType::Enum(Box::new(enum_type));

                        // Extract visibility
                        let visibility = if child.children_with_tokens().any(|c| {
                            c.as_token()
                                .map(|t| t.kind() == SyntaxKind::PubKw)
                                .unwrap_or(false)
                        }) {
                            HirVisibility::Public
                        } else {
                            HirVisibility::Private
                        };

                        // Store enum type for later reference in symbol table
                        self.symbols
                            .user_types
                            .insert(name.clone(), type_def.clone());

                        // Add to HIR for export
                        hir.user_defined_types.push(HirUserDefinedType {
                            name,
                            visibility,
                            type_def,
                        });
                    }
                }
                SyntaxKind::UnionDecl => {
                    if let Some(union_type) = self.build_union_type(&child) {
                        let name = union_type.name.clone();
                        let type_def = HirType::Union(union_type);

                        // Extract visibility
                        let visibility = if child.children_with_tokens().any(|c| {
                            c.as_token()
                                .map(|t| t.kind() == SyntaxKind::PubKw)
                                .unwrap_or(false)
                        }) {
                            HirVisibility::Public
                        } else {
                            HirVisibility::Private
                        };

                        // Store union type for later reference in symbol table
                        self.symbols
                            .user_types
                            .insert(name.clone(), type_def.clone());

                        // Add to HIR for export
                        hir.user_defined_types.push(HirUserDefinedType {
                            name,
                            visibility,
                            type_def,
                        });
                    }
                }
                SyntaxKind::ConstantDecl => {
                    if let Some(constant) = self.build_constant(&child) {
                        // Store constant in HIR for later use
                        // Top-level constants are added to a special "global" implementation
                        // For now, we'll create a default impl if none exists
                        if hir.implementations.is_empty() {
                            hir.implementations.push(HirImplementation {
                                entity: EntityId(0), // Dummy entity ID for global scope
                                signals: Vec::new(),
                                variables: Vec::new(),
                                constants: Vec::new(),
                                functions: Vec::new(),
                                event_blocks: Vec::new(),
                                assignments: Vec::new(),
                                instances: Vec::new(),
                                covergroups: Vec::new(),
                                formal_blocks: Vec::new(),
                            });
                        }
                        // Add constant to the first implementation (global scope)
                        if let Some(impl_block) = hir.implementations.first_mut() {
                            impl_block.constants.push(constant);
                        }
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
                SyntaxKind::TypeAlias => {
                    if let Some(type_alias) = self.build_type_alias(&child) {
                        hir.type_aliases.push(type_alias);
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
        let mut signals = Vec::new();
        let mut assignments = Vec::new();

        if let Some(port_list) = node.first_child_of_kind(SyntaxKind::PortList) {
            // First pass: build ports
            for port_node in port_list.children_of_kind(SyntaxKind::PortDecl) {
                if let Some(port) = self.build_port(&port_node) {
                    ports.push(port);
                }
            }

            // Second pass: build entity body items (signals, assignments, let bindings)
            for child in port_list.children() {
                match child.kind() {
                    SyntaxKind::SignalDecl => {
                        if let Some(signal) = self.build_signal(&child) {
                            signals.push(signal);
                        }
                    }
                    SyntaxKind::AssignmentStmt => {
                        eprintln!("[HIR_ENTITY_DEBUG] Processing entity body AssignmentStmt");

                        // Check if this is a tuple destructuring assignment (LHS is a TupleExpr)
                        if let Some(tuple_expr) = child
                            .children()
                            .find(|n| n.kind() == SyntaxKind::TupleExpr)
                        {
                            eprintln!("[HIR_ENTITY_DEBUG] Detected tuple destructuring");
                            // Extract signal names from tuple expression
                            let signal_names: Vec<String> = tuple_expr
                                .children()
                                .filter(|n| n.kind() == SyntaxKind::IdentExpr)
                                .filter_map(|n| {
                                    n.children_with_tokens()
                                        .filter_map(|elem| elem.into_token())
                                        .find(|t| t.kind() == SyntaxKind::Ident)
                                        .map(|t| t.text().to_string())
                                })
                                .collect();

                            // Extract RHS expression
                            // Skip the first child (LHS TupleExpr) and collect all RHS nodes
                            // Parser creates function calls as: IdentExpr + CallExpr siblings
                            // Use build_chained_rhs_expression to combine them
                            eprintln!("[HIR_ENTITY_DEBUG] Assignment statement children:");
                            for (i, ch) in child.children().enumerate() {
                                eprintln!("  [{}] {:?}", i, ch.kind());
                            }

                            // Build the RHS by combining IdentExpr + CallExpr siblings
                            let rhs_nodes: Vec<_> = child.children().skip(1).collect();
                            eprintln!("[HIR_ENTITY_DEBUG] Building RHS from {} nodes", rhs_nodes.len());

                            let rhs_expr = if rhs_nodes.len() == 2
                                && rhs_nodes[0].kind() == SyntaxKind::IdentExpr
                                && rhs_nodes[1].kind() == SyntaxKind::CallExpr
                            {
                                // IdentExpr + CallExpr pattern: function call
                                // Extract function name from IdentExpr
                                let func_name = rhs_nodes[0]
                                    .children_with_tokens()
                                    .filter_map(|elem| elem.into_token())
                                    .find(|t| t.kind() == SyntaxKind::Ident)
                                    .map(|t| t.text().to_string());

                                // Extract arguments from CallExpr
                                let call_node = &rhs_nodes[1];
                                let args: Vec<HirExpression> = call_node
                                    .children()
                                    .filter_map(|arg_node| self.build_expression(&arg_node))
                                    .collect();

                                func_name.map(|name| HirExpression::Call(HirCallExpr {
                                    function: name,
                                    type_args: vec![],
                                    args,
                                }))
                            } else {
                                None
                            };

                            if let Some(rhs_expr) = rhs_expr {
                                eprintln!("[HIR_ENTITY_DEBUG] Expanding tuple destructuring into {} assignments", signal_names.len());
                                eprintln!("[HIR_ENTITY_DEBUG] RHS expression type: {:?}", std::mem::discriminant(&rhs_expr));
                                    // Create an assignment for each signal: signal_i = rhs.i
                                    for (idx, signal_name) in signal_names.iter().enumerate() {
                                        // Find the signal ID by name
                                        if let Some(&signal_id) = self.symbols.signals.get(signal_name) {
                                            let assignment = HirAssignment {
                                                id: self.next_assignment_id(),
                                                lhs: HirLValue::Signal(signal_id),
                                                assignment_type: HirAssignmentType::Combinational,
                                                rhs: HirExpression::FieldAccess {
                                                    base: Box::new(rhs_expr.clone()),
                                                    field: idx.to_string(),
                                                },
                                            };
                                            assignments.push(assignment);
                                        }
                                    }
                            }
                        } else if let Some(assignment) =
                            self.build_assignment(&child, HirAssignmentType::Combinational)
                        {
                            eprintln!("[HIR_ENTITY_DEBUG] Assignment built successfully");
                            assignments.push(assignment);
                        }
                    }
                    SyntaxKind::LetStmt => {
                        // Let bindings in entity bodies are treated as combinational assignments
                        // Convert let statements to signals + assignments
                        let let_stmts = self.build_let_statements_from_node(&child);
                        for stmt in let_stmts {
                            if let HirStatement::Let(let_stmt) = stmt {
                                // Create a signal for the let binding
                                let signal = HirSignal {
                                    id: SignalId(let_stmt.id.0), // Reuse the variable ID as signal ID
                                    name: let_stmt.name.clone(),
                                    signal_type: let_stmt.var_type.clone(),
                                    initial_value: None,
                                    clock_domain: None,
                                };
                                signals.push(signal);

                                // Create a combinational assignment for the initialization
                                let assignment = HirAssignment {
                                    id: self.next_assignment_id(),
                                    lhs: HirLValue::Signal(SignalId(let_stmt.id.0)),
                                    assignment_type: HirAssignmentType::Combinational,
                                    rhs: let_stmt.value,
                                };
                                assignments.push(assignment);
                            }
                        }
                    }
                    _ => {} // Ports are already handled, ignore others
                }
            }
        }

        // Build clock domains - look for lifetime-like annotations
        let mut clock_domains = Vec::new();

        // For now, extract any clock domain from port types
        // In the future, this should parse explicit clock domain parameters
        let mut seen_domains = std::collections::HashSet::new();
        for port in &ports {
            let domain_id = match &port.port_type {
                HirType::Clock(Some(domain_id)) => Some(*domain_id),
                HirType::Reset {
                    clock_domain: Some(domain_id),
                    ..
                } => Some(*domain_id),
                _ => None,
            };

            if let Some(domain_id) = domain_id {
                if seen_domains.insert(domain_id) {
                    clock_domains.push(HirClockDomain {
                        id: domain_id,
                        name: format!("clk_{}", domain_id.0),
                    });
                }
            }
        }

        eprintln!("[HIR_ENTITY_DEBUG] Built entity '{}' with {} signals and {} assignments", name, signals.len(), assignments.len());

        Some(HirEntity {
            id,
            name,
            visibility: crate::hir::HirVisibility::Private,
            ports,
            generics,
            clock_domains,
            signals,
            assignments,
        })
    }

    /// Build port from syntax node
    fn build_port(&mut self, node: &SyntaxNode) -> Option<HirPort> {
        let id = self.next_port_id();
        // Use extract_name_allow_keywords for ports since keywords are valid port names
        let name = self.extract_name_allow_keywords(node)?;

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
        self.symbols.port_types.insert(id, port_type.clone()); // BUG FIX #5
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
                    eprintln!("[HIR_IMPL_DEBUG] Processing AssignmentStmt");
                    if let Some(assignment) =
                        self.build_assignment(&child, HirAssignmentType::Combinational)
                    {
                        eprintln!("[HIR_IMPL_DEBUG] Assignment built successfully, pushing to assignments vector");
                        assignments.push(assignment);
                    } else {
                        eprintln!("[HIR_IMPL_DEBUG] build_assignment returned None!");
                    }
                }
                SyntaxKind::LetStmt => {
                    // Let bindings in impl blocks are treated as variables with combinational assignments
                    // Handle both simple let and tuple destructuring
                    let let_stmts = self.build_let_statements_from_node(&child);
                    for stmt in let_stmts {
                        if let HirStatement::Let(let_stmt) = stmt {
                            // BUG #65/#66 DEBUG: Log if creating variable with Float16 type
                            if matches!(let_stmt.var_type, HirType::Float16) {
                                eprintln!("[BUG #65/#66 FOUND IT IN HIR!] Creating HIR variable '{}' with Float16 type", let_stmt.name);
                            }

                            // Create a variable for the let binding
                            let variable = HirVariable {
                                id: let_stmt.id,
                                name: let_stmt.name.clone(),
                                var_type: let_stmt.var_type.clone(),
                                initial_value: None,
                            };

                            // Variables are registered in symbol table by build_let_statement()
                            // (this happens inside build_let_statements_from_node)
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

    /// Convert generic argument expression based on parameter type
    fn convert_generic_arg_expr(
        &self,
        expr: HirExpression,
        param_type: &HirGenericType,
    ) -> HirExpression {
        match param_type {
            HirGenericType::Type => {
                // For type parameters, convert identifier/generic param to type reference
                match &expr {
                    HirExpression::GenericParam(name) => {
                        // Wrap the type in a Cast expression so the collector can extract it
                        HirExpression::Cast(HirCastExpr {
                            expr: Box::new(HirExpression::Literal(HirLiteral::Integer(0))),
                            target_type: HirType::Custom(name.clone()),
                        })
                    }
                    _ => expr, // Already a proper expression (e.g., from parse_type)
                }
            }
            HirGenericType::Const(_) | HirGenericType::Width => {
                // Const and width parameters stay as expressions
                expr
            }
            _ => expr, // Other parameter types pass through unchanged
        }
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

        // Get the entity definition's generic parameters (clone to avoid borrow issues)
        let entity_generics = self
            .built_entities
            .get(&entity_name)
            .map(|e| e.generics.clone())
            .unwrap_or_default();

        // Extract generic arguments if present
        let mut generic_args = Vec::new();
        if let Some(arg_list) = node.first_child_of_kind(SyntaxKind::ArgList) {
            let mut arg_index = 0;
            for arg_node in arg_list.children() {
                // Each child is an Arg node, which contains the actual expression
                if arg_node.kind() == SyntaxKind::Arg {
                    // Find the expression or type inside the Arg node
                    if let Some(expr_node) = arg_node.children().next() {
                        let expr = if expr_node.kind() == SyntaxKind::TypeAnnotation {
                            // For type arguments, build the type and wrap in a Cast expression
                            let ty = self.build_hir_type(&expr_node);
                            Some(HirExpression::Cast(HirCastExpr {
                                expr: Box::new(HirExpression::Literal(HirLiteral::Integer(0))),
                                target_type: ty,
                            }))
                        } else {
                            self.build_expression(&expr_node)
                        };

                        if let Some(mut e) = expr {
                            // Convert expression to appropriate form based on generic parameter type
                            if arg_index < entity_generics.len() {
                                let param = &entity_generics[arg_index];
                                e = self.convert_generic_arg_expr(e, &param.param_type);
                            }
                            generic_args.push(e);
                        }
                    }
                    arg_index += 1;
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
        // Get port name (first identifier OR keyword, since keywords can be used as port names)
        let port_name = node
            .first_token_of_kind(SyntaxKind::Ident)
            .or_else(|| {
                // If not an Ident, look for any keyword token
                node.children_with_tokens()
                    .filter_map(|elem| elem.into_token())
                    .find(|t| t.kind().is_keyword())
            })
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
        self.symbols.signal_types.insert(id, signal_type.clone()); // BUG FIX #5
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

        // Extract generic parameters (Phase 1)
        let mut generics = Vec::new();
        if let Some(generic_list) = node.first_child_of_kind(SyntaxKind::GenericParamList) {
            for generic_node in generic_list.children_of_kind(SyntaxKind::GenericParam) {
                if let Some(generic) = self.build_generic_param(&generic_node) {
                    generics.push(generic);
                }
            }
        }

        // Build parameters
        let mut params = Vec::new();
        if let Some(param_list) = node.first_child_of_kind(SyntaxKind::ParameterList) {
            for param_node in param_list.children_of_kind(SyntaxKind::Parameter) {
                if let Some(param) = self.build_parameter(&param_node) {
                    params.push(param);
                }
            }
        }

        // Extract optional return type (BUG FIX #67: Support all type nodes including TupleType)
        // The return type appears as a TypeAnnotation child node AFTER the ParameterList
        // NOTE: The arrow (->) is a token, not a node, so we can't search for it in .children()
        let return_type = node
            .children()
            .skip_while(|n| n.kind() == SyntaxKind::ParameterList)
            .find(|n| n.kind() == SyntaxKind::TypeAnnotation)
            .map(|n| self.extract_hir_type(&n));

        // CRITICAL FIX (Bug #32): Push a new scope for function parameters
        // This ensures parameters don't leak into the outer scope
        self.symbols.enter_scope();

        // Register function parameters in the local scope
        // BEFORE building the function body, so parameter references in the body can be resolved
        for param in &params {
            self.symbols
                .add_to_scope(&param.name, SymbolId::GenericParam(param.name.clone()));
        }

        // Build function body (statements from block)
        let body = if let Some(block) = node.first_child_of_kind(SyntaxKind::BlockStmt) {
            self.build_statements(&block)
        } else {
            Vec::new()
        };

        // Pop the function parameter scope
        self.symbols.exit_scope();

        // Register function in symbol table
        self.symbols.add_to_scope(&name, SymbolId::Function(id));

        // BUG FIX #67: Register function return type for type inference of function calls
        // This allows tuple destructuring to correctly infer element types from function call results
        if let Some(ref ret_type) = return_type {
            eprintln!(
                "\u{1f527} BUG #67 FIX: Registering return type for function '{}': {:?}",
                name, ret_type
            );
            self.symbols
                .function_return_types
                .insert(name.clone(), ret_type.clone());
        }

        Some(HirFunction {
            id,
            is_const,
            name,
            generics, // Phase 1: Extracted from AST
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

        // Collect path segments  (before any UseTree)
        let mut segments = Vec::new();
        let mut has_use_tree = false;

        for element in use_path.children_with_tokens() {
            match element {
                rowan::NodeOrToken::Node(child) => {
                    if child.kind() == SyntaxKind::UseTree {
                        has_use_tree = true;
                    }
                }
                rowan::NodeOrToken::Token(token) => {
                    if token.kind() == SyntaxKind::Ident {
                        let text = token.text().to_string();
                        segments.push(text);
                    }
                }
            }
        }

        if segments.is_empty() {
            return None;
        }

        // Check for UseTree (nested imports like {Foo, Bar})
        if has_use_tree {
            if let Some(use_tree) = use_path.first_child_of_kind(SyntaxKind::UseTree) {
                // Parse nested import paths
                let mut paths = Vec::new();
                for tree_child in use_tree.children() {
                    if tree_child.kind() == SyntaxKind::UsePath {
                        // Extract the symbol name from this nested path (tokens, not nodes)
                        for element in tree_child.children_with_tokens() {
                            if let rowan::NodeOrToken::Token(token) = element {
                                if token.kind() == SyntaxKind::Ident {
                                    let symbol = token.text().to_string();
                                    // Create a simple path for this symbol
                                    paths.push(HirImportPath::Simple {
                                        segments: vec![symbol],
                                    });
                                }
                            }
                        }
                    }
                }

                if !paths.is_empty() {
                    return Some(HirImportPath::Nested {
                        prefix: segments,
                        paths,
                    });
                }
            }
        }

        // Check for glob (*)
        if use_path.children_with_tokens().any(|c| {
            c.as_token()
                .map(|t| t.kind() == SyntaxKind::Star)
                .unwrap_or(false)
        }) {
            return Some(HirImportPath::Glob { segments });
        }

        // Check for rename (as keyword)
        // TODO: Implement renamed imports properly

        // Simple path import
        Some(HirImportPath::Simple { segments })
    }

    /// Build type alias from syntax node
    /// Type alias: `pub type Name<T> = Type;`
    fn build_type_alias(&mut self, node: &SyntaxNode) -> Option<HirTypeAlias> {
        // Extract name (first Ident after TypeKw)
        let name = self.extract_name(node)?;

        // Extract visibility - check for PubKw token in node or parent
        let visibility = if node.children_with_tokens().any(|child| {
            child
                .as_token()
                .map(|t| t.kind() == SyntaxKind::PubKw)
                .unwrap_or(false)
        }) || node
            .parent()
            .and_then(|p| p.first_token_of_kind(SyntaxKind::PubKw))
            .is_some()
        {
            HirVisibility::Public
        } else {
            HirVisibility::Private
        };

        // Extract generic parameters if present
        let generics =
            if let Some(generic_list) = node.first_child_of_kind(SyntaxKind::GenericParamList) {
                self.parse_generic_params(&generic_list)
            } else {
                Vec::new()
            };

        // Extract target type (the type after '=')
        // Find the TypeExpr or TypeAnnotation child node
        let target_type = node
            .children()
            .find(|child| {
                matches!(
                    child.kind(),
                    SyntaxKind::TypeExpr | SyntaxKind::TypeAnnotation
                )
            })
            .map(|type_node| self.extract_hir_type(&type_node))
            .unwrap_or(HirType::Bit(1)); // Fallback to bit<1> if type not found

        Some(HirTypeAlias {
            name,
            visibility,
            generics,
            target_type,
        })
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
                    eprintln!("[HIR_COLLECT] Found AssignmentStmt, calling build_assignment");
                    if let Some(assignment) = self.build_assignment(&child, assignment_type) {
                        eprintln!("[HIR_COLLECT] ✓ build_assignment succeeded, added to statements");
                        statements.push(HirStatement::Assignment(assignment));
                    } else {
                        eprintln!("[HIR_COLLECT] ❌ build_assignment returned None, assignment DROPPED!");
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
                    // Handle both simple let and tuple destructuring
                    let let_stmts = self.build_let_statements_from_node(&child);
                    statements.extend(let_stmts);
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
            SyntaxKind::Attribute => {
                // Process attribute and set pending_mux_style
                self.process_attribute(node);
                // Attributes themselves don't produce statements
                None
            }
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
            SyntaxKind::LetStmt => {
                // Handle both simple let and tuple destructuring
                let let_stmts = self.build_let_statements_from_node(node);
                if let_stmts.is_empty() {
                    None
                } else if let_stmts.len() == 1 {
                    Some(let_stmts.into_iter().next().unwrap())
                } else {
                    // Tuple destructuring generates multiple statements - wrap in block
                    Some(HirStatement::Block(let_stmts))
                }
            }
            SyntaxKind::ReturnStmt => {
                // Return statement: return [expr]
                // NOTE: Reverse children to get the LAST expression child, not the first.
                // The parser sometimes creates both simple (IdentExpr) and complex (BinaryExpr) nodes,
                // and we want the most complete expression.
                let children: Vec<_> = node.children().collect();

                // Find the last expression child (in case there are multiple)
                let expr_node = children
                    .iter()
                    .rev() // IMPORTANT: Reverse to get the last match, not the first
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
                                | SyntaxKind::TupleExpr // CRITICAL FIX: Support tuple returns
                                | SyntaxKind::ConcatExpr // CRITICAL FIX: Support concatenation returns
                                | SyntaxKind::CastExpr // Also support cast expressions in returns
                                | SyntaxKind::StructLiteral // Also support struct literals in returns
                        )
                    });

                let expr = expr_node.and_then(|n| self.build_expression(n));
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
                                                type_args: Vec::new(), // TODO Phase 1: Parse from AST
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
                                type_args: Vec::new(), // TODO Phase 1: Parse from AST
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
                        | SyntaxKind::TupleExpr // CRITICAL FIX: Support tuple literal assignments
                        | SyntaxKind::CastExpr // BUG FIX: Support cast expressions in assignments (e.g., y = x as fp32)
                        | SyntaxKind::ConcatExpr // BUG FIX: Support concat expressions in assignments (e.g., result = {high, low})
                )
            })
            .collect();

        eprintln!(
            "[HIR_ASSIGN_DEBUG] build_assignment found {} expressions",
            exprs.len()
        );
        for (i, expr) in exprs.iter().enumerate() {
            eprintln!("  [{}] {:?}", i, expr.kind());
        }

        if exprs.len() < 2 {
            eprintln!("[HIR_ASSIGN_DEBUG] Not enough expressions, returning None");
            return None;
        }

        // Handle array indexing/slicing: memory[index] <= value or state[3:1] <= value
        // Parser splits this into: IdentExpr("memory"), IndexExpr("[index]"), IdentExpr("value")
        // Also handle field access: pos.x <= value
        // Parser splits this into: IdentExpr("pos"), FieldExpr(".x"), IdentExpr("value")

        // Track where RHS starts (will be updated by LHS parsing)
        let mut rhs_start_idx = 1;

        let lhs = if exprs.len() == 3 && exprs[1].kind() == SyntaxKind::IndexExpr {
            // Combine base and index/range to form indexed lvalue
            rhs_start_idx = 2; // RHS starts after IndexExpr

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
                // BUG#26 FIX: Prefer BinaryExpr over IdentExpr/LiteralExpr
                // The parser creates children like [IdentExpr(wr_ptr), BinaryExpr(% DEPTH)]
                // for expressions like `mem[wr_ptr % DEPTH]`. We need to use the BinaryExpr
                // which contains the full expression tree, not just the first IdentExpr.
                let index_expr = index_node
                    .children()
                    .find(|n| n.kind() == SyntaxKind::BinaryExpr)
                    .or_else(|| {
                        index_node.children().find(|n| {
                            matches!(n.kind(), SyntaxKind::IdentExpr | SyntaxKind::LiteralExpr)
                        })
                    })
                    .and_then(|n| self.build_expression(&n))?;

                HirLValue::Index(Box::new(base), index_expr)
            }
        } else if exprs.len() >= 3 && exprs[1].kind() == SyntaxKind::FieldExpr {
            // Field access on LHS: pos.x <= value or nested: out_vertex.position.x <= value
            // Parser creates: [IdentExpr(base), FieldExpr(.field1), FieldExpr(.field2), ..., RHS]
            // We need to build nested FieldAccess LValues

            // Start with the base
            let mut current_lval = self.build_lvalue(&exprs[0])?;

            // Build nested field access for each FieldExpr until we hit the RHS
            for (i, expr) in exprs.iter().enumerate().skip(1) {
                if expr.kind() != SyntaxKind::FieldExpr {
                    // We've hit the RHS, stop building LHS
                    rhs_start_idx = i;
                    break;
                }

                // Extract the field name from this FieldExpr (identifier or numeric literal for tuples)
                let field_name = expr
                    .children_with_tokens()
                    .filter_map(|e| e.into_token())
                    .filter(|t| t.kind() == SyntaxKind::Ident || t.kind() == SyntaxKind::IntLiteral)
                    .last()
                    .map(|t| t.text().to_string())?;

                // Build nested FieldAccess
                current_lval = HirLValue::FieldAccess {
                    base: Box::new(current_lval),
                    field: field_name,
                };

                rhs_start_idx = i + 1; // RHS starts after this FieldExpr (will be updated if we find more)
            }

            current_lval
        } else {
            eprintln!("[HIR_ASSIGN_DEBUG] Building LHS from exprs[0]");
            let lhs_result = self.build_lvalue(&exprs[0]);
            if lhs_result.is_none() {
                eprintln!("[HIR_ASSIGN_DEBUG] build_lvalue returned None!");
            }
            lhs_result?
        };

        eprintln!(
            "[HIR_ASSIGN_DEBUG] LHS built successfully, rhs_start_idx={}",
            rhs_start_idx
        );

        // Handle RHS - if there are multiple expressions, we need to combine them
        let rhs = if rhs_start_idx >= exprs.len() {
            eprintln!("[HIR_ASSIGN_DEBUG] rhs_start_idx >= exprs.len(), returning None");
            return None;
        } else if rhs_start_idx == exprs.len() - 1 {
            // Simple case: single RHS expression
            eprintln!(
                "[HIR_ASSIGN_DEBUG] Building single RHS from exprs[{}]",
                rhs_start_idx
            );
            self.build_expression(&exprs[rhs_start_idx])?
        } else if exprs.len() == 3 && rhs_start_idx == 1 {
            // Special case for old 3-expression patterns (not nested field access)
            // This preserves existing behavior for simple cases
            let second_expr = &exprs[1];
            let third_expr = &exprs[2];

            // Check if this is an indexed assignment (base[index] <= value)
            // In this case, exprs[1] is the IndexExpr which we already consumed in LHS
            if second_expr.kind() == SyntaxKind::IndexExpr {
                // The RHS is exprs[2]
                self.build_expression(&exprs[2])?
            } else if second_expr.kind() == SyntaxKind::FieldExpr {
                // Check if this is a field access on LHS (pos.x <= value)
                // In this case, exprs[1] is the FieldExpr which we already consumed in LHS
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

        // Chain the remaining expressions using a while loop to allow lookahead
        // BUG FIX #71 Part 4e: Need to detect FieldExpr+CallExpr pattern (method calls)
        let mut i = 1;
        while i < rhs_exprs.len() {
            let current_node = &rhs_exprs[i];

            match current_node.kind() {
                SyntaxKind::FieldExpr => {
                    // Check if next node is CallExpr (method call pattern)
                    if i + 1 < rhs_exprs.len() && rhs_exprs[i + 1].kind() == SyntaxKind::CallExpr {
                        // This is a method call: FieldExpr(.method) + CallExpr(args)
                        // Current result is the receiver
                        let method_name = current_node
                            .children_with_tokens()
                            .filter_map(|elem| elem.into_token())
                            .find(|t| {
                                t.kind() == SyntaxKind::Ident || t.kind() == SyntaxKind::IntLiteral
                            })
                            .map(|t| t.text().to_string())?;

                        eprintln!("[HIR_CHAINED_RHS] Detected method call pattern: receiver + FieldExpr(.{}) + CallExpr", method_name);

                        // Parse CallExpr arguments
                        let call_node = &rhs_exprs[i + 1];
                        let call_children: Vec<_> = call_node.children().collect();

                        // Build arguments from CallExpr children
                        let mut args = vec![result]; // Receiver is first argument

                        let mut j = 0;
                        while j < call_children.len() {
                            let child = &call_children[j];

                            // Skip IdentExpr if followed by CallExpr (nested function call)
                            if child.kind() == SyntaxKind::IdentExpr
                                && j + 1 < call_children.len()
                                && call_children[j + 1].kind() == SyntaxKind::CallExpr
                            {
                                j += 1;
                                continue;
                            }

                            // Check if this is a primary expression
                            if matches!(
                                child.kind(),
                                SyntaxKind::IdentExpr
                                    | SyntaxKind::LiteralExpr
                                    | SyntaxKind::BinaryExpr
                                    | SyntaxKind::UnaryExpr
                                    | SyntaxKind::CallExpr
                                    | SyntaxKind::StructLiteral
                                    | SyntaxKind::ArrayLiteral
                                    | SyntaxKind::TupleExpr
                                    | SyntaxKind::CastExpr
                                    | SyntaxKind::ParenExpr
                            ) {
                                // Collect this expression and any following postfix operations
                                let arg_start = j;
                                let mut arg_end = j + 1;
                                while arg_end < call_children.len()
                                    && matches!(
                                        call_children[arg_end].kind(),
                                        SyntaxKind::FieldExpr | SyntaxKind::IndexExpr
                                    )
                                {
                                    // Check for nested method call pattern
                                    if call_children[arg_end].kind() == SyntaxKind::FieldExpr
                                        && arg_end + 1 < call_children.len()
                                        && call_children[arg_end + 1].kind() == SyntaxKind::CallExpr
                                    {
                                        // Include the method call in this argument
                                        arg_end += 2; // Include both FieldExpr and CallExpr
                                        break;
                                    }
                                    arg_end += 1;
                                }

                                // Build the argument expression
                                let arg_nodes = &call_children[arg_start..arg_end];
                                if let Some(arg_expr) = self.build_chained_rhs_expression(arg_nodes)
                                {
                                    eprintln!(
                                        "[HIR_CHAINED_RHS]   Built method call arg from {} nodes",
                                        arg_nodes.len()
                                    );
                                    args.push(arg_expr);
                                }

                                j = arg_end;
                            } else {
                                // Skip non-primary expressions
                                j += 1;
                            }
                        }

                        eprintln!("[HIR_CHAINED_RHS] Created method call '{}' with {} total args (receiver + {} explicit)", method_name, args.len(), args.len() - 1);

                        result = HirExpression::Call(HirCallExpr {
                            function: method_name,
                            type_args: Vec::new(), // TODO Phase 1: Parse from AST
                            args,
                        });

                        // Skip both the FieldExpr and CallExpr
                        i += 2;
                    } else {
                        // Regular field access
                        let field_name = current_node
                            .children_with_tokens()
                            .filter_map(|elem| elem.into_token())
                            .find(|t| {
                                t.kind() == SyntaxKind::Ident || t.kind() == SyntaxKind::IntLiteral
                            })
                            .map(|t| t.text().to_string())?;

                        result = HirExpression::FieldAccess {
                            base: Box::new(result),
                            field: field_name,
                        };

                        i += 1;
                    }
                }
                SyntaxKind::IndexExpr => {
                    // Combine result with index access
                    result = self.build_index_with_base(result, current_node)?;
                    i += 1;
                }
                SyntaxKind::BinaryExpr => {
                    // Combine result with binary operation
                    result = self.combine_expressions_with_binary(result, current_node)?;
                    i += 1;
                }
                SyntaxKind::CallExpr => {
                    // Function call - just build it normally
                    // The parser should have created proper structure
                    result = self.build_expression(current_node)?;
                    i += 1;
                }
                _ => {
                    // Unknown expression type - skip it
                    i += 1;
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
        // Get condition expression - we need to find the TOP-LEVEL expression node that is a direct child
        // of the IfStmt, not nested expressions. The parser creates: IfStmt -> Expression -> BlockStmt
        //
        // CRITICAL FIX: For compound conditions like "state == 0 && enable", we must select the outermost
        // BinaryExpr (the && node), not an inner one (the == node). Simply using .find() would return
        // the first match encountered during traversal, which could be an inner expression.
        //
        // Solution: Find the expression node that is a DIRECT CHILD of the IfStmt node (not nested deeper).
        // We look for expression nodes BEFORE the first BlockStmt (the then-block).
        let condition = {
            let mut found_condition = None;
            for child in node.children() {
                // Stop when we hit the then-block
                if child.kind() == SyntaxKind::BlockStmt {
                    break;
                }
                // Capture any expression node (prefer complex over simple)
                // BUG FIX #7: Include CallExpr and FieldExpr for method calls like a.lt(0.0)
                let is_complex = matches!(
                    child.kind(),
                    SyntaxKind::BinaryExpr
                        | SyntaxKind::UnaryExpr
                        | SyntaxKind::ParenExpr
                        | SyntaxKind::CallExpr // BUG #7: Method calls like lt(), gt()
                        | SyntaxKind::FieldExpr // BUG #7: Field access (part of chained calls)
                );
                let is_simple = matches!(
                    child.kind(),
                    SyntaxKind::IdentExpr | SyntaxKind::LiteralExpr
                );

                if is_complex || (is_simple && found_condition.is_none()) {
                    found_condition = Some(child);
                }
            }
            found_condition.and_then(|n| self.build_expression(&n))?
        };

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

        // Use pending mux_style from attribute, then clear it
        let mux_style = self.pending_mux_style.take().unwrap_or_default();

        Some(HirIfStatement {
            condition,
            then_statements,
            else_statements,
            mux_style,
        })
    }

    /// Build let statement(s) from a LetStmt node, handling tuple destructuring
    /// Returns a vector of statements (single for simple let, multiple for tuple destructuring)
    fn build_let_statements_from_node(&mut self, node: &SyntaxNode) -> Vec<HirStatement> {
        // Check if this is a tuple pattern
        if let Some(pattern_node) = node
            .children()
            .find(|n| n.kind() == SyntaxKind::TuplePattern)
        {
            return self.build_tuple_destructuring(node, &pattern_node);
        }

        // Simple identifier pattern - delegate to original function
        if let Some(let_stmt) = self.build_let_statement(node) {
            vec![HirStatement::Let(let_stmt)]
        } else {
            Vec::new()
        }
    }

    /// Build tuple destructuring: let (a, b, c) = expr
    /// Expands to: let _tmp = expr; let a = _tmp.0; let b = _tmp.1; let c = _tmp.2;
    fn build_tuple_destructuring(
        &mut self,
        let_node: &SyntaxNode,
        pattern_node: &SyntaxNode,
    ) -> Vec<HirStatement> {
        let mut statements = Vec::new();

        // Extract mutability flag (Bug #78 fix)
        // For tuple destructuring, all variables inherit the mutability from the let statement
        let mutable = let_node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .any(|t| t.kind() == SyntaxKind::MutKw);

        // Extract variable names from tuple pattern
        let var_names: Vec<String> = pattern_node
            .children()
            .filter(|n| n.kind() == SyntaxKind::IdentPattern)
            .filter_map(|n| {
                n.children_with_tokens()
                    .filter_map(|elem| elem.into_token())
                    .find(|t| t.kind() == SyntaxKind::Ident)
                    .map(|t| t.text().to_string())
            })
            .collect();

        if var_names.is_empty() {
            return statements;
        }

        // Extract the initializer expression
        let expr_children: Vec<_> = let_node
            .children()
            .filter(|n| {
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
                        | SyntaxKind::IfExpr
                        | SyntaxKind::MatchExpr
                        | SyntaxKind::TupleExpr
                        | SyntaxKind::CastExpr // CRITICAL FIX (Bug #38): Support cast expressions in tuple destructuring
                )
            })
            .collect();

        let value_node = expr_children
            .iter()
            .find(|n| n.kind() == SyntaxKind::BinaryExpr)
            .or_else(|| {
                expr_children
                    .iter()
                    .find(|n| n.kind() == SyntaxKind::UnaryExpr)
            })
            .or_else(|| {
                expr_children
                    .iter()
                    .find(|n| n.kind() == SyntaxKind::CallExpr)
            })
            .or_else(|| expr_children.last());

        let Some(value) = value_node.and_then(|n| self.build_expression(n)) else {
            return statements;
        };

        // Extract optional type annotation (will be a tuple type)
        let tuple_type = let_node
            .children()
            .find(|n| n.kind() == SyntaxKind::TupleType)
            .map(|n| self.extract_hir_type(&n));

        // Create temporary variable for the tuple expression
        let tmp_id = self.next_variable_id();
        let tmp_name = format!("_tuple_tmp_{}", tmp_id.0);

        // Infer or use explicit tuple type
        let tmp_type = if let Some(HirType::Tuple(element_types)) = tuple_type {
            HirType::Tuple(element_types.clone())
        } else {
            // Try to infer type from RHS expression
            // If RHS is a tuple literal, we can infer from the number of elements
            if let HirExpression::TupleLiteral(ref elements) = value {
                // Create tuple type with placeholders matching element count
                // Type inference will refine these later
                let element_types = vec![HirType::Nat(32); elements.len()];
                eprintln!("🔍 TUPLE TYPE INFERENCE: TupleLiteral with {} elements -> Nat(32) placeholders", elements.len());
                HirType::Tuple(element_types)
            } else if let HirExpression::Variable(var_id) = value {
                // If RHS is a variable, look up its type from the variable_types map
                let var_type = self
                    .symbols
                    .variable_types
                    .get(&var_id)
                    .cloned()
                    .unwrap_or_else(|| {
                        // Fallback: Create tuple type with Nat(32) placeholders
                        let element_types = vec![HirType::Nat(32); var_names.len()];
                        HirType::Tuple(element_types)
                    });
                eprintln!(
                    "🔍 TUPLE TYPE INFERENCE: Variable({}) -> {:?}",
                    var_id.0, var_type
                );
                var_type
            } else if let HirExpression::Call(ref call_expr) = value {
                // BUG FIX #69: If RHS is a function call, look up its return type from function_return_types
                let func_return_type = self
                    .symbols
                    .function_return_types
                    .get(&call_expr.function)
                    .cloned()
                    .unwrap_or_else(|| {
                        // Fallback: Create tuple type with Nat(32) placeholders
                        let element_types = vec![HirType::Nat(32); var_names.len()];
                        HirType::Tuple(element_types)
                    });
                eprintln!(
                    "🔍 TUPLE TYPE INFERENCE: Call({}) -> {:?}",
                    call_expr.function, func_return_type
                );
                func_return_type
            } else {
                // Fallback: Create tuple type with Nat(32) placeholders for each element
                let element_types = vec![HirType::Nat(32); var_names.len()];
                eprintln!("🔍 TUPLE TYPE INFERENCE: Fallback (expr discriminant {:?}) -> Nat(32) placeholders", std::mem::discriminant(&value));
                HirType::Tuple(element_types)
            }
        };

        // Register temporary variable
        self.symbols.variables.insert(tmp_name.clone(), tmp_id);
        self.symbols
            .add_to_scope(&tmp_name, SymbolId::Variable(tmp_id));
        // Register type for type inference
        self.symbols.variable_types.insert(tmp_id, tmp_type.clone());

        // Create let statement for temporary: let _tuple_tmp_N = expr
        statements.push(HirStatement::Let(HirLetStatement {
            id: tmp_id,
            name: tmp_name.clone(),
            mutable, // Inherit mutability from original let statement
            var_type: tmp_type.clone(),
            value,
        }));

        // Create let statements for each tuple element: let a = _tuple_tmp_N.0
        for (index, var_name) in var_names.iter().enumerate() {
            let var_id = self.next_variable_id();

            // Determine element type from tuple type if available
            let element_type = if let HirType::Tuple(ref element_types) = tmp_type {
                element_types
                    .get(index)
                    .cloned()
                    .unwrap_or(HirType::Nat(32))
            } else {
                HirType::Nat(32)
            };

            // DEBUG: Log element type assignment for tuple destructuring
            eprintln!(
                "🔍 TUPLE DESTRUCTURE: var='{}', index={}, element_type={:?}, tuple_type={:?}",
                var_name, index, element_type, tmp_type
            );

            // Create field access expression: _tuple_tmp_N.index
            let field_access = HirExpression::FieldAccess {
                base: Box::new(HirExpression::Variable(tmp_id)),
                field: index.to_string(),
            };

            // Register variable in symbol table
            self.symbols.variables.insert(var_name.clone(), var_id);
            self.symbols
                .add_to_scope(var_name, SymbolId::Variable(var_id));
            // Register type for type inference
            self.symbols
                .variable_types
                .insert(var_id, element_type.clone());

            // Create let statement: let var = _tuple_tmp_N.index
            statements.push(HirStatement::Let(HirLetStatement {
                id: var_id,
                name: var_name.clone(),
                mutable, // Inherit mutability from original let statement
                var_type: element_type,
                value: field_access,
            }));
        }

        statements
    }

    /// Build simple let statement (single identifier pattern)
    fn build_let_statement(&mut self, node: &SyntaxNode) -> Option<HirLetStatement> {
        // Extract mutability flag (Bug #78 fix)
        let mutable = node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .any(|t| t.kind() == SyntaxKind::MutKw);

        // Extract variable name - look for IdentPattern first, then fallback to bare Ident
        let name = node
            .children()
            .find(|n| n.kind() == SyntaxKind::IdentPattern)
            .and_then(|pattern_node| {
                pattern_node
                    .children_with_tokens()
                    .filter_map(|elem| elem.into_token())
                    .find(|t| t.kind() == SyntaxKind::Ident)
                    .map(|t| t.text().to_string())
            })
            .or_else(|| {
                // Fallback: look for direct Ident token (legacy behavior)
                node.children_with_tokens()
                    .filter_map(|elem| elem.into_token())
                    .find(|t| t.kind() == SyntaxKind::Ident)
                    .map(|t| t.text().to_string())
            })?;

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
                        | SyntaxKind::TupleType
                )
            })
            .map(|n| self.extract_hir_type(&n));

        // Extract initializer expression
        // For "let x = a + b", the parser may create multiple expression children:
        // IdentExpr(a), BinaryExpr(+ b), etc. We want the complete expression,
        // which is typically the LAST expression child or a BinaryExpr/UnaryExpr.
        // Priority: BinaryExpr > UnaryExpr > other expressions
        let expr_children: Vec<_> = node
            .children()
            .filter(|n| {
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
                        | SyntaxKind::IfExpr
                        | SyntaxKind::MatchExpr
                        | SyntaxKind::TupleExpr
                        | SyntaxKind::CastExpr // CRITICAL FIX (Bug #38): Support cast expressions in let bindings
                        | SyntaxKind::StructLiteral // BUG FIX #71: Support struct literal expressions in let bindings
                )
            })
            .collect();

        let value_node = expr_children
            .iter()
            .find(|n| n.kind() == SyntaxKind::BinaryExpr)
            .or_else(|| {
                expr_children
                    .iter()
                    .find(|n| n.kind() == SyntaxKind::UnaryExpr)
            })
            .or_else(|| expr_children.last());

        // BUG #71 DEBUG: Log StructLiteral let statements
        if let Some(vn) = value_node {
            if vn.kind() == SyntaxKind::StructLiteral {
                eprintln!("[HIR_LET_DEBUG] Processing let {} with StructLiteral", name);
            }
        }

        // WORKAROUND for parser bug: When the value_node is a CastExpr, the parser may have
        // created the source expression as a SIBLING instead of a child of CastExpr.
        // For example: "let x = a as fp32" creates: LetStmt { IdentExpr(a), CastExpr(fp32) }
        // instead of: LetStmt { CastExpr { IdentExpr(a), TypeAnnotation(fp32) } }
        // If we detect this pattern, manually build the cast expression.
        let value = if let Some(vn) = value_node {
            if vn.kind() == SyntaxKind::CastExpr && expr_children.len() >= 2 {
                // Check if CastExpr has no child expression (only TypeAnnotation)
                let has_expr_child = vn.children().any(|n| {
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
                    )
                });

                if !has_expr_child {
                    // Parser Bug #43: CastExpr has no child expression, source is a sibling
                    // Find the expression that should be inside the cast (the one before CastExpr)
                    let cast_expr_pos = expr_children
                        .iter()
                        .position(|n| n.kind() == SyntaxKind::CastExpr);
                    if let Some(pos) = cast_expr_pos {
                        if pos > 0 {
                            let source_expr_node = &expr_children[pos - 1];

                            // Build the source expression and extract target type
                            self.build_expression(source_expr_node)
                                .and_then(|source_expr| {
                                    vn.children()
                                        .find(|n| n.kind() == SyntaxKind::TypeAnnotation)
                                        .map(|n| self.build_hir_type(&n))
                                        .map(|target_type| {
                                            HirExpression::Cast(HirCastExpr {
                                                expr: Box::new(source_expr),
                                                target_type,
                                            })
                                        })
                                })
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    self.build_expression(vn)
                }
            } else {
                self.build_expression(vn)
            }
        } else {
            None
        }?;

        // Allocate a variable ID for this let binding
        let id = self.next_variable_id();

        // Use explicit type if provided, otherwise infer from expression
        let var_type = explicit_type.unwrap_or_else(|| self.infer_expression_type(&value));

        // Register in symbol table so the variable can be resolved
        self.symbols.variables.insert(name.clone(), id);
        self.symbols.add_to_scope(&name, SymbolId::Variable(id));
        // Register type for type inference
        self.symbols.variable_types.insert(id, var_type.clone());

        Some(HirLetStatement {
            id,
            name,
            mutable,
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

        // Use pending mux_style from attribute, then clear it
        let mux_style = self.pending_mux_style.take().unwrap_or_default();

        Some(HirMatchStatement {
            expr,
            arms,
            mux_style,
        })
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
                    // Single identifier - check if it's a constant
                    let name = &idents[0];

                    // BUG #33 FIX: Check if this identifier refers to a constant
                    // If yes, mark it as a constant pattern using Path("__CONST__", name)
                    // This will be resolved to the constant's value during MIR conversion
                    if let Some(SymbolId::Constant(_)) = self.symbols.lookup(name) {
                        // Found a constant - use special Path pattern to mark it
                        Some(HirPattern::Path("__CONST__".to_string(), name.clone()))
                    } else {
                        // Not a constant - treat as variable binding
                        Some(HirPattern::Variable(name.clone()))
                    }
                } else {
                    // Fallback to first identifier
                    let name = node
                        .first_token_of_kind(SyntaxKind::Ident)
                        .map(|t| t.text().to_string())?;

                    // BUG #33 FIX: Check if it's a constant
                    if let Some(SymbolId::Constant(_)) = self.symbols.lookup(&name) {
                        Some(HirPattern::Path("__CONST__".to_string(), name))
                    } else {
                        Some(HirPattern::Variable(name))
                    }
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
                // Accept both identifiers AND keywords (like "output") as L-value names
                // This is needed because the parser allows keywords as signal/port names
                let name = node
                    .first_token_of_kind(SyntaxKind::Ident)
                    .or_else(|| {
                        // If not an Ident, look for any keyword token
                        node.children_with_tokens()
                            .filter_map(|elem| elem.into_token())
                            .find(|t| t.kind().is_keyword())
                    })
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
                // Handle field access like pos.x or nested like out_vertex.position.x
                // Get all identifier tokens
                let tokens: Vec<_> = node
                    .children_with_tokens()
                    .filter_map(|e| e.into_token())
                    .filter(|t| t.kind() == SyntaxKind::Ident)
                    .collect();

                if tokens.len() < 2 {
                    return None;
                }

                // Build nested field access from left to right
                // For "out_vertex.position.x": tokens = [out_vertex, position, x]
                // Result: FieldAccess { base: FieldAccess { base: Signal(out_vertex), field: "position" }, field: "x" }

                let base_name = tokens[0].text().to_string();

                // Look up the base as a signal/variable/port
                let mut current_lval = if let Some(symbol) = self.symbols.lookup(&base_name) {
                    match symbol {
                        SymbolId::Signal(id) => HirLValue::Signal(*id),
                        SymbolId::Variable(id) => HirLValue::Variable(*id),
                        SymbolId::Port(id) => HirLValue::Port(*id),
                        _ => return None,
                    }
                } else {
                    return None;
                };

                // Build nested field accesses for each remaining field
                for token in tokens.iter().skip(1) {
                    let field_name = token.text().to_string();
                    current_lval = HirLValue::FieldAccess {
                        base: Box::new(current_lval),
                        field: field_name,
                    };
                }

                Some(current_lval)
            }
            _ => None,
        }
    }

    /// Build expression
    #[allow(clippy::comparison_chain)]
    fn build_expression(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        eprintln!(
            "[HIR_BUILD_EXPR] Building expression, node kind: {:?}",
            node.kind()
        );
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
            SyntaxKind::ArrayLiteral => self.build_array_literal(node),
            SyntaxKind::TupleExpr => self.build_tuple_expr(node),
            SyntaxKind::ConcatExpr => self.build_concat_expr(node),
            SyntaxKind::TernaryExpr => self.build_ternary_expr(node),
            SyntaxKind::FieldExpr => self.build_field_expr(node),
            SyntaxKind::IndexExpr => self.build_index_expr(node),
            SyntaxKind::PathExpr => self.build_path_expr(node),
            SyntaxKind::IfExpr => self.build_if_expr(node),
            SyntaxKind::MatchExpr => self.build_match_expr(node),
            SyntaxKind::CastExpr => self.build_cast_expr(node),
            SyntaxKind::BlockExpr => self.build_block_expr(node),
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
                                    | SyntaxKind::TupleExpr
                                    | SyntaxKind::PathExpr
                                    | SyntaxKind::ParenExpr
                                    | SyntaxKind::IfExpr
                                    | SyntaxKind::MatchExpr
                                    | SyntaxKind::CastExpr // BUG #7: Support cast expressions in parentheses
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
            eprintln!(
                "[HIR_LITERAL_DEBUG] build_literal_expr: token.kind() = {:?}",
                token.kind()
            );
            if let Some(t) = token.as_token() {
                eprintln!("[HIR_LITERAL_DEBUG] token text = '{}'", t.text());
            }
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

        eprintln!("[HIR_IDENT_DEBUG] build_ident_expr: name='{}'", name);

        // Look up symbol FIRST - user-defined symbols (ports, signals, variables) take
        // precedence over builtin functions. This allows users to name ports "min", "max", etc.
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
            // Symbol not found in table - check if it's a builtin function
            if BUILTIN_FUNCTIONS.contains(&name.as_str()) {
                // Builtin functions should only appear in call expressions
                // If we see them as plain identifiers, something is wrong with the parse tree
                // Return None to indicate this should be handled differently
                return None;
            }

            // Treat unresolved identifiers as generic parameters or function parameters
            // This allows const function parameters to be referenced in function bodies
            // They will be bound during const evaluation
            Some(HirExpression::GenericParam(name))
        }
    }

    /// Build function call expression
    fn build_call_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        eprintln!("[HIR_BUILD_CALL] Building call expression");
        // Check if this is a method call: CallExpr preceded by a FieldExpr sibling
        // The parser creates FieldExpr and CallExpr as SIBLINGS, not parent-child:
        //   Statement
        //     FieldExpr(receiver, method_name)  ← sibling
        //     CallExpr(args)                    ← sibling
        //
        // We transform receiver.method(args) to: method(receiver, args)
        if let Some(parent) = node.parent() {
            eprintln!("[HIR_BUILD_CALL] Has parent");

            let siblings: Vec<_> = parent.children().collect();
            let call_pos = siblings.iter().position(|n| n == node)?;

            if call_pos > 0 {
                if let Some(prev_sibling) = siblings.get(call_pos - 1) {
                    if prev_sibling.kind() == SyntaxKind::FieldExpr {
                        // This is a method call!
                        let field_expr = prev_sibling;

                        // Extract method name from FieldExpr (last Ident token after the dot)
                        let method_name_result = field_expr
                            .children_with_tokens()
                            .filter_map(|elem| elem.into_token())
                            .filter(|t| t.kind() == SyntaxKind::Ident)
                            .last() // Method name is the last identifier
                            .map(|t| t.text().to_string());

                        let method_name = method_name_result?;

                        // Build receiver expression - it's the sibling BEFORE the FieldExpr
                        // In the parse tree: IdentExpr(x), FieldExpr, CallExpr are all siblings
                        // We need to find the expression sibling that comes before FieldExpr
                        let field_expr_pos = siblings.iter().position(|n| n == field_expr)?;

                        // Find the receiver (expression node before FieldExpr)
                        let receiver_node_result = siblings[..field_expr_pos]
                            .iter()
                            .rev() // Search backwards from FieldExpr
                            .find(|n| {
                                matches!(
                                    n.kind(),
                                    SyntaxKind::IdentExpr
                                        | SyntaxKind::CallExpr
                                        | SyntaxKind::FieldExpr
                                        | SyntaxKind::IndexExpr
                                        | SyntaxKind::LiteralExpr
                                        | SyntaxKind::CastExpr
                                        | SyntaxKind::ParenExpr
                                        | SyntaxKind::BinaryExpr
                                        | SyntaxKind::UnaryExpr
                                )
                            });

                        let receiver_node = receiver_node_result?;

                        // BUG FIX #5: Handle chained field access method calls like a.x.add(b.x)
                        // If receiver is a FieldExpr with no children, it's just ".field" and we need
                        // to find the base in an earlier sibling
                        let receiver = if receiver_node.kind() == SyntaxKind::FieldExpr
                            && receiver_node.children().count() == 0
                        {
                            // This is a bare FieldExpr like ".x" - need to find the base before it
                            let receiver_field_pos =
                                siblings.iter().position(|n| n == receiver_node)?;

                            // Find the base expression before this FieldExpr
                            let base_node =
                                siblings[..receiver_field_pos].iter().rev().find(|n| {
                                    matches!(
                                        n.kind(),
                                        SyntaxKind::IdentExpr
                                            | SyntaxKind::CallExpr
                                            | SyntaxKind::FieldExpr
                                            | SyntaxKind::IndexExpr
                                            | SyntaxKind::LiteralExpr
                                    )
                                })?;

                            // Build the field access from parts
                            self.build_field_access_from_parts(base_node, receiver_node)?
                        } else {
                            // Normal receiver - build as expression
                            self.build_expression(receiver_node)?
                        };

                        // Parse arguments from CallExpr children
                        // BUG FIX #5: Arguments may be chained expressions (e.g., b.x where b is IdentExpr and .x is FieldExpr)
                        // The parser creates these as sibling children, not nested
                        let mut args = vec![receiver]; // Receiver is the first argument

                        let call_children: Vec<_> = node.children().collect();

                        // Group children into arguments by building chained expressions
                        // Each argument starts with a primary expression (IdentExpr, LiteralExpr, etc.)
                        // and may be followed by postfix operations (FieldExpr, IndexExpr)
                        eprintln!("[HIR_METHOD_CALL] Method '{}': parsing {} CallExpr children as arguments", method_name, call_children.len());
                        let mut i = 0;
                        while i < call_children.len() {
                            let child = &call_children[i];
                            eprintln!("[HIR_METHOD_CALL]   child[{}]: {:?}", i, child.kind());

                            // BUG FIX #71 Part 4b: Skip IdentExpr if followed by CallExpr
                            // This pattern indicates a function call: IdentExpr(function_name) + CallExpr(args)
                            // Example: vec_dot(a, b) creates [IdentExpr("vec_dot"), CallExpr([a,b])]
                            // We should skip the IdentExpr and process only the CallExpr
                            if child.kind() == SyntaxKind::IdentExpr
                                && i + 1 < call_children.len()
                                && call_children[i + 1].kind() == SyntaxKind::CallExpr
                            {
                                eprintln!("[HIR_METHOD_CALL]   Skipping IdentExpr (function name for following CallExpr)");
                                i += 1;
                                continue;
                            }

                            // Check if this is a primary expression that starts an argument
                            if matches!(
                                child.kind(),
                                SyntaxKind::IdentExpr
                                    | SyntaxKind::LiteralExpr
                                    | SyntaxKind::BinaryExpr
                                    | SyntaxKind::UnaryExpr
                                    | SyntaxKind::CallExpr
                                    | SyntaxKind::StructLiteral
                                    | SyntaxKind::ArrayLiteral
                                    | SyntaxKind::TupleExpr
                                    | SyntaxKind::CastExpr
                                    | SyntaxKind::ParenExpr
                            ) {
                                // BUG FIX #71 Part 4e: Collect postfix operations (FieldExpr, IndexExpr)
                                // INCLUDE FieldExpr+CallExpr pattern (nested method calls) in the argument
                                // Example: a.x.mul(b) creates [IdentExpr(a), FieldExpr(.x), FieldExpr(.mul), CallExpr([b])]
                                // We want nodes[0..4] as one arg (a.x.mul(b)), not nodes[0..2] (a.x)
                                let arg_start = i;
                                let mut arg_end = i + 1;
                                while arg_end < call_children.len()
                                    && matches!(
                                        call_children[arg_end].kind(),
                                        SyntaxKind::FieldExpr | SyntaxKind::IndexExpr
                                    )
                                {
                                    // Check if this FieldExpr is followed by a CallExpr (method call pattern)
                                    if call_children[arg_end].kind() == SyntaxKind::FieldExpr
                                        && arg_end + 1 < call_children.len()
                                        && call_children[arg_end + 1].kind() == SyntaxKind::CallExpr
                                    {
                                        // This FieldExpr+CallExpr is a nested method call
                                        // INCLUDE both in the argument range
                                        // BUG FIX #71 Part 4f: Continue collecting to handle chained method calls
                                        // Example: (4.0).mul(a).mul(c) should collect all of: Literal(4.0), FieldExpr(.mul), CallExpr([a]), FieldExpr(.mul), CallExpr([c])
                                        eprintln!("[HIR_METHOD_CALL]   Including FieldExpr+CallExpr pattern (nested method call) in argument");
                                        arg_end += 2; // Include both FieldExpr and CallExpr
                                                      // Don't break - continue to collect more chained method calls
                                    } else {
                                        arg_end += 1;
                                    }
                                }

                                // Build chained expression from arg_start to arg_end
                                eprintln!(
                                    "[HIR_METHOD_CALL]   Building arg from nodes[{}..{}]",
                                    arg_start, arg_end
                                );
                                #[allow(clippy::needless_range_loop)]
                                for j in arg_start..arg_end {
                                    eprintln!(
                                        "[HIR_METHOD_CALL]     arg_node[{}]: {:?}",
                                        j,
                                        call_children[j].kind()
                                    );
                                }
                                let arg_nodes = &call_children[arg_start..arg_end];
                                if let Some(arg_expr) = self.build_chained_rhs_expression(arg_nodes)
                                {
                                    eprintln!("[HIR_METHOD_CALL]   Successfully built arg, adding to args list");
                                    args.push(arg_expr);
                                } else {
                                    eprintln!("[HIR_METHOD_CALL]   WARNING: build_chained_rhs_expression returned None, argument DROPPED!");
                                }

                                i = arg_end;
                            } else {
                                // Not a primary expression
                                // Skip non-primary expressions (delimiters, tokens, etc.)
                                // Note: With Bug Fix #71 Part 4e, FieldExpr+CallExpr patterns are now
                                // included in the primary expression's argument range, so we shouldn't
                                // encounter them here in the non-primary branch
                                eprintln!("[HIR_METHOD_CALL]   Skipping non-primary expression");
                                i += 1;
                            }
                        }
                        eprintln!("[HIR_METHOD_CALL] Method '{}': created Call with {} total args (receiver + {} explicit)", method_name, args.len(), args.len() - 1);

                        return Some(HirExpression::Call(HirCallExpr {
                            function: method_name,
                            type_args: Vec::new(), // TODO Phase 1: Parse from AST
                            args,
                        }));
                    }
                }
            }
        }

        // Regular function call (not a method call)
        // Get function name - try direct Ident token first,
        // then check for preceding sibling IdentExpr (postfix call syntax)
        //
        // NOTE: Do NOT use first_child_of_kind(IdentExpr) as the function name!
        // All IdentExpr children inside CallExpr are arguments, not the function name.
        let function = if let Some(ident_token) = node.first_token_of_kind(SyntaxKind::Ident) {
            ident_token.text().to_string()
        } else if let Some(parent) = node.parent() {
            // Postfix call: IdentExpr and CallExpr are siblings
            // Look for preceding IdentExpr sibling
            let siblings: Vec<_> = parent.children().collect();
            let call_pos = siblings.iter().position(|n| n == node)?;

            if call_pos > 0 {
                if let Some(prev_sibling) = siblings.get(call_pos - 1) {
                    if prev_sibling.kind() == SyntaxKind::IdentExpr {
                        prev_sibling
                            .first_token_of_kind(SyntaxKind::Ident)
                            .map(|t| t.text().to_string())?
                    } else if prev_sibling.kind() == SyntaxKind::PathExpr {
                        // BUG #21 FIX: Handle module-qualified function calls (e.g., imported_funcs::process_data)
                        // PathExpr contains multiple Ident tokens separated by :: tokens
                        let mut idents = Vec::new();
                        for elem in prev_sibling.children_with_tokens() {
                            if let Some(token) = elem.as_token() {
                                if token.kind() == SyntaxKind::Ident {
                                    idents.push(token.text().to_string());
                                }
                            }
                        }
                        if idents.is_empty() {
                            return None;
                        }
                        // Join all identifiers with "::" to reconstruct the full path
                        idents.join("::")
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

        // Extract type arguments (Phase 1: Generic function calls like func::<T>(args))
        let mut type_args = Vec::new();
        if let Some(arg_list) = node.first_child_of_kind(SyntaxKind::ArgList) {
            eprintln!("[HIR_TYPE_ARGS] Found ArgList with {} children", arg_list.children().count());
            for arg_node in arg_list.children() {
                eprintln!("[HIR_TYPE_ARGS] Processing arg_node kind: {:?}", arg_node.kind());
                let hir_type = self.extract_hir_type(&arg_node);
                eprintln!("[HIR_TYPE_ARGS] Extracted type: {:?}", hir_type);
                type_args.push(hir_type);
            }
        }

        // Parse arguments - need to handle nested function calls correctly
        // Skip ArgList children (those are type arguments, not value arguments)
        // BUG FIX: Skip IdentExpr if followed by CallExpr (nested function call pattern)
        let mut args = Vec::new();

        let call_children: Vec<_> = node.children().collect();
        let mut i = 0;
        while i < call_children.len() {
            let child = &call_children[i];

            // Skip ArgList (type arguments)
            if child.kind() == SyntaxKind::ArgList {
                i += 1;
                continue;
            }

            // Skip IdentExpr if followed by CallExpr (nested function call pattern)
            // Example: fp_mul(fp_sub(a, b), c) creates [IdentExpr("fp_sub"), CallExpr([a,b]), IdentExpr("c")]
            // We should skip the IdentExpr and process only the CallExpr
            if child.kind() == SyntaxKind::IdentExpr
                && i + 1 < call_children.len()
                && call_children[i + 1].kind() == SyntaxKind::CallExpr
            {
                eprintln!("[HIR_CALL_FIX] Skipping IdentExpr (function name for following CallExpr)");
                i += 1;
                continue;
            }

            // Process as argument
            if let Some(expr) = self.build_expression(&child) {
                args.push(expr);
            }

            i += 1;
        }

        Some(HirExpression::Call(HirCallExpr {
            function,
            type_args, // Phase 1: Extracted from AST
            args,
        }))
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
        let all_children: Vec<_> = node.children().collect();
        eprintln!(
            "[HIR_STRUCT_DEBUG] StructLiteral {} has {} total children",
            type_name,
            all_children.len()
        );
        for (i, child) in all_children.iter().enumerate() {
            eprintln!("  child[{}]: {:?}", i, child.kind());
            if child.kind() == SyntaxKind::StructFieldInit {
                if let Some(field_init) = self.build_struct_field_init(child) {
                    fields.push(field_init);
                }
            }
        }

        eprintln!(
            "[HIR_STRUCT_DEBUG] build_struct_literal: type={}, {} fields",
            type_name,
            fields.len()
        );
        for (i, f) in fields.iter().enumerate() {
            eprintln!("  [{}] field: {}", i, f.name);
        }

        Some(HirExpression::StructLiteral(HirStructLiteral {
            type_name,
            fields,
        }))
    }

    /// Build array literal: [1, 2, 3] or [value; count]
    fn build_array_literal(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // Check if this is a repeat array [value; count] by looking for semicolon token
        let has_semicolon = node.children_with_tokens().any(|elem| {
            elem.as_token()
                .is_some_and(|t| t.kind() == SyntaxKind::Semicolon)
        });

        if has_semicolon {
            // Repeat syntax: [value; count]
            let expressions: Vec<_> = node
                .children()
                .filter(|n| {
                    matches!(
                        n.kind(),
                        SyntaxKind::LiteralExpr
                            | SyntaxKind::IdentExpr
                            | SyntaxKind::BinaryExpr
                            | SyntaxKind::PathExpr
                            | SyntaxKind::CallExpr
                            | SyntaxKind::UnaryExpr
                            | SyntaxKind::FieldExpr
                            | SyntaxKind::IndexExpr
                            | SyntaxKind::ParenExpr
                    )
                })
                .collect();

            if expressions.len() == 2 {
                let value = Box::new(self.build_expression(&expressions[0])?);
                let count = Box::new(self.build_expression(&expressions[1])?);
                return Some(HirExpression::ArrayRepeat { value, count });
            }
        } else {
            // Regular array literal: [elem1, elem2, ...]
            let elements: Vec<_> = node
                .children()
                .filter(|n| {
                    matches!(
                        n.kind(),
                        SyntaxKind::LiteralExpr
                            | SyntaxKind::IdentExpr
                            | SyntaxKind::BinaryExpr
                            | SyntaxKind::PathExpr
                            | SyntaxKind::CallExpr
                            | SyntaxKind::UnaryExpr
                            | SyntaxKind::FieldExpr
                            | SyntaxKind::IndexExpr
                            | SyntaxKind::ParenExpr
                            | SyntaxKind::IfExpr
                            | SyntaxKind::MatchExpr
                            | SyntaxKind::RangeExpr
                            | SyntaxKind::TernaryExpr
                            | SyntaxKind::ArrayLiteral
                            | SyntaxKind::ConcatExpr
                    )
                })
                .filter_map(|n| self.build_expression(&n))
                .collect();

            if !elements.is_empty() {
                return Some(HirExpression::ArrayLiteral(elements));
            }
        }

        None
    }

    /// Build tuple literal expression: (elem1, elem2, ...)
    ///
    /// Handles parser quirk where binary expressions are flattened.
    /// For example: (input_val + 5, input_val[7:0] + 1) becomes:
    ///   TupleExpr
    ///     IdentExpr(input_val)
    ///     BinaryExpr(+ 5)
    ///     Comma
    ///     IdentExpr(input_val)
    ///     IndexExpr([7:0])
    ///     BinaryExpr(+ 1)
    ///
    /// We split by comma delimiters and reconstruct each element.
    fn build_tuple_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // Split children into groups by comma tokens
        let mut element_groups: Vec<Vec<SyntaxNode>> = Vec::new();
        let mut current_group: Vec<SyntaxNode> = Vec::new();

        for child_or_token in node.children_with_tokens() {
            match child_or_token {
                rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::Comma => {
                    // Comma delimits elements
                    if !current_group.is_empty() {
                        element_groups.push(current_group);
                        current_group = Vec::new();
                    }
                }
                rowan::NodeOrToken::Node(n) => {
                    // Only collect expression nodes, not parens
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
                            | SyntaxKind::StructLiteral
                            | SyntaxKind::ArrayLiteral
                            | SyntaxKind::TupleExpr
                            | SyntaxKind::ConcatExpr
                            | SyntaxKind::CastExpr
                    ) {
                        current_group.push(n);
                    }
                }
                _ => {}
            }
        }

        // Add the last group
        if !current_group.is_empty() {
            element_groups.push(current_group);
        }

        // Build each element from its group
        let elements: Vec<HirExpression> = element_groups
            .into_iter()
            .filter_map(|group| self.build_tuple_element(group))
            .collect();

        if elements.is_empty() {
            None
        } else {
            Some(HirExpression::TupleLiteral(elements))
        }
    }

    /// Build a single tuple element from a group of nodes (handles parser bug for binary expressions)
    fn build_tuple_element(&mut self, nodes: Vec<SyntaxNode>) -> Option<HirExpression> {
        if nodes.is_empty() {
            return None;
        }

        // If there's only one node, just build it normally
        if nodes.len() == 1 {
            return self.build_expression(&nodes[0]);
        }

        // Multiple nodes - need to reconstruct the expression
        // Common patterns:
        // 1. [IdentExpr, BinaryExpr] -> binary expression with IdentExpr as left operand
        // 2. [IdentExpr, IndexExpr, BinaryExpr] -> binary expression with IndexExpr as left operand

        // First, handle IdentExpr + IndexExpr pairs (convert to proper IndexExpr)
        let mut processed_nodes = Vec::new();
        let mut i = 0;
        while i < nodes.len() {
            if i + 1 < nodes.len()
                && matches!(
                    nodes[i].kind(),
                    SyntaxKind::IdentExpr | SyntaxKind::FieldExpr | SyntaxKind::PathExpr
                )
                && nodes[i + 1].kind() == SyntaxKind::IndexExpr
            {
                // Build the IndexExpr which will incorporate the IdentExpr
                if let Some(expr) = self.build_expression(&nodes[i + 1]) {
                    processed_nodes.push(expr);
                }
                i += 2; // Skip both nodes
            } else {
                // Build the node normally
                if let Some(expr) = self.build_expression(&nodes[i]) {
                    processed_nodes.push(expr);
                }
                i += 1;
            }
        }

        // Now, if we have [expr, BinaryExpr], reconstruct the binary expression
        // The BinaryExpr node contains the operator and right operand
        if processed_nodes.len() == 2 {
            // Check if the second is a binary expression
            if let HirExpression::Binary(bin_expr) = &processed_nodes[1] {
                // The first expression is the left operand
                // Replace the binary expression's left operand
                return Some(HirExpression::Binary(HirBinaryExpr {
                    left: Box::new(processed_nodes[0].clone()),
                    op: bin_expr.op.clone(),
                    right: bin_expr.right.clone(),
                }));
            }
        }

        // If we couldn't reconstruct, return the first expression
        processed_nodes.into_iter().next()
    }

    /// Build concatenation expression: {a, b, c}
    fn build_concat_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        eprintln!("[HIR_CONCAT_DEBUG] build_concat_expr called");
        eprintln!("[HIR_CONCAT_DEBUG] Node children:");
        for child in node.children() {
            eprintln!("  - {:?}", child.kind());
        }

        // Collect all expression children
        let expressions: Vec<HirExpression> = node
            .children()
            .filter(|n| {
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
                        | SyntaxKind::ConcatExpr
                        | SyntaxKind::CastExpr
                )
            })
            .filter_map(|n| self.build_expression(&n))
            .collect();

        eprintln!(
            "[HIR_CONCAT_DEBUG] Found {} expressions in concat",
            expressions.len()
        );
        if expressions.is_empty() {
            eprintln!("[HIR_CONCAT_DEBUG] No expressions found, returning None");
            None
        } else {
            eprintln!(
                "[HIR_CONCAT_DEBUG] Returning Concat with {} elements",
                expressions.len()
            );
            Some(HirExpression::Concat(expressions))
        }
    }

    /// Build ternary conditional expression: condition ? true_expr : false_expr
    fn build_ternary_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // Collect all expression children (ignoring ? and : tokens)
        let expressions: Vec<HirExpression> = node
            .children()
            .filter(|n| {
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
                        | SyntaxKind::ConcatExpr
                        | SyntaxKind::TernaryExpr
                        | SyntaxKind::CastExpr
                )
            })
            .filter_map(|n| self.build_expression(&n))
            .collect();

        // Ternary expression should have exactly 3 parts: condition, true_expr, false_expr
        if expressions.len() == 3 {
            Some(HirExpression::Ternary {
                condition: Box::new(expressions[0].clone()),
                true_expr: Box::new(expressions[1].clone()),
                false_expr: Box::new(expressions[2].clone()),
            })
        } else {
            None
        }
    }

    /// Build struct field initialization
    fn build_struct_field_init(&mut self, node: &SyntaxNode) -> Option<HirStructFieldInit> {
        // StructFieldInit has: field_name : expression
        let name = node
            .first_token_of_kind(SyntaxKind::Ident)
            .map(|t| t.text().to_string())?;

        // Debug: Show all children of this StructFieldInit
        let all_children: Vec<_> = node.children().collect();
        eprintln!(
            "[HIR_FIELD_DEBUG] StructFieldInit '{}' has {} children:",
            name,
            all_children.len()
        );
        for (i, child) in all_children.iter().enumerate() {
            eprintln!("  child[{}]: {:?}", i, child.kind());
        }

        // Find the expression child
        let expr_child = node.children().find(|n| {
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
                    | SyntaxKind::CastExpr // BUG FIX #71 Part 3: Support cast expressions in struct field initializers
            )
        });

        if expr_child.is_none() {
            eprintln!(
                "[HIR_FIELD_DEBUG] StructFieldInit '{}': NO MATCHING EXPRESSION CHILD FOUND!",
                name
            );
            return None;
        }

        eprintln!(
            "[HIR_FIELD_DEBUG] StructFieldInit '{}': found expression child {:?}",
            name,
            expr_child.as_ref().unwrap().kind()
        );

        let value = self.build_expression(expr_child.as_ref().unwrap());
        if value.is_none() {
            eprintln!(
                "[HIR_FIELD_DEBUG] StructFieldInit '{}': build_expression RETURNED NONE!",
                name
            );
            return None;
        }

        eprintln!("[HIR_FIELD_DEBUG] StructFieldInit '{}': SUCCESS", name);
        Some(HirStructFieldInit {
            name,
            value: value.unwrap(),
        })
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

        // BUG FIX #9: Filter out expressions that are immediately followed by FieldExpr
        // When the parser creates "a.x", it makes siblings [IdentExpr(a), FieldExpr(.x)]
        // The FieldExpr will handle the IdentExpr as its base, so we shouldn't treat
        // the IdentExpr as a separate operand in the binary expression
        let mut indices_to_skip = Vec::new();
        for i in 0..expr_children.len() {
            if i + 1 < expr_children.len() && expr_children[i + 1].kind() == SyntaxKind::FieldExpr {
                // Check if they're adjacent siblings (not separated by other nodes)
                if let Some(parent) = expr_children[i].parent() {
                    if parent == expr_children[i + 1].parent().unwrap_or(parent.clone()) {
                        let siblings: Vec<_> = parent.children().collect();
                        if let (Some(curr_pos), Some(next_pos)) = (
                            siblings.iter().position(|n| n == &expr_children[i]),
                            siblings.iter().position(|n| n == &expr_children[i + 1]),
                        ) {
                            if next_pos == curr_pos + 1 {
                                // They're adjacent - skip the current node
                                indices_to_skip.push(i);
                            }
                        }
                    }
                }
            }
        }

        // Remove skipped indices in reverse order
        for &idx in indices_to_skip.iter().rev() {
            expr_children.remove(idx);
        }

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
        // BUG FIX #66: Also check for NEXT siblings which represent chained operations!
        if let Some(parent) = node.parent() {
            let siblings: Vec<_> = parent.children().collect();

            // Find our position in siblings
            if let Some(pos) = siblings.iter().position(|n| n == node) {
                // Check for previous sibling (left operand for this expression)
                if expr_children.len() == 1 && pos > 0 {
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

                // BUG FIX #66: Check for next siblings (chained operations like "+ c")
                // If there are more BinaryExpr siblings after us, they represent chained operations
                for next in siblings.iter().skip(pos + 1) {
                    if next.kind() == SyntaxKind::BinaryExpr {
                        // This is a chained operation!
                        expr_children.push(next.clone());
                    } else if matches!(
                        next.kind(),
                        SyntaxKind::LiteralExpr
                            | SyntaxKind::IdentExpr
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
                        // Also could be a regular operand
                        // But break at non-expression nodes
                        break;
                    } else {
                        // Stop at non-expression nodes
                        break;
                    }
                }
            }
        }

        if expr_children.len() < 2 {
            return None;
        }

        // BUG FIX #66: Handle chained operations (a + b + c)
        // If we have more than 2 children, the first is the left operand and subsequent
        // children are nested BinaryExprs that need to be combined left-associatively
        if expr_children.len() > 2 {
            // Build left-associative chain: ((a op b) op c) op d ...
            let mut result = self.build_expression(&expr_children[0])?;

            for child in &expr_children[1..] {
                // Each subsequent child should be a BinaryExpr with operator + right operand
                // or just a regular expression (fallback)
                if child.kind() == SyntaxKind::BinaryExpr {
                    // Extract operator from this BinaryExpr
                    let op_token = child
                        .children_with_tokens()
                        .filter_map(|e| e.into_token())
                        .find(|t| t.kind().is_operator());

                    if let Some(op_tok) = op_token {
                        let op = self.token_to_binary_op(op_tok.kind())?;

                        // Extract right operand from this BinaryExpr's children
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
                                        | SyntaxKind::CastExpr
                                        | SyntaxKind::ArrayLiteral
                                )
                            })
                            .collect();

                        if let Some(right_node) = binary_expr_children.first() {
                            let right = Box::new(self.build_expression(right_node)?);
                            result = HirExpression::Binary(HirBinaryExpr {
                                left: Box::new(result),
                                op,
                                right,
                            });
                        }
                    }
                } else {
                    // Fallback: treat as a regular operand with the previous operator
                    // This shouldn't happen in well-formed ASTs but handle it gracefully
                    let tokens: Vec<_> = node
                        .children_with_tokens()
                        .filter_map(|elem| elem.into_token())
                        .collect();

                    if let Some(op_tok) = tokens.iter().find(|t| t.kind().is_operator()) {
                        let op = self.token_to_binary_op(op_tok.kind())?;
                        let right = Box::new(self.build_expression(child)?);
                        result = HirExpression::Binary(HirBinaryExpr {
                            left: Box::new(result),
                            op,
                            right,
                        });
                    }
                }
            }

            return Some(result);
        }

        // Standard case: exactly 2 children
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
                    SyntaxKind::Bang
                        | SyntaxKind::Tilde
                        | SyntaxKind::Minus
                        | SyntaxKind::Amp
                        | SyntaxKind::Caret
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
        // BUG FIX #75: Check if this FieldExpr is followed by a CallExpr (method call pattern)
        // If so, return None - this FieldExpr should be consumed by the CallExpr, not built independently
        if let Some(parent) = node.parent() {
            let siblings: Vec<_> = parent.children().collect();
            if let Some(pos) = siblings.iter().position(|n| n == node) {
                if pos + 1 < siblings.len() && siblings[pos + 1].kind() == SyntaxKind::CallExpr {
                    // Extract field name for logging
                    let field_name = node
                        .children_with_tokens()
                        .filter_map(|elem| elem.into_token())
                        .find(|t| {
                            t.kind() == SyntaxKind::Ident || t.kind() == SyntaxKind::IntLiteral
                        })
                        .map(|t| t.text().to_string());

                    eprintln!("[HIR_FIELD_EXPR_DEBUG] build_field_expr: FieldExpr(.{}) followed by CallExpr - this is a method call, returning None", field_name.unwrap_or_else(|| "?".to_string()));
                    return None;
                }
            }
        }

        // Get base expression and field name
        let children: Vec<_> = node.children().collect();
        eprintln!(
            "[HIR_FIELD_EXPR_DEBUG] build_field_expr: {} children",
            children.len()
        );

        // Also show children_with_tokens to see what's in this node
        let all_elems: Vec<_> = node.children_with_tokens().collect();
        eprintln!(
            "[HIR_FIELD_EXPR_DEBUG] build_field_expr: {} children_with_tokens:",
            all_elems.len()
        );
        for (i, elem) in all_elems.iter().enumerate() {
            if let Some(token) = elem.as_token() {
                eprintln!(
                    "  [{}] TOKEN: kind={:?}, text={}",
                    i,
                    token.kind(),
                    token.text()
                );
            } else if let Some(child) = elem.as_node() {
                eprintln!("  [{}] NODE: kind={:?}", i, child.kind());
            }
        }

        // BUG FIX #71 Part 3b: Handle parser bug where FieldExpr has no children
        // Parser creates sibling structure: [IdentExpr, FieldExpr] instead of FieldExpr(IdentExpr)
        // Example: "a_col0.0" becomes siblings [IdentExpr(a_col0), FieldExpr(.0)]
        if children.is_empty() {
            eprintln!("[HIR_FIELD_EXPR_DEBUG] build_field_expr: NO CHILDREN, looking for preceding sibling");

            // Find the field name from tokens
            let field_name = node
                .children_with_tokens()
                .filter_map(|elem| elem.into_token())
                .find(|t| t.kind() == SyntaxKind::Ident || t.kind() == SyntaxKind::IntLiteral)
                .map(|t| t.text().to_string());

            if field_name.is_none() {
                eprintln!("[HIR_FIELD_EXPR_DEBUG] build_field_expr: NO FIELD NAME, returning None");
                return None;
            }

            // Look for a preceding sibling that could be the base
            if let Some(parent) = node.parent() {
                let siblings: Vec<_> = parent.children().collect();
                if let Some(pos) = siblings.iter().position(|n| n == node) {
                    if pos > 0 {
                        let prev = &siblings[pos - 1];
                        eprintln!(
                            "[HIR_FIELD_EXPR_DEBUG] build_field_expr: found preceding sibling {:?}",
                            prev.kind()
                        );

                        // Check if the previous sibling is a valid base expression
                        if matches!(
                            prev.kind(),
                            SyntaxKind::IdentExpr
                                | SyntaxKind::PathExpr
                                | SyntaxKind::FieldExpr
                                | SyntaxKind::IndexExpr
                                | SyntaxKind::CallExpr
                                | SyntaxKind::ParenExpr
                        ) {
                            if let Some(base_expr) = self.build_expression(prev) {
                                eprintln!("[HIR_FIELD_EXPR_DEBUG] build_field_expr: SUCCESS with sibling base, field='{}'", field_name.as_ref().unwrap());
                                return Some(HirExpression::FieldAccess {
                                    base: Box::new(base_expr),
                                    field: field_name.unwrap(),
                                });
                            }
                        }
                    }
                }
            }

            eprintln!("[HIR_FIELD_EXPR_DEBUG] build_field_expr: NO VALID PRECEDING SIBLING, returning None");
            return None;
        }

        eprintln!(
            "[HIR_FIELD_EXPR_DEBUG] build_field_expr: child[0] kind = {:?}",
            children[0].kind()
        );
        let base_expr = self.build_expression(&children[0]);
        if base_expr.is_none() {
            eprintln!("[HIR_FIELD_EXPR_DEBUG] build_field_expr: BASE EXPRESSION BUILD FAILED, returning None");
            return None;
        }
        let base = Box::new(base_expr.unwrap());

        // Find the field name (identifier for struct fields, or numeric literal for tuple indices)
        let all_tokens: Vec<_> = node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .collect();
        eprintln!(
            "[HIR_FIELD_EXPR_DEBUG] build_field_expr: {} tokens total",
            all_tokens.len()
        );
        for (i, token) in all_tokens.iter().enumerate() {
            eprintln!(
                "  token[{}]: kind={:?}, text={}",
                i,
                token.kind(),
                token.text()
            );
        }

        let field_name = node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|t| t.kind() == SyntaxKind::Ident || t.kind() == SyntaxKind::IntLiteral)
            .map(|t| {
                // For tuple indices, convert numeric literal to string (e.g., "0", "1", "2")
                // For struct fields, just use the identifier as-is
                t.text().to_string()
            });

        if field_name.is_none() {
            eprintln!(
                "[HIR_FIELD_EXPR_DEBUG] build_field_expr: FIELD NAME NOT FOUND, returning None"
            );
            return None;
        }

        eprintln!(
            "[HIR_FIELD_EXPR_DEBUG] build_field_expr: SUCCESS, field='{}'",
            field_name.as_ref().unwrap()
        );
        Some(HirExpression::FieldAccess {
            base,
            field: field_name.unwrap(),
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

        // Extract the field name from the field node (identifier or numeric literal for tuples)
        let field_name = field_node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|t| t.kind() == SyntaxKind::Ident || t.kind() == SyntaxKind::IntLiteral)
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
            // Fix for Bug #30: Handle parser quirk where binary expressions are split
            // Parser creates: [IdentExpr(rd_ptr), BinaryExpr(% DEPTH)] for "rd_ptr % DEPTH"
            // The BinaryExpr only contains the operator and right operand; left operand is separate
            let index_expr = if indices.len() == 2
                && matches!(
                    indices[0].kind(),
                    SyntaxKind::IdentExpr | SyntaxKind::LiteralExpr
                )
                && indices[1].kind() == SyntaxKind::BinaryExpr
            {
                // Combine left operand (indices[0]) with binary expression (indices[1])
                let left_expr = self.build_expression(&indices[0])?;
                self.combine_expressions_with_binary(left_expr, &indices[1])?
            } else if let Some(binary_node) =
                indices.iter().find(|n| n.kind() == SyntaxKind::BinaryExpr)
            {
                // Found a standalone BinaryExpr
                self.build_expression(binary_node)?
            } else {
                // Simple index: just use the first (and only) expression
                self.build_expression(&indices[0])?
            };

            Some(HirExpression::Index(base, Box::new(index_expr)))
        }
    }

    /// Build path expression (e.g., State::Idle, fp32::ZERO)
    fn build_path_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // Get the type/enum name and variant/constant name from the tokens
        let mut idents = Vec::new();
        for elem in node.children_with_tokens() {
            if let Some(token) = elem.as_token() {
                if token.kind() == SyntaxKind::Ident {
                    idents.push(token.text().to_string());
                }
            }
        }

        if idents.len() >= 2 {
            let type_name = idents[0].clone();
            let member_name = idents[1].clone();

            // Heuristic to distinguish between enum variants and associated constants:
            // - Associated constants are typically SCREAMING_SNAKE_CASE (all caps with underscores)
            // - Enum variants are typically PascalCase or lowercase
            // This heuristic works for the standard library (fp32::ZERO, T::MAX_VALUE, etc.)
            let is_const = member_name.chars().all(|c| c.is_uppercase() || c == '_');

            if is_const {
                // Associated constant (e.g., fp32::ZERO, T::MAX_VALUE)
                Some(HirExpression::AssociatedConstant {
                    type_name,
                    constant_name: member_name,
                })
            } else {
                // Enum variant (e.g., State::Idle)
                Some(HirExpression::EnumVariant {
                    enum_type: type_name,
                    variant: member_name,
                })
            }
        } else {
            None
        }
    }

    /// Build if expression
    fn build_if_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // Parse structure: if <condition> { <then_block> } else { <else_block> } or else if ...
        // Due to parser bug, sub-expressions may appear as siblings.
        // We need to filter based on tokens to find the correct expressions and blocks.

        // Find expressions and blocks by looking at tokens:
        // Structure: if <cond> { <then_block> } else { <else_block> }

        let mut found_if = false;
        let mut found_lbrace1 = false;
        let mut found_rbrace1 = false;
        let mut found_else = false;
        let mut found_lbrace2 = false;

        let mut condition_expr = None;
        let mut then_block = None;
        let mut else_block = None;

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
                    // Look for BlockExpr nodes for then and else blocks
                    if n.kind() == SyntaxKind::BlockExpr {
                        if found_lbrace1 && !found_rbrace1 && then_block.is_none() {
                            then_block = Some(n);
                        } else if found_else && else_block.is_none() {
                            else_block = Some(n);
                        }
                    }
                    // Look for condition expression and fallback for else if
                    else if matches!(
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
                            | SyntaxKind::ConcatExpr
                    ) {
                        if found_if && !found_lbrace1 {
                            // Take the LAST expression before the first brace (to handle sub-expressions)
                            condition_expr = Some(n);
                        } else if found_else && n.kind() == SyntaxKind::IfExpr {
                            // else if chain - take the IfExpr as the else block
                            else_block = Some(n);
                        }
                    }
                }
            }
        }

        // Build the condition expression
        let condition = self.build_expression(&condition_expr?)?;

        // Build then block or expression
        let then = if let Some(block_node) = then_block {
            self.build_block_expr(&block_node)?
        } else {
            // Fallback: look for a direct expression (for backwards compatibility)
            let expr_node = node.children().find(|n| {
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
                        | SyntaxKind::ConcatExpr
                )
            })?;
            self.build_expression(&expr_node)?
        };

        // Build else block or expression
        let else_val = if let Some(block_node) = else_block {
            if block_node.kind() == SyntaxKind::BlockExpr {
                self.build_block_expr(&block_node)?
            } else {
                // else if chain
                self.build_expression(&block_node)?
            }
        } else {
            // Fallback: look for a direct expression
            let expr_node = node
                .children()
                .skip_while(|n| n.kind() != SyntaxKind::ElseKw)
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
                            | SyntaxKind::ConcatExpr
                    )
                })?;
            self.build_expression(&expr_node)?
        };

        Some(HirExpression::If(HirIfExpr {
            condition: Box::new(condition),
            then_expr: Box::new(then),
            else_expr: Box::new(else_val),
        }))
    }

    /// Build block expression with statements and a final expression
    /// Example: { let x = 10; x + 5 } - statements followed by final expression
    fn build_block_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        let mut statements = Vec::new();
        let mut result_expr = None;

        // Process all children of the BlockExpr node
        for child in node.children() {
            match child.kind() {
                // Statements
                SyntaxKind::LetStmt => {
                    // Build let statement(s) - this handles tuple destructuring too
                    let let_stmts = self.build_let_statements_from_node(&child);
                    statements.extend(let_stmts);
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
                SyntaxKind::ReturnStmt => {
                    // Return statements - build them as generic statements
                    if let Some(stmt) = self.build_statement(&child) {
                        statements.push(stmt);
                    }
                }
                // Expressions - the last one becomes the result
                _ if matches!(
                    child.kind(),
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
                        | SyntaxKind::CastExpr
                        | SyntaxKind::TupleExpr
                        | SyntaxKind::StructLiteral
                        | SyntaxKind::ArrayLiteral
                        | SyntaxKind::ConcatExpr
                ) =>
                {
                    // This is an expression - it becomes the result
                    result_expr = self.build_expression(&child);
                }
                _ => {
                    // Unknown node kind - skip
                }
            }
        }

        // If there's no explicit result expression, use a unit/void value (literal 0)
        let final_expr = result_expr.unwrap_or(HirExpression::Literal(HirLiteral::Integer(0)));

        Some(HirExpression::Block {
            statements,
            result_expr: Box::new(final_expr),
        })
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
                        | SyntaxKind::ConcatExpr
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

        // Use pending mux_style from attribute, then clear it
        let mux_style = self.pending_mux_style.take().unwrap_or_default();

        Some(HirExpression::Match(HirMatchExpr {
            expr: Box::new(expr),
            arms,
            mux_style,
        }))
    }

    /// Build cast expression (expr as Type)
    fn build_cast_expr(&mut self, node: &SyntaxNode) -> Option<HirExpression> {
        // CastExpr structure from parser (manual tree building):
        //   ParenExpr (or other expression)
        //   TypeAnnotation
        // BUG #18 FIX: The parser creates sibling IdentExpr + FieldExpr nodes for field access
        // Example: "my_vec.x as bit[32]" creates:
        //   CastExpr
        //     IdentExpr("my_vec")
        //     FieldExpr(".x")
        //     TypeAnnotation("bit[32]")
        // We need to combine these into FieldAccess(Ident("my_vec"), "x")

        // Check if we have both IdentExpr and FieldExpr as siblings
        let has_ident = node.children().any(|n| n.kind() == SyntaxKind::IdentExpr);
        let has_field = node.children().any(|n| n.kind() == SyntaxKind::FieldExpr);

        let expr = if has_ident && has_field {
            // Build field access from sibling Ident + Field nodes
            let base = node
                .children()
                .find(|n| n.kind() == SyntaxKind::IdentExpr)
                .and_then(|n| self.build_expression(&n))?;

            let field_text = node
                .children()
                .find(|n| n.kind() == SyntaxKind::FieldExpr)
                .map(|n| n.text().to_string())?;

            // Extract field name (remove leading '.')
            let field_name = field_text.trim_start_matches('.');

            Some(HirExpression::FieldAccess {
                base: Box::new(base),
                field: field_name.to_string(),
            })
        } else {
            // Normal case: find single expression child
            node.children()
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
                .and_then(|n| self.build_expression(&n))
        }?;

        // Find the target type from TypeAnnotation
        let target_type = node
            .children()
            .find(|n| n.kind() == SyntaxKind::TypeAnnotation)
            .map(|type_node| self.build_hir_type(&type_node))?;

        Some(HirExpression::Cast(HirCastExpr {
            expr: Box::new(expr),
            target_type,
        }))
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
                                | SyntaxKind::ConcatExpr
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
                                | SyntaxKind::BlockExpr // FIX: Support block expressions in match arms
                                | SyntaxKind::ConcatExpr // FIX: Support concat expressions in match arms
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

            // Fix for Bug #30: Prefer BinaryExpr over IdentExpr for array indices
            // When parsing mem[rd_ptr % DEPTH], the children might be [IdentExpr(rd_ptr), BinaryExpr(% DEPTH)]
            // We want the BinaryExpr (complete expression), not the IdentExpr (first operand)
            let index_node = children
                .iter()
                .find(|n| n.kind() == SyntaxKind::BinaryExpr)
                .or_else(|| children.first())
                .expect("At least one child should exist");

            let index = Box::new(self.build_expression(index_node)?);
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

        // Check for single-line intent syntax: `intent parallel = mux_style::parallel;`
        // This creates an IntentValue node containing the value expression
        if let Some(intent_value) = node.first_child_of_kind(SyntaxKind::IntentValue) {
            // Extract the intent value path (e.g., "mux_style::parallel")
            if let Some(mux_style) = self.extract_mux_style_from_intent_value(&intent_value) {
                // Store this intent's mux_style for later lookup
                self.intent_mux_styles.insert(name.clone(), mux_style);
            }

            // Single-line intents don't have traditional constraints
            return Some(HirIntent {
                id,
                name,
                description: String::new(),
                constraints: Vec::new(),
            });
        }

        // Parse intent constraints (block form)
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

    /// Extract MuxStyle from single-line intent value like `mux_style::parallel`
    fn extract_mux_style_from_intent_value(&self, node: &SyntaxNode) -> Option<MuxStyle> {
        // Collect all identifier tokens from the intent value
        let tokens: Vec<String> = node
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .filter(|token| token.kind() == SyntaxKind::Ident)
            .map(|token| token.text().to_string())
            .collect();

        // Check for mux_style::parallel or mux_style::priority patterns
        if tokens.len() >= 2 && tokens[0] == "mux_style" {
            match tokens[1].as_str() {
                "parallel" => return Some(MuxStyle::Parallel),
                "priority" => return Some(MuxStyle::Priority),
                "auto" => return Some(MuxStyle::Auto),
                _ => {}
            }
        }

        // Also check for direct intent name references (e.g., `parallel` alone)
        // This supports composition: `intent fast = parallel + critical;`
        if tokens.len() == 1 {
            if let Some(&style) = self.intent_mux_styles.get(&tokens[0]) {
                return Some(style);
            }
        }

        None
    }

    /// Process an Attribute node and set pending_mux_style if applicable
    /// Called when we see `#[parallel]` or `#[mux_style::parallel]` before a statement
    fn process_attribute(&mut self, node: &SyntaxNode) {
        // Extract the intent/attribute name from the Attribute node
        // Attribute contains IntentValue which contains identifiers
        if let Some(intent_value) = node.first_child_of_kind(SyntaxKind::IntentValue) {
            // First try to extract mux_style directly (e.g., #[mux_style::parallel])
            if let Some(style) = self.extract_mux_style_from_intent_value(&intent_value) {
                self.pending_mux_style = Some(style);
                return;
            }

            // Otherwise, look up the intent by name (e.g., #[parallel])
            let tokens: Vec<String> = intent_value
                .children_with_tokens()
                .filter_map(|elem| elem.into_token())
                .filter(|token| token.kind() == SyntaxKind::Ident)
                .map(|token| token.text().to_string())
                .collect();

            if let Some(intent_name) = tokens.first() {
                if let Some(&style) = self.intent_mux_styles.get(intent_name) {
                    self.pending_mux_style = Some(style);
                }
            }
        }
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

    /// Extract name that allows keywords (for contexts like port names where keywords are valid)
    fn extract_name_allow_keywords(&self, node: &SyntaxNode) -> Option<String> {
        node.children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|t| {
                // Accept identifiers and keywords
                t.kind() == SyntaxKind::Ident || t.kind().is_keyword()
            })
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
                    // Look for SlewRate child node or direct keyword
                    if let Some(slew_node) = pair.first_child_of_kind(SyntaxKind::SlewRate) {
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
                        // Try direct keyword
                        match value_kind {
                            Some(SyntaxKind::FastKw) => {
                                constraints.slew_rate = Some(SlewRate::Fast);
                            }
                            Some(SyntaxKind::SlowKw) => {
                                constraints.slew_rate = Some(SlewRate::Slow)
                            }
                            Some(SyntaxKind::MediumKw) => {
                                constraints.slew_rate = Some(SlewRate::Medium)
                            }
                            _ => {}
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
                    return self.build_reset_type(&child);
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
                SyntaxKind::InlineStructType => {
                    // Inline struct: struct { field1: Type1, field2: Type2 }
                    return self.build_inline_struct_type(&child);
                }
                SyntaxKind::InlineEnumType => {
                    // Inline enum: enum { Variant1, Variant2, ... }
                    return self.build_inline_enum_type(&child);
                }
                SyntaxKind::InlineUnionType => {
                    // Inline union: union { field1: Type1, field2: Type2 }
                    return self.build_inline_union_type(&child);
                }
                SyntaxKind::TupleType => {
                    // Tuple type: (Type1, Type2, ...)
                    // Extract all element types from children
                    // Extract all element types from TypeAnnotation children
                    let element_types: Vec<_> = child
                        .children()
                        .filter_map(|c| {
                            // Process TypeAnnotation children to get the actual types
                            if c.kind() == SyntaxKind::TypeAnnotation {
                                Some(self.extract_hir_type(&c))
                            } else {
                                None
                            }
                        })
                        .collect();
                    return HirType::Tuple(element_types);
                }
                _ => {}
            }
        }

        // Check if this is a literal expression (for const generic arguments like ::<32>)
        // Phase 1: Generic functions with const parameters
        // The node might be wrapped in an Arg node, so check children too
        if node.kind() == SyntaxKind::LiteralExpr {
            if let Some(expr) = self.build_literal_expr(node) {
                // Wrap literals in NatExpr so they can be used as const arguments
                return HirType::NatExpr(Box::new(expr));
            }
        }

        // Check if any child is a literal (for type arguments wrapped in Arg nodes)
        for child in node.children() {
            if child.kind() == SyntaxKind::LiteralExpr {
                if let Some(expr) = self.build_literal_expr(&child) {
                    return HirType::NatExpr(Box::new(expr));
                }
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
                SyntaxKind::ResetType => return self.build_reset_type(&child),
                // FP types (Bug #39 fix)
                SyntaxKind::Fp16Type => return HirType::Float16,
                SyntaxKind::Fp32Type => return HirType::Float32,
                SyntaxKind::Fp64Type => return HirType::Float64,
                SyntaxKind::StreamType => {
                    // Stream<T> type
                    if let Some(inner_type_node) = child.children().next() {
                        let inner = Box::new(self.extract_hir_type(&inner_type_node));
                        return HirType::Stream(inner);
                    } else {
                        return HirType::Stream(Box::new(HirType::Bit(8)));
                    }
                }
                SyntaxKind::TupleType => {
                    // Tuple type: (Type1, Type2, ...)
                    let element_types = child
                        .children()
                        .filter_map(|c| {
                            if c.kind() == SyntaxKind::TypeExpr {
                                Some(self.extract_hir_type(&c))
                            } else {
                                None
                            }
                        })
                        .collect();
                    return HirType::Tuple(element_types);
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
                        // Collect all children and identify top-level expressions
                        // Skip IdentExpr nodes that are part of larger BinaryExpr
                        let children: Vec<_> = call_node.children().collect();
                        for (idx, arg_child) in children.iter().enumerate() {
                            // Skip IdentExpr if the next node is a BinaryExpr
                            // (it means this ident is part of the binary expr)
                            if arg_child.kind() == SyntaxKind::IdentExpr
                                && idx + 1 < children.len()
                                && children[idx + 1].kind() == SyntaxKind::BinaryExpr
                            {
                                continue; // Skip this - it's part of the next binary expr
                            }

                            if let Some(arg_expr) = self.build_expression(arg_child) {
                                args.push(arg_expr);
                            }
                        }

                        let call_expr = HirExpression::Call(HirCallExpr {
                            function: func_name,
                            type_args: Vec::new(), // TODO Phase 1: Parse from AST
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

    /// Build reset type from syntax node
    /// Supports: reset, reset(active_high), reset(active_low)
    fn build_reset_type(&self, node: &SyntaxNode) -> HirType {
        // Default polarity is active high
        let mut polarity = HirResetPolarity::ActiveHigh;
        let mut clock_domain = None;

        // Look for polarity specifier in parentheses
        // Syntax: reset(active_high) or reset(active_low)
        if let Some(ident_token) = node.first_token_of_kind(SyntaxKind::Ident) {
            let polarity_str = ident_token.text();
            polarity = match polarity_str {
                "active_low" => HirResetPolarity::ActiveLow,
                "active_high" => HirResetPolarity::ActiveHigh,
                _ => {
                    // Could be a domain name or other parameter
                    // For now, default to active high
                    HirResetPolarity::ActiveHigh
                }
            };
        }

        // Could also look for lifetime for clock domain
        if let Some(lifetime_token) = node.first_token_of_kind(SyntaxKind::Lifetime) {
            let lifetime_name = lifetime_token.text().trim_start_matches('\'');
            if let Some(domain_id) = self.symbols.clock_domains.get(lifetime_name) {
                clock_domain = Some(*domain_id);
            }
        }

        HirType::Reset {
            polarity,
            clock_domain,
        }
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
        // Look for expression after '=' in the node's children_with_tokens
        let mut found_assign = false;

        // Iterate through all children and tokens
        for elem in node.children_with_tokens() {
            if found_assign {
                // We've found the '=' token, now look for the expression node
                if let Some(child_node) = elem.as_node() {
                    return self.build_expression(child_node);
                }
            }
            // Check if this element is the '=' token
            if let Some(token) = elem.as_token() {
                if token.kind() == SyntaxKind::Assign {
                    found_assign = true;
                }
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
            SyntaxKind::Amp => Some(HirUnaryOp::AndReduce),
            SyntaxKind::Pipe => Some(HirUnaryOp::OrReduce),
            SyntaxKind::Caret => Some(HirUnaryOp::XorReduce),
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

        // Check for explicit discriminant value (e.g., Idle = 0)
        let value = node
            .children()
            .find(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::LiteralExpr
                        | SyntaxKind::BinaryExpr
                        | SyntaxKind::IdentExpr
                        | SyntaxKind::UnaryExpr
                )
            })
            .and_then(|n| self.build_expression(&n));

        // Check for associated data types (tuple variant syntax)
        // Look for TypeAnnotation nodes between LParen and RParen
        let associated_data = if node.children().any(|n| n.kind() == SyntaxKind::LParen) {
            let types: Vec<HirType> = node
                .children()
                .filter(|n| n.kind() == SyntaxKind::TypeAnnotation)
                .map(|type_node| self.build_hir_type(&type_node))
                .collect();

            if types.is_empty() {
                None
            } else {
                Some(types)
            }
        } else {
            None
        };

        Some(HirEnumVariant {
            name,
            value,
            associated_data,
        })
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

    /// Build inline struct type from syntax node
    /// Inline struct: `struct { x: bit[32], y: bit[32] }`
    fn build_inline_struct_type(&mut self, node: &SyntaxNode) -> HirType {
        let mut fields = Vec::new();

        // Find the field list (directly under this node, since it's inline)
        if let Some(field_list) = node.first_child_of_kind(SyntaxKind::StructFieldList) {
            for field_node in field_list.children_of_kind(SyntaxKind::StructField) {
                if let Some(field) = self.build_struct_field(&field_node) {
                    fields.push(field);
                }
            }
        }

        // Generate an anonymous name for the inline struct
        let name = format!("__inline_struct_{}", self.next_entity_id);

        HirType::Struct(HirStructType {
            name,
            fields,
            packed: false,
        })
    }

    /// Build inline enum type from syntax node
    /// Inline enum: `enum { Idle, Active, Done }`
    fn build_inline_enum_type(&mut self, node: &SyntaxNode) -> HirType {
        let mut variants = Vec::new();

        // Find the variant list
        if let Some(variant_list) = node.first_child_of_kind(SyntaxKind::EnumVariantList) {
            for variant_node in variant_list.children_of_kind(SyntaxKind::EnumVariant) {
                if let Some(variant) = self.build_enum_variant(&variant_node) {
                    variants.push(variant);
                }
            }
        }

        // Generate an anonymous name for the inline enum
        let name = format!("__inline_enum_{}", self.next_entity_id);

        // Default base type is nat[32]
        let base_type = Box::new(HirType::Nat(32));

        HirType::Enum(Box::new(HirEnumType {
            name,
            variants,
            base_type,
        }))
    }

    /// Build inline union type from syntax node
    /// Inline union: `union { x: bit[32], y: float32 }`
    fn build_inline_union_type(&mut self, node: &SyntaxNode) -> HirType {
        let mut fields = Vec::new();

        // Find the field list
        if let Some(field_list) = node.first_child_of_kind(SyntaxKind::UnionFieldList) {
            for field_node in field_list.children_of_kind(SyntaxKind::UnionField) {
                if let Some(field) = self.build_struct_field(&field_node) {
                    fields.push(field);
                }
            }
        }

        // Generate an anonymous name for the inline union
        let name = format!("__inline_union_{}", self.next_entity_id);

        HirType::Union(HirUnionType {
            name,
            fields,
            packed: false,
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
            variable_types: HashMap::new(),
            signal_types: HashMap::new(),          // BUG FIX #5
            port_types: HashMap::new(),            // BUG FIX #5
            function_return_types: HashMap::new(), // BUG FIX #67
            scopes: vec![HashMap::new()],          // Start with global scope
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
        // Extract trait name from first identifier
        let ident_tokens: Vec<_> = node
            .children_with_tokens()
            .filter_map(|element| element.into_token())
            .filter(|token| token.kind() == SyntaxKind::Ident)
            .collect();

        if ident_tokens.is_empty() {
            return None;
        }

        let trait_name = ident_tokens[0].text().to_string();

        // Find the target type (comes after 'for' keyword, now as TypeAnnotation)
        let target = if let Some(type_node) = node.first_child_of_kind(SyntaxKind::TypeAnnotation) {
            // Parse as type (e.g., nat[32], bit[8], fp32, CustomType<T>)
            let target_type = self.build_hir_type(&type_node);
            TraitImplTarget::Type(target_type)
        } else if ident_tokens.len() >= 2 {
            // Old format: try parsing as entity name for backward compatibility
            let target_name = ident_tokens[1].text().to_string();
            if let Some(entity_id) = self.symbols.entities.get(&target_name) {
                TraitImplTarget::Entity(*entity_id)
            } else {
                return None;
            }
        } else {
            return None;
        };

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
            target,
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
            // Regular type parameter (e.g., T or T: Trait1 + Trait2 or WIDTH: nat = 8)
            let name = node
                .first_token_of_kind(SyntaxKind::Ident)
                .map(|t| t.text().to_string())?;

            // Check if this is a type parameter with trait bounds
            // Look for TraitBoundList node which indicates trait bounds (T: FloatingPoint)
            let param_type = if let Some(trait_bound_list) = node.first_child_of_kind(SyntaxKind::TraitBoundList) {
                // Extract trait names from TraitBoundList
                let mut trait_bounds = Vec::new();

                for trait_bound in trait_bound_list.children_of_kind(SyntaxKind::TraitBound) {
                    if let Some(trait_name) = trait_bound.first_token_of_kind(SyntaxKind::Ident) {
                        trait_bounds.push(trait_name.text().to_string());
                    }
                }

                if !trait_bounds.is_empty() {
                    HirGenericType::TypeWithBounds(trait_bounds)
                } else {
                    HirGenericType::Type
                }
            } else {
                // No trait bounds, just a plain type parameter
                HirGenericType::Type
            };

            // Check for default value (= expression after type)
            let default_value = self.find_initial_value_expr(node);

            Some(HirGeneric {
                name,
                param_type,
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

        // Look for ParameterList node first, then iterate its Parameter children
        // This matches how function parameters are parsed (see build_function_definition)
        if let Some(param_list) = node.first_child_of_kind(SyntaxKind::ParameterList) {
            for param_node in param_list.children_of_kind(SyntaxKind::Parameter) {
                if let Some(param) = self.build_parameter(&param_node) {
                    parameters.push(param);
                }
            }
        }

        parameters
    }

    /// Build a single parameter from AST
    fn build_parameter(&mut self, node: &SyntaxNode) -> Option<HirParameter> {
        // Check for 'self' parameter first (special case - uses SelfKw, not Ident)
        if let Some(self_token) = node.first_token_of_kind(SyntaxKind::SelfKw) {
            let name = self_token.text().to_string(); // "self"

            // Extract type if present (e.g., self: Self or self: &Self)
            let mut param_type = self.extract_hir_type(node);

            // If no type annotation, default to Self
            // extract_hir_type returns Bit(8) as default when no type is found
            if matches!(param_type, HirType::Bit(8)) {
                param_type = HirType::Custom("Self".to_string());
            }

            return Some(HirParameter {
                name,
                param_type,
                default_value: None,
            });
        }

        // Extract regular parameter name
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

    /// Infer the type of an expression for let bindings
    /// This handles the most common cases to determine proper wire widths
    fn infer_expression_type(&self, expr: &HirExpression) -> HirType {
        match expr {
            // Single bit index: a[31] => bit[1]
            HirExpression::Index(_, _) => HirType::Bit(1),

            // Range extraction: a[4:0] => bit[5]
            HirExpression::Range(_, high, low) => {
                // Try to evaluate constant range bounds
                if let (Some(h), Some(l)) = (self.try_eval_const(high), self.try_eval_const(low)) {
                    let width = if h >= l { h - l + 1 } else { 1 };
                    HirType::Bit(width as u32)
                } else {
                    // Can't determine width statically, use default
                    HirType::Bit(32)
                }
            }

            // Binary operations
            HirExpression::Binary(bin_expr) => {
                let left_type = self.infer_expression_type(&bin_expr.left);
                let right_type = self.infer_expression_type(&bin_expr.right);
                self.infer_binary_op_result_type(&bin_expr.op, &left_type, &right_type)
            }

            // Unary operations preserve operand type
            HirExpression::Unary(un_expr) => self.infer_expression_type(&un_expr.operand),

            // Ternary: result type is the type of branches
            HirExpression::Ternary {
                true_expr,
                false_expr,
                ..
            } => {
                let true_type = self.infer_expression_type(true_expr);
                let false_type = self.infer_expression_type(false_expr);
                // Use the wider of the two types
                self.wider_type(&true_type, &false_type)
            }

            // Cast expression: use target type
            HirExpression::Cast(cast_expr) => cast_expr.target_type.clone(),

            // Variable/Signal/Port: look up registered type
            HirExpression::Variable(id) => self
                .symbols
                .variable_types
                .get(id)
                .cloned()
                .unwrap_or(HirType::Nat(32)),

            HirExpression::Signal(id) => {
                // BUG FIX #5: Look up signal type from symbols
                self.symbols
                    .signal_types
                    .get(id)
                    .cloned()
                    .unwrap_or(HirType::Nat(32))
            }

            HirExpression::Port(id) => {
                // BUG FIX #5: Look up port type from symbols
                self.symbols
                    .port_types
                    .get(id)
                    .cloned()
                    .unwrap_or(HirType::Nat(32))
            }

            HirExpression::Constant(_) => {
                // Constant types would need to be tracked separately
                // For now, use default width
                HirType::Nat(32)
            }

            // BUG FIX #5: Handle GenericParam (function parameters)
            HirExpression::GenericParam(name) => {
                // Function parameters might be represented as GenericParam
                // Look up in variable types by name
                // This is a workaround until we have better parameter tracking
                HirType::Nat(32) // Will be improved when we track param types properly
            }

            // Literals: infer from value
            HirExpression::Literal(lit) => match lit {
                HirLiteral::Integer(val) => {
                    // Infer minimum width needed
                    let width = if *val == 0 {
                        1
                    } else {
                        64 - val.leading_zeros()
                    };
                    HirType::Nat(width)
                }
                HirLiteral::BitVector(bits) => HirType::Bit(bits.len() as u32),
                HirLiteral::String(_) => HirType::String,
                HirLiteral::Boolean(_) => HirType::Bool,
                HirLiteral::Float(_) => HirType::Float32,
            },

            // If expression: type of branches
            HirExpression::If(if_expr) => {
                let then_type = self.infer_expression_type(&if_expr.then_expr);
                let else_type = self.infer_expression_type(&if_expr.else_expr);
                self.wider_type(&then_type, &else_type)
            }

            // Match expression: type of first arm (assuming all arms have same type)
            HirExpression::Match(match_expr) => {
                if let Some(first_arm) = match_expr.arms.first() {
                    self.infer_expression_type(&first_arm.expr)
                } else {
                    HirType::Nat(32)
                }
            }

            // Block expression: type of result expression
            HirExpression::Block { result_expr, .. } => self.infer_expression_type(result_expr),

            // Field access: try to infer from base type
            HirExpression::FieldAccess { base, field } => {
                let base_type = self.infer_expression_type(base);
                // For structs, we'd need to look up field type
                // For now, use default
                match base_type {
                    HirType::Struct(struct_type) => {
                        // Look up field type
                        struct_type
                            .fields
                            .iter()
                            .find(|f| f.name == *field)
                            .map(|f| f.field_type.clone())
                            .unwrap_or(HirType::Nat(32))
                    }
                    // BUG FIX #5: Handle vec2/vec3/vec4 field access
                    HirType::Vec2(elem_type)
                    | HirType::Vec3(elem_type)
                    | HirType::Vec4(elem_type)
                        if matches!(field.as_str(), "x" | "y" | "z" | "w") =>
                    {
                        *elem_type
                    }
                    // BUG FIX #5: Handle Custom("vec2"/"vec3"/"vec4") types (Bug #45 workaround)
                    HirType::Custom(type_name)
                        if type_name.starts_with("vec")
                            && matches!(field.as_str(), "x" | "y" | "z" | "w") =>
                    {
                        // Vec components default to Float32
                        HirType::Float32
                    }
                    _ => HirType::Nat(32),
                }
            }

            // Arrays: element type
            HirExpression::ArrayLiteral(elements) => {
                if let Some(first) = elements.first() {
                    let elem_type = self.infer_expression_type(first);
                    HirType::Array(Box::new(elem_type), elements.len() as u32)
                } else {
                    HirType::Array(Box::new(HirType::Nat(32)), 0)
                }
            }

            // Tuples
            HirExpression::TupleLiteral(elements) => {
                let elem_types: Vec<_> = elements
                    .iter()
                    .map(|e| self.infer_expression_type(e))
                    .collect();
                HirType::Tuple(elem_types)
            }

            // Concat: sum of widths
            HirExpression::Concat(exprs) => {
                let total_width: u32 = exprs
                    .iter()
                    .map(|e| {
                        let t = self.infer_expression_type(e);
                        self.get_type_width(&t)
                    })
                    .sum();
                HirType::Bit(total_width)
            }

            // Array repeat: multiply element width by count
            HirExpression::ArrayRepeat { value, count } => {
                let elem_type = self.infer_expression_type(value);
                if let Some(count_val) = self.try_eval_const(count) {
                    HirType::Array(Box::new(elem_type), count_val as u32)
                } else {
                    HirType::Array(Box::new(elem_type), 1)
                }
            }

            // Function calls: Look up return type from function signature
            // BUG FIX #67: Use actual function return types instead of default Nat(32)
            HirExpression::Call(call) => {
                // BUG FIX #5: Check if this is a built-in FP method (add, sub, mul, div, lt, gt, etc.)
                // For these methods, infer type from the receiver (first argument)
                match call.function.as_str() {
                    // FP arithmetic methods return the same type as receiver
                    "add" | "sub" | "mul" | "div" | "sqrt" | "abs" | "neg"
                        if !call.args.is_empty() =>
                    {
                        let receiver_type = self.infer_expression_type(&call.args[0]);
                        if matches!(
                            receiver_type,
                            HirType::Float16 | HirType::Float32 | HirType::Float64
                        ) {
                            return receiver_type;
                        }
                        // Not an FP type, fall through to normal lookup
                    }
                    // FP comparison methods always return bit[1]
                    "lt" | "gt" | "le" | "ge" | "eq" | "ne" if !call.args.is_empty() => {
                        let receiver_type = self.infer_expression_type(&call.args[0]);
                        if matches!(
                            receiver_type,
                            HirType::Float16 | HirType::Float32 | HirType::Float64
                        ) {
                            return HirType::Bit(1);
                        }
                        // Not an FP type, fall through to normal lookup
                    }
                    _ => {
                        // Not a built-in FP method, use normal lookup
                    }
                }

                // Look up in function signatures table
                self.symbols
                    .function_return_types
                    .get(&call.function)
                    .cloned()
                    .unwrap_or(HirType::Nat(32))
            }

            // Enum variants
            HirExpression::EnumVariant { enum_type, .. } => HirType::Custom(enum_type.clone()),

            // Associated constants
            HirExpression::AssociatedConstant { .. } => HirType::Nat(32),

            // Struct literals
            HirExpression::StructLiteral(struct_lit) => {
                HirType::Custom(struct_lit.type_name.clone())
            }
        }
    }

    /// Infer result type of binary operation
    fn infer_binary_op_result_type(
        &self,
        op: &HirBinaryOp,
        left: &HirType,
        right: &HirType,
    ) -> HirType {
        match op {
            // Comparison operators always return 1 bit
            HirBinaryOp::Equal
            | HirBinaryOp::NotEqual
            | HirBinaryOp::Less
            | HirBinaryOp::LessEqual
            | HirBinaryOp::Greater
            | HirBinaryOp::GreaterEqual
            | HirBinaryOp::LogicalAnd
            | HirBinaryOp::LogicalOr => HirType::Bit(1),

            // Shifts preserve left operand type
            HirBinaryOp::LeftShift | HirBinaryOp::RightShift => left.clone(),

            // Arithmetic and bitwise operations: use wider type
            HirBinaryOp::Add
            | HirBinaryOp::Sub
            | HirBinaryOp::Mul
            | HirBinaryOp::Div
            | HirBinaryOp::Mod
            | HirBinaryOp::And
            | HirBinaryOp::Or
            | HirBinaryOp::Xor => self.wider_type(left, right),
        }
    }

    /// Get width of a type in bits
    #[allow(clippy::only_used_in_recursion)]
    fn get_type_width(&self, ty: &HirType) -> u32 {
        match ty {
            HirType::Bit(w) | HirType::Logic(w) | HirType::Int(w) | HirType::Nat(w) => *w,
            HirType::Bool => 1,
            HirType::Array(elem_type, count) => self.get_type_width(elem_type) * count,
            // BUG FIX #65/#66: IEEE 754 floating-point types have specific widths
            HirType::Float16 => 16,
            HirType::Float32 => 32,
            HirType::Float64 => 64,
            _ => 32, // Default width
        }
    }

    /// Choose the wider of two types
    fn wider_type(&self, left: &HirType, right: &HirType) -> HirType {
        let left_width = self.get_type_width(left);
        let right_width = self.get_type_width(right);

        if left_width >= right_width {
            left.clone()
        } else {
            right.clone()
        }
    }

    /// Try to evaluate a constant expression to u64
    /// Returns Some(value) if the expression is a compile-time constant
    #[allow(clippy::only_used_in_recursion)]
    fn try_eval_const(&self, expr: &HirExpression) -> Option<u64> {
        match expr {
            HirExpression::Literal(HirLiteral::Integer(val)) => Some(*val),
            HirExpression::Binary(bin_expr) => {
                let left = self.try_eval_const(&bin_expr.left)?;
                let right = self.try_eval_const(&bin_expr.right)?;
                match bin_expr.op {
                    HirBinaryOp::Add => Some(left.wrapping_add(right)),
                    HirBinaryOp::Sub => Some(left.wrapping_sub(right)),
                    HirBinaryOp::Mul => Some(left.wrapping_mul(right)),
                    HirBinaryOp::Div if right != 0 => Some(left / right),
                    _ => None,
                }
            }
            HirExpression::Constant(id) => {
                // Would need to look up constant value
                // For now, can't evaluate
                None
            }
            _ => None,
        }
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
