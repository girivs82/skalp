//! Monomorphization Pass for Generic Functions (Phase 1)
//!
//! This module implements Rust-style monomorphization for generic functions.
//! Generic functions are specialized for each unique set of type arguments,
//! generating concrete (monomorphic) functions in the MIR.
//!
//! # Algorithm
//!
//! 1. **Collect Calls**: Find all calls to generic functions
//! 2. **Queue Specializations**: For each call, determine type arguments
//! 3. **Generate Specializations**: Create specialized function for each unique type args
//! 4. **Replace Calls**: Update call sites to use specialized function names
//!
//! # Example
//!
//! ```skalp
//! // User writes:
//! fn add<const W: nat>(a: bit[W], b: bit[W]) -> bit[W] {
//!     return a + b
//! }
//!
//! let x = add::<32>(a, b);
//! let y = add::<64>(c, d);
//!
//! // Monomorphizer generates:
//! fn add_c32(a: bit[32], b: bit[32]) -> bit[32] { return a + b }
//! fn add_c64(a: bit[64], b: bit[64]) -> bit[64] { return a + b }
//!
//! let x = add_c32(a, b);
//! let y = add_c64(c, d);
//! ```

use crate::mir::Mir;
use skalp_frontend::hir::{self, Hir, HirFunction, HirType};
use std::collections::{HashMap, HashSet};

/// Concrete type for monomorphization
///
/// Represents the concrete type that a generic parameter is instantiated with.
#[derive(Debug, Clone)]
pub enum ConcreteType {
    /// A type (e.g., bit[32], fp32)
    Type(HirType),
    /// A const value (e.g., 32 for const W: nat)
    ConstValue(u64),
}

impl ConcreteType {
    /// Get a string representation for use in keys
    fn to_key_string(&self) -> String {
        match self {
            ConcreteType::Type(ty) => format!("T:{:?}", ty),
            ConcreteType::ConstValue(val) => format!("C:{}", val),
        }
    }
}

/// Specialization request
///
/// Represents a pending specialization that needs to be generated.
#[derive(Debug, Clone)]
struct SpecializationRequest {
    /// The generic function to specialize
    function_id: hir::FunctionId,
    /// The concrete type arguments
    type_args: Vec<ConcreteType>,
}

/// Type substitution helper
///
/// Maps generic parameter names to concrete types and performs substitution.
struct TypeSubstitution {
    /// Map from generic parameter name to concrete type
    mappings: HashMap<String, ConcreteType>,
}

impl TypeSubstitution {
    fn new() -> Self {
        Self {
            mappings: HashMap::new(),
        }
    }

    fn add_mapping(&mut self, param_name: String, concrete_type: ConcreteType) {
        self.mappings.insert(param_name, concrete_type);
    }

    /// Substitute a type
    fn substitute_type(&self, ty: &HirType) -> HirType {
        match ty {
            // Parametric types that need substitution
            HirType::BitParam(param_name) => {
                if let Some(ConcreteType::ConstValue(width)) = self.mappings.get(param_name) {
                    HirType::Bit(*width as u32)
                } else {
                    ty.clone()
                }
            }
            HirType::LogicParam(param_name) => {
                if let Some(ConcreteType::ConstValue(width)) = self.mappings.get(param_name) {
                    HirType::Logic(*width as u32)
                } else {
                    ty.clone()
                }
            }
            HirType::IntParam(param_name) => {
                if let Some(ConcreteType::ConstValue(width)) = self.mappings.get(param_name) {
                    HirType::Int(*width as u32)
                } else {
                    ty.clone()
                }
            }
            HirType::NatParam(param_name) => {
                if let Some(ConcreteType::ConstValue(width)) = self.mappings.get(param_name) {
                    HirType::Nat(*width as u32)
                } else {
                    ty.clone()
                }
            }

            // Type expressions that may contain parameters
            HirType::BitExpr(expr) => HirType::BitExpr(Box::new(self.substitute_expression(expr))),
            HirType::LogicExpr(expr) => HirType::LogicExpr(Box::new(self.substitute_expression(expr))),

            // Composite types that may contain parametric types
            HirType::Array(elem_ty, size) => {
                HirType::Array(Box::new(self.substitute_type(elem_ty)), *size)
            }
            HirType::ArrayExpr(elem_ty, size_expr) => {
                HirType::ArrayExpr(
                    Box::new(self.substitute_type(elem_ty)),
                    Box::new(self.substitute_expression(size_expr)),
                )
            }
            HirType::Tuple(types) => {
                HirType::Tuple(types.iter().map(|t| self.substitute_type(t)).collect())
            }
            HirType::Vec2(elem_ty) => HirType::Vec2(Box::new(self.substitute_type(elem_ty))),
            HirType::Vec3(elem_ty) => HirType::Vec3(Box::new(self.substitute_type(elem_ty))),
            HirType::Vec4(elem_ty) => HirType::Vec4(Box::new(self.substitute_type(elem_ty))),

            // Concrete types - no substitution needed
            _ => ty.clone(),
        }
    }

    /// Substitute types in a statement
    fn substitute_statement(&self, stmt: &hir::HirStatement) -> hir::HirStatement {
        match stmt {
            hir::HirStatement::Expression(expr) => {
                hir::HirStatement::Expression(self.substitute_expression(expr))
            }
            hir::HirStatement::Let(let_stmt) => hir::HirStatement::Let(hir::HirLetStatement {
                id: let_stmt.id,
                name: let_stmt.name.clone(),
                mutable: let_stmt.mutable,
                var_type: self.substitute_type(&let_stmt.var_type),
                value: self.substitute_expression(&let_stmt.value),
            }),
            hir::HirStatement::Return(expr_opt) => {
                hir::HirStatement::Return(expr_opt.as_ref().map(|e| self.substitute_expression(e)))
            }
            hir::HirStatement::If(if_stmt) => hir::HirStatement::If(hir::HirIfStatement {
                condition: self.substitute_expression(&if_stmt.condition),
                then_statements: if_stmt
                    .then_statements
                    .iter()
                    .map(|s| self.substitute_statement(s))
                    .collect(),
                else_statements: if_stmt.else_statements.as_ref().map(|stmts| {
                    stmts.iter().map(|s| self.substitute_statement(s)).collect()
                }),
            }),
            hir::HirStatement::Match(match_stmt) => {
                hir::HirStatement::Match(hir::HirMatchStatement {
                    expr: self.substitute_expression(&match_stmt.expr),
                    arms: match_stmt
                        .arms
                        .iter()
                        .map(|arm| hir::HirMatchArm {
                            pattern: arm.pattern.clone(), // Patterns don't contain types
                            guard: arm.guard.as_ref().map(|g| self.substitute_expression(g)),
                            statements: arm
                                .statements
                                .iter()
                                .map(|s| self.substitute_statement(s))
                                .collect(),
                        })
                        .collect(),
                })
            }
            hir::HirStatement::Block(stmts) => hir::HirStatement::Block(
                stmts.iter().map(|s| self.substitute_statement(s)).collect(),
            ),
            _ => stmt.clone(), // Other statements don't contain types
        }
    }

    /// Substitute types in an expression
    fn substitute_expression(&self, expr: &hir::HirExpression) -> hir::HirExpression {
        match expr {
            hir::HirExpression::Call(call) => hir::HirExpression::Call(hir::HirCallExpr {
                function: call.function.clone(),
                type_args: call.type_args.iter().map(|t| self.substitute_type(t)).collect(),
                args: call.args.iter().map(|a| self.substitute_expression(a)).collect(),
            }),
            hir::HirExpression::Binary(binary) => {
                hir::HirExpression::Binary(hir::HirBinaryExpr {
                    left: Box::new(self.substitute_expression(&binary.left)),
                    op: binary.op.clone(),
                    right: Box::new(self.substitute_expression(&binary.right)),
                })
            }
            hir::HirExpression::Unary(unary) => hir::HirExpression::Unary(hir::HirUnaryExpr {
                op: unary.op.clone(),
                operand: Box::new(self.substitute_expression(&unary.operand)),
            }),
            hir::HirExpression::If(if_expr) => hir::HirExpression::If(hir::HirIfExpr {
                condition: Box::new(self.substitute_expression(&if_expr.condition)),
                then_expr: Box::new(self.substitute_expression(&if_expr.then_expr)),
                else_expr: Box::new(self.substitute_expression(&if_expr.else_expr)),
            }),
            hir::HirExpression::Match(match_expr) => {
                hir::HirExpression::Match(hir::HirMatchExpr {
                    expr: Box::new(self.substitute_expression(&match_expr.expr)),
                    arms: match_expr
                        .arms
                        .iter()
                        .map(|arm| hir::HirMatchArmExpr {
                            pattern: arm.pattern.clone(),
                            guard: arm.guard.as_ref().map(|g| self.substitute_expression(g)),
                            expr: self.substitute_expression(&arm.expr),
                        })
                        .collect(),
                })
            }
            hir::HirExpression::Block {
                statements,
                result_expr,
            } => hir::HirExpression::Block {
                statements: statements
                    .iter()
                    .map(|s| self.substitute_statement(s))
                    .collect(),
                result_expr: Box::new(self.substitute_expression(result_expr)),
            },
            hir::HirExpression::Cast(cast) => hir::HirExpression::Cast(hir::HirCastExpr {
                expr: Box::new(self.substitute_expression(&cast.expr)),
                target_type: self.substitute_type(&cast.target_type),
            }),
            hir::HirExpression::StructLiteral(struct_lit) => {
                hir::HirExpression::StructLiteral(hir::HirStructLiteral {
                    type_name: struct_lit.type_name.clone(),
                    fields: struct_lit
                        .fields
                        .iter()
                        .map(|field| hir::HirStructFieldInit {
                            name: field.name.clone(),
                            value: self.substitute_expression(&field.value),
                        })
                        .collect(),
                })
            }
            hir::HirExpression::TupleLiteral(exprs) => hir::HirExpression::TupleLiteral(
                exprs.iter().map(|e| self.substitute_expression(e)).collect(),
            ),
            hir::HirExpression::ArrayLiteral(exprs) => hir::HirExpression::ArrayLiteral(
                exprs.iter().map(|e| self.substitute_expression(e)).collect(),
            ),
            hir::HirExpression::Concat(exprs) => hir::HirExpression::Concat(
                exprs.iter().map(|e| self.substitute_expression(e)).collect(),
            ),
            hir::HirExpression::Ternary {
                condition,
                true_expr,
                false_expr,
            } => hir::HirExpression::Ternary {
                condition: Box::new(self.substitute_expression(condition)),
                true_expr: Box::new(self.substitute_expression(true_expr)),
                false_expr: Box::new(self.substitute_expression(false_expr)),
            },
            hir::HirExpression::Index(base, index) => hir::HirExpression::Index(
                Box::new(self.substitute_expression(base)),
                Box::new(self.substitute_expression(index)),
            ),
            hir::HirExpression::Range(base, start, end) => hir::HirExpression::Range(
                Box::new(self.substitute_expression(base)),
                Box::new(self.substitute_expression(start)),
                Box::new(self.substitute_expression(end)),
            ),
            hir::HirExpression::FieldAccess { base, field } => {
                hir::HirExpression::FieldAccess {
                    base: Box::new(self.substitute_expression(base)),
                    field: field.clone(),
                }
            }
            // Leaf expressions - no substitution needed
            _ => expr.clone(),
        }
    }
}

/// Trait method implementation info
///
/// Stores a trait method implementation for resolution
#[derive(Debug, Clone)]
struct TraitMethodInfo {
    /// Trait name
    trait_name: String,
    /// Method name
    method_name: String,
    /// Method body statements
    body: Vec<hir::HirStatement>,
    /// Method parameters (including self)
    parameters: Vec<hir::HirParameter>,
    /// Return type
    return_type: Option<HirType>,
}

/// Type context for tracking variable types during monomorphization
///
/// This allows us to infer receiver types for method calls
#[derive(Debug, Clone)]
struct TypeContext {
    /// Map from variable name to type
    variables: HashMap<String, HirType>,
    /// Map from variable ID to type (for let bindings)
    variable_ids: HashMap<hir::VariableId, HirType>,
}

impl TypeContext {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
            variable_ids: HashMap::new(),
        }
    }

    fn add_variable(&mut self, name: String, ty: HirType) {
        self.variables.insert(name, ty);
    }

    fn add_variable_id(&mut self, id: hir::VariableId, ty: HirType) {
        self.variable_ids.insert(id, ty);
    }

    fn get_variable_type(&self, name: &str) -> Option<&HirType> {
        self.variables.get(name)
    }

    fn get_variable_id_type(&self, id: &hir::VariableId) -> Option<&HirType> {
        self.variable_ids.get(id)
    }

    fn with_variables(&self, vars: Vec<(String, HirType)>) -> Self {
        let mut new_ctx = self.clone();
        for (name, ty) in vars {
            new_ctx.add_variable(name, ty);
        }
        new_ctx
    }
}

/// Monomorphizer
///
/// Main struct for performing monomorphization on a HIR.
pub struct Monomorphizer {
    /// Map from specialization key to specialized function name
    /// Key format: "func_id:type_arg1:type_arg2:..."
    specializations: HashMap<String, String>,

    /// Queue of pending specializations to generate
    pending: Vec<SpecializationRequest>,

    /// Generated specialized functions
    specialized_functions: Vec<HirFunction>,

    /// Original generic functions (for reference during specialization)
    generic_functions: HashMap<hir::FunctionId, HirFunction>,

    /// Set of function IDs that are generic
    generic_function_ids: HashSet<hir::FunctionId>,

    /// Trait method registry: (trait_name, type_key) -> HashMap<method_name -> TraitMethodInfo>
    /// Maps (trait, type) pairs to their method implementations for method resolution
    trait_methods: HashMap<(String, String), HashMap<String, TraitMethodInfo>>,
}

impl Monomorphizer {
    /// Create a new monomorphizer
    pub fn new() -> Self {
        Self {
            specializations: HashMap::new(),
            pending: Vec::new(),
            specialized_functions: Vec::new(),
            generic_functions: HashMap::new(),
            generic_function_ids: HashSet::new(),
            trait_methods: HashMap::new(),
        }
    }

    /// Monomorphize all generic functions in a HIR
    ///
    /// This is the main entry point for monomorphization.
    pub fn monomorphize(&mut self, hir: &Hir) -> Hir {
        eprintln!("🔄 [MONOMORPHIZE] Starting monomorphization pass");

        // Step 1: Build trait method registry
        self.build_trait_registry(hir);

        // Step 2: Identify generic functions
        self.identify_generic_functions(hir);

        // Step 3: Collect all calls to generic functions
        self.collect_generic_calls(hir);

        // Step 4: Generate specializations (breadth-first)
        self.generate_all_specializations();

        // Step 4: Replace generic calls with specialized calls
        self.replace_calls_in_hir(hir)
    }

    /// Build trait method registry for method resolution
    ///
    /// Extracts all trait implementations and stores method info for later resolution
    fn build_trait_registry(&mut self, hir: &Hir) {
        eprintln!("  [TRAIT] Building trait method registry");

        for trait_impl in &hir.trait_implementations {
            // Only process type-based implementations
            // Entity-based impls are not relevant for function calls
            if let hir::TraitImplTarget::Type(target_type) = &trait_impl.target {
                // Get type key string for lookup
                let type_key = format!("{:?}", target_type);

                eprintln!(
                    "  [TRAIT] Found impl {} for {:?}",
                    trait_impl.trait_name,
                    target_type
                );

                // Get trait definition to get parameter info
                let trait_def = hir.trait_definitions.iter()
                    .find(|t| t.name == trait_impl.trait_name);

                if let Some(def) = trait_def {
                    eprintln!(
                        "    [TRAIT_DEBUG] Found trait definition '{}' with {} methods",
                        def.name,
                        def.methods.len()
                    );
                } else {
                    eprintln!(
                        "    [TRAIT_DEBUG] ❌ No trait definition found for '{}'",
                        trait_impl.trait_name
                    );
                }

                // Get or create method map for this (trait, type) pair
                let key = (trait_impl.trait_name.clone(), type_key.clone());
                let methods_map = self.trait_methods.entry(key).or_insert_with(HashMap::new);

                for method_impl in &trait_impl.method_implementations {
                    // Find corresponding trait method definition for params/return type
                    let trait_method = trait_def
                        .and_then(|t| t.methods.iter().find(|m| m.name == method_impl.name));

                    if let Some(tm) = trait_method {
                        eprintln!(
                            "    [TRAIT_DEBUG] Found trait method '{}' with {} parameters: {:?}",
                            tm.name,
                            tm.parameters.len(),
                            tm.parameters.iter().map(|p| &p.name).collect::<Vec<_>>()
                        );
                    }

                    let mut parameters = trait_method
                        .map(|m| m.parameters.clone())
                        .unwrap_or_default();

                    // WORKAROUND: Parser doesn't capture 'self' parameter without type annotation
                    // Prepend 'self: Self' if not present (method calls need it for resolution)
                    let has_self = parameters.iter().any(|p| p.name == "self");
                    if !has_self {
                        eprintln!(
                            "    [TRAIT_DEBUG] Adding implicit 'self' parameter (parser doesn't capture untyped self)"
                        );
                        parameters.insert(0, hir::HirParameter {
                            name: "self".to_string(),
                            param_type: HirType::Custom("Self".to_string()),
                            default_value: None,
                        });
                    }

                    eprintln!(
                        "    [TRAIT_DEBUG] Method '{}' has {} parameters",
                        method_impl.name,
                        parameters.len()
                    );

                    let info = TraitMethodInfo {
                        trait_name: trait_impl.trait_name.clone(),
                        method_name: method_impl.name.clone(),
                        body: method_impl.body.clone(),
                        parameters,
                        return_type: trait_method
                            .and_then(|m| m.return_type.clone()),
                    };

                    methods_map.insert(method_impl.name.clone(), info);

                    eprintln!(
                        "    [TRAIT] Registered method: {}.{}",
                        trait_impl.trait_name,
                        method_impl.name
                    );
                }
            }
        }

        let total_methods: usize = self.trait_methods.values().map(|m| m.len()).sum();
        eprintln!(
            "  [TRAIT] Registered {} trait implementations with {} total methods",
            self.trait_methods.len(),
            total_methods
        );
    }

    /// Identify which functions are generic
    fn identify_generic_functions(&mut self, hir: &Hir) {
        // Check top-level functions
        for func in &hir.functions {
            if !func.generics.is_empty() {
                eprintln!(
                    "  [MONO] Found generic function: {} with {} generic params",
                    func.name,
                    func.generics.len()
                );
                self.generic_function_ids.insert(func.id);
                self.generic_functions.insert(func.id, func.clone());
            }
        }

        // Check functions in implementation blocks
        for impl_block in &hir.implementations {
            for func in &impl_block.functions {
                if !func.generics.is_empty() {
                    eprintln!(
                        "  [MONO] Found generic function in impl: {} with {} generic params",
                        func.name,
                        func.generics.len()
                    );
                    self.generic_function_ids.insert(func.id);
                    self.generic_functions.insert(func.id, func.clone());
                }
            }
        }

        eprintln!(
            "  [MONO] Identified {} generic functions",
            self.generic_function_ids.len()
        );
    }

    /// Collect all calls to generic functions
    fn collect_generic_calls(&mut self, hir: &Hir) {
        eprintln!("  [MONO] Collecting generic function calls");

        // Collect from top-level functions
        for func in &hir.functions {
            self.collect_calls_from_statements(&func.body, &func.name);
        }

        // Collect from implementation blocks
        for impl_block in &hir.implementations {
            // Collect from functions in impl blocks
            for func in &impl_block.functions {
                self.collect_calls_from_statements(&func.body, &func.name);
            }

            // Collect from event blocks (e.g., on(clk.rise) { ... })
            for (idx, event_block) in impl_block.event_blocks.iter().enumerate() {
                let context = format!("event_block_{}", idx);
                self.collect_calls_from_statements(&event_block.statements, &context);
            }

            // Collect from assignments (e.g., port = expr)
            for assignment in &impl_block.assignments {
                self.collect_calls_from_expression(&assignment.rhs, "assignment");
            }
        }

        eprintln!(
            "  [MONO] Found {} specialization requests",
            self.pending.len()
        );
    }

    /// Collect calls from a list of statements
    fn collect_calls_from_statements(&mut self, statements: &[hir::HirStatement], context: &str) {
        for stmt in statements {
            self.collect_calls_from_statement(stmt, context);
        }
    }

    /// Collect calls from a single statement
    fn collect_calls_from_statement(&mut self, stmt: &hir::HirStatement, context: &str) {
        match stmt {
            hir::HirStatement::Assignment(assign) => {
                // Assignment statements have expressions on the RHS
                self.collect_calls_from_expression(&assign.rhs, context);
            }
            hir::HirStatement::Expression(expr) => {
                self.collect_calls_from_expression(expr, context);
            }
            hir::HirStatement::Let(let_stmt) => {
                self.collect_calls_from_expression(&let_stmt.value, context);
            }
            hir::HirStatement::Return(Some(expr)) => {
                self.collect_calls_from_expression(expr, context);
            }
            hir::HirStatement::If(if_stmt) => {
                self.collect_calls_from_expression(&if_stmt.condition, context);
                self.collect_calls_from_statements(&if_stmt.then_statements, context);
                if let Some(else_stmts) = &if_stmt.else_statements {
                    self.collect_calls_from_statements(else_stmts, context);
                }
            }
            hir::HirStatement::Match(match_stmt) => {
                self.collect_calls_from_expression(&match_stmt.expr, context);
                for arm in &match_stmt.arms {
                    self.collect_calls_from_statements(&arm.statements, context);
                }
            }
            hir::HirStatement::Block(stmts) => {
                self.collect_calls_from_statements(stmts, context);
            }
            _ => {
                // Other statement types don't contain expressions
            }
        }
    }

    /// Collect calls from an expression
    fn collect_calls_from_expression(&mut self, expr: &hir::HirExpression, context: &str) {
        match expr {
            hir::HirExpression::Call(call) => {
                eprintln!("    [MONO_DEBUG] Found call to '{}' with {} type args",
                    call.function, call.type_args.len());

                // Check if this is a call to a generic function with type arguments
                if !call.type_args.is_empty() {
                    eprintln!("    [MONO_DEBUG] Call has type args, looking up function...");

                    // Look up the function by name to get its ID
                    if let Some((func_id, func)) = self.find_generic_function_by_name(&call.function) {
                        eprintln!(
                            "  [MONO] Found call to generic function '{}' with {} type args in {}",
                            call.function,
                            call.type_args.len(),
                            context
                        );

                        // Convert type arguments to ConcreteType based on generic parameter kind
                        let type_args: Vec<ConcreteType> = call
                            .type_args
                            .iter()
                            .zip(&func.generics)
                            .map(|(ty, generic)| {
                                eprintln!("    [MONO_DEBUG] Converting type arg: {:?}, generic param type: {:?}",
                                    ty, generic.param_type);
                                match &generic.param_type {
                                    hir::HirGenericType::Const(_) => {
                                        // For const parameters, extract the constant value
                                        match ty {
                                            HirType::NatExpr(expr) => {
                                                // Extract literal value from expression
                                                if let hir::HirExpression::Literal(lit) = expr.as_ref() {
                                                    let value = match lit {
                                                        hir::HirLiteral::Integer(v) => *v,
                                                        _ => {
                                                            eprintln!("    [MONO_DEBUG] NatExpr contains non-integer literal: {:?}", lit);
                                                            0
                                                        }
                                                    };
                                                    eprintln!("    [MONO_DEBUG] Extracted const value from NatExpr: {}", value);
                                                    ConcreteType::ConstValue(value)
                                                } else {
                                                    eprintln!("    [MONO_DEBUG] NatExpr contains non-literal expression: {:?}", expr);
                                                    ConcreteType::Type(ty.clone())
                                                }
                                            }
                                            HirType::Bit(width) => {
                                                eprintln!("    [MONO_DEBUG] Extracted const value from Bit: {}", width);
                                                ConcreteType::ConstValue(*width as u64)
                                            }
                                            _ => {
                                                eprintln!("    [MONO_DEBUG] Type arg is not NatExpr or Bit, treating as type: {:?}", ty);
                                                // Fallback: treat as type
                                                ConcreteType::Type(ty.clone())
                                            }
                                        }
                                    }
                                    _ => {
                                        // For type parameters, use the type directly
                                        eprintln!("    [MONO_DEBUG] Type parameter, using type directly");
                                        ConcreteType::Type(ty.clone())
                                    }
                                }
                            })
                            .collect();

                        // Queue this specialization
                        self.request_specialization(func_id, type_args);
                    } else {
                        eprintln!("    [MONO_DEBUG] Function '{}' not found in generic_functions map", call.function);
                    }
                } else {
                    eprintln!("    [MONO_DEBUG] Call to '{}' has no type args", call.function);
                }

                // Recursively collect from arguments
                for arg in &call.args {
                    self.collect_calls_from_expression(arg, context);
                }
            }
            hir::HirExpression::Binary(binary) => {
                self.collect_calls_from_expression(&binary.left, context);
                self.collect_calls_from_expression(&binary.right, context);
            }
            hir::HirExpression::Unary(unary) => {
                self.collect_calls_from_expression(&unary.operand, context);
            }
            hir::HirExpression::If(if_expr) => {
                self.collect_calls_from_expression(&if_expr.condition, context);
                self.collect_calls_from_expression(&if_expr.then_expr, context);
                self.collect_calls_from_expression(&if_expr.else_expr, context);
            }
            hir::HirExpression::Match(match_expr) => {
                self.collect_calls_from_expression(&match_expr.expr, context);
                for arm in &match_expr.arms {
                    self.collect_calls_from_expression(&arm.expr, context);
                }
            }
            hir::HirExpression::Block { statements, result_expr } => {
                self.collect_calls_from_statements(statements, context);
                self.collect_calls_from_expression(result_expr, context);
            }
            hir::HirExpression::StructLiteral(struct_lit) => {
                for field in &struct_lit.fields {
                    self.collect_calls_from_expression(&field.value, context);
                }
            }
            hir::HirExpression::TupleLiteral(exprs) | hir::HirExpression::ArrayLiteral(exprs) => {
                for expr in exprs {
                    self.collect_calls_from_expression(expr, context);
                }
            }
            hir::HirExpression::Concat(exprs) => {
                for expr in exprs {
                    self.collect_calls_from_expression(expr, context);
                }
            }
            hir::HirExpression::Ternary { condition, true_expr, false_expr } => {
                self.collect_calls_from_expression(condition, context);
                self.collect_calls_from_expression(true_expr, context);
                self.collect_calls_from_expression(false_expr, context);
            }
            hir::HirExpression::Cast(cast) => {
                self.collect_calls_from_expression(&cast.expr, context);
            }
            hir::HirExpression::Index(base, index) => {
                self.collect_calls_from_expression(base, context);
                self.collect_calls_from_expression(index, context);
            }
            hir::HirExpression::Range(base, start, end) => {
                self.collect_calls_from_expression(base, context);
                self.collect_calls_from_expression(start, context);
                self.collect_calls_from_expression(end, context);
            }
            hir::HirExpression::FieldAccess { base, .. } => {
                self.collect_calls_from_expression(base, context);
            }
            _ => {
                // Leaf expressions (literals, variables, etc.)
            }
        }
    }

    /// Find a generic function by name
    fn find_generic_function_by_name(&self, name: &str) -> Option<(hir::FunctionId, &HirFunction)> {
        self.generic_functions
            .iter()
            .find(|(_, func)| func.name == name)
            .map(|(id, func)| (*id, func))
    }

    /// Create a key for the specializations map
    fn make_specialization_key(&self, function_id: hir::FunctionId, type_args: &[ConcreteType]) -> String {
        let mut key = format!("{:?}", function_id);
        for arg in type_args {
            key.push(':');
            key.push_str(&arg.to_key_string());
        }
        key
    }

    /// Request a specialization
    fn request_specialization(&mut self, function_id: hir::FunctionId, type_args: Vec<ConcreteType>) {
        // Check if we've already specialized this combination
        let key = self.make_specialization_key(function_id, &type_args);
        if !self.specializations.contains_key(&key) {
            eprintln!(
                "    [MONO] Queuing specialization for function ID {:?} with {} type args",
                function_id,
                type_args.len()
            );
            self.pending.push(SpecializationRequest {
                function_id,
                type_args,
            });
        }
    }

    /// Generate all pending specializations
    fn generate_all_specializations(&mut self) {
        eprintln!("  [MONO] Generating specializations");

        while let Some(req) = self.pending.pop() {
            self.generate_specialization(req);
        }

        eprintln!(
            "  [MONO] Generated {} specialized functions",
            self.specialized_functions.len()
        );
    }

    /// Generate a single specialization
    fn generate_specialization(&mut self, req: SpecializationRequest) {
        let generic_func = match self.generic_functions.get(&req.function_id) {
            Some(func) => func,
            None => {
                eprintln!(
                    "    [MONO] ERROR: Generic function {:?} not found",
                    req.function_id
                );
                return;
            }
        };

        // Generate specialized name
        let specialized_name = self.mangle_name(&generic_func.name, &req.type_args);

        eprintln!(
            "    [MONO] Generating specialization: {} -> {}",
            generic_func.name, specialized_name
        );

        // Record the specialization
        let key = self.make_specialization_key(req.function_id, &req.type_args);
        self.specializations.insert(key, specialized_name.clone());

        // Build substitution map: generic param name -> concrete type
        let mut substitution = TypeSubstitution::new();
        for (generic_param, concrete_type) in generic_func.generics.iter().zip(req.type_args.iter()) {
            substitution.add_mapping(generic_param.name.clone(), concrete_type.clone());
        }

        // Create specialized function by substituting types
        let specialized_func = HirFunction {
            id: generic_func.id, // Will be updated when added to HIR
            is_const: generic_func.is_const,
            name: specialized_name.clone(),
            generics: Vec::new(), // No generics in specialized version
            params: generic_func
                .params
                .iter()
                .map(|p| hir::HirParameter {
                    name: p.name.clone(),
                    param_type: substitution.substitute_type(&p.param_type),
                    default_value: p.default_value.as_ref().map(|v| substitution.substitute_expression(v)),
                })
                .collect(),
            return_type: generic_func
                .return_type
                .as_ref()
                .map(|ty| substitution.substitute_type(ty)),
            body: generic_func
                .body
                .iter()
                .map(|stmt| substitution.substitute_statement(stmt))
                .collect(),
        };

        eprintln!(
            "    [MONO] Created specialized function '{}' with {} params",
            specialized_func.name,
            specialized_func.params.len()
        );

        self.specialized_functions.push(specialized_func);
    }

    /// Mangle a function name with type arguments
    ///
    /// Examples:
    /// - add::<32> → add_c32
    /// - identity::<bit[32]> → identity_bit32
    /// - fp_add::<fp32> → fp_add_fp32
    fn mangle_name(&self, base_name: &str, type_args: &[ConcreteType]) -> String {
        if type_args.is_empty() {
            return base_name.to_string();
        }

        let mut result = base_name.to_string();
        result.push('_');

        for (i, arg) in type_args.iter().enumerate() {
            if i > 0 {
                result.push('_');
            }

            match arg {
                ConcreteType::Type(ty) => {
                    result.push_str(&self.mangle_type(ty));
                }
                ConcreteType::ConstValue(val) => {
                    result.push('c');
                    result.push_str(&val.to_string());
                }
            }
        }

        result
    }

    /// Mangle a type into a string
    fn mangle_type(&self, ty: &HirType) -> String {
        match ty {
            HirType::Bit(width) => format!("bit{}", width),
            HirType::Logic(width) => format!("logic{}", width),
            HirType::Int(width) => format!("int{}", width),
            HirType::Nat(width) => format!("nat{}", width),
            HirType::Float16 => "fp16".to_string(),
            HirType::Float32 => "fp32".to_string(),
            HirType::Float64 => "fp64".to_string(),
            HirType::Bool => "bool".to_string(),
            HirType::Custom(name) => name.clone(),
            _ => "T".to_string(), // Fallback for complex types
        }
    }

    /// Replace generic calls with specialized calls in the HIR
    fn replace_calls_in_hir(&mut self, hir: &Hir) -> Hir {
        eprintln!("  [MONO] Replacing generic calls with specialized versions");

        let mut new_hir = hir.clone();

        // Replace calls in all functions
        for func in &mut new_hir.functions {
            // Create type context with function parameters
            let mut ctx = TypeContext::new();
            for param in &func.params {
                ctx.add_variable(param.name.clone(), param.param_type.clone());
            }

            eprintln!(
                "  [TYPE_CTX] Function '{}' has {} parameters in context",
                func.name,
                ctx.variables.len()
            );

            // Process statements with accumulating context (for let bindings)
            let (new_body, _final_ctx) = func.body.iter().fold(
                (Vec::new(), ctx.clone()),
                |(mut stmts, mut acc_ctx), stmt| {
                    let (new_stmt, updated_ctx) = self.replace_calls_in_statement_with_context(stmt, acc_ctx);
                    stmts.push(new_stmt);
                    (stmts, updated_ctx)
                }
            );
            func.body = new_body;
        }

        // Replace calls in implementation blocks
        for impl_block in &mut new_hir.implementations {
            // Replace in functions
            for func in &mut impl_block.functions {
                // Create type context with function parameters
                let mut func_ctx = TypeContext::new();
                for param in &func.params {
                    func_ctx.add_variable(param.name.clone(), param.param_type.clone());
                }

                func.body = func
                    .body
                    .iter()
                    .map(|stmt| self.replace_calls_in_statement(stmt, &func_ctx))
                    .collect();
            }

            // Replace in event blocks (no parameters)
            let event_ctx = TypeContext::new();
            for event_block in &mut impl_block.event_blocks {
                event_block.statements = event_block
                    .statements
                    .iter()
                    .map(|stmt| self.replace_calls_in_statement(stmt, &event_ctx))
                    .collect();
            }

            // Replace in assignments (no parameters)
            let assign_ctx = TypeContext::new();
            for assignment in &mut impl_block.assignments {
                assignment.rhs = self.replace_calls_in_expression(&assignment.rhs, &assign_ctx);
            }
        }

        // Add specialized functions to the HIR
        new_hir.functions.extend(self.specialized_functions.clone());

        eprintln!(
            "  [MONO] Replaced calls and added {} specialized functions",
            self.specialized_functions.len()
        );

        new_hir
    }

    /// Try to resolve a call as a trait method and generate specialized function
    ///
    /// Returns Some(specialized_function_name) if this is a resolvable trait method call
    fn try_resolve_trait_method(&mut self, call: &hir::HirCallExpr, ctx: &TypeContext) -> Option<String> {
        // Method calls have at least one argument (the receiver)
        if call.args.is_empty() {
            return None;
        }

        // Method calls don't have explicit type arguments
        if !call.type_args.is_empty() {
            return None;
        }

        // Try to infer the type of the first argument (receiver)
        let receiver_type = self.infer_simple_type(&call.args[0], ctx)?;
        let type_key = format!("{:?}", receiver_type);

        eprintln!(
            "    [TRAIT_RESOLVE] Checking if call to '{}' with receiver type '{}' is a trait method",
            call.function, type_key
        );

        // Look through all trait implementations for this type
        for ((trait_name, impl_type_key), methods) in &self.trait_methods {
            if impl_type_key == &type_key {
                if let Some(method_info) = methods.get(&call.function) {
                    // Found a matching trait method!
                    let specialized_name = format!("{}_{}_{}",
                        trait_name,
                        self.mangle_type(&receiver_type),
                        call.function
                    );

                    eprintln!(
                        "    [TRAIT_RESOLVE] ✅ Resolved '{}' to trait method '{}'",
                        call.function, specialized_name
                    );

                    // Check if we've already generated this function
                    if !self.specializations.contains_key(&specialized_name) {
                        // Clone method_info to avoid borrow checker issues
                        let method_info_cloned = method_info.clone();
                        // Generate the specialized function
                        self.generate_trait_method_function(
                            &specialized_name,
                            &method_info_cloned,
                            &receiver_type
                        );
                    }

                    return Some(specialized_name);
                }
            }
        }

        eprintln!(
            "    [TRAIT_RESOLVE] ❌ No trait method found for '{}'",
            call.function
        );

        None
    }

    /// Generate a specialized function from a trait method implementation
    fn generate_trait_method_function(
        &mut self,
        specialized_name: &str,
        method_info: &TraitMethodInfo,
        receiver_type: &HirType,
    ) {
        eprintln!(
            "    [TRAIT_GEN] Generating function '{}' from trait method '{}.{}'",
            specialized_name, method_info.trait_name, method_info.method_name
        );

        // Generate unique function ID
        let func_id = hir::FunctionId(self.specialized_functions.len() as u32 + 10000);

        // Substitute 'Self' type with concrete receiver type in parameters
        let specialized_params: Vec<hir::HirParameter> = method_info
            .parameters
            .iter()
            .map(|param| {
                let substituted_type = self.substitute_self_type(&param.param_type, receiver_type);
                hir::HirParameter {
                    name: param.name.clone(),
                    param_type: substituted_type,
                    default_value: param.default_value.clone(),
                }
            })
            .collect();

        // Substitute 'Self' type in return type
        let specialized_return_type = method_info
            .return_type
            .as_ref()
            .map(|ty| self.substitute_self_type(ty, receiver_type));

        // Create specialized function
        let specialized_func = HirFunction {
            id: func_id,
            is_const: false,
            name: specialized_name.to_string(),
            generics: Vec::new(), // No generics in specialized version
            params: specialized_params,
            return_type: specialized_return_type,
            body: method_info.body.clone(), // TODO: May need to substitute Self in body too
        };

        eprintln!(
            "    [TRAIT_GEN] Created function '{}' with {} params",
            specialized_name,
            specialized_func.params.len()
        );

        // Record that we've generated this specialization
        self.specializations.insert(specialized_name.to_string(), specialized_name.to_string());

        // Add to specialized functions list
        self.specialized_functions.push(specialized_func);
    }

    /// Substitute 'Self' type with concrete type
    fn substitute_self_type(&self, ty: &HirType, concrete_type: &HirType) -> HirType {
        match ty {
            // 'Self' type gets replaced
            HirType::Custom(name) if name == "Self" => concrete_type.clone(),

            // Recursively substitute in composite types
            HirType::Array(elem_ty, size) => {
                HirType::Array(Box::new(self.substitute_self_type(elem_ty, concrete_type)), *size)
            }
            HirType::Tuple(types) => {
                HirType::Tuple(types.iter().map(|t| self.substitute_self_type(t, concrete_type)).collect())
            }
            HirType::Vec2(elem_ty) => {
                HirType::Vec2(Box::new(self.substitute_self_type(elem_ty, concrete_type)))
            }
            HirType::Vec3(elem_ty) => {
                HirType::Vec3(Box::new(self.substitute_self_type(elem_ty, concrete_type)))
            }
            HirType::Vec4(elem_ty) => {
                HirType::Vec4(Box::new(self.substitute_self_type(elem_ty, concrete_type)))
            }

            // Other types pass through unchanged
            _ => ty.clone(),
        }
    }

    /// Try to infer the type of an expression (simple cases only)
    ///
    /// This is a simplified type inference for method receiver resolution.
    /// Full type inference would be more complex and is out of scope for now.
    fn infer_simple_type(&self, expr: &hir::HirExpression, ctx: &TypeContext) -> Option<HirType> {
        match expr {
            // Generic parameters - look up in context!
            hir::HirExpression::GenericParam(name) => {
                if let Some(ty) = ctx.get_variable_type(name) {
                    eprintln!(
                        "    [TRAIT_RESOLVE] Found type for generic param '{}': {:?}",
                        name, ty
                    );
                    Some(ty.clone())
                } else {
                    eprintln!(
                        "    [TRAIT_RESOLVE] No type found for generic param '{}' in context",
                        name
                    );
                    None
                }
            }

            // Variables - look up by ID in context!
            hir::HirExpression::Variable(var_id) => {
                if let Some(ty) = ctx.get_variable_id_type(var_id) {
                    eprintln!(
                        "    [TRAIT_RESOLVE] Found type for variable ID {:?}: {:?}",
                        var_id, ty
                    );
                    Some(ty.clone())
                } else {
                    eprintln!(
                        "    [TRAIT_RESOLVE] No type found for variable ID {:?} in context",
                        var_id
                    );
                    None
                }
            }

            // Literals have obvious types
            hir::HirExpression::Literal(lit) => {
                match lit {
                    hir::HirLiteral::Integer(_) => Some(HirType::Nat(32)), // Default to nat[32]
                    hir::HirLiteral::Float(_) => Some(HirType::Float32),
                    hir::HirLiteral::Boolean(_) => Some(HirType::Bool),
                    _ => None,
                }
            }

            // Cast expressions have explicit target type
            hir::HirExpression::Cast(cast) => Some(cast.target_type.clone()),

            // For other expressions, we can't easily infer the type without a full type checker
            // In the future, we could:
            // - Infer from function return types
            // - Use trait bounds from generic parameters
            _ => {
                eprintln!("    [TRAIT_RESOLVE] Cannot infer type for expression: {:?}", expr);
                None
            }
        }
    }

    /// Replace calls in a statement, returning both the new statement and updated context
    fn replace_calls_in_statement_with_context(
        &mut self,
        stmt: &hir::HirStatement,
        ctx: TypeContext
    ) -> (hir::HirStatement, TypeContext) {
        match stmt {
            hir::HirStatement::Let(let_stmt) => {
                // Process the value expression first with current context
                let new_value = self.replace_calls_in_expression(&let_stmt.value, &ctx);

                // Create new context with this variable for subsequent statements
                let mut new_ctx = ctx.clone();
                new_ctx.add_variable(let_stmt.name.clone(), let_stmt.var_type.clone());
                new_ctx.add_variable_id(let_stmt.id, let_stmt.var_type.clone());

                eprintln!(
                    "  [TYPE_CTX] Added let binding '{}' (id={:?}) with type {:?}",
                    let_stmt.name, let_stmt.id, let_stmt.var_type
                );

                let new_stmt = hir::HirStatement::Let(hir::HirLetStatement {
                    id: let_stmt.id,
                    name: let_stmt.name.clone(),
                    mutable: let_stmt.mutable,
                    var_type: let_stmt.var_type.clone(),
                    value: new_value,
                });

                (new_stmt, new_ctx)
            }
            // All other statements don't modify context
            _ => {
                let new_stmt = self.replace_calls_in_statement(stmt, &ctx);
                (new_stmt, ctx)
            }
        }
    }

    /// Replace calls in a statement (preserves context)
    fn replace_calls_in_statement(&mut self, stmt: &hir::HirStatement, ctx: &TypeContext) -> hir::HirStatement {
        match stmt {
            hir::HirStatement::Assignment(assign) => {
                hir::HirStatement::Assignment(hir::HirAssignment {
                    id: assign.id,
                    lhs: assign.lhs.clone(),
                    assignment_type: assign.assignment_type.clone(),
                    rhs: self.replace_calls_in_expression(&assign.rhs, ctx),
                })
            }
            hir::HirStatement::Expression(expr) => {
                hir::HirStatement::Expression(self.replace_calls_in_expression(expr, ctx))
            }
            hir::HirStatement::Let(let_stmt) => {
                // For this non-context-returning version, just process the expression
                hir::HirStatement::Let(hir::HirLetStatement {
                    id: let_stmt.id,
                    name: let_stmt.name.clone(),
                    mutable: let_stmt.mutable,
                    var_type: let_stmt.var_type.clone(),
                    value: self.replace_calls_in_expression(&let_stmt.value, ctx),
                })
            }
            hir::HirStatement::Return(expr_opt) => {
                hir::HirStatement::Return(expr_opt.as_ref().map(|e| self.replace_calls_in_expression(e, ctx)))
            }
            hir::HirStatement::If(if_stmt) => hir::HirStatement::If(hir::HirIfStatement {
                condition: self.replace_calls_in_expression(&if_stmt.condition, ctx),
                then_statements: if_stmt
                    .then_statements
                    .iter()
                    .map(|s| self.replace_calls_in_statement(s, ctx))
                    .collect(),
                else_statements: if_stmt.else_statements.as_ref().map(|stmts| {
                    stmts.iter().map(|s| self.replace_calls_in_statement(s, ctx)).collect()
                }),
            }),
            hir::HirStatement::Match(match_stmt) => {
                hir::HirStatement::Match(hir::HirMatchStatement {
                    expr: self.replace_calls_in_expression(&match_stmt.expr, ctx),
                    arms: match_stmt
                        .arms
                        .iter()
                        .map(|arm| hir::HirMatchArm {
                            pattern: arm.pattern.clone(),
                            guard: arm.guard.as_ref().map(|g| self.replace_calls_in_expression(g, ctx)),
                            statements: arm
                                .statements
                                .iter()
                                .map(|s| self.replace_calls_in_statement(s, ctx))
                                .collect(),
                        })
                        .collect(),
                })
            }
            hir::HirStatement::Block(stmts) => hir::HirStatement::Block(
                stmts.iter().map(|s| self.replace_calls_in_statement(s, ctx)).collect(),
            ),
            _ => stmt.clone(),
        }
    }

    /// Replace calls in an expression
    fn replace_calls_in_expression(&mut self, expr: &hir::HirExpression, ctx: &TypeContext) -> hir::HirExpression {
        match expr {
            hir::HirExpression::Call(call) => {
                // Check if this is a generic call that needs replacement
                if !call.type_args.is_empty() {
                    if let Some((func_id, func)) = self.find_generic_function_by_name(&call.function) {
                        let type_args: Vec<ConcreteType> = call
                            .type_args
                            .iter()
                            .zip(&func.generics)
                            .map(|(ty, generic)| {
                                match &generic.param_type {
                                    hir::HirGenericType::Const(_) => {
                                        // For const parameters, extract the constant value
                                        match ty {
                                            HirType::NatExpr(expr) => {
                                                // Extract literal value from expression
                                                if let hir::HirExpression::Literal(lit) = expr.as_ref() {
                                                    let value = match lit {
                                                        hir::HirLiteral::Integer(v) => *v,
                                                        _ => 0,
                                                    };
                                                    ConcreteType::ConstValue(value)
                                                } else {
                                                    ConcreteType::Type(ty.clone())
                                                }
                                            }
                                            HirType::Bit(width) => {
                                                ConcreteType::ConstValue(*width as u64)
                                            }
                                            _ => {
                                                // Fallback: treat as type
                                                ConcreteType::Type(ty.clone())
                                            }
                                        }
                                    }
                                    _ => {
                                        // For type parameters, use the type directly
                                        ConcreteType::Type(ty.clone())
                                    }
                                }
                            })
                            .collect();

                        let key = self.make_specialization_key(func_id, &type_args);
                        if let Some(specialized_name) = self.specializations.get(&key) {
                            eprintln!(
                                "    [MONO] Replacing call '{}' -> '{}'",
                                call.function, specialized_name
                            );

                            // Replace with call to specialized function (no type args)
                            return hir::HirExpression::Call(hir::HirCallExpr {
                                function: specialized_name.clone(),
                                type_args: Vec::new(), // No type args in specialized call
                                args: call
                                    .args
                                    .iter()
                                    .map(|a| self.replace_calls_in_expression(a, ctx))
                                    .collect(),
                            });
                        }
                    }
                }

                // Try to resolve as trait method call
                if let Some(specialized_name) = self.try_resolve_trait_method(call, ctx) {
                    eprintln!(
                        "    [TRAIT] Replacing method call '{}' -> '{}'",
                        call.function, specialized_name
                    );

                    return hir::HirExpression::Call(hir::HirCallExpr {
                        function: specialized_name,
                        type_args: Vec::new(),
                        args: call
                            .args
                            .iter()
                            .map(|a| self.replace_calls_in_expression(a, ctx))
                            .collect(),
                    });
                }

                // Not a generic call or trait method, but still recurse into arguments
                hir::HirExpression::Call(hir::HirCallExpr {
                    function: call.function.clone(),
                    type_args: call.type_args.clone(),
                    args: call
                        .args
                        .iter()
                        .map(|a| self.replace_calls_in_expression(a, ctx))
                        .collect(),
                })
            }
            hir::HirExpression::Binary(binary) => {
                hir::HirExpression::Binary(hir::HirBinaryExpr {
                    left: Box::new(self.replace_calls_in_expression(&binary.left, ctx)),
                    op: binary.op.clone(),
                    right: Box::new(self.replace_calls_in_expression(&binary.right, ctx)),
                })
            }
            hir::HirExpression::Unary(unary) => hir::HirExpression::Unary(hir::HirUnaryExpr {
                op: unary.op.clone(),
                operand: Box::new(self.replace_calls_in_expression(&unary.operand, ctx)),
            }),
            hir::HirExpression::If(if_expr) => hir::HirExpression::If(hir::HirIfExpr {
                condition: Box::new(self.replace_calls_in_expression(&if_expr.condition, ctx)),
                then_expr: Box::new(self.replace_calls_in_expression(&if_expr.then_expr, ctx)),
                else_expr: Box::new(self.replace_calls_in_expression(&if_expr.else_expr, ctx)),
            }),
            hir::HirExpression::Match(match_expr) => {
                hir::HirExpression::Match(hir::HirMatchExpr {
                    expr: Box::new(self.replace_calls_in_expression(&match_expr.expr, ctx)),
                    arms: match_expr
                        .arms
                        .iter()
                        .map(|arm| hir::HirMatchArmExpr {
                            pattern: arm.pattern.clone(),
                            guard: arm.guard.as_ref().map(|g| self.replace_calls_in_expression(g, ctx)),
                            expr: self.replace_calls_in_expression(&arm.expr, ctx),
                        })
                        .collect(),
                })
            }
            hir::HirExpression::Block {
                statements,
                result_expr,
            } => hir::HirExpression::Block {
                statements: statements
                    .iter()
                    .map(|s| self.replace_calls_in_statement(s, ctx))
                    .collect(),
                result_expr: Box::new(self.replace_calls_in_expression(result_expr, ctx)),
            },
            hir::HirExpression::Cast(cast) => hir::HirExpression::Cast(hir::HirCastExpr {
                expr: Box::new(self.replace_calls_in_expression(&cast.expr, ctx)),
                target_type: cast.target_type.clone(),
            }),
            hir::HirExpression::StructLiteral(struct_lit) => {
                hir::HirExpression::StructLiteral(hir::HirStructLiteral {
                    type_name: struct_lit.type_name.clone(),
                    fields: struct_lit
                        .fields
                        .iter()
                        .map(|field| hir::HirStructFieldInit {
                            name: field.name.clone(),
                            value: self.replace_calls_in_expression(&field.value, ctx),
                        })
                        .collect(),
                })
            }
            hir::HirExpression::TupleLiteral(exprs) => hir::HirExpression::TupleLiteral(
                exprs
                    .iter()
                    .map(|e| self.replace_calls_in_expression(e, ctx))
                    .collect(),
            ),
            hir::HirExpression::ArrayLiteral(exprs) => hir::HirExpression::ArrayLiteral(
                exprs
                    .iter()
                    .map(|e| self.replace_calls_in_expression(e, ctx))
                    .collect(),
            ),
            hir::HirExpression::Concat(exprs) => hir::HirExpression::Concat(
                exprs
                    .iter()
                    .map(|e| self.replace_calls_in_expression(e, ctx))
                    .collect(),
            ),
            hir::HirExpression::Ternary {
                condition,
                true_expr,
                false_expr,
            } => hir::HirExpression::Ternary {
                condition: Box::new(self.replace_calls_in_expression(condition, ctx)),
                true_expr: Box::new(self.replace_calls_in_expression(true_expr, ctx)),
                false_expr: Box::new(self.replace_calls_in_expression(false_expr, ctx)),
            },
            hir::HirExpression::Index(base, index) => hir::HirExpression::Index(
                Box::new(self.replace_calls_in_expression(base, ctx)),
                Box::new(self.replace_calls_in_expression(index, ctx)),
            ),
            hir::HirExpression::Range(base, start, end) => hir::HirExpression::Range(
                Box::new(self.replace_calls_in_expression(base, ctx)),
                Box::new(self.replace_calls_in_expression(start, ctx)),
                Box::new(self.replace_calls_in_expression(end, ctx)),
            ),
            hir::HirExpression::FieldAccess { base, field } => {
                hir::HirExpression::FieldAccess {
                    base: Box::new(self.replace_calls_in_expression(base, ctx)),
                    field: field.clone(),
                }
            }
            // Leaf expressions - no replacement needed
            _ => expr.clone(),
        }
    }
}

impl Default for Monomorphizer {
    fn default() -> Self {
        Self::new()
    }
}
