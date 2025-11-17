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
        }
    }

    /// Monomorphize all generic functions in a HIR
    ///
    /// This is the main entry point for monomorphization.
    pub fn monomorphize(&mut self, hir: &Hir) -> Hir {
        eprintln!("🔄 [MONOMORPHIZE] Starting monomorphization pass");

        // Step 1: Identify generic functions
        self.identify_generic_functions(hir);

        // Step 2: Collect all calls to generic functions
        self.collect_generic_calls(hir);

        // Step 3: Generate specializations (breadth-first)
        self.generate_all_specializations();

        // Step 4: Replace generic calls with specialized calls
        self.replace_calls_in_hir(hir)
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
    fn replace_calls_in_hir(&self, hir: &Hir) -> Hir {
        eprintln!("  [MONO] Replacing generic calls with specialized versions");

        let mut new_hir = hir.clone();

        // Replace calls in all functions
        for func in &mut new_hir.functions {
            func.body = func
                .body
                .iter()
                .map(|stmt| self.replace_calls_in_statement(stmt))
                .collect();
        }

        // Replace calls in implementation blocks
        for impl_block in &mut new_hir.implementations {
            // Replace in functions
            for func in &mut impl_block.functions {
                func.body = func
                    .body
                    .iter()
                    .map(|stmt| self.replace_calls_in_statement(stmt))
                    .collect();
            }

            // Replace in event blocks
            for event_block in &mut impl_block.event_blocks {
                event_block.statements = event_block
                    .statements
                    .iter()
                    .map(|stmt| self.replace_calls_in_statement(stmt))
                    .collect();
            }

            // Replace in assignments
            for assignment in &mut impl_block.assignments {
                assignment.rhs = self.replace_calls_in_expression(&assignment.rhs);
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

    /// Replace calls in a statement
    fn replace_calls_in_statement(&self, stmt: &hir::HirStatement) -> hir::HirStatement {
        match stmt {
            hir::HirStatement::Assignment(assign) => {
                hir::HirStatement::Assignment(hir::HirAssignment {
                    id: assign.id,
                    lhs: assign.lhs.clone(),
                    assignment_type: assign.assignment_type.clone(),
                    rhs: self.replace_calls_in_expression(&assign.rhs),
                })
            }
            hir::HirStatement::Expression(expr) => {
                hir::HirStatement::Expression(self.replace_calls_in_expression(expr))
            }
            hir::HirStatement::Let(let_stmt) => hir::HirStatement::Let(hir::HirLetStatement {
                id: let_stmt.id,
                name: let_stmt.name.clone(),
                mutable: let_stmt.mutable,
                var_type: let_stmt.var_type.clone(),
                value: self.replace_calls_in_expression(&let_stmt.value),
            }),
            hir::HirStatement::Return(expr_opt) => {
                hir::HirStatement::Return(expr_opt.as_ref().map(|e| self.replace_calls_in_expression(e)))
            }
            hir::HirStatement::If(if_stmt) => hir::HirStatement::If(hir::HirIfStatement {
                condition: self.replace_calls_in_expression(&if_stmt.condition),
                then_statements: if_stmt
                    .then_statements
                    .iter()
                    .map(|s| self.replace_calls_in_statement(s))
                    .collect(),
                else_statements: if_stmt.else_statements.as_ref().map(|stmts| {
                    stmts.iter().map(|s| self.replace_calls_in_statement(s)).collect()
                }),
            }),
            hir::HirStatement::Match(match_stmt) => {
                hir::HirStatement::Match(hir::HirMatchStatement {
                    expr: self.replace_calls_in_expression(&match_stmt.expr),
                    arms: match_stmt
                        .arms
                        .iter()
                        .map(|arm| hir::HirMatchArm {
                            pattern: arm.pattern.clone(),
                            guard: arm.guard.as_ref().map(|g| self.replace_calls_in_expression(g)),
                            statements: arm
                                .statements
                                .iter()
                                .map(|s| self.replace_calls_in_statement(s))
                                .collect(),
                        })
                        .collect(),
                })
            }
            hir::HirStatement::Block(stmts) => hir::HirStatement::Block(
                stmts.iter().map(|s| self.replace_calls_in_statement(s)).collect(),
            ),
            _ => stmt.clone(),
        }
    }

    /// Replace calls in an expression
    fn replace_calls_in_expression(&self, expr: &hir::HirExpression) -> hir::HirExpression {
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
                                    .map(|a| self.replace_calls_in_expression(a))
                                    .collect(),
                            });
                        }
                    }
                }

                // Not a generic call, but still recurse into arguments
                hir::HirExpression::Call(hir::HirCallExpr {
                    function: call.function.clone(),
                    type_args: call.type_args.clone(),
                    args: call
                        .args
                        .iter()
                        .map(|a| self.replace_calls_in_expression(a))
                        .collect(),
                })
            }
            hir::HirExpression::Binary(binary) => {
                hir::HirExpression::Binary(hir::HirBinaryExpr {
                    left: Box::new(self.replace_calls_in_expression(&binary.left)),
                    op: binary.op.clone(),
                    right: Box::new(self.replace_calls_in_expression(&binary.right)),
                })
            }
            hir::HirExpression::Unary(unary) => hir::HirExpression::Unary(hir::HirUnaryExpr {
                op: unary.op.clone(),
                operand: Box::new(self.replace_calls_in_expression(&unary.operand)),
            }),
            hir::HirExpression::If(if_expr) => hir::HirExpression::If(hir::HirIfExpr {
                condition: Box::new(self.replace_calls_in_expression(&if_expr.condition)),
                then_expr: Box::new(self.replace_calls_in_expression(&if_expr.then_expr)),
                else_expr: Box::new(self.replace_calls_in_expression(&if_expr.else_expr)),
            }),
            hir::HirExpression::Match(match_expr) => {
                hir::HirExpression::Match(hir::HirMatchExpr {
                    expr: Box::new(self.replace_calls_in_expression(&match_expr.expr)),
                    arms: match_expr
                        .arms
                        .iter()
                        .map(|arm| hir::HirMatchArmExpr {
                            pattern: arm.pattern.clone(),
                            guard: arm.guard.as_ref().map(|g| self.replace_calls_in_expression(g)),
                            expr: self.replace_calls_in_expression(&arm.expr),
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
                    .map(|s| self.replace_calls_in_statement(s))
                    .collect(),
                result_expr: Box::new(self.replace_calls_in_expression(result_expr)),
            },
            hir::HirExpression::Cast(cast) => hir::HirExpression::Cast(hir::HirCastExpr {
                expr: Box::new(self.replace_calls_in_expression(&cast.expr)),
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
                            value: self.replace_calls_in_expression(&field.value),
                        })
                        .collect(),
                })
            }
            hir::HirExpression::TupleLiteral(exprs) => hir::HirExpression::TupleLiteral(
                exprs
                    .iter()
                    .map(|e| self.replace_calls_in_expression(e))
                    .collect(),
            ),
            hir::HirExpression::ArrayLiteral(exprs) => hir::HirExpression::ArrayLiteral(
                exprs
                    .iter()
                    .map(|e| self.replace_calls_in_expression(e))
                    .collect(),
            ),
            hir::HirExpression::Concat(exprs) => hir::HirExpression::Concat(
                exprs
                    .iter()
                    .map(|e| self.replace_calls_in_expression(e))
                    .collect(),
            ),
            hir::HirExpression::Ternary {
                condition,
                true_expr,
                false_expr,
            } => hir::HirExpression::Ternary {
                condition: Box::new(self.replace_calls_in_expression(condition)),
                true_expr: Box::new(self.replace_calls_in_expression(true_expr)),
                false_expr: Box::new(self.replace_calls_in_expression(false_expr)),
            },
            hir::HirExpression::Index(base, index) => hir::HirExpression::Index(
                Box::new(self.replace_calls_in_expression(base)),
                Box::new(self.replace_calls_in_expression(index)),
            ),
            hir::HirExpression::Range(base, start, end) => hir::HirExpression::Range(
                Box::new(self.replace_calls_in_expression(base)),
                Box::new(self.replace_calls_in_expression(start)),
                Box::new(self.replace_calls_in_expression(end)),
            ),
            hir::HirExpression::FieldAccess { base, field } => {
                hir::HirExpression::FieldAccess {
                    base: Box::new(self.replace_calls_in_expression(base)),
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
