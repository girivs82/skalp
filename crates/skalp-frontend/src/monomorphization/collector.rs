//! Instantiation Collector
//!
//! Walks the HIR to find all generic entity instantiations

use crate::const_eval::{ConstEvaluator, ConstValue};
use crate::hir::{
    EntityId, Hir, HirEntity, HirExpression, HirGeneric, HirGenericType, HirImplementation,
    HirInstance, HirPort, HirType,
};
use std::collections::{HashMap, HashSet};

/// A specific instantiation of a generic entity
#[derive(Debug, Clone)]
pub struct Instantiation {
    /// Name of the generic entity
    pub entity_name: String,
    /// Entity ID
    pub entity_id: EntityId,
    /// Type arguments (parameter name -> concrete type)
    pub type_args: HashMap<String, HirType>,
    /// Const arguments (parameter name -> evaluated value)
    pub const_args: HashMap<String, ConstValue>,
    /// Intent arguments (parameter name -> intent value)
    pub intent_args: HashMap<String, IntentValue>,
}

// Manual PartialEq implementation
impl PartialEq for Instantiation {
    fn eq(&self, other: &Self) -> bool {
        self.entity_name == other.entity_name
            && self.entity_id == other.entity_id
            && self.mangled_name() == other.mangled_name()
    }
}

impl Eq for Instantiation {}

// Manual Hash implementation based on mangled name
impl std::hash::Hash for Instantiation {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.entity_id.hash(state);
        self.mangled_name().hash(state);
    }
}

/// Intent parameter value
#[derive(Debug, Clone)]
pub struct IntentValue {
    /// Intent name (e.g., "FAST_INTENT", "SMALL_INTENT")
    pub name: String,
    /// Intent fields (for compile-time evaluation)
    pub fields: HashMap<String, ConstValue>,
}

impl PartialEq for IntentValue {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for IntentValue {}

impl std::hash::Hash for IntentValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl Instantiation {
    /// Generate a unique mangled name for this instantiation
    pub fn mangled_name(&self) -> String {
        let mut parts = vec![self.entity_name.clone()];

        // Add const arguments in sorted order for determinism
        let mut const_keys: Vec<_> = self.const_args.keys().collect();
        const_keys.sort();
        for key in const_keys {
            if let Some(value) = self.const_args.get(key) {
                parts.push(mangle_const_value(value));
            }
        }

        // Add type arguments
        let mut type_keys: Vec<_> = self.type_args.keys().collect();
        type_keys.sort();
        for key in type_keys {
            if let Some(ty) = self.type_args.get(key) {
                parts.push(mangle_type(ty));
            }
        }

        // Add intent arguments
        let mut intent_keys: Vec<_> = self.intent_args.keys().collect();
        intent_keys.sort();
        for key in intent_keys {
            if let Some(intent) = self.intent_args.get(key) {
                parts.push(intent.name.clone());
            }
        }

        parts.join("_")
    }
}

/// Mangle a const value into a string
fn mangle_const_value(value: &ConstValue) -> String {
    match value {
        ConstValue::Nat(n) => n.to_string(),
        ConstValue::Int(i) => {
            if *i >= 0 {
                i.to_string()
            } else {
                format!("n{}", i.abs())
            }
        }
        ConstValue::Bool(b) => if *b { "true" } else { "false" }.to_string(),
        ConstValue::String(s) => s.replace([' ', '-'], "_"),
        ConstValue::Float(f) => format!("f{}", f.to_bits()),
        ConstValue::FloatFormat(fmt) => format!("fp{}", fmt.total_bits),
        ConstValue::Struct(_) => "struct".to_string(),
    }
}

/// Mangle a type into a string
fn mangle_type(ty: &HirType) -> String {
    match ty {
        HirType::Custom(name) => name.clone(),
        HirType::Bit(width) => format!("bit{}", width),
        HirType::Array(elem, size) => format!("arr{}_{}", mangle_type(elem), size),
        HirType::Clock(_) => "clock".to_string(),
        HirType::Reset { .. } => "reset".to_string(),
        HirType::Float32 => "fp32".to_string(),
        HirType::Float64 => "fp64".to_string(),
        HirType::Float16 => "fp16".to_string(),
        HirType::FpParametric { .. } => "fp".to_string(),
        HirType::FixedParametric { .. } => "fixed".to_string(),
        HirType::IntParametric { .. } => "int".to_string(),
        HirType::VecParametric { .. } => "vec".to_string(),
        // Vector types
        HirType::Vec2(elem) => format!("vec2_{}", mangle_type(elem)),
        HirType::Vec3(elem) => format!("vec3_{}", mangle_type(elem)),
        HirType::Vec4(elem) => format!("vec4_{}", mangle_type(elem)),
        // Struct types
        HirType::Struct(struct_type) => struct_type.name.clone(),
        // Other parametric types
        HirType::Nat(_) => "nat".to_string(),
        HirType::Int(_) => "int".to_string(),
        HirType::Logic(_) => "logic".to_string(),
        HirType::Bool => "bool".to_string(),
        _ => "unknown".to_string(),
    }
}

/// Collects all generic instantiations from HIR
pub struct InstantiationCollector<'hir> {
    /// All discovered instantiations
    instantiations: HashSet<Instantiation>,
    /// Const evaluator for parameter values
    evaluator: ConstEvaluator,
    /// Map from entity ID to entity
    entities: HashMap<EntityId, HirEntity>,
    /// Reference to HIR for type lookups
    hir: &'hir Hir,
    /// Current implementation context (for looking up signals/variables)
    current_impl: Option<&'hir HirImplementation>,
}

impl<'hir> InstantiationCollector<'hir> {
    /// Create a new instantiation collector
    pub fn new(hir: &'hir Hir) -> Self {
        let mut entities = HashMap::new();
        for entity in &hir.entities {
            entities.insert(entity.id, entity.clone());
        }

        // Create evaluator and register enums for enum variant resolution
        let mut evaluator = ConstEvaluator::new();
        for user_type in &hir.user_defined_types {
            if let crate::hir::HirType::Enum(enum_type) = &user_type.type_def {
                evaluator.register_enum(enum_type.as_ref().clone());
            }
        }

        // BUG #179 FIX: Only register constants from GLOBAL impl blocks (EntityId(0))
        // Entity-specific constants (like W, E, M in impl FpAdd<F>) have IDs that may
        // collide with global constants. Entity-specific constants should only be used
        // when processing that entity.
        // Global constants (like IEEE754_32, MODE_SYSTOLIC) are stored in impl with EntityId(0)
        for implementation in &hir.implementations {
            if implementation.entity == crate::hir::EntityId(0)
                && !implementation.constants.is_empty()
            {
                evaluator.register_constants(&implementation.constants);
            }
        }

        Self {
            instantiations: HashSet::new(),
            evaluator,
            entities,
            hir,
            current_impl: None,
        }
    }

    /// Collect all instantiations from the HIR
    pub fn collect(mut self, hir: &'hir Hir) -> HashSet<Instantiation> {
        // Start from all implementations
        for implementation in &hir.implementations {
            self.collect_from_implementation(implementation);
        }

        // Also collect from trait implementations (where entity instantiations live)
        for trait_impl in &hir.trait_implementations {
            eprintln!(
                "[COLLECTOR] Processing trait impl '{}' on {:?}: {} methods",
                trait_impl.trait_name,
                trait_impl.target,
                trait_impl.method_implementations.len()
            );
            for method_impl in &trait_impl.method_implementations {
                eprintln!(
                    "[COLLECTOR]   Method '{}' has {} body statements",
                    method_impl.name,
                    method_impl.body.len()
                );
                for stmt in &method_impl.body {
                    self.collect_from_statement(stmt);
                }
            }
        }

        self.instantiations
    }

    /// Collect instantiations from an implementation
    fn collect_from_implementation(&mut self, implementation: &'hir HirImplementation) {
        // Find entity name for logging
        let entity_name = self
            .hir
            .entities
            .iter()
            .find(|e| e.id == implementation.entity)
            .map(|e| e.name.as_str())
            .unwrap_or("unknown");

        eprintln!(
            "[COLLECTOR] Processing impl for entity '{}': {} instances, {} event_blocks, {} statements, {} assignments",
            entity_name,
            implementation.instances.len(),
            implementation.event_blocks.len(),
            implementation.statements.len(),
            implementation.assignments.len()
        );

        // Set current implementation context
        self.current_impl = Some(implementation);

        // Register implementation's constants for const identifier resolution
        // This enables `const WIDTH: nat = 32; let adder = Adder<WIDTH> { ... }`
        self.evaluator.register_constants(&implementation.constants);

        // Collect from all module instances
        for instance in &implementation.instances {
            self.collect_from_instance(instance);
        }

        // Collect from event blocks (which contain statements with struct literals)
        for event_block in &implementation.event_blocks {
            for stmt in &event_block.statements {
                self.collect_from_statement(stmt);
            }
        }

        // Collect from standalone statements
        for stmt in &implementation.statements {
            self.collect_from_statement(stmt);
        }

        // Collect from assignments (RHS can contain struct literals)
        for assignment in &implementation.assignments {
            self.collect_from_expression(&assignment.rhs);
        }

        // Clear current implementation context
        self.current_impl = None;
    }

    /// Collect instantiations from a statement (walking nested expressions)
    fn collect_from_statement(&mut self, stmt: &crate::hir::HirStatement) {
        use crate::hir::HirStatement;
        match stmt {
            HirStatement::Let(let_stmt) => {
                self.collect_from_expression(&let_stmt.value);
            }
            HirStatement::Assignment(assignment) => {
                self.collect_from_expression(&assignment.rhs);
            }
            HirStatement::If(if_stmt) => {
                self.collect_from_expression(&if_stmt.condition);
                for s in &if_stmt.then_statements {
                    self.collect_from_statement(s);
                }
                if let Some(else_stmts) = &if_stmt.else_statements {
                    for s in else_stmts {
                        self.collect_from_statement(s);
                    }
                }
            }
            HirStatement::For(for_stmt) => {
                self.collect_from_expression(&for_stmt.range.start);
                self.collect_from_expression(&for_stmt.range.end);
                if let Some(step) = &for_stmt.range.step {
                    self.collect_from_expression(step);
                }
                for s in &for_stmt.body {
                    self.collect_from_statement(s);
                }
            }
            HirStatement::Match(match_stmt) => {
                self.collect_from_expression(&match_stmt.expr);
                for arm in &match_stmt.arms {
                    for s in &arm.statements {
                        self.collect_from_statement(s);
                    }
                }
            }
            HirStatement::Return(Some(expr)) => {
                self.collect_from_expression(expr);
            }
            _ => {}
        }
    }

    /// Collect instantiations from an expression (looking for struct literal entity instantiations)
    fn collect_from_expression(&mut self, expr: &HirExpression) {
        match expr {
            HirExpression::StructLiteral(struct_lit) => {
                eprintln!(
                    "[COLLECTOR] Found StructLiteral: type_name='{}', generic_args={}",
                    struct_lit.type_name,
                    struct_lit.generic_args.len()
                );
                // Check if this struct literal corresponds to an entity
                self.try_collect_struct_literal_entity(struct_lit);
                // Also recurse into field values
                for field in &struct_lit.fields {
                    self.collect_from_expression(&field.value);
                }
            }
            HirExpression::Binary(binary) => {
                self.collect_from_expression(&binary.left);
                self.collect_from_expression(&binary.right);
            }
            HirExpression::Unary(unary) => {
                self.collect_from_expression(&unary.operand);
            }
            HirExpression::Index(base, index) => {
                self.collect_from_expression(base);
                self.collect_from_expression(index);
            }
            HirExpression::Ternary {
                condition,
                true_expr,
                false_expr,
            } => {
                self.collect_from_expression(condition);
                self.collect_from_expression(true_expr);
                self.collect_from_expression(false_expr);
            }
            HirExpression::Call(call) => {
                for arg in &call.args {
                    self.collect_from_expression(arg);
                }
            }
            HirExpression::Cast(cast) => {
                self.collect_from_expression(&cast.expr);
            }
            HirExpression::Concat(elements) => {
                for elem in elements {
                    self.collect_from_expression(elem);
                }
            }
            HirExpression::ArrayRepeat { value, count } => {
                self.collect_from_expression(value);
                self.collect_from_expression(count);
            }
            HirExpression::ArrayLiteral(elements) => {
                for elem in elements {
                    self.collect_from_expression(elem);
                }
            }
            HirExpression::FieldAccess { base, .. } => {
                self.collect_from_expression(base);
            }
            HirExpression::If(if_expr) => {
                self.collect_from_expression(&if_expr.condition);
                self.collect_from_expression(&if_expr.then_expr);
                self.collect_from_expression(&if_expr.else_expr);
            }
            HirExpression::Match(match_expr) => {
                self.collect_from_expression(&match_expr.expr);
                for arm in &match_expr.arms {
                    self.collect_from_expression(&arm.expr);
                }
            }
            HirExpression::Block {
                statements,
                result_expr,
            } => {
                for stmt in statements {
                    self.collect_from_statement(stmt);
                }
                self.collect_from_expression(result_expr);
            }
            HirExpression::Range(start, end, _step) => {
                self.collect_from_expression(start);
                self.collect_from_expression(end);
            }
            HirExpression::TupleLiteral(elements) => {
                for elem in elements {
                    self.collect_from_expression(elem);
                }
            }
            _ => {}
        }
    }

    /// Try to collect an instantiation from a struct literal that matches an entity
    fn try_collect_struct_literal_entity(&mut self, struct_lit: &crate::hir::HirStructLiteral) {
        let type_name = &struct_lit.type_name;
        eprintln!(
            "[COLLECTOR] Looking for entity '{}' among {} entities (struct literal has {} generic_args)",
            type_name,
            self.hir.entities.len(),
            struct_lit.generic_args.len()
        );

        // Find the entity by name
        let entity = match self.hir.entities.iter().find(|e| e.name == *type_name) {
            Some(e) => e.clone(),
            None => {
                eprintln!("[COLLECTOR] Entity '{}' NOT FOUND", type_name);
                return;
            }
        };

        eprintln!(
            "[COLLECTOR] Found entity '{}' with {} generics",
            entity.name,
            entity.generics.len()
        );

        // If entity has no generics, nothing to collect
        if entity.generics.is_empty() {
            eprintln!(
                "[COLLECTOR] Entity '{}' has no generics, skipping",
                entity.name
            );
            return;
        }

        // Build instantiation record
        let mut type_args = HashMap::new();
        let mut const_args = HashMap::new();
        let mut intent_args = HashMap::new();

        // Extract generic arguments from the struct literal
        for (i, arg) in struct_lit.generic_args.iter().enumerate() {
            if i >= entity.generics.len() {
                break;
            }

            let generic = &entity.generics[i];
            eprintln!(
                "[COLLECTOR] Processing generic arg[{}] for '{}' (type: {:?})",
                i, generic.name, generic.param_type
            );

            match &generic.param_type {
                HirGenericType::Type => {
                    if let Some(ty) = self.extract_type_from_expr(arg) {
                        type_args.insert(generic.name.clone(), ty);
                    }
                }
                HirGenericType::Const(_const_type) => {
                    if let Ok(value) = self.evaluator.eval(arg) {
                        const_args.insert(generic.name.clone(), value);
                    }
                }
                HirGenericType::Intent => {
                    let intent_name = self.extract_intent_name(arg);
                    let intent_value = IntentValue {
                        name: intent_name,
                        fields: HashMap::new(),
                    };
                    intent_args.insert(generic.name.clone(), intent_value);
                }
                _ => {}
            }
        }

        // For generic parameters without provided arguments, use defaults
        for (i, generic) in entity.generics.iter().enumerate() {
            let provided_positionally = i < struct_lit.generic_args.len();

            if !provided_positionally {
                match &generic.param_type {
                    HirGenericType::Const(_const_type) => {
                        if let Some(ref default) = generic.default_value {
                            if let Ok(value) = self.evaluator.eval(default) {
                                const_args.entry(generic.name.clone()).or_insert(value);
                            }
                        }
                    }
                    HirGenericType::Intent => {
                        if let Some(ref default) = generic.default_value {
                            let intent_name = self.extract_intent_name(default);
                            let intent_value = IntentValue {
                                name: intent_name,
                                fields: HashMap::new(),
                            };
                            intent_args
                                .entry(generic.name.clone())
                                .or_insert(intent_value);
                        }
                    }
                    _ => {}
                }
            }
        }

        eprintln!(
            "[COLLECTOR] Creating instantiation for '{}': const_args={:?}",
            entity.name, const_args
        );

        // Create instantiation record
        let instantiation = Instantiation {
            entity_name: entity.name.clone(),
            entity_id: entity.id,
            type_args,
            const_args,
            intent_args,
        };

        self.instantiations.insert(instantiation);
    }

    /// Collect instantiation from a module instance
    fn collect_from_instance(&mut self, instance: &HirInstance) {
        // Get the entity being instantiated
        let entity_id = instance.entity;

        let entity = match self.entities.get(&entity_id) {
            Some(e) => e,
            None => return, // Entity not found
        };

        // If entity has no generics, nothing to collect
        if entity.generics.is_empty() {
            return;
        }

        // Build instantiation record
        let mut type_args = HashMap::new();
        let mut const_args = HashMap::new();
        let mut intent_args = HashMap::new();

        // Extract generic arguments from instance
        // Match provided arguments with entity's generic parameters
        for (i, arg) in instance.generic_args.iter().enumerate() {
            if i >= entity.generics.len() {
                break; // More arguments than parameters
            }

            let generic = &entity.generics[i];

            match &generic.param_type {
                HirGenericType::Type => {
                    // Type parameter - extract type from expression
                    if let Some(ty) = self.extract_type_from_expr(arg) {
                        type_args.insert(generic.name.clone(), ty);
                    }
                }
                HirGenericType::Const(_const_type) => {
                    // Const parameter - evaluate the argument expression
                    if let Ok(value) = self.evaluator.eval(arg) {
                        const_args.insert(generic.name.clone(), value);
                    }
                }
                HirGenericType::Intent => {
                    // Intent parameter - extract intent value
                    let intent_name = self.extract_intent_name(arg);
                    let intent_value = IntentValue {
                        name: intent_name.clone(),
                        fields: HashMap::new(), // Would be populated from intent definition
                    };
                    intent_args.insert(generic.name.clone(), intent_value);
                }
                _ => {}
            }
        }

        // Process named generic arguments (e.g., Entity<WIDTH: 32, DEPTH: 16>)
        for (param_name, arg) in &instance.named_generic_args {
            // Find the generic parameter with this name
            if let Some(generic) = entity.generics.iter().find(|g| &g.name == param_name) {
                match &generic.param_type {
                    HirGenericType::Type => {
                        if let Some(ty) = self.extract_type_from_expr(arg) {
                            type_args.insert(param_name.clone(), ty);
                        }
                    }
                    HirGenericType::Const(_const_type) => {
                        if let Ok(value) = self.evaluator.eval(arg) {
                            const_args.insert(param_name.clone(), value);
                        }
                    }
                    HirGenericType::Intent => {
                        let intent_name = self.extract_intent_name(arg);
                        let intent_value = IntentValue {
                            name: intent_name.clone(),
                            fields: HashMap::new(),
                        };
                        intent_args.insert(param_name.clone(), intent_value);
                    }
                    _ => {}
                }
            }
        }

        // For generic parameters without provided arguments, use defaults
        // Skip if already provided positionally or by name
        for (i, generic) in entity.generics.iter().enumerate() {
            let provided_positionally = i < instance.generic_args.len();
            let provided_by_name = instance.named_generic_args.contains_key(&generic.name);

            if !provided_positionally && !provided_by_name {
                match &generic.param_type {
                    HirGenericType::Const(_const_type) => {
                        if let Some(ref default) = generic.default_value {
                            if let Ok(value) = self.evaluator.eval(default) {
                                const_args.entry(generic.name.clone()).or_insert(value);
                            }
                        }
                    }
                    HirGenericType::Intent => {
                        if let Some(ref default) = generic.default_value {
                            let intent_name = self.extract_intent_name(default);
                            let intent_value = IntentValue {
                                name: intent_name,
                                fields: HashMap::new(),
                            };
                            intent_args
                                .entry(generic.name.clone())
                                .or_insert(intent_value);
                        }
                    }
                    _ => {}
                }
            }
        }

        // If type_args is still empty, try to infer from port connections
        if type_args.is_empty() {
            type_args = self.infer_type_args_from_connections(entity, instance);
        }

        // Create instantiation record
        let instantiation = Instantiation {
            entity_name: entity.name.clone(),
            entity_id,
            type_args,
            const_args,
            intent_args,
        };

        self.instantiations.insert(instantiation);
    }

    /// Extract type from expression
    ///
    /// Since type arguments aren't directly represented in HirExpression yet,
    /// we try to infer them from various expression forms.
    fn extract_type_from_expr(&self, expr: &HirExpression) -> Option<HirType> {
        match expr {
            // Custom type reference might be stored as a constant identifier
            HirExpression::Constant(const_id) => {
                // Try to look up the constant name and match it to a type
                // For now, we can't extract types from constants without more context
                None
            }
            // Cast expressions contain type information
            HirExpression::Cast(cast_expr) => Some(cast_expr.target_type.clone()),
            // For other expressions, we can't extract type information
            // Type arguments should ideally be passed as a separate mechanism
            _ => None,
        }
    }

    /// Infer type arguments from port connections
    ///
    /// When generic type arguments aren't explicitly provided, we can sometimes
    /// infer them from the types of signals connected to generic ports.
    fn infer_type_args_from_connections(
        &self,
        entity: &HirEntity,
        instance: &HirInstance,
    ) -> HashMap<String, HirType> {
        let mut inferred_types = HashMap::new();

        // Build map of generic type parameter names
        let mut type_param_names = HashSet::new();
        for generic in &entity.generics {
            if matches!(generic.param_type, HirGenericType::Type) {
                type_param_names.insert(generic.name.clone());
            }
        }

        // For each port, check if its type is a generic parameter
        // If so, try to infer the concrete type from the connection
        for port in &entity.ports {
            // Check if port type is a generic type parameter
            let param_name = match &port.port_type {
                HirType::Custom(name) if type_param_names.contains(name) => name.clone(),
                HirType::Vec2(elem) => {
                    if let HirType::Custom(name) = elem.as_ref() {
                        if type_param_names.contains(name) {
                            name.clone()
                        } else {
                            continue;
                        }
                    } else {
                        continue;
                    }
                }
                HirType::Vec3(elem) | HirType::Vec4(elem) => {
                    if let HirType::Custom(name) = elem.as_ref() {
                        if type_param_names.contains(name) {
                            name.clone()
                        } else {
                            continue;
                        }
                    } else {
                        continue;
                    }
                }
                HirType::Array(elem, _) => {
                    if let HirType::Custom(name) = elem.as_ref() {
                        if type_param_names.contains(name) {
                            name.clone()
                        } else {
                            continue;
                        }
                    } else {
                        continue;
                    }
                }
                _ => continue,
            };

            // Find the connection for this port
            if let Some(connection) = instance.connections.iter().find(|c| c.port == port.name) {
                // Try to infer the type from the connected expression
                if let Some(concrete_type) = self.infer_expr_type(&connection.expr) {
                    inferred_types
                        .entry(param_name.clone())
                        .or_insert(concrete_type);
                }
            }
        }

        inferred_types
    }

    /// Try to infer the type of an expression
    ///
    /// This is a simplified type inference that handles common cases.
    /// A full type inference would require the complete type context.
    fn infer_expr_type(&self, expr: &HirExpression) -> Option<HirType> {
        match expr {
            // For struct literals, infer the struct type
            HirExpression::StructLiteral(struct_lit) => {
                Some(HirType::Custom(struct_lit.type_name.clone()))
            }
            // Cast expressions have explicit types
            HirExpression::Cast(cast_expr) => Some(cast_expr.target_type.clone()),
            // Signal references
            HirExpression::Signal(signal_id) => {
                if let Some(impl_ctx) = self.current_impl {
                    impl_ctx
                        .signals
                        .iter()
                        .find(|s| s.id == *signal_id)
                        .map(|s| s.signal_type.clone())
                } else {
                    None
                }
            }
            // Port references
            HirExpression::Port(port_id) => {
                // Look up port from all entities
                for entity in self.hir.entities.iter() {
                    if let Some(port) = entity.ports.iter().find(|p| p.id == *port_id) {
                        return Some(port.port_type.clone());
                    }
                }
                None
            }
            // Variable references
            HirExpression::Variable(var_id) => {
                if let Some(impl_ctx) = self.current_impl {
                    impl_ctx
                        .variables
                        .iter()
                        .find(|v| v.id == *var_id)
                        .map(|v| v.var_type.clone())
                } else {
                    None
                }
            }
            // For other expressions, we'd need full type inference
            _ => None,
        }
    }

    /// Extract intent name from expression
    fn extract_intent_name(&self, expr: &HirExpression) -> String {
        match expr {
            HirExpression::Constant(const_id) => {
                // Would look up constant by ID
                "DEFAULT_INTENT".to_string()
            }
            HirExpression::Literal(lit) => {
                // Would extract from literal
                "DEFAULT_INTENT".to_string()
            }
            _ => "DEFAULT_INTENT".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mangle_const_value() {
        assert_eq!(mangle_const_value(&ConstValue::Nat(42)), "42");
        assert_eq!(mangle_const_value(&ConstValue::Int(-5)), "n5");
        assert_eq!(mangle_const_value(&ConstValue::Bool(true)), "true");
    }

    #[test]
    fn test_mangle_type() {
        assert_eq!(mangle_type(&HirType::Custom("fp32".to_string())), "fp32");
        assert_eq!(mangle_type(&HirType::Bit(8)), "bit8");
        assert_eq!(mangle_type(&HirType::Clock(None)), "clock");
    }

    #[test]
    fn test_mangled_name() {
        let mut inst = Instantiation {
            entity_name: "FpAdd".to_string(),
            entity_id: EntityId(0),
            type_args: HashMap::new(),
            const_args: HashMap::new(),
            intent_args: HashMap::new(),
        };

        // Just entity name
        assert_eq!(inst.mangled_name(), "FpAdd");

        // With const arg
        inst.const_args.insert("N".to_string(), ConstValue::Nat(8));
        assert_eq!(inst.mangled_name(), "FpAdd_8");

        // With type arg
        inst.type_args
            .insert("T".to_string(), HirType::Custom("fp32".to_string()));
        assert_eq!(inst.mangled_name(), "FpAdd_8_fp32");
    }
}
