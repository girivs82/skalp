//! Monomorphization Engine
//!
//! Generates concrete entity definitions from generic ones

use crate::const_eval::{ConstEvaluator, ConstValue};
use crate::hir::{Hir, HirEntity, HirExpression, HirImplementation, HirType};
use crate::monomorphization::collector::{Instantiation, IntentValue};
use std::collections::HashMap;

/// Monomorphization engine
pub struct MonomorphizationEngine<'hir> {
    /// Const evaluator
    evaluator: ConstEvaluator,
    /// Reference to HIR for type resolution
    hir: Option<&'hir Hir>,
}

impl<'hir> MonomorphizationEngine<'hir> {
    /// Create a new monomorphization engine
    pub fn new() -> Self {
        Self {
            evaluator: ConstEvaluator::new(),
            hir: None,
        }
    }

    /// Monomorphize the entire HIR
    pub fn monomorphize(&mut self, hir: &'hir Hir) -> Hir {
        // Store HIR reference for type resolution
        self.hir = Some(hir);
        use crate::monomorphization::InstantiationCollector;

        // Step 1: Collect all generic instantiations
        let collector = InstantiationCollector::new(hir);
        let instantiations = collector.collect(hir);

        // If no generic instantiations found, return unchanged
        if instantiations.is_empty() {
            return hir.clone();
        }

        // Step 2: Find next available entity ID and port ID
        let mut next_entity_id = hir.entities.iter().map(|e| e.id.0).max().unwrap_or(0) + 1;

        let mut next_port_id: u32 = hir
            .entities
            .iter()
            .flat_map(|e| e.ports.iter())
            .map(|p| p.id.0)
            .max()
            .unwrap_or(0)
            + 1;

        // Step 3: Generate specialized entities and implementations
        let mut specialized_entities = Vec::new();
        let mut specialized_implementations = Vec::new();
        let mut entity_map = HashMap::new(); // Map from entity ID to entity
        let mut impl_map = HashMap::new(); // Map from entity ID to implementation
        let mut specialization_map = HashMap::new(); // Map from instantiation to specialized entity ID

        for entity in &hir.entities {
            entity_map.insert(entity.id, entity);
        }

        for impl_block in &hir.implementations {
            impl_map.insert(impl_block.entity, impl_block);
        }

        for instantiation in &instantiations {
            if let Some(entity) = entity_map.get(&instantiation.entity_id) {
                // Generate unique entity ID for specialized version
                let specialized_id = crate::hir::EntityId(next_entity_id);
                next_entity_id += 1;

                let (specialized_entity, port_id_map) = self.specialize_entity(
                    entity,
                    instantiation,
                    specialized_id,
                    &mut next_port_id,
                );

                // Record the mapping from instantiation to specialized entity
                specialization_map.insert(instantiation.clone(), specialized_id);

                // If there's an implementation for this entity, specialize it too
                if let Some(impl_block) = impl_map.get(&instantiation.entity_id) {
                    let specialized_impl = self.specialize_implementation(
                        impl_block,
                        &specialized_entity,
                        instantiation,
                        &port_id_map,
                    );
                    specialized_implementations.push(specialized_impl);
                }

                specialized_entities.push(specialized_entity);
            }
        }

        // Step 4: Update instance references in all implementations to point to specialized entities
        let mut new_implementations = hir.implementations.clone();
        new_implementations.extend(specialized_implementations);

        for impl_block in &mut new_implementations {
            for instance in &mut impl_block.instances {
                // Check if this instance uses a generic entity
                if let Some(entity) = entity_map.get(&instance.entity) {
                    if !entity.generics.is_empty() && !instance.generic_args.is_empty() {
                        // This is a generic instantiation - find the specialized entity
                        if let Some(instantiation) =
                            self.find_matching_instantiation(entity, instance, &instantiations)
                        {
                            if let Some(&specialized_id) = specialization_map.get(instantiation) {
                                // Update instance to reference specialized entity
                                instance.entity = specialized_id;
                                // Clear generic args since specialized entity doesn't have generics
                                instance.generic_args.clear();
                            }
                        }
                    }
                }
            }
        }

        // Step 5: Build new HIR, filtering out fully-specialized generic entities
        // Collect IDs of entities that were specialized
        let specialized_entity_ids: std::collections::HashSet<_> =
            instantiations.iter().map(|inst| inst.entity_id).collect();

        // Keep only non-generic entities and generic entities that weren't specialized
        let mut new_entities: Vec<HirEntity> = hir
            .entities
            .iter()
            .filter(|entity| {
                // Keep if entity has no generics (it's concrete)
                entity.generics.is_empty()
                    // OR keep if it's generic but wasn't specialized
                    || !specialized_entity_ids.contains(&entity.id)
            })
            .cloned()
            .collect();

        // Add specialized entities
        new_entities.extend(specialized_entities);

        // Filter implementations to remove those for removed entities
        let entity_ids_in_output: std::collections::HashSet<_> =
            new_entities.iter().map(|e| e.id).collect();
        let new_implementations: Vec<HirImplementation> = new_implementations
            .into_iter()
            .filter(|impl_block| entity_ids_in_output.contains(&impl_block.entity))
            .collect();

        Hir {
            name: hir.name.clone(),
            entities: new_entities,
            implementations: new_implementations,
            protocols: hir.protocols.clone(),
            intents: hir.intents.clone(),
            requirements: hir.requirements.clone(),
            trait_definitions: hir.trait_definitions.clone(),
            trait_implementations: hir.trait_implementations.clone(),
            type_aliases: hir.type_aliases.clone(),
            user_defined_types: hir.user_defined_types.clone(),
            global_constraints: hir.global_constraints.clone(),
            modules: hir.modules.clone(),
            imports: hir.imports.clone(),
            functions: hir.functions.clone(),
        }
    }

    /// Specialize a generic entity with concrete type/const arguments
    /// Returns the specialized entity and a mapping from old port IDs to new port IDs
    pub fn specialize_entity(
        &mut self,
        entity: &HirEntity,
        instantiation: &Instantiation,
        specialized_id: crate::hir::EntityId,
        next_port_id: &mut u32,
    ) -> (HirEntity, HashMap<crate::hir::PortId, crate::hir::PortId>) {
        // Create new evaluator with const bindings
        let mut evaluator = ConstEvaluator::new();
        evaluator.bind_all(instantiation.const_args.clone());

        // Create mapping from old port IDs to new port IDs
        let mut port_id_map = HashMap::new();

        // Substitute types in ports and assign new IDs
        let specialized_ports = entity
            .ports
            .iter()
            .map(|port| {
                let old_id = port.id;
                let new_id = crate::hir::PortId(*next_port_id);
                *next_port_id += 1;

                port_id_map.insert(old_id, new_id);

                crate::hir::HirPort {
                    id: new_id,
                    name: port.name.clone(),
                    direction: port.direction.clone(),
                    port_type: self.substitute_type(&port.port_type, instantiation),
                    physical_constraints: port.physical_constraints.clone(),
                }
            })
            .collect();

        // Create specialized entity with unique ID
        let specialized_entity = HirEntity {
            id: specialized_id,
            name: instantiation.mangled_name(),
            visibility: entity.visibility,
            ports: specialized_ports,
            generics: vec![], // No generics in specialized version
            clock_domains: entity.clock_domains.clone(),
        };

        (specialized_entity, port_id_map)
    }

    /// Specialize a generic implementation with concrete type/const arguments
    pub fn specialize_implementation(
        &self,
        impl_block: &HirImplementation,
        specialized_entity: &HirEntity,
        instantiation: &Instantiation,
        port_id_map: &HashMap<crate::hir::PortId, crate::hir::PortId>,
    ) -> HirImplementation {
        use crate::hir::{HirAssignment, HirSignal, HirVariable};

        // Specialize signals - substitute types
        let specialized_signals = impl_block
            .signals
            .iter()
            .map(|signal| {
                let mut new_signal = signal.clone();
                new_signal.signal_type = self.substitute_type(&signal.signal_type, instantiation);
                // Substitute initial value expression if present
                if let Some(ref init_expr) = signal.initial_value {
                    new_signal.initial_value =
                        Some(self.substitute_expr(init_expr, &instantiation.const_args));
                }
                new_signal
            })
            .collect();

        // Specialize variables - substitute types
        let specialized_variables = impl_block
            .variables
            .iter()
            .map(|variable| {
                let mut new_variable = variable.clone();
                new_variable.var_type = self.substitute_type(&variable.var_type, instantiation);
                // Substitute initial value expression if present
                if let Some(ref init_expr) = variable.initial_value {
                    new_variable.initial_value =
                        Some(self.substitute_expr(init_expr, &instantiation.const_args));
                }
                new_variable
            })
            .collect();

        // Specialize assignments - substitute expressions and remap port IDs
        let specialized_assignments = impl_block
            .assignments
            .iter()
            .map(|assignment| {
                let mut new_assignment = assignment.clone();
                new_assignment.rhs =
                    self.substitute_expr(&assignment.rhs, &instantiation.const_args);
                // Remap port IDs in both LHS and RHS
                new_assignment.lhs = self.remap_lvalue_ports(&assignment.lhs, port_id_map);
                new_assignment.rhs = self.remap_expr_ports(&new_assignment.rhs, port_id_map);
                new_assignment
            })
            .collect();

        // Specialize event blocks - substitute expressions in statements
        let specialized_event_blocks = impl_block
            .event_blocks
            .iter()
            .map(|event_block| {
                let mut new_event_block = event_block.clone();
                // Substitute statements within event block
                new_event_block.statements = event_block
                    .statements
                    .iter()
                    .map(|stmt| self.substitute_statement(stmt, &instantiation.const_args))
                    .collect();
                new_event_block
            })
            .collect();

        // Create specialized implementation
        HirImplementation {
            entity: specialized_entity.id,
            signals: specialized_signals,
            variables: specialized_variables,
            constants: impl_block.constants.clone(), // Constants are already evaluated
            functions: impl_block.functions.clone(), // TODO: specialize functions if needed
            event_blocks: specialized_event_blocks,
            assignments: specialized_assignments,
            instances: impl_block.instances.clone(), // TODO: update instance references
            covergroups: impl_block.covergroups.clone(),
            formal_blocks: impl_block.formal_blocks.clone(),
        }
    }

    /// Substitute const parameters in a statement
    fn substitute_statement(
        &self,
        stmt: &crate::hir::HirStatement,
        const_args: &HashMap<String, ConstValue>,
    ) -> crate::hir::HirStatement {
        use crate::hir::HirStatement;

        match stmt {
            HirStatement::Assignment(assign) => {
                let mut new_assign = assign.clone();
                new_assign.rhs = self.substitute_expr(&assign.rhs, const_args);
                HirStatement::Assignment(new_assign)
            }
            HirStatement::If(if_stmt) => {
                let mut new_if = if_stmt.clone();
                new_if.condition = self.substitute_expr(&if_stmt.condition, const_args);
                new_if.then_statements = if_stmt
                    .then_statements
                    .iter()
                    .map(|s| self.substitute_statement(s, const_args))
                    .collect();
                new_if.else_statements = if_stmt.else_statements.as_ref().map(|stmts| {
                    stmts
                        .iter()
                        .map(|s| self.substitute_statement(s, const_args))
                        .collect()
                });
                HirStatement::If(new_if)
            }
            HirStatement::Match(match_stmt) => {
                let mut new_match = match_stmt.clone();
                new_match.expr = self.substitute_expr(&match_stmt.expr, const_args);
                // Substitute in all match arms
                new_match.arms = match_stmt
                    .arms
                    .iter()
                    .map(|arm| {
                        let mut new_arm = arm.clone();
                        new_arm.statements = arm
                            .statements
                            .iter()
                            .map(|s| self.substitute_statement(s, const_args))
                            .collect();
                        new_arm
                    })
                    .collect();
                HirStatement::Match(new_match)
            }
            HirStatement::Let(let_stmt) => {
                let mut new_let = let_stmt.clone();
                new_let.value = self.substitute_expr(&let_stmt.value, const_args);
                new_let.var_type = self.substitute_type(
                    &let_stmt.var_type,
                    &Instantiation {
                        entity_name: String::new(),
                        entity_id: crate::hir::EntityId(0),
                        type_args: HashMap::new(),
                        const_args: const_args.clone(),
                        intent_args: HashMap::new(),
                    },
                );
                HirStatement::Let(new_let)
            }
            // Other statement types pass through (for now)
            _ => stmt.clone(),
        }
    }

    /// Resolve a custom type to its actual definition
    fn resolve_custom_type(&self, ty: &HirType) -> HirType {
        match ty {
            HirType::Custom(name) => {
                // Look up the type in HIR's user-defined types
                if let Some(hir) = self.hir {
                    for user_type in &hir.user_defined_types {
                        if user_type.name == *name {
                            // Found it - return the type definition
                            // Don't recursively resolve to avoid infinite loops
                            return user_type.type_def.clone();
                        }
                    }

                    // Also check type aliases
                    for type_alias in &hir.type_aliases {
                        if type_alias.name == *name {
                            // Recursively resolve type aliases
                            return self.resolve_custom_type(&type_alias.target_type);
                        }
                    }
                }
                // Type not found - return as-is
                ty.clone()
            }
            // For non-custom types, return as-is
            _ => ty.clone(),
        }
    }

    /// Substitute type parameters in a type
    #[allow(clippy::only_used_in_recursion)]
    fn substitute_type(&self, ty: &HirType, instantiation: &Instantiation) -> HirType {
        match ty {
            // Custom type might be a type parameter
            HirType::Custom(name) => {
                if let Some(concrete_ty) = instantiation.type_args.get(name) {
                    // Recursively substitute and resolve the concrete type
                    let substituted = self.substitute_type(concrete_ty, instantiation);
                    // Resolve custom types to their definitions
                    self.resolve_custom_type(&substituted)
                } else {
                    // Not a type parameter - try to resolve as a user-defined type
                    self.resolve_custom_type(ty)
                }
            }

            // Array: recursively substitute element type
            HirType::Array(elem, size) => {
                let elem_substituted = self.substitute_type(elem, instantiation);
                // Also resolve any custom types in the element
                let elem_resolved = self.resolve_custom_type(&elem_substituted);
                HirType::Array(Box::new(elem_resolved), *size)
            }

            // Parametric types: evaluate to concrete types
            HirType::FpParametric { format } => {
                // Evaluate format expression to get concrete bit width
                let mut eval = ConstEvaluator::new();
                eval.bind_all(instantiation.const_args.clone());

                if let Ok(value) = eval.eval(format) {
                    if let Some(n) = value.as_nat() {
                        // For now, just use bit width
                        HirType::Bit(n as u32)
                    } else {
                        ty.clone()
                    }
                } else {
                    ty.clone()
                }
            }

            HirType::FixedParametric {
                width,
                frac,
                signed,
            } => {
                let mut eval = ConstEvaluator::new();
                eval.bind_all(instantiation.const_args.clone());

                if let (Ok(w), Ok(_f), Ok(_s)) =
                    (eval.eval(width), eval.eval(frac), eval.eval(signed))
                {
                    if let Some(width_val) = w.as_nat() {
                        HirType::Bit(width_val as u32)
                    } else {
                        ty.clone()
                    }
                } else {
                    ty.clone()
                }
            }

            HirType::IntParametric { width, signed } => {
                let mut eval = ConstEvaluator::new();
                eval.bind_all(instantiation.const_args.clone());

                if let (Ok(w), Ok(_s)) = (eval.eval(width), eval.eval(signed)) {
                    if let Some(width_val) = w.as_nat() {
                        HirType::Bit(width_val as u32)
                    } else {
                        ty.clone()
                    }
                } else {
                    ty.clone()
                }
            }

            HirType::VecParametric {
                element_type,
                dimension,
            } => {
                let elem_substituted = self.substitute_type(element_type, instantiation);
                let mut eval = ConstEvaluator::new();
                eval.bind_all(instantiation.const_args.clone());

                if let Ok(dim_val) = eval.eval(dimension) {
                    if let Some(n) = dim_val.as_nat() {
                        HirType::Array(Box::new(elem_substituted), n as u32)
                    } else {
                        ty.clone()
                    }
                } else {
                    ty.clone()
                }
            }

            // BitParam, LogicParam, etc. - parameter names that need substitution
            HirType::BitParam(param_name)
            | HirType::LogicParam(param_name)
            | HirType::IntParam(param_name)
            | HirType::NatParam(param_name) => {
                if let Some(value) = instantiation.const_args.get(param_name) {
                    if let Some(width) = value.as_nat() {
                        match ty {
                            HirType::BitParam(_) => HirType::Bit(width as u32),
                            HirType::LogicParam(_) => HirType::Logic(width as u32),
                            HirType::IntParam(_) => HirType::Int(width as u32),
                            HirType::NatParam(_) => HirType::Nat(width as u32),
                            _ => unreachable!(),
                        }
                    } else {
                        ty.clone()
                    }
                } else {
                    ty.clone()
                }
            }

            // BitExpr, LogicExpr, etc. - expressions that need evaluation
            HirType::BitExpr(expr)
            | HirType::LogicExpr(expr)
            | HirType::IntExpr(expr)
            | HirType::NatExpr(expr) => {
                let mut eval = ConstEvaluator::new();
                eval.bind_all(instantiation.const_args.clone());

                // Substitute expr first
                let subst_expr = self.substitute_expr(expr, &instantiation.const_args);

                if let Ok(value) = eval.eval(&subst_expr) {
                    if let Some(width) = value.as_nat() {
                        match ty {
                            HirType::BitExpr(_) => HirType::Bit(width as u32),
                            HirType::LogicExpr(_) => HirType::Logic(width as u32),
                            HirType::IntExpr(_) => HirType::Int(width as u32),
                            HirType::NatExpr(_) => HirType::Nat(width as u32),
                            _ => unreachable!(),
                        }
                    } else {
                        ty.clone()
                    }
                } else {
                    ty.clone()
                }
            }

            // ArrayExpr - array with expression-based size
            HirType::ArrayExpr(elem_type, size_expr) => {
                let elem_substituted = self.substitute_type(elem_type, instantiation);
                let mut eval = ConstEvaluator::new();
                eval.bind_all(instantiation.const_args.clone());

                let subst_expr = self.substitute_expr(size_expr, &instantiation.const_args);

                if let Ok(value) = eval.eval(&subst_expr) {
                    if let Some(size) = value.as_nat() {
                        HirType::Array(Box::new(elem_substituted), size as u32)
                    } else {
                        ty.clone()
                    }
                } else {
                    ty.clone()
                }
            }

            // Vec types - recursively substitute element type
            HirType::Vec2(elem) | HirType::Vec3(elem) | HirType::Vec4(elem) => {
                let elem_substituted = self.substitute_type(elem, instantiation);
                // Also resolve any custom types in the element
                let elem_resolved = self.resolve_custom_type(&elem_substituted);
                match ty {
                    HirType::Vec2(_) => HirType::Vec2(Box::new(elem_resolved)),
                    HirType::Vec3(_) => HirType::Vec3(Box::new(elem_resolved)),
                    HirType::Vec4(_) => HirType::Vec4(Box::new(elem_resolved)),
                    _ => unreachable!(),
                }
            }

            // Other types pass through unchanged
            _ => ty.clone(),
        }
    }

    /// Substitute const parameters in an expression
    fn substitute_expr(
        &self,
        expr: &HirExpression,
        const_args: &HashMap<String, ConstValue>,
    ) -> HirExpression {
        match expr {
            // Generic parameter reference - substitute with value
            HirExpression::GenericParam(name) => {
                if let Some(value) = const_args.get(name) {
                    // Convert ConstValue to HirExpression
                    self.const_value_to_expr(value)
                } else {
                    expr.clone()
                }
            }

            // Binary expression - recursively substitute
            HirExpression::Binary(bin) => {
                let left = self.substitute_expr(&bin.left, const_args);
                let right = self.substitute_expr(&bin.right, const_args);

                // Try to evaluate at compile time
                let mut eval = ConstEvaluator::new();
                eval.bind_all(const_args.clone());

                let new_bin = crate::hir::HirBinaryExpr {
                    op: bin.op.clone(),
                    left: Box::new(left.clone()),
                    right: Box::new(right.clone()),
                };

                // If we can evaluate, replace with literal
                if let Ok(result) = eval.eval(&HirExpression::Binary(new_bin.clone())) {
                    self.const_value_to_expr(&result)
                } else {
                    HirExpression::Binary(new_bin)
                }
            }

            // If expression - may be intent-based conditional
            HirExpression::If(if_expr) => {
                let cond = self.substitute_expr(&if_expr.condition, const_args);
                let then_expr = self.substitute_expr(&if_expr.then_expr, const_args);
                let else_expr = self.substitute_expr(&if_expr.else_expr, const_args);

                // Try to evaluate condition
                let mut eval = ConstEvaluator::new();
                eval.bind_all(const_args.clone());

                if let Ok(cond_val) = eval.eval(&cond) {
                    if let Some(b) = cond_val.as_bool() {
                        // Condition is known at compile time - select branch
                        return if b { then_expr } else { else_expr };
                    }
                }

                // Condition not compile-time constant, keep if expression
                HirExpression::If(crate::hir::HirIfExpr {
                    condition: Box::new(cond),
                    then_expr: Box::new(then_expr),
                    else_expr: Box::new(else_expr),
                })
            }

            // Field access - might be intent field
            HirExpression::FieldAccess { base, field } => {
                let base_subst = self.substitute_expr(base, const_args);

                // Try to evaluate field access
                let mut eval = ConstEvaluator::new();
                eval.bind_all(const_args.clone());

                let field_expr = HirExpression::FieldAccess {
                    base: Box::new(base_subst.clone()),
                    field: field.clone(),
                };

                if let Ok(result) = eval.eval(&field_expr) {
                    self.const_value_to_expr(&result)
                } else {
                    field_expr
                }
            }

            // Other expressions pass through
            _ => expr.clone(),
        }
    }

    /// Convert ConstValue to HirExpression
    fn const_value_to_expr(&self, value: &ConstValue) -> HirExpression {
        match value {
            ConstValue::Nat(n) => {
                HirExpression::Literal(crate::hir::HirLiteral::Integer(*n as u64))
            }
            ConstValue::Int(i) => {
                HirExpression::Literal(crate::hir::HirLiteral::Integer(*i as u64))
            }
            ConstValue::Bool(b) => HirExpression::Literal(crate::hir::HirLiteral::Boolean(*b)),
            ConstValue::String(s) => {
                HirExpression::Literal(crate::hir::HirLiteral::String(s.clone()))
            }
            ConstValue::Float(f) => HirExpression::Literal(crate::hir::HirLiteral::Float(*f)),
            _ => {
                // Complex values can't be directly converted
                HirExpression::Literal(crate::hir::HirLiteral::Integer(0))
            }
        }
    }

    /// Evaluate intent-based conditional and select implementation
    pub fn evaluate_intent_conditional(
        &self,
        expr: &HirExpression,
        intent: &IntentValue,
    ) -> HirExpression {
        match expr {
            HirExpression::If(if_expr) => {
                // Check if condition references intent
                if self.is_intent_condition(&if_expr.condition, intent) {
                    // Evaluate condition with intent values
                    let mut eval = ConstEvaluator::new();

                    // Bind intent fields
                    for (field_name, field_value) in &intent.fields {
                        eval.bind(field_name.clone(), field_value.clone());
                    }

                    if let Ok(cond_result) = eval.eval(&if_expr.condition) {
                        if let Some(b) = cond_result.as_bool() {
                            // Select branch based on condition
                            return if b {
                                self.evaluate_intent_conditional(&if_expr.then_expr, intent)
                            } else {
                                self.evaluate_intent_conditional(&if_expr.else_expr, intent)
                            };
                        }
                    }
                }

                // Not intent-based or can't evaluate - recurse
                let cond = self.evaluate_intent_conditional(&if_expr.condition, intent);
                let then_expr = self.evaluate_intent_conditional(&if_expr.then_expr, intent);
                let else_expr = self.evaluate_intent_conditional(&if_expr.else_expr, intent);

                HirExpression::If(crate::hir::HirIfExpr {
                    condition: Box::new(cond),
                    then_expr: Box::new(then_expr),
                    else_expr: Box::new(else_expr),
                })
            }
            _ => expr.clone(),
        }
    }

    /// Check if expression is an intent-based condition
    fn is_intent_condition(&self, expr: &HirExpression, _intent: &IntentValue) -> bool {
        match expr {
            HirExpression::Binary(bin) => {
                // Check if either side references intent
                self.references_intent(&bin.left) || self.references_intent(&bin.right)
            }
            HirExpression::FieldAccess { base, .. } => self.references_intent(base),
            _ => false,
        }
    }

    /// Check if expression references intent parameter
    #[allow(clippy::only_used_in_recursion)]
    fn references_intent(&self, expr: &HirExpression) -> bool {
        match expr {
            HirExpression::GenericParam(name) => {
                // Check if this is an intent parameter (conventionally named I)
                name == "I" || name.ends_with("_INTENT")
            }
            HirExpression::FieldAccess { base, .. } => self.references_intent(base),
            HirExpression::Binary(bin) => {
                self.references_intent(&bin.left) || self.references_intent(&bin.right)
            }
            _ => false,
        }
    }

    /// Find the instantiation that matches a given instance
    fn find_matching_instantiation<'a>(
        &self,
        entity: &HirEntity,
        instance: &crate::hir::HirInstance,
        instantiations: &'a std::collections::HashSet<Instantiation>,
    ) -> Option<&'a Instantiation> {
        // Evaluate instance's generic arguments
        let mut const_args = HashMap::new();
        let mut evaluator = ConstEvaluator::new();

        for (i, arg) in instance.generic_args.iter().enumerate() {
            if i >= entity.generics.len() {
                break;
            }

            let generic = &entity.generics[i];
            if let crate::hir::HirGenericType::Const(_) = generic.param_type {
                if let Ok(value) = evaluator.eval(arg) {
                    const_args.insert(generic.name.clone(), value);
                }
            }
        }

        // Find matching instantiation
        instantiations
            .iter()
            .find(|inst| inst.entity_id == entity.id && inst.const_args == const_args)
    }

    /// Remap port IDs in an LValue
    #[allow(clippy::only_used_in_recursion)]
    fn remap_lvalue_ports(
        &self,
        lvalue: &crate::hir::HirLValue,
        port_id_map: &HashMap<crate::hir::PortId, crate::hir::PortId>,
    ) -> crate::hir::HirLValue {
        use crate::hir::HirLValue;

        match lvalue {
            HirLValue::Port(old_id) => {
                if let Some(&new_id) = port_id_map.get(old_id) {
                    HirLValue::Port(new_id)
                } else {
                    lvalue.clone()
                }
            }
            HirLValue::Index(base, idx) => {
                let new_base = self.remap_lvalue_ports(base, port_id_map);
                let new_idx = self.remap_expr_ports(idx, port_id_map);
                HirLValue::Index(Box::new(new_base), new_idx)
            }
            HirLValue::Range(base, high, low) => {
                let new_base = self.remap_lvalue_ports(base, port_id_map);
                let new_high = self.remap_expr_ports(high, port_id_map);
                let new_low = self.remap_expr_ports(low, port_id_map);
                HirLValue::Range(Box::new(new_base), new_high, new_low)
            }
            HirLValue::FieldAccess { base, field } => {
                let new_base = self.remap_lvalue_ports(base, port_id_map);
                HirLValue::FieldAccess {
                    base: Box::new(new_base),
                    field: field.clone(),
                }
            }
            _ => lvalue.clone(),
        }
    }

    /// Remap port IDs in an expression
    #[allow(clippy::only_used_in_recursion)]
    fn remap_expr_ports(
        &self,
        expr: &HirExpression,
        port_id_map: &HashMap<crate::hir::PortId, crate::hir::PortId>,
    ) -> HirExpression {
        match expr {
            HirExpression::Port(old_id) => {
                if let Some(&new_id) = port_id_map.get(old_id) {
                    HirExpression::Port(new_id)
                } else {
                    expr.clone()
                }
            }
            HirExpression::Binary(bin) => {
                let left = self.remap_expr_ports(&bin.left, port_id_map);
                let right = self.remap_expr_ports(&bin.right, port_id_map);
                HirExpression::Binary(crate::hir::HirBinaryExpr {
                    op: bin.op.clone(),
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }
            HirExpression::Unary(unary) => {
                let operand = self.remap_expr_ports(&unary.operand, port_id_map);
                HirExpression::Unary(crate::hir::HirUnaryExpr {
                    op: unary.op.clone(),
                    operand: Box::new(operand),
                })
            }
            HirExpression::Index(base, index) => {
                let new_base = self.remap_expr_ports(base, port_id_map);
                let new_index = self.remap_expr_ports(index, port_id_map);
                HirExpression::Index(Box::new(new_base), Box::new(new_index))
            }
            HirExpression::Range(base, high, low) => {
                let new_base = self.remap_expr_ports(base, port_id_map);
                let new_high = self.remap_expr_ports(high, port_id_map);
                let new_low = self.remap_expr_ports(low, port_id_map);
                HirExpression::Range(Box::new(new_base), Box::new(new_high), Box::new(new_low))
            }
            HirExpression::FieldAccess { base, field } => {
                let new_base = self.remap_expr_ports(base, port_id_map);
                HirExpression::FieldAccess {
                    base: Box::new(new_base),
                    field: field.clone(),
                }
            }
            HirExpression::Call(call) => {
                let new_args = call
                    .args
                    .iter()
                    .map(|arg| self.remap_expr_ports(arg, port_id_map))
                    .collect();
                HirExpression::Call(crate::hir::HirCallExpr {
                    function: call.function.clone(),
                    args: new_args,
                })
            }
            HirExpression::If(if_expr) => {
                let new_cond = self.remap_expr_ports(&if_expr.condition, port_id_map);
                let new_then = self.remap_expr_ports(&if_expr.then_expr, port_id_map);
                let new_else = self.remap_expr_ports(&if_expr.else_expr, port_id_map);
                HirExpression::If(crate::hir::HirIfExpr {
                    condition: Box::new(new_cond),
                    then_expr: Box::new(new_then),
                    else_expr: Box::new(new_else),
                })
            }
            HirExpression::Concat(exprs) => {
                let new_exprs = exprs
                    .iter()
                    .map(|e| self.remap_expr_ports(e, port_id_map))
                    .collect();
                HirExpression::Concat(new_exprs)
            }
            _ => expr.clone(),
        }
    }
}

impl<'hir> Default for MonomorphizationEngine<'hir> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hir::{EntityId, HirPort, HirPortDirection};

    #[test]
    fn test_substitute_type() {
        let engine = MonomorphizationEngine::new();

        let mut inst = Instantiation {
            entity_name: "Test".to_string(),
            entity_id: EntityId(0),
            type_args: HashMap::new(),
            const_args: HashMap::new(),
            intent_args: HashMap::new(),
        };

        // Type parameter substitution
        inst.type_args
            .insert("T".to_string(), HirType::Custom("fp32".to_string()));

        let original_type = HirType::Custom("T".to_string());
        let substituted = engine.substitute_type(&original_type, &inst);

        assert!(matches!(substituted, HirType::Custom(name) if name == "fp32"));
    }

    #[test]
    fn test_const_value_to_expr() {
        let engine = MonomorphizationEngine::new();

        let expr = engine.const_value_to_expr(&ConstValue::Nat(42));
        assert!(matches!(
            expr,
            HirExpression::Literal(crate::hir::HirLiteral::Integer(42))
        ));

        let expr = engine.const_value_to_expr(&ConstValue::Bool(true));
        assert!(matches!(
            expr,
            HirExpression::Literal(crate::hir::HirLiteral::Boolean(true))
        ));
    }
}
