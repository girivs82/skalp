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
    /// Current implementation constants (set during specialization)
    current_constants: Vec<crate::hir::HirConstant>,
}

impl<'hir> MonomorphizationEngine<'hir> {
    /// Create a new monomorphization engine
    pub fn new() -> Self {
        Self {
            evaluator: ConstEvaluator::new(),
            hir: None,
            current_constants: Vec::new(),
        }
    }

    /// Monomorphize the entire HIR
    ///
    /// Uses iterative monomorphization to handle nested generics:
    /// - A generic entity may instantiate another generic entity with substituted parameters
    /// - We repeatedly collect instantiations and specialize until reaching a fixed point
    ///
    /// Example:
    /// ```text
    /// entity Decoder<const SIZE: nat> { ... }
    /// entity Wrapper<const SIZE: nat> {
    ///     let decoder = Decoder<SIZE>  // Nested generic instantiation
    /// }
    /// entity Top {
    ///     let wrapper = Wrapper<256>   // Triggers Wrapper<256> AND Decoder<256>
    /// }
    /// ```
    pub fn monomorphize(&mut self, hir: &'hir Hir) -> Hir {
        // Store HIR reference for type resolution
        self.hir = Some(hir);
        use crate::monomorphization::InstantiationCollector;

        // Track all instantiations across all iterations
        let mut all_instantiations = std::collections::HashSet::new();

        // Build initial entity and implementation maps
        let mut entity_map = HashMap::new();
        let mut impl_map = HashMap::new();

        for entity in &hir.entities {
            entity_map.insert(entity.id, entity.clone());
        }

        for impl_block in &hir.implementations {
            impl_map.insert(impl_block.entity, impl_block.clone());
        }

        // Track next available IDs
        let mut next_entity_id = hir.entities.iter().map(|e| e.id.0).max().unwrap_or(0) + 1;
        let mut next_port_id: u32 = hir
            .entities
            .iter()
            .flat_map(|e| e.ports.iter())
            .map(|p| p.id.0)
            .max()
            .unwrap_or(0)
            + 1;

        // Accumulators for all specialized entities and implementations
        let mut all_specialized_entities = Vec::new();
        let mut all_specialized_implementations = Vec::new();
        let mut specialization_map = HashMap::new(); // Instantiation -> specialized entity ID

        // ITERATIVE MONOMORPHIZATION: Repeat until no new instantiations found
        // This handles nested generics where specializing one entity reveals new instantiations
        loop {
            // Build temporary HIR with entities and implementations discovered so far
            let current_entities: Vec<HirEntity> = hir
                .entities
                .iter()
                .cloned()
                .chain(all_specialized_entities.iter().cloned())
                .collect();

            let current_implementations: Vec<HirImplementation> = hir
                .implementations
                .iter()
                .cloned()
                .chain(all_specialized_implementations.iter().cloned())
                .collect();

            let current_hir = Hir {
                name: hir.name.clone(),
                entities: current_entities.clone(),
                implementations: current_implementations.clone(),
                protocols: hir.protocols.clone(),
                intents: hir.intents.clone(),
                requirements: hir.requirements.clone(),
                trait_definitions: hir.trait_definitions.clone(),
                trait_implementations: hir.trait_implementations.clone(),
                type_aliases: hir.type_aliases.clone(),
                distinct_types: hir.distinct_types.clone(),
                user_defined_types: hir.user_defined_types.clone(),
                global_constraints: hir.global_constraints.clone(),
                modules: hir.modules.clone(),
                imports: hir.imports.clone(),
                functions: hir.functions.clone(),
                safety_definitions: hir.safety_definitions.clone(),
            };

            // Collect instantiations from current state
            let collector = InstantiationCollector::new(&current_hir);
            let new_instantiations_set = collector.collect(&current_hir);

            // Debug: show what was found
            eprintln!(
                "[MONOMORPHIZE] Collected {} instantiations from {} entities, {} implementations, {} trait_impls",
                new_instantiations_set.len(),
                current_hir.entities.len(),
                current_hir.implementations.len(),
                current_hir.trait_implementations.len()
            );
            for inst in &new_instantiations_set {
                eprintln!(
                    "[MONOMORPHIZE]   - {} (entity_id={:?}) const_args={:?}",
                    inst.entity_name, inst.entity_id, inst.const_args
                );
            }

            // Find truly new instantiations (not seen before)
            let new_instantiations: Vec<Instantiation> = new_instantiations_set
                .into_iter()
                .filter(|inst| !all_instantiations.contains(inst))
                .collect();

            // If no new instantiations, we've reached a fixed point
            if new_instantiations.is_empty() {
                eprintln!("[MONOMORPHIZE] No new instantiations, breaking loop");
                break;
            }

            // Add new instantiations to the set of all seen instantiations
            for inst in &new_instantiations {
                all_instantiations.insert(inst.clone());
            }

            // Specialize each new instantiation
            for instantiation in &new_instantiations {
                // Get entity from either original or specialized entities
                let entity = entity_map.get(&instantiation.entity_id).or_else(|| {
                    all_specialized_entities
                        .iter()
                        .find(|e| e.id == instantiation.entity_id)
                });

                if let Some(entity) = entity {
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
                    let impl_block = impl_map.get(&instantiation.entity_id).or_else(|| {
                        all_specialized_implementations
                            .iter()
                            .find(|i| i.entity == instantiation.entity_id)
                    });

                    if let Some(impl_block) = impl_block {
                        let specialized_impl = self.specialize_implementation(
                            impl_block,
                            &specialized_entity,
                            instantiation,
                            &port_id_map,
                        );
                        all_specialized_implementations.push(specialized_impl);
                    }

                    all_specialized_entities.push(specialized_entity);
                }
            }
        }

        // If no instantiations found at all, return unchanged
        if all_instantiations.is_empty() {
            return hir.clone();
        }

        // Convert to sorted vec for deterministic ordering
        let mut instantiations: Vec<Instantiation> = all_instantiations.into_iter().collect();
        instantiations.sort_by_key(|a| a.mangled_name());

        // Update instance references in all implementations to point to specialized entities
        let mut new_implementations = hir.implementations.clone();
        new_implementations.extend(all_specialized_implementations.clone());

        // Build combined entity map including specialized entities
        let mut all_entities_map = entity_map.clone();
        for entity in &all_specialized_entities {
            all_entities_map.insert(entity.id, entity.clone());
        }

        for impl_block in &mut new_implementations {
            for instance in &mut impl_block.instances {
                // Check if this instance uses a generic entity
                // Look in both original and specialized entities
                if let Some(entity) = all_entities_map.get(&instance.entity) {
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

        // Build new HIR - keep ALL entities (including generic templates) for HIR→MIR conversion
        // Generic templates will be filtered out during SystemVerilog codegen
        // This ensures entity_map in HIR→MIR has entries for all entity IDs referenced by instances
        let mut new_entities: Vec<HirEntity> = hir.entities.clone();

        // Add specialized entities
        new_entities.extend(all_specialized_entities);

        // Keep all implementations (including those for generic templates)
        // They will be filtered during codegen
        let new_implementations: Vec<HirImplementation> = new_implementations;

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
            distinct_types: hir.distinct_types.clone(),
            user_defined_types: hir.user_defined_types.clone(),
            global_constraints: hir.global_constraints.clone(),
            modules: hir.modules.clone(),
            imports: hir.imports.clone(),
            functions: hir.functions.clone(),
            safety_definitions: hir.safety_definitions.clone(),
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
                    detection_config: port.detection_config.clone(),
                    power_domain_config: port.power_domain_config.clone(),
                    isolation_config: port.isolation_config.clone(),
                    retention_config: port.retention_config.clone(),
                }
            })
            .collect();

        // Create specialized entity with unique ID
        let specialized_entity = HirEntity {
            id: specialized_id,
            name: instantiation.mangled_name(),
            is_async: entity.is_async, // Preserve async status for NCL entities
            visibility: entity.visibility,
            ports: specialized_ports,
            generics: vec![], // No generics in specialized version
            clock_domains: entity.clock_domains.clone(),
            signals: entity.signals.clone(),
            assignments: entity.assignments.clone(),
            span: entity.span.clone(), // Preserve source span from original entity
            pipeline_config: entity.pipeline_config.clone(), // Preserve pipeline config
            vendor_ip_config: entity.vendor_ip_config.clone(), // Preserve vendor IP config
            power_domains: entity.power_domains.clone(), // Preserve power domains
            power_domain_config: entity.power_domain_config.clone(), // Preserve power domain config for CCF
            safety_mechanism_config: entity.safety_mechanism_config.clone(), // Preserve safety config
            seooc_config: entity.seooc_config.clone(), // Preserve SEooC config
            compiled_ip_config: entity.compiled_ip_config.clone(), // Preserve compiled IP config
        };

        (specialized_entity, port_id_map)
    }

    /// Specialize a generic implementation with concrete type/const arguments
    pub fn specialize_implementation(
        &mut self,
        impl_block: &HirImplementation,
        specialized_entity: &HirEntity,
        instantiation: &Instantiation,
        port_id_map: &HashMap<crate::hir::PortId, crate::hir::PortId>,
    ) -> HirImplementation {
        use crate::hir::{HirAssignment, HirConstant, HirSignal, HirVariable};

        // First, evaluate constants with bound generic arguments
        // This is needed for constants like `const E: nat = F.exponent_bits`
        // where F is a generic parameter bound to IEEE754_32
        let mut evaluator = ConstEvaluator::new();
        evaluator.bind_all(instantiation.const_args.clone());

        // Register enums for variant resolution
        if let Some(hir) = self.hir {
            self.register_enums_to_evaluator(&mut evaluator, hir);
        }

        // Evaluate and specialize constants
        // BUG #169 FIX: Evaluate constants with bound generic args and register by ID
        // This handles cases like `const E: nat = F.exponent_bits` where F is bound to FloatFormat
        let specialized_constants: Vec<HirConstant> = impl_block
            .constants
            .iter()
            .map(|constant| {
                // Substitute the value expression with bound generic args
                let subst_value = self.substitute_expr(&constant.value, &instantiation.const_args);
                // Try to evaluate to a concrete value
                match evaluator.eval(&subst_value) {
                    Ok(eval_value) => {
                        // Convert evaluated value to expression for storage
                        let specialized_expr = self.const_value_to_expr(&eval_value);

                        // Register the evaluated value for subsequent evaluations
                        // - By name for GenericParam resolution
                        evaluator.bind(constant.name.clone(), eval_value.clone());
                        // - By ID for Constant(ConstantId) resolution
                        evaluator.register_constant(constant.id, specialized_expr.clone());

                        HirConstant {
                            id: constant.id,
                            name: constant.name.clone(),
                            const_type: constant.const_type.clone(),
                            value: specialized_expr,
                        }
                    }
                    Err(_) => {
                        // Keep original if evaluation fails
                        constant.clone()
                    }
                }
            })
            .collect();

        // Store specialized constants for this implementation
        self.current_constants = specialized_constants;

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
                // BUG #28 FIX: Substitute const params in BOTH LHS and RHS
                new_assignment.lhs =
                    self.substitute_lvalue(&assignment.lhs, &instantiation.const_args);
                new_assignment.rhs =
                    self.substitute_expr(&assignment.rhs, &instantiation.const_args);
                // Remap port IDs in both LHS and RHS
                new_assignment.lhs = self.remap_lvalue_ports(&new_assignment.lhs, port_id_map);
                new_assignment.rhs = self.remap_expr_ports(&new_assignment.rhs, port_id_map);
                new_assignment
            })
            .collect();

        // Specialize event blocks - substitute expressions in statements AND remap trigger port IDs
        let specialized_event_blocks = impl_block
            .event_blocks
            .iter()
            .map(|event_block| {
                let mut new_event_block = event_block.clone();
                // CRITICAL: Substitute statements AND remap port IDs within event block
                new_event_block.statements = event_block
                    .statements
                    .iter()
                    .map(|stmt| {
                        self.substitute_statement_with_ports(
                            stmt,
                            &instantiation.const_args,
                            port_id_map,
                        )
                    })
                    .collect();
                // CRITICAL: Remap trigger port IDs to match specialized entity ports
                new_event_block.triggers = event_block
                    .triggers
                    .iter()
                    .map(|trigger| self.remap_event_trigger_ports(trigger, port_id_map))
                    .collect();
                new_event_block
            })
            .collect();

        // Specialize instances - substitute generic arguments
        let specialized_instances = impl_block
            .instances
            .iter()
            .map(|instance| {
                let mut new_instance = instance.clone();
                // Substitute const parameters in generic arguments
                new_instance.generic_args = instance
                    .generic_args
                    .iter()
                    .map(|arg| self.substitute_expr(arg, &instantiation.const_args))
                    .collect();
                new_instance
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
            instances: specialized_instances,
            covergroups: impl_block.covergroups.clone(),
            formal_blocks: impl_block.formal_blocks.clone(),
            statements: impl_block.statements.clone(), // Copy statements including assertions
        }
    }

    /// Substitute const parameters AND remap port IDs in a statement
    fn substitute_statement_with_ports(
        &self,
        stmt: &crate::hir::HirStatement,
        const_args: &HashMap<String, ConstValue>,
        port_id_map: &HashMap<crate::hir::PortId, crate::hir::PortId>,
    ) -> crate::hir::HirStatement {
        use crate::hir::HirStatement;

        match stmt {
            HirStatement::Assignment(assign) => {
                let mut new_assign = assign.clone();
                // BUG #28 FIX: Substitute const params in BOTH LHS and RHS
                new_assign.lhs = self.substitute_lvalue(&assign.lhs, const_args);
                new_assign.rhs = self.substitute_expr(&assign.rhs, const_args);
                // CRITICAL: Remap port IDs in assignment
                new_assign.lhs = self.remap_lvalue_ports(&new_assign.lhs, port_id_map);
                new_assign.rhs = self.remap_expr_ports(&new_assign.rhs, port_id_map);
                HirStatement::Assignment(new_assign)
            }
            HirStatement::If(if_stmt) => {
                let mut new_if = if_stmt.clone();
                new_if.condition = self.substitute_expr(&if_stmt.condition, const_args);
                // CRITICAL: Remap port IDs in condition!
                new_if.condition = self.remap_expr_ports(&new_if.condition, port_id_map);
                new_if.then_statements = if_stmt
                    .then_statements
                    .iter()
                    .map(|s| self.substitute_statement_with_ports(s, const_args, port_id_map))
                    .collect();
                new_if.else_statements = if_stmt.else_statements.as_ref().map(|stmts| {
                    stmts
                        .iter()
                        .map(|s| self.substitute_statement_with_ports(s, const_args, port_id_map))
                        .collect()
                });
                HirStatement::If(new_if)
            }
            HirStatement::Match(match_stmt) => {
                let mut new_match = match_stmt.clone();
                new_match.expr = self.substitute_expr(&match_stmt.expr, const_args);
                // CRITICAL: Remap port IDs in match expression
                new_match.expr = self.remap_expr_ports(&new_match.expr, port_id_map);
                // Substitute in all match arms
                new_match.arms = match_stmt
                    .arms
                    .iter()
                    .map(|arm| {
                        let mut new_arm = arm.clone();
                        new_arm.statements = arm
                            .statements
                            .iter()
                            .map(|s| {
                                self.substitute_statement_with_ports(s, const_args, port_id_map)
                            })
                            .collect();
                        new_arm
                    })
                    .collect();
                HirStatement::Match(new_match)
            }
            HirStatement::Let(let_stmt) => {
                let mut new_let = let_stmt.clone();
                new_let.value = self.substitute_expr(&let_stmt.value, const_args);
                // CRITICAL: Remap port IDs in let value
                new_let.value = self.remap_expr_ports(&new_let.value, port_id_map);
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

    /// Create a const evaluator with current constants and enums registered
    fn create_evaluator_with_constants(&self) -> ConstEvaluator {
        let mut eval = ConstEvaluator::new();
        // Register enums for enum variant resolution AND module-level constants
        // BUG #170 FIX: Module-level constants like IEEE754_32 need to be registered
        // for const generic argument evaluation to work correctly
        if let Some(hir) = self.hir {
            self.register_enums_to_evaluator(&mut eval, hir);
            // Register constants from ALL impl blocks
            // This includes module-level constants (which may have various entity IDs
            // depending on parse order) and implementation-specific constants
            for impl_block in &hir.implementations {
                eval.register_constants(&impl_block.constants);
            }
        }
        // BUG #171 FIX: Register specialized constants LAST so they take precedence
        // over the original unspecialized constants from HIR impl blocks.
        // This ensures `const E: nat = F.exponent_bits` evaluates to `8` (specialized)
        // rather than `F.exponent_bits` (unspecialized).
        //
        // BUG #172 FIX: When modules are merged, constants from different modules
        // may have colliding IDs. Constants that couldn't be converted to numeric
        // literals (like FloatFormat structs) get value Literal(Integer(0)), which
        // would incorrectly overwrite valid numeric constants with the same ID.
        // Solution: Only register constants that have valid numeric values, skip
        // struct literals that were converted to 0.
        for c in &self.current_constants {
            // Skip constants that are struct literals that failed conversion
            // (they become Literal(Integer(0)) but shouldn't override actual values)
            let is_failed_struct_conversion = matches!(
                &c.value,
                HirExpression::Literal(crate::hir::HirLiteral::Integer(0))
            ) && matches!(
                &c.const_type,
                HirType::Custom(_) // FloatFormat and other custom types
            );

            if !is_failed_struct_conversion {
                eval.register_constant(c.id, c.value.clone());
                eval.register_named_constant(c.name.clone(), c.value.clone());
            }
        }
        eval
    }

    /// Extract and register enum types from HIR to a const evaluator
    fn register_enums_to_evaluator(&self, eval: &mut ConstEvaluator, hir: &Hir) {
        for user_type in &hir.user_defined_types {
            if let HirType::Enum(enum_type) = &user_type.type_def {
                eval.register_enum(enum_type.as_ref().clone());
            }
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
            // BUG #169 FIX: Handle fp<F> where F is a FloatFormat const generic
            HirType::FpParametric { format } => {
                // Evaluate format expression to get concrete bit width
                let mut eval = self.create_evaluator_with_constants();
                eval.bind_all(instantiation.const_args.clone());

                match eval.eval(format) {
                    Ok(value) => {
                        // Handle both direct nat values and FloatFormat values
                        if let Some(n) = value.as_nat() {
                            // Direct bit width specification
                            HirType::Bit(n as u32)
                        } else if let crate::const_eval::ConstValue::FloatFormat(fmt) = value {
                            // FloatFormat struct - extract total_bits
                            HirType::Bit(fmt.total_bits as u32)
                        } else {
                            ty.clone()
                        }
                    }
                    Err(_) => ty.clone(),
                }
            }

            HirType::FixedParametric {
                width,
                frac,
                signed,
            } => {
                let mut eval = self.create_evaluator_with_constants();
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
                let mut eval = self.create_evaluator_with_constants();
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
                let mut eval = self.create_evaluator_with_constants();
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
                let mut eval = self.create_evaluator_with_constants();
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
                let mut eval = self.create_evaluator_with_constants();
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
                let mut eval = self.create_evaluator_with_constants();
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
                let mut eval = self.create_evaluator_with_constants();
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

            // Field access - might be intent field or struct field access
            HirExpression::FieldAccess { base, field } => {
                // First, try to evaluate the ORIGINAL expression with bindings
                // This handles cases like F.total_bits where F is bound to FloatFormat
                let mut eval = self.create_evaluator_with_constants();
                eval.bind_all(const_args.clone());

                // Try evaluating the original expression directly
                if let Ok(result) = eval.eval(expr) {
                    return self.const_value_to_expr(&result);
                }

                // If direct evaluation failed, try substituting the base and evaluating again
                let base_subst = self.substitute_expr(base, const_args);
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

            // Index expression - recursively substitute base and index
            // BUG #30 FIX: Array index expressions need const param substitution
            // Example: mem[rd_ptr % DEPTH] where DEPTH is a const generic
            HirExpression::Index(base, index) => {
                let base_subst = self.substitute_expr(base, const_args);
                let index_subst = self.substitute_expr(index, const_args);
                HirExpression::Index(Box::new(base_subst), Box::new(index_subst))
            }

            // Range expression - recursively substitute base, high, and low
            HirExpression::Range(base, high, low) => {
                let base_subst = self.substitute_expr(base, const_args);
                let high_subst = self.substitute_expr(high, const_args);
                let low_subst = self.substitute_expr(low, const_args);
                HirExpression::Range(
                    Box::new(base_subst),
                    Box::new(high_subst),
                    Box::new(low_subst),
                )
            }

            // Constant reference - look up in current_constants and return the value
            // BUG #173 FIX: Constants like M = ConstantId(2) need to be resolved to their
            // specialized values (e.g., Literal(23)) during expression substitution.
            // Without this, range expressions like a[W-2:M] become a[30:ConstantId(2)]
            // instead of a[30:23].
            HirExpression::Constant(id) => {
                // Look up the constant in current_constants (which have been specialized)
                if let Some(constant) = self.current_constants.iter().find(|c| c.id == *id) {
                    // Return the specialized value (should be a literal for numeric constants)
                    constant.value.clone()
                } else {
                    // Constant not found in current_constants, try to evaluate via evaluator
                    let mut eval = self.create_evaluator_with_constants();
                    eval.bind_all(const_args.clone());
                    if let Ok(result) = eval.eval(expr) {
                        self.const_value_to_expr(&result)
                    } else {
                        expr.clone()
                    }
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
                    let mut eval = self.create_evaluator_with_constants();

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
        instantiations: &'a [Instantiation],
    ) -> Option<&'a Instantiation> {
        // Evaluate instance's generic arguments
        let mut const_args = HashMap::new();
        // Use helper to create evaluator with constants AND enums registered
        // BUG #168 FIX: Previously only enums were registered, causing FloatFormat
        // constants like IEEE754_32 to not be found during evaluation
        let mut evaluator = self.create_evaluator_with_constants();

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

    /// Remap port IDs in an event trigger
    fn remap_event_trigger_ports(
        &self,
        trigger: &crate::hir::HirEventTrigger,
        port_id_map: &HashMap<crate::hir::PortId, crate::hir::PortId>,
    ) -> crate::hir::HirEventTrigger {
        use crate::hir::{HirEventSignal, HirEventTrigger};

        let new_signal = match &trigger.signal {
            HirEventSignal::Port(old_port_id) => {
                if let Some(&new_port_id) = port_id_map.get(old_port_id) {
                    HirEventSignal::Port(new_port_id)
                } else {
                    // Port not in map - keep old ID (shouldn't happen for well-formed code)
                    trigger.signal.clone()
                }
            }
            HirEventSignal::Signal(_) => {
                // Signal IDs don't change during entity specialization
                trigger.signal.clone()
            }
        };

        HirEventTrigger {
            signal: new_signal,
            edge: trigger.edge.clone(),
        }
    }

    /// Substitute const parameters in an LValue (BUG #28 FIX)
    /// This is needed because LValues can contain index expressions with GenericParam references
    /// Example: mem[wr_ptr % DEPTH] where DEPTH is a const generic parameter
    #[allow(clippy::only_used_in_recursion)]
    fn substitute_lvalue(
        &self,
        lvalue: &crate::hir::HirLValue,
        const_args: &HashMap<String, ConstValue>,
    ) -> crate::hir::HirLValue {
        use crate::hir::HirLValue;

        match lvalue {
            HirLValue::Index(base, idx) => {
                let new_base = self.substitute_lvalue(base, const_args);
                let new_idx = self.substitute_expr(idx, const_args); // BUG #28 FIX: substitute const params!
                HirLValue::Index(Box::new(new_base), new_idx)
            }
            HirLValue::Range(base, high, low) => {
                let new_base = self.substitute_lvalue(base, const_args);
                let new_high = self.substitute_expr(high, const_args); // BUG #28 FIX: substitute const params!
                let new_low = self.substitute_expr(low, const_args); // BUG #28 FIX: substitute const params!
                HirLValue::Range(Box::new(new_base), new_high, new_low)
            }
            HirLValue::FieldAccess { base, field } => {
                let new_base = self.substitute_lvalue(base, const_args);
                HirLValue::FieldAccess {
                    base: Box::new(new_base),
                    field: field.clone(),
                }
            }
            _ => lvalue.clone(),
        }
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
                    type_args: call.type_args.clone(), // Preserve type args during port remapping
                    named_type_args: call.named_type_args.clone(), // Preserve named type args
                    args: new_args,
                    impl_style: call.impl_style,
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

impl Default for MonomorphizationEngine<'_> {
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
