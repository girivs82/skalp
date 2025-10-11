//! Monomorphization Engine
//!
//! Generates concrete entity definitions from generic ones

use crate::const_eval::{ConstEvaluator, ConstValue};
use crate::hir::{Hir, HirEntity, HirExpression, HirImplementation, HirType};
use crate::monomorphization::collector::{Instantiation, IntentValue};
use std::collections::HashMap;

/// Monomorphization engine
pub struct MonomorphizationEngine {
    /// Const evaluator
    evaluator: ConstEvaluator,
}

impl MonomorphizationEngine {
    /// Create a new monomorphization engine
    pub fn new() -> Self {
        Self {
            evaluator: ConstEvaluator::new(),
        }
    }

    /// Monomorphize the entire HIR
    pub fn monomorphize(&mut self, hir: &Hir) -> Hir {
        use crate::monomorphization::InstantiationCollector;

        // Step 1: Collect all generic instantiations
        let collector = InstantiationCollector::new(hir);
        let instantiations = collector.collect(hir);

        // If no generic instantiations found, return unchanged
        if instantiations.is_empty() {
            return hir.clone();
        }

        // Step 2: Generate specialized entities
        let mut specialized_entities = Vec::new();
        let mut entity_map = HashMap::new(); // Map from entity ID to entity

        for entity in &hir.entities {
            entity_map.insert(entity.id, entity);
        }

        for instantiation in &instantiations {
            if let Some(entity) = entity_map.get(&instantiation.entity_id) {
                let specialized = self.specialize_entity(entity, instantiation);
                specialized_entities.push(specialized);
            }
        }

        // Step 3: Build new HIR with both original and specialized entities
        // For now, keep all entities (generic and specialized)
        // TODO: Remove generic entities that have been fully specialized
        let mut new_entities = hir.entities.clone();
        new_entities.extend(specialized_entities);

        Hir {
            name: hir.name.clone(),
            entities: new_entities,
            implementations: hir.implementations.clone(),
            protocols: hir.protocols.clone(),
            intents: hir.intents.clone(),
            requirements: hir.requirements.clone(),
            trait_definitions: hir.trait_definitions.clone(),
            trait_implementations: hir.trait_implementations.clone(),
            global_constraints: hir.global_constraints.clone(),
            modules: hir.modules.clone(),
            imports: hir.imports.clone(),
            functions: hir.functions.clone(),
        }
    }

    /// Specialize a generic entity with concrete type/const arguments
    pub fn specialize_entity(
        &mut self,
        entity: &HirEntity,
        instantiation: &Instantiation,
    ) -> HirEntity {
        // Create new evaluator with const bindings
        let mut evaluator = ConstEvaluator::new();
        evaluator.bind_all(instantiation.const_args.clone());

        // Substitute types in ports
        let specialized_ports = entity
            .ports
            .iter()
            .map(|port| {
                let mut new_port = port.clone();
                new_port.port_type = self.substitute_type(&port.port_type, instantiation);
                new_port
            })
            .collect();

        // Create specialized entity
        HirEntity {
            id: entity.id, // Will be assigned new ID in final pass
            name: instantiation.mangled_name(),
            visibility: entity.visibility,
            ports: specialized_ports,
            generics: vec![], // No generics in specialized version
            clock_domains: entity.clock_domains.clone(),
        }
    }

    /// Substitute type parameters in a type
    #[allow(clippy::only_used_in_recursion)]
    fn substitute_type(&self, ty: &HirType, instantiation: &Instantiation) -> HirType {
        match ty {
            // Custom type might be a type parameter
            HirType::Custom(name) => {
                if let Some(concrete_ty) = instantiation.type_args.get(name) {
                    concrete_ty.clone()
                } else {
                    ty.clone()
                }
            }

            // Array: recursively substitute element type
            HirType::Array(elem, size) => {
                let elem_substituted = self.substitute_type(elem, instantiation);
                HirType::Array(Box::new(elem_substituted), *size)
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
}

impl Default for MonomorphizationEngine {
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
