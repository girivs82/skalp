//! Instantiation Collector
//!
//! Walks the HIR to find all generic entity instantiations

use crate::const_eval::{ConstEvaluator, ConstValue};
use crate::hir::{
    EntityId, Hir, HirEntity, HirExpression, HirGeneric, HirGenericType, HirImplementation,
    HirInstance, HirType,
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
        _ => "unknown".to_string(),
    }
}

/// Collects all generic instantiations from HIR
pub struct InstantiationCollector {
    /// All discovered instantiations
    instantiations: HashSet<Instantiation>,
    /// Const evaluator for parameter values
    evaluator: ConstEvaluator,
    /// Map from entity ID to entity
    entities: HashMap<EntityId, HirEntity>,
    /// Entities we've already processed (to avoid infinite recursion)
    visited: HashSet<EntityId>,
}

impl InstantiationCollector {
    /// Create a new instantiation collector
    pub fn new(hir: &Hir) -> Self {
        let mut entities = HashMap::new();
        for entity in &hir.entities {
            entities.insert(entity.id, entity.clone());
        }

        Self {
            instantiations: HashSet::new(),
            evaluator: ConstEvaluator::new(),
            entities,
            visited: HashSet::new(),
        }
    }

    /// Collect all instantiations from the HIR
    pub fn collect(mut self, hir: &Hir) -> HashSet<Instantiation> {
        // Start from all implementations
        for implementation in &hir.implementations {
            self.collect_from_implementation(implementation);
        }

        self.instantiations
    }

    /// Collect instantiations from an implementation
    fn collect_from_implementation(&mut self, implementation: &HirImplementation) {
        // Collect from all module instances
        for instance in &implementation.instances {
            self.collect_from_instance(instance);
        }
    }

    /// Collect instantiation from a module instance
    fn collect_from_instance(&mut self, instance: &HirInstance) {
        // Get the entity being instantiated
        let entity_id = instance.entity;

        // Avoid infinite recursion
        if self.visited.contains(&entity_id) {
            return;
        }

        let entity = match self.entities.get(&entity_id) {
            Some(e) => e,
            None => return, // Entity not found
        };

        // If entity has no generics, nothing to collect
        if entity.generics.is_empty() {
            return;
        }

        // Build instantiation record
        let type_args = HashMap::new();
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
                    // Type parameter - would extract type from expression
                    // For now, skip
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

        // For generic parameters without provided arguments, use defaults
        for (i, generic) in entity.generics.iter().enumerate() {
            if i >= instance.generic_args.len() {
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

        // Create instantiation record
        let instantiation = Instantiation {
            entity_name: entity.name.clone(),
            entity_id,
            type_args,
            const_args,
            intent_args,
        };

        self.instantiations.insert(instantiation);

        // Mark as visited
        self.visited.insert(entity_id);

        // Recursively collect from this entity's implementation
        // (Find the implementation for this entity and process it)
        // This would require finding the impl block and processing its instances
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
