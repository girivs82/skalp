//! Generic type system and polymorphic instantiation

use crate::ast::{Generic, GenericKind, Type};
use indexmap::IndexMap;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum GenericError {
    #[error("Unbound type parameter: {0}")]
    UnboundTypeParameter(String),
    #[error("Type mismatch: expected {expected}, got {actual}")]
    TypeMismatch { expected: String, actual: String },
    #[error("Cannot infer type for parameter: {0}")]
    CannotInferType(String),
    #[error("Recursive type definition: {0}")]
    RecursiveType(String),
}

/// Type substitution map
#[derive(Debug, Clone, Default)]
pub struct TypeSubstitution {
    /// Maps type parameters to concrete types
    types: IndexMap<String, Type>,
    /// Maps const parameters to values
    consts: IndexMap<String, i64>,
    /// Maps width parameters to values
    widths: IndexMap<String, usize>,
}

impl TypeSubstitution {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn bind_type(&mut self, param: String, ty: Type) {
        self.types.insert(param, ty);
    }

    pub fn bind_const(&mut self, param: String, value: i64) {
        self.consts.insert(param, value);
    }

    pub fn bind_width(&mut self, param: String, width: usize) {
        self.widths.insert(param, width);
    }

    pub fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Named(name) => {
                // Check if it's a type parameter
                if let Some(substituted) = self.types.get(name) {
                    substituted.clone()
                } else {
                    ty.clone()
                }
            }
            Type::Array(elem, size) => Type::Array(Box::new(self.apply(elem)), *size),
            Type::Generic(base, args) => Type::Generic(
                base.clone(),
                args.iter().map(|arg| self.apply(arg)).collect(),
            ),
            _ => ty.clone(),
        }
    }
}

/// Type inference engine
pub struct TypeInference {
    /// Current substitutions
    substitution: TypeSubstitution,
    /// Type constraints
    constraints: Vec<Constraint>,
}

#[derive(Debug, Clone)]
enum Constraint {
    /// Type equality constraint
    Equal(Type, Type),
    /// Subtype constraint
    Subtype(Type, Type),
    /// Trait bound constraint
    TraitBound(String, String), // type param, trait name
}

impl Default for TypeInference {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeInference {
    pub fn new() -> Self {
        Self {
            substitution: TypeSubstitution::new(),
            constraints: Vec::new(),
        }
    }

    /// Add a type equality constraint
    pub fn constrain_equal(&mut self, t1: Type, t2: Type) {
        self.constraints.push(Constraint::Equal(t1, t2));
    }

    /// Add a trait bound constraint
    pub fn constrain_trait(&mut self, type_param: String, trait_name: String) {
        self.constraints
            .push(Constraint::TraitBound(type_param, trait_name));
    }

    /// Solve all constraints and return substitution
    pub fn solve(mut self) -> Result<TypeSubstitution, GenericError> {
        // Simple unification algorithm
        let mut changed = true;
        while changed {
            changed = false;

            // Clone constraints to avoid borrowing issues
            let constraints = self.constraints.clone();
            for constraint in &constraints {
                match constraint {
                    Constraint::Equal(t1, t2) => {
                        // Apply current substitution to both sides
                        let t1_sub = self.substitution.apply(t1);
                        let t2_sub = self.substitution.apply(t2);

                        if let Some(sub) = self.unify(&t1_sub, &t2_sub)? {
                            self.apply_substitution(sub);
                            changed = true;
                        }
                    }
                    Constraint::Subtype(sub, sup) => {
                        // Handle subtyping (e.g., bit<8> <: bit<16>)
                        if !self.is_subtype(sub, sup) {
                            return Err(GenericError::TypeMismatch {
                                expected: format!("{:?}", sup),
                                actual: format!("{:?}", sub),
                            });
                        }
                    }
                    Constraint::TraitBound(param, trait_name) => {
                        // Record trait bound for later checking
                        // This would integrate with trait resolution
                    }
                }
            }
        }

        Ok(self.substitution)
    }

    fn unify(&self, t1: &Type, t2: &Type) -> Result<Option<(String, Type)>, GenericError> {
        match (t1, t2) {
            (Type::Named(n1), Type::Named(n2)) if n1 == n2 => Ok(None),
            (Type::Named(param), concrete) if self.is_type_param(param) => {
                Ok(Some((param.clone(), concrete.clone())))
            }
            (concrete, Type::Named(param)) if self.is_type_param(param) => {
                Ok(Some((param.clone(), concrete.clone())))
            }
            (Type::Bit(w1), Type::Bit(w2)) if w1 == w2 => Ok(None),
            (Type::Array(e1, s1), Type::Array(e2, s2)) if s1 == s2 => self.unify(e1, e2),
            _ => Err(GenericError::TypeMismatch {
                expected: format!("{:?}", t1),
                actual: format!("{:?}", t2),
            }),
        }
    }

    fn is_type_param(&self, name: &str) -> bool {
        // Check if name is a known type parameter
        name.chars().next().is_some_and(|c| c.is_uppercase())
    }

    fn is_subtype(&self, _sub: &Type, _sup: &Type) -> bool {
        // Simplified subtyping check
        true // TODO: Implement proper subtyping
    }

    fn apply_substitution(&mut self, sub: (String, Type)) {
        self.substitution.bind_type(sub.0, sub.1);
    }
}

/// Polymorphic instantiation
pub struct Instantiator {
    /// Generic parameters
    generics: Vec<Generic>,
    /// Type arguments
    type_args: Vec<Type>,
}

impl Instantiator {
    pub fn new(generics: Vec<Generic>, type_args: Vec<Type>) -> Self {
        Self {
            generics,
            type_args,
        }
    }

    /// Create substitution from generic parameters and arguments
    pub fn create_substitution(&self) -> Result<TypeSubstitution, GenericError> {
        if self.generics.len() != self.type_args.len() {
            return Err(GenericError::TypeMismatch {
                expected: format!("{} type arguments", self.generics.len()),
                actual: format!("{} type arguments", self.type_args.len()),
            });
        }

        let mut substitution = TypeSubstitution::new();

        for (param, arg) in self.generics.iter().zip(&self.type_args) {
            match &param.kind {
                GenericKind::Type => {
                    substitution.bind_type(param.name.clone(), arg.clone());
                }
                GenericKind::Const(_expected_type) => {
                    // Extract const value from argument
                    if let Type::Const(value) = arg {
                        substitution.bind_const(param.name.clone(), *value);
                    } else {
                        return Err(GenericError::TypeMismatch {
                            expected: "const value".to_string(),
                            actual: format!("{:?}", arg),
                        });
                    }
                }
                GenericKind::Width => {
                    // Extract width value from argument
                    if let Type::Bit(Some(width)) = arg {
                        substitution.bind_width(param.name.clone(), *width as usize);
                    } else {
                        return Err(GenericError::TypeMismatch {
                            expected: "width value".to_string(),
                            actual: format!("{:?}", arg),
                        });
                    }
                }
            }
        }

        Ok(substitution)
    }

    /// Instantiate a generic entity with concrete types
    pub fn instantiate<T: Instantiable>(&self, entity: &T) -> Result<T, GenericError> {
        let substitution = self.create_substitution()?;
        Ok(entity.instantiate(&substitution))
    }
}

/// Trait for types that can be instantiated
pub trait Instantiable: Sized {
    fn instantiate(&self, substitution: &TypeSubstitution) -> Self;
}

impl Instantiable for Type {
    fn instantiate(&self, substitution: &TypeSubstitution) -> Self {
        substitution.apply(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_substitution() {
        let mut sub = TypeSubstitution::new();
        sub.bind_type("T".to_string(), Type::Bit(Some(8)));

        let generic = Type::Named("T".to_string());
        let result = sub.apply(&generic);

        assert!(matches!(result, Type::Bit(Some(8))));
    }

    #[test]
    fn test_type_inference() {
        let mut inf = TypeInference::new();
        inf.constrain_equal(Type::Named("T".to_string()), Type::Bit(Some(16)));

        let substitution = inf.solve().unwrap();
        assert!(substitution.types.contains_key("T"));
    }

    #[test]
    fn test_polymorphic_instantiation() {
        let generics = vec![
            Generic {
                name: "T".to_string(),
                kind: GenericKind::Type,
            },
            Generic {
                name: "N".to_string(),
                kind: GenericKind::Width,
            },
        ];

        let type_args = vec![Type::Bit(Some(8)), Type::Bit(Some(32))];

        let inst = Instantiator::new(generics, type_args);
        let substitution = inst.create_substitution().unwrap();

        assert!(substitution.types.contains_key("T"));
        assert!(substitution.widths.contains_key("N"));
    }
}
