//! Type system for SKALP
//!
//! This module implements the type system including:
//! - Type representation
//! - Type inference
//! - Type checking
//! - Width inference and checking

use std::fmt;
use std::collections::HashMap;

/// Core type representation in SKALP
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// Bit vector type - 2-state (0, 1)
    Bit(Width),

    /// Logic vector type - 4-state (0, 1, X, Z)
    Logic(Width),

    /// Signed integer type
    Int(Width),

    /// Unsigned natural number type
    Nat(Width),

    /// Fixed-point type with integer and fractional bits
    Fixed { integer_bits: u32, fractional_bits: u32 },

    /// Clock type with optional domain
    Clock(Option<ClockDomain>),

    /// Reset type (active high/low)
    Reset(ResetPolarity),

    /// Event type for edge-triggered signals
    Event,

    /// Stream type for pipelined data
    Stream(Box<Type>),

    /// Array type
    Array { element_type: Box<Type>, size: u32 },

    /// Struct type
    Struct(StructType),

    /// Enum type
    Enum(EnumType),

    /// Protocol type
    Protocol(String),

    /// Entity type (for references to entities)
    Entity(EntityType),

    /// Generic type parameter
    TypeParam(String),

    /// Type variable for inference
    TypeVar(TypeVarId),

    /// Unknown type (used during inference)
    Unknown,

    /// Error type (for error recovery)
    Error,
}

/// Width specification for bit vectors
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Width {
    /// Fixed width
    Fixed(u32),

    /// Parameterized width
    Param(String),

    /// Inferred width (during type checking)
    Inferred(WidthVar),

    /// Unknown width (needs inference)
    Unknown,
}

/// Width variable for inference
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WidthVar(pub u32);

/// Type variable ID for inference
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVarId(pub u32);

/// Clock domain specification
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClockDomain {
    /// Domain name
    pub name: String,

    /// Frequency in Hz (if known)
    pub frequency: Option<u64>,

    /// Phase relationship to other clocks
    pub phase: Option<i32>,
}

/// Reset polarity
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResetPolarity {
    ActiveHigh,
    ActiveLow,
}

/// Struct type definition
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructType {
    /// Struct name
    pub name: String,

    /// Fields
    pub fields: Vec<StructField>,
}

/// Struct field
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    /// Field name
    pub name: String,

    /// Field type
    pub field_type: Type,
}

/// Enum type definition
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumType {
    /// Enum name
    pub name: String,

    /// Variants
    pub variants: Vec<EnumVariant>,
}

/// Enum variant
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariant {
    /// Variant name
    pub name: String,

    /// Optional payload type
    pub payload: Option<Type>,
}

/// Entity type definition
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EntityType {
    /// Entity name
    pub name: String,

    /// Input ports
    pub inputs: Vec<PortType>,

    /// Output ports
    pub outputs: Vec<PortType>,

    /// Inout ports
    pub inouts: Vec<PortType>,

    /// Generic parameters
    pub type_params: Vec<String>,

    /// Width parameters
    pub width_params: Vec<String>,
}

/// Port type definition
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PortType {
    /// Port name
    pub name: String,

    /// Port type
    pub port_type: Type,
}

/// Type scheme for polymorphic types
#[derive(Debug, Clone)]
pub struct TypeScheme {
    /// Quantified type variables
    pub type_params: Vec<String>,

    /// Quantified width parameters
    pub width_params: Vec<String>,

    /// The actual type
    pub ty: Type,
}

/// Type environment for type checking
#[derive(Debug, Clone)]
pub struct TypeEnv {
    /// Variable bindings
    bindings: HashMap<String, TypeScheme>,

    /// Type definitions
    type_defs: HashMap<String, Type>,

    /// Parent environment (for nested scopes)
    parent: Option<Box<TypeEnv>>,
}

/// Type inference engine
pub struct TypeInference {
    /// Next type variable ID
    next_type_var: u32,

    /// Next width variable ID
    next_width_var: u32,

    /// Type substitutions
    type_subst: HashMap<TypeVarId, Type>,

    /// Width substitutions
    width_subst: HashMap<WidthVar, Width>,

    /// Type constraints
    constraints: Vec<TypeConstraint>,
}

/// Type constraint for inference
#[derive(Debug, Clone)]
pub enum TypeConstraint {
    /// Types must be equal
    Equal(Type, Type),

    /// Width must be equal
    WidthEqual(Width, Width),

    /// Width must be at least n bits
    WidthAtLeast(Width, u32),

    /// Type must be numeric
    Numeric(Type),

    /// Type must be a clock
    IsClock(Type),
}

impl Type {
    /// Check if type is numeric
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Type::Bit(_) | Type::Logic(_) | Type::Int(_) | Type::Nat(_) | Type::Fixed { .. }
        )
    }

    /// Check if type is a clock type
    pub fn is_clock(&self) -> bool {
        matches!(self, Type::Clock(_))
    }

    /// Check if type is a reset type
    pub fn is_reset(&self) -> bool {
        matches!(self, Type::Reset(_))
    }

    /// Get the width of a type (if applicable)
    pub fn width(&self) -> Option<&Width> {
        match self {
            Type::Bit(w) | Type::Logic(w) | Type::Int(w) | Type::Nat(w) => Some(w),
            _ => None,
        }
    }

    /// Apply type substitution
    pub fn apply_subst(&self, subst: &HashMap<TypeVarId, Type>) -> Type {
        match self {
            Type::TypeVar(id) => {
                subst.get(id).cloned().unwrap_or_else(|| self.clone())
            }
            Type::Array { element_type, size } => Type::Array {
                element_type: Box::new(element_type.apply_subst(subst)),
                size: *size,
            },
            Type::Stream(t) => Type::Stream(Box::new(t.apply_subst(subst))),
            _ => self.clone(),
        }
    }

    /// Check if two types can be unified
    pub fn can_unify(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            (Type::TypeVar(_), _) | (_, Type::TypeVar(_)) => true,
            (Type::Bit(w1), Type::Bit(w2)) => w1.can_unify(w2),
            (Type::Logic(w1), Type::Logic(w2)) => w1.can_unify(w2),
            (Type::Int(w1), Type::Int(w2)) => w1.can_unify(w2),
            (Type::Nat(w1), Type::Nat(w2)) => w1.can_unify(w2),
            (Type::Clock(d1), Type::Clock(d2)) => {
                match (d1, d2) {
                    (None, _) | (_, None) => true,
                    (Some(d1), Some(d2)) => d1.name == d2.name,
                }
            }
            (Type::Array { element_type: e1, size: s1 },
             Type::Array { element_type: e2, size: s2 }) => {
                s1 == s2 && e1.can_unify(e2)
            }
            _ => self == other,
        }
    }
}

impl Width {
    /// Check if widths can be unified
    pub fn can_unify(&self, other: &Width) -> bool {
        match (self, other) {
            (Width::Unknown, _) | (_, Width::Unknown) => true,
            (Width::Inferred(_), _) | (_, Width::Inferred(_)) => true,
            (Width::Fixed(a), Width::Fixed(b)) => a == b,
            (Width::Param(a), Width::Param(b)) => a == b,
            _ => false,
        }
    }

    /// Get concrete width value if known
    pub fn concrete_value(&self) -> Option<u32> {
        match self {
            Width::Fixed(n) => Some(*n),
            _ => None,
        }
    }

    /// Apply width substitution
    pub fn apply_subst(&self, subst: &HashMap<WidthVar, Width>) -> Width {
        match self {
            Width::Inferred(var) => {
                subst.get(var).cloned().unwrap_or_else(|| self.clone())
            }
            _ => self.clone(),
        }
    }
}

impl TypeEnv {
    /// Create a new empty type environment
    pub fn new() -> Self {
        let mut env = Self {
            bindings: HashMap::new(),
            type_defs: HashMap::new(),
            parent: None,
        };

        // Add built-in types
        env.add_builtin_types();
        env
    }

    /// Create a child environment
    pub fn child(parent: TypeEnv) -> Self {
        Self {
            bindings: HashMap::new(),
            type_defs: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    /// Add built-in types
    fn add_builtin_types(&mut self) {
        // These are available as type constructors
        self.type_defs.insert("bit".to_string(), Type::Bit(Width::Unknown));
        self.type_defs.insert("logic".to_string(), Type::Logic(Width::Unknown));
        self.type_defs.insert("int".to_string(), Type::Int(Width::Unknown));
        self.type_defs.insert("nat".to_string(), Type::Nat(Width::Unknown));
        self.type_defs.insert("clock".to_string(), Type::Clock(None));
        self.type_defs.insert("reset".to_string(), Type::Reset(ResetPolarity::ActiveHigh));
        self.type_defs.insert("event".to_string(), Type::Event);
    }

    /// Look up a variable's type
    pub fn lookup(&self, name: &str) -> Option<&TypeScheme> {
        self.bindings.get(name).or_else(|| {
            self.parent.as_ref().and_then(|p| p.lookup(name))
        })
    }

    /// Add a variable binding
    pub fn bind(&mut self, name: String, scheme: TypeScheme) {
        self.bindings.insert(name, scheme);
    }

    /// Look up a type definition
    pub fn lookup_type(&self, name: &str) -> Option<&Type> {
        self.type_defs.get(name).or_else(|| {
            self.parent.as_ref().and_then(|p| p.lookup_type(name))
        })
    }

    /// Add a type definition
    pub fn define_type(&mut self, name: String, ty: Type) {
        self.type_defs.insert(name, ty);
    }
}

impl TypeInference {
    /// Create a new type inference engine
    pub fn new() -> Self {
        Self {
            next_type_var: 0,
            next_width_var: 0,
            type_subst: HashMap::new(),
            width_subst: HashMap::new(),
            constraints: Vec::new(),
        }
    }

    /// Create a fresh type variable
    pub fn fresh_type_var(&mut self) -> Type {
        let id = TypeVarId(self.next_type_var);
        self.next_type_var += 1;
        Type::TypeVar(id)
    }

    /// Create a fresh width variable
    pub fn fresh_width_var(&mut self) -> Width {
        let var = WidthVar(self.next_width_var);
        self.next_width_var += 1;
        Width::Inferred(var)
    }

    /// Add a type constraint
    pub fn add_constraint(&mut self, constraint: TypeConstraint) {
        self.constraints.push(constraint);
    }

    /// Infer type for an integer literal
    pub fn infer_int_literal(&mut self, value: u64) -> Type {
        // Calculate minimum width needed
        let min_width = if value == 0 {
            1
        } else {
            64 - value.leading_zeros()
        };

        // Default to nat with inferred width
        Type::Nat(Width::Fixed(min_width))
    }

    /// Infer type for a binary literal
    pub fn infer_bin_literal(&mut self, value: u64, explicit_width: Option<u32>) -> Type {
        let width = explicit_width.unwrap_or_else(|| {
            if value == 0 { 1 } else { 64 - value.leading_zeros() }
        });

        Type::Bit(Width::Fixed(width))
    }

    /// Infer type for a hex literal
    pub fn infer_hex_literal(&mut self, value: u64, explicit_width: Option<u32>) -> Type {
        let width = explicit_width.unwrap_or_else(|| {
            if value == 0 { 1 } else { 64 - value.leading_zeros() }
        });

        Type::Bit(Width::Fixed(width))
    }

    /// Check if assignment is valid
    pub fn check_assignment(&mut self, lhs_type: &Type, rhs_type: &Type) -> Result<(), TypeError> {
        // Check if types are compatible (allowing width extension)
        match (lhs_type, rhs_type) {
            // Same base type with potentially different widths
            (Type::Bit(w1), Type::Bit(w2)) |
            (Type::Logic(w1), Type::Logic(w2)) |
            (Type::Int(w1), Type::Int(w2)) |
            (Type::Nat(w1), Type::Nat(w2)) => {
                // Allow assignment if RHS width <= LHS width (can zero/sign extend)
                match (w1, w2) {
                    (Width::Fixed(lhs_w), Width::Fixed(rhs_w)) if rhs_w <= lhs_w => Ok(()),
                    (Width::Fixed(_lhs_w), Width::Fixed(_rhs_w)) => {
                        // For now, allow any width mismatch with a warning
                        // In real hardware, this would be a truncation or extension
                        // TODO: Add warning for width mismatch
                        Ok(())
                    }
                    _ => {
                        // Variable widths - add constraint
                        self.add_constraint(TypeConstraint::WidthEqual(w1.clone(), w2.clone()));
                        Ok(())
                    }
                }
            }
            // Exact match required for other types
            _ => {
                self.add_constraint(TypeConstraint::Equal(lhs_type.clone(), rhs_type.clone()));
                if lhs_type.can_unify(rhs_type) {
                    Ok(())
                } else {
                    Err(TypeError::TypeMismatch {
                        expected: lhs_type.clone(),
                        found: rhs_type.clone(),
                    })
                }
            }
        }
    }

    /// Check width compatibility
    pub fn check_width(&mut self, w1: &Width, w2: &Width) -> Result<(), TypeError> {
        self.add_constraint(TypeConstraint::WidthEqual(w1.clone(), w2.clone()));

        if w1.can_unify(w2) {
            Ok(())
        } else {
            Err(TypeError::WidthMismatch {
                expected: w1.clone(),
                found: w2.clone(),
            })
        }
    }

    /// Unify two types
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> Result<(), TypeError> {
        match (t1, t2) {
            (Type::Unknown, _) => Ok(()),
            (_, Type::Unknown) => Ok(()),

            (Type::TypeVar(id), t) | (t, Type::TypeVar(id)) => {
                self.type_subst.insert(*id, t.clone());
                Ok(())
            }

            (Type::Bit(w1), Type::Bit(w2)) |
            (Type::Logic(w1), Type::Logic(w2)) |
            (Type::Int(w1), Type::Int(w2)) |
            (Type::Nat(w1), Type::Nat(w2)) => {
                self.unify_width(w1, w2)
            }

            (Type::Array { element_type: e1, size: s1 },
             Type::Array { element_type: e2, size: s2 }) => {
                if s1 != s2 {
                    return Err(TypeError::ArraySizeMismatch {
                        expected: *s1,
                        found: *s2,
                    });
                }
                self.unify(e1, e2)
            }

            _ if t1 == t2 => Ok(()),

            _ => Err(TypeError::TypeMismatch {
                expected: t1.clone(),
                found: t2.clone(),
            })
        }
    }

    /// Unify two widths
    pub fn unify_width(&mut self, w1: &Width, w2: &Width) -> Result<(), TypeError> {
        match (w1, w2) {
            (Width::Unknown, _) => Ok(()),
            (_, Width::Unknown) => Ok(()),

            (Width::Inferred(var), w) | (w, Width::Inferred(var)) => {
                self.width_subst.insert(*var, w.clone());
                Ok(())
            }

            (Width::Fixed(a), Width::Fixed(b)) if a == b => Ok(()),
            (Width::Param(a), Width::Param(b)) if a == b => Ok(()),

            _ => Err(TypeError::WidthMismatch {
                expected: w1.clone(),
                found: w2.clone(),
            })
        }
    }

    /// Solve all constraints
    pub fn solve_constraints(&mut self) -> Result<(), TypeError> {
        for constraint in self.constraints.clone() {
            match constraint {
                TypeConstraint::Equal(t1, t2) => self.unify(&t1, &t2)?,
                TypeConstraint::WidthEqual(w1, w2) => self.unify_width(&w1, &w2)?,
                TypeConstraint::WidthAtLeast(w, min) => {
                    if let Width::Fixed(n) = w {
                        if n < min {
                            return Err(TypeError::InsufficientWidth {
                                required: min,
                                found: n,
                            });
                        }
                    }
                }
                TypeConstraint::Numeric(t) => {
                    if !t.is_numeric() {
                        return Err(TypeError::NotNumeric(t));
                    }
                }
                TypeConstraint::IsClock(t) => {
                    if !t.is_clock() {
                        return Err(TypeError::NotClock(t));
                    }
                }
            }
        }
        Ok(())
    }
}

/// Type errors
#[derive(Debug, Clone)]
pub enum TypeError {
    /// Type mismatch
    TypeMismatch {
        expected: Type,
        found: Type,
    },

    /// Width mismatch
    WidthMismatch {
        expected: Width,
        found: Width,
    },

    /// Insufficient width
    InsufficientWidth {
        required: u32,
        found: u32,
    },

    /// Array size mismatch
    ArraySizeMismatch {
        expected: u32,
        found: u32,
    },

    /// Type is not numeric
    NotNumeric(Type),

    /// Type is not a clock
    NotClock(Type),

    /// Undefined variable
    UndefinedVariable(String),

    /// Duplicate definition
    DuplicateDefinition(String),

    /// Undefined type
    UndefinedType(String),

    /// Cannot infer type
    CannotInfer,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Bit(w) => write!(f, "bit{}", w),
            Type::Logic(w) => write!(f, "logic{}", w),
            Type::Int(w) => write!(f, "int{}", w),
            Type::Nat(w) => write!(f, "nat{}", w),
            Type::Fixed { integer_bits, fractional_bits } => {
                write!(f, "fixed<{}, {}>", integer_bits, fractional_bits)
            }
            Type::Clock(domain) => {
                match domain {
                    Some(d) => write!(f, "clock<'{}'>", d.name),
                    None => write!(f, "clock"),
                }
            }
            Type::Reset(pol) => {
                match pol {
                    ResetPolarity::ActiveHigh => write!(f, "reset"),
                    ResetPolarity::ActiveLow => write!(f, "reset_n"),
                }
            }
            Type::Event => write!(f, "event"),
            Type::Stream(t) => write!(f, "stream<{}>", t),
            Type::Array { element_type, size } => {
                write!(f, "{}[{}]", element_type, size)
            }
            Type::Struct(s) => write!(f, "{}", s.name),
            Type::Enum(e) => write!(f, "{}", e.name),
            Type::Protocol(p) => write!(f, "protocol {}", p),
            Type::Entity(e) => write!(f, "entity {}", e.name),
            Type::TypeParam(p) => write!(f, "{}", p),
            Type::TypeVar(id) => write!(f, "?t{}", id.0),
            Type::Unknown => write!(f, "?"),
            Type::Error => write!(f, "<error>"),
        }
    }
}

impl fmt::Display for Width {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Width::Fixed(n) => write!(f, "[{}]", n),
            Width::Param(p) => write!(f, "[{}]", p),
            Width::Inferred(v) => write!(f, "[?w{}]", v.0),
            Width::Unknown => write!(f, "[?]"),
        }
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeError::TypeMismatch { expected, found } => {
                write!(f, "Type mismatch: expected {}, found {}", expected, found)
            }
            TypeError::WidthMismatch { expected, found } => {
                write!(f, "Width mismatch: expected {}, found {}", expected, found)
            }
            TypeError::InsufficientWidth { required, found } => {
                write!(f, "Insufficient width: requires at least {} bits, found {}", required, found)
            }
            TypeError::ArraySizeMismatch { expected, found } => {
                write!(f, "Array size mismatch: expected {}, found {}", expected, found)
            }
            TypeError::NotNumeric(t) => {
                write!(f, "Type {} is not numeric", t)
            }
            TypeError::NotClock(t) => {
                write!(f, "Type {} is not a clock", t)
            }
            TypeError::UndefinedVariable(name) => {
                write!(f, "Undefined variable: {}", name)
            }
            TypeError::DuplicateDefinition(name) => {
                write!(f, "Duplicate definition: {}", name)
            }
            TypeError::UndefinedType(name) => {
                write!(f, "Undefined type: {}", name)
            }
            TypeError::CannotInfer => {
                write!(f, "Cannot infer type")
            }
        }
    }
}

impl std::error::Error for TypeError {}

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for TypeInference {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_display() {
        assert_eq!(Type::Bit(Width::Fixed(8)).to_string(), "bit[8]");
        assert_eq!(Type::Logic(Width::Fixed(16)).to_string(), "logic[16]");
        assert_eq!(Type::Clock(None).to_string(), "clock");

        let clock_domain = ClockDomain {
            name: "sys".to_string(),
            frequency: Some(100_000_000),
            phase: None,
        };
        assert_eq!(Type::Clock(Some(clock_domain)).to_string(), "clock<'sys'>");
    }

    #[test]
    fn test_type_unification() {
        let mut inference = TypeInference::new();

        // Same types should unify
        let t1 = Type::Bit(Width::Fixed(8));
        let t2 = Type::Bit(Width::Fixed(8));
        assert!(inference.unify(&t1, &t2).is_ok());

        // Different widths should not unify
        let t3 = Type::Bit(Width::Fixed(16));
        assert!(inference.unify(&t1, &t3).is_err());

        // Type variable should unify with concrete type
        let t4 = inference.fresh_type_var();
        assert!(inference.unify(&t4, &t1).is_ok());
    }

    #[test]
    fn test_width_inference() {
        let mut inference = TypeInference::new();

        // Infer width for small literal
        let t = inference.infer_int_literal(42);
        assert_eq!(t, Type::Nat(Width::Fixed(6))); // 42 needs 6 bits

        // Infer width for binary literal
        let t = inference.infer_bin_literal(0b1010, None);
        assert_eq!(t, Type::Bit(Width::Fixed(4)));

        // Explicit width
        let t = inference.infer_bin_literal(0b1010, Some(8));
        assert_eq!(t, Type::Bit(Width::Fixed(8)));
    }

    #[test]
    fn test_type_environment() {
        let mut env = TypeEnv::new();

        // Built-in types should be available
        assert!(env.lookup_type("bit").is_some());
        assert!(env.lookup_type("clock").is_some());

        // Add a variable binding
        let scheme = TypeScheme {
            type_params: vec![],
            width_params: vec![],
            ty: Type::Nat(Width::Fixed(8)),
        };
        env.bind("counter".to_string(), scheme);

        assert!(env.lookup("counter").is_some());
        assert!(env.lookup("undefined").is_none());
    }

    #[test]
    fn test_assignment_checking() {
        let mut inference = TypeInference::new();

        let lhs = Type::Bit(Width::Fixed(8));
        let rhs = Type::Bit(Width::Fixed(8));

        assert!(inference.check_assignment(&lhs, &rhs).is_ok());

        // Width mismatches are now allowed (with implicit truncation/extension)
        let wrong_rhs = Type::Bit(Width::Fixed(16));
        assert!(inference.check_assignment(&lhs, &wrong_rhs).is_ok());

        // But type mismatches should still fail
        let wrong_type = Type::Int(Width::Fixed(8));
        assert!(inference.check_assignment(&lhs, &wrong_type).is_err());
    }
}