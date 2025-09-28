//! Property specification language for formal verification

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Temporal logic property
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Property {
    /// Property name
    pub name: String,
    /// Property type
    pub property_type: PropertyType,
    /// Temporal logic formula
    pub formula: TemporalFormula,
    /// Optional description
    pub description: Option<String>,
    /// Severity level
    pub severity: Severity,
    /// Verification parameters
    pub params: VerificationParams,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PropertyType {
    /// Safety property (nothing bad happens)
    Safety,
    /// Liveness property (something good eventually happens)
    Liveness,
    /// Invariant property (always true)
    Invariant,
    /// Assertion property (immediate check)
    Assertion,
    /// Coverage property (reachability)
    Coverage,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Severity {
    Info,
    Warning,
    Error,
    Critical,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VerificationParams {
    /// Bounded model checking depth
    pub bmc_depth: Option<u32>,
    /// Induction depth for k-induction
    pub induction_depth: Option<u32>,
    /// Enable abstraction
    pub use_abstraction: bool,
    /// Timeout in seconds
    pub timeout: Option<u64>,
}

impl Default for VerificationParams {
    fn default() -> Self {
        Self {
            bmc_depth: Some(20),
            induction_depth: Some(10),
            use_abstraction: false,
            timeout: Some(60),
        }
    }
}

/// Temporal logic formula in CTL/LTL
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TemporalFormula {
    /// Atomic proposition
    Atomic(String),
    /// Boolean literal
    Bool(bool),
    /// Logical negation
    Not(Box<TemporalFormula>),
    /// Logical conjunction
    And(Box<TemporalFormula>, Box<TemporalFormula>),
    /// Logical disjunction
    Or(Box<TemporalFormula>, Box<TemporalFormula>),
    /// Logical implication
    Implies(Box<TemporalFormula>, Box<TemporalFormula>),
    /// Equivalence
    Iff(Box<TemporalFormula>, Box<TemporalFormula>),

    // LTL operators
    /// Next (X φ)
    Next(Box<TemporalFormula>),
    /// Eventually (F φ)
    Eventually(Box<TemporalFormula>),
    /// Always (G φ)
    Always(Box<TemporalFormula>),
    /// Until (φ U ψ)
    Until(Box<TemporalFormula>, Box<TemporalFormula>),
    /// Release (φ R ψ)
    Release(Box<TemporalFormula>, Box<TemporalFormula>),
    /// Weak until (φ W ψ)
    WeakUntil(Box<TemporalFormula>, Box<TemporalFormula>),

    // CTL operators
    /// Exists next (EX φ)
    ExistsNext(Box<TemporalFormula>),
    /// All next (AX φ)
    AllNext(Box<TemporalFormula>),
    /// Exists eventually (EF φ)
    ExistsEventually(Box<TemporalFormula>),
    /// All eventually (AF φ)
    AllEventually(Box<TemporalFormula>),
    /// Exists always (EG φ)
    ExistsAlways(Box<TemporalFormula>),
    /// All always (AG φ)
    AllAlways(Box<TemporalFormula>),
    /// Exists until (E[φ U ψ])
    ExistsUntil(Box<TemporalFormula>, Box<TemporalFormula>),
    /// All until (A[φ U ψ])
    AllUntil(Box<TemporalFormula>, Box<TemporalFormula>),

    // Bounded operators
    /// Bounded eventually (F[a,b] φ)
    BoundedEventually(Box<TemporalFormula>, u32, u32),
    /// Bounded always (G[a,b] φ)
    BoundedAlways(Box<TemporalFormula>, u32, u32),
    /// Bounded until (φ U[a,b] ψ)
    BoundedUntil(Box<TemporalFormula>, Box<TemporalFormula>, u32, u32),
}

impl Property {
    /// Create a safety property
    pub fn safety(name: String, condition: String) -> Self {
        Self {
            name,
            property_type: PropertyType::Safety,
            formula: TemporalFormula::Always(Box::new(TemporalFormula::Atomic(condition))),
            description: None,
            severity: Severity::Error,
            params: VerificationParams::default(),
        }
    }

    /// Create a liveness property
    pub fn liveness(name: String, condition: String) -> Self {
        Self {
            name,
            property_type: PropertyType::Liveness,
            formula: TemporalFormula::Eventually(Box::new(TemporalFormula::Atomic(condition))),
            description: None,
            severity: Severity::Warning,
            params: VerificationParams::default(),
        }
    }

    /// Create an invariant property
    pub fn invariant(name: String, condition: String) -> Self {
        Self {
            name,
            property_type: PropertyType::Invariant,
            formula: TemporalFormula::Always(Box::new(TemporalFormula::Atomic(condition))),
            description: None,
            severity: Severity::Critical,
            params: VerificationParams::default(),
        }
    }

    /// Create an assertion property
    pub fn assertion(name: String, condition: String) -> Self {
        Self {
            name,
            property_type: PropertyType::Assertion,
            formula: TemporalFormula::Atomic(condition),
            description: None,
            severity: Severity::Error,
            params: VerificationParams::default(),
        }
    }

    /// Create a coverage property
    pub fn coverage(name: String, target: String) -> Self {
        Self {
            name,
            property_type: PropertyType::Coverage,
            formula: TemporalFormula::Eventually(Box::new(TemporalFormula::Atomic(target))),
            description: None,
            severity: Severity::Info,
            params: VerificationParams::default(),
        }
    }

    /// Create property from string specification
    pub fn from_string(name: String, spec: &str) -> Result<Self, String> {
        let formula = parse_temporal_formula(spec)?;

        // Infer property type from formula
        let property_type = match &formula {
            TemporalFormula::Always(_) => PropertyType::Safety,
            TemporalFormula::Eventually(_) => PropertyType::Liveness,
            TemporalFormula::AllAlways(_) => PropertyType::Invariant,
            _ => PropertyType::Assertion,
        };

        Ok(Self {
            name,
            property_type,
            formula,
            description: None,
            severity: Severity::Error,
            params: VerificationParams::default(),
        })
    }

    /// Set description
    pub fn with_description(mut self, desc: String) -> Self {
        self.description = Some(desc);
        self
    }

    /// Set severity
    pub fn with_severity(mut self, severity: Severity) -> Self {
        self.severity = severity;
        self
    }

    /// Set verification parameters
    pub fn with_params(mut self, params: VerificationParams) -> Self {
        self.params = params;
        self
    }

    /// Convert to SMT-LIB format
    pub fn to_smt(&self) -> String {
        format!(
            "; Property: {}\n(assert {})",
            self.name,
            formula_to_smt(&self.formula)
        )
    }

    /// Convert to NuSMV format
    pub fn to_nusmv(&self) -> String {
        format!(
            "-- Property: {}\nSPEC {}",
            self.name,
            formula_to_nusmv(&self.formula)
        )
    }
}

/// Parse temporal logic formula from string
fn parse_temporal_formula(spec: &str) -> Result<TemporalFormula, String> {
    // Simplified parser - would need full grammar in production
    let spec = spec.trim();

    if spec == "true" {
        return Ok(TemporalFormula::Bool(true));
    }
    if spec == "false" {
        return Ok(TemporalFormula::Bool(false));
    }

    // Handle temporal operators
    if spec.starts_with("G ") || spec.starts_with("[] ") {
        let inner = if spec.starts_with("G ") {
            &spec[2..]
        } else {
            &spec[3..]
        };
        return Ok(TemporalFormula::Always(Box::new(parse_temporal_formula(inner)?)));
    }

    if spec.starts_with("F ") || spec.starts_with("<> ") {
        let inner = if spec.starts_with("F ") {
            &spec[2..]
        } else {
            &spec[3..]
        };
        return Ok(TemporalFormula::Eventually(Box::new(parse_temporal_formula(inner)?)));
    }

    if spec.starts_with("X ") {
        let inner = &spec[2..];
        return Ok(TemporalFormula::Next(Box::new(parse_temporal_formula(inner)?)));
    }

    // Handle boolean operators
    if let Some(pos) = spec.find(" && ") {
        let left = parse_temporal_formula(&spec[..pos])?;
        let right = parse_temporal_formula(&spec[pos + 4..])?;
        return Ok(TemporalFormula::And(Box::new(left), Box::new(right)));
    }

    if let Some(pos) = spec.find(" || ") {
        let left = parse_temporal_formula(&spec[..pos])?;
        let right = parse_temporal_formula(&spec[pos + 4..])?;
        return Ok(TemporalFormula::Or(Box::new(left), Box::new(right)));
    }

    if let Some(pos) = spec.find(" -> ") {
        let left = parse_temporal_formula(&spec[..pos])?;
        let right = parse_temporal_formula(&spec[pos + 4..])?;
        return Ok(TemporalFormula::Implies(Box::new(left), Box::new(right)));
    }

    if spec.starts_with("! ") {
        let inner = &spec[2..];
        return Ok(TemporalFormula::Not(Box::new(parse_temporal_formula(inner)?)));
    }

    // Otherwise treat as atomic proposition
    Ok(TemporalFormula::Atomic(spec.to_string()))
}

/// Convert formula to SMT-LIB format
fn formula_to_smt(formula: &TemporalFormula) -> String {
    match formula {
        TemporalFormula::Atomic(prop) => prop.clone(),
        TemporalFormula::Bool(b) => b.to_string(),
        TemporalFormula::Not(f) => format!("(not {})", formula_to_smt(f)),
        TemporalFormula::And(l, r) => {
            format!("(and {} {})", formula_to_smt(l), formula_to_smt(r))
        }
        TemporalFormula::Or(l, r) => {
            format!("(or {} {})", formula_to_smt(l), formula_to_smt(r))
        }
        TemporalFormula::Implies(l, r) => {
            format!("(=> {} {})", formula_to_smt(l), formula_to_smt(r))
        }
        TemporalFormula::Always(f) => {
            format!("(G {})", formula_to_smt(f))
        }
        TemporalFormula::Eventually(f) => {
            format!("(F {})", formula_to_smt(f))
        }
        TemporalFormula::Next(f) => {
            format!("(X {})", formula_to_smt(f))
        }
        _ => "unsupported".to_string(), // Simplified for now
    }
}

/// Convert formula to NuSMV format
fn formula_to_nusmv(formula: &TemporalFormula) -> String {
    match formula {
        TemporalFormula::Atomic(prop) => prop.clone(),
        TemporalFormula::Bool(b) => if *b { "TRUE" } else { "FALSE" }.to_string(),
        TemporalFormula::Not(f) => format!("! {}", formula_to_nusmv(f)),
        TemporalFormula::And(l, r) => {
            format!("({} & {})", formula_to_nusmv(l), formula_to_nusmv(r))
        }
        TemporalFormula::Or(l, r) => {
            format!("({} | {})", formula_to_nusmv(l), formula_to_nusmv(r))
        }
        TemporalFormula::Implies(l, r) => {
            format!("({} -> {})", formula_to_nusmv(l), formula_to_nusmv(r))
        }
        TemporalFormula::Always(f) => {
            format!("G {}", formula_to_nusmv(f))
        }
        TemporalFormula::Eventually(f) => {
            format!("F {}", formula_to_nusmv(f))
        }
        TemporalFormula::Next(f) => {
            format!("X {}", formula_to_nusmv(f))
        }
        TemporalFormula::AllAlways(f) => {
            format!("AG {}", formula_to_nusmv(f))
        }
        TemporalFormula::ExistsEventually(f) => {
            format!("EF {}", formula_to_nusmv(f))
        }
        _ => "unsupported".to_string(),
    }
}

/// Property library with common hardware verification properties
pub struct PropertyLibrary {
    properties: HashMap<String, Property>,
}

impl PropertyLibrary {
    pub fn new() -> Self {
        let mut lib = Self {
            properties: HashMap::new(),
        };
        lib.add_standard_properties();
        lib
    }

    fn add_standard_properties(&mut self) {
        // Clock domain crossing properties
        self.add_property(
            "cdc_no_metastability".to_string(),
            Property::safety(
                "cdc_no_metastability".to_string(),
                "!metastable_state".to_string(),
            )
        );

        // Reset properties
        self.add_property(
            "reset_synchronous".to_string(),
            Property::invariant(
                "reset_synchronous".to_string(),
                "reset -> (X !ready)".to_string(),
            )
        );

        // FIFO properties
        self.add_property(
            "fifo_no_overflow".to_string(),
            Property::safety(
                "fifo_no_overflow".to_string(),
                "!(full && push)".to_string(),
            )
        );

        self.add_property(
            "fifo_no_underflow".to_string(),
            Property::safety(
                "fifo_no_underflow".to_string(),
                "!(empty && pop)".to_string(),
            )
        );

        // Handshake protocol properties
        self.add_property(
            "handshake_valid_ready".to_string(),
            Property::safety(
                "handshake_valid_ready".to_string(),
                "valid -> F ready".to_string(),
            )
        );

        // Memory interface properties
        self.add_property(
            "memory_write_read_consistency".to_string(),
            Property::liveness(
                "memory_write_read_consistency".to_string(),
                "(write_enable && write_addr == read_addr) -> X (read_data == write_data)".to_string(),
            )
        );

        // Pipeline properties
        self.add_property(
            "pipeline_no_stall_deadlock".to_string(),
            Property::liveness(
                "pipeline_no_stall_deadlock".to_string(),
                "stall -> F !stall".to_string(),
            )
        );
    }

    pub fn add_property(&mut self, name: String, property: Property) {
        self.properties.insert(name, property);
    }

    pub fn get_property(&self, name: &str) -> Option<&Property> {
        self.properties.get(name)
    }

    pub fn list_properties(&self) -> Vec<&String> {
        self.properties.keys().collect()
    }

    /// Get properties by category
    pub fn get_properties_by_type(&self, prop_type: PropertyType) -> Vec<&Property> {
        self.properties
            .values()
            .filter(|p| std::mem::discriminant(&p.property_type) == std::mem::discriminant(&prop_type))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_property_creation() {
        let prop = Property::safety(
            "test_safety".to_string(),
            "counter < 256".to_string(),
        );

        assert_eq!(prop.name, "test_safety");
        assert!(matches!(prop.property_type, PropertyType::Safety));
    }

    #[test]
    fn test_formula_parsing() {
        let formula = parse_temporal_formula("G (counter < 256)").unwrap();

        match formula {
            TemporalFormula::Always(_) => {}, // Expected
            _ => panic!("Expected Always formula"),
        }
    }

    #[test]
    fn test_smt_conversion() {
        let prop = Property::safety(
            "test".to_string(),
            "x > 0".to_string(),
        );

        let smt = prop.to_smt();
        assert!(smt.contains("(assert"));
        assert!(smt.contains("x > 0"));
    }

    #[test]
    fn test_property_library() {
        let lib = PropertyLibrary::new();

        assert!(lib.get_property("fifo_no_overflow").is_some());
        assert!(lib.get_property("reset_synchronous").is_some());

        let safety_props = lib.get_properties_by_type(PropertyType::Safety);
        assert!(!safety_props.is_empty());
    }
}