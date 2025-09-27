//! Property and sequence definitions for temporal assertions

use serde::{Deserialize, Serialize};
use std::fmt;

/// Property definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Property {
    /// Property name
    pub name: String,

    /// Property expression
    pub expression: PropertyExpr,

    /// Clock domain
    pub clock: Option<String>,

    /// Reset condition
    pub reset: Option<String>,

    /// Property kind
    pub kind: PropertyKind,
}

/// Kind of property
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum PropertyKind {
    /// Safety property (nothing bad happens)
    Safety,

    /// Liveness property (something good eventually happens)
    Liveness,

    /// Fairness property
    Fairness,

    /// Invariant
    Invariant,
}

/// Property expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PropertyExpr {
    /// Atomic proposition
    Atom(String),

    /// Negation
    Not(Box<PropertyExpr>),

    /// Conjunction
    And(Box<PropertyExpr>, Box<PropertyExpr>),

    /// Disjunction
    Or(Box<PropertyExpr>, Box<PropertyExpr>),

    /// Implication
    Implies(Box<PropertyExpr>, Box<PropertyExpr>),

    /// Temporal operator
    Temporal(TemporalOperator, Box<PropertyExpr>),

    /// Sequence
    Sequence(Sequence),

    /// Property reference
    Reference(String),
}

/// Temporal operators
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TemporalOperator {
    /// Next (X)
    Next,

    /// Eventually (F)
    Eventually { bound: Option<usize> },

    /// Always/Globally (G)
    Always { bound: Option<usize> },

    /// Until (U)
    Until { bound: Option<usize> },

    /// Release (R)
    Release,

    /// Weak until (W)
    WeakUntil,
}

/// Sequence definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Sequence {
    /// Sequence name
    pub name: String,

    /// Sequence elements
    pub elements: Vec<SequenceElement>,
}

/// Sequence element
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SequenceElement {
    /// Expression
    Expr(String),

    /// Delay ##n
    Delay(usize),

    /// Range delay ##[m:n]
    RangeDelay { min: usize, max: usize },

    /// Repetition [*n]
    Repeat { count: usize },

    /// Range repetition [*m:n]
    RangeRepeat { min: usize, max: usize },

    /// Non-consecutive repetition [=n]
    NonConsecutiveRepeat { count: usize },

    /// Goto repetition [->n]
    GotoRepeat { count: usize },

    /// Throughout
    Throughout(String),

    /// Within
    Within { start: usize, end: usize },
}

/// Property builder for fluent API
pub struct PropertyBuilder {
    name: String,
    expr: Option<PropertyExpr>,
    clock: Option<String>,
    reset: Option<String>,
    kind: PropertyKind,
}

impl PropertyBuilder {
    /// Create a new property builder
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            expr: None,
            clock: None,
            reset: None,
            kind: PropertyKind::Safety,
        }
    }

    /// Set the property expression
    pub fn expression(mut self, expr: PropertyExpr) -> Self {
        self.expr = Some(expr);
        self
    }

    /// Set the clock
    pub fn clock(mut self, clock: impl Into<String>) -> Self {
        self.clock = Some(clock.into());
        self
    }

    /// Set the reset
    pub fn reset(mut self, reset: impl Into<String>) -> Self {
        self.reset = Some(reset.into());
        self
    }

    /// Set as safety property
    pub fn safety(mut self) -> Self {
        self.kind = PropertyKind::Safety;
        self
    }

    /// Set as liveness property
    pub fn liveness(mut self) -> Self {
        self.kind = PropertyKind::Liveness;
        self
    }

    /// Build the property
    pub fn build(self) -> Result<Property, String> {
        let expr = self.expr.ok_or("Property expression required")?;

        Ok(Property {
            name: self.name,
            expression: expr,
            clock: self.clock,
            reset: self.reset,
            kind: self.kind,
        })
    }
}

/// Sequence builder for fluent API
pub struct SequenceBuilder {
    name: String,
    elements: Vec<SequenceElement>,
}

impl SequenceBuilder {
    /// Create a new sequence builder
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            elements: Vec::new(),
        }
    }

    /// Add an expression
    pub fn expr(mut self, expr: impl Into<String>) -> Self {
        self.elements.push(SequenceElement::Expr(expr.into()));
        self
    }

    /// Add a delay
    pub fn delay(mut self, cycles: usize) -> Self {
        self.elements.push(SequenceElement::Delay(cycles));
        self
    }

    /// Add a range delay
    pub fn range_delay(mut self, min: usize, max: usize) -> Self {
        self.elements.push(SequenceElement::RangeDelay { min, max });
        self
    }

    /// Add repetition
    pub fn repeat(mut self, count: usize) -> Self {
        self.elements.push(SequenceElement::Repeat { count });
        self
    }

    /// Build the sequence
    pub fn build(self) -> Sequence {
        Sequence {
            name: self.name,
            elements: self.elements,
        }
    }
}

/// Property evaluation context
pub struct PropertyEvaluator {
    /// Properties to evaluate
    properties: Vec<Property>,

    /// Current time
    time: u64,

    /// Signal values
    signals: std::collections::HashMap<String, bool>,

    /// Property states
    states: Vec<PropertyState>,
}

/// Property evaluation state
#[derive(Debug, Clone)]
struct PropertyState {
    /// Property being evaluated
    property_id: usize,

    /// Current state
    state: EvalState,

    /// History for temporal operators
    history: Vec<bool>,
}

/// Evaluation state
#[derive(Debug, Clone)]
enum EvalState {
    /// Not started
    Idle,

    /// In progress
    Active { start_time: u64 },

    /// Passed
    Passed { at_time: u64 },

    /// Failed
    Failed { at_time: u64 },

    /// Vacuous (antecedent never true)
    Vacuous,
}

impl PropertyEvaluator {
    /// Create a new evaluator
    pub fn new() -> Self {
        Self {
            properties: Vec::new(),
            time: 0,
            signals: std::collections::HashMap::new(),
            states: Vec::new(),
        }
    }

    /// Add a property to evaluate
    pub fn add_property(&mut self, property: Property) {
        let id = self.properties.len();
        self.properties.push(property);
        self.states.push(PropertyState {
            property_id: id,
            state: EvalState::Idle,
            history: Vec::new(),
        });
    }

    /// Update signal value
    pub fn update_signal(&mut self, name: &str, value: bool) {
        self.signals.insert(name.to_string(), value);
    }

    /// Step evaluation
    pub fn step(&mut self) {
        self.time += 1;

        for i in 0..self.states.len() {
            let property = &self.properties[self.states[i].property_id];
            let result = self.evaluate_expr(&property.expression);
            self.states[i].history.push(result);

            // Update state based on property kind and result
            match property.kind {
                PropertyKind::Safety => {
                    if !result {
                        self.states[i].state = EvalState::Failed { at_time: self.time };
                    }
                }
                PropertyKind::Liveness => {
                    if result && matches!(self.states[i].state, EvalState::Active { .. }) {
                        self.states[i].state = EvalState::Passed { at_time: self.time };
                    }
                }
                _ => {}
            }
        }
    }


    /// Evaluate a property expression
    fn evaluate_expr(&self, expr: &PropertyExpr) -> bool {
        match expr {
            PropertyExpr::Atom(name) => {
                *self.signals.get(name).unwrap_or(&false)
            }
            PropertyExpr::Not(e) => {
                !self.evaluate_expr(e)
            }
            PropertyExpr::And(a, b) => {
                self.evaluate_expr(a) && self.evaluate_expr(b)
            }
            PropertyExpr::Or(a, b) => {
                self.evaluate_expr(a) || self.evaluate_expr(b)
            }
            PropertyExpr::Implies(a, b) => {
                !self.evaluate_expr(a) || self.evaluate_expr(b)
            }
            PropertyExpr::Temporal(op, e) => {
                self.evaluate_temporal(op, e)
            }
            _ => false, // Simplified
        }
    }

    /// Evaluate temporal operator
    fn evaluate_temporal(&self, op: &TemporalOperator, expr: &PropertyExpr) -> bool {
        match op {
            TemporalOperator::Next => {
                // Would check next state
                false
            }
            TemporalOperator::Eventually { bound } => {
                // Would check if eventually true within bound
                false
            }
            TemporalOperator::Always { bound } => {
                // Would check if always true within bound
                true
            }
            _ => false,
        }
    }

    /// Get evaluation report
    pub fn get_report(&self) -> PropertyReport {
        let mut passed = 0;
        let mut failed = 0;
        let mut vacuous = 0;

        for state in &self.states {
            match state.state {
                EvalState::Passed { .. } => passed += 1,
                EvalState::Failed { .. } => failed += 1,
                EvalState::Vacuous => vacuous += 1,
                _ => {}
            }
        }

        PropertyReport {
            total: self.properties.len(),
            passed,
            failed,
            vacuous,
            in_progress: self.properties.len() - passed - failed - vacuous,
        }
    }
}

/// Property evaluation report
#[derive(Debug)]
pub struct PropertyReport {
    /// Total properties
    pub total: usize,

    /// Passed properties
    pub passed: usize,

    /// Failed properties
    pub failed: usize,

    /// Vacuous properties
    pub vacuous: usize,

    /// In progress
    pub in_progress: usize,
}

impl fmt::Display for PropertyReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Properties: {} total, {} passed, {} failed, {} vacuous, {} in progress",
               self.total, self.passed, self.failed, self.vacuous, self.in_progress)
    }
}