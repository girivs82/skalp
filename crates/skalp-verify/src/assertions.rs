//! Assertion support for SKALP verification
//!
//! Supports both immediate (procedural) and concurrent (temporal) assertions

use serde::{Deserialize, Serialize};
use skalp_mir::mir::Expression;

/// Assertion in the design
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Assertion {
    /// Unique identifier
    pub id: String,

    /// Assertion kind
    pub kind: AssertionKind,

    /// Source location
    pub location: SourceLocation,

    /// Optional message
    pub message: Option<String>,

    /// Severity level
    pub severity: Severity,
}

/// Kind of assertion
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AssertionKind {
    /// Immediate assertion (evaluated immediately)
    Immediate(ImmediateAssertion),

    /// Concurrent assertion (evaluated over time)
    Concurrent(ConcurrentAssertion),

    /// Assumption (constrains formal analysis)
    Assume(Expression),

    /// Coverage point
    Cover(CoverPoint),
}

/// Immediate assertion
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImmediateAssertion {
    /// Condition to check
    pub condition: Expression,

    /// When to check (always, on edge, etc.)
    pub timing: AssertionTiming,

    /// Action on failure
    pub action: FailureAction,
}

/// Concurrent assertion
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConcurrentAssertion {
    /// Property to check
    pub property: PropertyExpression,

    /// Clock for evaluation
    pub clock: ClockSpec,

    /// Reset condition
    pub reset: Option<Expression>,

    /// Action on failure
    pub action: FailureAction,
}

/// Property expression for concurrent assertions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PropertyExpression {
    /// Simple boolean expression
    Boolean(Expression),

    /// Implication: antecedent |-> consequent
    Implication {
        antecedent: Box<PropertyExpression>,
        consequent: Box<PropertyExpression>,
    },

    /// Sequence of expressions
    Sequence(Vec<PropertyExpression>),

    /// Delay: ##n
    Delay {
        cycles: usize,
        property: Box<PropertyExpression>,
    },

    /// Repetition: [*n] or [*n:m]
    Repetition {
        min: usize,
        max: Option<usize>,
        property: Box<PropertyExpression>,
    },

    /// Throughout: expr throughout property
    Throughout {
        condition: Expression,
        property: Box<PropertyExpression>,
    },

    /// Eventually: ##[0:$] property
    Eventually(Box<PropertyExpression>),

    /// Always: property [*1:$]
    Always(Box<PropertyExpression>),
}

/// Coverage point
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverPoint {
    /// Name of the coverage point
    pub name: String,

    /// Expression to cover
    pub expression: Expression,

    /// Bins for value coverage
    pub bins: Vec<CoverageBin>,

    /// Cross coverage with other points
    pub cross: Vec<String>,
}

/// Coverage bin
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageBin {
    /// Bin name
    pub name: String,

    /// Values that fall in this bin
    pub values: BinValues,

    /// Target hits
    pub target: usize,

    /// Actual hits
    pub hits: usize,
}

/// Bin value specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BinValues {
    /// Single value
    Single(u64),

    /// Range of values
    Range { min: u64, max: u64 },

    /// List of values
    List(Vec<u64>),

    /// Transition (for FSM coverage)
    Transition { from: u64, to: u64 },
}

/// When to evaluate assertion
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AssertionTiming {
    /// Always (combinational)
    Always,

    /// On clock edge
    OnClock(ClockSpec),

    /// Final (at end of simulation)
    Final,

    /// Deferred (end of time step)
    Deferred,
}

/// Clock specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClockSpec {
    /// Clock signal name
    pub signal: String,

    /// Edge type
    pub edge: EdgeType,
}

/// Clock edge type
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum EdgeType {
    Rising,
    Falling,
    Both,
}

/// Action when assertion fails
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum FailureAction {
    /// Report error and continue
    Error,

    /// Report warning and continue
    Warning,

    /// Report info and continue
    Info,

    /// Stop simulation
    Fatal,
}

/// Severity level
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Severity {
    Info,
    Warning,
    Error,
    Fatal,
}

/// Source location for reporting
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceLocation {
    /// File name
    pub file: String,

    /// Line number
    pub line: usize,

    /// Column number
    pub column: usize,
}

/// Assertion checker
pub struct AssertionChecker {
    /// Assertions to check
    assertions: Vec<Assertion>,

    /// Failed assertions
    failures: Vec<AssertionFailure>,

    /// Coverage points
    coverage_points: Vec<CoverPoint>,
}

/// Assertion failure record
#[derive(Debug, Clone)]
pub struct AssertionFailure {
    /// Assertion that failed
    pub assertion: Assertion,

    /// Time of failure
    pub time: u64,

    /// Values at failure
    pub values: Vec<(String, u64)>,

    /// Failure message
    pub message: String,
}

impl AssertionChecker {
    /// Create a new assertion checker
    pub fn new() -> Self {
        Self {
            assertions: Vec::new(),
            failures: Vec::new(),
            coverage_points: Vec::new(),
        }
    }

    /// Add an assertion
    pub fn add_assertion(&mut self, assertion: Assertion) {
        self.assertions.push(assertion);
    }

    /// Check immediate assertion
    pub fn check_immediate(&mut self, assertion: &ImmediateAssertion, time: u64) -> bool {
        // Simplified - would evaluate expression
        true
    }

    /// Check concurrent assertion
    pub fn check_concurrent(&mut self, assertion: &ConcurrentAssertion, time: u64) -> bool {
        // Simplified - would evaluate property over time
        true
    }

    /// Update coverage point
    pub fn update_coverage(&mut self, point: &mut CoverPoint, value: u64) {
        for bin in &mut point.bins {
            if Self::value_in_bin(value, &bin.values) {
                bin.hits += 1;
            }
        }
    }

    /// Check if value is in bin
    fn value_in_bin(value: u64, bin_values: &BinValues) -> bool {
        match bin_values {
            BinValues::Single(v) => value == *v,
            BinValues::Range { min, max } => value >= *min && value <= *max,
            BinValues::List(values) => values.contains(&value),
            BinValues::Transition { from, to } => false, // Needs state tracking
        }
    }

    /// Get assertion report
    pub fn get_report(&self) -> AssertionReport {
        let total = self.assertions.len();
        let passed = total - self.failures.len();

        AssertionReport {
            total_assertions: total,
            passed: passed,
            failed: self.failures.len(),
            failures: self.failures.clone(),
            coverage_summary: self.get_coverage_summary(),
        }
    }

    /// Get coverage summary
    fn get_coverage_summary(&self) -> CoverageSummary {
        let mut total_bins = 0;
        let mut covered_bins = 0;

        for point in &self.coverage_points {
            for bin in &point.bins {
                total_bins += 1;
                if bin.hits >= bin.target {
                    covered_bins += 1;
                }
            }
        }

        CoverageSummary {
            total_points: self.coverage_points.len(),
            total_bins,
            covered_bins,
            coverage_percentage: if total_bins > 0 {
                100.0 * covered_bins as f64 / total_bins as f64
            } else {
                0.0
            },
        }
    }
}

/// Assertion report
#[derive(Debug, Clone)]
pub struct AssertionReport {
    /// Total assertions
    pub total_assertions: usize,

    /// Passed assertions
    pub passed: usize,

    /// Failed assertions
    pub failed: usize,

    /// Failure details
    pub failures: Vec<AssertionFailure>,

    /// Coverage summary
    pub coverage_summary: CoverageSummary,
}

/// Coverage summary
#[derive(Debug, Clone)]
pub struct CoverageSummary {
    /// Total coverage points
    pub total_points: usize,

    /// Total bins
    pub total_bins: usize,

    /// Covered bins
    pub covered_bins: usize,

    /// Coverage percentage
    pub coverage_percentage: f64,
}

impl AssertionReport {
    /// Print the report
    pub fn print(&self) {
        println!("=== Assertion Report ===");
        println!("Total: {}", self.total_assertions);
        println!("Passed: {}", self.passed);
        println!("Failed: {}", self.failed);

        if !self.failures.is_empty() {
            println!("\nFailures:");
            for failure in &self.failures {
                println!("  [{}] {} at time {}",
                         failure.assertion.id,
                         failure.message,
                         failure.time);
            }
        }

        println!("\n=== Coverage Summary ===");
        println!("Coverage points: {}", self.coverage_summary.total_points);
        println!("Total bins: {}", self.coverage_summary.total_bins);
        println!("Covered bins: {}", self.coverage_summary.covered_bins);
        println!("Coverage: {:.1}%", self.coverage_summary.coverage_percentage);
    }
}