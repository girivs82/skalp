//! Automated Safety Analysis via Fault Simulation
//!
//! This module implements the novel approach to FMEA/FMEDA generation through
//! top-down, simulation-driven safety analysis. Instead of manual bottom-up
//! failure analysis with estimated DC, this approach:
//!
//! 1. Safety Goal defines observable failure effects at entity level
//! 2. Compiler injects faults at every primitive
//! 3. Simulation determines which primitives cause which effects
//! 4. DC is MEASURED from simulation, not estimated from tables
//! 5. FMEA/FMEDA is auto-generated with evidence
//!
//! Key Innovation: Transform safety analysis from manual, late-stage verification
//! into compile-time, iterative design feedback.

use crate::asil::AsilLevel;
use crate::fmeda_library::TechLibrary;
use crate::hierarchy::{DesignRef, FailureClass, InstancePath, Severity};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::time::Duration;

// ============================================================================
// Fault Types and Sites
// ============================================================================

/// Types of faults that can be injected at primitives
///
/// This enum provides a high-level classification of fault types for safety
/// analysis. These map to the more detailed fault types in `skalp_sim::sir::FaultType`
/// for actual simulation.
///
/// # Fault Categories
///
/// 1. **Permanent Faults**: Manufacturing defects, wear-out
///    - StuckAt0, StuckAt1, Bridging, Open
///
/// 2. **Transient Faults**: Radiation, EMI, power transients
///    - Transient, MultiBitUpset
///
/// 3. **Timing Violations**: Design margins, temperature, voltage
///    - SetupViolation, HoldViolation, Metastability
///
/// 4. **Power-Related**: Digital effects of analog power issues
///    - VoltageDropout, GroundBounce
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FaultType {
    // ========================================================================
    // Permanent/Hard Faults (Manufacturing, Wear-out)
    // ========================================================================
    /// Output stuck at logic 0
    /// Cause: Manufacturing defect, oxide breakdown, electromigration
    StuckAt0,

    /// Output stuck at logic 1
    /// Cause: Manufacturing defect, oxide breakdown, electromigration
    StuckAt1,

    /// Bridging fault (short between signals)
    /// Cause: Metal bridging, contamination, ESD damage
    Bridging,

    /// Open fault (signal disconnected)
    /// Cause: Broken via, cracked metal, bond wire failure
    Open,

    // ========================================================================
    // Transient/Soft Faults (Radiation, EMI)
    // ========================================================================
    /// Single-bit transient (SET/SEU)
    /// Cause: Single-Event Upset from radiation, alpha particles
    Transient,

    /// Multi-bit upset (for memory cells)
    /// Cause: High-energy particle affecting multiple bits
    MultiBitUpset,

    // ========================================================================
    // Timing Violations
    // ========================================================================
    /// Setup time violation
    /// Cause: Data path too slow, clock arrives too early
    /// Effect: FF captures previous data value instead of current
    SetupViolation,

    /// Hold time violation
    /// Cause: Data path too fast, clock arrives too late
    /// Effect: FF captures corrupted/inverted value
    HoldViolation,

    /// Metastability
    /// Cause: Async input, clock domain crossing
    /// Effect: FF output undefined for N cycles, then resolves randomly
    Metastability,

    /// Generic timing violation (legacy, use SetupViolation/HoldViolation instead)
    #[deprecated(note = "Use SetupViolation or HoldViolation for more precise analysis")]
    TimingViolation,

    // ========================================================================
    // Power-Related Faults (Digital Effects)
    // ========================================================================
    /// Voltage dropout / IR drop
    /// Cause: Sudden current demand, inadequate power grid
    /// Effect: Regional setup violations (modeled as SetupViolation on affected FFs)
    VoltageDropout,

    /// Ground bounce
    /// Cause: Simultaneous switching noise (SSN)
    /// Effect: Transient glitches on outputs
    GroundBounce,

    /// Crosstalk-induced glitch
    /// Cause: Capacitive coupling from adjacent signal
    /// Effect: Brief glitch on victim signal
    CrosstalkGlitch,

    // ========================================================================
    // Clock Faults
    // ========================================================================
    /// Clock glitch (extra clock edge)
    /// Cause: EMI, power supply noise
    /// Effect: FFs may capture data twice
    ClockGlitch,
}

impl FaultType {
    /// Get the minimal standard fault set for basic analysis
    ///
    /// This set covers the most common failure modes and is sufficient
    /// for initial design validation. Includes only stuck-at faults.
    pub fn minimal_set() -> Vec<FaultType> {
        vec![FaultType::StuckAt0, FaultType::StuckAt1]
    }

    /// Get the standard fault set for comprehensive stuck-at analysis
    ///
    /// This is the traditional fault set used in ATPG and DFT.
    /// Adds transient faults to the minimal set.
    pub fn standard_set() -> Vec<FaultType> {
        vec![
            FaultType::StuckAt0,
            FaultType::StuckAt1,
            FaultType::Transient,
        ]
    }

    /// Get the timing-aware fault set
    ///
    /// Adds setup/hold violations for timing-critical designs.
    /// Use this for designs with tight timing margins or ASIL-D requirements.
    pub fn timing_aware_set() -> Vec<FaultType> {
        vec![
            FaultType::StuckAt0,
            FaultType::StuckAt1,
            FaultType::Transient,
            FaultType::SetupViolation,
            FaultType::HoldViolation,
        ]
    }

    /// Get the extended fault set including all digital-level faults
    ///
    /// Comprehensive set for ASIL-D certification with full coverage of:
    /// - Permanent faults (stuck-at, open)
    /// - Transient faults (SEU, MBU)
    /// - Timing violations (setup, hold, metastability)
    /// - Power effects (voltage dropout, ground bounce)
    pub fn extended_set() -> Vec<FaultType> {
        vec![
            // Permanent
            FaultType::StuckAt0,
            FaultType::StuckAt1,
            FaultType::Open,
            // Transient
            FaultType::Transient,
            FaultType::MultiBitUpset,
            // Timing
            FaultType::SetupViolation,
            FaultType::HoldViolation,
            FaultType::Metastability,
            // Power
            FaultType::VoltageDropout,
            FaultType::GroundBounce,
        ]
    }

    /// Get the ASIL-D comprehensive fault set
    ///
    /// Maximum coverage for ISO 26262 ASIL-D certification.
    /// Includes all fault types that can be modeled at the digital level.
    pub fn asil_d_set() -> Vec<FaultType> {
        vec![
            // Permanent faults
            FaultType::StuckAt0,
            FaultType::StuckAt1,
            FaultType::Bridging,
            FaultType::Open,
            // Transient faults
            FaultType::Transient,
            FaultType::MultiBitUpset,
            // Timing violations
            FaultType::SetupViolation,
            FaultType::HoldViolation,
            FaultType::Metastability,
            // Power-related
            FaultType::VoltageDropout,
            FaultType::GroundBounce,
            FaultType::CrosstalkGlitch,
            // Clock faults
            FaultType::ClockGlitch,
        ]
    }

    /// Human-readable name for reports
    pub fn name(&self) -> &'static str {
        match self {
            FaultType::StuckAt0 => "stuck_at_0",
            FaultType::StuckAt1 => "stuck_at_1",
            FaultType::Transient => "transient",
            FaultType::MultiBitUpset => "multi_bit_upset",
            FaultType::SetupViolation => "setup_violation",
            FaultType::HoldViolation => "hold_violation",
            FaultType::Metastability => "metastability",
            #[allow(deprecated)]
            FaultType::TimingViolation => "timing_violation",
            FaultType::Bridging => "bridging",
            FaultType::Open => "open",
            FaultType::VoltageDropout => "voltage_dropout",
            FaultType::GroundBounce => "ground_bounce",
            FaultType::CrosstalkGlitch => "crosstalk_glitch",
            FaultType::ClockGlitch => "clock_glitch",
        }
    }

    /// Human-readable display name for UI
    pub fn display_name(&self) -> &'static str {
        match self {
            FaultType::StuckAt0 => "Stuck-at-0",
            FaultType::StuckAt1 => "Stuck-at-1",
            FaultType::Transient => "Transient (SEU)",
            FaultType::MultiBitUpset => "Multi-bit Upset",
            FaultType::SetupViolation => "Setup Violation",
            FaultType::HoldViolation => "Hold Violation",
            FaultType::Metastability => "Metastability",
            #[allow(deprecated)]
            FaultType::TimingViolation => "Timing Violation",
            FaultType::Bridging => "Bridging Fault",
            FaultType::Open => "Open Fault",
            FaultType::VoltageDropout => "Voltage Dropout",
            FaultType::GroundBounce => "Ground Bounce",
            FaultType::CrosstalkGlitch => "Crosstalk Glitch",
            FaultType::ClockGlitch => "Clock Glitch",
        }
    }

    /// Get the fault category for grouping in reports
    pub fn category(&self) -> FaultCategory {
        match self {
            FaultType::StuckAt0 | FaultType::StuckAt1 | FaultType::Bridging | FaultType::Open => {
                FaultCategory::Permanent
            }
            FaultType::Transient | FaultType::MultiBitUpset => FaultCategory::Transient,
            FaultType::SetupViolation | FaultType::HoldViolation | FaultType::Metastability => {
                FaultCategory::Timing
            }
            #[allow(deprecated)]
            FaultType::TimingViolation => FaultCategory::Timing,
            FaultType::VoltageDropout | FaultType::GroundBounce | FaultType::CrosstalkGlitch => {
                FaultCategory::Power
            }
            FaultType::ClockGlitch => FaultCategory::Clock,
        }
    }

    /// Check if this fault type is applicable to sequential elements (FFs)
    pub fn is_sequential_fault(&self) -> bool {
        matches!(
            self,
            FaultType::SetupViolation
                | FaultType::HoldViolation
                | FaultType::Metastability
                | FaultType::ClockGlitch
        )
    }

    /// Check if this fault type is a regional fault (affects multiple primitives)
    pub fn is_regional_fault(&self) -> bool {
        matches!(
            self,
            FaultType::VoltageDropout | FaultType::GroundBounce | FaultType::MultiBitUpset
        )
    }
}

/// Category of fault for grouping in reports
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FaultCategory {
    /// Permanent/hard faults (manufacturing defects, wear-out)
    Permanent,
    /// Transient/soft faults (radiation, EMI)
    Transient,
    /// Timing violations (design margins, temperature, voltage)
    Timing,
    /// Power-related faults (voltage, ground, crosstalk)
    Power,
    /// Clock-related faults (glitches, jitter)
    Clock,
}

impl FaultCategory {
    /// Human-readable name
    pub fn name(&self) -> &'static str {
        match self {
            FaultCategory::Permanent => "Permanent",
            FaultCategory::Transient => "Transient",
            FaultCategory::Timing => "Timing",
            FaultCategory::Power => "Power",
            FaultCategory::Clock => "Clock",
        }
    }

    /// Description for reports
    pub fn description(&self) -> &'static str {
        match self {
            FaultCategory::Permanent => "Manufacturing defects and wear-out failures",
            FaultCategory::Transient => "Radiation-induced and EMI-related upsets",
            FaultCategory::Timing => "Setup/hold violations and metastability",
            FaultCategory::Power => "Voltage droop, ground bounce, and crosstalk",
            FaultCategory::Clock => "Clock tree glitches and jitter",
        }
    }
}

/// Definition of a power region for regional fault injection
///
/// Power regions group primitives that share power/ground rails and are
/// susceptible to correlated failures (e.g., voltage dropout affecting
/// all FFs in a region simultaneously).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerRegion {
    /// Name of the power region
    pub name: String,
    /// Description
    pub description: Option<String>,
    /// Glob pattern matching primitives in this region
    pub primitive_pattern: String,
    /// Explicit list of primitive paths (if pattern not sufficient)
    pub explicit_primitives: Vec<DesignRef>,
}

impl PowerRegion {
    /// Create a new power region with a glob pattern
    pub fn new(name: &str, pattern: &str) -> Self {
        Self {
            name: name.to_string(),
            description: None,
            primitive_pattern: pattern.to_string(),
            explicit_primitives: Vec::new(),
        }
    }

    /// Add description
    pub fn with_description(mut self, desc: &str) -> Self {
        self.description = Some(desc.to_string());
        self
    }

    /// Add explicit primitive
    pub fn with_primitive(mut self, prim: DesignRef) -> Self {
        self.explicit_primitives.push(prim);
        self
    }
}

/// A specific fault injection site (primitive + fault type)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct FaultSite {
    /// Hierarchical path to the primitive instance
    pub primitive_path: DesignRef,
    /// Type of fault to inject
    pub fault_type: FaultType,
    /// Bit index for multi-bit primitives (e.g., register bit)
    pub bit_index: Option<u32>,
}

impl FaultSite {
    pub fn new(primitive_path: DesignRef, fault_type: FaultType) -> Self {
        Self {
            primitive_path,
            fault_type,
            bit_index: None,
        }
    }

    pub fn with_bit(mut self, bit: u32) -> Self {
        self.bit_index = Some(bit);
        self
    }

    /// Generate unique identifier for this fault site
    pub fn id(&self) -> String {
        let mut id = format!("{}@{}", self.primitive_path, self.fault_type.name());
        if let Some(bit) = self.bit_index {
            id.push_str(&format!("[{}]", bit));
        }
        id
    }
}

// ============================================================================
// Failure Effect Definitions (from Safety Goal)
// ============================================================================

/// Temporal qualifier for failure effect conditions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TemporalQualifier {
    /// Effect is instant (single cycle)
    Immediate,
    /// Effect persists for at least N cycles
    ForCycles(u64),
    /// Effect persists for at least duration
    ForDuration(Duration),
    /// Effect occurs within N cycles
    WithinCycles(u64),
    /// Effect eventually occurs
    Eventually,
}

/// Comparison operator for conditions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CompareOp {
    Equal,
    NotEqual,
    GreaterThan,
    GreaterOrEqual,
    LessThan,
    LessOrEqual,
}

/// Value to compare against in conditions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CompareValue {
    /// Literal constant value
    Literal(u64),
    /// Golden model signal reference
    Golden(String),
    /// Previous cycle value
    Previous,
    /// Another signal in the design
    Signal(DesignRef),
}

/// A single condition term (signal op value)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConditionTerm {
    /// Signal to observe
    pub signal: DesignRef,
    /// Comparison operator
    pub op: CompareOp,
    /// Value to compare against
    pub value: CompareValue,
}

/// Built-in temporal operators for failure effect conditions
/// These correspond to the @operator() syntax in safety goals
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TemporalOperator {
    // === Edge Detection ===
    /// @rose(s) - Rising edge (0→1 this cycle)
    Rose(String),
    /// @fell(s) - Falling edge (1→0 this cycle)
    Fell(String),
    /// @changed(s) - Value changed this cycle
    Changed(String),
    /// @stable(s, n) - Unchanged for n consecutive cycles
    Stable(String, u64),

    // === History/Past Values ===
    /// @prev(s) - Value in previous cycle
    Prev(String),
    /// @prev(s, n) - Value from n cycles ago
    PrevN(String, u64),
    /// @was_high(s, n) - Was 1 at some point in last n cycles
    WasHigh(String, u64),
    /// @was_low(s, n) - Was 0 at some point in last n cycles
    WasLow(String, u64),
    /// @cycles_since(event) - Cycles since event was true
    CyclesSince(Box<EffectCondition>),

    // === Range/Set Membership ===
    /// @in_range(s, lo, hi) - lo ≤ s ≤ hi
    InRange(String, u64, u64),
    /// @outside_range(s, lo, hi) - s < lo OR s > hi
    OutsideRange(String, u64, u64),
    /// @oneof(s, [vals]) - s in set
    OneOf(String, Vec<u64>),
    /// @not_oneof(s, [vals]) - s not in set
    NotOneOf(String, Vec<u64>),

    // === Arithmetic ===
    /// @abs_diff(a, b) - |a - b|
    AbsDiff(String, String),
    /// @max_deviation(a, b, ...) - Maximum pairwise |diff|
    MaxDeviation(Vec<String>),
    /// @popcount(s) - Count of 1 bits
    PopCount(String),
    /// @hamming_distance(a, b) - Differing bit count
    HammingDistance(String, String),

    // === Counting/Frequency ===
    /// @pulse_count(s, window) - Rising edges in window
    PulseCount(String, u64),
    /// @glitch_count(s, window) - Value changes in window
    GlitchCount(String, u64),
    /// @period(s) - Cycles between rising edges
    Period(String),

    // === Data Integrity ===
    /// @crc8(data) - Calculate CRC-8
    Crc8(String),
    /// @crc16(data) - Calculate CRC-16
    Crc16(String),
    /// @crc32(data) - Calculate CRC-32
    Crc32(String),
    /// @parity(data) - Calculate parity
    Parity(String),

    // === Timing ===
    /// @latency(trigger, response) - Cycles between events
    Latency(String, String),
}

/// Compound condition (AND/OR of terms)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EffectCondition {
    /// Single term (signal op value)
    Term(ConditionTerm),
    /// AND of conditions
    And(Vec<EffectCondition>),
    /// OR of conditions
    Or(Vec<EffectCondition>),
    /// NOT of condition
    Not(Box<EffectCondition>),
    /// Temporal: condition holds for duration
    Temporal(Box<EffectCondition>, TemporalQualifier),
    /// Built-in temporal operator
    Operator(TemporalOperator),
    /// Comparison involving temporal operator result
    OperatorCompare(TemporalOperator, CompareOp, u64),
}

impl EffectCondition {
    /// Create a simple equality condition
    pub fn equals(signal: DesignRef, value: u64) -> Self {
        Self::Term(ConditionTerm {
            signal,
            op: CompareOp::Equal,
            value: CompareValue::Literal(value),
        })
    }

    /// Create a deviation from golden model condition
    pub fn deviates_from_golden(signal: DesignRef, golden_signal: &str) -> Self {
        Self::Term(ConditionTerm {
            signal,
            op: CompareOp::NotEqual,
            value: CompareValue::Golden(golden_signal.to_string()),
        })
    }

    /// Create a stuck condition (signal same as previous)
    pub fn is_stuck(signal: DesignRef, cycles: u64) -> Self {
        Self::Temporal(
            Box::new(Self::Term(ConditionTerm {
                signal,
                op: CompareOp::Equal,
                value: CompareValue::Previous,
            })),
            TemporalQualifier::ForCycles(cycles),
        )
    }

    /// AND two conditions
    pub fn and(self, other: Self) -> Self {
        match self {
            Self::And(mut terms) => {
                terms.push(other);
                Self::And(terms)
            }
            _ => Self::And(vec![self, other]),
        }
    }

    /// OR two conditions
    pub fn or(self, other: Self) -> Self {
        match self {
            Self::Or(mut terms) => {
                terms.push(other);
                Self::Or(terms)
            }
            _ => Self::Or(vec![self, other]),
        }
    }

    /// Negate a condition (logical NOT)
    pub fn negate(self) -> Self {
        Self::Not(Box::new(self))
    }

    // === Temporal Operator Builders ===

    /// @rose(signal) - Rising edge detected
    pub fn rose(signal: &str) -> Self {
        Self::Operator(TemporalOperator::Rose(signal.to_string()))
    }

    /// @fell(signal) - Falling edge detected
    pub fn fell(signal: &str) -> Self {
        Self::Operator(TemporalOperator::Fell(signal.to_string()))
    }

    /// @changed(signal) - Value changed this cycle
    pub fn changed(signal: &str) -> Self {
        Self::Operator(TemporalOperator::Changed(signal.to_string()))
    }

    /// @stable(signal, cycles) - Signal unchanged for N cycles
    pub fn stable(signal: &str, cycles: u64) -> Self {
        Self::Operator(TemporalOperator::Stable(signal.to_string(), cycles))
    }

    /// @in_range(signal, lo, hi) - Signal in range
    pub fn in_range(signal: &str, lo: u64, hi: u64) -> Self {
        Self::Operator(TemporalOperator::InRange(signal.to_string(), lo, hi))
    }

    /// @outside_range(signal, lo, hi) - Signal outside range
    pub fn outside_range(signal: &str, lo: u64, hi: u64) -> Self {
        Self::Operator(TemporalOperator::OutsideRange(signal.to_string(), lo, hi))
    }

    /// @oneof(signal, values) - Signal is one of the values
    pub fn oneof(signal: &str, values: Vec<u64>) -> Self {
        Self::Operator(TemporalOperator::OneOf(signal.to_string(), values))
    }

    /// @not_oneof(signal, values) - Signal is not one of the values
    pub fn not_oneof(signal: &str, values: Vec<u64>) -> Self {
        Self::Operator(TemporalOperator::NotOneOf(signal.to_string(), values))
    }

    /// @abs_diff(a, b) > threshold
    pub fn abs_diff_exceeds(a: &str, b: &str, threshold: u64) -> Self {
        Self::OperatorCompare(
            TemporalOperator::AbsDiff(a.to_string(), b.to_string()),
            CompareOp::GreaterThan,
            threshold,
        )
    }

    /// @max_deviation(signals) > threshold
    pub fn max_deviation_exceeds(signals: Vec<&str>, threshold: u64) -> Self {
        Self::OperatorCompare(
            TemporalOperator::MaxDeviation(signals.into_iter().map(|s| s.to_string()).collect()),
            CompareOp::GreaterThan,
            threshold,
        )
    }

    /// @pulse_count(signal, window) < min
    pub fn pulse_count_below(signal: &str, window: u64, min: u64) -> Self {
        Self::OperatorCompare(
            TemporalOperator::PulseCount(signal.to_string(), window),
            CompareOp::LessThan,
            min,
        )
    }

    /// @pulse_count(signal, window) > max
    pub fn pulse_count_above(signal: &str, window: u64, max: u64) -> Self {
        Self::OperatorCompare(
            TemporalOperator::PulseCount(signal.to_string(), window),
            CompareOp::GreaterThan,
            max,
        )
    }

    /// @cycles_since(event) > timeout
    pub fn timeout_since(event: EffectCondition, timeout: u64) -> Self {
        Self::OperatorCompare(
            TemporalOperator::CyclesSince(Box::new(event)),
            CompareOp::GreaterThan,
            timeout,
        )
    }
}

/// Definition of an observable failure effect
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FailureEffectDef {
    /// Unique name for this effect
    pub name: String,
    /// Description of what this effect means
    pub description: Option<String>,
    /// Condition that defines this effect
    pub condition: EffectCondition,
    /// Safety severity classification
    pub severity: Severity,
    /// Target diagnostic coverage for this effect
    pub target_dc: f64,
}

// ============================================================================
// Safety Goal Extensions (Monitor and Effects)
// ============================================================================

/// Signals to monitor during fault simulation
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct MonitorSpec {
    /// Signals to observe for failure effects
    pub signals: Vec<DesignRef>,
    /// Signals that indicate fault detection (from safety mechanisms)
    pub detection_signals: Vec<DesignRef>,
    /// Safe state indicators
    pub safe_state_signals: Vec<DesignRef>,
}

impl MonitorSpec {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_signal(&mut self, signal: DesignRef) {
        self.signals.push(signal);
    }

    pub fn add_detection(&mut self, signal: DesignRef) {
        self.detection_signals.push(signal);
    }

    pub fn add_safe_state(&mut self, signal: DesignRef) {
        self.safe_state_signals.push(signal);
    }
}

/// Golden model specification for comparison
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GoldenSpec {
    /// Golden values as signal -> expression mapping
    pub values: HashMap<String, String>,
    /// External golden model reference (e.g., C model path)
    pub external_model: Option<String>,
}

/// Test scenario for fault simulation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestScenario {
    /// Name of the test scenario
    pub name: String,
    /// Description
    pub description: Option<String>,
    /// Test vector source (file path or inline)
    pub vectors: TestVectorSource,
    /// Number of cycles to simulate
    pub cycles: u64,
    /// Weight for this scenario in DC calculation
    pub weight: f64,
}

/// Source of test vectors
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TestVectorSource {
    /// Path to test vector file
    File(String),
    /// Inline test vectors (name -> values per cycle)
    Inline(HashMap<String, Vec<Vec<u8>>>),
    /// Randomly generated vectors
    Random { seed: u64, count: u64 },
    /// Reference to testbench entity
    Testbench(String),
}

/// Extended safety goal with simulation specifications
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyGoalSimSpec {
    /// Safety goal name (references existing SafetyGoal)
    pub goal_name: String,
    /// ASIL level
    pub asil: AsilLevel,
    /// Fault tolerance time interval
    pub ftti: Option<Duration>,

    /// Signals to monitor
    pub monitor: MonitorSpec,
    /// Golden model specification
    pub golden: Option<GoldenSpec>,
    /// Failure effect definitions
    pub failure_effects: Vec<FailureEffectDef>,
    /// Test scenarios
    pub test_scenarios: Vec<TestScenario>,

    /// ASIL-derived DC targets per severity
    pub dc_targets: HashMap<Severity, f64>,
}

impl SafetyGoalSimSpec {
    pub fn new(goal_name: &str, asil: AsilLevel) -> Self {
        let mut dc_targets = HashMap::new();
        // ASIL-derived targets per ISO 26262
        match asil {
            AsilLevel::D => {
                dc_targets.insert(Severity::S3, 0.99);
                dc_targets.insert(Severity::S2, 0.97);
                dc_targets.insert(Severity::S1, 0.90);
            }
            AsilLevel::C => {
                dc_targets.insert(Severity::S3, 0.97);
                dc_targets.insert(Severity::S2, 0.90);
                dc_targets.insert(Severity::S1, 0.80);
            }
            AsilLevel::B => {
                dc_targets.insert(Severity::S3, 0.90);
                dc_targets.insert(Severity::S2, 0.80);
                dc_targets.insert(Severity::S1, 0.60);
            }
            AsilLevel::A => {
                dc_targets.insert(Severity::S3, 0.80);
                dc_targets.insert(Severity::S2, 0.60);
                dc_targets.insert(Severity::S1, 0.60);
            }
            AsilLevel::QM => {}
        }
        Self {
            goal_name: goal_name.to_string(),
            asil,
            ftti: None,
            monitor: MonitorSpec::new(),
            golden: None,
            failure_effects: Vec::new(),
            test_scenarios: Vec::new(),
            dc_targets,
        }
    }

    pub fn add_effect(&mut self, effect: FailureEffectDef) {
        self.failure_effects.push(effect);
    }

    pub fn add_scenario(&mut self, scenario: TestScenario) {
        self.test_scenarios.push(scenario);
    }

    /// Get target DC for a given severity
    pub fn target_dc(&self, severity: Severity) -> f64 {
        *self.dc_targets.get(&severity).unwrap_or(&0.0)
    }
}

// ============================================================================
// Simulation Results
// ============================================================================

/// Result of simulating a single fault
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FaultSimulationResult {
    /// The fault that was injected
    pub fault_site: FaultSite,
    /// Which test scenario was run
    pub scenario_name: String,
    /// Did the fault cause any observable failure effect?
    pub caused_effect: Option<String>,
    /// Was the fault detected by a safety mechanism?
    pub detected: bool,
    /// Which safety mechanism detected it (if any)
    pub detected_by: Option<String>,
    /// Cycle at which fault was injected
    pub injection_cycle: u64,
    /// Cycle at which effect was observed (if any)
    pub effect_cycle: Option<u64>,
    /// Cycle at which fault was detected (if any)
    pub detection_cycle: Option<u64>,
    /// Latency from injection to detection
    pub detection_latency: Option<u64>,
}

impl FaultSimulationResult {
    /// Check if this is a dangerous undetected fault
    pub fn is_dangerous_undetected(&self) -> bool {
        self.caused_effect.is_some() && !self.detected
    }

    /// Check if this is a safe fault (no effect or detected before effect)
    pub fn is_safe(&self) -> bool {
        self.caused_effect.is_none()
            || (self.detected
                && self.detection_cycle.is_some()
                && self.effect_cycle.is_some()
                && self.detection_cycle.unwrap() <= self.effect_cycle.unwrap())
    }
}

/// Aggregated results for a failure effect
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EffectAnalysis {
    /// Effect name
    pub effect_name: String,
    /// Severity
    pub severity: Severity,
    /// Total faults that cause this effect
    pub total_faults_causing: u64,
    /// Faults detected before effect occurred
    pub faults_detected: u64,
    /// Measured diagnostic coverage
    pub measured_dc: f64,
    /// Target DC from ASIL
    pub target_dc: f64,
    /// Does measured DC meet target?
    pub meets_target: bool,
    /// Primitives contributing to this effect (path -> count)
    pub contributors: HashMap<String, u64>,
    /// Undetected fault sites (for gap analysis)
    pub undetected_sites: Vec<FaultSite>,
    /// Detection breakdown by mechanism
    pub detection_by_mechanism: HashMap<String, u64>,
}

impl EffectAnalysis {
    pub fn new(effect_name: &str, severity: Severity, target_dc: f64) -> Self {
        Self {
            effect_name: effect_name.to_string(),
            severity,
            total_faults_causing: 0,
            faults_detected: 0,
            measured_dc: 0.0,
            target_dc,
            meets_target: false,
            contributors: HashMap::new(),
            undetected_sites: Vec::new(),
            detection_by_mechanism: HashMap::new(),
        }
    }

    /// Update DC calculation
    pub fn update_dc(&mut self) {
        if self.total_faults_causing > 0 {
            self.measured_dc = self.faults_detected as f64 / self.total_faults_causing as f64;
        }
        self.meets_target = self.measured_dc >= self.target_dc;
    }

    /// Get gap (how much DC is missing)
    pub fn dc_gap(&self) -> f64 {
        if self.meets_target {
            0.0
        } else {
            self.target_dc - self.measured_dc
        }
    }

    /// Get number of additional faults that need detection to meet target
    pub fn faults_needed_for_target(&self) -> u64 {
        if self.meets_target {
            0
        } else {
            let needed_detected = (self.target_dc * self.total_faults_causing as f64).ceil() as u64;
            needed_detected.saturating_sub(self.faults_detected)
        }
    }
}

/// Complete simulation campaign results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SimulationCampaignResults {
    /// Safety goal being analyzed
    pub goal_name: String,
    /// Design being analyzed
    pub design_name: String,
    /// Total primitives in design
    pub total_primitives: u64,
    /// Total fault sites (primitives × fault types)
    pub total_fault_sites: u64,
    /// Total faults simulated
    pub faults_simulated: u64,
    /// Total simulation cycles
    pub total_cycles: u64,
    /// Wall clock time for simulation
    pub wall_time: Duration,
    /// Per-effect analysis results
    pub effect_analyses: HashMap<String, EffectAnalysis>,
    /// Overall SPFM
    pub spfm: f64,
    /// Overall LFM
    pub lfm: f64,
    /// Overall PMHF (if FIT data available)
    pub pmhf: Option<f64>,
    /// Does analysis meet ASIL requirements?
    pub meets_asil: bool,
    /// Gap summary (effects not meeting targets)
    pub gaps: Vec<GapSummary>,
}

impl SimulationCampaignResults {
    pub fn new(goal_name: &str, design_name: &str) -> Self {
        Self {
            goal_name: goal_name.to_string(),
            design_name: design_name.to_string(),
            total_primitives: 0,
            total_fault_sites: 0,
            faults_simulated: 0,
            total_cycles: 0,
            wall_time: Duration::ZERO,
            effect_analyses: HashMap::new(),
            spfm: 0.0,
            lfm: 0.0,
            pmhf: None,
            meets_asil: false,
            gaps: Vec::new(),
        }
    }

    /// Update overall metrics from effect analyses
    pub fn update_metrics(&mut self) {
        // Calculate SPFM: fraction of single-point faults that are detected
        let mut total_spf = 0u64;
        let mut detected_spf = 0u64;

        for analysis in self.effect_analyses.values() {
            total_spf += analysis.total_faults_causing;
            detected_spf += analysis.faults_detected;
        }

        if total_spf > 0 {
            self.spfm = detected_spf as f64 / total_spf as f64;
        }

        // Check if all effects meet targets
        self.meets_asil = self.effect_analyses.values().all(|a| a.meets_target);

        // Build gap summary
        self.gaps.clear();
        for analysis in self.effect_analyses.values() {
            if !analysis.meets_target {
                self.gaps.push(GapSummary {
                    effect_name: analysis.effect_name.clone(),
                    severity: analysis.severity,
                    measured_dc: analysis.measured_dc,
                    target_dc: analysis.target_dc,
                    gap: analysis.dc_gap(),
                    undetected_count: analysis.undetected_sites.len(),
                    top_contributors: analysis
                        .contributors
                        .iter()
                        .take(5)
                        .map(|(k, v)| (k.clone(), *v))
                        .collect(),
                });
            }
        }
    }
}

/// Summary of a gap (effect not meeting target)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GapSummary {
    /// Effect name
    pub effect_name: String,
    /// Severity
    pub severity: Severity,
    /// Measured DC
    pub measured_dc: f64,
    /// Target DC
    pub target_dc: f64,
    /// Gap (target - measured)
    pub gap: f64,
    /// Number of undetected fault sites
    pub undetected_count: usize,
    /// Top contributing primitive patterns
    pub top_contributors: Vec<(String, u64)>,
}

// ============================================================================
// Fault Simulation Engine Configuration
// ============================================================================

/// Configuration for fault simulation campaign
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SimulationCampaignConfig {
    /// Fault types to inject
    pub fault_types: Vec<FaultType>,
    /// Maximum faults to simulate (for sampling)
    pub max_faults: Option<u64>,
    /// Random sampling ratio (0.0-1.0, None = exhaustive)
    pub sampling_ratio: Option<f64>,
    /// Random seed for reproducibility
    pub seed: u64,
    /// Parallel simulation threads
    pub parallel_threads: usize,
    /// Use GPU acceleration if available
    pub use_gpu: bool,
    /// Timeout per fault simulation (ms)
    pub timeout_per_fault_ms: u64,
    /// Enable fault collapsing optimization
    pub fault_collapsing: bool,
    /// Enable incremental mode (only changed portions)
    pub incremental: bool,
    /// Checkpoint interval (save progress every N faults)
    pub checkpoint_interval: Option<u64>,
}

impl Default for SimulationCampaignConfig {
    fn default() -> Self {
        Self {
            fault_types: FaultType::standard_set(),
            max_faults: None,
            sampling_ratio: None,
            seed: 42,
            parallel_threads: 4,
            use_gpu: true,
            timeout_per_fault_ms: 10_000, // 10 seconds per fault
            fault_collapsing: true,
            incremental: false,
            checkpoint_interval: Some(10_000), // Checkpoint every 10k faults
        }
    }
}

impl SimulationCampaignConfig {
    /// Config for quick estimation (sampling mode)
    pub fn quick_estimate() -> Self {
        Self {
            sampling_ratio: Some(0.01), // 1% sampling
            max_faults: Some(10_000),
            ..Default::default()
        }
    }

    /// Config for full exhaustive analysis
    pub fn exhaustive() -> Self {
        Self {
            sampling_ratio: None,
            max_faults: None,
            fault_types: FaultType::extended_set(),
            ..Default::default()
        }
    }
}

// ============================================================================
// Auto-Generated FMEDA
// ============================================================================

/// Auto-generated FMEDA entry from simulation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AutoFmedaEntry {
    /// Failure mode name (from effect name)
    pub name: String,
    /// Condition that defines this failure mode
    pub condition: String,
    /// Severity
    pub severity: Severity,
    /// Failure class (derived from detection status)
    pub class: FailureClass,
    /// Total FIT (from tech library, weighted by contribution)
    pub total_fit: f64,
    /// Measured DC (from simulation)
    pub measured_dc: f64,

    /// Contributors (primitive pattern -> weight)
    pub contributors: Vec<AutoFmedaContributor>,
    /// Detection breakdown by mechanism
    pub detection: Vec<AutoFmedaDetection>,
    /// Undetected primitive paths (for gap analysis)
    pub undetected: Vec<String>,
}

/// Contributor to an auto-generated FMEDA entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AutoFmedaContributor {
    /// Primitive pattern (may be glob)
    pub pattern: String,
    /// Number of primitives matching pattern
    pub primitive_count: u64,
    /// FIT contribution
    pub fit: f64,
    /// Weight (fraction of total)
    pub weight: f64,
}

/// Detection breakdown for auto-generated FMEDA
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AutoFmedaDetection {
    /// Safety mechanism name
    pub mechanism: String,
    /// Number of faults detected by this mechanism
    pub covered: u64,
    /// Measured DC for this mechanism
    pub dc: f64,
}

/// Complete auto-generated FMEDA
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AutoFmeda {
    /// Analysis metadata
    pub metadata: AutoFmedaMetadata,
    /// Summary metrics
    pub summary: AutoFmedaSummary,
    /// Failure mode entries
    pub entries: Vec<AutoFmedaEntry>,
}

/// Metadata about the auto-generated FMEDA
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AutoFmedaMetadata {
    /// Safety goal name
    pub goal_name: String,
    /// Design name
    pub design_name: String,
    /// Analysis timestamp
    pub timestamp: String,
    /// Tool version
    pub tool_version: String,
    /// Number of primitives analyzed
    pub primitives_analyzed: u64,
    /// Number of faults injected
    pub faults_injected: u64,
    /// Total simulation cycles
    pub simulation_cycles: u64,
    /// Wall clock time
    pub wall_time: String,
}

/// Summary metrics for auto-generated FMEDA
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AutoFmedaSummary {
    /// Total FIT
    pub total_fit: f64,
    /// SPFM
    pub spfm: f64,
    /// LFM
    pub lfm: f64,
    /// PMHF
    pub pmhf: f64,
    /// ASIL achieved
    pub asil_achieved: AsilLevel,
    /// Overall pass/fail
    pub meets_requirements: bool,
}

// ============================================================================
// Compile-Time Error Reporting
// ============================================================================

/// Safety analysis error for compile-time feedback
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyAnalysisError {
    /// Error code
    pub code: String,
    /// Error message
    pub message: String,
    /// Source location (safety goal file)
    pub location: Option<String>,
    /// Gaps identified
    pub gaps: Vec<GapDetail>,
    /// Suggestions for fixing
    pub suggestions: Vec<String>,
}

/// Detailed gap information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GapDetail {
    /// Effect/failure mode name
    pub effect: String,
    /// Measured DC
    pub measured_dc: f64,
    /// Target DC
    pub target_dc: f64,
    /// Undetected primitive locations
    pub undetected_locations: Vec<String>,
    /// Primitive count
    pub primitive_count: u64,
}

impl SafetyAnalysisError {
    /// Create error for DC not met
    pub fn dc_not_met(
        goal_name: &str,
        effect: &str,
        measured: f64,
        target: f64,
        asil: AsilLevel,
    ) -> Self {
        Self {
            code: "SAFETY001".to_string(),
            message: format!(
                "ASIL-{:?} requirements not met for '{}': DC {:.1}% < target {:.1}%",
                asil,
                effect,
                measured * 100.0,
                target * 100.0
            ),
            location: Some(format!("safety_goal {}", goal_name)),
            gaps: Vec::new(),
            suggestions: Vec::new(),
        }
    }

    /// Add gap detail
    pub fn with_gap(mut self, gap: GapDetail) -> Self {
        self.gaps.push(gap);
        self
    }

    /// Add suggestion
    pub fn with_suggestion(mut self, suggestion: &str) -> Self {
        self.suggestions.push(suggestion.to_string());
        self
    }

    /// Format for compiler output
    pub fn format_error(&self) -> String {
        let mut output = String::new();

        // Error header
        output.push_str(&format!("error[{}]: {}\n", self.code, self.message));

        // Location
        if let Some(ref loc) = self.location {
            output.push_str(&format!("  --> {}\n", loc));
        }

        // Gap details
        for gap in &self.gaps {
            output.push_str(&format!(
                "\nGap Analysis for '{}' (DC: {:.1}%, need {:.1}%):\n",
                gap.effect,
                gap.measured_dc * 100.0,
                gap.target_dc * 100.0
            ));
            output.push_str("┌─────────────────────────────────────┬────────┐\n");
            output.push_str("│ Undetected Fault Location           │ Count  │\n");
            output.push_str("├─────────────────────────────────────┼────────┤\n");
            for loc in &gap.undetected_locations {
                output.push_str(&format!("│ {:35} │ {:>6} │\n", loc, gap.primitive_count));
            }
            output.push_str("└─────────────────────────────────────┴────────┘\n");
        }

        // Suggestions
        if !self.suggestions.is_empty() {
            output.push_str("\n  = help: ");
            output.push_str(&self.suggestions.join("\n  = help: "));
            output.push('\n');
        }

        output
    }
}

// ============================================================================
// Fault Campaign Conversion (skalp-sim integration)
// ============================================================================

/// Convert fault simulation results from skalp-sim to safety analysis format.
///
/// This function bridges the gap between the simulation engine (skalp-sim) and
/// the safety analysis framework (skalp-safety). It transforms raw fault injection
/// results into structured safety metrics.
///
/// # Arguments
///
/// * `campaign` - Fault simulation results from skalp-sim
/// * `primitive_to_path` - Mapping from PrimitiveId to cell path (from GateNetlist)
/// * `annotations` - Safety mechanism annotations (which mechanisms cover which paths)
/// * `goal_name` - Name of the safety goal being analyzed
/// * `design_name` - Name of the design being analyzed
///
/// # Returns
///
/// `SimulationCampaignResults` with per-mechanism detection metrics and overall SPFM/LFM.
///
/// # Example Flow
///
/// ```ignore
/// // 1. Compile design to GateNetlist
/// let netlist = compile_to_gate_netlist(&source)?;
///
/// // 2. Convert to SIR for simulation
/// let mut converter = GateNetlistToSirConverter::new();
/// let sir_result = converter.convert(&netlist);
///
/// // 3. Build primitive-to-path mapping
/// let primitive_to_path = build_primitive_path_map(&netlist, &converter);
///
/// // 4. Run fault simulation
/// let campaign_results = simulator.run_fault_campaign(&config)?;
///
/// // 5. Convert to safety analysis format
/// let safety_results = fault_campaign_to_safety_results(
///     &campaign_results,
///     &primitive_to_path,
///     &annotations,
///     "BrakingSafety",
///     "BrakeController",
/// );
/// ```
#[cfg(feature = "sim-integration")]
pub fn fault_campaign_to_safety_results(
    campaign: &skalp_sim::FaultCampaignResults,
    primitive_to_path: &std::collections::HashMap<skalp_lir::lir::PrimitiveId, String>,
    annotations: &[crate::design_resolver::SafetyAnnotation],
    goal_name: &str,
    design_name: &str,
) -> SimulationCampaignResults {
    use crate::design_resolver::SafetyAnnotation;
    use std::time::Duration;

    let mut results = SimulationCampaignResults::new(goal_name, design_name);
    results.total_primitives = primitive_to_path.len() as u64;
    results.faults_simulated = campaign.total_faults as u64;

    // Create a default effect analysis for the safety goal
    // In a more complete implementation, effects would come from SafetyGoal definitions
    let default_effect = format!("{}_violation", goal_name);
    results.effect_analyses.insert(
        default_effect.clone(),
        EffectAnalysis::new(&default_effect, Severity::S3, 0.99), // Default to S3 severity, 99% DC target
    );

    // Build a map from path prefix to mechanism name for quick lookup
    let mut path_to_mechanism: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    for annotation in annotations {
        if annotation.goal_name == goal_name {
            path_to_mechanism.insert(
                annotation.design_ref.to_string(),
                annotation.mechanism_name.clone(),
            );
        }
    }

    // Process each fault result
    for fault_result in &campaign.fault_results {
        let primitive_id = fault_result.fault.target_primitive;

        // Get the cell path for this primitive
        let cell_path = match primitive_to_path.get(&primitive_id) {
            Some(path) => path.clone(),
            None => format!("unknown_primitive_{}", primitive_id.0),
        };

        // Find which mechanism covers this cell (if any)
        let mechanism = find_covering_mechanism(&cell_path, &path_to_mechanism);

        // Update effect analysis
        if let Some(effect_analysis) = results.effect_analyses.get_mut(&default_effect) {
            // Every fault potentially causes this effect (conservative assumption)
            effect_analysis.total_faults_causing += 1;

            if fault_result.detected {
                effect_analysis.faults_detected += 1;

                // Credit the mechanism if found
                if let Some(mech_name) = &mechanism {
                    *effect_analysis
                        .detection_by_mechanism
                        .entry(mech_name.clone())
                        .or_insert(0) += 1;
                }
            } else {
                // Record undetected fault site
                let fault_type = convert_sim_fault_type(fault_result.fault.fault_type);
                effect_analysis
                    .undetected_sites
                    .push(FaultSite::new(DesignRef::parse(&cell_path), fault_type));
            }

            // Track contributors
            *effect_analysis.contributors.entry(cell_path).or_insert(0) += 1;
        }
    }

    // Calculate measured DC for each effect
    for effect_analysis in results.effect_analyses.values_mut() {
        if effect_analysis.total_faults_causing > 0 {
            effect_analysis.measured_dc = effect_analysis.faults_detected as f64
                / effect_analysis.total_faults_causing as f64;
        }
        effect_analysis.meets_target = effect_analysis.measured_dc >= effect_analysis.target_dc;
    }

    // Update overall metrics
    results.update_metrics();

    results
}

/// Find which safety mechanism covers a given cell path
#[cfg(feature = "sim-integration")]
fn find_covering_mechanism(
    cell_path: &str,
    path_to_mechanism: &std::collections::HashMap<String, String>,
) -> Option<String> {
    // Check for exact match first
    if let Some(mechanism) = path_to_mechanism.get(cell_path) {
        return Some(mechanism.clone());
    }

    // Check for prefix matches (hierarchical coverage)
    for (annotated_path, mechanism) in path_to_mechanism {
        if cell_path.starts_with(annotated_path) {
            return Some(mechanism.clone());
        }
    }

    None
}

/// Convert skalp-sim FaultType to skalp-safety FaultType
#[cfg(feature = "sim-integration")]
fn convert_sim_fault_type(sim_fault: skalp_sim::sir::FaultType) -> FaultType {
    match sim_fault {
        // Permanent faults
        skalp_sim::sir::FaultType::StuckAt0 => FaultType::StuckAt0,
        skalp_sim::sir::FaultType::StuckAt1 => FaultType::StuckAt1,
        skalp_sim::sir::FaultType::Bridge { .. } => FaultType::Bridging,
        skalp_sim::sir::FaultType::Open { .. } => FaultType::Open,

        // Transient faults
        skalp_sim::sir::FaultType::Transient => FaultType::Transient,
        skalp_sim::sir::FaultType::BitFlip => FaultType::Transient,
        skalp_sim::sir::FaultType::MultiBitUpset { .. } => FaultType::MultiBitUpset,

        // Timing violations
        skalp_sim::sir::FaultType::SetupViolation => FaultType::SetupViolation,
        skalp_sim::sir::FaultType::HoldViolation => FaultType::HoldViolation,
        skalp_sim::sir::FaultType::Metastability { .. } => FaultType::Metastability,
        skalp_sim::sir::FaultType::TimingDelay { .. } => FaultType::SetupViolation, // Model as setup violation

        // Power-related faults
        skalp_sim::sir::FaultType::VoltageDropout => FaultType::VoltageDropout,
        skalp_sim::sir::FaultType::GroundBounce => FaultType::GroundBounce,
        skalp_sim::sir::FaultType::CrosstalkGlitch => FaultType::CrosstalkGlitch,
        skalp_sim::sir::FaultType::ClockGlitch => FaultType::ClockGlitch,
        skalp_sim::sir::FaultType::ClockStretch { .. } => FaultType::ClockGlitch,
    }
}

/// Run effect-aware fault campaign - the "double advantage" implementation
///
/// This is the key innovation: the same simulation cycle provides BOTH:
/// 1. Failure effect identification - which faults cause which safety goal violations
/// 2. Measured diagnostic coverage - which faults are detected by which mechanisms
///
/// # Arguments
/// * `campaign` - Raw fault campaign results from skalp-sim
/// * `primitive_to_path` - Mapping from primitive IDs to design paths
/// * `spec` - Safety goal specification with effect conditions
/// * `annotations` - Safety annotations for mechanism mapping
///
/// # Returns
/// `SimulationCampaignResults` with properly classified effects and measured DC
#[cfg(feature = "sim-integration")]
pub fn fault_campaign_with_effects(
    campaign: &skalp_sim::FaultCampaignResults,
    primitive_to_path: &std::collections::HashMap<skalp_lir::lir::PrimitiveId, String>,
    spec: &SafetyGoalSimSpec,
    annotations: &[crate::design_resolver::SafetyAnnotation],
) -> SimulationCampaignResults {
    use crate::safety_driven_fmea::EffectMonitor;

    let mut results = SimulationCampaignResults::new(&spec.goal_name, "design");
    results.total_primitives = primitive_to_path.len() as u64;
    results.faults_simulated = campaign.total_faults as u64;

    // Initialize effect analyses from the safety goal specification
    for effect in &spec.failure_effects {
        results.effect_analyses.insert(
            effect.name.clone(),
            EffectAnalysis::new(&effect.name, effect.severity, effect.target_dc),
        );
    }

    // Add a catch-all effect for output corruption not matching specific effects
    results.effect_analyses.insert(
        "_output_corruption".to_string(),
        EffectAnalysis::new("_output_corruption", Severity::S2, 0.95),
    );

    // Build mechanism lookup from annotations
    let mut path_to_mechanism: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    for annotation in annotations {
        if annotation.goal_name == spec.goal_name {
            path_to_mechanism.insert(
                annotation.design_ref.to_string(),
                annotation.mechanism_name.clone(),
            );
        }
    }

    // Create the effect monitor
    let monitor = EffectMonitor::new(spec);

    // Process each fault result
    for fault_result in &campaign.fault_results {
        let primitive_id = fault_result.fault.target_primitive;
        let cell_path = primitive_to_path
            .get(&primitive_id)
            .cloned()
            .unwrap_or_else(|| format!("unknown_primitive_{}", primitive_id.0));

        // Convert output differences to signal values for effect evaluation
        let signal_values = extract_signal_values_from_diffs(&fault_result.output_diffs);

        // Check which effects were triggered by this fault
        let triggered_effects = if signal_values.is_empty() {
            // No output corruption - safe fault
            vec![]
        } else {
            // Evaluate effect conditions
            let effects = monitor.check_effects(&signal_values);
            if effects.is_empty() {
                // Output corruption but no specific effect - record as generic
                vec!["_output_corruption".to_string()]
            } else {
                effects
            }
        };

        // Skip safe faults (no effects)
        if triggered_effects.is_empty() {
            continue;
        }

        // Find covering mechanism
        let mechanism = find_covering_mechanism(&cell_path, &path_to_mechanism);

        // Update effect analyses with the double advantage:
        // - Effect identification: which effect was triggered
        // - Detection tracking: was it detected, by which mechanism
        for effect_name in &triggered_effects {
            if let Some(analysis) = results.effect_analyses.get_mut(effect_name) {
                analysis.total_faults_causing += 1;

                if fault_result.detected {
                    analysis.faults_detected += 1;
                    if let Some(mech_name) = &mechanism {
                        *analysis
                            .detection_by_mechanism
                            .entry(mech_name.clone())
                            .or_insert(0) += 1;
                    }
                } else {
                    // Record undetected fault site
                    let fault_type = convert_sim_fault_type(fault_result.fault.fault_type);
                    analysis.undetected_sites.push(FaultSite::new(
                        crate::hierarchy::DesignRef::parse(&cell_path),
                        fault_type,
                    ));
                }

                // Track contributing cells
                *analysis.contributors.entry(cell_path.clone()).or_insert(0) += 1;
            }
        }
    }

    // Update measured DC for all effects
    for analysis in results.effect_analyses.values_mut() {
        analysis.update_dc();
    }

    // Calculate overall metrics
    let mut total_dangerous = 0u64;
    let mut total_detected = 0u64;
    let mut total_latent = 0u64;
    for analysis in results.effect_analyses.values() {
        total_dangerous += analysis.total_faults_causing;
        total_detected += analysis.faults_detected;
        total_latent += analysis.undetected_sites.len() as u64;
    }

    // SPFM = detected / total_dangerous
    results.spfm = if total_dangerous > 0 {
        total_detected as f64 / total_dangerous as f64
    } else {
        1.0
    };

    // LFM = detected / (detected + latent)
    results.lfm = if total_detected + total_latent > 0 {
        total_detected as f64 / (total_detected + total_latent) as f64
    } else {
        1.0
    };

    // Check if meets ASIL requirements
    results.meets_asil = results
        .effect_analyses
        .values()
        .filter(|a| !a.effect_name.starts_with('_'))
        .all(|a| a.meets_target);

    results
}

/// Extract signal values from output differences for effect condition evaluation
#[cfg(feature = "sim-integration")]
fn extract_signal_values_from_diffs(
    output_diffs: &std::collections::HashMap<String, (Vec<bool>, Vec<bool>)>,
) -> std::collections::HashMap<String, u64> {
    let mut values = std::collections::HashMap::new();

    for (signal_name, (_normal, faulty)) in output_diffs {
        // Convert boolean vector to u64 value (faulty value)
        let value = faulty.iter().enumerate().fold(
            0u64,
            |acc, (i, &bit)| {
                if bit {
                    acc | (1 << i)
                } else {
                    acc
                }
            },
        );
        values.insert(signal_name.clone(), value);
    }

    values
}

/// Helper function to build primitive-to-path mapping from GateNetlist and converter.
///
/// This should be called after converting GateNetlist to SIR to build the mapping
/// needed by `fault_campaign_to_safety_results`.
///
/// # Example
///
/// ```ignore
/// let mut converter = GateNetlistToSirConverter::new();
/// let _sir_result = converter.convert(&netlist);
/// let primitive_to_path = build_primitive_path_map(&netlist, &converter);
/// ```
#[cfg(feature = "sim-integration")]
pub fn build_primitive_path_map(
    netlist: &skalp_lir::GateNetlist,
    converter: &skalp_sim::GateNetlistToSirConverter,
) -> std::collections::HashMap<skalp_lir::lir::PrimitiveId, String> {
    let mut map = std::collections::HashMap::new();

    for cell in &netlist.cells {
        if let Some(primitive_id) = converter.get_primitive_id(cell.id) {
            map.insert(primitive_id, cell.path.clone());
        }
    }

    map
}

// ============================================================================
// Power Domain Fault Injection
// ============================================================================

/// Types of domain-wide power faults
///
/// These fault types affect entire power domains simultaneously, modeling
/// realistic power supply failures for ISO 26262 Common Cause Failure analysis.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DomainFaultType {
    /// Complete power failure - all cells in domain stuck-at-0
    /// Cause: Power regulator failure, short circuit, fuse blow
    /// Effect: All outputs in domain go to 0
    PowerFailure,

    /// Voltage dropout - cells fail based on voltage sensitivity
    /// Cause: Load transient, IR drop, regulator droop
    /// Effect: Sensitive cells fail first, robust cells survive longer
    VoltageDropout {
        /// Target voltage in millivolts (nominal is typically 1000mV)
        target_voltage_mv: u16,
    },

    /// Progressive brownout - cells fail in sensitivity order
    /// Cause: Gradual power supply degradation, battery discharge
    /// Effect: Cells fail one by one in order of voltage sensitivity
    Brownout {
        /// Starting voltage in millivolts
        start_voltage_mv: u16,
        /// Ending voltage in millivolts
        end_voltage_mv: u16,
        /// Rate of voltage drop in mV per cycle
        droop_rate_mv_per_cycle: u16,
    },

    /// Ground bounce - transient glitches on all outputs
    /// Cause: Simultaneous switching noise (SSN)
    /// Effect: Brief glitches, proportional to cell current draw
    GroundBounce {
        /// Magnitude of bounce in millivolts
        magnitude_mv: u16,
        /// Duration in cycles
        duration_cycles: u32,
    },

    /// Power-on reset failure - cells come up in random state
    /// Cause: POR circuit failure, slow voltage ramp
    /// Effect: State elements have undefined initial values
    PowerOnResetFailure,
}

impl DomainFaultType {
    /// Get name for reporting
    pub fn name(&self) -> &'static str {
        match self {
            DomainFaultType::PowerFailure => "power_failure",
            DomainFaultType::VoltageDropout { .. } => "voltage_dropout",
            DomainFaultType::Brownout { .. } => "brownout",
            DomainFaultType::GroundBounce { .. } => "ground_bounce",
            DomainFaultType::PowerOnResetFailure => "por_failure",
        }
    }

    /// Get human-readable description
    pub fn description(&self) -> String {
        match self {
            DomainFaultType::PowerFailure => {
                "Complete power failure - all cells stuck at 0".to_string()
            }
            DomainFaultType::VoltageDropout { target_voltage_mv } => {
                format!(
                    "Voltage dropout to {}mV - cells fail based on sensitivity",
                    target_voltage_mv
                )
            }
            DomainFaultType::Brownout {
                start_voltage_mv,
                end_voltage_mv,
                droop_rate_mv_per_cycle,
            } => {
                format!(
                    "Progressive brownout {}mV→{}mV at {}mV/cycle",
                    start_voltage_mv, end_voltage_mv, droop_rate_mv_per_cycle
                )
            }
            DomainFaultType::GroundBounce {
                magnitude_mv,
                duration_cycles,
            } => {
                format!(
                    "Ground bounce {}mV for {} cycles",
                    magnitude_mv, duration_cycles
                )
            }
            DomainFaultType::PowerOnResetFailure => {
                "POR failure - undefined initial states".to_string()
            }
        }
    }

    /// Get standard domain fault set for ASIL-D analysis
    pub fn asil_d_set() -> Vec<DomainFaultType> {
        vec![
            DomainFaultType::PowerFailure,
            DomainFaultType::VoltageDropout {
                target_voltage_mv: 700,
            },
            DomainFaultType::VoltageDropout {
                target_voltage_mv: 800,
            },
            DomainFaultType::Brownout {
                start_voltage_mv: 1000,
                end_voltage_mv: 600,
                droop_rate_mv_per_cycle: 10,
            },
            DomainFaultType::GroundBounce {
                magnitude_mv: 200,
                duration_cycles: 5,
            },
            DomainFaultType::PowerOnResetFailure,
        ]
    }
}

/// A domain-wide fault injection site
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DomainFaultSite {
    /// Power domain name
    pub domain_name: String,
    /// Domain voltage (nominal, in mV)
    pub domain_voltage_mv: u16,
    /// Type of domain fault to inject
    pub fault_type: DomainFaultType,
    /// Primitive paths affected by this domain
    pub affected_primitives: Vec<String>,
    /// Per-primitive voltage sensitivity (1=most sensitive, 10=most robust)
    pub primitive_sensitivities: HashMap<String, u8>,
}

impl DomainFaultSite {
    /// Create a new domain fault site
    pub fn new(domain_name: &str, domain_voltage_mv: u16, fault_type: DomainFaultType) -> Self {
        Self {
            domain_name: domain_name.to_string(),
            domain_voltage_mv,
            fault_type,
            affected_primitives: Vec::new(),
            primitive_sensitivities: HashMap::new(),
        }
    }

    /// Add a primitive to this domain with its voltage sensitivity
    pub fn with_primitive(mut self, path: &str, sensitivity: u8) -> Self {
        self.affected_primitives.push(path.to_string());
        self.primitive_sensitivities
            .insert(path.to_string(), sensitivity);
        self
    }

    /// Add multiple primitives
    pub fn with_primitives(mut self, paths: &[(String, u8)]) -> Self {
        for (path, sensitivity) in paths {
            self.affected_primitives.push(path.clone());
            self.primitive_sensitivities
                .insert(path.clone(), *sensitivity);
        }
        self
    }

    /// Generate unique identifier for this domain fault
    pub fn id(&self) -> String {
        format!("domain:{}@{}", self.domain_name, self.fault_type.name())
    }

    /// Get primitives sorted by voltage sensitivity (most sensitive first)
    pub fn primitives_by_sensitivity(&self) -> Vec<&String> {
        let mut paths: Vec<_> = self.affected_primitives.iter().collect();
        paths.sort_by_key(|p| self.primitive_sensitivities.get(*p).copied().unwrap_or(5));
        paths
    }

    /// Determine which primitives would fail at a given voltage
    ///
    /// Uses voltage sensitivity to determine failure order:
    /// - Sensitivity 1-2: Fail at 900mV (most sensitive - e.g., retention cells)
    /// - Sensitivity 3-4: Fail at 800mV (high sensitivity - e.g., sequential)
    /// - Sensitivity 5-6: Fail at 750mV (medium - e.g., combinational)
    /// - Sensitivity 7-8: Fail at 700mV (low - e.g., isolation cells)
    /// - Sensitivity 9-10: Fail at 650mV (very robust - e.g., power switches)
    pub fn primitives_failing_at_voltage(&self, voltage_mv: u16) -> Vec<&String> {
        self.affected_primitives
            .iter()
            .filter(|p| {
                let sensitivity = self.primitive_sensitivities.get(*p).copied().unwrap_or(5);
                let fail_voltage = match sensitivity {
                    1..=2 => 900,  // Most sensitive
                    3..=4 => 800,  // High sensitivity
                    5..=6 => 750,  // Medium
                    7..=8 => 700,  // Low
                    9..=10 => 650, // Very robust
                    _ => 750,      // Default to medium
                };
                voltage_mv <= fail_voltage
            })
            .collect()
    }
}

/// Result of domain-wide fault injection
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DomainFaultResult {
    /// The domain fault that was injected
    pub domain_fault: DomainFaultSite,
    /// Number of primitives affected
    pub primitives_affected: usize,
    /// Primitives that failed (for progressive faults like brownout)
    pub failed_primitives: Vec<String>,
    /// Digital fault effects applied to each primitive
    pub primitive_faults: HashMap<String, FaultType>,
    /// Whether any detection signal triggered
    pub detected: bool,
    /// Which detection signals triggered
    pub detecting_signals: Vec<String>,
    /// Cycles until first detection (if detected)
    pub detection_latency_cycles: Option<u64>,
}

impl DomainFaultResult {
    /// Create result for complete power failure
    pub fn power_failure(domain_fault: DomainFaultSite) -> Self {
        let primitives_affected = domain_fault.affected_primitives.len();
        let primitive_faults: HashMap<String, FaultType> = domain_fault
            .affected_primitives
            .iter()
            .map(|p| (p.clone(), FaultType::StuckAt0))
            .collect();

        Self {
            failed_primitives: domain_fault.affected_primitives.clone(),
            domain_fault,
            primitives_affected,
            primitive_faults,
            detected: false,
            detecting_signals: Vec::new(),
            detection_latency_cycles: None,
        }
    }

    /// Create result for voltage dropout
    pub fn voltage_dropout(domain_fault: DomainFaultSite, voltage_mv: u16) -> Self {
        let failed = domain_fault.primitives_failing_at_voltage(voltage_mv);
        let primitives_affected = failed.len();
        let failed_primitives: Vec<String> = failed.iter().map(|s| (*s).clone()).collect();

        // Sensitive cells get stuck-at-0, others get timing violations
        let primitive_faults: HashMap<String, FaultType> = domain_fault
            .affected_primitives
            .iter()
            .map(|p| {
                let sensitivity = domain_fault
                    .primitive_sensitivities
                    .get(p)
                    .copied()
                    .unwrap_or(5);
                let fault = if failed_primitives.contains(p) {
                    FaultType::StuckAt0 // Hard failure
                } else if sensitivity <= 6 {
                    FaultType::SetupViolation // Timing degradation
                } else {
                    FaultType::Transient // Minor glitch
                };
                (p.clone(), fault)
            })
            .collect();

        Self {
            domain_fault,
            primitives_affected,
            failed_primitives,
            primitive_faults,
            detected: false,
            detecting_signals: Vec::new(),
            detection_latency_cycles: None,
        }
    }
}

/// Configuration for domain fault injection campaigns
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DomainFaultCampaignConfig {
    /// Domain fault types to inject
    pub fault_types: Vec<DomainFaultType>,
    /// Whether to include progressive brownout simulation
    pub include_brownout: bool,
    /// Brownout voltage steps (if include_brownout is true)
    pub brownout_steps_mv: Vec<u16>,
    /// Cycles to simulate after fault injection
    pub cycles_per_fault: u64,
    /// Whether to track individual primitive failures
    pub track_primitive_failures: bool,
}

impl Default for DomainFaultCampaignConfig {
    fn default() -> Self {
        Self {
            fault_types: vec![
                DomainFaultType::PowerFailure,
                DomainFaultType::VoltageDropout {
                    target_voltage_mv: 800,
                },
            ],
            include_brownout: true,
            brownout_steps_mv: vec![950, 900, 850, 800, 750, 700, 650],
            cycles_per_fault: 1000,
            track_primitive_failures: true,
        }
    }
}

/// Results from a domain fault injection campaign
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DomainFaultCampaignResults {
    /// Configuration used
    pub config: DomainFaultCampaignConfig,
    /// Results per domain
    pub domain_results: HashMap<String, Vec<DomainFaultResult>>,
    /// Total domain faults injected
    pub total_domain_faults: usize,
    /// Domains with detection capability
    pub domains_with_detection: usize,
    /// Average detection latency across all detected faults
    pub avg_detection_latency_cycles: f64,
    /// Brownout analysis results (if enabled)
    pub brownout_analysis: Option<BrownoutAnalysis>,
}

/// Analysis of progressive brownout simulation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BrownoutAnalysis {
    /// Per-domain brownout results
    pub domain_brownout: HashMap<String, DomainBrownoutResult>,
    /// Voltage at which first detection occurs (per domain)
    pub first_detection_voltage_mv: HashMap<String, u16>,
    /// Voltage at which safety goal is violated (per domain)
    pub safety_violation_voltage_mv: HashMap<String, u16>,
}

/// Brownout results for a single domain
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DomainBrownoutResult {
    /// Domain name
    pub domain_name: String,
    /// Voltage steps simulated
    pub voltage_steps: Vec<u16>,
    /// Primitives failing at each voltage step
    pub failures_at_voltage: HashMap<u16, Vec<String>>,
    /// Cumulative failure count at each step
    pub cumulative_failures: Vec<(u16, usize)>,
    /// Detection status at each voltage step
    pub detection_at_voltage: HashMap<u16, bool>,
}

impl DomainBrownoutResult {
    /// Create new brownout result for a domain
    pub fn new(domain_name: &str) -> Self {
        Self {
            domain_name: domain_name.to_string(),
            voltage_steps: Vec::new(),
            failures_at_voltage: HashMap::new(),
            cumulative_failures: Vec::new(),
            detection_at_voltage: HashMap::new(),
        }
    }

    /// Add a voltage step result
    pub fn add_step(&mut self, voltage_mv: u16, failures: Vec<String>, detected: bool) {
        self.voltage_steps.push(voltage_mv);
        let cumulative = self
            .cumulative_failures
            .last()
            .map(|(_, c)| *c)
            .unwrap_or(0)
            + failures.len();
        self.failures_at_voltage.insert(voltage_mv, failures);
        self.cumulative_failures.push((voltage_mv, cumulative));
        self.detection_at_voltage.insert(voltage_mv, detected);
    }

    /// Get the voltage at which detection first occurs
    pub fn first_detection_voltage(&self) -> Option<u16> {
        self.voltage_steps
            .iter()
            .rev() // Start from lowest voltage (last step)
            .find(|v| self.detection_at_voltage.get(*v).copied().unwrap_or(false))
            .copied()
    }

    /// Get primitives that fail at or above a given voltage
    pub fn primitives_failing_above(&self, threshold_mv: u16) -> Vec<String> {
        self.failures_at_voltage
            .iter()
            .filter(|(v, _)| **v >= threshold_mv)
            .flat_map(|(_, prims)| prims.iter().cloned())
            .collect()
    }
}

/// Generate domain fault injection sites from LIR
///
/// This function analyzes the LIR to identify power domains and creates
/// domain fault sites for each domain with appropriate voltage sensitivities.
pub fn generate_domain_fault_sites(
    lir: &skalp_lir::Lir,
    _tech_library: Option<&crate::fmeda_library::TechLibrary>,
) -> Vec<DomainFaultSite> {
    let mut sites = Vec::new();
    let mut domain_primitives: HashMap<String, Vec<(String, u8)>> = HashMap::new();
    let mut domain_voltages: HashMap<String, u16> = HashMap::new();

    // Collect primitives by power domain
    for prim in &lir.primitives {
        let domain_name = prim
            .power_domain
            .clone()
            .unwrap_or_else(|| "vdd_default".to_string());

        // Determine voltage sensitivity based on primitive type
        let sensitivity = match &prim.ptype {
            // Sequential cells - more sensitive
            skalp_lir::lir::PrimitiveType::DffP
            | skalp_lir::lir::PrimitiveType::DffN
            | skalp_lir::lir::PrimitiveType::DffNeg
            | skalp_lir::lir::PrimitiveType::DffE
            | skalp_lir::lir::PrimitiveType::DffAR
            | skalp_lir::lir::PrimitiveType::DffAS
            | skalp_lir::lir::PrimitiveType::DffScan
            | skalp_lir::lir::PrimitiveType::Dlatch
            | skalp_lir::lir::PrimitiveType::SRlatch
            | skalp_lir::lir::PrimitiveType::MemCell
            | skalp_lir::lir::PrimitiveType::RegCell => 3, // Sequential - sensitive

            // Power infrastructure - most robust
            skalp_lir::lir::PrimitiveType::PowerSwitch { .. } => 9, // Very robust
            skalp_lir::lir::PrimitiveType::AlwaysOnBuf => 9,        // Very robust

            // Isolation cells - designed to be robust
            skalp_lir::lir::PrimitiveType::IsolationCell { .. } => 8, // Robust

            // Level shifters - moderately sensitive
            skalp_lir::lir::PrimitiveType::LevelShifter { .. } => 4, // Sensitive

            // Retention cells - very sensitive due to balloon latch
            skalp_lir::lir::PrimitiveType::RetentionDff { .. } => 2, // Very sensitive

            // Combinational - medium sensitivity
            _ => 6,
        };

        // Infer domain voltage from name
        let voltage_mv = infer_domain_voltage(&domain_name);
        domain_voltages.insert(domain_name.clone(), voltage_mv);

        domain_primitives
            .entry(domain_name)
            .or_default()
            .push((prim.path.clone(), sensitivity));
    }

    // Create fault sites for each domain with all fault types
    for (domain_name, primitives) in domain_primitives {
        let voltage_mv = domain_voltages.get(&domain_name).copied().unwrap_or(1000);

        // Power failure
        let mut site =
            DomainFaultSite::new(&domain_name, voltage_mv, DomainFaultType::PowerFailure);
        site = site.with_primitives(&primitives);
        sites.push(site);

        // Voltage dropout at various levels
        for target_mv in [900u16, 800, 750, 700].iter() {
            if *target_mv < voltage_mv {
                let mut site = DomainFaultSite::new(
                    &domain_name,
                    voltage_mv,
                    DomainFaultType::VoltageDropout {
                        target_voltage_mv: *target_mv,
                    },
                );
                site = site.with_primitives(&primitives);
                sites.push(site);
            }
        }

        // Progressive brownout
        let mut site = DomainFaultSite::new(
            &domain_name,
            voltage_mv,
            DomainFaultType::Brownout {
                start_voltage_mv: voltage_mv,
                end_voltage_mv: 600,
                droop_rate_mv_per_cycle: 10,
            },
        );
        site = site.with_primitives(&primitives);
        sites.push(site);

        // Ground bounce
        let mut site = DomainFaultSite::new(
            &domain_name,
            voltage_mv,
            DomainFaultType::GroundBounce {
                magnitude_mv: 200,
                duration_cycles: 5,
            },
        );
        site = site.with_primitives(&primitives);
        sites.push(site);
    }

    sites
}

/// Infer domain voltage from domain name patterns
fn infer_domain_voltage(domain_name: &str) -> u16 {
    let name_lower = domain_name.to_lowercase();

    // Check for explicit voltage in name
    if name_lower.contains("3v3") || name_lower.contains("3.3v") {
        return 3300;
    }
    if name_lower.contains("1v8") || name_lower.contains("1.8v") {
        return 1800;
    }
    if name_lower.contains("1v2") || name_lower.contains("1.2v") {
        return 1200;
    }
    if name_lower.contains("1v0") || name_lower.contains("1.0v") || name_lower.contains("1v") {
        return 1000;
    }
    if name_lower.contains("0v9") || name_lower.contains("0.9v") {
        return 900;
    }
    if name_lower.contains("5v") {
        return 5000;
    }

    // Infer from domain type
    if name_lower.contains("io") || name_lower.contains("pad") {
        return 3300; // I/O typically 3.3V
    }
    if name_lower.contains("core") || name_lower.contains("logic") {
        return 1000; // Core typically 1.0V
    }
    if name_lower.contains("analog") {
        return 1800; // Analog often 1.8V
    }
    if name_lower.contains("mem") || name_lower.contains("sram") {
        return 1000; // Memory typically same as core
    }

    // Default to 1.0V core voltage
    1000
}

/// Convert domain fault results to standard fault simulation results
///
/// This allows domain faults to be analyzed using the existing FMEA framework.
pub fn domain_fault_to_standard_faults(domain_result: &DomainFaultResult) -> Vec<FaultSite> {
    domain_result
        .primitive_faults
        .iter()
        .map(|(path, fault_type)| FaultSite::new(DesignRef::parse(path), *fault_type))
        .collect()
}

/// Simulate brownout for a domain and return analysis results
pub fn simulate_domain_brownout(
    domain_fault: &DomainFaultSite,
    voltage_steps: &[u16],
) -> DomainBrownoutResult {
    let mut result = DomainBrownoutResult::new(&domain_fault.domain_name);

    for &voltage_mv in voltage_steps {
        let failing = domain_fault.primitives_failing_at_voltage(voltage_mv);
        let failures: Vec<String> = failing.iter().map(|s| (*s).clone()).collect();

        // In real implementation, this would run simulation to check detection
        // For now, assume detection if > 50% of cells fail
        let detected = failures.len() > domain_fault.affected_primitives.len() / 2;

        result.add_step(voltage_mv, failures, detected);
    }

    result
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fault_type_names() {
        assert_eq!(FaultType::StuckAt0.name(), "stuck_at_0");
        assert_eq!(FaultType::StuckAt1.name(), "stuck_at_1");
        assert_eq!(FaultType::Transient.name(), "transient");
    }

    #[test]
    fn test_fault_site_id() {
        let path = DesignRef::parse("top.cpu.alu::adder");
        let site = FaultSite::new(path, FaultType::StuckAt0);
        assert_eq!(site.id(), "top.cpu.alu::adder@stuck_at_0");

        let site_bit = site.clone().with_bit(7);
        assert!(site_bit.id().contains("[7]"));
    }

    #[test]
    fn test_effect_condition_builder() {
        let signal = DesignRef::parse("top.brake::valve_cmd");

        // Simple equality
        let cond1 = EffectCondition::equals(signal.clone(), 0xFFFF);
        assert!(matches!(cond1, EffectCondition::Term(_)));

        // Compound AND
        let cond2 = cond1.and(EffectCondition::equals(signal.clone(), 0));
        assert!(matches!(cond2, EffectCondition::And(_)));

        // Stuck condition
        let stuck = EffectCondition::is_stuck(signal.clone(), 100);
        assert!(matches!(stuck, EffectCondition::Temporal(_, _)));
    }

    #[test]
    fn test_safety_goal_sim_spec() {
        let spec = SafetyGoalSimSpec::new("BrakingSafety", AsilLevel::D);

        // ASIL-D should have 99% target for S3
        assert_eq!(spec.target_dc(Severity::S3), 0.99);
        assert_eq!(spec.target_dc(Severity::S2), 0.97);
    }

    #[test]
    fn test_effect_analysis_dc_calculation() {
        let mut analysis = EffectAnalysis::new("valve_corrupted", Severity::S3, 0.99);

        analysis.total_faults_causing = 10000;
        analysis.faults_detected = 9900;
        analysis.update_dc();

        assert!((analysis.measured_dc - 0.99).abs() < 0.001);
        assert!(analysis.meets_target);
    }

    #[test]
    fn test_effect_analysis_gap() {
        let mut analysis = EffectAnalysis::new("silent_failure", Severity::S3, 0.99);

        analysis.total_faults_causing = 10000;
        analysis.faults_detected = 8700; // 87% DC, below 99% target
        analysis.update_dc();

        assert!(!analysis.meets_target);
        assert!((analysis.dc_gap() - 0.12).abs() < 0.001);
        assert_eq!(analysis.faults_needed_for_target(), 1200);
    }

    #[test]
    fn test_simulation_campaign_config() {
        let quick = SimulationCampaignConfig::quick_estimate();
        assert_eq!(quick.sampling_ratio, Some(0.01));
        assert_eq!(quick.max_faults, Some(10_000));

        let exhaustive = SimulationCampaignConfig::exhaustive();
        assert!(exhaustive.sampling_ratio.is_none());
        assert!(exhaustive.max_faults.is_none());
    }

    #[test]
    fn test_fault_simulation_result() {
        let site = FaultSite::new(DesignRef::parse("top.cpu::reg"), FaultType::StuckAt0);

        // Dangerous undetected fault
        let dangerous = FaultSimulationResult {
            fault_site: site.clone(),
            scenario_name: "test1".to_string(),
            caused_effect: Some("silent_failure".to_string()),
            detected: false,
            detected_by: None,
            injection_cycle: 100,
            effect_cycle: Some(105),
            detection_cycle: None,
            detection_latency: None,
        };
        assert!(dangerous.is_dangerous_undetected());
        assert!(!dangerous.is_safe());

        // Safe detected fault
        let safe = FaultSimulationResult {
            fault_site: site.clone(),
            scenario_name: "test1".to_string(),
            caused_effect: Some("valve_corrupted".to_string()),
            detected: true,
            detected_by: Some("TmrVoter".to_string()),
            injection_cycle: 100,
            effect_cycle: Some(110),
            detection_cycle: Some(102),
            detection_latency: Some(2),
        };
        assert!(!safe.is_dangerous_undetected());
        assert!(safe.is_safe());
    }

    #[test]
    fn test_safety_analysis_error_format() {
        let error = SafetyAnalysisError::dc_not_met(
            "BrakingSafety",
            "valve_corrupted",
            0.873,
            0.99,
            AsilLevel::D,
        )
        .with_gap(GapDetail {
            effect: "valve_corrupted".to_string(),
            measured_dc: 0.873,
            target_dc: 0.99,
            undetected_locations: vec!["top.brake.datapath.alu.*".to_string()],
            primitive_count: 312,
        })
        .with_suggestion("Add safety mechanism to datapath output");

        let formatted = error.format_error();
        assert!(formatted.contains("error[SAFETY001]"));
        assert!(formatted.contains("ASIL-D requirements not met"));
        assert!(formatted.contains("87.3%"));
        assert!(formatted.contains("99.0%"));
        assert!(formatted.contains("top.brake.datapath.alu"));
    }

    #[test]
    fn test_campaign_results_metrics() {
        let mut results = SimulationCampaignResults::new("BrakingSafety", "top.brake");

        // Add effect analyses
        let mut effect1 = EffectAnalysis::new("valve_corrupted", Severity::S3, 0.99);
        effect1.total_faults_causing = 10000;
        effect1.faults_detected = 9920;
        effect1.update_dc();
        results
            .effect_analyses
            .insert("valve_corrupted".to_string(), effect1);

        let mut effect2 = EffectAnalysis::new("silent_failure", Severity::S3, 0.99);
        effect2.total_faults_causing = 500;
        effect2.faults_detected = 450;
        effect2.update_dc();
        results
            .effect_analyses
            .insert("silent_failure".to_string(), effect2);

        results.update_metrics();

        // SPFM should be (9920 + 450) / (10000 + 500) ≈ 98.86%
        assert!(results.spfm > 0.98);

        // Should not meet ASIL because silent_failure has 90% < 99% target
        assert!(!results.meets_asil);
        assert!(!results.gaps.is_empty());
    }

    #[test]
    fn test_temporal_operator_builders() {
        // Edge detection operators
        let rose = EffectCondition::rose("clk");
        assert!(matches!(
            rose,
            EffectCondition::Operator(TemporalOperator::Rose(_))
        ));

        let fell = EffectCondition::fell("reset");
        assert!(matches!(
            fell,
            EffectCondition::Operator(TemporalOperator::Fell(_))
        ));

        let changed = EffectCondition::changed("state");
        assert!(matches!(
            changed,
            EffectCondition::Operator(TemporalOperator::Changed(_))
        ));

        let stable = EffectCondition::stable("fsm_state", 100);
        assert!(matches!(
            stable,
            EffectCondition::Operator(TemporalOperator::Stable(_, 100))
        ));
    }

    #[test]
    fn test_range_operators() {
        let in_range = EffectCondition::in_range("voltage", 1000, 1200);
        assert!(matches!(
            in_range,
            EffectCondition::Operator(TemporalOperator::InRange(_, 1000, 1200))
        ));

        let outside = EffectCondition::outside_range("temp", 0, 85);
        assert!(matches!(
            outside,
            EffectCondition::Operator(TemporalOperator::OutsideRange(_, 0, 85))
        ));

        let valid_states = vec![0, 1, 2, 3];
        let oneof = EffectCondition::oneof("state", valid_states);
        assert!(matches!(
            oneof,
            EffectCondition::Operator(TemporalOperator::OneOf(_, _))
        ));

        let invalid_states = vec![10, 11, 12];
        let not_oneof = EffectCondition::not_oneof("error_code", invalid_states);
        assert!(matches!(
            not_oneof,
            EffectCondition::Operator(TemporalOperator::NotOneOf(_, _))
        ));
    }

    #[test]
    fn test_comparison_operators() {
        let abs_diff = EffectCondition::abs_diff_exceeds("sensor_a", "sensor_b", 50);
        assert!(matches!(
            abs_diff,
            EffectCondition::OperatorCompare(
                TemporalOperator::AbsDiff(_, _),
                CompareOp::GreaterThan,
                50
            )
        ));

        let max_dev = EffectCondition::max_deviation_exceeds(vec!["s1", "s2", "s3"], 10);
        assert!(matches!(
            max_dev,
            EffectCondition::OperatorCompare(
                TemporalOperator::MaxDeviation(_),
                CompareOp::GreaterThan,
                10
            )
        ));
    }

    #[test]
    fn test_pulse_count_operators() {
        let pulse_low = EffectCondition::pulse_count_below("heartbeat", 1000, 5);
        assert!(matches!(
            pulse_low,
            EffectCondition::OperatorCompare(
                TemporalOperator::PulseCount(_, 1000),
                CompareOp::LessThan,
                5
            )
        ));

        let pulse_high = EffectCondition::pulse_count_above("irq", 100, 50);
        assert!(matches!(
            pulse_high,
            EffectCondition::OperatorCompare(
                TemporalOperator::PulseCount(_, 100),
                CompareOp::GreaterThan,
                50
            )
        ));
    }

    #[test]
    fn test_compound_temporal_conditions() {
        // Complex condition: @stable(fsm_state, 100) && !@oneof(fsm_state, [0, 15])
        let fsm_stuck_invalid = EffectCondition::stable("fsm_state", 100)
            .and(EffectCondition::not_oneof("fsm_state", vec![0, 15]));

        assert!(matches!(fsm_stuck_invalid, EffectCondition::And(_)));

        // Timeout condition: @cycles_since(@rose(request)) > 1000
        let timeout = EffectCondition::timeout_since(EffectCondition::rose("request"), 1000);
        assert!(matches!(
            timeout,
            EffectCondition::OperatorCompare(
                TemporalOperator::CyclesSince(_),
                CompareOp::GreaterThan,
                1000
            )
        ));
    }

    #[test]
    fn test_watchdog_pattern() {
        // Typical watchdog failure patterns:
        // 1. No kicks for too long
        let no_kicks = EffectCondition::pulse_count_below("kick", 5000, 3);

        // 2. Kicks too frequent (runaway)
        let kicks_too_fast = EffectCondition::pulse_count_above("kick", 100, 10);

        // 3. Timeout without effect
        let timeout_ignored =
            EffectCondition::rose("timeout").and(EffectCondition::stable("cpu_alive", 100));

        assert!(matches!(
            no_kicks,
            EffectCondition::OperatorCompare(_, _, _)
        ));
        assert!(matches!(
            kicks_too_fast,
            EffectCondition::OperatorCompare(_, _, _)
        ));
        assert!(matches!(timeout_ignored, EffectCondition::And(_)));
    }

    #[test]
    fn test_redundancy_pattern() {
        // Triple-modular redundancy failure patterns:
        // Channels should agree within tolerance
        let sensor_disagree =
            EffectCondition::max_deviation_exceeds(vec!["sensor_a", "sensor_b", "sensor_c"], 50);

        // Pairwise comparison
        let a_b_differ = EffectCondition::abs_diff_exceeds("sensor_a", "sensor_b", 100);
        let b_c_differ = EffectCondition::abs_diff_exceeds("sensor_b", "sensor_c", 100);
        let total_disagree = a_b_differ.and(b_c_differ);

        assert!(matches!(
            sensor_disagree,
            EffectCondition::OperatorCompare(_, _, _)
        ));
        assert!(matches!(total_disagree, EffectCondition::And(_)));
    }

    // ========================================================================
    // Extended Fault Type Tests
    // ========================================================================

    #[test]
    fn test_timing_violation_fault_types() {
        // Setup violation
        assert_eq!(FaultType::SetupViolation.name(), "setup_violation");
        assert_eq!(FaultType::SetupViolation.category(), FaultCategory::Timing);

        // Hold violation
        assert_eq!(FaultType::HoldViolation.name(), "hold_violation");
        assert_eq!(FaultType::HoldViolation.category(), FaultCategory::Timing);

        // Metastability
        assert_eq!(FaultType::Metastability.name(), "metastability");
        assert_eq!(FaultType::Metastability.category(), FaultCategory::Timing);
    }

    #[test]
    fn test_power_effect_fault_types() {
        // Voltage dropout
        assert_eq!(FaultType::VoltageDropout.name(), "voltage_dropout");
        assert_eq!(FaultType::VoltageDropout.category(), FaultCategory::Power);

        // Ground bounce
        assert_eq!(FaultType::GroundBounce.name(), "ground_bounce");
        assert_eq!(FaultType::GroundBounce.category(), FaultCategory::Power);

        // Crosstalk
        assert_eq!(FaultType::CrosstalkGlitch.name(), "crosstalk_glitch");
        assert_eq!(FaultType::CrosstalkGlitch.category(), FaultCategory::Power);
    }

    #[test]
    fn test_clock_fault_types() {
        assert_eq!(FaultType::ClockGlitch.name(), "clock_glitch");
        assert_eq!(FaultType::ClockGlitch.category(), FaultCategory::Clock);
    }

    #[test]
    fn test_other_fault_types() {
        // Open fault
        assert_eq!(FaultType::Open.name(), "open");
        assert_eq!(FaultType::Open.category(), FaultCategory::Permanent);

        // Multi-bit upset
        assert_eq!(FaultType::MultiBitUpset.name(), "multi_bit_upset");
        assert_eq!(
            FaultType::MultiBitUpset.category(),
            FaultCategory::Transient
        );

        // Bridging fault
        assert_eq!(FaultType::Bridging.name(), "bridging");
        assert_eq!(FaultType::Bridging.category(), FaultCategory::Permanent);
    }

    // ========================================================================
    // Fault Category Tests
    // ========================================================================

    #[test]
    fn test_fault_category_names() {
        assert_eq!(FaultCategory::Permanent.name(), "Permanent");
        assert_eq!(FaultCategory::Transient.name(), "Transient");
        assert_eq!(FaultCategory::Timing.name(), "Timing");
        assert_eq!(FaultCategory::Power.name(), "Power");
        assert_eq!(FaultCategory::Clock.name(), "Clock");
    }

    #[test]
    fn test_fault_category_descriptions() {
        assert!(FaultCategory::Permanent
            .description()
            .contains("Manufacturing"));
        assert!(FaultCategory::Transient.description().contains("Radiation"));
        assert!(FaultCategory::Timing.description().contains("Setup/hold"));
        assert!(FaultCategory::Power.description().contains("Voltage"));
        assert!(FaultCategory::Clock.description().contains("Clock"));
    }

    // ========================================================================
    // Fault Set Tests
    // ========================================================================

    #[test]
    fn test_minimal_fault_set() {
        let set = FaultType::minimal_set();
        assert_eq!(set.len(), 2);
        assert!(set.contains(&FaultType::StuckAt0));
        assert!(set.contains(&FaultType::StuckAt1));
    }

    #[test]
    fn test_standard_fault_set() {
        let set = FaultType::standard_set();
        assert_eq!(set.len(), 3);
        assert!(set.contains(&FaultType::StuckAt0));
        assert!(set.contains(&FaultType::StuckAt1));
        assert!(set.contains(&FaultType::Transient));
    }

    #[test]
    fn test_timing_aware_fault_set() {
        let set = FaultType::timing_aware_set();
        assert_eq!(set.len(), 5);
        assert!(set.contains(&FaultType::StuckAt0));
        assert!(set.contains(&FaultType::StuckAt1));
        assert!(set.contains(&FaultType::Transient));
        assert!(set.contains(&FaultType::SetupViolation));
        assert!(set.contains(&FaultType::HoldViolation));
    }

    #[test]
    fn test_extended_fault_set() {
        let set = FaultType::extended_set();
        // Should include permanent, transient, timing, and power faults
        assert!(set.len() >= 10);
        // Check key fault types
        assert!(set.contains(&FaultType::StuckAt0));
        assert!(set.contains(&FaultType::Open));
        assert!(set.contains(&FaultType::Transient));
        assert!(set.contains(&FaultType::MultiBitUpset));
        assert!(set.contains(&FaultType::SetupViolation));
        assert!(set.contains(&FaultType::Metastability));
        assert!(set.contains(&FaultType::VoltageDropout));
        assert!(set.contains(&FaultType::GroundBounce));
    }

    #[test]
    fn test_asil_d_fault_set() {
        let set = FaultType::asil_d_set();
        // ASIL-D requires comprehensive coverage
        assert!(set.len() >= 12);
        // Must include bridging (required for ASIL-D)
        assert!(set.contains(&FaultType::Bridging));
        // Must include timing violations
        assert!(set.contains(&FaultType::SetupViolation));
        assert!(set.contains(&FaultType::HoldViolation));
        assert!(set.contains(&FaultType::Metastability));
        // Must include power effects
        assert!(set.contains(&FaultType::VoltageDropout));
        assert!(set.contains(&FaultType::GroundBounce));
    }

    // ========================================================================
    // Power Region Tests
    // ========================================================================

    #[test]
    fn test_power_region_creation() {
        let region = PowerRegion::new("core_vdd", "top.cpu.*");
        assert_eq!(region.name, "core_vdd");
        assert_eq!(region.primitive_pattern, "top.cpu.*");
        assert!(region.description.is_none());
        assert!(region.explicit_primitives.is_empty());
    }

    #[test]
    fn test_power_region_with_description() {
        let region =
            PowerRegion::new("io_vdd", "top.io.*").with_description("I/O ring power domain");
        assert_eq!(region.name, "io_vdd");
        assert_eq!(
            region.description,
            Some("I/O ring power domain".to_string())
        );
    }

    #[test]
    fn test_power_region_with_primitives() {
        let region = PowerRegion::new("analog_vdd", "top.adc.*")
            .with_primitive(DesignRef::parse("top.adc::dac_core"))
            .with_primitive(DesignRef::parse("top.adc::comparator"));

        assert_eq!(region.explicit_primitives.len(), 2);
        assert_eq!(
            region.explicit_primitives[0].to_string(),
            "top.adc::dac_core"
        );
    }

    // ========================================================================
    // Fault Site with Extended Types Tests
    // ========================================================================

    #[test]
    fn test_fault_site_timing_violation() {
        let path = DesignRef::parse("top.cpu.reg_file::ff_0");
        let site = FaultSite::new(path, FaultType::SetupViolation);
        assert_eq!(site.id(), "top.cpu.reg_file::ff_0@setup_violation");
    }

    #[test]
    fn test_fault_site_power_effect() {
        let path = DesignRef::parse("top.mem.bank0::sram_cell");
        let site = FaultSite::new(path.clone(), FaultType::VoltageDropout);
        assert_eq!(site.id(), "top.mem.bank0::sram_cell@voltage_dropout");

        let site2 = FaultSite::new(path, FaultType::GroundBounce);
        assert_eq!(site2.id(), "top.mem.bank0::sram_cell@ground_bounce");
    }

    #[test]
    fn test_fault_site_metastability_with_bit() {
        let path = DesignRef::parse("top.sync::synchronizer_ff");
        let site = FaultSite::new(path, FaultType::Metastability).with_bit(3);
        assert_eq!(site.id(), "top.sync::synchronizer_ff@metastability[3]");
    }

    // ========================================================================
    // Category-based Fault Analysis Tests
    // ========================================================================

    #[test]
    fn test_fault_type_category_mapping() {
        // Verify all fault types have correct categories
        let permanent_faults = vec![
            FaultType::StuckAt0,
            FaultType::StuckAt1,
            FaultType::Bridging,
            FaultType::Open,
        ];
        for ft in permanent_faults {
            assert_eq!(ft.category(), FaultCategory::Permanent, "{:?}", ft);
        }

        let transient_faults = vec![FaultType::Transient, FaultType::MultiBitUpset];
        for ft in transient_faults {
            assert_eq!(ft.category(), FaultCategory::Transient, "{:?}", ft);
        }

        let timing_faults = vec![
            FaultType::SetupViolation,
            FaultType::HoldViolation,
            FaultType::Metastability,
        ];
        for ft in timing_faults {
            assert_eq!(ft.category(), FaultCategory::Timing, "{:?}", ft);
        }

        let power_faults = vec![
            FaultType::VoltageDropout,
            FaultType::GroundBounce,
            FaultType::CrosstalkGlitch,
        ];
        for ft in power_faults {
            assert_eq!(ft.category(), FaultCategory::Power, "{:?}", ft);
        }

        assert_eq!(FaultType::ClockGlitch.category(), FaultCategory::Clock);
    }

    // ========================================================================
    // Domain Fault Injection Tests
    // ========================================================================

    #[test]
    fn test_domain_fault_type_names() {
        assert_eq!(DomainFaultType::PowerFailure.name(), "power_failure");
        assert_eq!(
            DomainFaultType::VoltageDropout {
                target_voltage_mv: 800
            }
            .name(),
            "voltage_dropout"
        );
        assert_eq!(
            DomainFaultType::Brownout {
                start_voltage_mv: 1000,
                end_voltage_mv: 600,
                droop_rate_mv_per_cycle: 10
            }
            .name(),
            "brownout"
        );
        assert_eq!(
            DomainFaultType::GroundBounce {
                magnitude_mv: 200,
                duration_cycles: 5
            }
            .name(),
            "ground_bounce"
        );
        assert_eq!(DomainFaultType::PowerOnResetFailure.name(), "por_failure");
    }

    #[test]
    fn test_domain_fault_type_descriptions() {
        let desc = DomainFaultType::PowerFailure.description();
        assert!(desc.contains("power failure"));

        let desc = DomainFaultType::VoltageDropout {
            target_voltage_mv: 800,
        }
        .description();
        assert!(desc.contains("800mV"));

        let desc = DomainFaultType::Brownout {
            start_voltage_mv: 1000,
            end_voltage_mv: 600,
            droop_rate_mv_per_cycle: 10,
        }
        .description();
        assert!(desc.contains("1000mV"));
        assert!(desc.contains("600mV"));
    }

    #[test]
    fn test_domain_fault_asil_d_set() {
        let set = DomainFaultType::asil_d_set();
        assert!(set.len() >= 5); // At least power failure, dropouts, brownout, ground bounce, POR
        assert!(set.contains(&DomainFaultType::PowerFailure));
        assert!(set.contains(&DomainFaultType::PowerOnResetFailure));
    }

    #[test]
    fn test_domain_fault_site_creation() {
        let site = DomainFaultSite::new("vdd_core", 1000, DomainFaultType::PowerFailure)
            .with_primitive("top.core.cpu.alu", 6)
            .with_primitive("top.core.cpu.reg_file", 3);

        assert_eq!(site.domain_name, "vdd_core");
        assert_eq!(site.domain_voltage_mv, 1000);
        assert_eq!(site.affected_primitives.len(), 2);
        assert_eq!(
            site.primitive_sensitivities.get("top.core.cpu.alu"),
            Some(&6)
        );
        assert_eq!(
            site.primitive_sensitivities.get("top.core.cpu.reg_file"),
            Some(&3)
        );
    }

    #[test]
    fn test_domain_fault_site_id() {
        let site = DomainFaultSite::new(
            "vdd_io",
            3300,
            DomainFaultType::VoltageDropout {
                target_voltage_mv: 2800,
            },
        );
        assert_eq!(site.id(), "domain:vdd_io@voltage_dropout");
    }

    #[test]
    fn test_primitives_by_sensitivity() {
        let site = DomainFaultSite::new("vdd_core", 1000, DomainFaultType::PowerFailure)
            .with_primitive("robust_cell", 9) // Most robust
            .with_primitive("sensitive_cell", 2) // Most sensitive
            .with_primitive("medium_cell", 5); // Medium

        let sorted = site.primitives_by_sensitivity();
        assert_eq!(sorted[0], "sensitive_cell"); // Sensitivity 2 first
        assert_eq!(sorted[1], "medium_cell"); // Sensitivity 5 second
        assert_eq!(sorted[2], "robust_cell"); // Sensitivity 9 last
    }

    #[test]
    fn test_primitives_failing_at_voltage() {
        let site = DomainFaultSite::new("vdd_core", 1000, DomainFaultType::PowerFailure)
            .with_primitive("retention_ff", 2) // Sens 1-2: Fails at <= 900mV
            .with_primitive("dff", 3) // Sens 3-4: Fails at <= 800mV
            .with_primitive("combo", 6) // Sens 5-6: Fails at <= 750mV
            .with_primitive("isolation", 8) // Sens 7-8: Fails at <= 700mV
            .with_primitive("power_switch", 10); // Sens 9-10: Fails at <= 650mV

        // At 900mV - only most sensitive cells fail (sens 1-2)
        let failing_900 = site.primitives_failing_at_voltage(900);
        assert_eq!(failing_900.len(), 1); // Only retention_ff
        assert!(failing_900.iter().any(|p| *p == "retention_ff"));

        // At 800mV - sens 1-4 cells fail
        let failing_800 = site.primitives_failing_at_voltage(800);
        assert_eq!(failing_800.len(), 2); // retention_ff and dff

        // At 700mV - sens 1-8 cells fail
        let failing_700 = site.primitives_failing_at_voltage(700);
        assert_eq!(failing_700.len(), 4); // All except power_switch

        // At 650mV - all cells fail
        let failing_650 = site.primitives_failing_at_voltage(650);
        assert_eq!(failing_650.len(), 5); // All cells
    }

    #[test]
    fn test_domain_fault_result_power_failure() {
        let site = DomainFaultSite::new("vdd_core", 1000, DomainFaultType::PowerFailure)
            .with_primitive("cell1", 5)
            .with_primitive("cell2", 5);

        let result = DomainFaultResult::power_failure(site);
        assert_eq!(result.primitives_affected, 2);
        assert_eq!(result.failed_primitives.len(), 2);
        assert_eq!(result.primitive_faults.len(), 2);
        assert_eq!(
            result.primitive_faults.get("cell1"),
            Some(&FaultType::StuckAt0)
        );
        assert_eq!(
            result.primitive_faults.get("cell2"),
            Some(&FaultType::StuckAt0)
        );
    }

    #[test]
    fn test_domain_fault_result_voltage_dropout() {
        let site = DomainFaultSite::new("vdd_core", 1000, DomainFaultType::PowerFailure)
            .with_primitive("sensitive", 2) // Will fail at 900mV
            .with_primitive("robust", 9); // Won't fail at 850mV

        let result = DomainFaultResult::voltage_dropout(site, 850);
        assert_eq!(result.failed_primitives.len(), 1);
        assert!(result.failed_primitives.contains(&"sensitive".to_string()));
        assert_eq!(
            result.primitive_faults.get("sensitive"),
            Some(&FaultType::StuckAt0)
        );
    }

    #[test]
    fn test_infer_domain_voltage() {
        assert_eq!(infer_domain_voltage("vdd_core_1v0"), 1000);
        assert_eq!(infer_domain_voltage("vdd_io_3v3"), 3300);
        assert_eq!(infer_domain_voltage("vdd_io"), 3300);
        assert_eq!(infer_domain_voltage("vdd_core"), 1000);
        assert_eq!(infer_domain_voltage("vdd_analog_1v8"), 1800);
        assert_eq!(infer_domain_voltage("vdd_default"), 1000);
    }

    #[test]
    fn test_domain_brownout_result() {
        let mut result = DomainBrownoutResult::new("vdd_core");

        result.add_step(950, vec![], false);
        result.add_step(900, vec!["cell1".to_string()], false);
        result.add_step(850, vec!["cell2".to_string()], true);
        result.add_step(800, vec!["cell3".to_string()], true);

        assert_eq!(result.voltage_steps.len(), 4);
        assert_eq!(result.cumulative_failures.last(), Some(&(800, 3)));

        // Detection first occurs at 850mV (iterating from low to high)
        let first_detect = result.first_detection_voltage();
        assert_eq!(first_detect, Some(800)); // Lowest voltage where detected
    }

    #[test]
    fn test_simulate_domain_brownout() {
        let site = DomainFaultSite::new("vdd_core", 1000, DomainFaultType::PowerFailure)
            .with_primitive("sensitive", 2)
            .with_primitive("medium", 5)
            .with_primitive("robust", 9);

        let result = simulate_domain_brownout(&site, &[950, 900, 850, 800, 750, 700, 650]);

        assert_eq!(result.voltage_steps.len(), 7);
        // At 900mV, only sensitive (sens=2) fails
        assert_eq!(
            result.failures_at_voltage.get(&900).map(|v| v.len()),
            Some(1)
        );
        // At 650mV, all cells should fail
        let failures_650 = result.failures_at_voltage.get(&650).unwrap();
        assert_eq!(failures_650.len(), 3);
    }

    #[test]
    fn test_domain_fault_campaign_config_default() {
        let config = DomainFaultCampaignConfig::default();
        assert!(config.include_brownout);
        assert!(!config.brownout_steps_mv.is_empty());
        assert!(config.track_primitive_failures);
        assert!(config.cycles_per_fault > 0);
    }
}
