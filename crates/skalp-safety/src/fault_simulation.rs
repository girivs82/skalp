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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FaultType {
    /// Output stuck at logic 0
    StuckAt0,
    /// Output stuck at logic 1
    StuckAt1,
    /// Single-bit transient (SET/SEU)
    Transient,
    /// Multi-bit upset (for memory cells)
    MultiBitUpset,
    /// Timing fault (setup/hold violation)
    TimingViolation,
    /// Bridging fault (short between signals)
    Bridging,
    /// Open fault (signal disconnected)
    Open,
}

impl FaultType {
    /// Get all standard fault types for exhaustive analysis
    pub fn standard_set() -> Vec<FaultType> {
        vec![
            FaultType::StuckAt0,
            FaultType::StuckAt1,
            FaultType::Transient,
        ]
    }

    /// Get extended fault types including timing and bridging
    pub fn extended_set() -> Vec<FaultType> {
        vec![
            FaultType::StuckAt0,
            FaultType::StuckAt1,
            FaultType::Transient,
            FaultType::TimingViolation,
            FaultType::Open,
        ]
    }

    /// Human-readable name
    pub fn name(&self) -> &'static str {
        match self {
            FaultType::StuckAt0 => "stuck_at_0",
            FaultType::StuckAt1 => "stuck_at_1",
            FaultType::Transient => "transient",
            FaultType::MultiBitUpset => "multi_bit_upset",
            FaultType::TimingViolation => "timing_violation",
            FaultType::Bridging => "bridging",
            FaultType::Open => "open",
        }
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
        let mut id = format!("{}@{}", self.primitive_path.to_string(), self.fault_type.name());
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

/// Compound condition (AND/OR of terms)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EffectCondition {
    /// Single term
    Term(ConditionTerm),
    /// AND of conditions
    And(Vec<EffectCondition>),
    /// OR of conditions
    Or(Vec<EffectCondition>),
    /// NOT of condition
    Not(Box<EffectCondition>),
    /// Temporal: condition holds for duration
    Temporal(Box<EffectCondition>, TemporalQualifier),
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

    /// NOT a condition
    pub fn not(self) -> Self {
        Self::Not(Box::new(self))
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
            let needed_detected =
                (self.target_dc * self.total_faults_causing as f64).ceil() as u64;
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
        output.push_str(&format!(
            "error[{}]: {}\n",
            self.code, self.message
        ));

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
        let path = DesignRef::from_str("top.cpu.alu::adder");
        let site = FaultSite::new(path, FaultType::StuckAt0);
        assert_eq!(site.id(), "top.cpu.alu::adder@stuck_at_0");

        let site_bit = site.clone().with_bit(7);
        assert!(site_bit.id().contains("[7]"));
    }

    #[test]
    fn test_effect_condition_builder() {
        let signal = DesignRef::from_str("top.brake::valve_cmd");

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
        let site = FaultSite::new(
            DesignRef::from_str("top.cpu::reg"),
            FaultType::StuckAt0,
        );

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
        results.effect_analyses.insert("valve_corrupted".to_string(), effect1);

        let mut effect2 = EffectAnalysis::new("silent_failure", Severity::S3, 0.99);
        effect2.total_faults_causing = 500;
        effect2.faults_detected = 450;
        effect2.update_dc();
        results.effect_analyses.insert("silent_failure".to_string(), effect2);

        results.update_metrics();

        // SPFM should be (9920 + 450) / (10000 + 500) ≈ 98.86%
        assert!(results.spfm > 0.98);

        // Should not meet ASIL because silent_failure has 90% < 99% target
        assert!(!results.meets_asil);
        assert!(!results.gaps.is_empty());
    }
}
