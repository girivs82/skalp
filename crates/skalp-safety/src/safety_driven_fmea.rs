//! Safety-Driven FMEA Generation via Fault Injection
//!
//! This module implements the novel approach where FMEA is generated from
//! fault injection campaigns with effect condition monitoring.
//!
//! # Key Innovation
//!
//! Traditional FMEA is bottom-up with estimated DC from lookup tables.
//! This approach is top-down with **measured** DC from simulation:
//!
//! ```text
//! Safety Goal → Effect Conditions → Fault Injection → Measured FMEA
//!     │                │                  │                │
//!     │                │                  │                └── Actual DC values
//!     │                │                  └── Every primitive tested
//!     │                └── Observable failure definitions
//!     └── SG-001: Prevent unintended torque
//! ```
//!
//! # Double Advantage
//!
//! The same simulation cycle provides:
//! 1. **Failure effect identification** - Which faults cause which safety violations
//! 2. **Measured diagnostic coverage** - Which faults are detected by which mechanisms
//!
//! # Example
//!
//! ```ignore
//! use skalp_safety::safety_driven_fmea::{SafetyDrivenFmeaGenerator, EffectMonitor};
//!
//! // Define safety goal with effect conditions
//! let mut spec = SafetyGoalSimSpec::new("PreventUnintendedTorque", AsilLevel::D);
//! spec.add_effect(FailureEffectDef {
//!     name: "unintended_motor_active".to_string(),
//!     condition: EffectCondition::equals(motor_enable, 1)
//!         .and(EffectCondition::equals(safe_mode, 0)),
//!     severity: Severity::S3,
//!     target_dc: 0.99,
//! });
//!
//! // Run FI-driven FMEA generation
//! let generator = SafetyDrivenFmeaGenerator::new(spec);
//! let fmea = generator.generate_from_netlist(&netlist, &annotations)?;
//!
//! // fmea.entries now contains failure modes with MEASURED DC
//! ```

use crate::asil::AsilLevel;
use crate::fault_simulation::{
    AutoFmeda, AutoFmedaContributor, AutoFmedaDetection, AutoFmedaEntry, AutoFmedaMetadata,
    AutoFmedaSummary, CompareOp, CompareValue, ConditionTerm, EffectAnalysis, EffectCondition,
    FailureEffectDef, FaultSite, FaultType, SafetyGoalSimSpec, SimulationCampaignResults,
    TemporalOperator,
};
use crate::hierarchy::{DesignRef, FailureClass, InstancePath, Severity};
use serde::{Deserialize, Serialize};
use skalp_frontend::hir::DetectionMode;
use std::collections::{HashMap, HashSet};
use std::time::{Duration, Instant};

// ============================================================================
// Effect Monitor - Evaluates conditions during simulation
// ============================================================================

/// Runtime monitor for effect conditions during fault simulation
#[derive(Debug, Clone)]
pub struct EffectMonitor {
    /// Effect definitions to monitor
    effects: Vec<FailureEffectDef>,
    /// Signal history for temporal operators (signal_name -> last N values)
    signal_history: HashMap<String, Vec<u64>>,
    /// History depth (cycles to keep)
    history_depth: usize,
    /// Current cycle
    current_cycle: u64,
}

impl EffectMonitor {
    /// Create a new effect monitor from safety goal spec
    pub fn new(spec: &SafetyGoalSimSpec) -> Self {
        Self {
            effects: spec.failure_effects.clone(),
            signal_history: HashMap::new(),
            history_depth: 1000, // Keep 1000 cycles of history
            current_cycle: 0,
        }
    }

    /// Update signal values at current cycle
    pub fn update_signals(&mut self, signals: &HashMap<String, u64>) {
        for (name, value) in signals {
            let history = self.signal_history.entry(name.clone()).or_default();
            history.push(*value);
            // Trim to history depth
            if history.len() > self.history_depth {
                history.remove(0);
            }
        }
        self.current_cycle += 1;
    }

    /// Check which effects are triggered by current signal state
    pub fn check_effects(&self, signals: &HashMap<String, u64>) -> Vec<String> {
        let mut triggered = Vec::new();
        for effect in &self.effects {
            if self.evaluate_condition(&effect.condition, signals) {
                triggered.push(effect.name.clone());
            }
        }
        triggered
    }

    /// Evaluate an effect condition against current signals
    fn evaluate_condition(
        &self,
        condition: &EffectCondition,
        signals: &HashMap<String, u64>,
    ) -> bool {
        match condition {
            EffectCondition::Term(term) => self.evaluate_term(term, signals),
            EffectCondition::And(conditions) => conditions
                .iter()
                .all(|c| self.evaluate_condition(c, signals)),
            EffectCondition::Or(conditions) => conditions
                .iter()
                .any(|c| self.evaluate_condition(c, signals)),
            EffectCondition::Not(inner) => !self.evaluate_condition(inner, signals),
            EffectCondition::Temporal(inner, qualifier) => {
                // Simplified temporal evaluation
                self.evaluate_condition(inner, signals)
            }
            EffectCondition::Operator(op) => self.evaluate_temporal_operator(op, signals),
            EffectCondition::OperatorCompare(op, cmp, threshold) => {
                let value = self.evaluate_temporal_operator_value(op, signals);
                match cmp {
                    CompareOp::Equal => value == *threshold,
                    CompareOp::NotEqual => value != *threshold,
                    CompareOp::GreaterThan => value > *threshold,
                    CompareOp::GreaterOrEqual => value >= *threshold,
                    CompareOp::LessThan => value < *threshold,
                    CompareOp::LessOrEqual => value <= *threshold,
                }
            }
        }
    }

    /// Evaluate a single condition term
    fn evaluate_term(&self, term: &ConditionTerm, signals: &HashMap<String, u64>) -> bool {
        let signal_name = term.signal.to_string();
        let actual = signals.get(&signal_name).copied().unwrap_or(0);

        let expected = match &term.value {
            CompareValue::Literal(v) => *v,
            CompareValue::Golden(g) => signals.get(g).copied().unwrap_or(0),
            CompareValue::Previous => self
                .signal_history
                .get(&signal_name)
                .and_then(|h| h.iter().rev().nth(1).copied())
                .unwrap_or(0),
            CompareValue::Signal(s) => signals.get(&s.to_string()).copied().unwrap_or(0),
        };

        match term.op {
            CompareOp::Equal => actual == expected,
            CompareOp::NotEqual => actual != expected,
            CompareOp::GreaterThan => actual > expected,
            CompareOp::GreaterOrEqual => actual >= expected,
            CompareOp::LessThan => actual < expected,
            CompareOp::LessOrEqual => actual <= expected,
        }
    }

    /// Evaluate a temporal operator (boolean result)
    fn evaluate_temporal_operator(
        &self,
        op: &TemporalOperator,
        signals: &HashMap<String, u64>,
    ) -> bool {
        match op {
            TemporalOperator::Rose(sig) => {
                let history = self.signal_history.get(sig);
                if let Some(h) = history {
                    if h.len() >= 2 {
                        let prev = h[h.len() - 2];
                        let curr = h[h.len() - 1];
                        return prev == 0 && curr != 0;
                    }
                }
                false
            }
            TemporalOperator::Fell(sig) => {
                let history = self.signal_history.get(sig);
                if let Some(h) = history {
                    if h.len() >= 2 {
                        let prev = h[h.len() - 2];
                        let curr = h[h.len() - 1];
                        return prev != 0 && curr == 0;
                    }
                }
                false
            }
            TemporalOperator::Changed(sig) => {
                let history = self.signal_history.get(sig);
                if let Some(h) = history {
                    if h.len() >= 2 {
                        return h[h.len() - 2] != h[h.len() - 1];
                    }
                }
                false
            }
            TemporalOperator::Stable(sig, cycles) => {
                let history = self.signal_history.get(sig);
                if let Some(h) = history {
                    if h.len() >= *cycles as usize {
                        let recent: Vec<_> = h.iter().rev().take(*cycles as usize).collect();
                        return recent.windows(2).all(|w| w[0] == w[1]);
                    }
                }
                false
            }
            TemporalOperator::InRange(sig, lo, hi) => {
                let val = signals.get(sig).copied().unwrap_or(0);
                val >= *lo && val <= *hi
            }
            TemporalOperator::OutsideRange(sig, lo, hi) => {
                let val = signals.get(sig).copied().unwrap_or(0);
                val < *lo || val > *hi
            }
            TemporalOperator::OneOf(sig, vals) => {
                let val = signals.get(sig).copied().unwrap_or(0);
                vals.contains(&val)
            }
            TemporalOperator::NotOneOf(sig, vals) => {
                let val = signals.get(sig).copied().unwrap_or(0);
                !vals.contains(&val)
            }
            _ => false, // Other operators return boolean false by default
        }
    }

    /// Evaluate a temporal operator that returns a numeric value
    fn evaluate_temporal_operator_value(
        &self,
        op: &TemporalOperator,
        signals: &HashMap<String, u64>,
    ) -> u64 {
        match op {
            TemporalOperator::AbsDiff(a, b) => {
                let va = signals.get(a).copied().unwrap_or(0);
                let vb = signals.get(b).copied().unwrap_or(0);
                va.abs_diff(vb)
            }
            TemporalOperator::MaxDeviation(sigs) => {
                let vals: Vec<u64> = sigs
                    .iter()
                    .map(|s| signals.get(s).copied().unwrap_or(0))
                    .collect();
                let mut max_dev = 0u64;
                for i in 0..vals.len() {
                    for j in (i + 1)..vals.len() {
                        let dev = vals[i].abs_diff(vals[j]);
                        max_dev = max_dev.max(dev);
                    }
                }
                max_dev
            }
            TemporalOperator::PopCount(sig) => {
                signals.get(sig).copied().unwrap_or(0).count_ones() as u64
            }
            TemporalOperator::HammingDistance(a, b) => {
                let va = signals.get(a).copied().unwrap_or(0);
                let vb = signals.get(b).copied().unwrap_or(0);
                (va ^ vb).count_ones() as u64
            }
            TemporalOperator::PulseCount(sig, window) => {
                let history = self.signal_history.get(sig);
                if let Some(h) = history {
                    let n = (*window as usize).min(h.len());
                    let recent: Vec<_> = h.iter().rev().take(n).copied().collect();
                    // Count rising edges
                    recent
                        .windows(2)
                        .filter(|w| w[1] == 0 && w[0] != 0) // Note: reversed order
                        .count() as u64
                } else {
                    0
                }
            }
            TemporalOperator::CyclesSince(_) => {
                // Would need event tracking - simplified to current cycle
                self.current_cycle
            }
            _ => 0,
        }
    }

    /// Get effect severity by name
    pub fn get_effect_severity(&self, effect_name: &str) -> Option<Severity> {
        self.effects
            .iter()
            .find(|e| e.name == effect_name)
            .map(|e| e.severity)
    }

    /// Get target DC by effect name
    pub fn get_target_dc(&self, effect_name: &str) -> f64 {
        self.effects
            .iter()
            .find(|e| e.name == effect_name)
            .map(|e| e.target_dc)
            .unwrap_or(0.99) // Default to ASIL-D requirement
    }
}

// ============================================================================
// FI-Driven FMEA Generation Result
// ============================================================================

/// Result from a single fault injection with effect monitoring
#[derive(Debug, Clone)]
pub struct FaultEffectResult {
    /// The fault site that was injected
    pub fault_site: FaultSite,
    /// Primitive path (cell name in netlist)
    pub primitive_path: String,
    /// Effects triggered by this fault
    pub triggered_effects: Vec<String>,
    /// Was the fault detected by a safety mechanism?
    pub detected: bool,
    /// Which mechanism detected it (if any)
    pub detected_by: Option<String>,
    /// Cycle at which effect was first observed
    pub effect_cycle: Option<u64>,
    /// Cycle at which detection occurred
    pub detection_cycle: Option<u64>,
    /// Is this primitive part of a safety mechanism (from #[implements(...)] annotation)?
    pub is_safety_mechanism: bool,
    /// Detection mode of the signal that detected the fault (Continuous, Boot, Periodic, OnDemand)
    /// Used to calculate separate runtime_dc (Continuous) vs boot_dc (Boot/Periodic/OnDemand)
    pub detection_mode: Option<DetectionMode>,
    /// Is this primitive boot-time-only (e.g., BIST hardware)?
    /// Boot-time-only hardware is inactive during steady-state operation, so its faults
    /// don't contribute to steady-state PMHF. Default is false.
    #[allow(dead_code)]
    pub is_boot_time_only: bool,
}

impl FaultEffectResult {
    /// Check if this fault caused a safety violation without detection
    pub fn is_dangerous_undetected(&self) -> bool {
        !self.triggered_effects.is_empty() && !self.detected
    }

    /// Check if detection occurred before or at effect (safe)
    pub fn is_safe_detection(&self) -> bool {
        if let (Some(det), Some(eff)) = (self.detection_cycle, self.effect_cycle) {
            det <= eff
        } else {
            self.detected
        }
    }
}

/// Complete FI-driven FMEA generation results
#[derive(Debug, Clone)]
pub struct FiDrivenFmeaResult {
    /// Safety goal that was analyzed
    pub goal_name: String,
    /// Design name
    pub design_name: String,
    /// Total primitives in design
    pub total_primitives: usize,
    /// Total fault injections performed
    pub total_injections: usize,
    /// Per-effect analysis with measured DC
    pub effect_analyses: HashMap<String, EffectAnalysis>,
    /// All individual fault results
    pub fault_results: Vec<FaultEffectResult>,
    /// Generated FMEA
    pub auto_fmea: AutoFmeda,
    /// Overall SPFM (measured)
    pub measured_spfm: f64,
    /// Overall LF (measured)
    pub measured_lf: f64,
    /// Overall PMHF (if FIT data available)
    pub measured_pmhf: Option<f64>,
    /// Simulation wall time
    pub wall_time: Duration,
    /// Meets ASIL requirements?
    pub meets_asil: bool,
    /// Runtime DC (continuous detection only) - for SPFM calculation
    pub runtime_dc: Option<f64>,
    /// Boot DC (boot-time detection) - for LFM calculation
    pub boot_dc: Option<f64>,
    /// CCF contribution to PMHF (λDPF_CCF) - from power domains and other CCF sources
    pub ccf_contribution: Option<f64>,
    /// Safety mechanism failure contribution (λSM)
    pub sm_contribution: Option<f64>,
}

impl FiDrivenFmeaResult {
    /// Apply CCF contribution to PMHF calculation
    ///
    /// Per ISO 26262-5 + ISO 26262-9:
    /// PMHF = λSPF + λRF + λSM + λDPF_CCF
    ///
    /// The FI-driven analysis gives us λRF (residual faults).
    /// This method adds λDPF_CCF from CCF analysis (power domains, etc.)
    pub fn apply_ccf_contribution(&mut self, ccf_fit: f64) {
        self.ccf_contribution = Some(ccf_fit);
        self.update_pmhf();
    }

    /// Apply safety mechanism failure contribution to PMHF
    ///
    /// λSM accounts for failures in the safety mechanisms themselves
    pub fn apply_sm_contribution(&mut self, sm_fit: f64) {
        self.sm_contribution = Some(sm_fit);
        self.update_pmhf();
    }

    /// Recalculate PMHF with all contributions
    ///
    /// Full formula: PMHF = λRF + λSM + λDPF_CCF
    /// Where λRF = base_pmhf from FI analysis (residual faults)
    fn update_pmhf(&mut self) {
        if let Some(base_pmhf) = self.measured_pmhf {
            let ccf = self.ccf_contribution.unwrap_or(0.0);
            let sm = self.sm_contribution.unwrap_or(0.0);
            self.measured_pmhf = Some(base_pmhf + ccf + sm);
        }
    }

    /// Get PMHF breakdown for reporting
    pub fn pmhf_breakdown(&self) -> PmhfBreakdown {
        let base = self.measured_pmhf.unwrap_or(0.0)
            - self.ccf_contribution.unwrap_or(0.0)
            - self.sm_contribution.unwrap_or(0.0);
        PmhfBreakdown {
            residual_fit: base,
            ccf_fit: self.ccf_contribution.unwrap_or(0.0),
            sm_fit: self.sm_contribution.unwrap_or(0.0),
            total_pmhf: self.measured_pmhf.unwrap_or(0.0),
        }
    }
}

/// PMHF breakdown for detailed reporting
#[derive(Debug, Clone, Default)]
pub struct PmhfBreakdown {
    /// λRF - Residual faults (undetected dangerous faults)
    pub residual_fit: f64,
    /// λDPF_CCF - Common cause failure contribution
    pub ccf_fit: f64,
    /// λSM - Safety mechanism failure contribution
    pub sm_fit: f64,
    /// Total PMHF = λRF + λDPF_CCF + λSM
    pub total_pmhf: f64,
}

// ============================================================================
// Safety-Driven FMEA Generator
// ============================================================================

/// Configuration for FI-driven FMEA generation
#[derive(Debug, Clone)]
pub struct FiDrivenConfig {
    /// Fault types to inject
    pub fault_types: Vec<FaultType>,
    /// Cycles per fault injection
    pub cycles_per_fault: u64,
    /// Maximum faults to simulate (None = all)
    pub max_faults: Option<usize>,
    /// Random sampling ratio (None = exhaustive)
    pub sampling_ratio: Option<f64>,
    /// Parallel threads for simulation
    pub parallel_threads: usize,
    /// Clock signal name
    pub clock_name: String,
    /// Detection signal patterns (signals matching these are considered detectors)
    pub detection_patterns: Vec<String>,
}

impl Default for FiDrivenConfig {
    fn default() -> Self {
        Self {
            fault_types: vec![FaultType::StuckAt0, FaultType::StuckAt1],
            cycles_per_fault: 100,
            max_faults: None,
            sampling_ratio: None,
            parallel_threads: 4,
            clock_name: "clk".to_string(),
            detection_patterns: vec![
                "fault".to_string(),
                "error".to_string(),
                "detect".to_string(),
                "alarm".to_string(),
                "mismatch".to_string(),
                "timeout".to_string(),
                "disagreement".to_string(),
            ],
        }
    }
}

impl FiDrivenConfig {
    /// Quick config for estimation (1% sampling)
    pub fn quick_estimate() -> Self {
        Self {
            sampling_ratio: Some(0.01),
            max_faults: Some(10_000),
            ..Default::default()
        }
    }

    /// Exhaustive config for certification
    pub fn exhaustive() -> Self {
        Self {
            fault_types: FaultType::asil_d_set(),
            sampling_ratio: None,
            max_faults: None,
            ..Default::default()
        }
    }
}

/// Safety-Driven FMEA Generator
///
/// Generates FMEA from fault injection campaigns with effect condition monitoring.
pub struct SafetyDrivenFmeaGenerator {
    /// Safety goal specification with effect conditions
    spec: SafetyGoalSimSpec,
    /// Configuration
    config: FiDrivenConfig,
    /// Effect monitor instance
    monitor: EffectMonitor,
}

impl SafetyDrivenFmeaGenerator {
    /// Create a new generator from safety goal specification
    pub fn new(spec: SafetyGoalSimSpec, config: FiDrivenConfig) -> Self {
        let monitor = EffectMonitor::new(&spec);
        Self {
            spec,
            config,
            monitor,
        }
    }

    /// Generate FMEA from simulation campaign results
    ///
    /// This is the main entry point when you have already run simulation
    /// and just need to process results into FMEA format.
    pub fn generate_from_campaign_results(
        &self,
        results: &[FaultEffectResult],
        primitive_count: usize,
        design_name: &str,
        total_fit: f64,
    ) -> FiDrivenFmeaResult {
        let start = Instant::now();

        // Initialize effect analyses from spec
        let mut effect_analyses: HashMap<String, EffectAnalysis> = HashMap::new();
        for effect in &self.spec.failure_effects {
            effect_analyses.insert(
                effect.name.clone(),
                EffectAnalysis::new(&effect.name, effect.severity, effect.target_dc),
            );
        }

        // Also track faults that don't match any defined effect (as "other")
        effect_analyses.insert(
            "_output_corruption".to_string(),
            EffectAnalysis::new(
                "_output_corruption",
                Severity::S2,
                self.spec.target_dc(Severity::S2),
            ),
        );

        // Process each fault result
        let mut detection_by_mechanism: HashMap<String, usize> = HashMap::new();

        for result in results {
            if result.triggered_effects.is_empty() {
                // Fault didn't cause any defined effect - safe fault
                continue;
            }

            // Update effect analyses
            for effect_name in &result.triggered_effects {
                if let Some(analysis) = effect_analyses.get_mut(effect_name) {
                    analysis.total_faults_causing += 1;

                    if result.detected {
                        analysis.faults_detected += 1;
                        if let Some(mech) = &result.detected_by {
                            *analysis
                                .detection_by_mechanism
                                .entry(mech.clone())
                                .or_insert(0) += 1;
                            *detection_by_mechanism.entry(mech.clone()).or_insert(0) += 1;
                        }
                    } else {
                        analysis.undetected_sites.push(result.fault_site.clone());
                    }

                    // Track contributors
                    *analysis
                        .contributors
                        .entry(result.primitive_path.clone())
                        .or_insert(0) += 1;
                }
            }
        }

        // Update measured DC for each effect
        for analysis in effect_analyses.values_mut() {
            analysis.update_dc();
        }

        // Calculate overall metrics
        let (spfm, lf, pmhf) =
            self.calculate_metrics(&effect_analyses, results, results.len(), total_fit);

        // Check if meets ASIL requirements
        let meets_asil = effect_analyses
            .values()
            .filter(|a| !a.effect_name.starts_with('_')) // Skip internal effects
            .all(|a| a.meets_target);

        // Generate AutoFmeda
        let auto_fmea = self.generate_auto_fmea(
            &effect_analyses,
            results,
            &detection_by_mechanism,
            design_name,
            results.len(),
            total_fit,
        );

        // Calculate runtime_dc and boot_dc from FI results based on detection mode
        // runtime_dc = faults detected by Continuous mode / total dangerous faults
        // boot_dc = faults detected by Boot/Periodic/OnDemand modes / total dangerous faults
        let (runtime_dc, boot_dc) = self.calculate_mode_specific_dc(results);

        FiDrivenFmeaResult {
            goal_name: self.spec.goal_name.clone(),
            design_name: design_name.to_string(),
            total_primitives: primitive_count,
            total_injections: results.len(),
            effect_analyses,
            fault_results: results.to_vec(),
            auto_fmea,
            measured_spfm: spfm,
            measured_lf: lf,
            measured_pmhf: Some(pmhf),
            wall_time: start.elapsed(),
            meets_asil,
            runtime_dc,
            boot_dc,
            ccf_contribution: None, // Set via apply_ccf_contribution()
            sm_contribution: None,  // Set via apply_sm_contribution()
        }
    }

    /// Calculate detection mode specific DC values from FI results
    ///
    /// ISO 26262 distinguishes between:
    /// - Runtime (Continuous) detection: Contributes to SPFM calculation
    /// - Boot-time/Periodic detection: Contributes to LFM calculation only
    fn calculate_mode_specific_dc(
        &self,
        results: &[FaultEffectResult],
    ) -> (Option<f64>, Option<f64>) {
        // Count faults that caused dangerous effects (not safe faults)
        let dangerous_faults: Vec<_> = results
            .iter()
            .filter(|r| !r.triggered_effects.is_empty())
            .collect();

        if dangerous_faults.is_empty() {
            return (None, None);
        }

        let total_dangerous = dangerous_faults.len();

        // Count faults detected by each mode
        let mut continuous_detected = 0usize;
        let mut boot_detected = 0usize;

        for result in &dangerous_faults {
            if result.detected {
                match result.detection_mode {
                    Some(DetectionMode::Continuous) | None => {
                        // Default to Continuous if not specified (backwards compatibility)
                        continuous_detected += 1;
                    }
                    Some(DetectionMode::Boot)
                    | Some(DetectionMode::Periodic)
                    | Some(DetectionMode::OnDemand) => {
                        boot_detected += 1;
                    }
                }
            }
        }

        // Calculate DC percentages
        let runtime_dc = if continuous_detected > 0 {
            Some((continuous_detected as f64 / total_dangerous as f64) * 100.0)
        } else {
            Some(0.0)
        };

        let boot_dc = if boot_detected > 0 {
            Some((boot_detected as f64 / total_dangerous as f64) * 100.0)
        } else {
            Some(0.0)
        };

        (runtime_dc, boot_dc)
    }

    /// Calculate SPFM, LFM, and PMHF from effect analyses
    ///
    /// ISO 26262-5 definitions:
    /// - SPFM = 1 - (λSPF / λ) where SPF = dangerous faults not detected at RUNTIME
    ///   SPFM only counts Continuous (runtime) detections
    ///   Boot-time-only hardware (e.g., BIST) is excluded - it's inactive during runtime
    /// - LFM = 1 - (λMPF_latent / (λ - λSPF)) where latent = not detected by ANY mode
    ///   LFM counts ALL detections (Continuous + Boot + Periodic + OnDemand)
    ///   Boot-time-only hardware faults are detected at boot, so they're not latent
    /// - PMHF = Σ(λ × (1 - DC))
    fn calculate_metrics(
        &self,
        analyses: &HashMap<String, EffectAnalysis>,
        results: &[FaultEffectResult],
        total_injections: usize,
        total_fit: f64,
    ) -> (f64, f64, f64) {
        let mut total_dangerous = 0u64;

        for analysis in analyses.values() {
            total_dangerous += analysis.total_faults_causing;
        }

        let total = total_injections as u64;

        // Count boot-time-only faults - these are excluded from SPFM calculation
        // because the hardware is inactive during runtime operation
        let boot_time_only_faults = results.iter().filter(|r| r.is_boot_time_only).count() as u64;

        // For SPFM: exclude boot-time-only hardware entirely (not relevant to runtime)
        let runtime_total = total.saturating_sub(boot_time_only_faults);

        // Count dangerous faults excluding boot-time-only hardware
        let boot_time_only_dangerous = results
            .iter()
            .filter(|r| r.is_boot_time_only && !r.triggered_effects.is_empty())
            .count() as u64;
        let runtime_dangerous = total_dangerous.saturating_sub(boot_time_only_dangerous);
        let runtime_safe = runtime_total.saturating_sub(runtime_dangerous);

        // For LFM: boot-time-only faults ARE detected (at boot), so they count as detected
        let safe_faults = total.saturating_sub(total_dangerous);

        // Count faults by detection mode
        // SPFM only counts Continuous (runtime) detections on runtime hardware
        // LFM counts ALL detections
        let mut continuous_detected = 0u64;
        let mut all_detected = 0u64;

        for result in results {
            // Only count faults that triggered dangerous effects
            if !result.triggered_effects.is_empty() && result.detected {
                all_detected += 1;
                // Count runtime (Continuous) detections for SPFM
                // Exclude boot-time-only hardware from SPFM counting
                if !result.is_boot_time_only {
                    match result.detection_mode {
                        Some(DetectionMode::Continuous) | None => {
                            // Default to Continuous if not specified (backwards compatibility)
                            continuous_detected += 1;
                        }
                        Some(DetectionMode::Boot)
                        | Some(DetectionMode::Periodic)
                        | Some(DetectionMode::OnDemand) => {
                            // These count for LFM but not SPFM
                        }
                    }
                }
            }
        }

        // Boot-time-only faults are effectively "detected" for LFM purposes
        // because BIST detects them at boot - they're not latent
        let boot_time_only_detected = boot_time_only_dangerous; // All boot-time faults detected by BIST

        // SPFM = 1 - (SPF / λ_runtime) = (safe + runtime_detected) / runtime_total
        // SPF = single point faults = dangerous runtime faults not detected at RUNTIME
        // Boot-time-only hardware is excluded from both numerator and denominator
        let spfm = if runtime_total > 0 {
            (runtime_safe + continuous_detected) as f64 / runtime_total as f64
        } else {
            1.0
        };

        // LFM = 1 - (latent / total) = (safe + all_detected) / total
        // Latent faults = dangerous faults not detected by ANY mechanism (runtime OR test)
        // Boot-time-only faults are detected at boot, so they're not latent
        let lfm = if total > 0 {
            (safe_faults + all_detected + boot_time_only_detected) as f64 / total as f64
        } else {
            1.0
        };

        // PMHF = λ × (1 - DC_runtime)
        // PMHF uses runtime DC because it represents probability of failure in operation
        //
        // IMPORTANT: Exclude boot-time-only hardware (e.g., BIST) from steady-state PMHF
        // Boot-time hardware is inactive during normal operation, so its faults don't
        // contribute to the probability of failure during operation.
        let boot_time_only_faults = results.iter().filter(|r| r.is_boot_time_only).count() as u64;

        // Estimate FIT for boot-time-only hardware (proportional to fault count)
        // This assumes uniform FIT distribution across primitives
        let boot_time_only_fit = if total > 0 {
            total_fit * (boot_time_only_faults as f64 / total as f64)
        } else {
            0.0
        };

        // Steady-state FIT excludes boot-time hardware
        let steady_state_fit = total_fit - boot_time_only_fit;

        // Filter dangerous faults to exclude boot-time-only for runtime DC
        let steady_state_dangerous = results
            .iter()
            .filter(|r| !r.triggered_effects.is_empty() && !r.is_boot_time_only)
            .count() as u64;

        let steady_state_detected = results
            .iter()
            .filter(|r| {
                !r.triggered_effects.is_empty()
                    && r.detected
                    && !r.is_boot_time_only
                    && matches!(r.detection_mode, Some(DetectionMode::Continuous) | None)
            })
            .count() as u64;

        let dc_runtime = if steady_state_dangerous > 0 {
            steady_state_detected as f64 / steady_state_dangerous as f64
        } else {
            1.0
        };
        let pmhf = steady_state_fit * (1.0 - dc_runtime);

        (spfm, lfm, pmhf)
    }

    /// Generate AutoFmeda from analyses
    fn generate_auto_fmea(
        &self,
        analyses: &HashMap<String, EffectAnalysis>,
        results: &[FaultEffectResult],
        detection_by_mechanism: &HashMap<String, usize>,
        design_name: &str,
        faults_injected: usize,
        total_fit: f64,
    ) -> AutoFmeda {
        let mut entries = Vec::new();

        for (effect_name, analysis) in analyses {
            if effect_name.starts_with('_') {
                continue; // Skip internal effects
            }

            // Determine failure class based on DC
            // - Residual: DC >= 99% (meets ASIL-D)
            // - MultiPoint: DC > 0% but < 99% (partial protection)
            // - SinglePointFault: DC = 0% (no protection)
            let class = if analysis.measured_dc >= 0.99 {
                FailureClass::Residual
            } else if analysis.measured_dc > 0.0 {
                FailureClass::MultiPoint
            } else {
                FailureClass::SinglePointFault
            };

            // Build contributors list
            let mut contributors: Vec<AutoFmedaContributor> = analysis
                .contributors
                .iter()
                .take(10) // Top 10
                .map(|(path, count)| {
                    let weight = *count as f64 / analysis.total_faults_causing.max(1) as f64;
                    AutoFmedaContributor {
                        pattern: path.clone(),
                        primitive_count: *count,
                        fit: total_fit * weight,
                        weight,
                    }
                })
                .collect();
            contributors.sort_by(|a, b| b.weight.partial_cmp(&a.weight).unwrap());

            // Build detection breakdown
            let detections: Vec<AutoFmedaDetection> = analysis
                .detection_by_mechanism
                .iter()
                .map(|(mech, count)| AutoFmedaDetection {
                    mechanism: mech.clone(),
                    covered: *count,
                    dc: *count as f64 / analysis.total_faults_causing.max(1) as f64,
                })
                .collect();

            // Build undetected list
            let undetected: Vec<String> = analysis
                .undetected_sites
                .iter()
                .take(100) // Limit to first 100
                .map(|site| site.primitive_path.to_string())
                .collect();

            entries.push(AutoFmedaEntry {
                name: effect_name.clone(),
                condition: format!("Safety goal effect: {}", effect_name),
                severity: analysis.severity,
                class,
                total_fit: total_fit
                    * (analysis.total_faults_causing as f64 / faults_injected.max(1) as f64),
                measured_dc: analysis.measured_dc,
                contributors,
                detection: detections,
                undetected,
            });
        }

        // Calculate summary
        let (spfm, lf, pmhf) =
            self.calculate_metrics(analyses, results, faults_injected, total_fit);
        let meets = entries
            .iter()
            .all(|e| e.measured_dc >= 0.99 * e.total_fit.signum());

        AutoFmeda {
            metadata: AutoFmedaMetadata {
                goal_name: self.spec.goal_name.clone(),
                design_name: design_name.to_string(),
                timestamp: chrono::Utc::now().to_rfc3339(),
                tool_version: env!("CARGO_PKG_VERSION").to_string(),
                primitives_analyzed: 0, // Set by caller
                faults_injected: faults_injected as u64,
                simulation_cycles: self.config.cycles_per_fault * faults_injected as u64,
                wall_time: "N/A".to_string(),
            },
            summary: AutoFmedaSummary {
                total_fit,
                spfm,
                lfm: lf,
                pmhf,
                asil_achieved: if meets && spfm >= 0.99 && lf >= 0.90 && pmhf <= 10.0 {
                    AsilLevel::D
                } else if spfm >= 0.97 && lf >= 0.80 && pmhf <= 100.0 {
                    AsilLevel::C
                } else if spfm >= 0.90 && lf >= 0.60 {
                    AsilLevel::B
                } else {
                    AsilLevel::QM
                },
                meets_requirements: meets,
            },
            entries,
        }
    }

    /// Get the effect monitor for external use
    pub fn monitor(&self) -> &EffectMonitor {
        &self.monitor
    }

    /// Get mutable effect monitor for signal updates
    pub fn monitor_mut(&mut self) -> &mut EffectMonitor {
        &mut self.monitor
    }
}

// ============================================================================
// Helper: Convert existing FaultCampaignResults to FI-Driven format
// ============================================================================

/// Convert skalp-sim FaultCampaignResults to FI-driven format with effect monitoring
///
/// This bridges the existing fault simulation infrastructure with the new
/// effect-based FMEA generation.
#[cfg(feature = "sim-integration")]
pub fn convert_campaign_to_effect_results(
    campaign: &skalp_sim::FaultCampaignResults,
    primitive_paths: &HashMap<skalp_lir::lir::PrimitiveId, String>,
    monitor: &EffectMonitor,
    output_signals: &HashMap<String, u64>,
    golden_outputs: &HashMap<String, u64>,
) -> Vec<FaultEffectResult> {
    let mut results = Vec::new();

    for fault_result in &campaign.fault_results {
        let primitive_id = fault_result.fault.target_primitive;
        let path = primitive_paths
            .get(&primitive_id)
            .cloned()
            .unwrap_or_else(|| format!("primitive_{}", primitive_id.0));

        // Determine which effects were triggered
        // For now, any output difference counts as "output corruption"
        let triggered_effects = if !fault_result.output_diffs.is_empty() {
            // Check monitor for specific effects
            let mut effects = monitor.check_effects(output_signals);
            if effects.is_empty() {
                effects.push("_output_corruption".to_string());
            }
            effects
        } else {
            vec![]
        };

        // Determine detection mechanism based on which detection signal fired
        let detected_by = if fault_result.detected {
            // Would need to track which specific signal detected
            Some("safety_mechanism".to_string())
        } else {
            None
        };

        let fault_type = convert_fault_type(fault_result.fault.fault_type);

        // Convert detection mode from SIR to HIR enum
        let detection_mode = fault_result.detection_mode.map(|m| {
            use skalp_sim::sir::SirDetectionMode;
            match m {
                SirDetectionMode::Continuous => DetectionMode::Continuous,
                SirDetectionMode::Boot => DetectionMode::Boot,
                SirDetectionMode::Periodic => DetectionMode::Periodic,
                SirDetectionMode::OnDemand => DetectionMode::OnDemand,
            }
        });

        results.push(FaultEffectResult {
            fault_site: FaultSite::new(DesignRef::parse(&path), fault_type),
            primitive_path: path,
            triggered_effects,
            detected: fault_result.detected,
            detected_by,
            effect_cycle: None, // Would need cycle-accurate monitoring
            detection_cycle: fault_result.detection_cycle,
            is_safety_mechanism: false, // Set by caller from annotations
            detection_mode,
            is_boot_time_only: false, // Set by caller from annotations
        });
    }

    results
}

/// Convert skalp-sim FaultType to skalp-safety FaultType
#[cfg(feature = "sim-integration")]
fn convert_fault_type(sim_fault: skalp_sim::sir::FaultType) -> FaultType {
    match sim_fault {
        skalp_sim::sir::FaultType::StuckAt0 => FaultType::StuckAt0,
        skalp_sim::sir::FaultType::StuckAt1 => FaultType::StuckAt1,
        skalp_sim::sir::FaultType::Bridge { .. } => FaultType::Bridging,
        skalp_sim::sir::FaultType::Open { .. } => FaultType::Open,
        skalp_sim::sir::FaultType::Transient => FaultType::Transient,
        skalp_sim::sir::FaultType::BitFlip => FaultType::Transient,
        skalp_sim::sir::FaultType::MultiBitUpset { .. } => FaultType::MultiBitUpset,
        skalp_sim::sir::FaultType::SetupViolation => FaultType::SetupViolation,
        skalp_sim::sir::FaultType::HoldViolation => FaultType::HoldViolation,
        skalp_sim::sir::FaultType::Metastability { .. } => FaultType::Metastability,
        skalp_sim::sir::FaultType::TimingDelay { .. } => FaultType::SetupViolation,
        skalp_sim::sir::FaultType::VoltageDropout => FaultType::VoltageDropout,
        skalp_sim::sir::FaultType::GroundBounce => FaultType::GroundBounce,
        skalp_sim::sir::FaultType::CrosstalkGlitch => FaultType::CrosstalkGlitch,
        skalp_sim::sir::FaultType::ClockGlitch => FaultType::ClockGlitch,
        skalp_sim::sir::FaultType::ClockStretch { .. } => FaultType::ClockGlitch,
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_effect_monitor_simple_condition() {
        let spec = SafetyGoalSimSpec::new("TestGoal", AsilLevel::D);
        let monitor = EffectMonitor::new(&spec);

        let mut signals = HashMap::new();
        signals.insert("motor_enable".to_string(), 1u64);
        signals.insert("safe_mode".to_string(), 0u64);

        // With no effects defined, should return empty
        let effects = monitor.check_effects(&signals);
        assert!(effects.is_empty());
    }

    #[test]
    fn test_effect_monitor_with_effect() {
        let mut spec = SafetyGoalSimSpec::new("TestGoal", AsilLevel::D);
        spec.add_effect(FailureEffectDef {
            name: "unintended_motor".to_string(),
            description: Some("Motor active in unsafe state".to_string()),
            condition: EffectCondition::Term(ConditionTerm {
                signal: DesignRef::parse("motor_enable"),
                op: CompareOp::NotEqual,
                value: CompareValue::Literal(0),
            }),
            severity: Severity::S3,
            target_dc: 0.99,
        });

        let monitor = EffectMonitor::new(&spec);

        let mut signals = HashMap::new();
        signals.insert("motor_enable".to_string(), 1u64);

        let effects = monitor.check_effects(&signals);
        assert_eq!(effects.len(), 1);
        assert_eq!(effects[0], "unintended_motor");
    }

    #[test]
    fn test_effect_monitor_compound_condition() {
        let mut spec = SafetyGoalSimSpec::new("TestGoal", AsilLevel::D);

        // motor_enable != 0 AND safe_mode == 0
        let condition = EffectCondition::And(vec![
            EffectCondition::Term(ConditionTerm {
                signal: DesignRef::parse("motor_enable"),
                op: CompareOp::NotEqual,
                value: CompareValue::Literal(0),
            }),
            EffectCondition::Term(ConditionTerm {
                signal: DesignRef::parse("safe_mode"),
                op: CompareOp::Equal,
                value: CompareValue::Literal(0),
            }),
        ]);

        spec.add_effect(FailureEffectDef {
            name: "unsafe_motor_active".to_string(),
            description: None,
            condition,
            severity: Severity::S3,
            target_dc: 0.99,
        });

        let monitor = EffectMonitor::new(&spec);

        // Both conditions true
        let mut signals = HashMap::new();
        signals.insert("motor_enable".to_string(), 1u64);
        signals.insert("safe_mode".to_string(), 0u64);
        let effects = monitor.check_effects(&signals);
        assert_eq!(effects.len(), 1);

        // safe_mode = 1 (should not trigger)
        signals.insert("safe_mode".to_string(), 1u64);
        let effects = monitor.check_effects(&signals);
        assert!(effects.is_empty());
    }

    #[test]
    fn test_fi_driven_fmea_generation() {
        let mut spec = SafetyGoalSimSpec::new("BrakingSafety", AsilLevel::D);
        spec.add_effect(FailureEffectDef {
            name: "brake_failure".to_string(),
            description: Some("Brake command corrupted".to_string()),
            condition: EffectCondition::Term(ConditionTerm {
                signal: DesignRef::parse("brake_cmd"),
                op: CompareOp::NotEqual,
                value: CompareValue::Golden("golden_brake_cmd".to_string()),
            }),
            severity: Severity::S3,
            target_dc: 0.99,
        });

        let config = FiDrivenConfig::default();
        let generator = SafetyDrivenFmeaGenerator::new(spec, config);

        // Simulate some fault results
        let fault_results = vec![
            FaultEffectResult {
                fault_site: FaultSite::new(DesignRef::parse("top.brake.reg0"), FaultType::StuckAt0),
                primitive_path: "top.brake.reg0".to_string(),
                triggered_effects: vec!["brake_failure".to_string()],
                detected: true,
                detected_by: Some("TmrVoter".to_string()),
                effect_cycle: Some(10),
                detection_cycle: Some(5),
                is_safety_mechanism: false,
                detection_mode: None,
                is_boot_time_only: false,
            },
            FaultEffectResult {
                fault_site: FaultSite::new(DesignRef::parse("top.brake.reg1"), FaultType::StuckAt1),
                primitive_path: "top.brake.reg1".to_string(),
                triggered_effects: vec!["brake_failure".to_string()],
                detected: false,
                detected_by: None,
                effect_cycle: Some(15),
                detection_cycle: None,
                is_safety_mechanism: false,
                detection_mode: None,
                is_boot_time_only: false,
            },
        ];

        let result = generator.generate_from_campaign_results(
            &fault_results,
            100,   // primitives
            "top", // design name
            500.0, // total FIT
        );

        assert_eq!(result.goal_name, "BrakingSafety");
        assert_eq!(result.total_injections, 2);

        // Check effect analysis
        let brake_analysis = result.effect_analyses.get("brake_failure").unwrap();
        assert_eq!(brake_analysis.total_faults_causing, 2);
        assert_eq!(brake_analysis.faults_detected, 1);
        assert!((brake_analysis.measured_dc - 0.5).abs() < 0.01); // 1/2 = 50%

        // Check FMEA was generated
        assert!(!result.auto_fmea.entries.is_empty());
    }

    #[test]
    fn test_fault_effect_result_classification() {
        let dangerous = FaultEffectResult {
            fault_site: FaultSite::new(DesignRef::parse("cell"), FaultType::StuckAt0),
            primitive_path: "cell".to_string(),
            triggered_effects: vec!["hazard".to_string()],
            detected: false,
            detected_by: None,
            effect_cycle: Some(10),
            detection_cycle: None,
            is_safety_mechanism: false,
            detection_mode: None,
            is_boot_time_only: false,
        };
        assert!(dangerous.is_dangerous_undetected());

        let safe = FaultEffectResult {
            fault_site: FaultSite::new(DesignRef::parse("cell"), FaultType::StuckAt0),
            primitive_path: "cell".to_string(),
            triggered_effects: vec!["hazard".to_string()],
            detected: true,
            detected_by: Some("SM".to_string()),
            effect_cycle: Some(10),
            detection_cycle: Some(5), // Detected before effect
            is_safety_mechanism: false,
            detection_mode: None,
            is_boot_time_only: false,
        };
        assert!(!safe.is_dangerous_undetected());
        assert!(safe.is_safe_detection());
    }
}
