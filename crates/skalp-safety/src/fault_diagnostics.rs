//! Enhanced Fault Diagnostics for Safety Analysis
//!
//! Provides detailed classification of undetected faults to help engineers
//! understand WHY faults are not detected and prioritize fixes.

use crate::fault_simulation::{FaultSite, FaultType};
use serde::{Deserialize, Serialize};
use skalp_lir::Lir;
use std::collections::{HashMap, HashSet};

/// Classification of WHY a fault is undetected
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum UndetectedReason {
    /// Fault is in safety mechanism internal logic (SM itself is unprotected)
    SmInternal,
    /// Fault is in the output path after the safety mechanism
    OutputPath,
    /// Fault is in the input path before redundancy split (common input)
    InputPath,
    /// Fault is in functional logic but SM doesn't detect it (coverage gap)
    CoverageGap,
    /// TRUE CCF: Fault on shared signal BEFORE redundancy split (clk, rst, enable)
    /// This is the most dangerous - affects ALL channels simultaneously
    SharedInputCcf,
    /// DESIGN CCF: Same design pattern replicated across channels
    /// Systematic design bug could affect all copies
    ReplicatedDesignCcf,
    /// Same fault in all redundant channels (common cause failure potential)
    /// NOTE: This is a weaker classification based on primitive name matching
    CommonCausePotential,
    /// SM detected but detection came too late (after FTTI)
    LateDetection,
    /// Identical SM implementations defeat comparison-based detection
    /// (two voters of same type will produce same wrong output)
    IdenticalSmReplication,
    /// Unknown classification
    Unknown,
}

/// Simplified fault classification for BIST generation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FaultClassification {
    /// Fault in functional logic not covered by runtime SM (needs BIST)
    CoverageGap,
    /// Fault inside safety mechanism (needs SM-of-SM)
    SmInternal,
    /// Fault in output path after SM (needs output encoding)
    OutputPath,
    /// Fault is CCF source (needs input protection)
    CommonCause,
    /// Other/unknown classification
    Other,
}

impl From<UndetectedReason> for FaultClassification {
    fn from(reason: UndetectedReason) -> Self {
        match reason {
            UndetectedReason::CoverageGap => FaultClassification::CoverageGap,
            UndetectedReason::SmInternal | UndetectedReason::IdenticalSmReplication => {
                FaultClassification::SmInternal
            }
            UndetectedReason::OutputPath => FaultClassification::OutputPath,
            UndetectedReason::SharedInputCcf
            | UndetectedReason::ReplicatedDesignCcf
            | UndetectedReason::CommonCausePotential
            | UndetectedReason::InputPath => FaultClassification::CommonCause,
            _ => FaultClassification::Other,
        }
    }
}

/// Information about an undetected fault for BIST generation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UndetectedFaultInfo {
    /// Fault site path
    pub fault_site: String,
    /// Fault type (stuck_at_0 or stuck_at_1)
    pub fault_type: String,
    /// Component containing the fault
    pub component: String,
    /// FIT contribution
    pub fit_contribution: f64,
    /// Classification for mitigation strategy
    pub classification: FaultClassification,
}

impl UndetectedReason {
    /// Human-readable name
    pub fn name(&self) -> &'static str {
        match self {
            UndetectedReason::SmInternal => "SM Internal",
            UndetectedReason::OutputPath => "Output Path",
            UndetectedReason::InputPath => "Input Path",
            UndetectedReason::CoverageGap => "Coverage Gap",
            UndetectedReason::SharedInputCcf => "Shared Input CCF",
            UndetectedReason::ReplicatedDesignCcf => "Replicated Design CCF",
            UndetectedReason::CommonCausePotential => "Common Cause",
            UndetectedReason::LateDetection => "Late Detection",
            UndetectedReason::IdenticalSmReplication => "Identical SM Replication",
            UndetectedReason::Unknown => "Unknown",
        }
    }

    /// Recommended action to fix
    pub fn recommendation(&self) -> &'static str {
        match self {
            UndetectedReason::SmInternal => "Add SM-of-SM (e.g., dual voters with comparator)",
            UndetectedReason::OutputPath => "Add output protection (e.g., encoding, E2E check)",
            UndetectedReason::InputPath => "Add input validation or move redundancy split earlier",
            UndetectedReason::CoverageGap => "Extend SM coverage pattern or add additional SM",
            UndetectedReason::SharedInputCcf => {
                "Add input protection BEFORE redundancy split (voting, validation, redundant sources)"
            }
            UndetectedReason::ReplicatedDesignCcf => {
                "Use diverse implementations per channel (different algorithms/architectures)"
            }
            UndetectedReason::CommonCausePotential => {
                "Add diversity or independence between channels"
            }
            UndetectedReason::LateDetection => "Reduce detection latency or increase FTTI budget",
            UndetectedReason::IdenticalSmReplication => {
                "Use DIVERSE SM implementations (different algorithms) - identical SMs produce same wrong output, defeating comparison"
            }
            UndetectedReason::Unknown => "Manual analysis required",
        }
    }

    /// ISO 26262 reference for this failure mode
    pub fn iso_reference(&self) -> &'static str {
        match self {
            UndetectedReason::SmInternal => "ISO 26262-5:7.4.4 (SM coverage)",
            UndetectedReason::OutputPath => "ISO 26262-5:7.4.3 (Residual faults)",
            UndetectedReason::InputPath => "ISO 26262-9:7 (Dependent failures)",
            UndetectedReason::CoverageGap => "ISO 26262-5:8.4.5 (DC evaluation)",
            UndetectedReason::SharedInputCcf => "ISO 26262-9:7.4.1 (Dependent failure initiators)",
            UndetectedReason::ReplicatedDesignCcf => "ISO 26262-9:7.4.3 (Systematic failures)",
            UndetectedReason::CommonCausePotential => "ISO 26262-9:7.4 (CCF analysis)",
            UndetectedReason::LateDetection => "ISO 26262-5:7.4.2 (FTTI)",
            UndetectedReason::IdenticalSmReplication => {
                "ISO 26262-9:7.4.3 (Diversity for SM-of-SM)"
            }
            UndetectedReason::Unknown => "ISO 26262-5:8 (Safety analysis)",
        }
    }
}

/// Classified undetected fault with diagnostic information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClassifiedFault {
    /// The fault site
    pub fault_site: FaultSite,
    /// Why this fault is undetected
    pub reason: UndetectedReason,
    /// Path from fault to output (if traced)
    pub propagation_path: Option<Vec<String>>,
    /// FIT contribution of this fault
    pub fit_contribution: f64,
    /// Component containing this fault
    pub component: String,
    /// Is this fault in a safety mechanism?
    pub in_safety_mechanism: bool,
    /// Related faults (for common cause grouping)
    pub related_faults: Vec<String>,
}

/// Summary of fault classifications
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct FaultClassificationSummary {
    /// Count by reason
    pub by_reason: HashMap<UndetectedReason, usize>,
    /// Count by component
    pub by_component: HashMap<String, usize>,
    /// Total FIT by reason
    pub fit_by_reason: HashMap<UndetectedReason, f64>,
    /// Top contributors to PMHF
    pub top_pmhf_contributors: Vec<(String, f64)>,
}

// ============================================================================
// Common Cause Failure (CCF) Analysis
// ============================================================================

/// Type of common cause failure source
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum CcfSourceType {
    /// Shared input signal before redundancy split (TRUE CCF)
    /// e.g., clk, rst, enable feeding all channels
    SharedInput {
        /// Signal name
        signal_name: String,
        /// Channels it feeds
        affected_channels: Vec<String>,
    },
    /// Same design pattern replicated (Design CCF)
    /// Systematic design fault could affect all copies
    ReplicatedDesign {
        /// Primitive type/pattern
        pattern: String,
        /// Instances in each channel
        instances: Vec<String>,
    },
    /// Shared clock domain (all channels use same clock)
    SharedClock {
        clock_name: String,
        affected_channels: Vec<String>,
    },
    /// Shared reset (all channels use same reset)
    SharedReset {
        reset_name: String,
        affected_channels: Vec<String>,
    },
}

impl CcfSourceType {
    /// Human-readable name
    pub fn name(&self) -> &str {
        match self {
            CcfSourceType::SharedInput { .. } => "Shared Input",
            CcfSourceType::ReplicatedDesign { .. } => "Replicated Design",
            CcfSourceType::SharedClock { .. } => "Shared Clock",
            CcfSourceType::SharedReset { .. } => "Shared Reset",
        }
    }

    /// Risk level (1-5, 5 being highest)
    pub fn risk_level(&self) -> u8 {
        match self {
            CcfSourceType::SharedInput {
                affected_channels, ..
            } => {
                if affected_channels.len() >= 3 {
                    5
                } else {
                    4
                }
            }
            CcfSourceType::SharedClock { .. } => 5, // Clock failure is catastrophic
            CcfSourceType::SharedReset { .. } => 4,
            CcfSourceType::ReplicatedDesign { .. } => 3, // Design CCF is systematic
        }
    }

    /// ISO 26262 reference
    pub fn iso_reference(&self) -> &'static str {
        match self {
            CcfSourceType::SharedInput { .. } => "ISO 26262-9:7.4.1 (Dependent failure initiators)",
            CcfSourceType::ReplicatedDesign { .. } => "ISO 26262-9:7.4.3 (Systematic failures)",
            CcfSourceType::SharedClock { .. } => "ISO 26262-9:7.4.2 (Common cause initiators)",
            CcfSourceType::SharedReset { .. } => "ISO 26262-9:7.4.2 (Common cause initiators)",
        }
    }

    /// Recommended mitigation
    pub fn mitigation(&self) -> &'static str {
        match self {
            CcfSourceType::SharedInput { .. } => {
                "Add input validation/voting before redundancy split, or use diverse input sources"
            }
            CcfSourceType::ReplicatedDesign { .. } => {
                "Use diverse implementations (different algorithms/architectures per channel)"
            }
            CcfSourceType::SharedClock { .. } => {
                "Add clock monitor, use redundant clock sources, or add temporal diversity"
            }
            CcfSourceType::SharedReset { .. } => {
                "Add reset monitor, use sequenced/diverse reset generation"
            }
        }
    }
}

/// A single CCF source with its affected primitives
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CcfSource {
    /// Type of CCF
    pub source_type: CcfSourceType,
    /// Signal/net name that is the source
    pub source_signal: String,
    /// Primitives directly affected by this CCF source
    pub affected_primitives: Vec<String>,
    /// Number of redundant channels affected
    pub channels_affected: usize,
    /// Estimated FIT contribution if this CCF occurs
    pub fit_impact: f64,
    /// Failure mode if this signal fails
    pub failure_modes: Vec<String>,
}

/// Complete CCF analysis result
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CcfAnalysis {
    /// All identified CCF sources
    pub sources: Vec<CcfSource>,
    /// Shared signals that feed multiple channels
    pub shared_signals: HashMap<String, SharedSignalInfo>,
    /// Summary statistics
    pub total_ccf_sources: usize,
    pub high_risk_sources: usize,
    pub total_fit_at_risk: f64,
}

/// Information about a shared signal
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SharedSignalInfo {
    /// Signal name
    pub name: String,
    /// Is this a primary input?
    pub is_primary_input: bool,
    /// Is this a clock signal?
    pub is_clock: bool,
    /// Is this a reset signal?
    pub is_reset: bool,
    /// Channels this signal feeds
    pub feeds_channels: Vec<String>,
    /// Total fanout (number of primitives driven)
    pub fanout: usize,
    /// Primitives driven by this signal
    pub driven_primitives: Vec<String>,
}

// ============================================================================
// Safety Mechanism Diversity Analysis
// ============================================================================

/// Analysis result for SM diversity (detects identical SM replication)
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SmDiversityAnalysis {
    /// Pairs of identical SM instances (same entity type)
    pub identical_sm_pairs: Vec<IdenticalSmPair>,
    /// Whether any comparator exists that compares these identical SMs
    pub has_comparator: bool,
    /// Overall diversity score (0.0 = no diversity, 1.0 = full diversity)
    pub diversity_score: f64,
    /// Detailed warnings about diversity issues
    pub warnings: Vec<SmDiversityWarning>,
    /// Total FIT at risk due to identical SM replication
    pub fit_at_risk: f64,
}

/// A pair of SM instances that are identical (same entity type)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IdenticalSmPair {
    /// Instance A path (e.g., "top.voter_a")
    pub instance_a: String,
    /// Instance B path (e.g., "top.voter_b")
    pub instance_b: String,
    /// Entity type name (e.g., "TmrVoter")
    pub entity_type: String,
    /// Whether these are being compared by another SM
    pub outputs_compared: bool,
    /// Matching primitives in both instances (same relative path)
    pub matching_primitives: Vec<MatchingPrimitivePair>,
    /// FIT contribution from faults that will produce identical wrong outputs
    pub identical_failure_fit: f64,
}

/// A pair of primitives that match across identical SM instances
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MatchingPrimitivePair {
    /// Path in instance A
    pub path_a: String,
    /// Path in instance B
    pub path_b: String,
    /// Primitive type (e.g., "mux", "and", "dff")
    pub primitive_type: String,
    /// Relative path within the SM (e.g., "mux_92")
    pub relative_path: String,
}

/// Warning about SM diversity issues
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SmDiversityWarning {
    /// Severity (Critical, High, Medium, Low)
    pub severity: WarningSeverity,
    /// Warning message
    pub message: String,
    /// Detailed explanation
    pub explanation: String,
    /// Affected SM instances
    pub affected_instances: Vec<String>,
    /// ISO 26262 reference
    pub iso_reference: String,
    /// Recommended fix
    pub recommendation: String,
}

/// Severity level for warnings
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WarningSeverity {
    Critical,
    High,
    Medium,
    Low,
}

impl WarningSeverity {
    pub fn emoji(&self) -> &'static str {
        match self {
            WarningSeverity::Critical => "🔴",
            WarningSeverity::High => "🟠",
            WarningSeverity::Medium => "🟡",
            WarningSeverity::Low => "🟢",
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            WarningSeverity::Critical => "CRITICAL",
            WarningSeverity::High => "HIGH",
            WarningSeverity::Medium => "MEDIUM",
            WarningSeverity::Low => "LOW",
        }
    }
}

/// Fault diagnostic analyzer
pub struct FaultDiagnostics {
    /// Safety mechanism paths (components marked as SM)
    sm_paths: HashSet<String>,
    /// Output signal paths
    output_paths: HashSet<String>,
    /// Input signal paths (before redundancy)
    input_paths: HashSet<String>,
    /// FIT rates by primitive type
    fit_rates: HashMap<String, f64>,
    /// Redundant channel patterns
    channel_patterns: Vec<String>,
}

impl FaultDiagnostics {
    /// Create a new fault diagnostics analyzer
    pub fn new() -> Self {
        Self {
            sm_paths: HashSet::new(),
            output_paths: HashSet::new(),
            input_paths: HashSet::new(),
            fit_rates: HashMap::new(),
            channel_patterns: Vec::new(),
        }
    }

    /// Add a safety mechanism path pattern
    pub fn add_sm_path(&mut self, pattern: &str) {
        self.sm_paths.insert(pattern.to_string());
    }

    /// Add an output path pattern
    pub fn add_output_path(&mut self, pattern: &str) {
        self.output_paths.insert(pattern.to_string());
    }

    /// Add an input path pattern (before redundancy split)
    pub fn add_input_path(&mut self, pattern: &str) {
        self.input_paths.insert(pattern.to_string());
    }

    /// Set FIT rate for a primitive type
    pub fn set_fit_rate(&mut self, primitive_type: &str, fit: f64) {
        self.fit_rates.insert(primitive_type.to_string(), fit);
    }

    /// Add redundant channel pattern (e.g., "ch_*" for ch_a, ch_b, ch_c)
    pub fn add_channel_pattern(&mut self, pattern: &str) {
        self.channel_patterns.push(pattern.to_string());
    }

    /// Classify a single undetected fault
    pub fn classify_fault(&self, fault: &FaultSite) -> ClassifiedFault {
        let path = fault.primitive_path.to_string();
        let component = extract_component(&path);

        // Determine reason based on path analysis
        let reason = self.determine_reason(&path, &component);

        // Estimate FIT contribution
        let fit_contribution = self.estimate_fit(&path, &fault.fault_type);

        // Check if in SM
        let in_sm = self.is_in_sm(&path);

        ClassifiedFault {
            fault_site: fault.clone(),
            reason,
            propagation_path: None, // Will be filled by path tracing
            fit_contribution,
            component,
            in_safety_mechanism: in_sm,
            related_faults: Vec::new(),
        }
    }

    /// Classify all undetected faults and generate summary
    /// Note: This uses heuristics only. For accurate CCF classification,
    /// use `classify_with_ccf()` which uses actual netlist connectivity.
    pub fn classify_all(
        &self,
        faults: &[FaultSite],
    ) -> (Vec<ClassifiedFault>, FaultClassificationSummary) {
        let mut classified = Vec::new();

        for fault in faults {
            let cf = self.classify_fault(fault);
            classified.push(cf);
        }

        // NOTE: We no longer call identify_common_cause() here.
        // The primitive name matching heuristic was misleading - it grouped
        // independent faults as "CCF potential" just because they had the same
        // primitive type (e.g., all assign statements).
        //
        // True CCF classification requires actual netlist connectivity analysis,
        // which is done in classify_with_ccf().

        // Build summary from final classified list
        let summary = Self::build_summary(&classified);

        (classified, summary)
    }

    /// Build summary from classified faults
    fn build_summary(classified: &[ClassifiedFault]) -> FaultClassificationSummary {
        let mut summary = FaultClassificationSummary::default();

        for cf in classified {
            *summary.by_reason.entry(cf.reason).or_insert(0) += 1;
            *summary
                .by_component
                .entry(cf.component.clone())
                .or_insert(0) += 1;
            *summary.fit_by_reason.entry(cf.reason).or_insert(0.0) += cf.fit_contribution;
        }

        // Sort by FIT contribution to find top PMHF contributors
        let mut fit_by_path: HashMap<String, f64> = HashMap::new();
        for cf in classified {
            *fit_by_path
                .entry(cf.fault_site.primitive_path.to_string())
                .or_insert(0.0) += cf.fit_contribution;
        }
        let mut contributors: Vec<_> = fit_by_path.into_iter().collect();
        contributors.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
        summary.top_pmhf_contributors = contributors.into_iter().take(10).collect();

        summary
    }

    /// Classify all undetected faults using actual CCF analysis results
    /// This provides more accurate classification by using LIR connectivity data
    pub fn classify_with_ccf(
        &self,
        faults: &[FaultSite],
        ccf_analysis: &CcfAnalysis,
    ) -> (Vec<ClassifiedFault>, FaultClassificationSummary) {
        let (mut classified, _) = self.classify_all(faults);

        // Build lookup of primitives affected by shared inputs
        let mut shared_input_prims: HashSet<String> = HashSet::new();
        let mut replicated_design_prims: HashSet<String> = HashSet::new();

        for source in &ccf_analysis.sources {
            match &source.source_type {
                CcfSourceType::SharedInput { .. }
                | CcfSourceType::SharedClock { .. }
                | CcfSourceType::SharedReset { .. } => {
                    for prim in &source.affected_primitives {
                        shared_input_prims.insert(prim.clone());
                    }
                }
                CcfSourceType::ReplicatedDesign { .. } => {
                    for prim in &source.affected_primitives {
                        replicated_design_prims.insert(prim.clone());
                    }
                }
            }
        }

        // Reclassify faults based on actual CCF sources
        for cf in &mut classified {
            let path = cf.fault_site.primitive_path.to_string();

            // Check if this fault is on a shared input path (TRUE CCF)
            if shared_input_prims.contains(&path) {
                // Only reclassify if not already SM Internal (SM faults take priority)
                if cf.reason != UndetectedReason::SmInternal {
                    cf.reason = UndetectedReason::SharedInputCcf;
                }
            }
            // Check if this fault is in a replicated design pattern
            else if replicated_design_prims.contains(&path)
                && (cf.reason == UndetectedReason::CommonCausePotential
                    || cf.reason == UndetectedReason::CoverageGap)
            {
                cf.reason = UndetectedReason::ReplicatedDesignCcf;
            }
        }

        // Rebuild summary from final classified list
        let summary = Self::build_summary(&classified);

        (classified, summary)
    }

    /// Determine the reason for undetection
    fn determine_reason(&self, path: &str, component: &str) -> UndetectedReason {
        // Check if in safety mechanism
        if self.is_in_sm(path) {
            return UndetectedReason::SmInternal;
        }

        // Check if in output path (after SM)
        if self.is_output_path(path) {
            return UndetectedReason::OutputPath;
        }

        // Check if in input path (before redundancy)
        if self.is_input_path(path) {
            return UndetectedReason::InputPath;
        }

        // Default to coverage gap
        UndetectedReason::CoverageGap
    }

    /// Check if path is in a safety mechanism
    fn is_in_sm(&self, path: &str) -> bool {
        for sm_pattern in &self.sm_paths {
            if path_matches(path, sm_pattern) {
                return true;
            }
        }
        // Heuristic: check for common SM component names
        let lower = path.to_lowercase();
        lower.contains("voter")
            || lower.contains("tmr")
            || lower.contains("dmr")
            || lower.contains("watchdog")
            || lower.contains("checker")
            || lower.contains("comparator")
            || lower.contains("monitor")
    }

    /// Check if path is in output path
    fn is_output_path(&self, path: &str) -> bool {
        for out_pattern in &self.output_paths {
            if path_matches(path, out_pattern) {
                return true;
            }
        }
        // Heuristic: check for common output patterns
        path.contains("_out") ||
        path.contains("output") ||
        path.contains("_o[") ||
        // Assign statements at top level often represent output wiring
        (path.contains("assign_") && path.split('.').count() <= 2)
    }

    /// Check if path is in input path
    fn is_input_path(&self, path: &str) -> bool {
        for in_pattern in &self.input_paths {
            if path_matches(path, in_pattern) {
                return true;
            }
        }
        // Heuristic: check for common input patterns
        path.contains("_in") || path.contains("input") || path.contains("_i[")
    }

    /// Estimate FIT contribution for a fault
    fn estimate_fit(&self, path: &str, fault_type: &FaultType) -> f64 {
        // Default FIT rates by primitive type (per ISO 26262 typical values)
        let base_fit = if path.contains("dff") || path.contains("reg") {
            0.5 // Flip-flop: 0.5 FIT
        } else if path.contains("mux") {
            0.2 // Mux: 0.2 FIT
        } else if path.contains("and")
            || path.contains("or")
            || path.contains("xor")
            || path.contains("xnor")
        {
            0.1 // Basic gate: 0.1 FIT
        } else if path.contains("adder") || path.contains("add_") {
            0.3 // Adder: 0.3 FIT
        } else {
            0.1 // Default: 0.1 FIT
        };

        // Fault type modifier
        let modifier = match fault_type {
            FaultType::StuckAt0 | FaultType::StuckAt1 => 1.0,
            FaultType::Transient => 0.5, // Transients are less likely
            FaultType::Open | FaultType::Bridging => 0.3,
            _ => 0.1,
        };

        base_fit * modifier
    }

    /// Identify potential common cause failures
    fn identify_common_cause(&self, classified: &mut [ClassifiedFault]) {
        // Group faults by primitive name (ignoring hierarchy)
        let mut by_primitive: HashMap<String, Vec<usize>> = HashMap::new();

        for (i, cf) in classified.iter().enumerate() {
            let prim_name = extract_primitive_name(&cf.fault_site.primitive_path.to_string());
            by_primitive.entry(prim_name).or_default().push(i);
        }

        // If same primitive name appears in multiple channels, mark as potential CCF
        for (prim_name, indices) in &by_primitive {
            if indices.len() >= 2 {
                // Check if they're in different channels
                let channels: HashSet<_> = indices
                    .iter()
                    .map(|&i| extract_channel(&classified[i].fault_site.primitive_path.to_string()))
                    .collect();

                if channels.len() >= 2 {
                    // Same primitive in multiple channels = CCF potential
                    for &i in indices {
                        classified[i].reason = UndetectedReason::CommonCausePotential;
                        classified[i].related_faults = indices
                            .iter()
                            .filter(|&&j| j != i)
                            .map(|&j| classified[j].fault_site.primitive_path.to_string())
                            .collect();
                    }
                }
            }
        }
    }
}

impl Default for FaultDiagnostics {
    fn default() -> Self {
        Self::new()
    }
}

/// Extract component name from path (e.g., "top.voter.TmrVoter.mux_0" -> "voter")
fn extract_component(path: &str) -> String {
    let parts: Vec<&str> = path.split('.').collect();
    if parts.len() >= 2 {
        // Skip "top" prefix
        let start = if parts[0] == "top" { 1 } else { 0 };
        if parts.len() > start {
            return parts[start].to_string();
        }
    }
    "top".to_string()
}

/// Extract primitive name (last component, stripped of indices)
fn extract_primitive_name(path: &str) -> String {
    let parts: Vec<&str> = path.split('.').collect();
    if let Some(last) = parts.last() {
        // Handle underscore-prefixed names (e.g., "_safe_state_buf" -> "buf")
        let trimmed = last.trim_start_matches('_');
        if trimmed.is_empty() {
            return last.to_string();
        }
        // Extract the type suffix (e.g., "_safe_state_buf" -> "buf", "mux_0" -> "mux")
        // For buffer names like "_foo_buf", get the last meaningful part
        let parts_inner: Vec<&str> = trimmed.split('_').collect();
        if parts_inner.len() > 1 {
            // Check if last part is a number (index) - if so, use second-to-last
            if let Some(last_part) = parts_inner.last() {
                if last_part.chars().all(|c| c.is_ascii_digit()) {
                    // It's an index, return the type before it
                    if parts_inner.len() > 1 {
                        return parts_inner[parts_inner.len() - 2].to_string();
                    }
                }
                // Otherwise return the last non-empty part (e.g., "buf" from "safe_state_buf")
                return last_part.to_string();
            }
        }
        // Single part - return it
        return trimmed.to_string();
    }
    path.to_string()
}

/// Extract channel identifier (e.g., "top.ch_a.counter" -> "ch_a")
fn extract_channel(path: &str) -> String {
    let parts: Vec<&str> = path.split('.').collect();
    for part in &parts {
        if part.starts_with("ch_") || part.starts_with("channel_") {
            return part.to_string();
        }
    }
    // Default: use second-level component
    if parts.len() >= 2 {
        let start = if parts[0] == "top" { 1 } else { 0 };
        if parts.len() > start {
            return parts[start].to_string();
        }
    }
    "unknown".to_string()
}

/// Simple glob-style path matching
fn path_matches(path: &str, pattern: &str) -> bool {
    if let Some(prefix) = pattern.strip_suffix('*') {
        path.starts_with(prefix)
    } else if let Some(suffix) = pattern.strip_prefix('*') {
        path.ends_with(suffix)
    } else if pattern.contains('*') {
        let parts: Vec<&str> = pattern.split('*').collect();
        if parts.len() == 2 {
            path.starts_with(parts[0]) && path.ends_with(parts[1])
        } else {
            path == pattern
        }
    } else {
        path == pattern || path.starts_with(&format!("{}.", pattern))
    }
}

// ============================================================================
// CCF Analysis from LIR Netlist
// ============================================================================

/// Analyze the LIR netlist for common cause failure sources
///
/// This traces the actual signal connectivity to identify:
/// 1. Shared input signals that feed multiple redundant channels
/// 2. Shared clock/reset signals
/// 3. Same design patterns replicated across channels
pub fn analyze_ccf(lir: &Lir, _channel_patterns: &[&str]) -> CcfAnalysis {
    let mut analysis = CcfAnalysis::default();

    // Build connectivity map: net_id -> primitives that use this net as input
    // (Since loads aren't populated during MIR-to-LIR, we build it here)
    let mut net_to_prims: HashMap<u32, Vec<&skalp_lir::Primitive>> = HashMap::new();
    for prim in &lir.primitives {
        // Track all input nets
        for input_net in &prim.inputs {
            net_to_prims.entry(input_net.0).or_default().push(prim);
        }
        // Track clock/reset connections
        if let Some(clk) = prim.clock {
            net_to_prims.entry(clk.0).or_default().push(prim);
        }
        if let Some(rst) = prim.reset {
            net_to_prims.entry(rst.0).or_default().push(prim);
        }
        if let Some(en) = prim.enable {
            net_to_prims.entry(en.0).or_default().push(prim);
        }
    }

    // Step 1: Find all primary inputs and analyze their fanout
    for net in &lir.nets {
        if !net.is_primary_input {
            continue;
        }

        // Get all primitives driven by this input using our connectivity map
        let driven_prims: Vec<String> = net_to_prims
            .get(&net.id.0)
            .map(|prims| prims.iter().map(|p| p.path.clone()).collect())
            .unwrap_or_default();

        if driven_prims.is_empty() {
            continue;
        }

        // Identify which channels this signal feeds
        let mut channels_fed: HashSet<String> = HashSet::new();
        for prim_path in &driven_prims {
            let channel = extract_channel(prim_path);
            if channel != "unknown" && channel != "top" {
                channels_fed.insert(channel);
            }
        }

        // Check if this is a clock or reset signal
        let is_clock = net.name.contains("clk")
            || net.name.contains("clock")
            || lir.clocks.iter().any(|c| c.0 == net.id.0);
        let is_reset = net.name.contains("rst")
            || net.name.contains("reset")
            || lir.resets.iter().any(|r| r.0 == net.id.0);

        // Only interested in signals that feed multiple channels
        if channels_fed.len() >= 2 {
            let channels_vec: Vec<String> = channels_fed.into_iter().collect();

            // Create SharedSignalInfo
            let shared_info = SharedSignalInfo {
                name: net.name.clone(),
                is_primary_input: true,
                is_clock,
                is_reset,
                feeds_channels: channels_vec.clone(),
                fanout: driven_prims.len(),
                driven_primitives: driven_prims.clone(),
            };

            analysis
                .shared_signals
                .insert(net.name.clone(), shared_info);

            // Create CcfSource based on type
            let (source_type, failure_modes) = if is_clock {
                (
                    CcfSourceType::SharedClock {
                        clock_name: net.name.clone(),
                        affected_channels: channels_vec.clone(),
                    },
                    vec![
                        "Clock stuck low → all channels frozen".to_string(),
                        "Clock stuck high → all channels frozen".to_string(),
                        "Clock glitch → all channels corrupt simultaneously".to_string(),
                    ],
                )
            } else if is_reset {
                (
                    CcfSourceType::SharedReset {
                        reset_name: net.name.clone(),
                        affected_channels: channels_vec.clone(),
                    },
                    vec![
                        "Reset stuck active → all channels held in reset".to_string(),
                        "Reset stuck inactive → all channels miss initialization".to_string(),
                        "Reset glitch → all channels reset simultaneously".to_string(),
                    ],
                )
            } else {
                (
                    CcfSourceType::SharedInput {
                        signal_name: net.name.clone(),
                        affected_channels: channels_vec.clone(),
                    },
                    vec![
                        format!(
                            "Signal '{}' stuck-at-0 → affects all {} channels",
                            net.name,
                            channels_vec.len()
                        ),
                        format!(
                            "Signal '{}' stuck-at-1 → affects all {} channels",
                            net.name,
                            channels_vec.len()
                        ),
                    ],
                )
            };

            // Estimate FIT impact (all primitives driven by this signal)
            let fit_impact: f64 = driven_prims
                .iter()
                .filter_map(|path| lir.primitives.iter().find(|p| &p.path == path))
                .map(|p| p.ptype.base_fit())
                .sum();

            let ccf_source = CcfSource {
                source_type,
                source_signal: net.name.clone(),
                affected_primitives: driven_prims,
                channels_affected: channels_vec.len(),
                fit_impact,
                failure_modes,
            };

            if ccf_source.source_type.risk_level() >= 4 {
                analysis.high_risk_sources += 1;
            }
            analysis.total_fit_at_risk += fit_impact;
            analysis.sources.push(ccf_source);
        }
    }

    // Step 2: Find replicated design patterns (same primitive instantiated in all channels)
    let mut prim_by_base_name: HashMap<String, Vec<&skalp_lir::Primitive>> = HashMap::new();
    for prim in &lir.primitives {
        // Extract base name (remove channel prefix and numeric suffix)
        let base = extract_primitive_base_name(&prim.path);
        prim_by_base_name.entry(base).or_default().push(prim);
    }

    for (base_name, prims) in &prim_by_base_name {
        if prims.len() < 2 {
            continue;
        }

        // Check if these primitives are in different channels
        let channels: HashSet<String> = prims
            .iter()
            .map(|p| extract_channel(&p.path))
            .filter(|c| c != "unknown" && c != "top")
            .collect();

        if channels.len() >= 2 {
            let instances: Vec<String> = prims.iter().map(|p| p.path.clone()).collect();
            let channels_vec: Vec<String> = channels.into_iter().collect();

            // Only add if not already covered by a shared input
            let already_covered = analysis.sources.iter().any(|s| {
                matches!(
                    &s.source_type,
                    CcfSourceType::SharedInput { .. }
                        | CcfSourceType::SharedClock { .. }
                        | CcfSourceType::SharedReset { .. }
                ) && s
                    .affected_primitives
                    .iter()
                    .any(|ap| instances.contains(ap))
            });

            if !already_covered && !base_name.is_empty() {
                let fit_impact: f64 = prims.iter().map(|p| p.ptype.base_fit()).sum();

                let ccf_source = CcfSource {
                    source_type: CcfSourceType::ReplicatedDesign {
                        pattern: base_name.clone(),
                        instances: instances.clone(),
                    },
                    source_signal: format!("design_pattern:{}", base_name),
                    affected_primitives: instances,
                    channels_affected: channels_vec.len(),
                    fit_impact,
                    failure_modes: vec![format!(
                        "Systematic design bug in '{}' affects all channels",
                        base_name
                    )],
                };

                analysis.sources.push(ccf_source);
            }
        }
    }

    analysis.total_ccf_sources = analysis.sources.len();

    // Sort by risk level (highest first)
    analysis.sources.sort_by(|a, b| {
        b.source_type
            .risk_level()
            .cmp(&a.source_type.risk_level())
            .then_with(|| {
                b.fit_impact
                    .partial_cmp(&a.fit_impact)
                    .unwrap_or(std::cmp::Ordering::Equal)
            })
    });

    analysis
}

/// Extract base name for pattern matching (removes channel and index suffixes)
fn extract_primitive_base_name(path: &str) -> String {
    let parts: Vec<&str> = path.split('.').collect();
    if parts.len() >= 3 {
        // Get the primitive name part
        if let Some(last) = parts.last() {
            // Remove numeric suffix but keep type (e.g., "dff_3" -> "dff")
            let base = last.split('_').next().unwrap_or(last);
            // Include module type for better matching
            if parts.len() >= 3 {
                let module_type = parts.get(parts.len() - 2).unwrap_or(&"");
                return format!("{}.{}", module_type, base);
            }
            return base.to_string();
        }
    }
    String::new()
}

// ============================================================================
// SM Diversity Analysis
// ============================================================================

/// Analyze the LIR netlist for identical SM replication patterns
///
/// This detects when:
/// 1. Multiple SM instances are of the same entity type (e.g., two TmrVoter instances)
/// 2. These identical SMs are being compared by a comparator
/// 3. This defeats the comparison because identical implementations produce identical wrong outputs
pub fn analyze_sm_diversity(lir: &Lir) -> SmDiversityAnalysis {
    let mut analysis = SmDiversityAnalysis::default();

    // Step 1: Identify all SM instances by looking for SM-related names
    // Group primitives by their parent module instance
    let mut instances_by_type: HashMap<String, Vec<String>> = HashMap::new();
    let mut primitives_by_instance: HashMap<String, Vec<&skalp_lir::Primitive>> = HashMap::new();

    for prim in &lir.primitives {
        // Extract the instance path (e.g., "top.voter_a.TmrVoter" from "top.voter_a.TmrVoter.mux_0")
        let parts: Vec<&str> = prim.path.split('.').collect();
        if parts.len() >= 3 {
            // Get instance path (everything except the primitive name)
            let instance_path = parts[..parts.len() - 1].join(".");
            // Get entity type (second to last part, e.g., "TmrVoter")
            let entity_type = parts[parts.len() - 2].to_string();

            // Only track SM-like instances
            if is_safety_mechanism_type(&entity_type) || is_safety_mechanism_type(&instance_path) {
                instances_by_type
                    .entry(entity_type.clone())
                    .or_default()
                    .push(instance_path.clone());
                primitives_by_instance
                    .entry(instance_path)
                    .or_default()
                    .push(prim);
            }
        }
    }

    // Deduplicate instance lists
    for instances in instances_by_type.values_mut() {
        instances.sort();
        instances.dedup();
    }

    // Step 2: Find identical SM pairs (same entity type, multiple instances)
    for (entity_type, instances) in &instances_by_type {
        if instances.len() < 2 {
            continue;
        }

        // Check if there's a comparator that compares these instances
        let has_comparator = check_for_comparator(lir, instances);

        // Generate pairs
        for i in 0..instances.len() {
            for j in (i + 1)..instances.len() {
                let instance_a = &instances[i];
                let instance_b = &instances[j];

                // Find matching primitives in both instances
                let prims_a = primitives_by_instance.get(instance_a);
                let prims_b = primitives_by_instance.get(instance_b);

                let mut matching_primitives = Vec::new();
                let mut identical_failure_fit = 0.0;

                if let (Some(prims_a), Some(prims_b)) = (prims_a, prims_b) {
                    // Create a map of relative paths for instance B
                    let b_relative: HashMap<String, &skalp_lir::Primitive> = prims_b
                        .iter()
                        .map(|p| {
                            let rel = p.path.strip_prefix(instance_b).unwrap_or(&p.path);
                            let rel = rel.strip_prefix('.').unwrap_or(rel);
                            (rel.to_string(), *p)
                        })
                        .collect();

                    // Find matching primitives
                    for prim_a in prims_a {
                        let rel_a = prim_a.path.strip_prefix(instance_a).unwrap_or(&prim_a.path);
                        let rel_a = rel_a.strip_prefix('.').unwrap_or(rel_a);

                        if let Some(prim_b) = b_relative.get(rel_a) {
                            // This primitive exists in both instances with same relative path
                            // A fault here will produce identical wrong outputs in both!
                            matching_primitives.push(MatchingPrimitivePair {
                                path_a: prim_a.path.clone(),
                                path_b: prim_b.path.clone(),
                                primitive_type: format!("{:?}", prim_a.ptype),
                                relative_path: rel_a.to_string(),
                            });
                            identical_failure_fit += prim_a.ptype.base_fit();
                        }
                    }
                }

                if !matching_primitives.is_empty() {
                    let pair = IdenticalSmPair {
                        instance_a: instance_a.clone(),
                        instance_b: instance_b.clone(),
                        entity_type: entity_type.clone(),
                        outputs_compared: has_comparator,
                        matching_primitives,
                        identical_failure_fit,
                    };

                    analysis.fit_at_risk += identical_failure_fit;
                    analysis.identical_sm_pairs.push(pair);
                }
            }
        }
    }

    // Step 3: Generate warnings
    for pair in &analysis.identical_sm_pairs {
        if pair.outputs_compared {
            // Critical warning: identical SMs with comparator
            analysis.warnings.push(SmDiversityWarning {
                severity: WarningSeverity::Critical,
                message: format!(
                    "Identical SM instances '{}' and '{}' defeat comparison-based detection",
                    extract_instance_name(&pair.instance_a),
                    extract_instance_name(&pair.instance_b)
                ),
                explanation: format!(
                    "Both instances are of type '{}' with identical implementations. \
                     A fault in '{}' (e.g., mux stuck-at) will cause the same wrong output \
                     in BOTH instances. The comparator will see matching outputs and NOT detect the fault. \
                     This affects {} primitives with {:.2} FIT at risk.",
                    pair.entity_type,
                    pair.instance_a,
                    pair.matching_primitives.len(),
                    pair.identical_failure_fit
                ),
                affected_instances: vec![pair.instance_a.clone(), pair.instance_b.clone()],
                iso_reference: "ISO 26262-9:7.4.3 (Diversity requirements for SM-of-SM)".to_string(),
                recommendation: "Replace one instance with a DIVERSE implementation using a different algorithm. \
                     For voters: use conditional logic in one, bitwise majority in the other. \
                     Example: TmrVoterLogical vs TmrVoterBitwise.".to_string(),
            });
            analysis.has_comparator = true;
        } else {
            // Medium warning: identical SMs without comparator (redundant but not compared)
            analysis.warnings.push(SmDiversityWarning {
                severity: WarningSeverity::Medium,
                message: format!(
                    "Identical SM instances '{}' and '{}' share same implementation",
                    extract_instance_name(&pair.instance_a),
                    extract_instance_name(&pair.instance_b)
                ),
                explanation: format!(
                    "Both instances are of type '{}'. While not currently compared, \
                     this represents potential systematic failure vulnerability.",
                    pair.entity_type
                ),
                affected_instances: vec![pair.instance_a.clone(), pair.instance_b.clone()],
                iso_reference: "ISO 26262-9:7.4.3 (Systematic failures)".to_string(),
                recommendation: "Consider using diverse implementations for ASIL C/D requirements."
                    .to_string(),
            });
        }
    }

    // Calculate diversity score
    // 0.0 = all SMs are identical, 1.0 = all SMs are diverse
    let total_sm_instances: usize = instances_by_type.values().map(|v| v.len()).sum();
    let identical_pairs = analysis.identical_sm_pairs.len();
    if total_sm_instances > 1 && identical_pairs > 0 {
        // Penalize heavily for identical pairs with comparator
        let critical_pairs = analysis
            .identical_sm_pairs
            .iter()
            .filter(|p| p.outputs_compared)
            .count();
        analysis.diversity_score = if critical_pairs > 0 {
            0.0 // No diversity credit if identical SMs are being compared
        } else {
            0.5 // Partial credit if not compared
        };
    } else {
        analysis.diversity_score = 1.0; // Full diversity (no identical pairs)
    }

    analysis
}

/// Check if a path represents a safety mechanism type
fn is_safety_mechanism_type(name: &str) -> bool {
    let lower = name.to_lowercase();
    lower.contains("voter")
        || lower.contains("tmr")
        || lower.contains("dmr")
        || lower.contains("watchdog")
        || lower.contains("checker")
        || lower.contains("comparator")
        || lower.contains("monitor")
        || lower.contains("ecc")
        || lower.contains("crc")
        || lower.contains("parity")
}

/// Check if there's a comparator that compares the outputs of the given instances
fn check_for_comparator(lir: &Lir, instances: &[String]) -> bool {
    // Look for comparator-like components in the design
    for prim in &lir.primitives {
        let lower_path = prim.path.to_lowercase();
        if lower_path.contains("comparator")
            || lower_path.contains("checker")
            || lower_path.contains("voter_check")
            || lower_path.contains("sm_check")
        {
            return true;
        }
    }

    // Also check for eq/xnor primitives that might be comparing voter outputs
    // This is a heuristic - if we see eq primitives at the top level comparing
    // signals that include instance names, it's likely a comparison
    for prim in &lir.primitives {
        if prim.path.contains("eq") || prim.path.contains("xnor") {
            // Check if this is at a level that suggests output comparison
            let parts: Vec<&str> = prim.path.split('.').collect();
            if parts.len() <= 3 {
                // Top-level comparison
                return true;
            }
        }
    }

    false
}

/// Extract the instance name from a full path
fn extract_instance_name(path: &str) -> String {
    let parts: Vec<&str> = path.split('.').collect();
    if parts.len() >= 2 {
        // Return "voter_a" from "top.voter_a.TmrVoter"
        let start = if parts[0] == "top" { 1 } else { 0 };
        if parts.len() > start {
            return parts[start].to_string();
        }
    }
    path.to_string()
}

/// Generate SM diversity analysis report section
pub fn generate_sm_diversity_report(analysis: &SmDiversityAnalysis) -> String {
    let mut report = String::new();

    if analysis.identical_sm_pairs.is_empty() && analysis.warnings.is_empty() {
        report.push_str("\n## Safety Mechanism Diversity Analysis\n\n");
        report.push_str(
            "✅ No identical SM replication detected. SM diversity requirements satisfied.\n\n",
        );
        return report;
    }

    report.push_str("\n## Safety Mechanism Diversity Analysis\n\n");
    report.push_str("Analysis of SM implementation diversity to detect patterns that defeat comparison-based detection.\n\n");

    // Summary
    report.push_str("### Diversity Summary\n\n");
    report.push_str("| Metric | Value | Status |\n");
    report.push_str("|--------|-------|--------|\n");
    report.push_str(&format!(
        "| Diversity Score | {:.0}% | {} |\n",
        analysis.diversity_score * 100.0,
        if analysis.diversity_score >= 0.9 {
            "✅ GOOD"
        } else if analysis.diversity_score >= 0.5 {
            "⚠️ PARTIAL"
        } else {
            "❌ INSUFFICIENT"
        }
    ));
    report.push_str(&format!(
        "| Identical SM Pairs | {} | {} |\n",
        analysis.identical_sm_pairs.len(),
        if analysis.identical_sm_pairs.is_empty() {
            "✅"
        } else {
            "⚠️"
        }
    ));
    report.push_str(&format!(
        "| FIT at Risk | {:.2} FIT | {} |\n",
        analysis.fit_at_risk,
        if analysis.fit_at_risk < 1.0 {
            "✅"
        } else {
            "❌"
        }
    ));

    // Critical warnings first
    let critical_warnings: Vec<_> = analysis
        .warnings
        .iter()
        .filter(|w| w.severity == WarningSeverity::Critical)
        .collect();

    if !critical_warnings.is_empty() {
        report.push_str("\n### 🔴 CRITICAL: Identical SM Replication Detected\n\n");
        report.push_str(
            "**These identical SM instances DEFEAT comparison-based fault detection!**\n\n",
        );

        for warning in critical_warnings {
            report.push_str(&format!(
                "#### {} {}\n\n",
                warning.severity.emoji(),
                warning.message
            ));
            report.push_str(&format!("**Problem**: {}\n\n", warning.explanation));
            report.push_str(&format!("**ISO Reference**: {}\n\n", warning.iso_reference));
            report.push_str(&format!("**Fix**: {}\n\n", warning.recommendation));
        }
    }

    // Identical SM pair details
    if !analysis.identical_sm_pairs.is_empty() {
        report.push_str("\n### Identical SM Instance Details\n\n");

        for pair in &analysis.identical_sm_pairs {
            report.push_str(&format!(
                "#### `{}` ↔ `{}` (Type: {})\n\n",
                extract_instance_name(&pair.instance_a),
                extract_instance_name(&pair.instance_b),
                pair.entity_type
            ));

            report.push_str(&format!(
                "- **Outputs Compared**: {}\n",
                if pair.outputs_compared {
                    "Yes (defeats detection!)"
                } else {
                    "No"
                }
            ));
            report.push_str(&format!(
                "- **Matching Primitives**: {} (identical implementation)\n",
                pair.matching_primitives.len()
            ));
            report.push_str(&format!(
                "- **FIT at Risk**: {:.2} FIT\n\n",
                pair.identical_failure_fit
            ));

            if pair.outputs_compared {
                report.push_str("**Why this defeats detection**:\n");
                report.push_str("```\n");
                report.push_str(&format!(
                    "Fault in {}.mux_X (stuck-at-1)\n",
                    pair.instance_a
                ));
                report.push_str(&format!(
                    "  → {} produces wrong output Y\n",
                    pair.instance_a
                ));
                report.push_str(&format!(
                    "  → {} produces SAME wrong output Y (identical logic!)\n",
                    pair.instance_b
                ));
                report.push_str("  → Comparator sees Y == Y → NO FAULT DETECTED\n");
                report.push_str("```\n\n");
            }

            // Show some matching primitives
            report.push_str(
                "**Sample matching primitives** (same relative path in both instances):\n\n",
            );
            report.push_str("| Relative Path | Type | Instance A | Instance B |\n");
            report.push_str("|---------------|------|------------|------------|\n");
            for mp in pair.matching_primitives.iter().take(5) {
                report.push_str(&format!(
                    "| `{}` | {} | `{}` | `{}` |\n",
                    mp.relative_path,
                    mp.primitive_type,
                    mp.path_a.split('.').next_back().unwrap_or(&mp.path_a),
                    mp.path_b.split('.').next_back().unwrap_or(&mp.path_b)
                ));
            }
            if pair.matching_primitives.len() > 5 {
                report.push_str(&format!(
                    "| ... | | ({} more) | |\n",
                    pair.matching_primitives.len() - 5
                ));
            }
            report.push('\n');
        }
    }

    // Other warnings
    let other_warnings: Vec<_> = analysis
        .warnings
        .iter()
        .filter(|w| w.severity != WarningSeverity::Critical)
        .collect();

    if !other_warnings.is_empty() {
        report.push_str("\n### Other Diversity Warnings\n\n");
        for warning in other_warnings {
            report.push_str(&format!(
                "- {} **{}**: {}\n",
                warning.severity.emoji(),
                warning.severity.name(),
                warning.message
            ));
        }
    }

    report
}

/// Generate CCF analysis report section
pub fn generate_ccf_report(analysis: &CcfAnalysis) -> String {
    let mut report = String::new();

    if analysis.sources.is_empty() {
        report.push_str("\n## Common Cause Failure Analysis\n\n");
        report.push_str("No significant CCF sources identified.\n\n");
        return report;
    }

    report.push_str("\n## Common Cause Failure Analysis\n\n");
    report.push_str("Analysis of signals and patterns that could cause simultaneous failures across redundant channels.\n\n");

    // Summary
    report.push_str("### CCF Summary\n\n");
    report.push_str("| Metric | Value |\n");
    report.push_str("|--------|-------|\n");
    report.push_str(&format!(
        "| Total CCF Sources | {} |\n",
        analysis.total_ccf_sources
    ));
    report.push_str(&format!(
        "| High Risk Sources (≥4) | {} |\n",
        analysis.high_risk_sources
    ));
    report.push_str(&format!(
        "| Total FIT at Risk | {:.2} FIT |\n",
        analysis.total_fit_at_risk
    ));

    // Shared Signals Table
    if !analysis.shared_signals.is_empty() {
        report.push_str("\n### Shared Input Signals\n\n");
        report.push_str("These primary inputs feed multiple redundant channels. A fault on any of these signals affects ALL channels simultaneously.\n\n");
        report.push_str("| Signal | Type | Channels | Fanout | Risk |\n");
        report.push_str("|--------|------|----------|--------|------|\n");

        let mut signals: Vec<_> = analysis.shared_signals.values().collect();
        signals.sort_by(|a, b| b.fanout.cmp(&a.fanout));

        for sig in signals {
            let sig_type = if sig.is_clock {
                "🕐 Clock"
            } else if sig.is_reset {
                "🔄 Reset"
            } else {
                "📥 Input"
            };

            let risk = if sig.is_clock {
                "⚠️ CRITICAL"
            } else if sig.is_reset || sig.feeds_channels.len() >= 3 {
                "⚠️ HIGH"
            } else {
                "Medium"
            };

            report.push_str(&format!(
                "| `{}` | {} | {} | {} | {} |\n",
                sig.name,
                sig_type,
                sig.feeds_channels.join(", "),
                sig.fanout,
                risk
            ));
        }
    }

    // Detailed CCF Sources
    report.push_str("\n### CCF Source Details\n\n");

    for (i, source) in analysis.sources.iter().enumerate() {
        let risk_emoji = match source.source_type.risk_level() {
            5 => "🔴",
            4 => "🟠",
            3 => "🟡",
            _ => "🟢",
        };

        report.push_str(&format!(
            "#### {}. {} `{}` (Risk: {}/5)\n\n",
            i + 1,
            risk_emoji,
            source.source_signal,
            source.source_type.risk_level()
        ));

        report.push_str(&format!("**Type**: {}\n\n", source.source_type.name()));
        report.push_str(&format!(
            "**ISO Reference**: {}\n\n",
            source.source_type.iso_reference()
        ));
        report.push_str(&format!(
            "**Channels Affected**: {}\n\n",
            source.channels_affected
        ));
        report.push_str(&format!("**FIT Impact**: {:.2} FIT\n\n", source.fit_impact));

        report.push_str("**Failure Modes**:\n");
        for fm in &source.failure_modes {
            report.push_str(&format!("- {}\n", fm));
        }

        report.push_str(&format!(
            "\n**Affected Primitives** ({}):\n",
            source.affected_primitives.len()
        ));
        for prim in source.affected_primitives.iter().take(5) {
            report.push_str(&format!("- `{}`\n", prim));
        }
        if source.affected_primitives.len() > 5 {
            report.push_str(&format!(
                "- ... and {} more\n",
                source.affected_primitives.len() - 5
            ));
        }

        report.push_str(&format!(
            "\n**Recommended Mitigation**: {}\n\n",
            source.source_type.mitigation()
        ));
    }

    report
}

/// Generate enhanced diagnostic report
pub fn generate_diagnostic_report(
    classified: &[ClassifiedFault],
    summary: &FaultClassificationSummary,
    total_undetected: usize,
) -> String {
    let mut report = String::new();

    report.push_str("## Enhanced Fault Diagnostics\n\n");
    report.push_str("This section provides detailed analysis of WHY faults are not detected.\n\n");

    // Summary by reason
    report.push_str("### Classification Summary\n\n");
    report.push_str(
        "| Classification | Count | % of Undetected | FIT Contribution | Recommendation |\n",
    );
    report.push_str(
        "|----------------|-------|-----------------|------------------|----------------|\n",
    );

    let mut reasons: Vec<_> = summary.by_reason.iter().collect();
    reasons.sort_by(|a, b| b.1.cmp(a.1));

    for (reason, count) in reasons {
        let pct = (*count as f64 / total_undetected as f64) * 100.0;
        let fit = summary.fit_by_reason.get(reason).copied().unwrap_or(0.0);
        report.push_str(&format!(
            "| {} | {} | {:.1}% | {:.2} FIT | {} |\n",
            reason.name(),
            count,
            pct,
            fit,
            reason.recommendation()
        ));
    }

    // Top PMHF contributors
    if !summary.top_pmhf_contributors.is_empty() {
        report.push_str("\n### Top PMHF Contributors\n\n");
        report.push_str(
            "These faults contribute most to the Probabilistic Metric for Hardware Failures:\n\n",
        );
        report.push_str("| Rank | Fault Site | FIT Contribution |\n");
        report.push_str("|------|------------|------------------|\n");

        for (i, (path, fit)) in summary.top_pmhf_contributors.iter().enumerate() {
            report.push_str(&format!("| {} | `{}` | {:.3} FIT |\n", i + 1, path, fit));
        }
    }

    // Detailed breakdown by classification
    report.push_str("\n### Detailed Breakdown by Classification\n\n");

    let mut by_reason: HashMap<UndetectedReason, Vec<&ClassifiedFault>> = HashMap::new();
    for cf in classified {
        by_reason.entry(cf.reason).or_default().push(cf);
    }

    for reason in [
        UndetectedReason::SharedInputCcf,
        UndetectedReason::ReplicatedDesignCcf,
        UndetectedReason::IdenticalSmReplication,
        UndetectedReason::SmInternal,
        UndetectedReason::OutputPath,
        UndetectedReason::InputPath,
        UndetectedReason::CommonCausePotential,
        UndetectedReason::CoverageGap,
    ] {
        if let Some(faults) = by_reason.get(&reason) {
            if !faults.is_empty() {
                report.push_str(&format!(
                    "\n#### {} ({} faults)\n\n",
                    reason.name(),
                    faults.len()
                ));
                report.push_str(&format!(
                    "**ISO Reference**: {}\n\n",
                    reason.iso_reference()
                ));
                report.push_str(&format!(
                    "**Recommendation**: {}\n\n",
                    reason.recommendation()
                ));

                report.push_str("| Component | Fault Site | Type | FIT |\n");
                report.push_str("|-----------|------------|------|-----|\n");

                for cf in faults.iter().take(10) {
                    report.push_str(&format!(
                        "| {} | `{}` | {} | {:.3} |\n",
                        cf.component,
                        cf.fault_site.primitive_path,
                        cf.fault_site.fault_type.name(),
                        cf.fit_contribution
                    ));
                }

                if faults.len() > 10 {
                    report.push_str(&format!("| ... | ({} more) | | |\n", faults.len() - 10));
                }
            }
        }
    }

    // Common cause analysis
    let ccf_faults: Vec<_> = classified
        .iter()
        .filter(|cf| cf.reason == UndetectedReason::CommonCausePotential)
        .collect();

    if !ccf_faults.is_empty() {
        report.push_str("\n### Common Cause Failure Analysis\n\n");
        report.push_str(
            "The following faults affect identical primitives across redundant channels,\n",
        );
        report.push_str("suggesting potential common cause failure vulnerability:\n\n");

        let mut groups: HashMap<String, Vec<&ClassifiedFault>> = HashMap::new();
        for cf in &ccf_faults {
            let prim = extract_primitive_name(&cf.fault_site.primitive_path.to_string());
            groups.entry(prim).or_default().push(cf);
        }

        for (prim, faults) in groups {
            if faults.len() >= 2 {
                report.push_str(&format!(
                    "**Primitive `{}`** (affects {} channels):\n",
                    prim,
                    faults.len()
                ));
                for cf in faults {
                    report.push_str(&format!("- `{}`\n", cf.fault_site.primitive_path));
                }
                report.push('\n');
            }
        }
    }

    // Action items
    report.push_str("\n### Recommended Actions (Priority Order)\n\n");

    let mut actions: Vec<(UndetectedReason, usize, f64)> = summary
        .by_reason
        .iter()
        .map(|(reason, count)| {
            let fit = summary.fit_by_reason.get(reason).copied().unwrap_or(0.0);
            (*reason, *count, fit)
        })
        .collect();
    actions.sort_by(|a, b| b.2.partial_cmp(&a.2).unwrap_or(std::cmp::Ordering::Equal));

    for (i, (reason, count, fit)) in actions.iter().enumerate() {
        if *count > 0 {
            report.push_str(&format!(
                "{}. **{}** ({} faults, {:.2} FIT): {}\n",
                i + 1,
                reason.name(),
                count,
                fit,
                reason.recommendation()
            ));
        }
    }

    report
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hierarchy::DesignRef;

    #[test]
    fn test_extract_component() {
        assert_eq!(extract_component("top.voter.TmrVoter.mux_0"), "voter");
        assert_eq!(extract_component("top.ch_a.counter"), "ch_a");
        assert_eq!(extract_component("top.assign_0"), "assign_0");
    }

    #[test]
    fn test_extract_primitive_name() {
        assert_eq!(extract_primitive_name("top.voter.mux_0"), "mux");
        assert_eq!(extract_primitive_name("top.ch_a.dff_10"), "dff");
        // Test underscore-prefixed buffer names
        assert_eq!(
            extract_primitive_name("top.safe_ctrl._safe_state_buf"),
            "buf"
        );
        assert_eq!(extract_primitive_name("top.bist._bist_pass_buf"), "buf");
        assert_eq!(extract_primitive_name("top.cmd_proc._clk_buf"), "buf");
        // Test regular assign
        assert_eq!(
            extract_primitive_name("top.cmd_proc.CommandProcessor.assign_20"),
            "assign"
        );
    }

    #[test]
    fn test_extract_channel() {
        assert_eq!(extract_channel("top.ch_a.counter.dff_0"), "ch_a");
        assert_eq!(extract_channel("top.ch_b.counter.dff_0"), "ch_b");
        assert_eq!(extract_channel("top.voter.mux_0"), "voter");
    }

    #[test]
    fn test_path_matches() {
        assert!(path_matches("top.voter.mux_0", "top.voter*"));
        assert!(path_matches("top.ch_a.counter", "*.counter"));
        assert!(!path_matches("top.ch_a.adder", "*.counter"));
    }

    #[test]
    fn test_classify_sm_fault() {
        let diagnostics = FaultDiagnostics::new();
        let fault = FaultSite::new(
            DesignRef::parse("top.voter.TmrVoter.mux_0"),
            FaultType::StuckAt0,
        );
        let classified = diagnostics.classify_fault(&fault);
        assert_eq!(classified.reason, UndetectedReason::SmInternal);
    }

    #[test]
    fn test_undetected_reason_recommendations() {
        assert!(UndetectedReason::SmInternal
            .recommendation()
            .contains("SM-of-SM"));
        assert!(UndetectedReason::OutputPath
            .recommendation()
            .contains("output"));
        assert!(UndetectedReason::CommonCausePotential
            .recommendation()
            .contains("diversity"));
    }
}
