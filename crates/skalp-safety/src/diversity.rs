//! Diversity Analysis Module for ISO 26262 Common Cause Failure Prevention
//!
//! Implements diversity analysis and single-point-of-failure detection for ASIL C/D compliance.
//! Per ISO 26262-9:7, analysis of dependent failures (common cause failures) is required.
//!
//! # ISO 26262 Context
//!
//! - ISO 26262-9:7 - Analysis of dependent failures
//! - ISO 26262-5:7.4.2 - Evaluation of safety goal violations
//! - ISO 26262-5:D.4 - Independence of elements
//!
//! # Key Concepts
//!
//! - **Diversity**: Using different methods/technologies to reduce common cause failures
//! - **Single Point of Failure (SPF)**: A failure that directly violates a safety goal
//! - **Common Cause Failure (CCF)**: A failure affecting multiple elements due to shared cause

use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

use crate::design_rules::DiversityAspect;

// ============================================================================
// Diversity Analysis Types
// ============================================================================

/// Complete diversity analysis for a design
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiversityAnalysis {
    /// Pairs of redundant channels analyzed
    pub redundant_pairs: Vec<RedundantPair>,
    /// Diversity matrix showing diversity between all channels
    pub diversity_matrix: DiversityMatrix,
    /// Elements that are common across channels (potential CCF sources)
    pub common_elements: Vec<CommonElement>,
    /// Overall diversity score (0.0 - 1.0)
    pub overall_score: f64,
    /// Analysis summary
    pub summary: DiversitySummary,
}

impl DiversityAnalysis {
    /// Create a new empty analysis
    pub fn new() -> Self {
        Self {
            redundant_pairs: Vec::new(),
            diversity_matrix: DiversityMatrix::new(),
            common_elements: Vec::new(),
            overall_score: 0.0,
            summary: DiversitySummary::default(),
        }
    }

    /// Check if diversity requirements are met
    pub fn requirements_met(&self, required_aspects: &[DiversityAspect]) -> bool {
        for pair in &self.redundant_pairs {
            let achieved: HashSet<_> = pair
                .diversity_aspects
                .iter()
                .filter(|(_, evidence)| evidence.verified)
                .map(|(aspect, _)| std::mem::discriminant(aspect))
                .collect();

            for required in required_aspects {
                if !achieved.contains(&std::mem::discriminant(required)) {
                    return false;
                }
            }
        }
        true
    }
}

impl Default for DiversityAnalysis {
    fn default() -> Self {
        Self::new()
    }
}

/// A pair of redundant channels being analyzed
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RedundantPair {
    /// First channel identifier
    pub channel_a: String,
    /// Second channel identifier
    pub channel_b: String,
    /// Diversity aspects achieved with evidence
    pub diversity_aspects: Vec<(DiversityAspect, DiversityEvidence)>,
    /// Components shared between channels (should be minimal)
    pub shared_components: Vec<String>,
    /// Diversity score for this pair (0.0 - 1.0)
    pub pair_score: f64,
}

impl RedundantPair {
    /// Create a new redundant pair
    pub fn new(channel_a: String, channel_b: String) -> Self {
        Self {
            channel_a,
            channel_b,
            diversity_aspects: Vec::new(),
            shared_components: Vec::new(),
            pair_score: 0.0,
        }
    }

    /// Add diversity evidence for an aspect
    pub fn add_evidence(&mut self, aspect: DiversityAspect, evidence: DiversityEvidence) {
        self.diversity_aspects.push((aspect, evidence));
        self.recalculate_score();
    }

    /// Calculate pair diversity score based on evidence
    fn recalculate_score(&mut self) {
        if self.diversity_aspects.is_empty() {
            self.pair_score = 0.0;
            return;
        }

        let verified_count = self
            .diversity_aspects
            .iter()
            .filter(|(_, e)| e.verified)
            .count();
        let total = self.diversity_aspects.len();

        // Base score from verified aspects
        let base_score = verified_count as f64 / total as f64;

        // Penalty for shared components
        let shared_penalty = (self.shared_components.len() as f64 * 0.05).min(0.3);

        self.pair_score = (base_score - shared_penalty).max(0.0);
    }
}

/// Evidence supporting a diversity claim
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiversityEvidence {
    /// Type of evidence provided
    pub evidence_type: EvidenceType,
    /// Description of the diversity
    pub description: String,
    /// Has this been verified/reviewed
    pub verified: bool,
    /// Verification method used
    pub verification_method: Option<String>,
    /// Supporting document references
    pub document_refs: Vec<String>,
}

impl DiversityEvidence {
    /// Create new evidence
    pub fn new(evidence_type: EvidenceType, description: String) -> Self {
        Self {
            evidence_type,
            description,
            verified: false,
            verification_method: None,
            document_refs: Vec::new(),
        }
    }

    /// Mark evidence as verified
    pub fn verify(&mut self, method: &str) {
        self.verified = true;
        self.verification_method = Some(method.to_string());
    }
}

/// Type of evidence for diversity claims
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum EvidenceType {
    /// Design documentation states diversity
    DesignDocument,
    /// Automated analysis detected diversity
    AutomatedAnalysis,
    /// Manual review confirmed diversity
    ManualReview,
    /// Test results demonstrate diversity
    TestResults,
    /// Process records (different teams, tools)
    ProcessRecords,
    /// Physical measurement/inspection
    PhysicalInspection,
}

/// Matrix showing diversity between all channel pairs
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiversityMatrix {
    /// Channel names (row/column headers)
    pub channels: Vec<String>,
    /// Matrix entries [row][col] containing diversity info
    pub entries: Vec<Vec<DiversityMatrixEntry>>,
}

impl DiversityMatrix {
    /// Create empty matrix
    pub fn new() -> Self {
        Self {
            channels: Vec::new(),
            entries: Vec::new(),
        }
    }

    /// Create matrix for given channels
    pub fn with_channels(channels: Vec<String>) -> Self {
        let n = channels.len();
        let entries = vec![vec![DiversityMatrixEntry::default(); n]; n];
        Self { channels, entries }
    }

    /// Get entry for channel pair
    pub fn get(&self, channel_a: &str, channel_b: &str) -> Option<&DiversityMatrixEntry> {
        let i = self.channels.iter().position(|c| c == channel_a)?;
        let j = self.channels.iter().position(|c| c == channel_b)?;
        self.entries.get(i).and_then(|row| row.get(j))
    }

    /// Set entry for channel pair (symmetric)
    pub fn set(&mut self, channel_a: &str, channel_b: &str, entry: DiversityMatrixEntry) {
        if let (Some(i), Some(j)) = (
            self.channels.iter().position(|c| c == channel_a),
            self.channels.iter().position(|c| c == channel_b),
        ) {
            self.entries[i][j] = entry.clone();
            self.entries[j][i] = entry;
        }
    }
}

impl Default for DiversityMatrix {
    fn default() -> Self {
        Self::new()
    }
}

/// Entry in diversity matrix
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DiversityMatrixEntry {
    /// Diversity score between these channels (0.0 - 1.0)
    pub score: f64,
    /// Diversity aspects achieved
    pub aspects: Vec<DiversityAspect>,
    /// Number of shared elements
    pub shared_count: usize,
}

/// An element that is common across multiple channels
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CommonElement {
    /// Element identifier (path or name)
    pub element: String,
    /// Channels that share this element
    pub shared_by: Vec<String>,
    /// Type of shared element
    pub element_type: CommonElementType,
    /// Potential failure modes that could affect all channels
    pub failure_modes: Vec<String>,
    /// Risk assessment for this common element
    pub risk_level: RiskLevel,
    /// Mitigation if any
    pub mitigation: Option<String>,
}

/// Type of common/shared element
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CommonElementType {
    /// Shared clock source
    Clock,
    /// Shared reset signal
    Reset,
    /// Shared power supply
    PowerSupply,
    /// Shared interconnect/bus
    Interconnect,
    /// Shared memory
    Memory,
    /// Shared IO
    InputOutput,
    /// Shared logic
    Logic,
    /// External interface
    ExternalInterface,
}

/// Risk level for common elements
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum RiskLevel {
    /// Low risk - acceptable
    Low,
    /// Medium risk - should be reviewed
    Medium,
    /// High risk - requires mitigation
    High,
    /// Critical risk - must be addressed
    Critical,
}

/// Summary of diversity analysis
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DiversitySummary {
    /// Total number of redundant pairs analyzed
    pub pairs_analyzed: usize,
    /// Number of pairs meeting diversity requirements
    pub pairs_compliant: usize,
    /// Total common elements identified
    pub common_elements_count: usize,
    /// High/critical risk common elements
    pub high_risk_elements: usize,
    /// Recommendations for improvement
    pub recommendations: Vec<String>,
}

// ============================================================================
// Single Point of Failure Analysis
// ============================================================================

/// Complete single point of failure analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SinglePointAnalysis {
    /// Potential single points of failure identified
    pub potential_spfs: Vec<PotentialSpf>,
    /// Paths that are protected by safety mechanisms
    pub protected_paths: Vec<ProtectedPath>,
    /// Single Point Fault Metric (SPFM)
    pub spf_metric: f64,
    /// Summary statistics
    pub summary: SpfSummary,
}

impl SinglePointAnalysis {
    /// Create new empty analysis
    pub fn new() -> Self {
        Self {
            potential_spfs: Vec::new(),
            protected_paths: Vec::new(),
            spf_metric: 0.0,
            summary: SpfSummary::default(),
        }
    }

    /// Calculate SPFM based on analysis
    ///
    /// SPFM = 1 - (SPF_FIT / Total_FIT)
    /// Where SPF_FIT is the FIT rate of unprotected single points
    pub fn calculate_spfm(&mut self, total_fit: f64) {
        let spf_fit: f64 = self
            .potential_spfs
            .iter()
            .filter(|spf| spf.protected_by.is_none())
            .map(|spf| spf.fit_contribution)
            .sum();

        if total_fit > 0.0 {
            self.spf_metric = 1.0 - (spf_fit / total_fit);
        } else {
            self.spf_metric = 1.0;
        }
    }

    /// Check if SPFM meets ASIL requirements
    pub fn meets_asil_requirement(&self, asil: &str) -> bool {
        let required = match asil.to_uppercase().as_str() {
            "D" => 0.99,
            "C" => 0.97,
            "B" => 0.90,
            "A" => 0.0, // No specific requirement
            _ => 0.0,
        };
        self.spf_metric >= required
    }
}

impl Default for SinglePointAnalysis {
    fn default() -> Self {
        Self::new()
    }
}

/// A potential single point of failure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PotentialSpf {
    /// Cell/element identifier
    pub cell: String,
    /// Failure mode that makes this an SPF
    pub failure_mode: SpfFailureMode,
    /// Path from this cell to safety-relevant output
    pub path_to_output: Vec<String>,
    /// Safety mechanism protecting this SPF (if any)
    pub protected_by: Option<String>,
    /// FIT contribution of this cell
    pub fit_contribution: f64,
    /// Additional notes
    pub notes: Option<String>,
}

impl PotentialSpf {
    /// Create new potential SPF
    pub fn new(cell: String, failure_mode: SpfFailureMode) -> Self {
        Self {
            cell,
            failure_mode,
            path_to_output: Vec::new(),
            protected_by: None,
            fit_contribution: 0.0,
            notes: None,
        }
    }

    /// Check if this SPF is protected
    pub fn is_protected(&self) -> bool {
        self.protected_by.is_some()
    }
}

/// Failure mode for SPF classification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SpfFailureMode {
    /// Output stuck at logic 0
    StuckAt0,
    /// Output stuck at logic 1
    StuckAt1,
    /// Timing failure (late/early)
    TimingFailure,
    /// Functional failure
    FunctionalFailure {
        /// Description of the functional failure
        description: String,
    },
}

/// A path that is protected by a safety mechanism
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProtectedPath {
    /// Start element of the path
    pub start: String,
    /// End element (safety-relevant output)
    pub end: String,
    /// Elements along the path
    pub path_elements: Vec<String>,
    /// Safety mechanism providing protection
    pub mechanism: String,
    /// Diagnostic coverage of the mechanism
    pub coverage: f64,
}

/// Summary of SPF analysis
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SpfSummary {
    /// Total cells analyzed
    pub total_cells: usize,
    /// Potential SPFs identified
    pub potential_spf_count: usize,
    /// SPFs with protection
    pub protected_spf_count: usize,
    /// SPFs without protection (violations)
    pub unprotected_spf_count: usize,
    /// Total FIT of analyzed cells
    pub total_fit: f64,
    /// Unprotected SPF FIT contribution
    pub unprotected_spf_fit: f64,
}

// ============================================================================
// Analysis Functions
// ============================================================================

/// Analyze diversity between redundant channels
///
/// This function examines redundant implementations to verify
/// that sufficient diversity exists to prevent common cause failures.
///
/// # Arguments
/// * `channel_definitions` - Map of channel name to cell paths
/// * `diversity_requirements` - Required diversity aspects
///
/// # Returns
/// Complete diversity analysis
pub fn analyze_diversity(
    channel_definitions: &HashMap<String, Vec<String>>,
    diversity_requirements: &[DiversityAspect],
) -> DiversityAnalysis {
    let mut analysis = DiversityAnalysis::new();
    let channels: Vec<_> = channel_definitions.keys().cloned().collect();

    // Initialize diversity matrix
    analysis.diversity_matrix = DiversityMatrix::with_channels(channels.clone());

    // Analyze each pair of channels
    for (i, channel_a) in channels.iter().enumerate() {
        for channel_b in channels.iter().skip(i + 1) {
            let cells_a = channel_definitions.get(channel_a).unwrap();
            let cells_b = channel_definitions.get(channel_b).unwrap();

            let mut pair = RedundantPair::new(channel_a.clone(), channel_b.clone());

            // Find shared components
            let set_a: HashSet<_> = cells_a.iter().collect();
            let set_b: HashSet<_> = cells_b.iter().collect();
            pair.shared_components = set_a.intersection(&set_b).map(|s| (*s).clone()).collect();

            // Analyze structural diversity (different implementations)
            if pair.shared_components.is_empty() {
                let evidence = DiversityEvidence::new(
                    EvidenceType::AutomatedAnalysis,
                    "No shared components detected".to_string(),
                );
                pair.add_evidence(DiversityAspect::DifferentImplementation, evidence);
            }

            // Check for temporal diversity (would need timing info)
            // This is a placeholder - real implementation would analyze timing
            for req in diversity_requirements {
                if let DiversityAspect::TemporalDiversity { min_offset_ns } = req {
                    let mut evidence = DiversityEvidence::new(
                        EvidenceType::ManualReview,
                        format!(
                            "Temporal diversity requires manual verification (min offset: {}ns)",
                            min_offset_ns
                        ),
                    );
                    evidence.verified = false; // Requires manual verification
                    pair.add_evidence(req.clone(), evidence);
                }
            }

            // Update matrix entry
            let entry = DiversityMatrixEntry {
                score: pair.pair_score,
                aspects: pair
                    .diversity_aspects
                    .iter()
                    .map(|(a, _)| a.clone())
                    .collect(),
                shared_count: pair.shared_components.len(),
            };
            analysis.diversity_matrix.set(channel_a, channel_b, entry);

            analysis.redundant_pairs.push(pair);
        }
    }

    // Identify common elements across all channels
    if channels.len() >= 2 {
        let mut all_cells: Vec<HashSet<&String>> = Vec::new();
        for channel in &channels {
            if let Some(cells) = channel_definitions.get(channel) {
                all_cells.push(cells.iter().collect());
            }
        }

        if all_cells.len() >= 2 {
            let mut common: HashSet<&String> = all_cells[0].clone();
            for cells in all_cells.iter().skip(1) {
                common = common.intersection(cells).cloned().collect();
            }

            for element in common {
                analysis.common_elements.push(CommonElement {
                    element: element.clone(),
                    shared_by: channels.clone(),
                    element_type: infer_element_type(element),
                    failure_modes: vec!["stuck-at-0".to_string(), "stuck-at-1".to_string()],
                    risk_level: RiskLevel::High,
                    mitigation: None,
                });
            }
        }
    }

    // Calculate overall score
    if !analysis.redundant_pairs.is_empty() {
        analysis.overall_score = analysis
            .redundant_pairs
            .iter()
            .map(|p| p.pair_score)
            .sum::<f64>()
            / analysis.redundant_pairs.len() as f64;
    }

    // Generate summary
    analysis.summary = DiversitySummary {
        pairs_analyzed: analysis.redundant_pairs.len(),
        pairs_compliant: analysis
            .redundant_pairs
            .iter()
            .filter(|p| p.pair_score >= 0.8)
            .count(),
        common_elements_count: analysis.common_elements.len(),
        high_risk_elements: analysis
            .common_elements
            .iter()
            .filter(|e| e.risk_level >= RiskLevel::High)
            .count(),
        recommendations: generate_diversity_recommendations(&analysis),
    };

    analysis
}

/// Infer element type from name/path
fn infer_element_type(element: &str) -> CommonElementType {
    let lower = element.to_lowercase();
    if lower.contains("clk") || lower.contains("clock") {
        CommonElementType::Clock
    } else if lower.contains("rst") || lower.contains("reset") {
        CommonElementType::Reset
    } else if lower.contains("pwr") || lower.contains("power") || lower.contains("vdd") {
        CommonElementType::PowerSupply
    } else if lower.contains("bus") || lower.contains("axi") || lower.contains("apb") {
        CommonElementType::Interconnect
    } else if lower.contains("mem") || lower.contains("ram") || lower.contains("rom") {
        CommonElementType::Memory
    } else if lower.contains("io") || lower.contains("gpio") || lower.contains("pad") {
        CommonElementType::InputOutput
    } else {
        CommonElementType::Logic
    }
}

/// Generate recommendations based on diversity analysis
fn generate_diversity_recommendations(analysis: &DiversityAnalysis) -> Vec<String> {
    let mut recommendations = Vec::new();

    // Check for low diversity scores
    for pair in &analysis.redundant_pairs {
        if pair.pair_score < 0.5 {
            recommendations.push(format!(
                "Low diversity between '{}' and '{}' (score: {:.2}). Consider different implementations.",
                pair.channel_a, pair.channel_b, pair.pair_score
            ));
        }
    }

    // Check for high-risk common elements
    for element in &analysis.common_elements {
        if element.risk_level >= RiskLevel::High && element.mitigation.is_none() {
            recommendations.push(format!(
                "High-risk common element '{}' shared by {} channels requires mitigation.",
                element.element,
                element.shared_by.len()
            ));
        }
    }

    // Check for unverified diversity claims
    for pair in &analysis.redundant_pairs {
        let unverified: Vec<_> = pair
            .diversity_aspects
            .iter()
            .filter(|(_, e)| !e.verified)
            .collect();
        if !unverified.is_empty() {
            recommendations.push(format!(
                "{} diversity aspects between '{}' and '{}' require manual verification.",
                unverified.len(),
                pair.channel_a,
                pair.channel_b
            ));
        }
    }

    recommendations
}

/// Detect single points of failure in a design
///
/// Analyzes the design to identify cells whose failure could
/// directly violate a safety goal without detection.
///
/// # Arguments
/// * `cells` - Map of cell name to (fit_rate, is_safety_mechanism)
/// * `safety_outputs` - Output signals related to safety goals
/// * `connectivity` - Map of cell to its fanout cells
///
/// # Returns
/// Single point of failure analysis
pub fn detect_single_points(
    cells: &HashMap<String, (f64, bool)>,
    safety_outputs: &[String],
    connectivity: &HashMap<String, Vec<String>>,
) -> SinglePointAnalysis {
    let mut analysis = SinglePointAnalysis::new();
    let mut total_fit = 0.0;

    // Build reverse connectivity (what drives each cell)
    let mut reverse_connectivity: HashMap<String, Vec<String>> = HashMap::new();
    for (driver, fanouts) in connectivity {
        for fanout in fanouts {
            reverse_connectivity
                .entry(fanout.clone())
                .or_default()
                .push(driver.clone());
        }
    }

    // Find all cells on paths to safety outputs
    let mut cells_on_safety_path: HashSet<String> = HashSet::new();
    let mut to_visit: Vec<String> = safety_outputs.to_vec();

    while let Some(cell) = to_visit.pop() {
        if cells_on_safety_path.contains(&cell) {
            continue;
        }
        cells_on_safety_path.insert(cell.clone());

        if let Some(drivers) = reverse_connectivity.get(&cell) {
            for driver in drivers {
                to_visit.push(driver.clone());
            }
        }
    }

    // Analyze each cell on safety path
    for cell in &cells_on_safety_path {
        if let Some((fit, is_sm)) = cells.get(cell) {
            total_fit += fit;

            // Safety mechanisms are not SPFs themselves
            if *is_sm {
                continue;
            }

            // Check if this cell has redundancy (multiple paths to output)
            let fanout_count = connectivity.get(cell).map(|f| f.len()).unwrap_or(0);

            // Simple SPF detection: cells with single fanout on safety path
            // Real implementation would do full path analysis
            if fanout_count <= 1 {
                let mut spf = PotentialSpf::new(cell.clone(), SpfFailureMode::StuckAt0);
                spf.fit_contribution = *fit;

                // Build path to output
                let mut current = cell.clone();
                while let Some(fanouts) = connectivity.get(&current) {
                    if let Some(next) = fanouts.first() {
                        spf.path_to_output.push(next.clone());
                        if safety_outputs.contains(next) {
                            break;
                        }
                        current = next.clone();
                    } else {
                        break;
                    }
                }

                // Check if protected by any safety mechanism
                for (sm_cell, (_, is_sm)) in cells {
                    if *is_sm {
                        // Check if SM monitors this cell's output
                        if let Some(sm_fanout) = connectivity.get(cell) {
                            if sm_fanout.contains(sm_cell) {
                                spf.protected_by = Some(sm_cell.clone());
                                break;
                            }
                        }
                    }
                }

                analysis.potential_spfs.push(spf);
            }
        }
    }

    // Calculate metrics
    analysis.calculate_spfm(total_fit);

    // Generate summary
    let unprotected_count = analysis
        .potential_spfs
        .iter()
        .filter(|spf| !spf.is_protected())
        .count();
    let unprotected_fit: f64 = analysis
        .potential_spfs
        .iter()
        .filter(|spf| !spf.is_protected())
        .map(|spf| spf.fit_contribution)
        .sum();

    analysis.summary = SpfSummary {
        total_cells: cells_on_safety_path.len(),
        potential_spf_count: analysis.potential_spfs.len(),
        protected_spf_count: analysis.potential_spfs.len() - unprotected_count,
        unprotected_spf_count: unprotected_count,
        total_fit,
        unprotected_spf_fit: unprotected_fit,
    };

    analysis
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_diversity_analysis_empty() {
        let channels: HashMap<String, Vec<String>> = HashMap::new();
        let requirements = vec![];
        let analysis = analyze_diversity(&channels, &requirements);

        assert!(analysis.redundant_pairs.is_empty());
        assert_eq!(analysis.overall_score, 0.0);
    }

    #[test]
    fn test_diversity_analysis_two_channels() {
        let mut channels = HashMap::new();
        channels.insert(
            "channel_a".to_string(),
            vec!["cell1".to_string(), "cell2".to_string()],
        );
        channels.insert(
            "channel_b".to_string(),
            vec!["cell3".to_string(), "cell4".to_string()],
        );

        let requirements = vec![DiversityAspect::DifferentImplementation];
        let analysis = analyze_diversity(&channels, &requirements);

        assert_eq!(analysis.redundant_pairs.len(), 1);
        assert!(analysis.redundant_pairs[0].shared_components.is_empty());
    }

    #[test]
    fn test_diversity_analysis_shared_components() {
        let mut channels = HashMap::new();
        channels.insert(
            "channel_a".to_string(),
            vec!["shared_clk".to_string(), "cell1".to_string()],
        );
        channels.insert(
            "channel_b".to_string(),
            vec!["shared_clk".to_string(), "cell2".to_string()],
        );

        let requirements = vec![];
        let analysis = analyze_diversity(&channels, &requirements);

        assert_eq!(analysis.redundant_pairs.len(), 1);
        assert_eq!(analysis.redundant_pairs[0].shared_components.len(), 1);
        assert!(analysis
            .common_elements
            .iter()
            .any(|e| e.element == "shared_clk"));
    }

    #[test]
    fn test_spf_detection_basic() {
        let mut cells = HashMap::new();
        cells.insert("cell1".to_string(), (10.0, false));
        cells.insert("cell2".to_string(), (10.0, false));
        cells.insert("output".to_string(), (5.0, false));

        let safety_outputs = vec!["output".to_string()];

        let mut connectivity = HashMap::new();
        connectivity.insert("cell1".to_string(), vec!["cell2".to_string()]);
        connectivity.insert("cell2".to_string(), vec!["output".to_string()]);

        let analysis = detect_single_points(&cells, &safety_outputs, &connectivity);

        assert!(analysis.summary.total_cells > 0);
        assert!(analysis.spf_metric >= 0.0 && analysis.spf_metric <= 1.0);
    }

    #[test]
    fn test_spf_with_safety_mechanism() {
        let mut cells = HashMap::new();
        cells.insert("func_cell".to_string(), (10.0, false));
        cells.insert("sm_checker".to_string(), (5.0, true)); // Safety mechanism
        cells.insert("output".to_string(), (5.0, false));

        let safety_outputs = vec!["output".to_string()];

        let mut connectivity = HashMap::new();
        connectivity.insert(
            "func_cell".to_string(),
            vec!["output".to_string(), "sm_checker".to_string()],
        );

        let analysis = detect_single_points(&cells, &safety_outputs, &connectivity);

        // The functional cell should be identified but protected
        let protected_spfs = analysis
            .potential_spfs
            .iter()
            .filter(|spf| spf.is_protected())
            .count();
        assert!(protected_spfs > 0 || analysis.potential_spfs.is_empty());
    }

    #[test]
    fn test_spfm_calculation() {
        let mut analysis = SinglePointAnalysis::new();
        analysis.potential_spfs.push(PotentialSpf {
            cell: "cell1".to_string(),
            failure_mode: SpfFailureMode::StuckAt0,
            path_to_output: vec![],
            protected_by: None,
            fit_contribution: 10.0,
            notes: None,
        });

        analysis.calculate_spfm(100.0);

        // SPFM = 1 - (10/100) = 0.9
        assert!((analysis.spf_metric - 0.9).abs() < 0.001);
    }

    #[test]
    fn test_asil_requirements() {
        let mut analysis = SinglePointAnalysis::new();
        analysis.spf_metric = 0.95;

        assert!(!analysis.meets_asil_requirement("D")); // Requires 0.99
        assert!(!analysis.meets_asil_requirement("C")); // Requires 0.97
        assert!(analysis.meets_asil_requirement("B")); // Requires 0.90
        assert!(analysis.meets_asil_requirement("A")); // No specific requirement
    }

    #[test]
    fn test_common_element_type_inference() {
        assert_eq!(infer_element_type("sys_clk"), CommonElementType::Clock);
        assert_eq!(infer_element_type("reset_n"), CommonElementType::Reset);
        assert_eq!(
            infer_element_type("axi_bus"),
            CommonElementType::Interconnect
        );
        assert_eq!(infer_element_type("data_mem"), CommonElementType::Memory);
        assert_eq!(
            infer_element_type("gpio_pad"),
            CommonElementType::InputOutput
        );
        assert_eq!(infer_element_type("random_logic"), CommonElementType::Logic);
    }

    #[test]
    fn test_diversity_evidence() {
        let mut evidence = DiversityEvidence::new(
            EvidenceType::AutomatedAnalysis,
            "Structural analysis".to_string(),
        );

        assert!(!evidence.verified);
        evidence.verify("Tool analysis");
        assert!(evidence.verified);
        assert_eq!(
            evidence.verification_method.as_deref(),
            Some("Tool analysis")
        );
    }
}
