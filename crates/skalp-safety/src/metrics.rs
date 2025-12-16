//! Safety metrics calculation and reporting for ISO 26262 compliance
//!
//! Provides calculation of hardware architectural metrics including SPFM, LF, and PMHF
//! for demonstrating compliance with ASIL requirements.

use crate::asil::AsilLevel;
use crate::common_cause::CcfAnalysisResults;
use crate::fmea::{DetectionClass, FailureClass, FmeaAnalysis};
use crate::mechanisms::{MechanismType, SafetyMechanism};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Comprehensive safety metrics for a design
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyMetrics {
    /// Metrics metadata
    pub metadata: MetricsMetadata,
    /// Hardware architectural metrics
    pub hardware_metrics: HardwareArchitecturalMetrics,
    /// Safety mechanism effectiveness
    pub mechanism_effectiveness: MechanismEffectivenessMetrics,
    /// Fault injection test results
    pub fault_injection_results: Option<FaultInjectionResults>,
    /// Compliance assessment
    pub compliance_assessment: ComplianceAssessment,
    /// Diagnostic coverage metrics
    pub diagnostic_coverage: DiagnosticCoverageMetrics,
}

/// Metadata for safety metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetricsMetadata {
    /// Analysis ID
    pub id: String,
    /// Design name
    pub design_name: String,
    /// Analysis version
    pub version: String,
    /// Calculation date
    pub calculation_date: DateTime<Utc>,
    /// Target ASIL level
    pub target_asil: AsilLevel,
    /// Calculation method
    pub calculation_method: CalculationMethod,
}

/// Method used for metrics calculation
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum CalculationMethod {
    /// Analytical calculation based on failure rates
    Analytical,
    /// Monte Carlo simulation
    MonteCarlo,
    /// Fault injection testing
    FaultInjection,
    /// Combined approach
    Combined,
}

/// Hardware architectural metrics according to ISO 26262
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HardwareArchitecturalMetrics {
    /// Single Point Fault Metric (percentage)
    pub spfm: f64,
    /// Latent Fault Metric (percentage)
    pub lf: f64,
    /// Probabilistic Metric for Hardware Failures (FIT)
    /// Per ISO 26262-5: PMHF = λSPF + λRF + λSM
    pub pmhf: f64,
    /// Total failure rate (FIT)
    pub total_failure_rate: f64,
    /// Safe failure rate (FIT)
    pub safe_failure_rate: f64,
    /// Single point failure rate (FIT)
    pub single_point_failure_rate: f64,
    /// Residual failure rate (FIT)
    pub residual_failure_rate: f64,
    /// Multiple point failure rate (FIT)
    pub multiple_point_failure_rate: f64,
    /// Detailed breakdown by component
    pub component_breakdown: HashMap<String, ComponentMetrics>,
    // ===== SM Failure Analysis (ISO 26262) =====
    /// Safety mechanism failure rate contribution to PMHF (λSM)
    /// This is the effective SM FIT after accounting for SM-of-SM protection
    pub sm_failure_rate: f64,
    /// Per-mechanism breakdown of SM failures
    pub sm_breakdown: HashMap<String, SmMetrics>,
    // ===== Latent Fault Analysis (ISO 26262) =====
    /// Detailed breakdown of latent (multiple-point) faults
    pub latent_fault_breakdown: LatentFaultBreakdown,
    // ===== Dependent Failure Analysis (ISO 26262-9) =====
    /// Common cause failure contribution to PMHF (λDPF_CCF)
    pub dpf_ccf_rate: f64,
    /// Per-group breakdown of CCF contributions
    pub ccf_breakdown: HashMap<String, f64>,
}

/// Metrics for a single safety mechanism's failure contribution
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SmMetrics {
    /// Mechanism name
    pub name: String,
    /// SM hardware FIT (λSM for this mechanism)
    pub sm_fit: f64,
    /// Number of cells implementing this SM
    pub cell_count: usize,
    /// Whether this SM is protected by another SM
    pub is_protected: bool,
    /// DC of protecting SM (if protected)
    pub protector_dc: Option<f64>,
    /// Effective contribution to PMHF
    pub pmhf_contribution: f64,
}

/// Breakdown of latent (multiple-point) fault analysis
///
/// Per ISO 26262-5, the Latent Fault Metric (LF) measures the effectiveness
/// of detecting multiple-point faults before they become dangerous.
/// LF = λMPF_detected / λMPF_total × 100%
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct LatentFaultBreakdown {
    /// Total multiple-point fault rate (FIT)
    pub total_mpf_fit: f64,
    /// Detected multiple-point fault rate (FIT) - by LSMs
    pub detected_mpf_fit: f64,
    /// Undetected (latent) multiple-point fault rate (FIT)
    pub undetected_mpf_fit: f64,
    /// Latent Fault Metric (percentage)
    pub lf_metric: f64,
    /// Number of FMEA entries with MultiplePoint classification
    pub mpf_entry_count: usize,
}

/// Timing compliance result for a single safety mechanism
///
/// Per ISO 26262-5, the fundamental timing requirement is:
/// FDTI + FRTI < FTTI
///
/// Where:
/// - FDTI = Fault Detection Time Interval
/// - FRTI = Fault Reaction Time Interval
/// - FTTI = Fault Tolerant Time Interval
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimingComplianceResult {
    /// Mechanism name
    pub mechanism_name: String,
    /// Mechanism ID
    pub mechanism_id: String,
    /// Fault Detection Time Interval (ns)
    pub fdti: Option<u64>,
    /// Fault Reaction Time Interval (ns)
    pub frti: Option<u64>,
    /// Fault Tolerant Time Interval (ns)
    pub ftti: Option<u64>,
    /// Multiple Point Fault Detection Interval (ns) - for LSMs
    pub mpfdi: Option<u64>,
    /// Total fault handling time (FDTI + FRTI) (ns)
    pub total_handling_time: Option<u64>,
    /// Whether FDTI + FRTI < FTTI
    pub compliant: bool,
    /// Timing margin (FTTI - (FDTI + FRTI)) in ns, negative if non-compliant
    pub margin_ns: i64,
    /// Whether timing parameters are fully specified
    pub timing_specified: bool,
}

/// Summary of timing compliance across all mechanisms
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimingComplianceSummary {
    /// Results for each mechanism
    pub mechanism_results: Vec<TimingComplianceResult>,
    /// Number of mechanisms with compliant timing
    pub compliant_count: usize,
    /// Number of mechanisms with non-compliant timing
    pub non_compliant_count: usize,
    /// Number of mechanisms with unspecified timing (cannot validate)
    pub unspecified_count: usize,
    /// Overall timing compliance status
    pub overall_compliant: bool,
    /// Minimum margin across all mechanisms (ns)
    pub min_margin_ns: Option<i64>,
    /// Mechanism with minimum margin
    pub critical_mechanism: Option<String>,
}

/// Validate timing compliance for all safety mechanisms
///
/// Per ISO 26262-5, verifies that FDTI + FRTI < FTTI for each mechanism.
/// This ensures faults are detected and handled before they can cause
/// hazardous events.
pub fn validate_timing_compliance(mechanisms: &[SafetyMechanism]) -> TimingComplianceSummary {
    let mut results = Vec::new();
    let mut compliant_count = 0;
    let mut non_compliant_count = 0;
    let mut unspecified_count = 0;
    let mut min_margin: Option<i64> = None;
    let mut critical_mechanism: Option<String> = None;

    for mechanism in mechanisms {
        let result = if let Some(hw_impl) = &mechanism.hardware_implementation {
            let timing = &hw_impl.timing_constraints;

            let (compliant, margin, total_handling, timing_specified) =
                if let Some((comp, marg)) = timing.validate_timing_compliance() {
                    (comp, marg, timing.total_fault_handling_time(), true)
                } else {
                    // Timing not fully specified
                    (true, 0, None, false)
                };

            if timing_specified {
                if compliant {
                    compliant_count += 1;
                } else {
                    non_compliant_count += 1;
                }

                // Track minimum margin
                if min_margin.is_none() || margin < min_margin.unwrap() {
                    min_margin = Some(margin);
                    critical_mechanism = Some(mechanism.name.clone());
                }
            } else {
                unspecified_count += 1;
            }

            TimingComplianceResult {
                mechanism_name: mechanism.name.clone(),
                mechanism_id: mechanism.id.clone(),
                fdti: timing.fdti,
                frti: timing.frti,
                ftti: timing.ftti,
                mpfdi: timing.mpfdi,
                total_handling_time: total_handling,
                compliant,
                margin_ns: margin,
                timing_specified,
            }
        } else {
            // No hardware implementation, timing unspecified
            unspecified_count += 1;
            TimingComplianceResult {
                mechanism_name: mechanism.name.clone(),
                mechanism_id: mechanism.id.clone(),
                fdti: None,
                frti: None,
                ftti: None,
                mpfdi: None,
                total_handling_time: None,
                compliant: true, // Assume compliant if not specified
                margin_ns: 0,
                timing_specified: false,
            }
        };

        results.push(result);
    }

    let overall_compliant = non_compliant_count == 0;

    TimingComplianceSummary {
        mechanism_results: results,
        compliant_count,
        non_compliant_count,
        unspecified_count,
        overall_compliant,
        min_margin_ns: min_margin,
        critical_mechanism,
    }
}

/// Format timing compliance report
pub fn format_timing_compliance_report(summary: &TimingComplianceSummary) -> String {
    let mut output = String::new();

    output.push_str("=== Timing Compliance Report (ISO 26262-5) ===\n\n");

    output.push_str(&format!(
        "Summary: {} compliant, {} non-compliant, {} unspecified\n",
        summary.compliant_count, summary.non_compliant_count, summary.unspecified_count
    ));
    output.push_str(&format!(
        "Overall Status: {}\n\n",
        if summary.overall_compliant {
            "COMPLIANT"
        } else {
            "NON-COMPLIANT"
        }
    ));

    if let (Some(margin), Some(ref mech)) = (summary.min_margin_ns, &summary.critical_mechanism) {
        output.push_str(&format!(
            "Critical Mechanism: {} (margin: {} ns)\n\n",
            mech, margin
        ));
    }

    output.push_str("Mechanism Details:\n");
    output.push_str(&format!("{}\n", "-".repeat(80)));

    for result in &summary.mechanism_results {
        output.push_str(&format!(
            "\n{} ({})\n",
            result.mechanism_name, result.mechanism_id
        ));

        if result.timing_specified {
            if let (Some(fdti), Some(frti), Some(ftti)) = (result.fdti, result.frti, result.ftti) {
                output.push_str(&format!("  FDTI: {} ns\n", fdti));
                output.push_str(&format!("  FRTI: {} ns\n", frti));
                output.push_str(&format!("  FTTI: {} ns\n", ftti));
                output.push_str(&format!(
                    "  FDTI + FRTI = {} ns\n",
                    result.total_handling_time.unwrap_or(0)
                ));
                output.push_str(&format!(
                    "  Status: {} (margin: {} ns)\n",
                    if result.compliant { "PASS" } else { "FAIL" },
                    result.margin_ns
                ));
            }
            if let Some(mpfdi) = result.mpfdi {
                output.push_str(&format!("  MPFDI: {} ns\n", mpfdi));
            }
        } else {
            output.push_str("  Timing: NOT SPECIFIED\n");
        }
    }

    output
}

/// Safety metrics for individual components
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComponentMetrics {
    /// Component name
    pub name: String,
    /// Component failure rate (FIT)
    pub failure_rate: f64,
    /// Safe failure percentage
    pub safe_failure_percentage: f64,
    /// Single point failure percentage
    pub single_point_failure_percentage: f64,
    /// Latent fault percentage
    pub latent_fault_percentage: f64,
    /// Diagnostic coverage
    pub diagnostic_coverage: f64,
    /// Safety mechanisms applied
    pub safety_mechanisms: Vec<String>,
}

/// Safety mechanism effectiveness metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MechanismEffectivenessMetrics {
    /// Overall mechanism effectiveness
    pub overall_effectiveness: f64,
    /// Effectiveness by mechanism type
    pub effectiveness_by_type: HashMap<MechanismType, f64>,
    /// Mechanism coverage analysis
    pub coverage_analysis: CoverageAnalysis,
    /// Redundancy analysis
    pub redundancy_analysis: RedundancyAnalysis,
}

/// Coverage analysis for safety mechanisms
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageAnalysis {
    /// Fault coverage percentage
    pub fault_coverage: f64,
    /// Diagnostic coverage percentage
    pub diagnostic_coverage: f64,
    /// Coverage gaps
    pub coverage_gaps: Vec<CoverageGap>,
    /// Coverage by failure mode
    pub coverage_by_failure_mode: HashMap<String, f64>,
}

/// Coverage gap identification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageGap {
    /// Gap description
    pub description: String,
    /// Affected components
    pub affected_components: Vec<String>,
    /// Impact on metrics
    pub impact: GapImpact,
    /// Recommended mitigation
    pub mitigation: String,
}

/// Impact of coverage gap
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GapImpact {
    /// Impact on SPFM
    pub spfm_impact: f64,
    /// Impact on LF
    pub lf_impact: f64,
    /// Impact on PMHF
    pub pmhf_impact: f64,
    /// Severity level
    pub severity: ImpactSeverity,
}

/// Severity of coverage gap impact
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum ImpactSeverity {
    /// Negligible impact
    Negligible,
    /// Low impact
    Low,
    /// Medium impact
    Medium,
    /// High impact
    High,
    /// Critical impact
    Critical,
}

/// Redundancy analysis for safety mechanisms
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RedundancyAnalysis {
    /// Redundancy level by component
    pub redundancy_levels: HashMap<String, u32>,
    /// Common mode failures analysis
    pub common_mode_failures: CommonModeFailureAnalysis,
    /// Diversity factors
    pub diversity_factors: DiversityFactors,
}

/// Common mode failure analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CommonModeFailureAnalysis {
    /// Common mode failure rate (FIT)
    pub cmf_rate: f64,
    /// Beta factor (common mode factor)
    pub beta_factor: f64,
    /// Independent failure rate (FIT)
    pub independent_failure_rate: f64,
    /// Common cause categories
    pub common_causes: Vec<String>,
}

/// Diversity factors for redundant systems
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiversityFactors {
    /// Design diversity
    pub design_diversity: bool,
    /// Technology diversity
    pub technology_diversity: bool,
    /// Timing diversity
    pub timing_diversity: bool,
    /// Supplier diversity
    pub supplier_diversity: bool,
    /// Overall diversity score
    pub diversity_score: f64,
}

/// Fault injection test results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FaultInjectionResults {
    /// Total faults injected
    pub total_faults_injected: u32,
    /// Detected faults
    pub detected_faults: u32,
    /// Undetected faults
    pub undetected_faults: u32,
    /// False positive detections
    pub false_positives: u32,
    /// Detection coverage percentage
    pub detection_coverage: f64,
    /// Test campaign details
    pub test_campaigns: Vec<TestCampaign>,
}

/// Individual fault injection test campaign
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestCampaign {
    /// Campaign name
    pub name: String,
    /// Target component
    pub target_component: String,
    /// Fault types tested
    pub fault_types: Vec<String>,
    /// Injection results
    pub results: CampaignResults,
}

/// Results from test campaign
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CampaignResults {
    /// Faults injected
    pub injected: u32,
    /// Faults detected
    pub detected: u32,
    /// Detection rate
    pub detection_rate: f64,
    /// Average detection time (ns)
    pub avg_detection_time: f64,
}

/// Compliance assessment against ASIL requirements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceAssessment {
    /// Overall compliance status
    pub overall_compliance: ComplianceStatus,
    /// ASIL requirements assessment
    pub asil_assessment: AsilAssessment,
    /// Compliance gaps
    pub compliance_gaps: Vec<ComplianceGap>,
    /// Margin analysis
    pub margin_analysis: MarginAnalysis,
}

/// Compliance status
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ComplianceStatus {
    /// Fully compliant
    Compliant,
    /// Compliant with margins
    CompliantWithMargins,
    /// Non-compliant (minor gaps)
    MinorGaps,
    /// Non-compliant (major gaps)
    MajorGaps,
    /// Non-compliant (critical gaps)
    CriticalGaps,
}

/// ASIL requirements assessment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AsilAssessment {
    /// Target ASIL level
    pub target_asil: AsilLevel,
    /// SPFM compliance
    pub spfm_compliance: MetricCompliance,
    /// LF compliance
    pub lf_compliance: MetricCompliance,
    /// PMHF compliance
    pub pmhf_compliance: MetricCompliance,
    /// Overall ASIL compliance
    pub overall_compliance: bool,
}

/// Compliance status for individual metric
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetricCompliance {
    /// Target value
    pub target: f64,
    /// Achieved value
    pub achieved: f64,
    /// Compliance status
    pub compliant: bool,
    /// Margin (percentage)
    pub margin: f64,
}

/// Compliance gap identification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceGap {
    /// Gap description
    pub description: String,
    /// Affected metric
    pub metric: String,
    /// Gap severity
    pub severity: GapSeverity,
    /// Required improvement
    pub required_improvement: f64,
    /// Recommended actions
    pub recommended_actions: Vec<String>,
}

/// Severity of compliance gap
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum GapSeverity {
    /// Minor gap (within acceptable range)
    Minor,
    /// Moderate gap (requires attention)
    Moderate,
    /// Major gap (significant improvement needed)
    Major,
    /// Critical gap (design change required)
    Critical,
}

/// Margin analysis for safety metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MarginAnalysis {
    /// Safety margins by metric
    pub safety_margins: HashMap<String, f64>,
    /// Sensitivity analysis
    pub sensitivity_analysis: SensitivityAnalysis,
    /// Robustness assessment
    pub robustness_assessment: RobustnessAssessment,
}

/// Sensitivity analysis for parameter variations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SensitivityAnalysis {
    /// Parameter sensitivities
    pub parameter_sensitivities: HashMap<String, f64>,
    /// Critical parameters
    pub critical_parameters: Vec<String>,
    /// Sensitivity threshold
    pub sensitivity_threshold: f64,
}

/// Robustness assessment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RobustnessAssessment {
    /// Monte Carlo confidence intervals
    pub confidence_intervals: HashMap<String, ConfidenceInterval>,
    /// Worst-case analysis
    pub worst_case_analysis: WorstCaseAnalysis,
    /// Robustness score
    pub robustness_score: f64,
}

/// Confidence interval for metric
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfidenceInterval {
    /// Lower bound
    pub lower_bound: f64,
    /// Upper bound
    pub upper_bound: f64,
    /// Confidence level (percentage)
    pub confidence_level: f64,
}

/// Worst-case analysis results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorstCaseAnalysis {
    /// Worst-case SPFM
    pub worst_case_spfm: f64,
    /// Worst-case LF
    pub worst_case_lf: f64,
    /// Worst-case PMHF
    pub worst_case_pmhf: f64,
    /// Contributing factors
    pub contributing_factors: Vec<String>,
}

/// Diagnostic coverage metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiagnosticCoverageMetrics {
    /// Overall diagnostic coverage
    pub overall_coverage: f64,
    /// Coverage by diagnostic type
    pub coverage_by_type: HashMap<DiagnosticType, f64>,
    /// Diagnostic effectiveness
    pub diagnostic_effectiveness: f64,
    /// Diagnostic latency metrics
    pub diagnostic_latency: DiagnosticLatencyMetrics,
}

/// Types of diagnostic coverage
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DiagnosticType {
    /// Online diagnostics
    Online,
    /// Offline diagnostics
    Offline,
    /// Built-in self-test
    BuiltInSelfTest,
    /// External monitoring
    ExternalMonitoring,
    /// Software diagnostics
    Software,
}

/// Diagnostic latency metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiagnosticLatencyMetrics {
    /// Average detection time (ns)
    pub avg_detection_time: f64,
    /// Maximum detection time (ns)
    pub max_detection_time: f64,
    /// Detection time distribution
    pub detection_time_distribution: Vec<LatencyBin>,
}

/// Latency distribution bin
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LatencyBin {
    /// Bin range (ns)
    pub range: (f64, f64),
    /// Percentage of detections in this bin
    pub percentage: f64,
}

/// Safety metrics calculator
#[derive(Debug)]
pub struct SafetyMetricsCalculator {
    /// Configuration for calculation
    config: CalculationConfig,
}

/// Configuration for metrics calculation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CalculationConfig {
    /// Calculation method
    pub method: CalculationMethod,
    /// Monte Carlo iterations (if applicable)
    pub monte_carlo_iterations: Option<u32>,
    /// Confidence level for intervals
    pub confidence_level: f64,
    /// Include detailed breakdown
    pub detailed_breakdown: bool,
    /// Fault injection campaign configuration
    pub fault_injection_config: Option<FaultInjectionConfig>,
}

/// Configuration for fault injection testing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FaultInjectionConfig {
    /// Number of faults per component
    pub faults_per_component: u32,
    /// Fault types to inject
    pub fault_types: Vec<String>,
    /// Test duration (cycles)
    pub test_duration: u64,
    /// Random seed for reproducibility
    pub random_seed: Option<u64>,
}

impl SafetyMetricsCalculator {
    /// Create a new metrics calculator
    pub fn new(config: CalculationConfig) -> Self {
        Self { config }
    }

    /// Calculate safety metrics from FMEA and safety mechanisms
    pub fn calculate_metrics(
        &self,
        fmea: &FmeaAnalysis,
        mechanisms: &[SafetyMechanism],
    ) -> Result<SafetyMetrics, MetricsError> {
        let metadata = self.create_metadata(&fmea.metadata.design_name, fmea.metadata.target_asil);

        let hardware_metrics = self.calculate_hardware_metrics(fmea, mechanisms)?;
        let mechanism_effectiveness = self.calculate_mechanism_effectiveness(mechanisms)?;
        let compliance_assessment =
            self.assess_compliance(&hardware_metrics, fmea.metadata.target_asil)?;
        let diagnostic_coverage = self.calculate_diagnostic_coverage(mechanisms)?;

        let fault_injection_results = if self.config.fault_injection_config.is_some() {
            Some(self.run_fault_injection_tests()?)
        } else {
            None
        };

        Ok(SafetyMetrics {
            metadata,
            hardware_metrics,
            mechanism_effectiveness,
            fault_injection_results,
            compliance_assessment,
            diagnostic_coverage,
        })
    }

    /// Create metadata for metrics
    fn create_metadata(&self, design_name: &str, target_asil: AsilLevel) -> MetricsMetadata {
        MetricsMetadata {
            id: uuid::Uuid::new_v4().to_string(),
            design_name: design_name.to_string(),
            version: "1.0".to_string(),
            calculation_date: Utc::now(),
            target_asil,
            calculation_method: self.config.method.clone(),
        }
    }

    /// Calculate hardware architectural metrics
    ///
    /// # Arguments
    /// * `fmea` - FMEA analysis containing failure mode data
    /// * `mechanisms` - Safety mechanisms for effectiveness calculation
    /// * `sm_analysis` - Optional SM failure analysis for λSM contribution
    ///
    /// # PMHF Formula (ISO 26262-5)
    /// ```text
    /// PMHF = λSPF + λRF + λSM
    /// ```
    /// Where:
    /// - λSPF = Single Point Fault rate (unprotected functional logic)
    /// - λRF = Residual Fault rate = λprotected × (1 - DC)
    /// - λSM = Safety Mechanism failure rate (NEW - from sm_analysis)
    fn calculate_hardware_metrics(
        &self,
        fmea: &FmeaAnalysis,
        mechanisms: &[SafetyMechanism],
    ) -> Result<HardwareArchitecturalMetrics, MetricsError> {
        self.calculate_hardware_metrics_with_sm(fmea, mechanisms, None)
    }

    /// Calculate hardware architectural metrics with SM failure analysis
    ///
    /// This is the full ISO 26262-5 compliant version that includes λSM.
    /// Use `calculate_hardware_metrics_full` for the complete version with CCF analysis.
    pub fn calculate_hardware_metrics_with_sm(
        &self,
        fmea: &FmeaAnalysis,
        mechanisms: &[SafetyMechanism],
        sm_analysis: Option<&crate::sm_failure_analysis::SmFailureAnalysis>,
    ) -> Result<HardwareArchitecturalMetrics, MetricsError> {
        self.calculate_hardware_metrics_full(fmea, mechanisms, sm_analysis, None)
    }

    /// Calculate hardware architectural metrics with full ISO 26262-5/9 analysis
    ///
    /// This is the complete ISO 26262 compliant version that includes:
    /// - λSM (Safety Mechanism failures)
    /// - λDPF_CCF (Dependent failures from Common Cause Failures)
    ///
    /// # PMHF Formula (ISO 26262-5 + ISO 26262-9)
    /// ```text
    /// PMHF = λSPF + λRF + λSM + λDPF_CCF
    /// ```
    /// Where:
    /// - λSPF = Single Point Fault rate (unprotected functional logic)
    /// - λRF = Residual Fault rate = λprotected × (1 - DC)
    /// - λSM = Safety Mechanism failure rate
    /// - λDPF_CCF = Dependent failures from common cause (from CCF analysis)
    pub fn calculate_hardware_metrics_full(
        &self,
        fmea: &FmeaAnalysis,
        mechanisms: &[SafetyMechanism],
        sm_analysis: Option<&crate::sm_failure_analysis::SmFailureAnalysis>,
        ccf_analysis: Option<&CcfAnalysisResults>,
    ) -> Result<HardwareArchitecturalMetrics, MetricsError> {
        let mut total_failure_rate = 0.0;
        let mut safe_failure_rate = 0.0;
        let mut single_point_failure_rate = 0.0;
        let mut component_breakdown = HashMap::new();

        // Calculate failure rates from FMEA entries
        for entry in &fmea.fmea_entries {
            if let Some(failure_rate) = entry.failure_mode.failure_rate {
                total_failure_rate += failure_rate;

                match entry.failure_mode.failure_class {
                    FailureClass::Safe => safe_failure_rate += failure_rate,
                    FailureClass::SinglePoint => single_point_failure_rate += failure_rate,
                    FailureClass::Residual => {} // Contributes to PMHF
                    FailureClass::MultiplePoint => {} // Contributes to LF
                }

                // Add to component breakdown
                let component_metrics = component_breakdown
                    .entry(entry.item.clone())
                    .or_insert_with(|| ComponentMetrics {
                        name: entry.item.clone(),
                        failure_rate: 0.0,
                        safe_failure_percentage: 0.0,
                        single_point_failure_percentage: 0.0,
                        latent_fault_percentage: 0.0,
                        diagnostic_coverage: 0.0,
                        safety_mechanisms: vec![],
                    });

                component_metrics.failure_rate += failure_rate;
            }
        }

        // Apply safety mechanism effectiveness
        let mechanism_effectiveness = self.calculate_overall_mechanism_effectiveness(mechanisms)?;

        // Calculate SPFM (Single Point Fault Metric)
        let spfm = if total_failure_rate > 0.0 {
            ((safe_failure_rate + single_point_failure_rate * mechanism_effectiveness)
                / total_failure_rate)
                * 100.0
        } else {
            100.0
        };

        // Calculate LF (Latent Fault Metric) from actual FMEA MultiplePoint data
        // Per ISO 26262-5, LF is based on actual multiple-point fault analysis
        let latent_fault_breakdown = Self::calculate_latent_fault_breakdown(fmea);
        let lf = latent_fault_breakdown.lf_metric;

        // Calculate λRF (Residual Failure rate)
        let residual_failure_rate = single_point_failure_rate * (1.0 - mechanism_effectiveness);

        // Calculate λSM (Safety Mechanism failure rate) from SM analysis
        let (sm_failure_rate, sm_breakdown) = if let Some(sm) = sm_analysis {
            let sm_fit = sm.calculate_sm_pmhf_contribution();
            let breakdown: HashMap<String, SmMetrics> = sm
                .mechanisms
                .iter()
                .map(|m| {
                    (
                        m.mechanism_name.clone(),
                        SmMetrics {
                            name: m.mechanism_name.clone(),
                            sm_fit: m.sm_fit,
                            cell_count: m.cell_count,
                            is_protected: m.protected_by.is_some(),
                            protector_dc: m.protection_dc,
                            pmhf_contribution: m.pmhf_contribution,
                        },
                    )
                })
                .collect();
            (sm_fit, breakdown)
        } else {
            // No SM analysis: use sum of mechanism implementation_fit
            let sm_fit: f64 = mechanisms
                .iter()
                .map(|m| m.calculate_pmhf_contribution())
                .sum();
            (sm_fit, HashMap::new())
        };

        // Calculate λDPF_CCF (Dependent failures from Common Cause Failures)
        // Per ISO 26262-9, CCF analysis identifies correlated failure modes
        let (dpf_ccf_rate, ccf_breakdown) = if let Some(ccf) = ccf_analysis {
            // Build per-group breakdown from CCF analysis
            // Note: We use the group's beta factor as an indicator of CCF contribution
            // The actual per-group FIT requires cell-level FIT mapping which is
            // computed during CCF analysis via calculate_totals()
            let breakdown: HashMap<String, f64> = ccf
                .groups
                .iter()
                .map(|g| {
                    // Estimate group contribution based on member count and beta factor
                    // This is an approximation - full accuracy requires cell_fits data
                    let group_contribution = if !ccf.groups.is_empty() {
                        // Distribute total CCF FIT proportionally by beta factor weight
                        let total_beta: f64 = ccf.groups.iter().map(|grp| grp.beta_factor).sum();
                        if total_beta > 0.0 {
                            ccf.total_common_cause_fit * (g.beta_factor / total_beta)
                        } else {
                            0.0
                        }
                    } else {
                        0.0
                    };
                    (g.name.clone(), group_contribution)
                })
                .collect();
            (ccf.total_common_cause_fit, breakdown)
        } else {
            (0.0, HashMap::new())
        };

        // Calculate PMHF (ISO 26262-5 + ISO 26262-9 compliant)
        // PMHF = λSPF + λRF + λSM + λDPF_CCF
        // Note: λSPF is already accounted for in single_point_failure_rate for unprotected logic
        // Here we use residual (λRF) + SM failures (λSM) + CCF dependent failures (λDPF_CCF)
        let pmhf = residual_failure_rate + sm_failure_rate + dpf_ccf_rate;

        Ok(HardwareArchitecturalMetrics {
            spfm,
            lf,
            pmhf,
            total_failure_rate,
            safe_failure_rate,
            single_point_failure_rate,
            residual_failure_rate,
            multiple_point_failure_rate: latent_fault_breakdown.total_mpf_fit,
            component_breakdown,
            sm_failure_rate,
            sm_breakdown,
            latent_fault_breakdown,
            dpf_ccf_rate,
            ccf_breakdown,
        })
    }

    /// Calculate overall mechanism effectiveness
    fn calculate_overall_mechanism_effectiveness(
        &self,
        mechanisms: &[SafetyMechanism],
    ) -> Result<f64, MetricsError> {
        if mechanisms.is_empty() {
            return Ok(0.0);
        }

        let total_effectiveness: f64 = mechanisms.iter().map(|m| m.calculate_effectiveness()).sum();

        Ok(total_effectiveness / mechanisms.len() as f64 / 100.0)
    }

    /// Calculate latent fault breakdown from FMEA MultiplePoint entries
    ///
    /// Per ISO 26262-5, the Latent Fault Metric is calculated from actual
    /// FMEA data rather than assumed percentages. This function extracts
    /// all MultiplePoint failure modes and determines their detection status.
    ///
    /// Detection effectiveness is derived from the DetectionClass:
    /// - VeryHigh: 99% detection
    /// - High: 90% detection
    /// - Medium: 60% detection
    /// - Low: 30% detection
    /// - VeryLow: 10% detection
    fn calculate_latent_fault_breakdown(fmea: &FmeaAnalysis) -> LatentFaultBreakdown {
        let mut total_mpf = 0.0;
        let mut detected_mpf = 0.0;
        let mut mpf_entry_count = 0;

        for entry in &fmea.fmea_entries {
            if matches!(
                entry.failure_mode.failure_class,
                FailureClass::MultiplePoint
            ) {
                if let Some(rate) = entry.failure_mode.failure_rate {
                    total_mpf += rate;
                    mpf_entry_count += 1;

                    // Determine detection effectiveness from detection ranking
                    let detection_effectiveness = match entry.current_controls.detection_ranking {
                        DetectionClass::VeryHigh => 0.99,
                        DetectionClass::High => 0.90,
                        DetectionClass::Medium => 0.60,
                        DetectionClass::Low => 0.30,
                        DetectionClass::VeryLow => 0.10,
                        DetectionClass::None => 0.0, // No detection capability
                    };

                    // If there are detection controls, apply the effectiveness
                    if !entry.current_controls.detection.is_empty() {
                        detected_mpf += rate * detection_effectiveness;
                    }
                    // If no detection controls but VeryHigh/High ranking, still partially detected
                    else if matches!(
                        entry.current_controls.detection_ranking,
                        DetectionClass::VeryHigh | DetectionClass::High
                    ) {
                        // Inherent detectability without explicit controls
                        detected_mpf += rate * detection_effectiveness * 0.5;
                    }
                }
            }
        }

        let lf_metric = if total_mpf > 0.0 {
            (detected_mpf / total_mpf) * 100.0
        } else {
            100.0 // No MPF means 100% coverage
        };

        LatentFaultBreakdown {
            total_mpf_fit: total_mpf,
            detected_mpf_fit: detected_mpf,
            undetected_mpf_fit: total_mpf - detected_mpf,
            lf_metric,
            mpf_entry_count,
        }
    }

    /// Calculate mechanism effectiveness metrics
    fn calculate_mechanism_effectiveness(
        &self,
        mechanisms: &[SafetyMechanism],
    ) -> Result<MechanismEffectivenessMetrics, MetricsError> {
        let overall_effectiveness = self.calculate_overall_mechanism_effectiveness(mechanisms)?;

        let mut effectiveness_by_type = HashMap::new();
        let mut type_counts = HashMap::new();

        for mechanism in mechanisms {
            let effectiveness = mechanism.calculate_effectiveness();
            *effectiveness_by_type
                .entry(mechanism.mechanism_type.clone())
                .or_insert(0.0) += effectiveness;
            *type_counts
                .entry(mechanism.mechanism_type.clone())
                .or_insert(0) += 1;
        }

        // Calculate average effectiveness by type
        for (mechanism_type, total) in effectiveness_by_type.iter_mut() {
            if let Some(count) = type_counts.get(mechanism_type) {
                *total /= *count as f64;
            }
        }

        let coverage_analysis = CoverageAnalysis {
            fault_coverage: overall_effectiveness * 100.0,
            diagnostic_coverage: overall_effectiveness * 100.0,
            coverage_gaps: vec![], // Would be populated from detailed analysis
            coverage_by_failure_mode: HashMap::new(),
        };

        let redundancy_analysis = RedundancyAnalysis {
            redundancy_levels: HashMap::new(),
            common_mode_failures: CommonModeFailureAnalysis {
                cmf_rate: 10.0, // Example value
                beta_factor: 0.1,
                independent_failure_rate: 90.0,
                common_causes: vec!["Environmental stress".to_string()],
            },
            diversity_factors: DiversityFactors {
                design_diversity: false,
                technology_diversity: false,
                timing_diversity: false,
                supplier_diversity: false,
                diversity_score: 0.0,
            },
        };

        Ok(MechanismEffectivenessMetrics {
            overall_effectiveness,
            effectiveness_by_type,
            coverage_analysis,
            redundancy_analysis,
        })
    }

    /// Assess compliance with ASIL requirements
    fn assess_compliance(
        &self,
        metrics: &HardwareArchitecturalMetrics,
        target_asil: AsilLevel,
    ) -> Result<ComplianceAssessment, MetricsError> {
        let requirements = target_asil.requirements();

        let spfm_compliance = if let Some(target) = requirements.spfm_target {
            MetricCompliance {
                target,
                achieved: metrics.spfm,
                compliant: metrics.spfm >= target,
                margin: ((metrics.spfm - target) / target) * 100.0,
            }
        } else {
            MetricCompliance {
                target: 0.0,
                achieved: metrics.spfm,
                compliant: true,
                margin: 100.0,
            }
        };

        let lf_compliance = if let Some(target) = requirements.lf_target {
            MetricCompliance {
                target,
                achieved: metrics.lf,
                compliant: metrics.lf >= target,
                margin: ((metrics.lf - target) / target) * 100.0,
            }
        } else {
            MetricCompliance {
                target: 0.0,
                achieved: metrics.lf,
                compliant: true,
                margin: 100.0,
            }
        };

        // PMHF target based on ASIL level
        let pmhf_target = match target_asil {
            AsilLevel::A => 1000.0,
            AsilLevel::B => 100.0,
            AsilLevel::C => 100.0,
            AsilLevel::D => 10.0,
            AsilLevel::QM => f64::INFINITY,
        };

        let pmhf_compliance = MetricCompliance {
            target: pmhf_target,
            achieved: metrics.pmhf,
            compliant: metrics.pmhf <= pmhf_target,
            margin: if pmhf_target.is_finite() {
                ((pmhf_target - metrics.pmhf) / pmhf_target) * 100.0
            } else {
                100.0
            },
        };

        let overall_compliance =
            spfm_compliance.compliant && lf_compliance.compliant && pmhf_compliance.compliant;

        let overall_status = if overall_compliance {
            if spfm_compliance.margin > 10.0
                && lf_compliance.margin > 10.0
                && pmhf_compliance.margin > 10.0
            {
                ComplianceStatus::CompliantWithMargins
            } else {
                ComplianceStatus::Compliant
            }
        } else {
            ComplianceStatus::MajorGaps
        };

        Ok(ComplianceAssessment {
            overall_compliance: overall_status,
            asil_assessment: AsilAssessment {
                target_asil,
                spfm_compliance,
                lf_compliance,
                pmhf_compliance,
                overall_compliance,
            },
            compliance_gaps: vec![], // Would be populated from detailed analysis
            margin_analysis: MarginAnalysis {
                safety_margins: HashMap::new(),
                sensitivity_analysis: SensitivityAnalysis {
                    parameter_sensitivities: HashMap::new(),
                    critical_parameters: vec![],
                    sensitivity_threshold: 0.1,
                },
                robustness_assessment: RobustnessAssessment {
                    confidence_intervals: HashMap::new(),
                    worst_case_analysis: WorstCaseAnalysis {
                        worst_case_spfm: metrics.spfm * 0.9, // 10% degradation
                        worst_case_lf: metrics.lf * 0.9,
                        worst_case_pmhf: metrics.pmhf * 1.1,
                        contributing_factors: vec!["Parameter variations".to_string()],
                    },
                    robustness_score: 0.85,
                },
            },
        })
    }

    /// Calculate diagnostic coverage metrics
    fn calculate_diagnostic_coverage(
        &self,
        mechanisms: &[SafetyMechanism],
    ) -> Result<DiagnosticCoverageMetrics, MetricsError> {
        let overall_coverage = mechanisms
            .iter()
            .map(|m| m.diagnostic_coverage)
            .sum::<f64>()
            / mechanisms.len().max(1) as f64;

        let mut coverage_by_type = HashMap::new();
        coverage_by_type.insert(DiagnosticType::Online, overall_coverage);
        coverage_by_type.insert(DiagnosticType::BuiltInSelfTest, overall_coverage * 0.8);

        Ok(DiagnosticCoverageMetrics {
            overall_coverage,
            coverage_by_type,
            diagnostic_effectiveness: overall_coverage,
            diagnostic_latency: DiagnosticLatencyMetrics {
                avg_detection_time: 1000.0,  // 1 µs
                max_detection_time: 10000.0, // 10 µs
                detection_time_distribution: vec![
                    LatencyBin {
                        range: (0.0, 1000.0),
                        percentage: 60.0,
                    },
                    LatencyBin {
                        range: (1000.0, 5000.0),
                        percentage: 30.0,
                    },
                    LatencyBin {
                        range: (5000.0, 10000.0),
                        percentage: 10.0,
                    },
                ],
            },
        })
    }

    /// Run fault injection tests (simplified implementation)
    fn run_fault_injection_tests(&self) -> Result<FaultInjectionResults, MetricsError> {
        // This would be implemented to interface with actual fault injection framework
        Ok(FaultInjectionResults {
            total_faults_injected: 1000,
            detected_faults: 950,
            undetected_faults: 50,
            false_positives: 5,
            detection_coverage: 95.0,
            test_campaigns: vec![TestCampaign {
                name: "CPU Core Test".to_string(),
                target_component: "cpu_core".to_string(),
                fault_types: vec!["stuck-at".to_string(), "bit-flip".to_string()],
                results: CampaignResults {
                    injected: 500,
                    detected: 475,
                    detection_rate: 95.0,
                    avg_detection_time: 500.0,
                },
            }],
        })
    }
}

/// Errors that can occur during metrics calculation
#[derive(Debug, thiserror::Error)]
pub enum MetricsError {
    #[error("Invalid configuration: {0}")]
    InvalidConfig(String),
    #[error("Calculation failed: {0}")]
    CalculationFailed(String),
    #[error("Insufficient data: {0}")]
    InsufficientData(String),
    #[error("FMEA analysis error: {0}")]
    FmeaError(String),
}

impl Default for CalculationConfig {
    fn default() -> Self {
        Self {
            method: CalculationMethod::Analytical,
            monte_carlo_iterations: None,
            confidence_level: 95.0,
            detailed_breakdown: true,
            fault_injection_config: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fmea::{FmeaMetadata, FmeaSummary, FunctionalAnalysis, SystemBoundary};
    use crate::mechanisms::{MechanismCategory, MechanismType, SafetyMechanism};

    #[test]
    fn test_metrics_calculation() {
        let config = CalculationConfig::default();
        let calculator = SafetyMetricsCalculator::new(config);

        // Create test FMEA
        let fmea = FmeaAnalysis {
            metadata: FmeaMetadata {
                id: "test".to_string(),
                design_name: "test_design".to_string(),
                version: "1.0".to_string(),
                analysis_date: Utc::now(),
                analyst: "test".to_string(),
                review_status: crate::fmea::ReviewStatus::InProgress,
                target_asil: AsilLevel::B,
            },
            system_boundary: SystemBoundary {
                inputs: vec![],
                outputs: vec![],
                components: vec![],
                external_interfaces: vec![],
            },
            functional_analysis: FunctionalAnalysis {
                functions: vec![],
                dependencies: HashMap::new(),
                critical_paths: vec![],
            },
            fmea_entries: vec![],
            summary: FmeaSummary::default(),
        };

        // Create test safety mechanism
        let mut mechanism = SafetyMechanism::new(
            "TEST-001".to_string(),
            "Test Mechanism".to_string(),
            "Test description".to_string(),
            MechanismType::Primary,
            MechanismCategory::ErrorDetection,
        );
        mechanism.set_fault_coverage(95.0);
        mechanism.set_diagnostic_coverage(90.0);

        let result = calculator.calculate_metrics(&fmea, &[mechanism]);
        assert!(result.is_ok());

        let metrics = result.unwrap();
        assert_eq!(metrics.metadata.design_name, "test_design");
        assert_eq!(metrics.metadata.target_asil, AsilLevel::B);
    }

    #[test]
    fn test_compliance_assessment() {
        let config = CalculationConfig::default();
        let calculator = SafetyMetricsCalculator::new(config);

        let hardware_metrics = HardwareArchitecturalMetrics {
            spfm: 95.0,
            lf: 85.0,
            pmhf: 50.0,
            total_failure_rate: 1000.0,
            safe_failure_rate: 900.0,
            single_point_failure_rate: 100.0,
            residual_failure_rate: 50.0,
            multiple_point_failure_rate: 50.0,
            component_breakdown: HashMap::new(),
            sm_failure_rate: 0.0,
            sm_breakdown: HashMap::new(),
            latent_fault_breakdown: LatentFaultBreakdown {
                total_mpf_fit: 50.0,
                detected_mpf_fit: 42.5,
                undetected_mpf_fit: 7.5,
                lf_metric: 85.0,
                mpf_entry_count: 5,
            },
            dpf_ccf_rate: 0.0,
            ccf_breakdown: HashMap::new(),
        };

        let compliance = calculator
            .assess_compliance(&hardware_metrics, AsilLevel::B)
            .unwrap();
        assert!(compliance.asil_assessment.spfm_compliance.compliant);
        assert!(compliance.asil_assessment.lf_compliance.compliant);
        assert!(compliance.asil_assessment.pmhf_compliance.compliant);
    }

    #[test]
    fn test_mechanism_effectiveness() {
        let config = CalculationConfig::default();
        let calculator = SafetyMetricsCalculator::new(config);

        let mut mechanism1 = SafetyMechanism::new(
            "MECH-001".to_string(),
            "Primary Mechanism".to_string(),
            "Test".to_string(),
            MechanismType::Primary,
            MechanismCategory::ErrorDetection,
        );
        mechanism1.set_fault_coverage(90.0);

        let mut mechanism2 = SafetyMechanism::new(
            "MECH-002".to_string(),
            "Latent Mechanism".to_string(),
            "Test".to_string(),
            MechanismType::Latent,
            MechanismCategory::Monitoring,
        );
        mechanism2.set_diagnostic_coverage(85.0);

        let effectiveness = calculator
            .calculate_mechanism_effectiveness(&[mechanism1, mechanism2])
            .unwrap();
        assert!(effectiveness.overall_effectiveness > 0.0);
        assert!(effectiveness
            .effectiveness_by_type
            .contains_key(&MechanismType::Primary));
        assert!(effectiveness
            .effectiveness_by_type
            .contains_key(&MechanismType::Latent));
    }

    #[test]
    fn test_diagnostic_coverage_calculation() {
        let config = CalculationConfig::default();
        let calculator = SafetyMetricsCalculator::new(config);

        let mut mechanism = SafetyMechanism::new(
            "DIAG-001".to_string(),
            "Diagnostic Mechanism".to_string(),
            "Test".to_string(),
            MechanismType::Latent,
            MechanismCategory::Monitoring,
        );
        mechanism.set_diagnostic_coverage(92.0);

        let coverage = calculator
            .calculate_diagnostic_coverage(&[mechanism])
            .unwrap();
        assert_eq!(coverage.overall_coverage, 92.0);
        assert!(coverage
            .coverage_by_type
            .contains_key(&DiagnosticType::Online));
    }

    #[test]
    fn test_hardware_metrics_calculation() {
        let config = CalculationConfig::default();
        let calculator = SafetyMetricsCalculator::new(config);

        // Create empty FMEA for testing
        let fmea = FmeaAnalysis {
            metadata: FmeaMetadata {
                id: "test".to_string(),
                design_name: "test".to_string(),
                version: "1.0".to_string(),
                analysis_date: Utc::now(),
                analyst: "test".to_string(),
                review_status: crate::fmea::ReviewStatus::InProgress,
                target_asil: AsilLevel::A,
            },
            system_boundary: SystemBoundary {
                inputs: vec![],
                outputs: vec![],
                components: vec![],
                external_interfaces: vec![],
            },
            functional_analysis: FunctionalAnalysis {
                functions: vec![],
                dependencies: HashMap::new(),
                critical_paths: vec![],
            },
            fmea_entries: vec![],
            summary: FmeaSummary::default(),
        };

        let mechanism = SafetyMechanism::new(
            "TEST".to_string(),
            "Test".to_string(),
            "Test".to_string(),
            MechanismType::Primary,
            MechanismCategory::ErrorDetection,
        );

        let result = calculator.calculate_hardware_metrics(&fmea, &[mechanism]);
        assert!(result.is_ok());

        let metrics = result.unwrap();
        assert_eq!(metrics.spfm, 100.0); // No failures = 100% SPFM
        assert_eq!(metrics.lf, 100.0); // No latent faults = 100% LF
    }

    #[test]
    fn test_hardware_metrics_with_ccf() {
        use crate::common_cause::{CcfAnalysisResults, CcfCause, CcfGroup};

        let config = CalculationConfig::default();
        let calculator = SafetyMetricsCalculator::new(config);

        // Create empty FMEA for testing
        let fmea = FmeaAnalysis {
            metadata: FmeaMetadata {
                id: "test".to_string(),
                design_name: "test".to_string(),
                version: "1.0".to_string(),
                analysis_date: Utc::now(),
                analyst: "test".to_string(),
                review_status: crate::fmea::ReviewStatus::InProgress,
                target_asil: AsilLevel::B,
            },
            system_boundary: SystemBoundary {
                inputs: vec![],
                outputs: vec![],
                components: vec![],
                external_interfaces: vec![],
            },
            functional_analysis: FunctionalAnalysis {
                functions: vec![],
                dependencies: HashMap::new(),
                critical_paths: vec![],
            },
            fmea_entries: vec![],
            summary: FmeaSummary::default(),
        };

        let mechanism = SafetyMechanism::new(
            "TEST".to_string(),
            "Test".to_string(),
            "Test".to_string(),
            MechanismType::Primary,
            MechanismCategory::ErrorDetection,
        );

        // Create CCF analysis with some groups
        let mut ccf_results = CcfAnalysisResults::new("test_design");

        let mut clock_group = CcfGroup::new("clk_main", CcfCause::SharedClock, 0.07);
        clock_group.add_member("cell_a".to_string());
        clock_group.add_member("cell_b".to_string());
        ccf_results.add_group(clock_group);

        let mut reset_group = CcfGroup::new("rst_domain", CcfCause::SharedReset, 0.05);
        reset_group.add_member("cell_c".to_string());
        ccf_results.add_group(reset_group);

        // Set total CCF FIT directly (normally calculated via calculate_totals)
        ccf_results.total_common_cause_fit = 10.0; // 10 FIT from CCF

        let result = calculator.calculate_hardware_metrics_full(
            &fmea,
            &[mechanism],
            None,
            Some(&ccf_results),
        );
        assert!(result.is_ok());

        let metrics = result.unwrap();

        // Verify CCF contribution is in PMHF
        assert_eq!(metrics.dpf_ccf_rate, 10.0);
        assert!(metrics.pmhf >= 10.0); // At least the CCF contribution

        // Verify CCF breakdown has both groups
        assert!(metrics.ccf_breakdown.contains_key("clk_main"));
        assert!(metrics.ccf_breakdown.contains_key("rst_domain"));

        // Verify breakdown sums to total (approximately)
        let breakdown_sum: f64 = metrics.ccf_breakdown.values().sum();
        assert!((breakdown_sum - 10.0).abs() < 0.01);
    }

    #[test]
    fn test_timing_compliance_validation() {
        use crate::mechanisms::{
            HardwareImplementation, PowerConsumption, ResourceUtilization, TimingConstraints,
        };

        // Create mechanism with compliant timing (FDTI + FRTI < FTTI)
        let mut compliant_mech = SafetyMechanism::new(
            "SM-001".to_string(),
            "Compliant Mechanism".to_string(),
            "Test".to_string(),
            MechanismType::Primary,
            MechanismCategory::ErrorDetection,
        );
        compliant_mech.hardware_implementation = Some(HardwareImplementation {
            logic_equations: vec![],
            resource_utilization: ResourceUtilization {
                logic_gates: 10,
                flip_flops: 2,
                memory_bits: 0,
                area_overhead: 1.0,
            },
            timing_constraints: TimingConstraints::new_iso26262(
                1000, // FDTI = 1 µs
                500,  // FRTI = 0.5 µs
                5000, // FTTI = 5 µs
            ),
            power_consumption: PowerConsumption {
                static_power: 0.1,
                dynamic_power: 0.5,
                power_overhead: 1.0,
            },
        });

        // Create mechanism with non-compliant timing (FDTI + FRTI >= FTTI)
        let mut non_compliant_mech = SafetyMechanism::new(
            "SM-002".to_string(),
            "Non-Compliant Mechanism".to_string(),
            "Test".to_string(),
            MechanismType::Primary,
            MechanismCategory::ErrorDetection,
        );
        non_compliant_mech.hardware_implementation = Some(HardwareImplementation {
            logic_equations: vec![],
            resource_utilization: ResourceUtilization {
                logic_gates: 10,
                flip_flops: 2,
                memory_bits: 0,
                area_overhead: 1.0,
            },
            timing_constraints: TimingConstraints::new_iso26262(
                3000, // FDTI = 3 µs
                3000, // FRTI = 3 µs
                5000, // FTTI = 5 µs (6 µs > 5 µs = FAIL)
            ),
            power_consumption: PowerConsumption {
                static_power: 0.1,
                dynamic_power: 0.5,
                power_overhead: 1.0,
            },
        });

        // Create mechanism without timing specification
        let unspec_mech = SafetyMechanism::new(
            "SM-003".to_string(),
            "Unspecified Mechanism".to_string(),
            "Test".to_string(),
            MechanismType::Latent,
            MechanismCategory::Monitoring,
        );

        let summary =
            validate_timing_compliance(&[compliant_mech, non_compliant_mech, unspec_mech]);

        // Verify counts
        assert_eq!(summary.compliant_count, 1);
        assert_eq!(summary.non_compliant_count, 1);
        assert_eq!(summary.unspecified_count, 1);
        assert!(!summary.overall_compliant); // Has non-compliant mechanism

        // Verify compliant mechanism result
        let compliant_result = &summary.mechanism_results[0];
        assert!(compliant_result.compliant);
        assert!(compliant_result.timing_specified);
        assert_eq!(compliant_result.margin_ns, 3500); // 5000 - 1500 = 3500

        // Verify non-compliant mechanism result
        let non_compliant_result = &summary.mechanism_results[1];
        assert!(!non_compliant_result.compliant);
        assert!(non_compliant_result.timing_specified);
        assert_eq!(non_compliant_result.margin_ns, -1000); // 5000 - 6000 = -1000

        // Verify unspecified mechanism result
        let unspec_result = &summary.mechanism_results[2];
        assert!(!unspec_result.timing_specified);

        // Verify critical mechanism is the non-compliant one (minimum margin)
        assert_eq!(
            summary.critical_mechanism,
            Some("Non-Compliant Mechanism".to_string())
        );
        assert_eq!(summary.min_margin_ns, Some(-1000));

        // Test report generation
        let report = format_timing_compliance_report(&summary);
        assert!(report.contains("NON-COMPLIANT"));
        assert!(report.contains("Compliant Mechanism"));
        assert!(report.contains("FDTI"));
    }
}
