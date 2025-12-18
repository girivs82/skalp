//! SEooC (Safety Element out of Context) Analysis
//!
//! Implements ISO 26262-10:9 SEooC analysis for hardware designs that are developed
//! without full knowledge of the integrating system.
//!
//! Key features:
//! - Calculate internal SPFM/LFM from internal safety mechanisms
//! - Derive required DC for assumed external mechanisms based on gap to target ASIL
//! - Support iterative design flow (baseline mode without external mechanisms)
//! - Generate derived safety requirements for system integrator

use crate::asil::AsilLevel;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Result of SEooC analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SeoocAnalysisResult {
    /// Entity name being analyzed
    pub entity_name: String,
    /// Target ASIL level
    pub target_asil: AsilLevel,
    /// Internal SPFM achieved by internal mechanisms only
    pub internal_spfm: f64,
    /// Internal LFM achieved by internal mechanisms only
    pub internal_lfm: f64,
    /// Gap to target SPFM (target - internal)
    pub spfm_gap: f64,
    /// Gap to target LFM (target - internal)
    pub lfm_gap: f64,
    /// Total dangerous faults found
    pub total_dangerous_faults: usize,
    /// Internally detected faults
    pub internally_detected_faults: usize,
    /// Undetected faults by category (for baseline mode suggestions)
    pub undetected_by_category: UndetectedFaultCategories,
    /// Derived safety requirements (one per assumed mechanism)
    pub derived_requirements: Vec<DerivedSafetyRequirement>,
    /// Projected SPFM if all mechanisms achieve their required DC
    pub projected_spfm: f64,
    /// Whether target ASIL is achievable with current mechanisms
    pub target_achievable: bool,
    /// Power domain CCF coverage (for mechanisms covering VoltageDropout/GroundBounce)
    pub power_ccf_coverage: Option<PowerCcfCoverage>,
}

/// Power CCF coverage by an assumed mechanism
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PowerCcfCoverage {
    /// Mechanism ID that covers power faults
    pub mechanism_id: String,
    /// Original CCF contribution (λDPF_power in FIT)
    pub original_ccf_fit: f64,
    /// Assumed DC for the mechanism
    pub assumed_dc: f64,
    /// Residual CCF after mechanism (FIT)
    pub residual_ccf_fit: f64,
    /// PMHF improvement from this coverage (FIT reduction)
    pub pmhf_improvement_fit: f64,
}

/// Categorization of undetected faults for baseline analysis
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct UndetectedFaultCategories {
    /// Permanent faults (StuckAt0, StuckAt1)
    pub permanent: usize,
    /// Transient faults (BitFlip, SEU)
    pub transient: usize,
    /// Power-related faults (VoltageDropout, GroundBounce)
    pub power: usize,
    /// Total undetected
    pub total: usize,
}

/// Derived safety requirement for an assumed external mechanism
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DerivedSafetyRequirement {
    /// Requirement ID (e.g., "REQ-EXT-001")
    pub id: String,
    /// Assumed mechanism ID from the design
    pub assumed_mechanism_id: String,
    /// Mechanism type (e.g., "voltage_monitor", "watchdog")
    pub mechanism_type: String,
    /// Fault types this mechanism covers
    pub fault_types_covered: Vec<String>,
    /// Number of faults this mechanism could detect
    pub fault_count: usize,
    /// Required DC to achieve target ASIL (calculated)
    pub required_dc: f64,
    /// DC category per ISO 26262-5 Table D.5
    pub dc_category: DcCategory,
    /// SPFM contribution if this mechanism achieves required DC
    pub spfm_contribution: f64,
    /// Projected cumulative SPFM after this mechanism
    pub projected_cumulative_spfm: f64,
    /// Rationale for the derived DC requirement
    pub rationale: String,
}

/// Diagnostic Coverage category per ISO 26262-5 Table D.5
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DcCategory {
    /// DC < 60% (not recommended for safety-critical)
    None,
    /// 60% ≤ DC < 90%
    Low,
    /// 90% ≤ DC < 99%
    Medium,
    /// DC ≥ 99%
    High,
}

impl std::fmt::Display for DcCategory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DcCategory::None => write!(f, "None (<60%)"),
            DcCategory::Low => write!(f, "Low (60-90%)"),
            DcCategory::Medium => write!(f, "Medium (90-99%)"),
            DcCategory::High => write!(f, "High (≥99%)"),
        }
    }
}

impl DcCategory {
    /// Categorize a DC value per ISO 26262-5 Table D.5
    pub fn from_dc(dc: f64) -> Self {
        if dc >= 99.0 {
            DcCategory::High
        } else if dc >= 90.0 {
            DcCategory::Medium
        } else if dc >= 60.0 {
            DcCategory::Low
        } else {
            DcCategory::None
        }
    }
}

/// Input configuration for SEooC analysis
#[derive(Debug, Clone)]
pub struct SeoocAnalysisConfig {
    /// Entity name
    pub entity_name: String,
    /// Target ASIL level
    pub target_asil: AsilLevel,
    /// Assumed external mechanisms (from #[assumed_mechanism] attributes)
    pub assumed_mechanisms: Vec<AssumedMechanism>,
}

/// Assumed external mechanism configuration
#[derive(Debug, Clone)]
pub struct AssumedMechanism {
    /// Mechanism ID (e.g., "EXT_VM")
    pub id: String,
    /// Mechanism type (e.g., "voltage_monitor")
    pub mechanism_type: String,
    /// Fault types this mechanism can detect
    pub covers: Vec<String>,
    /// Optional description
    pub description: Option<String>,
}

/// Fault injection results needed for SEooC analysis
#[derive(Debug, Clone)]
pub struct FaultInjectionData {
    /// Total dangerous faults (faults that corrupt outputs)
    pub total_dangerous: usize,
    /// Faults detected by internal mechanisms
    pub internally_detected: usize,
    /// Undetected faults with their types
    pub undetected_faults: Vec<UndetectedFault>,
}

/// Power domain CCF (Common Cause Failure) data for SEooC analysis
/// Power faults are treated as CCF because they affect entire domains simultaneously
#[derive(Debug, Clone, Default)]
pub struct PowerDomainCcfData {
    /// Total CCF contribution from power domains (λDPF_power in FIT)
    pub ccf_fit: f64,
    /// Number of power domains analyzed
    pub domain_count: usize,
    /// Whether this affects PMHF calculation
    pub affects_pmhf: bool,
}

/// An undetected fault with its classification
#[derive(Debug, Clone)]
pub struct UndetectedFault {
    /// Fault site path
    pub site: String,
    /// Fault type (e.g., "StuckAt0", "BitFlip", "VoltageDropout")
    pub fault_type: String,
    /// FIT contribution of this fault
    pub fit: f64,
}

/// Check if a mechanism covers power fault types (VoltageDropout/GroundBounce)
fn mechanism_covers_power_faults(mechanism: &AssumedMechanism) -> bool {
    for cover in &mechanism.covers {
        let cover_lower = cover.to_lowercase();
        if cover_lower.contains("voltage")
            || cover_lower.contains("ground")
            || cover_lower.contains("power")
        {
            return true;
        }
    }
    false
}

/// Analyze a SEooC design and derive external mechanism requirements
/// This version supports power domain CCF coverage for voltage monitors
pub fn analyze_seooc_with_ccf(
    config: &SeoocAnalysisConfig,
    fi_data: &FaultInjectionData,
    power_ccf: Option<&PowerDomainCcfData>,
) -> SeoocAnalysisResult {
    // Get ASIL targets
    let (target_spfm, target_lfm) = get_asil_targets(config.target_asil);

    // Calculate internal metrics
    let internal_spfm = if fi_data.total_dangerous > 0 {
        (fi_data.internally_detected as f64 / fi_data.total_dangerous as f64) * 100.0
    } else {
        100.0
    };

    // For now, LFM uses same calculation (could be refined for boot-time vs runtime)
    let internal_lfm = internal_spfm;

    // Calculate gaps
    let spfm_gap = (target_spfm - internal_spfm).max(0.0);
    let lfm_gap = (target_lfm - internal_lfm).max(0.0);

    // Categorize undetected faults
    let mut undetected_by_category = categorize_undetected_faults(&fi_data.undetected_faults);

    // Add power CCF to the undetected count if present
    // Power CCF represents faults that would cause domain-wide failures
    if let Some(ccf) = power_ccf {
        if ccf.ccf_fit > 0.0 && ccf.affects_pmhf {
            // Represent power CCF as "virtual" undetected power faults for display
            // The actual impact is on PMHF, not SPFM
            undetected_by_category.power = ccf.domain_count;
        }
    }

    // Calculate derived requirements for each assumed mechanism
    let mut derived_requirements = Vec::new();
    let mut cumulative_spfm = internal_spfm;
    let mut req_counter = 1;
    let mut power_ccf_coverage = None;

    for mechanism in &config.assumed_mechanisms {
        // Check if this mechanism covers power faults (CCF path)
        if mechanism_covers_power_faults(mechanism) {
            if let Some(ccf) = power_ccf {
                if ccf.ccf_fit > 0.0 {
                    // This mechanism covers power CCF - calculate PMHF impact
                    // For a voltage monitor with DC ≥ 99%, residual = CCF × (1 - DC)
                    let assumed_dc = 99.0; // High DC assumed for voltage monitors
                    let residual_ccf_fit = ccf.ccf_fit * (1.0 - assumed_dc / 100.0);
                    let pmhf_improvement = ccf.ccf_fit - residual_ccf_fit;

                    power_ccf_coverage = Some(PowerCcfCoverage {
                        mechanism_id: mechanism.id.clone(),
                        original_ccf_fit: ccf.ccf_fit,
                        assumed_dc,
                        residual_ccf_fit,
                        pmhf_improvement_fit: pmhf_improvement,
                    });

                    // Create a derived requirement for the power CCF coverage
                    let rationale = format!(
                        "Voltage monitor covers power domain CCF ({:.2} FIT). With DC ≥ {:.0}%, reduces λDPF_CCF to {:.2} FIT.",
                        ccf.ccf_fit, assumed_dc, residual_ccf_fit
                    );

                    derived_requirements.push(DerivedSafetyRequirement {
                        id: format!("REQ-EXT-{:03}", req_counter),
                        assumed_mechanism_id: mechanism.id.clone(),
                        mechanism_type: mechanism.mechanism_type.clone(),
                        fault_types_covered: mechanism.covers.clone(),
                        fault_count: ccf.domain_count, // Number of power domains
                        required_dc: assumed_dc,
                        dc_category: DcCategory::High,
                        spfm_contribution: 0.0, // CCF affects PMHF, not SPFM
                        projected_cumulative_spfm: cumulative_spfm,
                        rationale,
                    });
                    req_counter += 1;
                    continue;
                }
            }
        }

        // Count faults this mechanism covers (non-power faults)
        let covered_faults: Vec<&UndetectedFault> = fi_data
            .undetected_faults
            .iter()
            .filter(|f| mechanism_covers_fault(mechanism, &f.fault_type))
            .collect();

        let fault_count = covered_faults.len();
        if fault_count == 0 {
            continue;
        }

        // Calculate required DC to close the gap
        let faults_still_needed = ((target_spfm - cumulative_spfm) / 100.0
            * fi_data.total_dangerous as f64)
            .ceil() as usize;

        let required_dc = if faults_still_needed > 0 && fault_count > 0 {
            let raw_dc = (faults_still_needed as f64 / fault_count as f64) * 100.0;
            raw_dc.clamp(60.0, 99.9)
        } else {
            60.0
        };

        // Calculate SPFM contribution
        let spfm_contribution =
            (fault_count as f64 * required_dc / 100.0) / fi_data.total_dangerous as f64 * 100.0;

        cumulative_spfm = (cumulative_spfm + spfm_contribution).min(100.0);

        let dc_category = DcCategory::from_dc(required_dc);

        let rationale = format!(
            "To achieve {} (SPFM ≥ {:.0}%), this mechanism must detect at least {:.0}% of the {} {} faults it covers.",
            config.target_asil, target_spfm, required_dc, fault_count,
            mechanism.covers.join("/")
        );

        derived_requirements.push(DerivedSafetyRequirement {
            id: format!("REQ-EXT-{:03}", req_counter),
            assumed_mechanism_id: mechanism.id.clone(),
            mechanism_type: mechanism.mechanism_type.clone(),
            fault_types_covered: mechanism.covers.clone(),
            fault_count,
            required_dc,
            dc_category,
            spfm_contribution,
            projected_cumulative_spfm: cumulative_spfm,
            rationale,
        });

        req_counter += 1;
    }

    let target_achievable = cumulative_spfm >= target_spfm;

    SeoocAnalysisResult {
        entity_name: config.entity_name.clone(),
        target_asil: config.target_asil,
        internal_spfm,
        internal_lfm,
        spfm_gap,
        lfm_gap,
        total_dangerous_faults: fi_data.total_dangerous,
        internally_detected_faults: fi_data.internally_detected,
        undetected_by_category,
        derived_requirements,
        projected_spfm: cumulative_spfm,
        target_achievable,
        power_ccf_coverage,
    }
}

/// Analyze a SEooC design and derive external mechanism requirements (legacy, no CCF)
pub fn analyze_seooc(
    config: &SeoocAnalysisConfig,
    fi_data: &FaultInjectionData,
) -> SeoocAnalysisResult {
    analyze_seooc_with_ccf(config, fi_data, None)
}

/// Get SPFM and LFM targets for an ASIL level
fn get_asil_targets(asil: AsilLevel) -> (f64, f64) {
    match asil {
        AsilLevel::D => (99.0, 90.0),
        AsilLevel::C => (97.0, 80.0),
        AsilLevel::B => (90.0, 60.0),
        AsilLevel::A => (90.0, 60.0),
        AsilLevel::QM => (0.0, 0.0),
    }
}

/// Categorize undetected faults by type
fn categorize_undetected_faults(faults: &[UndetectedFault]) -> UndetectedFaultCategories {
    let mut categories = UndetectedFaultCategories::default();

    for fault in faults {
        categories.total += 1;

        let fault_type_lower = fault.fault_type.to_lowercase();
        if fault_type_lower.contains("stuck")
            || fault_type_lower.contains("sa0")
            || fault_type_lower.contains("sa1")
            || fault_type_lower.contains("voltage")
        {
            // VoltageDropout is categorized as permanent per ISO 26262
            categories.permanent += 1;
        } else if fault_type_lower.contains("bitflip")
            || fault_type_lower.contains("seu")
            || fault_type_lower.contains("transient")
            || fault_type_lower.contains("ground")
        {
            // GroundBounce is categorized as transient per ISO 26262
            categories.transient += 1;
        } else if fault_type_lower.contains("power") {
            // Generic power faults
            categories.power += 1;
        } else {
            // Default to permanent for unknown types
            categories.permanent += 1;
        }
    }

    // Power faults are also tracked as a view
    for fault in faults {
        let fault_type_lower = fault.fault_type.to_lowercase();
        if fault_type_lower.contains("voltage") || fault_type_lower.contains("ground") {
            categories.power += 1;
        }
    }

    categories
}

/// Check if a mechanism covers a given fault type
fn mechanism_covers_fault(mechanism: &AssumedMechanism, fault_type: &str) -> bool {
    let fault_type_lower = fault_type.to_lowercase();

    for cover in &mechanism.covers {
        let cover_lower = cover.to_lowercase();

        // Direct match
        if fault_type_lower.contains(&cover_lower) || cover_lower.contains(&fault_type_lower) {
            return true;
        }

        // Common aliases
        match cover_lower.as_str() {
            "voltagedropout" | "voltage_dropout" => {
                if fault_type_lower.contains("voltage") {
                    return true;
                }
            }
            "groundbounce" | "ground_bounce" => {
                if fault_type_lower.contains("ground") || fault_type_lower.contains("bounce") {
                    return true;
                }
            }
            "stuckat0" | "stuck_at_0" | "sa0" => {
                if fault_type_lower.contains("stuck_at_0")
                    || fault_type_lower.contains("stuckat0")
                    || fault_type_lower == "sa0"
                {
                    return true;
                }
            }
            "stuckat1" | "stuck_at_1" | "sa1" => {
                if fault_type_lower.contains("stuck_at_1")
                    || fault_type_lower.contains("stuckat1")
                    || fault_type_lower == "sa1"
                {
                    return true;
                }
            }
            "bitflip" | "bit_flip" | "seu" => {
                if fault_type_lower.contains("bitflip")
                    || fault_type_lower.contains("transient")
                    || fault_type_lower.contains("seu")
                {
                    return true;
                }
            }
            _ => {}
        }
    }

    false
}

/// Generate baseline analysis suggestions when no mechanisms are declared
pub fn generate_baseline_suggestions(
    undetected: &UndetectedFaultCategories,
    target_asil: AsilLevel,
) -> Vec<String> {
    let mut suggestions = Vec::new();

    if undetected.permanent > 0 {
        suggestions.push(format!(
            "Add watchdog or control flow monitor for {} permanent faults (StuckAt)",
            undetected.permanent
        ));
    }

    if undetected.transient > 0 {
        suggestions.push(format!(
            "Add ECC or TMR for {} transient faults (BitFlip/SEU)",
            undetected.transient
        ));
    }

    if undetected.power > 0 {
        suggestions.push(format!(
            "Add voltage_monitor for {} power-related faults",
            undetected.power
        ));
    }

    if target_asil >= AsilLevel::C {
        suggestions.push("Consider E2E protection for data path integrity".to_string());
    }

    if target_asil == AsilLevel::D {
        suggestions.push("Consider redundant clock sources with clock_monitor".to_string());
    }

    suggestions
}

/// Format SEooC analysis result as a report
pub fn format_seooc_report(result: &SeoocAnalysisResult) -> String {
    let mut output = String::new();

    output.push_str(&format!(
        "\n{} SEooC Analysis: {}\n",
        "🛡️", result.entity_name
    ));
    output.push_str(&"━".repeat(50));
    output.push_str(&format!("\nTarget ASIL: {}\n\n", result.target_asil));

    // Internal metrics
    output.push_str("📊 Internal Metrics:\n");
    let (target_spfm, _target_lfm) = get_asil_targets(result.target_asil);
    output.push_str(&format!(
        "   SPFM: {:.1}% (target: {:.0}%)\n",
        result.internal_spfm, target_spfm
    ));

    if result.spfm_gap > 0.0 {
        let faults_needed = (result.spfm_gap / 100.0 * result.total_dangerous_faults as f64).ceil();
        output.push_str(&format!(
            "   Gap: {:.1}% (need {:.0} more faults detected)\n",
            result.spfm_gap, faults_needed
        ));
    } else {
        output.push_str("   Gap: None (internal mechanisms sufficient)\n");
    }

    output.push('\n');

    // Undetected fault breakdown (baseline mode info)
    if result.undetected_by_category.total > 0 {
        output.push_str("📋 Undetected Faults by Category:\n");
        if result.undetected_by_category.permanent > 0 {
            output.push_str(&format!(
                "   ├─ Permanent (SA0/SA1): {} faults ({:.0}%)\n",
                result.undetected_by_category.permanent,
                result.undetected_by_category.permanent as f64
                    / result.undetected_by_category.total as f64
                    * 100.0
            ));
        }
        if result.undetected_by_category.transient > 0 {
            output.push_str(&format!(
                "   ├─ Transient (BitFlip/SEU): {} faults ({:.0}%)\n",
                result.undetected_by_category.transient,
                result.undetected_by_category.transient as f64
                    / result.undetected_by_category.total as f64
                    * 100.0
            ));
        }
        if result.undetected_by_category.power > 0 {
            output.push_str(&format!(
                "   └─ Power (Voltage/Ground): {} faults ({:.0}%)\n",
                result.undetected_by_category.power,
                result.undetected_by_category.power as f64
                    / result.undetected_by_category.total as f64
                    * 100.0
            ));
        }
        output.push('\n');
    }

    // Derived requirements
    if !result.derived_requirements.is_empty() {
        output.push_str("📋 Derived Requirements:\n\n");

        for req in &result.derived_requirements {
            output.push_str(&format!(
                "   {}: {} ({})\n",
                req.id, req.mechanism_type, req.assumed_mechanism_id
            ));
            output.push_str(&format!(
                "   ├─ Covers: {} ({} faults)\n",
                req.fault_types_covered.join(", "),
                req.fault_count
            ));
            output.push_str(&format!("   ├─ Required DC: ≥ {:.0}%\n", req.required_dc));
            output.push_str(&format!(
                "   ├─ DC Category: {} (ISO 26262-5 Table D.5)\n",
                req.dc_category
            ));
            output.push_str(&format!(
                "   ├─ Contribution: +{:.1}% SPFM\n",
                req.spfm_contribution
            ));
            output.push_str(&format!(
                "   └─ Projected SPFM: {:.1}%\n\n",
                req.projected_cumulative_spfm
            ));
        }

        // Summary
        output.push_str("📈 Summary:\n");
        output.push_str(&format!("   Internal SPFM: {:.1}%\n", result.internal_spfm));

        for req in &result.derived_requirements {
            output.push_str(&format!(
                "   + {}: +{:.1}% → {:.1}%\n",
                req.assumed_mechanism_id, req.spfm_contribution, req.projected_cumulative_spfm
            ));
        }

        output.push_str(&format!("   Target: {:.0}% ", target_spfm));
        if result.target_achievable {
            output.push_str("✓ ACHIEVABLE\n");
        } else {
            output.push_str("✗ GAP REMAINS\n");
        }
    } else {
        // Baseline mode - no mechanisms declared
        output.push_str("💡 Suggestions (no external mechanisms declared):\n");
        let suggestions =
            generate_baseline_suggestions(&result.undetected_by_category, result.target_asil);
        for suggestion in suggestions {
            output.push_str(&format!("   - {}\n", suggestion));
        }
    }

    // Power CCF coverage (if applicable)
    if let Some(ref ccf_cov) = result.power_ccf_coverage {
        output.push_str("\n⚡ Power Domain CCF Coverage:\n");
        output.push_str(&format!(
            "   Mechanism: {} (voltage_monitor)\n",
            ccf_cov.mechanism_id
        ));
        output.push_str(&format!(
            "   ├─ Original λDPF_CCF: {:.2} FIT\n",
            ccf_cov.original_ccf_fit
        ));
        output.push_str(&format!("   ├─ Assumed DC: ≥ {:.0}%\n", ccf_cov.assumed_dc));
        output.push_str(&format!(
            "   ├─ Residual λDPF_CCF: {:.2} FIT\n",
            ccf_cov.residual_ccf_fit
        ));
        output.push_str(&format!(
            "   └─ PMHF Improvement: -{:.2} FIT\n",
            ccf_cov.pmhf_improvement_fit
        ));
    }

    // Integration checklist
    if !result.derived_requirements.is_empty() {
        output.push_str("\n✅ Integration Checklist:\n");
        for req in &result.derived_requirements {
            output.push_str(&format!(
                "   [ ] {}: {} DC ≥ {:.0}%\n",
                req.id, req.mechanism_type, req.required_dc
            ));
        }
    }

    output.push('\n');
    output
}

/// Convert HIR assumed mechanism config to analysis format
pub fn from_hir_assumed_mechanism(
    hir_config: &skalp_frontend::hir::AssumedMechanismConfig,
) -> AssumedMechanism {
    AssumedMechanism {
        id: hir_config.id.clone(),
        mechanism_type: hir_config.mechanism_type.clone(),
        covers: hir_config.covers.clone(),
        description: hir_config.description.clone(),
    }
}

/// Convert HIR SEooC config to analysis config
pub fn from_hir_seooc_config(
    entity_name: &str,
    hir_config: &skalp_frontend::hir::SeoocConfig,
) -> SeoocAnalysisConfig {
    let target_asil = match hir_config.target_asil.to_uppercase().as_str() {
        "D" => AsilLevel::D,
        "C" => AsilLevel::C,
        "B" => AsilLevel::B,
        "A" => AsilLevel::A,
        _ => AsilLevel::QM,
    };

    let assumed_mechanisms = hir_config
        .assumed_mechanisms
        .iter()
        .map(from_hir_assumed_mechanism)
        .collect();

    SeoocAnalysisConfig {
        entity_name: entity_name.to_string(),
        target_asil,
        assumed_mechanisms,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dc_category_from_dc() {
        assert_eq!(DcCategory::from_dc(99.5), DcCategory::High);
        assert_eq!(DcCategory::from_dc(99.0), DcCategory::High);
        assert_eq!(DcCategory::from_dc(95.0), DcCategory::Medium);
        assert_eq!(DcCategory::from_dc(90.0), DcCategory::Medium);
        assert_eq!(DcCategory::from_dc(75.0), DcCategory::Low);
        assert_eq!(DcCategory::from_dc(60.0), DcCategory::Low);
        assert_eq!(DcCategory::from_dc(50.0), DcCategory::None);
    }

    #[test]
    fn test_asil_targets() {
        assert_eq!(get_asil_targets(AsilLevel::D), (99.0, 90.0));
        assert_eq!(get_asil_targets(AsilLevel::C), (97.0, 80.0));
        assert_eq!(get_asil_targets(AsilLevel::B), (90.0, 60.0));
    }

    #[test]
    fn test_mechanism_covers_fault() {
        let vm_mechanism = AssumedMechanism {
            id: "EXT_VM".to_string(),
            mechanism_type: "voltage_monitor".to_string(),
            covers: vec!["VoltageDropout".to_string(), "GroundBounce".to_string()],
            description: None,
        };

        assert!(mechanism_covers_fault(&vm_mechanism, "VoltageDropout"));
        assert!(mechanism_covers_fault(&vm_mechanism, "voltage_dropout"));
        assert!(mechanism_covers_fault(&vm_mechanism, "GroundBounce"));
        assert!(!mechanism_covers_fault(&vm_mechanism, "StuckAt0"));
    }

    #[test]
    fn test_analyze_seooc_baseline() {
        let config = SeoocAnalysisConfig {
            entity_name: "TestEntity".to_string(),
            target_asil: AsilLevel::D,
            assumed_mechanisms: vec![], // No mechanisms = baseline mode
        };

        let fi_data = FaultInjectionData {
            total_dangerous: 100,
            internally_detected: 45,
            undetected_faults: vec![
                UndetectedFault {
                    site: "test.cell1".to_string(),
                    fault_type: "stuck_at_0".to_string(),
                    fit: 0.1,
                },
                UndetectedFault {
                    site: "test.cell2".to_string(),
                    fault_type: "transient".to_string(),
                    fit: 0.1,
                },
            ],
        };

        let result = analyze_seooc(&config, &fi_data);

        assert_eq!(result.internal_spfm, 45.0);
        assert_eq!(result.spfm_gap, 54.0); // 99 - 45
        assert!(result.derived_requirements.is_empty()); // No mechanisms declared
        assert!(!result.target_achievable);
    }

    #[test]
    fn test_analyze_seooc_with_mechanisms() {
        let config = SeoocAnalysisConfig {
            entity_name: "TestEntity".to_string(),
            target_asil: AsilLevel::D,
            assumed_mechanisms: vec![AssumedMechanism {
                id: "EXT_WDG".to_string(),
                mechanism_type: "watchdog".to_string(),
                covers: vec!["StuckAt0".to_string(), "StuckAt1".to_string()],
                description: None,
            }],
        };

        // Create 100 total faults, 45 internally detected, 55 undetected stuck-at
        let mut undetected_faults = Vec::new();
        for i in 0..55 {
            undetected_faults.push(UndetectedFault {
                site: format!("test.cell{}", i),
                fault_type: if i % 2 == 0 {
                    "stuck_at_0".to_string()
                } else {
                    "stuck_at_1".to_string()
                },
                fit: 0.1,
            });
        }

        let fi_data = FaultInjectionData {
            total_dangerous: 100,
            internally_detected: 45,
            undetected_faults,
        };

        let result = analyze_seooc(&config, &fi_data);

        assert_eq!(result.internal_spfm, 45.0);
        assert_eq!(result.derived_requirements.len(), 1);

        let req = &result.derived_requirements[0];
        assert_eq!(req.assumed_mechanism_id, "EXT_WDG");
        assert_eq!(req.fault_count, 55);
        assert!(req.required_dc >= 60.0); // At least minimum DC
    }

    #[test]
    fn test_format_report() {
        let result = SeoocAnalysisResult {
            entity_name: "TestMCU".to_string(),
            target_asil: AsilLevel::D,
            internal_spfm: 45.0,
            internal_lfm: 45.0,
            spfm_gap: 54.0,
            lfm_gap: 45.0,
            total_dangerous_faults: 100,
            internally_detected_faults: 45,
            undetected_by_category: UndetectedFaultCategories {
                permanent: 40,
                transient: 10,
                power: 5,
                total: 55,
            },
            derived_requirements: vec![],
            projected_spfm: 45.0,
            target_achievable: false,
            power_ccf_coverage: None,
        };

        let report = format_seooc_report(&result);
        assert!(report.contains("TestMCU"));
        assert!(report.contains("ASIL D"));
        assert!(report.contains("45.0%"));
        assert!(report.contains("Suggestions"));
    }
}
