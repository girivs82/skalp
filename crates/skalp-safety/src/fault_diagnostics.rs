//! Enhanced Fault Diagnostics for Safety Analysis
//!
//! Provides detailed classification of undetected faults to help engineers
//! understand WHY faults are not detected and prioritize fixes.

use crate::fault_simulation::{FaultSite, FaultType};
use serde::{Deserialize, Serialize};
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
    /// Same fault in all redundant channels (common cause failure potential)
    CommonCausePotential,
    /// SM detected but detection came too late (after FTTI)
    LateDetection,
    /// Unknown classification
    Unknown,
}

impl UndetectedReason {
    /// Human-readable name
    pub fn name(&self) -> &'static str {
        match self {
            UndetectedReason::SmInternal => "SM Internal",
            UndetectedReason::OutputPath => "Output Path",
            UndetectedReason::InputPath => "Input Path",
            UndetectedReason::CoverageGap => "Coverage Gap",
            UndetectedReason::CommonCausePotential => "Common Cause",
            UndetectedReason::LateDetection => "Late Detection",
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
            UndetectedReason::CommonCausePotential => {
                "Add diversity or independence between channels"
            }
            UndetectedReason::LateDetection => "Reduce detection latency or increase FTTI budget",
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
            UndetectedReason::CommonCausePotential => "ISO 26262-9:7.4 (CCF analysis)",
            UndetectedReason::LateDetection => "ISO 26262-5:7.4.2 (FTTI)",
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
    pub fn classify_all(
        &self,
        faults: &[FaultSite],
    ) -> (Vec<ClassifiedFault>, FaultClassificationSummary) {
        let mut classified = Vec::new();
        let mut summary = FaultClassificationSummary::default();

        for fault in faults {
            let cf = self.classify_fault(fault);

            // Update summary
            *summary.by_reason.entry(cf.reason).or_insert(0) += 1;
            *summary
                .by_component
                .entry(cf.component.clone())
                .or_insert(0) += 1;
            *summary.fit_by_reason.entry(cf.reason).or_insert(0.0) += cf.fit_contribution;

            classified.push(cf);
        }

        // Sort by FIT contribution to find top PMHF contributors
        let mut fit_by_path: HashMap<String, f64> = HashMap::new();
        for cf in &classified {
            *fit_by_path
                .entry(cf.fault_site.primitive_path.to_string())
                .or_insert(0.0) += cf.fit_contribution;
        }
        let mut contributors: Vec<_> = fit_by_path.into_iter().collect();
        contributors.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
        summary.top_pmhf_contributors = contributors.into_iter().take(10).collect();

        // Identify common cause potential
        self.identify_common_cause(&mut classified);

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
        // Remove numeric suffix (e.g., "mux_0" -> "mux")
        let name = last.split('_').next().unwrap_or(last);
        return name.to_string();
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
