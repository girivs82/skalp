//! Common Cause Failure (CCF) Analysis
//!
//! Implements dependent failure analysis for ISO 26262 compliance.
//! Common cause failures affect multiple elements simultaneously due to shared
//! resources like clocks, resets, power domains, or physical proximity.
//!
//! ISO 26262 requires dependent failure analysis (DFA) for ASIL C and D.
//! This module identifies potential CCF groups and applies beta factors
//! to split FIT rates into independent and common cause portions.

use std::collections::{HashMap, HashSet};

use serde::{Deserialize, Serialize};

/// Common cause failure group
///
/// Represents a group of elements that share a potential common cause
/// of failure. All members of a CCF group may fail simultaneously
/// due to the shared cause.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CcfGroup {
    /// Group name (e.g., "clk_main_domain", "reset_tree", "nand_cells")
    pub name: String,
    /// Cell paths that belong to this group
    pub members: Vec<String>,
    /// Beta factor: fraction of failures that are common cause (0.0-1.0)
    /// Higher beta means more failures are correlated
    pub beta_factor: f64,
    /// Type of common cause
    pub cause: CcfCause,
    /// Optional description of the group
    pub description: Option<String>,
}

impl CcfGroup {
    /// Create a new CCF group
    pub fn new(name: &str, cause: CcfCause, beta_factor: f64) -> Self {
        Self {
            name: name.to_string(),
            members: Vec::new(),
            beta_factor,
            cause,
            description: None,
        }
    }

    /// Add a member cell path to this group
    pub fn add_member(&mut self, cell_path: String) {
        self.members.push(cell_path);
    }

    /// Set optional description
    pub fn with_description(mut self, desc: &str) -> Self {
        self.description = Some(desc.to_string());
        self
    }

    /// Number of members in the group
    pub fn member_count(&self) -> usize {
        self.members.len()
    }
}

/// Types of common cause failures
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum CcfCause {
    /// All cells on the same clock domain
    /// Typical beta: 0.05 - 0.10
    SharedClock,
    /// All cells share the same reset signal
    /// Typical beta: 0.05
    SharedReset,
    /// Same power domain
    /// Typical beta: 0.05 - 0.10
    SharedPower,
    /// Physically adjacent cells (requires P&R data)
    /// Typical beta: 0.01
    PhysicalProximity,
    /// Same cell type (systematic design errors)
    /// Typical beta: 0.02
    SharedDesign,
    /// Same module/macro instance
    /// Typical beta: 0.03
    SharedModule,
    /// Shared interconnect/routing
    /// Typical beta: 0.01
    SharedRouting,
}

impl CcfCause {
    /// Get the typical beta factor for this cause type
    /// Based on ISO 26262 recommendations
    pub fn typical_beta(&self) -> f64 {
        match self {
            CcfCause::SharedClock => 0.07,       // Mid-range of 0.05-0.10
            CcfCause::SharedReset => 0.05,       // Standard value
            CcfCause::SharedPower => 0.07,       // Mid-range of 0.05-0.10
            CcfCause::PhysicalProximity => 0.01, // Low correlation
            CcfCause::SharedDesign => 0.02,      // Systematic errors
            CcfCause::SharedModule => 0.03,      // Module-level correlation
            CcfCause::SharedRouting => 0.01,     // Routing correlation
        }
    }

    /// Get a human-readable description of this cause
    pub fn description(&self) -> &'static str {
        match self {
            CcfCause::SharedClock => "Elements share a common clock signal",
            CcfCause::SharedReset => "Elements share a common reset signal",
            CcfCause::SharedPower => "Elements are in the same power domain",
            CcfCause::PhysicalProximity => "Elements are physically adjacent",
            CcfCause::SharedDesign => "Elements are the same cell type (systematic)",
            CcfCause::SharedModule => "Elements are in the same module instance",
            CcfCause::SharedRouting => "Elements share common routing resources",
        }
    }
}

/// Result of applying beta factor to a FIT rate
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct CcfFitSplit {
    /// Original total FIT rate
    pub total_fit: f64,
    /// Independent failure FIT (not correlated with others)
    pub independent_fit: f64,
    /// Common cause failure FIT (correlated with group)
    pub common_cause_fit: f64,
    /// Beta factor applied
    pub beta_factor: f64,
}

/// Apply beta factor to split FIT into independent and common cause portions
///
/// The beta factor model splits failures as:
/// - Independent FIT = total_fit × (1 - beta)
/// - Common cause FIT = total_fit × beta
///
/// # Arguments
/// * `fit` - Total FIT rate for the element
/// * `beta` - Beta factor (0.0-1.0), fraction of failures that are common cause
///
/// # Returns
/// A `CcfFitSplit` containing independent and common cause FIT values
pub fn apply_beta_factor(fit: f64, beta: f64) -> CcfFitSplit {
    let beta = beta.clamp(0.0, 1.0);
    CcfFitSplit {
        total_fit: fit,
        independent_fit: fit * (1.0 - beta),
        common_cause_fit: fit * beta,
        beta_factor: beta,
    }
}

/// CCF analysis results for a design
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CcfAnalysisResults {
    /// Design being analyzed
    pub design_name: String,
    /// Identified CCF groups
    pub groups: Vec<CcfGroup>,
    /// Total independent FIT
    pub total_independent_fit: f64,
    /// Total common cause FIT
    pub total_common_cause_fit: f64,
    /// Cells not assigned to any CCF group
    pub unassigned_cells: Vec<String>,
}

impl CcfAnalysisResults {
    /// Create new CCF analysis results
    pub fn new(design_name: &str) -> Self {
        Self {
            design_name: design_name.to_string(),
            groups: Vec::new(),
            total_independent_fit: 0.0,
            total_common_cause_fit: 0.0,
            unassigned_cells: Vec::new(),
        }
    }

    /// Add a CCF group to the results
    pub fn add_group(&mut self, group: CcfGroup) {
        self.groups.push(group);
    }

    /// Calculate totals from all groups
    pub fn calculate_totals(&mut self, cell_fits: &HashMap<String, f64>) {
        let mut assigned_cells: HashSet<String> = HashSet::new();

        for group in &self.groups {
            for member in &group.members {
                if let Some(&fit) = cell_fits.get(member) {
                    let split = apply_beta_factor(fit, group.beta_factor);
                    self.total_independent_fit += split.independent_fit;
                    self.total_common_cause_fit += split.common_cause_fit;
                }
                assigned_cells.insert(member.clone());
            }
        }

        // Find unassigned cells
        for cell in cell_fits.keys() {
            if !assigned_cells.contains(cell) {
                self.unassigned_cells.push(cell.clone());
                // Unassigned cells are treated as fully independent
                if let Some(&fit) = cell_fits.get(cell) {
                    self.total_independent_fit += fit;
                }
            }
        }
    }

    /// Get total FIT (independent + common cause)
    pub fn total_fit(&self) -> f64 {
        self.total_independent_fit + self.total_common_cause_fit
    }

    /// Get the CCF ratio (common cause / total)
    pub fn ccf_ratio(&self) -> f64 {
        let total = self.total_fit();
        if total > 0.0 {
            self.total_common_cause_fit / total
        } else {
            0.0
        }
    }
}

/// Identify CCF groups based on shared resources
///
/// This function analyzes a design to identify elements that share common
/// resources and may be subject to common cause failures.
///
/// # Arguments
/// * `clock_domains` - Map of clock name -> list of cell paths on that clock
/// * `reset_signals` - Map of reset name -> list of cell paths using that reset
/// * `cell_types` - Map of cell path -> cell type name
///
/// # Returns
/// A vector of identified CCF groups
pub fn identify_ccf_groups(
    clock_domains: &HashMap<String, Vec<String>>,
    reset_signals: &HashMap<String, Vec<String>>,
    cell_types: &HashMap<String, String>,
) -> Vec<CcfGroup> {
    let mut groups = Vec::new();

    // Create CCF groups for clock domains
    for (clock_name, cells) in clock_domains {
        if cells.len() > 1 {
            let mut group = CcfGroup::new(
                &format!("clk_{}", clock_name),
                CcfCause::SharedClock,
                CcfCause::SharedClock.typical_beta(),
            );
            group.description = Some(format!("Elements on clock domain '{}'", clock_name));
            for cell in cells {
                group.add_member(cell.clone());
            }
            groups.push(group);
        }
    }

    // Create CCF groups for reset signals
    for (reset_name, cells) in reset_signals {
        if cells.len() > 1 {
            let mut group = CcfGroup::new(
                &format!("rst_{}", reset_name),
                CcfCause::SharedReset,
                CcfCause::SharedReset.typical_beta(),
            );
            group.description = Some(format!("Elements sharing reset signal '{}'", reset_name));
            for cell in cells {
                group.add_member(cell.clone());
            }
            groups.push(group);
        }
    }

    // Create CCF groups for same cell types (systematic errors)
    let mut type_groups: HashMap<String, Vec<String>> = HashMap::new();
    for (cell_path, cell_type) in cell_types {
        type_groups
            .entry(cell_type.clone())
            .or_default()
            .push(cell_path.clone());
    }

    for (cell_type, cells) in type_groups {
        if cells.len() > 1 {
            let mut group = CcfGroup::new(
                &format!("type_{}", cell_type),
                CcfCause::SharedDesign,
                CcfCause::SharedDesign.typical_beta(),
            );
            group.description = Some(format!(
                "Same cell type '{}' (systematic failure mode)",
                cell_type
            ));
            for cell in cells {
                group.add_member(cell);
            }
            groups.push(group);
        }
    }

    groups
}

/// Generate a human-readable CCF analysis report
pub fn format_ccf_report(results: &CcfAnalysisResults) -> String {
    let mut output = String::new();

    output.push_str(&format!("CCF Analysis Report: {}\n", results.design_name));
    output.push_str(&format!("{}\n", "=".repeat(50)));
    output.push_str(&format!(
        "Total groups identified: {}\n",
        results.groups.len()
    ));
    output.push_str(&format!(
        "Total independent FIT: {:.2e}\n",
        results.total_independent_fit
    ));
    output.push_str(&format!(
        "Total common cause FIT: {:.2e}\n",
        results.total_common_cause_fit
    ));
    output.push_str(&format!(
        "CCF ratio: {:.1}%\n\n",
        results.ccf_ratio() * 100.0
    ));

    output.push_str("CCF Groups:\n");
    output.push_str(&format!("{}\n", "-".repeat(50)));

    for group in &results.groups {
        output.push_str(&format!(
            "\n{} ({:?}, beta={:.2})\n",
            group.name, group.cause, group.beta_factor
        ));
        if let Some(desc) = &group.description {
            output.push_str(&format!("  Description: {}\n", desc));
        }
        output.push_str(&format!("  Members: {}\n", group.member_count()));
        if group.member_count() <= 10 {
            for member in &group.members {
                output.push_str(&format!("    - {}\n", member));
            }
        } else {
            for member in group.members.iter().take(5) {
                output.push_str(&format!("    - {}\n", member));
            }
            output.push_str(&format!("    ... and {} more\n", group.member_count() - 5));
        }
    }

    if !results.unassigned_cells.is_empty() {
        output.push_str(&format!(
            "\nUnassigned cells: {} (treated as independent)\n",
            results.unassigned_cells.len()
        ));
    }

    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_apply_beta_factor() {
        let fit = 100.0;
        let beta = 0.05;
        let split = apply_beta_factor(fit, beta);

        assert_eq!(split.total_fit, 100.0);
        assert!((split.independent_fit - 95.0).abs() < 0.001);
        assert!((split.common_cause_fit - 5.0).abs() < 0.001);
    }

    #[test]
    fn test_beta_factor_clamping() {
        let split = apply_beta_factor(100.0, 1.5);
        assert_eq!(split.beta_factor, 1.0);
        assert_eq!(split.independent_fit, 0.0);
        assert_eq!(split.common_cause_fit, 100.0);

        let split = apply_beta_factor(100.0, -0.5);
        assert_eq!(split.beta_factor, 0.0);
        assert_eq!(split.independent_fit, 100.0);
        assert_eq!(split.common_cause_fit, 0.0);
    }

    #[test]
    fn test_ccf_cause_typical_beta() {
        assert!((CcfCause::SharedClock.typical_beta() - 0.07).abs() < 0.001);
        assert!((CcfCause::SharedReset.typical_beta() - 0.05).abs() < 0.001);
        assert!((CcfCause::SharedDesign.typical_beta() - 0.02).abs() < 0.001);
    }

    #[test]
    fn test_identify_ccf_groups() {
        let mut clock_domains: HashMap<String, Vec<String>> = HashMap::new();
        clock_domains.insert(
            "clk_main".to_string(),
            vec![
                "cell_a".to_string(),
                "cell_b".to_string(),
                "cell_c".to_string(),
            ],
        );
        clock_domains.insert("clk_fast".to_string(), vec!["cell_d".to_string()]);

        let mut reset_signals: HashMap<String, Vec<String>> = HashMap::new();
        reset_signals.insert(
            "rst_n".to_string(),
            vec!["cell_a".to_string(), "cell_b".to_string()],
        );

        let mut cell_types: HashMap<String, String> = HashMap::new();
        cell_types.insert("cell_a".to_string(), "DFF".to_string());
        cell_types.insert("cell_b".to_string(), "DFF".to_string());
        cell_types.insert("cell_c".to_string(), "DFF".to_string());
        cell_types.insert("cell_d".to_string(), "NAND2".to_string());

        let groups = identify_ccf_groups(&clock_domains, &reset_signals, &cell_types);

        // Should have clock, reset, and cell type groups
        assert!(groups.len() >= 2);

        // Find the clock domain group
        let clock_group = groups.iter().find(|g| g.name == "clk_clk_main");
        assert!(clock_group.is_some());
        assert_eq!(clock_group.unwrap().member_count(), 3);
        assert_eq!(clock_group.unwrap().cause, CcfCause::SharedClock);
    }

    #[test]
    fn test_ccf_analysis_results() {
        let mut results = CcfAnalysisResults::new("test_design");

        let mut group = CcfGroup::new("clk_main", CcfCause::SharedClock, 0.07);
        group.add_member("cell_a".to_string());
        group.add_member("cell_b".to_string());
        results.add_group(group);

        let mut cell_fits: HashMap<String, f64> = HashMap::new();
        cell_fits.insert("cell_a".to_string(), 100.0);
        cell_fits.insert("cell_b".to_string(), 100.0);
        cell_fits.insert("cell_c".to_string(), 50.0); // Unassigned

        results.calculate_totals(&cell_fits);

        // cell_a and cell_b: 200 total, 14 CCF (7%), 186 independent
        // cell_c: 50 independent (unassigned)
        assert!((results.total_common_cause_fit - 14.0).abs() < 0.1);
        assert!((results.total_independent_fit - 236.0).abs() < 0.1);
        assert_eq!(results.unassigned_cells.len(), 1);
    }

    #[test]
    fn test_format_report() {
        let mut results = CcfAnalysisResults::new("test_design");
        results.total_independent_fit = 950.0;
        results.total_common_cause_fit = 50.0;

        let mut group = CcfGroup::new("clk_main", CcfCause::SharedClock, 0.05)
            .with_description("Main clock domain");
        group.add_member("cell_a".to_string());
        group.add_member("cell_b".to_string());
        results.add_group(group);

        let report = format_ccf_report(&results);
        assert!(report.contains("CCF Analysis Report"));
        assert!(report.contains("clk_main"));
        assert!(report.contains("SharedClock"));
    }
}
