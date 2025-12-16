//! Safety Mechanism Failure Analysis
//!
//! Analyzes the impact of safety mechanism hardware failures on system safety.
//! Per ISO 26262-5, SM failures must be included in PMHF calculation.
//!
//! # Background
//!
//! Safety mechanisms (SM) are hardware that detects or prevents failures in functional logic.
//! However, the SM hardware itself can also fail. When an SM fails:
//! 1. The protection it provides is LOST
//! 2. Functional faults that would have been detected are now undetected
//! 3. This is a dependent failure path per ISO 26262-9
//!
//! # PMHF Formula
//!
//! ```text
//! PMHF = λSPF + λRF + λSM_contribution
//!
//! Where:
//!   λSPF = Single Point Fault rate (unprotected functional logic)
//!   λRF = Residual Fault rate = λprotected × (1 - DC)
//!   λSM_contribution = SM failures that go undetected
//! ```
//!
//! # SM-of-SM Hierarchy
//!
//! Safety mechanisms can protect other safety mechanisms:
//! ```text
//! Watchdog (SM-of-SM)
//!     │
//!     └─── monitors ───► TMR Voter (SM)
//!                           │
//!                           └─── protects ───► Sensor Logic (Functional)
//! ```
//!
//! For protected SMs: λSM_contribution = λSM × (1 - DC_protector)

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::design_resolver::SafetyAnnotation;
use skalp_lir::{CellSafetyClassification, GateNetlist};

// ============================================================================
// SM Cell Mapping
// ============================================================================

/// Mapping of cells to safety mechanisms
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SmCellMapping {
    /// Cell path -> mechanism name
    pub cell_to_sm: HashMap<String, String>,
    /// Mechanism name -> list of cell paths
    pub sm_to_cells: HashMap<String, Vec<String>>,
    /// Mechanism name -> total FIT
    pub sm_fit: HashMap<String, f64>,
    /// Total number of SM cells
    pub total_sm_cells: usize,
    /// Total SM FIT
    pub total_sm_fit: f64,
    /// Total number of functional cells
    pub total_functional_cells: usize,
    /// Total functional FIT
    pub total_functional_fit: f64,
}

impl SmCellMapping {
    /// Create an empty mapping
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a cell to a mechanism
    pub fn add_cell(&mut self, cell_path: String, mechanism_name: String, cell_fit: f64) {
        self.cell_to_sm
            .insert(cell_path.clone(), mechanism_name.clone());
        self.sm_to_cells
            .entry(mechanism_name.clone())
            .or_default()
            .push(cell_path);
        *self.sm_fit.entry(mechanism_name).or_insert(0.0) += cell_fit;
        self.total_sm_cells += 1;
        self.total_sm_fit += cell_fit;
    }

    /// Add a functional cell (not part of any SM)
    pub fn add_functional_cell(&mut self, cell_fit: f64) {
        self.total_functional_cells += 1;
        self.total_functional_fit += cell_fit;
    }

    /// Get the mechanism name for a cell
    pub fn get_mechanism(&self, cell_path: &str) -> Option<&String> {
        self.cell_to_sm.get(cell_path)
    }

    /// Get all cells for a mechanism
    pub fn get_cells(&self, mechanism_name: &str) -> Option<&Vec<String>> {
        self.sm_to_cells.get(mechanism_name)
    }

    /// Get FIT for a mechanism
    pub fn get_fit(&self, mechanism_name: &str) -> f64 {
        self.sm_fit.get(mechanism_name).copied().unwrap_or(0.0)
    }

    /// Get all mechanism names
    pub fn mechanism_names(&self) -> Vec<&String> {
        self.sm_to_cells.keys().collect()
    }
}

// ============================================================================
// SM Hierarchy
// ============================================================================

/// Hierarchical SM relationships (SM-of-SM)
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SmHierarchy {
    /// SM -> protected-by-SM relationships (which SM protects this SM)
    pub protectors: HashMap<String, String>,
    /// SM -> protects-which-SMs relationships (which SMs does this SM protect)
    pub protects: HashMap<String, Vec<String>>,
    /// SM -> DC provided by the protector
    pub protector_dc: HashMap<String, f64>,
}

impl SmHierarchy {
    /// Create an empty hierarchy
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a protection relationship
    pub fn add_protection(&mut self, sm_name: &str, protector_name: &str, dc: f64) {
        self.protectors
            .insert(sm_name.to_string(), protector_name.to_string());
        self.protects
            .entry(protector_name.to_string())
            .or_default()
            .push(sm_name.to_string());
        self.protector_dc.insert(sm_name.to_string(), dc);
    }

    /// Get the protector for an SM
    pub fn get_protector(&self, sm_name: &str) -> Option<&String> {
        self.protectors.get(sm_name)
    }

    /// Get the DC of the protector for an SM
    pub fn get_protector_dc(&self, sm_name: &str) -> Option<f64> {
        self.protector_dc.get(sm_name).copied()
    }

    /// Check if an SM is protected
    pub fn is_protected(&self, sm_name: &str) -> bool {
        self.protectors.contains_key(sm_name)
    }

    /// Get SMs that protect others (meta-SMs)
    pub fn get_meta_sms(&self) -> Vec<&String> {
        self.protects.keys().collect()
    }

    /// Get SMs that don't protect any other SMs (leaf SMs)
    pub fn get_leaf_sms(&self, all_sms: &[String]) -> Vec<String> {
        all_sms
            .iter()
            .filter(|sm| !self.protects.contains_key(*sm))
            .cloned()
            .collect()
    }

    /// Validate the hierarchy (detect cycles)
    pub fn validate(&self) -> Result<(), String> {
        for sm in self.protectors.keys() {
            let mut visited = vec![sm.clone()];
            let mut current = sm.clone();

            while let Some(protector) = self.protectors.get(&current) {
                if visited.contains(protector) {
                    return Err(format!(
                        "Cycle detected in SM hierarchy: {} -> {}",
                        visited.join(" -> "),
                        protector
                    ));
                }
                visited.push(protector.clone());
                current = protector.clone();
            }
        }
        Ok(())
    }
}

// ============================================================================
// SM Failure Analysis Result
// ============================================================================

/// Result of analyzing a single safety mechanism
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SmFailureResult {
    /// Mechanism name
    pub mechanism_name: String,
    /// Safety goal this mechanism implements
    pub goal_name: String,
    /// FIT rate of SM hardware (λSM)
    pub sm_fit: f64,
    /// Number of cells implementing this SM
    pub cell_count: usize,
    /// Cell paths implementing this SM
    pub implementation_cells: Vec<String>,
    /// FIT of functional logic protected by this SM (for context)
    pub protected_fit: f64,
    /// DC provided by this SM
    pub diagnostic_coverage: f64,
    /// SM protecting this SM (if any)
    pub protected_by: Option<String>,
    /// DC of the protecting SM
    pub protection_dc: Option<f64>,
    /// Effective contribution to PMHF
    /// - Unprotected: λSM
    /// - Protected: λSM × (1 - DC_protector)
    pub pmhf_contribution: f64,
}

impl SmFailureResult {
    /// Create a new SM failure result
    pub fn new(mechanism_name: &str, goal_name: &str) -> Self {
        Self {
            mechanism_name: mechanism_name.to_string(),
            goal_name: goal_name.to_string(),
            sm_fit: 0.0,
            cell_count: 0,
            implementation_cells: Vec::new(),
            protected_fit: 0.0,
            diagnostic_coverage: 0.0,
            protected_by: None,
            protection_dc: None,
            pmhf_contribution: 0.0,
        }
    }

    /// Calculate the PMHF contribution
    pub fn calculate_pmhf_contribution(&mut self) {
        if let Some(dc) = self.protection_dc {
            // Protected: only undetected failures contribute
            self.pmhf_contribution = self.sm_fit * (1.0 - dc / 100.0);
        } else {
            // Unprotected: all failures contribute
            self.pmhf_contribution = self.sm_fit;
        }
    }
}

// ============================================================================
// SM Failure Analysis
// ============================================================================

/// Complete SM failure analysis for a design
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SmFailureAnalysis {
    /// Design name
    pub design_name: String,
    /// Per-mechanism analysis results
    pub mechanisms: Vec<SmFailureResult>,
    /// Total SM FIT (sum of all SM hardware)
    pub total_sm_fit: f64,
    /// Effective SM FIT after accounting for SM-of-SM coverage
    pub effective_sm_fit: f64,
    /// SM hierarchy
    pub hierarchy: SmHierarchy,
    /// Cell mapping
    pub cell_mapping: SmCellMapping,
}

impl SmFailureAnalysis {
    /// Create a new empty analysis
    pub fn new(design_name: &str) -> Self {
        Self {
            design_name: design_name.to_string(),
            mechanisms: Vec::new(),
            total_sm_fit: 0.0,
            effective_sm_fit: 0.0,
            hierarchy: SmHierarchy::new(),
            cell_mapping: SmCellMapping::new(),
        }
    }

    /// Analyze SM failures from a gate netlist
    ///
    /// This function:
    /// 1. Identifies SM cells from the netlist (using cell.safety_classification)
    /// 2. Groups cells by mechanism
    /// 3. Calculates per-mechanism FIT
    /// 4. Builds SM hierarchy from annotations
    /// 5. Calculates effective PMHF contribution
    pub fn analyze(netlist: &GateNetlist, annotations: &[SafetyAnnotation]) -> Self {
        let mut analysis = Self::new(&netlist.name);

        // Step 1: Build cell mapping from netlist
        analysis.cell_mapping = identify_sm_cells(netlist);

        // Step 2: Build SM results from cell mapping
        let mut mechanism_results: HashMap<String, SmFailureResult> = HashMap::new();

        for cell in &netlist.cells {
            match &cell.safety_classification {
                CellSafetyClassification::SafetyMechanism {
                    goal_name,
                    mechanism_name,
                } => {
                    let result = mechanism_results
                        .entry(mechanism_name.clone())
                        .or_insert_with(|| SmFailureResult::new(mechanism_name, goal_name));
                    result.sm_fit += cell.fit;
                    result.cell_count += 1;
                    result.implementation_cells.push(cell.path.clone());
                }
                CellSafetyClassification::SafetyMechanismOfSm {
                    protected_sm_name,
                    goal_name,
                    mechanism_name,
                } => {
                    let result = mechanism_results
                        .entry(mechanism_name.clone())
                        .or_insert_with(|| SmFailureResult::new(mechanism_name, goal_name));
                    result.sm_fit += cell.fit;
                    result.cell_count += 1;
                    result.implementation_cells.push(cell.path.clone());

                    // Record the protection relationship (will be processed later)
                    // Note: DC will be looked up from annotations
                    analysis
                        .hierarchy
                        .protects
                        .entry(mechanism_name.clone())
                        .or_default()
                        .push(protected_sm_name.clone());
                }
                CellSafetyClassification::Functional => {
                    // Not an SM cell, skip
                }
            }
        }

        // Step 3: Apply hierarchy (SM-of-SM relationships) from annotations
        for annotation in annotations {
            // Look for "protects" relationships in annotations
            // For now, use the hierarchy built from SafetyMechanismOfSm classifications
        }

        // Step 4: Calculate PMHF contributions
        for result in mechanism_results.values_mut() {
            // Look up protection DC from hierarchy
            if let Some(dc) = analysis.hierarchy.get_protector_dc(&result.mechanism_name) {
                result.protected_by = analysis
                    .hierarchy
                    .get_protector(&result.mechanism_name)
                    .cloned();
                result.protection_dc = Some(dc);
            }
            result.calculate_pmhf_contribution();
        }

        // Step 5: Aggregate results
        analysis.mechanisms = mechanism_results.into_values().collect();
        analysis.total_sm_fit = analysis.mechanisms.iter().map(|m| m.sm_fit).sum();
        analysis.effective_sm_fit = analysis
            .mechanisms
            .iter()
            .map(|m| m.pmhf_contribution)
            .sum();

        analysis
    }

    /// Calculate the total λSM contribution to PMHF
    pub fn calculate_sm_pmhf_contribution(&self) -> f64 {
        self.effective_sm_fit
    }

    /// Get mechanisms that protect functional logic (not other SMs)
    pub fn get_primary_sms(&self) -> Vec<&SmFailureResult> {
        self.mechanisms
            .iter()
            .filter(|m| {
                // Check if this SM protects any other SMs
                !self.hierarchy.protects.contains_key(&m.mechanism_name)
            })
            .collect()
    }

    /// Get mechanisms that protect other SMs
    pub fn get_meta_sms(&self) -> Vec<&SmFailureResult> {
        self.mechanisms
            .iter()
            .filter(|m| self.hierarchy.protects.contains_key(&m.mechanism_name))
            .collect()
    }

    /// Get unprotected SMs (highest risk)
    pub fn get_unprotected_sms(&self) -> Vec<&SmFailureResult> {
        self.mechanisms
            .iter()
            .filter(|m| m.protected_by.is_none())
            .collect()
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Identify SM cells from a gate netlist
///
/// Uses the `safety_classification` field on cells to determine
/// which cells belong to safety mechanisms.
pub fn identify_sm_cells(netlist: &GateNetlist) -> SmCellMapping {
    let mut mapping = SmCellMapping::new();

    for cell in &netlist.cells {
        match &cell.safety_classification {
            CellSafetyClassification::SafetyMechanism { mechanism_name, .. } => {
                mapping.add_cell(cell.path.clone(), mechanism_name.clone(), cell.fit);
            }
            CellSafetyClassification::SafetyMechanismOfSm { mechanism_name, .. } => {
                mapping.add_cell(cell.path.clone(), mechanism_name.clone(), cell.fit);
            }
            CellSafetyClassification::Functional => {
                mapping.add_functional_cell(cell.fit);
            }
        }
    }

    mapping
}

/// Identify SM cells from annotations (path-based matching)
///
/// This function matches cells to SMs using hierarchical path matching
/// against `#[implements(...)]` annotations. Useful when cells don't
/// have explicit safety_classification set.
pub fn identify_sm_cells_from_annotations(
    netlist: &GateNetlist,
    annotations: &[SafetyAnnotation],
) -> SmCellMapping {
    let mut mapping = SmCellMapping::new();

    // Build annotation map: path -> (goal, mechanism)
    let annotation_map: HashMap<String, (&str, &str)> = annotations
        .iter()
        .map(|a| {
            (
                a.design_ref.instance.to_string(),
                (a.goal_name.as_str(), a.mechanism_name.as_str()),
            )
        })
        .collect();

    for cell in &netlist.cells {
        let mut matched = false;

        // Try to match cell path against annotations
        for (prefix, (_, mechanism)) in &annotation_map {
            if cell.path.starts_with(prefix) {
                mapping.add_cell(cell.path.clone(), mechanism.to_string(), cell.fit);
                matched = true;
                break;
            }
        }

        if !matched {
            mapping.add_functional_cell(cell.fit);
        }
    }

    mapping
}

/// Generate a human-readable SM failure analysis report
pub fn format_sm_failure_report(analysis: &SmFailureAnalysis) -> String {
    let mut output = String::new();

    output.push_str(&format!(
        "Safety Mechanism Failure Analysis: {}\n",
        analysis.design_name
    ));
    output.push_str("═══════════════════════════════════════════════════════════════\n\n");

    // Summary
    output.push_str("SUMMARY\n");
    output.push_str("───────────────────────────────────────────────────────────────\n");
    output.push_str(&format!(
        "Total SM cells: {}\n",
        analysis.cell_mapping.total_sm_cells
    ));
    output.push_str(&format!(
        "Total SM FIT (λSM_raw): {:.4} FIT\n",
        analysis.total_sm_fit
    ));
    output.push_str(&format!(
        "Effective SM FIT (λSM_eff): {:.4} FIT\n",
        analysis.effective_sm_fit
    ));
    output.push_str(&format!(
        "Functional cells: {}\n",
        analysis.cell_mapping.total_functional_cells
    ));
    output.push_str(&format!(
        "Functional FIT: {:.4} FIT\n\n",
        analysis.cell_mapping.total_functional_fit
    ));

    // Per-mechanism breakdown
    output.push_str("PER-MECHANISM BREAKDOWN\n");
    output.push_str("───────────────────────────────────────────────────────────────\n");

    for result in &analysis.mechanisms {
        let protected_str = if let Some(ref protector) = result.protected_by {
            format!(
                " [protected by: {}, DC: {:.1}%]",
                protector,
                result.protection_dc.unwrap_or(0.0)
            )
        } else {
            " [UNPROTECTED]".to_string()
        };

        output.push_str(&format!(
            "{}::{}{}\n",
            result.goal_name, result.mechanism_name, protected_str
        ));
        output.push_str(&format!("  Cells: {}\n", result.cell_count));
        output.push_str(&format!("  λSM: {:.4} FIT\n", result.sm_fit));
        output.push_str(&format!(
            "  PMHF contribution: {:.4} FIT\n\n",
            result.pmhf_contribution
        ));
    }

    // Warnings
    let unprotected = analysis.get_unprotected_sms();
    if !unprotected.is_empty() {
        output.push_str("WARNINGS\n");
        output.push_str("───────────────────────────────────────────────────────────────\n");
        for sm in unprotected {
            if sm.sm_fit > 1.0 {
                output.push_str(&format!(
                    "⚠ {} has high unprotected FIT: {:.4} FIT\n",
                    sm.mechanism_name, sm.sm_fit
                ));
            }
        }
    }

    output
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_lir::{Cell, CellId, GateNetId};

    fn make_test_netlist_with_sm() -> GateNetlist {
        let mut netlist = GateNetlist::new("test_design".to_string(), "generic".to_string());

        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());
        let out = netlist.add_output("out".to_string());

        // Functional cell (FIT = 0.1)
        let func_cell = Cell::new_comb(
            CellId(0),
            "NAND2_X1".to_string(),
            "generic".to_string(),
            0.1,
            "top.sensor.nand".to_string(),
            vec![a, b],
            vec![GateNetId(10)],
        );
        netlist.add_cell(func_cell);

        // SM cell: TMR voter (FIT = 0.2)
        let sm_cell = Cell::new_comb(
            CellId(0),
            "NAND3_X1".to_string(),
            "generic".to_string(),
            0.2,
            "top.tmr.voter".to_string(),
            vec![GateNetId(10)],
            vec![out],
        )
        .with_safety_classification(CellSafetyClassification::SafetyMechanism {
            goal_name: "BrakingSafety".to_string(),
            mechanism_name: "TmrVoting".to_string(),
        });
        netlist.add_cell(sm_cell);

        netlist
    }

    #[test]
    fn test_identify_sm_cells() {
        let netlist = make_test_netlist_with_sm();
        let mapping = identify_sm_cells(&netlist);

        assert_eq!(mapping.total_sm_cells, 1);
        assert_eq!(mapping.total_functional_cells, 1);
        assert!((mapping.total_sm_fit - 0.2).abs() < 0.001);
        assert!((mapping.total_functional_fit - 0.1).abs() < 0.001);
        assert_eq!(
            mapping.get_mechanism("top.tmr.voter"),
            Some(&"TmrVoting".to_string())
        );
    }

    #[test]
    fn test_sm_failure_analysis() {
        let netlist = make_test_netlist_with_sm();
        let analysis = SmFailureAnalysis::analyze(&netlist, &[]);

        assert_eq!(analysis.mechanisms.len(), 1);
        assert!((analysis.total_sm_fit - 0.2).abs() < 0.001);

        let tmr = &analysis.mechanisms[0];
        assert_eq!(tmr.mechanism_name, "TmrVoting");
        assert_eq!(tmr.cell_count, 1);
        assert!(tmr.protected_by.is_none()); // Unprotected
        assert!((tmr.pmhf_contribution - 0.2).abs() < 0.001); // Full contribution
    }

    #[test]
    fn test_sm_hierarchy_validation() {
        let mut hierarchy = SmHierarchy::new();
        hierarchy.add_protection("TmrVoting", "Watchdog", 90.0);

        assert!(hierarchy.validate().is_ok());

        // Add a cycle
        hierarchy.add_protection("Watchdog", "TmrVoting", 90.0);
        assert!(hierarchy.validate().is_err());
    }

    #[test]
    fn test_protected_sm_pmhf_contribution() {
        let mut result = SmFailureResult::new("TmrVoting", "BrakingSafety");
        result.sm_fit = 1.0;
        result.protection_dc = Some(90.0); // 90% DC from protector
        result.calculate_pmhf_contribution();

        // Should be 1.0 × (1 - 0.9) = 0.1 FIT
        assert!((result.pmhf_contribution - 0.1).abs() < 0.001);
    }

    #[test]
    fn test_unprotected_sm_pmhf_contribution() {
        let mut result = SmFailureResult::new("TmrVoting", "BrakingSafety");
        result.sm_fit = 1.0;
        result.protection_dc = None; // Unprotected
        result.calculate_pmhf_contribution();

        // Should be full 1.0 FIT
        assert!((result.pmhf_contribution - 1.0).abs() < 0.001);
    }

    #[test]
    fn test_format_report() {
        let netlist = make_test_netlist_with_sm();
        let analysis = SmFailureAnalysis::analyze(&netlist, &[]);
        let report = format_sm_failure_report(&analysis);

        assert!(report.contains("Safety Mechanism Failure Analysis"));
        assert!(report.contains("TmrVoting"));
        assert!(report.contains("UNPROTECTED"));
    }
}
