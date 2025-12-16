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

use std::collections::{HashMap, HashSet, VecDeque};

use serde::{Deserialize, Serialize};

use crate::design_resolver::SafetyAnnotation;
use skalp_lir::{CellSafetyClassification, GateNetId, GateNetlist};

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
// SM Coverage Mapping
// ============================================================================

/// Maps safety mechanisms to the functional cells they protect
///
/// This traces the fanin cone of each SM's detection points to identify
/// which functional cells are covered by the mechanism's protection.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SmCoverageMap {
    /// SM name -> list of protected functional cell paths
    pub sm_to_protected: HashMap<String, Vec<String>>,
    /// Functional cell path -> list of SM names that protect it
    pub functional_cell_to_sms: HashMap<String, Vec<String>>,
    /// SM name -> FIT of protected functional cells
    pub sm_protected_fit: HashMap<String, f64>,
    /// Functional cells protected by at least one SM
    pub protected_cells: Vec<String>,
    /// Total FIT of protected functional cells
    pub protected_fit: f64,
    /// Functional cells not protected by any SM
    pub unprotected_cells: Vec<String>,
    /// Total FIT of unprotected cells
    pub unprotected_fit: f64,
    /// Coverage percentage (protected / total functional)
    pub coverage_percentage: f64,
    /// Coverage percentage by FIT (protected FIT / total functional FIT)
    pub fit_coverage_percentage: f64,
    /// Total functional cells analyzed
    pub total_functional_cells: usize,
    /// Total functional FIT analyzed
    pub total_functional_fit: f64,
}

impl SmCoverageMap {
    /// Create a new empty coverage map
    pub fn new() -> Self {
        Self::default()
    }

    /// Get the list of SMs protecting a functional cell
    pub fn get_protectors(&self, cell_path: &str) -> Option<&Vec<String>> {
        self.functional_cell_to_sms.get(cell_path)
    }

    /// Check if a functional cell is protected by any SM
    pub fn is_protected(&self, cell_path: &str) -> bool {
        self.functional_cell_to_sms.contains_key(cell_path)
    }

    /// Get the protection level (number of SMs protecting a cell)
    pub fn protection_level(&self, cell_path: &str) -> usize {
        self.functional_cell_to_sms
            .get(cell_path)
            .map(|v| v.len())
            .unwrap_or(0)
    }

    /// Get cells with redundant protection (protected by multiple SMs)
    pub fn redundantly_protected_cells(&self) -> Vec<(&String, &Vec<String>)> {
        self.functional_cell_to_sms
            .iter()
            .filter(|(_, sms)| sms.len() > 1)
            .collect()
    }

    /// Summary of coverage by SM
    pub fn coverage_by_sm(&self) -> Vec<SmCoverageEntry> {
        self.sm_to_protected
            .iter()
            .map(|(sm_name, protected)| SmCoverageEntry {
                sm_name: sm_name.clone(),
                protected_cell_count: protected.len(),
                protected_fit: self.sm_protected_fit.get(sm_name).copied().unwrap_or(0.0),
                coverage_percentage: if self.total_functional_cells > 0 {
                    (protected.len() as f64 / self.total_functional_cells as f64) * 100.0
                } else {
                    0.0
                },
                fit_coverage_percentage: if self.total_functional_fit > 0.0 {
                    (self.sm_protected_fit.get(sm_name).copied().unwrap_or(0.0)
                        / self.total_functional_fit)
                        * 100.0
                } else {
                    0.0
                },
            })
            .collect()
    }
}

/// Summary of coverage for a single SM
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SmCoverageEntry {
    /// Mechanism name
    pub sm_name: String,
    /// Number of functional cells protected
    pub protected_cell_count: usize,
    /// Total FIT of protected cells
    pub protected_fit: f64,
    /// Percentage of functional cells protected
    pub coverage_percentage: f64,
    /// Percentage of functional FIT protected
    pub fit_coverage_percentage: f64,
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

/// Map SM coverage to protected functional cells
///
/// This function traces the fanin cone of each safety mechanism to identify
/// which functional cells are protected. A functional cell is considered
/// protected if its outputs feed (directly or transitively) into an SM's inputs.
///
/// # Algorithm
/// 1. Build connectivity maps (net -> driving cell, net -> driven cells)
/// 2. For each SM cell, trace backwards through its inputs (BFS fanin traversal)
/// 3. Collect all functional cells encountered in the fanin cone
/// 4. Aggregate results and calculate coverage metrics
pub fn map_sm_coverage(netlist: &GateNetlist, sm_mapping: &SmCellMapping) -> SmCoverageMap {
    let mut coverage = SmCoverageMap::new();

    // Build connectivity maps
    // net_id -> cell index that drives this net
    let mut net_to_driver: HashMap<GateNetId, usize> = HashMap::new();
    // net_id -> cell indices that are driven by this net
    let mut net_to_driven: HashMap<GateNetId, Vec<usize>> = HashMap::new();

    for (cell_idx, cell) in netlist.cells.iter().enumerate() {
        // This cell drives its output nets
        for output in &cell.outputs {
            net_to_driver.insert(*output, cell_idx);
        }
        // This cell is driven by its input nets
        for input in &cell.inputs {
            net_to_driven.entry(*input).or_default().push(cell_idx);
        }
    }

    // Build cell path -> cell index map for quick lookup
    let cell_path_to_idx: HashMap<&str, usize> = netlist
        .cells
        .iter()
        .enumerate()
        .map(|(idx, cell)| (cell.path.as_str(), idx))
        .collect();

    // Collect all functional cell indices and their info
    let mut functional_cells: HashMap<usize, (String, f64)> = HashMap::new();
    for (idx, cell) in netlist.cells.iter().enumerate() {
        if matches!(
            cell.safety_classification,
            CellSafetyClassification::Functional
        ) {
            functional_cells.insert(idx, (cell.path.clone(), cell.fit));
        }
    }

    coverage.total_functional_cells = functional_cells.len();
    coverage.total_functional_fit = functional_cells.values().map(|(_, fit)| fit).sum();

    // Track which functional cells are protected by which SMs
    let mut cell_to_sms: HashMap<usize, HashSet<String>> = HashMap::new();

    // For each SM, trace its fanin cone
    for (sm_name, sm_cell_paths) in &sm_mapping.sm_to_cells {
        let mut protected_in_this_sm: HashSet<usize> = HashSet::new();

        // Get all SM cell indices for this mechanism
        let sm_cell_indices: Vec<usize> = sm_cell_paths
            .iter()
            .filter_map(|path| cell_path_to_idx.get(path.as_str()).copied())
            .collect();

        // BFS from each SM cell backwards through fanin
        for start_idx in &sm_cell_indices {
            let mut visited: HashSet<usize> = HashSet::new();
            let mut queue: VecDeque<usize> = VecDeque::new();
            queue.push_back(*start_idx);
            visited.insert(*start_idx);

            while let Some(cell_idx) = queue.pop_front() {
                let cell = &netlist.cells[cell_idx];

                // Traverse backwards through inputs
                for input_net in &cell.inputs {
                    // Find the cell that drives this net
                    if let Some(&driver_idx) = net_to_driver.get(input_net) {
                        if !visited.contains(&driver_idx) {
                            visited.insert(driver_idx);

                            // If this is a functional cell, it's protected by this SM
                            if functional_cells.contains_key(&driver_idx) {
                                protected_in_this_sm.insert(driver_idx);
                                cell_to_sms
                                    .entry(driver_idx)
                                    .or_default()
                                    .insert(sm_name.clone());
                            }

                            // Continue traversing (unless it's an SM cell from a different mechanism)
                            let driver_cell = &netlist.cells[driver_idx];
                            let is_different_sm = match &driver_cell.safety_classification {
                                CellSafetyClassification::SafetyMechanism {
                                    mechanism_name,
                                    ..
                                } => mechanism_name != sm_name,
                                CellSafetyClassification::SafetyMechanismOfSm {
                                    mechanism_name,
                                    ..
                                } => mechanism_name != sm_name,
                                CellSafetyClassification::Functional => false,
                            };

                            if !is_different_sm {
                                queue.push_back(driver_idx);
                            }
                        }
                    }
                }
            }
        }

        // Record protected cells for this SM
        let protected_paths: Vec<String> = protected_in_this_sm
            .iter()
            .filter_map(|idx| functional_cells.get(idx).map(|(path, _)| path.clone()))
            .collect();

        let protected_fit: f64 = protected_in_this_sm
            .iter()
            .filter_map(|idx| functional_cells.get(idx).map(|(_, fit)| *fit))
            .sum();

        coverage
            .sm_to_protected
            .insert(sm_name.clone(), protected_paths);
        coverage
            .sm_protected_fit
            .insert(sm_name.clone(), protected_fit);
    }

    // Build reverse mapping and aggregate results
    for (cell_idx, sms) in &cell_to_sms {
        if let Some((path, _)) = functional_cells.get(cell_idx) {
            let sm_list: Vec<String> = sms.iter().cloned().collect();
            coverage
                .functional_cell_to_sms
                .insert(path.clone(), sm_list);
        }
    }

    // Calculate protected vs unprotected
    for (cell_idx, (path, fit)) in &functional_cells {
        if cell_to_sms.contains_key(cell_idx) {
            coverage.protected_cells.push(path.clone());
            coverage.protected_fit += fit;
        } else {
            coverage.unprotected_cells.push(path.clone());
            coverage.unprotected_fit += fit;
        }
    }

    // Calculate coverage percentages
    if coverage.total_functional_cells > 0 {
        coverage.coverage_percentage = (coverage.protected_cells.len() as f64
            / coverage.total_functional_cells as f64)
            * 100.0;
    }
    if coverage.total_functional_fit > 0.0 {
        coverage.fit_coverage_percentage =
            (coverage.protected_fit / coverage.total_functional_fit) * 100.0;
    }

    coverage
}

/// Trace the fanin cone of a specific cell
///
/// Returns all cell indices in the fanin cone (including the start cell).
pub fn trace_fanin_cone(
    netlist: &GateNetlist,
    start_cell_path: &str,
    max_depth: Option<usize>,
) -> Vec<String> {
    // Build connectivity map
    let mut net_to_driver: HashMap<GateNetId, usize> = HashMap::new();
    for (cell_idx, cell) in netlist.cells.iter().enumerate() {
        for output in &cell.outputs {
            net_to_driver.insert(*output, cell_idx);
        }
    }

    // Find start cell
    let start_idx = netlist.cells.iter().position(|c| c.path == start_cell_path);

    let Some(start_idx) = start_idx else {
        return Vec::new();
    };

    // BFS with optional depth limit
    let mut visited: HashSet<usize> = HashSet::new();
    let mut queue: VecDeque<(usize, usize)> = VecDeque::new(); // (cell_idx, depth)
    queue.push_back((start_idx, 0));
    visited.insert(start_idx);

    while let Some((cell_idx, depth)) = queue.pop_front() {
        if let Some(max) = max_depth {
            if depth >= max {
                continue;
            }
        }

        let cell = &netlist.cells[cell_idx];
        for input_net in &cell.inputs {
            if let Some(&driver_idx) = net_to_driver.get(input_net) {
                if !visited.contains(&driver_idx) {
                    visited.insert(driver_idx);
                    queue.push_back((driver_idx, depth + 1));
                }
            }
        }
    }

    visited
        .into_iter()
        .map(|idx| netlist.cells[idx].path.clone())
        .collect()
}

/// Trace the fanout cone of a specific cell
///
/// Returns all cell indices in the fanout cone (cells that this cell feeds into).
pub fn trace_fanout_cone(
    netlist: &GateNetlist,
    start_cell_path: &str,
    max_depth: Option<usize>,
) -> Vec<String> {
    // Build connectivity map
    let mut net_to_driven: HashMap<GateNetId, Vec<usize>> = HashMap::new();
    for (cell_idx, cell) in netlist.cells.iter().enumerate() {
        for input in &cell.inputs {
            net_to_driven.entry(*input).or_default().push(cell_idx);
        }
    }

    // Find start cell
    let start_idx = netlist.cells.iter().position(|c| c.path == start_cell_path);

    let Some(start_idx) = start_idx else {
        return Vec::new();
    };

    // BFS with optional depth limit
    let mut visited: HashSet<usize> = HashSet::new();
    let mut queue: VecDeque<(usize, usize)> = VecDeque::new();
    queue.push_back((start_idx, 0));
    visited.insert(start_idx);

    while let Some((cell_idx, depth)) = queue.pop_front() {
        if let Some(max) = max_depth {
            if depth >= max {
                continue;
            }
        }

        let cell = &netlist.cells[cell_idx];
        for output_net in &cell.outputs {
            if let Some(driven_cells) = net_to_driven.get(output_net) {
                for &driven_idx in driven_cells {
                    if !visited.contains(&driven_idx) {
                        visited.insert(driven_idx);
                        queue.push_back((driven_idx, depth + 1));
                    }
                }
            }
        }
    }

    visited
        .into_iter()
        .map(|idx| netlist.cells[idx].path.clone())
        .collect()
}

/// Generate a human-readable SM coverage report
pub fn format_sm_coverage_report(coverage: &SmCoverageMap) -> String {
    let mut output = String::new();

    output.push_str("Safety Mechanism Coverage Analysis\n");
    output.push_str("═══════════════════════════════════════════════════════════════\n\n");

    // Summary
    output.push_str("SUMMARY\n");
    output.push_str("───────────────────────────────────────────────────────────────\n");
    output.push_str(&format!(
        "Total functional cells: {}\n",
        coverage.total_functional_cells
    ));
    output.push_str(&format!(
        "Total functional FIT: {:.4} FIT\n",
        coverage.total_functional_fit
    ));
    output.push_str(&format!(
        "Protected cells: {} ({:.1}%)\n",
        coverage.protected_cells.len(),
        coverage.coverage_percentage
    ));
    output.push_str(&format!(
        "Protected FIT: {:.4} FIT ({:.1}%)\n",
        coverage.protected_fit, coverage.fit_coverage_percentage
    ));
    output.push_str(&format!(
        "Unprotected cells: {}\n",
        coverage.unprotected_cells.len()
    ));
    output.push_str(&format!(
        "Unprotected FIT: {:.4} FIT\n\n",
        coverage.unprotected_fit
    ));

    // Per-SM breakdown
    output.push_str("PER-SM COVERAGE\n");
    output.push_str("───────────────────────────────────────────────────────────────\n");

    let sm_entries = coverage.coverage_by_sm();
    for entry in &sm_entries {
        output.push_str(&format!("{}:\n", entry.sm_name));
        output.push_str(&format!(
            "  Protected cells: {} ({:.1}%)\n",
            entry.protected_cell_count, entry.coverage_percentage
        ));
        output.push_str(&format!(
            "  Protected FIT: {:.4} FIT ({:.1}%)\n\n",
            entry.protected_fit, entry.fit_coverage_percentage
        ));
    }

    // Unprotected cells warning
    if !coverage.unprotected_cells.is_empty() {
        output.push_str("UNPROTECTED CELLS\n");
        output.push_str("───────────────────────────────────────────────────────────────\n");
        output.push_str(&format!(
            "WARNING: {} functional cells have no SM protection!\n",
            coverage.unprotected_cells.len()
        ));
        output.push_str(&format!(
            "Unprotected FIT contributes directly to SPF: {:.4} FIT\n\n",
            coverage.unprotected_fit
        ));

        // List first few unprotected cells
        let show_limit = 10;
        for (i, cell) in coverage
            .unprotected_cells
            .iter()
            .take(show_limit)
            .enumerate()
        {
            output.push_str(&format!("  {}. {}\n", i + 1, cell));
        }
        if coverage.unprotected_cells.len() > show_limit {
            output.push_str(&format!(
                "  ... and {} more\n",
                coverage.unprotected_cells.len() - show_limit
            ));
        }
    }

    // Redundantly protected cells (good for reliability)
    let redundant = coverage.redundantly_protected_cells();
    if !redundant.is_empty() {
        output.push_str("\nREDUNDANTLY PROTECTED CELLS\n");
        output.push_str("───────────────────────────────────────────────────────────────\n");
        output.push_str(&format!(
            "{} cells protected by multiple SMs (good for reliability):\n",
            redundant.len()
        ));
        let show_limit = 5;
        for (cell, sms) in redundant.iter().take(show_limit) {
            output.push_str(&format!("  {} -> {:?}\n", cell, sms));
        }
        if redundant.len() > show_limit {
            output.push_str(&format!(
                "  ... and {} more\n",
                redundant.len() - show_limit
            ));
        }
    }

    output
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

    // ======================================================================
    // SM Coverage Mapping Tests
    // ======================================================================

    fn make_test_netlist_for_coverage() -> GateNetlist {
        // Build a netlist with:
        //   func1 -> func2 -> SM_voter -> output
        //   func3 (unprotected)
        let mut netlist = GateNetlist::new("coverage_test".to_string(), "generic".to_string());

        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());
        let c = netlist.add_input("c".to_string());
        let out = netlist.add_output("out".to_string());

        // Net IDs for internal connections
        let net_func1_out = GateNetId(10);
        let net_func2_out = GateNetId(11);
        let net_func3_out = GateNetId(12);

        // func1: a,b -> net10 (protected by SM)
        let func1 = Cell::new_comb(
            CellId(0),
            "AND2_X1".to_string(),
            "generic".to_string(),
            0.1,
            "top.func1".to_string(),
            vec![a, b],
            vec![net_func1_out],
        );
        netlist.add_cell(func1);

        // func2: net10 -> net11 (protected by SM, downstream of func1)
        let func2 = Cell::new_comb(
            CellId(1),
            "INV_X1".to_string(),
            "generic".to_string(),
            0.05,
            "top.func2".to_string(),
            vec![net_func1_out],
            vec![net_func2_out],
        );
        netlist.add_cell(func2);

        // SM voter: net11 -> out
        let sm_voter = Cell::new_comb(
            CellId(2),
            "MAJ3_X1".to_string(),
            "generic".to_string(),
            0.2,
            "top.sm.voter".to_string(),
            vec![net_func2_out],
            vec![out],
        )
        .with_safety_classification(CellSafetyClassification::SafetyMechanism {
            goal_name: "Safety".to_string(),
            mechanism_name: "Voter".to_string(),
        });
        netlist.add_cell(sm_voter);

        // func3: c -> net12 (NOT protected - doesn't feed into SM)
        let func3 = Cell::new_comb(
            CellId(3),
            "BUF_X1".to_string(),
            "generic".to_string(),
            0.08,
            "top.func3_unprotected".to_string(),
            vec![c],
            vec![net_func3_out],
        );
        netlist.add_cell(func3);

        netlist
    }

    #[test]
    fn test_sm_coverage_basic() {
        let netlist = make_test_netlist_for_coverage();
        let sm_mapping = identify_sm_cells(&netlist);
        let coverage = map_sm_coverage(&netlist, &sm_mapping);

        // Should have 3 functional cells total
        assert_eq!(coverage.total_functional_cells, 3);

        // func1 and func2 should be protected, func3 should not
        assert_eq!(coverage.protected_cells.len(), 2);
        assert_eq!(coverage.unprotected_cells.len(), 1);

        // Verify the unprotected cell is func3
        assert!(coverage
            .unprotected_cells
            .contains(&"top.func3_unprotected".to_string()));
    }

    #[test]
    fn test_sm_coverage_fit_calculation() {
        let netlist = make_test_netlist_for_coverage();
        let sm_mapping = identify_sm_cells(&netlist);
        let coverage = map_sm_coverage(&netlist, &sm_mapping);

        // Total functional FIT: 0.1 + 0.05 + 0.08 = 0.23
        assert!((coverage.total_functional_fit - 0.23).abs() < 0.001);

        // Protected FIT: 0.1 + 0.05 = 0.15
        assert!((coverage.protected_fit - 0.15).abs() < 0.001);

        // Unprotected FIT: 0.08
        assert!((coverage.unprotected_fit - 0.08).abs() < 0.001);
    }

    #[test]
    fn test_sm_coverage_percentage() {
        let netlist = make_test_netlist_for_coverage();
        let sm_mapping = identify_sm_cells(&netlist);
        let coverage = map_sm_coverage(&netlist, &sm_mapping);

        // Cell coverage: 2/3 = 66.67%
        assert!((coverage.coverage_percentage - 66.67).abs() < 1.0);

        // FIT coverage: 0.15/0.23 = 65.22%
        assert!((coverage.fit_coverage_percentage - 65.22).abs() < 1.0);
    }

    #[test]
    fn test_sm_coverage_per_sm_breakdown() {
        let netlist = make_test_netlist_for_coverage();
        let sm_mapping = identify_sm_cells(&netlist);
        let coverage = map_sm_coverage(&netlist, &sm_mapping);

        // Should have one SM entry
        let sm_entries = coverage.coverage_by_sm();
        assert_eq!(sm_entries.len(), 1);

        let voter_entry = &sm_entries[0];
        assert_eq!(voter_entry.sm_name, "Voter");
        assert_eq!(voter_entry.protected_cell_count, 2);
        assert!((voter_entry.protected_fit - 0.15).abs() < 0.001);
    }

    #[test]
    fn test_sm_coverage_is_protected() {
        let netlist = make_test_netlist_for_coverage();
        let sm_mapping = identify_sm_cells(&netlist);
        let coverage = map_sm_coverage(&netlist, &sm_mapping);

        assert!(coverage.is_protected("top.func1"));
        assert!(coverage.is_protected("top.func2"));
        assert!(!coverage.is_protected("top.func3_unprotected"));
    }

    fn make_test_netlist_redundant_protection() -> GateNetlist {
        // Build a netlist with redundant protection:
        //   func1 -> SM_A -> ...
        //   func1 -> SM_B -> ...
        let mut netlist = GateNetlist::new("redundant_test".to_string(), "generic".to_string());

        let a = netlist.add_input("a".to_string());
        let out_a = netlist.add_output("out_a".to_string());
        let out_b = netlist.add_output("out_b".to_string());

        let net_func1_out = GateNetId(10);

        // func1: a -> net10 (protected by both SM_A and SM_B)
        let func1 = Cell::new_comb(
            CellId(0),
            "BUF_X1".to_string(),
            "generic".to_string(),
            0.1,
            "top.func1".to_string(),
            vec![a],
            vec![net_func1_out],
        );
        netlist.add_cell(func1);

        // SM_A: net10 -> out_a
        let sm_a = Cell::new_comb(
            CellId(1),
            "CHECK_X1".to_string(),
            "generic".to_string(),
            0.15,
            "top.sm_a.check".to_string(),
            vec![net_func1_out],
            vec![out_a],
        )
        .with_safety_classification(CellSafetyClassification::SafetyMechanism {
            goal_name: "Safety".to_string(),
            mechanism_name: "SM_A".to_string(),
        });
        netlist.add_cell(sm_a);

        // SM_B: net10 -> out_b
        let sm_b = Cell::new_comb(
            CellId(2),
            "CHECK_X1".to_string(),
            "generic".to_string(),
            0.15,
            "top.sm_b.check".to_string(),
            vec![net_func1_out],
            vec![out_b],
        )
        .with_safety_classification(CellSafetyClassification::SafetyMechanism {
            goal_name: "Safety".to_string(),
            mechanism_name: "SM_B".to_string(),
        });
        netlist.add_cell(sm_b);

        netlist
    }

    #[test]
    fn test_sm_coverage_redundant_protection() {
        let netlist = make_test_netlist_redundant_protection();
        let sm_mapping = identify_sm_cells(&netlist);
        let coverage = map_sm_coverage(&netlist, &sm_mapping);

        // func1 should be protected by both SM_A and SM_B
        assert_eq!(coverage.protection_level("top.func1"), 2);

        let redundant = coverage.redundantly_protected_cells();
        assert_eq!(redundant.len(), 1);

        let protectors = coverage.get_protectors("top.func1").unwrap();
        assert!(protectors.contains(&"SM_A".to_string()));
        assert!(protectors.contains(&"SM_B".to_string()));
    }

    #[test]
    fn test_sm_coverage_report_format() {
        let netlist = make_test_netlist_for_coverage();
        let sm_mapping = identify_sm_cells(&netlist);
        let coverage = map_sm_coverage(&netlist, &sm_mapping);
        let report = format_sm_coverage_report(&coverage);

        assert!(report.contains("Safety Mechanism Coverage Analysis"));
        assert!(report.contains("Protected cells:"));
        assert!(report.contains("Unprotected FIT"));
        assert!(report.contains("WARNING:"));
        assert!(report.contains("func3_unprotected"));
    }

    #[test]
    fn test_trace_fanin_cone() {
        let netlist = make_test_netlist_for_coverage();
        let fanin = trace_fanin_cone(&netlist, "top.sm.voter", None);

        // Fanin of SM voter should include func2 and func1 (and the voter itself)
        assert!(fanin.contains(&"top.sm.voter".to_string()));
        assert!(fanin.contains(&"top.func2".to_string()));
        assert!(fanin.contains(&"top.func1".to_string()));
        // Should NOT include func3 (it's not in the fanin)
        assert!(!fanin.contains(&"top.func3_unprotected".to_string()));
    }

    #[test]
    fn test_trace_fanout_cone() {
        let netlist = make_test_netlist_for_coverage();
        let fanout = trace_fanout_cone(&netlist, "top.func1", None);

        // Fanout of func1 should include func2 and SM voter (and func1 itself)
        assert!(fanout.contains(&"top.func1".to_string()));
        assert!(fanout.contains(&"top.func2".to_string()));
        assert!(fanout.contains(&"top.sm.voter".to_string()));
        // Should NOT include func3
        assert!(!fanout.contains(&"top.func3_unprotected".to_string()));
    }
}
