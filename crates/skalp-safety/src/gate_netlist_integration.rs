//! Gate Netlist Integration for FMEDA
//!
//! This module bridges the technology-mapped gate netlist (from skalp-lir)
//! with the safety analysis hierarchy. It uses:
//!
//! 1. **Explicit annotations** (`#[implements(...)]`) - Designer declares safety mechanisms
//! 2. **Measured DC from fault injection** - Not lookup tables
//!
//! # Design Philosophy
//!
//! Diagnostic coverage should be MEASURED, not assumed:
//! ```text
//! DC = (detected faults / total injected faults) × 100%
//! ```
//!
//! ISO 26262 Table D.8 values are guidelines for early estimates.
//! For ASIL certification, DC must be demonstrated through fault injection.
//!
//! # Flow
//!
//! ```text
//! GateNetlist + Annotations + FaultSimResults
//!                    ↓
//!              FmeaData (with measured DC)
//!                    ↓
//!              SafetyEntity.fmea
//! ```

use crate::design_resolver::{AnnotationLevel, SafetyAnnotation};
use crate::fault_simulation::{EffectAnalysis, SimulationCampaignResults};
use crate::hierarchy::{
    DesignRef, DetectorRef, FailureClass, FailureMode, FmeaComponent, FmeaData, InstancePath,
    MechanismType, Severity,
};
use crate::sm_failure_analysis::{SmCellMapping, SmFailureAnalysis};
use indexmap::IndexMap;
use skalp_frontend::hir::DetectionMode;
use skalp_lir::{
    Cell, CellFailureMode, CellSafetyClassification, FaultType, GateNetId, GateNetlist,
};
use std::collections::HashSet;

/// Configuration for gate netlist to FMEA conversion
#[derive(Debug, Clone)]
pub struct GateToFmeaConfig {
    /// Library name for FMEA components
    pub library_name: String,
    /// Base hierarchical path for design references
    pub base_path: String,
    /// Fallback DC if no simulation results (should be conservative, e.g., 0%)
    pub fallback_dc: f64,
}

impl Default for GateToFmeaConfig {
    fn default() -> Self {
        Self {
            library_name: "tech_library".to_string(),
            base_path: "top".to_string(),
            // Conservative fallback - no coverage assumed without simulation
            fallback_dc: 0.0,
        }
    }
}

/// Result of gate netlist to FMEA conversion
#[derive(Debug, Clone)]
pub struct GateToFmeaResult {
    /// Generated FMEA data for functional cells
    pub fmea_data: FmeaData,
    /// Safety mechanism coverage summary
    pub coverage_summary: CoverageSummary,
    /// Cells not covered by any safety mechanism
    pub uncovered_cells: Vec<String>,
    /// Warnings generated during conversion
    pub warnings: Vec<String>,
    /// FMEA data for safety mechanism cells
    pub sm_fmea: SmFmeaData,
    /// SM failure analysis results
    pub sm_analysis: SmFailureAnalysis,
}

/// FMEA data specifically for safety mechanism cells
#[derive(Debug, Clone, Default)]
pub struct SmFmeaData {
    /// FMEA components for SM cells
    pub sm_components: Vec<FmeaComponent>,
    /// Total FIT of all SM cells
    pub total_sm_fit: f64,
    /// Failure modes by mechanism name
    pub by_mechanism: IndexMap<String, Vec<FailureMode>>,
}

/// Summary of safety mechanism coverage
#[derive(Debug, Clone, Default)]
pub struct CoverageSummary {
    /// Total cells in netlist
    pub total_cells: usize,
    /// Cells covered by annotated safety mechanisms
    pub covered_cells: usize,
    /// Coverage by mechanism
    pub by_mechanism: IndexMap<String, MechanismCoverageSummary>,
    /// Total FIT before safety mechanisms
    pub total_fit_raw: f64,
    /// FIT after accounting for measured DC
    pub total_fit_residual: f64,
    /// Whether DC values are from simulation (true) or fallback (false)
    pub dc_from_simulation: bool,
    /// Number of cells that are safety mechanisms (not functional)
    pub sm_cell_count: usize,
    /// Number of functional cells
    pub functional_cell_count: usize,
    /// Total FIT of SM cells
    pub sm_fit: f64,
    /// Total FIT of functional cells
    pub functional_fit: f64,
    /// Number of detection signal outputs in the design
    pub detection_signal_count: usize,
    /// Cells covered by detection signal propagation (not just annotations)
    pub detection_covered_cells: usize,
    /// DC derived from detection signal coverage (%)
    pub detection_based_dc: f64,
    /// Runtime DC (continuous detection only) - for SPFM calculation
    pub runtime_dc: f64,
    /// Boot DC (boot-time detection) - for LFM calculation
    pub boot_dc: f64,
    /// Cells covered by continuous (runtime) detection
    pub continuous_detection_cells: usize,
    /// Cells covered by boot-time detection
    pub boot_detection_cells: usize,
}

/// Per-mechanism coverage summary
#[derive(Debug, Clone, Default)]
pub struct MechanismCoverageSummary {
    /// Number of cells covered
    pub cell_count: usize,
    /// Measured DC from fault injection (if available)
    pub measured_dc: Option<f64>,
    /// Target DC for ASIL compliance
    pub target_dc: f64,
    /// Whether measured DC meets target
    pub meets_target: bool,
}

/// Resolved safety coverage for a cell based on annotations + simulation
#[derive(Debug, Clone)]
pub struct CellSafetyCoverage {
    /// Safety goal name
    pub goal: String,
    /// Mechanism name within the goal
    pub mechanism: String,
    /// Measured DC from fault injection (None if not simulated)
    pub measured_dc: Option<f64>,
    /// Annotation level (entity/instance/signal)
    pub level: AnnotationLevel,
}

/// Convert a gate netlist to FMEA data using annotations and simulation results
///
/// # Arguments
/// * `netlist` - Gate-level netlist with cells and FIT rates
/// * `annotations` - Safety annotations from `#[implements(...)]` attributes
/// * `sim_results` - Fault injection simulation results (optional)
/// * `config` - Conversion configuration
///
/// # DC Source Priority
/// 1. Measured DC from `sim_results.effect_analyses` (preferred)
/// 2. `config.fallback_dc` (conservative default: 0%)
///
/// ISO 26262 Table D.8 values should NOT be used - they're for early estimates only.
pub fn gate_netlist_to_fmea(
    netlist: &GateNetlist,
    annotations: &[SafetyAnnotation],
    sim_results: Option<&SimulationCampaignResults>,
    config: &GateToFmeaConfig,
) -> GateToFmeaResult {
    let mut fmea_data = FmeaData::new();
    let mut sm_fmea = SmFmeaData::default();
    let mut coverage_summary = CoverageSummary::default();
    let mut uncovered_cells = Vec::new();
    let mut warnings = Vec::new();

    // Perform SM failure analysis
    let sm_analysis = SmFailureAnalysis::analyze(netlist, annotations);

    // Build annotation lookup
    let annotation_map = build_annotation_map(annotations);

    // Build DC lookup from simulation results
    let dc_map = build_dc_map_from_simulation(sim_results);
    coverage_summary.dc_from_simulation = sim_results.is_some() && !dc_map.is_empty();

    // Analyze detection signal coverage from LIR
    let detection_coverage = analyze_detection_coverage(netlist);
    coverage_summary.detection_signal_count = detection_coverage.detection_nets.len();
    coverage_summary.detection_covered_cells = detection_coverage.detection_covered_cells.len();
    coverage_summary.detection_based_dc = detection_coverage.calculated_dc;
    // Mode-based DC breakdown
    coverage_summary.runtime_dc = detection_coverage.runtime_dc;
    coverage_summary.boot_dc = detection_coverage.boot_dc;
    coverage_summary.continuous_detection_cells = detection_coverage.continuous_covered_cells.len();
    coverage_summary.boot_detection_cells = detection_coverage.boot_covered_cells.len();

    if !coverage_summary.dc_from_simulation && sim_results.is_none() {
        if coverage_summary.detection_signal_count > 0 {
            warnings.push(format!(
                "No fault injection results provided. Using detection-based DC of {:.1}% \
                 from {} detection signals covering {} cells.",
                coverage_summary.detection_based_dc,
                coverage_summary.detection_signal_count,
                coverage_summary.detection_covered_cells
            ));
        } else {
            warnings.push(
                "No fault injection results and no detection signals found. Using fallback DC of 0%. \
                 Run fault simulation or add #[detection_signal] annotations for accurate FMEDA."
                    .to_string(),
            );
        }
    }

    coverage_summary.total_cells = netlist.cells.len();

    for cell in &netlist.cells {
        let design_ref = cell_path_to_design_ref(&cell.path, &config.base_path);
        let part_name = &cell.cell_type;

        let mut component = FmeaComponent::new(
            design_ref.clone(),
            config.library_name.clone(),
            part_name.clone(),
        );

        // Find coverage from annotations and detection signals
        let coverage =
            find_coverage_for_cell(&cell.path, &annotation_map, &dc_map, &detection_coverage);

        // Convert cell failure modes to FMEA failure modes
        let mut failure_modes_for_sm: Vec<FailureMode> = Vec::new();

        for cell_fm in &cell.failure_modes {
            let (failure_mode, detector) =
                convert_cell_failure_mode(cell_fm, coverage.as_ref(), config);

            // Store failure mode for SM tracking if this is an SM cell
            if !matches!(
                cell.safety_classification,
                CellSafetyClassification::Functional
            ) {
                failure_modes_for_sm.push(failure_mode.clone());
            }

            match detector {
                DetectorRef::Psm(psm_name) => {
                    component.add_psm_failure_mode(&psm_name, failure_mode);
                }
                DetectorRef::Lsm(lsm_name) => {
                    component.add_lsm_failure_mode(&lsm_name, failure_mode);
                }
                DetectorRef::Safe => {
                    component.add_safe_mode(failure_mode);
                }
            }
        }

        // Default failure modes if none defined
        if cell.failure_modes.is_empty() && cell.fit > 0.0 {
            let default_modes = create_default_failure_modes(cell, coverage.as_ref(), config);
            for (fm, detector) in default_modes {
                // Store failure mode for SM tracking if this is an SM cell
                if !matches!(
                    cell.safety_classification,
                    CellSafetyClassification::Functional
                ) {
                    failure_modes_for_sm.push(fm.clone());
                }

                match detector {
                    DetectorRef::Psm(psm_name) => {
                        component.add_psm_failure_mode(&psm_name, fm);
                    }
                    DetectorRef::Lsm(lsm_name) => {
                        component.add_lsm_failure_mode(&lsm_name, fm);
                    }
                    DetectorRef::Safe => {
                        component.add_safe_mode(fm);
                    }
                }
            }
        }

        // Update coverage statistics based on cell classification
        coverage_summary.total_fit_raw += cell.fit;

        match &cell.safety_classification {
            CellSafetyClassification::Functional => {
                coverage_summary.functional_cell_count += 1;
                coverage_summary.functional_fit += cell.fit;

                if let Some(cov) = &coverage {
                    coverage_summary.covered_cells += 1;

                    let dc = cov.measured_dc.unwrap_or(config.fallback_dc);

                    // Update per-mechanism summary
                    let mech_summary = coverage_summary
                        .by_mechanism
                        .entry(cov.mechanism.clone())
                        .or_default();
                    mech_summary.cell_count += 1;
                    if cov.measured_dc.is_some() {
                        mech_summary.measured_dc = cov.measured_dc;
                    }

                    // Residual FIT after DC
                    coverage_summary.total_fit_residual += cell.fit * (1.0 - dc / 100.0);
                } else {
                    uncovered_cells.push(cell.path.clone());
                    coverage_summary.total_fit_residual += cell.fit;
                }

                fmea_data.add_component(component);
            }
            CellSafetyClassification::SafetyMechanism {
                goal_name,
                mechanism_name,
            } => {
                coverage_summary.sm_cell_count += 1;
                coverage_summary.sm_fit += cell.fit;
                sm_fmea.total_sm_fit += cell.fit;

                // Track failure modes by mechanism
                let mech_modes = sm_fmea
                    .by_mechanism
                    .entry(mechanism_name.clone())
                    .or_default();
                mech_modes.extend(failure_modes_for_sm);

                sm_fmea.sm_components.push(component);
            }
            CellSafetyClassification::SafetyMechanismOfSm {
                protected_sm_name,
                goal_name,
                mechanism_name,
            } => {
                coverage_summary.sm_cell_count += 1;
                coverage_summary.sm_fit += cell.fit;
                sm_fmea.total_sm_fit += cell.fit;

                // Track failure modes by mechanism (SM-of-SM)
                let sm_of_sm_key = format!("{}::protecting::{}", mechanism_name, protected_sm_name);
                let mech_modes = sm_fmea.by_mechanism.entry(sm_of_sm_key).or_default();
                mech_modes.extend(failure_modes_for_sm);

                sm_fmea.sm_components.push(component);
            }
        }
    }

    // Generate warnings
    if coverage_summary.total_fit_residual > 10.0 {
        warnings.push(format!(
            "High residual FIT: {:.2} FIT (ISO 26262 ASIL D target: 10 FIT)",
            coverage_summary.total_fit_residual
        ));
    }

    if !uncovered_cells.is_empty() {
        warnings.push(format!(
            "{} cells not covered by any #[implements(...)] annotation.",
            uncovered_cells.len()
        ));
    }

    // Add SM-related warnings
    if sm_fmea.total_sm_fit > 0.0 {
        let sm_pmhf_contribution = sm_analysis.calculate_sm_pmhf_contribution();
        if sm_pmhf_contribution > 1.0 {
            warnings.push(format!(
                "Safety mechanism hardware contributes {:.2} FIT to PMHF. Consider SM-of-SM protection.",
                sm_pmhf_contribution
            ));
        }
    }

    GateToFmeaResult {
        fmea_data,
        coverage_summary,
        uncovered_cells,
        warnings,
        sm_fmea,
        sm_analysis,
    }
}

/// Build a map from path prefixes to annotations
fn build_annotation_map(annotations: &[SafetyAnnotation]) -> IndexMap<String, &SafetyAnnotation> {
    let mut map = IndexMap::new();
    for annotation in annotations {
        let path = annotation.design_ref.instance.to_string();
        map.insert(path, annotation);
    }
    map
}

/// Build DC lookup from simulation results
///
/// Maps mechanism name -> measured DC from fault injection (as percentage 0-100)
fn build_dc_map_from_simulation(
    sim_results: Option<&SimulationCampaignResults>,
) -> IndexMap<String, f64> {
    let mut dc_map = IndexMap::new();

    if let Some(results) = sim_results {
        for (effect_name, analysis) in &results.effect_analyses {
            // The effect analysis stores measured_dc as a fraction (0.0-1.0)
            // Convert to percentage (0-100) for consistency
            let dc_pct = analysis.measured_dc * 100.0;

            // Map this back to mechanisms via detection_by_mechanism
            for mechanism in analysis.detection_by_mechanism.keys() {
                dc_map.insert(mechanism.clone(), dc_pct);
            }

            // Also store by effect name
            dc_map.insert(effect_name.clone(), dc_pct);
        }
    }

    dc_map
}

/// Detection coverage information derived from LIR detection signals
#[derive(Debug, Clone, Default)]
pub struct DetectionCoverageInfo {
    /// Set of net IDs that are marked as detection signals
    pub detection_nets: HashSet<u32>,
    /// Names of detection signal outputs
    pub detection_signal_names: Vec<String>,
    /// Cells whose outputs directly connect to detection signals
    pub detection_driver_cells: HashSet<String>,
    /// All cells that can propagate faults to detection outputs (via cone of influence)
    pub detection_covered_cells: HashSet<String>,
    /// Calculated DC based on detection coverage (%)
    pub calculated_dc: f64,

    // === Detection Mode Classification (ISO 26262 SPFM vs LFM) ===
    /// Cells covered by continuous (runtime) detection → contributes to SPFM
    pub continuous_covered_cells: HashSet<String>,
    /// Cells covered by boot-time detection → contributes to LFM only
    pub boot_covered_cells: HashSet<String>,
    /// Cells covered by periodic detection → contributes to LFM with timing factor
    pub periodic_covered_cells: HashSet<String>,
    /// Map of net ID to detection mode
    pub net_detection_modes: IndexMap<u32, DetectionMode>,
    /// Runtime DC (continuous detection only) - for SPFM calculation
    pub runtime_dc: f64,
    /// Boot DC (boot-time detection) - for LFM calculation
    pub boot_dc: f64,
}

/// Analyze detection signal coverage in the gate netlist
///
/// This function identifies detection signals (marked with `#[detection_signal]`)
/// and traces back which cells feed into them, enabling DC calculation.
///
/// Per ISO 26262, detection signals are outputs from safety mechanisms that
/// indicate fault detection. Cells that can propagate faults to these outputs
/// are considered "detected" by the safety mechanism.
///
/// ## Detection Mode Classification
///
/// - **Continuous**: Runtime detection → contributes to SPFM (Single Point Fault Metric)
/// - **Boot**: Boot-time detection (BIST) → contributes to LFM (Latent Fault Metric) only
/// - **Periodic**: Periodic detection → contributes to LFM with timing factor
/// - **OnDemand**: Software-triggered → requires SW analysis, not included in HW metrics
fn analyze_detection_coverage(netlist: &GateNetlist) -> DetectionCoverageInfo {
    let mut info = DetectionCoverageInfo::default();

    // 1. Find all detection output nets and categorize by mode
    for net in &netlist.nets {
        if net.is_detection {
            info.detection_nets.insert(net.id.0);
            info.detection_signal_names.push(net.name.clone());

            // Extract detection mode from config (default to Continuous if not specified)
            let mode = net
                .detection_config
                .as_ref()
                .map(|c| c.mode)
                .unwrap_or(DetectionMode::Continuous);

            info.net_detection_modes.insert(net.id.0, mode);

            println!(
                "✅ [DETECTION_ANALYSIS] Found detection net: {} (id={}) mode={:?}",
                net.name, net.id.0, mode
            );
        }
    }

    if info.detection_nets.is_empty() {
        println!("⚠️ [DETECTION_ANALYSIS] No detection signals found in netlist");
        return info;
    }

    // 2. For each detection mode, trace backward to find covered cells
    // We trace separately so cells can be categorized by which mode detects them
    for (&net_id, &mode) in &info.net_detection_modes.clone() {
        let covered_cells = trace_detection_cone(netlist, net_id, &info.detection_nets);

        // Categorize by mode
        for cell_path in covered_cells {
            info.detection_covered_cells.insert(cell_path.clone());

            match mode {
                DetectionMode::Continuous => {
                    info.continuous_covered_cells.insert(cell_path);
                }
                DetectionMode::Boot => {
                    info.boot_covered_cells.insert(cell_path);
                }
                DetectionMode::Periodic => {
                    info.periodic_covered_cells.insert(cell_path);
                }
                DetectionMode::OnDemand => {
                    // OnDemand detection requires SW analysis, track separately if needed
                    // For now, treat as boot-time (conservative)
                    info.boot_covered_cells.insert(cell_path);
                }
            }
        }
    }

    // 3. Find cells that directly drive detection outputs
    for cell in &netlist.cells {
        for &output_net_id in &cell.outputs {
            if info.detection_nets.contains(&output_net_id.0) {
                info.detection_driver_cells.insert(cell.path.clone());
            }
        }
    }

    // 4. Calculate DC values by mode
    let functional_cells: Vec<_> = netlist
        .cells
        .iter()
        .filter(|c| {
            matches!(
                c.safety_classification,
                CellSafetyClassification::Functional
            )
        })
        .collect();

    let total_functional = functional_cells.len();
    let covered_count = info.detection_covered_cells.len();
    let continuous_count = info.continuous_covered_cells.len();
    let boot_count = info.boot_covered_cells.len();

    if total_functional > 0 {
        // Overall DC (all detection modes)
        info.calculated_dc = (covered_count as f64 / total_functional as f64) * 100.0;
        // Runtime DC (continuous only) - for SPFM
        info.runtime_dc = (continuous_count as f64 / total_functional as f64) * 100.0;
        // Boot DC (boot-time) - for LFM
        info.boot_dc = (boot_count as f64 / total_functional as f64) * 100.0;
    }

    println!("✅ [DETECTION_ANALYSIS] Detection coverage summary:");
    println!(
        "   Total: {}/{} cells = {:.1}% DC",
        covered_count, total_functional, info.calculated_dc
    );
    println!(
        "   Runtime (SPFM): {}/{} cells = {:.1}% DC",
        continuous_count, total_functional, info.runtime_dc
    );
    println!(
        "   Boot (LFM): {}/{} cells = {:.1}% DC",
        boot_count, total_functional, info.boot_dc
    );

    info
}

/// Trace the cone of influence from a detection net backward
/// Returns all functional cells that can propagate faults to this detection output
fn trace_detection_cone(
    netlist: &GateNetlist,
    detection_net_id: u32,
    all_detection_nets: &HashSet<u32>,
) -> HashSet<String> {
    let mut covered_cells = HashSet::new();
    let mut frontier: Vec<GateNetId> = Vec::new();
    let mut visited_nets: HashSet<u32> = HashSet::new();

    // Find the cell that drives this detection net
    if let Some(net) = netlist.nets.get(detection_net_id as usize) {
        if let Some(driver_cell_id) = net.driver {
            if let Some(driver_cell) = netlist.cells.get(driver_cell_id.0 as usize) {
                // Initialize frontier with input nets of the detection driver cell
                for &input_net_id in &driver_cell.inputs {
                    if !visited_nets.contains(&input_net_id.0) {
                        frontier.push(input_net_id);
                        visited_nets.insert(input_net_id.0);
                    }
                }
            }
        }
    }

    // BFS to find all cells in the cone of influence
    while let Some(net_id) = frontier.pop() {
        // Skip other detection nets (don't cross into other detection cones)
        if all_detection_nets.contains(&net_id.0) && net_id.0 != detection_net_id {
            continue;
        }

        // Find the cell that drives this net
        if let Some(net) = netlist.nets.get(net_id.0 as usize) {
            if let Some(driver_cell_id) = net.driver {
                if let Some(driver_cell) = netlist.cells.get(driver_cell_id.0 as usize) {
                    // Only include functional cells (not SM cells themselves)
                    if matches!(
                        driver_cell.safety_classification,
                        CellSafetyClassification::Functional
                    ) {
                        covered_cells.insert(driver_cell.path.clone());
                    }

                    // Add this cell's inputs to the frontier
                    for &input_net_id in &driver_cell.inputs {
                        if !visited_nets.contains(&input_net_id.0) {
                            frontier.push(input_net_id);
                            visited_nets.insert(input_net_id.0);
                        }
                    }
                }
            }
        }
    }

    covered_cells
}

/// Find safety coverage for a cell by matching against annotations and detection signals
///
/// Coverage is determined in priority order:
/// 1. Explicit annotation match (exact or prefix)
/// 2. Detection signal coverage (cell in cone of influence)
/// 3. None (no coverage)
fn find_coverage_for_cell(
    cell_path: &str,
    annotation_map: &IndexMap<String, &SafetyAnnotation>,
    dc_map: &IndexMap<String, f64>,
    detection_coverage: &DetectionCoverageInfo,
) -> Option<CellSafetyCoverage> {
    // Exact match first
    if let Some(annotation) = annotation_map.get(cell_path) {
        return Some(annotation_to_coverage(annotation, dc_map));
    }

    // Prefix match (cell under annotated instance)
    let mut best_match: Option<(&SafetyAnnotation, usize)> = None;

    for (path, annotation) in annotation_map {
        if cell_path.starts_with(path) && cell_path[path.len()..].starts_with('.') {
            let match_len = path.len();
            if best_match.is_none() || match_len > best_match.unwrap().1 {
                best_match = Some((annotation, match_len));
            }
        }
    }

    if let Some((annotation, _)) = best_match {
        return Some(annotation_to_coverage(annotation, dc_map));
    }

    // Check if cell is covered by detection signals
    if detection_coverage
        .detection_covered_cells
        .contains(cell_path)
    {
        // Create synthetic coverage based on detection signal analysis
        return Some(CellSafetyCoverage {
            goal: "DetectionSignalCoverage".to_string(),
            mechanism: "HierarchicalDetection".to_string(),
            // Use calculated DC from detection coverage analysis
            measured_dc: Some(detection_coverage.calculated_dc),
            level: AnnotationLevel::Signal,
        });
    }

    None
}

/// Convert a SafetyAnnotation to CellSafetyCoverage with measured DC
fn annotation_to_coverage(
    annotation: &SafetyAnnotation,
    dc_map: &IndexMap<String, f64>,
) -> CellSafetyCoverage {
    // Look up measured DC by mechanism name or goal::mechanism
    let full_name = format!("{}::{}", annotation.goal_name, annotation.mechanism_name);
    let measured_dc = dc_map
        .get(&full_name)
        .or_else(|| dc_map.get(&annotation.mechanism_name))
        .copied();

    CellSafetyCoverage {
        goal: annotation.goal_name.clone(),
        mechanism: annotation.mechanism_name.clone(),
        measured_dc,
        level: annotation.level,
    }
}

/// Convert a cell path to a DesignRef
fn cell_path_to_design_ref(cell_path: &str, base_path: &str) -> DesignRef {
    let full_path = if cell_path.starts_with(base_path) {
        cell_path.to_string()
    } else {
        format!("{}.{}", base_path, cell_path)
    };

    DesignRef::instance(InstancePath::parse(&full_path))
}

/// Convert a cell failure mode to an FMEA failure mode
fn convert_cell_failure_mode(
    cell_fm: &CellFailureMode,
    coverage: Option<&CellSafetyCoverage>,
    config: &GateToFmeaConfig,
) -> (FailureMode, DetectorRef) {
    let severity = fault_type_to_severity(&cell_fm.fault_type);
    let (failure_class, coverage_pct, detector) =
        determine_failure_classification(coverage, config);

    let fm = FailureMode::new(cell_fm.name.clone(), severity, failure_class)
        .with_fit(cell_fm.fit)
        .with_coverage(coverage_pct)
        .with_effect(fault_type_to_effect(&cell_fm.fault_type));

    (fm, detector)
}

/// Map fault type to severity
fn fault_type_to_severity(fault_type: &FaultType) -> Severity {
    match fault_type {
        FaultType::StuckAt0 | FaultType::StuckAt1 | FaultType::Bridge | FaultType::Open => {
            Severity::S3
        }
        FaultType::Timing | FaultType::Delay | FaultType::ClockPath => Severity::S2,
        FaultType::Transient | FaultType::DataRetention | FaultType::ResetPath => Severity::S2,
    }
}

/// Generate effect description from fault type
fn fault_type_to_effect(fault_type: &FaultType) -> String {
    match fault_type {
        FaultType::StuckAt0 => "Output permanently stuck at logic 0".to_string(),
        FaultType::StuckAt1 => "Output permanently stuck at logic 1".to_string(),
        FaultType::Transient => "Single-cycle incorrect output value".to_string(),
        FaultType::Timing => "Setup/hold violation causing metastability".to_string(),
        FaultType::Bridge => "Short circuit between nets".to_string(),
        FaultType::Open => "Open circuit causing floating output".to_string(),
        FaultType::Delay => "Signal arrives late but with correct value".to_string(),
        FaultType::DataRetention => "Register fails to retain data".to_string(),
        FaultType::ClockPath => "Clock signal fails to reach register".to_string(),
        FaultType::ResetPath => "Reset signal fails to reach register".to_string(),
    }
}

/// Determine failure classification based on coverage
fn determine_failure_classification(
    coverage: Option<&CellSafetyCoverage>,
    config: &GateToFmeaConfig,
) -> (FailureClass, f64, DetectorRef) {
    match coverage {
        Some(cov) => {
            let dc = cov.measured_dc.unwrap_or(config.fallback_dc);
            let psm_name = format!("{}::{}", cov.goal, cov.mechanism);

            // If we have measured DC, it's residual (detected faults removed)
            // If no measured DC, conservative: single point fault
            let failure_class = if cov.measured_dc.is_some() {
                FailureClass::Residual
            } else {
                FailureClass::SinglePointFault
            };

            (failure_class, dc, DetectorRef::Psm(psm_name))
        }
        None => (FailureClass::SinglePointFault, 0.0, DetectorRef::Safe),
    }
}

/// Create default failure modes when cell doesn't have explicit ones
fn create_default_failure_modes(
    cell: &Cell,
    coverage: Option<&CellSafetyCoverage>,
    config: &GateToFmeaConfig,
) -> Vec<(FailureMode, DetectorRef)> {
    let mut modes = Vec::new();

    let stuck_at_fit = cell.fit * 0.4;
    let other_fit = cell.fit * 0.2;

    let (class, coverage_pct, detector) = determine_failure_classification(coverage, config);

    modes.push((
        FailureMode::new("stuck_at_0".to_string(), Severity::S3, class)
            .with_fit(stuck_at_fit)
            .with_coverage(coverage_pct)
            .with_effect("Output permanently stuck at logic 0".to_string()),
        detector.clone(),
    ));

    modes.push((
        FailureMode::new("stuck_at_1".to_string(), Severity::S3, class)
            .with_fit(stuck_at_fit)
            .with_coverage(coverage_pct)
            .with_effect("Output permanently stuck at logic 1".to_string()),
        detector.clone(),
    ));

    modes.push((
        FailureMode::new("transient".to_string(), Severity::S2, class)
            .with_fit(other_fit)
            .with_coverage(coverage_pct)
            .with_effect("Single-cycle incorrect output".to_string()),
        detector,
    ));

    modes
}

/// Merge FMEA data into a SafetyEntity's FMEA components
pub fn merge_fmea_into_entity(fmea_data: &FmeaData, entity_fmea: &mut Vec<FmeaComponent>) {
    for component in &fmea_data.components {
        entity_fmea.push(component.clone());
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_lir::CellId;

    fn make_test_netlist() -> GateNetlist {
        let mut netlist = GateNetlist::new("test".to_string(), "generic_asic".to_string());

        let a = netlist.add_input("a".to_string());
        let b = netlist.add_input("b".to_string());
        let out = netlist.add_output("out".to_string());

        let mut cell = Cell::new_comb(
            CellId(0),
            "NAND2_X1".to_string(),
            "generic_asic".to_string(),
            0.1,
            "top.voting.nand".to_string(),
            vec![a, b],
            vec![out],
        );

        cell.failure_modes.push(CellFailureMode {
            name: "stuck_at_0".to_string(),
            fit: 0.05,
            fault_type: FaultType::StuckAt0,
        });
        cell.failure_modes.push(CellFailureMode {
            name: "stuck_at_1".to_string(),
            fit: 0.05,
            fault_type: FaultType::StuckAt1,
        });

        netlist.add_cell(cell);
        netlist
    }

    #[test]
    fn test_gate_to_fmea_no_simulation() {
        let netlist = make_test_netlist();
        let annotations: Vec<SafetyAnnotation> = vec![];
        let config = GateToFmeaConfig::default();

        let result = gate_netlist_to_fmea(&netlist, &annotations, None, &config);

        assert_eq!(result.coverage_summary.total_cells, 1);
        assert_eq!(result.coverage_summary.covered_cells, 0);
        assert!(!result.coverage_summary.dc_from_simulation);
        // Warning about no simulation results
        assert!(result
            .warnings
            .iter()
            .any(|w| w.contains("fault injection")));
    }

    #[test]
    fn test_gate_to_fmea_with_annotation_no_sim() {
        let netlist = make_test_netlist();

        let annotations = vec![SafetyAnnotation {
            design_ref: DesignRef::instance(InstancePath::parse("top.voting")),
            goal_name: "BrakingSafety".to_string(),
            mechanism_name: "SensorVoting".to_string(),
            level: AnnotationLevel::Instance,
        }];

        let config = GateToFmeaConfig::default();
        let result = gate_netlist_to_fmea(&netlist, &annotations, None, &config);

        assert_eq!(result.coverage_summary.covered_cells, 1);
        // Without simulation, DC = fallback (0%)
        // So residual FIT = raw FIT
        assert!(
            (result.coverage_summary.total_fit_residual - result.coverage_summary.total_fit_raw)
                .abs()
                < 0.001
        );
    }

    #[test]
    fn test_gate_to_fmea_with_measured_dc() {
        let netlist = make_test_netlist();

        let annotations = vec![SafetyAnnotation {
            design_ref: DesignRef::instance(InstancePath::parse("top.voting")),
            goal_name: "BrakingSafety".to_string(),
            mechanism_name: "SensorVoting".to_string(),
            level: AnnotationLevel::Instance,
        }];

        // Create simulation results with measured DC
        let mut sim_results = SimulationCampaignResults::new("BrakingSafety", "top");
        let mut effect = EffectAnalysis::new("valve_stuck", Severity::S3, 99.0);
        effect.total_faults_causing = 1000;
        effect.faults_detected = 985; // 98.5% DC
        effect.update_dc();
        effect
            .detection_by_mechanism
            .insert("SensorVoting".to_string(), 985);
        sim_results
            .effect_analyses
            .insert("valve_stuck".to_string(), effect);

        let config = GateToFmeaConfig::default();
        let result = gate_netlist_to_fmea(&netlist, &annotations, Some(&sim_results), &config);

        assert_eq!(result.coverage_summary.covered_cells, 1);
        assert!(result.coverage_summary.dc_from_simulation);

        // With 98.5% DC, residual should be ~1.5% of raw
        let expected_residual = result.coverage_summary.total_fit_raw * 0.015;
        assert!((result.coverage_summary.total_fit_residual - expected_residual).abs() < 0.01);
    }

    #[test]
    fn test_fallback_dc_is_conservative() {
        let config = GateToFmeaConfig::default();
        // Default fallback should be 0% (conservative - assume no detection)
        assert!((config.fallback_dc - 0.0).abs() < 0.001);
    }

    #[test]
    fn test_fault_type_to_severity() {
        assert_eq!(fault_type_to_severity(&FaultType::StuckAt0), Severity::S3);
        assert_eq!(fault_type_to_severity(&FaultType::StuckAt1), Severity::S3);
        assert_eq!(fault_type_to_severity(&FaultType::Transient), Severity::S2);
    }

    #[test]
    fn test_cell_path_to_design_ref() {
        let ref1 = cell_path_to_design_ref("top.counter.bit0", "top");
        assert_eq!(ref1.instance.to_string(), "top.counter.bit0");

        let ref2 = cell_path_to_design_ref("counter.bit0", "top");
        assert_eq!(ref2.instance.to_string(), "top.counter.bit0");
    }
}
