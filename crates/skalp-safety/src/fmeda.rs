//! FMEDA Generation for ISO 26262 Compliance
//!
//! This module provides automatic FMEDA (Failure Mode, Effects, and Diagnostic Analysis)
//! generation from gate-level netlists. FMEDA extends FMEA by adding diagnostic coverage
//! analysis and failure mode distribution.
//!
//! Key features:
//! - Automatic FMEDA generation from gate netlist
//! - Standard failure mode distribution by cell type
//! - Diagnostic coverage calculation
//! - CSV export for assessor review

use crate::asil::AsilLevel;
use crate::mechanisms::SafetyMechanism;
use serde::{Deserialize, Serialize};
use skalp_lir::{Cell, CellSafetyClassification, GateNetlist, TechLibrary};
use std::collections::HashMap;
use std::io::{self, Write};
use std::path::Path;

/// FMEDA entry for a single cell or component
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmedaEntry {
    /// Cell or component identifier
    pub cell_id: String,
    /// Cell type (e.g., "NAND2", "DFF", "INV")
    pub cell_type: String,
    /// Hierarchical path to the cell
    pub hierarchy_path: String,
    /// Safety classification
    pub safety_class: CellSafetyClassification,
    /// Total base FIT rate
    pub base_fit: f64,
    /// Failure mode distribution
    pub failure_distribution: FailureDistribution,
    /// Applied diagnostic coverage (%)
    pub diagnostic_coverage: f64,
    /// Safety mechanism protecting this cell (if any)
    pub safety_mechanism: Option<String>,
    /// Resulting FIT after DC application
    pub effective_fit: EffectiveFit,
}

/// Failure mode distribution for a cell
///
/// Per IEC 61508 and ISO 26262-5, failure modes are categorized as:
/// - Safe: Failures that don't lead to hazard
/// - Dangerous Detected: Dangerous failures detected by safety mechanisms
/// - Dangerous Undetected: Dangerous failures not detected (contribute to PMHF)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FailureDistribution {
    /// Percentage of failures that are inherently safe
    pub safe_pct: f64,
    /// Percentage of dangerous failures that are detected
    pub dangerous_detected_pct: f64,
    /// Percentage of dangerous failures that are undetected
    pub dangerous_undetected_pct: f64,
    /// Percentage classified as "no effect" (subset of safe)
    pub no_effect_pct: f64,
}

impl Default for FailureDistribution {
    fn default() -> Self {
        // Default distribution per IEC 61508 Annex C
        Self {
            safe_pct: 60.0,
            dangerous_detected_pct: 30.0,
            dangerous_undetected_pct: 10.0,
            no_effect_pct: 20.0,
        }
    }
}

impl FailureDistribution {
    /// Create custom failure distribution
    pub fn new(safe_pct: f64, dd_pct: f64, du_pct: f64) -> Self {
        Self {
            safe_pct,
            dangerous_detected_pct: dd_pct,
            dangerous_undetected_pct: du_pct,
            no_effect_pct: 0.0,
        }
    }

    /// Get distribution for combinational logic (gates)
    pub fn for_combinational() -> Self {
        Self {
            safe_pct: 50.0,
            dangerous_detected_pct: 35.0,
            dangerous_undetected_pct: 15.0,
            no_effect_pct: 10.0,
        }
    }

    /// Get distribution for sequential logic (flip-flops)
    pub fn for_sequential() -> Self {
        Self {
            safe_pct: 45.0,
            dangerous_detected_pct: 40.0,
            dangerous_undetected_pct: 15.0,
            no_effect_pct: 5.0,
        }
    }

    /// Get distribution for memory elements
    pub fn for_memory() -> Self {
        Self {
            safe_pct: 40.0,
            dangerous_detected_pct: 45.0,
            dangerous_undetected_pct: 15.0,
            no_effect_pct: 5.0,
        }
    }

    /// Get distribution for clock/control logic
    pub fn for_clock_control() -> Self {
        Self {
            safe_pct: 30.0,
            dangerous_detected_pct: 50.0,
            dangerous_undetected_pct: 20.0,
            no_effect_pct: 0.0,
        }
    }

    /// Validate distribution sums to ~100%
    pub fn is_valid(&self) -> bool {
        let total = self.safe_pct + self.dangerous_detected_pct + self.dangerous_undetected_pct;
        (total - 100.0).abs() < 1.0 // Allow 1% tolerance
    }
}

/// Effective FIT after applying diagnostic coverage
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EffectiveFit {
    /// Safe failure FIT (including detected dangerous)
    pub safe_fit: f64,
    /// Single point failure FIT (unprotected dangerous)
    pub spf_fit: f64,
    /// Residual failure FIT (protected but not fully detected)
    pub residual_fit: f64,
    /// Multiple point / latent failure FIT
    pub mpf_fit: f64,
    /// Total FIT (should equal base_fit)
    pub total_fit: f64,
}

impl EffectiveFit {
    /// Calculate effective FIT from base FIT, distribution, and DC
    pub fn calculate(base_fit: f64, dist: &FailureDistribution, dc: f64) -> Self {
        let safe_base = base_fit * dist.safe_pct / 100.0;
        let dd_base = base_fit * dist.dangerous_detected_pct / 100.0;
        let du_base = base_fit * dist.dangerous_undetected_pct / 100.0;

        // DC converts dangerous undetected to detected
        let dc_fraction = dc / 100.0;
        let detected_from_du = du_base * dc_fraction;
        let remaining_du = du_base - detected_from_du;

        Self {
            safe_fit: safe_base + dd_base + detected_from_du,
            spf_fit: remaining_du,
            residual_fit: 0.0, // Would need SM analysis for this
            mpf_fit: 0.0,      // Would need MPF analysis
            total_fit: base_fit,
        }
    }
}

/// Complete FMEDA report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmedaReport {
    /// Report metadata
    pub metadata: FmedaMetadata,
    /// FMEDA entries for all cells
    pub entries: Vec<FmedaEntry>,
    /// Summary statistics
    pub summary: FmedaSummary,
    /// Metrics by cell type
    pub by_cell_type: HashMap<String, CellTypeSummary>,
    /// Metrics by safety classification
    pub by_safety_class: HashMap<String, ClassificationSummary>,
}

/// FMEDA report metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmedaMetadata {
    /// Report ID
    pub id: String,
    /// Design name
    pub design_name: String,
    /// Target ASIL level
    pub target_asil: AsilLevel,
    /// Analysis date
    pub analysis_date: chrono::DateTime<chrono::Utc>,
    /// Analyst name
    pub analyst: String,
    /// Technology library used
    pub tech_library: String,
    /// Report version
    pub version: String,
}

/// FMEDA summary statistics
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct FmedaSummary {
    /// Total number of cells analyzed
    pub total_cells: usize,
    /// Total base FIT
    pub total_base_fit: f64,
    /// Total safe failure FIT
    pub total_safe_fit: f64,
    /// Total SPF FIT
    pub total_spf_fit: f64,
    /// Total residual FIT
    pub total_residual_fit: f64,
    /// Total MPF FIT
    pub total_mpf_fit: f64,
    /// Safe Failure Fraction (%)
    pub safe_failure_fraction: f64,
    /// Average diagnostic coverage (%)
    pub avg_diagnostic_coverage: f64,
    /// Cells with safety mechanism coverage
    pub cells_with_sm: usize,
    /// Cells without safety mechanism coverage
    pub cells_without_sm: usize,
}

/// Summary for a cell type
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CellTypeSummary {
    /// Cell type name
    pub cell_type: String,
    /// Count of cells
    pub count: usize,
    /// Total FIT for this type
    pub total_fit: f64,
    /// Average DC for this type
    pub avg_dc: f64,
}

/// Summary by safety classification
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ClassificationSummary {
    /// Classification name
    pub classification: String,
    /// Count of cells
    pub count: usize,
    /// Total FIT
    pub total_fit: f64,
    /// Percentage of total
    pub percentage: f64,
}

/// Generate FMEDA from gate netlist
///
/// This function analyzes a gate netlist and generates a complete FMEDA report
/// with failure mode distribution, diagnostic coverage, and FIT breakdown.
pub fn generate_fmeda(
    netlist: &GateNetlist,
    tech_library: &TechLibrary,
    mechanisms: &[SafetyMechanism],
    metadata: FmedaMetadata,
) -> FmedaReport {
    let mut entries = Vec::new();
    let mut by_cell_type: HashMap<String, CellTypeSummary> = HashMap::new();
    let mut by_safety_class: HashMap<String, ClassificationSummary> = HashMap::new();

    // Build mechanism lookup by protected cell paths
    let mechanism_lookup = build_mechanism_lookup(mechanisms);

    for cell in &netlist.cells {
        // Get cell info from tech library
        let cell_id = get_cell_id(cell);
        let cell_type = get_cell_type_name(cell);
        let base_fit = get_cell_fit(cell, tech_library);
        let safety_class = cell.safety_classification.clone();

        // Determine failure distribution based on cell type
        let distribution = get_failure_distribution(&cell_type);

        // Get diagnostic coverage from protecting mechanism
        let (dc, sm_name) = get_diagnostic_coverage(cell, &mechanism_lookup);

        // Calculate effective FIT
        let effective_fit = EffectiveFit::calculate(base_fit, &distribution, dc);

        // Get class name before moving safety_class
        let class_name = format!("{:?}", safety_class);

        let entry = FmedaEntry {
            cell_id: cell_id.clone(),
            cell_type: cell_type.clone(),
            hierarchy_path: cell.path.clone(),
            safety_class,
            base_fit,
            failure_distribution: distribution,
            diagnostic_coverage: dc,
            safety_mechanism: sm_name,
            effective_fit,
        };

        // Update cell type summary
        let type_summary =
            by_cell_type
                .entry(cell_type.clone())
                .or_insert_with(|| CellTypeSummary {
                    cell_type: cell_type.clone(),
                    count: 0,
                    total_fit: 0.0,
                    avg_dc: 0.0,
                });
        type_summary.count += 1;
        type_summary.total_fit += base_fit;
        // Running average for DC
        type_summary.avg_dc = (type_summary.avg_dc * (type_summary.count - 1) as f64 + dc)
            / type_summary.count as f64;

        // Update safety classification summary
        let class_summary = by_safety_class
            .entry(class_name.clone())
            .or_insert_with(|| ClassificationSummary {
                classification: class_name,
                count: 0,
                total_fit: 0.0,
                percentage: 0.0,
            });
        class_summary.count += 1;
        class_summary.total_fit += base_fit;

        entries.push(entry);
    }

    // Calculate summary
    let summary = calculate_summary(&entries, mechanisms);

    // Update classification percentages
    let total_fit = summary.total_base_fit;
    for class_summary in by_safety_class.values_mut() {
        class_summary.percentage = if total_fit > 0.0 {
            (class_summary.total_fit / total_fit) * 100.0
        } else {
            0.0
        };
    }

    FmedaReport {
        metadata,
        entries,
        summary,
        by_cell_type,
        by_safety_class,
    }
}

/// Build lookup table from mechanism name to cell paths
fn build_mechanism_lookup(mechanisms: &[SafetyMechanism]) -> HashMap<String, (String, f64)> {
    let mut lookup = HashMap::new();
    for mech in mechanisms {
        let dc = mech.diagnostic_coverage;
        for cell_path in &mech.implementation_cells {
            lookup.insert(cell_path.clone(), (mech.name.clone(), dc));
        }
    }
    lookup
}

/// Get cell type name from cell
fn get_cell_type_name(cell: &Cell) -> String {
    cell.cell_type.clone()
}

/// Get unique cell identifier (using path)
fn get_cell_id(cell: &Cell) -> String {
    // Use path as the identifier
    if !cell.path.is_empty() {
        cell.path.clone()
    } else {
        format!("cell_{}", cell.id.0)
    }
}

/// Get cell FIT from tech library or use cell's FIT
fn get_cell_fit(cell: &Cell, tech_library: &TechLibrary) -> f64 {
    // First try to get from cell itself (already populated from tech mapping)
    if cell.fit > 0.0 {
        return cell.fit;
    }

    // Try to get from tech library
    if let Some(lib_cell) = tech_library.get_cell(&cell.cell_type) {
        lib_cell.fit
    } else {
        // Default FIT based on cell type
        default_fit_for_cell_type(&cell.cell_type)
    }
}

/// Default FIT values for common cell types
fn default_fit_for_cell_type(cell_type: &str) -> f64 {
    let ct = cell_type.to_uppercase();
    if ct.contains("DFF") || ct.contains("LATCH") || ct.contains("FF") {
        1.0 // Sequential elements: 1 FIT
    } else if ct.contains("MUX") {
        0.8 // Multiplexers
    } else if ct.contains("BUF") || ct.contains("INV") {
        0.3 // Simple buffers/inverters
    } else if ct.contains("AND") || ct.contains("OR") || ct.contains("NAND") || ct.contains("NOR") {
        0.5 // Basic gates
    } else if ct.contains("XOR") || ct.contains("XNOR") {
        0.7 // XOR gates (more complex)
    } else if ct.contains("ADD") || ct.contains("HA") || ct.contains("FA") {
        1.5 // Adders
    } else {
        0.5 // Default
    }
}

/// Get failure distribution based on cell type
fn get_failure_distribution(cell_type: &str) -> FailureDistribution {
    let ct = cell_type.to_uppercase();
    if ct.contains("DFF") || ct.contains("LATCH") || ct.contains("FF") {
        FailureDistribution::for_sequential()
    } else if ct.contains("MEM") || ct.contains("RAM") || ct.contains("ROM") {
        FailureDistribution::for_memory()
    } else if ct.contains("CLK") || ct.contains("RST") {
        FailureDistribution::for_clock_control()
    } else {
        FailureDistribution::for_combinational()
    }
}

/// Get diagnostic coverage and mechanism name for a cell
fn get_diagnostic_coverage(
    cell: &Cell,
    mechanism_lookup: &HashMap<String, (String, f64)>,
) -> (f64, Option<String>) {
    // Check if cell path is in mechanism lookup
    if !cell.path.is_empty() {
        if let Some((name, dc)) = mechanism_lookup.get(&cell.path) {
            return (*dc, Some(name.clone()));
        }
    }

    // Default: no mechanism coverage
    (0.0, None)
}

/// Calculate summary statistics from entries
fn calculate_summary(entries: &[FmedaEntry], mechanisms: &[SafetyMechanism]) -> FmedaSummary {
    let total_cells = entries.len();
    let mut total_base_fit = 0.0;
    let mut total_safe_fit = 0.0;
    let mut total_spf_fit = 0.0;
    let mut total_residual_fit = 0.0;
    let mut total_mpf_fit = 0.0;
    let mut total_dc = 0.0;
    let mut cells_with_sm = 0;

    for entry in entries {
        total_base_fit += entry.base_fit;
        total_safe_fit += entry.effective_fit.safe_fit;
        total_spf_fit += entry.effective_fit.spf_fit;
        total_residual_fit += entry.effective_fit.residual_fit;
        total_mpf_fit += entry.effective_fit.mpf_fit;
        total_dc += entry.diagnostic_coverage;

        if entry.safety_mechanism.is_some() {
            cells_with_sm += 1;
        }
    }

    let avg_dc = if total_cells > 0 {
        total_dc / total_cells as f64
    } else {
        0.0
    };

    let safe_failure_fraction = if total_base_fit > 0.0 {
        (total_safe_fit / total_base_fit) * 100.0
    } else {
        100.0
    };

    FmedaSummary {
        total_cells,
        total_base_fit,
        total_safe_fit,
        total_spf_fit,
        total_residual_fit,
        total_mpf_fit,
        safe_failure_fraction,
        avg_diagnostic_coverage: avg_dc,
        cells_with_sm,
        cells_without_sm: total_cells - cells_with_sm,
    }
}

/// Export FMEDA report to CSV format
pub fn export_fmeda_csv<W: Write>(report: &FmedaReport, writer: &mut W) -> io::Result<()> {
    // Header
    writeln!(
        writer,
        "Cell ID,Cell Type,Hierarchy Path,Safety Class,Base FIT,Safe %,DD %,DU %,DC %,Safety Mechanism,Safe FIT,SPF FIT,Residual FIT,MPF FIT,Total FIT"
    )?;

    // Entries
    for entry in &report.entries {
        writeln!(
            writer,
            "{},{},{},{:?},{:.4},{:.1},{:.1},{:.1},{:.1},{},{:.4},{:.4},{:.4},{:.4},{:.4}",
            entry.cell_id,
            entry.cell_type,
            entry.hierarchy_path,
            entry.safety_class,
            entry.base_fit,
            entry.failure_distribution.safe_pct,
            entry.failure_distribution.dangerous_detected_pct,
            entry.failure_distribution.dangerous_undetected_pct,
            entry.diagnostic_coverage,
            entry.safety_mechanism.as_deref().unwrap_or("None"),
            entry.effective_fit.safe_fit,
            entry.effective_fit.spf_fit,
            entry.effective_fit.residual_fit,
            entry.effective_fit.mpf_fit,
            entry.effective_fit.total_fit,
        )?;
    }

    Ok(())
}

/// Export FMEDA to CSV file
pub fn export_fmeda_to_file(report: &FmedaReport, path: &Path) -> io::Result<()> {
    let mut file = std::fs::File::create(path)?;
    export_fmeda_csv(report, &mut file)
}

/// Format FMEDA summary report
pub fn format_fmeda_summary(report: &FmedaReport) -> String {
    let mut output = String::new();

    output.push_str("=== FMEDA Report Summary ===\n\n");
    output.push_str(&format!("Design: {}\n", report.metadata.design_name));
    output.push_str(&format!("Target ASIL: {:?}\n", report.metadata.target_asil));
    output.push_str(&format!(
        "Analysis Date: {}\n",
        report.metadata.analysis_date
    ));
    output.push_str(&format!("Technology: {}\n\n", report.metadata.tech_library));

    output.push_str("--- Overall Statistics ---\n");
    output.push_str(&format!("Total Cells: {}\n", report.summary.total_cells));
    output.push_str(&format!(
        "Total Base FIT: {:.2}\n",
        report.summary.total_base_fit
    ));
    output.push_str(&format!(
        "Safe Failure Fraction: {:.1}%\n",
        report.summary.safe_failure_fraction
    ));
    output.push_str(&format!(
        "Average Diagnostic Coverage: {:.1}%\n\n",
        report.summary.avg_diagnostic_coverage
    ));

    output.push_str("--- FIT Breakdown ---\n");
    output.push_str(&format!("Safe FIT: {:.2}\n", report.summary.total_safe_fit));
    output.push_str(&format!("SPF FIT: {:.2}\n", report.summary.total_spf_fit));
    output.push_str(&format!(
        "Residual FIT: {:.2}\n",
        report.summary.total_residual_fit
    ));
    output.push_str(&format!("MPF FIT: {:.2}\n\n", report.summary.total_mpf_fit));

    output.push_str("--- Safety Mechanism Coverage ---\n");
    output.push_str(&format!(
        "Cells with SM: {} ({:.1}%)\n",
        report.summary.cells_with_sm,
        (report.summary.cells_with_sm as f64 / report.summary.total_cells.max(1) as f64) * 100.0
    ));
    output.push_str(&format!(
        "Cells without SM: {} ({:.1}%)\n\n",
        report.summary.cells_without_sm,
        (report.summary.cells_without_sm as f64 / report.summary.total_cells.max(1) as f64) * 100.0
    ));

    output.push_str("--- By Cell Type ---\n");
    let mut types: Vec<_> = report.by_cell_type.values().collect();
    types.sort_by(|a, b| b.total_fit.partial_cmp(&a.total_fit).unwrap());
    for ct in types.iter().take(10) {
        output.push_str(&format!(
            "{}: {} cells, {:.2} FIT, {:.1}% DC\n",
            ct.cell_type, ct.count, ct.total_fit, ct.avg_dc
        ));
    }

    output.push_str("\n--- By Safety Classification ---\n");
    let mut classes: Vec<_> = report.by_safety_class.values().collect();
    classes.sort_by(|a, b| b.total_fit.partial_cmp(&a.total_fit).unwrap());
    for class in &classes {
        output.push_str(&format!(
            "{}: {} cells, {:.2} FIT ({:.1}%)\n",
            class.classification, class.count, class.total_fit, class.percentage
        ));
    }

    output
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;
    use skalp_lir::{CellId, GateNetId, GateNetlist, TechLibrary};

    fn create_test_netlist() -> GateNetlist {
        let mut netlist = GateNetlist::new("test_design".to_string(), "generic".to_string());

        // Add some test cells using the proper constructors
        let cell1 = Cell::new_comb(
            CellId(0),
            "NAND2".to_string(),
            "generic".to_string(),
            0.5, // FIT
            "top/module1/nand1".to_string(),
            vec![GateNetId(0), GateNetId(1)],
            vec![GateNetId(2)],
        );

        let cell2 = Cell::new_seq(
            CellId(1),
            "DFF".to_string(),
            "generic".to_string(),
            1.0, // FIT
            "top/module1/dff1".to_string(),
            vec![GateNetId(2)],
            vec![GateNetId(3)],
            GateNetId(100), // clock
            None,           // reset
        );

        let cell3 = Cell::new_comb(
            CellId(2),
            "INV".to_string(),
            "generic".to_string(),
            0.3, // FIT
            "top/module1/inv1".to_string(),
            vec![GateNetId(3)],
            vec![GateNetId(4)],
        )
        .with_safety_classification(CellSafetyClassification::SafetyMechanism {
            goal_name: "TestGoal".to_string(),
            mechanism_name: "TestMech".to_string(),
        });

        netlist.add_cell(cell1);
        netlist.add_cell(cell2);
        netlist.add_cell(cell3);

        netlist
    }

    fn create_test_metadata() -> FmedaMetadata {
        FmedaMetadata {
            id: "FMEDA-001".to_string(),
            design_name: "test_design".to_string(),
            target_asil: AsilLevel::B,
            analysis_date: Utc::now(),
            analyst: "test".to_string(),
            tech_library: "generic".to_string(),
            version: "1.0".to_string(),
        }
    }

    #[test]
    fn test_failure_distribution_default() {
        let dist = FailureDistribution::default();
        assert!(dist.is_valid());
        assert_eq!(dist.safe_pct, 60.0);
    }

    #[test]
    fn test_failure_distribution_types() {
        let comb = FailureDistribution::for_combinational();
        let seq = FailureDistribution::for_sequential();
        let mem = FailureDistribution::for_memory();

        assert!(comb.is_valid());
        assert!(seq.is_valid());
        assert!(mem.is_valid());

        // Sequential should have higher DD than combinational
        assert!(seq.dangerous_detected_pct >= comb.dangerous_detected_pct);
    }

    #[test]
    fn test_effective_fit_calculation() {
        let dist = FailureDistribution::new(60.0, 30.0, 10.0);
        let fit = EffectiveFit::calculate(100.0, &dist, 90.0);

        // Safe = 60 + 30 (DD) + 9 (90% of DU detected) = 99
        assert!((fit.safe_fit - 99.0).abs() < 0.1);
        // SPF = 10 * 0.1 = 1
        assert!((fit.spf_fit - 1.0).abs() < 0.1);
        assert_eq!(fit.total_fit, 100.0);
    }

    #[test]
    fn test_generate_fmeda() {
        let netlist = create_test_netlist();
        let tech_library = TechLibrary::new("generic");
        let metadata = create_test_metadata();

        let report = generate_fmeda(&netlist, &tech_library, &[], metadata);

        assert_eq!(report.entries.len(), 3);
        assert_eq!(report.summary.total_cells, 3);
        assert!(report.summary.total_base_fit > 0.0);
    }

    #[test]
    fn test_export_csv() {
        let netlist = create_test_netlist();
        let tech_library = TechLibrary::new("generic");
        let metadata = create_test_metadata();

        let report = generate_fmeda(&netlist, &tech_library, &[], metadata);

        let mut buffer = Vec::new();
        export_fmeda_csv(&report, &mut buffer).unwrap();

        let csv_string = String::from_utf8(buffer).unwrap();
        assert!(csv_string.contains("Cell ID,Cell Type"));
        assert!(csv_string.contains("NAND2"));
    }

    #[test]
    fn test_format_summary() {
        let netlist = create_test_netlist();
        let tech_library = TechLibrary::new("generic");
        let metadata = create_test_metadata();

        let report = generate_fmeda(&netlist, &tech_library, &[], metadata);
        let summary = format_fmeda_summary(&report);

        assert!(summary.contains("FMEDA Report Summary"));
        assert!(summary.contains("test_design"));
        assert!(summary.contains("Total Cells: 3"));
    }

    #[test]
    fn test_default_fit_values() {
        // DFF should have higher FIT than simple gate
        let dff_fit = default_fit_for_cell_type("DFF");
        let inv_fit = default_fit_for_cell_type("INV");

        assert!(dff_fit > inv_fit);
    }
}
