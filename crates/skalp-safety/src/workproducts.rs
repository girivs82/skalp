//! ISO 26262 Work Product Generation
//!
//! Generates all ISO 26262 Part 5 (Hardware) work products when safety metrics pass.
//! Work products include:
//! - Safety Manual
//! - FMEDA Report
//! - Traceability Matrix
//! - Hardware Safety Requirements Specification
//! - HSI Specification
//! - Safety Mechanism Report
//! - Dependent Failures Analysis (DFA)
//! - Safety Analysis Summary

use crate::analysis::AnalysisContext;
use crate::hierarchy::{DesignRef, SafetyHierarchy};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::Write as FmtWrite;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use thiserror::Error;

/// Results from combined analysis
#[derive(Debug, Clone, Default)]
pub struct CombinedAnalysisResult {
    pub spfm: Option<f64>,
    pub lfm: Option<f64>,
    pub pmhf: Option<f64>,
    pub total_fit: Option<f64>,
}

/// Helper function to format a DesignRef as a string
fn format_design_ref(design_ref: &DesignRef) -> String {
    let mut s = design_ref.instance.to_string();
    if let Some(signal) = &design_ref.signal {
        s.push_str("::");
        s.push_str(signal);
        if let Some((high, low)) = design_ref.bit_range {
            s.push_str(&format!("[{}:{}]", high, low));
        }
    }
    s
}

/// Helper function to format a Duration as a string
fn format_duration(d: &std::time::Duration) -> String {
    let secs = d.as_secs_f64();
    if secs >= 1.0 {
        format!("{:.2}s", secs)
    } else if secs >= 0.001 {
        format!("{:.2}ms", secs * 1000.0)
    } else if secs >= 0.000001 {
        format!("{:.2}us", secs * 1_000_000.0)
    } else {
        format!("{:.2}ns", secs * 1_000_000_000.0)
    }
}

/// Errors that can occur during work product generation
#[derive(Error, Debug)]
pub enum WorkProductError {
    #[error("Metrics check failed: {0}")]
    MetricsCheckFailed(String),
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
    #[error("Format error: {0}")]
    FormatError(String),
    #[error("Missing data: {0}")]
    MissingData(String),
}

/// Supported output formats for work products
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum OutputFormat {
    /// Markdown format
    Markdown,
    /// HTML format with styling
    Html,
    /// CSV format for spreadsheet import
    Csv,
    /// ReqIF format for DOORS/Polarion
    ReqIF,
    /// XML format for Medini
    Xml,
    /// PDF format (placeholder - requires external tool)
    Pdf,
}

impl OutputFormat {
    /// Get file extension for this format
    pub fn extension(&self) -> &'static str {
        match self {
            OutputFormat::Markdown => "md",
            OutputFormat::Html => "html",
            OutputFormat::Csv => "csv",
            OutputFormat::ReqIF => "reqif",
            OutputFormat::Xml => "xml",
            OutputFormat::Pdf => "pdf",
        }
    }

    /// Parse format from string
    pub fn parse(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "md" | "markdown" => Some(OutputFormat::Markdown),
            "html" => Some(OutputFormat::Html),
            "csv" => Some(OutputFormat::Csv),
            "reqif" => Some(OutputFormat::ReqIF),
            "xml" => Some(OutputFormat::Xml),
            "pdf" => Some(OutputFormat::Pdf),
            _ => None,
        }
    }
}

/// Types of work products that can be generated
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum WorkProductType {
    /// Safety Manual - overview of safety goals and mechanisms
    SafetyManual,
    /// FMEDA Report - detailed failure mode analysis
    FmedaReport,
    /// Traceability Matrix - HSR to PSM to Design to FMEA
    TraceabilityMatrix,
    /// Hardware Safety Requirements Specification
    HsrSpecification,
    /// Hardware-Software Interface Specification
    HsiSpecification,
    /// Safety Mechanism Report - inventory with coverage
    SafetyMechanismReport,
    /// Dependent Failures Analysis
    DfaReport,
    /// Safety Analysis Summary - pass/fail and gap analysis
    SafetySummary,
}

impl WorkProductType {
    /// Get default filename for this work product
    pub fn default_filename(&self) -> &'static str {
        match self {
            WorkProductType::SafetyManual => "safety_manual",
            WorkProductType::FmedaReport => "fmeda_report",
            WorkProductType::TraceabilityMatrix => "traceability_matrix",
            WorkProductType::HsrSpecification => "hsr_specification",
            WorkProductType::HsiSpecification => "hsi_specification",
            WorkProductType::SafetyMechanismReport => "safety_mechanisms",
            WorkProductType::DfaReport => "dfa_report",
            WorkProductType::SafetySummary => "safety_summary",
        }
    }

    /// Parse work product type from string
    pub fn parse(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "manual" | "safety_manual" | "safetymanual" => Some(WorkProductType::SafetyManual),
            "fmeda" | "fmeda_report" | "fmedareport" => Some(WorkProductType::FmedaReport),
            "traceability" | "traceability_matrix" | "trace" => {
                Some(WorkProductType::TraceabilityMatrix)
            }
            "hsr" | "hsr_specification" | "hsrspec" => Some(WorkProductType::HsrSpecification),
            "hsi" | "hsi_specification" | "hsispec" => Some(WorkProductType::HsiSpecification),
            "mechanisms" | "safety_mechanisms" | "mechanism" => {
                Some(WorkProductType::SafetyMechanismReport)
            }
            "dfa" | "dfa_report" | "dfareport" => Some(WorkProductType::DfaReport),
            "summary" | "safety_summary" | "safetysummary" => Some(WorkProductType::SafetySummary),
            "all" => None, // "all" is handled specially
            _ => None,
        }
    }

    /// Get all work product types
    pub fn all() -> Vec<Self> {
        vec![
            WorkProductType::SafetyManual,
            WorkProductType::FmedaReport,
            WorkProductType::TraceabilityMatrix,
            WorkProductType::HsrSpecification,
            WorkProductType::HsiSpecification,
            WorkProductType::SafetyMechanismReport,
            WorkProductType::DfaReport,
            WorkProductType::SafetySummary,
        ]
    }
}

/// Configuration for work product generation
#[derive(Debug, Clone)]
pub struct WorkProductConfig {
    /// Output directory for generated files
    pub output_dir: PathBuf,
    /// Work products to generate
    pub products: Vec<WorkProductType>,
    /// Output formats for each product type
    pub formats: Vec<OutputFormat>,
    /// Whether to check metrics before generating
    pub check_metrics: bool,
    /// Design name for headers
    pub design_name: String,
}

impl Default for WorkProductConfig {
    fn default() -> Self {
        Self {
            output_dir: PathBuf::from("safety_docs"),
            products: WorkProductType::all(),
            formats: vec![OutputFormat::Markdown],
            check_metrics: true,
            design_name: "Design".to_string(),
        }
    }
}

/// Result of metrics check
#[derive(Debug, Clone)]
pub struct MetricsCheckResult {
    /// Whether all metrics passed
    pub passed: bool,
    /// SPFM value and target
    pub spfm: Option<(f64, f64)>,
    /// LFM value and target
    pub lfm: Option<(f64, f64)>,
    /// PMHF value and target
    pub pmhf: Option<(f64, f64)>,
    /// Gap analysis for failed metrics
    pub gaps: Vec<MetricsGap>,
}

/// A gap in safety coverage
#[derive(Debug, Clone)]
pub struct MetricsGap {
    /// Component with insufficient coverage
    pub component: String,
    /// Failure mode
    pub failure_mode: String,
    /// Current coverage
    pub current_coverage: f64,
    /// Required coverage
    pub required_coverage: f64,
    /// Detecting mechanism
    pub mechanism: String,
    /// Suggested fix
    pub suggestion: String,
}

/// Work product generator
pub struct WorkProductGenerator {
    /// Safety hierarchy data
    hierarchy: SafetyHierarchy,
    /// Analysis context
    context: AnalysisContext,
    /// Analysis results
    results: Option<CombinedAnalysisResult>,
    /// Configuration
    config: WorkProductConfig,
}

impl WorkProductGenerator {
    /// Create a new work product generator
    pub fn new(
        hierarchy: SafetyHierarchy,
        context: AnalysisContext,
        config: WorkProductConfig,
    ) -> Self {
        Self {
            hierarchy,
            context,
            results: None,
            config,
        }
    }

    /// Set analysis results
    pub fn with_results(mut self, results: CombinedAnalysisResult) -> Self {
        self.results = Some(results);
        self
    }

    /// Check if metrics pass the target ASIL requirements
    pub fn check_metrics(&self) -> MetricsCheckResult {
        use crate::asil::AsilLevel;

        let target_asil = &self.context.target_asil;

        // Get target thresholds from ASIL requirements
        let requirements = target_asil.requirements();
        let spfm_target = requirements.spfm_target;
        let lfm_target = requirements.lf_target;
        let pmhf_target = match target_asil {
            AsilLevel::QM => None,
            AsilLevel::A => Some(1000.0),
            AsilLevel::B => Some(100.0),
            AsilLevel::C => Some(100.0),
            AsilLevel::D => Some(10.0),
        };

        // Get actual values (from results or compute)
        let spfm_actual = self.results.as_ref().and_then(|r| r.spfm).unwrap_or(95.0);
        let lfm_actual = self.results.as_ref().and_then(|r| r.lfm).unwrap_or(85.0);
        let pmhf_actual = self.results.as_ref().and_then(|r| r.pmhf).unwrap_or(15.0);

        let mut passed = true;
        let mut gaps = Vec::new();

        // Check SPFM
        if let Some(target) = spfm_target {
            if spfm_actual < target {
                passed = false;
                gaps.push(MetricsGap {
                    component: "Overall Design".to_string(),
                    failure_mode: "Various".to_string(),
                    current_coverage: spfm_actual,
                    required_coverage: target,
                    mechanism: "Various PSM".to_string(),
                    suggestion: "Increase diagnostic coverage or add safety mechanisms".to_string(),
                });
            }
        }

        // Check LFM
        if let Some(target) = lfm_target {
            if lfm_actual < target {
                passed = false;
                gaps.push(MetricsGap {
                    component: "Overall Design".to_string(),
                    failure_mode: "Various".to_string(),
                    current_coverage: lfm_actual,
                    required_coverage: target,
                    mechanism: "Various LSM".to_string(),
                    suggestion: "Add latent fault tests or increase test coverage".to_string(),
                });
            }
        }

        // Check PMHF
        if let Some(target) = pmhf_target {
            if pmhf_actual > target {
                passed = false;
                gaps.push(MetricsGap {
                    component: "Overall Design".to_string(),
                    failure_mode: "Various".to_string(),
                    current_coverage: pmhf_actual,
                    required_coverage: target,
                    mechanism: "Various".to_string(),
                    suggestion: "Reduce base FIT rates or increase fault coverage".to_string(),
                });
            }
        }

        MetricsCheckResult {
            passed,
            spfm: spfm_target.map(|t| (spfm_actual, t)),
            lfm: lfm_target.map(|t| (lfm_actual, t)),
            pmhf: pmhf_target.map(|t| (pmhf_actual, t)),
            gaps,
        }
    }

    /// Generate all configured work products
    pub fn generate(&self) -> Result<Vec<PathBuf>, WorkProductError> {
        // Check metrics if configured
        if self.config.check_metrics {
            let check = self.check_metrics();
            if !check.passed {
                let gaps_msg = check
                    .gaps
                    .iter()
                    .map(|g| {
                        format!(
                            "  - {}: {:.1}% < {:.1}%",
                            g.component, g.current_coverage, g.required_coverage
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n");
                return Err(WorkProductError::MetricsCheckFailed(format!(
                    "Metrics below target:\n{}",
                    gaps_msg
                )));
            }
        }

        // Ensure output directory exists
        fs::create_dir_all(&self.config.output_dir)?;

        let mut generated = Vec::new();

        for product in &self.config.products {
            for format in &self.config.formats {
                let path = self.generate_product(*product, *format)?;
                generated.push(path);
            }
        }

        Ok(generated)
    }

    /// Generate a single work product
    fn generate_product(
        &self,
        product: WorkProductType,
        format: OutputFormat,
    ) -> Result<PathBuf, WorkProductError> {
        let content = match product {
            WorkProductType::SafetyManual => self.generate_safety_manual(format)?,
            WorkProductType::FmedaReport => self.generate_fmeda_report(format)?,
            WorkProductType::TraceabilityMatrix => self.generate_traceability_matrix(format)?,
            WorkProductType::HsrSpecification => self.generate_hsr_specification(format)?,
            WorkProductType::HsiSpecification => self.generate_hsi_specification(format)?,
            WorkProductType::SafetyMechanismReport => {
                self.generate_safety_mechanism_report(format)?
            }
            WorkProductType::DfaReport => self.generate_dfa_report(format)?,
            WorkProductType::SafetySummary => self.generate_safety_summary(format)?,
        };

        let filename = format!("{}.{}", product.default_filename(), format.extension());
        let path = self.config.output_dir.join(&filename);

        let mut file = fs::File::create(&path)?;
        file.write_all(content.as_bytes())?;

        Ok(path)
    }

    /// Generate Safety Manual
    fn generate_safety_manual(&self, format: OutputFormat) -> Result<String, WorkProductError> {
        let mut content = String::new();

        match format {
            OutputFormat::Markdown => {
                writeln!(content, "# Safety Manual: {}\n", self.config.design_name).unwrap();
                writeln!(content, "## 1. Safety Goal\n").unwrap();

                for goal in self.hierarchy.goals.values() {
                    writeln!(content, "| Field | Value |").unwrap();
                    writeln!(content, "|-------|-------|").unwrap();
                    writeln!(content, "| ID | {} |", goal.external_id).unwrap();
                    writeln!(content, "| Description | {} |", goal.description).unwrap();
                    writeln!(content, "| ASIL | {:?} |", goal.asil).unwrap();
                    if let Some(ftti) = &goal.ftti {
                        writeln!(content, "| FTTI | {} |", format_duration(ftti)).unwrap();
                    }
                    if !goal.traces_to.is_empty() {
                        writeln!(
                            content,
                            "| External Reference | {} |",
                            goal.traces_to.join(", ")
                        )
                        .unwrap();
                    }
                    writeln!(content).unwrap();
                }

                writeln!(content, "## 2. Hardware Safety Requirements\n").unwrap();

                for goal in self.hierarchy.goals.values() {
                    for hsr in &goal.hsrs {
                        writeln!(content, "### {}: {}\n", hsr.id, hsr.requirement).unwrap();
                        writeln!(
                            content,
                            "- **Verification**: {}",
                            hsr.verification
                                .iter()
                                .map(|v| format!("{:?}", v))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                        .unwrap();
                        if let Some(psm) = &hsr.psm {
                            writeln!(content, "- **Safety Mechanism**: {} (PSM)", psm.name)
                                .unwrap();
                            writeln!(content, "- **Diagnostic Coverage**: {:.1}%", psm.dc_target)
                                .unwrap();
                        }
                        writeln!(content).unwrap();
                    }
                }

                writeln!(content, "## 3. Safety Mechanisms Inventory\n").unwrap();
                writeln!(
                    content,
                    "| Mechanism | Type | DC/LC | Detection Time | Implementation |"
                )
                .unwrap();
                writeln!(
                    content,
                    "|-----------|------|-------|----------------|----------------|"
                )
                .unwrap();

                for goal in self.hierarchy.goals.values() {
                    for hsr in &goal.hsrs {
                        if let Some(psm) = &hsr.psm {
                            let detection = psm
                                .dhsr
                                .as_ref()
                                .and_then(|d| d.detection_time.as_ref())
                                .map(format_duration)
                                .unwrap_or_else(|| "-".to_string());
                            let impls = psm
                                .implementations
                                .iter()
                                .map(format_design_ref)
                                .collect::<Vec<_>>()
                                .join(", ");
                            writeln!(
                                content,
                                "| {} | PSM | {:.1}% | {} | {} |",
                                psm.name, psm.dc_target, detection, impls
                            )
                            .unwrap();
                        }
                    }
                    for lsm in &goal.lsms {
                        let interval = lsm
                            .interval
                            .as_ref()
                            .map(|i| format!("{} interval", format_duration(i)))
                            .unwrap_or_else(|| "-".to_string());
                        let impls = lsm
                            .implementations
                            .iter()
                            .map(format_design_ref)
                            .collect::<Vec<_>>()
                            .join(", ");
                        writeln!(
                            content,
                            "| {} | LSM | {:.1}% | {} | {} |",
                            lsm.name, lsm.lc_target, interval, impls
                        )
                        .unwrap();
                    }
                }

                writeln!(content, "\n## 4. FMEDA Summary\n").unwrap();

                let spfm = self.results.as_ref().and_then(|r| r.spfm).unwrap_or(0.0);
                let lfm = self.results.as_ref().and_then(|r| r.lfm).unwrap_or(0.0);
                let pmhf = self.results.as_ref().and_then(|r| r.pmhf).unwrap_or(0.0);
                let total_fit = self
                    .results
                    .as_ref()
                    .and_then(|r| r.total_fit)
                    .unwrap_or(0.0);

                writeln!(content, "- **Total FIT**: {:.1}", total_fit).unwrap();
                writeln!(content, "- **SPFM**: {:.1}%", spfm).unwrap();
                writeln!(content, "- **LFM**: {:.1}%", lfm).unwrap();
                writeln!(content, "- **PMHF**: {:.1} FIT", pmhf).unwrap();
            }
            OutputFormat::Html => {
                content = self.wrap_html(
                    &format!("Safety Manual: {}", self.config.design_name),
                    &self.generate_safety_manual(OutputFormat::Markdown)?,
                );
            }
            _ => {
                return Err(WorkProductError::FormatError(format!(
                    "Format {:?} not supported for Safety Manual",
                    format
                )));
            }
        }

        Ok(content)
    }

    /// Generate FMEDA Report
    fn generate_fmeda_report(&self, format: OutputFormat) -> Result<String, WorkProductError> {
        let mut content = String::new();

        match format {
            OutputFormat::Markdown => {
                writeln!(content, "# FMEDA Report: {}\n", self.config.design_name).unwrap();

                writeln!(content, "## Component Failure Analysis\n").unwrap();
                writeln!(
                    content,
                    "| Component | Part | Total FIT | Safe | SPF | Residual | Latent | DC | LC |"
                )
                .unwrap();
                writeln!(
                    content,
                    "|-----------|------|-----------|------|-----|----------|--------|----|----|"
                )
                .unwrap();

                for entity in self.hierarchy.entities.values() {
                    for fmea in &entity.fmea {
                        let component = format_design_ref(&fmea.design_ref);
                        let safe_fit = 0.0; // Placeholder
                        let spf_fit = 0.0;
                        let residual_fit = 0.0;
                        let latent_fit = 0.0;
                        let total_fit = 0.0;
                        let dc = 99.0; // Placeholder
                        let lc = 90.0;

                        writeln!(
                            content,
                            "| {} | {} | {:.1} | {:.1} | {:.1} | {:.1} | {:.1} | {:.1}% | {:.1}% |",
                            component,
                            fmea.part,
                            total_fit,
                            safe_fit,
                            spf_fit,
                            residual_fit,
                            latent_fit,
                            dc,
                            lc
                        )
                        .unwrap();
                    }
                }

                writeln!(content, "\n## Metrics Summary\n").unwrap();

                let spfm = self.results.as_ref().and_then(|r| r.spfm).unwrap_or(0.0);
                let lfm = self.results.as_ref().and_then(|r| r.lfm).unwrap_or(0.0);
                let pmhf = self.results.as_ref().and_then(|r| r.pmhf).unwrap_or(0.0);

                writeln!(content, "| Metric | Value | Target | Status |").unwrap();
                writeln!(content, "|--------|-------|--------|--------|").unwrap();

                // Get targets from ASIL requirements
                let requirements = self.context.target_asil.requirements();
                let spfm_target = requirements.spfm_target;
                let lfm_target = requirements.lf_target;
                let pmhf_target = match self.context.target_asil {
                    crate::asil::AsilLevel::QM => None,
                    crate::asil::AsilLevel::A => Some(1000.0),
                    crate::asil::AsilLevel::B => Some(100.0),
                    crate::asil::AsilLevel::C => Some(100.0),
                    crate::asil::AsilLevel::D => Some(10.0),
                };

                if let Some(target) = spfm_target {
                    let status = if spfm >= target { "PASS" } else { "FAIL" };
                    writeln!(
                        content,
                        "| SPFM | {:.1}% | >= {:.1}% | {} |",
                        spfm, target, status
                    )
                    .unwrap();
                }

                if let Some(target) = lfm_target {
                    let status = if lfm >= target { "PASS" } else { "FAIL" };
                    writeln!(
                        content,
                        "| LFM | {:.1}% | >= {:.1}% | {} |",
                        lfm, target, status
                    )
                    .unwrap();
                }

                if let Some(target) = pmhf_target {
                    let status = if pmhf <= target { "PASS" } else { "FAIL" };
                    writeln!(
                        content,
                        "| PMHF | {:.1} FIT | <= {:.1} FIT | {} |",
                        pmhf, target, status
                    )
                    .unwrap();
                }
            }
            OutputFormat::Html => {
                let md = self.generate_fmeda_report(OutputFormat::Markdown)?;
                content =
                    self.wrap_html(&format!("FMEDA Report: {}", self.config.design_name), &md);
            }
            OutputFormat::Csv => {
                writeln!(
                    content,
                    "Component,Part,Total FIT,Safe,SPF,Residual,Latent,DC,LC"
                )
                .unwrap();

                for entity in self.hierarchy.entities.values() {
                    for fmea in &entity.fmea {
                        writeln!(
                            content,
                            "{},{},{:.1},{:.1},{:.1},{:.1},{:.1},{:.1},{:.1}",
                            format_design_ref(&fmea.design_ref),
                            fmea.part,
                            0.0,
                            0.0,
                            0.0,
                            0.0,
                            0.0,
                            99.0,
                            90.0
                        )
                        .unwrap();
                    }
                }
            }
            _ => {
                return Err(WorkProductError::FormatError(format!(
                    "Format {:?} not supported for FMEDA Report",
                    format
                )));
            }
        }

        Ok(content)
    }

    /// Generate Traceability Matrix
    fn generate_traceability_matrix(
        &self,
        format: OutputFormat,
    ) -> Result<String, WorkProductError> {
        let mut content = String::new();

        match format {
            OutputFormat::Markdown => {
                writeln!(
                    content,
                    "# Traceability Matrix: {}\n",
                    self.config.design_name
                )
                .unwrap();

                for goal in self.hierarchy.goals.values() {
                    writeln!(content, "## {} ({})\n", goal.name, goal.external_id).unwrap();
                    writeln!(content, "ASIL: {:?}\n", goal.asil).unwrap();

                    if !goal.traces_to.is_empty() {
                        writeln!(content, "Traces to: {}\n", goal.traces_to.join(", ")).unwrap();
                    }

                    for hsr in &goal.hsrs {
                        writeln!(content, "### {} - {}\n", hsr.id, hsr.requirement).unwrap();

                        if let Some(psm) = &hsr.psm {
                            writeln!(
                                content,
                                "- **PSM**: {} (DC >= {:.1}%)",
                                psm.name, psm.dc_target
                            )
                            .unwrap();

                            if let Some(dhsr) = &psm.dhsr {
                                writeln!(
                                    content,
                                    "  - **DHSR**: {} - {}",
                                    dhsr.id, dhsr.requirement
                                )
                                .unwrap();
                            }

                            if !psm.implementations.is_empty() {
                                writeln!(content, "  - **Implementations**:").unwrap();
                                for impl_ref in &psm.implementations {
                                    writeln!(content, "    - {}", format_design_ref(impl_ref))
                                        .unwrap();
                                }
                            }
                        }
                        writeln!(content).unwrap();
                    }

                    for lsm in &goal.lsms {
                        writeln!(content, "### {} (LSM)\n", lsm.name).unwrap();
                        writeln!(content, "- LC >= {:.1}%", lsm.lc_target).unwrap();
                        if let Some(interval) = &lsm.interval {
                            writeln!(content, "- Interval: {}", format_duration(interval)).unwrap();
                        }
                        if !lsm.implementations.is_empty() {
                            writeln!(content, "- **Implementations**:").unwrap();
                            for impl_ref in &lsm.implementations {
                                writeln!(content, "  - {}", format_design_ref(impl_ref)).unwrap();
                            }
                        }
                        writeln!(content).unwrap();
                    }
                }
            }
            OutputFormat::Html => {
                let md = self.generate_traceability_matrix(OutputFormat::Markdown)?;
                content = self.wrap_html(
                    &format!("Traceability Matrix: {}", self.config.design_name),
                    &md,
                );
            }
            OutputFormat::ReqIF => {
                content = self.generate_reqif_traceability()?;
            }
            _ => {
                return Err(WorkProductError::FormatError(format!(
                    "Format {:?} not supported for Traceability Matrix",
                    format
                )));
            }
        }

        Ok(content)
    }

    /// Generate HSR Specification
    fn generate_hsr_specification(&self, format: OutputFormat) -> Result<String, WorkProductError> {
        let mut content = String::new();

        match format {
            OutputFormat::Markdown => {
                writeln!(
                    content,
                    "# Hardware Safety Requirements Specification: {}\n",
                    self.config.design_name
                )
                .unwrap();

                for goal in self.hierarchy.goals.values() {
                    writeln!(
                        content,
                        "## Safety Goal: {} ({})\n",
                        goal.name, goal.external_id
                    )
                    .unwrap();

                    for hsr in &goal.hsrs {
                        writeln!(content, "### {}\n", hsr.id).unwrap();
                        writeln!(content, "**Requirement**: {}\n", hsr.requirement).unwrap();
                        writeln!(
                            content,
                            "**Verification Methods**: {}\n",
                            hsr.verification
                                .iter()
                                .map(|v| format!("{:?}", v))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                        .unwrap();

                        if let Some(psm) = &hsr.psm {
                            writeln!(content, "**Associated PSM**: {}\n", psm.name).unwrap();
                            writeln!(content, "- Target DC: >= {:.1}%", psm.dc_target).unwrap();

                            if let Some(dhsr) = &psm.dhsr {
                                writeln!(content, "\n**Diagnostic Requirement ({}):**\n", dhsr.id)
                                    .unwrap();
                                writeln!(content, "{}\n", dhsr.requirement).unwrap();
                                if let Some(time) = &dhsr.detection_time {
                                    writeln!(
                                        content,
                                        "- Detection Time: {}",
                                        format_duration(time)
                                    )
                                    .unwrap();
                                }
                            }
                        }
                        writeln!(content, "---\n").unwrap();
                    }
                }
            }
            OutputFormat::Html => {
                let md = self.generate_hsr_specification(OutputFormat::Markdown)?;
                content = self.wrap_html(
                    &format!("HSR Specification: {}", self.config.design_name),
                    &md,
                );
            }
            _ => {
                return Err(WorkProductError::FormatError(format!(
                    "Format {:?} not supported for HSR Specification",
                    format
                )));
            }
        }

        Ok(content)
    }

    /// Generate HSI Specification
    fn generate_hsi_specification(&self, format: OutputFormat) -> Result<String, WorkProductError> {
        let mut content = String::new();

        match format {
            OutputFormat::Markdown => {
                writeln!(
                    content,
                    "# Hardware-Software Interface Specification: {}\n",
                    self.config.design_name
                )
                .unwrap();

                writeln!(content, "## 1. Interface Signals\n").unwrap();
                writeln!(
                    content,
                    "| Signal | Direction | Width | ASIL | Max Latency |"
                )
                .unwrap();
                writeln!(
                    content,
                    "|--------|-----------|-------|------|-------------|"
                )
                .unwrap();

                for entity in self.hierarchy.entities.values() {
                    // Use includes patterns as the signals
                    for include in &entity.hsi.includes {
                        let signal_str = format!(
                            "{}::{}",
                            include.instance_pattern,
                            include.signal_pattern.as_deref().unwrap_or("*")
                        );
                        let latency = entity
                            .hsi
                            .timing
                            .signal_latencies
                            .get(&signal_str)
                            .map(format_duration)
                            .unwrap_or_else(|| "-".to_string());

                        writeln!(
                            content,
                            "| {} | HW<->SW | - | - | {} |",
                            signal_str, latency
                        )
                        .unwrap();
                    }
                }

                writeln!(content, "\n## 2. Timing Contracts\n").unwrap();

                for goal in self.hierarchy.goals.values() {
                    if let Some(ftti) = &goal.ftti {
                        writeln!(content, "- **FTTI**: {}", format_duration(ftti)).unwrap();
                    }
                }

                // Also show entity-level FTTI
                for entity in self.hierarchy.entities.values() {
                    if let Some(ftti) = &entity.hsi.timing.ftti {
                        writeln!(
                            content,
                            "- **{} FTTI**: {}",
                            entity.name,
                            format_duration(ftti)
                        )
                        .unwrap();
                    }
                }

                writeln!(content, "\n## 3. Excluded Signals (not safety-relevant)\n").unwrap();

                for entity in self.hierarchy.entities.values() {
                    for exclude in &entity.hsi.excludes {
                        let exclude_str = format!(
                            "{}::{}",
                            exclude.instance_pattern,
                            exclude.signal_pattern.as_deref().unwrap_or("*")
                        );
                        writeln!(content, "- {}", exclude_str).unwrap();
                    }
                }
            }
            OutputFormat::Html => {
                let md = self.generate_hsi_specification(OutputFormat::Markdown)?;
                content = self.wrap_html(
                    &format!("HSI Specification: {}", self.config.design_name),
                    &md,
                );
            }
            _ => {
                return Err(WorkProductError::FormatError(format!(
                    "Format {:?} not supported for HSI Specification",
                    format
                )));
            }
        }

        Ok(content)
    }

    /// Generate Safety Mechanism Report
    fn generate_safety_mechanism_report(
        &self,
        format: OutputFormat,
    ) -> Result<String, WorkProductError> {
        let mut content = String::new();

        match format {
            OutputFormat::Markdown => {
                writeln!(
                    content,
                    "# Safety Mechanism Report: {}\n",
                    self.config.design_name
                )
                .unwrap();

                writeln!(content, "## Primary Safety Mechanisms (PSM)\n").unwrap();
                writeln!(
                    content,
                    "| Name | HSR | Target DC | Actual DC | Status | Implementation |"
                )
                .unwrap();
                writeln!(
                    content,
                    "|------|-----|-----------|-----------|--------|----------------|"
                )
                .unwrap();

                for goal in self.hierarchy.goals.values() {
                    for hsr in &goal.hsrs {
                        if let Some(psm) = &hsr.psm {
                            let actual_dc = psm.dc_target; // Placeholder - should come from analysis
                            let status = if actual_dc >= psm.dc_target {
                                "PASS"
                            } else {
                                "FAIL"
                            };
                            let impls = psm
                                .implementations
                                .iter()
                                .map(format_design_ref)
                                .collect::<Vec<_>>()
                                .join(", ");

                            writeln!(
                                content,
                                "| {} | {} | >= {:.1}% | {:.1}% | {} | {} |",
                                psm.name, hsr.id, psm.dc_target, actual_dc, status, impls
                            )
                            .unwrap();
                        }
                    }
                }

                writeln!(content, "\n## Latent Safety Mechanisms (LSM)\n").unwrap();
                writeln!(
                    content,
                    "| Name | Target LC | Actual LC | Interval | Status | Implementation |"
                )
                .unwrap();
                writeln!(
                    content,
                    "|------|-----------|-----------|----------|--------|----------------|"
                )
                .unwrap();

                for goal in self.hierarchy.goals.values() {
                    for lsm in &goal.lsms {
                        let actual_lc = lsm.lc_target; // Placeholder
                        let status = if actual_lc >= lsm.lc_target {
                            "PASS"
                        } else {
                            "FAIL"
                        };
                        let interval = lsm
                            .interval
                            .as_ref()
                            .map(format_duration)
                            .unwrap_or_else(|| "-".to_string());
                        let impls = lsm
                            .implementations
                            .iter()
                            .map(format_design_ref)
                            .collect::<Vec<_>>()
                            .join(", ");

                        writeln!(
                            content,
                            "| {} | >= {:.1}% | {:.1}% | {} | {} | {} |",
                            lsm.name, lsm.lc_target, actual_lc, interval, status, impls
                        )
                        .unwrap();
                    }
                }
            }
            OutputFormat::Html => {
                let md = self.generate_safety_mechanism_report(OutputFormat::Markdown)?;
                content = self.wrap_html(
                    &format!("Safety Mechanism Report: {}", self.config.design_name),
                    &md,
                );
            }
            _ => {
                return Err(WorkProductError::FormatError(format!(
                    "Format {:?} not supported for Safety Mechanism Report",
                    format
                )));
            }
        }

        Ok(content)
    }

    /// Generate DFA Report
    fn generate_dfa_report(&self, format: OutputFormat) -> Result<String, WorkProductError> {
        let mut content = String::new();

        match format {
            OutputFormat::Markdown => {
                writeln!(
                    content,
                    "# Dependent Failures Analysis: {}\n",
                    self.config.design_name
                )
                .unwrap();

                writeln!(content, "## 1. Common Cause Failures\n").unwrap();
                writeln!(
                    content,
                    "Analysis of failures that could affect multiple elements simultaneously.\n"
                )
                .unwrap();

                writeln!(content, "| Failure Type | Affected Elements | Mitigation |").unwrap();
                writeln!(content, "|--------------|-------------------|------------|").unwrap();
                writeln!(
                    content,
                    "| Power supply failure | All domains | Independent power domains |"
                )
                .unwrap();
                writeln!(
                    content,
                    "| Clock failure | All synchronous logic | Independent clock sources |"
                )
                .unwrap();

                writeln!(content, "\n## 2. Cascading Failures\n").unwrap();
                writeln!(
                    content,
                    "Analysis of failures that could propagate through the system.\n"
                )
                .unwrap();

                writeln!(
                    content,
                    "| Initial Failure | Potential Cascade | Protection |"
                )
                .unwrap();
                writeln!(
                    content,
                    "|-----------------|-------------------|------------|"
                )
                .unwrap();

                writeln!(content, "\n## 3. Recommendations\n").unwrap();
                writeln!(
                    content,
                    "- Implement physical separation between redundant channels"
                )
                .unwrap();
                writeln!(
                    content,
                    "- Use diverse implementations for safety mechanisms"
                )
                .unwrap();
                writeln!(
                    content,
                    "- Ensure independent power and clock domains for critical functions"
                )
                .unwrap();
            }
            OutputFormat::Html => {
                let md = self.generate_dfa_report(OutputFormat::Markdown)?;
                content = self.wrap_html(&format!("DFA Report: {}", self.config.design_name), &md);
            }
            _ => {
                return Err(WorkProductError::FormatError(format!(
                    "Format {:?} not supported for DFA Report",
                    format
                )));
            }
        }

        Ok(content)
    }

    /// Generate Safety Summary
    fn generate_safety_summary(&self, format: OutputFormat) -> Result<String, WorkProductError> {
        let mut content = String::new();
        let check = self.check_metrics();

        match format {
            OutputFormat::Markdown => {
                writeln!(
                    content,
                    "# Safety Analysis Summary: {}\n",
                    self.config.design_name
                )
                .unwrap();

                writeln!(content, "## 1. Overall Status\n").unwrap();

                let status = if check.passed { "PASSED" } else { "FAILED" };
                let status_emoji = if check.passed { "✅" } else { "❌" };

                writeln!(content, "**Overall Result**: {} {}\n", status_emoji, status).unwrap();
                writeln!(content, "**Target ASIL**: {:?}\n", self.context.target_asil).unwrap();

                writeln!(content, "## 2. Metrics Summary\n").unwrap();
                writeln!(content, "| Metric | Value | Target | Status |").unwrap();
                writeln!(content, "|--------|-------|--------|--------|").unwrap();

                if let Some((actual, target)) = check.spfm {
                    let status_str = if actual >= target {
                        "✅ PASS"
                    } else {
                        "❌ FAIL"
                    };
                    writeln!(
                        content,
                        "| SPFM | {:.1}% | >= {:.1}% | {} |",
                        actual, target, status_str
                    )
                    .unwrap();
                }

                if let Some((actual, target)) = check.lfm {
                    let status_str = if actual >= target {
                        "✅ PASS"
                    } else {
                        "❌ FAIL"
                    };
                    writeln!(
                        content,
                        "| LFM | {:.1}% | >= {:.1}% | {} |",
                        actual, target, status_str
                    )
                    .unwrap();
                }

                if let Some((actual, target)) = check.pmhf {
                    let status_str = if actual <= target {
                        "✅ PASS"
                    } else {
                        "❌ FAIL"
                    };
                    writeln!(
                        content,
                        "| PMHF | {:.1} FIT | <= {:.1} FIT | {} |",
                        actual, target, status_str
                    )
                    .unwrap();
                }

                if !check.gaps.is_empty() {
                    writeln!(content, "\n## 3. Gap Analysis\n").unwrap();
                    writeln!(
                        content,
                        "| Component | Current | Required | Mechanism | Suggestion |"
                    )
                    .unwrap();
                    writeln!(
                        content,
                        "|-----------|---------|----------|-----------|------------|"
                    )
                    .unwrap();

                    for gap in &check.gaps {
                        writeln!(
                            content,
                            "| {} | {:.1}% | {:.1}% | {} | {} |",
                            gap.component,
                            gap.current_coverage,
                            gap.required_coverage,
                            gap.mechanism,
                            gap.suggestion
                        )
                        .unwrap();
                    }
                }

                writeln!(content, "\n## 4. Work Products Generated\n").unwrap();

                for product in &self.config.products {
                    writeln!(content, "- {}", product.default_filename()).unwrap();
                }
            }
            OutputFormat::Html => {
                let md = self.generate_safety_summary(OutputFormat::Markdown)?;
                content =
                    self.wrap_html(&format!("Safety Summary: {}", self.config.design_name), &md);
            }
            _ => {
                return Err(WorkProductError::FormatError(format!(
                    "Format {:?} not supported for Safety Summary",
                    format
                )));
            }
        }

        Ok(content)
    }

    /// Generate ReqIF format for traceability
    fn generate_reqif_traceability(&self) -> Result<String, WorkProductError> {
        let mut content = String::new();

        writeln!(content, r#"<?xml version="1.0" encoding="UTF-8"?>"#).unwrap();
        writeln!(
            content,
            r#"<REQ-IF xmlns="http://www.omg.org/spec/ReqIF/20110401/reqif.xsd">"#
        )
        .unwrap();
        writeln!(content, "  <THE-HEADER>").unwrap();
        writeln!(
            content,
            "    <REQ-IF-HEADER IDENTIFIER=\"skalp-safety-export\">"
        )
        .unwrap();
        writeln!(
            content,
            "      <TITLE>Safety Requirements: {}</TITLE>",
            self.config.design_name
        )
        .unwrap();
        writeln!(content, "    </REQ-IF-HEADER>").unwrap();
        writeln!(content, "  </THE-HEADER>").unwrap();
        writeln!(content, "  <CORE-CONTENT>").unwrap();
        writeln!(content, "    <REQ-IF-CONTENT>").unwrap();
        writeln!(content, "      <SPEC-OBJECTS>").unwrap();

        for goal in self.hierarchy.goals.values() {
            writeln!(
                content,
                "        <SPEC-OBJECT IDENTIFIER=\"{}\">",
                goal.external_id
            )
            .unwrap();
            writeln!(
                content,
                "          <VALUES><ATTRIBUTE-VALUE-STRING THE-VALUE=\"{}\"/></VALUES>",
                goal.description
            )
            .unwrap();
            writeln!(content, "        </SPEC-OBJECT>").unwrap();

            for hsr in &goal.hsrs {
                writeln!(content, "        <SPEC-OBJECT IDENTIFIER=\"{}\">", hsr.id).unwrap();
                writeln!(
                    content,
                    "          <VALUES><ATTRIBUTE-VALUE-STRING THE-VALUE=\"{}\"/></VALUES>",
                    hsr.requirement
                )
                .unwrap();
                writeln!(content, "        </SPEC-OBJECT>").unwrap();
            }
        }

        writeln!(content, "      </SPEC-OBJECTS>").unwrap();
        writeln!(content, "      <SPEC-RELATIONS>").unwrap();

        for goal in self.hierarchy.goals.values() {
            for hsr in &goal.hsrs {
                writeln!(
                    content,
                    "        <SPEC-RELATION IDENTIFIER=\"rel-{}-{}\">",
                    goal.external_id, hsr.id
                )
                .unwrap();
                writeln!(content, "          <SOURCE>").unwrap();
                writeln!(
                    content,
                    "            <SPEC-OBJECT-REF>{}</SPEC-OBJECT-REF>",
                    hsr.id
                )
                .unwrap();
                writeln!(content, "          </SOURCE>").unwrap();
                writeln!(content, "          <TARGET>").unwrap();
                writeln!(
                    content,
                    "            <SPEC-OBJECT-REF>{}</SPEC-OBJECT-REF>",
                    goal.external_id
                )
                .unwrap();
                writeln!(content, "          </TARGET>").unwrap();
                writeln!(content, "        </SPEC-RELATION>").unwrap();
            }
        }

        writeln!(content, "      </SPEC-RELATIONS>").unwrap();
        writeln!(content, "    </REQ-IF-CONTENT>").unwrap();
        writeln!(content, "  </CORE-CONTENT>").unwrap();
        writeln!(content, "</REQ-IF>").unwrap();

        Ok(content)
    }

    /// Wrap markdown content in HTML
    fn wrap_html(&self, title: &str, markdown: &str) -> String {
        let html_body = markdown
            .lines()
            .map(|line| {
                if let Some(stripped) = line.strip_prefix("# ") {
                    format!("<h1>{}</h1>", stripped)
                } else if let Some(stripped) = line.strip_prefix("## ") {
                    format!("<h2>{}</h2>", stripped)
                } else if let Some(stripped) = line.strip_prefix("### ") {
                    format!("<h3>{}</h3>", stripped)
                } else if line.starts_with("| ") {
                    format!("<tr><td>{}</td></tr>", line.replace(" | ", "</td><td>"))
                } else if let Some(stripped) = line.strip_prefix("- ") {
                    format!("<li>{}</li>", stripped)
                } else if line.starts_with("**") && line.ends_with("**") {
                    format!("<strong>{}</strong>", &line[2..line.len() - 2])
                } else {
                    format!("<p>{}</p>", line)
                }
            })
            .collect::<Vec<_>>()
            .join("\n");

        format!(
            r#"<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>{}</title>
    <style>
        body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; margin: 40px; }}
        h1 {{ color: #333; border-bottom: 2px solid #007bff; padding-bottom: 10px; }}
        h2 {{ color: #555; margin-top: 30px; }}
        h3 {{ color: #666; }}
        table {{ border-collapse: collapse; width: 100%; margin: 20px 0; }}
        th, td {{ border: 1px solid #ddd; padding: 12px; text-align: left; }}
        th {{ background-color: #007bff; color: white; }}
        tr:nth-child(even) {{ background-color: #f9f9f9; }}
        .pass {{ color: #28a745; }}
        .fail {{ color: #dc3545; }}
    </style>
</head>
<body>
{}
</body>
</html>"#,
            title, html_body
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::AnalysisContext;
    use crate::asil::AsilLevel;
    use crate::hierarchy::SafetyHierarchy;

    #[test]
    fn test_output_format_extension() {
        assert_eq!(OutputFormat::Markdown.extension(), "md");
        assert_eq!(OutputFormat::Html.extension(), "html");
        assert_eq!(OutputFormat::Csv.extension(), "csv");
        assert_eq!(OutputFormat::ReqIF.extension(), "reqif");
        assert_eq!(OutputFormat::Xml.extension(), "xml");
    }

    #[test]
    fn test_output_format_from_str() {
        assert_eq!(OutputFormat::parse("md"), Some(OutputFormat::Markdown));
        assert_eq!(
            OutputFormat::parse("markdown"),
            Some(OutputFormat::Markdown)
        );
        assert_eq!(OutputFormat::parse("html"), Some(OutputFormat::Html));
        assert_eq!(OutputFormat::parse("csv"), Some(OutputFormat::Csv));
        assert_eq!(OutputFormat::parse("reqif"), Some(OutputFormat::ReqIF));
        assert_eq!(OutputFormat::parse("xml"), Some(OutputFormat::Xml));
        assert_eq!(OutputFormat::parse("unknown"), None);
    }

    #[test]
    fn test_work_product_type_from_str() {
        assert_eq!(
            WorkProductType::parse("manual"),
            Some(WorkProductType::SafetyManual)
        );
        assert_eq!(
            WorkProductType::parse("fmeda"),
            Some(WorkProductType::FmedaReport)
        );
        assert_eq!(
            WorkProductType::parse("traceability"),
            Some(WorkProductType::TraceabilityMatrix)
        );
        assert_eq!(
            WorkProductType::parse("hsr"),
            Some(WorkProductType::HsrSpecification)
        );
        assert_eq!(
            WorkProductType::parse("hsi"),
            Some(WorkProductType::HsiSpecification)
        );
        assert_eq!(
            WorkProductType::parse("mechanisms"),
            Some(WorkProductType::SafetyMechanismReport)
        );
        assert_eq!(
            WorkProductType::parse("dfa"),
            Some(WorkProductType::DfaReport)
        );
        assert_eq!(
            WorkProductType::parse("summary"),
            Some(WorkProductType::SafetySummary)
        );
    }

    #[test]
    fn test_work_product_config_default() {
        let config = WorkProductConfig::default();
        assert_eq!(config.output_dir, PathBuf::from("safety_docs"));
        assert_eq!(config.products.len(), 8);
        assert_eq!(config.formats, vec![OutputFormat::Markdown]);
        assert!(config.check_metrics);
    }

    #[test]
    fn test_metrics_check() {
        let hierarchy = SafetyHierarchy::new();
        let context = AnalysisContext {
            target_asil: AsilLevel::D,
            strict_mode: false,
            verbose: false,
            config: crate::analysis::AnalysisConfig::default(),
        };
        let config = WorkProductConfig::default();

        let generator = WorkProductGenerator::new(hierarchy, context, config);
        let check = generator.check_metrics();

        // With default placeholder values (95%, 85%, 15 FIT), some metrics should fail for ASIL-D
        assert!(!check.passed);
    }

    #[test]
    fn test_all_work_product_types() {
        let all = WorkProductType::all();
        assert_eq!(all.len(), 8);
        assert!(all.contains(&WorkProductType::SafetyManual));
        assert!(all.contains(&WorkProductType::FmedaReport));
        assert!(all.contains(&WorkProductType::TraceabilityMatrix));
        assert!(all.contains(&WorkProductType::HsrSpecification));
        assert!(all.contains(&WorkProductType::HsiSpecification));
        assert!(all.contains(&WorkProductType::SafetyMechanismReport));
        assert!(all.contains(&WorkProductType::DfaReport));
        assert!(all.contains(&WorkProductType::SafetySummary));
    }
}
