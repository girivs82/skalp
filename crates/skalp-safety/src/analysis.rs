//! Safety Analysis Pass Trait and Common Types
//!
//! Defines the interface for safety analysis passes that validate
//! safety requirements, calculate metrics, and check compliance.

use crate::asil::AsilLevel;
use crate::design_resolver::DesignPathResolver;
use crate::fmeda_library::FmedaSummary;
use crate::hierarchy::{DesignRef, SafetyEntity, SafetyGoal, SafetyHierarchy};
use serde::{Deserialize, Serialize};
use std::fmt;

// ============================================================================
// Analysis Pass Trait
// ============================================================================

/// Trait for safety analysis passes
pub trait SafetyAnalysisPass {
    /// Name of this analysis pass
    fn name(&self) -> &'static str;

    /// Description of what this pass does
    fn description(&self) -> &'static str;

    /// Run the analysis pass
    fn run(
        &self,
        hierarchy: &SafetyHierarchy,
        resolver: Option<&DesignPathResolver>,
        context: &AnalysisContext,
    ) -> AnalysisResult;

    /// Check if this pass is required for the given ASIL level
    fn required_for_asil(&self, level: AsilLevel) -> bool {
        // By default, all passes are required for ASIL A and above
        level > AsilLevel::QM
    }
}

/// Context for analysis passes
#[derive(Debug, Clone)]
pub struct AnalysisContext {
    /// Target ASIL level
    pub target_asil: AsilLevel,
    /// Enable strict mode (fail on warnings)
    pub strict_mode: bool,
    /// Enable verbose output
    pub verbose: bool,
    /// Custom configuration
    pub config: AnalysisConfig,
}

impl Default for AnalysisContext {
    fn default() -> Self {
        Self {
            target_asil: AsilLevel::QM,
            strict_mode: false,
            verbose: false,
            config: AnalysisConfig::default(),
        }
    }
}

impl AnalysisContext {
    /// Create context for a specific ASIL level
    pub fn for_asil(level: AsilLevel) -> Self {
        Self {
            target_asil: level,
            ..Default::default()
        }
    }

    /// Enable strict mode
    pub fn strict(mut self) -> Self {
        self.strict_mode = true;
        self
    }
}

/// Analysis configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct AnalysisConfig {
    /// Skip specific checks
    pub skip_checks: Vec<String>,
    /// Override SPFM target
    pub spfm_target_override: Option<f64>,
    /// Override LFM target
    pub lfm_target_override: Option<f64>,
    /// Override PMHF target
    pub pmhf_target_override: Option<f64>,
}

// ============================================================================
// Analysis Results
// ============================================================================

/// Result of running an analysis pass
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnalysisResult {
    /// Pass name
    pub pass_name: String,
    /// Overall status
    pub status: AnalysisStatus,
    /// Errors found
    pub errors: Vec<AnalysisError>,
    /// Warnings found
    pub warnings: Vec<AnalysisWarning>,
    /// Notes/info
    pub notes: Vec<String>,
    /// Metrics calculated (if applicable)
    pub metrics: Option<AnalysisMetrics>,
}

impl AnalysisResult {
    /// Create a passing result
    pub fn pass(pass_name: &str) -> Self {
        Self {
            pass_name: pass_name.to_string(),
            status: AnalysisStatus::Pass,
            errors: Vec::new(),
            warnings: Vec::new(),
            notes: Vec::new(),
            metrics: None,
        }
    }

    /// Create a failing result
    pub fn fail(pass_name: &str, errors: Vec<AnalysisError>) -> Self {
        Self {
            pass_name: pass_name.to_string(),
            status: AnalysisStatus::Fail,
            errors,
            warnings: Vec::new(),
            notes: Vec::new(),
            metrics: None,
        }
    }

    /// Add an error
    pub fn with_error(mut self, error: AnalysisError) -> Self {
        self.errors.push(error);
        if self.status == AnalysisStatus::Pass {
            self.status = AnalysisStatus::Fail;
        }
        self
    }

    /// Add a warning
    pub fn with_warning(mut self, warning: AnalysisWarning) -> Self {
        self.warnings.push(warning);
        self
    }

    /// Add a note
    pub fn with_note(mut self, note: String) -> Self {
        self.notes.push(note);
        self
    }

    /// Set metrics
    pub fn with_metrics(mut self, metrics: AnalysisMetrics) -> Self {
        self.metrics = Some(metrics);
        self
    }

    /// Check if result is passing
    pub fn is_pass(&self) -> bool {
        self.status == AnalysisStatus::Pass
    }

    /// Check if result is failing
    pub fn is_fail(&self) -> bool {
        self.status == AnalysisStatus::Fail
    }

    /// Get error count
    pub fn error_count(&self) -> usize {
        self.errors.len()
    }

    /// Get warning count
    pub fn warning_count(&self) -> usize {
        self.warnings.len()
    }
}

/// Analysis status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum AnalysisStatus {
    /// All checks passed
    Pass,
    /// Some checks failed
    Fail,
    /// Pass with warnings
    PassWithWarnings,
    /// Analysis was skipped
    Skipped,
}

/// Analysis error
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnalysisError {
    /// Error code (e.g., "E0501")
    pub code: String,
    /// Error message
    pub message: String,
    /// Location (safety file, line, etc.)
    pub location: Option<String>,
    /// Related design reference
    pub design_ref: Option<DesignRef>,
    /// Suggested fix
    pub suggestion: Option<String>,
}

impl AnalysisError {
    /// Create a new error
    pub fn new(code: &str, message: &str) -> Self {
        Self {
            code: code.to_string(),
            message: message.to_string(),
            location: None,
            design_ref: None,
            suggestion: None,
        }
    }

    /// Set location
    pub fn at(mut self, location: &str) -> Self {
        self.location = Some(location.to_string());
        self
    }

    /// Set design reference
    pub fn for_ref(mut self, design_ref: DesignRef) -> Self {
        self.design_ref = Some(design_ref);
        self
    }

    /// Add suggestion
    pub fn suggest(mut self, suggestion: &str) -> Self {
        self.suggestion = Some(suggestion.to_string());
        self
    }
}

impl fmt::Display for AnalysisError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error[{}]: {}", self.code, self.message)?;
        if let Some(ref loc) = self.location {
            write!(f, "\n  --> {}", loc)?;
        }
        if let Some(ref sug) = self.suggestion {
            write!(f, "\n  = help: {}", sug)?;
        }
        Ok(())
    }
}

/// Analysis warning
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnalysisWarning {
    /// Warning code (e.g., "W0101")
    pub code: String,
    /// Warning message
    pub message: String,
    /// Location
    pub location: Option<String>,
    /// Related design reference
    pub design_ref: Option<DesignRef>,
}

impl AnalysisWarning {
    /// Create a new warning
    pub fn new(code: &str, message: &str) -> Self {
        Self {
            code: code.to_string(),
            message: message.to_string(),
            location: None,
            design_ref: None,
        }
    }

    /// Set location
    pub fn at(mut self, location: &str) -> Self {
        self.location = Some(location.to_string());
        self
    }
}

impl fmt::Display for AnalysisWarning {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "warning[{}]: {}", self.code, self.message)?;
        if let Some(ref loc) = self.location {
            write!(f, "\n  --> {}", loc)?;
        }
        Ok(())
    }
}

/// Analysis metrics (FMEDA results)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnalysisMetrics {
    /// SPFM (Single Point Fault Metric)
    pub spfm: f64,
    /// LFM (Latent Fault Metric)
    pub lfm: f64,
    /// PMHF (Probabilistic Metric for Hardware Failures)
    pub pmhf: f64,
    /// Total FIT
    pub total_fit: f64,
    /// Meets target ASIL
    pub meets_target: bool,
    /// Achievable ASIL based on metrics
    pub achievable_asil: AsilLevel,
}

impl From<FmedaSummary> for AnalysisMetrics {
    fn from(summary: FmedaSummary) -> Self {
        Self {
            spfm: summary.spfm,
            lfm: summary.lfm,
            pmhf: summary.pmhf,
            total_fit: summary.total_fit,
            meets_target: false, // Set by caller
            achievable_asil: summary.achievable_asil,
        }
    }
}

// ============================================================================
// Error Codes
// ============================================================================

/// Standard error codes for safety analysis
pub mod error_codes {
    /// ASIL decomposition violation
    pub const E0501_ASIL_DECOMPOSITION: &str = "E0501";
    /// SPFM below target
    pub const E0502_SPFM_BELOW_TARGET: &str = "E0502";
    /// LFM below target
    pub const E0503_LFM_BELOW_TARGET: &str = "E0503";
    /// Unresolved design path
    pub const E0504_UNRESOLVED_PATH: &str = "E0504";
    /// Undefined safety entity
    pub const E0505_UNDEFINED_ENTITY: &str = "E0505";
    /// Missing implementation
    pub const E0506_MISSING_IMPLEMENTATION: &str = "E0506";
    /// HSI violation
    pub const E0507_HSI_VIOLATION: &str = "E0507";
    /// DHSR not satisfied
    pub const E0508_DHSR_NOT_SATISFIED: &str = "E0508";
    /// Conflicting definitions
    pub const E0509_CONFLICT: &str = "E0509";
    /// Conflicting PSM definitions (traits)
    pub const E0510_PSM_CONFLICT: &str = "E0510";

    /// Warning: Uncovered instance
    pub const W0101_UNCOVERED_INSTANCE: &str = "W0101";
    /// Warning: Low diagnostic coverage
    pub const W0102_LOW_DC: &str = "W0102";
    /// Warning: Missing verification
    pub const W0103_MISSING_VERIFICATION: &str = "W0103";
}

// ============================================================================
// Combined Analysis Results
// ============================================================================

/// Combined results from all analysis passes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CombinedAnalysisResult {
    /// Target ASIL
    pub target_asil: AsilLevel,
    /// Individual pass results
    pub pass_results: Vec<AnalysisResult>,
    /// Overall status
    pub overall_status: AnalysisStatus,
    /// Total errors
    pub total_errors: usize,
    /// Total warnings
    pub total_warnings: usize,
    /// Final metrics (if available)
    pub final_metrics: Option<AnalysisMetrics>,
    /// Work products can be generated
    pub can_generate_workproducts: bool,
}

impl CombinedAnalysisResult {
    /// Create from pass results
    pub fn from_results(target_asil: AsilLevel, results: Vec<AnalysisResult>) -> Self {
        let total_errors: usize = results.iter().map(|r| r.error_count()).sum();
        let total_warnings: usize = results.iter().map(|r| r.warning_count()).sum();

        let overall_status = if total_errors > 0 {
            AnalysisStatus::Fail
        } else if total_warnings > 0 {
            AnalysisStatus::PassWithWarnings
        } else {
            AnalysisStatus::Pass
        };

        // Get final metrics from the last result that has them
        let final_metrics = results.iter().rev().find_map(|r| r.metrics.clone());

        let can_generate_workproducts = overall_status == AnalysisStatus::Pass
            || overall_status == AnalysisStatus::PassWithWarnings;

        Self {
            target_asil,
            pass_results: results,
            overall_status,
            total_errors,
            total_warnings,
            final_metrics,
            can_generate_workproducts,
        }
    }

    /// Check if analysis passed
    pub fn passed(&self) -> bool {
        self.overall_status == AnalysisStatus::Pass
            || self.overall_status == AnalysisStatus::PassWithWarnings
    }

    /// Get all errors
    pub fn all_errors(&self) -> Vec<&AnalysisError> {
        self.pass_results.iter().flat_map(|r| &r.errors).collect()
    }

    /// Get all warnings
    pub fn all_warnings(&self) -> Vec<&AnalysisWarning> {
        self.pass_results.iter().flat_map(|r| &r.warnings).collect()
    }

    /// Generate summary report
    pub fn summary(&self) -> String {
        let mut output = String::new();

        output.push_str("=== Safety Analysis Summary ===\n");
        output.push_str(&format!("Target ASIL: {:?}\n", self.target_asil));
        output.push_str(&format!("Status: {:?}\n", self.overall_status));
        output.push_str(&format!("Errors: {}\n", self.total_errors));
        output.push_str(&format!("Warnings: {}\n", self.total_warnings));

        if let Some(ref metrics) = self.final_metrics {
            output.push_str("\nMetrics:\n");
            output.push_str(&format!("  SPFM: {:.1}%\n", metrics.spfm));
            output.push_str(&format!("  LFM: {:.1}%\n", metrics.lfm));
            output.push_str(&format!("  PMHF: {:.1} FIT\n", metrics.pmhf));
            output.push_str(&format!(
                "  Achievable ASIL: {:?}\n",
                metrics.achievable_asil
            ));
        }

        if self.can_generate_workproducts {
            output.push_str("\n✓ Work products can be generated\n");
        } else {
            output.push_str("\n✗ Work products NOT available - fix errors first\n");
        }

        output
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_analysis_result_creation() {
        let result = AnalysisResult::pass("test_pass")
            .with_warning(AnalysisWarning::new("W0001", "Test warning"))
            .with_note("Test note".to_string());

        assert!(result.is_pass());
        assert_eq!(result.warning_count(), 1);
        assert_eq!(result.error_count(), 0);
    }

    #[test]
    fn test_analysis_result_failure() {
        let result =
            AnalysisResult::pass("test_pass").with_error(AnalysisError::new("E0001", "Test error"));

        assert!(result.is_fail());
        assert_eq!(result.error_count(), 1);
    }

    #[test]
    fn test_error_display() {
        let error = AnalysisError::new("E0501", "ASIL decomposition violation")
            .at("safety.sk:15:1")
            .suggest("use ASIL_B + ASIL_B");

        let display = format!("{}", error);
        assert!(display.contains("E0501"));
        assert!(display.contains("ASIL decomposition"));
        assert!(display.contains("safety.sk:15:1"));
        assert!(display.contains("ASIL_B + ASIL_B"));
    }

    #[test]
    fn test_combined_result() {
        let results = vec![
            AnalysisResult::pass("pass1"),
            AnalysisResult::pass("pass2").with_warning(AnalysisWarning::new("W0001", "Warning")),
        ];

        let combined = CombinedAnalysisResult::from_results(AsilLevel::D, results);

        assert!(combined.passed());
        assert_eq!(combined.total_errors, 0);
        assert_eq!(combined.total_warnings, 1);
        assert_eq!(combined.overall_status, AnalysisStatus::PassWithWarnings);
    }

    #[test]
    fn test_combined_result_failure() {
        let results = vec![
            AnalysisResult::pass("pass1"),
            AnalysisResult::fail("pass2", vec![AnalysisError::new("E0001", "Error")]),
        ];

        let combined = CombinedAnalysisResult::from_results(AsilLevel::D, results);

        assert!(!combined.passed());
        assert!(!combined.can_generate_workproducts);
    }

    #[test]
    fn test_analysis_context() {
        let context = AnalysisContext::for_asil(AsilLevel::D).strict();

        assert_eq!(context.target_asil, AsilLevel::D);
        assert!(context.strict_mode);
    }
}
