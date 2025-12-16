//! Tool Qualification Module for ISO 26262 Compliance
//!
//! Implements tool qualification requirements per ISO 26262-8:11.
//! This module provides structures and functions to assess tool confidence level,
//! manage validation test suites, and generate tool qualification reports.
//!
//! # ISO 26262 Context
//!
//! Per ISO 26262-8:11, software tools used in safety-related development must be
//! qualified based on:
//! - Tool Impact (TI): Can the tool introduce errors or fail to detect them?
//! - Tool Error Detection (TD): How likely are tool errors to be detected?
//! - Tool Confidence Level (TCL): Combined assessment determining qualification needs
//!
//! # Tool Confidence Levels
//!
//! | TI | TD | TCL |
//! |----|----|----|
//! | TI1 | - | TCL1 |
//! | TI2 | TD1 | TCL1 |
//! | TI2 | TD2 | TCL2 |
//! | TI2 | TD3 | TCL3 |

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ============================================================================
// Tool Confidence Level Assessment
// ============================================================================

/// Complete tool confidence level assessment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TclAssessment {
    /// Name of the tool being assessed
    pub tool_name: String,
    /// Version of the tool
    pub tool_version: String,
    /// Components/modules of the tool
    pub tool_components: Vec<ToolComponent>,
    /// Use cases for the tool
    pub use_cases: Vec<ToolUseCase>,
    /// Assessed tool confidence level
    pub assessed_tcl: ToolConfidenceLevel,
    /// Required qualification method based on TCL
    pub required_qualification: QualificationMethod,
    /// Date of assessment
    pub assessment_date: Option<String>,
    /// Assessor information
    pub assessor: Option<String>,
}

impl TclAssessment {
    /// Create a new TCL assessment for a tool
    pub fn new(tool_name: &str, tool_version: &str) -> Self {
        Self {
            tool_name: tool_name.to_string(),
            tool_version: tool_version.to_string(),
            tool_components: Vec::new(),
            use_cases: Vec::new(),
            assessed_tcl: ToolConfidenceLevel::Tcl1,
            required_qualification: QualificationMethod::ValidationByToolDeveloper {
                test_suite_required: true,
            },
            assessment_date: None,
            assessor: None,
        }
    }

    /// Add a tool component
    pub fn add_component(&mut self, component: ToolComponent) {
        self.tool_components.push(component);
    }

    /// Add a use case
    pub fn add_use_case(&mut self, use_case: ToolUseCase) {
        self.use_cases.push(use_case);
    }

    /// Calculate TCL based on components and use cases
    pub fn calculate_tcl(&mut self) {
        // Find worst-case TCL across all components
        let mut worst_tcl = ToolConfidenceLevel::Tcl3;

        for component in &self.tool_components {
            let component_tcl =
                Self::derive_tcl(&component.tool_impact, &component.error_detection);
            if component_tcl < worst_tcl {
                worst_tcl = component_tcl;
            }
        }

        self.assessed_tcl = worst_tcl;
        self.required_qualification = Self::determine_qualification(&self.assessed_tcl);
    }

    /// Derive TCL from TI and TD per ISO 26262-8:11.4.5
    fn derive_tcl(ti: &ToolImpact, td: &ToolErrorDetection) -> ToolConfidenceLevel {
        match (ti, td) {
            (ToolImpact::Ti1, _) => ToolConfidenceLevel::Tcl1,
            (ToolImpact::Ti2, ToolErrorDetection::Td1) => ToolConfidenceLevel::Tcl1,
            (ToolImpact::Ti2, ToolErrorDetection::Td2) => ToolConfidenceLevel::Tcl2,
            (ToolImpact::Ti2, ToolErrorDetection::Td3) => ToolConfidenceLevel::Tcl3,
        }
    }

    /// Determine qualification method from TCL
    fn determine_qualification(tcl: &ToolConfidenceLevel) -> QualificationMethod {
        match tcl {
            ToolConfidenceLevel::Tcl1 => QualificationMethod::NoQualificationRequired,
            ToolConfidenceLevel::Tcl2 => {
                QualificationMethod::IncreasedConfidenceFromUse { required_uses: 3 }
            }
            ToolConfidenceLevel::Tcl3 => QualificationMethod::ValidationByToolDeveloper {
                test_suite_required: true,
            },
        }
    }
}

/// A component/module of the tool
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolComponent {
    /// Component name (e.g., "skalp-safety", "skalp-mir")
    pub name: String,
    /// Function description (e.g., "FMEA Generation", "Code Synthesis")
    pub function: String,
    /// Tool impact classification
    pub tool_impact: ToolImpact,
    /// Error detection capability
    pub error_detection: ToolErrorDetection,
    /// Detailed error impact description
    pub error_impact: ErrorImpact,
    /// Output criticality
    pub output_criticality: OutputCriticality,
    /// ISO 26262 reference for classification rationale
    pub iso_reference: Option<String>,
}

impl ToolComponent {
    /// Create a new tool component
    pub fn new(name: &str, function: &str) -> Self {
        Self {
            name: name.to_string(),
            function: function.to_string(),
            tool_impact: ToolImpact::Ti2,
            error_detection: ToolErrorDetection::Td2,
            error_impact: ErrorImpact::MayFailToDetectError,
            output_criticality: OutputCriticality::SafetyRelevant,
            iso_reference: None,
        }
    }

    /// Set tool impact
    pub fn with_tool_impact(mut self, ti: ToolImpact) -> Self {
        self.tool_impact = ti;
        self
    }

    /// Set error detection
    pub fn with_error_detection(mut self, td: ToolErrorDetection) -> Self {
        self.error_detection = td;
        self
    }
}

/// Tool use case
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolUseCase {
    /// Use case identifier
    pub id: String,
    /// Description of the use case
    pub description: String,
    /// ASIL level for this use case
    pub asil: String,
    /// Components involved
    pub components: Vec<String>,
    /// Expected inputs
    pub inputs: Vec<String>,
    /// Expected outputs
    pub outputs: Vec<String>,
}

/// Tool Impact classification per ISO 26262-8:11.4.4
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum ToolImpact {
    /// TI1: No possibility of malfunction that affects safety
    Ti1,
    /// TI2: Tool can introduce or fail to detect errors
    Ti2,
}

/// Tool Error Detection per ISO 26262-8:11.4.4
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum ToolErrorDetection {
    /// TD1: High degree of confidence - high coverage automated detection
    Td1,
    /// TD2: Medium degree of confidence
    Td2,
    /// TD3: Low degree of confidence - errors unlikely to be detected
    Td3,
}

/// Tool Confidence Level per ISO 26262-8:11.4.5
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum ToolConfidenceLevel {
    /// TCL1: Lowest confidence - highest qualification requirements
    Tcl1,
    /// TCL2: Medium confidence
    Tcl2,
    /// TCL3: High confidence - minimal qualification requirements
    Tcl3,
}

/// How tool errors affect safety
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ErrorImpact {
    /// Tool error could directly introduce safety-relevant errors
    MayIntroduceError {
        /// Description of potential errors
        error_types: Vec<String>,
    },
    /// Tool might not detect an error, but won't create one
    MayFailToDetectError,
    /// Tool error has no safety impact
    NoSafetyImpact,
}

/// Criticality of tool output
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum OutputCriticality {
    /// Output directly affects safety-related elements
    SafetyRelevant,
    /// Output used in safety analysis but not direct implementation
    SafetyAnalysis,
    /// Output has no safety relevance
    NonSafetyRelevant,
}

/// Qualification method required
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum QualificationMethod {
    /// No qualification needed (TCL1 with TI1)
    NoQualificationRequired,
    /// Increased confidence from use (TCL2)
    IncreasedConfidenceFromUse {
        /// Number of successful uses required
        required_uses: u32,
    },
    /// Validation of software tool (TCL3)
    ValidationByToolDeveloper {
        /// Full test suite required
        test_suite_required: bool,
    },
    /// Development process assessment
    DevelopmentProcessAssessment {
        /// Required process standards
        required_standards: Vec<String>,
    },
}

// ============================================================================
// Validation Test Suite
// ============================================================================

/// Complete validation test suite for a tool component
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationTestSuite {
    /// Tool component being validated
    pub tool_component: String,
    /// Suite version
    pub version: String,
    /// Test cases in the suite
    pub test_cases: Vec<ValidationTestCase>,
    /// Coverage requirements and achieved
    pub coverage: TestCoverage,
}

impl ValidationTestSuite {
    /// Create a new test suite
    pub fn new(component: &str, version: &str) -> Self {
        Self {
            tool_component: component.to_string(),
            version: version.to_string(),
            test_cases: Vec::new(),
            coverage: TestCoverage::default(),
        }
    }

    /// Add a test case
    pub fn add_test_case(&mut self, test_case: ValidationTestCase) {
        self.test_cases.push(test_case);
    }

    /// Get test cases by type
    pub fn get_by_type(&self, test_type: TestType) -> Vec<&ValidationTestCase> {
        self.test_cases
            .iter()
            .filter(|tc| tc.test_type == test_type)
            .collect()
    }
}

/// Individual validation test case
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationTestCase {
    /// Test case identifier
    pub id: String,
    /// Description of what is being tested
    pub description: String,
    /// Type of test
    pub test_type: TestType,
    /// Test input specification
    pub input: TestInput,
    /// Expected output/result
    pub expected: ExpectedOutput,
    /// ISO 26262 reference if applicable
    pub iso_reference: Option<String>,
    /// Traceability to requirements
    pub requirements: Vec<String>,
}

impl ValidationTestCase {
    /// Create a new test case
    pub fn new(id: &str, description: &str, test_type: TestType) -> Self {
        Self {
            id: id.to_string(),
            description: description.to_string(),
            test_type,
            input: TestInput::default(),
            expected: ExpectedOutput::default(),
            iso_reference: None,
            requirements: Vec::new(),
        }
    }
}

/// Type of validation test
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TestType {
    /// Valid input produces correct output
    PositiveTest,
    /// Invalid input produces appropriate error
    NegativeTest,
    /// Edge case/boundary condition testing
    BoundaryTest,
    /// Stress/malformed input handling
    RobustnessTest,
    /// Performance requirement verification
    PerformanceTest,
}

/// Test input specification
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TestInput {
    /// Input file or data description
    pub description: String,
    /// Input file paths
    pub files: Vec<String>,
    /// Input parameters
    pub parameters: HashMap<String, String>,
    /// Preconditions
    pub preconditions: Vec<String>,
}

/// Expected output specification
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ExpectedOutput {
    /// Expected result description
    pub description: String,
    /// Expected output files
    pub files: Vec<String>,
    /// Expected values/results
    pub values: HashMap<String, String>,
    /// Acceptance criteria
    pub acceptance_criteria: Vec<String>,
}

/// Test coverage information
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TestCoverage {
    /// Required coverage percentage
    pub required_percentage: f64,
    /// Achieved coverage percentage
    pub achieved_percentage: f64,
    /// Coverage by test type
    pub by_type: HashMap<String, CoverageEntry>,
    /// Uncovered areas
    pub uncovered: Vec<String>,
}

/// Coverage entry for a category
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CoverageEntry {
    /// Number of test cases
    pub count: usize,
    /// Required count
    pub required: usize,
    /// Coverage percentage
    pub percentage: f64,
}

// ============================================================================
// Validation Report
// ============================================================================

/// Result of running validation tests
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationReport {
    /// Tool component validated
    pub component: String,
    /// Test suite version
    pub suite_version: String,
    /// Number of passed tests
    pub passed: u32,
    /// Number of failed tests
    pub failed: u32,
    /// Number of skipped tests
    pub skipped: u32,
    /// Coverage achieved
    pub coverage_achieved: f64,
    /// Detailed results for each test
    pub detailed_results: Vec<TestResult>,
    /// Overall status
    pub status: ValidationStatus,
    /// Report generation timestamp
    pub timestamp: Option<String>,
}

impl ValidationReport {
    /// Create a new validation report
    pub fn new(component: &str, suite_version: &str) -> Self {
        Self {
            component: component.to_string(),
            suite_version: suite_version.to_string(),
            passed: 0,
            failed: 0,
            skipped: 0,
            coverage_achieved: 0.0,
            detailed_results: Vec::new(),
            status: ValidationStatus::NotRun,
            timestamp: None,
        }
    }

    /// Add a test result
    pub fn add_result(&mut self, result: TestResult) {
        match result.status {
            TestStatus::Passed => self.passed += 1,
            TestStatus::Failed => self.failed += 1,
            TestStatus::Skipped => self.skipped += 1,
            TestStatus::Error => self.failed += 1,
        }
        self.detailed_results.push(result);
        self.update_status();
    }

    /// Update overall status based on results
    fn update_status(&mut self) {
        if self.failed > 0 {
            self.status = ValidationStatus::Failed;
        } else if self.skipped > 0 {
            self.status = ValidationStatus::PassedWithWarnings;
        } else if self.passed > 0 {
            self.status = ValidationStatus::Passed;
        }
    }

    /// Calculate pass rate
    pub fn pass_rate(&self) -> f64 {
        let total = self.passed + self.failed;
        if total == 0 {
            0.0
        } else {
            self.passed as f64 / total as f64
        }
    }
}

/// Individual test result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestResult {
    /// Test case ID
    pub test_id: String,
    /// Test status
    pub status: TestStatus,
    /// Actual output/result
    pub actual_output: String,
    /// Expected vs actual comparison
    pub comparison: Option<String>,
    /// Error message if failed
    pub error_message: Option<String>,
    /// Execution time in milliseconds
    pub execution_time_ms: Option<u64>,
}

/// Status of an individual test
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TestStatus {
    /// Test passed
    Passed,
    /// Test failed
    Failed,
    /// Test skipped
    Skipped,
    /// Test encountered an error
    Error,
}

/// Overall validation status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ValidationStatus {
    /// Validation not yet run
    NotRun,
    /// All tests passed
    Passed,
    /// Passed but with warnings (skipped tests)
    PassedWithWarnings,
    /// One or more tests failed
    Failed,
}

// ============================================================================
// Known Limitations
// ============================================================================

/// Known limitations documentation
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct KnownLimitations {
    /// List of known limitations
    pub limitations: Vec<Limitation>,
    /// Workarounds for limitations
    pub workarounds: Vec<Workaround>,
    /// User guidance items
    pub user_guidance: Vec<GuidanceItem>,
}

impl KnownLimitations {
    /// Create new empty limitations
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a limitation
    pub fn add_limitation(&mut self, limitation: Limitation) {
        self.limitations.push(limitation);
    }

    /// Add a workaround
    pub fn add_workaround(&mut self, workaround: Workaround) {
        self.workarounds.push(workaround);
    }

    /// Get workarounds for a specific limitation
    pub fn workarounds_for(&self, limitation_id: &str) -> Vec<&Workaround> {
        self.workarounds
            .iter()
            .filter(|w| w.limitation_id == limitation_id)
            .collect()
    }
}

/// A known limitation of the tool
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Limitation {
    /// Limitation identifier
    pub id: String,
    /// Component affected
    pub component: String,
    /// Description of the limitation
    pub description: String,
    /// Safety impact description
    pub impact: String,
    /// Severity of the limitation
    pub severity: LimitationSeverity,
    /// ISO 26262 reference if applicable
    pub iso_reference: Option<String>,
    /// Status of limitation
    pub status: LimitationStatus,
}

impl Limitation {
    /// Create a new limitation
    pub fn new(id: &str, component: &str, description: &str) -> Self {
        Self {
            id: id.to_string(),
            component: component.to_string(),
            description: description.to_string(),
            impact: String::new(),
            severity: LimitationSeverity::Medium,
            iso_reference: None,
            status: LimitationStatus::Open,
        }
    }
}

/// Severity of a limitation
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum LimitationSeverity {
    /// Low severity - minimal impact
    Low,
    /// Medium severity - some impact, workaround available
    Medium,
    /// High severity - significant impact
    High,
    /// Critical - must be addressed
    Critical,
}

/// Status of a limitation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum LimitationStatus {
    /// Limitation is open/known
    Open,
    /// Workaround is available
    WorkaroundAvailable,
    /// Being addressed in development
    InProgress,
    /// Limitation has been resolved
    Resolved,
    /// Will not be fixed
    WontFix,
}

/// Workaround for a limitation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Workaround {
    /// Limitation ID this workaround addresses
    pub limitation_id: String,
    /// Description of the workaround
    pub description: String,
    /// Manual steps required
    pub manual_steps: Vec<String>,
    /// Effectiveness of workaround (0.0 - 1.0)
    pub effectiveness: f64,
    /// Additional effort required
    pub effort_description: Option<String>,
}

impl Workaround {
    /// Create a new workaround
    pub fn new(limitation_id: &str, description: &str) -> Self {
        Self {
            limitation_id: limitation_id.to_string(),
            description: description.to_string(),
            manual_steps: Vec::new(),
            effectiveness: 1.0,
            effort_description: None,
        }
    }

    /// Add a manual step
    pub fn add_step(&mut self, step: &str) {
        self.manual_steps.push(step.to_string());
    }
}

/// User guidance item
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GuidanceItem {
    /// Guidance identifier
    pub id: String,
    /// Title
    pub title: String,
    /// Guidance description
    pub description: String,
    /// Related component
    pub component: Option<String>,
    /// Category of guidance
    pub category: GuidanceCategory,
}

/// Category of guidance
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum GuidanceCategory {
    /// Usage best practices
    BestPractice,
    /// Safety-related guidance
    SafetyGuidance,
    /// Configuration recommendation
    Configuration,
    /// Interpretation of results
    ResultInterpretation,
    /// Warning about potential issues
    Warning,
}

// ============================================================================
// Tool Qualification Report
// ============================================================================

/// Complete tool qualification report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolQualificationReport {
    /// Report metadata
    pub metadata: ReportMetadata,
    /// TCL assessment
    pub tcl_assessment: TclAssessment,
    /// Validation results
    pub validation: ValidationReport,
    /// Known limitations
    pub limitations: KnownLimitations,
    /// Qualification conclusion
    pub conclusion: QualificationConclusion,
    /// Appendices
    pub appendices: Vec<Appendix>,
}

impl ToolQualificationReport {
    /// Generate a complete tool qualification report
    pub fn generate(
        assessment: TclAssessment,
        validation: ValidationReport,
        limitations: KnownLimitations,
    ) -> Self {
        let conclusion = Self::derive_conclusion(&assessment, &validation, &limitations);

        Self {
            metadata: ReportMetadata::new(&assessment.tool_name, &assessment.tool_version),
            tcl_assessment: assessment,
            validation,
            limitations,
            conclusion,
            appendices: Vec::new(),
        }
    }

    /// Derive qualification conclusion from evidence
    fn derive_conclusion(
        assessment: &TclAssessment,
        validation: &ValidationReport,
        limitations: &KnownLimitations,
    ) -> QualificationConclusion {
        let mut conclusion = QualificationConclusion {
            qualified: false,
            qualification_level: assessment.assessed_tcl,
            conditions: Vec::new(),
            restrictions: Vec::new(),
            valid_until: None,
            summary: String::new(),
        };

        // Check validation status
        let validation_passed = matches!(
            validation.status,
            ValidationStatus::Passed | ValidationStatus::PassedWithWarnings
        );

        // Check for critical limitations
        let has_critical_limitations = limitations.limitations.iter().any(|l| {
            l.severity == LimitationSeverity::Critical && l.status == LimitationStatus::Open
        });

        // Determine qualification
        conclusion.qualified = validation_passed && !has_critical_limitations;

        // Add conditions based on TCL
        match assessment.assessed_tcl {
            ToolConfidenceLevel::Tcl1 => {
                conclusion.summary =
                    "Tool qualified at TCL1 - minimal qualification requirements".to_string();
            }
            ToolConfidenceLevel::Tcl2 => {
                conclusion
                    .conditions
                    .push("Increased confidence from use: maintain usage records".to_string());
                conclusion.summary =
                    "Tool qualified at TCL2 - increased confidence from use required".to_string();
            }
            ToolConfidenceLevel::Tcl3 => {
                if validation_passed {
                    conclusion.summary =
                        "Tool qualified at TCL3 - validation by tool developer complete"
                            .to_string();
                } else {
                    conclusion.qualified = false;
                    conclusion.summary = format!(
                        "Tool NOT qualified at TCL3 - validation failed ({} of {} tests passed)",
                        validation.passed,
                        validation.passed + validation.failed
                    );
                }
            }
        }

        // Add restrictions based on limitations
        for limitation in &limitations.limitations {
            if limitation.severity >= LimitationSeverity::High
                && limitation.status != LimitationStatus::Resolved
            {
                conclusion.restrictions.push(format!(
                    "Limitation {}: {} ({})",
                    limitation.id, limitation.description, limitation.impact
                ));
            }
        }

        conclusion
    }

    /// Add an appendix
    pub fn add_appendix(&mut self, appendix: Appendix) {
        self.appendices.push(appendix);
    }
}

/// Report metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReportMetadata {
    /// Report title
    pub title: String,
    /// Tool name
    pub tool_name: String,
    /// Tool version
    pub tool_version: String,
    /// Report version
    pub report_version: String,
    /// Generation date
    pub date: String,
    /// Author
    pub author: Option<String>,
    /// Reviewer
    pub reviewer: Option<String>,
    /// Approver
    pub approver: Option<String>,
}

impl ReportMetadata {
    /// Create new metadata
    pub fn new(tool_name: &str, tool_version: &str) -> Self {
        Self {
            title: format!("Tool Qualification Report - {}", tool_name),
            tool_name: tool_name.to_string(),
            tool_version: tool_version.to_string(),
            report_version: "1.0".to_string(),
            date: chrono_lite_now(),
            author: None,
            reviewer: None,
            approver: None,
        }
    }
}

/// Get current date in ISO format (simple implementation without chrono)
fn chrono_lite_now() -> String {
    // Placeholder - in real implementation would use chrono or similar
    "2024-01-01".to_string()
}

/// Qualification conclusion
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualificationConclusion {
    /// Is the tool qualified?
    pub qualified: bool,
    /// Qualification level achieved
    pub qualification_level: ToolConfidenceLevel,
    /// Conditions for qualification
    pub conditions: Vec<String>,
    /// Usage restrictions
    pub restrictions: Vec<String>,
    /// Qualification valid until (if applicable)
    pub valid_until: Option<String>,
    /// Summary statement
    pub summary: String,
}

/// Report appendix
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Appendix {
    /// Appendix identifier
    pub id: String,
    /// Title
    pub title: String,
    /// Content type
    pub content_type: AppendixType,
    /// Content
    pub content: String,
}

/// Type of appendix content
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AppendixType {
    /// Test case details
    TestDetails,
    /// Configuration used
    Configuration,
    /// Raw validation output
    ValidationOutput,
    /// Supporting documentation reference
    DocumentReference,
    /// Change history
    ChangeHistory,
}

// ============================================================================
// Pre-defined Tool Components for SKALP
// ============================================================================

/// Get pre-defined SKALP tool components for qualification
pub fn skalp_tool_components() -> Vec<ToolComponent> {
    vec![
        ToolComponent {
            name: "skalp-frontend".to_string(),
            function: "Source code parsing and HIR generation".to_string(),
            tool_impact: ToolImpact::Ti2,
            error_detection: ToolErrorDetection::Td2,
            error_impact: ErrorImpact::MayIntroduceError {
                error_types: vec![
                    "Incorrect parsing of safety attributes".to_string(),
                    "Missing propagation of safety annotations".to_string(),
                ],
            },
            output_criticality: OutputCriticality::SafetyRelevant,
            iso_reference: Some("ISO 26262-8:11.4.4".to_string()),
        },
        ToolComponent {
            name: "skalp-mir".to_string(),
            function: "MIR generation and optimization".to_string(),
            tool_impact: ToolImpact::Ti2,
            error_detection: ToolErrorDetection::Td2,
            error_impact: ErrorImpact::MayIntroduceError {
                error_types: vec![
                    "Incorrect lowering affecting safety properties".to_string(),
                    "Optimization removing safety-relevant logic".to_string(),
                ],
            },
            output_criticality: OutputCriticality::SafetyRelevant,
            iso_reference: Some("ISO 26262-8:11.4.4".to_string()),
        },
        ToolComponent {
            name: "skalp-lir".to_string(),
            function: "Gate-level netlist generation".to_string(),
            tool_impact: ToolImpact::Ti2,
            error_detection: ToolErrorDetection::Td2,
            error_impact: ErrorImpact::MayIntroduceError {
                error_types: vec![
                    "Incorrect cell classification".to_string(),
                    "FIT rate calculation errors".to_string(),
                ],
            },
            output_criticality: OutputCriticality::SafetyRelevant,
            iso_reference: Some("ISO 26262-8:11.4.4".to_string()),
        },
        ToolComponent {
            name: "skalp-safety".to_string(),
            function: "Safety analysis (FMEA, metrics, design rules)".to_string(),
            tool_impact: ToolImpact::Ti2,
            error_detection: ToolErrorDetection::Td3,
            error_impact: ErrorImpact::MayFailToDetectError,
            output_criticality: OutputCriticality::SafetyAnalysis,
            iso_reference: Some("ISO 26262-8:11.4.4".to_string()),
        },
        ToolComponent {
            name: "skalp-sim".to_string(),
            function: "Fault simulation for DC calculation".to_string(),
            tool_impact: ToolImpact::Ti2,
            error_detection: ToolErrorDetection::Td2,
            error_impact: ErrorImpact::MayFailToDetectError,
            output_criticality: OutputCriticality::SafetyAnalysis,
            iso_reference: Some("ISO 26262-8:11.4.4".to_string()),
        },
    ]
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tcl_assessment_creation() {
        let assessment = TclAssessment::new("skalp", "0.1.0");
        assert_eq!(assessment.tool_name, "skalp");
        assert_eq!(assessment.tool_version, "0.1.0");
    }

    #[test]
    fn test_tcl_calculation() {
        let mut assessment = TclAssessment::new("test-tool", "1.0");

        // Add component with TI2/TD2 -> should result in TCL2
        let component = ToolComponent::new("test-component", "testing")
            .with_tool_impact(ToolImpact::Ti2)
            .with_error_detection(ToolErrorDetection::Td2);
        assessment.add_component(component);

        assessment.calculate_tcl();
        assert_eq!(assessment.assessed_tcl, ToolConfidenceLevel::Tcl2);
    }

    #[test]
    fn test_tcl_derivation() {
        // TI1 always results in TCL1
        assert_eq!(
            TclAssessment::derive_tcl(&ToolImpact::Ti1, &ToolErrorDetection::Td1),
            ToolConfidenceLevel::Tcl1
        );

        // TI2 + TD1 = TCL1
        assert_eq!(
            TclAssessment::derive_tcl(&ToolImpact::Ti2, &ToolErrorDetection::Td1),
            ToolConfidenceLevel::Tcl1
        );

        // TI2 + TD2 = TCL2
        assert_eq!(
            TclAssessment::derive_tcl(&ToolImpact::Ti2, &ToolErrorDetection::Td2),
            ToolConfidenceLevel::Tcl2
        );

        // TI2 + TD3 = TCL3
        assert_eq!(
            TclAssessment::derive_tcl(&ToolImpact::Ti2, &ToolErrorDetection::Td3),
            ToolConfidenceLevel::Tcl3
        );
    }

    #[test]
    fn test_validation_report() {
        let mut report = ValidationReport::new("test-component", "1.0");

        report.add_result(TestResult {
            test_id: "TC-001".to_string(),
            status: TestStatus::Passed,
            actual_output: "OK".to_string(),
            comparison: None,
            error_message: None,
            execution_time_ms: Some(100),
        });

        report.add_result(TestResult {
            test_id: "TC-002".to_string(),
            status: TestStatus::Failed,
            actual_output: "ERROR".to_string(),
            comparison: Some("Expected OK, got ERROR".to_string()),
            error_message: Some("Assertion failed".to_string()),
            execution_time_ms: Some(50),
        });

        assert_eq!(report.passed, 1);
        assert_eq!(report.failed, 1);
        assert_eq!(report.pass_rate(), 0.5);
        assert_eq!(report.status, ValidationStatus::Failed);
    }

    #[test]
    fn test_known_limitations() {
        let mut limitations = KnownLimitations::new();

        let limitation = Limitation::new(
            "LIM-001",
            "skalp-safety",
            "FMEA does not consider temporal failures",
        );
        limitations.add_limitation(limitation);

        let mut workaround = Workaround::new("LIM-001", "Manually add temporal failure modes");
        workaround.add_step("Review timing specifications");
        workaround.add_step("Add temporal failure modes to FMEA");
        limitations.add_workaround(workaround);

        assert_eq!(limitations.limitations.len(), 1);
        assert_eq!(limitations.workarounds_for("LIM-001").len(), 1);
    }

    #[test]
    fn test_report_generation() {
        let assessment = TclAssessment::new("test-tool", "1.0");
        let mut validation = ValidationReport::new("test-tool", "1.0");
        validation.add_result(TestResult {
            test_id: "TC-001".to_string(),
            status: TestStatus::Passed,
            actual_output: "OK".to_string(),
            comparison: None,
            error_message: None,
            execution_time_ms: None,
        });
        validation.status = ValidationStatus::Passed;

        let limitations = KnownLimitations::new();

        let report = ToolQualificationReport::generate(assessment, validation, limitations);

        assert!(report.conclusion.qualified);
    }

    #[test]
    fn test_skalp_components() {
        let components = skalp_tool_components();
        assert!(!components.is_empty());

        // Check that skalp-safety is included
        assert!(components.iter().any(|c| c.name == "skalp-safety"));

        // Check that all components have ISO references
        for component in &components {
            assert!(component.iso_reference.is_some());
        }
    }

    #[test]
    fn test_test_suite() {
        let mut suite = ValidationTestSuite::new("test-component", "1.0");

        suite.add_test_case(ValidationTestCase::new(
            "TC-001",
            "Test positive case",
            TestType::PositiveTest,
        ));

        suite.add_test_case(ValidationTestCase::new(
            "TC-002",
            "Test negative case",
            TestType::NegativeTest,
        ));

        assert_eq!(suite.test_cases.len(), 2);
        assert_eq!(suite.get_by_type(TestType::PositiveTest).len(), 1);
        assert_eq!(suite.get_by_type(TestType::NegativeTest).len(), 1);
    }
}
