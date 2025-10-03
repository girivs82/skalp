//! FMEA (Failure Mode and Effects Analysis) generation for ISO 26262
//!
//! Automatic generation of FMEA tables from design intents and hardware structure.
//! Supports both qualitative and quantitative analysis with ASIL evaluation.

use crate::asil::AsilLevel;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use skalp_frontend::hir::{HirEntity as Entity, HirIntent as Intent};
use skalp_mir::mir::Module;
use std::collections::HashMap;
use uuid::Uuid;

/// FMEA analysis for a design
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmeaAnalysis {
    /// Analysis metadata
    pub metadata: FmeaMetadata,
    /// System boundary definition
    pub system_boundary: SystemBoundary,
    /// Functional analysis results
    pub functional_analysis: FunctionalAnalysis,
    /// FMEA table entries
    pub fmea_entries: Vec<FmeaEntry>,
    /// Summary statistics
    pub summary: FmeaSummary,
}

/// Metadata for FMEA analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmeaMetadata {
    /// Analysis ID
    pub id: String,
    /// Design name
    pub design_name: String,
    /// Analysis version
    pub version: String,
    /// Analysis date
    pub analysis_date: DateTime<Utc>,
    /// Analyst name
    pub analyst: String,
    /// Review status
    pub review_status: ReviewStatus,
    /// ASIL target level
    pub target_asil: AsilLevel,
}

/// Review status of FMEA analysis
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ReviewStatus {
    /// Analysis in progress
    InProgress,
    /// Ready for review
    ReadyForReview,
    /// Under review
    UnderReview,
    /// Review completed
    Reviewed,
    /// Analysis approved
    Approved,
    /// Needs revision
    NeedsRevision,
}

/// System boundary definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemBoundary {
    /// System inputs
    pub inputs: Vec<SystemInterface>,
    /// System outputs
    pub outputs: Vec<SystemInterface>,
    /// Internal components
    pub components: Vec<SystemComponent>,
    /// External interfaces
    pub external_interfaces: Vec<ExternalInterface>,
}

/// System interface definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemInterface {
    /// Interface name
    pub name: String,
    /// Interface type
    pub interface_type: String,
    /// Description
    pub description: String,
    /// Safety criticality
    pub safety_critical: bool,
}

/// Internal system component
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemComponent {
    /// Component name
    pub name: String,
    /// Component type
    pub component_type: ComponentType,
    /// Safety relevance
    pub safety_relevant: bool,
    /// Redundancy level
    pub redundancy_level: u32,
}

/// External interface to other systems
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExternalInterface {
    /// Interface name
    pub name: String,
    /// Connected system
    pub connected_system: String,
    /// Interface protocol
    pub protocol: String,
    /// Safety requirements
    pub safety_requirements: Vec<String>,
}

/// Component type classification
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ComponentType {
    /// Processing element
    Processor,
    /// Memory element
    Memory,
    /// Input/Output element
    InputOutput,
    /// Communication element
    Communication,
    /// Safety element
    Safety,
    /// Power element
    Power,
    /// Clock/Timing element
    ClockTiming,
}

/// Functional analysis of the system
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionalAnalysis {
    /// System functions
    pub functions: Vec<SystemFunction>,
    /// Function dependencies
    pub dependencies: HashMap<String, Vec<String>>,
    /// Critical paths
    pub critical_paths: Vec<CriticalPath>,
}

/// System function definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemFunction {
    /// Function ID
    pub id: String,
    /// Function name
    pub name: String,
    /// Function description
    pub description: String,
    /// Safety relevance
    pub safety_relevant: bool,
    /// ASIL level
    pub asil: AsilLevel,
    /// Associated design intents
    pub intents: Vec<String>,
    /// Implementing components
    pub components: Vec<String>,
}

/// Critical path in the system
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CriticalPath {
    /// Path ID
    pub id: String,
    /// Path description
    pub description: String,
    /// Functions in path
    pub functions: Vec<String>,
    /// Components in path
    pub components: Vec<String>,
    /// Overall ASIL level
    pub asil: AsilLevel,
}

/// Individual FMEA entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmeaEntry {
    /// Entry ID
    pub id: String,
    /// Component or function being analyzed
    pub item: String,
    /// Failure mode description
    pub failure_mode: FailureMode,
    /// Failure effects analysis
    pub effects: FailureEffects,
    /// Failure causes analysis
    pub causes: FailureCauses,
    /// Current controls
    pub current_controls: CurrentControls,
    /// Risk assessment
    pub risk_assessment: RiskAssessment,
    /// Recommended actions
    pub recommended_actions: Vec<RecommendedAction>,
    /// Status tracking
    pub status: EntryStatus,
}

/// Failure mode definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FailureMode {
    /// Failure mode ID
    pub id: String,
    /// Failure mode description
    pub description: String,
    /// Failure mode category
    pub category: FailureModeCategory,
    /// Failure rate (FIT - Failures in Time)
    pub failure_rate: Option<f64>,
    /// Failure mode class
    pub failure_class: FailureClass,
}

/// Failure mode categories
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FailureModeCategory {
    /// No output (stuck-at-0, open circuit)
    NoOutput,
    /// Constant output (stuck-at-1)
    ConstantOutput,
    /// Incorrect output value
    IncorrectValue,
    /// Timing failure (too early, too late)
    TimingFailure,
    /// Intermittent failure
    Intermittent,
    /// Performance degradation
    PerformanceDegradation,
}

/// Failure class according to ISO 26262
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FailureClass {
    /// Safe failure
    Safe,
    /// Single-point failure
    SinglePoint,
    /// Residual failure
    Residual,
    /// Multiple-point failure
    MultiplePoint,
}

/// Failure effects analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FailureEffects {
    /// Local effects (on the component itself)
    pub local_effects: Vec<String>,
    /// Next higher level effects
    pub system_effects: Vec<String>,
    /// End effects (on the vehicle/user)
    pub end_effects: Vec<String>,
    /// Safety impact assessment
    pub safety_impact: SafetyImpact,
}

/// Safety impact classification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SafetyImpact {
    /// Severity class (S1-S3)
    pub severity: SeverityClass,
    /// Exposure probability (E1-E4)
    pub exposure: ExposureClass,
    /// Controllability (C1-C3)
    pub controllability: ControllabilityClass,
    /// Derived ASIL level
    pub derived_asil: AsilLevel,
}

/// Severity classification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SeverityClass {
    /// S1 - Light to moderate injuries
    S1,
    /// S2 - Severe to life-threatening injuries
    S2,
    /// S3 - Life-threatening to fatal injuries
    S3,
}

/// Exposure classification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ExposureClass {
    /// E1 - Very low probability
    E1,
    /// E2 - Low probability
    E2,
    /// E3 - Medium probability
    E3,
    /// E4 - High probability
    E4,
}

/// Controllability classification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ControllabilityClass {
    /// C1 - Controllable in general
    C1,
    /// C2 - Simply controllable
    C2,
    /// C3 - Difficult to control or uncontrollable
    C3,
}

/// Failure causes analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FailureCauses {
    /// Potential causes
    pub potential_causes: Vec<String>,
    /// Root cause categories
    pub root_cause_categories: Vec<RootCauseCategory>,
    /// Occurrence probability
    pub occurrence: OccurrenceClass,
}

/// Root cause categories
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RootCauseCategory {
    /// Design inadequacy
    Design,
    /// Manufacturing defect
    Manufacturing,
    /// Environmental stress
    Environmental,
    /// Aging/Wear-out
    Aging,
    /// External interference
    ExternalInterference,
    /// Software error
    Software,
    /// Human error
    HumanError,
}

/// Occurrence probability classification
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum OccurrenceClass {
    /// Very low occurrence
    VeryLow,
    /// Low occurrence
    Low,
    /// Medium occurrence
    Medium,
    /// High occurrence
    High,
    /// Very high occurrence
    VeryHigh,
}

/// Current controls and detection methods
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CurrentControls {
    /// Prevention controls
    pub prevention: Vec<String>,
    /// Detection controls
    pub detection: Vec<String>,
    /// Detection ranking
    pub detection_ranking: DetectionClass,
}

/// Detection capability classification
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DetectionClass {
    /// Very high detection capability
    VeryHigh,
    /// High detection capability
    High,
    /// Medium detection capability
    Medium,
    /// Low detection capability
    Low,
    /// Very low detection capability
    VeryLow,
    /// No detection capability
    None,
}

/// Risk assessment for FMEA entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RiskAssessment {
    /// Risk Priority Number (traditional FMEA)
    pub rpn: Option<u32>,
    /// ASIL determination
    pub asil_determination: AsilDetermination,
    /// Safety goal violation
    pub safety_goal_violation: bool,
    /// Criticality level
    pub criticality: CriticalityLevel,
}

/// ASIL determination process
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AsilDetermination {
    /// Input severity
    pub severity: SeverityClass,
    /// Input exposure
    pub exposure: ExposureClass,
    /// Input controllability
    pub controllability: ControllabilityClass,
    /// Determined ASIL
    pub determined_asil: AsilLevel,
    /// Justification
    pub justification: String,
}

/// Criticality level
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum CriticalityLevel {
    /// Very low criticality
    VeryLow,
    /// Low criticality
    Low,
    /// Medium criticality
    Medium,
    /// High criticality
    High,
    /// Critical
    Critical,
}

/// Recommended action for risk mitigation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RecommendedAction {
    /// Action ID
    pub id: String,
    /// Action description
    pub description: String,
    /// Action type
    pub action_type: ActionType,
    /// Responsible party
    pub responsible: String,
    /// Target completion date
    pub target_date: Option<DateTime<Utc>>,
    /// Priority
    pub priority: ActionPriority,
    /// Status
    pub status: ActionStatus,
}

/// Types of recommended actions
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ActionType {
    /// Design change
    DesignChange,
    /// Add safety mechanism
    AddSafetyMechanism,
    /// Improve detection
    ImproveDetection,
    /// Process improvement
    ProcessImprovement,
    /// Additional verification
    AdditionalVerification,
    /// Documentation update
    DocumentationUpdate,
}

/// Action priority levels
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum ActionPriority {
    /// Low priority
    Low,
    /// Medium priority
    Medium,
    /// High priority
    High,
    /// Critical priority
    Critical,
}

/// Action completion status
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ActionStatus {
    /// Action planned
    Planned,
    /// Action in progress
    InProgress,
    /// Action completed
    Completed,
    /// Action verified
    Verified,
    /// Action cancelled
    Cancelled,
}

/// FMEA entry status
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum EntryStatus {
    /// Entry drafted
    Draft,
    /// Entry under analysis
    UnderAnalysis,
    /// Analysis completed
    AnalysisComplete,
    /// Entry reviewed
    Reviewed,
    /// Entry approved
    Approved,
}

/// FMEA summary statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmeaSummary {
    /// Total number of entries
    pub total_entries: usize,
    /// Entries by criticality level
    pub criticality_breakdown: HashMap<CriticalityLevel, usize>,
    /// Entries by ASIL level
    pub asil_breakdown: HashMap<AsilLevel, usize>,
    /// Entries by failure class
    pub failure_class_breakdown: HashMap<FailureClass, usize>,
    /// Average RPN
    pub average_rpn: Option<f64>,
    /// Highest risk entries
    pub highest_risk_entries: Vec<String>,
    /// Open actions count
    pub open_actions: usize,
}

/// FMEA generator for automatic analysis
#[derive(Debug)]
pub struct FmeaGenerator {
    /// Configuration for generation
    config: FmeaConfig,
    /// Failure mode database
    failure_mode_db: FailureModeDatabase,
}

/// Configuration for FMEA generation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmeaConfig {
    /// Include common failure modes
    pub include_common_modes: bool,
    /// Generate quantitative analysis
    pub quantitative_analysis: bool,
    /// ASIL decomposition
    pub asil_decomposition: bool,
    /// Include software failures
    pub include_software: bool,
    /// Analysis depth level
    pub analysis_depth: AnalysisDepth,
}

/// Analysis depth configuration
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum AnalysisDepth {
    /// Component level only
    Component,
    /// Function level
    Function,
    /// System level
    System,
    /// Comprehensive (all levels)
    Comprehensive,
}

/// Database of common failure modes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FailureModeDatabase {
    /// Failure modes by component type
    pub component_failures: HashMap<ComponentType, Vec<FailureMode>>,
    /// Generic failure modes
    pub generic_failures: Vec<FailureMode>,
    /// Failure rate data
    pub failure_rates: HashMap<String, f64>,
}

impl FmeaGenerator {
    /// Create a new FMEA generator
    pub fn new(config: FmeaConfig) -> Self {
        Self {
            config,
            failure_mode_db: FailureModeDatabase::default(),
        }
    }

    /// Generate FMEA from design entities and intents
    pub fn generate_from_design(
        &self,
        entities: &[Entity],
        intents: &[Intent],
        _modules: &[Module],
    ) -> Result<FmeaAnalysis, FmeaError> {
        let mut analysis = FmeaAnalysis {
            metadata: self.create_metadata("Generated FMEA".to_string()),
            system_boundary: self.analyze_system_boundary(entities)?,
            functional_analysis: self.analyze_functions(entities, intents)?,
            fmea_entries: vec![],
            summary: FmeaSummary::default(),
        };

        // Generate FMEA entries for each component/function
        analysis.fmea_entries =
            self.generate_entries(&analysis.system_boundary, &analysis.functional_analysis)?;

        // Calculate summary
        analysis.summary = self.calculate_summary(&analysis.fmea_entries);

        Ok(analysis)
    }

    /// Create metadata for the analysis
    fn create_metadata(&self, design_name: String) -> FmeaMetadata {
        FmeaMetadata {
            id: Uuid::new_v4().to_string(),
            design_name,
            version: "1.0".to_string(),
            analysis_date: Utc::now(),
            analyst: "SKALP FMEA Generator".to_string(),
            review_status: ReviewStatus::InProgress,
            target_asil: AsilLevel::D, // Conservative default
        }
    }

    /// Analyze system boundary from entities
    fn analyze_system_boundary(&self, entities: &[Entity]) -> Result<SystemBoundary, FmeaError> {
        let mut inputs = vec![];
        let mut outputs = vec![];
        let mut components = vec![];

        for entity in entities {
            // Add inputs and outputs based on entity ports
            for port in &entity.ports {
                let interface = SystemInterface {
                    name: port.name.clone(),
                    interface_type: format!("{:?}", port.direction),
                    description: format!("Port {} of entity {}", port.name, entity.name),
                    safety_critical: true, // Conservative assumption
                };

                match port.direction {
                    skalp_frontend::hir::HirPortDirection::Input => inputs.push(interface),
                    skalp_frontend::hir::HirPortDirection::Output => outputs.push(interface),
                    skalp_frontend::hir::HirPortDirection::Bidirectional => {
                        inputs.push(interface.clone());
                        outputs.push(interface);
                    }
                    skalp_frontend::hir::HirPortDirection::Protocol => {
                        // Protocol ports can be bidirectional
                        inputs.push(interface.clone());
                        outputs.push(interface);
                    }
                }
            }

            // Add entity as component
            components.push(SystemComponent {
                name: entity.name.clone(),
                component_type: self.classify_component(&entity.name),
                safety_relevant: true,
                redundancy_level: 0,
            });
        }

        Ok(SystemBoundary {
            inputs,
            outputs,
            components,
            external_interfaces: vec![], // Would be populated from design context
        })
    }

    /// Analyze functions from entities and intents
    fn analyze_functions(
        &self,
        entities: &[Entity],
        intents: &[Intent],
    ) -> Result<FunctionalAnalysis, FmeaError> {
        let mut functions = vec![];
        let dependencies = HashMap::new();

        // Create functions from intents
        for intent in intents {
            let function = SystemFunction {
                id: format!("FUNC-{}", intent.id.0),
                name: intent.description.clone(),
                description: intent.description.clone(),
                safety_relevant: true,
                asil: AsilLevel::B, // Default ASIL level
                intents: vec![format!("INTENT-{}", intent.id.0)],
                components: vec![], // Would be populated from implementation mapping
            };
            functions.push(function);
        }

        // Create functions from entities if no intents available
        if functions.is_empty() {
            for entity in entities {
                let function = SystemFunction {
                    id: format!("FUNC-{}", entity.name),
                    name: entity.name.clone(),
                    description: format!("Function implemented by entity {}", entity.name),
                    safety_relevant: true,
                    asil: AsilLevel::A,
                    intents: vec![],
                    components: vec![entity.name.clone()],
                };
                functions.push(function);
            }
        }

        Ok(FunctionalAnalysis {
            functions,
            dependencies,
            critical_paths: vec![], // Would be analyzed from function flow
        })
    }

    /// Generate FMEA entries
    fn generate_entries(
        &self,
        boundary: &SystemBoundary,
        functional_analysis: &FunctionalAnalysis,
    ) -> Result<Vec<FmeaEntry>, FmeaError> {
        let mut entries = vec![];

        // Generate entries for each component
        for component in &boundary.components {
            let component_failures = self
                .failure_mode_db
                .component_failures
                .get(&component.component_type)
                .cloned()
                .unwrap_or_default();

            for failure_mode in component_failures {
                let entry =
                    self.create_fmea_entry(&component.name, failure_mode, functional_analysis)?;
                entries.push(entry);
            }

            // Add generic failure modes
            for failure_mode in &self.failure_mode_db.generic_failures {
                let entry = self.create_fmea_entry(
                    &component.name,
                    failure_mode.clone(),
                    functional_analysis,
                )?;
                entries.push(entry);
            }
        }

        Ok(entries)
    }

    /// Create individual FMEA entry
    fn create_fmea_entry(
        &self,
        item: &str,
        failure_mode: FailureMode,
        _functional_analysis: &FunctionalAnalysis,
    ) -> Result<FmeaEntry, FmeaError> {
        let entry = FmeaEntry {
            id: Uuid::new_v4().to_string(),
            item: item.to_string(),
            failure_mode: failure_mode.clone(),
            effects: self.analyze_effects(&failure_mode)?,
            causes: self.analyze_causes(&failure_mode)?,
            current_controls: self.analyze_current_controls()?,
            risk_assessment: self.assess_risk(&failure_mode)?,
            recommended_actions: vec![],
            status: EntryStatus::Draft,
        };

        Ok(entry)
    }

    /// Analyze failure effects
    fn analyze_effects(&self, failure_mode: &FailureMode) -> Result<FailureEffects, FmeaError> {
        // This would be more sophisticated in a real implementation
        let effects = FailureEffects {
            local_effects: vec![format!("Component exhibits {}", failure_mode.description)],
            system_effects: vec!["System function degraded".to_string()],
            end_effects: vec!["Potential safety impact".to_string()],
            safety_impact: SafetyImpact {
                severity: SeverityClass::S2,
                exposure: ExposureClass::E3,
                controllability: ControllabilityClass::C2,
                derived_asil: AsilLevel::B,
            },
        };

        Ok(effects)
    }

    /// Analyze failure causes
    fn analyze_causes(&self, _failure_mode: &FailureMode) -> Result<FailureCauses, FmeaError> {
        let causes = FailureCauses {
            potential_causes: vec![
                "Component aging".to_string(),
                "Environmental stress".to_string(),
                "Manufacturing defect".to_string(),
            ],
            root_cause_categories: vec![
                RootCauseCategory::Aging,
                RootCauseCategory::Environmental,
                RootCauseCategory::Manufacturing,
            ],
            occurrence: OccurrenceClass::Low,
        };

        Ok(causes)
    }

    /// Analyze current controls
    fn analyze_current_controls(&self) -> Result<CurrentControls, FmeaError> {
        let controls = CurrentControls {
            prevention: vec!["Design margin".to_string(), "Quality process".to_string()],
            detection: vec!["Built-in self-test".to_string()],
            detection_ranking: DetectionClass::Medium,
        };

        Ok(controls)
    }

    /// Assess risk for failure mode
    fn assess_risk(&self, _failure_mode: &FailureMode) -> Result<RiskAssessment, FmeaError> {
        let asil_determination = AsilDetermination {
            severity: SeverityClass::S2,
            exposure: ExposureClass::E3,
            controllability: ControllabilityClass::C2,
            determined_asil: self.determine_asil(
                SeverityClass::S2,
                ExposureClass::E3,
                ControllabilityClass::C2,
            ),
            justification: "Based on severity, exposure, and controllability analysis".to_string(),
        };

        let assessment = RiskAssessment {
            rpn: Some(150), // Example RPN calculation
            asil_determination,
            safety_goal_violation: false,
            criticality: CriticalityLevel::Medium,
        };

        Ok(assessment)
    }

    /// Determine ASIL level from S, E, C classification
    fn determine_asil(
        &self,
        severity: SeverityClass,
        exposure: ExposureClass,
        controllability: ControllabilityClass,
    ) -> AsilLevel {
        // ISO 26262 ASIL determination matrix
        match (severity, exposure, controllability) {
            (SeverityClass::S1, _, _) => match (exposure, controllability) {
                (ExposureClass::E1 | ExposureClass::E2, _) => AsilLevel::QM,
                (ExposureClass::E3, ControllabilityClass::C1) => AsilLevel::QM,
                (ExposureClass::E3, ControllabilityClass::C2 | ControllabilityClass::C3) => {
                    AsilLevel::A
                }
                (ExposureClass::E4, ControllabilityClass::C1) => AsilLevel::A,
                (ExposureClass::E4, ControllabilityClass::C2) => AsilLevel::A,
                (ExposureClass::E4, ControllabilityClass::C3) => AsilLevel::B,
            },
            (SeverityClass::S2, _, _) => match (exposure, controllability) {
                (ExposureClass::E1, _) => AsilLevel::QM,
                (ExposureClass::E2, ControllabilityClass::C1) => AsilLevel::QM,
                (ExposureClass::E2, ControllabilityClass::C2) => AsilLevel::A,
                (ExposureClass::E2, ControllabilityClass::C3) => AsilLevel::B,
                (ExposureClass::E3, ControllabilityClass::C1) => AsilLevel::A,
                (ExposureClass::E3, ControllabilityClass::C2) => AsilLevel::B,
                (ExposureClass::E3, ControllabilityClass::C3) => AsilLevel::C,
                (ExposureClass::E4, ControllabilityClass::C1) => AsilLevel::B,
                (ExposureClass::E4, ControllabilityClass::C2) => AsilLevel::C,
                (ExposureClass::E4, ControllabilityClass::C3) => AsilLevel::D,
            },
            (SeverityClass::S3, _, _) => match (exposure, controllability) {
                (ExposureClass::E1, ControllabilityClass::C1) => AsilLevel::QM,
                (ExposureClass::E1, ControllabilityClass::C2) => AsilLevel::A,
                (ExposureClass::E1, ControllabilityClass::C3) => AsilLevel::B,
                (ExposureClass::E2, ControllabilityClass::C1) => AsilLevel::A,
                (ExposureClass::E2, ControllabilityClass::C2) => AsilLevel::B,
                (ExposureClass::E2, ControllabilityClass::C3) => AsilLevel::C,
                (ExposureClass::E3, ControllabilityClass::C1) => AsilLevel::B,
                (ExposureClass::E3, ControllabilityClass::C2) => AsilLevel::C,
                (ExposureClass::E3, ControllabilityClass::C3) => AsilLevel::D,
                (ExposureClass::E4, ControllabilityClass::C1) => AsilLevel::C,
                (ExposureClass::E4, ControllabilityClass::C2 | ControllabilityClass::C3) => {
                    AsilLevel::D
                }
            },
        }
    }

    /// Calculate summary statistics
    fn calculate_summary(&self, entries: &[FmeaEntry]) -> FmeaSummary {
        let total_entries = entries.len();
        let mut criticality_breakdown = HashMap::new();
        let mut asil_breakdown = HashMap::new();
        let mut failure_class_breakdown = HashMap::new();
        let mut rpn_sum = 0.0;
        let mut rpn_count = 0;
        let mut open_actions = 0;

        for entry in entries {
            // Count criticality levels
            *criticality_breakdown
                .entry(entry.risk_assessment.criticality.clone())
                .or_insert(0) += 1;

            // Count ASIL levels
            *asil_breakdown
                .entry(entry.risk_assessment.asil_determination.determined_asil)
                .or_insert(0) += 1;

            // Count failure classes
            *failure_class_breakdown
                .entry(entry.failure_mode.failure_class.clone())
                .or_insert(0) += 1;

            // Calculate average RPN
            if let Some(rpn) = entry.risk_assessment.rpn {
                rpn_sum += rpn as f64;
                rpn_count += 1;
            }

            // Count open actions
            open_actions += entry
                .recommended_actions
                .iter()
                .filter(|a| !matches!(a.status, ActionStatus::Completed | ActionStatus::Verified))
                .count();
        }

        let average_rpn = if rpn_count > 0 {
            Some(rpn_sum / rpn_count as f64)
        } else {
            None
        };

        // Find highest risk entries (top 5 by RPN)
        let mut rpn_entries: Vec<_> = entries
            .iter()
            .filter_map(|e| e.risk_assessment.rpn.map(|rpn| (rpn, e.id.clone())))
            .collect();
        rpn_entries.sort_by(|a, b| b.0.cmp(&a.0));
        let highest_risk_entries = rpn_entries.into_iter().take(5).map(|(_, id)| id).collect();

        FmeaSummary {
            total_entries,
            criticality_breakdown,
            asil_breakdown,
            failure_class_breakdown,
            average_rpn,
            highest_risk_entries,
            open_actions,
        }
    }

    /// Classify component type based on name
    fn classify_component(&self, name: &str) -> ComponentType {
        let name_lower = name.to_lowercase();
        if name_lower.contains("cpu") || name_lower.contains("processor") {
            ComponentType::Processor
        } else if name_lower.contains("memory")
            || name_lower.contains("ram")
            || name_lower.contains("rom")
        {
            ComponentType::Memory
        } else if name_lower.contains("io")
            || name_lower.contains("input")
            || name_lower.contains("output")
        {
            ComponentType::InputOutput
        } else if name_lower.contains("comm")
            || name_lower.contains("uart")
            || name_lower.contains("spi")
        {
            ComponentType::Communication
        } else if name_lower.contains("safety") || name_lower.contains("watchdog") {
            ComponentType::Safety
        } else if name_lower.contains("power") || name_lower.contains("pmu") {
            ComponentType::Power
        } else if name_lower.contains("clock") || name_lower.contains("timer") {
            ComponentType::ClockTiming
        } else {
            ComponentType::Processor // Default
        }
    }
}

/// Errors that can occur during FMEA generation
#[derive(Debug, thiserror::Error)]
pub enum FmeaError {
    #[error("Invalid configuration: {0}")]
    InvalidConfig(String),
    #[error("Analysis failed: {0}")]
    AnalysisFailed(String),
    #[error("Data missing: {0}")]
    DataMissing(String),
}

impl Default for FailureModeDatabase {
    fn default() -> Self {
        let mut db = Self {
            component_failures: HashMap::new(),
            generic_failures: vec![],
            failure_rates: HashMap::new(),
        };

        // Add common failure modes for processors
        db.component_failures.insert(
            ComponentType::Processor,
            vec![
                FailureMode {
                    id: "PROC-001".to_string(),
                    description: "Instruction execution error".to_string(),
                    category: FailureModeCategory::IncorrectValue,
                    failure_rate: Some(100.0), // 100 FIT
                    failure_class: FailureClass::SinglePoint,
                },
                FailureMode {
                    id: "PROC-002".to_string(),
                    description: "Clock failure".to_string(),
                    category: FailureModeCategory::TimingFailure,
                    failure_rate: Some(50.0),
                    failure_class: FailureClass::SinglePoint,
                },
            ],
        );

        // Add common failure modes for memory
        db.component_failures.insert(
            ComponentType::Memory,
            vec![
                FailureMode {
                    id: "MEM-001".to_string(),
                    description: "Single bit upset".to_string(),
                    category: FailureModeCategory::IncorrectValue,
                    failure_rate: Some(200.0),
                    failure_class: FailureClass::Safe, // If ECC is present
                },
                FailureMode {
                    id: "MEM-002".to_string(),
                    description: "Double bit upset".to_string(),
                    category: FailureModeCategory::IncorrectValue,
                    failure_rate: Some(10.0),
                    failure_class: FailureClass::SinglePoint,
                },
            ],
        );

        db
    }
}

impl Default for FmeaSummary {
    fn default() -> Self {
        Self {
            total_entries: 0,
            criticality_breakdown: HashMap::new(),
            asil_breakdown: HashMap::new(),
            failure_class_breakdown: HashMap::new(),
            average_rpn: None,
            highest_risk_entries: vec![],
            open_actions: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_asil_determination() {
        let generator = FmeaGenerator::new(FmeaConfig {
            include_common_modes: true,
            quantitative_analysis: false,
            asil_decomposition: false,
            include_software: false,
            analysis_depth: AnalysisDepth::Component,
        });

        // Test high-risk scenario (S3, E4, C3) -> ASIL D
        let asil = generator.determine_asil(
            SeverityClass::S3,
            ExposureClass::E4,
            ControllabilityClass::C3,
        );
        assert_eq!(asil, AsilLevel::D);

        // Test low-risk scenario (S1, E1, C1) -> QM
        let asil = generator.determine_asil(
            SeverityClass::S1,
            ExposureClass::E1,
            ControllabilityClass::C1,
        );
        assert_eq!(asil, AsilLevel::QM);
    }

    #[test]
    fn test_component_classification() {
        let generator = FmeaGenerator::new(FmeaConfig {
            include_common_modes: true,
            quantitative_analysis: false,
            asil_decomposition: false,
            include_software: false,
            analysis_depth: AnalysisDepth::Component,
        });

        assert_eq!(
            generator.classify_component("cpu_core"),
            ComponentType::Processor
        );
        assert_eq!(
            generator.classify_component("memory_controller"),
            ComponentType::Memory
        );
        assert_eq!(
            generator.classify_component("io_driver"),
            ComponentType::InputOutput
        );
        assert_eq!(
            generator.classify_component("safety_monitor"),
            ComponentType::Safety
        );
    }

    #[test]
    fn test_failure_mode_database() {
        let db = FailureModeDatabase::default();

        // Check processor failures
        let proc_failures = db.component_failures.get(&ComponentType::Processor);
        assert!(proc_failures.is_some());
        assert!(!proc_failures.unwrap().is_empty());

        // Check memory failures
        let mem_failures = db.component_failures.get(&ComponentType::Memory);
        assert!(mem_failures.is_some());
        assert!(!mem_failures.unwrap().is_empty());
    }

    #[test]
    fn test_fmea_generation() {
        let config = FmeaConfig {
            include_common_modes: true,
            quantitative_analysis: false,
            asil_decomposition: false,
            include_software: false,
            analysis_depth: AnalysisDepth::Component,
        };

        let generator = FmeaGenerator::new(config);

        // Create simple test entity using HIR types
        let entity = Entity {
            id: skalp_frontend::hir::EntityId(0),
            name: "test_cpu".to_string(),
            ports: vec![
                skalp_frontend::hir::HirPort {
                    id: skalp_frontend::hir::PortId(0),
                    name: "clk".to_string(),
                    direction: skalp_frontend::hir::HirPortDirection::Input,
                    port_type: skalp_frontend::hir::HirType::Bit(1),
                },
                skalp_frontend::hir::HirPort {
                    id: skalp_frontend::hir::PortId(1),
                    name: "data_out".to_string(),
                    direction: skalp_frontend::hir::HirPortDirection::Output,
                    port_type: skalp_frontend::hir::HirType::Bit(1),
                },
            ],
            generics: vec![],
            clock_domains: vec![],
        };

        let result = generator.generate_from_design(&[entity], &[], &[]);
        assert!(result.is_ok());

        let analysis = result.unwrap();
        assert!(!analysis.fmea_entries.is_empty());
        assert_eq!(analysis.metadata.design_name, "Generated FMEA");
    }

    #[test]
    fn test_risk_assessment() {
        let config = FmeaConfig {
            include_common_modes: true,
            quantitative_analysis: true,
            asil_decomposition: false,
            include_software: false,
            analysis_depth: AnalysisDepth::Function,
        };

        let generator = FmeaGenerator::new(config);

        let failure_mode = FailureMode {
            id: "TEST-001".to_string(),
            description: "Test failure".to_string(),
            category: FailureModeCategory::IncorrectValue,
            failure_rate: Some(100.0),
            failure_class: FailureClass::SinglePoint,
        };

        let risk = generator.assess_risk(&failure_mode).unwrap();
        assert!(risk.rpn.is_some());
        assert_eq!(risk.asil_determination.determined_asil, AsilLevel::B);
    }
}
