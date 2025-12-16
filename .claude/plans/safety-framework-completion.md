# Plan: Complete Safety Framework for ASIL C/D Certification

## Goal
Implement all missing features to achieve full ISO 26262 ASIL D certification support.

## Current State
- ASIL A/B ready with PMHF, SPFM, LF, FTA, FMEDA, CCF, GSN
- Missing: Safety annotation flow, HSI, systematic coverage, tool qualification

---

## Phase 1: ASIL C/D Enablers (Critical Path)

### 1.1 Safety Annotation Parsing and Propagation

**New File**: `crates/skalp-frontend/src/safety_attributes.rs`

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SafetyAttribute {
    SafetyGoal {
        id: String,
        asil: AsilLevel,
        description: Option<String>,
        ftti_ns: Option<u64>,
    },
    Implements {
        goal_id: String,
    },
    SafetyMechanism {
        name: String,
        mechanism_type: MechanismType,
        dc_target: f64,
        protects: Option<String>,
    },
    SafetyCritical {
        reason: String,
    },
}

pub fn parse_safety_attributes(attrs: &[Attribute]) -> Vec<SafetyAttribute>;
```

**Modify**: `crates/skalp-frontend/src/hir/mod.rs`
- Add `safety_attributes: Vec<SafetyAttribute>` to Component, Signal, Port

**Modify**: `crates/skalp-mir/src/types.rs`
- Add `safety_info: Option<SafetyInfo>` to Module, Signal

**Modify**: `crates/skalp-mir/src/hir_to_mir.rs`
- Transfer safety attributes during lowering

**Modify**: `crates/skalp-lir/src/mir_to_gate_netlist.rs`
- Auto-set CellSafetyClassification from safety_info
- Maintain traceability map: Cell → Source Safety Attribute

**Tests**:
- `test_parse_safety_goal_attribute`
- `test_parse_safety_mechanism_attribute`
- `test_propagation_hir_to_mir`
- `test_propagation_mir_to_lir`
- `test_auto_cell_classification`
- `test_traceability_preservation`

---

### 1.2 Hardware-Software Interface (HSI) Module

**New File**: `crates/skalp-safety/src/hsi.rs`

```rust
/// HSI signal direction
pub enum HsiDirection {
    HardwareToSoftware,
    SoftwareToHardware,
    Bidirectional,
}

/// Single HSI signal
pub struct HsiSignal {
    pub name: String,
    pub direction: HsiDirection,
    pub width: u32,
    pub safety_relevant: bool,
    pub asil: Option<AsilLevel>,
    pub timing: HsiTiming,
}

pub struct HsiTiming {
    pub max_latency_ns: u64,
    pub sampling_period_ns: Option<u64>,
    pub setup_time_ns: Option<u64>,
    pub hold_time_ns: Option<u64>,
}

/// HSI requirement
pub struct HsiRequirement {
    pub id: String,
    pub description: String,
    pub signal: String,
    pub allocated_to: HsiAllocation,
    pub asil: AsilLevel,
    pub verification_method: VerificationMethod,
}

pub enum HsiAllocation {
    Hardware,
    Software,
    Both { hw_part: String, sw_part: String },
}

/// Complete HSI specification
pub struct HardwareSoftwareInterface {
    pub design_name: String,
    pub signals: Vec<HsiSignal>,
    pub requirements: Vec<HsiRequirement>,
}

impl HardwareSoftwareInterface {
    pub fn new(design_name: &str) -> Self;
    pub fn add_signal(&mut self, signal: HsiSignal);
    pub fn add_requirement(&mut self, req: HsiRequirement);
    pub fn generate_fmea(&self) -> Vec<FmeaEntry>;
    pub fn verify_timing(&self) -> Vec<HsiTimingResult>;
    pub fn export_specification(&self) -> String;
}

/// HSI-specific failure modes
fn hsi_failure_modes(signal: &HsiSignal) -> Vec<FailureMode> {
    vec![
        FailureMode::StuckAt { value: 0 },
        FailureMode::StuckAt { value: 1 },
        FailureMode::TimingViolation,
        FailureMode::DataCorruption,
        FailureMode::SignalGlitch,
    ]
}
```

**Modify**: `crates/skalp-safety/src/lib.rs`
- Add `pub mod hsi;`

**Modify**: `crates/skalp-safety/src/hierarchy.rs`
- Add `hsi: Option<HardwareSoftwareInterface>` to SafetyHierarchy

**Tests**:
- `test_hsi_signal_definition`
- `test_hsi_fmea_generation`
- `test_hsi_timing_verification`
- `test_hsi_requirement_allocation`

---

### 1.3 Systematic Failure Coverage

**New File**: `crates/skalp-safety/src/design_rules.rs`

```rust
/// Safety design rule types
pub enum SafetyDesignRule {
    /// No single component failure can violate safety goal
    NoSinglePointOfFailure {
        goal_id: String,
    },
    /// Minimum redundancy level required
    RedundancyRequired {
        element: String,
        min_copies: u32,
    },
    /// Diversity required between redundant elements
    DiversityRequired {
        elements: Vec<String>,
        aspects: Vec<DiversityAspect>,
    },
    /// Independence required between elements
    IndependenceRequired {
        element_a: String,
        element_b: String,
        separation_type: SeparationType,
    },
    /// Watchdog timer required
    WatchdogRequired {
        monitored_element: String,
        timeout_ms: u64,
    },
    /// Voting logic required
    VotingRequired {
        inputs: Vec<String>,
        voter_type: VoterType,
    },
}

pub enum DiversityAspect {
    DifferentAlgorithm,
    DifferentImplementation,
    DifferentTechnology,
    DifferentDesignTeam,
    DifferentToolchain,
}

pub enum SeparationType {
    PhysicalSeparation { min_distance_um: f64 },
    TemporalSeparation { min_delay_ns: u64 },
    LogicalSeparation,
    PowerDomainSeparation,
}

/// Design rule checker
pub struct DesignRuleChecker {
    rules: Vec<SafetyDesignRule>,
}

impl DesignRuleChecker {
    pub fn new() -> Self;
    pub fn add_rule(&mut self, rule: SafetyDesignRule);
    pub fn check(&self, netlist: &GateNetlist, hierarchy: &SafetyHierarchy) -> DesignRuleReport;
}

pub struct DesignRuleReport {
    pub passed: Vec<RuleResult>,
    pub failed: Vec<RuleResult>,
    pub warnings: Vec<RuleResult>,
    pub overall_compliant: bool,
}
```

**New File**: `crates/skalp-safety/src/diversity.rs`

```rust
/// Diversity analysis for redundant elements
pub struct DiversityAnalysis {
    pub element_pairs: Vec<ElementPair>,
    pub diversity_matrix: HashMap<(String, String), DiversityScore>,
    pub common_mode_risks: Vec<CommonModeRisk>,
}

pub struct ElementPair {
    pub element_a: String,
    pub element_b: String,
    pub intended_diversity: Vec<DiversityAspect>,
    pub actual_diversity: Vec<DiversityAspect>,
    pub compliant: bool,
}

pub struct DiversityScore {
    pub algorithm_diversity: f64,      // 0.0 - 1.0
    pub implementation_diversity: f64,
    pub technology_diversity: f64,
    pub overall_score: f64,
}

pub fn analyze_diversity(
    netlist: &GateNetlist,
    redundant_elements: &[(String, String)],
) -> DiversityAnalysis;

pub fn identify_common_elements(
    netlist: &GateNetlist,
    element_a: &str,
    element_b: &str,
) -> Vec<String>;
```

**Tests**:
- `test_single_point_failure_detection`
- `test_redundancy_verification`
- `test_diversity_analysis`
- `test_independence_verification`
- `test_common_element_identification`

---

### 1.4 Tool Qualification Package

**New File**: `crates/skalp-safety/src/tool_qualification.rs`

```rust
/// Tool Confidence Level per ISO 26262-8
pub enum ToolConfidenceLevel {
    TCL1,  // Tool can introduce errors, no detection
    TCL2,  // Tool can introduce errors, partial detection
    TCL3,  // Tool unlikely to introduce undetected errors
}

/// Tool Impact classification
pub enum ToolImpact {
    TI1,  // Could introduce errors in safety item
    TI2,  // Could fail to detect errors but not introduce
}

/// Tool Error Detection capability
pub enum ToolErrorDetection {
    TD1,  // High confidence in error detection
    TD2,  // Medium confidence
    TD3,  // Low confidence
}

pub struct TclAssessment {
    pub tool_name: String,
    pub tool_version: String,
    pub use_cases: Vec<ToolUseCase>,
    pub impact: ToolImpact,
    pub error_detection: ToolErrorDetection,
    pub tcl: ToolConfidenceLevel,
    pub qualification_required: QualificationMethod,
}

pub struct ToolUseCase {
    pub name: String,
    pub description: String,
    pub input_artifacts: Vec<String>,
    pub output_artifacts: Vec<String>,
    pub safety_relevance: String,
}

pub enum QualificationMethod {
    IncreasedConfidenceFromUse,
    ValidationOfSoftwareTool,
    DevelopmentPerSoftwareStandard,
}

/// Validation test case
pub struct ValidationTestCase {
    pub id: String,
    pub description: String,
    pub preconditions: Vec<String>,
    pub test_steps: Vec<String>,
    pub expected_result: String,
    pub actual_result: Option<String>,
    pub status: TestStatus,
}

pub struct ToolQualificationReport {
    pub assessment: TclAssessment,
    pub validation_tests: Vec<ValidationTestCase>,
    pub known_limitations: Vec<KnownLimitation>,
    pub user_guidance: Vec<String>,
}

pub fn generate_qualification_report(
    tool_name: &str,
    version: &str,
    use_cases: &[ToolUseCase],
) -> ToolQualificationReport;
```

**Tests**:
- `test_tcl_assessment`
- `test_validation_test_execution`
- `test_qualification_report_generation`

---

## Phase 2: Enhanced Analysis

### 2.1 Uncertainty Quantification

**New File**: `crates/skalp-safety/src/uncertainty.rs`

```rust
pub struct MonteCarloConfig {
    pub iterations: u32,
    pub confidence_level: f64,
    pub seed: Option<u64>,
    pub parallel: bool,
}

impl Default for MonteCarloConfig {
    fn default() -> Self {
        Self {
            iterations: 10000,
            confidence_level: 0.95,
            seed: None,
            parallel: true,
        }
    }
}

pub enum FitDistribution {
    LogNormal { mu: f64, sigma: f64 },
    Exponential { lambda: f64 },
    Weibull { shape: f64, scale: f64 },
    Uniform { min: f64, max: f64 },
    Fixed(f64),
}

pub struct UncertainMetric {
    pub mean: f64,
    pub median: f64,
    pub std_dev: f64,
    pub min: f64,
    pub max: f64,
    pub percentile_5: f64,
    pub percentile_95: f64,
    pub samples: Vec<f64>,
}

pub fn monte_carlo_pmhf(
    fmea: &FmeaAnalysis,
    fit_distributions: &HashMap<String, FitDistribution>,
    config: &MonteCarloConfig,
) -> UncertainMetric;

pub struct SensitivityResult {
    pub parameter: String,
    pub sensitivity_index: f64,  // Sobol index
    pub rank: usize,
}

pub fn sensitivity_analysis(
    fmea: &FmeaAnalysis,
    target: MetricType,
) -> Vec<SensitivityResult>;
```

---

### 2.2 Dynamic FTA Gates

**Modify**: `crates/skalp-safety/src/fta.rs`

```rust
pub enum DynamicGateType {
    /// Priority AND - events must occur in order
    PAND {
        ordered_inputs: Vec<FtaNodeId>,
    },
    /// Standby spare with cold/warm/hot spare
    SPARE {
        primary: FtaNodeId,
        spares: Vec<FtaNodeId>,
        spare_type: SpareType,
        switching_failure_prob: f64,
    },
    /// Sequence enforcer
    SEQ {
        sequence: Vec<FtaNodeId>,
    },
    /// Functional dependency
    FDEP {
        trigger: FtaNodeId,
        dependents: Vec<FtaNodeId>,
    },
}

pub enum SpareType {
    Cold,  // No failure rate until activated
    Warm,  // Reduced failure rate
    Hot,   // Same failure rate as primary
}

/// Temporal BDD for dynamic gates
pub struct TemporalBdd {
    pub static_structure: Bdd,
    pub temporal_constraints: Vec<TemporalConstraint>,
}

pub struct TemporalConstraint {
    pub before: FtaNodeId,
    pub after: FtaNodeId,
}

pub fn analyze_dynamic_fault_tree(tree: &FaultTree) -> DynamicFtaResult;
```

---

### 2.3 Formal SM Verification

**New File**: `crates/skalp-safety/src/sm_verification.rs`

```rust
pub struct SmProperty {
    pub name: String,
    pub property_type: SmPropertyType,
}

pub enum SmPropertyType {
    /// SM detects stuck-at fault on signal
    DetectsStuckAt {
        signal: String,
        value: bool,
        within_cycles: u32,
    },
    /// SM detects transient fault
    DetectsTransient {
        signal: String,
        duration_cycles: u32,
    },
    /// SM responds within time limit
    ResponseTime {
        max_cycles: u32,
    },
    /// SM achieves coverage target
    CoverageTarget {
        fault_type: FaultType,
        min_coverage: f64,
    },
}

pub struct SmVerificationResult {
    pub sm_name: String,
    pub properties_checked: Vec<PropertyResult>,
    pub overall_verified: bool,
}

pub struct PropertyResult {
    pub property: SmProperty,
    pub verified: bool,
    pub counterexample: Option<Counterexample>,
    pub proof_time_ms: u64,
}

pub fn verify_sm_properties(
    sm: &SafetyMechanism,
    netlist: &GateNetlist,
    properties: &[SmProperty],
) -> SmVerificationResult;
```

---

### 2.4 Power Domain Completion

**Modify**: `crates/skalp-safety/src/power_domains.rs`

```rust
pub struct IsolationCell {
    pub name: String,
    pub cell_type: IsolationCellType,
    pub source_domain: String,
    pub target_domain: String,
    pub isolation_value: Option<bool>,
}

pub enum IsolationCellType {
    AndIsolation,
    OrIsolation,
    LatchIsolation,
    ClampHigh,
    ClampLow,
}

pub struct LevelShifter {
    pub name: String,
    pub source_voltage: f64,
    pub target_voltage: f64,
    pub bidirectional: bool,
}

pub fn verify_isolation_cells(
    netlist: &GateNetlist,
    domains: &[PowerDomainInfo],
) -> IsolationVerificationResult;

pub struct IsolationVerificationResult {
    pub verified_crossings: Vec<VerifiedCrossing>,
    pub missing_isolation: Vec<MissingIsolation>,
    pub level_shifter_issues: Vec<LevelShifterIssue>,
    pub compliant: bool,
}

pub fn analyze_power_sequencing(
    domains: &[PowerDomainInfo],
) -> PowerSequencingAnalysis;
```

---

## Phase 3: Certification Artifacts

### 3.1 Enhanced Export Formats

**Modify**: `crates/skalp-safety/src/workproducts.rs`

```rust
pub fn export_fmea_xlsx(
    fmea: &FmeaAnalysis,
    path: &Path,
) -> io::Result<()>;

pub fn export_fmeda_xlsx(
    fmeda: &FmedaReport,
    path: &Path,
) -> io::Result<()>;

pub fn export_metrics_xlsx(
    metrics: &HardwareArchitecturalMetrics,
    path: &Path,
) -> io::Result<()>;
```

### 3.2 Safety Manual Generator

```rust
pub struct SafetyManual {
    pub design_name: String,
    pub version: String,
    pub asil: AsilLevel,
    pub sections: Vec<SafetyManualSection>,
}

pub enum SafetyManualSection {
    Introduction { purpose: String, scope: String },
    AssumptionsOfUse(Vec<Assumption>),
    OperatingConditions(OperatingConditions),
    SafetyMechanisms(Vec<SmDescription>),
    KnownLimitations(Vec<Limitation>),
    MaintenanceRequirements(Vec<MaintenanceItem>),
}

pub fn generate_safety_manual(
    hierarchy: &SafetyHierarchy,
    metrics: &HardwareArchitecturalMetrics,
    hsi: Option<&HardwareSoftwareInterface>,
) -> SafetyManual;
```

---

## Implementation Order

| Step | Module | Effort | Dependencies |
|------|--------|--------|--------------|
| 1 | Safety attributes (frontend) | 2 weeks | None |
| 2 | MIR propagation | 1 week | Step 1 |
| 3 | LIR propagation | 2 weeks | Step 2 |
| 4 | HSI module | 3 weeks | Step 3 |
| 5 | Design rules | 2 weeks | Step 3 |
| 6 | Diversity analysis | 1 week | Step 5 |
| 7 | Tool qualification | 3 weeks | None (parallel) |
| 8 | Uncertainty | 2 weeks | None (parallel) |
| 9 | Dynamic FTA | 2 weeks | None (parallel) |
| 10 | SM verification | 3 weeks | Step 3 |
| 11 | Power domain completion | 2 weeks | Step 3 |
| 12 | Enhanced exports | 1 week | None |
| 13 | Safety manual | 1 week | Steps 4, 5 |

**Total Estimated Effort**: ~6 months

---

## Files Summary

| File | Action | Phase |
|------|--------|-------|
| `crates/skalp-frontend/src/safety_attributes.rs` | NEW | 1.1 |
| `crates/skalp-frontend/src/hir/mod.rs` | MODIFY | 1.1 |
| `crates/skalp-mir/src/types.rs` | MODIFY | 1.1 |
| `crates/skalp-mir/src/hir_to_mir.rs` | MODIFY | 1.1 |
| `crates/skalp-lir/src/mir_to_gate_netlist.rs` | MODIFY | 1.1 |
| `crates/skalp-safety/src/hsi.rs` | NEW | 1.2 |
| `crates/skalp-safety/src/design_rules.rs` | NEW | 1.3 |
| `crates/skalp-safety/src/diversity.rs` | NEW | 1.3 |
| `crates/skalp-safety/src/tool_qualification.rs` | NEW | 1.4 |
| `crates/skalp-safety/src/uncertainty.rs` | NEW | 2.1 |
| `crates/skalp-safety/src/fta.rs` | MODIFY | 2.2 |
| `crates/skalp-safety/src/sm_verification.rs` | NEW | 2.3 |
| `crates/skalp-safety/src/power_domains.rs` | MODIFY | 2.4 |
| `crates/skalp-safety/src/workproducts.rs` | MODIFY | 3.1 |
| `crates/skalp-safety/src/lib.rs` | MODIFY | All |
