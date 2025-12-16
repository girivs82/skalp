# SKALP Safety Framework - Gaps Analysis and Implementation Roadmap

This document provides a comprehensive analysis of the current safety framework limitations
and a detailed implementation plan to achieve full ISO 26262 ASIL D certification support.

## Current State Summary

The skalp-safety framework currently implements:
- PMHF calculation with full formula (λSPF + λRF + λSM + λDPF_CCF)
- SPFM/LF metrics from FMEA MultiplePoint analysis
- Timing compliance validation (FDTI + FRTI < FTTI)
- FTA with BDD-based minimal cut set extraction
- FMEDA with cell-level failure distributions
- CCF analysis with beta factor modeling
- SM coverage mapping via fanin cone tracing
- Technology derating (Arrhenius, process corners, voltage)
- GSN safety case generation

**Current Certification Readiness: ASIL A/B**

---

## Part 1: Comprehensive Gap Analysis

### 1.1 Integration Gaps

#### GAP-INT-001: No Automatic Safety Annotation Parsing
**Severity**: HIGH (ASIL C/D Blocker)
**ISO 26262 Reference**: Part 5, Section 6.4.2 (Traceability)

**Current State**:
- CellSafetyClassification must be manually set on each cell
- No parsing of safety attributes from source code
- No automatic propagation through compilation pipeline

**Impact**:
- Manual classification is error-prone
- Traceability from requirements to implementation is weak
- Large designs become unmanageable

**Required Capability**:
```rust
// In source .sk file:
#[safety_goal("SG-001", asil = "D")]
#[safety_mechanism("TMR_Voter", dc = 99.0)]
component VotingLogic {
    // ...
}
```

---

#### GAP-INT-002: HIR/MIR → LIR Safety Propagation
**Severity**: HIGH (ASIL C/D Blocker)
**ISO 26262 Reference**: Part 5, Section 6.4.3 (Verification of traceability)

**Current State**:
- Safety annotations exist in HIR but don't flow to LIR
- Gate netlist cells lose association with safety requirements
- No automatic SM cell identification

**Impact**:
- Cannot automatically identify which cells implement safety mechanisms
- Manual mapping required between design and gate netlist
- Breaks traceability chain

---

#### GAP-INT-003: Fault Simulation Integration
**Severity**: MEDIUM
**ISO 26262 Reference**: Part 5, Section 7.4.4 (Fault injection testing)

**Current State**:
- `sim-integration` feature exists but is optional
- No automated fault campaign generation
- DC measurement requires manual test setup

**Impact**:
- Cannot automatically verify diagnostic coverage claims
- Manual fault injection is time-consuming
- May miss corner cases

---

### 1.2 Analysis Limitations

#### GAP-ANA-001: Simplified FTA (No Dynamic Gates)
**Severity**: MEDIUM
**ISO 26262 Reference**: Part 5, Annex D (Fault tree analysis)

**Current State**:
- Basic gates: AND, OR, K-of-N, NOT, XOR
- No dynamic gates: PAND (Priority AND), SPARE, SEQ, FDEP
- No temporal fault ordering

**Impact**:
- Cannot model time-dependent failure sequences
- Standby redundancy analysis limited
- Some fault trees cannot be accurately represented

**Missing Gates**:
| Gate | Purpose |
|------|---------|
| PAND | Priority AND - order matters |
| SPARE | Standby spares with switching |
| SEQ | Sequence enforcing |
| FDEP | Functional dependency |

---

#### GAP-ANA-002: No Uncertainty Quantification
**Severity**: MEDIUM
**ISO 26262 Reference**: Part 5, Section 8.4.6 (Confidence in FIT values)

**Current State**:
- Point estimates only for all metrics
- No confidence intervals
- No sensitivity analysis
- FIT values treated as exact

**Impact**:
- Cannot express uncertainty in PMHF
- Assessors may question confidence level
- No way to identify critical parameters

**Required Capability**:
```rust
pub struct UncertainMetric {
    pub mean: f64,
    pub std_dev: f64,
    pub confidence_interval_90: (f64, f64),
    pub confidence_interval_95: (f64, f64),
}
```

---

#### GAP-ANA-003: Power Domain Isolation Incomplete
**Severity**: MEDIUM
**ISO 26262 Reference**: Part 5, Section 7.4.11 (Power supply monitoring)

**Current State**:
- Power domain CCF groups created
- No isolation cell verification
- No level shifter checking
- No cross-domain signal analysis

**Impact**:
- Cannot verify power domain isolation
- May miss cross-domain failure propagation
- Incomplete DFA for power-related CCF

---

#### GAP-ANA-004: No Formal SM Verification
**Severity**: MEDIUM
**ISO 26262 Reference**: Part 5, Section 7.4.3 (Verification of safety mechanisms)

**Current State**:
- SM coverage claimed but not proven
- No formal verification of SM behavior
- Cannot prove SM detects specified faults

**Impact**:
- SM effectiveness is assumed, not verified
- May have implementation bugs in SM
- Cannot demonstrate SM correctness to assessors

---

#### GAP-ANA-005: No Markov Chain Analysis
**Severity**: LOW
**ISO 26262 Reference**: Part 5, Annex E (Markov models)

**Current State**:
- Static failure rate analysis only
- No state-based reliability modeling
- No repair rate consideration

**Impact**:
- Cannot model repairable systems
- Limited for availability analysis
- No transient vs permanent fault distinction

---

### 1.3 Missing ISO 26262 Modules

#### GAP-ISO-001: Hardware-Software Interface (HSI) Analysis
**Severity**: HIGH (ASIL C/D Blocker)
**ISO 26262 Reference**: Part 5, Section 6 (Hardware-software interface)

**Current State**:
- No HSI module exists
- No interface signal definition
- No HW/SW safety requirement allocation

**Required Capability**:
- Define HW/SW interface signals
- Allocate safety requirements to HW vs SW
- HSI-specific FMEA
- Interface timing requirements

---

#### GAP-ISO-002: Systematic Failure Coverage
**Severity**: HIGH (ASIL C/D Blocker)
**ISO 26262 Reference**: Part 5, Section 7 (Evaluation of the hardware architectural metrics)

**Current State**:
- Only random hardware failures analyzed
- No systematic failure prevention evidence
- No diversity verification

**Impact**:
- Cannot claim ASIL C/D without systematic failure coverage
- Assessors will require evidence of design diversity
- Common mode failure prevention not demonstrated

**Required Evidence**:
- Design rule checking results
- Diversity analysis (different algorithms, implementations)
- Independence arguments

---

#### GAP-ISO-003: Production/Operation Phase
**Severity**: MEDIUM
**ISO 26262 Reference**: Part 7 (Production and operation)

**Current State**:
- No production phase consideration
- No field return analysis
- No operational profile modeling

**Impact**:
- Safety case incomplete for full lifecycle
- Cannot track field failures
- No feedback loop to design

---

#### GAP-ISO-004: Confirmation Measures
**Severity**: MEDIUM
**ISO 26262 Reference**: Part 8 (Supporting processes)

**Current State**:
- No review/audit evidence generation
- No confirmation review checklists
- No assessment evidence packaging

**Impact**:
- Manual work for assessor reviews
- No structured evidence for audits
- Certification documentation incomplete

---

### 1.4 Tool Qualification Gaps

#### GAP-TQ-001: No Tool Qualification Package
**Severity**: HIGH (Required for any certification)
**ISO 26262 Reference**: Part 8, Section 11 (Qualification of software tools)

**Current State**:
- No TCL (Tool Confidence Level) assessment
- No tool validation test suite
- No tool qualification report

**Impact**:
- Tool output cannot be trusted for certification
- Assessor will require tool qualification evidence
- May need to manually verify all tool outputs

**Required for TCL2/TCL3**:
- Tool validation plan
- Tool validation test cases
- Tool validation report
- Known limitations documentation

---

### 1.5 Reporting and Export Gaps

#### GAP-RPT-001: Limited Export Formats
**Severity**: LOW
**ISO 26262 Reference**: Part 8, Section 9 (Documentation)

**Current State**:
- FMEDA CSV export exists
- GSN exports (YAML, JSON, DOT, Markdown)
- No Excel/XLSX export for FMEA
- No PDF generation

**Impact**:
- Manual formatting for assessor deliverables
- Integration with existing tools limited

---

#### GAP-RPT-002: No Safety Manual Generation
**Severity**: MEDIUM
**ISO 26262 Reference**: Part 5, Section 9 (Hardware safety requirements specification)

**Current State**:
- No automated safety manual template
- No assumption of use documentation
- No operating conditions documentation

**Impact**:
- Manual safety manual creation
- May miss required content
- Inconsistent documentation

---

## Part 2: Implementation Roadmap

### Phase 1: ASIL C/D Enablers (Critical Path)

#### 1.1 Safety Annotation Flow (4-6 weeks)

**Step 1.1.1: Define Safety Attributes in Frontend**
```rust
// New file: crates/skalp-frontend/src/safety_attributes.rs

#[derive(Debug, Clone)]
pub enum SafetyAttribute {
    SafetyGoal {
        id: String,
        asil: AsilLevel,
        description: Option<String>,
    },
    Implements {
        goal_id: String,
    },
    SafetyMechanism {
        name: String,
        mechanism_type: MechanismType,
        dc_target: f64,
    },
    SafetyCritical {
        reason: String,
    },
    FaultTolerantTime {
        ftti_ns: u64,
    },
}
```

**Step 1.1.2: Parse Attributes in HIR**
- Extend attribute parsing in `crates/skalp-frontend/src/hir/attributes.rs`
- Store safety attributes on HIR nodes
- Validate attribute combinations

**Step 1.1.3: Propagate to MIR**
- Add `safety_info: Option<SafetyInfo>` to MIR Module/Signal/Component
- Transfer attributes during HIR → MIR lowering
- Maintain traceability map

**Step 1.1.4: Propagate to LIR**
- Add `source_safety_info` to Primitive/Cell
- During MIR → LIR lowering, propagate safety info
- Auto-set CellSafetyClassification based on attributes

**Step 1.1.5: Integration Test**
- End-to-end test: `.sk` file with attributes → classified gate netlist
- Verify traceability preservation

**Files to Modify/Create**:
| File | Action |
|------|--------|
| `crates/skalp-frontend/src/safety_attributes.rs` | NEW |
| `crates/skalp-frontend/src/hir/attributes.rs` | MODIFY |
| `crates/skalp-mir/src/types.rs` | MODIFY |
| `crates/skalp-mir/src/hir_to_mir.rs` | MODIFY |
| `crates/skalp-lir/src/mir_to_gate_netlist.rs` | MODIFY |
| `crates/skalp-lir/src/gate_netlist.rs` | MODIFY |

---

#### 1.2 HSI Analysis Module (3-4 weeks)

**Step 1.2.1: Define HSI Data Structures**
```rust
// New file: crates/skalp-safety/src/hsi.rs

pub struct HardwareSoftwareInterface {
    pub signals: Vec<HsiSignal>,
    pub requirements: Vec<HsiRequirement>,
    pub timing: Vec<HsiTiming>,
}

pub struct HsiSignal {
    pub name: String,
    pub direction: HsiDirection,  // HwToSw, SwToHw, Bidirectional
    pub width: u32,
    pub safety_classification: HsiSafetyClass,
    pub timing_requirements: Option<HsiTiming>,
}

pub enum HsiSafetyClass {
    SafetyRelevant { asil: AsilLevel },
    NotSafetyRelevant,
}

pub struct HsiRequirement {
    pub id: String,
    pub description: String,
    pub allocated_to: HsiAllocation,  // Hardware, Software, Both
    pub verification_method: Vec<VerificationMethod>,
}
```

**Step 1.2.2: HSI FMEA Generation**
```rust
pub fn generate_hsi_fmea(hsi: &HardwareSoftwareInterface) -> Vec<FmeaEntry> {
    // For each HSI signal, generate failure modes:
    // - Stuck-at-0, Stuck-at-1, Timing violation, Data corruption
    // - Analyze effect on HW and SW sides
}
```

**Step 1.2.3: HSI Timing Analysis**
```rust
pub struct HsiTimingAnalysis {
    pub signal: String,
    pub hw_timing: TimingBudget,
    pub sw_timing: TimingBudget,
    pub margin: i64,
    pub compliant: bool,
}
```

**Step 1.2.4: Integration with Safety Hierarchy**
- Link HSI requirements to safety goals
- Add HSI coverage to metrics calculation

---

#### 1.3 Systematic Failure Coverage (2-3 weeks)

**Step 1.3.1: Design Rule Checker**
```rust
// New file: crates/skalp-safety/src/design_rules.rs

pub struct DesignRuleChecker {
    pub rules: Vec<SafetyDesignRule>,
}

pub enum SafetyDesignRule {
    NoSinglePointOfFailure,
    RedundancyRequired { min_copies: u32 },
    DiversityRequired { aspects: Vec<DiversityAspect> },
    IndependenceRequired { separation: SeparationType },
    WatchdogRequired,
    VotingRequired { voters: u32 },
}

pub fn check_design_rules(
    netlist: &GateNetlist,
    hierarchy: &SafetyHierarchy,
) -> DesignRuleReport;
```

**Step 1.3.2: Diversity Analysis**
```rust
pub struct DiversityAnalysis {
    pub redundant_elements: Vec<RedundantPair>,
    pub diversity_score: f64,
    pub common_elements: Vec<String>,  // Violations
}

pub enum DiversityAspect {
    DifferentAlgorithm,
    DifferentImplementation,
    DifferentTechnology,
    DifferentTeam,
    DifferentTool,
}
```

**Step 1.3.3: Independence Arguments**
```rust
pub struct IndependenceArgument {
    pub element_a: String,
    pub element_b: String,
    pub independence_type: IndependenceType,
    pub evidence: Vec<String>,
    pub valid: bool,
}
```

---

### Phase 2: Enhanced Analysis (Medium Priority)

#### 2.1 Uncertainty Quantification (2-3 weeks)

**Step 2.1.1: Monte Carlo Framework**
```rust
// New file: crates/skalp-safety/src/uncertainty.rs

pub struct MonteCarloConfig {
    pub iterations: u32,        // Default: 10000
    pub confidence_level: f64,  // Default: 0.95
    pub seed: Option<u64>,
}

pub struct UncertainFit {
    pub nominal: f64,
    pub distribution: FitDistribution,
}

pub enum FitDistribution {
    LogNormal { mu: f64, sigma: f64 },
    Exponential { lambda: f64 },
    Weibull { shape: f64, scale: f64 },
    Fixed(f64),
}

pub fn monte_carlo_pmhf(
    fmea: &FmeaAnalysis,
    fit_distributions: &HashMap<String, UncertainFit>,
    config: &MonteCarloConfig,
) -> UncertainMetric;
```

**Step 2.1.2: Sensitivity Analysis**
```rust
pub struct SensitivityAnalysis {
    pub parameter_sensitivities: HashMap<String, f64>,
    pub critical_parameters: Vec<String>,  // Top contributors
    pub tornado_chart_data: Vec<TornadoEntry>,
}

pub fn analyze_sensitivity(
    fmea: &FmeaAnalysis,
    target_metric: MetricType,
) -> SensitivityAnalysis;
```

---

#### 2.2 Dynamic FTA Gates (2-3 weeks)

**Step 2.2.1: Extend Gate Types**
```rust
pub enum DynamicGateType {
    PAND,           // Priority AND
    SPARE {         // Standby spare
        primary: FtaNodeId,
        spares: Vec<FtaNodeId>,
        switching_probability: f64,
    },
    SEQ,            // Sequence enforcer
    FDEP {          // Functional dependency
        trigger: FtaNodeId,
        dependents: Vec<FtaNodeId>,
    },
}
```

**Step 2.2.2: Temporal BDD Extension**
```rust
pub struct TemporalBdd {
    pub static_bdd: Bdd,
    pub temporal_constraints: Vec<TemporalConstraint>,
}

pub struct TemporalConstraint {
    pub event_a: FtaNodeId,
    pub event_b: FtaNodeId,
    pub relation: TemporalRelation,  // Before, After, Simultaneous
}
```

---

#### 2.3 Formal SM Verification (3-4 weeks)

**Step 2.3.1: SM Property Specification**
```rust
// New file: crates/skalp-safety/src/sm_verification.rs

pub struct SmProperty {
    pub name: String,
    pub property_type: SmPropertyType,
    pub specification: String,  // PSL or SVA format
}

pub enum SmPropertyType {
    DetectsStuckAt { signal: String, value: bool },
    DetectsTransient { signal: String },
    ResponseTime { max_cycles: u32 },
    Coverage { fault_type: FaultType, coverage: f64 },
}
```

**Step 2.3.2: Integration with skalp-verify**
```rust
pub fn verify_sm_properties(
    sm: &SafetyMechanism,
    netlist: &GateNetlist,
    properties: &[SmProperty],
) -> SmVerificationResult;
```

---

#### 2.4 Power Domain Completion (2 weeks)

**Step 2.4.1: Isolation Cell Verification**
```rust
pub fn verify_isolation_cells(
    netlist: &GateNetlist,
    domains: &[PowerDomainInfo],
) -> IsolationVerificationResult;

pub struct IsolationVerificationResult {
    pub verified_boundaries: Vec<String>,
    pub missing_isolation: Vec<CrossingViolation>,
    pub level_shifter_issues: Vec<LevelShifterIssue>,
}
```

**Step 2.4.2: Cross-Domain Signal Analysis**
```rust
pub fn analyze_cross_domain_signals(
    netlist: &GateNetlist,
    domains: &[PowerDomainInfo],
) -> CrossDomainAnalysis;
```

---

### Phase 3: Certification Support

#### 3.1 Tool Qualification Package (3-4 weeks)

**Step 3.1.1: TCL Assessment**
```rust
// New file: crates/skalp-safety/src/tool_qualification.rs

pub struct TclAssessment {
    pub tool_name: String,
    pub tool_version: String,
    pub use_cases: Vec<ToolUseCase>,
    pub tcl: ToolConfidenceLevel,
    pub required_qualification: QualificationMethod,
}

pub enum ToolConfidenceLevel {
    TCL1,  // Low confidence, high possibility of error
    TCL2,  // Medium
    TCL3,  // High confidence
}
```

**Step 3.1.2: Validation Test Suite**
```rust
pub struct ValidationTestSuite {
    pub test_cases: Vec<ValidationTestCase>,
    pub coverage_analysis: TestCoverageAnalysis,
}

pub struct ValidationTestCase {
    pub id: String,
    pub description: String,
    pub input: TestInput,
    pub expected_output: ExpectedOutput,
    pub actual_output: Option<ActualOutput>,
    pub status: TestStatus,
}
```

**Step 3.1.3: Known Limitations Documentation**
- Document all known limitations
- Workarounds and mitigations
- User guidance

---

#### 3.2 Enhanced Reporting (2 weeks)

**Step 3.2.1: Excel/XLSX Export**
```rust
pub fn export_fmea_xlsx(fmea: &FmeaAnalysis, path: &Path) -> io::Result<()>;
pub fn export_fmeda_xlsx(fmeda: &FmedaReport, path: &Path) -> io::Result<()>;
```

**Step 3.2.2: Safety Manual Generator**
```rust
pub struct SafetyManual {
    pub design_name: String,
    pub asil: AsilLevel,
    pub assumptions_of_use: Vec<Assumption>,
    pub operating_conditions: OperatingConditions,
    pub safety_mechanisms: Vec<SmDescription>,
    pub known_limitations: Vec<Limitation>,
}

pub fn generate_safety_manual(
    hierarchy: &SafetyHierarchy,
    metrics: &HardwareArchitecturalMetrics,
) -> SafetyManual;
```

---

#### 3.3 Confirmation Review Support (2 weeks)

**Step 3.3.1: Review Checklists**
```rust
pub struct ReviewChecklist {
    pub review_type: ReviewType,
    pub items: Vec<ChecklistItem>,
    pub status: ChecklistStatus,
}

pub enum ReviewType {
    ConceptPhaseReview,
    HardwareDesignReview,
    HardwareIntegrationReview,
    SafetyValidationReview,
    FunctionalSafetyAssessment,
}
```

**Step 3.3.2: Assessment Package Generator**
```rust
pub fn generate_assessment_package(
    hierarchy: &SafetyHierarchy,
    metrics: &SafetyMetrics,
    fta: &CutSetAnalysis,
    fmea: &FmeaAnalysis,
) -> AssessmentPackage;
```

---

## Part 3: Implementation Schedule

### Timeline Overview

```
Month 1-2: Phase 1.1 (Safety Annotation Flow)
  Week 1-2: Frontend attribute parsing
  Week 3-4: MIR propagation
  Week 5-6: LIR propagation + integration tests

Month 2-3: Phase 1.2 (HSI Analysis)
  Week 7-8: HSI data structures
  Week 9-10: HSI FMEA + timing

Month 3: Phase 1.3 (Systematic Failure Coverage)
  Week 11-12: Design rule checker
  Week 13: Diversity analysis

Month 4: Phase 2.1-2.2 (Uncertainty + Dynamic FTA)
  Week 14-15: Monte Carlo framework
  Week 16-17: Dynamic FTA gates

Month 5: Phase 2.3-2.4 (SM Verification + Power Domains)
  Week 18-19: SM property verification
  Week 20-21: Power domain completion

Month 6: Phase 3 (Certification Support)
  Week 22-23: Tool qualification package
  Week 24-25: Enhanced reporting
  Week 26: Confirmation review support
```

### Milestone Summary

| Milestone | Target | Deliverable |
|-----------|--------|-------------|
| M1: Annotation Flow | Month 2 | Auto-classified gate netlist |
| M2: HSI Complete | Month 3 | HSI analysis module |
| M3: ASIL C/D Ready | Month 3 | Systematic failure coverage |
| M4: Enhanced Analysis | Month 5 | Uncertainty + formal verification |
| M5: Certification Ready | Month 6 | Full tool qualification |

---

## Part 4: Priority Matrix

### Must Have (ASIL C/D Certification)

| Item | Gap ID | Effort | Impact |
|------|--------|--------|--------|
| Safety annotation flow | GAP-INT-001, GAP-INT-002 | 6 weeks | Critical |
| HSI analysis | GAP-ISO-001 | 4 weeks | Critical |
| Systematic failure coverage | GAP-ISO-002 | 3 weeks | Critical |
| Tool qualification | GAP-TQ-001 | 4 weeks | Critical |

### Should Have (Enhanced Quality)

| Item | Gap ID | Effort | Impact |
|------|--------|--------|--------|
| Uncertainty quantification | GAP-ANA-002 | 3 weeks | High |
| Formal SM verification | GAP-ANA-004 | 4 weeks | High |
| Power domain completion | GAP-ANA-003 | 2 weeks | Medium |

### Nice to Have (Complete Solution)

| Item | Gap ID | Effort | Impact |
|------|--------|--------|--------|
| Dynamic FTA gates | GAP-ANA-001 | 3 weeks | Medium |
| Markov analysis | GAP-ANA-005 | 4 weeks | Low |
| Enhanced exports | GAP-RPT-001 | 2 weeks | Low |
| Safety manual | GAP-RPT-002 | 2 weeks | Medium |

---

## Part 5: Risk Assessment

### Technical Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Annotation propagation complexity | Medium | High | Incremental implementation with tests |
| skalp-verify integration issues | Medium | Medium | Early prototype integration |
| Performance with uncertainty analysis | Low | Medium | Optimize Monte Carlo, parallel execution |

### Schedule Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Scope creep in HSI | Medium | Medium | Fixed scope definition upfront |
| Tool qualification overhead | High | Medium | Reuse existing test infrastructure |
| Integration testing delays | Medium | High | Continuous integration from start |

---

## Appendix A: File Structure After Implementation

```
crates/skalp-safety/src/
├── lib.rs
├── analysis.rs
├── asil.rs
├── common_cause.rs
├── coverage_verification.rs
├── design_resolver.rs
├── design_rules.rs          # NEW: Phase 1.3
├── diversity.rs             # NEW: Phase 1.3
├── do254.rs
├── fault_simulation.rs
├── fmea.rs
├── fmeda.rs
├── fmeda_library.rs
├── fta.rs
├── gate_netlist_integration.rs
├── hierarchy.rs
├── hsi.rs                   # NEW: Phase 1.2
├── iec61508.rs
├── mechanisms.rs
├── metrics.rs
├── pipeline.rs
├── power_domains.rs
├── requirements.rs
├── safety_case.rs
├── sm_failure_analysis.rs
├── sm_verification.rs       # NEW: Phase 2.3
├── tool_qualification.rs    # NEW: Phase 3.1
├── traits.rs
├── uncertainty.rs           # NEW: Phase 2.1
└── workproducts.rs

crates/skalp-frontend/src/
├── safety_attributes.rs     # NEW: Phase 1.1
└── hir/
    └── attributes.rs        # MODIFY: Phase 1.1
```

---

## Appendix B: Test Requirements

### Phase 1 Tests

1. **Annotation Flow Tests**
   - Parse all safety attribute types
   - Propagate through HIR → MIR → LIR
   - Verify CellSafetyClassification auto-set
   - Traceability preservation

2. **HSI Tests**
   - HSI signal definition
   - HSI FMEA generation
   - Timing analysis
   - Requirement allocation

3. **Systematic Coverage Tests**
   - Design rule violations detected
   - Diversity analysis correct
   - Independence arguments valid

### Phase 2 Tests

4. **Uncertainty Tests**
   - Monte Carlo convergence
   - Confidence interval accuracy
   - Sensitivity ranking correct

5. **Dynamic FTA Tests**
   - PAND gate semantics
   - SPARE gate switching
   - Temporal constraint handling

6. **SM Verification Tests**
   - Property specification parsing
   - Integration with skalp-verify
   - Coverage claims validated

### Phase 3 Tests

7. **Tool Qualification Tests**
   - TCL assessment accuracy
   - Validation test execution
   - Report generation

8. **Export Tests**
   - XLSX format compliance
   - Safety manual completeness
   - Assessment package structure

---

## Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2024-12-16 | Claude/SKALP Team | Initial comprehensive gap analysis |
