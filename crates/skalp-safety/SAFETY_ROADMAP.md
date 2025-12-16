# SKALP Safety Framework Roadmap

This document tracks the current state, limitations, and planned improvements for the SKALP safety analysis framework.

## Current Capabilities

### ISO 26262 Compliance ✅
- [x] ASIL A-D + QM level definitions with metric targets
- [x] SPFM (Single Point Fault Metric) calculation
- [x] LF (Latent Fault Metric) calculation - proper FMEA-based implementation
- [x] PMHF calculation with complete formula: `PMHF = λSPF + λRF + λSM + λDPF_CCF`
- [x] Safety mechanism failure analysis (λSM contribution)
- [x] SM-of-SM hierarchy with cycle detection
- [x] Cell-level safety classification in gate netlist
- [x] Diagnostic coverage verification per ASIL level
- [x] Detection timing validation (FDTI + FRTI < FTTI)
- [x] DFA/CCF integration into PMHF (λDPF_CCF)

### Fault Tree Analysis (FTA) ✅
- [x] Top-down fault tree construction
- [x] Static gate types: AND, OR, K-of-N, PAND, NOT, XOR
- [x] Dynamic gate types: SPARE, SEQ, FDEP, INHIBIT
- [x] BDD-based minimal cut set calculation
- [x] Probability propagation
- [x] Importance measures (Birnbaum, Fussell-Vesely)
- [x] ISO 26262 compliance checking
- [x] Export to standard tools (OpenFTA, Isograph, JSON, Galileo)

### FMEA/FMEDA Infrastructure ✅
- [x] Complete FMEA entry structure (failure modes, effects, detection)
- [x] Risk assessment (severity, occurrence, detection ratings)
- [x] System boundary analysis
- [x] Functional dependency tracking
- [x] Component-level failure mode database
- [x] FMEDA generation from gate netlist
- [x] Technology library failure mode mappings
- [x] Export to CSV for assessor review
- [x] Full technology derating model:
  - [x] Arrhenius temperature acceleration: `AF = exp(Ea/k * (1/T_ref - 1/T_use))`
  - [x] Voltage stress factors: `AF = (V/V_nom)^n`
  - [x] Process corner variation (±3σ)
  - [x] Typical/worst-case FIT bounds

### Safety Mechanisms ✅
- [x] SM types: Primary, Latent, Dual
- [x] SM categories: ECC, Redundancy, Monitoring, Isolation, FailSafe, BIST
- [x] Implementation tracking (FIT, cell counts, effectiveness)
- [x] Predefined mechanisms (ECC, Lockstep, Watchdog)
- [x] SM-to-protected logic mapping (fanin cone analysis)
- [x] Coverage metrics: protected_fit / total_functional_fit

### Dependent Failure Analysis ✅
- [x] CCF groups with beta factors
- [x] Shared resource identification (clock, reset, power, cell type)
- [x] Safety mechanism CCF cause (β=1.0)
- [x] FIT splitting (independent/common-cause)
- [x] Full DFA integration into PMHF

### Uncertainty Quantification ✅
- [x] Monte Carlo simulation for safety metrics
- [x] Multiple FIT distribution types (LogNormal, Exponential, Weibull, Normal, Uniform)
- [x] Confidence interval calculation
- [x] Sensitivity analysis with tornado charts
- [x] Convergence detection

### Power Domain Analysis ✅
- [x] Power domain boundary identification
- [x] Level shifter verification
- [x] Isolation cell verification
- [x] Cross-domain signal analysis
- [x] Cross-domain CCF groups
- [x] Safety-critical path analysis

### Design Verification ✅
- [x] Design rule checking
- [x] Diversity analysis (redundant channels)
- [x] Single point fault detection
- [x] SPF with safety mechanism protection

### Multi-Standard Support ✅
- [x] ISO 26262 (automotive)
- [x] IEC 61508 (industrial)
- [x] DO-254 (aerospace)

### Certification Support ✅
- [x] Work product generation (HSRS, FMEA reports)
- [x] GSN safety case structure
- [x] Tool qualification (TCL assessment)
- [x] HSI (Hardware-Software Interface) definition

---

## Remaining Gaps

### HIGH Priority

#### 1. Safety Annotation Parser (Complete ✅)
**Location**: `skalp-frontend`, `skalp-mir`, `skalp-lir`
**Current State**: Full pipeline propagation implemented and validated
**Completed**:
- [x] Parser support for `safety_goal` and `safety_entity` CST nodes
- [x] HIR builder processing for `SafetyGoalDecl` and `SafetyEntityDecl`
- [x] `build_safety_goal()`, `build_hsr()`, `process_safety_entity()` functions
- [x] HIR → MIR propagation of safety annotations (commit `49516ec`)
- [x] MIR → LIR propagation via `add_primitive_with_safety()`
- [x] MIR → WordLir → GateNetlist propagation via `add_cell()`
- [x] End-to-end validation tests (`test_safety_annotation_pipeline.rs`)
- [x] Fixed `TypeKw` token handling in `#[safety_mechanism]` attribute parsing
- [x] Fixed `from_lir_safety_info` to handle standalone safety mechanisms
**Remaining**:
- [ ] Error reporting for missing/invalid annotations (LOW priority)

### LOW Priority

#### 2. Enhanced GSN Export
**Location**: `safety_case.rs`
**Impact**: Basic GSN structure exists but limited export
**Required**:
- ASCE tool format export
- Astah GSN format export
- Better evidence linking

#### 3. Comprehensive Validation Test Suite
**Location**: `tool_qualification.rs`
**Impact**: TCL documentation exists but limited test suite
**Required**:
- Known-answer tests for all metrics
- Regression test suite
- TCL3 qualification evidence package

---

## Implementation Plan

### Phase 1: Safety Annotation Parser (Complete ✅)

| Item | Status | Files |
|------|--------|-------|
| 1.1 Parser support for `safety_goal` CST | ✅ | `skalp-frontend/src/parser.rs` |
| 1.2 HIR builder for safety annotations | ✅ | `skalp-frontend/src/hir_builder.rs` |
| 1.3 HIR → MIR propagation | ✅ | `skalp-mir/src/hir_to_mir.rs` |
| 1.4 MIR → LIR propagation | ✅ | `skalp-lir/src/mir_to_gate_netlist.rs`, `mir_to_word_lir.rs`, `tech_mapper.rs` |
| 1.5 End-to-end validation tests | ✅ | `tests/test_safety_annotation_pipeline.rs` |
| 1.6 Bug fixes (TypeKw, standalone SMs) | ✅ | `skalp-frontend/src/hir_builder.rs`, `skalp-lir/src/gate_netlist.rs` |

### Phase 2: Technology Derating (MEDIUM - Complete ✅)

| Item | Status | Files |
|------|--------|-------|
| 2.1 Arrhenius model implementation | ✅ | `fmeda_library.rs` |
| 2.2 Voltage stress factors | ✅ | `fmeda_library.rs` |
| 2.3 Process corner variation | ✅ | `fmeda_library.rs` |

### Phase 3: Tool Integration (LOW - Partially Complete)

| Item | Status | Files |
|------|--------|-------|
| 3.1 OpenFTA/Isograph/JSON export | ✅ | `fta.rs` |
| 3.2 GSN tool export | TODO | `safety_case.rs` |
| 3.3 Validation test suite | TODO | `tool_qualification.rs` |

---

## Recently Completed

### Commit `49516ec` - Safety Annotation Pipeline Propagation
1. **MIR → LIR Propagation** (`mir_to_gate_netlist.rs`)
   - `module_safety_context` field in transformer
   - `safety_context_to_lir_info()` conversion function
   - `apply_safety_info()` and `add_primitive_with_safety()` helpers
   - All primitives inherit module-level `SafetyContext`

2. **MIR → WordLir → GateNetlist Propagation**
   - `module_safety_info` field in `WordLir` struct (`word_lir.rs`)
   - Safety context propagation in `mir_to_word_lir.rs`
   - `CellSafetyClassification` applied in `tech_mapper.rs` `add_cell()`

### Commit `9e908f3` - FTA Export to Standard Tools
1. **FTA Export Formats** (`fta.rs`)
   - OpenFTA XML format export
   - Isograph Reliability Workbench XML format
   - Generic JSON interchange format with analysis results
   - Galileo text format for academic tools
   - `FtaExportFormat` enum with unified `export_fta()` function

### Commit `03fd2e4` - Technology Derating Model
1. **Full Arrhenius Derating** (`fmeda_library.rs`)
   - Temperature acceleration: `AF = exp(Ea/k * (1/T_ref - 1/T_use))`
   - Voltage stress factors: `AF = (V/V_nom)^n`
   - Process corner variation (±3σ)
   - Preset parameters (gate_oxide, electromigration, hot_carrier, soft_error)
   - `DeratingCalculator` with `fit_bounds()` for typical/worst-case

### Commit `c6687cd` - Safety Annotation Parser (HIR)
1. **HIR Builder Support** (`hir_builder.rs`)
   - `build_safety_goal()` for SafetyGoalDecl processing
   - `build_hsr()` for Hardware Safety Requirements
   - `process_safety_entity()` for entity declarations
   - ASIL level extraction and validation

### Commit `29bfa69` - Phase 2 Safety Features
1. **Uncertainty Quantification** (`uncertainty.rs`)
   - Monte Carlo simulation for PMHF, SPFM, LF
   - 5 distribution types (LogNormal, Exponential, Weibull, Normal, Uniform)
   - Confidence interval calculation
   - Sensitivity analysis with tornado charts

2. **Dynamic FTA Gates** (`fta.rs`)
   - SPARE gate (standby redundancy)
   - SEQ gate (sequence-enforcing)
   - FDEP gate (functional dependency)
   - INHIBIT gate (conditional failures)

3. **Power Domain Isolation Verification** (`power_domains.rs`)
   - `verify_isolation_cells()`
   - `verify_level_shifters()`
   - `analyze_cross_domain_signals()`

### End-to-End Validation Tests and Bug Fixes
1. **Validation Test Suite** (`tests/test_safety_annotation_pipeline.rs`)
   - 10 comprehensive tests for safety annotation pipeline
   - Tests HIR, MIR, WordLir, and GateNetlist propagation
   - Tests `CellSafetyClassification` on cells
   - Tests `ModuleSafetyDefinitions` container and validation

2. **Bug Fixes**
   - Fixed `TypeKw` token handling in `#[safety_mechanism]` attribute parsing
     - `type` was being tokenized as `TypeKw` instead of `Ident`
   - Fixed `from_lir_safety_info()` in `gate_netlist.rs`
     - Now handles standalone safety mechanisms (mechanism_name only, no goal_name)
     - Uses "unassigned" placeholder for goal_name when not specified

---

## Metric Targets Reference

### ISO 26262 Hardware Architectural Metrics

| ASIL | SPFM | LF | PMHF (FIT) |
|------|------|-----|------------|
| QM | - | - | - |
| A | ≥90% | ≥60% | <1000 |
| B | ≥90% | ≥80% | <100 |
| C | ≥97% | ≥80% | <100 |
| D | ≥99% | ≥90% | <10 |

### Diagnostic Coverage Targets

| ASIL | DC Target |
|------|-----------|
| QM | 0% |
| A | 90% |
| B | 90% |
| C | 97% |
| D | 99% |

### Beta Factors (CCF)

| Cause | Typical β |
|-------|-----------|
| Shared Clock | 0.07 |
| Shared Reset | 0.05 |
| Shared Power | 0.07 |
| Physical Proximity | 0.01 |
| Same Cell Type | 0.02 |
| Same Module | 0.03 |
| SM Failure | 1.00 |

---

## References

- ISO 26262:2018 Parts 1-12
- ISO 26262-5:2018 Product development at the hardware level
- ISO 26262-9:2018 ASIL-oriented and safety-oriented analyses
- IEC 61508:2010 Functional Safety
- DO-254:2000 Design Assurance Guidance for Airborne Electronic Hardware
