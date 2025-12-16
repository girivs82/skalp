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

### FMEA/FMEDA Infrastructure ✅
- [x] Complete FMEA entry structure (failure modes, effects, detection)
- [x] Risk assessment (severity, occurrence, detection ratings)
- [x] System boundary analysis
- [x] Functional dependency tracking
- [x] Component-level failure mode database
- [x] FMEDA generation from gate netlist
- [x] Technology library failure mode mappings
- [x] Export to CSV for assessor review
- [x] Temperature derating support (reference_temperature, derating_factor)

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

#### 1. Safety Annotation Parser
**Location**: `skalp-frontend` (not yet implemented)
**Impact**: Requires manual annotation in gate netlist
**Current State**: Must set `CellSafetyClassification` manually
**Required**:
- Parser support for `#[safety_goal(...)]` attribute
- Parser support for `#[implements(goal, mechanism)]` attribute
- Propagation through HIR → MIR → LIR → Gate Netlist
- Validation of goal/mechanism references

### MEDIUM Priority

#### 2. Full Technology Derating Model
**Location**: `fmeda_library.rs`
**Impact**: FIT values have basic derating but no full Arrhenius model
**Current State**: Has `derating_factor` and `reference_temperature` fields
**Required**:
- Complete Arrhenius temperature acceleration model
- Voltage stress factors
- Process corner variation (±3σ)
- Typical/worst-case FIT bounds

#### 3. Export to Standard FTA Tools
**Location**: `fta.rs`
**Impact**: Cannot export to external tools
**Required**:
- OpenFTA format export
- Isograph format export
- Standard XML/JSON interchange formats

### LOW Priority

#### 4. Enhanced GSN Export
**Location**: `safety_case.rs`
**Impact**: Basic GSN structure exists but limited export
**Required**:
- ASCE tool format export
- Astah GSN format export
- Better evidence linking

#### 5. Comprehensive Validation Test Suite
**Location**: `tool_qualification.rs`
**Impact**: TCL documentation exists but limited test suite
**Required**:
- Known-answer tests for all metrics
- Regression test suite
- TCL3 qualification evidence package

---

## Implementation Plan

### Phase 1: Safety Annotation Parser (HIGH)

| Item | Effort | Files |
|------|--------|-------|
| 1.1 Parser support for `#[safety_goal()]` | Medium | `skalp-frontend/src/safety_attributes.rs` |
| 1.2 Parser support for `#[implements()]` | Medium | `skalp-frontend/src/hir_builder.rs` |
| 1.3 HIR → MIR propagation | Medium | `skalp-mir/src/hir_to_mir.rs` |
| 1.4 MIR → LIR propagation | Medium | `skalp-lir/src/mir_to_gate_netlist.rs` |
| 1.5 Validation and error reporting | Small | `skalp-safety/src/pipeline.rs` |

### Phase 2: Technology Derating (MEDIUM)

| Item | Effort | Files |
|------|--------|-------|
| 2.1 Arrhenius model implementation | Medium | `fmeda_library.rs` |
| 2.2 Voltage stress factors | Small | `fmeda_library.rs` |
| 2.3 Process corner variation | Small | `fmeda_library.rs` |

### Phase 3: Tool Integration (LOW)

| Item | Effort | Files |
|------|--------|-------|
| 3.1 OpenFTA export | Medium | `fta.rs` |
| 3.2 GSN tool export | Medium | `safety_case.rs` |
| 3.3 Validation test suite | Medium | `tool_qualification.rs` |

---

## Recently Completed (Phase 2)

The following items were completed in commit `29bfa69`:

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
