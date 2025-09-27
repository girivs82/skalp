# FMEA Architecture - Intent-Driven Failure Analysis

## Overview

SKALP revolutionizes FMEA (Failure Mode and Effects Analysis) by using intent declarations as semantic anchors for automated failure effect analysis. Unlike traditional FMEA which requires manual annotation of every gate/flop, SKALP's compiler automatically decomposes module-level intent to gate-level failure effects through sophisticated program analysis.

## Core Innovation: Intent as FMEA Annotation

Traditional FMEA requires engineers to manually annotate failure effects for thousands of gates. SKALP eliminates this by:

1. **Intent declarations provide the human insight** - Engineers declare what the module should do
2. **Compiler traces failures to intent violations** - Automatic analysis determines how gate failures violate intent
3. **Hierarchical decomposition maintains precision** - Gate-level granularity with module-level semantics

## Architecture Components

### 1. Intent Declaration System

Intent declarations serve as the semantic foundation for FMEA:

```rust
entity SafetyController with intent {
    // Primary safety intents
    hazards: {
        unintended_actuation: "Output activates without valid input",
        loss_of_function: "No output when valid input present",
        timing_violation: "Response time exceeds safety threshold",
    },

    // Severity classification (maps to ISO 26262 severity levels)
    failure_impact: {
        unintended_actuation: catastrophic,  // S3 - potential injury
        loss_of_function: critical,          // S3 - loss of safety
        timing_violation: moderate,           // S2 - degraded operation
    },

    // Detection hints guide compiler analysis
    detection_hints: {
        unintended_actuation: "Cross-check output vs input state machine",
        loss_of_function: "Monitor critical path availability",
        timing_violation: "Track propagation delay through pipeline",
    },

    // Quantitative constraints
    timing_requirements: {
        max_response_time: 10ms,
        min_hold_time: 100us,
    },
}
```

### 2. Multi-Level Decomposition Engine

The compiler performs hierarchical decomposition from intent to gates:

```rust
pub struct FMEADecompositionEngine {
    // Three-level hierarchy
    intent_level: IntentAnalyzer,      // Module-level semantics
    path_level: PathAnalyzer,          // Critical path identification
    gate_level: GateAnalyzer,          // Individual gate failures

    pub fn decompose(&mut self, entity: &Entity) -> HierarchicalFMEA {
        // Step 1: Extract and parse intent
        let intents = self.intent_level.extract_safety_intents(entity);

        // Step 2: Synthesize to gate-level netlist
        let netlist = self.synthesize_with_tracking(entity);

        // Step 3: Build cone of influence for each gate
        let influence_map = self.build_influence_cones(&netlist);

        // Step 4: Trace failures through hierarchy
        let mut fmea = HierarchicalFMEA::new();

        for gate in netlist.gates() {
            // Get cone of influence
            let cone = &influence_map[&gate];

            // Determine which intents this gate can affect
            let affected_intents = self.trace_gate_to_intents(gate, cone, &intents);

            // For each affected intent, analyze failure propagation
            for intent in affected_intents {
                let failure_analysis = self.analyze_failure_propagation(
                    gate, intent, cone
                );

                fmea.add_failure_chain(gate, intent, failure_analysis);
            }
        }

        fmea
    }
}
```

### 3. Cone of Influence Analysis

Precise tracing from gates to module outputs:

```rust
pub struct ConeOfInfluence {
    root_gate: GateId,
    fanout_tree: HashMap<GateId, Vec<GateId>>,
    affected_outputs: Vec<OutputId>,
    logic_depth: Vec<Vec<GateId>>,  // Topologically sorted levels
    critical_paths: Vec<CriticalPath>,

    pub fn trace_failure(&self, failure_mode: FailureMode) -> FailurePropagation {
        let mut propagation = FailurePropagation::new(self.root_gate, failure_mode);

        // BFS through fanout tree
        let mut frontier = vec![(self.root_gate, failure_mode)];
        let mut visited = HashSet::new();

        while let Some((gate, mode)) = frontier.pop() {
            visited.insert(gate);

            // Analyze how this gate transforms the failure
            let gate_behavior = self.analyze_gate_failure(gate, mode);
            propagation.add_gate_effect(gate, gate_behavior);

            // Propagate to fanout
            for &fanout_gate in &self.fanout_tree[&gate] {
                if !visited.contains(&fanout_gate) {
                    let propagated_mode = gate_behavior.output_failure_mode();
                    frontier.push((fanout_gate, propagated_mode));
                }
            }
        }

        // Determine output effects
        for output in &self.affected_outputs {
            if propagation.reaches(output) {
                propagation.mark_output_corrupted(output);
            }
        }

        propagation
    }
}
```

### 4. Symbolic Failure Simulation

Determine intent violations through symbolic execution:

```rust
pub struct SymbolicFailureSimulator {
    symbolic_engine: SymbolicExecutor,
    intent_checker: IntentVerifier,

    pub fn simulate_failure(
        &mut self,
        gate: GateId,
        failure_mode: FailureMode,
        intent: &SafetyIntent,
        cone: &ConeOfInfluence,
    ) -> IntentViolation {
        // Create symbolic state
        let mut state = SymbolicState::new();

        // Inject failure at gate
        state.inject_fault(gate, failure_mode);

        // Symbolically execute through cone
        for level in cone.logic_depth.iter() {
            for &gate_id in level {
                let gate_logic = cone.get_gate_logic(gate_id);
                state.execute_gate(gate_id, gate_logic);
            }
        }

        // Check intent properties
        let violation = self.intent_checker.check_violation(&state, intent);

        IntentViolation {
            violated_intent: intent.clone(),
            failure_cause: (gate, failure_mode),
            propagation_path: state.get_execution_trace(),
            severity: intent.failure_impact,
            detection_difficulty: self.estimate_detection_difficulty(&state),
        }
    }
}
```

### 5. Automatic Detection Generation

Generate detection mechanisms from intent:

```rust
pub struct DetectionGenerator {
    pub fn generate_from_intent(
        &self,
        intent: &SafetyIntent,
        fmea: &HierarchicalFMEA,
    ) -> DetectionScheme {
        let mut scheme = DetectionScheme::new();

        // Analyze critical paths for this intent
        let critical_paths = fmea.get_critical_paths_for_intent(intent);

        // Generate assertions from intent properties
        let assertions = self.intent_to_assertions(intent);

        // Find optimal detection points
        for path in critical_paths {
            // Find minimum cut set for detection
            let cut_points = self.find_minimum_cut(path);

            for point in cut_points {
                let detector = DetectionMechanism {
                    location: point,
                    detection_type: self.select_detection_type(point, intent),
                    coverage: self.calculate_coverage(point, path),
                    latency: self.calculate_detection_latency(point, path),
                    implementation: self.generate_detection_logic(point, assertions),
                };

                scheme.add_detector(detector);
            }
        }

        scheme
    }
}
```

### 6. Hierarchical FMEA Structure

Three-level analysis maintains both precision and semantics:

```rust
pub struct HierarchicalFMEA {
    // Level 1: Gate-level failures
    gate_failures: HashMap<GateId, Vec<FailureMode>>,

    // Level 2: Path-level propagation
    path_analysis: HashMap<PathId, PathFailureEffect>,

    // Level 3: Intent-level violations
    intent_violations: HashMap<IntentId, Vec<Violation>>,

    // Cross-level traceability
    gate_to_paths: HashMap<GateId, HashSet<PathId>>,
    path_to_intents: HashMap<PathId, HashSet<IntentId>>,
    gate_to_intents: HashMap<GateId, HashSet<IntentId>>,

    pub fn analyze_gate_failure(&self, gate: GateId) -> CompleteFMEA {
        CompleteFMEA {
            // Local effects (gate-level)
            local_effect: self.gate_failures[&gate].clone(),

            // Path effects (system-level)
            affected_paths: self.gate_to_paths[&gate]
                .iter()
                .map(|p| &self.path_analysis[p])
                .collect(),

            // Intent violations (safety-level)
            violated_intents: self.gate_to_intents[&gate]
                .iter()
                .map(|i| &self.intent_violations[i])
                .collect(),

            // Calculate Risk Priority Number
            rpn: self.calculate_rpn(gate),
        }
    }
}
```

## Key Algorithms

### 1. Intent-to-Gate Tracing Algorithm

```rust
impl IntentTracer {
    pub fn trace_intent_to_gates(&self, intent: &Intent, netlist: &Netlist) -> Vec<GateId> {
        let mut relevant_gates = Vec::new();

        // Start from outputs mentioned in intent
        let intent_outputs = self.extract_output_refs(intent);

        // Backward cone of logic from outputs
        for output in intent_outputs {
            let cone = self.backward_cone(output, netlist);
            relevant_gates.extend(cone);
        }

        // Filter by criticality
        relevant_gates.retain(|g| self.is_critical_for_intent(g, intent));

        relevant_gates
    }
}
```

### 2. Failure Propagation Algorithm

```rust
impl FailurePropagator {
    pub fn propagate(&self, gate: GateId, mode: FailureMode) -> PropagationTree {
        let mut tree = PropagationTree::new(gate, mode);

        // Build propagation tree
        self.build_tree_recursive(&mut tree, gate, mode);

        // Prune non-observable failures
        tree.prune_unobservable();

        // Annotate with probabilities
        tree.calculate_propagation_probability();

        tree
    }
}
```

### 3. RPN Calculation with Intent

```rust
impl RPNCalculator {
    pub fn calculate_with_intent(
        &self,
        failure: &GateFailure,
        intent_violation: &IntentViolation,
    ) -> u32 {
        // Severity from intent
        let severity = match intent_violation.severity {
            Catastrophic => 10,
            Critical => 8,
            Moderate => 5,
            Minor => 2,
        };

        // Occurrence from failure rate
        let occurrence = self.failure_rate_to_occurrence(failure.base_rate);

        // Detection from analysis
        let detection = match intent_violation.detection_difficulty {
            Easy => 2,      // Multiple detection points
            Moderate => 5,  // Single detection point
            Difficult => 8, // No direct detection
            None => 10,     // Undetectable
        };

        severity * occurrence * detection
    }
}
```

## Integration Points

### 1. With Safety Requirements

FMEA automatically considers PSM/LSM coverage:

```rust
// PSM reduces detection difficulty
if let Some(psm) = self.find_covering_psm(gate) {
    failure.detection = psm.diagnostic_coverage;
}

// LSM provides latent fault detection
if let Some(lsm) = self.find_covering_lsm(psm) {
    failure.latent_detection = lsm.coverage;
}
```

### 2. With Simulation

FMEA results guide fault injection campaigns:

```rust
// Use FMEA to prioritize fault injection
let critical_failures = fmea.get_high_rpn_failures();
for failure in critical_failures {
    fault_campaign.add_targeted_fault(failure.gate, failure.mode);
}
```

### 3. With Synthesis

Synthesis preserves FMEA traceability:

```rust
// Tag gates with FMEA metadata during synthesis
for gate in synthesized_netlist.gates() {
    gate.add_metadata(FMEAMetadata {
        affects_intents: fmea.get_affected_intents(gate),
        criticality: fmea.get_criticality(gate),
        detection_points: fmea.get_detection_points(gate),
    });
}
```

## Compile-Time Verification

FMEA completeness checked during compilation:

```rust
impl Compiler {
    pub fn verify_fmea_coverage(&self, design: &Design) -> Result<()> {
        let fmea = self.generate_fmea(design);

        // Check all hazards covered
        for hazard in design.get_declared_hazards() {
            if !fmea.has_analysis_for(hazard) {
                return Err(format!("Hazard {} not covered by FMEA", hazard));
            }
        }

        // Check high RPN items have mitigation
        for failure in fmea.get_critical_failures() {
            if !design.has_mitigation_for(failure) {
                return Err(format!("Critical failure {} lacks mitigation", failure));
            }
        }

        // Check detection coverage
        let coverage = fmea.calculate_detection_coverage();
        if coverage < design.required_coverage {
            return Err(format!("Detection coverage {}% below required {}%",
                coverage, design.required_coverage));
        }

        Ok(())
    }
}
```

## Benefits Over Traditional FMEA

1. **No Manual Gate Annotation** - Intent provides semantic context automatically
2. **Precise Propagation Analysis** - Cone of influence traces exact failure paths
3. **Automatic Detection Generation** - Compiler places detection optimally
4. **Hierarchical Views** - Different stakeholders see appropriate granularity
5. **Integrated with Safety** - PSM/LSM mechanisms automatically considered
6. **Compile-Time Verification** - FMEA completeness guaranteed before synthesis
7. **Traceable Through Flow** - FMEA metadata preserved through all compilation stages

## Example: Complete FMEA Flow

```rust
// 1. Engineer declares intent
entity BrakeController with intent {
    hazards: {
        unintended_braking: "Brake without pedal",
    },
    failure_impact: {
        unintended_braking: catastrophic,
    },
}

// 2. Compiler generates FMEA automatically
let fmea = compiler.generate_fmea(brake_controller);

// 3. Results show gate-level detail with intent context
FMEA Report:
  Gate G234 (AND gate, path: pedal_logic):
    Failure Mode: Stuck-at-1
    Local Effect: Output always high
    System Effect: Unintended braking (violates intent)
    Severity: 10 (Catastrophic - from intent)
    Occurrence: 4 (10^-6 failures/hour)
    Detection: 3 (PSM coverage 99%)
    RPN: 120

// 4. Automatic detection inserted
Detection Point DP1:
  Location: After G234
  Type: Dual-modular comparison
  Coverage: 99%
  Latency: 1 cycle
```

## FMEDA Extension - Design Data Integration

FMEDA extends FMEA by associating each failure mode with precise design data elements, enabling quantitative safety metrics calculation for ISO 26262 compliance.

### Automatic Design Data Association

The compiler automatically extracts design metrics during synthesis and associates them with FMEA results:

```rust
pub struct FMEDA {
    fmea: HierarchicalFMEA,
    design_data: DesignMetrics,

    pub fn generate_fmeda(&mut self) -> FMEDAReport {
        // For each failure mode in FMEA
        for (gate_id, failures) in &self.fmea.gate_failures {
            // Extract design data for this gate
            let gate_metrics = self.extract_gate_metrics(gate_id);

            for failure_mode in failures {
                // Associate design elements
                let fmeda_entry = FMEDAEntry {
                    // From FMEA
                    failure_mode: failure_mode.clone(),
                    failure_effect: self.fmea.get_effect(gate_id, failure_mode),
                    severity: self.fmea.get_severity(gate_id),

                    // Design data association
                    element_type: gate_metrics.gate_type,
                    transistor_count: gate_metrics.transistor_count,
                    area: gate_metrics.area_um2,
                    power: gate_metrics.power_mw,

                    // Failure rates from library
                    base_failure_rate: self.get_failure_rate(gate_metrics.gate_type),
                    failure_distribution: self.get_distribution(gate_metrics.gate_type),

                    // Diagnostic coverage
                    diagnostic_coverage: self.calculate_diagnostic_coverage(gate_id),
                    safe_failure_fraction: self.calculate_sff(gate_id),
                };

                self.fmeda_entries.push(fmeda_entry);
            }
        }

        self.calculate_safety_metrics()
    }
}
```

### Design Metrics Extraction

```rust
pub struct DesignMetricsExtractor {
    pub fn extract_from_synthesis(&self, netlist: &Netlist) -> DesignMetrics {
        let mut metrics = DesignMetrics::new();

        for gate in netlist.gates() {
            metrics.add_gate(GateMetrics {
                gate_id: gate.id,
                gate_type: gate.cell_type,

                // From technology library
                transistor_count: self.tech_lib.get_transistor_count(gate.cell_type),
                area_um2: self.tech_lib.get_area(gate.cell_type),
                power_mw: self.power_analysis.get_power(gate),

                // Timing data
                propagation_delay: self.timing.get_delay(gate),
                setup_time: self.timing.get_setup(gate),

                // Physical placement
                location: self.placement.get_coordinates(gate),
                routing_congestion: self.routing.get_congestion(gate),
            });
        }

        metrics
    }
}
```

### Failure Rate Calculation

```rust
impl FMEDACalculator {
    pub fn calculate_failure_rates(&self, gate: &GateMetrics) -> FailureRates {
        // Base failure rate from technology
        let base_rate = match gate.gate_type {
            GateType::NAND2 => 0.01 * gate.transistor_count,  // FIT
            GateType::DFF => 0.05 * gate.transistor_count,
            GateType::MUX2 => 0.02 * gate.transistor_count,
            // ... more gate types
        };

        // Environmental factors
        let temperature_factor = self.calculate_temp_factor(gate.location);
        let voltage_factor = self.calculate_voltage_factor(gate.power);
        let process_factor = self.tech_lib.process_factor;

        FailureRates {
            permanent: base_rate * 0.6 * temperature_factor,
            transient: base_rate * 0.3 * voltage_factor,
            intermittent: base_rate * 0.1 * process_factor,
            total: base_rate * temperature_factor * voltage_factor * process_factor,
        }
    }
}
```

### Safety Metrics Calculation

```rust
pub struct SafetyMetricsCalculator {
    pub fn calculate_from_fmeda(&self, fmeda: &FMEDA) -> ISO26262Metrics {
        let mut metrics = ISO26262Metrics::new();

        // Single Point Fault Metric (SPFM)
        let total_failure_rate: f64 = fmeda.entries.iter()
            .map(|e| e.base_failure_rate)
            .sum();

        let detected_failure_rate: f64 = fmeda.entries.iter()
            .map(|e| e.base_failure_rate * e.diagnostic_coverage)
            .sum();

        metrics.spfm = (detected_failure_rate / total_failure_rate) * 100.0;

        // Latent Fault Metric (LFM)
        let latent_dangerous: f64 = fmeda.entries.iter()
            .filter(|e| e.is_latent_dangerous())
            .map(|e| e.base_failure_rate * (1.0 - e.diagnostic_coverage))
            .sum();

        metrics.lfm = ((total_failure_rate - latent_dangerous) / total_failure_rate) * 100.0;

        // Probabilistic Metric for Hardware Failures (PMHF)
        metrics.pmhf = fmeda.entries.iter()
            .filter(|e| e.violates_safety_goal())
            .map(|e| e.base_failure_rate * (1.0 - e.diagnostic_coverage))
            .sum();

        metrics
    }
}
```

### FMEDA Report Generation

```rust
impl FMEDAReportGenerator {
    pub fn generate_report(&self, fmeda: &FMEDA) -> FMEDADocument {
        FMEDADocument {
            // Executive summary
            summary: FMEDASummary {
                total_elements: fmeda.entries.len(),
                total_failure_rate: fmeda.calculate_total_fit(),
                spfm: fmeda.metrics.spfm,
                lfm: fmeda.metrics.lfm,
                pmhf: fmeda.metrics.pmhf,
                asil_achieved: fmeda.determine_asil_level(),
            },

            // Detailed worksheet
            worksheet: fmeda.entries.iter().map(|e| FMEDARow {
                element: e.element_name(),
                element_type: e.element_type,
                quantity: e.transistor_count,
                failure_mode: e.failure_mode,
                failure_rate: e.base_failure_rate,
                failure_distribution: e.failure_distribution,
                failure_effect: e.failure_effect,
                diagnostic_coverage: e.diagnostic_coverage,
                safe_failure_fraction: e.safe_failure_fraction,
            }).collect(),

            // Design data correlation
            design_correlation: DesignCorrelation {
                gate_to_failure_map: fmeda.gate_failure_map(),
                module_contribution: fmeda.module_failure_contribution(),
                critical_paths: fmeda.critical_path_analysis(),
            },

            // Compliance assessment
            compliance: ComplianceAssessment {
                iso26262_targets: fmeda.get_asil_targets(),
                achieved_metrics: fmeda.metrics,
                gap_analysis: fmeda.identify_gaps(),
                recommendations: fmeda.generate_recommendations(),
            },
        }
    }
}
```

### Integration with Intent and Safety Mechanisms

```rust
impl FMEDAWithIntent {
    pub fn enhance_with_intent(&mut self, entity: &Entity) {
        // Use intent to classify failures
        for entry in &mut self.fmeda_entries {
            if let Some(intent) = entity.intent.affects_element(entry.gate_id) {
                // Intent provides context for safety classification
                entry.safety_relevant = intent.is_safety_critical();
                entry.safety_goal = intent.get_safety_goal();

                // Adjust diagnostic coverage based on intent detection hints
                if let Some(detection) = intent.detection_hints.for_element(entry.gate_id) {
                    entry.diagnostic_coverage = detection.coverage;
                }
            }
        }

        // Consider PSM/LSM coverage
        for entry in &mut self.fmeda_entries {
            if let Some(psm) = self.find_covering_psm(entry.gate_id) {
                entry.diagnostic_coverage = psm.diagnostic_coverage.max(entry.diagnostic_coverage);
                entry.safety_mechanism = Some(psm.name);
            }
        }
    }
}
```

### Example FMEDA Output

```rust
// FMEDA Report for BrakeController
┌─────────────┬────────┬──────────┬─────────────┬─────────┬──────────┬─────────┐
│ Element     │ Type   │ λ (FIT)  │ Failure Mode│ DC (%)  │ SFF (%)  │ PMHF    │
├─────────────┼────────┼──────────┼─────────────┼─────────┼──────────┼─────────┤
│ G234/AND2   │ Comb   │ 0.12     │ Stuck-at-1  │ 99.0    │ 99.5     │ 1.2e-9  │
│ G235/DFF    │ Seq    │ 0.25     │ Stuck-at-0  │ 98.5    │ 99.2     │ 3.8e-9  │
│ G236/MUX2   │ Comb   │ 0.18     │ Select fail │ 97.8    │ 98.9     │ 3.9e-9  │
└─────────────┴────────┴──────────┴─────────────┴─────────┴──────────┴─────────┘

Safety Metrics Summary:
- SPFM: 98.7% (Target: >99% for ASIL D) ✓
- LFM: 97.2% (Target: >90% for ASIL D) ✓
- PMHF: 8.9 FIT (Target: <10 FIT) ✓
- ASIL Level Achieved: ASIL D
```

## Future Enhancements

1. **Machine Learning for RPN** - Learn occurrence rates from field data
2. **Formal Methods Integration** - Prove absence of certain failure propagations
3. **Dynamic FMEA Updates** - Update analysis as design evolves
4. **Cross-Module FMEA** - Analyze failures across module boundaries
5. **Probabilistic FMEA** - Include failure probability distributions
6. **Field Data Feedback** - Update FMEDA failure rates from deployed systems
7. **Technology Scaling** - Adjust failure rates for different process nodes

This architecture ensures FMEA/FMEDA is not a documentation afterthought but an integral part of the safety-critical design flow, with intent providing the crucial semantic bridge between high-level safety requirements and low-level implementation details.