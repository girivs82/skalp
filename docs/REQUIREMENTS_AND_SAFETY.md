# Requirements and Functional Safety Management

## Part 1: Requirements Lifecycle Management

### Overview

SKALP provides first-class support for requirements traceability, making requirements an integral part of the hardware design flow rather than a bolt-on documentation exercise. Every entity and implementation can explicitly declare which requirements it satisfies, with compile-time verification of requirement coverage.

### Requirements Definition

Requirements are defined directly in SKALP or imported from external requirements management systems:

```rust
// Define requirements in SKALP
requirement REQ_PERF_001 {
    id: "SYS-PERF-001",
    title: "Inference Throughput",
    description: "AI accelerator shall achieve 1 TOPS at 2W power",
    category: performance,
    criticality: high,
    measurable: {
        metric: throughput,
        target: 1_TOPS,
        conditions: { power: <= 2W, temperature: 25C },
    },
    verification: [
        performance_simulation,
        silicon_validation,
    ],
}

requirement REQ_POWER_001 {
    id: "SYS-PWR-001",
    title: "Standby Power",
    description: "System standby power shall not exceed 100mW",
    category: power,
    measurable: {
        metric: power_consumption,
        target: <= 100mW,
        conditions: { state: standby },
    },
}

// Import from external systems
requirement_import {
    source: "DOORS",
    connection: "https://requirements.company.com/api",
    project: "PROJ-2024-001",
    baseline: "v1.0",
    sync: bidirectional,
}
```

### Entity-Level Requirement Satisfaction

Entities explicitly declare which requirements they satisfy, providing evidence:

```rust
entity AIAccelerator {
    in data: stream<'clk>[512]
    out result: stream<'clk>[256]
} satisfies [REQ_PERF_001] with evidence {
    throughput_achieved: 1.1_TOPS,
    power_measured: 1.8W,
    verification_report: "reports/ai_accel_perf.html",
}

impl AIAccelerator {
    // Implementation automatically traced to REQ_PERF_001
    flow {
        result = data |> matrix_multiply() |> activation()
    }
}

// Partial satisfaction
entity PowerManager {
    // ... ports ...
} partially_satisfies [REQ_POWER_001] with evidence {
    contribution: "Manages CPU/GPU power domains",
    requires: "External PMIC for full compliance",
}
```

### Hierarchical Requirement Inheritance

Requirements flow hierarchically through the design:

```rust
entity System {
    accelerator: AIAccelerator;  // Inherits REQ_PERF_001
    power_mgr: PowerManager;     // Partial REQ_POWER_001
} satisfies [REQ_PERF_001, REQ_POWER_001] with evidence {
    REQ_PERF_001: {
        satisfied_by: accelerator,
        system_validation: "validation/system_perf.log",
    },
    REQ_POWER_001: {
        satisfied_by: [power_mgr, "external_pmic"],
        power_analysis: "power/full_system.rpt",
    },
}
```

### Compile-Time Requirement Verification

The compiler verifies all requirements are satisfied:

```rust
impl RequirementsCompiler {
    pub fn compile_with_requirements(&mut self, design: &Design) -> Result<RequirementsReport> {
        // Build requirement dependency graph
        let req_graph = self.build_requirement_graph(design);

        // Check all requirements are satisfied
        for requirement in &design.requirements {
            match self.check_satisfaction(requirement, design) {
                FullySatisfied(evidence) => {
                    report.satisfied.push((requirement, evidence));
                },
                PartiallySatisfied(evidence, missing) => {
                    self.warn!("Requirement {} partially satisfied", requirement.id);
                },
                NotSatisfied => {
                    self.error!("Requirement {} not satisfied!", requirement.id);
                },
            }
        }

        // Check for entities without requirements
        for entity in &design.entities {
            if !self.has_requirement_coverage(entity, &req_graph) {
                self.warn!("Entity {} has no requirement coverage", entity.name);
            }
        }

        // Generate traceability matrix
        report.traceability_matrix = self.generate_matrix(design, req_graph);

        Ok(report)
    }
}
```

### Requirement Verification Methods

Different verification strategies per requirement:

```rust
requirement REQ_TIMING_001 {
    id: "TMG-001",
    title: "Critical Path Timing",

    verification: {
        static_timing_analysis: {
            corner: worst_case,
            margin: 10%,
        },

        formal_proof: {
            property: "@(posedge clk) delay < 5ns",
        },

        simulation: {
            testbench: "tb/timing_test.sk",
            coverage_target: 100%,
        },

        silicon_validation: {
            test: "scan_timing_test",
            samples: 1000,
        },
    },
}
```

### Requirements Coverage Analysis

```rust
pub struct RequirementsCoverage {
    pub fn analyze_coverage(&self, design: &Design) -> CoverageReport {
        // Requirement → Entity mapping
        for req in &design.requirements {
            let entities = self.find_implementing_entities(req);
            if entities.is_empty() {
                report.uncovered_requirements.push(req);
            }
        }

        // Entity → Requirement mapping
        for entity in &design.entities {
            let reqs = self.find_requirements_for(entity);
            if reqs.is_empty() && !entity.is_utility() {
                report.entities_without_requirements.push(entity);
            }
        }

        report.coverage_percentage =
            report.covered_requirements() / report.total_requirements();

        report
    }
}
```

### Traceability Throughout Compilation

Requirements are tracked through all IR levels:

```rust
pub struct RequirementTraceability {
    hir_trace: HashMap<Requirement, Vec<HirNode>>,
    mir_trace: HashMap<Requirement, Vec<MirNode>>,
    lir_trace: HashMap<Requirement, Vec<LirNode>>,
    sir_trace: HashMap<Requirement, Vec<SimNode>>,
}

// Tags preserved during compilation
impl Compiler {
    fn lower_with_requirements(&mut self, hir: HIR) -> MIR {
        let mir = self.lower_to_mir(hir);

        // Propagate requirement tags
        for (req, hir_nodes) in &self.traceability.hir_trace {
            let mir_nodes = self.map_hir_to_mir(hir_nodes);
            for node in mir_nodes {
                node.add_requirement_tag(req);
            }
        }

        mir
    }
}
```

---

## Part 2: Functional Safety Management (ISO 26262)

### Overview

SKALP provides comprehensive support for ISO 26262 functional safety development. Safety requirements are managed separately from regular requirements and are implemented through Primary Safety Mechanisms (PSM) and Latent-fault Safety Mechanisms (LSM).

### Safety Requirements Definition

Safety requirements are distinct from regular requirements and directly map to safety mechanisms:

```rust
// Safety requirements are separate
safety_requirement SREQ_CPU_001 {
    id: "SAF-CPU-001",
    title: "CPU Lockstep Error Detection",
    description: "Detect errors in CPU execution through lockstep comparison",
    asil_level: ASIL_D,
    safety_goal: "Prevent undetected CPU errors from causing unintended acceleration",

    failure_mode: {
        type: random_hardware_failure,
        failure_rate: 10_FIT,  // Failures In Time
        diagnostic_coverage: 99%,
    },

    safety_mechanism_type: PSM,  // Primary Safety Mechanism

    verification: {
        fault_injection: {
            fault_types: [bit_flip, stuck_at, transient],
            coverage_target: 99%,
        },
        formal_verification: {
            property: "lockstep_detection_complete",
        },
    },
}

safety_requirement SREQ_CPU_002 {
    id: "SAF-CPU-002",
    title: "Lockstep Comparator Self-Test",
    description: "Detect latent faults in lockstep comparison logic",
    asil_level: ASIL_D,

    safety_mechanism_type: LSM,  // Latent-fault Safety Mechanism
    protects: SREQ_CPU_001,  // This LSM protects the PSM

    diagnostic_interval: 100ms,  // Run every 100ms
    diagnostic_time: 1ms,  // Complete within 1ms
}
```

### Safety Mechanism Implementation

Entities declare themselves as safety mechanisms implementing specific safety requirements:

```rust
// PSM Implementation
entity LockstepComparator implements_safety_mechanism {
    type: PSM,
    implements: SREQ_CPU_001,
    diagnostic_coverage: 99.2%,
} {
    in core1_output: logic<'clk>[32]
    in core2_output: logic<'clk>[32]
    out error_detected: logic<'clk>
    out syndrome: logic<'clk>[8]
} with safety_properties {
    detection_latency: 1_cycle,
    false_positive_rate: < 0.001%,
    fault_containment: immediate,
}

impl LockstepComparator {
    always_ff (posedge clk) {
        if (core1_output != core2_output) {
            error_detected <= 1;
            syndrome <= compute_syndrome(core1_output, core2_output);

            // Safety action
            trigger_safe_state();
            log_safety_event(SREQ_CPU_001);
        }
    }
}

// LSM Implementation
entity LockstepSelfTest implements_safety_mechanism {
    type: LSM,
    implements: SREQ_CPU_002,
    protects: LockstepComparator,  // The PSM it's protecting
} {
    in test_enable: logic<'clk>
    out comparator: LockstepComparator
    out test_result: logic<'clk>
} with safety_properties {
    test_coverage: 98%,
    test_duration: 1ms,
    test_interval: 100ms,
}

impl LockstepSelfTest {
    // Periodic self-test of the comparator
    always_ff (posedge clk) {
        if (test_enable && test_timer.expired()) {
            // Inject known differences
            let test_pattern = generate_test_pattern();

            // Verify comparator detects them
            if (!comparator.detects(test_pattern)) {
                // Latent fault detected in PSM!
                report_latent_fault(SREQ_CPU_002);
                enter_degraded_mode();
            }

            test_timer.reset(100ms);
        }
    }
}
```

### Safety Architecture Declaration

System-level safety architecture showing PSM/LSM relationships:

```rust
safety_architecture AutomotiveSafetyArch {
    // Primary Safety Mechanisms
    psm_mechanisms: [
        LockstepComparator,      // SREQ_CPU_001
        EccMemoryController,     // SREQ_MEM_001
        WatchdogTimer,          // SREQ_SYS_001
        VoltageMonitor,         // SREQ_PWR_001
    ],

    // Latent-fault Safety Mechanisms
    lsm_mechanisms: [
        LockstepSelfTest,       // SREQ_CPU_002 - protects LockstepComparator
        EccLogicBist,           // SREQ_MEM_002 - protects EccMemoryController
        WatchdogTest,           // SREQ_SYS_002 - protects WatchdogTimer
    ],

    // Safety chains
    safety_chains: {
        cpu_safety: [
            LockstepComparator -> LockstepSelfTest -> SafeStateController
        ],
        memory_safety: [
            EccMemoryController -> EccLogicBist -> MemoryErrorHandler
        ],
    },

    // Coverage analysis
    diagnostic_coverage: {
        target: 99%,
        achieved: calculate_from_mechanisms(),
    },
}
```

### Fault Injection and Verification

```rust
// Automatic fault injection for safety mechanisms
safety_verification {
    fault_campaign: {
        target_mechanisms: [PSM, LSM],

        fault_models: [
            stuck_at_0,
            stuck_at_1,
            bit_flip,
            transient_fault,
            timing_violation,
        ],

        injection_points: automatic,  // SKALP identifies critical nodes

        coverage_requirements: {
            ASIL_D: 99%,
            ASIL_C: 97%,
            ASIL_B: 95%,
        },
    },

    verification_flow: {
        1: verify_psm_detection,      // PSMs detect faults
        2: verify_lsm_detection,      // LSMs detect PSM failures
        3: verify_safe_state_entry,   // System enters safe state
        4: verify_diagnostic_coverage, // Meet ISO 26262 metrics
    },
}
```

### Safety Metrics Calculation

```rust
impl SafetyMetricsCalculator {
    pub fn calculate_metrics(&self, design: &Design) -> SafetyMetrics {
        let mut metrics = SafetyMetrics::new();

        // Single Point Fault Metric (SPFM)
        metrics.spfm = self.calculate_spfm(design);

        // Latent Fault Metric (LFM)
        metrics.lfm = self.calculate_lfm(design);

        // Probabilistic Metric for Hardware Failures (PMHF)
        metrics.pmhf = self.calculate_pmhf(design);

        // Diagnostic Coverage
        for psm in design.get_psm_mechanisms() {
            metrics.add_diagnostic_coverage(psm);
        }

        // Verify against ASIL targets
        metrics.verify_asil_compliance(design.asil_level);

        metrics
    }

    fn calculate_spfm(&self, design: &Design) -> f64 {
        let total_failure_rate = design.total_failure_rate();
        let covered_by_psm = design.psm_covered_failure_rate();

        (covered_by_psm / total_failure_rate) * 100.0
    }
}
```

### Safety Case Generation

```rust
impl SafetyCaseGenerator {
    pub fn generate_safety_case(&self, design: &Design) -> SafetyCase {
        SafetyCase {
            // Link safety requirements to mechanisms
            requirement_to_mechanism: self.build_req_to_mechanism_map(design),

            // PSM/LSM coverage
            psm_coverage: self.analyze_psm_coverage(design),
            lsm_coverage: self.analyze_lsm_coverage(design),

            // Fault tree analysis
            fault_trees: self.generate_fault_trees(design),

            // FMEDA results
            fmeda: self.perform_fmeda(design),

            // Verification evidence
            verification_results: self.collect_verification_evidence(design),

            // ISO 26262 compliance
            iso26262_compliance: self.check_iso_compliance(design),
        }
    }
}
```

### Integration with Requirements System

Safety requirements work alongside regular requirements:

```rust
entity SafeSystem {
    // Regular requirement
    cpu: Processor satisfies [REQ_PERF_001];

    // Safety mechanism implementing safety requirement
    lockstep: LockstepComparator implements_safety_mechanism {
        type: PSM,
        implements: SREQ_CPU_001,
    };

    // LSM protecting the PSM
    lockstep_test: LockstepSelfTest implements_safety_mechanism {
        type: LSM,
        implements: SREQ_CPU_002,
        protects: lockstep,
    };
}

// Compile-time verification
impl SafetyCompiler {
    pub fn compile_with_safety(&mut self, design: &Design) -> Result<SafetyReport> {
        // Verify all safety requirements have mechanisms
        for sreq in design.safety_requirements {
            if !self.has_implementing_mechanism(sreq) {
                self.error!("Safety requirement {} not implemented!", sreq.id);
            }
        }

        // Verify PSMs have LSMs (for high ASIL)
        for psm in design.psm_mechanisms {
            if design.asil_level >= ASIL_C && !self.has_protecting_lsm(psm) {
                self.warn!("PSM {} lacks LSM protection for ASIL_{}", psm.name, design.asil_level);
            }
        }

        // Calculate and verify safety metrics
        let metrics = self.calculate_safety_metrics(design);
        if !metrics.meets_asil_requirements(design.asil_level) {
            self.error!("Safety metrics do not meet ASIL_{} requirements", design.asil_level);
        }

        Ok(SafetyReport { metrics, coverage, verification })
    }
}
```

This architecture provides:
1. **Separate safety requirements** from regular requirements
2. **PSM/LSM declaration** as first-class safety mechanisms
3. **Traceability** from safety requirements to implementing mechanisms
4. **Compile-time verification** of safety coverage
5. **Automatic safety metrics** calculation
6. **Fault injection** framework
7. **ISO 26262 compliance** checking

---

## Part 3: FMEA (Failure Mode and Effects Analysis) with Intent-Based Automation

### Overview

SKALP provides automated FMEA generation by leveraging intent declarations as human annotations for failure effects. The compiler automatically traces gate/flop-level failures through to module-level intent violations, bridging the gap between low-level failure modes and high-level safety impacts.

### Intent-Based FMEA Declaration

Intent declarations serve as the human annotation for FMEA:

```rust
entity BrakeController with intent {
    // Safety-critical intents become FMEA annotations
    hazards: {
        unintended_braking: "Brake force applied without pedal input",
        loss_of_braking: "No brake force when pedal input present",
        delayed_braking: "Brake response time exceeds 100ms",
    },

    // Severity ratings for each hazard
    failure_impact: {
        unintended_braking: critical,    // S3 - Could cause accident
        loss_of_braking: critical,        // S3 - Loss of safety function
        delayed_braking: moderate,        // S2 - Degraded performance
    },

    // Detection capabilities hint to compiler
    detection_hints: {
        unintended_braking: "Monitor brake_output vs pedal_input correlation",
        loss_of_braking: "Watchdog on brake actuation path",
        delayed_braking: "Timing assertions on critical path",
    },
}

impl BrakeController {
    // Implementation with implicit FMEA tracking
    flow {
        brake_output = pedal_input
            |> debounce()      // Compiler tracks gates here
            |> validate()      // And here
            |> actuate()       // Critical path identified
    }
}
```

### Automatic Gate-to-Intent Decomposition

The compiler decomposes module-level intent to gate-level failures:

```rust
impl FMEACompiler {
    pub fn decompose_intent_to_gates(&mut self, entity: &Entity) -> HierarchicalFMEA {
        let mut fmea = HierarchicalFMEA::new();

        // Step 1: Extract intent properties
        let intents = entity.extract_safety_intents();

        // Step 2: Generate netlist with gate mapping
        let netlist = self.synthesize_to_netlist(entity);

        // Step 3: Build cone of influence for each gate
        for gate in netlist.gates() {
            let cone = self.build_cone_of_influence(gate, &netlist);

            // Step 4: Trace to intent violations
            for intent in &intents {
                if self.gate_affects_intent(gate, intent, &cone) {
                    // Symbolic simulation of failure effect
                    let failure_modes = self.get_failure_modes_for_gate(gate);

                    for mode in failure_modes {
                        let effect = self.simulate_failure_propagation(
                            gate, mode, intent, &cone
                        );

                        fmea.add_failure_chain(
                            gate, mode, intent, effect
                        );
                    }
                }
            }
        }

        // Step 5: Calculate RPN and criticality
        fmea.calculate_risk_priority();

        fmea
    }

    fn simulate_failure_propagation(
        &self,
        gate: GateId,
        mode: FailureMode,
        intent: &SafetyIntent,
        cone: &ConeOfInfluence,
    ) -> FailureEffect {
        // Symbolic simulation through cone
        let mut sim_state = SymbolicState::new();
        sim_state.inject_fault(gate, mode);

        // Propagate through logic cone
        for level in cone.topological_levels() {
            sim_state.propagate_level(level);
        }

        // Check if intent is violated
        let violation = sim_state.check_intent_violation(intent);

        FailureEffect {
            severity: intent.failure_impact,
            occurrence: mode.base_failure_rate,
            detection: self.estimate_detection_capability(intent, cone),
            propagation_path: sim_state.get_critical_path(),
            violated_intent: violation,
        }
    }
}
```

### Hierarchical FMEA Structure

Three levels of granularity maintained by the compiler:

```rust
pub struct HierarchicalFMEA {
    // Level 1: Gate/flop failures
    gate_level: HashMap<GateId, Vec<GateFailure>>,

    // Level 2: Path-level propagation
    path_level: HashMap<PathId, PathFailureAnalysis>,

    // Level 3: Intent-level effects
    intent_level: HashMap<IntentId, IntentViolation>,

    // Cross-level traceability
    gate_to_path: HashMap<GateId, Vec<PathId>>,
    path_to_intent: HashMap<PathId, Vec<IntentId>>,

    pub fn generate_fmea_report(&self) -> FMEAReport {
        FMEAReport {
            // Traditional FMEA worksheet
            worksheet: self.generate_worksheet(),

            // Gate-level detail (for safety engineers)
            gate_analysis: self.gate_level.clone(),

            // Path criticality (for architects)
            critical_paths: self.identify_critical_paths(),

            // Intent coverage (for system engineers)
            intent_coverage: self.calculate_intent_coverage(),

            // Risk Priority Numbers
            rpn_ranking: self.calculate_rpn_ranking(),
        }
    }
}
```

### Cone of Influence Analysis

Compiler builds precise failure propagation paths:

```rust
pub struct ConeOfInfluence {
    root_gate: GateId,
    affected_outputs: Vec<OutputId>,
    logic_levels: Vec<Vec<GateId>>,
    critical_paths: Vec<Path>,

    pub fn analyze_failure_propagation(&self, failure_mode: FailureMode) -> PropagationAnalysis {
        let mut analysis = PropagationAnalysis::new();

        // Forward propagation from failed gate
        let mut frontier = vec![self.root_gate];
        let mut visited = HashSet::new();

        while !frontier.is_empty() {
            let gate = frontier.pop().unwrap();
            visited.insert(gate);

            // Analyze how failure propagates through this gate
            let propagation = self.analyze_gate_propagation(gate, failure_mode);
            analysis.add_propagation(gate, propagation);

            // Add fanout gates to frontier
            for fanout in self.get_fanout(gate) {
                if !visited.contains(&fanout) {
                    frontier.push(fanout);
                }
            }
        }

        // Determine which outputs are affected
        for output in &self.affected_outputs {
            if analysis.reaches_output(*output) {
                analysis.mark_output_affected(*output);
            }
        }

        analysis
    }
}
```

### Detection Point Generation

Compiler automatically inserts detection based on intent:

```rust
impl FMEACompiler {
    pub fn generate_detection_mechanisms(&mut self, fmea: &HierarchicalFMEA) -> DetectionScheme {
        let mut detection = DetectionScheme::new();

        for (intent_id, violation) in &fmea.intent_level {
            // Generate assertions from intent
            let assertions = self.intent_to_assertions(intent_id);

            // Place detection points optimally
            let placement = self.optimal_detection_placement(
                &violation.critical_paths,
                &assertions
            );

            // Generate detection logic
            for point in placement {
                let detector = DetectionPoint {
                    location: point.gate_id,
                    check_type: point.detection_type,
                    coverage: point.fault_coverage,
                    latency: point.detection_latency,
                };

                detection.add_detector(intent_id, detector);
            }
        }

        detection
    }
}
```

### FMEA Report Generation

Comprehensive FMEA output with full traceability:

```rust
impl FMEAReportGenerator {
    pub fn generate_comprehensive_fmea(&self, design: &Design) -> FMEADocument {
        let mut doc = FMEADocument::new();

        // Traditional FMEA worksheet format
        for entity in design.entities_with_intents() {
            let fmea = self.compiler.decompose_intent_to_gates(entity);

            // Generate rows for each failure
            for (gate, failures) in &fmea.gate_level {
                for failure in failures {
                    doc.add_row(FMEARow {
                        item: format!("{}/Gate_{}", entity.name, gate),
                        function: self.describe_gate_function(gate),
                        failure_mode: failure.mode.description(),
                        failure_cause: failure.cause.description(),
                        failure_effect: {
                            // Local effect (gate-level)
                            local: failure.local_effect,
                            // System effect (intent-level)
                            system: failure.intent_violation.description(),
                        },
                        severity: failure.severity,
                        occurrence: failure.occurrence,
                        detection: failure.detection_capability,
                        rpn: failure.calculate_rpn(),
                        recommended_action: self.suggest_mitigation(failure),
                    });
                }
            }
        }

        // Enhanced traceability section
        doc.add_traceability(TraceabilityMatrix {
            gate_to_intent: fmea.generate_gate_intent_matrix(),
            critical_paths: fmea.identify_critical_paths(),
            coverage_analysis: fmea.calculate_coverage(),
        });

        doc
    }
}
```

### Integration with Safety Mechanisms

FMEA automatically considers PSM/LSM coverage:

```rust
impl FMEAWithSafetyMechanisms {
    pub fn analyze_with_safety_coverage(&self, fmea: &HierarchicalFMEA) -> EnhancedFMEA {
        let mut enhanced = fmea.clone();

        // For each failure mode, check if covered by safety mechanism
        for (gate, failures) in &mut enhanced.gate_level {
            for failure in failures {
                // Check PSM coverage
                if let Some(psm) = self.find_covering_psm(gate, failure) {
                    failure.detection_capability = psm.diagnostic_coverage;
                    failure.detection_mechanism = Some(psm.name.clone());
                }

                // Check LSM coverage for the PSM
                if let Some(lsm) = self.find_covering_lsm(&psm) {
                    failure.latent_fault_detection = Some(lsm.coverage);
                }

                // Recalculate RPN with safety mechanisms
                failure.rpn = failure.calculate_rpn_with_safety();
            }
        }

        enhanced
    }
}
```

### Compile-Time FMEA Verification

```rust
impl Compiler {
    pub fn compile_with_fmea(&mut self, design: &Design) -> Result<FMEACompilationReport> {
        // Generate FMEA during compilation
        let fmea = self.generate_fmea(design);

        // Verify all hazards have detection
        for (intent, hazard) in design.get_safety_hazards() {
            if !fmea.has_detection_for(hazard) {
                self.warn!("Hazard {} has no detection mechanism", hazard.id);
            }
        }

        // Check RPN thresholds
        for failure in fmea.get_high_rpn_failures() {
            if failure.rpn > design.rpn_threshold {
                self.error!("Failure {} exceeds RPN threshold: {}",
                    failure.id, failure.rpn);
            }
        }

        // Generate FMEA documentation
        let report = FMEACompilationReport {
            fmea_worksheet: fmea.generate_worksheet(),
            critical_failures: fmea.get_critical_failures(),
            coverage_metrics: fmea.calculate_coverage(),
            traceability: fmea.generate_traceability(),
        };

        Ok(report)
    }
}
```

This FMEA architecture provides:
1. **Intent-based annotations** replace manual gate-level annotation
2. **Automatic decomposition** from module intent to gate failures
3. **Cone of influence** analysis for precise propagation
4. **Hierarchical analysis** maintains gate, path, and intent levels
5. **Automatic detection** point generation from intents
6. **PSM/LSM integration** for accurate RPN calculation
7. **Compile-time verification** of FMEA completeness