//! Safety Analysis Pipeline
//!
//! Orchestrates multiple safety analysis passes to validate
//! ISO 26262 compliance and generate metrics.

use crate::analysis::{
    error_codes, AnalysisContext, AnalysisError, AnalysisMetrics, AnalysisResult, AnalysisStatus,
    AnalysisWarning, CombinedAnalysisResult, SafetyAnalysisPass,
};
use crate::asil::AsilLevel;
use crate::design_resolver::DesignPathResolver;
use crate::fmeda_library::{FmedaBinding, FmedaCalculator, FmedaLibraryManager};
use crate::hierarchy::{
    DesignPattern, DesignRef, MechanismType, SafetyEntity, SafetyGoal, SafetyHierarchy,
};

// ============================================================================
// Safety Analysis Pipeline
// ============================================================================

/// Pipeline for running safety analysis passes
pub struct SafetyPipeline {
    /// Analysis passes to run
    passes: Vec<Box<dyn SafetyAnalysisPass>>,
    /// FMEDA library manager
    library_manager: FmedaLibraryManager,
}

impl SafetyPipeline {
    /// Create a new pipeline with default passes
    pub fn new() -> Self {
        let mut library_manager = FmedaLibraryManager::new();
        library_manager.load_standard_libraries();

        Self {
            passes: Vec::new(),
            library_manager,
        }
    }

    /// Create pipeline with standard ISO 26262 passes
    pub fn with_standard_passes() -> Self {
        let mut pipeline = Self::new();
        pipeline.add_pass(Box::new(GoalValidationPass));
        pipeline.add_pass(Box::new(EntityValidationPass));
        pipeline.add_pass(Box::new(AsilDecompositionPass));
        pipeline.add_pass(Box::new(DesignBindingPass));
        pipeline.add_pass(Box::new(HsiValidationPass));
        pipeline.add_pass(Box::new(FmedaAnalysisPass::new(pipeline.library_manager.clone())));
        pipeline.add_pass(Box::new(TraceabilityPass));
        pipeline
    }

    /// Add a pass to the pipeline
    pub fn add_pass(&mut self, pass: Box<dyn SafetyAnalysisPass>) {
        self.passes.push(pass);
    }

    /// Set library manager
    pub fn with_library_manager(mut self, manager: FmedaLibraryManager) -> Self {
        self.library_manager = manager;
        self
    }

    /// Run all passes
    pub fn run(
        &self,
        hierarchy: &SafetyHierarchy,
        resolver: Option<&DesignPathResolver>,
        context: &AnalysisContext,
    ) -> CombinedAnalysisResult {
        let mut results = Vec::new();

        for pass in &self.passes {
            // Skip passes not required for target ASIL
            if !pass.required_for_asil(context.target_asil) {
                let skipped = AnalysisResult {
                    pass_name: pass.name().to_string(),
                    status: AnalysisStatus::Skipped,
                    errors: Vec::new(),
                    warnings: Vec::new(),
                    notes: vec![format!(
                        "Skipped - not required for {:?}",
                        context.target_asil
                    )],
                    metrics: None,
                };
                results.push(skipped);
                continue;
            }

            // Run the pass
            let result = pass.run(hierarchy, resolver, context);
            results.push(result);

            // Stop on failure if strict mode
            if context.strict_mode && results.last().map(|r| r.is_fail()).unwrap_or(false) {
                break;
            }
        }

        CombinedAnalysisResult::from_results(context.target_asil, results)
    }

    /// Run single pass by name
    pub fn run_pass(
        &self,
        pass_name: &str,
        hierarchy: &SafetyHierarchy,
        resolver: Option<&DesignPathResolver>,
        context: &AnalysisContext,
    ) -> Option<AnalysisResult> {
        self.passes
            .iter()
            .find(|p| p.name() == pass_name)
            .map(|p| p.run(hierarchy, resolver, context))
    }

    /// Get list of pass names
    pub fn pass_names(&self) -> Vec<&'static str> {
        self.passes.iter().map(|p| p.name()).collect()
    }
}

impl Default for SafetyPipeline {
    fn default() -> Self {
        Self::with_standard_passes()
    }
}

// ============================================================================
// Standard Analysis Passes
// ============================================================================

/// Validates safety goal definitions
pub struct GoalValidationPass;

impl SafetyAnalysisPass for GoalValidationPass {
    fn name(&self) -> &'static str {
        "goal_validation"
    }

    fn description(&self) -> &'static str {
        "Validates safety goal definitions and target metrics"
    }

    fn run(
        &self,
        hierarchy: &SafetyHierarchy,
        _resolver: Option<&DesignPathResolver>,
        context: &AnalysisContext,
    ) -> AnalysisResult {
        let mut result = AnalysisResult::pass(self.name());

        for (name, goal) in &hierarchy.goals {
            // Check ASIL is set
            if goal.asil == AsilLevel::QM && context.target_asil > AsilLevel::QM {
                result = result.with_warning(
                    AnalysisWarning::new(
                        "W0104",
                        &format!("Safety goal '{}' has QM level but target is {:?}",
                            name, context.target_asil),
                    )
                );
            }

            // Check target metrics for high ASIL
            if goal.asil >= AsilLevel::B {
                if goal.target_spfm.is_none() {
                    result = result.with_warning(
                        AnalysisWarning::new(
                            "W0105",
                            &format!("Safety goal '{}' missing SPFM target for {:?}",
                                name, goal.asil),
                        )
                    );
                }
                if goal.target_lfm.is_none() {
                    result = result.with_warning(
                        AnalysisWarning::new(
                            "W0106",
                            &format!("Safety goal '{}' missing LFM target for {:?}",
                                name, goal.asil),
                        )
                    );
                }
            }

            // Check HSRs have mechanisms
            for hsr in &goal.hsrs {
                if hsr.psm.is_none() {
                    result = result.with_warning(
                        AnalysisWarning::new(
                            "W0107",
                            &format!("HSR '{}' in goal '{}' has no PSM defined",
                                hsr.id, name),
                        )
                    );
                }

                // Check verification methods
                if hsr.verification.is_empty() && goal.asil >= AsilLevel::C {
                    result = result.with_warning(
                        AnalysisWarning::new(
                            error_codes::W0103_MISSING_VERIFICATION,
                            &format!("HSR '{}' has no verification methods for {:?}",
                                hsr.id, goal.asil),
                        )
                    );
                }
            }
        }

        result
    }
}

/// Validates safety entity definitions
pub struct EntityValidationPass;

impl SafetyAnalysisPass for EntityValidationPass {
    fn name(&self) -> &'static str {
        "entity_validation"
    }

    fn description(&self) -> &'static str {
        "Validates safety entity definitions and references"
    }

    fn run(
        &self,
        hierarchy: &SafetyHierarchy,
        _resolver: Option<&DesignPathResolver>,
        _context: &AnalysisContext,
    ) -> AnalysisResult {
        let mut result = AnalysisResult::pass(self.name());

        for (name, entity) in &hierarchy.entities {
            // Check implements references existing goal
            if !hierarchy.goals.contains_key(&entity.implements) {
                result = result.with_error(
                    AnalysisError::new(
                        error_codes::E0505_UNDEFINED_ENTITY,
                        &format!("Safety entity '{}' implements undefined goal '{}'",
                            name, entity.implements),
                    )
                );
            }

            // Check covers patterns are non-empty
            if entity.covers.is_empty() {
                result = result.with_warning(
                    AnalysisWarning::new(
                        "W0108",
                        &format!("Safety entity '{}' has no 'covers' patterns",
                            name),
                    )
                );
            }

            // Check child instances reference existing entities
            for inst in &entity.instances {
                if !hierarchy.entities.contains_key(&inst.entity_type) {
                    result = result.with_error(
                        AnalysisError::new(
                            error_codes::E0505_UNDEFINED_ENTITY,
                            &format!("Instance '{}' in '{}' references undefined entity '{}'",
                                inst.name, name, inst.entity_type),
                        )
                    );
                }
            }
        }

        result
    }
}

/// Validates ASIL decomposition
pub struct AsilDecompositionPass;

impl SafetyAnalysisPass for AsilDecompositionPass {
    fn name(&self) -> &'static str {
        "asil_decomposition"
    }

    fn description(&self) -> &'static str {
        "Validates ASIL decomposition per ISO 26262"
    }

    fn run(
        &self,
        hierarchy: &SafetyHierarchy,
        _resolver: Option<&DesignPathResolver>,
        _context: &AnalysisContext,
    ) -> AnalysisResult {
        let mut result = AnalysisResult::pass(self.name());

        for (name, entity) in &hierarchy.entities {
            if let Some(ref decomp) = entity.decomposition {
                if !decomp.is_valid() {
                    result = result.with_error(
                        AnalysisError::new(
                            error_codes::E0501_ASIL_DECOMPOSITION,
                            &format!(
                                "{:?} cannot decompose to ({:?}, {:?})",
                                decomp.original_asil,
                                decomp.parts.get(0).unwrap_or(&AsilLevel::QM),
                                decomp.parts.get(1).unwrap_or(&AsilLevel::QM),
                            ),
                        )
                        .at(&format!("safety_entity '{}' decomposition", name))
                        .suggest(&format!(
                            "Valid decompositions for {:?}: {:?}",
                            decomp.original_asil,
                            decomp.original_asil.decompose()
                        )),
                    );
                }
            }
        }

        result
    }
}

/// Validates design bindings (resolves paths)
pub struct DesignBindingPass;

impl SafetyAnalysisPass for DesignBindingPass {
    fn name(&self) -> &'static str {
        "design_binding"
    }

    fn description(&self) -> &'static str {
        "Validates design path references exist"
    }

    fn run(
        &self,
        hierarchy: &SafetyHierarchy,
        resolver: Option<&DesignPathResolver>,
        _context: &AnalysisContext,
    ) -> AnalysisResult {
        let mut result = AnalysisResult::pass(self.name());

        let Some(resolver) = resolver else {
            // If no resolver, skip validation but warn
            return result.with_note("Design resolver not available - skipping path validation".to_string());
        };

        for (name, entity) in &hierarchy.entities {
            // Validate covers patterns
            for pattern in &entity.covers {
                match resolver.validate_pattern(pattern) {
                    crate::design_resolver::PatternValidationResult::NoMatches { suggestion, .. } => {
                        let mut error = AnalysisError::new(
                            error_codes::E0504_UNRESOLVED_PATH,
                            &format!(
                                "Pattern '{}' in entity '{}' matches no design elements",
                                pattern.instance_pattern, name
                            ),
                        );
                        if let Some(sug) = suggestion {
                            error = error.suggest(&sug);
                        }
                        result = result.with_error(error);
                    }
                    crate::design_resolver::PatternValidationResult::Matches(_) => {}
                }
            }

            // Validate HSI patterns
            for include in &entity.hsi.includes {
                match resolver.validate_pattern(include) {
                    crate::design_resolver::PatternValidationResult::NoMatches { .. } => {
                        result = result.with_warning(
                            AnalysisWarning::new(
                                "W0109",
                                &format!("HSI include pattern '{}' matches no signals",
                                    include.instance_pattern),
                            )
                        );
                    }
                    _ => {}
                }
            }
        }

        result
    }

    fn required_for_asil(&self, _level: AsilLevel) -> bool {
        // This pass is always useful
        true
    }
}

/// Validates HSI definitions
pub struct HsiValidationPass;

impl SafetyAnalysisPass for HsiValidationPass {
    fn name(&self) -> &'static str {
        "hsi_validation"
    }

    fn description(&self) -> &'static str {
        "Validates Hardware-Software Interface specifications"
    }

    fn run(
        &self,
        hierarchy: &SafetyHierarchy,
        _resolver: Option<&DesignPathResolver>,
        context: &AnalysisContext,
    ) -> AnalysisResult {
        let mut result = AnalysisResult::pass(self.name());

        for (name, entity) in &hierarchy.entities {
            // Check HSI has content for high ASIL
            if context.target_asil >= AsilLevel::C
                && entity.hsi.includes.is_empty()
            {
                result = result.with_warning(
                    AnalysisWarning::new(
                        "W0110",
                        &format!("Safety entity '{}' has empty HSI for {:?}",
                            name, context.target_asil),
                    )
                );
            }

            // Check timing constraints are set for ASIL D
            if context.target_asil == AsilLevel::D
                && entity.hsi.timing.ftti.is_none()
            {
                result = result.with_warning(
                    AnalysisWarning::new(
                        "W0111",
                        &format!("Safety entity '{}' missing FTTI in HSI timing for ASIL D",
                            name),
                    )
                );
            }
        }

        result
    }

    fn required_for_asil(&self, level: AsilLevel) -> bool {
        level >= AsilLevel::B
    }
}

/// Performs FMEDA analysis
pub struct FmedaAnalysisPass {
    library_manager: FmedaLibraryManager,
}

impl FmedaAnalysisPass {
    pub fn new(library_manager: FmedaLibraryManager) -> Self {
        Self { library_manager }
    }
}

impl SafetyAnalysisPass for FmedaAnalysisPass {
    fn name(&self) -> &'static str {
        "fmeda_analysis"
    }

    fn description(&self) -> &'static str {
        "Calculates FMEDA metrics (SPFM, LFM, PMHF)"
    }

    fn run(
        &self,
        hierarchy: &SafetyHierarchy,
        _resolver: Option<&DesignPathResolver>,
        context: &AnalysisContext,
    ) -> AnalysisResult {
        let mut result = AnalysisResult::pass(self.name());

        // Collect FMEDA bindings from entities
        let mut bindings = Vec::new();

        for entity in hierarchy.entities.values() {
            for fmea_component in &entity.fmea {
                // Determine mechanisms from PSM overrides
                let mechanisms: Vec<MechanismType> = entity
                    .psm_overrides
                    .keys()
                    .filter_map(|name| {
                        // Simple mapping from PSM name to mechanism type
                        match name.as_str() {
                            s if s.contains("Voting") => Some(MechanismType::Tmr),
                            s if s.contains("Crc") || s.contains("Integrity") => Some(MechanismType::Crc),
                            s if s.contains("Ecc") => Some(MechanismType::Ecc),
                            s if s.contains("Watchdog") => Some(MechanismType::Watchdog),
                            s if s.contains("Lockstep") => Some(MechanismType::Lockstep),
                            _ => None,
                        }
                    })
                    .collect();

                bindings.push(FmedaBinding {
                    design_ref: fmea_component.design_ref.clone(),
                    library: fmea_component.library.clone(),
                    part_number: fmea_component.part.clone(),
                    mechanisms,
                    overrides: Default::default(),
                });
            }
        }

        if bindings.is_empty() {
            return result.with_note("No FMEA bindings found - metrics not calculated".to_string());
        }

        // Run FMEDA calculation
        let calculator = FmedaCalculator::new(self.library_manager.clone());
        let analysis = calculator.calculate(&bindings);

        // Get targets from context or goals
        let spfm_target = context.config.spfm_target_override
            .or_else(|| {
                hierarchy.goals.values().next()
                    .and_then(|g| g.target_spfm)
            })
            .unwrap_or_else(|| context.target_asil.requirements().spfm_target.unwrap_or(0.0));

        let lfm_target = context.config.lfm_target_override
            .or_else(|| {
                hierarchy.goals.values().next()
                    .and_then(|g| g.target_lfm)
            })
            .unwrap_or_else(|| context.target_asil.requirements().lf_target.unwrap_or(0.0));

        // Check metrics against targets
        if analysis.summary.spfm < spfm_target {
            result = result.with_error(
                AnalysisError::new(
                    error_codes::E0502_SPFM_BELOW_TARGET,
                    &format!(
                        "SPFM {:.1}% is below target {:.1}% for {:?}",
                        analysis.summary.spfm, spfm_target, context.target_asil
                    ),
                )
                .suggest("Add safety mechanisms to improve diagnostic coverage"),
            );
        }

        if analysis.summary.lfm < lfm_target {
            result = result.with_error(
                AnalysisError::new(
                    error_codes::E0503_LFM_BELOW_TARGET,
                    &format!(
                        "LFM {:.1}% is below target {:.1}% for {:?}",
                        analysis.summary.lfm, lfm_target, context.target_asil
                    ),
                )
                .suggest("Add latent fault testing mechanisms"),
            );
        }

        // Add metrics to result
        let meets_target = analysis.summary.meets_asil(context.target_asil);
        let mut metrics: AnalysisMetrics = analysis.summary.into();
        metrics.meets_target = meets_target;

        result = result.with_metrics(metrics);
        result = result.with_note(format!(
            "FMEDA analyzed {} components",
            bindings.len()
        ));

        result
    }

    fn required_for_asil(&self, level: AsilLevel) -> bool {
        level >= AsilLevel::B
    }
}

/// Validates traceability completeness
pub struct TraceabilityPass;

impl SafetyAnalysisPass for TraceabilityPass {
    fn name(&self) -> &'static str {
        "traceability"
    }

    fn description(&self) -> &'static str {
        "Verifies requirement traceability completeness"
    }

    fn run(
        &self,
        hierarchy: &SafetyHierarchy,
        resolver: Option<&DesignPathResolver>,
        context: &AnalysisContext,
    ) -> AnalysisResult {
        let mut result = AnalysisResult::pass(self.name());

        for (goal_name, goal) in &hierarchy.goals {
            // Check each PSM has implementations
            for hsr in &goal.hsrs {
                if let Some(ref psm) = hsr.psm {
                    // Check if we have implementations from resolver annotations
                    let has_implementations = if let Some(resolver) = resolver {
                        !resolver.find_implementations(goal_name, &psm.name).is_empty()
                    } else {
                        !psm.implementations.is_empty()
                    };

                    if !has_implementations {
                        result = result.with_error(
                            AnalysisError::new(
                                error_codes::E0506_MISSING_IMPLEMENTATION,
                                &format!(
                                    "PSM '{}' in goal '{}' has no design implementations",
                                    psm.name, goal_name
                                ),
                            )
                            .suggest(&format!(
                                "Add #[implements({}::{})] annotation in design",
                                goal_name, psm.name
                            )),
                        );
                    }
                }
            }
        }

        // Generate traceability matrix
        let matrix = hierarchy.generate_traceability();
        result = result.with_note(format!(
            "Traceability matrix: {} entries",
            matrix.entries.len()
        ));

        result
    }

    fn required_for_asil(&self, level: AsilLevel) -> bool {
        level >= AsilLevel::A
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hierarchy::{
        DiagnosticHardwareSafetyRequirement, HardwareSafetyRequirement,
        PrimarySafetyMechanism, SafetyGoalId, SafetyEntityId,
    };

    fn create_test_hierarchy() -> SafetyHierarchy {
        let mut hierarchy = SafetyHierarchy::new();

        // Create a safety goal
        let psm = PrimarySafetyMechanism::new("SensorVoting".to_string(), 99.0);
        let hsr = HardwareSafetyRequirement::new(
            "HSR_001".to_string(),
            "Detect sensor faults".to_string(),
        )
        .with_psm(psm);

        let mut goal = SafetyGoal::new(
            SafetyGoalId(1),
            "SG-001".to_string(),
            "BrakingSafety".to_string(),
            "Prevent unintended braking".to_string(),
            AsilLevel::D,
        );
        goal.target_spfm = Some(99.0);
        goal.target_lfm = Some(90.0);
        goal.hsrs.push(hsr);

        hierarchy.add_goal(goal);

        // Create a safety entity
        let mut entity = SafetyEntity::new(
            SafetyEntityId(1),
            "BrakingControl".to_string(),
            "BrakingSafety".to_string(),
        );
        entity.covers(DesignPattern::instances("top.brake_main"));

        hierarchy.add_entity(entity);

        hierarchy
    }

    #[test]
    fn test_pipeline_creation() {
        let pipeline = SafetyPipeline::with_standard_passes();
        assert!(!pipeline.pass_names().is_empty());
        assert!(pipeline.pass_names().contains(&"goal_validation"));
        assert!(pipeline.pass_names().contains(&"fmeda_analysis"));
    }

    #[test]
    fn test_goal_validation_pass() {
        let hierarchy = create_test_hierarchy();
        let context = AnalysisContext::for_asil(AsilLevel::D);

        let pass = GoalValidationPass;
        let result = pass.run(&hierarchy, None, &context);

        // Should have warnings about missing verification methods
        assert!(result.is_pass());
    }

    #[test]
    fn test_entity_validation_pass() {
        let hierarchy = create_test_hierarchy();
        let context = AnalysisContext::for_asil(AsilLevel::D);

        let pass = EntityValidationPass;
        let result = pass.run(&hierarchy, None, &context);

        assert!(result.is_pass());
    }

    #[test]
    fn test_asil_decomposition_pass() {
        let mut hierarchy = create_test_hierarchy();

        // Add invalid decomposition
        let mut entity = SafetyEntity::new(
            SafetyEntityId(2),
            "InvalidDecomp".to_string(),
            "BrakingSafety".to_string(),
        );
        entity.decomposition = Some(crate::hierarchy::AsilDecomposition {
            original_asil: AsilLevel::D,
            parts: vec![AsilLevel::C, AsilLevel::C], // Invalid!
            decomposes: "BrakingSafety".to_string(),
        });
        hierarchy.add_entity(entity);

        let context = AnalysisContext::for_asil(AsilLevel::D);
        let pass = AsilDecompositionPass;
        let result = pass.run(&hierarchy, None, &context);

        assert!(result.is_fail());
        assert!(result.errors.iter().any(|e| e.code == error_codes::E0501_ASIL_DECOMPOSITION));
    }

    #[test]
    fn test_pipeline_run() {
        let hierarchy = create_test_hierarchy();
        let context = AnalysisContext::for_asil(AsilLevel::B);

        let pipeline = SafetyPipeline::with_standard_passes();
        let result = pipeline.run(&hierarchy, None, &context);

        // Should have some results
        assert!(!result.pass_results.is_empty());
    }

    #[test]
    fn test_pipeline_strict_mode() {
        let hierarchy = SafetyHierarchy::new(); // Empty hierarchy
        let context = AnalysisContext::for_asil(AsilLevel::D).strict();

        let pipeline = SafetyPipeline::with_standard_passes();
        let result = pipeline.run(&hierarchy, None, &context);

        // In strict mode, should stop at first failure
        // Empty hierarchy should pass validation (nothing to validate)
        assert!(result.pass_results.len() > 0);
    }
}
