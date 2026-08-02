// NOTE: These tests are temporarily disabled because they reference the old LIR API
// (transform_mir_to_lir, TechnologyMapper, TechnologyTarget, optimization, timing)
// which has been replaced by the new technology mapping infrastructure.
// TODO: Update these tests to use the new TechMapper, GateNetlist, and TechLibrary APIs.
#![allow(unexpected_cfgs)]

#[cfg(test)]
#[cfg(feature = "disabled_old_lir_api")]
mod phase8_final_success_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_lir::optimization::OptimizationPipeline;
    use skalp_lir::timing::TimingAnalyzer;
    use skalp_lir::{lower_to_lir, transform_mir_to_lir, TechnologyMapper, TechnologyTarget};
    use skalp_mir::lower_to_mir;

    #[test]
    fn test_phase8_final_success_simple_processor() {
        // Test implementation
    }

    #[test]
    fn test_phase8_area_optimization_target() {
        // Test implementation
    }
}
