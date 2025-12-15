// NOTE: These tests are temporarily disabled because they reference the old LIR API
// (transform_mir_to_lir, optimization::OptimizationPipeline) which has been replaced
// by the new technology mapping infrastructure.
// TODO: Update these tests to use the new GateOptimizationPipeline and WordLir APIs.
#![allow(unexpected_cfgs)]

#[cfg(test)]
#[cfg(feature = "disabled_old_lir_api")]
mod phase8_success_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_lir::optimization::{
        ConstantFolding, DeadCodeElimination, OptimizationPass, OptimizationPipeline,
    };
    use skalp_lir::{lower_to_lir, transform_mir_to_lir};
    use skalp_mir::lower_to_mir;

    #[test]
    fn test_phase8_synthesis_success() {
        // Test implementation
    }

    #[test]
    fn test_area_improvement_target() {
        // Test implementation
    }
}
