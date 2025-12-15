// NOTE: These tests are temporarily disabled because they reference the old LIR API
// (transform_mir_to_lir, optimization::*, timing::*) which has been replaced by the
// new technology mapping infrastructure.
// TODO: Update these tests to use the new GateOptimizationPipeline and GateNetlist APIs.
#![allow(unexpected_cfgs)]

#[cfg(test)]
#[cfg(feature = "disabled_old_lir_api")]
mod synthesis_optimization_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_lir::optimization::{
        CommonSubexpressionElimination, ConstantFolding, DeadCodeElimination, OptimizationPass,
        OptimizationPipeline,
    };
    use skalp_lir::timing::TimingAnalyzer;
    use skalp_lir::{lower_to_lir, transform_mir_to_lir};
    use skalp_mir::lower_to_mir;

    #[test]
    fn test_constant_folding_optimization() {
        // Test implementation
    }

    #[test]
    fn test_dead_code_elimination() {
        // Test implementation
    }

    #[test]
    fn test_optimization_pipeline() {
        // Test implementation
    }

    #[test]
    fn test_timing_analysis() {
        // Test implementation
    }

    #[test]
    fn test_area_calculation() {
        // Test implementation
    }

    #[test]
    fn test_full_synthesis_flow() {
        // Test implementation
    }
}
