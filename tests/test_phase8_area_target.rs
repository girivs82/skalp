// NOTE: These tests are temporarily disabled because they reference the old LIR API
// (transform_mir_to_lir, OptimizationPipeline) which has been replaced by the new
// technology mapping infrastructure.
// TODO: Update these tests to use the new GateOptimizationPipeline and GateNetlist APIs.
#![allow(unexpected_cfgs)]

#[cfg(test)]
#[cfg(feature = "disabled_old_lir_api")]
mod phase8_area_target_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_lir::{transform_mir_to_lir, OptimizationPipeline};
    use skalp_mir::lower_to_mir;

    #[test]
    fn test_redundant_logic_optimization() {
        // Test implementation
    }
}
