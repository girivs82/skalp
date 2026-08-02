// NOTE: These tests are temporarily disabled because they reference the old LIR API
// (transform_mir_to_lir, lir_design.name, lir_design.modules) which has been replaced
// by the new technology mapping infrastructure.
// TODO: Update these tests to use the new MirToLirResult API.
#![allow(unexpected_cfgs)]

#[cfg(test)]
#[cfg(feature = "disabled_old_lir_api")]
mod lir_synthesis_tests {
    use skalp_frontend::parse_and_build_hir;
    use skalp_lir::{lower_to_lir, transform_mir_to_lir};
    use skalp_mir::lower_to_mir;

    #[test]
    fn test_basic_mir_to_lir_conversion() {
        // Test implementation
    }

    #[test]
    fn test_lir_design_creation() {
        // Test implementation
    }

    #[test]
    fn test_complex_design_synthesis() {
        // Test implementation
    }
}
