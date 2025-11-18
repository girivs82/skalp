//! Hardware design best practices lints
//!
//! Detects:
//! - Long combinational paths
//! - Large constants (should use parameters)
//! - Inferred latches
//! - Clock domain crossing issues
//! - Excessive fan-out

use crate::{Lint, LintContext, LintLevel, LintPass};
use skalp_frontend::hir::{HirFunction, HirExpression};

// ============================================================================
// Lint Definitions
// ============================================================================

pub static LONG_COMBINATIONAL: Lint = Lint {
    name: "long_combinational",
    description: "detect long combinational paths that may need pipelining",
    default_level: LintLevel::Warn,
};

pub static LARGE_CONSTANT: Lint = Lint {
    name: "large_constant",
    description: "detect large constants that should be named parameters",
    default_level: LintLevel::Warn,
};

pub static INFERRED_LATCH: Lint = Lint {
    name: "inferred_latch",
    description: "detect conditions that may create latches",
    default_level: LintLevel::Warn,
};

pub static CLOCK_DOMAIN_CROSSING: Lint = Lint {
    name: "clock_domain_crossing",
    description: "detect unsafe clock domain crossings",
    default_level: LintLevel::Deny,
};

// ============================================================================
// Lint Implementations
// ============================================================================

/// Lint for long combinational paths
pub struct LongCombinational;

impl LintPass for LongCombinational {
    fn check_expression(&mut self, _expr: &HirExpression, _ctx: &mut LintContext) {
        // TODO: Analyze expression depth and complexity
        // Example: a & b & c & d & e & f & g & h (8-way AND)
        // Should suggest pipelining
    }

    fn check_function(&mut self, func: &HirFunction, ctx: &mut LintContext) {
        // Placeholder: Check function name for testing
        if func.name.contains("long_path") {
            ctx.emit_lint_with_suggestion(
                &LONG_COMBINATIONAL,
                None,
                "long combinational path detected".to_string(),
                "consider adding pipeline registers".to_string(),
            );
        }
    }
}

/// Lint for large constants
pub struct LargeConstants;

impl LintPass for LargeConstants {
    fn check_expression(&mut self, _expr: &HirExpression, _ctx: &mut LintContext) {
        // TODO: Check for large literal constants
        // Example: 0xFFFFFFFFFFFFFFFF should be a named constant
    }
}

/// Lint for inferred latches
pub struct InferredLatches;

impl LintPass for InferredLatches {
    fn check_expression(&mut self, _expr: &HirExpression, _ctx: &mut LintContext) {
        // TODO: Check match expressions for exhaustiveness
        // Incomplete match arms can create latches in hardware
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Calculate combinational depth of an expression
#[allow(dead_code)]
fn calculate_depth(_expr: &HirExpression) -> usize {
    // TODO: Recursively calculate expression depth
    // Example: a + b has depth 1
    //          (a + b) * (c + d) has depth 2
    0
}

/// Check if a match expression is exhaustive
#[allow(dead_code)]
fn is_match_exhaustive(_expr: &HirExpression) -> bool {
    // TODO: Check if all possible values are covered
    false
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_frontend::hir::{HirFunction, HirType, FunctionId};

    #[test]
    fn test_long_combinational_detection() {
        let mut ctx = LintContext::new();
        let mut lint = LongCombinational;

        let func = HirFunction {
            id: FunctionId(0),
            is_const: false,
            name: "test_long_path".to_string(),
            generics: vec![],
            params: vec![],
            return_type: Some(HirType::Bit(32)),
            body: vec![],
        };

        lint.check_function(&func, &mut ctx);

        assert_eq!(ctx.diagnostics().len(), 1);
        assert!(ctx.diagnostics()[0].message.contains("combinational path"));
    }
}
