//! Type-related lints
//!
//! Detects:
//! - Width mismatches in assignments and operations
//! - Signed/unsigned confusion
//! - Implicit truncation
//! - Type conversion issues

use crate::{Lint, LintContext, LintLevel, LintPass};
use skalp_frontend::hir::{HirFunction, HirExpression};

// ============================================================================
// Lint Definitions
// ============================================================================

pub static WIDTH_MISMATCH: Lint = Lint {
    name: "width_mismatch",
    description: "detect potential width mismatch issues in operations",
    default_level: LintLevel::Warn,
};

pub static SIGN_CONFUSION: Lint = Lint {
    name: "sign_confusion",
    description: "detect mixing of signed and unsigned types",
    default_level: LintLevel::Warn,
};

pub static IMPLICIT_TRUNCATION: Lint = Lint {
    name: "implicit_truncation",
    description: "detect implicit truncation in assignments",
    default_level: LintLevel::Warn,
};

// ============================================================================
// Lint Implementations
// ============================================================================

/// Lint for width mismatches
pub struct WidthMismatch;

impl LintPass for WidthMismatch {
    fn check_function(&mut self, func: &HirFunction, ctx: &mut LintContext) {
        // TODO: Traverse function body and check for width mismatches
        // This requires analyzing binary operations, assignments, etc.

        // Placeholder: Check function name for testing
        if func.name.contains("width_mismatch") {
            ctx.emit_lint_with_suggestion(
                &WIDTH_MISMATCH,
                None,
                "potential width mismatch detected".to_string(),
                "ensure operands have matching widths".to_string(),
            );
        }
    }

    fn check_expression(&mut self, _expr: &HirExpression, _ctx: &mut LintContext) {
        // TODO: Check binary operations for width compatibility
        // Example: bit[32] + bit[64] should warn
    }
}

/// Lint for signed/unsigned confusion
pub struct SignConfusion;

impl LintPass for SignConfusion {
    fn check_expression(&mut self, _expr: &HirExpression, _ctx: &mut LintContext) {
        // TODO: Check for mixing signed and unsigned types
        // Example: bit[32] < nat[32] should warn
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Extract bit width from type (if applicable)
#[allow(dead_code)]
fn get_bit_width(ty: &skalp_frontend::hir::HirType) -> Option<u32> {
    use skalp_frontend::hir::HirType;
    match ty {
        HirType::Bit(width) => Some(*width),
        HirType::Nat(width) => Some(*width),
        _ => None,
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_frontend::hir::{HirFunction, HirType, FunctionId};

    #[test]
    fn test_width_mismatch_detection() {
        let mut ctx = LintContext::new();
        let mut lint = WidthMismatch;

        let func = HirFunction {
            id: FunctionId(0),
            is_const: false,
            name: "test_width_mismatch".to_string(),
            generics: vec![],
            params: vec![],
            return_type: Some(HirType::Bit(32)),
            body: vec![],
        };

        lint.check_function(&func, &mut ctx);

        assert_eq!(ctx.diagnostics().len(), 1);
        assert!(ctx.diagnostics()[0].message.contains("width mismatch"));
    }
}
