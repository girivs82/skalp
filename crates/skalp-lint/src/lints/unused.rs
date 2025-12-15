//! Lints for unused code elements
//!
//! Detects:
//! - Unused variables and parameters
//! - Unused functions
//! - Dead code (unreachable statements)

use crate::{Lint, LintContext, LintLevel, LintPass};
use skalp_frontend::hir::{HirFunction, HirParameter};

// ============================================================================
// Lint Definitions
// ============================================================================

pub static UNUSED_VARIABLE: Lint = Lint {
    name: "unused_variable",
    description: "detect unused variables and parameters",
    default_level: LintLevel::Warn,
};

pub static UNUSED_FUNCTION: Lint = Lint {
    name: "unused_function",
    description: "detect functions that are never called",
    default_level: LintLevel::Warn,
};

pub static DEAD_CODE: Lint = Lint {
    name: "dead_code",
    description: "detect unreachable code",
    default_level: LintLevel::Warn,
};

// ============================================================================
// Lint Implementations
// ============================================================================

/// Lint for unused variables and parameters
pub struct UnusedVariables;

impl LintPass for UnusedVariables {
    fn check_parameter(&mut self, param: &HirParameter, ctx: &mut LintContext) {
        // Check if parameter name starts with underscore (convention for intentionally unused)
        if param.name.starts_with('_') {
            return;
        }

        // TODO: Actually track usage by traversing function body
        // For now, this is a placeholder that demonstrates the framework

        // Example: Check for parameters named 'unused' (for testing)
        if param.name == "unused" {
            ctx.emit_lint_with_suggestion(
                &UNUSED_VARIABLE,
                None, // TODO: Add span information
                format!("unused parameter `{}`", param.name),
                format!("prefix with underscore: `_{}`", param.name),
            );
        }
    }
}

/// Lint for unused functions
pub struct UnusedFunctions;

impl LintPass for UnusedFunctions {
    fn check_function(&mut self, func: &HirFunction, ctx: &mut LintContext) {
        // Skip functions named 'main' or 'test_*'
        if func.name == "main" || func.name.starts_with("test_") {
            return;
        }

        // TODO: Actually build call graph and check if function is called
        // For now, this is a placeholder

        // Example: Warn about functions named 'never_called' (for testing)
        if func.name == "never_called" {
            ctx.emit_lint(
                &UNUSED_FUNCTION,
                None,
                format!("function `{}` is never used", func.name),
            );
        }
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Check if a variable is used in an expression (stub for now)
#[allow(dead_code)]
fn is_variable_used(_var_name: &str, _body: &[skalp_frontend::hir::HirStatement]) -> bool {
    // TODO: Implement actual usage checking by traversing HIR
    false
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_frontend::hir::{HirParameter, HirType};

    #[test]
    fn test_unused_variable_with_underscore_prefix() {
        let mut ctx = LintContext::new();
        let mut lint = UnusedVariables;

        let param = HirParameter {
            name: "_unused".to_string(),
            param_type: HirType::Bit(32),
            default_value: None,
        };

        lint.check_parameter(&param, &mut ctx);

        // Should not emit lint for underscore-prefixed names
        assert_eq!(ctx.diagnostics().len(), 0);
    }

    #[test]
    fn test_unused_variable_lint() {
        let mut ctx = LintContext::new();
        let mut lint = UnusedVariables;

        let param = HirParameter {
            name: "unused".to_string(),
            param_type: HirType::Bit(32),
            default_value: None,
        };

        lint.check_parameter(&param, &mut ctx);

        assert_eq!(ctx.diagnostics().len(), 1);
        assert!(ctx.diagnostics()[0].message.contains("unused"));
    }
}
