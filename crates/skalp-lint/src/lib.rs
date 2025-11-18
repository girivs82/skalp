//! SKALP Linter - Static analysis and code quality checks for SKALP HDL
//!
//! This crate provides a comprehensive linting framework for SKALP code,
//! catching common mistakes, suggesting improvements, and enforcing
//! hardware design best practices.
//!
//! # Architecture
//!
//! The linter uses a visitor pattern over SKALP's HIR (High-level Intermediate
//! Representation) and MIR (Mid-level Intermediate Representation):
//!
//! 1. **LintPass**: Trait for implementing individual lints
//! 2. **LintContext**: Collects diagnostics and manages lint configuration
//! 3. **Lint Registry**: Maps lint names to severity levels
//! 4. **Lints**: Individual lint implementations in the `lints` module
//!
//! # Example
//!
//! ```rust,ignore
//! use skalp_lint::{LintContext, run_lints};
//! use skalp_frontend::parse_and_build_hir;
//!
//! let hir = parse_and_build_hir(source_code)?;
//! let mut ctx = LintContext::new();
//! run_lints(&hir, &mut ctx);
//!
//! for diagnostic in ctx.diagnostics() {
//!     println!("{}", diagnostic);
//! }
//! ```

use indexmap::IndexMap;
use skalp_frontend::hir::{Hir, HirFunction, HirParameter, HirStatement, HirExpression};
use std::fmt;

pub mod lints;

// ============================================================================
// Core Types
// ============================================================================

/// Lint severity level
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LintLevel {
    /// Lint is disabled
    Allow,
    /// Emit warning but continue compilation
    Warn,
    /// Emit error and fail compilation
    Deny,
}

impl fmt::Display for LintLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LintLevel::Allow => write!(f, "allow"),
            LintLevel::Warn => write!(f, "warn"),
            LintLevel::Deny => write!(f, "deny"),
        }
    }
}

/// Individual lint definition
#[derive(Debug, Clone)]
pub struct Lint {
    /// Unique identifier for this lint (e.g., "unused_variable")
    pub name: &'static str,
    /// Human-readable description
    pub description: &'static str,
    /// Default severity level
    pub default_level: LintLevel,
}

impl Lint {
    pub const fn new(
        name: &'static str,
        description: &'static str,
        default_level: LintLevel,
    ) -> Self {
        Self {
            name,
            description,
            default_level,
        }
    }
}

/// Source location for diagnostics
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub file: String,
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.column)
    }
}

/// Lint diagnostic message
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub lint: &'static Lint,
    pub level: LintLevel,
    pub span: Option<Span>,
    pub message: String,
    pub suggestion: Option<String>,
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let level_str = match self.level {
            LintLevel::Allow => return Ok(()), // Don't display allowed lints
            LintLevel::Warn => "warning",
            LintLevel::Deny => "error",
        };

        if let Some(ref span) = self.span {
            write!(f, "{}: ", span)?;
        }

        write!(f, "{}: {}", level_str, self.message)?;

        if let Some(ref suggestion) = self.suggestion {
            write!(f, "\n  help: {}", suggestion)?;
        }

        write!(f, "\n  lint: {}", self.lint.name)?;

        Ok(())
    }
}

// ============================================================================
// Lint Context
// ============================================================================

/// Context for running lints
///
/// Manages lint configuration and collects diagnostics.
pub struct LintContext {
    /// Lint name -> severity level mapping
    lint_levels: IndexMap<String, LintLevel>,
    /// Collected diagnostics
    diagnostics: Vec<Diagnostic>,
}

impl LintContext {
    /// Create a new lint context with default configuration
    pub fn new() -> Self {
        Self {
            lint_levels: IndexMap::new(),
            diagnostics: Vec::new(),
        }
    }

    /// Set the level for a specific lint
    pub fn set_lint_level(&mut self, lint_name: String, level: LintLevel) {
        self.lint_levels.insert(lint_name, level);
    }

    /// Get the effective level for a lint
    pub fn get_lint_level(&self, lint: &Lint) -> LintLevel {
        self.lint_levels
            .get(lint.name)
            .copied()
            .unwrap_or(lint.default_level)
    }

    /// Emit a lint diagnostic
    pub fn emit_lint(
        &mut self,
        lint: &'static Lint,
        span: Option<Span>,
        message: String,
    ) {
        let level = self.get_lint_level(lint);

        if level == LintLevel::Allow {
            return;
        }

        self.diagnostics.push(Diagnostic {
            lint,
            level,
            span,
            message,
            suggestion: None,
        });
    }

    /// Emit a lint diagnostic with suggestion
    pub fn emit_lint_with_suggestion(
        &mut self,
        lint: &'static Lint,
        span: Option<Span>,
        message: String,
        suggestion: String,
    ) {
        let level = self.get_lint_level(lint);

        if level == LintLevel::Allow {
            return;
        }

        self.diagnostics.push(Diagnostic {
            lint,
            level,
            span,
            message,
            suggestion: Some(suggestion),
        });
    }

    /// Get all collected diagnostics
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// Check if any errors were emitted
    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| d.level == LintLevel::Deny)
    }

    /// Get count of warnings
    pub fn warning_count(&self) -> usize {
        self.diagnostics
            .iter()
            .filter(|d| d.level == LintLevel::Warn)
            .count()
    }

    /// Get count of errors
    pub fn error_count(&self) -> usize {
        self.diagnostics
            .iter()
            .filter(|d| d.level == LintLevel::Deny)
            .count()
    }
}

impl Default for LintContext {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Lint Pass Trait
// ============================================================================

/// Trait for implementing lints
///
/// Implement this trait to create a new lint. The lint framework will
/// call the appropriate methods as it traverses the HIR.
pub trait LintPass {
    /// Called for each function in the HIR
    fn check_function(&mut self, _func: &HirFunction, _ctx: &mut LintContext) {}

    /// Called for each parameter in a function
    fn check_parameter(&mut self, _param: &HirParameter, _ctx: &mut LintContext) {}

    /// Called for each statement
    fn check_statement(&mut self, _stmt: &HirStatement, _ctx: &mut LintContext) {}

    /// Called for each expression
    fn check_expression(&mut self, _expr: &HirExpression, _ctx: &mut LintContext) {}
}

// ============================================================================
// Lint Runner
// ============================================================================

/// Run all registered lints on the given HIR
pub fn run_lints(hir: &Hir, ctx: &mut LintContext) {
    // Create lint passes
    let mut passes: Vec<Box<dyn LintPass>> = vec![
        Box::new(lints::unused::UnusedVariables),
        Box::new(lints::unused::UnusedFunctions),
        Box::new(lints::types::WidthMismatch),
        Box::new(lints::hardware::LongCombinational),
    ];

    // Run each lint pass
    for func in &hir.functions {
        for pass in &mut passes {
            pass.check_function(func, ctx);

            // Check parameters
            for param in &func.params {
                pass.check_parameter(param, ctx);
            }

            // TODO: Traverse statements and expressions
            // This requires adding visitor methods to HIR nodes
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lint_context_creation() {
        let ctx = LintContext::new();
        assert_eq!(ctx.diagnostics().len(), 0);
        assert!(!ctx.has_errors());
    }

    static TEST_LINT: Lint = Lint {
        name: "test_lint",
        description: "test lint",
        default_level: LintLevel::Warn,
    };

    static UNUSED_VAR_LINT: Lint = Lint {
        name: "unused_variable",
        description: "detect unused variables",
        default_level: LintLevel::Warn,
    };

    #[test]
    fn test_lint_level_configuration() {
        let mut ctx = LintContext::new();
        ctx.set_lint_level("unused_variable".to_string(), LintLevel::Deny);

        assert_eq!(ctx.get_lint_level(&UNUSED_VAR_LINT), LintLevel::Deny);
    }

    #[test]
    fn test_emit_lint() {
        let mut ctx = LintContext::new();

        ctx.emit_lint(
            &TEST_LINT,
            None,
            "test message".to_string(),
        );

        assert_eq!(ctx.diagnostics().len(), 1);
        assert_eq!(ctx.warning_count(), 1);
        assert_eq!(ctx.error_count(), 0);
    }
}
