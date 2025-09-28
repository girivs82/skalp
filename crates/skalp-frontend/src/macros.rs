//! Macro system for SKALP metaprogramming

use crate::ast::{Type, Statement, Expression, Literal};
use std::collections::HashMap;
use thiserror::Error;

/// Token for macro system (different from lexer Token)
#[derive(Debug, Clone)]
pub struct MacroToken {
    pub kind: TokenKind,
    pub span: std::ops::Range<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Identifier(String),
    IntLiteral(i64),
    StringLiteral(String),
    Keyword(String),
    Symbol(char),
    Comment(String),
}

#[derive(Error, Debug)]
pub enum MacroError {
    #[error("Unknown macro: {0}")]
    UnknownMacro(String),
    #[error("Invalid macro syntax: {0}")]
    InvalidSyntax(String),
    #[error("Macro expansion failed: {0}")]
    ExpansionFailed(String),
    #[error("Recursive macro limit exceeded")]
    RecursionLimit,
    #[error("Invalid macro argument: {0}")]
    InvalidArgument(String),
}

/// Macro definition
#[derive(Debug, Clone)]
pub struct MacroDef {
    /// Macro name
    pub name: String,
    /// Parameters
    pub params: Vec<MacroParam>,
    /// Body tokens
    pub body: Vec<MacroToken>,
    /// Whether this is a procedural macro
    pub is_proc: bool,
}

/// Macro parameter
#[derive(Debug, Clone)]
pub struct MacroParam {
    /// Parameter name
    pub name: String,
    /// Parameter kind
    pub kind: MacroParamKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MacroParamKind {
    /// Token parameter (single token)
    Token,
    /// Expression parameter
    Expr,
    /// Type parameter
    Type,
    /// Statement parameter
    Stmt,
    /// Block parameter
    Block,
    /// Repetition parameter
    Repetition { separator: Option<String> },
}

/// Macro invocation
#[derive(Debug, Clone)]
pub struct MacroInvocation {
    /// Macro name
    pub name: String,
    /// Arguments
    pub args: Vec<MacroArg>,
    /// Span
    pub span: std::ops::Range<usize>,
}

/// Macro argument
#[derive(Debug, Clone)]
pub enum MacroArg {
    /// Token argument
    Token(MacroToken),
    /// Expression argument
    Expr(Expression),
    /// Type argument
    Type(Type),
    /// Statement argument
    Stmt(Statement),
    /// Token tree (for complex arguments)
    TokenTree(Vec<MacroToken>),
}

/// Macro expansion context
pub struct MacroExpander {
    /// Defined macros
    macros: HashMap<String, MacroDef>,
    /// Recursion depth
    depth: usize,
    /// Maximum recursion depth
    max_depth: usize,
}

impl Default for MacroExpander {
    fn default() -> Self {
        Self::new()
    }
}

impl MacroExpander {
    pub fn new() -> Self {
        let mut expander = Self {
            macros: HashMap::new(),
            depth: 0,
            max_depth: 32,
        };

        // Register built-in macros
        expander.register_builtins();
        expander
    }

    fn register_builtins(&mut self) {
        // Register @generate macro for repetitive structures
        self.register_macro(MacroDef {
            name: "generate".to_string(),
            params: vec![
                MacroParam {
                    name: "count".to_string(),
                    kind: MacroParamKind::Expr,
                },
                MacroParam {
                    name: "body".to_string(),
                    kind: MacroParamKind::Block,
                },
            ],
            body: vec![],  // Built-in implementation
            is_proc: true,
        });

        // Register @assert macro for compile-time assertions
        self.register_macro(MacroDef {
            name: "assert".to_string(),
            params: vec![
                MacroParam {
                    name: "condition".to_string(),
                    kind: MacroParamKind::Expr,
                },
                MacroParam {
                    name: "message".to_string(),
                    kind: MacroParamKind::Token,
                },
            ],
            body: vec![],  // Built-in implementation
            is_proc: true,
        });

        // Register @bits macro for bit manipulation
        self.register_macro(MacroDef {
            name: "bits".to_string(),
            params: vec![
                MacroParam {
                    name: "width".to_string(),
                    kind: MacroParamKind::Expr,
                },
            ],
            body: vec![],  // Built-in implementation
            is_proc: true,
        });

        // Register @pipeline macro for pipeline generation
        self.register_macro(MacroDef {
            name: "pipeline".to_string(),
            params: vec![
                MacroParam {
                    name: "stages".to_string(),
                    kind: MacroParamKind::Expr,
                },
                MacroParam {
                    name: "body".to_string(),
                    kind: MacroParamKind::Block,
                },
            ],
            body: vec![],  // Built-in implementation
            is_proc: true,
        });

        // Register @derive macro for trait derivation
        self.register_macro(MacroDef {
            name: "derive".to_string(),
            params: vec![
                MacroParam {
                    name: "traits".to_string(),
                    kind: MacroParamKind::Repetition {
                        separator: Some(",".to_string())
                    },
                },
            ],
            body: vec![],  // Built-in implementation
            is_proc: true,
        });
    }

    /// Register a macro definition
    pub fn register_macro(&mut self, def: MacroDef) {
        self.macros.insert(def.name.clone(), def);
    }

    /// Expand a macro invocation
    pub fn expand(&mut self, invocation: &MacroInvocation) -> Result<Vec<MacroToken>, MacroError> {
        // Check recursion depth
        if self.depth >= self.max_depth {
            return Err(MacroError::RecursionLimit);
        }

        self.depth += 1;
        let result = self.expand_impl(invocation);
        self.depth -= 1;

        result
    }

    fn expand_impl(&mut self, invocation: &MacroInvocation) -> Result<Vec<MacroToken>, MacroError> {
        let macro_def = self.macros.get(&invocation.name)
            .ok_or_else(|| MacroError::UnknownMacro(invocation.name.clone()))?
            .clone();

        if macro_def.is_proc {
            self.expand_proc_macro(&macro_def, invocation)
        } else {
            self.expand_declarative_macro(&macro_def, invocation)
        }
    }

    fn expand_proc_macro(
        &mut self,
        def: &MacroDef,
        invocation: &MacroInvocation,
    ) -> Result<Vec<MacroToken>, MacroError> {
        match def.name.as_str() {
            "generate" => self.expand_generate(invocation),
            "assert" => self.expand_assert(invocation),
            "bits" => self.expand_bits(invocation),
            "pipeline" => self.expand_pipeline(invocation),
            "derive" => self.expand_derive(invocation),
            _ => Err(MacroError::UnknownMacro(def.name.clone())),
        }
    }

    fn expand_declarative_macro(
        &mut self,
        def: &MacroDef,
        invocation: &MacroInvocation,
    ) -> Result<Vec<MacroToken>, MacroError> {
        // Match parameters to arguments
        if def.params.len() != invocation.args.len() {
            return Err(MacroError::InvalidArgument(
                format!("Expected {} arguments, got {}",
                    def.params.len(), invocation.args.len())
            ));
        }

        // Create substitution map
        let mut substitutions = HashMap::new();
        for (param, arg) in def.params.iter().zip(&invocation.args) {
            substitutions.insert(param.name.clone(), arg.clone());
        }

        // Substitute in body
        let mut result = Vec::new();
        for token in &def.body {
            if let TokenKind::Identifier(name) = &token.kind {
                if let Some(arg) = substitutions.get(name) {
                    // Substitute parameter with argument
                    result.extend(self.arg_to_tokens(arg)?);
                } else {
                    result.push(token.clone());
                }
            } else {
                result.push(token.clone());
            }
        }

        Ok(result)
    }

    fn expand_generate(&mut self, invocation: &MacroInvocation) -> Result<Vec<MacroToken>, MacroError> {
        // @generate(N) { body } - generates N copies of body with $i substituted
        if invocation.args.len() != 2 {
            return Err(MacroError::InvalidArgument(
                "generate requires 2 arguments".to_string()
            ));
        }

        let count = match &invocation.args[0] {
            MacroArg::Expr(Expression::Literal(Literal::Decimal(n))) => {
                // Use integer literal directly
                *n as usize
            }
            _ => return Err(MacroError::InvalidArgument("count must be literal".to_string())),
        };

        let body = match &invocation.args[1] {
            MacroArg::TokenTree(tokens) => tokens,
            _ => return Err(MacroError::InvalidArgument("body must be token tree".to_string())),
        };

        let mut result = Vec::new();
        for i in 0..count {
            // Substitute $i with current index
            for token in body {
                if let TokenKind::Identifier(name) = &token.kind {
                    if name == "$i" {
                        result.push(MacroToken {
                            kind: TokenKind::IntLiteral(i as i64),
                            span: token.span.clone(),
                        });
                    } else {
                        result.push(token.clone());
                    }
                } else {
                    result.push(token.clone());
                }
            }
        }

        Ok(result)
    }

    fn expand_assert(&mut self, invocation: &MacroInvocation) -> Result<Vec<MacroToken>, MacroError> {
        // @assert(condition, message) - compile-time assertion
        if invocation.args.len() != 2 {
            return Err(MacroError::InvalidArgument(
                "assert requires 2 arguments".to_string()
            ));
        }

        // For now, just generate a static assertion
        // In full implementation, would evaluate at compile time
        Ok(vec![
            MacroToken {
                kind: TokenKind::Keyword("static_assert".to_string()),
                span: invocation.span.clone(),
            },
        ])
    }

    fn expand_bits(&mut self, invocation: &MacroInvocation) -> Result<Vec<MacroToken>, MacroError> {
        // @bits(width) - generates bit<width> type
        if invocation.args.len() != 1 {
            return Err(MacroError::InvalidArgument(
                "bits requires 1 argument".to_string()
            ));
        }

        let width = match &invocation.args[0] {
            MacroArg::Expr(Expression::Literal(Literal::Decimal(w))) => w.to_string(),
            _ => return Err(MacroError::InvalidArgument("width must be literal".to_string())),
        };

        Ok(vec![
            MacroToken {
                kind: TokenKind::Keyword("bit".to_string()),
                span: invocation.span.clone(),
            },
            MacroToken {
                kind: TokenKind::Symbol('<'),
                span: invocation.span.clone(),
            },
            MacroToken {
                kind: TokenKind::IntLiteral(width.parse().unwrap_or(0)),
                span: invocation.span.clone(),
            },
            MacroToken {
                kind: TokenKind::Symbol('>'),
                span: invocation.span.clone(),
            },
        ])
    }

    fn expand_pipeline(&mut self, invocation: &MacroInvocation) -> Result<Vec<MacroToken>, MacroError> {
        // @pipeline(stages) { body } - generates pipeline stages
        if invocation.args.len() != 2 {
            return Err(MacroError::InvalidArgument(
                "pipeline requires 2 arguments".to_string()
            ));
        }

        // Generate pipeline structure
        // Simplified for now - full implementation would generate complete pipeline
        Ok(vec![
            MacroToken {
                kind: TokenKind::Keyword("pipeline".to_string()),
                span: invocation.span.clone(),
            },
        ])
    }

    fn expand_derive(&mut self, invocation: &MacroInvocation) -> Result<Vec<MacroToken>, MacroError> {
        // @derive(Trait1, Trait2, ...) - derives trait implementations
        // Simplified for now - full implementation would generate trait impls
        Ok(vec![
            MacroToken {
                kind: TokenKind::Comment("// Derived traits".to_string()),
                span: invocation.span.clone(),
            },
        ])
    }

    fn arg_to_tokens(&self, arg: &MacroArg) -> Result<Vec<MacroToken>, MacroError> {
        match arg {
            MacroArg::Token(t) => Ok(vec![t.clone()]),
            MacroArg::TokenTree(tokens) => Ok(tokens.clone()),
            MacroArg::Expr(_) | MacroArg::Type(_) | MacroArg::Stmt(_) => {
                // Would need to serialize back to tokens
                Ok(vec![])
            }
        }
    }
}

/// Hygienic macro expansion
pub struct HygienicExpander {
    /// Base expander
    base: MacroExpander,
    /// Scope counter for hygiene
    scope_counter: usize,
}

impl Default for HygienicExpander {
    fn default() -> Self {
        Self::new()
    }
}

impl HygienicExpander {
    pub fn new() -> Self {
        Self {
            base: MacroExpander::new(),
            scope_counter: 0,
        }
    }

    /// Expand with hygiene
    pub fn expand(&mut self, invocation: &MacroInvocation) -> Result<Vec<MacroToken>, MacroError> {
        self.scope_counter += 1;
        let scope = self.scope_counter;

        let mut tokens = self.base.expand(invocation)?;

        // Rename identifiers to preserve hygiene
        for token in &mut tokens {
            if let TokenKind::Identifier(name) = &token.kind {
                if !name.starts_with("$") {
                    // Add scope suffix to prevent name collisions
                    let hygienic_name = format!("{}_{}", name, scope);
                    token.kind = TokenKind::Identifier(hygienic_name);
                }
            }
        }

        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_macro() {
        let mut expander = MacroExpander::new();

        let invocation = MacroInvocation {
            name: "generate".to_string(),
            args: vec![
                MacroArg::Expr(Expression::Literal(Literal::Decimal(3))),
                MacroArg::TokenTree(vec![
                    MacroToken {
                        kind: TokenKind::Identifier("wire".to_string()),
                        span: 0..4,
                    },
                    MacroToken {
                        kind: TokenKind::Identifier("$i".to_string()),
                        span: 5..7,
                    },
                ]),
            ],
            span: 0..20,
        };

        let result = expander.expand(&invocation).unwrap();
        assert_eq!(result.len(), 6); // 3 * 2 tokens
    }

    #[test]
    fn test_bits_macro() {
        let mut expander = MacroExpander::new();

        let invocation = MacroInvocation {
            name: "bits".to_string(),
            args: vec![
                MacroArg::Expr(Expression::Literal(Literal::Decimal(16))),
            ],
            span: 0..10,
        };

        let result = expander.expand(&invocation).unwrap();
        assert_eq!(result.len(), 4); // bit < 16 >
    }

    #[test]
    fn test_hygienic_expansion() {
        let mut expander = HygienicExpander::new();

        let invocation = MacroInvocation {
            name: "assert".to_string(),
            args: vec![
                MacroArg::Expr(Expression::Literal(Literal::Bool(true))),
                MacroArg::Token(MacroToken {
                    kind: TokenKind::StringLiteral("assertion failed".to_string()),
                    span: 0..16,
                }),
            ],
            span: 0..30,
        };

        let result = expander.expand(&invocation).unwrap();
        assert!(!result.is_empty());
    }
}