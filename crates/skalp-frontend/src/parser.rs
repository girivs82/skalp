//! Parser for SKALP using Rowan
//!
//! Converts tokens into a lossless syntax tree

use crate::lexer::{Lexer, TokenWithPos, Token};
use crate::ast::SourceFile;

/// SKALP Parser using Rowan
pub struct Parser {
    /// Input tokens
    tokens: Vec<TokenWithPos>,
    /// Current position
    current: usize,
}

impl Parser {
    /// Create a new parser from source text
    pub fn new(source: &str) -> Self {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        Self {
            tokens,
            current: 0,
        }
    }

    /// Parse the source into an AST
    pub fn parse(&mut self) -> Result<SourceFile, ParseError> {
        // Stub implementation - will be expanded in Week 2
        Ok(SourceFile {
            items: Vec::new(),
        })
    }

    /// Get the current token
    fn current_token(&self) -> Option<&Token> {
        self.tokens.get(self.current).map(|t| &t.token)
    }

    /// Advance to the next token
    fn advance(&mut self) -> Option<&Token> {
        self.current += 1;
        self.current_token()
    }

    /// Check if current token matches expected token
    fn matches(&self, token: &Token) -> bool {
        match (self.current_token(), token) {
            (Some(current), expected) => std::mem::discriminant(current) == std::mem::discriminant(expected),
            _ => false,
        }
    }

    /// Consume a token if it matches, otherwise return error
    fn consume(&mut self, expected: Token) -> Result<(), ParseError> {
        if self.matches(&expected) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: format!("{:?}", expected),
                found: self.current_token().map(|t| format!("{:?}", t)).unwrap_or_else(|| "EOF".to_string()),
                position: self.current,
            })
        }
    }

    /// Check if we're at the end of input
    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }
}

/// Parse errors
#[derive(Debug, Clone)]
pub enum ParseError {
    /// Unexpected token encountered
    UnexpectedToken {
        expected: String,
        found: String,
        position: usize,
    },
    /// Unexpected end of file
    UnexpectedEof {
        expected: String,
    },
    /// Invalid syntax
    InvalidSyntax {
        message: String,
        position: usize,
    },
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken { expected, found, position } => {
                write!(f, "Parse error at position {}: expected {}, found {}", position, expected, found)
            }
            ParseError::UnexpectedEof { expected } => {
                write!(f, "Parse error: unexpected end of file, expected {}", expected)
            }
            ParseError::InvalidSyntax { message, position } => {
                write!(f, "Parse error at position {}: {}", position, message)
            }
        }
    }
}

impl std::error::Error for ParseError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_creation() {
        let mut parser = Parser::new("entity Test {}");
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_empty_source() {
        let mut parser = Parser::new("");
        let result = parser.parse();
        assert!(result.is_ok());
        assert_eq!(result.unwrap().items.len(), 0);
    }
}