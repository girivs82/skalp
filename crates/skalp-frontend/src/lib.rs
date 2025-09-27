//! SKALP Frontend - Lexer, Parser, and AST
//!
//! This crate handles:
//! - Lexical analysis (tokenization)
//! - Parsing SKALP source to AST
//! - Semantic analysis and type checking
//! - HIR (High-level IR) generation

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod semantic;
pub mod hir;

pub use lexer::Lexer;
pub use parser::Parser;
pub use hir::Hir;