//! SKALP Frontend - Lexer, Parser, and AST
//!
//! This crate handles:
//! - Lexical analysis (tokenization)
//! - Parsing SKALP source to AST
//! - Semantic analysis and type checking
//! - HIR (High-level IR) generation

pub mod ast;
pub mod lexer;
pub mod syntax;
pub mod parse;
pub mod types;
pub mod typeck;
pub mod parser;
pub mod semantic;
pub mod hir;
pub mod hir_builder;

pub use lexer::Lexer;
pub use parser::Parser;
pub use hir::Hir;