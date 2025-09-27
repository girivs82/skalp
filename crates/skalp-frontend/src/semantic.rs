//! Semantic analysis for SKALP
//!
//! Type checking, name resolution, and semantic validation

use crate::ast::SourceFile;
use std::collections::HashMap;

/// Semantic analyzer
pub struct SemanticAnalyzer {
    /// Symbol table
    symbols: SymbolTable,
    /// Type environment
    types: TypeEnvironment,
    /// Current scope depth
    scope_depth: usize,
}

/// Symbol table for name resolution
#[derive(Debug, Clone)]
pub struct SymbolTable {
    /// Scopes stack
    scopes: Vec<HashMap<String, Symbol>>,
}

/// Symbol information
#[derive(Debug, Clone)]
pub struct Symbol {
    /// Symbol name
    pub name: String,
    /// Symbol type
    pub symbol_type: SymbolType,
    /// Scope where defined
    pub scope: usize,
    /// Whether symbol is mutable
    pub mutable: bool,
}

/// Types of symbols
#[derive(Debug, Clone)]
pub enum SymbolType {
    Entity,
    Signal,
    Variable,
    Constant,
    Port,
    Function,
    Type,
}

/// Type environment for type checking
#[derive(Debug, Clone)]
pub struct TypeEnvironment {
    /// Type bindings
    bindings: HashMap<String, Type>,
}

/// Type representation for semantic analysis
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Bit(Option<u32>),
    Logic(Option<u32>),
    Int(Option<u32>),
    Nat(Option<u32>),
    Clock,
    Reset,
    Event,
    Array(Box<Type>, u32),
    Custom(String),
    Error, // For error recovery
}

/// Semantic analysis result
#[derive(Debug)]
pub struct SemanticResult {
    /// Whether analysis succeeded
    pub success: bool,
    /// Semantic errors found
    pub errors: Vec<SemanticError>,
    /// Warnings generated
    pub warnings: Vec<SemanticWarning>,
}

/// Semantic errors
#[derive(Debug, Clone)]
pub enum SemanticError {
    /// Undefined symbol
    UndefinedSymbol {
        name: String,
        position: usize,
    },
    /// Type mismatch
    TypeMismatch {
        expected: Type,
        found: Type,
        position: usize,
    },
    /// Duplicate definition
    DuplicateDefinition {
        name: String,
        first_position: usize,
        second_position: usize,
    },
    /// Invalid assignment
    InvalidAssignment {
        message: String,
        position: usize,
    },
}

/// Semantic warnings
#[derive(Debug, Clone)]
pub enum SemanticWarning {
    /// Unused symbol
    UnusedSymbol {
        name: String,
        position: usize,
    },
    /// Dead code
    DeadCode {
        position: usize,
    },
}

impl SemanticAnalyzer {
    /// Create a new semantic analyzer
    pub fn new() -> Self {
        Self {
            symbols: SymbolTable::new(),
            types: TypeEnvironment::new(),
            scope_depth: 0,
        }
    }

    /// Analyze a source file
    pub fn analyze(&mut self, _source_file: &SourceFile) -> SemanticResult {
        // Stub implementation - will be expanded in Week 3
        SemanticResult {
            success: true,
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    /// Enter a new scope
    pub fn enter_scope(&mut self) {
        self.symbols.enter_scope();
        self.scope_depth += 1;
    }

    /// Exit current scope
    pub fn exit_scope(&mut self) {
        self.symbols.exit_scope();
        if self.scope_depth > 0 {
            self.scope_depth -= 1;
        }
    }

    /// Define a symbol
    pub fn define_symbol(&mut self, symbol: Symbol) -> Result<(), SemanticError> {
        self.symbols.define(symbol)
    }

    /// Look up a symbol
    pub fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        self.symbols.lookup(name)
    }
}

impl SymbolTable {
    /// Create a new symbol table
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()], // Global scope
        }
    }

    /// Enter a new scope
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Exit current scope
    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Define a symbol in current scope
    pub fn define(&mut self, symbol: Symbol) -> Result<(), SemanticError> {
        if let Some(current_scope) = self.scopes.last_mut() {
            if current_scope.contains_key(&symbol.name) {
                return Err(SemanticError::DuplicateDefinition {
                    name: symbol.name.clone(),
                    first_position: 0, // Would need proper position tracking
                    second_position: 0,
                });
            }
            current_scope.insert(symbol.name.clone(), symbol);
        }
        Ok(())
    }

    /// Look up a symbol in all scopes
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }
}

impl TypeEnvironment {
    /// Create a new type environment
    pub fn new() -> Self {
        let mut bindings = HashMap::new();

        // Add built-in types
        bindings.insert("bit".to_string(), Type::Bit(None));
        bindings.insert("logic".to_string(), Type::Logic(None));
        bindings.insert("int".to_string(), Type::Int(None));
        bindings.insert("nat".to_string(), Type::Nat(None));
        bindings.insert("clock".to_string(), Type::Clock);
        bindings.insert("reset".to_string(), Type::Reset);
        bindings.insert("event".to_string(), Type::Event);

        Self { bindings }
    }

    /// Look up a type
    pub fn lookup(&self, name: &str) -> Option<&Type> {
        self.bindings.get(name)
    }

    /// Define a new type
    pub fn define(&mut self, name: String, type_def: Type) {
        self.bindings.insert(name, type_def);
    }
}

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for TypeEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SemanticError::UndefinedSymbol { name, position } => {
                write!(f, "Undefined symbol '{}' at position {}", name, position)
            }
            SemanticError::TypeMismatch { expected, found, position } => {
                write!(f, "Type mismatch at position {}: expected {:?}, found {:?}", position, expected, found)
            }
            SemanticError::DuplicateDefinition { name, first_position, second_position } => {
                write!(f, "Duplicate definition of '{}': first at {}, second at {}", name, first_position, second_position)
            }
            SemanticError::InvalidAssignment { message, position } => {
                write!(f, "Invalid assignment at position {}: {}", position, message)
            }
        }
    }
}

impl std::error::Error for SemanticError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_table() {
        let mut table = SymbolTable::new();

        let symbol = Symbol {
            name: "test".to_string(),
            symbol_type: SymbolType::Variable,
            scope: 0,
            mutable: true,
        };

        assert!(table.define(symbol).is_ok());
        assert!(table.lookup("test").is_some());
        assert!(table.lookup("nonexistent").is_none());
    }

    #[test]
    fn test_type_environment() {
        let env = TypeEnvironment::new();

        assert!(env.lookup("bit").is_some());
        assert!(env.lookup("logic").is_some());
        assert!(env.lookup("clock").is_some());
        assert!(env.lookup("nonexistent").is_none());
    }
}