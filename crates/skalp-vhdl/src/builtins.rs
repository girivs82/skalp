use skalp_frontend::hir::HirType;
use std::collections::HashSet;

/// Tracks which IEEE library packages have been imported via `use` clauses
#[derive(Default)]
pub struct BuiltinScope {
    imported_packages: HashSet<String>,
}

impl BuiltinScope {
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a use clause: e.g. "ieee.std_logic_1164.all"
    pub fn register_use(&mut self, selected_name: &str) {
        let lower = selected_name.to_ascii_lowercase();
        self.imported_packages.insert(lower);
    }

    /// Check if std_logic_1164 types are available
    pub fn has_std_logic_1164(&self) -> bool {
        self.imported_packages
            .iter()
            .any(|p| p.contains("std_logic_1164"))
    }

    /// Check if numeric_std types are available
    pub fn has_numeric_std(&self) -> bool {
        self.imported_packages
            .iter()
            .any(|p| p.contains("numeric_std"))
    }

    /// Resolve a builtin type name to HirType
    pub fn resolve_builtin_type(&self, name: &str) -> Option<HirType> {
        let lower = name.to_ascii_lowercase();
        match lower.as_str() {
            "std_logic" | "std_ulogic" if self.has_std_logic_1164() => Some(HirType::Logic(1)),
            "std_logic" | "std_ulogic" => {
                // Allow even without explicit import for ergonomics
                Some(HirType::Logic(1))
            }
            "boolean" => Some(HirType::Bool),
            "integer" => Some(HirType::Nat(32)),
            "natural" => Some(HirType::Nat(32)),
            "positive" => Some(HirType::Nat(32)),
            "bit" => Some(HirType::Logic(1)),
            "real" => None, // Not synthesizable
            _ => None,
        }
    }

    /// Check if a function name is a built-in
    pub fn is_builtin_function(&self, name: &str) -> bool {
        let lower = name.to_ascii_lowercase();
        matches!(
            lower.as_str(),
            "rising_edge"
                | "falling_edge"
                | "to_unsigned"
                | "to_signed"
                | "to_integer"
                | "resize"
                | "unsigned"
                | "signed"
                | "std_logic_vector"
                | "std_ulogic_vector"
                | "conv_integer"
                | "conv_std_logic_vector"
        )
    }
}

/// Compute ceiling of log2(n), minimum 1 bit
pub fn clog2(n: u64) -> u32 {
    if n <= 1 {
        1
    } else {
        64 - (n - 1).leading_zeros()
    }
}
