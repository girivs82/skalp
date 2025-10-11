//! Name resolution for SKALP
//!
//! This crate handles:
//! - Module system (use/mod/pub)
//! - Name resolution (finding definitions)
//! - Scope management
//! - Visibility checking (pub/pub(crate)/private)
//! - Import resolution

pub mod resolver;
pub mod scope;
pub mod visibility;

pub use resolver::{ResolutionMap, ResolveError, Resolver};
pub use scope::{Scope, ScopeId, ScopeTree};
pub use visibility::Visibility;

/// Unique identifier for a module
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

/// Unique identifier for a use statement
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UseId(pub u32);

/// Unique identifier for any item (entity, module, type, etc.)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ItemId(pub u32);

/// A path in the module system (e.g., skalp::numeric::fp::FpAdd)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub segments: Vec<String>,
}

impl Path {
    pub fn new(segments: Vec<String>) -> Self {
        Self { segments }
    }
}

impl std::str::FromStr for Path {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self {
            segments: s.split("::").map(String::from).collect(),
        })
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.segments.join("::"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path_from_str() {
        let path: Path = "skalp::numeric::fp::FpAdd".parse().unwrap();
        assert_eq!(path.segments.len(), 4);
        assert_eq!(path.segments[0], "skalp");
        assert_eq!(path.segments[3], "FpAdd");
    }

    #[test]
    fn test_path_to_string() {
        let path = Path::new(vec![
            "skalp".to_string(),
            "numeric".to_string(),
            "fp".to_string(),
        ]);
        assert_eq!(path.to_string(), "skalp::numeric::fp");
    }
}
