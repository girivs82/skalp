//! Package management for SKALP
//!
//! This crate provides dependency resolution, package downloading, and caching
//! for the SKALP hardware description language.

pub mod cache;
pub mod error;
pub mod lockfile;
pub mod registry;
pub mod resolver;
pub mod source;

pub use cache::CachedPackageInfo;
pub use error::{PackageError, Result};
pub use lockfile::Lockfile;
pub use registry::{PackageMetadata, RegistryClient, VersionInfo};
pub use resolver::Resolver;
pub use source::{PackageSource, SourceKind};

use std::path::PathBuf;

/// Package registry configuration
#[derive(Debug, Clone)]
pub struct RegistryConfig {
    /// Registry URL
    pub url: String,
    /// Cache directory
    pub cache_dir: PathBuf,
}

impl Default for RegistryConfig {
    fn default() -> Self {
        let cache_dir = dirs::cache_dir()
            .unwrap_or_else(|| PathBuf::from(".skalp-cache"))
            .join("skalp");

        Self {
            url: "https://registry.skalp.dev".to_string(),
            cache_dir,
        }
    }
}

// Helper to get cache directory (stub for now, will use dirs crate later)
mod dirs {
    use std::path::PathBuf;

    pub fn cache_dir() -> Option<PathBuf> {
        std::env::var_os("HOME").map(|home| PathBuf::from(home).join(".cache"))
    }
}
