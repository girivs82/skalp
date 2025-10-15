//! SKALP manifest parsing and manipulation
//!
//! This crate handles parsing and validation of `skalp.toml` manifest files,
//! which define library metadata, dependencies, and build configuration.

pub mod dependency;
pub mod error;
pub mod manifest;

pub use dependency::{Dependency, DependencySpec, VersionReq};
pub use error::{ManifestError, Result};
pub use manifest::{BuildConfig, LibraryConfig, Manifest, Package};

use std::path::Path;

/// Parse a manifest from a file path
pub fn from_path(path: impl AsRef<Path>) -> Result<Manifest> {
    let contents =
        std::fs::read_to_string(path.as_ref()).map_err(|e| ManifestError::Io(e.to_string()))?;
    from_str(&contents)
}

/// Parse a manifest from a string
pub fn from_str(s: &str) -> Result<Manifest> {
    toml::from_str(s).map_err(|e| ManifestError::Parse(e.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_manifest_parse() {
        let toml = r#"
            [package]
            name = "my-lib"
            version = "0.1.0"
            authors = ["Test Author <test@example.com>"]
        "#;

        let manifest = from_str(toml).unwrap();
        assert_eq!(manifest.package.name, "my-lib");
        assert_eq!(manifest.package.version.to_string(), "0.1.0");
    }

    #[test]
    fn test_manifest_with_dependencies() {
        let toml = r#"
            [package]
            name = "my-lib"
            version = "0.1.0"

            [dependencies]
            skalp-numeric = "1.0"
            skalp-dsp = { version = "2.1", features = ["fft"] }
        "#;

        let manifest = from_str(toml).unwrap();
        assert_eq!(manifest.dependencies.len(), 2);
        assert!(manifest.dependencies.contains_key("skalp-numeric"));
        assert!(manifest.dependencies.contains_key("skalp-dsp"));
    }
}
