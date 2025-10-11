//! Lockfile generation and parsing

use crate::error::{PackageError, Result};
use crate::source::PackageSource;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

/// Lockfile structure (skalp.lock)
///
/// Similar to Cargo.lock, this records exact versions of all dependencies
/// to ensure reproducible builds.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Lockfile {
    /// Lockfile format version
    pub version: u32,

    /// Resolved package sources
    #[serde(rename = "package")]
    pub packages: Vec<LockedPackage>,

    /// Metadata
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub metadata: Option<LockfileMetadata>,
}

/// A locked package with its dependencies
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockedPackage {
    /// Package name
    pub name: String,

    /// Resolved source kind
    #[serde(flatten)]
    pub source_kind: crate::source::SourceKind,

    /// Direct dependencies of this package
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub dependencies: Vec<String>,

    /// Checksum for integrity verification
    #[serde(skip_serializing_if = "Option::is_none")]
    pub checksum: Option<String>,
}

impl LockedPackage {
    /// Create a new locked package
    pub fn new(source: PackageSource, dependencies: Vec<String>, checksum: Option<String>) -> Self {
        Self {
            name: source.name,
            source_kind: source.kind,
            dependencies,
            checksum,
        }
    }

    /// Get the full package source
    pub fn source(&self) -> PackageSource {
        PackageSource {
            name: self.name.clone(),
            kind: self.source_kind.clone(),
        }
    }
}

/// Lockfile metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockfileMetadata {
    /// When the lockfile was generated
    pub generated: String,
    /// Root package name
    pub root: String,
}

impl Lockfile {
    /// Create a new empty lockfile
    pub fn new() -> Self {
        Self {
            version: 1,
            packages: Vec::new(),
            metadata: None,
        }
    }

    /// Load a lockfile from disk
    pub fn load(path: impl AsRef<Path>) -> Result<Self> {
        let contents = fs::read_to_string(path.as_ref())
            .map_err(|e| PackageError::Lockfile(format!("Failed to read lockfile: {}", e)))?;

        toml::from_str(&contents)
            .map_err(|e| PackageError::Lockfile(format!("Failed to parse lockfile: {}", e)))
    }

    /// Save the lockfile to disk
    pub fn save(&self, path: impl AsRef<Path>) -> Result<()> {
        let contents = toml::to_string_pretty(self)
            .map_err(|e| PackageError::Lockfile(format!("Failed to serialize lockfile: {}", e)))?;

        fs::write(path.as_ref(), contents)
            .map_err(|e| PackageError::Lockfile(format!("Failed to write lockfile: {}", e)))
    }

    /// Add a locked package
    pub fn add_package(&mut self, package: LockedPackage) {
        self.packages.push(package);
    }

    /// Get a package by name
    pub fn get_package(&self, name: &str) -> Option<&LockedPackage> {
        self.packages.iter().find(|p| p.name == name)
    }

    /// Get all packages as a map
    pub fn packages_map(&self) -> HashMap<String, &LockedPackage> {
        self.packages.iter().map(|p| (p.name.clone(), p)).collect()
    }

    /// Validate the lockfile
    pub fn validate(&self) -> Result<()> {
        // Check version
        if self.version != 1 {
            return Err(PackageError::Lockfile(format!(
                "Unsupported lockfile version: {}",
                self.version
            )));
        }

        // Check for duplicate packages
        let mut seen = std::collections::HashSet::new();
        for package in &self.packages {
            if !seen.insert(&package.name) {
                return Err(PackageError::Lockfile(format!(
                    "Duplicate package in lockfile: {}",
                    package.name
                )));
            }
        }

        // Validate each package source
        for package in &self.packages {
            package.source().validate()?;
        }

        Ok(())
    }

    /// Check if lockfile is up to date with manifest dependencies
    pub fn is_up_to_date(&self, manifest_deps: &[String]) -> bool {
        let locked_names: std::collections::HashSet<_> =
            self.packages.iter().map(|p| &p.name).collect();

        for dep in manifest_deps {
            if !locked_names.contains(dep) {
                return false;
            }
        }

        true
    }
}

impl Default for Lockfile {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_lockfile() {
        let lockfile = Lockfile::new();
        assert_eq!(lockfile.version, 1);
        assert_eq!(lockfile.packages.len(), 0);
    }

    #[test]
    fn test_add_package() {
        let mut lockfile = Lockfile::new();
        let source = PackageSource::registry("skalp-numeric", "2.0");
        lockfile.add_package(LockedPackage::new(
            source,
            vec![],
            Some("abc123".to_string()),
        ));

        assert_eq!(lockfile.packages.len(), 1);
        assert!(lockfile.get_package("skalp-numeric").is_some());
    }

    #[test]
    fn test_validate_duplicate_packages() {
        let mut lockfile = Lockfile::new();
        let source1 = PackageSource::registry("lib", "1.0");
        lockfile.add_package(LockedPackage::new(source1, vec![], None));

        let source2 = PackageSource::registry("lib", "2.0");
        lockfile.add_package(LockedPackage::new(source2, vec![], None));

        assert!(lockfile.validate().is_err());
    }

    #[test]
    fn test_is_up_to_date() {
        let mut lockfile = Lockfile::new();
        let source = PackageSource::registry("lib1", "1.0");
        lockfile.add_package(LockedPackage::new(source, vec![], None));

        assert!(lockfile.is_up_to_date(&["lib1".to_string()]));
        assert!(!lockfile.is_up_to_date(&["lib1".to_string(), "lib2".to_string()]));
    }

    #[test]
    fn test_serialize_deserialize() {
        let mut lockfile = Lockfile::new();
        let source = PackageSource::registry("skalp-numeric", "2.0");
        lockfile.add_package(LockedPackage::new(
            source,
            vec!["skalp-stdlib".to_string()],
            Some("abc123".to_string()),
        ));

        let toml_str = toml::to_string(&lockfile).unwrap();
        let deserialized: Lockfile = toml::from_str(&toml_str).unwrap();

        assert_eq!(deserialized.packages.len(), 1);
        assert_eq!(deserialized.packages[0].name, "skalp-numeric");
    }
}
