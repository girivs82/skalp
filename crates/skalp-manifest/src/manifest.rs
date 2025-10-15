//! Manifest structure definitions

use crate::dependency::DependencySpec;
use crate::error::{ManifestError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Complete manifest for a SKALP library or project
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    /// Package metadata
    pub package: Package,

    /// Dependencies
    #[serde(default)]
    pub dependencies: HashMap<String, DependencySpec>,

    /// Development dependencies (only used for tests)
    #[serde(default, rename = "dev-dependencies")]
    pub dev_dependencies: HashMap<String, DependencySpec>,

    /// Library configuration
    #[serde(default)]
    pub lib: Option<LibraryConfig>,

    /// Build configuration
    #[serde(default)]
    pub build: Option<BuildConfig>,

    /// Features
    #[serde(default)]
    pub features: HashMap<String, Vec<String>>,

    /// Workspace configuration (for multi-package projects)
    #[serde(default)]
    pub workspace: Option<WorkspaceConfig>,

    /// Patch dependencies (override dependencies from a source)
    #[serde(default)]
    pub patch: HashMap<String, HashMap<String, DependencySpec>>,

    /// Replace dependencies (replace a dependency with another)
    #[serde(default)]
    pub replace: HashMap<String, DependencySpec>,
}

/// Package metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    /// Package name (must be unique in registry)
    pub name: String,

    /// Semantic version
    pub version: semver::Version,

    /// Authors
    #[serde(default)]
    pub authors: Vec<String>,

    /// Package description
    #[serde(default)]
    pub description: Option<String>,

    /// Documentation URL
    #[serde(default)]
    pub documentation: Option<String>,

    /// Homepage URL
    #[serde(default)]
    pub homepage: Option<String>,

    /// Repository URL
    #[serde(default)]
    pub repository: Option<String>,

    /// License identifier (SPDX)
    #[serde(default)]
    pub license: Option<String>,

    /// Keywords for discovery
    #[serde(default)]
    pub keywords: Vec<String>,

    /// Categories
    #[serde(default)]
    pub categories: Vec<String>,

    /// Minimum SKALP compiler version required
    #[serde(default)]
    pub skalp: Option<String>,

    /// Edition (future-proofing for language editions)
    #[serde(default)]
    pub edition: Option<String>,
}

/// Library configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LibraryConfig {
    /// Library name (defaults to package name)
    #[serde(default)]
    pub name: Option<String>,

    /// Path to library root (default: "src/lib.sk")
    #[serde(default)]
    pub path: Option<String>,

    /// Whether this library can be used as a dependency
    #[serde(default = "default_true")]
    pub crate_type: bool,
}

fn default_true() -> bool {
    true
}

/// Workspace configuration for multi-package projects
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkspaceConfig {
    /// Member packages
    pub members: Vec<String>,

    /// Packages to exclude
    #[serde(default)]
    pub exclude: Vec<String>,
}

/// Build configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildConfig {
    /// Main entry point (default: "src/main.sk")
    #[serde(default)]
    pub main: Option<String>,

    /// Additional source directories to search for modules
    #[serde(default)]
    pub src_dirs: Vec<String>,

    /// Output directory (default: "build")
    #[serde(default)]
    pub out_dir: Option<String>,
}

impl Manifest {
    /// Validate the manifest
    pub fn validate(&self) -> Result<()> {
        // Validate package name
        self.validate_package_name()?;

        // Validate features
        self.validate_features()?;

        // Validate dependencies
        self.validate_dependencies()?;

        // Validate overrides
        self.validate_overrides()?;

        Ok(())
    }

    /// Validate package name follows naming conventions
    fn validate_package_name(&self) -> Result<()> {
        let name = &self.package.name;

        // Must not be empty
        if name.is_empty() {
            return Err(ManifestError::InvalidPackageName(
                "Package name cannot be empty".to_string(),
            ));
        }

        // Must start with letter
        if !name.chars().next().unwrap().is_alphabetic() {
            return Err(ManifestError::InvalidPackageName(
                "Package name must start with a letter".to_string(),
            ));
        }

        // Can only contain alphanumeric, dash, underscore
        if !name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
        {
            return Err(ManifestError::InvalidPackageName(
                "Package name can only contain letters, numbers, dashes, and underscores"
                    .to_string(),
            ));
        }

        Ok(())
    }

    /// Validate feature specifications
    fn validate_features(&self) -> Result<()> {
        for (feature_name, deps) in &self.features {
            // Feature names must be valid identifiers
            if feature_name.is_empty() {
                return Err(ManifestError::InvalidFeature(
                    "Feature name cannot be empty".to_string(),
                ));
            }

            // Check that referenced features/dependencies exist
            for dep in deps {
                if dep.contains('/') {
                    // Format: "dep_name/feature_name"
                    let parts: Vec<&str> = dep.split('/').collect();
                    if parts.len() != 2 {
                        return Err(ManifestError::InvalidFeature(format!(
                            "Invalid feature dependency format: {}",
                            dep
                        )));
                    }

                    let dep_name = parts[0];
                    if !self.dependencies.contains_key(dep_name)
                        && !self.dev_dependencies.contains_key(dep_name)
                    {
                        return Err(ManifestError::FeatureNotFound(
                            dep.clone(),
                            self.package.name.clone(),
                        ));
                    }
                }
            }
        }

        Ok(())
    }

    /// Validate dependency specifications
    fn validate_dependencies(&self) -> Result<()> {
        for (name, spec) in &self.dependencies {
            spec.validate(name)?;
        }

        for (name, spec) in &self.dev_dependencies {
            spec.validate(name)?;
        }

        Ok(())
    }

    /// Validate override specifications
    fn validate_overrides(&self) -> Result<()> {
        // Validate patch dependencies
        for (source, patches) in &self.patch {
            if source.is_empty() {
                return Err(ManifestError::Validation(
                    "Patch source cannot be empty".to_string(),
                ));
            }

            for (name, spec) in patches {
                spec.validate(name)?;
            }
        }

        // Validate replace dependencies
        for (name, spec) in &self.replace {
            spec.validate(name)?;
        }

        Ok(())
    }

    /// Get all dependencies including those enabled by features
    pub fn get_dependencies_with_features(
        &self,
        enabled_features: &[String],
    ) -> HashMap<String, &DependencySpec> {
        let mut deps = HashMap::new();

        // Add non-optional base dependencies
        for (name, spec) in &self.dependencies {
            if !spec.is_optional() {
                deps.insert(name.clone(), spec);
            }
        }

        // Add dependencies from enabled features
        for feature in enabled_features {
            if let Some(feature_deps) = self.features.get(feature) {
                for dep in feature_deps {
                    if dep.contains('/') {
                        // Feature enables a feature in a dependency
                        // Already handled by dependency itself
                        continue;
                    } else {
                        // Feature enables an optional dependency
                        if let Some(spec) = self.dependencies.get(dep) {
                            deps.insert(dep.clone(), spec);
                        }
                    }
                }
            }
        }

        deps
    }

    /// Apply overrides to a dependency specification
    ///
    /// This resolves [patch] and [replace] sections to determine the actual
    /// dependency source that should be used.
    pub fn apply_overrides(&self, dep_name: &str, spec: &DependencySpec) -> DependencySpec {
        // First check if there's a direct replacement
        if let Some(replacement) = self.replace.get(dep_name) {
            return replacement.clone();
        }

        // Check if this dependency should be patched based on its source
        if let Some(git_url) = self.get_git_source(spec) {
            // Check if there's a patch for this git source
            if let Some(patches) = self.patch.get(git_url) {
                if let Some(patched) = patches.get(dep_name) {
                    return patched.clone();
                }
            }
        }

        // No override applies, return original spec
        spec.clone()
    }

    /// Get the git source URL from a dependency spec, if it's a git dependency
    fn get_git_source<'a>(&self, spec: &'a DependencySpec) -> Option<&'a str> {
        match spec {
            DependencySpec::Detailed(dep) => dep.git.as_deref(),
            _ => None,
        }
    }

    /// Get all dependencies with features and overrides applied
    pub fn get_resolved_dependencies(
        &self,
        enabled_features: &[String],
    ) -> HashMap<String, DependencySpec> {
        let deps = self.get_dependencies_with_features(enabled_features);

        deps.into_iter()
            .map(|(name, spec)| {
                let resolved = self.apply_overrides(&name, spec);
                (name, resolved)
            })
            .collect()
    }
}

impl Default for LibraryConfig {
    fn default() -> Self {
        Self {
            name: None,
            path: Some("src/lib.sk".to_string()),
            crate_type: true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_package_name() {
        let mut manifest = create_test_manifest();

        // Valid names
        manifest.package.name = "my-lib".to_string();
        assert!(manifest.validate().is_ok());

        manifest.package.name = "my_lib".to_string();
        assert!(manifest.validate().is_ok());

        manifest.package.name = "mylib123".to_string();
        assert!(manifest.validate().is_ok());

        // Invalid names
        manifest.package.name = "".to_string();
        assert!(manifest.validate().is_err());

        manifest.package.name = "123lib".to_string();
        assert!(manifest.validate().is_err());

        manifest.package.name = "my lib".to_string();
        assert!(manifest.validate().is_err());
    }

    fn create_test_manifest() -> Manifest {
        Manifest {
            package: Package {
                name: "test".to_string(),
                version: semver::Version::new(1, 0, 0),
                authors: vec![],
                description: None,
                documentation: None,
                homepage: None,
                repository: None,
                license: None,
                keywords: vec![],
                categories: vec![],
                skalp: None,
                edition: None,
            },
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            lib: None,
            build: None,
            features: HashMap::new(),
            workspace: None,
            patch: HashMap::new(),
            replace: HashMap::new(),
        }
    }
}
