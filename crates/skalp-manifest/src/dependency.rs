//! Dependency specification types

use crate::error::{ManifestError, Result};
use serde::{Deserialize, Serialize};
use std::fmt;

pub use semver::VersionReq;

/// A dependency specification
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum DependencySpec {
    /// Simple version requirement: "1.0"
    Simple(String),
    /// Detailed specification
    Detailed(Dependency),
}

/// Detailed dependency specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dependency {
    /// Version requirement
    #[serde(default)]
    pub version: Option<String>,

    /// Git repository URL
    #[serde(default)]
    pub git: Option<String>,

    /// Git branch
    #[serde(default)]
    pub branch: Option<String>,

    /// Git tag
    #[serde(default)]
    pub tag: Option<String>,

    /// Git revision (commit hash)
    #[serde(default, rename = "rev")]
    pub revision: Option<String>,

    /// Local path (for development)
    #[serde(default)]
    pub path: Option<String>,

    /// Registry (if not the default)
    #[serde(default)]
    pub registry: Option<String>,

    /// Features to enable
    #[serde(default)]
    pub features: Vec<String>,

    /// Whether to use default features
    #[serde(default = "default_true", rename = "default-features")]
    pub default_features: bool,

    /// Whether this is an optional dependency
    #[serde(default)]
    pub optional: bool,

    /// Package name (if different from dependency key)
    #[serde(default)]
    pub package: Option<String>,
}

fn default_true() -> bool {
    true
}

impl DependencySpec {
    /// Validate the dependency specification
    pub fn validate(&self, name: &str) -> Result<()> {
        match self {
            DependencySpec::Simple(version) => {
                // Parse version requirement to validate
                version.parse::<VersionReq>().map_err(|e| {
                    ManifestError::InvalidDependency(name.to_string(), e.to_string())
                })?;
            }
            DependencySpec::Detailed(dep) => {
                dep.validate(name)?;
            }
        }
        Ok(())
    }

    /// Get the version requirement if specified
    pub fn version_req(&self) -> Option<Result<VersionReq>> {
        match self {
            DependencySpec::Simple(v) => Some(
                v.parse::<VersionReq>()
                    .map_err(|e| ManifestError::InvalidVersion(e.to_string())),
            ),
            DependencySpec::Detailed(dep) => dep.version.as_ref().map(|v| {
                v.parse::<VersionReq>()
                    .map_err(|e| ManifestError::InvalidVersion(e.to_string()))
            }),
        }
    }

    /// Check if this is a git dependency
    pub fn is_git(&self) -> bool {
        matches!(
            self,
            DependencySpec::Detailed(dep) if dep.git.is_some()
        )
    }

    /// Check if this is a path dependency
    pub fn is_path(&self) -> bool {
        matches!(
            self,
            DependencySpec::Detailed(dep) if dep.path.is_some()
        )
    }

    /// Get features to enable for this dependency
    pub fn features(&self) -> Vec<String> {
        match self {
            DependencySpec::Simple(_) => vec![],
            DependencySpec::Detailed(dep) => dep.features.clone(),
        }
    }

    /// Check if default features should be enabled
    pub fn use_default_features(&self) -> bool {
        match self {
            DependencySpec::Simple(_) => true,
            DependencySpec::Detailed(dep) => dep.default_features,
        }
    }

    /// Check if this is an optional dependency
    pub fn is_optional(&self) -> bool {
        matches!(
            self,
            DependencySpec::Detailed(dep) if dep.optional
        )
    }
}

impl Dependency {
    /// Validate the dependency
    pub fn validate(&self, name: &str) -> Result<()> {
        // Must specify at least one source
        let sources = [
            self.version.is_some(),
            self.git.is_some(),
            self.path.is_some(),
        ];

        if !sources.iter().any(|&x| x) {
            return Err(ManifestError::InvalidDependency(
                name.to_string(),
                "Must specify version, git, or path".to_string(),
            ));
        }

        // Cannot specify multiple sources
        if sources.iter().filter(|&&x| x).count() > 1 {
            return Err(ManifestError::InvalidDependency(
                name.to_string(),
                "Cannot specify multiple dependency sources (version, git, path)".to_string(),
            ));
        }

        // Git dependencies must have exactly one of: branch, tag, or rev
        if self.git.is_some() {
            let git_refs = [
                self.branch.is_some(),
                self.tag.is_some(),
                self.revision.is_some(),
            ];

            let ref_count = git_refs.iter().filter(|&&x| x).count();
            if ref_count > 1 {
                return Err(ManifestError::InvalidDependency(
                    name.to_string(),
                    "Git dependency can only specify one of: branch, tag, or rev".to_string(),
                ));
            }
        }

        // Validate version if specified
        if let Some(version) = &self.version {
            version
                .parse::<VersionReq>()
                .map_err(|e| ManifestError::InvalidDependency(name.to_string(), e.to_string()))?;
        }

        Ok(())
    }
}

impl fmt::Display for DependencySpec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DependencySpec::Simple(v) => write!(f, "{}", v),
            DependencySpec::Detailed(dep) => {
                if let Some(version) = &dep.version {
                    write!(f, "{}", version)
                } else if let Some(git) = &dep.git {
                    write!(f, "git: {}", git)
                } else if let Some(path) = &dep.path {
                    write!(f, "path: {}", path)
                } else {
                    write!(f, "unspecified")
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_dependency() {
        let spec = DependencySpec::Simple("1.0".to_string());
        assert!(spec.validate("test").is_ok());

        let version = spec.version_req().unwrap().unwrap();
        assert_eq!(version.to_string(), "^1.0");
    }

    #[test]
    fn test_detailed_dependency() {
        let spec = DependencySpec::Detailed(Dependency {
            version: Some("2.1".to_string()),
            git: None,
            branch: None,
            tag: None,
            revision: None,
            path: None,
            registry: None,
            features: vec!["fft".to_string()],
            default_features: true,
            optional: false,
            package: None,
        });

        assert!(spec.validate("test").is_ok());
        assert_eq!(spec.features(), vec!["fft".to_string()]);
    }

    #[test]
    fn test_git_dependency() {
        let spec = DependencySpec::Detailed(Dependency {
            version: None,
            git: Some("https://github.com/example/lib.git".to_string()),
            branch: Some("main".to_string()),
            tag: None,
            revision: None,
            path: None,
            registry: None,
            features: vec![],
            default_features: true,
            optional: false,
            package: None,
        });

        assert!(spec.validate("test").is_ok());
        assert!(spec.is_git());
    }

    #[test]
    fn test_path_dependency() {
        let spec = DependencySpec::Detailed(Dependency {
            version: None,
            git: None,
            branch: None,
            tag: None,
            revision: None,
            path: Some("../local-lib".to_string()),
            registry: None,
            features: vec![],
            default_features: true,
            optional: false,
            package: None,
        });

        assert!(spec.validate("test").is_ok());
        assert!(spec.is_path());
    }

    #[test]
    fn test_invalid_multiple_sources() {
        let spec = DependencySpec::Detailed(Dependency {
            version: Some("1.0".to_string()),
            git: Some("https://github.com/example/lib.git".to_string()),
            branch: None,
            tag: None,
            revision: None,
            path: None,
            registry: None,
            features: vec![],
            default_features: true,
            optional: false,
            package: None,
        });

        assert!(spec.validate("test").is_err());
    }

    #[test]
    fn test_invalid_no_source() {
        let spec = DependencySpec::Detailed(Dependency {
            version: None,
            git: None,
            branch: None,
            tag: None,
            revision: None,
            path: None,
            registry: None,
            features: vec![],
            default_features: true,
            optional: false,
            package: None,
        });

        assert!(spec.validate("test").is_err());
    }
}
