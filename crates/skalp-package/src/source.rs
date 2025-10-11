//! Package source handling

use crate::error::{PackageError, Result};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Package source type
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum SourceKind {
    /// Registry package with version
    Registry { version: String },
    /// Git repository
    Git {
        url: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        branch: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        tag: Option<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        rev: Option<String>,
    },
    /// Local path
    Path { path: PathBuf },
}

/// Complete package source information
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PackageSource {
    /// Package name
    pub name: String,
    /// Source kind
    #[serde(flatten)]
    pub kind: SourceKind,
}

impl PackageSource {
    /// Create a registry source
    pub fn registry(name: impl Into<String>, version: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            kind: SourceKind::Registry {
                version: version.into(),
            },
        }
    }

    /// Create a git source
    pub fn git(name: impl Into<String>, url: impl Into<String>, reference: GitReference) -> Self {
        let (branch, tag, rev) = match reference {
            GitReference::Branch(b) => (Some(b), None, None),
            GitReference::Tag(t) => (None, Some(t), None),
            GitReference::Rev(r) => (None, None, Some(r)),
        };

        Self {
            name: name.into(),
            kind: SourceKind::Git {
                url: url.into(),
                branch,
                tag,
                rev,
            },
        }
    }

    /// Create a path source
    pub fn path(name: impl Into<String>, path: PathBuf) -> Self {
        Self {
            name: name.into(),
            kind: SourceKind::Path { path },
        }
    }

    /// Get a unique identifier for this source for caching
    pub fn cache_key(&self) -> String {
        match &self.kind {
            SourceKind::Registry { version } => format!("registry/{}/{}", self.name, version),
            SourceKind::Git {
                url,
                branch,
                tag,
                rev,
            } => {
                let ref_str = branch
                    .as_ref()
                    .map(|b| format!("branch={}", b))
                    .or_else(|| tag.as_ref().map(|t| format!("tag={}", t)))
                    .or_else(|| rev.as_ref().map(|r| format!("rev={}", r)))
                    .unwrap_or_else(|| "default".to_string());
                format!("git/{}/{}/{}", url, self.name, ref_str)
            }
            SourceKind::Path { path } => format!("path/{}/{}", self.name, path.display()),
        }
    }

    /// Check if this is a local source (doesn't need downloading)
    pub fn is_local(&self) -> bool {
        matches!(self.kind, SourceKind::Path { .. })
    }

    /// Validate the source
    pub fn validate(&self) -> Result<()> {
        match &self.kind {
            SourceKind::Registry { version } => {
                if version.is_empty() {
                    return Err(PackageError::InvalidSource(
                        "Registry version cannot be empty".to_string(),
                    ));
                }
            }
            SourceKind::Git {
                url,
                branch,
                tag,
                rev,
            } => {
                if url.is_empty() {
                    return Err(PackageError::InvalidSource(
                        "Git URL cannot be empty".to_string(),
                    ));
                }

                // Check that only one reference type is specified
                let refs = [branch.is_some(), tag.is_some(), rev.is_some()];
                if refs.iter().filter(|&&x| x).count() > 1 {
                    return Err(PackageError::InvalidSource(
                        "Git source can only specify one of: branch, tag, or rev".to_string(),
                    ));
                }
            }
            SourceKind::Path { path } => {
                if path.as_os_str().is_empty() {
                    return Err(PackageError::InvalidSource(
                        "Path cannot be empty".to_string(),
                    ));
                }
            }
        }
        Ok(())
    }
}

/// Git reference type
#[derive(Debug, Clone)]
pub enum GitReference {
    Branch(String),
    Tag(String),
    Rev(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_registry_source() {
        let source = PackageSource::registry("skalp-numeric", "2.0");
        assert_eq!(source.name, "skalp-numeric");
        assert!(matches!(source.kind, SourceKind::Registry { .. }));
        assert!(source.validate().is_ok());
        assert!(!source.is_local());
    }

    #[test]
    fn test_git_source() {
        let source = PackageSource::git(
            "skalp-experimental",
            "https://github.com/skalp/experimental",
            GitReference::Branch("main".to_string()),
        );
        assert_eq!(source.name, "skalp-experimental");
        assert!(matches!(source.kind, SourceKind::Git { .. }));
        assert!(source.validate().is_ok());
        assert!(!source.is_local());
    }

    #[test]
    fn test_path_source() {
        let source = PackageSource::path("local-lib", PathBuf::from("../local-lib"));
        assert_eq!(source.name, "local-lib");
        assert!(matches!(source.kind, SourceKind::Path { .. }));
        assert!(source.validate().is_ok());
        assert!(source.is_local());
    }

    #[test]
    fn test_cache_key() {
        let source = PackageSource::registry("lib", "1.0");
        assert_eq!(source.cache_key(), "registry/lib/1.0");

        let source = PackageSource::git(
            "lib",
            "https://github.com/example/lib",
            GitReference::Branch("main".to_string()),
        );
        assert!(source.cache_key().contains("git/"));
        assert!(source.cache_key().contains("branch=main"));
    }

    #[test]
    fn test_invalid_empty_version() {
        let source = PackageSource::registry("lib", "");
        assert!(source.validate().is_err());
    }

    #[test]
    fn test_invalid_empty_git_url() {
        let source = PackageSource::git("lib", "", GitReference::Branch("main".to_string()));
        assert!(source.validate().is_err());
    }
}
