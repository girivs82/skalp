//! Error types for package management

use thiserror::Error;

pub type Result<T> = std::result::Result<T, PackageError>;

#[derive(Debug, Error)]
pub enum PackageError {
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Manifest error: {0}")]
    Manifest(#[from] skalp_manifest::ManifestError),

    #[error("Package not found: {0}")]
    PackageNotFound(String),

    #[error("Version not found: {package} {version}")]
    VersionNotFound { package: String, version: String },

    #[error("Dependency resolution failed: {0}")]
    ResolutionFailed(String),

    #[error("Circular dependency detected: {0}")]
    CircularDependency(String),

    #[error("Conflicting versions for {package}: {v1} and {v2}")]
    VersionConflict {
        package: String,
        v1: String,
        v2: String,
    },

    #[error("Download failed: {0}")]
    DownloadFailed(String),

    #[error("Checksum mismatch for {package}: expected {expected}, got {actual}")]
    ChecksumMismatch {
        package: String,
        expected: String,
        actual: String,
    },

    #[error("Invalid package source: {0}")]
    InvalidSource(String),

    #[error("Lockfile error: {0}")]
    Lockfile(String),

    #[error("Cache error: {0}")]
    Cache(String),

    #[error("HTTP error: {0}")]
    Http(String),

    #[error("Parse error: {0}")]
    Parse(String),
}

impl From<reqwest::Error> for PackageError {
    fn from(err: reqwest::Error) -> Self {
        PackageError::Http(err.to_string())
    }
}

impl From<toml::de::Error> for PackageError {
    fn from(err: toml::de::Error) -> Self {
        PackageError::Parse(err.to_string())
    }
}
