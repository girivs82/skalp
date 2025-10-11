//! Error types for manifest parsing and validation

use thiserror::Error;

/// Result type for manifest operations
pub type Result<T> = std::result::Result<T, ManifestError>;

/// Errors that can occur during manifest operations
#[derive(Debug, Error)]
pub enum ManifestError {
    /// I/O error reading manifest file
    #[error("I/O error: {0}")]
    Io(String),

    /// TOML parsing error
    #[error("Failed to parse manifest: {0}")]
    Parse(String),

    /// Invalid package name
    #[error("Invalid package name: {0}")]
    InvalidPackageName(String),

    /// Invalid version
    #[error("Invalid version: {0}")]
    InvalidVersion(String),

    /// Missing required field
    #[error("Missing required field: {0}")]
    MissingField(String),

    /// Invalid dependency specification
    #[error("Invalid dependency specification for '{0}': {1}")]
    InvalidDependency(String, String),

    /// Circular dependency detected
    #[error("Circular dependency detected: {0}")]
    CircularDependency(String),

    /// Feature not found
    #[error("Feature '{0}' not found in package '{1}'")]
    FeatureNotFound(String, String),

    /// Invalid feature specification
    #[error("Invalid feature specification: {0}")]
    InvalidFeature(String),

    /// Validation error
    #[error("Validation error: {0}")]
    Validation(String),
}
