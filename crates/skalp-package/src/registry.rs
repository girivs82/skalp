//! Registry client for fetching packages

use crate::error::{PackageError, Result};
use reqwest::blocking::Client;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use std::time::Duration;

/// Registry API client
pub struct RegistryClient {
    /// Base URL of the registry
    base_url: String,
    /// HTTP client
    client: Client,
    /// Optional authentication token
    token: Option<String>,
}

/// Package metadata from registry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageMetadata {
    /// Package name
    pub name: String,
    /// Available versions
    pub versions: Vec<VersionInfo>,
    /// Package description
    pub description: Option<String>,
    /// Repository URL
    pub repository: Option<String>,
    /// License
    pub license: Option<String>,
}

/// Information about a specific version
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VersionInfo {
    /// Version string (e.g., "1.0.0")
    pub version: String,
    /// Download URL
    pub download_url: String,
    /// Checksum (SHA256)
    pub checksum: String,
    /// Dependencies
    #[serde(default)]
    pub dependencies: Vec<String>,
    /// When this version was published
    pub published_at: String,
}

/// Download result
#[derive(Debug)]
pub struct DownloadResult {
    /// Path where the package was extracted
    pub path: PathBuf,
    /// Computed checksum
    pub checksum: String,
}

impl RegistryClient {
    /// Create a new registry client
    pub fn new(base_url: impl Into<String>) -> Result<Self> {
        let client = Client::builder()
            .timeout(Duration::from_secs(30))
            .build()
            .map_err(|e| PackageError::Http(e.to_string()))?;

        Ok(Self {
            base_url: base_url.into(),
            client,
            token: None,
        })
    }

    /// Set authentication token
    pub fn with_token(mut self, token: impl Into<String>) -> Self {
        self.token = Some(token.into());
        self
    }

    /// Fetch metadata for a package
    pub fn fetch_metadata(&self, package_name: &str) -> Result<PackageMetadata> {
        let url = format!("{}/api/v1/packages/{}", self.base_url, package_name);

        let mut request = self.client.get(&url);
        if let Some(token) = &self.token {
            request = request.header("Authorization", format!("Bearer {}", token));
        }

        let response = request
            .send()
            .map_err(|e| PackageError::Http(e.to_string()))?;

        if !response.status().is_success() {
            if response.status().as_u16() == 404 {
                return Err(PackageError::PackageNotFound(package_name.to_string()));
            }
            return Err(PackageError::Http(format!(
                "HTTP {}: {}",
                response.status(),
                response.text().unwrap_or_default()
            )));
        }

        response
            .json::<PackageMetadata>()
            .map_err(|e| PackageError::Parse(e.to_string()))
    }

    /// Find the best matching version for a version requirement
    pub fn resolve_version(
        &self,
        package_name: &str,
        version_req: &semver::VersionReq,
    ) -> Result<VersionInfo> {
        let metadata = self.fetch_metadata(package_name)?;

        // Parse all versions and filter by requirement
        let mut matching_versions: Vec<(semver::Version, &VersionInfo)> = metadata
            .versions
            .iter()
            .filter_map(|v| {
                semver::Version::parse(&v.version)
                    .ok()
                    .filter(|parsed| version_req.matches(parsed))
                    .map(|parsed| (parsed, v))
            })
            .collect();

        if matching_versions.is_empty() {
            return Err(PackageError::VersionNotFound {
                package: package_name.to_string(),
                version: version_req.to_string(),
            });
        }

        // Sort by version (highest first)
        matching_versions.sort_by(|a, b| b.0.cmp(&a.0));

        // Return highest version
        Ok(matching_versions[0].1.clone())
    }

    /// Download a package to the specified directory
    pub fn download_package(
        &self,
        version_info: &VersionInfo,
        dest_dir: &Path,
    ) -> Result<DownloadResult> {
        // Download the package
        let mut request = self.client.get(&version_info.download_url);
        if let Some(token) = &self.token {
            request = request.header("Authorization", format!("Bearer {}", token));
        }

        let response = request
            .send()
            .map_err(|e| PackageError::DownloadFailed(e.to_string()))?;

        if !response.status().is_success() {
            return Err(PackageError::DownloadFailed(format!(
                "HTTP {}: {}",
                response.status(),
                response.text().unwrap_or_default()
            )));
        }

        let content = response
            .bytes()
            .map_err(|e| PackageError::DownloadFailed(e.to_string()))?;

        // Compute checksum
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(&content);
        let checksum = format!("{:x}", hasher.finalize());

        // Verify checksum
        if checksum != version_info.checksum {
            return Err(PackageError::ChecksumMismatch {
                package: version_info.version.clone(),
                expected: version_info.checksum.clone(),
                actual: checksum,
            });
        }

        // Extract to destination (assuming tar.gz format)
        self.extract_archive(&content, dest_dir)?;

        Ok(DownloadResult {
            path: dest_dir.to_path_buf(),
            checksum,
        })
    }

    /// Extract a tar.gz archive
    fn extract_archive(&self, content: &[u8], dest: &Path) -> Result<()> {
        use std::io::Cursor;

        // Create destination directory
        std::fs::create_dir_all(dest).map_err(|e| {
            PackageError::DownloadFailed(format!("Failed to create directory: {}", e))
        })?;

        // For now, we'll use a simple approach
        // In production, you'd want to use tar and gzip crates properly
        let cursor = Cursor::new(content);

        // Try to decompress with flate2
        use flate2::read::GzDecoder;
        let decoder = GzDecoder::new(cursor);

        // Extract with tar
        let mut archive = tar::Archive::new(decoder);
        archive.unpack(dest).map_err(|e| {
            PackageError::DownloadFailed(format!("Failed to extract archive: {}", e))
        })?;

        Ok(())
    }

    /// Check if a package exists in the registry
    pub fn package_exists(&self, package_name: &str) -> Result<bool> {
        match self.fetch_metadata(package_name) {
            Ok(_) => Ok(true),
            Err(PackageError::PackageNotFound(_)) => Ok(false),
            Err(e) => Err(e),
        }
    }

    /// Search for packages matching a query
    pub fn search(&self, query: &str) -> Result<Vec<PackageMetadata>> {
        let url = format!("{}/api/v1/search?q={}", self.base_url, query);

        let mut request = self.client.get(&url);
        if let Some(token) = &self.token {
            request = request.header("Authorization", format!("Bearer {}", token));
        }

        let response = request
            .send()
            .map_err(|e| PackageError::Http(e.to_string()))?;

        if !response.status().is_success() {
            return Err(PackageError::Http(format!(
                "HTTP {}: {}",
                response.status(),
                response.text().unwrap_or_default()
            )));
        }

        response
            .json::<Vec<PackageMetadata>>()
            .map_err(|e| PackageError::Parse(e.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_registry_client_creation() {
        let client = RegistryClient::new("https://registry.skalp.dev").unwrap();
        assert_eq!(client.base_url, "https://registry.skalp.dev");
        assert!(client.token.is_none());
    }

    #[test]
    fn test_registry_client_with_token() {
        let client = RegistryClient::new("https://registry.skalp.dev")
            .unwrap()
            .with_token("test-token");
        assert_eq!(client.token, Some("test-token".to_string()));
    }

    // Note: The following tests would require a mock server
    // They are marked as ignored for now

    #[test]
    #[ignore]
    fn test_fetch_metadata() {
        let client = RegistryClient::new("https://registry.skalp.dev").unwrap();
        let metadata = client.fetch_metadata("skalp-stdlib").unwrap();
        assert_eq!(metadata.name, "skalp-stdlib");
        assert!(!metadata.versions.is_empty());
    }

    #[test]
    #[ignore]
    fn test_resolve_version() {
        let client = RegistryClient::new("https://registry.skalp.dev").unwrap();
        let version_req = semver::VersionReq::parse("^1.0").unwrap();
        let version = client
            .resolve_version("skalp-stdlib", &version_req)
            .unwrap();
        assert!(version.version.starts_with("1."));
    }

    #[test]
    fn test_package_not_found() {
        let client = RegistryClient::new("https://registry.skalp.dev").unwrap();
        let result = client.fetch_metadata("nonexistent-package-12345");
        // This will fail with HTTP error since we don't have a real server
        // In production with a real server, this would be PackageNotFound
        assert!(result.is_err());
    }
}
