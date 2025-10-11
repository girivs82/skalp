//! Package caching

use crate::error::{PackageError, Result};
use crate::source::PackageSource;
use sha2::{Digest, Sha256};
use std::fs;
use std::path::{Path, PathBuf};

/// Information about a cached package
#[derive(Debug, Clone)]
pub struct CachedPackageInfo {
    /// Package name
    pub name: String,
    /// Package version
    pub version: String,
    /// Path to cached package
    pub path: PathBuf,
    /// Size in bytes
    pub size: u64,
}

/// Package cache manager
pub struct Cache {
    /// Root cache directory
    root: PathBuf,
}

impl Cache {
    /// Create a new cache with the given root directory
    pub fn new(root: PathBuf) -> Self {
        Self { root }
    }

    /// Get the cache directory for a package source
    pub fn package_dir(&self, source: &PackageSource) -> PathBuf {
        let key = source.cache_key();
        let hash = self.hash_key(&key);
        self.root.join(&hash[..2]).join(&hash[2..])
    }

    /// Check if a package is cached
    pub fn is_cached(&self, source: &PackageSource) -> bool {
        let dir = self.package_dir(source);
        dir.exists() && dir.join("skalp.toml").exists()
    }

    /// Get the path to a cached package's manifest
    pub fn manifest_path(&self, source: &PackageSource) -> PathBuf {
        self.package_dir(source).join("skalp.toml")
    }

    /// Store a package in the cache
    pub fn store(&self, source: &PackageSource, content_dir: &Path) -> Result<()> {
        let dest = self.package_dir(source);

        // Create parent directories
        if let Some(parent) = dest.parent() {
            fs::create_dir_all(parent).map_err(|e| {
                PackageError::Cache(format!("Failed to create cache directory: {}", e))
            })?;
        }

        // Copy directory contents
        Self::copy_dir_all(content_dir, &dest)?;

        Ok(())
    }

    /// Remove a package from the cache
    pub fn remove(&self, source: &PackageSource) -> Result<()> {
        let dir = self.package_dir(source);
        if dir.exists() {
            fs::remove_dir_all(&dir).map_err(|e| {
                PackageError::Cache(format!("Failed to remove cached package: {}", e))
            })?;
        }
        Ok(())
    }

    /// Clear the entire cache
    pub fn clear(&self) -> Result<()> {
        if self.root.exists() {
            fs::remove_dir_all(&self.root)
                .map_err(|e| PackageError::Cache(format!("Failed to clear cache: {}", e)))?;
        }
        Ok(())
    }

    /// Get the size of the cache in bytes
    pub fn size(&self) -> Result<u64> {
        if !self.root.exists() {
            return Ok(0);
        }

        let mut total = 0u64;
        for entry in walkdir::WalkDir::new(&self.root) {
            let entry = entry.map_err(|e| {
                PackageError::Cache(format!("Failed to walk cache directory: {}", e))
            })?;

            if entry.file_type().is_file() {
                total += entry
                    .metadata()
                    .map_err(|e| {
                        PackageError::Cache(format!("Failed to get file metadata: {}", e))
                    })?
                    .len();
            }
        }

        Ok(total)
    }

    /// List all cached packages
    pub fn list(&self) -> Result<Vec<CachedPackageInfo>> {
        if !self.root.exists() {
            return Ok(Vec::new());
        }

        let mut packages = Vec::new();

        // Walk the cache directory looking for skalp.toml files
        for entry in walkdir::WalkDir::new(&self.root)
            .max_depth(3) // Hash is 2 levels deep, then package dir
            .into_iter()
            .filter_map(|e| e.ok())
        {
            let path = entry.path();
            if path.file_name() == Some(std::ffi::OsStr::new("skalp.toml")) {
                // Found a manifest, extract package info
                if let Ok(content) = fs::read_to_string(path) {
                    if let Ok(manifest) = toml::from_str::<serde_json::Value>(&content) {
                        if let Some(package) = manifest.get("package") {
                            let name = package
                                .get("name")
                                .and_then(|v| v.as_str())
                                .unwrap_or("unknown")
                                .to_string();

                            let version = package
                                .get("version")
                                .and_then(|v| v.as_str())
                                .unwrap_or("unknown")
                                .to_string();

                            if let Some(pkg_dir) = path.parent() {
                                let size = Self::dir_size(pkg_dir)?;
                                packages.push(CachedPackageInfo {
                                    name,
                                    version,
                                    path: pkg_dir.to_path_buf(),
                                    size,
                                });
                            }
                        }
                    }
                }
            }
        }

        // Sort by name, then version
        packages.sort_by(|a, b| a.name.cmp(&b.name).then(a.version.cmp(&b.version)));

        Ok(packages)
    }

    /// Get size of a directory
    fn dir_size(dir: &Path) -> Result<u64> {
        let mut total = 0u64;
        for entry in walkdir::WalkDir::new(dir) {
            let entry = entry
                .map_err(|e| PackageError::Cache(format!("Failed to walk directory: {}", e)))?;

            if entry.file_type().is_file() {
                total += entry
                    .metadata()
                    .map_err(|e| PackageError::Cache(format!("Failed to get metadata: {}", e)))?
                    .len();
            }
        }
        Ok(total)
    }

    /// Compute checksum of a directory
    pub fn compute_checksum(&self, dir: &Path) -> Result<String> {
        let mut hasher = Sha256::new();

        // Walk directory and hash all files in sorted order
        let mut entries: Vec<_> = walkdir::WalkDir::new(dir)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_type().is_file())
            .collect();

        entries.sort_by_key(|e| e.path().to_path_buf());

        for entry in entries {
            let contents = fs::read(entry.path()).map_err(|e| {
                PackageError::Cache(format!("Failed to read file for checksum: {}", e))
            })?;
            hasher.update(&contents);
        }

        Ok(format!("{:x}", hasher.finalize()))
    }

    /// Hash a cache key
    fn hash_key(&self, key: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(key.as_bytes());
        format!("{:x}", hasher.finalize())
    }

    /// Copy directory recursively
    fn copy_dir_all(src: &Path, dst: &Path) -> Result<()> {
        fs::create_dir_all(dst)
            .map_err(|e| PackageError::Cache(format!("Failed to create directory: {}", e)))?;

        for entry in fs::read_dir(src)
            .map_err(|e| PackageError::Cache(format!("Failed to read directory: {}", e)))?
        {
            let entry = entry.map_err(|e| {
                PackageError::Cache(format!("Failed to read directory entry: {}", e))
            })?;

            let ty = entry
                .file_type()
                .map_err(|e| PackageError::Cache(format!("Failed to get file type: {}", e)))?;

            let dst_path = dst.join(entry.file_name());

            if ty.is_dir() {
                Self::copy_dir_all(&entry.path(), &dst_path)?;
            } else {
                fs::copy(entry.path(), &dst_path)
                    .map_err(|e| PackageError::Cache(format!("Failed to copy file: {}", e)))?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_cache_creation() {
        let temp = TempDir::new().unwrap();
        let cache = Cache::new(temp.path().to_path_buf());

        let source = PackageSource::registry("test-lib", "1.0");
        let dir = cache.package_dir(&source);

        assert!(dir.starts_with(temp.path()));
    }

    #[test]
    fn test_is_cached() {
        let temp = TempDir::new().unwrap();
        let cache = Cache::new(temp.path().to_path_buf());

        let source = PackageSource::registry("test-lib", "1.0");
        assert!(!cache.is_cached(&source));
    }

    #[test]
    fn test_hash_key() {
        let temp = TempDir::new().unwrap();
        let cache = Cache::new(temp.path().to_path_buf());

        let hash1 = cache.hash_key("test");
        let hash2 = cache.hash_key("test");
        let hash3 = cache.hash_key("different");

        assert_eq!(hash1, hash2);
        assert_ne!(hash1, hash3);
        assert_eq!(hash1.len(), 64); // SHA256 hex string
    }

    #[test]
    fn test_cache_size_empty() {
        let temp = TempDir::new().unwrap();
        let cache = Cache::new(temp.path().join("nonexistent"));

        assert_eq!(cache.size().unwrap(), 0);
    }
}
