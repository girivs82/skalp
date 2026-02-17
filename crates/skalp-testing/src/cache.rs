//! Compilation cache for SKALP testbench
//!
//! This module provides content-based caching for the SIR compilation pipeline.
//! It hashes source files (main + dependencies) to create a cache key and stores
//! the compiled SIR, avoiding expensive recompilation when only testbench code changes.
//!
//! # Cache Strategy
//! - Cache key: Blake3 hash of (main source + sorted dependencies content)
//! - Cache location: {source_dir}/build/.skalp-cache/{hash}/sir.bin
//! - On cache hit: Skip HIR→MIR→SIR, load SIR directly
//! - Invalidation: Automatic via content hashing

use anyhow::{Context, Result};
use skalp_sir::SirModule;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

/// Cache subdirectory name (placed in build/ next to source)
const CACHE_SUBDIR: &str = ".skalp-cache";

/// Compilation cache for SIR modules
pub struct CompilationCache {
    cache_dir: PathBuf,
    enabled: bool,
}

impl Default for CompilationCache {
    fn default() -> Self {
        Self::new()
    }
}

impl CompilationCache {
    /// Create a new compilation cache with default settings (uses cwd/target/skalp-cache)
    pub fn new() -> Self {
        // Check SKALP_CACHE environment variable
        let enabled = std::env::var("SKALP_CACHE")
            .map(|v| v != "0" && v.to_lowercase() != "false")
            .unwrap_or(true); // Enabled by default

        // Use SKALP_CACHE_DIR if set, otherwise fallback to target/skalp-cache
        let cache_dir = std::env::var("SKALP_CACHE_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from("target/skalp-cache"));

        Self { cache_dir, enabled }
    }

    /// Create a cache relative to the source file's directory
    /// Cache will be stored in: {source_dir}/build/.skalp-cache/
    pub fn for_source(source_path: &Path) -> Self {
        let enabled = std::env::var("SKALP_CACHE")
            .map(|v| v != "0" && v.to_lowercase() != "false")
            .unwrap_or(true);

        // If SKALP_CACHE_DIR is set, use it; otherwise use source-relative path
        let cache_dir = if let Ok(dir) = std::env::var("SKALP_CACHE_DIR") {
            PathBuf::from(dir)
        } else {
            // Get the source file's parent directory
            let source_dir = source_path.parent().unwrap_or(Path::new("."));
            // Cache goes in: source_dir/build/.skalp-cache/
            source_dir.join("build").join(CACHE_SUBDIR)
        };

        Self { cache_dir, enabled }
    }

    /// Create a cache with a custom directory
    pub fn with_dir(cache_dir: PathBuf) -> Self {
        Self {
            cache_dir,
            enabled: true,
        }
    }

    /// Disable the cache
    pub fn disabled() -> Self {
        Self {
            cache_dir: PathBuf::from("target/skalp-cache"),
            enabled: false,
        }
    }

    /// Check if caching is enabled
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    /// Get the cache directory path
    pub fn cache_dir(&self) -> &Path {
        &self.cache_dir
    }

    /// Compute a cache key for a source file and its dependencies
    pub fn compute_cache_key(
        &self,
        source_path: &Path,
        dependencies: &[PathBuf],
    ) -> Result<String> {
        let mut hasher = blake3::Hasher::new();

        // Hash the main source file
        let main_content = fs::read(source_path)
            .with_context(|| format!("Failed to read source file: {}", source_path.display()))?;
        hasher.update(&main_content);

        // Hash dependencies in sorted order for determinism
        let mut dep_contents: BTreeMap<String, Vec<u8>> = BTreeMap::new();
        for dep_path in dependencies {
            if dep_path.exists() {
                let content = fs::read(dep_path).with_context(|| {
                    format!("Failed to read dependency: {}", dep_path.display())
                })?;
                let key = dep_path.to_string_lossy().to_string();
                dep_contents.insert(key, content);
            }
        }

        for (path, content) in &dep_contents {
            hasher.update(path.as_bytes());
            hasher.update(content);
        }

        let hash = hasher.finalize();
        Ok(hash.to_hex().to_string())
    }

    /// Try to load a cached SIR module
    pub fn load(&self, cache_key: &str) -> Result<Option<SirModule>> {
        if !self.enabled {
            return Ok(None);
        }

        let cache_path = self.cache_dir.join(cache_key).join("sir.bin");
        if !cache_path.exists() {
            return Ok(None);
        }

        let data = fs::read(&cache_path)
            .with_context(|| format!("Failed to read cache file: {}", cache_path.display()))?;

        let sir: SirModule = bincode::deserialize(&data).with_context(|| {
            format!("Failed to deserialize cached SIR: {}", cache_path.display())
        })?;

        Ok(Some(sir))
    }

    /// Store a SIR module in the cache
    pub fn store(&self, cache_key: &str, sir: &SirModule) -> Result<()> {
        if !self.enabled {
            return Ok(());
        }

        let cache_entry_dir = self.cache_dir.join(cache_key);
        fs::create_dir_all(&cache_entry_dir).with_context(|| {
            format!(
                "Failed to create cache directory: {}",
                cache_entry_dir.display()
            )
        })?;

        let cache_path = cache_entry_dir.join("sir.bin");

        let data = bincode::serialize(sir).context("Failed to serialize SIR for caching")?;

        fs::write(&cache_path, &data)
            .with_context(|| format!("Failed to write cache file: {}", cache_path.display()))?;

        Ok(())
    }

    /// Clear the entire cache
    pub fn clear(&self) -> Result<()> {
        if self.cache_dir.exists() {
            fs::remove_dir_all(&self.cache_dir).with_context(|| {
                format!(
                    "Failed to clear cache directory: {}",
                    self.cache_dir.display()
                )
            })?;
        }
        Ok(())
    }

    /// Get cache statistics
    pub fn stats(&self) -> CacheStats {
        let mut stats = CacheStats::default();

        if !self.cache_dir.exists() {
            return stats;
        }

        if let Ok(entries) = fs::read_dir(&self.cache_dir) {
            for entry in entries.flatten() {
                if entry.path().is_dir() {
                    stats.entries += 1;
                    if let Ok(sir_path) = entry.path().join("sir.bin").metadata() {
                        stats.total_size += sir_path.len();
                    }
                }
            }
        }

        stats
    }
}

/// Cache statistics
#[derive(Debug, Default)]
pub struct CacheStats {
    pub entries: usize,
    pub total_size: u64,
}

impl CacheStats {
    pub fn human_readable_size(&self) -> String {
        if self.total_size < 1024 {
            format!("{} B", self.total_size)
        } else if self.total_size < 1024 * 1024 {
            format!("{:.1} KB", self.total_size as f64 / 1024.0)
        } else {
            format!("{:.1} MB", self.total_size as f64 / (1024.0 * 1024.0))
        }
    }
}

/// Collect all dependency paths for a source file
///
/// This walks the module resolution to find all transitive dependencies.
pub fn collect_dependencies(source_path: &Path) -> Result<Vec<PathBuf>> {
    let mut dependencies = Vec::new();
    let parent_dir = source_path.parent().unwrap_or(Path::new("."));

    // Read the source file and look for imports
    let content = fs::read_to_string(source_path)
        .with_context(|| format!("Failed to read source: {}", source_path.display()))?;

    // Parse simple "use module::*" or "mod module" patterns
    for line in content.lines() {
        let line = line.trim();

        // Handle "use module::*" style
        if line.starts_with("use ") && line.contains("::") {
            if let Some(module_name) = line.strip_prefix("use ").and_then(|s| s.split("::").next())
            {
                let module_name = module_name.trim();
                // Try to find the module file
                for ext in &["sk", "skalp"] {
                    let module_path = parent_dir.join(format!("{}.{}", module_name, ext));
                    if module_path.exists() && !dependencies.contains(&module_path) {
                        dependencies.push(module_path.clone());
                        // Recursively collect dependencies
                        if let Ok(sub_deps) = collect_dependencies(&module_path) {
                            for dep in sub_deps {
                                if !dependencies.contains(&dep) {
                                    dependencies.push(dep);
                                }
                            }
                        }
                    }
                }
            }
        }

        // Handle "mod module" style
        if line.starts_with("mod ") && line.ends_with(';') {
            if let Some(module_name) = line.strip_prefix("mod ").and_then(|s| s.strip_suffix(';')) {
                let module_name = module_name.trim();
                for ext in &["sk", "skalp"] {
                    let module_path = parent_dir.join(format!("{}.{}", module_name, ext));
                    if module_path.exists() && !dependencies.contains(&module_path) {
                        dependencies.push(module_path.clone());
                        if let Ok(sub_deps) = collect_dependencies(&module_path) {
                            for dep in sub_deps {
                                if !dependencies.contains(&dep) {
                                    dependencies.push(dep);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(dependencies)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cache_key_determinism() {
        let cache = CompilationCache::new();

        // Create a temp file
        let temp_dir = std::env::temp_dir().join("skalp_cache_test");
        fs::create_dir_all(&temp_dir).unwrap();
        let test_file = temp_dir.join("test.sk");
        fs::write(&test_file, "entity Test { in a: bit[8] out b: bit[8] }").unwrap();

        // Compute key twice
        let key1 = cache.compute_cache_key(&test_file, &[]).unwrap();
        let key2 = cache.compute_cache_key(&test_file, &[]).unwrap();

        assert_eq!(key1, key2, "Cache keys should be deterministic");

        // Clean up
        fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn test_cache_key_changes_with_content() {
        let cache = CompilationCache::new();

        let temp_dir = std::env::temp_dir().join("skalp_cache_test2");
        fs::create_dir_all(&temp_dir).unwrap();
        let test_file = temp_dir.join("test.sk");

        fs::write(&test_file, "entity A {}").unwrap();
        let key1 = cache.compute_cache_key(&test_file, &[]).unwrap();

        fs::write(&test_file, "entity B {}").unwrap();
        let key2 = cache.compute_cache_key(&test_file, &[]).unwrap();

        assert_ne!(key1, key2, "Cache keys should change when content changes");

        fs::remove_dir_all(&temp_dir).ok();
    }

    #[test]
    fn test_cache_stats() {
        let cache = CompilationCache::with_dir(PathBuf::from("/nonexistent"));
        let stats = cache.stats();
        assert_eq!(stats.entries, 0);
        assert_eq!(stats.total_size, 0);
    }
}
