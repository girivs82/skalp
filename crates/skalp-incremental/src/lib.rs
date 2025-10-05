#![allow(dead_code, unused_variables, unused_imports)]
//! Incremental build system for SKALP
//!
//! This crate provides:
//! - Dependency tracking and change detection
//! - Selective recompilation
//! - Build cache management
//! - Artifact versioning

pub mod builder;
pub mod cache;
pub mod dependencies;
pub mod fingerprint;

use chrono::{DateTime, Utc};
use std::collections::HashMap;
use std::path::PathBuf;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum IncrementalError {
    #[error("Build cache corrupted: {0}")]
    CacheCorrupted(String),
    #[error("Dependency cycle detected: {0}")]
    DependencyCycle(String),
    #[error("File not found: {0}")]
    FileNotFound(String),
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Serialization error: {0}")]
    Serialization(#[from] serde_json::Error),
}

pub type IncrementalResult<T> = Result<T, IncrementalError>;

/// Incremental build engine
pub struct IncrementalBuilder {
    /// Build cache
    cache: cache::BuildCache,
    /// Dependency tracker
    dependencies: dependencies::DependencyTracker,
    /// Build configuration
    config: BuildConfig,
}

#[derive(Debug, Clone)]
pub struct BuildConfig {
    /// Project root directory
    pub project_root: PathBuf,
    /// Cache directory
    pub cache_dir: PathBuf,
    /// Maximum cache size (MB)
    pub max_cache_size_mb: usize,
    /// Cache retention time (days)
    pub cache_retention_days: u32,
    /// Enable parallel builds
    pub parallel_builds: bool,
    /// Force full rebuild
    pub force_rebuild: bool,
}

impl Default for BuildConfig {
    fn default() -> Self {
        Self {
            project_root: PathBuf::from("."),
            cache_dir: PathBuf::from(".skalp/cache"),
            max_cache_size_mb: 1024,
            cache_retention_days: 30,
            parallel_builds: true,
            force_rebuild: false,
        }
    }
}

impl IncrementalBuilder {
    pub fn new(config: BuildConfig) -> IncrementalResult<Self> {
        let cache = cache::BuildCache::new(&config.cache_dir, config.max_cache_size_mb)?;
        let dependencies = dependencies::DependencyTracker::new();

        Ok(Self {
            cache,
            dependencies,
            config,
        })
    }

    /// Analyze what needs to be rebuilt
    pub fn analyze_changes(&mut self, target: &str) -> IncrementalResult<BuildPlan> {
        log::info!("Analyzing changes for target: {}", target);

        let mut plan = BuildPlan::new(target.to_string());

        if self.config.force_rebuild {
            log::info!("Force rebuild requested - rebuilding all");
            plan.mark_full_rebuild();
            return Ok(plan);
        }

        // Get dependency graph for target
        let dep_graph = self.dependencies.get_dependency_graph(target)?;

        // Check each dependency for changes
        for dep in dep_graph.iter() {
            let needs_rebuild = self.check_if_needs_rebuild(dep)?;
            if needs_rebuild {
                plan.add_target_to_rebuild(dep.clone());

                // Mark all dependents for rebuild too
                let dependents = self.dependencies.get_dependents(dep)?;
                for dependent in dependents {
                    plan.add_target_to_rebuild(dependent);
                }
            }
        }

        log::info!(
            "Build plan: {} targets to rebuild",
            plan.targets_to_rebuild.len()
        );
        Ok(plan)
    }

    /// Execute incremental build
    pub async fn build(&mut self, target: &str) -> IncrementalResult<BuildResults> {
        let plan = self.analyze_changes(target)?;

        let start_time = Utc::now();

        let results = if plan.is_full_rebuild {
            log::info!("Performing full rebuild");
            self.full_rebuild(target).await?
        } else if plan.targets_to_rebuild.is_empty() {
            log::info!("No changes detected - using cached results");
            self.load_cached_results(target)?
        } else {
            log::info!(
                "Performing incremental rebuild of {} targets",
                plan.targets_to_rebuild.len()
            );
            self.incremental_rebuild(&plan).await?
        };

        let mut results = results;
        results.build_time = Utc::now() - start_time;

        // Update cache with new results
        self.cache.store_build_results(target, &results)?;

        Ok(results)
    }

    fn check_if_needs_rebuild(&self, target: &str) -> IncrementalResult<bool> {
        // Check if target exists in cache
        if !self.cache.has_target(target) {
            log::debug!("Target {} not in cache - needs rebuild", target);
            return Ok(true);
        }

        // Get cached fingerprint
        let cached_fingerprint = self.cache.get_fingerprint(target)?;

        // Calculate current fingerprint
        let current_fingerprint = fingerprint::calculate_target_fingerprint(target)?;

        // Compare fingerprints
        let needs_rebuild = cached_fingerprint != current_fingerprint;

        if needs_rebuild {
            log::debug!("Target {} fingerprint changed - needs rebuild", target);
        } else {
            log::debug!("Target {} unchanged - using cache", target);
        }

        Ok(needs_rebuild)
    }

    async fn full_rebuild(&mut self, target: &str) -> IncrementalResult<BuildResults> {
        // Clear cache for target
        self.cache.invalidate_target(target)?;

        // Rebuild everything
        self.build_target_from_scratch(target).await
    }

    async fn incremental_rebuild(&mut self, plan: &BuildPlan) -> IncrementalResult<BuildResults> {
        let mut results = BuildResults::new(plan.main_target.clone());

        // Sort targets by dependency order
        let build_order = self
            .dependencies
            .topological_sort(&plan.targets_to_rebuild)?;

        // Build each target
        for target in build_order {
            log::info!("Rebuilding target: {}", target);

            let target_results = self.build_target_from_scratch(&target).await?;

            // Update cache before merging (which moves the value)
            self.cache.store_build_results(&target, &target_results)?;
            results.merge(target_results);
        }

        Ok(results)
    }

    async fn build_target_from_scratch(&self, target: &str) -> IncrementalResult<BuildResults> {
        // This would integrate with the main SKALP compiler
        let mut results = BuildResults::new(target.to_string());

        // Simulate compilation
        log::debug!("Compiling target: {}", target);

        // In real implementation, this would:
        // 1. Parse source files
        // 2. Run type checking
        // 3. Generate IR
        // 4. Optimize
        // 5. Generate output

        results.success = true;
        results.artifacts.insert(
            format!("{}.bit", target),
            Artifact {
                path: PathBuf::from(format!("{}.bit", target)),
                size_bytes: 1024 * 1024, // 1MB
                checksum: "abc123".to_string(),
                created_at: Utc::now(),
            },
        );

        Ok(results)
    }

    fn load_cached_results(&self, target: &str) -> IncrementalResult<BuildResults> {
        self.cache.load_build_results(target)
    }

    /// Clean build cache
    pub fn clean_cache(&mut self) -> IncrementalResult<()> {
        log::info!("Cleaning build cache");
        self.cache.clear()?;
        Ok(())
    }

    /// Get cache statistics
    pub fn cache_stats(&self) -> CacheStats {
        self.cache.get_stats()
    }
}

/// Build plan describing what needs to be rebuilt
#[derive(Debug)]
pub struct BuildPlan {
    /// Main target being built
    pub main_target: String,
    /// Whether this is a full rebuild
    pub is_full_rebuild: bool,
    /// Specific targets that need rebuilding
    pub targets_to_rebuild: Vec<String>,
}

impl BuildPlan {
    pub fn new(main_target: String) -> Self {
        Self {
            main_target,
            is_full_rebuild: false,
            targets_to_rebuild: Vec::new(),
        }
    }

    pub fn mark_full_rebuild(&mut self) {
        self.is_full_rebuild = true;
        self.targets_to_rebuild.clear();
    }

    pub fn add_target_to_rebuild(&mut self, target: String) {
        if !self.targets_to_rebuild.contains(&target) {
            self.targets_to_rebuild.push(target);
        }
    }
}

/// Build results
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct BuildResults {
    /// Target name
    pub target: String,
    /// Whether build succeeded
    pub success: bool,
    /// Build duration
    pub build_time: chrono::Duration,
    /// Generated artifacts
    pub artifacts: HashMap<String, Artifact>,
    /// Error messages
    pub errors: Vec<String>,
    /// Warnings
    pub warnings: Vec<String>,
}

impl BuildResults {
    pub fn new(target: String) -> Self {
        Self {
            target,
            success: false,
            build_time: chrono::Duration::zero(),
            artifacts: HashMap::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn merge(&mut self, other: BuildResults) {
        self.success &= other.success;
        self.build_time += other.build_time;
        self.artifacts.extend(other.artifacts);
        self.errors.extend(other.errors);
        self.warnings.extend(other.warnings);
    }
}

/// Build artifact
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Artifact {
    /// Artifact file path
    pub path: PathBuf,
    /// File size in bytes
    pub size_bytes: u64,
    /// File checksum
    pub checksum: String,
    /// Creation timestamp
    pub created_at: DateTime<Utc>,
}

/// Cache statistics
#[derive(Debug, Clone)]
pub struct CacheStats {
    /// Number of cached targets
    pub cached_targets: usize,
    /// Total cache size (bytes)
    pub total_size_bytes: u64,
    /// Cache hit rate
    pub hit_rate: f64,
    /// Number of cache hits
    pub hits: u64,
    /// Number of cache misses
    pub misses: u64,
}

impl CacheStats {
    pub fn efficiency(&self) -> f64 {
        self.hit_rate * 100.0
    }

    pub fn size_mb(&self) -> f64 {
        self.total_size_bytes as f64 / (1024.0 * 1024.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_build_config() {
        let config = BuildConfig::default();
        assert_eq!(config.project_root, PathBuf::from("."));
        assert_eq!(config.max_cache_size_mb, 1024);
    }

    #[test]
    fn test_build_plan() {
        let mut plan = BuildPlan::new("main".to_string());
        plan.add_target_to_rebuild("module1".to_string());
        plan.add_target_to_rebuild("module2".to_string());

        assert_eq!(plan.targets_to_rebuild.len(), 2);
        assert!(!plan.is_full_rebuild);

        plan.mark_full_rebuild();
        assert!(plan.is_full_rebuild);
        assert!(plan.targets_to_rebuild.is_empty());
    }

    #[tokio::test]
    async fn test_incremental_builder() {
        let temp_dir = TempDir::new().unwrap();
        let config = BuildConfig {
            cache_dir: temp_dir.path().to_path_buf(),
            ..Default::default()
        };

        let builder = IncrementalBuilder::new(config);
        assert!(builder.is_ok());
    }

    #[test]
    fn test_build_results_merge() {
        let mut results1 = BuildResults::new("target1".to_string());
        results1.success = true;
        results1.warnings.push("Warning 1".to_string());

        let mut results2 = BuildResults::new("target2".to_string());
        results2.success = false;
        results2.errors.push("Error 1".to_string());

        results1.merge(results2);

        assert!(!results1.success); // Should be false due to merge
        assert_eq!(results1.warnings.len(), 1);
        assert_eq!(results1.errors.len(), 1);
    }

    #[test]
    fn test_cache_stats() {
        let stats = CacheStats {
            cached_targets: 10,
            total_size_bytes: 1024 * 1024 * 500, // 500MB
            hit_rate: 0.85,
            hits: 85,
            misses: 15,
        };

        assert_eq!(stats.efficiency(), 85.0);
        assert_eq!(stats.size_mb(), 500.0);
    }
}
