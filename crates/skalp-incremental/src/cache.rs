//! Build cache implementation

use crate::{BuildResults, CacheStats, IncrementalResult};
use std::path::Path;

pub struct BuildCache {
    cache_dir: std::path::PathBuf,
    max_size_mb: usize,
}

impl BuildCache {
    pub fn new(cache_dir: &Path, max_size_mb: usize) -> IncrementalResult<Self> {
        Ok(Self {
            cache_dir: cache_dir.to_path_buf(),
            max_size_mb,
        })
    }

    pub fn has_target(&self, _target: &str) -> bool {
        false
    }

    pub fn get_fingerprint(&self, _target: &str) -> IncrementalResult<String> {
        Ok("abc123".to_string())
    }

    pub fn store_build_results(
        &self,
        _target: &str,
        _results: &BuildResults,
    ) -> IncrementalResult<()> {
        Ok(())
    }

    pub fn load_build_results(&self, _target: &str) -> IncrementalResult<BuildResults> {
        Ok(BuildResults::new("target".to_string()))
    }

    pub fn invalidate_target(&self, _target: &str) -> IncrementalResult<()> {
        Ok(())
    }

    pub fn clear(&self) -> IncrementalResult<()> {
        Ok(())
    }

    pub fn get_stats(&self) -> CacheStats {
        CacheStats {
            cached_targets: 0,
            total_size_bytes: 0,
            hit_rate: 0.0,
            hits: 0,
            misses: 0,
        }
    }
}
