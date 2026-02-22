//! C++ Compilation Infrastructure
//!
//! This module provides compilation and caching of C++ kernels for CPU simulation.
//! It uses SHA-256 hashing to cache compiled libraries, avoiding recompilation
//! of unchanged designs.
//!
//! # Supported Compilers
//!
//! - macOS: clang++ (from Xcode Command Line Tools)
//! - Linux: g++ or clang++
//! - Windows: cl.exe (MSVC) or clang++
//!
//! # Caching
//!
//! Compiled libraries are cached in `~/.cache/skalp/compiled/` with SHA-256 hash
//! as the filename. This allows instant reuse of previously compiled designs.

use sha2::{Digest, Sha256};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};
use thiserror::Error;

/// Errors that can occur during C++ compilation
#[derive(Error, Debug)]
pub enum CompileError {
    #[error("C++ compiler not found. Please install:\n  macOS:  xcode-select --install\n  Ubuntu: sudo apt install g++\n  Fedora: sudo dnf install gcc-c++")]
    CompilerNotFound,

    #[error("Compilation failed: {0}")]
    CompilationFailed(String),

    #[error("Failed to write source file: {0}")]
    WriteError(#[from] std::io::Error),

    #[error("Failed to create cache directory: {0}")]
    CacheError(String),
}

/// Result type for compilation operations
pub type CompileResult<T> = Result<T, CompileError>;

/// Get the cache directory for compiled libraries
fn cache_dir() -> PathBuf {
    let base = dirs::cache_dir()
        .or_else(dirs::home_dir)
        .unwrap_or_else(|| PathBuf::from("/tmp"));
    base.join("skalp").join("compiled")
}

/// Get the platform-specific dynamic library extension
fn dylib_ext() -> &'static str {
    if cfg!(target_os = "macos") {
        "dylib"
    } else if cfg!(target_os = "windows") {
        "dll"
    } else {
        "so"
    }
}

/// Compute SHA-256 hash of the source code
fn hash_source(source: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(source.as_bytes());
    let result = hasher.finalize();
    hex::encode(result)
}

/// Find a suitable C++ compiler on the system
fn find_cpp_compiler() -> CompileResult<PathBuf> {
    // Try common compiler paths
    let candidates = if cfg!(target_os = "macos") {
        vec![
            "clang++",
            "/usr/bin/clang++",
            "/Library/Developer/CommandLineTools/usr/bin/clang++",
        ]
    } else if cfg!(target_os = "windows") {
        vec!["cl.exe", "clang++.exe", "g++.exe"]
    } else {
        vec!["g++", "clang++", "/usr/bin/g++", "/usr/bin/clang++"]
    };

    for candidate in candidates {
        let path = Path::new(candidate);
        if path.is_absolute() && path.exists() {
            return Ok(path.to_path_buf());
        }

        // Try to find in PATH
        if let Ok(output) = Command::new("which").arg(candidate).output() {
            if output.status.success() {
                let path_str = String::from_utf8_lossy(&output.stdout);
                let path = PathBuf::from(path_str.trim());
                if path.exists() {
                    return Ok(path);
                }
            }
        }
    }

    Err(CompileError::CompilerNotFound)
}

/// Get compiler arguments for the current platform
fn compiler_args(compiler: &Path, output: &Path, source: &Path) -> Vec<String> {
    let compiler_name = compiler.file_name().and_then(|s| s.to_str()).unwrap_or("");

    if cfg!(target_os = "macos") {
        vec![
            "-shared".to_string(),
            "-O2".to_string(),
            "-std=c++14".to_string(),
            "-fPIC".to_string(),
            "-Wno-unused-function".to_string(),
            "-o".to_string(),
            output.to_string_lossy().to_string(),
            source.to_string_lossy().to_string(),
        ]
    } else if cfg!(target_os = "windows") && compiler_name.contains("cl") {
        vec![
            "/LD".to_string(),
            "/O2".to_string(),
            "/std:c++14".to_string(),
            source.to_string_lossy().to_string(),
            format!("/Fe:{}", output.to_string_lossy()),
        ]
    } else {
        // Linux or other Unix-like systems
        vec![
            "-shared".to_string(),
            "-O2".to_string(),
            "-std=c++14".to_string(),
            "-fPIC".to_string(),
            "-Wno-unused-function".to_string(),
            "-o".to_string(),
            output.to_string_lossy().to_string(),
            source.to_string_lossy().to_string(),
        ]
    }
}

/// Minimum valid size for a compiled dynamic library (bytes).
/// Any file smaller than this is likely corrupted or truncated.
const MIN_LIBRARY_SIZE: u64 = 1024;

/// Check if a cached library file is valid (exists and has reasonable size)
fn is_valid_cached_library(path: &Path) -> bool {
    match fs::metadata(path) {
        Ok(meta) => meta.len() >= MIN_LIBRARY_SIZE,
        Err(_) => false,
    }
}

/// Compile C++ source code to a dynamic library
///
/// This function:
/// 1. Computes a SHA-256 hash of the source
/// 2. Checks if a valid cached library exists
/// 3. If not cached, compiles to a temp file and atomically renames
///
/// Uses atomic writes to prevent race conditions when multiple tests
/// compile the same source in parallel (the compiler writes to a
/// PID-namespaced temp file, then renames to the final path).
///
/// # Arguments
///
/// * `source` - The C++ source code to compile
///
/// # Returns
///
/// Path to the compiled dynamic library
pub fn compile_cpp_kernel(source: &str) -> CompileResult<PathBuf> {
    let hash = hash_source(source);
    let cache = cache_dir();

    // Ensure cache directory exists
    fs::create_dir_all(&cache).map_err(|e| CompileError::CacheError(e.to_string()))?;

    let lib_path = cache.join(format!("{}.{}", hash, dylib_ext()));

    // Check cache - validate the file is not truncated/corrupted
    if is_valid_cached_library(&lib_path) {
        tracing::debug!("Using cached library: {}", lib_path.display());
        return Ok(lib_path);
    }

    // Remove corrupted cache entry if it exists
    if lib_path.exists() {
        tracing::warn!(
            "Removing corrupted cached library (too small): {}",
            lib_path.display()
        );
        let _ = fs::remove_file(&lib_path);
    }

    // Write source to temporary file
    let src_path = cache.join(format!("{}.cpp", hash));
    fs::write(&src_path, source)?;

    // Find compiler
    let compiler = find_cpp_compiler()?;
    tracing::debug!("Using compiler: {}", compiler.display());

    // Compile to a temp file first, then atomically rename.
    // This prevents other threads/processes from reading a partially-written library.
    // Use PID + atomic counter for guaranteed uniqueness across threads.
    static COMPILE_COUNTER: AtomicU64 = AtomicU64::new(0);
    let pid = std::process::id();
    let seq = COMPILE_COUNTER.fetch_add(1, Ordering::Relaxed);
    let tmp_lib_path = cache.join(format!("{}.{}.tmp.{}.{}", hash, dylib_ext(), pid, seq));

    let args = compiler_args(&compiler, &tmp_lib_path, &src_path);
    tracing::debug!("Compiling with args: {:?}", args);

    let output = Command::new(&compiler)
        .args(&args)
        .output()
        .map_err(|e| CompileError::CompilationFailed(e.to_string()))?;

    if !output.status.success() {
        // Clean up temp file on failure
        let _ = fs::remove_file(&tmp_lib_path);
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        return Err(CompileError::CompilationFailed(format!(
            "Compiler exited with status {}.\nstdout: {}\nstderr: {}",
            output.status, stdout, stderr
        )));
    }

    // Another thread may have cached the library while we were compiling.
    // If so, just use that and discard our copy.
    if is_valid_cached_library(&lib_path) {
        let _ = fs::remove_file(&tmp_lib_path);
        tracing::debug!(
            "Another thread cached library while we compiled: {}",
            lib_path.display()
        );
        return Ok(lib_path);
    }

    // Validate the compiled library before caching
    if !is_valid_cached_library(&tmp_lib_path) {
        let _ = fs::remove_file(&tmp_lib_path);
        return Err(CompileError::CompilationFailed(
            "Compiled library is too small, likely corrupted".to_string(),
        ));
    }

    // Atomically move temp file to final cache path.
    // fs::rename is atomic on the same filesystem (POSIX guarantee).
    if let Err(e) = fs::rename(&tmp_lib_path, &lib_path) {
        // Rename can fail cross-device; fall back to copy + delete
        tracing::warn!("Atomic rename failed, falling back to copy: {}", e);
        if let Err(copy_err) = fs::copy(&tmp_lib_path, &lib_path) {
            let _ = fs::remove_file(&tmp_lib_path);
            return Err(CompileError::CompilationFailed(format!(
                "Failed to cache compiled library: {}",
                copy_err
            )));
        }
        let _ = fs::remove_file(&tmp_lib_path);
    }

    tracing::info!("Compiled C++ kernel: {}", lib_path.display());
    Ok(lib_path)
}

/// Clear the compilation cache
pub fn clear_cache() -> CompileResult<()> {
    let cache = cache_dir();
    if cache.exists() {
        fs::remove_dir_all(&cache).map_err(|e| CompileError::CacheError(e.to_string()))?;
    }
    Ok(())
}

/// Get cache statistics
pub fn cache_stats() -> CompileResult<CacheStats> {
    let cache = cache_dir();
    let mut stats = CacheStats::default();

    if !cache.exists() {
        return Ok(stats);
    }

    for entry in fs::read_dir(&cache)? {
        let entry = entry?;
        let path = entry.path();
        if path.extension().map(|e| e == dylib_ext()).unwrap_or(false) {
            stats.num_libraries += 1;
            if let Ok(meta) = entry.metadata() {
                stats.total_size += meta.len();
            }
        }
    }

    Ok(stats)
}

/// Statistics about the compilation cache
#[derive(Debug, Default)]
pub struct CacheStats {
    /// Number of cached libraries
    pub num_libraries: usize,
    /// Total size in bytes
    pub total_size: u64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hash_source() {
        let hash1 = hash_source("int main() {}");
        let hash2 = hash_source("int main() {}");
        let hash3 = hash_source("int main() { return 0; }");

        assert_eq!(hash1, hash2);
        assert_ne!(hash1, hash3);
        assert_eq!(hash1.len(), 64); // SHA-256 produces 64 hex chars
    }

    #[test]
    fn test_find_compiler() {
        // This test may fail if no compiler is installed
        let result = find_cpp_compiler();
        if let Ok(compiler) = result {
            assert!(compiler.exists());
        }
    }

    #[test]
    fn test_dylib_ext() {
        let ext = dylib_ext();
        if cfg!(target_os = "macos") {
            assert_eq!(ext, "dylib");
        } else if cfg!(target_os = "windows") {
            assert_eq!(ext, "dll");
        } else {
            assert_eq!(ext, "so");
        }
    }
}
