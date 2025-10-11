# Cache List Enhancement

**Date**: 2025-10-11
**Status**: ✅ Complete

## Overview

Enhanced the `skalp cache list` command from a stub implementation to a fully functional feature that displays all cached packages with detailed metadata.

## Problem

The original implementation (Phase 6) was a stub that only showed a warning message:

```rust
fn list_cache() -> Result<()> {
    println!("⚠️  Cache listing not yet fully implemented");
    println!("   This feature will show all cached packages");
    Ok(())
}
```

## Solution

Implemented complete cache listing functionality with the following features:

### 1. CachedPackageInfo Struct

Added a new struct to represent cached package metadata:

```rust
#[derive(Debug, Clone)]
pub struct CachedPackageInfo {
    pub name: String,
    pub version: String,
    pub path: PathBuf,
    pub size: u64,  // Size in bytes
}
```

### 2. Cache::list() Method

Implemented a method that walks the cache directory and extracts package information:

```rust
pub fn list(&self) -> Result<Vec<CachedPackageInfo>> {
    // 1. Walk cache directory (max_depth = 3 for SHA256 structure)
    // 2. Find all skalp.toml manifest files
    // 3. Parse manifests to extract name and version
    // 4. Calculate directory sizes
    // 5. Return sorted list
}
```

**Implementation details:**
- Uses `walkdir::WalkDir` with `max_depth(3)` because cache structure is:
  - Level 1: First 2 chars of hash
  - Level 2: Remaining hash chars
  - Level 3: Package directory
- Parses TOML manifests using `toml::from_str::<serde_json::Value>()`
- Calculates sizes using recursive `dir_size()` helper
- Sorts results by name, then version

### 3. dir_size() Helper

Added helper method for calculating directory sizes:

```rust
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
```

### 4. Enhanced CLI Display

Updated `list_cache()` to show formatted output:

```rust
fn list_cache() -> Result<()> {
    use skalp_package::{cache::Cache, RegistryConfig};

    let config = RegistryConfig::default();
    let cache = Cache::new(config.cache_dir.clone());

    println!("📦 Cached packages in: {:?}\n", config.cache_dir);

    let packages = cache.list()?;

    if packages.is_empty() {
        println!("No packages in cache");
        return Ok(());
    }

    println!(
        "Found {} cached package{}:\n",
        packages.len(),
        if packages.len() == 1 { "" } else { "s" }
    );

    for (i, pkg) in packages.iter().enumerate() {
        let size_kb = pkg.size as f64 / 1024.0;
        let size_str = if size_kb < 1024.0 {
            format!("{:.2} KB", size_kb)
        } else {
            format!("{:.2} MB", size_kb / 1024.0)
        };

        println!("{}. {} v{}", i + 1, pkg.name, pkg.version);
        println!("   Size: {}", size_str);
        println!("   Path: {}", pkg.path.display());
        println!();
    }

    Ok(())
}
```

## Example Output

```bash
$ skalp cache list
📦 Cached packages in: "/Users/user/.cache/skalp"

Found 3 cached packages:

1. skalp-dsp v1.5.0
   Size: 2.45 MB
   Path: /Users/user/.cache/skalp/a8/9f3e2...

2. skalp-numeric v2.0.1
   Size: 5.12 MB
   Path: /Users/user/.cache/skalp/3c/d1b2e...

3. skalp-numeric v2.0.2
   Size: 5.18 MB
   Path: /Users/user/.cache/skalp/f4/8a3c9...
```

## Files Modified

1. **crates/skalp-package/src/cache.rs** (+83 lines)
   - Added `CachedPackageInfo` struct
   - Added `list()` method
   - Added `dir_size()` helper

2. **crates/skalp-package/src/lib.rs** (+1 line)
   - Exported `CachedPackageInfo` type

3. **crates/skalp-package/Cargo.toml** (+1 line)
   - Added `serde_json = "1.0"` dependency

4. **src/main.rs** (+28 lines modified, -3 lines removed)
   - Replaced stub with full implementation

5. **Cargo.lock**
   - Updated with serde_json dependency

## Dependencies Added

- **serde_json = "1.0"**: Required for flexible TOML parsing as JSON values

## Testing

### Compilation
✅ Successfully compiled with no errors

### CI Validation
✅ All checks passed:
- Formatting (`cargo fmt`)
- Clippy on stable
- Clippy on beta
- Build with all features
- All test suites

### Manual Testing
The feature was tested manually and verified to:
- Correctly identify cached packages
- Parse manifest files accurately
- Calculate sizes correctly
- Display formatted output
- Handle empty cache gracefully

## Design Decisions

### 1. serde_json for TOML Parsing
**Rationale**: Using `toml::from_str::<serde_json::Value>()` provides flexible parsing without requiring full Manifest structs. This allows us to extract just the fields we need (name, version) without deserializing the entire manifest.

### 2. SHA256 Cache Structure
**Rationale**: The cache uses content-addressable storage with SHA256 hashing. The directory structure splits the hash into 2 characters (first level) and the remaining characters (second level) to avoid having too many files in a single directory.

### 3. Sorted Output
**Rationale**: Sorting by name then version provides consistent, predictable output that's easy to scan visually.

### 4. Human-Readable Sizes
**Rationale**: Displaying sizes in KB/MB (instead of just bytes) makes the output more user-friendly.

## Integration

This enhancement integrates seamlessly with the existing Phase 5-6 implementation:

- Uses existing `Cache` structure
- Works with existing SHA256-based cache layout
- Compatible with all other cache operations (size, clear, remove)
- Follows established error handling patterns

## Performance Considerations

For large caches:
- **Directory Walking**: O(n) where n = number of files in cache
- **TOML Parsing**: O(m) where m = number of packages
- **Size Calculation**: O(n) again for counting file sizes

The implementation uses efficient iterators and avoids loading unnecessary data into memory. For typical caches with dozens to hundreds of packages, performance is excellent.

## Future Enhancements

Potential improvements for future work:

1. **Filtering**: Add `--name` or `--version` filters
2. **Sorting Options**: Allow sorting by size, date, etc.
3. **JSON Output**: Add `--json` flag for machine-readable output
4. **Validation**: Check cache integrity and report corrupted packages
5. **Statistics**: Show total cache size, oldest/newest packages

## Commit

**Commit**: ad27876
**Message**: "Implement cache list functionality for package management"

## Status

✅ **Complete and ready for use**

The cache list functionality is now fully implemented and provides users with clear visibility into their local package cache.
