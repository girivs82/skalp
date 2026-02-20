//! Golden File Testing Framework
//!
//! This module provides utilities for golden file testing - comparing actual
//! test outputs against stored expected outputs. When outputs don't match,
//! the framework shows a clear diff and provides an easy way to update the
//! golden files if the change is intentional.
//!
//! # Usage
//!
//! ```rust,no_run
//! use skalp_testing::golden::GoldenTest;
//!
//! #[test]
//! fn test_alu_codegen() {
//!     let mut test = GoldenTest::new("alu_basic");
//!
//!     // Generate output
//!     let verilog = compile_to_verilog("examples/alu.sk");
//!
//!     // Compare against golden file
//!     test.assert_eq("verilog", &verilog);
//! }
//! ```
//!
//! # Updating Golden Files
//!
//! When you intentionally change output, update golden files with:
//! ```bash
//! SKALP_UPDATE_GOLDEN=1 cargo test
//! ```

use std::env;
use std::fs;
use std::path::{Path, PathBuf};

/// A golden file test case
///
/// Golden tests compare actual output against stored expected output.
/// If they differ, the test fails with a clear diff. The golden files
/// can be updated by setting SKALP_UPDATE_GOLDEN=1 environment variable.
pub struct GoldenTest {
    /// Name of the test (used for golden file naming)
    name: String,

    /// Directory containing golden files (defaults to tests/golden)
    golden_dir: PathBuf,

    /// Whether to update golden files instead of comparing
    update_mode: bool,
}

impl GoldenTest {
    /// Create a new golden test with the given name
    ///
    /// The name is used to generate golden file paths. For example,
    /// a test named "alu_basic" with extension "sv" will look for
    /// `tests/golden/alu_basic.sv`.
    pub fn new(name: &str) -> Self {
        // Determine the golden directory relative to workspace root
        // Use CARGO_WORKSPACE_DIR if available (Rust 1.83+), otherwise use CARGO_MANIFEST_DIR
        let workspace_root = env::var("CARGO_WORKSPACE_DIR").unwrap_or_else(|_| {
            // Fallback: traverse up from manifest dir to find Cargo.toml with [workspace]
            let manifest_dir =
                env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR should be set");
            let mut current = PathBuf::from(&manifest_dir);

            // Check current directory and all parents
            loop {
                let cargo_toml = current.join("Cargo.toml");
                if cargo_toml.exists() {
                    if let Ok(contents) = fs::read_to_string(&cargo_toml) {
                        if contents.contains("[workspace]") {
                            return current.to_string_lossy().to_string();
                        }
                    }
                }

                // Move to parent directory
                if let Some(parent) = current.parent() {
                    current = parent.to_path_buf();
                } else {
                    break;
                }
            }

            panic!(
                "Could not find workspace root (started from {})",
                manifest_dir
            )
        });

        let golden_dir = PathBuf::from(workspace_root).join("tests").join("golden");

        // Check if we should update golden files
        let update_mode = env::var("SKALP_UPDATE_GOLDEN")
            .map(|v| v == "1" || v.to_lowercase() == "true")
            .unwrap_or(false);

        Self {
            name: name.to_string(),
            golden_dir,
            update_mode,
        }
    }

    /// Create a golden test with a custom golden directory
    pub fn with_golden_dir(name: &str, golden_dir: impl Into<PathBuf>) -> Self {
        let update_mode = env::var("SKALP_UPDATE_GOLDEN")
            .map(|v| v == "1" || v.to_lowercase() == "true")
            .unwrap_or(false);

        Self {
            name: name.to_string(),
            golden_dir: golden_dir.into(),
            update_mode,
        }
    }

    /// Assert that actual output matches the golden file
    ///
    /// The golden file path is: `{golden_dir}/{name}.{extension}`
    ///
    /// # Panics
    ///
    /// Panics if the outputs don't match (in normal mode) or if the
    /// golden file can't be written (in update mode).
    pub fn assert_eq(&self, extension: &str, actual: &str) {
        let golden_path = self.golden_path(extension);

        if self.update_mode {
            self.update_golden(&golden_path, actual);
        } else {
            self.compare_golden(&golden_path, actual);
        }
    }

    /// Get the path to a golden file
    fn golden_path(&self, extension: &str) -> PathBuf {
        self.golden_dir.join(format!("{}.{}", self.name, extension))
    }

    /// Update a golden file with new content
    fn update_golden(&self, path: &Path, content: &str) {
        // Create directory if it doesn't exist
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).unwrap_or_else(|e| {
                panic!("Failed to create golden directory {:?}: {}", parent, e)
            });
        }

        // Write the golden file
        fs::write(path, content)
            .unwrap_or_else(|e| panic!("Failed to write golden file {:?}: {}", path, e));
    }

    /// Compare actual output against golden file
    fn compare_golden(&self, path: &Path, actual: &str) {
        // Read the golden file
        let expected = fs::read_to_string(path).unwrap_or_else(|e| {
            panic!(
                "Failed to read golden file {:?}: {}\n\
                 \n\
                 Hint: If this is a new test, run with SKALP_UPDATE_GOLDEN=1 to create the golden file.\n\
                 Example: SKALP_UPDATE_GOLDEN=1 cargo test {}",
                path, e, self.name
            )
        });

        // Compare line by line for better diff output
        if expected != actual {
            let diff = self.generate_diff(&expected, actual);
            panic!(
                "Golden file mismatch for test '{}'\n\
                 Golden file: {}\n\
                 \n\
                 {}\n\
                 \n\
                 To update the golden file if this change is intentional:\n\
                 SKALP_UPDATE_GOLDEN=1 cargo test {}",
                self.name,
                path.display(),
                diff,
                self.name
            );
        }
    }

    /// Generate a diff between expected and actual output
    fn generate_diff(&self, expected: &str, actual: &str) -> String {
        let expected_lines: Vec<&str> = expected.lines().collect();
        let actual_lines: Vec<&str> = actual.lines().collect();

        let mut diff = String::new();
        diff.push_str("Differences:\n");
        diff.push_str("=============\n\n");

        let max_lines = expected_lines.len().max(actual_lines.len());
        let mut diff_count = 0;

        for i in 0..max_lines {
            let exp_line = expected_lines.get(i).copied().unwrap_or("");
            let act_line = actual_lines.get(i).copied().unwrap_or("");

            if exp_line != act_line {
                diff_count += 1;
                if diff_count <= 10 {
                    // Show first 10 differences
                    diff.push_str(&format!("Line {}:\n", i + 1));
                    diff.push_str(&format!("  Expected: {}\n", exp_line));
                    diff.push_str(&format!("  Actual:   {}\n", act_line));
                    diff.push('\n');
                }
            }
        }

        if diff_count > 10 {
            diff.push_str(&format!("... and {} more differences\n", diff_count - 10));
        }

        diff.push_str(&format!(
            "\nTotal lines: expected={}, actual={}\n",
            expected_lines.len(),
            actual_lines.len()
        ));
        diff.push_str(&format!("Different lines: {}\n", diff_count));

        diff
    }
}

/// Helper macro for golden file tests
///
/// # Example
///
/// ```rust,no_run
/// use skalp_testing::golden_test;
///
/// golden_test!(test_alu_codegen, "alu_basic", "sv", {
///     let verilog = compile_to_verilog("examples/alu.sk");
///     verilog
/// });
/// ```
#[macro_export]
macro_rules! golden_test {
    ($test_name:ident, $golden_name:expr, $extension:expr, $body:block) => {
        #[test]
        fn $test_name() {
            let mut test = $crate::golden::GoldenTest::new($golden_name);
            let actual: String = $body;
            test.assert_eq($extension, &actual);
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_golden_path_generation() {
        let test = GoldenTest::new("test_example");
        let path = test.golden_path("sv");
        assert!(path.to_string_lossy().contains("test_example.sv"));
    }

    #[test]
    fn test_update_mode_detection() {
        // Note: Can't easily test env var in unit tests without affecting other tests
        // This is more of a smoke test
        let test = GoldenTest::new("test");
        // Just verify the field exists and has a boolean value
        let _mode = test.update_mode;
    }
}
