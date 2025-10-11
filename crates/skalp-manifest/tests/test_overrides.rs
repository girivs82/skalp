//! Tests for dependency override system

use skalp_manifest::{from_str, Manifest};

#[test]
fn test_replace_dependency() {
    let toml = r#"
        [package]
        name = "test-lib"
        version = "1.0.0"

        [dependencies]
        skalp-numeric = "2.0"

        [replace]
        skalp-numeric = { path = "../local-numeric" }
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    assert!(manifest.validate().is_ok());

    // Check that replace section is parsed
    assert_eq!(manifest.replace.len(), 1);
    assert!(manifest.replace.contains_key("skalp-numeric"));

    // Test override resolution
    let resolved = manifest.get_resolved_dependencies(&[]);
    assert_eq!(resolved.len(), 1);

    let numeric_dep = resolved.get("skalp-numeric").unwrap();
    assert!(numeric_dep.is_path());
}

#[test]
fn test_patch_git_dependency() {
    let toml = r#"
        [package]
        name = "test-lib"
        version = "1.0.0"

        [dependencies]
        skalp-experimental = { git = "https://github.com/skalp/experimental", branch = "main" }

        [patch."https://github.com/skalp/experimental"]
        skalp-experimental = { path = "../local-experimental" }
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    assert!(manifest.validate().is_ok());

    // Check that patch section is parsed
    assert_eq!(manifest.patch.len(), 1);
    assert!(manifest
        .patch
        .contains_key("https://github.com/skalp/experimental"));

    // Test override resolution
    let resolved = manifest.get_resolved_dependencies(&[]);
    assert_eq!(resolved.len(), 1);

    let experimental_dep = resolved.get("skalp-experimental").unwrap();
    assert!(experimental_dep.is_path());
}

#[test]
fn test_multiple_patches() {
    let toml = r#"
        [package]
        name = "test-lib"
        version = "1.0.0"

        [dependencies]
        lib1 = { git = "https://github.com/example/monorepo", branch = "main" }
        lib2 = { git = "https://github.com/example/monorepo", branch = "main" }

        [patch."https://github.com/example/monorepo"]
        lib1 = { path = "../local-lib1" }
        lib2 = { path = "../local-lib2" }
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    assert!(manifest.validate().is_ok());

    let patches = manifest
        .patch
        .get("https://github.com/example/monorepo")
        .unwrap();
    assert_eq!(patches.len(), 2);

    // Test override resolution
    let resolved = manifest.get_resolved_dependencies(&[]);
    assert_eq!(resolved.len(), 2);

    assert!(resolved.get("lib1").unwrap().is_path());
    assert!(resolved.get("lib2").unwrap().is_path());
}

#[test]
fn test_no_override_applied() {
    let toml = r#"
        [package]
        name = "test-lib"
        version = "1.0.0"

        [dependencies]
        skalp-numeric = "2.0"

        [replace]
        other-lib = { path = "../other" }
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    assert!(manifest.validate().is_ok());

    // Test override resolution - numeric should not be affected
    let resolved = manifest.get_resolved_dependencies(&[]);
    assert_eq!(resolved.len(), 1);

    let numeric_dep = resolved.get("skalp-numeric").unwrap();
    assert!(!numeric_dep.is_path());
    assert!(!numeric_dep.is_git());
}

#[test]
fn test_replace_takes_precedence_over_patch() {
    let toml = r#"
        [package]
        name = "test-lib"
        version = "1.0.0"

        [dependencies]
        skalp-lib = { git = "https://github.com/skalp/lib", branch = "main" }

        [patch."https://github.com/skalp/lib"]
        skalp-lib = { path = "../patched-lib" }

        [replace]
        skalp-lib = { path = "../replaced-lib" }
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    assert!(manifest.validate().is_ok());

    // Replace should take precedence
    let resolved = manifest.get_resolved_dependencies(&[]);
    let lib_dep = resolved.get("skalp-lib").unwrap();
    assert!(lib_dep.is_path());
}

#[test]
fn test_patch_with_different_sources() {
    let toml = r#"
        [package]
        name = "test-lib"
        version = "1.0.0"

        [dependencies]
        lib1 = { git = "https://github.com/example/lib1", branch = "main" }
        lib2 = { git = "https://github.com/other/lib2", tag = "v1.0" }

        [patch."https://github.com/example/lib1"]
        lib1 = { path = "../local-lib1" }

        [patch."https://github.com/other/lib2"]
        lib2 = { path = "../local-lib2" }
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    assert!(manifest.validate().is_ok());

    assert_eq!(manifest.patch.len(), 2);

    let resolved = manifest.get_resolved_dependencies(&[]);
    assert!(resolved.get("lib1").unwrap().is_path());
    assert!(resolved.get("lib2").unwrap().is_path());
}

#[test]
fn test_override_with_features() {
    let toml = r#"
        [package]
        name = "test-lib"
        version = "1.0.0"

        [dependencies]
        base = "1.0"
        optional-lib = { version = "2.0", optional = true }

        [features]
        extra = ["optional-lib"]

        [replace]
        optional-lib = { path = "../local-optional" }
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    assert!(manifest.validate().is_ok());

    // Without feature, should only get base
    let resolved_no_feature = manifest.get_resolved_dependencies(&[]);
    assert_eq!(resolved_no_feature.len(), 1);
    assert!(resolved_no_feature.contains_key("base"));

    // With feature, should get both with override applied
    let resolved_with_feature = manifest.get_resolved_dependencies(&["extra".to_string()]);
    assert_eq!(resolved_with_feature.len(), 2);
    assert!(resolved_with_feature.contains_key("base"));
    assert!(resolved_with_feature.get("optional-lib").unwrap().is_path());
}

#[test]
fn test_invalid_empty_patch_source() {
    let toml = r#"
        [package]
        name = "test-lib"
        version = "1.0.0"

        [patch.""]
        lib = { path = "../lib" }
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    // Should fail validation
    assert!(manifest.validate().is_err());
}

#[test]
fn test_patch_with_version_upgrade() {
    let toml = r#"
        [package]
        name = "test-lib"
        version = "1.0.0"

        [dependencies]
        lib = { git = "https://github.com/example/lib", tag = "v1.0" }

        [patch."https://github.com/example/lib"]
        lib = { git = "https://github.com/example/lib", tag = "v2.0" }
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    assert!(manifest.validate().is_ok());

    let resolved = manifest.get_resolved_dependencies(&[]);
    let lib_dep = resolved.get("lib").unwrap();
    assert!(lib_dep.is_git());
}
