//! Integration tests for manifest parsing

use skalp_manifest::{from_str, Manifest};

#[test]
fn test_complete_manifest() {
    let toml = r#"
        [package]
        name = "skalp-dsp"
        version = "1.0.0"
        authors = ["Test Author <test@example.com>"]
        description = "DSP library"
        license = "MIT"
        keywords = ["dsp", "fft"]

        [dependencies]
        skalp-numeric = "2.0"
        skalp-stdlib = { version = "1.5", features = ["fp"] }

        [dev-dependencies]
        skalp-testing = "1.0"

        [features]
        default = ["fft"]
        fft = []
        gpu = ["skalp-stdlib/metal"]
    "#;

    let manifest: Manifest = from_str(toml).unwrap();

    assert_eq!(manifest.package.name, "skalp-dsp");
    assert_eq!(manifest.package.version.to_string(), "1.0.0");
    assert_eq!(manifest.package.license, Some("MIT".to_string()));
    assert_eq!(manifest.dependencies.len(), 2);
    assert_eq!(manifest.dev_dependencies.len(), 1);
    assert_eq!(manifest.features.len(), 3);

    assert!(manifest.validate().is_ok());
}

#[test]
fn test_simple_library() {
    let toml = r#"
        [package]
        name = "my-ip-core"
        version = "0.1.0"
        authors = ["Developer <dev@example.com>"]
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    assert_eq!(manifest.package.name, "my-ip-core");
    assert!(manifest.validate().is_ok());
}

#[test]
fn test_git_dependencies() {
    let toml = r#"
        [package]
        name = "test-lib"
        version = "1.0.0"

        [dependencies]
        experimental = { git = "https://github.com/example/lib", branch = "dev" }
        stable = { git = "https://github.com/example/stable", tag = "v1.0" }
        specific = { git = "https://github.com/example/specific", rev = "abc123" }
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    assert_eq!(manifest.dependencies.len(), 3);
    assert!(manifest.validate().is_ok());
}

#[test]
fn test_path_dependencies() {
    let toml = r#"
        [package]
        name = "test-lib"
        version = "1.0.0"

        [dependencies]
        local-lib = { path = "../local-lib" }
        another = { path = "/absolute/path/lib" }
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    assert_eq!(manifest.dependencies.len(), 2);
    assert!(manifest.validate().is_ok());
}

#[test]
fn test_optional_dependencies() {
    let toml = r#"
        [package]
        name = "test-lib"
        version = "1.0.0"

        [dependencies]
        required = "1.0"
        optional-lib = { version = "2.0", optional = true }

        [features]
        extra = ["optional-lib"]
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    assert_eq!(manifest.dependencies.len(), 2);

    // Check that optional-lib is marked as optional
    let optional_dep = manifest.dependencies.get("optional-lib").unwrap();
    assert!(optional_dep.is_optional());

    assert!(manifest.validate().is_ok());
}

#[test]
fn test_feature_dependencies() {
    let toml = r#"
        [package]
        name = "test-lib"
        version = "1.0.0"

        [dependencies]
        base = "1.0"
        extra = { version = "2.0", optional = true }

        [features]
        default = ["basic"]
        basic = []
        advanced = ["extra"]
        full = ["basic", "advanced"]
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    assert_eq!(manifest.features.len(), 4);

    // Test get_dependencies_with_features
    let basic_deps = manifest.get_dependencies_with_features(&["basic".to_string()]);
    assert_eq!(basic_deps.len(), 1); // Only base, not extra

    let advanced_deps = manifest.get_dependencies_with_features(&["advanced".to_string()]);
    assert_eq!(advanced_deps.len(), 2); // base + extra

    assert!(manifest.validate().is_ok());
}

#[test]
fn test_workspace_manifest() {
    let toml = r#"
        [workspace]
        members = [
            "crates/lib1",
            "crates/lib2",
            "examples/*"
        ]
        exclude = [
            "old-crates"
        ]

        [package]
        name = "workspace-root"
        version = "1.0.0"
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    assert!(manifest.workspace.is_some());

    let workspace = manifest.workspace.as_ref().unwrap();
    assert_eq!(workspace.members.len(), 3);
    assert_eq!(workspace.exclude.len(), 1);
}

#[test]
fn test_library_config() {
    let toml = r#"
        [package]
        name = "test-lib"
        version = "1.0.0"

        [lib]
        name = "custom_name"
        path = "src/custom.sk"
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    assert!(manifest.lib.is_some());

    let lib = manifest.lib.as_ref().unwrap();
    assert_eq!(lib.name, Some("custom_name".to_string()));
    assert_eq!(lib.path, Some("src/custom.sk".to_string()));
}

#[test]
fn test_invalid_package_name() {
    let toml = r#"
        [package]
        name = "123invalid"
        version = "1.0.0"
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    assert!(manifest.validate().is_err());
}

#[test]
fn test_invalid_dependency() {
    let toml = r#"
        [package]
        name = "test-lib"
        version = "1.0.0"

        [dependencies]
        bad-dep = { git = "https://example.com", version = "1.0" }
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    // Should fail validation due to multiple sources
    assert!(manifest.validate().is_err());
}

#[test]
fn test_missing_package_name() {
    let toml = r#"
        [package]
        version = "1.0.0"
    "#;

    // Should fail to parse
    let result = from_str(toml);
    assert!(result.is_err());
}

#[test]
fn test_minimal_valid_manifest() {
    let toml = r#"
        [package]
        name = "minimal"
        version = "0.1.0"
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    assert_eq!(manifest.package.name, "minimal");
    assert_eq!(manifest.package.version.to_string(), "0.1.0");
    assert_eq!(manifest.dependencies.len(), 0);
    assert_eq!(manifest.features.len(), 0);
    assert!(manifest.validate().is_ok());
}

#[test]
fn test_default_features_disabled() {
    let toml = r#"
        [package]
        name = "test"
        version = "1.0.0"

        [dependencies]
        lib = { version = "1.0", default-features = false, features = ["minimal"] }
    "#;

    let manifest: Manifest = from_str(toml).unwrap();
    let dep = manifest.dependencies.get("lib").unwrap();

    assert!(!dep.use_default_features());
    assert_eq!(dep.features(), vec!["minimal".to_string()]);
}
