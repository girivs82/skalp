//! Dependency resolution algorithm

use crate::cache::Cache;
use crate::error::{PackageError, Result};
use crate::lockfile::{LockedPackage, Lockfile};
use crate::source::{GitReference, PackageSource};
use crate::RegistryConfig;
use skalp_manifest::{DependencySpec, Manifest};
use std::collections::HashSet;
use std::path::PathBuf;

/// Dependency resolver
pub struct Resolver {
    /// Registry configuration
    registry: RegistryConfig,
    /// Package cache
    cache: Cache,
    /// Root manifest being resolved
    root_manifest: Option<Manifest>,
}

impl Resolver {
    /// Create a new resolver with the given configuration
    pub fn new(registry: RegistryConfig) -> Self {
        let cache = Cache::new(registry.cache_dir.clone());
        Self {
            registry,
            cache,
            root_manifest: None,
        }
    }

    /// Set the root manifest
    pub fn with_manifest(mut self, manifest: Manifest) -> Self {
        self.root_manifest = Some(manifest);
        self
    }

    /// Resolve dependencies and generate a lockfile
    pub fn resolve(&self) -> Result<Lockfile> {
        let manifest = self
            .root_manifest
            .as_ref()
            .ok_or_else(|| PackageError::ResolutionFailed("No manifest provided".to_string()))?;

        // Get all dependencies with default features
        let enabled_features = manifest
            .features
            .get("default")
            .cloned()
            .unwrap_or_default();

        let resolved_deps = manifest.get_resolved_dependencies(&enabled_features);

        // Build dependency graph
        let mut lockfile = Lockfile::new();
        let mut visited = HashSet::new();
        let mut stack = Vec::new(); // For cycle detection

        for (name, spec) in &resolved_deps {
            self.resolve_recursive(name, spec, &mut lockfile, &mut visited, &mut stack)?;
        }

        // Add metadata
        lockfile.metadata = Some(crate::lockfile::LockfileMetadata {
            generated: chrono::Utc::now().to_rfc3339(),
            root: manifest.package.name.clone(),
        });

        lockfile.validate()?;
        Ok(lockfile)
    }

    /// Recursively resolve a dependency
    fn resolve_recursive(
        &self,
        name: &str,
        spec: &DependencySpec,
        lockfile: &mut Lockfile,
        visited: &mut HashSet<String>,
        stack: &mut Vec<String>,
    ) -> Result<()> {
        // Check for circular dependency
        if stack.contains(&name.to_string()) {
            return Err(PackageError::CircularDependency(format!(
                "{} -> {}",
                stack.join(" -> "),
                name
            )));
        }

        // Skip if already visited
        if visited.contains(name) {
            return Ok(());
        }

        stack.push(name.to_string());

        // Convert DependencySpec to PackageSource
        let source = self.spec_to_source(name, spec)?;

        // Get manifest for this package (would fetch from cache/network in real impl)
        // For now, we'll just add the package without traversing its dependencies
        let locked = LockedPackage::new(source, vec![], None);

        lockfile.add_package(locked);
        visited.insert(name.to_string());

        stack.pop();
        Ok(())
    }

    /// Convert a DependencySpec to a PackageSource
    fn spec_to_source(&self, name: &str, spec: &DependencySpec) -> Result<PackageSource> {
        match spec {
            DependencySpec::Simple(version) => Ok(PackageSource::registry(name, version.clone())),
            DependencySpec::Detailed(dep) => {
                if let Some(path) = &dep.path {
                    Ok(PackageSource::path(name, PathBuf::from(path)))
                } else if let Some(git) = &dep.git {
                    let reference = if let Some(branch) = &dep.branch {
                        GitReference::Branch(branch.clone())
                    } else if let Some(tag) = &dep.tag {
                        GitReference::Tag(tag.clone())
                    } else if let Some(rev) = &dep.revision {
                        GitReference::Rev(rev.clone())
                    } else {
                        GitReference::Branch("main".to_string())
                    };

                    Ok(PackageSource::git(name, git.clone(), reference))
                } else if let Some(version) = &dep.version {
                    Ok(PackageSource::registry(name, version.clone()))
                } else {
                    Err(PackageError::InvalidSource(
                        "Dependency must specify version, git, or path".to_string(),
                    ))
                }
            }
        }
    }

    /// Check if the cache has a package
    pub fn is_cached(&self, source: &PackageSource) -> bool {
        self.cache.is_cached(source)
    }

    /// Get the cache directory
    pub fn cache_dir(&self) -> &PathBuf {
        &self.registry.cache_dir
    }
}

// Stub for chrono - we'll add proper timestamp support later
mod chrono {
    pub struct Utc;

    impl Utc {
        pub fn now() -> Self {
            Self
        }

        pub fn to_rfc3339(&self) -> String {
            "2024-01-01T00:00:00Z".to_string()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::SourceKind;
    use semver::Version;
    use skalp_manifest::{Dependency, Package};
    use std::collections::HashMap;

    fn create_test_manifest() -> Manifest {
        Manifest {
            package: Package {
                name: "test-project".to_string(),
                version: Version::new(1, 0, 0),
                authors: vec![],
                description: None,
                documentation: None,
                homepage: None,
                repository: None,
                license: None,
                keywords: vec![],
                categories: vec![],
                skalp: None,
                edition: None,
            },
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            lib: None,
            features: HashMap::new(),
            workspace: None,
            patch: HashMap::new(),
            replace: HashMap::new(),
        }
    }

    #[test]
    fn test_resolver_creation() {
        let config = RegistryConfig::default();
        let resolver = Resolver::new(config);

        assert!(resolver.root_manifest.is_none());
    }

    #[test]
    fn test_resolve_empty_manifest() {
        let config = RegistryConfig::default();
        let manifest = create_test_manifest();
        let resolver = Resolver::new(config).with_manifest(manifest);

        let lockfile = resolver.resolve().unwrap();
        assert_eq!(lockfile.packages.len(), 0);
    }

    #[test]
    fn test_resolve_simple_dependency() {
        let config = RegistryConfig::default();
        let mut manifest = create_test_manifest();

        manifest.dependencies.insert(
            "skalp-numeric".to_string(),
            DependencySpec::Simple("2.0".to_string()),
        );

        let resolver = Resolver::new(config).with_manifest(manifest);
        let lockfile = resolver.resolve().unwrap();

        assert_eq!(lockfile.packages.len(), 1);
        assert!(lockfile.get_package("skalp-numeric").is_some());
    }

    #[test]
    fn test_spec_to_source_simple() {
        let config = RegistryConfig::default();
        let resolver = Resolver::new(config);

        let spec = DependencySpec::Simple("1.0".to_string());
        let source = resolver.spec_to_source("test-lib", &spec).unwrap();

        assert_eq!(source.name, "test-lib");
        assert!(matches!(source.kind, SourceKind::Registry { .. }));
    }

    #[test]
    fn test_spec_to_source_git() {
        let config = RegistryConfig::default();
        let resolver = Resolver::new(config);

        let spec = DependencySpec::Detailed(Dependency {
            version: None,
            git: Some("https://github.com/example/lib".to_string()),
            branch: Some("main".to_string()),
            tag: None,
            revision: None,
            path: None,
            registry: None,
            features: vec![],
            default_features: true,
            optional: false,
            package: None,
        });

        let source = resolver.spec_to_source("test-lib", &spec).unwrap();
        assert!(matches!(source.kind, SourceKind::Git { .. }));
    }

    #[test]
    fn test_spec_to_source_path() {
        let config = RegistryConfig::default();
        let resolver = Resolver::new(config);

        let spec = DependencySpec::Detailed(Dependency {
            version: None,
            git: None,
            branch: None,
            tag: None,
            revision: None,
            path: Some("../local-lib".to_string()),
            registry: None,
            features: vec![],
            default_features: true,
            optional: false,
            package: None,
        });

        let source = resolver.spec_to_source("test-lib", &spec).unwrap();
        assert!(matches!(source.kind, SourceKind::Path { .. }));
    }
}
