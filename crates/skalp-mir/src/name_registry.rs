//! Name Registry for Collision-Free Signal/Port Naming
//!
//! This module provides a systematic approach to naming signals and ports that
//! guarantees no collisions between user-defined and compiler-generated names.
//!
//! ## Design Philosophy
//!
//! Users interact with signals using their natural hierarchical paths (e.g., `bms.connected`,
//! `controller.state`). Internally, all signals are assigned unique, collision-proof
//! internal names (`_s0`, `_s1`, etc.). The mapping between these is maintained by
//! the `NameRegistry`.
//!
//! ## Example
//!
//! ```ignore
//! // User writes in source code:
//! signal foo: bit
//! in bms: BmsData  // struct with field `connected`
//!
//! // Registry assigns:
//! // "foo" -> "_s0"
//! // "bms.connected" -> "_s1"
//! // "bms.fault" -> "_s2"
//!
//! // User references in testbench:
//! tb.set("bms.connected", 1)  // resolves to "_s1"
//! tb.get("foo")               // resolves to "_s0"
//! ```
//!
//! ## Benefits
//!
//! 1. **Zero collision risk** - internal names are systematically unique
//! 2. **Stable user API** - changing internal scheme doesn't break tests
//! 3. **Natural syntax** - users use hierarchical paths matching source code
//! 4. **Debuggable** - mapping available for inspection

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Unique internal name counter
static INTERNAL_NAME_PREFIX: &str = "_s";

/// Entry in the name registry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NameEntry {
    /// The internal name used in generated code (e.g., "_s42")
    pub internal_name: String,
    /// The user-facing hierarchical path (e.g., "bms.connected")
    pub hierarchical_path: String,
    /// The original source name (for debugging)
    pub source_name: String,
    /// Kind of named entity
    pub kind: NameKind,
    /// Bit width of the signal/port
    pub width: usize,
}

/// Kind of named entity in the registry
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum NameKind {
    /// Input port
    Input,
    /// Output port
    Output,
    /// Bidirectional port
    InOut,
    /// Internal signal
    Signal,
    /// Variable (in procedural block)
    Variable,
    /// State element (register)
    StateElement,
}

/// Bidirectional name registry for mapping hierarchical paths to internal names
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct NameRegistry {
    /// Counter for generating unique internal names
    next_id: u64,
    /// Map from hierarchical path to entry
    path_to_entry: IndexMap<String, NameEntry>,
    /// Map from internal name to hierarchical path (for reverse lookup)
    internal_to_path: HashMap<String, String>,
    /// Module name (for context in error messages)
    module_name: String,
}

impl NameRegistry {
    /// Create a new empty name registry
    pub fn new() -> Self {
        Self {
            next_id: 0,
            path_to_entry: IndexMap::new(),
            internal_to_path: HashMap::new(),
            module_name: String::new(),
        }
    }

    /// Create a new name registry for a specific module
    pub fn for_module(module_name: &str) -> Self {
        Self {
            next_id: 0,
            path_to_entry: IndexMap::new(),
            internal_to_path: HashMap::new(),
            module_name: module_name.to_string(),
        }
    }

    /// Register a new name and get its internal representation
    ///
    /// # Arguments
    /// * `hierarchical_path` - The user-facing path (e.g., "bms.connected")
    /// * `source_name` - The original name in source code
    /// * `kind` - The kind of named entity
    /// * `width` - Bit width of the signal/port
    ///
    /// # Returns
    /// The internal name assigned to this path
    pub fn register(
        &mut self,
        hierarchical_path: &str,
        source_name: &str,
        kind: NameKind,
        width: usize,
    ) -> String {
        // Check if already registered
        if let Some(entry) = self.path_to_entry.get(hierarchical_path) {
            return entry.internal_name.clone();
        }

        // Generate new internal name
        let internal_name = format!("{}{}", INTERNAL_NAME_PREFIX, self.next_id);
        self.next_id += 1;

        // Create entry
        let entry = NameEntry {
            internal_name: internal_name.clone(),
            hierarchical_path: hierarchical_path.to_string(),
            source_name: source_name.to_string(),
            kind,
            width,
        };

        // Store in both maps
        self.path_to_entry
            .insert(hierarchical_path.to_string(), entry);
        self.internal_to_path
            .insert(internal_name.clone(), hierarchical_path.to_string());

        internal_name
    }

    /// Register a port with struct field path
    ///
    /// # Arguments
    /// * `base_name` - The base port name (e.g., "bms")
    /// * `field_path` - The field path within the struct (e.g., ["connected"])
    /// * `kind` - Input/Output/InOut
    /// * `width` - Bit width
    ///
    /// # Returns
    /// The internal name for this port field
    pub fn register_port_field(
        &mut self,
        base_name: &str,
        field_path: &[String],
        kind: NameKind,
        width: usize,
    ) -> String {
        let hierarchical_path = if field_path.is_empty() {
            base_name.to_string()
        } else {
            format!("{}.{}", base_name, field_path.join("."))
        };

        let source_name = if field_path.is_empty() {
            base_name.to_string()
        } else {
            field_path.last().unwrap().clone()
        };

        self.register(&hierarchical_path, &source_name, kind, width)
    }

    /// Register a name with a specific internal name (for gate-level simulation)
    ///
    /// Unlike `register()`, this doesn't auto-generate an internal name.
    /// Used when loading gate-level netlists where signal names already exist.
    ///
    /// # Arguments
    /// * `hierarchical_path` - The user-facing path (e.g., "bms.connected")
    /// * `internal_name` - The actual signal name (e.g., "bms__connected")
    /// * `kind` - The kind of named entity
    /// * `width` - Bit width of the signal/port
    pub fn register_with_internal_name(
        &mut self,
        hierarchical_path: &str,
        internal_name: &str,
        kind: NameKind,
        width: usize,
    ) {
        // Skip if already registered
        if self.path_to_entry.contains_key(hierarchical_path) {
            return;
        }

        let entry = NameEntry {
            internal_name: internal_name.to_string(),
            hierarchical_path: hierarchical_path.to_string(),
            source_name: hierarchical_path.to_string(),
            kind,
            width,
        };

        self.path_to_entry
            .insert(hierarchical_path.to_string(), entry);
        self.internal_to_path
            .insert(internal_name.to_string(), hierarchical_path.to_string());
    }

    /// Resolve a hierarchical path to its internal name
    ///
    /// # Arguments
    /// * `path` - The hierarchical path to resolve
    ///
    /// # Returns
    /// The internal name if found, None otherwise
    pub fn resolve(&self, path: &str) -> Option<&str> {
        self.path_to_entry
            .get(path)
            .map(|e| e.internal_name.as_str())
    }

    /// Resolve an internal name back to its hierarchical path
    ///
    /// # Arguments
    /// * `internal_name` - The internal name to resolve
    ///
    /// # Returns
    /// The hierarchical path if found, None otherwise
    pub fn reverse_resolve(&self, internal_name: &str) -> Option<&str> {
        self.internal_to_path.get(internal_name).map(|s| s.as_str())
    }

    /// Get the entry for a hierarchical path
    pub fn get_entry(&self, path: &str) -> Option<&NameEntry> {
        self.path_to_entry.get(path)
    }

    /// Get the entry by internal name
    pub fn get_entry_by_internal(&self, internal_name: &str) -> Option<&NameEntry> {
        self.internal_to_path
            .get(internal_name)
            .and_then(|path| self.path_to_entry.get(path))
    }

    /// Check if a hierarchical path is registered
    pub fn contains(&self, path: &str) -> bool {
        self.path_to_entry.contains_key(path)
    }

    /// Get all entries of a specific kind
    pub fn entries_of_kind(&self, kind: NameKind) -> impl Iterator<Item = &NameEntry> {
        self.path_to_entry.values().filter(move |e| e.kind == kind)
    }

    /// Get all registered paths
    pub fn all_paths(&self) -> impl Iterator<Item = &str> {
        self.path_to_entry.keys().map(|s| s.as_str())
    }

    /// Get all entries
    pub fn all_entries(&self) -> impl Iterator<Item = &NameEntry> {
        self.path_to_entry.values()
    }

    /// Get the number of registered names
    pub fn len(&self) -> usize {
        self.path_to_entry.len()
    }

    /// Check if the registry is empty
    pub fn is_empty(&self) -> bool {
        self.path_to_entry.is_empty()
    }

    /// Get the module name
    pub fn module_name(&self) -> &str {
        &self.module_name
    }

    /// Merge another registry into this one
    ///
    /// Used when combining registries from multiple modules or instances
    /// Prefixes all paths from the other registry with the given prefix
    pub fn merge_with_prefix(&mut self, other: &NameRegistry, prefix: &str) {
        for entry in other.path_to_entry.values() {
            let new_path = if prefix.is_empty() {
                entry.hierarchical_path.clone()
            } else {
                format!("{}.{}", prefix, entry.hierarchical_path)
            };

            self.register(&new_path, &entry.source_name, entry.kind, entry.width);
        }
    }

    /// Create a debug dump of the registry
    pub fn debug_dump(&self) -> String {
        let mut output = format!("NameRegistry for module '{}'\n", self.module_name);
        output.push_str("=".repeat(60).as_str());
        output.push('\n');
        output.push_str(&format!(
            "{:<30} {:<10} {:>6} {:>8}\n",
            "Hierarchical Path", "Internal", "Width", "Kind"
        ));
        output.push_str("-".repeat(60).as_str());
        output.push('\n');

        for entry in self.path_to_entry.values() {
            let kind_str = match entry.kind {
                NameKind::Input => "input",
                NameKind::Output => "output",
                NameKind::InOut => "inout",
                NameKind::Signal => "signal",
                NameKind::Variable => "var",
                NameKind::StateElement => "state",
            };
            output.push_str(&format!(
                "{:<30} {:<10} {:>6} {:>8}\n",
                entry.hierarchical_path, entry.internal_name, entry.width, kind_str
            ));
        }

        output
    }
}

/// Name resolver that can resolve paths through a registry
/// with support for partial matches and array indices
#[derive(Debug)]
pub struct NameResolver<'a> {
    registry: &'a NameRegistry,
}

impl<'a> NameResolver<'a> {
    /// Create a new resolver for a registry
    pub fn new(registry: &'a NameRegistry) -> Self {
        Self { registry }
    }

    /// Resolve a path, with support for array indices and partial matches
    ///
    /// Supports paths like:
    /// - "foo" - direct signal
    /// - "bms.connected" - struct field
    /// - "data[0]" - array element
    /// - "fifo.data[5]" - nested array element
    pub fn resolve(&self, path: &str) -> Option<ResolvedName> {
        // First try exact match
        if let Some(entry) = self.registry.get_entry(path) {
            return Some(ResolvedName {
                internal_name: entry.internal_name.clone(),
                bit_select: None,
            });
        }

        // Try parsing array index
        if let Some((base, index)) = Self::parse_array_index(path) {
            // Try base.index format (for flattened arrays)
            let indexed_path = format!("{}.{}", base, index);
            if let Some(entry) = self.registry.get_entry(&indexed_path) {
                return Some(ResolvedName {
                    internal_name: entry.internal_name.clone(),
                    bit_select: None,
                });
            }

            // Try base with bit select
            if let Some(entry) = self.registry.get_entry(base) {
                return Some(ResolvedName {
                    internal_name: entry.internal_name.clone(),
                    bit_select: Some(index),
                });
            }
        }

        None
    }

    /// Parse array index from path like "data[5]"
    fn parse_array_index(path: &str) -> Option<(&str, usize)> {
        let bracket_start = path.find('[')?;
        let bracket_end = path.find(']')?;

        if bracket_end <= bracket_start {
            return None;
        }

        let base = &path[..bracket_start];
        let index_str = &path[bracket_start + 1..bracket_end];
        let index: usize = index_str.parse().ok()?;

        Some((base, index))
    }
}

/// Result of name resolution
#[derive(Debug, Clone)]
pub struct ResolvedName {
    /// The internal name
    pub internal_name: String,
    /// Optional bit select for array access
    pub bit_select: Option<usize>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_registration() {
        let mut registry = NameRegistry::for_module("test_module");

        let name1 = registry.register("foo", "foo", NameKind::Signal, 8);
        let name2 = registry.register("bar", "bar", NameKind::Signal, 16);

        assert_eq!(name1, "_s0");
        assert_eq!(name2, "_s1");

        assert_eq!(registry.resolve("foo"), Some("_s0"));
        assert_eq!(registry.resolve("bar"), Some("_s1"));
    }

    #[test]
    fn test_struct_field_registration() {
        let mut registry = NameRegistry::for_module("test_module");

        let name = registry.register_port_field(
            "bms",
            &["connected".to_string()],
            NameKind::Input,
            1,
        );

        assert_eq!(name, "_s0");
        assert_eq!(registry.resolve("bms.connected"), Some("_s0"));
    }

    #[test]
    fn test_nested_struct_field() {
        let mut registry = NameRegistry::for_module("test_module");

        let name = registry.register_port_field(
            "vertex",
            &["position".to_string(), "x".to_string()],
            NameKind::Input,
            32,
        );

        assert_eq!(registry.resolve("vertex.position.x"), Some(name.as_str()));
    }

    #[test]
    fn test_no_collision_with_user_names() {
        let mut registry = NameRegistry::for_module("test_module");

        // User defines a signal called "_s0"
        let user_s0 = registry.register("_s0", "_s0", NameKind::Signal, 1);
        // User defines a signal called "foo"
        let foo = registry.register("foo", "foo", NameKind::Signal, 1);

        // They get different internal names
        assert_eq!(user_s0, "_s0");
        assert_eq!(foo, "_s1");

        // Both can be resolved correctly
        assert_eq!(registry.resolve("_s0"), Some("_s0"));
        assert_eq!(registry.resolve("foo"), Some("_s1"));
    }

    #[test]
    fn test_reverse_resolution() {
        let mut registry = NameRegistry::for_module("test_module");

        registry.register("bms.connected", "connected", NameKind::Input, 1);

        assert_eq!(registry.reverse_resolve("_s0"), Some("bms.connected"));
    }

    #[test]
    fn test_resolver_array_index() {
        let mut registry = NameRegistry::for_module("test_module");

        // Register array elements
        registry.register("data.0", "0", NameKind::Signal, 8);
        registry.register("data.1", "1", NameKind::Signal, 8);

        let resolver = NameResolver::new(&registry);

        // Should resolve data[0] to data.0
        let resolved = resolver.resolve("data[0]").unwrap();
        assert_eq!(resolved.internal_name, "_s0");
    }

    #[test]
    fn test_idempotent_registration() {
        let mut registry = NameRegistry::for_module("test_module");

        let name1 = registry.register("foo", "foo", NameKind::Signal, 8);
        let name2 = registry.register("foo", "foo", NameKind::Signal, 8);

        // Same path should return same internal name
        assert_eq!(name1, name2);
        assert_eq!(registry.len(), 1);
    }

    #[test]
    fn test_merge_with_prefix() {
        let mut parent = NameRegistry::for_module("parent");
        let mut child = NameRegistry::for_module("child");

        child.register("clk", "clk", NameKind::Input, 1);
        child.register("data", "data", NameKind::Signal, 8);

        parent.merge_with_prefix(&child, "inst");

        assert!(parent.contains("inst.clk"));
        assert!(parent.contains("inst.data"));
    }
}
