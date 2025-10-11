//! Main resolver that performs name resolution across the module tree

use crate::{ItemId, ModuleId, Path, ScopeId, ScopeTree, UseId, Visibility};
use std::collections::HashMap;

/// The result of name resolution - maps AST nodes to their definitions
#[derive(Debug, Clone)]
pub struct ResolutionMap {
    /// Maps each name usage to its resolved definition
    pub resolutions: HashMap<NodeId, ItemId>,
    /// Maps each module to its scope
    pub module_scopes: HashMap<ModuleId, ScopeId>,
}

/// Unique identifier for AST/HIR nodes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub u32);

/// Errors that can occur during resolution
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolveError {
    /// Name not found in any scope
    NameNotFound { name: String, location: NodeId },
    /// Name is ambiguous (multiple imports)
    Ambiguous {
        name: String,
        candidates: Vec<ItemId>,
    },
    /// Item is not visible from current context
    NotVisible { name: String, item: ItemId },
    /// Circular dependency in imports
    CircularImport { path: Vec<UseId> },
    /// Invalid import path
    InvalidPath { path: Path },
}

/// Information about a resolved import
#[derive(Debug, Clone)]
pub struct ResolvedImport {
    pub use_id: UseId,
    pub path: Path,
    pub target: ItemId,
}

/// Information about a module
#[derive(Debug, Clone)]
pub struct Module {
    pub id: ModuleId,
    pub name: String,
    pub parent: Option<ModuleId>,
    pub scope: ScopeId,
    pub visibility: Visibility,
    pub items: HashMap<String, (ItemId, Visibility)>,
    pub submodules: Vec<ModuleId>,
}

/// Main resolver that performs name resolution
pub struct Resolver {
    /// All modules in the crate
    modules: HashMap<ModuleId, Module>,
    /// Scope tree for name lookup
    scopes: ScopeTree,
    /// Resolved imports
    #[allow(dead_code)]
    imports: HashMap<UseId, ResolvedImport>,
    /// Current module context (for visibility checking)
    current_module: Option<ModuleId>,
    /// Resolution results
    resolutions: HashMap<NodeId, ItemId>,
    /// Next IDs for various entities
    next_module_id: u32,
    next_use_id: u32,
    next_item_id: u32,
}

impl Resolver {
    pub fn new() -> Self {
        let scopes = ScopeTree::new();
        let root_scope = scopes.root();

        let root_module = Module {
            id: ModuleId(0),
            name: "root".to_string(),
            parent: None,
            scope: root_scope,
            visibility: Visibility::Public,
            items: HashMap::new(),
            submodules: Vec::new(),
        };

        let mut modules = HashMap::new();
        modules.insert(ModuleId(0), root_module);

        Self {
            modules,
            scopes,
            imports: HashMap::new(),
            current_module: Some(ModuleId(0)),
            resolutions: HashMap::new(),
            next_module_id: 1,
            next_use_id: 0,
            next_item_id: 0,
        }
    }

    /// Get the root module
    pub fn root_module(&self) -> ModuleId {
        ModuleId(0)
    }

    /// Create a new module
    pub fn create_module(
        &mut self,
        name: String,
        parent: ModuleId,
        visibility: Visibility,
    ) -> Result<ModuleId, ResolveError> {
        let id = ModuleId(self.next_module_id);
        self.next_module_id += 1;

        let parent_scope = self
            .modules
            .get(&parent)
            .ok_or_else(|| ResolveError::InvalidPath {
                path: Path::new(vec![name.clone()]),
            })?
            .scope;

        let scope = self.scopes.create_child(parent_scope);

        let module = Module {
            id,
            name: name.clone(),
            parent: Some(parent),
            scope,
            visibility,
            items: HashMap::new(),
            submodules: Vec::new(),
        };

        self.modules.insert(id, module);

        // Add to parent's submodules
        if let Some(parent_mod) = self.modules.get_mut(&parent) {
            parent_mod.submodules.push(id);
        }

        Ok(id)
    }

    /// Add an item to a module
    pub fn add_item(
        &mut self,
        module: ModuleId,
        name: String,
        visibility: Visibility,
    ) -> Result<ItemId, ResolveError> {
        let item_id = ItemId(self.next_item_id);
        self.next_item_id += 1;

        // Add to module's items
        if let Some(module_data) = self.modules.get_mut(&module) {
            module_data
                .items
                .insert(name.clone(), (item_id, visibility));

            // Add to scope
            if let Some(scope) = self.scopes.get_mut(module_data.scope) {
                scope.add_item(name, item_id);
            }
        }

        Ok(item_id)
    }

    /// Resolve a simple name in the current scope
    pub fn resolve_name(
        &self,
        name: &str,
        scope: ScopeId,
        node: NodeId,
    ) -> Result<ItemId, ResolveError> {
        // First try direct scope lookup
        if let Some(item) = self.scopes.lookup(name, scope) {
            // Check visibility
            if let Some(current_mod) = self.current_module {
                if self.is_visible(item, current_mod) {
                    return Ok(item);
                }
            }
        }

        Err(ResolveError::NameNotFound {
            name: name.to_string(),
            location: node,
        })
    }

    /// Resolve a path (e.g., std::ops::Add)
    pub fn resolve_path(
        &self,
        path: &Path,
        scope: ScopeId,
        node: NodeId,
    ) -> Result<ItemId, ResolveError> {
        if path.segments.is_empty() {
            return Err(ResolveError::InvalidPath { path: path.clone() });
        }

        // Start from the first segment
        let first = &path.segments[0];
        let mut current_item = self.resolve_name(first, scope, node)?;

        // Walk through remaining segments
        for segment in &path.segments[1..] {
            // TODO: Implement proper path walking through modules
            // For now, this is a placeholder
            current_item = self.resolve_name(segment, scope, node)?;
        }

        Ok(current_item)
    }

    /// Process a use statement
    pub fn process_use(&mut self, _path: Path, _scope: ScopeId) -> Result<UseId, ResolveError> {
        let use_id = UseId(self.next_use_id);
        self.next_use_id += 1;

        // Resolve the path to find the target
        // TODO: Implement proper use resolution
        // For now, this is a placeholder

        Ok(use_id)
    }

    /// Check if an item is visible from the current module
    fn is_visible(&self, item: ItemId, from_module: ModuleId) -> bool {
        // Find which module contains this item
        for (mod_id, module) in &self.modules {
            if let Some((_id, vis)) = module.items.values().find(|(id, _)| *id == item) {
                return vis.is_visible_from(*mod_id, from_module);
            }
        }

        // If we can't find the item, assume it's not visible
        false
    }

    /// Get the final resolution map
    pub fn finish(self) -> ResolutionMap {
        let module_scopes = self
            .modules
            .iter()
            .map(|(id, module)| (*id, module.scope))
            .collect();

        ResolutionMap {
            resolutions: self.resolutions,
            module_scopes,
        }
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolver_creation() {
        let resolver = Resolver::new();
        assert_eq!(resolver.root_module(), ModuleId(0));
    }

    #[test]
    fn test_create_module() {
        let mut resolver = Resolver::new();
        let root = resolver.root_module();

        let child = resolver
            .create_module("child".to_string(), root, Visibility::Public)
            .unwrap();

        assert_eq!(child, ModuleId(1));
        assert!(resolver.modules.contains_key(&child));
    }

    #[test]
    fn test_add_item() {
        let mut resolver = Resolver::new();
        let root = resolver.root_module();

        let item = resolver
            .add_item(root, "test_item".to_string(), Visibility::Public)
            .unwrap();

        assert_eq!(item, ItemId(0));

        let root_module = resolver.modules.get(&root).unwrap();
        assert!(root_module.items.contains_key("test_item"));
    }

    #[test]
    fn test_resolve_simple_name() {
        let mut resolver = Resolver::new();
        let root = resolver.root_module();
        let root_scope = resolver.modules.get(&root).unwrap().scope;

        let item = resolver
            .add_item(root, "test".to_string(), Visibility::Public)
            .unwrap();

        let resolved = resolver
            .resolve_name("test", root_scope, NodeId(0))
            .unwrap();

        assert_eq!(resolved, item);
    }

    #[test]
    fn test_resolve_nonexistent_name() {
        let resolver = Resolver::new();
        let root = resolver.root_module();
        let root_scope = resolver.modules.get(&root).unwrap().scope;

        let result = resolver.resolve_name("nonexistent", root_scope, NodeId(0));

        assert!(matches!(result, Err(ResolveError::NameNotFound { .. })));
    }

    #[test]
    fn test_nested_modules() {
        let mut resolver = Resolver::new();
        let root = resolver.root_module();

        let parent = resolver
            .create_module("parent".to_string(), root, Visibility::Public)
            .unwrap();

        let child = resolver
            .create_module("child".to_string(), parent, Visibility::Public)
            .unwrap();

        assert_eq!(child, ModuleId(2));

        let parent_module = resolver.modules.get(&parent).unwrap();
        assert!(parent_module.submodules.contains(&child));
    }
}
