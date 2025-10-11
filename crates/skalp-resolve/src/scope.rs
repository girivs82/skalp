//! Scope management for name resolution

use crate::{ItemId, UseId};
use std::collections::HashMap;

/// Unique identifier for a scope
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub u32);

/// A single scope containing names and their bindings
#[derive(Debug, Clone)]
pub struct Scope {
    pub id: ScopeId,
    pub parent: Option<ScopeId>,
    pub items: HashMap<String, ItemId>,
    pub imports: Vec<UseId>,
}

impl Scope {
    pub fn new(id: ScopeId, parent: Option<ScopeId>) -> Self {
        Self {
            id,
            parent,
            items: HashMap::new(),
            imports: Vec::new(),
        }
    }

    /// Add an item to this scope
    pub fn add_item(&mut self, name: String, item: ItemId) {
        self.items.insert(name, item);
    }

    /// Lookup a name in this scope (not parent)
    pub fn lookup_local(&self, name: &str) -> Option<ItemId> {
        self.items.get(name).copied()
    }
}

/// Tree of scopes for hierarchical name lookup
#[derive(Debug, Clone)]
pub struct ScopeTree {
    scopes: HashMap<ScopeId, Scope>,
    root: ScopeId,
    next_id: u32,
}

impl Default for ScopeTree {
    fn default() -> Self {
        Self::new()
    }
}

impl ScopeTree {
    pub fn new() -> Self {
        let root = ScopeId(0);
        let mut scopes = HashMap::new();
        scopes.insert(root, Scope::new(root, None));

        Self {
            scopes,
            root,
            next_id: 1,
        }
    }

    /// Create a new child scope
    pub fn create_child(&mut self, parent: ScopeId) -> ScopeId {
        let id = ScopeId(self.next_id);
        self.next_id += 1;

        let scope = Scope::new(id, Some(parent));
        self.scopes.insert(id, scope);

        id
    }

    /// Get root scope
    pub fn root(&self) -> ScopeId {
        self.root
    }

    /// Get a scope by ID
    pub fn get(&self, id: ScopeId) -> Option<&Scope> {
        self.scopes.get(&id)
    }

    /// Get a mutable scope by ID
    pub fn get_mut(&mut self, id: ScopeId) -> Option<&mut Scope> {
        self.scopes.get_mut(&id)
    }

    /// Lookup a name starting from the given scope, searching parent scopes
    pub fn lookup(&self, name: &str, scope: ScopeId) -> Option<ItemId> {
        let mut current = Some(scope);

        while let Some(scope_id) = current {
            if let Some(scope) = self.scopes.get(&scope_id) {
                if let Some(item) = scope.lookup_local(name) {
                    return Some(item);
                }
                current = scope.parent;
            } else {
                break;
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scope_tree_creation() {
        let tree = ScopeTree::new();
        assert_eq!(tree.root, ScopeId(0));
    }

    #[test]
    fn test_create_child_scope() {
        let mut tree = ScopeTree::new();
        let child = tree.create_child(tree.root());

        assert_eq!(child, ScopeId(1));
        assert!(tree.get(child).is_some());
        assert_eq!(tree.get(child).unwrap().parent, Some(tree.root()));
    }

    #[test]
    fn test_lookup_in_scope() {
        let mut tree = ScopeTree::new();
        let root = tree.root();

        let item_id = ItemId(42);
        tree.get_mut(root)
            .unwrap()
            .add_item("test".to_string(), item_id);

        assert_eq!(tree.lookup("test", root), Some(item_id));
        assert_eq!(tree.lookup("nonexistent", root), None);
    }

    #[test]
    fn test_lookup_in_parent_scope() {
        let mut tree = ScopeTree::new();
        let root = tree.root();
        let child = tree.create_child(root);

        let item_id = ItemId(42);
        tree.get_mut(root)
            .unwrap()
            .add_item("test".to_string(), item_id);

        // Should find item from parent scope
        assert_eq!(tree.lookup("test", child), Some(item_id));
    }
}
