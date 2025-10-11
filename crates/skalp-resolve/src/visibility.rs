//! Visibility rules for the module system

/// Visibility level for items
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Visibility {
    /// Public - visible everywhere (pub)
    Public,
    /// Crate-local - visible within the crate (pub(crate))
    Crate,
    /// Parent module only (pub(super))
    Super,
    /// Private - only visible in the same module (default)
    Private,
}

impl Visibility {
    /// Check if item with this visibility is visible from the given context
    pub fn is_visible_from(
        &self,
        item_module: crate::ModuleId,
        context_module: crate::ModuleId,
    ) -> bool {
        match self {
            Visibility::Public => true,
            Visibility::Private => item_module == context_module,
            // TODO: Implement proper crate/super checking
            Visibility::Crate => true, // Placeholder
            Visibility::Super => true, // Placeholder
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_public_always_visible() {
        let item_mod = crate::ModuleId(1);
        let context_mod = crate::ModuleId(2);

        assert!(Visibility::Public.is_visible_from(item_mod, context_mod));
    }

    #[test]
    fn test_private_only_same_module() {
        let same_mod = crate::ModuleId(1);
        let other_mod = crate::ModuleId(2);

        assert!(Visibility::Private.is_visible_from(same_mod, same_mod));
        assert!(!Visibility::Private.is_visible_from(same_mod, other_mod));
    }
}
