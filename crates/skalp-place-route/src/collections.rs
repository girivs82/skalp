//! Hash maps and sets that iterate in the same order on every run.
//!
//! `std::collections::HashMap` seeds its hasher randomly per map, so two maps
//! holding the same keys, even in one process, iterate in different orders.
//! Placement and routing iterate maps in many places where the order decides
//! something (which cell is placed first, which net is ripped up first, which
//! of two equal-cost sites wins), so with the std hasher the same netlist and
//! seed gave a different placement, routing and bitstream on every run.
//!
//! Every map and set in this crate uses these instead: the same types with a
//! fixed hasher ([`FxHasher`], which is also faster on the small integer keys
//! the placer and router use), so iteration order depends only on the keys and
//! the order they were inserted. [`HashMapExt`] and [`HashSetExt`] give them
//! the `new` and `with_capacity` constructors std provides only for its
//! randomly seeded hasher.

use rustc_hash::FxHasher;
use std::hash::BuildHasherDefault;

/// The fixed hasher every map and set in the crate uses
pub type FixedState = BuildHasherDefault<FxHasher>;

/// A `std::collections::HashMap` that iterates in a reproducible order
pub type HashMap<K, V> = std::collections::HashMap<K, V, FixedState>;

/// A `std::collections::HashSet` that iterates in a reproducible order
pub type HashSet<T> = std::collections::HashSet<T, FixedState>;

/// `new` and `with_capacity` for [`HashMap`]
pub trait HashMapExt {
    /// An empty map
    fn new() -> Self;
    /// An empty map with room for `capacity` entries
    fn with_capacity(capacity: usize) -> Self;
}

impl<K, V> HashMapExt for HashMap<K, V> {
    fn new() -> Self {
        Self::default()
    }

    fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_hasher(capacity, FixedState::default())
    }
}

/// `new` and `with_capacity` for [`HashSet`]
pub trait HashSetExt {
    /// An empty set
    fn new() -> Self;
    /// An empty set with room for `capacity` entries
    fn with_capacity(capacity: usize) -> Self;
}

impl<T> HashSetExt for HashSet<T> {
    fn new() -> Self {
        Self::default()
    }

    fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_hasher(capacity, FixedState::default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Two maps built the same way iterate the same way, which std's
    /// randomly seeded maps do not promise
    #[test]
    fn maps_built_alike_iterate_alike() {
        let build = || {
            let mut m: HashMap<u32, u32> = HashMap::new();
            for i in 0..1000u32 {
                m.insert(i.wrapping_mul(2_654_435_761), i);
            }
            m.into_iter().collect::<Vec<_>>()
        };
        assert_eq!(build(), build());
        let build_set = || {
            let mut s: HashSet<String> = HashSet::with_capacity(16);
            for i in 0..200 {
                s.insert(format!("net{i}"));
            }
            s.into_iter().collect::<Vec<_>>()
        };
        assert_eq!(build_set(), build_set());
    }
}
