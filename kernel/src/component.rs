//! The entity-space component store: a typed table of component `C` keyed by
//! an identity `K`. The generalization of [`crate::CellMap`] from a fixed
//! `CellId` key to any ordered key — the storage substrate an ECS component
//! registry is built on. Deterministic ascending-by-key iteration (the
//! no-`HashMap` rule's dividend); `BTreeMap`-backed. The dense-`Vec` backend
//! for dense keys and the permutation indexes are the query engine's work
//! (metaplan §4.5, campaign 4), not here.
#![warn(missing_docs)]

use std::collections::BTreeMap;

/// A typed store of component `C` keyed by identity `K`.
#[derive(Clone, Debug, PartialEq)]
pub struct ComponentStore<K: Ord, C> {
    map: BTreeMap<K, C>,
}

impl<K: Ord, C> ComponentStore<K, C> {
    /// An empty store.
    pub fn new() -> Self {
        Self {
            map: BTreeMap::new(),
        }
    }
    /// The component for `k`, if present.
    pub fn get(&self, k: &K) -> Option<&C> {
        self.map.get(k)
    }
    /// Insert `c` under `k`, returning any previous value.
    pub fn insert(&mut self, k: K, c: C) -> Option<C> {
        self.map.insert(k, c)
    }
    /// Whether `k` has a component.
    /// type-audit: bare-ok(flag)
    pub fn contains(&self, k: &K) -> bool {
        self.map.contains_key(k)
    }
    /// `(key, component)` pairs in ascending key order.
    pub fn iter(&self) -> impl Iterator<Item = (&K, &C)> {
        self.map.iter()
    }
    /// The keys, ascending.
    pub fn ids(&self) -> impl Iterator<Item = &K> {
        self.map.keys()
    }
    /// The number of components.
    /// type-audit: bare-ok(count)
    pub fn len(&self) -> usize {
        self.map.len()
    }
    /// Whether the store is empty.
    /// type-audit: bare-ok(flag)
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}

impl<C> ComponentStore<crate::KindId, C> {
    /// The component of the kind whose label equals `label`, if present.
    /// Runtime labels (e.g. from ledger text) cannot construct a `KindId`
    /// key, so this compares label content; rosters are small.
    /// type-audit: bare-ok(identifier-text: label)
    pub fn get_by_label(&self, label: &str) -> Option<&C> {
        self.iter().find(|(k, _)| k.0 == label).map(|(_, c)| c)
    }
}

impl<K: Ord, C> Default for ComponentStore<K, C> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Ord, C> FromIterator<(K, C)> for ComponentStore<K, C> {
    fn from_iter<I: IntoIterator<Item = (K, C)>>(iter: I) -> Self {
        Self {
            map: iter.into_iter().collect(),
        }
    }
}

/// Open marker for a component type. **Not sealed** — sealing would break the
/// constitutional open-extension rule (any domain may declare a component).
/// Invariant safety comes from each component's validating constructor, not
/// from restricting who may implement this trait.
pub trait Component {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stores_and_retrieves_by_key_in_ascending_order() {
        let s: ComponentStore<u32, &str> = [(3, "c"), (1, "a"), (2, "b")].into_iter().collect();
        assert_eq!(s.len(), 3);
        assert_eq!(s.get(&2), Some(&"b"));
        assert_eq!(s.get(&9), None);
        assert!(s.contains(&1));
        // deterministic ascending-by-key iteration (the no-HashMap dividend)
        let order: Vec<u32> = s.ids().copied().collect();
        assert_eq!(order, vec![1, 2, 3]);
    }
}
