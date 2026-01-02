//! Caching for derived values.
//!
//! The cache stores computed derived values along with their dependencies,
//! enabling lazy invalidation when underlying data changes.

use im::{OrdMap, OrdSet};

use crate::core::{ComponentTypeId, EntityId, RelationTypeId, Value};

/// A dependency that a derived value depends on.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Dependency {
    /// Depends on an entity's component value.
    Component(EntityId, ComponentTypeId),
    /// Depends on an entity's relations of a given type.
    Relation(EntityId, RelationTypeId),
}

impl Dependency {
    /// Create a component dependency.
    pub fn component(entity: EntityId, component: impl Into<ComponentTypeId>) -> Self {
        Dependency::Component(entity, component.into())
    }

    /// Create a relation dependency.
    pub fn relation(entity: EntityId, relation: impl Into<RelationTypeId>) -> Self {
        Dependency::Relation(entity, relation.into())
    }
}

/// A cached derived value with its dependencies.
#[derive(Debug, Clone)]
pub struct CacheEntry {
    /// The cached value.
    pub value: Value,
    /// What this value depends on.
    pub dependencies: Vec<Dependency>,
    /// The tick when this was computed.
    pub computed_at_tick: u64,
    /// Whether this entry is still valid.
    pub valid: bool,
}

impl CacheEntry {
    /// Create a new valid cache entry.
    pub fn new(value: Value, dependencies: Vec<Dependency>, tick: u64) -> Self {
        CacheEntry {
            value,
            dependencies,
            computed_at_tick: tick,
            valid: true,
        }
    }

    /// Mark this entry as invalid.
    pub fn invalidate(&mut self) {
        self.valid = false;
    }
}

/// Cache for derived property values.
///
/// The cache uses lazy invalidation: when a dependency changes, we mark
/// dependent entries as invalid rather than recomputing immediately.
/// Invalid entries are recomputed on the next access.
pub struct DerivedCache {
    /// Cached values: (entity, property) -> CacheEntry
    entries: OrdMap<(EntityId, ComponentTypeId), CacheEntry>,
    /// Reverse index: dependency -> entries that depend on it
    dependency_index: OrdMap<Dependency, OrdSet<(EntityId, ComponentTypeId)>>,
}

impl DerivedCache {
    /// Create a new empty cache.
    pub fn new() -> Self {
        DerivedCache {
            entries: OrdMap::new(),
            dependency_index: OrdMap::new(),
        }
    }

    /// Get a cached entry if it exists and is valid.
    pub fn get(
        &self,
        entity: EntityId,
        property: impl Into<ComponentTypeId>,
    ) -> Option<&CacheEntry> {
        let key = (entity, property.into());
        self.entries.get(&key).filter(|e| e.valid)
    }

    /// Get a cached entry regardless of validity.
    pub fn get_any(
        &self,
        entity: EntityId,
        property: impl Into<ComponentTypeId>,
    ) -> Option<&CacheEntry> {
        let key = (entity, property.into());
        self.entries.get(&key)
    }

    /// Insert a new cache entry.
    pub fn insert(
        &mut self,
        entity: EntityId,
        property: impl Into<ComponentTypeId>,
        entry: CacheEntry,
    ) {
        let property = property.into();
        let key = (entity, property);

        // Remove old dependency index entries if this key existed
        if let Some(old_entry) = self.entries.get(&key) {
            for dep in &old_entry.dependencies {
                if let Some(dependents) = self.dependency_index.get_mut(dep) {
                    dependents.remove(&key);
                }
            }
        }

        // Add new dependency index entries
        for dep in &entry.dependencies {
            let dependents = self
                .dependency_index
                .get(&dep.clone())
                .cloned()
                .unwrap_or_default();
            let mut new_dependents = dependents;
            new_dependents.insert(key);
            self.dependency_index.insert(dep.clone(), new_dependents);
        }

        // Store the entry
        self.entries.insert(key, entry);
    }

    /// Invalidate all entries that depend on a component.
    pub fn invalidate_for_component(
        &mut self,
        entity: EntityId,
        component: impl Into<ComponentTypeId>,
    ) {
        let dep = Dependency::Component(entity, component.into());
        self.invalidate_for_dependency(&dep);
    }

    /// Invalidate all entries that depend on a relation.
    pub fn invalidate_for_relation(
        &mut self,
        entity: EntityId,
        relation: impl Into<RelationTypeId>,
    ) {
        let dep = Dependency::Relation(entity, relation.into());
        self.invalidate_for_dependency(&dep);
    }

    /// Invalidate all entries that depend on a specific dependency.
    fn invalidate_for_dependency(&mut self, dep: &Dependency) {
        if let Some(dependents) = self.dependency_index.get(dep) {
            for key in dependents.iter() {
                if let Some(entry) = self.entries.get_mut(key) {
                    entry.invalidate();
                }
            }
        }
    }

    /// Remove a specific entry from the cache.
    pub fn remove(&mut self, entity: EntityId, property: impl Into<ComponentTypeId>) {
        let key = (entity, property.into());

        if let Some(entry) = self.entries.remove(&key) {
            // Clean up dependency index
            for dep in &entry.dependencies {
                if let Some(dependents) = self.dependency_index.get_mut(dep) {
                    dependents.remove(&key);
                }
            }
        }
    }

    /// Clear all cached entries.
    pub fn clear(&mut self) {
        self.entries.clear();
        self.dependency_index.clear();
    }

    /// Get the number of cached entries.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Check if the cache is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Get the number of valid entries.
    pub fn valid_count(&self) -> usize {
        self.entries.values().filter(|e| e.valid).count()
    }
}

impl Default for DerivedCache {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for DerivedCache {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DerivedCache")
            .field("entries", &self.entries.len())
            .field("valid", &self.valid_count())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cache_empty() {
        let cache = DerivedCache::new();
        assert!(cache.is_empty());
        assert_eq!(cache.len(), 0);
    }

    #[test]
    fn test_cache_insert_and_get() {
        let mut cache = DerivedCache::new();
        let entity = EntityId::from_raw(1);

        let entry = CacheEntry::new(Value::Int(42), vec![], 0);
        cache.insert(entity, "TestProp", entry);

        assert_eq!(cache.len(), 1);
        let cached = cache.get(entity, "TestProp").unwrap();
        assert_eq!(cached.value, Value::Int(42));
        assert!(cached.valid);
    }

    #[test]
    fn test_cache_get_nonexistent() {
        let cache = DerivedCache::new();
        let entity = EntityId::from_raw(1);

        assert!(cache.get(entity, "TestProp").is_none());
    }

    #[test]
    fn test_cache_invalidation() {
        let mut cache = DerivedCache::new();
        let entity = EntityId::from_raw(1);
        let dep_entity = EntityId::from_raw(2);

        // Insert entry that depends on dep_entity's HP component
        let entry = CacheEntry::new(
            Value::Int(100),
            vec![Dependency::component(dep_entity, "HP")],
            0,
        );
        cache.insert(entity, "DerivedHP", entry);

        // Entry should be valid
        assert!(cache.get(entity, "DerivedHP").is_some());

        // Invalidate by changing HP
        cache.invalidate_for_component(dep_entity, "HP");

        // Entry should now be invalid (get returns None)
        assert!(cache.get(entity, "DerivedHP").is_none());

        // But get_any should still return it
        let entry = cache.get_any(entity, "DerivedHP").unwrap();
        assert!(!entry.valid);
    }

    #[test]
    fn test_cache_invalidation_relation() {
        let mut cache = DerivedCache::new();
        let entity = EntityId::from_raw(1);

        // Insert entry that depends on entity's Location relation
        let entry = CacheEntry::new(
            Value::Float(0.5),
            vec![Dependency::relation(entity, "Location")],
            0,
        );
        cache.insert(entity, "BiomeBonus", entry);

        assert!(cache.get(entity, "BiomeBonus").is_some());

        // Invalidate by changing Location
        cache.invalidate_for_relation(entity, "Location");

        assert!(cache.get(entity, "BiomeBonus").is_none());
    }

    #[test]
    fn test_cache_unrelated_invalidation() {
        let mut cache = DerivedCache::new();
        let entity1 = EntityId::from_raw(1);
        let entity2 = EntityId::from_raw(2);

        // Insert entry for entity1 depending on its HP
        let entry = CacheEntry::new(
            Value::Int(100),
            vec![Dependency::component(entity1, "HP")],
            0,
        );
        cache.insert(entity1, "DerivedHP", entry);

        // Invalidate entity2's HP (unrelated)
        cache.invalidate_for_component(entity2, "HP");

        // Entity1's entry should still be valid
        assert!(cache.get(entity1, "DerivedHP").is_some());
    }

    #[test]
    fn test_cache_multiple_dependencies() {
        let mut cache = DerivedCache::new();
        let entity = EntityId::from_raw(1);
        let dep1 = EntityId::from_raw(2);
        let dep2 = EntityId::from_raw(3);

        // Entry depends on both dep1.HP and dep2.Strength
        let entry = CacheEntry::new(
            Value::Int(150),
            vec![
                Dependency::component(dep1, "HP"),
                Dependency::component(dep2, "Strength"),
            ],
            0,
        );
        cache.insert(entity, "CombinedStat", entry);

        assert!(cache.get(entity, "CombinedStat").is_some());

        // Invalidating either dependency should invalidate the entry
        cache.invalidate_for_component(dep2, "Strength");
        assert!(cache.get(entity, "CombinedStat").is_none());
    }

    #[test]
    fn test_cache_overwrite() {
        let mut cache = DerivedCache::new();
        let entity = EntityId::from_raw(1);
        let dep = EntityId::from_raw(2);

        // Insert with one dependency
        let entry1 = CacheEntry::new(Value::Int(100), vec![Dependency::component(dep, "HP")], 0);
        cache.insert(entity, "Stat", entry1);

        // Overwrite with different dependency
        let entry2 = CacheEntry::new(
            Value::Int(200),
            vec![Dependency::component(dep, "Strength")],
            1,
        );
        cache.insert(entity, "Stat", entry2);

        // Check the new value is there
        let cached = cache.get(entity, "Stat").unwrap();
        assert_eq!(cached.value, Value::Int(200));

        // Old dependency (HP) should not invalidate
        cache.invalidate_for_component(dep, "HP");
        assert!(cache.get(entity, "Stat").is_some());

        // New dependency (Strength) should invalidate
        cache.invalidate_for_component(dep, "Strength");
        assert!(cache.get(entity, "Stat").is_none());
    }

    #[test]
    fn test_cache_remove() {
        let mut cache = DerivedCache::new();
        let entity = EntityId::from_raw(1);

        let entry = CacheEntry::new(Value::Int(42), vec![], 0);
        cache.insert(entity, "Test", entry);

        assert_eq!(cache.len(), 1);

        cache.remove(entity, "Test");

        assert_eq!(cache.len(), 0);
        assert!(cache.get(entity, "Test").is_none());
    }

    #[test]
    fn test_cache_clear() {
        let mut cache = DerivedCache::new();

        for i in 0..5 {
            let entity = EntityId::from_raw(i);
            let entry = CacheEntry::new(Value::Int(i as i64), vec![], 0);
            cache.insert(entity, "Test", entry);
        }

        assert_eq!(cache.len(), 5);

        cache.clear();

        assert!(cache.is_empty());
    }

    #[test]
    fn test_cache_valid_count() {
        let mut cache = DerivedCache::new();
        let entity1 = EntityId::from_raw(1);
        let entity2 = EntityId::from_raw(2);

        cache.insert(
            entity1,
            "A",
            CacheEntry::new(Value::Int(1), vec![Dependency::component(entity1, "X")], 0),
        );
        cache.insert(
            entity2,
            "B",
            CacheEntry::new(Value::Int(2), vec![Dependency::component(entity2, "Y")], 0),
        );

        assert_eq!(cache.len(), 2);
        assert_eq!(cache.valid_count(), 2);

        // Invalidate one
        cache.invalidate_for_component(entity1, "X");

        assert_eq!(cache.len(), 2);
        assert_eq!(cache.valid_count(), 1);
    }
}
