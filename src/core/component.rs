//! Component storage for entities.
//!
//! Components are typed data attached to entities. The storage uses
//! persistent data structures for cheap snapshots and structural sharing.

use im::OrdMap;

use crate::core::{EntityId, Value};
use crate::symbol::Symbol;

/// Identifies a component type.
///
/// Component types are named by symbols (e.g., "Name", "Health", "Position").
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ComponentTypeId(pub Symbol);

impl ComponentTypeId {
    /// Create a component type ID from a name.
    pub fn new(name: &str) -> Self {
        ComponentTypeId(Symbol::new(name))
    }

    /// Get the name of this component type.
    pub fn name(&self) -> String {
        self.0.as_str()
    }
}

impl std::fmt::Display for ComponentTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<&str> for ComponentTypeId {
    fn from(s: &str) -> Self {
        ComponentTypeId::new(s)
    }
}

/// Storage for all component data.
///
/// Uses persistent (immutable) data structures for efficient snapshots.
/// Structure: `ComponentTypeId -> (EntityId -> Value)`
#[derive(Debug, Clone, Default)]
pub struct ComponentStorage {
    /// Maps component type to a map of entity values
    tables: OrdMap<ComponentTypeId, OrdMap<EntityId, Value>>,
}

impl ComponentStorage {
    /// Create empty component storage.
    pub fn new() -> Self {
        Self {
            tables: OrdMap::new(),
        }
    }

    /// Set a component value for an entity.
    ///
    /// Overwrites any existing value for this component on this entity.
    pub fn set(&mut self, entity: EntityId, component: ComponentTypeId, value: Value) {
        let table = self.tables.entry(component).or_default();
        table.insert(entity, value);
    }

    /// Get a component value for an entity.
    pub fn get(&self, entity: EntityId, component: ComponentTypeId) -> Option<&Value> {
        self.tables.get(&component)?.get(&entity)
    }

    /// Remove a component from an entity.
    ///
    /// Returns the removed value, if any.
    pub fn remove(&mut self, entity: EntityId, component: ComponentTypeId) -> Option<Value> {
        let table = self.tables.get_mut(&component)?;
        table.remove(&entity)
    }

    /// Check if an entity has a specific component.
    pub fn has(&self, entity: EntityId, component: ComponentTypeId) -> bool {
        self.get(entity, component).is_some()
    }

    /// Get all components for an entity.
    ///
    /// Returns an iterator of (ComponentTypeId, &Value) pairs.
    pub fn components_of(
        &self,
        entity: EntityId,
    ) -> impl Iterator<Item = (ComponentTypeId, &Value)> {
        self.tables
            .iter()
            .filter_map(move |(comp_id, table)| table.get(&entity).map(|value| (*comp_id, value)))
    }

    /// Get all entities that have a specific component.
    ///
    /// Returns an iterator of (EntityId, &Value) pairs.
    pub fn entities_with(
        &self,
        component: ComponentTypeId,
    ) -> impl Iterator<Item = (EntityId, &Value)> {
        self.tables
            .get(&component)
            .into_iter()
            .flat_map(|table| table.iter().map(|(e, v)| (*e, v)))
    }

    /// Remove all components for an entity.
    pub fn remove_entity(&mut self, entity: EntityId) {
        // Collect keys first since we'll modify tables
        let keys: Vec<_> = self.tables.keys().cloned().collect();
        for key in keys {
            if let Some(table) = self.tables.get_mut(&key) {
                table.remove(&entity);
            }
        }
    }

    /// Get all component types currently in use.
    pub fn component_types(&self) -> impl Iterator<Item = ComponentTypeId> + '_ {
        self.tables.keys().copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::EntityAllocator;

    #[test]
    fn test_set_and_get() {
        let mut alloc = EntityAllocator::new();
        let entity = alloc.create();
        let name_comp = ComponentTypeId::new("Name");

        let mut storage = ComponentStorage::new();
        storage.set(entity, name_comp, Value::string("goblin"));

        assert_eq!(
            storage.get(entity, name_comp),
            Some(&Value::string("goblin"))
        );
    }

    #[test]
    fn test_get_missing() {
        let mut alloc = EntityAllocator::new();
        let entity = alloc.create();
        let name_comp = ComponentTypeId::new("Name");

        let storage = ComponentStorage::new();
        assert_eq!(storage.get(entity, name_comp), None);
    }

    #[test]
    fn test_overwrite() {
        let mut alloc = EntityAllocator::new();
        let entity = alloc.create();
        let hp_comp = ComponentTypeId::new("HP");

        let mut storage = ComponentStorage::new();
        storage.set(entity, hp_comp, Value::Int(100));
        storage.set(entity, hp_comp, Value::Int(50));

        assert_eq!(storage.get(entity, hp_comp), Some(&Value::Int(50)));
    }

    #[test]
    fn test_remove() {
        let mut alloc = EntityAllocator::new();
        let entity = alloc.create();
        let name_comp = ComponentTypeId::new("Name");

        let mut storage = ComponentStorage::new();
        storage.set(entity, name_comp, Value::string("orc"));

        let removed = storage.remove(entity, name_comp);
        assert_eq!(removed, Some(Value::string("orc")));
        assert_eq!(storage.get(entity, name_comp), None);
    }

    #[test]
    fn test_has() {
        let mut alloc = EntityAllocator::new();
        let entity = alloc.create();
        let name_comp = ComponentTypeId::new("Name");
        let hp_comp = ComponentTypeId::new("HP");

        let mut storage = ComponentStorage::new();
        storage.set(entity, name_comp, Value::string("troll"));

        assert!(storage.has(entity, name_comp));
        assert!(!storage.has(entity, hp_comp));
    }

    #[test]
    fn test_components_of() {
        let mut alloc = EntityAllocator::new();
        let entity = alloc.create();
        let name_comp = ComponentTypeId::new("Name");
        let hp_comp = ComponentTypeId::new("HP");

        let mut storage = ComponentStorage::new();
        storage.set(entity, name_comp, Value::string("dragon"));
        storage.set(entity, hp_comp, Value::Int(500));

        let components: Vec<_> = storage.components_of(entity).collect();
        assert_eq!(components.len(), 2);
    }

    #[test]
    fn test_entities_with() {
        let mut alloc = EntityAllocator::new();
        let e1 = alloc.create();
        let e2 = alloc.create();
        let e3 = alloc.create();
        let name_comp = ComponentTypeId::new("Name");

        let mut storage = ComponentStorage::new();
        storage.set(e1, name_comp, Value::string("a"));
        storage.set(e2, name_comp, Value::string("b"));
        // e3 has no Name component

        let named: Vec<_> = storage.entities_with(name_comp).collect();
        assert_eq!(named.len(), 2);
        assert!(named.iter().any(|(e, _)| *e == e1));
        assert!(named.iter().any(|(e, _)| *e == e2));
        assert!(!named.iter().any(|(e, _)| *e == e3));
    }
}
