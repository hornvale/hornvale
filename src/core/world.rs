//! World state container.
//!
//! The World is the top-level container for all game state: entities,
//! components, relations, and the current simulation tick.

use crate::core::{
    ComponentStorage, ComponentTypeId, EntityAllocator, EntityId, RelationRegistry, RelationSchema,
    RelationTypeId, Value,
};

/// The world state.
///
/// For Phase 1, this is a simple single-layer implementation.
/// Later phases will add base/overlay split.
#[derive(Debug, Clone)]
pub struct World {
    /// Entity ID allocator
    entities: EntityAllocator,
    /// Component storage
    components: ComponentStorage,
    /// Relation storage
    relations: RelationRegistry,
    /// Current simulation tick
    tick: u64,
}

impl World {
    /// Create a new empty world.
    pub fn new() -> Self {
        Self {
            entities: EntityAllocator::new(),
            components: ComponentStorage::new(),
            relations: RelationRegistry::new(),
            tick: 0,
        }
    }

    /// Create a new entity and return its ID.
    pub fn create_entity(&mut self) -> EntityId {
        self.entities.create()
    }

    /// Get the number of entities that have been created.
    pub fn entity_count(&self) -> u64 {
        self.entities.count()
    }

    /// Set a component value for an entity.
    pub fn set_component(
        &mut self,
        entity: EntityId,
        component: impl Into<ComponentTypeId>,
        value: impl Into<Value>,
    ) {
        self.components.set(entity, component.into(), value.into());
    }

    /// Get a component value for an entity.
    pub fn get_component(
        &self,
        entity: EntityId,
        component: impl Into<ComponentTypeId>,
    ) -> Option<&Value> {
        self.components.get(entity, component.into())
    }

    /// Check if an entity has a specific component.
    pub fn has_component(&self, entity: EntityId, component: impl Into<ComponentTypeId>) -> bool {
        self.components.has(entity, component.into())
    }

    /// Get all components for an entity.
    pub fn components_of(
        &self,
        entity: EntityId,
    ) -> impl Iterator<Item = (ComponentTypeId, &Value)> {
        self.components.components_of(entity)
    }

    /// Get all entities with a specific component.
    pub fn entities_with(
        &self,
        component: impl Into<ComponentTypeId>,
    ) -> impl Iterator<Item = (EntityId, &Value)> {
        self.components.entities_with(component.into())
    }

    /// Get the current simulation tick.
    pub fn tick(&self) -> u64 {
        self.tick
    }

    /// Advance the simulation by one tick.
    pub fn advance_tick(&mut self) {
        self.tick += 1;
    }

    /// Advance the simulation by multiple ticks.
    pub fn advance_ticks(&mut self, count: u64) {
        self.tick += count;
    }

    // --- Relation methods ---

    /// Register a relation type with the world.
    pub fn register_relation(&mut self, schema: RelationSchema) {
        self.relations.register(schema);
    }

    /// Add a relation between two entities.
    ///
    /// Returns false if the relation type doesn't exist.
    pub fn add_relation(
        &mut self,
        relation: impl Into<RelationTypeId>,
        from: EntityId,
        to: EntityId,
    ) -> bool {
        self.relations.insert(relation.into(), from, to)
    }

    /// Remove a relation between two entities.
    ///
    /// Returns false if the relation type doesn't exist.
    pub fn remove_relation(
        &mut self,
        relation: impl Into<RelationTypeId>,
        from: EntityId,
        to: EntityId,
    ) -> bool {
        self.relations.remove(relation.into(), from, to)
    }

    /// Query forward: given 'from', what 'to' entities are related?
    pub fn query_relation_forward(
        &self,
        relation: impl Into<RelationTypeId>,
        from: EntityId,
    ) -> Vec<EntityId> {
        self.relations.query_forward(relation.into(), from)
    }

    /// Query reverse: given 'to', what 'from' entities are related?
    pub fn query_relation_reverse(
        &self,
        relation: impl Into<RelationTypeId>,
        to: EntityId,
    ) -> Vec<EntityId> {
        self.relations.query_reverse(relation.into(), to)
    }

    /// Check if a specific relation exists.
    pub fn has_relation(
        &self,
        relation: impl Into<RelationTypeId>,
        from: EntityId,
        to: EntityId,
    ) -> bool {
        self.relations.contains(relation.into(), from, to)
    }

    /// Check if an entity has any forward relations of a given type.
    pub fn has_any_relation(&self, relation: impl Into<RelationTypeId>, from: EntityId) -> bool {
        self.relations.has_forward(relation.into(), from)
    }

    /// Get all registered relation types.
    pub fn relation_types(&self) -> impl Iterator<Item = RelationTypeId> + '_ {
        self.relations.relation_types()
    }

    /// Get the schema for a relation type.
    pub fn relation_schema(&self, relation: impl Into<RelationTypeId>) -> Option<&RelationSchema> {
        self.relations.schema(relation.into())
    }

    /// Iterate over all entity IDs that have been created.
    ///
    /// Note: This iterates from 0 to entity_count, which includes
    /// all IDs ever allocated. In Phase 1 we don't support entity
    /// deletion, so all IDs are valid.
    pub fn all_entities(&self) -> impl Iterator<Item = EntityId> {
        let count = self.entity_count();
        (0..count).map(EntityId::from_raw)
    }
}

impl Default for World {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_entity() {
        let mut world = World::new();
        let e1 = world.create_entity();
        let e2 = world.create_entity();

        assert_ne!(e1, e2);
        assert_eq!(world.entity_count(), 2);
    }

    #[test]
    fn test_set_and_get_component() {
        let mut world = World::new();
        let entity = world.create_entity();

        world.set_component(entity, "Name", "goblin");

        assert_eq!(
            world.get_component(entity, "Name"),
            Some(&Value::string("goblin"))
        );
    }

    #[test]
    fn test_tick() {
        let mut world = World::new();
        assert_eq!(world.tick(), 0);

        world.advance_tick();
        assert_eq!(world.tick(), 1);

        world.advance_ticks(10);
        assert_eq!(world.tick(), 11);
    }

    #[test]
    fn test_entities_with() {
        let mut world = World::new();
        let e1 = world.create_entity();
        let e2 = world.create_entity();
        let _e3 = world.create_entity();

        world.set_component(e1, "Name", "alice");
        world.set_component(e2, "Name", "bob");

        let named: Vec<_> = world.entities_with("Name").collect();
        assert_eq!(named.len(), 2);
    }

    #[test]
    fn test_all_entities() {
        let mut world = World::new();
        world.create_entity();
        world.create_entity();
        world.create_entity();

        let all: Vec<_> = world.all_entities().collect();
        assert_eq!(all.len(), 3);
    }
}
