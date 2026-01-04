//! World state container.
//!
//! The World is the top-level container for all game state: entities,
//! components, relations, and the current simulation tick.
//!
//! ## Layering
//!
//! The world supports a base/overlay architecture:
//! - Call `freeze()` to snapshot the current state as the immutable "base"
//! - After freezing, mutations go to the overlay
//! - Call `reset_overlay()` to discard all changes since freeze
//! - Use `is_modified()` to check if a component changed from base
//!
//! ## Transactions
//!
//! The world supports nested transactions for speculative modifications:
//! - Call `begin_transaction()` to start a new transaction
//! - Call `commit_transaction()` to merge changes into parent frame
//! - Call `rollback_transaction()` to discard changes
//! - Transactions can be nested; rollback only affects the innermost
//!
//! This enables AI planners to speculatively explore future world states.

use crate::core::{
    ComponentStorage, ComponentTypeId, EntityAllocator, EntityId, RelationRegistry, RelationSchema,
    RelationTypeId, Value,
};
use crate::derive::DerivationEngine;
use crate::direction::DirectionRegistry;
use crate::input::{self, Command, Input};

/// A frozen snapshot of world state.
///
/// Used as the immutable "base" layer after `freeze()` is called.
#[derive(Debug, Clone)]
pub struct WorldSnapshot {
    /// Entity allocator state
    pub entities: EntityAllocator,
    /// Component storage
    pub components: ComponentStorage,
    /// Relation storage
    pub relations: RelationRegistry,
    /// Simulation tick at snapshot time
    pub tick: u64,
}

/// The world state.
///
/// Supports base/overlay layering for cheap snapshots and rollback.
/// Before `freeze()` is called, operates as a simple single-layer store.
/// After `freeze()`, mutations go to the overlay and can be discarded.
///
/// Also supports nested transactions for speculative modifications.
/// Each transaction creates a savepoint that can be rolled back.
#[derive(Debug, Clone)]
pub struct World {
    /// Entity ID allocator
    entities: EntityAllocator,
    /// Component storage (current state = base + overlay)
    components: ComponentStorage,
    /// Relation storage (current state = base + overlay)
    relations: RelationRegistry,
    /// Current simulation tick
    tick: u64,
    /// Base snapshot (None until freeze() is called)
    base: Option<WorldSnapshot>,
    /// Transaction savepoint stack (for nested transactions)
    transaction_stack: Vec<WorldSnapshot>,
}

impl World {
    /// Create a new empty world.
    pub fn new() -> Self {
        Self {
            entities: EntityAllocator::new(),
            components: ComponentStorage::new(),
            relations: RelationRegistry::new(),
            tick: 0,
            base: None,
            transaction_stack: Vec::new(),
        }
    }

    // --- Layering methods ---

    /// Freeze the current state as the immutable base layer.
    ///
    /// After freezing:
    /// - The base layer cannot be modified
    /// - All mutations go to the overlay
    /// - `reset_overlay()` restores to the base state
    /// - `is_modified()` compares current vs base
    ///
    /// Returns `Err` if already frozen. Use `reset_overlay()` + `freeze()` to re-freeze.
    pub fn freeze(&mut self) -> Result<(), &'static str> {
        if self.base.is_some() {
            return Err("world is already frozen");
        }
        self.base = Some(WorldSnapshot {
            entities: self.entities.clone(),
            components: self.components.clone(),
            relations: self.relations.clone(),
            tick: self.tick,
        });
        Ok(())
    }

    /// Check if the world has a frozen base layer.
    pub fn is_frozen(&self) -> bool {
        self.base.is_some()
    }

    /// Get a reference to the base snapshot, if frozen.
    pub fn base(&self) -> Option<&WorldSnapshot> {
        self.base.as_ref()
    }

    /// Discard all changes since `freeze()`, restoring to base state.
    ///
    /// Returns `Err` if not frozen.
    pub fn reset_overlay(&mut self) -> Result<(), &'static str> {
        let base = self.base.as_ref().ok_or("world is not frozen")?;

        // Restore from base snapshot (O(1) due to structural sharing)
        self.entities = base.entities.clone();
        self.components = base.components.clone();
        self.relations = base.relations.clone();
        self.tick = base.tick;

        Ok(())
    }

    /// Unfreeze the world, merging the overlay into a single layer.
    ///
    /// After unfreezing, there is no base layer and `reset_overlay()` will fail.
    /// This is useful when you want to make current changes permanent.
    pub fn unfreeze(&mut self) {
        self.base = None;
    }

    /// Check if a component has been modified from the base value.
    ///
    /// Returns `false` if not frozen, or if the component doesn't exist in either layer.
    pub fn is_component_modified(
        &self,
        entity: EntityId,
        component: impl Into<ComponentTypeId>,
    ) -> bool {
        let Some(base) = &self.base else {
            return false;
        };
        let component = component.into();
        let current = self.components.get(entity, component);
        let original = base.components.get(entity, component);
        current != original
    }

    /// Get the base value of a component (before any overlay modifications).
    ///
    /// Returns `None` if not frozen or if the component doesn't exist in base.
    pub fn get_base_component(
        &self,
        entity: EntityId,
        component: impl Into<ComponentTypeId>,
    ) -> Option<&Value> {
        self.base
            .as_ref()
            .and_then(|b| b.components.get(entity, component.into()))
    }

    /// Check if a relation has been modified from the base state.
    ///
    /// Returns `false` if not frozen.
    pub fn is_relation_modified(
        &self,
        relation: impl Into<RelationTypeId>,
        from: EntityId,
        to: EntityId,
    ) -> bool {
        let Some(base) = &self.base else {
            return false;
        };
        let relation = relation.into();
        let current = self.relations.contains(relation, from, to);
        let original = base.relations.contains(relation, from, to);
        current != original
    }

    /// Iterate over all modified components since freeze.
    ///
    /// Returns tuples of (entity, component_type, current_value, base_value).
    /// Only includes components where current != base.
    pub fn modified_components(
        &self,
    ) -> impl Iterator<Item = (EntityId, ComponentTypeId, Option<&Value>, Option<&Value>)> {
        let base = self.base.as_ref();

        // Collect all component types from both current and base
        let mut all_types: Vec<ComponentTypeId> = self.components.component_types().collect();
        if let Some(b) = base {
            for ct in b.components.component_types() {
                if !all_types.contains(&ct) {
                    all_types.push(ct);
                }
            }
        }

        // For each entity, check each component type
        let entity_count = self
            .entity_count()
            .max(base.map(|b| b.entities.count()).unwrap_or(0));

        let mut modified = Vec::new();
        for entity_raw in 0..entity_count {
            let entity = EntityId::from_raw(entity_raw);
            for &component in &all_types {
                let current = self.components.get(entity, component);
                let original = base.and_then(|b| b.components.get(entity, component));
                if current != original {
                    modified.push((entity, component, current, original));
                }
            }
        }
        modified.into_iter()
    }

    // --- Transaction methods ---

    /// Begin a new transaction.
    ///
    /// Creates a savepoint of the current state. Any mutations made after this
    /// call can be rolled back with `rollback_transaction()` or committed with
    /// `commit_transaction()`.
    ///
    /// Transactions can be nested. Each `begin_transaction()` must be matched
    /// with either `commit_transaction()` or `rollback_transaction()`.
    ///
    /// # Example
    ///
    /// ```
    /// use hornvale::World;
    ///
    /// let mut world = World::new();
    /// let entity = world.create_entity();
    /// world.set_component(entity, "HP", 100_i64);
    ///
    /// world.begin_transaction();
    /// world.set_component(entity, "HP", 50_i64);
    /// // Oops, we don't want this change
    /// world.rollback_transaction().unwrap();
    ///
    /// // HP is back to 100
    /// assert_eq!(world.get_component(entity, "HP").unwrap().as_int(), Some(100));
    /// ```
    pub fn begin_transaction(&mut self) {
        self.transaction_stack.push(WorldSnapshot {
            entities: self.entities.clone(),
            components: self.components.clone(),
            relations: self.relations.clone(),
            tick: self.tick,
        });
    }

    /// Commit the current transaction.
    ///
    /// Drops the savepoint, making all changes since `begin_transaction()` permanent
    /// (or part of the parent transaction if nested).
    ///
    /// Returns `Err` if not in a transaction.
    pub fn commit_transaction(&mut self) -> Result<(), &'static str> {
        if self.transaction_stack.is_empty() {
            return Err("not in a transaction");
        }
        // Simply drop the savepoint - current state becomes permanent
        self.transaction_stack.pop();
        Ok(())
    }

    /// Rollback the current transaction.
    ///
    /// Discards all changes since `begin_transaction()`, restoring to the savepoint.
    ///
    /// Returns `Err` if not in a transaction.
    pub fn rollback_transaction(&mut self) -> Result<(), &'static str> {
        let savepoint = self.transaction_stack.pop().ok_or("not in a transaction")?;

        // Restore from savepoint (O(1) due to structural sharing)
        self.entities = savepoint.entities;
        self.components = savepoint.components;
        self.relations = savepoint.relations;
        self.tick = savepoint.tick;

        Ok(())
    }

    /// Check if currently in a transaction.
    pub fn in_transaction(&self) -> bool {
        !self.transaction_stack.is_empty()
    }

    /// Get the current transaction nesting depth.
    ///
    /// Returns 0 if not in a transaction.
    pub fn transaction_depth(&self) -> usize {
        self.transaction_stack.len()
    }

    /// Get a reference to the transaction savepoint at a given depth.
    ///
    /// Depth 0 is the innermost (most recent) transaction.
    /// Returns `None` if the depth is out of bounds.
    pub fn transaction_savepoint(&self, depth: usize) -> Option<&WorldSnapshot> {
        let len = self.transaction_stack.len();
        if depth >= len {
            return None;
        }
        // depth 0 = last element, depth 1 = second-to-last, etc.
        Some(&self.transaction_stack[len - 1 - depth])
    }

    // --- Snapshot methods ---

    /// Create a lightweight snapshot of the current world state.
    ///
    /// Due to structural sharing (via the `im` crate), this is an O(1) operation
    /// regardless of world size. The snapshot shares most of its internal structure
    /// with the original world, only diverging as mutations are made.
    ///
    /// # Example
    ///
    /// ```
    /// use hornvale::World;
    ///
    /// let mut world = World::new();
    /// let entity = world.create_entity();
    /// world.set_component(entity, "HP", 100_i64);
    ///
    /// // Create a snapshot
    /// let mut snapshot = world.snapshot();
    ///
    /// // Modify the original
    /// world.set_component(entity, "HP", 50_i64);
    ///
    /// // Snapshot is unchanged
    /// assert_eq!(snapshot.get_component(entity, "HP").unwrap().as_int(), Some(100));
    /// assert_eq!(world.get_component(entity, "HP").unwrap().as_int(), Some(50));
    /// ```
    pub fn snapshot(&self) -> Self {
        self.clone()
    }

    /// Create a snapshot as a WorldSnapshot struct.
    ///
    /// This is useful when you need just the data without the full World wrapper.
    pub fn to_snapshot(&self) -> WorldSnapshot {
        WorldSnapshot {
            entities: self.entities.clone(),
            components: self.components.clone(),
            relations: self.relations.clone(),
            tick: self.tick,
        }
    }

    /// Restore world state from a snapshot.
    ///
    /// This replaces the current state with the snapshot's state.
    /// Note: This does NOT affect the frozen base layer if one exists.
    pub fn restore_from_snapshot(&mut self, snapshot: WorldSnapshot) {
        self.entities = snapshot.entities;
        self.components = snapshot.components;
        self.relations = snapshot.relations;
        self.tick = snapshot.tick;
        // Note: base layer is preserved, transaction stack is cleared
        self.transaction_stack.clear();
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

    // --- High-level world queries ---

    /// Get the room an entity is in (via InRoom relation).
    pub fn get_entity_room(&self, entity: EntityId) -> Option<EntityId> {
        self.query_relation_forward("InRoom", entity)
            .first()
            .copied()
    }

    /// Get the holder of an entity (via Contains relation, reverse lookup).
    pub fn get_entity_holder(&self, entity: EntityId) -> Option<EntityId> {
        self.query_relation_reverse("Contains", entity)
            .first()
            .copied()
    }

    /// Check if an entity is held by another entity (via Contains relation).
    pub fn is_held_by(&self, item: EntityId, holder: EntityId) -> bool {
        self.query_relation_forward("Contains", holder)
            .contains(&item)
    }

    /// Check if an entity is portable.
    ///
    /// An entity is portable if:
    /// - It has `Portable=true`, OR
    /// - It does NOT have `Fixed=true` (default is portable)
    pub fn is_portable(&self, entity: EntityId) -> bool {
        // Portable if has Portable=true
        if let Some(portable) = self.get_component(entity, "Portable") {
            if let Some(b) = portable.as_bool() {
                return b;
            }
        }

        // Not fixed = portable by default
        if let Some(fixed) = self.get_component(entity, "Fixed") {
            if let Some(b) = fixed.as_bool() {
                return !b;
            }
        }

        // Default: not fixed, so portable
        true
    }

    /// Check if an entity is in scope for an actor.
    ///
    /// An entity is in scope if:
    /// - It's the same entity as the actor
    /// - It's carried by the actor (via Contains)
    /// - It's in the same room as the actor (via InRoom)
    /// - It's inside a container that's in scope (recursive)
    pub fn is_in_scope(&self, actor: EntityId, target: EntityId) -> bool {
        // Same entity is always in scope
        if actor == target {
            return true;
        }

        // Check if carried by actor
        if self.is_held_by(target, actor) {
            return true;
        }

        // Check if in same room
        let actor_room = self.get_entity_room(actor);
        let target_room = self.get_entity_room(target);

        if let (Some(ar), Some(tr)) = (actor_room, target_room) {
            if ar == tr {
                return true;
            }
        }

        // Check if target is in a container that's in scope
        let containers = self.query_relation_reverse("Contains", target);
        for container in containers {
            if self.is_in_scope(actor, container) {
                // TODO: check if container is open
                return true;
            }
        }

        false
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

    // --- Derivation integration methods ---

    /// Get a component value, checking derived components first.
    ///
    /// If the component is marked as derived in the engine, this will
    /// compute and return the derived value. Otherwise, it returns the
    /// stored component value.
    ///
    /// Returns `Ok(None)` if the component doesn't exist.
    /// Returns `Err` if a circular dependency is detected.
    pub fn get_component_derived(
        &self,
        entity: EntityId,
        component: impl Into<ComponentTypeId>,
        engine: &DerivationEngine,
    ) -> Result<Option<Value>, crate::derive::DerivationError> {
        let component = component.into();

        // Check if this is a derived component
        if engine.is_derived(component) {
            // Derive the value
            engine.derive(self, entity, component)
        } else {
            // Return stored value (cloned to match derive signature)
            Ok(self.components.get(entity, component).cloned())
        }
    }

    /// Set a component value and notify the derivation engine.
    ///
    /// This sets the component and invalidates any cached derived values
    /// that depend on this component.
    pub fn set_component_notify(
        &mut self,
        entity: EntityId,
        component: impl Into<ComponentTypeId>,
        value: impl Into<Value>,
        engine: &DerivationEngine,
    ) {
        let component = component.into();
        self.components.set(entity, component, value.into());
        engine.notify_component_changed(entity, component);
    }

    /// Add a relation and notify the derivation engine.
    ///
    /// This adds the relation and invalidates any cached derived values
    /// that depend on this relation.
    pub fn add_relation_notify(
        &mut self,
        relation: impl Into<RelationTypeId>,
        from: EntityId,
        to: EntityId,
        engine: &DerivationEngine,
    ) -> bool {
        let relation = relation.into();
        let result = self.relations.insert(relation, from, to);
        if result {
            engine.notify_relation_changed(from, relation);
        }
        result
    }

    /// Remove a relation and notify the derivation engine.
    ///
    /// This removes the relation and invalidates any cached derived values
    /// that depend on this relation.
    pub fn remove_relation_notify(
        &mut self,
        relation: impl Into<RelationTypeId>,
        from: EntityId,
        to: EntityId,
        engine: &DerivationEngine,
    ) -> bool {
        let relation = relation.into();
        let result = self.relations.remove(relation, from, to);
        if result {
            engine.notify_relation_changed(from, relation);
        }
        result
    }

    // --- Input injection methods ---

    /// Inject raw input text into the world.
    ///
    /// Creates a new entity with Input, InputSource, and InputTick components.
    /// Returns the entity ID for the input event.
    ///
    /// # Example
    ///
    /// ```
    /// use hornvale::World;
    /// use hornvale::core::EntityId;
    ///
    /// let mut world = World::new();
    /// let player = world.create_entity();
    /// let input_entity = world.inject_input("go north", player);
    /// ```
    pub fn inject_input(&mut self, text: &str, source: EntityId) -> EntityId {
        let input = Input::new(text, self.tick).with_source(source);
        self.inject_input_event(input)
    }

    /// Inject a pre-built Input event into the world.
    ///
    /// Creates a new entity with the appropriate input components.
    pub fn inject_input_event(&mut self, input: Input) -> EntityId {
        let entity = self.create_entity();

        // Store raw text
        self.set_component(
            entity,
            input::components::input(),
            Value::string(input.text.as_ref()),
        );

        // Store tick
        self.set_component(
            entity,
            input::components::input_tick(),
            Value::Int(input.tick as i64),
        );

        // Store source if present
        if let Some(source) = input.source {
            self.set_component(
                entity,
                input::components::input_source(),
                Value::EntityRef(source),
            );
        }

        // Tokenize and store tokens
        let tokens = input.tokenize();
        let token_values: Vec<Value> = tokens
            .iter()
            .map(|t| Value::string(t.text.as_ref()))
            .collect();
        self.set_component(
            entity,
            input::components::tokens(),
            Value::list(token_values),
        );

        entity
    }

    /// Parse input on an entity and store the command.
    ///
    /// Returns the parsed command if successful.
    pub fn parse_input_entity(
        &mut self,
        entity: EntityId,
        directions: &DirectionRegistry,
    ) -> Option<Command> {
        // Get the raw input text
        let text = self.get_component(entity, input::components::input())?;
        let text = text.as_str()?;

        // Parse it
        let input = Input::new(text, 0);
        let command = input::parse_input(&input, directions)?;

        // Store the command
        self.set_component(entity, input::components::command(), command.to_value());

        Some(command)
    }

    /// Get all pending input entities (entities with Input but no Command).
    pub fn pending_inputs(&self) -> Vec<EntityId> {
        self.entities_with(input::components::input())
            .filter(|(e, _)| !self.has_component(*e, input::components::command()))
            .map(|(e, _)| e)
            .collect()
    }

    /// Process all pending inputs, parsing them into commands.
    ///
    /// Returns the number of inputs processed.
    pub fn process_inputs(&mut self, directions: &DirectionRegistry) -> usize {
        let pending: Vec<EntityId> = self.pending_inputs();
        let mut count = 0;

        for entity in pending {
            if self.parse_input_entity(entity, directions).is_some() {
                count += 1;
            }
        }

        count
    }

    // === Hook helpers ===

    /// Get a hook component for an entity.
    ///
    /// Hook components are named `{phase}:{action}`, e.g., "On:burn", "Before:take".
    ///
    /// # Arguments
    /// * `entity` - The entity to check
    /// * `phase` - The hook phase ("Before", "On", or "After")
    /// * `action` - The action name (e.g., "burn", "take")
    ///
    /// # Returns
    /// The hook expression value if the entity has a hook for this phase/action.
    pub fn get_hook(&self, entity: EntityId, phase: &str, action: &str) -> Option<&Value> {
        let component_name = format!("{phase}:{action}");
        self.get_component(entity, ComponentTypeId::new(&component_name))
    }

    /// Get all hook components for an entity matching a specific action.
    ///
    /// Returns (before, on, after) hooks for the action.
    pub fn get_hooks_for_action(
        &self,
        entity: EntityId,
        action: &str,
    ) -> (Option<&Value>, Option<&Value>, Option<&Value>) {
        (
            self.get_hook(entity, "Before", action),
            self.get_hook(entity, "On", action),
            self.get_hook(entity, "After", action),
        )
    }

    /// Check if an entity has any hook for a given action.
    pub fn has_hook(&self, entity: EntityId, phase: &str, action: &str) -> bool {
        self.get_hook(entity, phase, action).is_some()
    }

    /// Find all hook components on an entity for a given phase.
    ///
    /// Returns an iterator of (action_name, hook_value) pairs.
    pub fn hooks_for_phase<'a>(
        &'a self,
        entity: EntityId,
        phase: &'a str,
    ) -> impl Iterator<Item = (String, &'a Value)> {
        let prefix = format!("{phase}:");
        let prefix_for_strip = prefix.clone();
        self.components
            .components_matching(entity, move |name| name.starts_with(&prefix))
            .map(move |(comp_id, value)| {
                let name = comp_id.name();
                let action = name
                    .strip_prefix(&prefix_for_strip)
                    .unwrap_or(&name)
                    .to_string();
                (action, value)
            })
    }

    /// Delete an entity and all its components and relations.
    pub fn delete_entity(&mut self, entity: EntityId) {
        // Remove all components
        self.components.remove_entity(entity);

        // Remove from all relations (both directions)
        for rel_type in self.relation_types().collect::<Vec<_>>() {
            // Remove relations where this entity is the "from"
            let targets: Vec<_> = self.query_relation_forward(rel_type, entity);
            for target in targets {
                self.remove_relation(rel_type, entity, target);
            }
            // Remove relations where this entity is the "to"
            let sources: Vec<_> = self.query_relation_reverse(rel_type, entity);
            for source in sources {
                self.remove_relation(rel_type, source, entity);
            }
        }

        // Note: EntityAllocator doesn't currently support deletion,
        // but the entity is now orphaned (no components or relations).
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

    #[test]
    fn test_get_component_derived_stored() {
        use crate::derive::DerivationEngine;

        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "Name", "test");

        let engine = DerivationEngine::new();

        // "Name" is not derived, so should return stored value
        let result = world
            .get_component_derived(entity, "Name", &engine)
            .unwrap();
        assert_eq!(result, Some(Value::string("test")));
    }

    #[test]
    fn test_get_component_derived_computed() {
        use crate::derive::{DerivationEngine, DerivationRule, DerivedProperty};
        use crate::rules::Pattern;
        use std::sync::Arc;

        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "BaseHP", 100_i64);

        let mut engine = DerivationEngine::new();
        engine.add_rule(DerivationRule::new(
            "effective-hp",
            Pattern::has_component("?e", "BaseHP"),
            DerivedProperty::new("EffectiveHP"),
            Arc::new(|w, e| {
                let hp = w
                    .get_component(e, "BaseHP")
                    .and_then(|v| v.as_int())
                    .unwrap_or(0);
                Value::Int(hp * 2)
            }),
        ));

        // "EffectiveHP" is derived
        let result = world
            .get_component_derived(entity, "EffectiveHP", &engine)
            .unwrap();
        assert_eq!(result, Some(Value::Int(200)));
    }

    #[test]
    fn test_set_component_notify() {
        use crate::derive::{Dependency, DerivationEngine, DerivationRule, DerivedProperty};
        use crate::rules::Pattern;
        use std::sync::Arc;

        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        let mut engine = DerivationEngine::new();
        engine.add_rule(DerivationRule::new(
            "derived-hp",
            Pattern::has_component("?e", "HP"),
            DerivedProperty::new("DerivedHP"),
            Arc::new(|w, e| {
                let hp = w
                    .get_component(e, "HP")
                    .and_then(|v| v.as_int())
                    .unwrap_or(0);
                Value::Int(hp * 2)
            }),
        ));

        // Derive with dependencies
        let deps = vec![Dependency::component(entity, "HP")];
        engine
            .derive_with_dependencies(&world, entity, "DerivedHP", deps)
            .unwrap();

        // Cache should have one entry
        assert_eq!(engine.cache_stats(), (1, 1));

        // Update HP with notification
        world.set_component_notify(entity, "HP", 150_i64, &engine);

        // Cache should be invalidated
        assert_eq!(engine.cache_stats(), (1, 0));
    }

    #[test]
    fn test_inject_input() {
        let mut world = World::new();
        let player = world.create_entity();

        let input_entity = world.inject_input("go north", player);

        // Check input text was stored
        let text = world
            .get_component(input_entity, input::components::input())
            .unwrap();
        assert_eq!(text.as_str(), Some("go north"));

        // Check source was stored
        let source = world
            .get_component(input_entity, input::components::input_source())
            .unwrap();
        assert_eq!(source.as_entity_ref(), Some(player));

        // Check tick was stored
        let tick = world
            .get_component(input_entity, input::components::input_tick())
            .unwrap();
        assert_eq!(tick.as_int(), Some(0));

        // Check tokens were stored
        let tokens = world
            .get_component(input_entity, input::components::tokens())
            .unwrap();
        let token_list = tokens.as_list().unwrap();
        assert_eq!(token_list.len(), 2);
    }

    #[test]
    fn test_parse_input_entity() {
        let mut world = World::new();
        let player = world.create_entity();
        let directions = DirectionRegistry::with_standard_directions();

        let input_entity = world.inject_input("go north", player);

        // Parse the input
        let command = world.parse_input_entity(input_entity, &directions).unwrap();

        assert_eq!(command.verb.as_str(), "go");
        assert!(command.direct_object.is_some());
    }

    #[test]
    fn test_pending_inputs() {
        let mut world = World::new();
        let player = world.create_entity();
        let directions = DirectionRegistry::with_standard_directions();

        // Inject two inputs
        let input1 = world.inject_input("look", player);
        let input2 = world.inject_input("go north", player);

        // Both should be pending
        let pending = world.pending_inputs();
        assert_eq!(pending.len(), 2);

        // Parse one
        world.parse_input_entity(input1, &directions);

        // Only one should be pending now
        let pending = world.pending_inputs();
        assert_eq!(pending.len(), 1);
        assert_eq!(pending[0], input2);
    }

    #[test]
    fn test_process_inputs() {
        let mut world = World::new();
        let player = world.create_entity();
        let directions = DirectionRegistry::with_standard_directions();

        // Inject three inputs
        world.inject_input("look", player);
        world.inject_input("go north", player);
        world.inject_input("take lamp", player);

        // Process all
        let count = world.process_inputs(&directions);
        assert_eq!(count, 3);

        // None should be pending
        let pending = world.pending_inputs();
        assert_eq!(pending.len(), 0);
    }

    // --- Layering tests ---

    #[test]
    fn test_freeze_basic() {
        let mut world = World::new();
        assert!(!world.is_frozen());

        world.freeze().unwrap();
        assert!(world.is_frozen());

        // Can't freeze twice
        assert!(world.freeze().is_err());
    }

    #[test]
    fn test_freeze_and_reset() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "Name", "goblin");
        world.set_component(entity, "HP", 100_i64);

        // Freeze the world
        world.freeze().unwrap();

        // Modify after freeze
        world.set_component(entity, "HP", 50_i64);
        world.set_component(entity, "Name", "orc");

        // Verify modifications
        assert_eq!(world.get_component(entity, "HP"), Some(&Value::Int(50)));
        assert_eq!(
            world.get_component(entity, "Name"),
            Some(&Value::string("orc"))
        );

        // Reset to base
        world.reset_overlay().unwrap();

        // Should be back to original values
        assert_eq!(world.get_component(entity, "HP"), Some(&Value::Int(100)));
        assert_eq!(
            world.get_component(entity, "Name"),
            Some(&Value::string("goblin"))
        );
    }

    #[test]
    fn test_reset_unfrozen_fails() {
        let mut world = World::new();
        assert!(world.reset_overlay().is_err());
    }

    #[test]
    fn test_is_component_modified() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        // Not frozen yet - nothing is "modified"
        assert!(!world.is_component_modified(entity, "HP"));

        world.freeze().unwrap();

        // Still not modified (no changes since freeze)
        assert!(!world.is_component_modified(entity, "HP"));

        // Modify it
        world.set_component(entity, "HP", 50_i64);
        assert!(world.is_component_modified(entity, "HP"));

        // Reset and check again
        world.reset_overlay().unwrap();
        assert!(!world.is_component_modified(entity, "HP"));
    }

    #[test]
    fn test_get_base_component() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        // No base yet
        assert!(world.get_base_component(entity, "HP").is_none());

        world.freeze().unwrap();

        // Now we can get base
        assert_eq!(
            world.get_base_component(entity, "HP"),
            Some(&Value::Int(100))
        );

        // Modify current
        world.set_component(entity, "HP", 50_i64);

        // Base should still be 100
        assert_eq!(
            world.get_base_component(entity, "HP"),
            Some(&Value::Int(100))
        );
        // Current should be 50
        assert_eq!(world.get_component(entity, "HP"), Some(&Value::Int(50)));
    }

    #[test]
    fn test_is_relation_modified() {
        use crate::core::{Cardinality, RelationSchema};

        let mut world = World::new();
        world.register_relation(RelationSchema::new(
            "InRoom",
            Cardinality::Many,
            Cardinality::One,
        ));

        let room = world.create_entity();
        let goat = world.create_entity();
        let kitchen = world.create_entity();

        world.add_relation("InRoom", goat, room);

        world.freeze().unwrap();

        // Not modified yet
        assert!(!world.is_relation_modified("InRoom", goat, room));

        // Move goat to kitchen
        world.remove_relation("InRoom", goat, room);
        world.add_relation("InRoom", goat, kitchen);

        // Both should be modified
        assert!(world.is_relation_modified("InRoom", goat, room)); // was true, now false
        assert!(world.is_relation_modified("InRoom", goat, kitchen)); // was false, now true
    }

    #[test]
    fn test_modified_components() {
        let mut world = World::new();
        let e1 = world.create_entity();
        let e2 = world.create_entity();

        world.set_component(e1, "HP", 100_i64);
        world.set_component(e2, "HP", 200_i64);

        world.freeze().unwrap();

        // Modify e1's HP and add a new component to e2
        world.set_component(e1, "HP", 50_i64);
        world.set_component(e2, "Name", "goblin");

        let modified: Vec<_> = world.modified_components().collect();
        assert_eq!(modified.len(), 2);

        // Check that both modifications are detected
        let e1_hp_mod = modified
            .iter()
            .find(|(e, c, _, _)| *e == e1 && c.name() == "HP");
        assert!(e1_hp_mod.is_some());
        let (_, _, current, base) = e1_hp_mod.unwrap();
        assert_eq!(*current, Some(&Value::Int(50)));
        assert_eq!(*base, Some(&Value::Int(100)));

        let e2_name_mod = modified
            .iter()
            .find(|(e, c, _, _)| *e == e2 && c.name() == "Name");
        assert!(e2_name_mod.is_some());
        let (_, _, current, base) = e2_name_mod.unwrap();
        assert_eq!(*current, Some(&Value::string("goblin")));
        assert_eq!(*base, None);
    }

    #[test]
    fn test_unfreeze() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        world.freeze().unwrap();
        world.set_component(entity, "HP", 50_i64);

        // Unfreeze - current state becomes the only state
        world.unfreeze();
        assert!(!world.is_frozen());

        // Can't reset anymore
        assert!(world.reset_overlay().is_err());

        // HP should still be 50
        assert_eq!(world.get_component(entity, "HP"), Some(&Value::Int(50)));

        // Can freeze again
        world.freeze().unwrap();
        assert!(world.is_frozen());
    }

    #[test]
    fn test_freeze_preserves_tick() {
        let mut world = World::new();
        world.advance_ticks(100);

        world.freeze().unwrap();
        world.advance_ticks(50);

        assert_eq!(world.tick(), 150);

        world.reset_overlay().unwrap();

        assert_eq!(world.tick(), 100);
    }

    #[test]
    fn test_freeze_preserves_entities() {
        let mut world = World::new();
        world.create_entity();
        world.create_entity();
        world.create_entity();

        world.freeze().unwrap();

        // Create more entities
        world.create_entity();
        world.create_entity();

        assert_eq!(world.entity_count(), 5);

        world.reset_overlay().unwrap();

        // Should be back to 3
        assert_eq!(world.entity_count(), 3);
    }

    #[test]
    fn test_base_snapshot_access() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);
        world.advance_ticks(42);

        world.freeze().unwrap();

        let base = world.base().unwrap();
        assert_eq!(base.tick, 42);
        assert_eq!(base.entities.count(), 1);
        assert_eq!(
            base.components.get(entity, "HP".into()),
            Some(&Value::Int(100))
        );
    }

    // --- Transaction tests ---

    #[test]
    fn test_transaction_basic() {
        let mut world = World::new();
        assert!(!world.in_transaction());
        assert_eq!(world.transaction_depth(), 0);

        world.begin_transaction();
        assert!(world.in_transaction());
        assert_eq!(world.transaction_depth(), 1);

        world.commit_transaction().unwrap();
        assert!(!world.in_transaction());
        assert_eq!(world.transaction_depth(), 0);
    }

    #[test]
    fn test_transaction_rollback() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        world.begin_transaction();
        world.set_component(entity, "HP", 50_i64);
        assert_eq!(world.get_component(entity, "HP"), Some(&Value::Int(50)));

        world.rollback_transaction().unwrap();
        assert_eq!(world.get_component(entity, "HP"), Some(&Value::Int(100)));
    }

    #[test]
    fn test_transaction_commit() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        world.begin_transaction();
        world.set_component(entity, "HP", 50_i64);
        world.commit_transaction().unwrap();

        // Change persists
        assert_eq!(world.get_component(entity, "HP"), Some(&Value::Int(50)));
    }

    #[test]
    fn test_transaction_errors() {
        let mut world = World::new();

        // Can't commit without transaction
        assert!(world.commit_transaction().is_err());

        // Can't rollback without transaction
        assert!(world.rollback_transaction().is_err());
    }

    #[test]
    fn test_nested_transactions() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        // Outer transaction
        world.begin_transaction();
        world.set_component(entity, "HP", 80_i64);
        assert_eq!(world.transaction_depth(), 1);

        // Inner transaction
        world.begin_transaction();
        world.set_component(entity, "HP", 60_i64);
        assert_eq!(world.transaction_depth(), 2);

        // Rollback inner only
        world.rollback_transaction().unwrap();
        assert_eq!(world.transaction_depth(), 1);
        assert_eq!(world.get_component(entity, "HP"), Some(&Value::Int(80)));

        // Commit outer
        world.commit_transaction().unwrap();
        assert_eq!(world.transaction_depth(), 0);
        assert_eq!(world.get_component(entity, "HP"), Some(&Value::Int(80)));
    }

    #[test]
    fn test_nested_transactions_full_rollback() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        world.begin_transaction();
        world.set_component(entity, "HP", 80_i64);

        world.begin_transaction();
        world.set_component(entity, "HP", 60_i64);

        world.begin_transaction();
        world.set_component(entity, "HP", 40_i64);

        assert_eq!(world.transaction_depth(), 3);

        // Rollback all
        world.rollback_transaction().unwrap();
        assert_eq!(world.get_component(entity, "HP"), Some(&Value::Int(60)));

        world.rollback_transaction().unwrap();
        assert_eq!(world.get_component(entity, "HP"), Some(&Value::Int(80)));

        world.rollback_transaction().unwrap();
        assert_eq!(world.get_component(entity, "HP"), Some(&Value::Int(100)));

        assert_eq!(world.transaction_depth(), 0);
    }

    #[test]
    fn test_transaction_preserves_tick() {
        let mut world = World::new();
        world.advance_ticks(100);

        world.begin_transaction();
        world.advance_ticks(50);
        assert_eq!(world.tick(), 150);

        world.rollback_transaction().unwrap();
        assert_eq!(world.tick(), 100);
    }

    #[test]
    fn test_transaction_preserves_entities() {
        let mut world = World::new();
        let _e1 = world.create_entity();
        let _e2 = world.create_entity();
        assert_eq!(world.entity_count(), 2);

        world.begin_transaction();
        world.create_entity();
        world.create_entity();
        assert_eq!(world.entity_count(), 4);

        world.rollback_transaction().unwrap();
        assert_eq!(world.entity_count(), 2);
    }

    #[test]
    fn test_transaction_preserves_relations() {
        use crate::core::{Cardinality, RelationSchema};

        let mut world = World::new();
        world.register_relation(RelationSchema::new(
            "InRoom",
            Cardinality::Many,
            Cardinality::One,
        ));

        let room = world.create_entity();
        let goat = world.create_entity();
        let kitchen = world.create_entity();

        world.add_relation("InRoom", goat, room);

        world.begin_transaction();
        world.remove_relation("InRoom", goat, room);
        world.add_relation("InRoom", goat, kitchen);

        assert!(world.has_relation("InRoom", goat, kitchen));
        assert!(!world.has_relation("InRoom", goat, room));

        world.rollback_transaction().unwrap();

        assert!(world.has_relation("InRoom", goat, room));
        assert!(!world.has_relation("InRoom", goat, kitchen));
    }

    #[test]
    fn test_transaction_savepoint_access() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        world.begin_transaction();
        world.set_component(entity, "HP", 80_i64);

        world.begin_transaction();
        world.set_component(entity, "HP", 60_i64);

        // depth 0 = innermost (HP was 80 when we started)
        let inner = world.transaction_savepoint(0).unwrap();
        assert_eq!(
            inner.components.get(entity, "HP".into()),
            Some(&Value::Int(80))
        );

        // depth 1 = outer (HP was 100 when we started)
        let outer = world.transaction_savepoint(1).unwrap();
        assert_eq!(
            outer.components.get(entity, "HP".into()),
            Some(&Value::Int(100))
        );

        // depth 2 = out of bounds
        assert!(world.transaction_savepoint(2).is_none());
    }

    #[test]
    fn test_transaction_with_freeze() {
        // Transactions should work with frozen worlds
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        world.freeze().unwrap();

        // Transaction after freeze
        world.begin_transaction();
        world.set_component(entity, "HP", 50_i64);
        world.rollback_transaction().unwrap();

        // Should be back to 100
        assert_eq!(world.get_component(entity, "HP"), Some(&Value::Int(100)));

        // But base should still be 100 too
        assert_eq!(
            world.get_base_component(entity, "HP"),
            Some(&Value::Int(100))
        );
    }

    // --- Snapshot tests ---

    #[test]
    fn test_snapshot_basic() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        let snapshot = world.snapshot();

        // Modify original
        world.set_component(entity, "HP", 50_i64);

        // Snapshot is unchanged
        assert_eq!(snapshot.get_component(entity, "HP"), Some(&Value::Int(100)));
        assert_eq!(world.get_component(entity, "HP"), Some(&Value::Int(50)));
    }

    #[test]
    fn test_snapshot_independent_mutation() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        let mut snapshot = world.snapshot();

        // Modify both independently
        world.set_component(entity, "HP", 50_i64);
        snapshot.set_component(entity, "HP", 75_i64);

        // Each has its own value
        assert_eq!(world.get_component(entity, "HP"), Some(&Value::Int(50)));
        assert_eq!(snapshot.get_component(entity, "HP"), Some(&Value::Int(75)));
    }

    #[test]
    fn test_snapshot_preserves_entities() {
        let mut world = World::new();
        world.create_entity();
        world.create_entity();
        world.create_entity();

        let snapshot = world.snapshot();

        // Create more in original
        world.create_entity();
        world.create_entity();

        assert_eq!(world.entity_count(), 5);
        assert_eq!(snapshot.entity_count(), 3);
    }

    #[test]
    fn test_snapshot_preserves_relations() {
        use crate::core::{Cardinality, RelationSchema};

        let mut world = World::new();
        world.register_relation(RelationSchema::new(
            "InRoom",
            Cardinality::Many,
            Cardinality::One,
        ));

        let room = world.create_entity();
        let goat = world.create_entity();
        let kitchen = world.create_entity();

        world.add_relation("InRoom", goat, room);

        let snapshot = world.snapshot();

        // Move goat in original
        world.remove_relation("InRoom", goat, room);
        world.add_relation("InRoom", goat, kitchen);

        // Snapshot has old state
        assert!(snapshot.has_relation("InRoom", goat, room));
        assert!(!snapshot.has_relation("InRoom", goat, kitchen));

        // Original has new state
        assert!(!world.has_relation("InRoom", goat, room));
        assert!(world.has_relation("InRoom", goat, kitchen));
    }

    #[test]
    fn test_to_snapshot() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);
        world.advance_ticks(42);

        let snapshot = world.to_snapshot();

        assert_eq!(snapshot.tick, 42);
        assert_eq!(snapshot.entities.count(), 1);
        assert_eq!(
            snapshot.components.get(entity, "HP".into()),
            Some(&Value::Int(100))
        );
    }

    #[test]
    fn test_restore_from_snapshot() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);
        world.advance_ticks(42);

        let saved = world.to_snapshot();

        // Modify world
        world.set_component(entity, "HP", 50_i64);
        world.advance_ticks(10);
        world.create_entity();

        assert_eq!(world.get_component(entity, "HP"), Some(&Value::Int(50)));
        assert_eq!(world.tick(), 52);
        assert_eq!(world.entity_count(), 2);

        // Restore
        world.restore_from_snapshot(saved);

        assert_eq!(world.get_component(entity, "HP"), Some(&Value::Int(100)));
        assert_eq!(world.tick(), 42);
        assert_eq!(world.entity_count(), 1);
    }

    #[test]
    fn test_restore_clears_transactions() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        let saved = world.to_snapshot();

        world.begin_transaction();
        world.begin_transaction();
        assert_eq!(world.transaction_depth(), 2);

        world.restore_from_snapshot(saved);

        // Transaction stack should be cleared
        assert_eq!(world.transaction_depth(), 0);
        assert!(!world.in_transaction());
    }

    #[test]
    fn test_restore_preserves_base() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        world.freeze().unwrap();

        // Modify
        world.set_component(entity, "HP", 50_i64);

        // Save current state
        let saved = world.to_snapshot();

        // Modify more
        world.set_component(entity, "HP", 25_i64);

        // Restore
        world.restore_from_snapshot(saved);

        // Current is restored to saved
        assert_eq!(world.get_component(entity, "HP"), Some(&Value::Int(50)));

        // Base is preserved
        assert!(world.is_frozen());
        assert_eq!(
            world.get_base_component(entity, "HP"),
            Some(&Value::Int(100))
        );
    }
}
