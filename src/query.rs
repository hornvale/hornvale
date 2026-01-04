//! Unified query API for both Rust and DSL access.
//!
//! This module provides a consistent interface for querying:
//! - Entities by component or predicate
//! - Graph traversals (descendants, ancestors)
//! - Rules by trigger type
//!
//! The same API is available from Rust code and from DSL expressions.

use crate::core::{ComponentTypeId, EntityId, RelationTypeId, Value, World};
use crate::rules::RuleSet;
use crate::symbol::Symbol;

/// Query for finding entities.
#[derive(Debug, Clone)]
pub enum EntityQuery {
    /// All entities in the world.
    All,
    /// Entities with a specific component.
    WithComponent(Symbol),
    /// Entities with a component having a specific value.
    WithComponentValue(Symbol, Value),
    /// Entities matching a predicate (evaluated per-entity).
    /// The predicate is a compiled PredicatePattern entity.
    WithPredicate(EntityId),
}

/// Query for graph traversal.
#[derive(Debug, Clone)]
pub struct TraversalQuery {
    /// Starting entity.
    pub start: EntityId,
    /// Relation to follow.
    pub relation: RelationTypeId,
    /// Maximum depth (0 = unlimited for practical purposes, use high value).
    pub max_depth: usize,
    /// Direction of traversal.
    pub direction: TraversalDirection,
}

/// Direction of graph traversal.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TraversalDirection {
    /// Follow relations forward (descendants).
    Forward,
    /// Follow relations backward (ancestors).
    Backward,
}

/// Query for finding rules.
#[derive(Debug, Clone)]
pub enum RuleQuery {
    /// All rules.
    All,
    /// Periodic rules.
    Periodic,
    /// Hook rules for a specific action (all phases).
    HooksForAction(Symbol),
    /// Before hooks for a specific action.
    BeforeHooks(Symbol),
    /// On hooks for a specific action.
    OnHooks(Symbol),
    /// After hooks for a specific action.
    AfterHooks(Symbol),
    /// Derivation rules for a specific property.
    Derive(Symbol),
    /// Precondition rules by name.
    Precondition(Symbol),
}

/// Execute an entity query against the world.
pub fn query_entities(world: &World, query: &EntityQuery) -> Vec<EntityId> {
    match query {
        EntityQuery::All => world.all_entities().collect(),

        EntityQuery::WithComponent(component) => {
            let comp_id = ComponentTypeId(*component);
            world
                .all_entities()
                .filter(|&e| world.has_component(e, comp_id))
                .collect()
        }

        EntityQuery::WithComponentValue(component, expected) => {
            let comp_id = ComponentTypeId(*component);
            world
                .all_entities()
                .filter(|&e| world.get_component(e, comp_id) == Some(expected))
                .collect()
        }

        EntityQuery::WithPredicate(_predicate_entity) => {
            // TODO: Evaluate predicate pattern against each entity
            // For now, return empty - this requires VM integration
            Vec::new()
        }
    }
}

/// Execute a traversal query against the world.
pub fn query_traversal(world: &World, query: &TraversalQuery) -> Vec<EntityId> {
    let result = match query.direction {
        TraversalDirection::Forward => {
            world.descendants_all(query.start, query.relation, query.max_depth)
        }
        TraversalDirection::Backward => {
            world.ancestors(query.start, query.relation, query.max_depth)
        }
    };
    result.entities
}

/// Execute a rule query against the rule set.
pub fn query_rules<'a>(rules: &'a mut RuleSet, query: &RuleQuery) -> Vec<&'a crate::rules::Rule> {
    match query {
        RuleQuery::All => rules.rules().collect(),
        RuleQuery::Periodic => rules.periodic_rules(),
        RuleQuery::HooksForAction(action) => rules.hooks_for_action(&action.as_str()),
        RuleQuery::BeforeHooks(action) => rules.before_hooks(&action.as_str()),
        RuleQuery::OnHooks(action) => rules.on_hooks(&action.as_str()),
        RuleQuery::AfterHooks(action) => rules.after_hooks(&action.as_str()),
        RuleQuery::Derive(property) => rules.derive_rules(&property.as_str()),
        RuleQuery::Precondition(name) => rules.precondition_rules(&name.as_str()),
    }
}

/// Query builder for fluent API.
#[derive(Debug, Clone)]
pub struct QueryBuilder {
    query: EntityQuery,
}

impl QueryBuilder {
    /// Start a query for all entities.
    pub fn all() -> Self {
        Self {
            query: EntityQuery::All,
        }
    }

    /// Query entities with a specific component.
    pub fn with_component(component: impl Into<Symbol>) -> Self {
        Self {
            query: EntityQuery::WithComponent(component.into()),
        }
    }

    /// Query entities with a component having a specific value.
    pub fn with_component_value(component: impl Into<Symbol>, value: impl Into<Value>) -> Self {
        Self {
            query: EntityQuery::WithComponentValue(component.into(), value.into()),
        }
    }

    /// Execute the query.
    pub fn execute(&self, world: &World) -> Vec<EntityId> {
        query_entities(world, &self.query)
    }
}

/// Convenience functions on World for query access.
impl World {
    /// Query all entities with a specific component.
    pub fn entities_with_component(&self, component: impl Into<Symbol>) -> Vec<EntityId> {
        QueryBuilder::with_component(component).execute(self)
    }

    /// Query all entities with a component having a specific value.
    pub fn entities_with_component_value(
        &self,
        component: impl Into<Symbol>,
        value: impl Into<Value>,
    ) -> Vec<EntityId> {
        QueryBuilder::with_component_value(component, value).execute(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::{Cardinality, RelationSchema};

    fn setup_world() -> World {
        let mut world = World::new();

        // Create some entities with components
        let player = world.create_entity();
        world.set_component(player, "Name", "player");
        world.set_component(player, "HP", 100_i64);
        world.set_component(player, "Creature", true);

        let goblin = world.create_entity();
        world.set_component(goblin, "Name", "goblin");
        world.set_component(goblin, "HP", 20_i64);
        world.set_component(goblin, "Creature", true);

        let sword = world.create_entity();
        world.set_component(sword, "Name", "sword");
        world.set_component(sword, "Portable", true);

        let room = world.create_entity();
        world.set_component(room, "Name", "room");
        world.set_component(room, "Room", true);

        world
    }

    #[test]
    fn test_query_all_entities() {
        let world = setup_world();

        let all = query_entities(&world, &EntityQuery::All);
        assert_eq!(all.len(), 4);
    }

    #[test]
    fn test_query_with_component() {
        let world = setup_world();

        let creatures =
            query_entities(&world, &EntityQuery::WithComponent(Symbol::new("Creature")));
        assert_eq!(creatures.len(), 2);

        let rooms = query_entities(&world, &EntityQuery::WithComponent(Symbol::new("Room")));
        assert_eq!(rooms.len(), 1);

        let portables =
            query_entities(&world, &EntityQuery::WithComponent(Symbol::new("Portable")));
        assert_eq!(portables.len(), 1);
    }

    #[test]
    fn test_query_with_component_value() {
        let world = setup_world();

        let player = query_entities(
            &world,
            &EntityQuery::WithComponentValue(Symbol::new("Name"), Value::string("player")),
        );
        assert_eq!(player.len(), 1);
    }

    #[test]
    fn test_query_builder() {
        let world = setup_world();

        let creatures = QueryBuilder::with_component("Creature").execute(&world);
        assert_eq!(creatures.len(), 2);

        let player = QueryBuilder::with_component_value("Name", "player").execute(&world);
        assert_eq!(player.len(), 1);
    }

    #[test]
    fn test_world_convenience_methods() {
        let world = setup_world();

        let creatures = world.entities_with_component("Creature");
        assert_eq!(creatures.len(), 2);

        let player = world.entities_with_component_value("Name", "player");
        assert_eq!(player.len(), 1);
    }

    #[test]
    fn test_traversal_query() {
        let mut world = World::new();

        // Register Contains relation
        world.register_relation(RelationSchema::new(
            "Contains",
            Cardinality::One,
            Cardinality::Many,
        ));

        // Create structure: room Contains chest Contains key
        let room = world.create_entity();
        let chest = world.create_entity();
        let key = world.create_entity();

        world.add_relation("Contains", room, chest);
        world.add_relation("Contains", chest, key);

        // Query descendants of room
        let descendants = query_traversal(
            &world,
            &TraversalQuery {
                start: room,
                relation: "Contains".into(),
                max_depth: 10,
                direction: TraversalDirection::Forward,
            },
        );
        assert_eq!(descendants.len(), 2); // chest, key

        // Query ancestors of key
        let ancestors = query_traversal(
            &world,
            &TraversalQuery {
                start: key,
                relation: "Contains".into(),
                max_depth: 10,
                direction: TraversalDirection::Backward,
            },
        );
        assert_eq!(ancestors.len(), 2); // chest, room
    }
}
