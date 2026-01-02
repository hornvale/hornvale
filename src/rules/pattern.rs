//! Pattern matching for rules.
//!
//! Patterns describe conditions that must be true for a rule to fire.
//! They are interpreted (not compiled) for simplicity.

use crate::EntityId;
use crate::core::{ComponentTypeId, RelationTypeId, Value, World};
use crate::symbol::Symbol;

/// A pattern variable (e.g., `?e` in Lisp syntax).
///
/// Variables are used to bind entities matched by patterns.
/// For Phase 2, we support single-entity patterns where the variable
/// represents the entity being tested.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var(pub Symbol);

impl Var {
    /// Create a new pattern variable.
    ///
    /// The name can optionally include a leading `?` which will be stripped.
    /// Both `Var::new("e")` and `Var::new("?e")` create the same variable.
    pub fn new(name: &str) -> Self {
        let name = name.strip_prefix('?').unwrap_or(name);
        Var(Symbol::new(name))
    }

    /// Get the variable name (without the `?` prefix).
    pub fn name(&self) -> String {
        self.0.as_str()
    }
}

impl From<&str> for Var {
    fn from(s: &str) -> Self {
        Var::new(s)
    }
}

impl std::fmt::Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "?{}", self.0)
    }
}

/// A pattern that matches entities against world state.
///
/// Patterns are evaluated against a single entity to determine if it matches.
/// The `Var` in each pattern variant represents the entity being tested.
#[derive(Debug, Clone)]
pub enum Pattern {
    /// Matches any entity.
    /// `(entity ?e)` - always true
    Entity(Var),

    /// Matches entities that have a specific component.
    /// `(has ?e Name)` - true if entity has a Name component
    HasComponent(Var, ComponentTypeId),

    /// Matches entities with a component having a specific value.
    /// `(= (get ?e Name) "goat")` - true if Name component equals "goat"
    ComponentValue(Var, ComponentTypeId, Value),

    /// Matches entities that have a forward relation of a given type.
    /// `(located-in ?e ?_)` - true if entity has a Location relation
    InRelation(RelationTypeId, Var, Var),

    /// All sub-patterns must match.
    And(Vec<Pattern>),

    /// At least one sub-pattern must match.
    Or(Vec<Pattern>),

    /// Inverts the match result.
    Not(Box<Pattern>),
}

impl Pattern {
    // --- Convenience constructors ---

    /// Create an Entity pattern.
    pub fn entity(var: impl Into<Var>) -> Self {
        Pattern::Entity(var.into())
    }

    /// Create a HasComponent pattern.
    pub fn has_component(var: impl Into<Var>, component: impl Into<ComponentTypeId>) -> Self {
        Pattern::HasComponent(var.into(), component.into())
    }

    /// Create a ComponentValue pattern.
    pub fn component_value(
        var: impl Into<Var>,
        component: impl Into<ComponentTypeId>,
        value: impl Into<Value>,
    ) -> Self {
        Pattern::ComponentValue(var.into(), component.into(), value.into())
    }

    /// Create an InRelation pattern.
    pub fn in_relation(
        relation: impl Into<RelationTypeId>,
        from_var: impl Into<Var>,
        to_var: impl Into<Var>,
    ) -> Self {
        Pattern::InRelation(relation.into(), from_var.into(), to_var.into())
    }

    /// Create an And pattern.
    pub fn and(patterns: Vec<Pattern>) -> Self {
        Pattern::And(patterns)
    }

    /// Create an Or pattern.
    pub fn or(patterns: Vec<Pattern>) -> Self {
        Pattern::Or(patterns)
    }

    /// Create a Not pattern (negation).
    pub fn negated(pattern: Pattern) -> Self {
        Pattern::Not(Box::new(pattern))
    }

    // --- Matching ---

    /// Test if an entity matches this pattern.
    ///
    /// For Phase 2, all variables are assumed to bind to the entity being tested.
    /// Variable unification is a future enhancement.
    pub fn matches(&self, world: &World, entity: EntityId) -> bool {
        match self {
            Pattern::Entity(_) => {
                // Any entity matches
                true
            }

            Pattern::HasComponent(_, component) => {
                // Check if entity has the component
                world.has_component(entity, *component)
            }

            Pattern::ComponentValue(_, component, expected) => {
                // Check if component exists and has the expected value
                world
                    .get_component(entity, *component)
                    .is_some_and(|actual| actual == expected)
            }

            Pattern::InRelation(relation, _, _) => {
                // Check if entity has any forward relation of this type
                world.has_any_relation(*relation, entity)
            }

            Pattern::And(patterns) => {
                // All sub-patterns must match
                patterns.iter().all(|p| p.matches(world, entity))
            }

            Pattern::Or(patterns) => {
                // At least one sub-pattern must match
                patterns.iter().any(|p| p.matches(world, entity))
            }

            Pattern::Not(pattern) => {
                // Invert the result
                !pattern.matches(world, entity)
            }
        }
    }

    /// Get a human-readable description of this pattern.
    pub fn describe(&self) -> String {
        match self {
            Pattern::Entity(var) => format!("(entity {var})"),
            Pattern::HasComponent(var, comp) => format!("(has {var} {comp})"),
            Pattern::ComponentValue(var, comp, val) => {
                format!("(= (get {var} {comp}) {val})")
            }
            Pattern::InRelation(rel, from, to) => format!("({rel} {from} {to})"),
            Pattern::And(patterns) => {
                let inner: Vec<_> = patterns.iter().map(|p| p.describe()).collect();
                let inner_str = inner.join(" ");
                format!("(and {inner_str})")
            }
            Pattern::Or(patterns) => {
                let inner: Vec<_> = patterns.iter().map(|p| p.describe()).collect();
                let inner_str = inner.join(" ");
                format!("(or {inner_str})")
            }
            Pattern::Not(pattern) => {
                let inner = pattern.describe();
                format!("(not {inner})")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::{Cardinality, RelationSchema};

    fn setup_world() -> World {
        let mut world = World::new();

        // Register Location relation (many-to-one)
        world.register_relation(RelationSchema::new(
            "Location",
            Cardinality::Many,
            Cardinality::One,
        ));

        // Create a room
        let room = world.create_entity();
        world.set_component(room, "Name", "A small room");

        // Create a goat in the room
        let goat = world.create_entity();
        world.set_component(goat, "Name", "goat");
        world.set_component(goat, "HP", 10_i64);
        world.add_relation("Location", goat, room);

        // Create a chest (no location)
        let chest = world.create_entity();
        world.set_component(chest, "Name", "chest");

        world
    }

    #[test]
    fn test_entity_pattern_matches_any() {
        let world = setup_world();
        let pattern = Pattern::entity("?e");

        // All entities match
        for entity in world.all_entities() {
            assert!(pattern.matches(&world, entity));
        }
    }

    #[test]
    fn test_has_component_pattern() {
        let world = setup_world();
        let has_hp = Pattern::has_component("?e", "HP");

        // Only goat (entity 1) has HP
        let room = EntityId::from_raw(0);
        let goat = EntityId::from_raw(1);
        let chest = EntityId::from_raw(2);

        assert!(!has_hp.matches(&world, room));
        assert!(has_hp.matches(&world, goat));
        assert!(!has_hp.matches(&world, chest));
    }

    #[test]
    fn test_component_value_pattern() {
        let world = setup_world();
        let is_goat = Pattern::component_value("?e", "Name", "goat");

        let room = EntityId::from_raw(0);
        let goat = EntityId::from_raw(1);
        let chest = EntityId::from_raw(2);

        assert!(!is_goat.matches(&world, room));
        assert!(is_goat.matches(&world, goat));
        assert!(!is_goat.matches(&world, chest));
    }

    #[test]
    fn test_in_relation_pattern() {
        let world = setup_world();
        let has_location = Pattern::in_relation("Location", "?e", "?_");

        let room = EntityId::from_raw(0);
        let goat = EntityId::from_raw(1);
        let chest = EntityId::from_raw(2);

        // Room has no location (it is a location)
        assert!(!has_location.matches(&world, room));
        // Goat has a location
        assert!(has_location.matches(&world, goat));
        // Chest has no location
        assert!(!has_location.matches(&world, chest));
    }

    #[test]
    fn test_and_pattern() {
        let world = setup_world();

        // Match entities with both Name="goat" AND HP component
        let pattern = Pattern::and(vec![
            Pattern::component_value("?e", "Name", "goat"),
            Pattern::has_component("?e", "HP"),
        ]);

        let room = EntityId::from_raw(0);
        let goat = EntityId::from_raw(1);
        let chest = EntityId::from_raw(2);

        assert!(!pattern.matches(&world, room));
        assert!(pattern.matches(&world, goat));
        assert!(!pattern.matches(&world, chest));
    }

    #[test]
    fn test_or_pattern() {
        let world = setup_world();

        // Match entities with Name="goat" OR Name="chest"
        let pattern = Pattern::or(vec![
            Pattern::component_value("?e", "Name", "goat"),
            Pattern::component_value("?e", "Name", "chest"),
        ]);

        let room = EntityId::from_raw(0);
        let goat = EntityId::from_raw(1);
        let chest = EntityId::from_raw(2);

        assert!(!pattern.matches(&world, room));
        assert!(pattern.matches(&world, goat));
        assert!(pattern.matches(&world, chest));
    }

    #[test]
    fn test_not_pattern() {
        let world = setup_world();

        // Match entities that are NOT goats
        let pattern = Pattern::negated(Pattern::component_value("?e", "Name", "goat"));

        let room = EntityId::from_raw(0);
        let goat = EntityId::from_raw(1);
        let chest = EntityId::from_raw(2);

        assert!(pattern.matches(&world, room));
        assert!(!pattern.matches(&world, goat));
        assert!(pattern.matches(&world, chest));
    }

    #[test]
    fn test_complex_pattern() {
        let world = setup_world();

        // Match: entity named "goat" that has a location
        let pattern = Pattern::and(vec![
            Pattern::component_value("?e", "Name", "goat"),
            Pattern::in_relation("Location", "?e", "?_"),
        ]);

        let goat = EntityId::from_raw(1);
        assert!(pattern.matches(&world, goat));

        // Verify the pattern description
        let desc = pattern.describe();
        assert!(desc.contains("and"));
        assert!(desc.contains("goat"));
        assert!(desc.contains("Location"));
    }

    #[test]
    fn test_pattern_describe() {
        let pattern = Pattern::and(vec![
            Pattern::entity("?e"),
            Pattern::has_component("?e", "Name"),
            Pattern::component_value("?e", "Name", "goat"),
        ]);

        let desc = pattern.describe();
        assert!(desc.starts_with("(and"));
        assert!(desc.contains("(entity ?e)"));
        assert!(desc.contains("(has ?e Name)"));
    }
}
