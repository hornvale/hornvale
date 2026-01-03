//! Reference resolution for commands.
//!
//! This module resolves noun phrases in commands to entity references
//! by querying the world state.

use crate::core::{ComponentTypeId, EntityId, World};
use crate::symbol::Symbol;
use std::sync::Arc;

use super::{Command, ObjectRef};

/// Well-known component types for entity naming.
pub mod naming {
    use super::*;

    /// Primary name of an entity (e.g., "lamp").
    pub fn name() -> ComponentTypeId {
        ComponentTypeId::new("Name")
    }

    /// Alternative names/aliases (stored as list of strings).
    pub fn aliases() -> ComponentTypeId {
        ComponentTypeId::new("Aliases")
    }

    /// Adjectives that describe the entity (stored as list of strings).
    pub fn adjectives() -> ComponentTypeId {
        ComponentTypeId::new("Adjectives")
    }
}

/// Well-known relation types for scoping.
pub mod relations {
    use crate::core::RelationTypeId;

    /// Entity is located in a room/container.
    pub fn in_room() -> RelationTypeId {
        RelationTypeId::new("InRoom")
    }

    /// Entity is contained inside another.
    pub fn contains() -> RelationTypeId {
        RelationTypeId::new("Contains")
    }

    /// Entity is being carried by another.
    pub fn carried_by() -> RelationTypeId {
        RelationTypeId::new("CarriedBy")
    }
}

/// A candidate entity that might match a noun phrase.
#[derive(Debug, Clone)]
pub struct EntityCandidate {
    /// The entity ID.
    pub entity: EntityId,
    /// How well this entity matches (higher = better).
    pub score: i32,
    /// The name used for display.
    pub name: Arc<str>,
    /// Whether this match used adjectives.
    pub used_adjectives: bool,
}

impl EntityCandidate {
    /// Create a new candidate.
    pub fn new(entity: EntityId, score: i32, name: impl Into<Arc<str>>) -> Self {
        Self {
            entity,
            score,
            name: name.into(),
            used_adjectives: false,
        }
    }

    /// Mark that adjectives were used in matching.
    pub fn with_adjectives(mut self) -> Self {
        self.used_adjectives = true;
        self
    }
}

/// Result of resolving a noun phrase.
#[derive(Debug, Clone)]
pub enum ResolutionResult {
    /// Resolved to a single entity.
    Resolved(EntityId),
    /// Multiple entities match - ambiguous.
    Ambiguous(Vec<EntityCandidate>),
    /// No entities match.
    NotFound(Arc<str>),
    /// Resolution not needed (direction, pronoun, etc.).
    NotApplicable,
}

/// A command with resolved entity references.
#[derive(Debug, Clone)]
pub struct ResolvedCommand {
    /// The verb (action) symbol.
    pub verb: Symbol,
    /// Resolved direct object (if any).
    pub direct_object: Option<ResolvedObject>,
    /// Resolved indirect object (if any).
    pub indirect_object: Option<ResolvedObject>,
    /// Additional arguments with resolved objects.
    pub arguments: Vec<(Symbol, ResolvedObject)>,
}

/// A resolved object reference.
#[derive(Debug, Clone)]
pub enum ResolvedObject {
    /// Resolved to an entity.
    Entity(EntityId),
    /// Direction (north, south, etc.).
    Direction(Symbol),
    /// Resolution failed.
    Failed(ResolutionResult),
}

impl ResolvedObject {
    /// Get the entity if resolved.
    pub fn as_entity(&self) -> Option<EntityId> {
        match self {
            ResolvedObject::Entity(id) => Some(*id),
            _ => None,
        }
    }

    /// Get the direction if applicable.
    pub fn as_direction(&self) -> Option<Symbol> {
        match self {
            ResolvedObject::Direction(sym) => Some(*sym),
            _ => None,
        }
    }

    /// Check if resolution failed.
    pub fn is_failed(&self) -> bool {
        matches!(self, ResolvedObject::Failed(_))
    }
}

/// Trait for providing entities in scope for resolution.
pub trait ScopeProvider {
    /// Get all entities in scope for the given actor.
    fn entities_in_scope(&self, world: &World, actor: EntityId) -> Vec<EntityId>;
}

/// Default scope provider that finds entities:
/// - In the same room as the actor
/// - Carried by the actor
/// - Inside open containers in scope
#[derive(Debug, Clone, Default)]
pub struct DefaultScope;

impl ScopeProvider for DefaultScope {
    fn entities_in_scope(&self, world: &World, actor: EntityId) -> Vec<EntityId> {
        let mut entities = Vec::new();

        // Get the room the actor is in
        let rooms = world.query_relation_forward(relations::in_room(), actor);
        if let Some(&room) = rooms.first() {
            // Add everything in the same room
            let in_room = world.query_relation_reverse(relations::in_room(), room);
            entities.extend(in_room);

            // Also check Contains relation (for containers)
            let contained = world.query_relation_forward(relations::contains(), room);
            entities.extend(contained);
        }

        // Add things the actor is carrying
        let carrying = world.query_relation_forward(relations::contains(), actor);
        entities.extend(carrying);

        // Also check CarriedBy relation (reverse direction)
        let carried = world.query_relation_reverse(relations::carried_by(), actor);
        entities.extend(carried);

        // Remove duplicates and the actor itself
        entities.sort();
        entities.dedup();
        entities.retain(|&e| e != actor);

        entities
    }
}

/// Entity resolver that matches noun phrases to entities.
#[derive(Debug)]
pub struct Resolver<S: ScopeProvider = DefaultScope> {
    scope: S,
}

impl Default for Resolver<DefaultScope> {
    fn default() -> Self {
        Self::new()
    }
}

impl Resolver<DefaultScope> {
    /// Create a new resolver with default scope.
    pub fn new() -> Self {
        Self {
            scope: DefaultScope,
        }
    }
}

impl<S: ScopeProvider> Resolver<S> {
    /// Create a resolver with a custom scope provider.
    pub fn with_scope(scope: S) -> Self {
        Self { scope }
    }

    /// Resolve a noun phrase to entities.
    pub fn resolve_noun_phrase(
        &self,
        world: &World,
        actor: EntityId,
        phrase: &str,
    ) -> ResolutionResult {
        let words: Vec<&str> = phrase.split_whitespace().collect();
        if words.is_empty() {
            return ResolutionResult::NotFound(phrase.into());
        }

        let entities = self.scope.entities_in_scope(world, actor);
        let mut candidates: Vec<EntityCandidate> = Vec::new();

        for entity in entities {
            if let Some(candidate) = self.match_entity(world, entity, &words) {
                candidates.push(candidate);
            }
        }

        match candidates.len() {
            0 => ResolutionResult::NotFound(phrase.into()),
            1 => ResolutionResult::Resolved(candidates[0].entity),
            _ => {
                // Sort by score (highest first)
                candidates.sort_by(|a, b| b.score.cmp(&a.score));

                // If top score is unique, use it
                if candidates[0].score > candidates[1].score {
                    ResolutionResult::Resolved(candidates[0].entity)
                } else {
                    // Filter to only top-scoring candidates
                    let top_score = candidates[0].score;
                    candidates.retain(|c| c.score == top_score);
                    ResolutionResult::Ambiguous(candidates)
                }
            }
        }
    }

    /// Try to match an entity against words.
    fn match_entity(
        &self,
        world: &World,
        entity: EntityId,
        words: &[&str],
    ) -> Option<EntityCandidate> {
        // Get entity's name
        let name = world.get_component(entity, naming::name())?;
        let name_str = name.as_str()?;

        let mut score = 0;
        let mut matched_name = false;
        let mut matched_adjectives = false;

        // Check if any word matches the name
        let name_lower = name_str.to_lowercase();
        for &word in words {
            if name_lower == word.to_lowercase() {
                matched_name = true;
                score += 100; // Base score for name match
                break;
            }
        }

        // Check aliases
        if !matched_name {
            if let Some(aliases) = world.get_component(entity, naming::aliases()) {
                if let Some(alias_list) = aliases.as_list() {
                    for alias_val in alias_list {
                        if let Some(alias) = alias_val.as_str() {
                            let alias_lower = alias.to_lowercase();
                            for &word in words {
                                if alias_lower == word.to_lowercase() {
                                    matched_name = true;
                                    score += 80; // Slightly lower for alias match
                                    break;
                                }
                            }
                            if matched_name {
                                break;
                            }
                        }
                    }
                }
            }
        }

        // If no name match, no candidate
        if !matched_name {
            return None;
        }

        // Check adjectives for bonus score
        if let Some(adjectives) = world.get_component(entity, naming::adjectives()) {
            if let Some(adj_list) = adjectives.as_list() {
                for adj_val in adj_list {
                    if let Some(adj) = adj_val.as_str() {
                        let adj_lower = adj.to_lowercase();
                        for &word in words {
                            if adj_lower == word.to_lowercase() {
                                matched_adjectives = true;
                                score += 50; // Bonus for adjective match
                            }
                        }
                    }
                }
            }
        }

        let mut candidate = EntityCandidate::new(entity, score, name_str);
        if matched_adjectives {
            candidate = candidate.with_adjectives();
        }

        Some(candidate)
    }

    /// Resolve an ObjectRef.
    pub fn resolve_object_ref(
        &self,
        world: &World,
        actor: EntityId,
        obj: &ObjectRef,
    ) -> ResolvedObject {
        match obj {
            ObjectRef::Unresolved(phrase) => match self.resolve_noun_phrase(world, actor, phrase) {
                ResolutionResult::Resolved(entity) => ResolvedObject::Entity(entity),
                other => ResolvedObject::Failed(other),
            },
            ObjectRef::Entity(id) => ResolvedObject::Entity(*id),
            ObjectRef::Direction(sym) => ResolvedObject::Direction(*sym),
            ObjectRef::Pronoun(_sym) => {
                // TODO: Track "it" pronoun for last referenced entity
                ResolvedObject::Failed(ResolutionResult::NotApplicable)
            }
        }
    }

    /// Resolve a full command.
    pub fn resolve_command(
        &self,
        world: &World,
        actor: EntityId,
        command: &Command,
    ) -> ResolvedCommand {
        let direct_object = command
            .direct_object
            .as_ref()
            .map(|obj| self.resolve_object_ref(world, actor, obj));

        let indirect_object = command
            .indirect_object
            .as_ref()
            .map(|obj| self.resolve_object_ref(world, actor, obj));

        let arguments = command
            .arguments
            .iter()
            .map(|(prep, obj)| (*prep, self.resolve_object_ref(world, actor, obj)))
            .collect();

        ResolvedCommand {
            verb: command.verb,
            direct_object,
            indirect_object,
            arguments,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::{Cardinality, RelationSchema, Value};

    fn setup_world() -> World {
        let mut world = World::new();

        // Register relations
        // InRoom: Many entities can be in one room (Many -> One)
        world.register_relation(RelationSchema::new(
            "InRoom",
            Cardinality::Many,
            Cardinality::One,
        ));
        // Contains: One entity contains many (One -> Many)
        world.register_relation(RelationSchema::new(
            "Contains",
            Cardinality::One,
            Cardinality::Many,
        ));
        // CarriedBy: Many items can be carried by one entity (Many -> One)
        world.register_relation(RelationSchema::new(
            "CarriedBy",
            Cardinality::Many,
            Cardinality::One,
        ));

        world
    }

    #[test]
    fn test_resolve_simple_name() {
        let mut world = setup_world();

        // Create room and entities
        let room = world.create_entity();
        let player = world.create_entity();
        let lamp = world.create_entity();

        world.set_component(lamp, naming::name(), Value::string("lamp"));
        world.add_relation(relations::in_room(), player, room);
        world.add_relation(relations::in_room(), lamp, room);

        let resolver = Resolver::new();
        let result = resolver.resolve_noun_phrase(&world, player, "lamp");

        assert!(matches!(result, ResolutionResult::Resolved(e) if e == lamp));
    }

    #[test]
    fn test_resolve_with_adjective() {
        let mut world = setup_world();

        let room = world.create_entity();
        let player = world.create_entity();
        let brass_lamp = world.create_entity();
        let rusty_lamp = world.create_entity();

        world.set_component(brass_lamp, naming::name(), Value::string("lamp"));
        world.set_component(
            brass_lamp,
            naming::adjectives(),
            Value::list(vec![Value::string("brass")]),
        );

        world.set_component(rusty_lamp, naming::name(), Value::string("lamp"));
        world.set_component(
            rusty_lamp,
            naming::adjectives(),
            Value::list(vec![Value::string("rusty")]),
        );

        world.add_relation(relations::in_room(), player, room);
        world.add_relation(relations::in_room(), brass_lamp, room);
        world.add_relation(relations::in_room(), rusty_lamp, room);

        let resolver = Resolver::new();

        // "lamp" alone is ambiguous
        let result = resolver.resolve_noun_phrase(&world, player, "lamp");
        assert!(matches!(result, ResolutionResult::Ambiguous(_)));

        // "brass lamp" should resolve to brass_lamp
        let result = resolver.resolve_noun_phrase(&world, player, "brass lamp");
        assert!(matches!(result, ResolutionResult::Resolved(e) if e == brass_lamp));

        // "rusty lamp" should resolve to rusty_lamp
        let result = resolver.resolve_noun_phrase(&world, player, "rusty lamp");
        assert!(matches!(result, ResolutionResult::Resolved(e) if e == rusty_lamp));
    }

    #[test]
    fn test_resolve_alias() {
        let mut world = setup_world();

        let room = world.create_entity();
        let player = world.create_entity();
        let lantern = world.create_entity();

        world.set_component(lantern, naming::name(), Value::string("lantern"));
        world.set_component(
            lantern,
            naming::aliases(),
            Value::list(vec![Value::string("lamp"), Value::string("light")]),
        );

        world.add_relation(relations::in_room(), player, room);
        world.add_relation(relations::in_room(), lantern, room);

        let resolver = Resolver::new();

        // Both "lantern" and "lamp" should resolve
        let result = resolver.resolve_noun_phrase(&world, player, "lantern");
        assert!(matches!(result, ResolutionResult::Resolved(e) if e == lantern));

        let result = resolver.resolve_noun_phrase(&world, player, "lamp");
        assert!(matches!(result, ResolutionResult::Resolved(e) if e == lantern));
    }

    #[test]
    fn test_resolve_not_in_scope() {
        let mut world = setup_world();

        let room1 = world.create_entity();
        let room2 = world.create_entity();
        let player = world.create_entity();
        let lamp = world.create_entity();

        world.set_component(lamp, naming::name(), Value::string("lamp"));
        world.add_relation(relations::in_room(), player, room1);
        world.add_relation(relations::in_room(), lamp, room2); // Different room

        let resolver = Resolver::new();
        let result = resolver.resolve_noun_phrase(&world, player, "lamp");

        assert!(matches!(result, ResolutionResult::NotFound(_)));
    }

    #[test]
    fn test_resolve_carried() {
        let mut world = setup_world();

        let room = world.create_entity();
        let player = world.create_entity();
        let key = world.create_entity();

        world.set_component(key, naming::name(), Value::string("key"));
        world.add_relation(relations::in_room(), player, room);
        world.add_relation(relations::contains(), player, key); // Player carrying key

        let resolver = Resolver::new();
        let result = resolver.resolve_noun_phrase(&world, player, "key");

        assert!(matches!(result, ResolutionResult::Resolved(e) if e == key));
    }

    #[test]
    fn test_resolve_command() {
        let mut world = setup_world();

        let room = world.create_entity();
        let player = world.create_entity();
        let lamp = world.create_entity();

        world.set_component(lamp, naming::name(), Value::string("lamp"));
        world.add_relation(relations::in_room(), player, room);
        world.add_relation(relations::in_room(), lamp, room);

        let command = Command::verb("take").with_direct(ObjectRef::unresolved("lamp"));

        let resolver = Resolver::new();
        let resolved = resolver.resolve_command(&world, player, &command);

        assert_eq!(resolved.verb.as_str(), "take");
        assert!(resolved.direct_object.is_some());
        let direct = resolved.direct_object.unwrap();
        assert_eq!(direct.as_entity(), Some(lamp));
    }

    #[test]
    fn test_resolve_direction_passthrough() {
        let mut world = setup_world();
        let player = world.create_entity();

        let command = Command::verb("go").with_direct(ObjectRef::direction("north"));

        let resolver = Resolver::new();
        let resolved = resolver.resolve_command(&world, player, &command);

        assert_eq!(resolved.verb.as_str(), "go");
        assert!(resolved.direct_object.is_some());
        let direct = resolved.direct_object.unwrap();
        assert_eq!(direct.as_direction(), Some(Symbol::new("north")));
    }

    #[test]
    fn test_ambiguous_entities() {
        let mut world = setup_world();

        let room = world.create_entity();
        let player = world.create_entity();
        let coin1 = world.create_entity();
        let coin2 = world.create_entity();

        world.set_component(coin1, naming::name(), Value::string("coin"));
        world.set_component(coin2, naming::name(), Value::string("coin"));
        world.add_relation(relations::in_room(), player, room);
        world.add_relation(relations::in_room(), coin1, room);
        world.add_relation(relations::in_room(), coin2, room);

        let resolver = Resolver::new();
        let result = resolver.resolve_noun_phrase(&world, player, "coin");

        match result {
            ResolutionResult::Ambiguous(candidates) => {
                assert_eq!(candidates.len(), 2);
            }
            _ => panic!("Expected ambiguous result"),
        }
    }

    #[test]
    fn test_case_insensitive() {
        let mut world = setup_world();

        let room = world.create_entity();
        let player = world.create_entity();
        let lamp = world.create_entity();

        world.set_component(lamp, naming::name(), Value::string("Lamp"));
        world.add_relation(relations::in_room(), player, room);
        world.add_relation(relations::in_room(), lamp, room);

        let resolver = Resolver::new();

        // All case variants should match
        assert!(matches!(
            resolver.resolve_noun_phrase(&world, player, "lamp"),
            ResolutionResult::Resolved(e) if e == lamp
        ));
        assert!(matches!(
            resolver.resolve_noun_phrase(&world, player, "LAMP"),
            ResolutionResult::Resolved(e) if e == lamp
        ));
        assert!(matches!(
            resolver.resolve_noun_phrase(&world, player, "Lamp"),
            ResolutionResult::Resolved(e) if e == lamp
        ));
    }
}
