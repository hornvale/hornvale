//! Graph traversal queries for relations.
//!
//! This module provides transitive closure operations over the relation graph:
//! - `descendants`: Follow relations forward (e.g., "everything in this room")
//! - `ancestors`: Follow relations backward (e.g., "what contains this item")
//! - `reachable`: Follow multiple relation types (e.g., "rooms within 3 exits")

use im::OrdSet;

use crate::core::{EntityId, RelationTypeId, World};

/// Result of a graph traversal query.
#[derive(Debug, Clone)]
pub struct TraversalResult {
    /// Entities found, in BFS order (closest first).
    pub entities: Vec<EntityId>,
    /// Maximum depth actually reached.
    pub max_depth_reached: usize,
    /// Whether traversal was limited by max_depth (more might exist).
    pub truncated: bool,
}

impl TraversalResult {
    /// Create an empty result.
    pub fn empty() -> Self {
        Self {
            entities: Vec::new(),
            max_depth_reached: 0,
            truncated: false,
        }
    }

    /// Get the entities as a slice.
    pub fn as_slice(&self) -> &[EntityId] {
        &self.entities
    }

    /// Check if any entities were found.
    pub fn is_empty(&self) -> bool {
        self.entities.is_empty()
    }

    /// Get the number of entities found.
    pub fn len(&self) -> usize {
        self.entities.len()
    }
}

impl World {
    /// Find all entities reachable by following a relation forward, transitively.
    ///
    /// Starting from `start`, follows `relation` in the forward direction up to
    /// `max_depth` hops. The start entity is NOT included in results.
    ///
    /// The optional `stop` predicate controls recursion: if `stop(entity)` returns
    /// true, that entity IS included in results but its children are NOT explored.
    /// This is useful for "stop at closed containers" semantics.
    ///
    /// # Example
    ///
    /// ```ignore
    /// // Find everything in a room, recursively
    /// let contents = world.descendants(room, "Contains", 10, |_| false);
    ///
    /// // Find visible contents (stop at closed containers)
    /// let visible = world.descendants(room, "Contains", 10, |e| {
    ///     world.get_component(e, "Closed") == Some(&Value::Bool(true))
    /// });
    /// ```
    pub fn descendants<F>(
        &self,
        start: EntityId,
        relation: impl Into<RelationTypeId>,
        max_depth: usize,
        stop: F,
    ) -> TraversalResult
    where
        F: Fn(EntityId) -> bool,
    {
        let relation = relation.into();

        if max_depth == 0 {
            return TraversalResult::empty();
        }

        let mut result = Vec::new();
        let mut visited: OrdSet<EntityId> = OrdSet::new();
        let mut current_depth_entities: Vec<EntityId> = vec![start];
        let mut max_depth_reached = 0;
        let mut truncated = false;

        visited.insert(start);

        for depth in 1..=max_depth {
            let mut next_depth_entities: Vec<EntityId> = Vec::new();

            for &entity in &current_depth_entities {
                // Get children of this entity
                let children = self.query_relation_forward(relation, entity);

                for child in children {
                    if visited.contains(&child) {
                        continue;
                    }
                    visited.insert(child);
                    result.push(child);
                    max_depth_reached = depth;

                    // Check stop predicate - if true, include but don't recurse
                    if !stop(child) {
                        next_depth_entities.push(child);
                    }
                }
            }

            if next_depth_entities.is_empty() {
                break;
            }

            current_depth_entities = next_depth_entities;
        }

        // Check if truncated: we hit depth limit AND there are unexplored children
        if max_depth_reached == max_depth {
            for &entity in &current_depth_entities {
                let children = self.query_relation_forward(relation, entity);
                for child in children {
                    if !visited.contains(&child) {
                        truncated = true;
                        break;
                    }
                }
                if truncated {
                    break;
                }
            }
        }

        TraversalResult {
            entities: result,
            max_depth_reached,
            truncated,
        }
    }

    /// Find all entities that transitively relate TO this entity (reverse traversal).
    ///
    /// Starting from `start`, follows `relation` in the REVERSE direction up to
    /// `max_depth` hops. The start entity is NOT included in results.
    ///
    /// # Example
    ///
    /// ```ignore
    /// // Find the chain of containers holding an item
    /// let containers = world.ancestors(key, "Contains", 10);
    /// // Returns [chest, room] if room Contains chest Contains key
    /// ```
    pub fn ancestors(
        &self,
        start: EntityId,
        relation: impl Into<RelationTypeId>,
        max_depth: usize,
    ) -> TraversalResult {
        let relation = relation.into();

        if max_depth == 0 {
            return TraversalResult::empty();
        }

        let mut result = Vec::new();
        let mut visited: OrdSet<EntityId> = OrdSet::new();
        let mut current_depth_entities: Vec<EntityId> = vec![start];
        let mut max_depth_reached = 0;
        let mut truncated = false;

        visited.insert(start);

        for depth in 1..=max_depth {
            let mut next_depth_entities: Vec<EntityId> = Vec::new();

            for &entity in &current_depth_entities {
                // Get parents of this entity (reverse query)
                let parents = self.query_relation_reverse(relation, entity);

                for parent in parents {
                    if visited.contains(&parent) {
                        continue;
                    }
                    visited.insert(parent);
                    result.push(parent);
                    max_depth_reached = depth;
                    next_depth_entities.push(parent);
                }
            }

            if next_depth_entities.is_empty() {
                break;
            }

            current_depth_entities = next_depth_entities;
        }

        // Check if truncated: we hit depth limit AND there are unexplored parents
        if max_depth_reached == max_depth {
            for &entity in &current_depth_entities {
                let parents = self.query_relation_reverse(relation, entity);
                for parent in parents {
                    if !visited.contains(&parent) {
                        truncated = true;
                        break;
                    }
                }
                if truncated {
                    break;
                }
            }
        }

        TraversalResult {
            entities: result,
            max_depth_reached,
            truncated,
        }
    }

    /// Find all entities reachable via any of the specified relations.
    ///
    /// This is a multi-relation BFS that can follow different relation types.
    /// Useful for pathfinding ("rooms reachable via Exit or Portal").
    ///
    /// # Example
    ///
    /// ```ignore
    /// // Find all rooms within 3 steps via exits or portals
    /// let nearby = world.reachable(
    ///     current_room,
    ///     &["Exit".into(), "Portal".into()],
    ///     3,
    /// );
    /// ```
    pub fn reachable(
        &self,
        start: EntityId,
        relations: &[RelationTypeId],
        max_depth: usize,
    ) -> TraversalResult {
        if max_depth == 0 || relations.is_empty() {
            return TraversalResult::empty();
        }

        let mut result = Vec::new();
        let mut visited: OrdSet<EntityId> = OrdSet::new();
        let mut current_depth_entities: Vec<EntityId> = vec![start];
        let mut max_depth_reached = 0;
        let mut truncated = false;

        visited.insert(start);

        for depth in 1..=max_depth {
            let mut next_depth_entities: Vec<EntityId> = Vec::new();

            for &entity in &current_depth_entities {
                // Follow all relation types
                for &relation in relations {
                    let neighbors = self.query_relation_forward(relation, entity);

                    for neighbor in neighbors {
                        if visited.contains(&neighbor) {
                            continue;
                        }
                        visited.insert(neighbor);
                        result.push(neighbor);
                        max_depth_reached = depth;
                        next_depth_entities.push(neighbor);
                    }
                }
            }

            if next_depth_entities.is_empty() {
                break;
            }

            current_depth_entities = next_depth_entities;
        }

        // Check if truncated: we hit depth limit AND there are unexplored neighbors
        if max_depth_reached == max_depth {
            'outer: for &entity in &current_depth_entities {
                for &relation in relations {
                    let neighbors = self.query_relation_forward(relation, entity);
                    for neighbor in neighbors {
                        if !visited.contains(&neighbor) {
                            truncated = true;
                            break 'outer;
                        }
                    }
                }
            }
        }

        TraversalResult {
            entities: result,
            max_depth_reached,
            truncated,
        }
    }

    /// Convenience: find all descendants without a stop predicate.
    pub fn descendants_all(
        &self,
        start: EntityId,
        relation: impl Into<RelationTypeId>,
        max_depth: usize,
    ) -> TraversalResult {
        self.descendants(start, relation, max_depth, |_| false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::{Cardinality, RelationSchema};

    fn setup_containment_world() -> World {
        let mut world = World::new();

        // Register Contains relation (one-to-many)
        world.register_relation(RelationSchema::new(
            "Contains",
            Cardinality::One,
            Cardinality::Many,
        ));

        // Build structure:
        // room Contains chest, table
        // chest Contains key, coin
        // table Contains lamp

        let room = world.create_entity(); // 0
        let chest = world.create_entity(); // 1
        let table = world.create_entity(); // 2
        let key = world.create_entity(); // 3
        let coin = world.create_entity(); // 4
        let lamp = world.create_entity(); // 5

        world.set_component(room, "Name", "room");
        world.set_component(chest, "Name", "chest");
        world.set_component(table, "Name", "table");
        world.set_component(key, "Name", "key");
        world.set_component(coin, "Name", "coin");
        world.set_component(lamp, "Name", "lamp");

        world.add_relation("Contains", room, chest);
        world.add_relation("Contains", room, table);
        world.add_relation("Contains", chest, key);
        world.add_relation("Contains", chest, coin);
        world.add_relation("Contains", table, lamp);

        world
    }

    #[test]
    fn test_descendants_depth_1() {
        let world = setup_containment_world();
        let room = EntityId::from_raw(0);

        let result = world.descendants_all(room, "Contains", 1);

        assert_eq!(result.len(), 2); // chest, table
        assert!(result.entities.contains(&EntityId::from_raw(1))); // chest
        assert!(result.entities.contains(&EntityId::from_raw(2))); // table
        assert_eq!(result.max_depth_reached, 1);
    }

    #[test]
    fn test_descendants_depth_2() {
        let world = setup_containment_world();
        let room = EntityId::from_raw(0);

        let result = world.descendants_all(room, "Contains", 2);

        assert_eq!(result.len(), 5); // chest, table, key, coin, lamp
        assert_eq!(result.max_depth_reached, 2);
        assert!(!result.truncated);
    }

    #[test]
    fn test_descendants_unlimited() {
        let world = setup_containment_world();
        let room = EntityId::from_raw(0);

        let result = world.descendants_all(room, "Contains", 100);

        assert_eq!(result.len(), 5);
        assert_eq!(result.max_depth_reached, 2);
        assert!(!result.truncated);
    }

    #[test]
    fn test_descendants_with_stop_predicate() {
        let mut world = setup_containment_world();
        let room = EntityId::from_raw(0);
        let chest = EntityId::from_raw(1);

        // Mark chest as closed
        world.set_component(chest, "Closed", true);

        // Stop at closed containers
        let result = world.descendants(room, "Contains", 10, |e| {
            world.get_component(e, "Closed") == Some(&crate::Value::Bool(true))
        });

        // Should find chest and table at depth 1, but NOT key/coin inside chest
        // Table's lamp should still be found
        assert_eq!(result.len(), 3); // chest, table, lamp
        assert!(result.entities.contains(&EntityId::from_raw(1))); // chest (included but not recursed)
        assert!(result.entities.contains(&EntityId::from_raw(2))); // table
        assert!(result.entities.contains(&EntityId::from_raw(5))); // lamp
        assert!(!result.entities.contains(&EntityId::from_raw(3))); // key (hidden)
        assert!(!result.entities.contains(&EntityId::from_raw(4))); // coin (hidden)
    }

    #[test]
    fn test_ancestors() {
        let world = setup_containment_world();
        let key = EntityId::from_raw(3);

        let result = world.ancestors(key, "Contains", 10);

        // key -> chest -> room
        assert_eq!(result.len(), 2);
        assert_eq!(result.entities[0], EntityId::from_raw(1)); // chest (depth 1)
        assert_eq!(result.entities[1], EntityId::from_raw(0)); // room (depth 2)
        assert_eq!(result.max_depth_reached, 2);
    }

    #[test]
    fn test_ancestors_depth_limited() {
        let world = setup_containment_world();
        let key = EntityId::from_raw(3);

        let result = world.ancestors(key, "Contains", 1);

        assert_eq!(result.len(), 1);
        assert_eq!(result.entities[0], EntityId::from_raw(1)); // chest only
        assert!(result.truncated); // room exists but wasn't reached
    }

    #[test]
    fn test_descendants_empty() {
        let world = setup_containment_world();
        let key = EntityId::from_raw(3); // leaf node

        let result = world.descendants_all(key, "Contains", 10);

        assert!(result.is_empty());
        assert_eq!(result.max_depth_reached, 0);
    }

    #[test]
    fn test_descendants_zero_depth() {
        let world = setup_containment_world();
        let room = EntityId::from_raw(0);

        let result = world.descendants_all(room, "Contains", 0);

        assert!(result.is_empty());
    }

    #[test]
    fn test_reachable_single_relation() {
        let world = setup_containment_world();
        let room = EntityId::from_raw(0);

        let contains: RelationTypeId = "Contains".into();
        let result = world.reachable(room, &[contains], 2);

        assert_eq!(result.len(), 5);
    }

    #[test]
    fn test_reachable_multiple_relations() {
        let mut world = World::new();

        // Register two relation types
        world.register_relation(RelationSchema::new(
            "Exit",
            Cardinality::Many,
            Cardinality::Many,
        ));
        world.register_relation(RelationSchema::new(
            "Portal",
            Cardinality::Many,
            Cardinality::Many,
        ));

        // Create rooms connected by exits and portals
        let room_a = world.create_entity(); // 0
        let room_b = world.create_entity(); // 1
        let room_c = world.create_entity(); // 2
        let room_d = world.create_entity(); // 3

        // A --Exit--> B --Exit--> C
        // A --Portal--> D
        world.add_relation("Exit", room_a, room_b);
        world.add_relation("Exit", room_b, room_c);
        world.add_relation("Portal", room_a, room_d);

        let exit: RelationTypeId = "Exit".into();
        let portal: RelationTypeId = "Portal".into();

        // From A, can reach B, C, D
        let result = world.reachable(room_a, &[exit, portal], 10);

        assert_eq!(result.len(), 3);
        assert!(result.entities.contains(&room_b));
        assert!(result.entities.contains(&room_c));
        assert!(result.entities.contains(&room_d));
    }

    #[test]
    fn test_cycle_handling() {
        let mut world = World::new();

        world.register_relation(RelationSchema::new(
            "Link",
            Cardinality::Many,
            Cardinality::Many,
        ));

        // Create a cycle: A -> B -> C -> A
        let a = world.create_entity();
        let b = world.create_entity();
        let c = world.create_entity();

        world.add_relation("Link", a, b);
        world.add_relation("Link", b, c);
        world.add_relation("Link", c, a);

        let result = world.descendants_all(a, "Link", 100);

        // Should find B and C, but not loop forever
        assert_eq!(result.len(), 2);
        assert!(result.entities.contains(&b));
        assert!(result.entities.contains(&c));
    }
}
