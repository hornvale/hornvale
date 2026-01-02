//! Relation storage for entity relationships.
//!
//! Relations are binary connections between entities. Storage is optimized
//! based on declared cardinality (one-to-one, many-to-one, etc.).

use im::{OrdMap, OrdSet};

use crate::core::EntityId;
use crate::symbol::Symbol;

/// Identifies a relation type.
///
/// Relation types are named by symbols (e.g., "Location", "Contains", "Exit").
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RelationTypeId(pub Symbol);

impl RelationTypeId {
    /// Create a relation type ID from a name.
    pub fn new(name: &str) -> Self {
        RelationTypeId(Symbol::new(name))
    }

    /// Get the name of this relation type.
    pub fn name(&self) -> String {
        self.0.as_str()
    }
}

impl std::fmt::Display for RelationTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<&str> for RelationTypeId {
    fn from(s: &str) -> Self {
        RelationTypeId::new(s)
    }
}

/// Cardinality constraint for one side of a relation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Cardinality {
    /// At most one related entity (functional)
    One,
    /// Any number of related entities
    Many,
}

/// Schema defining a relation type's structure and constraints.
#[derive(Debug, Clone)]
pub struct RelationSchema {
    /// Relation type identifier
    pub id: RelationTypeId,
    /// Cardinality on the "from" side
    pub from_cardinality: Cardinality,
    /// Cardinality on the "to" side
    pub to_cardinality: Cardinality,
    /// Whether the relation is symmetric (a->b implies b->a)
    pub symmetric: bool,
}

impl RelationSchema {
    /// Create a new relation schema.
    pub fn new(name: &str, from_cardinality: Cardinality, to_cardinality: Cardinality) -> Self {
        Self {
            id: RelationTypeId::new(name),
            from_cardinality,
            to_cardinality,
            symmetric: false,
        }
    }

    /// Create a symmetric relation schema.
    pub fn symmetric(name: &str, cardinality: Cardinality) -> Self {
        Self {
            id: RelationTypeId::new(name),
            from_cardinality: cardinality,
            to_cardinality: cardinality,
            symmetric: true,
        }
    }

    /// Create the appropriate table type for this schema.
    pub fn create_table(&self) -> RelationTable {
        use Cardinality::*;
        match (self.from_cardinality, self.to_cardinality) {
            (One, One) => RelationTable::OneToOne {
                forward: OrdMap::new(),
                reverse: OrdMap::new(),
            },
            (Many, One) => RelationTable::ManyToOne {
                forward: OrdMap::new(),
                reverse: OrdMap::new(),
            },
            (One, Many) => RelationTable::OneToMany {
                forward: OrdMap::new(),
                reverse: OrdMap::new(),
            },
            (Many, Many) => RelationTable::ManyToMany {
                forward: OrdMap::new(),
                reverse: OrdMap::new(),
            },
        }
    }
}

/// Storage for relation instances, optimized by cardinality.
///
/// Each variant stores both forward (from->to) and reverse (to->from) indexes
/// for efficient queries in both directions.
#[derive(Debug, Clone)]
pub enum RelationTable {
    /// One-to-one: each entity relates to at most one other entity in each direction
    OneToOne {
        forward: OrdMap<EntityId, EntityId>,
        reverse: OrdMap<EntityId, EntityId>,
    },
    /// Many-to-one: many entities can relate to one entity (e.g., Location)
    ManyToOne {
        forward: OrdMap<EntityId, EntityId>,
        reverse: OrdMap<EntityId, OrdSet<EntityId>>,
    },
    /// One-to-many: one entity relates to many entities (e.g., Contains)
    OneToMany {
        forward: OrdMap<EntityId, OrdSet<EntityId>>,
        reverse: OrdMap<EntityId, EntityId>,
    },
    /// Many-to-many: any entity can relate to any number of entities
    ManyToMany {
        forward: OrdMap<EntityId, OrdSet<EntityId>>,
        reverse: OrdMap<EntityId, OrdSet<EntityId>>,
    },
}

impl RelationTable {
    /// Insert a relation from one entity to another.
    ///
    /// For functional directions (cardinality One), this replaces any existing relation.
    /// For symmetric relations, the caller must insert both directions.
    pub fn insert(&mut self, from: EntityId, to: EntityId) {
        match self {
            RelationTable::OneToOne { forward, reverse } => {
                // Remove old relations if they exist
                if let Some(old_to) = forward.get(&from) {
                    reverse.remove(old_to);
                }
                if let Some(old_from) = reverse.get(&to) {
                    forward.remove(old_from);
                }
                forward.insert(from, to);
                reverse.insert(to, from);
            }
            RelationTable::ManyToOne { forward, reverse } => {
                // Remove from old reverse set if changing target
                if let Some(old_to) = forward.get(&from) {
                    if let Some(set) = reverse.get_mut(old_to) {
                        set.remove(&from);
                    }
                }
                forward.insert(from, to);
                reverse.entry(to).or_default().insert(from);
            }
            RelationTable::OneToMany { forward, reverse } => {
                // Remove old reverse mapping if this 'to' was related elsewhere
                if let Some(old_from) = reverse.get(&to) {
                    if let Some(set) = forward.get_mut(old_from) {
                        set.remove(&to);
                    }
                }
                forward.entry(from).or_default().insert(to);
                reverse.insert(to, from);
            }
            RelationTable::ManyToMany { forward, reverse } => {
                forward.entry(from).or_default().insert(to);
                reverse.entry(to).or_default().insert(from);
            }
        }
    }

    /// Remove a specific relation.
    ///
    /// Returns true if the relation existed and was removed.
    pub fn remove(&mut self, from: EntityId, to: EntityId) -> bool {
        match self {
            RelationTable::OneToOne { forward, reverse } => {
                if forward.get(&from) == Some(&to) {
                    forward.remove(&from);
                    reverse.remove(&to);
                    true
                } else {
                    false
                }
            }
            RelationTable::ManyToOne { forward, reverse } => {
                if forward.get(&from) == Some(&to) {
                    forward.remove(&from);
                    if let Some(set) = reverse.get_mut(&to) {
                        set.remove(&from);
                    }
                    true
                } else {
                    false
                }
            }
            RelationTable::OneToMany { forward, reverse } => {
                if reverse.get(&to) == Some(&from) {
                    if let Some(set) = forward.get_mut(&from) {
                        set.remove(&to);
                    }
                    reverse.remove(&to);
                    true
                } else {
                    false
                }
            }
            RelationTable::ManyToMany { forward, reverse } => {
                let removed_forward = forward
                    .get_mut(&from)
                    .map(|set| set.remove(&to).is_some())
                    .unwrap_or(false);
                let removed_reverse = reverse
                    .get_mut(&to)
                    .map(|set| set.remove(&from).is_some())
                    .unwrap_or(false);
                removed_forward || removed_reverse
            }
        }
    }

    /// Remove all relations involving an entity (cascade delete).
    pub fn remove_entity(&mut self, entity: EntityId) {
        match self {
            RelationTable::OneToOne { forward, reverse } => {
                // Remove as 'from'
                if let Some(to) = forward.remove(&entity) {
                    reverse.remove(&to);
                }
                // Remove as 'to'
                if let Some(from) = reverse.remove(&entity) {
                    forward.remove(&from);
                }
            }
            RelationTable::ManyToOne { forward, reverse } => {
                // Remove as 'from'
                if let Some(to) = forward.remove(&entity) {
                    if let Some(set) = reverse.get_mut(&to) {
                        set.remove(&entity);
                    }
                }
                // Remove as 'to' - need to remove all 'from' entries
                if let Some(froms) = reverse.remove(&entity) {
                    for from in froms {
                        forward.remove(&from);
                    }
                }
            }
            RelationTable::OneToMany { forward, reverse } => {
                // Remove as 'from' - need to remove all 'to' entries
                if let Some(tos) = forward.remove(&entity) {
                    for to in tos {
                        reverse.remove(&to);
                    }
                }
                // Remove as 'to'
                if let Some(from) = reverse.remove(&entity) {
                    if let Some(set) = forward.get_mut(&from) {
                        set.remove(&entity);
                    }
                }
            }
            RelationTable::ManyToMany { forward, reverse } => {
                // Remove as 'from'
                if let Some(tos) = forward.remove(&entity) {
                    for to in tos {
                        if let Some(set) = reverse.get_mut(&to) {
                            set.remove(&entity);
                        }
                    }
                }
                // Remove as 'to'
                if let Some(froms) = reverse.remove(&entity) {
                    for from in froms {
                        if let Some(set) = forward.get_mut(&from) {
                            set.remove(&entity);
                        }
                    }
                }
            }
        }
    }

    /// Query the forward direction: given 'from', what are the 'to' entities?
    ///
    /// Returns a vec for consistency; for functional relations this will have 0 or 1 element.
    pub fn query_forward(&self, from: EntityId) -> Vec<EntityId> {
        match self {
            RelationTable::OneToOne { forward, .. } => {
                forward.get(&from).copied().into_iter().collect()
            }
            RelationTable::ManyToOne { forward, .. } => {
                forward.get(&from).copied().into_iter().collect()
            }
            RelationTable::OneToMany { forward, .. } => forward
                .get(&from)
                .map(|set| set.iter().copied().collect())
                .unwrap_or_default(),
            RelationTable::ManyToMany { forward, .. } => forward
                .get(&from)
                .map(|set| set.iter().copied().collect())
                .unwrap_or_default(),
        }
    }

    /// Query the reverse direction: given 'to', what are the 'from' entities?
    ///
    /// Returns a vec for consistency; for functional relations this will have 0 or 1 element.
    pub fn query_reverse(&self, to: EntityId) -> Vec<EntityId> {
        match self {
            RelationTable::OneToOne { reverse, .. } => {
                reverse.get(&to).copied().into_iter().collect()
            }
            RelationTable::ManyToOne { reverse, .. } => reverse
                .get(&to)
                .map(|set| set.iter().copied().collect())
                .unwrap_or_default(),
            RelationTable::OneToMany { reverse, .. } => {
                reverse.get(&to).copied().into_iter().collect()
            }
            RelationTable::ManyToMany { reverse, .. } => reverse
                .get(&to)
                .map(|set| set.iter().copied().collect())
                .unwrap_or_default(),
        }
    }

    /// Check if a specific relation exists.
    pub fn contains(&self, from: EntityId, to: EntityId) -> bool {
        match self {
            RelationTable::OneToOne { forward, .. } => forward.get(&from) == Some(&to),
            RelationTable::ManyToOne { forward, .. } => forward.get(&from) == Some(&to),
            RelationTable::OneToMany { forward, .. } => forward
                .get(&from)
                .map(|set| set.contains(&to))
                .unwrap_or(false),
            RelationTable::ManyToMany { forward, .. } => forward
                .get(&from)
                .map(|set| set.contains(&to))
                .unwrap_or(false),
        }
    }

    /// Check if an entity has any relations in the forward direction.
    pub fn has_forward(&self, from: EntityId) -> bool {
        match self {
            RelationTable::OneToOne { forward, .. } => forward.contains_key(&from),
            RelationTable::ManyToOne { forward, .. } => forward.contains_key(&from),
            RelationTable::OneToMany { forward, .. } => {
                forward.get(&from).map(|s| !s.is_empty()).unwrap_or(false)
            }
            RelationTable::ManyToMany { forward, .. } => {
                forward.get(&from).map(|s| !s.is_empty()).unwrap_or(false)
            }
        }
    }
}

/// Registry of all relation types and their data.
#[derive(Debug, Clone, Default)]
pub struct RelationRegistry {
    /// Schemas for each relation type
    schemas: OrdMap<RelationTypeId, RelationSchema>,
    /// Data tables for each relation type
    tables: OrdMap<RelationTypeId, RelationTable>,
}

impl RelationRegistry {
    /// Create an empty registry.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a new relation type.
    ///
    /// Creates the appropriate table based on the schema's cardinality.
    pub fn register(&mut self, schema: RelationSchema) {
        let table = schema.create_table();
        let id = schema.id;
        self.schemas.insert(id, schema);
        self.tables.insert(id, table);
    }

    /// Get the schema for a relation type.
    pub fn schema(&self, id: RelationTypeId) -> Option<&RelationSchema> {
        self.schemas.get(&id)
    }

    /// Insert a relation.
    ///
    /// For symmetric relations, automatically inserts the reverse direction.
    /// Returns false if the relation type doesn't exist.
    pub fn insert(&mut self, relation: RelationTypeId, from: EntityId, to: EntityId) -> bool {
        let Some(schema) = self.schemas.get(&relation) else {
            return false;
        };
        let symmetric = schema.symmetric;

        let Some(table) = self.tables.get_mut(&relation) else {
            return false;
        };

        table.insert(from, to);
        if symmetric && from != to {
            table.insert(to, from);
        }
        true
    }

    /// Remove a relation.
    ///
    /// For symmetric relations, automatically removes the reverse direction.
    /// Returns false if the relation type doesn't exist.
    pub fn remove(&mut self, relation: RelationTypeId, from: EntityId, to: EntityId) -> bool {
        let Some(schema) = self.schemas.get(&relation) else {
            return false;
        };
        let symmetric = schema.symmetric;

        let Some(table) = self.tables.get_mut(&relation) else {
            return false;
        };

        let removed = table.remove(from, to);
        if symmetric && from != to {
            table.remove(to, from);
        }
        removed
    }

    /// Remove all relations involving an entity (cascade delete).
    pub fn remove_entity(&mut self, entity: EntityId) {
        let keys: Vec<_> = self.tables.keys().copied().collect();
        for key in keys {
            if let Some(table) = self.tables.get_mut(&key) {
                table.remove_entity(entity);
            }
        }
    }

    /// Query forward: given 'from', what 'to' entities are related?
    pub fn query_forward(&self, relation: RelationTypeId, from: EntityId) -> Vec<EntityId> {
        self.tables
            .get(&relation)
            .map(|t| t.query_forward(from))
            .unwrap_or_default()
    }

    /// Query reverse: given 'to', what 'from' entities are related?
    pub fn query_reverse(&self, relation: RelationTypeId, to: EntityId) -> Vec<EntityId> {
        self.tables
            .get(&relation)
            .map(|t| t.query_reverse(to))
            .unwrap_or_default()
    }

    /// Check if a specific relation exists.
    pub fn contains(&self, relation: RelationTypeId, from: EntityId, to: EntityId) -> bool {
        self.tables
            .get(&relation)
            .map(|t| t.contains(from, to))
            .unwrap_or(false)
    }

    /// Check if an entity has any forward relations of a given type.
    pub fn has_forward(&self, relation: RelationTypeId, from: EntityId) -> bool {
        self.tables
            .get(&relation)
            .map(|t| t.has_forward(from))
            .unwrap_or(false)
    }

    /// Get all registered relation type IDs.
    pub fn relation_types(&self) -> impl Iterator<Item = RelationTypeId> + '_ {
        self.schemas.keys().copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::EntityAllocator;

    fn make_entities(count: usize) -> Vec<EntityId> {
        let mut alloc = EntityAllocator::new();
        (0..count).map(|_| alloc.create()).collect()
    }

    #[test]
    fn test_one_to_one_basic() {
        let schema = RelationSchema::new("Spouse", Cardinality::One, Cardinality::One);
        let mut table = schema.create_table();
        let [a, b, c] = make_entities(3).try_into().unwrap();

        table.insert(a, b);
        assert!(table.contains(a, b));
        assert_eq!(table.query_forward(a), vec![b]);
        assert_eq!(table.query_reverse(b), vec![a]);

        // Replace: a's spouse changes from b to c
        table.insert(a, c);
        assert!(!table.contains(a, b));
        assert!(table.contains(a, c));
        assert_eq!(table.query_reverse(b), vec![]); // b no longer has spouse
    }

    #[test]
    fn test_many_to_one_location() {
        let schema = RelationSchema::new("Location", Cardinality::Many, Cardinality::One);
        let mut table = schema.create_table();
        let [room, goat, chest] = make_entities(3).try_into().unwrap();

        // Both goat and chest are in room
        table.insert(goat, room);
        table.insert(chest, room);

        assert_eq!(table.query_forward(goat), vec![room]);
        assert_eq!(table.query_forward(chest), vec![room]);

        // Reverse query: what's in the room?
        let mut in_room = table.query_reverse(room);
        in_room.sort();
        let mut expected = vec![goat, chest];
        expected.sort();
        assert_eq!(in_room, expected);

        // Move goat to a new location
        let [_, _, _, new_room] = make_entities(4).try_into().unwrap();
        table.insert(goat, new_room);
        assert!(!table.contains(goat, room));
        assert!(table.contains(goat, new_room));
    }

    #[test]
    fn test_one_to_many_contains() {
        let schema = RelationSchema::new("Contains", Cardinality::One, Cardinality::Many);
        let mut table = schema.create_table();
        let [room, goat, chest] = make_entities(3).try_into().unwrap();

        table.insert(room, goat);
        table.insert(room, chest);

        // Forward: what does the room contain?
        let mut contains = table.query_forward(room);
        contains.sort();
        let mut expected = vec![goat, chest];
        expected.sort();
        assert_eq!(contains, expected);

        // Reverse: what contains the goat?
        assert_eq!(table.query_reverse(goat), vec![room]);
    }

    #[test]
    fn test_many_to_many_friendship() {
        let schema = RelationSchema::new("Knows", Cardinality::Many, Cardinality::Many);
        let mut table = schema.create_table();
        let [a, b, c] = make_entities(3).try_into().unwrap();

        table.insert(a, b);
        table.insert(a, c);
        table.insert(b, c);

        let mut a_knows = table.query_forward(a);
        a_knows.sort();
        let mut expected = vec![b, c];
        expected.sort();
        assert_eq!(a_knows, expected);

        // c is known by both a and b
        let mut know_c = table.query_reverse(c);
        know_c.sort();
        let mut expected = vec![a, b];
        expected.sort();
        assert_eq!(know_c, expected);
    }

    #[test]
    fn test_symmetric_relation() {
        let schema = RelationSchema::symmetric("Friendship", Cardinality::Many);
        let mut registry = RelationRegistry::new();
        registry.register(schema);
        let [a, b] = make_entities(2).try_into().unwrap();

        registry.insert("Friendship".into(), a, b);

        // Both directions should exist
        assert!(registry.contains("Friendship".into(), a, b));
        assert!(registry.contains("Friendship".into(), b, a));

        // Remove one direction removes both
        registry.remove("Friendship".into(), a, b);
        assert!(!registry.contains("Friendship".into(), a, b));
        assert!(!registry.contains("Friendship".into(), b, a));
    }

    #[test]
    fn test_cascade_delete() {
        let schema = RelationSchema::new("Location", Cardinality::Many, Cardinality::One);
        let mut registry = RelationRegistry::new();
        registry.register(schema);
        let [room, goat, chest] = make_entities(3).try_into().unwrap();

        registry.insert("Location".into(), goat, room);
        registry.insert("Location".into(), chest, room);

        // Delete the room - should remove all relations
        registry.remove_entity(room);

        assert!(!registry.has_forward("Location".into(), goat));
        assert!(!registry.has_forward("Location".into(), chest));
    }

    #[test]
    fn test_remove_specific_relation() {
        let schema = RelationSchema::new("Knows", Cardinality::Many, Cardinality::Many);
        let mut table = schema.create_table();
        let [a, b, c] = make_entities(3).try_into().unwrap();

        table.insert(a, b);
        table.insert(a, c);

        assert!(table.remove(a, b));
        assert!(!table.contains(a, b));
        assert!(table.contains(a, c));

        // Removing non-existent relation returns false
        assert!(!table.remove(a, b));
    }

    #[test]
    fn test_registry_relation_types() {
        let mut registry = RelationRegistry::new();
        registry.register(RelationSchema::new(
            "Location",
            Cardinality::Many,
            Cardinality::One,
        ));
        registry.register(RelationSchema::new(
            "Contains",
            Cardinality::One,
            Cardinality::Many,
        ));

        let types: Vec<_> = registry.relation_types().collect();
        assert_eq!(types.len(), 2);
    }

    #[test]
    fn test_unregistered_relation() {
        let registry = RelationRegistry::new();
        let [a, b] = make_entities(2).try_into().unwrap();

        // Operations on unregistered relations return empty/false
        assert_eq!(registry.query_forward("Unknown".into(), a), vec![]);
        assert!(!registry.contains("Unknown".into(), a, b));
    }
}
