//! Type predicates and slot types for the grammar system.

use crate::lang::SExpr;
use crate::symbol::Symbol;
use im::OrdMap;

/// A type predicate that can be evaluated against an entity.
///
/// Type predicates are DSL expressions that determine whether an entity
/// matches a semantic constraint like "portable" or "container".
///
/// ## Example
///
/// ```lisp
/// (type portable (has? entity :Portable))
/// (type container (has? entity :Container))
/// (type held (held-by? entity actor))
/// ```
#[derive(Debug, Clone)]
pub struct TypePredicate {
    /// Name of this type (e.g., "portable", "container").
    pub name: Symbol,
    /// The predicate expression to evaluate.
    /// The expression has access to `entity` (the candidate) and `actor` (the player).
    pub predicate: SExpr,
}

impl TypePredicate {
    /// Create a new type predicate.
    pub fn new(name: impl Into<Symbol>, predicate: SExpr) -> Self {
        Self {
            name: name.into(),
            predicate,
        }
    }
}

/// Registry of type predicates.
#[derive(Debug, Clone, Default)]
pub struct TypeRegistry {
    /// Types indexed by name.
    types: OrdMap<Symbol, TypePredicate>,
}

impl TypeRegistry {
    /// Create a new empty type registry.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a type predicate.
    pub fn register(&mut self, predicate: TypePredicate) {
        self.types.insert(predicate.name, predicate);
    }

    /// Get a type predicate by name.
    pub fn get(&self, name: Symbol) -> Option<&TypePredicate> {
        self.types.get(&name)
    }

    /// Check if a type is registered.
    pub fn contains(&self, name: Symbol) -> bool {
        self.types.contains_key(&name)
    }

    /// Iterate over all registered types.
    pub fn iter(&self) -> impl Iterator<Item = (&Symbol, &TypePredicate)> {
        self.types.iter()
    }

    /// Number of registered types.
    pub fn len(&self) -> usize {
        self.types.len()
    }

    /// Check if the registry is empty.
    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }
}

/// A slot type in a form pattern.
///
/// Slots capture parts of player input and can have type constraints
/// that are validated against the resolved entity.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SlotType {
    /// Built-in noun slot (matches any entity in scope).
    Noun,
    /// Built-in direction slot (matches compass directions).
    Direction,
    /// Custom type with predicate (e.g., "portable", "container").
    Custom(Symbol),
}

impl SlotType {
    /// Create a custom slot type.
    pub fn custom(name: impl Into<Symbol>) -> Self {
        Self::Custom(name.into())
    }

    /// Check if this is a built-in noun type.
    pub fn is_noun(&self) -> bool {
        matches!(self, SlotType::Noun)
    }

    /// Check if this is a built-in direction type.
    pub fn is_direction(&self) -> bool {
        matches!(self, SlotType::Direction)
    }

    /// Check if this is a custom type.
    pub fn is_custom(&self) -> bool {
        matches!(self, SlotType::Custom(_))
    }

    /// Get the custom type name, if any.
    pub fn custom_name(&self) -> Option<Symbol> {
        match self {
            SlotType::Custom(name) => Some(*name),
            _ => None,
        }
    }
}

impl std::fmt::Display for SlotType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SlotType::Noun => write!(f, "noun"),
            SlotType::Direction => write!(f, "direction"),
            SlotType::Custom(name) => write!(f, "{}", name.as_str()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::{Atom, Span};

    fn dummy_span() -> Span {
        Span::new(0, 0, 1, 1)
    }

    #[test]
    fn test_type_predicate_creation() {
        let pred = TypePredicate::new("portable", SExpr::Atom(Atom::Bool(true), dummy_span()));
        assert_eq!(pred.name.as_str(), "portable");
    }

    #[test]
    fn test_type_registry_basic() {
        let mut registry = TypeRegistry::new();
        assert!(registry.is_empty());

        let pred = TypePredicate::new("portable", SExpr::Atom(Atom::Bool(true), dummy_span()));
        registry.register(pred);

        assert_eq!(registry.len(), 1);
        assert!(registry.contains(Symbol::new("portable")));
        assert!(!registry.contains(Symbol::new("container")));

        let retrieved = registry.get(Symbol::new("portable"));
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().name.as_str(), "portable");
    }

    #[test]
    fn test_slot_type_variants() {
        assert!(SlotType::Noun.is_noun());
        assert!(!SlotType::Noun.is_direction());
        assert!(!SlotType::Noun.is_custom());

        assert!(!SlotType::Direction.is_noun());
        assert!(SlotType::Direction.is_direction());
        assert!(!SlotType::Direction.is_custom());

        let custom = SlotType::custom("portable");
        assert!(!custom.is_noun());
        assert!(!custom.is_direction());
        assert!(custom.is_custom());
        assert_eq!(custom.custom_name(), Some(Symbol::new("portable")));
    }

    #[test]
    fn test_slot_type_display() {
        assert_eq!(format!("{}", SlotType::Noun), "noun");
        assert_eq!(format!("{}", SlotType::Direction), "direction");
        assert_eq!(format!("{}", SlotType::custom("portable")), "portable");
    }
}
