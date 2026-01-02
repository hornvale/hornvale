//! Derivation rules define how properties are computed.
//!
//! A derivation rule specifies:
//! - A pattern that determines which entities the rule applies to
//! - Which property (component) the rule derives
//! - A function to compute the value
//! - How to compose with other rules deriving the same property

use std::sync::Arc;

use crate::core::{ComponentTypeId, EntityId, Value, World};
use crate::derive::ComposeMode;
use crate::rules::Pattern;
use crate::symbol::Symbol;

/// A function that computes a derived value for an entity.
///
/// The function receives the world state and the entity being evaluated.
/// It should be pure (no side effects) and deterministic.
pub type ValueFn = Arc<dyn Fn(&World, EntityId) -> Value + Send + Sync>;

/// What a derivation rule produces.
#[derive(Debug, Clone)]
pub struct DerivedProperty {
    /// The component type this rule derives.
    pub component: ComponentTypeId,
}

impl DerivedProperty {
    /// Create a new derived property target.
    pub fn new(component: impl Into<ComponentTypeId>) -> Self {
        DerivedProperty {
            component: component.into(),
        }
    }
}

/// A rule that derives a property value for entities.
pub struct DerivationRule {
    /// Unique name for this rule.
    pub name: Symbol,
    /// Pattern that determines which entities this rule applies to.
    pub pattern: Pattern,
    /// What property this rule derives.
    pub derives: DerivedProperty,
    /// Function to compute the derived value.
    pub value_fn: ValueFn,
    /// How to compose this rule's contribution with others.
    pub composition: ComposeMode,
    /// Priority for override modes (higher wins).
    pub priority: i32,
}

impl DerivationRule {
    /// Create a new derivation rule.
    pub fn new(
        name: impl Into<Symbol>,
        pattern: Pattern,
        derives: DerivedProperty,
        value_fn: ValueFn,
    ) -> Self {
        DerivationRule {
            name: name.into(),
            pattern,
            derives,
            value_fn,
            composition: ComposeMode::default(),
            priority: 0,
        }
    }

    /// Set the composition mode for this rule.
    pub fn with_composition(mut self, mode: ComposeMode) -> Self {
        self.composition = mode;
        self
    }

    /// Set the priority for this rule.
    pub fn with_priority(mut self, priority: i32) -> Self {
        self.priority = priority;
        self
    }

    /// Check if this rule applies to an entity.
    pub fn matches(&self, world: &World, entity: EntityId) -> bool {
        self.pattern.matches(world, entity)
    }

    /// Compute the derived value for an entity.
    ///
    /// This should only be called after `matches()` returns true.
    pub fn compute(&self, world: &World, entity: EntityId) -> Value {
        (self.value_fn)(world, entity)
    }

    /// Get a human-readable description of this rule.
    pub fn describe(&self) -> String {
        format!(
            "Rule '{}': {} -> {:?} ({:?}, priority={})",
            self.name,
            self.pattern.describe(),
            self.derives.component,
            self.composition,
            self.priority
        )
    }
}

impl std::fmt::Debug for DerivationRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DerivationRule")
            .field("name", &self.name)
            .field("pattern", &self.pattern)
            .field("derives", &self.derives)
            .field("composition", &self.composition)
            .field("priority", &self.priority)
            .field("value_fn", &"<fn>")
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_derivation_rule_creation() {
        let rule = DerivationRule::new(
            "test-rule",
            Pattern::entity("?e"),
            DerivedProperty::new("TestComponent"),
            Arc::new(|_, _| Value::Int(42)),
        );

        assert_eq!(rule.name.as_str(), "test-rule");
        assert_eq!(rule.derives.component.0.as_str(), "TestComponent");
        assert_eq!(rule.composition, ComposeMode::Add);
        assert_eq!(rule.priority, 0);
    }

    #[test]
    fn test_derivation_rule_builder() {
        let rule = DerivationRule::new(
            "test-rule",
            Pattern::entity("?e"),
            DerivedProperty::new("TestComponent"),
            Arc::new(|_, _| Value::Int(42)),
        )
        .with_composition(ComposeMode::Override)
        .with_priority(10);

        assert_eq!(rule.composition, ComposeMode::Override);
        assert_eq!(rule.priority, 10);
    }

    #[test]
    fn test_derivation_rule_matches() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "Name", "test");

        // Rule that requires Name component
        let rule = DerivationRule::new(
            "name-rule",
            Pattern::has_component("?e", "Name"),
            DerivedProperty::new("Derived"),
            Arc::new(|_, _| Value::Int(1)),
        );

        assert!(rule.matches(&world, entity));

        // Entity without Name
        let other = world.create_entity();
        assert!(!rule.matches(&world, other));
    }

    #[test]
    fn test_derivation_rule_compute() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        // Rule that doubles HP
        let rule = DerivationRule::new(
            "double-hp",
            Pattern::has_component("?e", "HP"),
            DerivedProperty::new("EffectiveHP"),
            Arc::new(|w, e| {
                let hp = w
                    .get_component(e, "HP")
                    .and_then(|v| v.as_int())
                    .unwrap_or(0);
                Value::Int(hp * 2)
            }),
        );

        let result = rule.compute(&world, entity);
        assert_eq!(result, Value::Int(200));
    }

    #[test]
    fn test_derivation_rule_describe() {
        let rule = DerivationRule::new(
            "fire-resistance",
            Pattern::has_component("?e", "InVolcanicBiome"),
            DerivedProperty::new("FireResistance"),
            Arc::new(|_, _| Value::Float(0.3)),
        )
        .with_composition(ComposeMode::Add)
        .with_priority(5);

        let desc = rule.describe();
        assert!(desc.contains("fire-resistance"));
        assert!(desc.contains("FireResistance"));
        assert!(desc.contains("Add"));
        assert!(desc.contains("priority=5"));
    }
}
