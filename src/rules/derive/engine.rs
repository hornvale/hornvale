//! The derivation engine evaluates rules and composes derived values.
//!
//! The engine maintains a registry of derivation rules and handles:
//! - Finding rules that apply to a given entity/property
//! - Evaluating patterns and computing values
//! - Composing multiple rule contributions
//! - Detecting circular dependencies

use std::cell::RefCell;

use im::OrdMap;
use im::OrdSet;

use super::{CacheEntry, Contribution, Dependency, DerivationRule, DerivedCache, compose_values};
#[cfg(test)]
use super::{ComposeMode, DerivedProperty};
use crate::core::{ComponentTypeId, EntityId, RelationTypeId, Value, World};
use crate::symbol::Symbol;

/// Errors that can occur during derivation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DerivationError {
    /// A circular dependency was detected during derivation.
    CircularDependency {
        /// The cycle path: (entity, property) pairs.
        cycle: Vec<(EntityId, ComponentTypeId)>,
    },
}

impl std::fmt::Display for DerivationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DerivationError::CircularDependency { cycle } => {
                write!(f, "Circular dependency detected: ")?;
                for (i, (entity, prop)) in cycle.iter().enumerate() {
                    if i > 0 {
                        write!(f, " -> ")?;
                    }
                    write!(f, "({entity}, {prop})")?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for DerivationError {}

/// The derivation engine manages rules and computes derived values.
pub struct DerivationEngine {
    /// All registered derivation rules.
    rules: Vec<DerivationRule>,
    /// Index: component type -> indices of rules that derive it.
    rules_by_component: OrdMap<ComponentTypeId, Vec<usize>>,
    /// Set of component types that are derived (not stored).
    derived_components: OrdSet<ComponentTypeId>,
    /// Stack of currently computing (entity, property) pairs for cycle detection.
    /// Uses RefCell for interior mutability during recursive derivation.
    computing_stack: RefCell<Vec<(EntityId, ComponentTypeId)>>,
    /// Cache for derived values.
    cache: RefCell<DerivedCache>,
    /// Whether caching is enabled.
    caching_enabled: bool,
}

impl DerivationEngine {
    /// Create a new empty derivation engine with caching enabled.
    pub fn new() -> Self {
        DerivationEngine {
            rules: Vec::new(),
            rules_by_component: OrdMap::new(),
            derived_components: OrdSet::new(),
            computing_stack: RefCell::new(Vec::new()),
            cache: RefCell::new(DerivedCache::new()),
            caching_enabled: true,
        }
    }

    /// Create a new derivation engine with caching disabled.
    pub fn new_without_cache() -> Self {
        DerivationEngine {
            rules: Vec::new(),
            rules_by_component: OrdMap::new(),
            derived_components: OrdSet::new(),
            computing_stack: RefCell::new(Vec::new()),
            cache: RefCell::new(DerivedCache::new()),
            caching_enabled: false,
        }
    }

    /// Enable or disable caching.
    pub fn set_caching(&mut self, enabled: bool) {
        self.caching_enabled = enabled;
        if !enabled {
            self.cache.borrow_mut().clear();
        }
    }

    /// Check if caching is enabled.
    pub fn caching_enabled(&self) -> bool {
        self.caching_enabled
    }

    /// Get cache statistics.
    pub fn cache_stats(&self) -> (usize, usize) {
        let cache = self.cache.borrow();
        (cache.len(), cache.valid_count())
    }

    /// Clear the cache.
    pub fn clear_cache(&self) {
        self.cache.borrow_mut().clear();
    }

    /// Register a derivation rule.
    ///
    /// The component type is automatically marked as derived.
    pub fn add_rule(&mut self, rule: DerivationRule) {
        let component = rule.derives.component;
        let index = self.rules.len();
        self.rules.push(rule);

        // Add to index
        let indices = self
            .rules_by_component
            .get(&component)
            .cloned()
            .unwrap_or_default();
        let mut new_indices = indices;
        new_indices.push(index);
        self.rules_by_component.insert(component, new_indices);

        // Mark as derived
        self.derived_components.insert(component);
    }

    /// Mark a component type as derived (computed, not stored).
    ///
    /// This is automatically called when adding rules, but can be called
    /// explicitly for components that should be derived but have no rules yet.
    pub fn register_derived_component(&mut self, component: impl Into<ComponentTypeId>) {
        self.derived_components.insert(component.into());
    }

    /// Check if a component type is derived.
    pub fn is_derived(&self, component: impl Into<ComponentTypeId>) -> bool {
        self.derived_components.contains(&component.into())
    }

    /// Get all registered rules.
    pub fn rules(&self) -> &[DerivationRule] {
        &self.rules
    }

    /// Get a rule by name.
    pub fn get_rule(&self, name: impl Into<Symbol>) -> Option<&DerivationRule> {
        let name = name.into();
        self.rules.iter().find(|r| r.name == name)
    }

    /// Get rules that derive a specific component.
    pub fn rules_for_component(
        &self,
        component: impl Into<ComponentTypeId>,
    ) -> Vec<&DerivationRule> {
        let component = component.into();
        self.rules_by_component
            .get(&component)
            .map(|indices| indices.iter().map(|&i| &self.rules[i]).collect())
            .unwrap_or_default()
    }

    /// Derive a property value for an entity.
    ///
    /// Returns `Ok(None)` if no rules apply to this entity/property.
    /// Returns `Ok(Some(value))` with the composed result if rules apply.
    /// Returns `Err(DerivationError::CircularDependency)` if a cycle is detected.
    ///
    /// If caching is enabled, results are cached for subsequent calls.
    pub fn derive(
        &self,
        world: &World,
        entity: EntityId,
        component: impl Into<ComponentTypeId>,
    ) -> Result<Option<Value>, DerivationError> {
        let component = component.into();

        // Check cache first (if enabled)
        if self.caching_enabled {
            if let Some(entry) = self.cache.borrow().get(entity, component) {
                return Ok(Some(entry.value.clone()));
            }
        }

        // Check for circular dependency
        {
            let stack = self.computing_stack.borrow();
            if stack.contains(&(entity, component)) {
                // Found a cycle - return the path
                let mut cycle: Vec<_> = stack
                    .iter()
                    .skip_while(|&&(e, c)| e != entity || c != component)
                    .cloned()
                    .collect();
                cycle.push((entity, component));
                return Err(DerivationError::CircularDependency { cycle });
            }
        }

        // Push onto computing stack
        self.computing_stack.borrow_mut().push((entity, component));

        // Evaluate rules
        let result = self.derive_inner(world, entity, component);

        // Pop from computing stack
        self.computing_stack.borrow_mut().pop();

        // Cache the result (if enabled and successful)
        if self.caching_enabled {
            if let Ok(Some(ref value)) = result {
                // For now, we don't track dependencies automatically.
                // This will be enhanced when we add dependency tracking to ValueFn.
                let entry = CacheEntry::new(value.clone(), vec![], world.tick());
                self.cache.borrow_mut().insert(entity, component, entry);
            }
        }

        result
    }

    /// Derive a property value with explicit dependency tracking.
    ///
    /// The provided dependencies are recorded for cache invalidation.
    pub fn derive_with_dependencies(
        &self,
        world: &World,
        entity: EntityId,
        component: impl Into<ComponentTypeId>,
        dependencies: Vec<Dependency>,
    ) -> Result<Option<Value>, DerivationError> {
        let component = component.into();

        // Check cache first (if enabled)
        if self.caching_enabled {
            if let Some(entry) = self.cache.borrow().get(entity, component) {
                return Ok(Some(entry.value.clone()));
            }
        }

        // Check for circular dependency
        {
            let stack = self.computing_stack.borrow();
            if stack.contains(&(entity, component)) {
                let mut cycle: Vec<_> = stack
                    .iter()
                    .skip_while(|&&(e, c)| e != entity || c != component)
                    .cloned()
                    .collect();
                cycle.push((entity, component));
                return Err(DerivationError::CircularDependency { cycle });
            }
        }

        // Push onto computing stack
        self.computing_stack.borrow_mut().push((entity, component));

        // Evaluate rules
        let result = self.derive_inner(world, entity, component);

        // Pop from computing stack
        self.computing_stack.borrow_mut().pop();

        // Cache with dependencies
        if self.caching_enabled {
            if let Ok(Some(ref value)) = result {
                let entry = CacheEntry::new(value.clone(), dependencies, world.tick());
                self.cache.borrow_mut().insert(entity, component, entry);
            }
        }

        result
    }

    /// Notify the engine that a component changed.
    ///
    /// This invalidates any cached values that depend on this component.
    pub fn notify_component_changed(
        &self,
        entity: EntityId,
        component: impl Into<ComponentTypeId>,
    ) {
        self.cache
            .borrow_mut()
            .invalidate_for_component(entity, component);
    }

    /// Notify the engine that a relation changed.
    ///
    /// This invalidates any cached values that depend on this relation.
    pub fn notify_relation_changed(&self, entity: EntityId, relation: impl Into<RelationTypeId>) {
        self.cache
            .borrow_mut()
            .invalidate_for_relation(entity, relation);
    }

    /// Inner derivation logic (after cycle check).
    fn derive_inner(
        &self,
        world: &World,
        entity: EntityId,
        component: ComponentTypeId,
    ) -> Result<Option<Value>, DerivationError> {
        let rule_indices = match self.rules_by_component.get(&component) {
            Some(indices) => indices,
            None => return Ok(None), // No rules for this component
        };

        // Collect contributions from matching rules
        let mut contributions: Vec<Contribution> = Vec::new();

        for &index in rule_indices {
            let rule = &self.rules[index];

            // Check if the pattern matches
            if rule.matches(world, entity) {
                // Compute the value
                let value = rule.compute(world, entity);
                contributions.push(Contribution::new(value, rule.composition, rule.priority));
            }
        }

        // Compose contributions
        Ok(compose_values(contributions))
    }

    /// Clear the computing stack (for testing/recovery).
    #[cfg(test)]
    fn clear_computing_stack(&self) {
        self.computing_stack.borrow_mut().clear();
    }
}

impl Default for DerivationEngine {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for DerivationEngine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DerivationEngine")
            .field("rules", &self.rules.len())
            .field("derived_components", &self.derived_components)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::{Cardinality, RelationSchema};
    use crate::rules::Pattern;
    use std::sync::Arc;

    fn setup_world() -> World {
        let mut world = World::new();

        // Register Location relation (many-to-one)
        world.register_relation(RelationSchema::new(
            "Location",
            Cardinality::Many,
            Cardinality::One,
        ));

        // Create a volcanic biome
        let biome = world.create_entity();
        world.set_component(biome, "Name", "Volcanic Wastes");
        world.set_component(biome, "Terrain", "volcanic");

        // Create a creature in the volcanic biome
        let creature = world.create_entity();
        world.set_component(creature, "Name", "Fire Salamander");
        world.set_component(creature, "IsCreature", true);
        world.add_relation("Location", creature, biome);

        // Create a creature NOT in volcanic biome
        let forest_creature = world.create_entity();
        world.set_component(forest_creature, "Name", "Forest Deer");
        world.set_component(forest_creature, "IsCreature", true);

        world
    }

    #[test]
    fn test_engine_creation() {
        let engine = DerivationEngine::new();
        assert!(engine.rules().is_empty());
        assert!(!engine.is_derived("FireResistance"));
    }

    #[test]
    fn test_add_rule_marks_component_derived() {
        let mut engine = DerivationEngine::new();

        let rule = DerivationRule::new(
            "fire-resistance-rule",
            Pattern::has_component("?e", "IsCreature"),
            DerivedProperty::new("FireResistance"),
            Arc::new(|_, _| Value::Float(0.3)),
        );

        engine.add_rule(rule);

        assert!(engine.is_derived("FireResistance"));
        assert_eq!(engine.rules().len(), 1);
    }

    #[test]
    fn test_derive_single_rule() {
        let world = setup_world();
        let mut engine = DerivationEngine::new();

        // Rule: all creatures get base fire resistance
        let rule = DerivationRule::new(
            "base-fire-resistance",
            Pattern::has_component("?e", "IsCreature"),
            DerivedProperty::new("FireResistance"),
            Arc::new(|_, _| Value::Float(0.1)),
        );
        engine.add_rule(rule);

        // Creature (entity 1) should get fire resistance
        let creature = EntityId::from_raw(1);
        let result = engine.derive(&world, creature, "FireResistance").unwrap();
        assert_eq!(result, Some(Value::Float(0.1)));

        // Biome (entity 0) should not match
        let biome = EntityId::from_raw(0);
        let result = engine.derive(&world, biome, "FireResistance").unwrap();
        assert_eq!(result, None);
    }

    #[test]
    fn test_derive_multiple_rules_add() {
        let world = setup_world();
        let mut engine = DerivationEngine::new();

        // Rule 1: base fire resistance for creatures
        engine.add_rule(
            DerivationRule::new(
                "base-fire-resistance",
                Pattern::has_component("?e", "IsCreature"),
                DerivedProperty::new("FireResistance"),
                Arc::new(|_, _| Value::Float(0.1)),
            )
            .with_composition(ComposeMode::Add),
        );

        // Rule 2: bonus fire resistance for creatures in volcanic terrain
        // (simplified: check if creature has location relation)
        engine.add_rule(
            DerivationRule::new(
                "volcanic-fire-resistance",
                Pattern::and(vec![
                    Pattern::has_component("?e", "IsCreature"),
                    Pattern::in_relation("Location", "?e", "?_"),
                ]),
                DerivedProperty::new("FireResistance"),
                Arc::new(|_, _| Value::Float(0.3)),
            )
            .with_composition(ComposeMode::Add),
        );

        // Fire Salamander (entity 1) is in volcanic biome: 0.1 + 0.3 = 0.4
        let salamander = EntityId::from_raw(1);
        let result = engine.derive(&world, salamander, "FireResistance").unwrap();
        assert_eq!(result, Some(Value::Float(0.4)));

        // Forest Deer (entity 2) has no location: only base 0.1
        let deer = EntityId::from_raw(2);
        let result = engine.derive(&world, deer, "FireResistance").unwrap();
        assert_eq!(result, Some(Value::Float(0.1)));
    }

    #[test]
    fn test_derive_multiple_rules_override() {
        let world = setup_world();
        let mut engine = DerivationEngine::new();

        // Low priority override
        engine.add_rule(
            DerivationRule::new(
                "default-speed",
                Pattern::has_component("?e", "IsCreature"),
                DerivedProperty::new("Speed"),
                Arc::new(|_, _| Value::Int(10)),
            )
            .with_composition(ComposeMode::Override)
            .with_priority(1),
        );

        // High priority override
        engine.add_rule(
            DerivationRule::new(
                "fast-creature-speed",
                Pattern::has_component("?e", "IsCreature"),
                DerivedProperty::new("Speed"),
                Arc::new(|_, _| Value::Int(20)),
            )
            .with_composition(ComposeMode::Override)
            .with_priority(10),
        );

        let creature = EntityId::from_raw(1);
        let result = engine.derive(&world, creature, "Speed").unwrap();
        // Priority 10 wins
        assert_eq!(result, Some(Value::Int(20)));
    }

    #[test]
    fn test_derive_no_matching_rules() {
        let world = setup_world();
        let mut engine = DerivationEngine::new();

        // Rule that won't match anything
        engine.add_rule(DerivationRule::new(
            "magic-rule",
            Pattern::has_component("?e", "IsMagical"),
            DerivedProperty::new("MagicPower"),
            Arc::new(|_, _| Value::Int(100)),
        ));

        let creature = EntityId::from_raw(1);
        let result = engine.derive(&world, creature, "MagicPower").unwrap();
        assert_eq!(result, None);
    }

    #[test]
    fn test_derive_unknown_component() {
        let world = setup_world();
        let engine = DerivationEngine::new();

        let creature = EntityId::from_raw(1);
        let result = engine.derive(&world, creature, "UnknownComponent").unwrap();
        assert_eq!(result, None);
    }

    #[test]
    fn test_circular_dependency_detection() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "Name", "Test");

        let engine = DerivationEngine::new();

        // Manually create a cycle by pushing to the computing stack
        engine
            .computing_stack
            .borrow_mut()
            .push((entity, "A".into()));
        engine
            .computing_stack
            .borrow_mut()
            .push((entity, "B".into()));

        // Try to derive "A" again - should detect cycle
        let result = engine.derive(&world, entity, "A");

        assert!(result.is_err());
        if let Err(DerivationError::CircularDependency { cycle }) = result {
            assert!(cycle.len() >= 2);
            assert_eq!(cycle.first().unwrap().1.0.as_str(), "A");
        }

        engine.clear_computing_stack();
    }

    #[test]
    fn test_get_rule_by_name() {
        let mut engine = DerivationEngine::new();

        engine.add_rule(DerivationRule::new(
            "my-rule",
            Pattern::entity("?e"),
            DerivedProperty::new("Test"),
            Arc::new(|_, _| Value::Int(1)),
        ));

        assert!(engine.get_rule("my-rule").is_some());
        assert!(engine.get_rule("nonexistent").is_none());
    }

    #[test]
    fn test_rules_for_component() {
        let mut engine = DerivationEngine::new();

        engine.add_rule(DerivationRule::new(
            "rule-1",
            Pattern::entity("?e"),
            DerivedProperty::new("ComponentA"),
            Arc::new(|_, _| Value::Int(1)),
        ));

        engine.add_rule(DerivationRule::new(
            "rule-2",
            Pattern::entity("?e"),
            DerivedProperty::new("ComponentA"),
            Arc::new(|_, _| Value::Int(2)),
        ));

        engine.add_rule(DerivationRule::new(
            "rule-3",
            Pattern::entity("?e"),
            DerivedProperty::new("ComponentB"),
            Arc::new(|_, _| Value::Int(3)),
        ));

        let rules_a = engine.rules_for_component("ComponentA");
        assert_eq!(rules_a.len(), 2);

        let rules_b = engine.rules_for_component("ComponentB");
        assert_eq!(rules_b.len(), 1);

        let rules_c = engine.rules_for_component("ComponentC");
        assert!(rules_c.is_empty());
    }

    #[test]
    fn test_derive_with_world_access() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "BaseHP", 100_i64);
        world.set_component(entity, "Level", 5_i64);

        let mut engine = DerivationEngine::new();

        // Derive EffectiveHP = BaseHP + (Level * 10)
        engine.add_rule(DerivationRule::new(
            "effective-hp",
            Pattern::and(vec![
                Pattern::has_component("?e", "BaseHP"),
                Pattern::has_component("?e", "Level"),
            ]),
            DerivedProperty::new("EffectiveHP"),
            Arc::new(|w, e| {
                let base = w
                    .get_component(e, "BaseHP")
                    .and_then(|v| v.as_int())
                    .unwrap_or(0);
                let level = w
                    .get_component(e, "Level")
                    .and_then(|v| v.as_int())
                    .unwrap_or(0);
                Value::Int(base + level * 10)
            }),
        ));

        let result = engine.derive(&world, entity, "EffectiveHP").unwrap();
        // 100 + (5 * 10) = 150
        assert_eq!(result, Some(Value::Int(150)));
    }

    #[test]
    fn test_caching_enabled_by_default() {
        let engine = DerivationEngine::new();
        assert!(engine.caching_enabled());
    }

    #[test]
    fn test_caching_stores_result() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "Name", "test");

        let mut engine = DerivationEngine::new();
        engine.add_rule(DerivationRule::new(
            "test-rule",
            Pattern::has_component("?e", "Name"),
            DerivedProperty::new("Derived"),
            Arc::new(|_, _| Value::Int(42)),
        ));

        // Initial cache should be empty
        assert_eq!(engine.cache_stats(), (0, 0));

        // First derivation computes and caches
        let result1 = engine.derive(&world, entity, "Derived").unwrap();
        assert_eq!(result1, Some(Value::Int(42)));

        // Cache should now have one entry
        assert_eq!(engine.cache_stats(), (1, 1));

        // Second derivation returns cached value
        let result2 = engine.derive(&world, entity, "Derived").unwrap();
        assert_eq!(result2, Some(Value::Int(42)));

        // Still just one entry
        assert_eq!(engine.cache_stats(), (1, 1));
    }

    #[test]
    fn test_caching_can_be_disabled() {
        let engine = DerivationEngine::new_without_cache();
        assert!(!engine.caching_enabled());
    }

    #[test]
    fn test_cache_invalidation_via_notify() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "HP", 100_i64);

        let mut engine = DerivationEngine::new();
        engine.add_rule(DerivationRule::new(
            "derived-hp",
            Pattern::has_component("?e", "HP"),
            DerivedProperty::new("EffectiveHP"),
            Arc::new(|w, e| {
                let hp = w
                    .get_component(e, "HP")
                    .and_then(|v| v.as_int())
                    .unwrap_or(0);
                Value::Int(hp * 2)
            }),
        ));

        // Derive with dependencies
        let deps = vec![super::Dependency::component(entity, "HP")];
        let result1 = engine
            .derive_with_dependencies(&world, entity, "EffectiveHP", deps)
            .unwrap();
        assert_eq!(result1, Some(Value::Int(200)));
        assert_eq!(engine.cache_stats(), (1, 1));

        // Notify HP changed
        engine.notify_component_changed(entity, "HP");

        // Cache entry should be invalid now
        assert_eq!(engine.cache_stats(), (1, 0));

        // Update the world and derive again
        world.set_component(entity, "HP", 150_i64);
        let deps2 = vec![super::Dependency::component(entity, "HP")];
        let result2 = engine
            .derive_with_dependencies(&world, entity, "EffectiveHP", deps2)
            .unwrap();
        assert_eq!(result2, Some(Value::Int(300)));
    }

    #[test]
    fn test_clear_cache() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "Name", "test");

        let mut engine = DerivationEngine::new();
        engine.add_rule(DerivationRule::new(
            "test-rule",
            Pattern::has_component("?e", "Name"),
            DerivedProperty::new("Derived"),
            Arc::new(|_, _| Value::Int(42)),
        ));

        engine.derive(&world, entity, "Derived").unwrap();
        assert_eq!(engine.cache_stats().0, 1);

        engine.clear_cache();
        assert_eq!(engine.cache_stats().0, 0);
    }
}
