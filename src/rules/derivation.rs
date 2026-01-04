//! Derivation system for computed properties with caching.
//!
//! Derivations allow properties to be computed from rules rather than stored directly.
//! Multiple rules can contribute to a property's final value using composition modes.
//!
//! ## Example
//!
//! ```ignore
//! // A creature's fire resistance is derived from multiple sources:
//! // - Base race value (replace mode)
//! // - Volcanic biome bonus (add mode)
//! // - Enchanted ring bonus (add mode)
//!
//! let resistance = derivation_engine.derive(&world, creature, "FireResistance");
//! ```
//!
//! ## Caching
//!
//! Derived values are cached and invalidated based on epochs. When an epoch
//! (tick, zodiac cycle, etc.) changes, caches that depend on it are invalidated.

use crate::core::{EntityId, Value};
use crate::symbol::Symbol;
use im::OrdMap;
use std::sync::Arc;

/// How multiple derivation rule values should be combined.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ComposeMode {
    /// Replace any previous value (last wins).
    #[default]
    Replace,
    /// Add to the accumulated value.
    Add,
    /// Multiply with the accumulated value.
    Multiply,
    /// Take the maximum of accumulated and new value.
    Max,
    /// Take the minimum of accumulated and new value.
    Min,
}

impl ComposeMode {
    /// Parse a compose mode from a symbol/keyword.
    pub fn from_symbol(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "replace" => Some(ComposeMode::Replace),
            "add" => Some(ComposeMode::Add),
            "multiply" | "mul" => Some(ComposeMode::Multiply),
            "max" => Some(ComposeMode::Max),
            "min" => Some(ComposeMode::Min),
            _ => None,
        }
    }
}

/// A value contribution from a derivation rule.
#[derive(Debug, Clone)]
pub struct DerivedValue {
    /// The value contributed by this rule.
    pub value: Value,
    /// How this value should be composed with others.
    pub compose_mode: ComposeMode,
    /// Priority for ordering (higher = later, can override).
    pub priority: i32,
}

impl DerivedValue {
    /// Create a new derived value.
    pub fn new(value: Value, compose_mode: ComposeMode, priority: i32) -> Self {
        Self {
            value,
            compose_mode,
            priority,
        }
    }

    /// Create a derived value with default compose mode (Replace) and priority (0).
    pub fn simple(value: Value) -> Self {
        Self {
            value,
            compose_mode: ComposeMode::Replace,
            priority: 0,
        }
    }
}

/// Epochs that can trigger cache invalidation.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Epochs {
    /// Current game tick.
    pub tick: u64,
    /// Other named epochs (e.g., "zodiac", "season").
    pub named: OrdMap<Symbol, u64>,
}

impl Epochs {
    /// Create epochs with just a tick value.
    pub fn new(tick: u64) -> Self {
        Self {
            tick,
            named: OrdMap::new(),
        }
    }

    /// Set a named epoch value.
    pub fn with_named(mut self, name: impl Into<Symbol>, value: u64) -> Self {
        self.named.insert(name.into(), value);
        self
    }

    /// Check if any epoch has changed since a cached value was computed.
    pub fn is_newer_than(&self, other: &Epochs) -> bool {
        if self.tick > other.tick {
            return true;
        }
        for (key, value) in &self.named {
            if let Some(other_value) = other.named.get(key) {
                if value > other_value {
                    return true;
                }
            } else {
                // New epoch type that didn't exist before
                return true;
            }
        }
        false
    }
}

/// A cached derivation result.
#[derive(Debug, Clone)]
pub struct CachedDerivation {
    /// The computed value.
    pub value: Value,
    /// Epochs when this value was computed.
    pub computed_at: Epochs,
    /// Dependencies that could invalidate this cache.
    pub dependencies: Vec<(EntityId, Symbol)>,
}

/// Cache for derived property values.
#[derive(Debug, Clone, Default)]
pub struct DerivationCache {
    /// Cached values indexed by (entity, property).
    entries: OrdMap<(EntityId, Symbol), CachedDerivation>,
}

impl DerivationCache {
    /// Create a new empty cache.
    pub fn new() -> Self {
        Self::default()
    }

    /// Get a cached derivation if it exists and is still valid.
    pub fn get(
        &self,
        entity: EntityId,
        property: Symbol,
        current_epochs: &Epochs,
    ) -> Option<&Value> {
        self.entries.get(&(entity, property)).and_then(|cached| {
            if current_epochs.is_newer_than(&cached.computed_at) {
                None // Cache is stale
            } else {
                Some(&cached.value)
            }
        })
    }

    /// Store a computed derivation in the cache.
    pub fn set(
        &mut self,
        entity: EntityId,
        property: Symbol,
        value: Value,
        epochs: Epochs,
        dependencies: Vec<(EntityId, Symbol)>,
    ) {
        self.entries.insert(
            (entity, property),
            CachedDerivation {
                value,
                computed_at: epochs,
                dependencies,
            },
        );
    }

    /// Invalidate all cache entries for an entity.
    pub fn invalidate_entity(&mut self, entity: EntityId) {
        // Collect keys to remove (im::OrdMap doesn't have retain)
        let keys_to_remove: Vec<_> = self
            .entries
            .keys()
            .filter(|(e, _)| *e == entity)
            .cloned()
            .collect();
        for key in keys_to_remove {
            self.entries.remove(&key);
        }
    }

    /// Invalidate a specific cache entry.
    pub fn invalidate(&mut self, entity: EntityId, property: Symbol) {
        self.entries.remove(&(entity, property));
    }

    /// Clear all cached entries.
    pub fn clear(&mut self) {
        self.entries.clear();
    }

    /// Get the number of cached entries.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Check if the cache is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

/// Compose multiple derived values into a final result.
///
/// Values are sorted by priority and then composed according to their modes.
pub fn compose_values(mut values: Vec<DerivedValue>) -> Value {
    if values.is_empty() {
        return Value::Nil;
    }

    // Sort by priority (lower first, so higher priority can override)
    values.sort_by_key(|v| v.priority);

    let mut result: Option<Value> = None;

    for derived in values {
        result = Some(match derived.compose_mode {
            ComposeMode::Replace => derived.value,
            ComposeMode::Add => match (result, &derived.value) {
                (Some(Value::Int(a)), Value::Int(b)) => Value::Int(a + b),
                (Some(Value::Float(a)), Value::Float(b)) => Value::Float(a + b),
                (Some(Value::Float(a)), Value::Int(b)) => Value::Float(a + *b as f64),
                (Some(Value::Int(a)), Value::Float(b)) => Value::Float(a as f64 + b),
                (None, v) => v.clone(),
                (Some(v), _) => v, // Can't add, keep previous
            },
            ComposeMode::Multiply => match (result, &derived.value) {
                (Some(Value::Int(a)), Value::Int(b)) => Value::Int(a * b),
                (Some(Value::Float(a)), Value::Float(b)) => Value::Float(a * b),
                (Some(Value::Float(a)), Value::Int(b)) => Value::Float(a * *b as f64),
                (Some(Value::Int(a)), Value::Float(b)) => Value::Float(a as f64 * b),
                (None, v) => v.clone(),
                (Some(v), _) => v, // Can't multiply, keep previous
            },
            ComposeMode::Max => match (result, &derived.value) {
                (Some(Value::Int(a)), Value::Int(b)) => Value::Int(a.max(*b)),
                (Some(Value::Float(a)), Value::Float(b)) => Value::Float(a.max(*b)),
                (None, v) => v.clone(),
                (Some(v), _) => v,
            },
            ComposeMode::Min => match (result, &derived.value) {
                (Some(Value::Int(a)), Value::Int(b)) => Value::Int(a.min(*b)),
                (Some(Value::Float(a)), Value::Float(b)) => Value::Float(a.min(*b)),
                (None, v) => v.clone(),
                (Some(v), _) => v,
            },
        });
    }

    result.unwrap_or(Value::Nil)
}

/// A derivation rule that contributes a value to a derived property.
#[derive(Debug, Clone)]
pub struct DerivationRule {
    /// Name of this derivation rule.
    pub name: Arc<str>,
    /// The property being derived.
    pub property: Symbol,
    /// The value to contribute.
    pub value: Value,
    /// How to compose with other values.
    pub compose_mode: ComposeMode,
    /// Priority for ordering.
    pub priority: i32,
}

impl DerivationRule {
    /// Create a new derivation rule.
    pub fn new(
        name: impl Into<Arc<str>>,
        property: impl Into<Symbol>,
        value: Value,
        compose_mode: ComposeMode,
        priority: i32,
    ) -> Self {
        Self {
            name: name.into(),
            property: property.into(),
            value,
            compose_mode,
            priority,
        }
    }

    /// Convert to a DerivedValue.
    pub fn to_derived_value(&self) -> DerivedValue {
        DerivedValue::new(self.value.clone(), self.compose_mode, self.priority)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compose_replace() {
        let values = vec![
            DerivedValue::new(Value::Int(10), ComposeMode::Replace, 0),
            DerivedValue::new(Value::Int(20), ComposeMode::Replace, 1),
        ];
        assert_eq!(compose_values(values), Value::Int(20));
    }

    #[test]
    fn test_compose_add() {
        let values = vec![
            DerivedValue::new(Value::Int(10), ComposeMode::Replace, 0),
            DerivedValue::new(Value::Int(5), ComposeMode::Add, 1),
            DerivedValue::new(Value::Int(3), ComposeMode::Add, 2),
        ];
        assert_eq!(compose_values(values), Value::Int(18));
    }

    #[test]
    fn test_compose_multiply() {
        let values = vec![
            DerivedValue::new(Value::Int(10), ComposeMode::Replace, 0),
            DerivedValue::new(Value::Int(2), ComposeMode::Multiply, 1),
        ];
        assert_eq!(compose_values(values), Value::Int(20));
    }

    #[test]
    fn test_compose_max() {
        let values = vec![
            DerivedValue::new(Value::Int(10), ComposeMode::Replace, 0),
            DerivedValue::new(Value::Int(25), ComposeMode::Max, 1),
            DerivedValue::new(Value::Int(15), ComposeMode::Max, 2),
        ];
        assert_eq!(compose_values(values), Value::Int(25));
    }

    #[test]
    fn test_compose_min() {
        let values = vec![
            DerivedValue::new(Value::Int(100), ComposeMode::Replace, 0),
            DerivedValue::new(Value::Int(25), ComposeMode::Min, 1),
            DerivedValue::new(Value::Int(50), ComposeMode::Min, 2),
        ];
        assert_eq!(compose_values(values), Value::Int(25));
    }

    #[test]
    fn test_compose_float() {
        let values = vec![
            DerivedValue::new(Value::Float(0.5), ComposeMode::Replace, 0),
            DerivedValue::new(Value::Float(0.3), ComposeMode::Add, 1),
        ];
        let result = compose_values(values);
        if let Value::Float(f) = result {
            assert!((f - 0.8).abs() < 0.001);
        } else {
            panic!("Expected float");
        }
    }

    #[test]
    fn test_compose_empty() {
        let values: Vec<DerivedValue> = vec![];
        assert_eq!(compose_values(values), Value::Nil);
    }

    #[test]
    fn test_compose_priority_ordering() {
        // Higher priority values are applied later
        let values = vec![
            DerivedValue::new(Value::Int(100), ComposeMode::Replace, 10), // Applied last
            DerivedValue::new(Value::Int(5), ComposeMode::Replace, 0),    // Applied first
        ];
        // Priority 0 sets 5, then priority 10 replaces with 100
        assert_eq!(compose_values(values), Value::Int(100));
    }

    #[test]
    fn test_epochs_is_newer_than() {
        let old = Epochs::new(100);
        let new = Epochs::new(101);
        assert!(new.is_newer_than(&old));
        assert!(!old.is_newer_than(&new));
        assert!(!old.is_newer_than(&old));
    }

    #[test]
    fn test_epochs_named() {
        let old = Epochs::new(100).with_named("zodiac", 5);
        let new = Epochs::new(100).with_named("zodiac", 6);
        assert!(new.is_newer_than(&old));
    }

    #[test]
    fn test_cache_basic() {
        let mut cache = DerivationCache::new();
        let entity = EntityId::from_raw(1);
        let property = Symbol::new("FireResistance");
        let epochs = Epochs::new(100);

        // Cache miss initially
        assert!(cache.get(entity, property, &epochs).is_none());

        // Store value
        cache.set(entity, property, Value::Float(0.5), epochs.clone(), vec![]);

        // Cache hit with same epochs
        assert_eq!(
            cache.get(entity, property, &epochs),
            Some(&Value::Float(0.5))
        );

        // Cache miss with newer epochs
        let newer = Epochs::new(101);
        assert!(cache.get(entity, property, &newer).is_none());
    }

    #[test]
    fn test_cache_invalidation() {
        let mut cache = DerivationCache::new();
        let entity = EntityId::from_raw(1);
        let property = Symbol::new("FireResistance");
        let epochs = Epochs::new(100);

        cache.set(entity, property, Value::Float(0.5), epochs.clone(), vec![]);
        assert!(cache.get(entity, property, &epochs).is_some());

        cache.invalidate(entity, property);
        assert!(cache.get(entity, property, &epochs).is_none());
    }

    #[test]
    fn test_compose_mode_from_symbol() {
        assert_eq!(ComposeMode::from_symbol("add"), Some(ComposeMode::Add));
        assert_eq!(ComposeMode::from_symbol("ADD"), Some(ComposeMode::Add));
        assert_eq!(
            ComposeMode::from_symbol("multiply"),
            Some(ComposeMode::Multiply)
        );
        assert_eq!(ComposeMode::from_symbol("mul"), Some(ComposeMode::Multiply));
        assert_eq!(ComposeMode::from_symbol("max"), Some(ComposeMode::Max));
        assert_eq!(ComposeMode::from_symbol("min"), Some(ComposeMode::Min));
        assert_eq!(
            ComposeMode::from_symbol("replace"),
            Some(ComposeMode::Replace)
        );
        assert_eq!(ComposeMode::from_symbol("unknown"), None);
    }
}
