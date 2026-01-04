//! Epoch tracking for cache invalidation.
//!
//! Epochs provide efficient cache invalidation without edge explosion.
//! Instead of tracking individual dependencies, we use epoch counters
//! for global state categories (zodiac, weather, tick, etc.).
//!
//! ## How It Works
//!
//! 1. Global state is categorized (e.g., "zodiac", "weather", "tick")
//! 2. Each category has an epoch counter
//! 3. When global state changes, bump the epoch counter
//! 4. Cached values store the epoch counters at computation time
//! 5. Cache is valid if all epoch counters match
//!
//! ## Example
//!
//! ```ignore
//! // Compute FireResistance, recording epochs
//! let epochs = world.current_epochs();
//! let value = compute_fire_resistance(entity);
//! cache.store(entity, :FireResistance, value, epochs);
//!
//! // Later, check if cache is still valid
//! if cache.epochs_match(world.current_epochs()) {
//!     return cache.value;
//! }
//! // Zodiac changed! Recompute...
//! ```

use crate::symbol::Symbol;
use im::OrdMap;
use std::fmt;

/// A collection of epoch counters.
///
/// Used to track which "version" of global state was used when
/// computing a cached value.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct EpochSnapshot {
    /// Map from epoch type to counter value
    epochs: OrdMap<Symbol, u64>,
}

impl EpochSnapshot {
    /// Create an empty epoch snapshot.
    pub fn new() -> Self {
        Self {
            epochs: OrdMap::new(),
        }
    }

    /// Create a snapshot from an iterator of (type, counter) pairs.
    pub fn from_pairs(iter: impl IntoIterator<Item = (Symbol, u64)>) -> Self {
        Self {
            epochs: iter.into_iter().collect(),
        }
    }

    /// Get the epoch counter for a given type.
    pub fn get(&self, epoch_type: Symbol) -> Option<u64> {
        self.epochs.get(&epoch_type).copied()
    }

    /// Check if this snapshot matches another snapshot.
    ///
    /// Returns true if all epoch types in `self` have the same
    /// counter value in `other`. Types not in `self` are ignored.
    pub fn matches(&self, other: &EpochSnapshot) -> bool {
        for (epoch_type, &counter) in &self.epochs {
            match other.epochs.get(epoch_type) {
                Some(&other_counter) if other_counter == counter => continue,
                _ => return false,
            }
        }
        true
    }

    /// Get an iterator over all epoch types and their counters.
    pub fn iter(&self) -> impl Iterator<Item = (Symbol, u64)> + '_ {
        self.epochs.iter().map(|(k, &v)| (*k, v))
    }

    /// Get the number of epoch types tracked.
    pub fn len(&self) -> usize {
        self.epochs.len()
    }

    /// Check if this snapshot is empty.
    pub fn is_empty(&self) -> bool {
        self.epochs.is_empty()
    }
}

impl fmt::Display for EpochSnapshot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        let mut first = true;
        for (epoch_type, counter) in &self.epochs {
            if !first {
                write!(f, ", ")?;
            }
            write!(f, "{epoch_type}: {counter}")?;
            first = false;
        }
        write!(f, "}}")
    }
}

/// Well-known epoch types.
pub mod epoch_types {
    use crate::symbol::Symbol;

    /// Epoch for the simulation tick.
    pub fn tick() -> Symbol {
        Symbol::new("tick")
    }

    /// Epoch for zodiac/celestial state.
    pub fn zodiac() -> Symbol {
        Symbol::new("zodiac")
    }

    /// Epoch for weather state.
    pub fn weather() -> Symbol {
        Symbol::new("weather")
    }

    /// Epoch for time of day.
    pub fn time_of_day() -> Symbol {
        Symbol::new("time_of_day")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_epoch_snapshot_new() {
        let snapshot = EpochSnapshot::new();
        assert!(snapshot.is_empty());
        assert_eq!(snapshot.len(), 0);
    }

    #[test]
    fn test_epoch_snapshot_from_pairs() {
        let tick = Symbol::new("tick");
        let zodiac = Symbol::new("zodiac");

        let snapshot = EpochSnapshot::from_pairs([(tick, 42), (zodiac, 7)]);

        assert_eq!(snapshot.get(tick), Some(42));
        assert_eq!(snapshot.get(zodiac), Some(7));
        assert_eq!(snapshot.len(), 2);
    }

    #[test]
    fn test_epoch_snapshot_matches() {
        let tick = Symbol::new("tick");
        let zodiac = Symbol::new("zodiac");
        let weather = Symbol::new("weather");

        let cached = EpochSnapshot::from_pairs([(tick, 42), (zodiac, 7)]);
        let current_match = EpochSnapshot::from_pairs([(tick, 42), (zodiac, 7), (weather, 3)]);
        let current_mismatch = EpochSnapshot::from_pairs([(tick, 43), (zodiac, 7)]);

        // Matches if all cached epochs have same value in current
        assert!(cached.matches(&current_match));

        // Doesn't match if any cached epoch differs
        assert!(!cached.matches(&current_mismatch));
    }

    #[test]
    fn test_epoch_snapshot_matches_empty() {
        let empty = EpochSnapshot::new();
        let non_empty = EpochSnapshot::from_pairs([(Symbol::new("tick"), 42)]);

        // Empty snapshot matches anything (no requirements)
        assert!(empty.matches(&non_empty));
        assert!(empty.matches(&empty));
    }

    #[test]
    fn test_epoch_snapshot_display() {
        let tick = Symbol::new("tick");
        let snapshot = EpochSnapshot::from_pairs([(tick, 42)]);

        let display = format!("{snapshot}");
        assert!(display.contains("tick"));
        assert!(display.contains("42"));
    }
}
