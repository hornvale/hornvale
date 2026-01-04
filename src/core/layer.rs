//! Entity layer classification.
//!
//! Layers provide stratification for the "everything is an entity" architecture.
//! Each layer has different mutation rules to ensure determinism and prevent chaos.
//!
//! ## The Four Layers
//!
//! - **Schema**: Type definitions, component schemas, relation schemas. Frozen after boot.
//! - **Meta**: Rules, patterns, effects, grammars. Frozen during tick, mutable between ticks.
//! - **World**: Game content (rooms, creatures, items). Mutable during tick, transactional.
//! - **Execution**: Ephemeral artifacts (Input, Command, Trace). Created/destroyed within tick.
//!
//! ## Invariants
//!
//! - Layer is immutable after entity creation (no `set_layer` method)
//! - Schema entities cannot be mutated after boot phase
//! - Meta entities cannot be mutated during tick (changes queue for next tick)
//! - Execution entities are cleaned up at end of tick unless archived

use std::fmt;

/// The layer an entity belongs to.
///
/// Layer determines when and how an entity can be mutated.
/// Once assigned at entity creation, layer cannot be changed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Layer {
    /// Type definitions and schemas. Frozen after boot.
    Schema,
    /// Rules, patterns, effects, grammars. Frozen during tick.
    Meta,
    /// Game content (rooms, creatures, items). Mutable during tick.
    World,
    /// Ephemeral execution artifacts. Cleared at end of tick.
    Execution,
}

impl Layer {
    /// Check if this layer is mutable during the given phase.
    pub fn is_mutable_in(&self, phase: super::Phase) -> bool {
        match (self, phase) {
            // Schema is only mutable during boot
            (Layer::Schema, super::Phase::Boot) => true,
            (Layer::Schema, _) => false,

            // Meta is mutable during boot and idle, but not during tick
            (Layer::Meta, super::Phase::Boot) => true,
            (Layer::Meta, super::Phase::Idle) => true,
            (Layer::Meta, super::Phase::Tick) => false,

            // World is always mutable (within transaction semantics)
            (Layer::World, _) => true,

            // Execution is always mutable (within tick lifecycle)
            (Layer::Execution, _) => true,
        }
    }

    /// Get the name of this layer as a string.
    pub fn name(&self) -> &'static str {
        match self {
            Layer::Schema => "Schema",
            Layer::Meta => "Meta",
            Layer::World => "World",
            Layer::Execution => "Execution",
        }
    }
}

impl fmt::Display for Layer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl Default for Layer {
    /// Default layer is World (for backward compatibility).
    fn default() -> Self {
        Layer::World
    }
}

/// Error returned when a layer mutation rule is violated.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LayerError {
    /// Attempted to mutate a Schema entity after boot phase.
    SchemaFrozen,
    /// Attempted to mutate a Meta entity during tick.
    MetaFrozenDuringTick,
    /// Attempted to change an entity's layer (layer is immutable).
    LayerImmutable,
}

impl fmt::Display for LayerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LayerError::SchemaFrozen => {
                write!(f, "cannot mutate Schema entity after boot phase")
            }
            LayerError::MetaFrozenDuringTick => {
                write!(
                    f,
                    "cannot mutate Meta entity during tick (queue for next tick)"
                )
            }
            LayerError::LayerImmutable => {
                write!(f, "entity layer cannot be changed after creation")
            }
        }
    }
}

impl std::error::Error for LayerError {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::Phase;

    #[test]
    fn test_schema_mutability() {
        assert!(Layer::Schema.is_mutable_in(Phase::Boot));
        assert!(!Layer::Schema.is_mutable_in(Phase::Idle));
        assert!(!Layer::Schema.is_mutable_in(Phase::Tick));
    }

    #[test]
    fn test_meta_mutability() {
        assert!(Layer::Meta.is_mutable_in(Phase::Boot));
        assert!(Layer::Meta.is_mutable_in(Phase::Idle));
        assert!(!Layer::Meta.is_mutable_in(Phase::Tick));
    }

    #[test]
    fn test_world_mutability() {
        assert!(Layer::World.is_mutable_in(Phase::Boot));
        assert!(Layer::World.is_mutable_in(Phase::Idle));
        assert!(Layer::World.is_mutable_in(Phase::Tick));
    }

    #[test]
    fn test_execution_mutability() {
        assert!(Layer::Execution.is_mutable_in(Phase::Boot));
        assert!(Layer::Execution.is_mutable_in(Phase::Idle));
        assert!(Layer::Execution.is_mutable_in(Phase::Tick));
    }

    #[test]
    fn test_default_layer() {
        assert_eq!(Layer::default(), Layer::World);
    }

    #[test]
    fn test_layer_ordering() {
        // Layers should have a defined ordering for determinism
        assert!(Layer::Schema < Layer::Meta);
        assert!(Layer::Meta < Layer::World);
        assert!(Layer::World < Layer::Execution);
    }
}
