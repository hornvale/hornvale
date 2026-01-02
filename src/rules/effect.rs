//! Effects that rules can produce.
//!
//! Effects are the actions taken when a rule fires.

use crate::core::{EntityId, World};
use crate::io::WorldIO;

/// An action taken when a rule fires.
#[derive(Debug, Clone)]
pub enum Effect {
    /// Emit a message to the output.
    EmitMessage(String),
    // Future: SetComponent, AddRelation, RemoveEntity, etc.
}

impl Effect {
    /// Create an EmitMessage effect.
    pub fn emit_message(message: impl Into<String>) -> Self {
        Effect::EmitMessage(message.into())
    }

    /// Execute this effect.
    ///
    /// The `entity` parameter is the entity that matched the rule's pattern.
    /// The `tick` is the current simulation tick.
    pub fn execute(&self, _world: &World, io: &mut dyn WorldIO, entity: EntityId, tick: u64) {
        match self {
            Effect::EmitMessage(message) => {
                io.println(&format!("[Tick {tick}] {message} (entity {entity})"));
            }
        }
    }

    /// Get a human-readable description of this effect.
    pub fn describe(&self) -> String {
        match self {
            Effect::EmitMessage(msg) => format!("emit \"{msg}\""),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::TestIO;

    #[test]
    fn test_emit_message_effect() {
        let world = World::new();
        let mut io = TestIO::new(vec![]);
        let entity = EntityId::from_raw(42);

        let effect = Effect::emit_message("Hello, world!");
        effect.execute(&world, &mut io, entity, 100);

        assert!(io.output.contains("Hello, world!"));
        assert!(io.output.contains("Tick 100"));
        assert!(io.output.contains("entity 42"));
    }

    #[test]
    fn test_effect_describe() {
        let effect = Effect::emit_message("Baa!");
        assert_eq!(effect.describe(), "emit \"Baa!\"");
    }
}
