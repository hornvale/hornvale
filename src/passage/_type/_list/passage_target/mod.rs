use crate::marker::PersistedEntity;
use crate::passage::PassageCondition;
use crate::passage::PassageResolver;

/// The `PassageTarget` enum.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum PassageTarget {
  /// None.
  None,
  /// Stub, meaning the passage is not yet connected to a room.
  Stub,
  /// A placeholder, meaning the passage is connected to a room, but the
  /// room is not yet generated.
  Placeholder,
  /// Message: this passage is passable and displays a custom message.
  PassableWithMessage {
    /// The message.
    message: String,
    /// The target.
    target: Box<PassageTarget>,
  },
  /// A message: this passage is impassable and displays a custom message.
  ImpassableWithMessage(String),
  /// Death: this passage is impassible and kills the player.
  DeathWithMessage(String),
  /// A room.
  Room(PersistedEntity),
  /// A room in another chunk.
  ChunkRoom(PersistedEntity, PersistedEntity),
  /// A room in another chunk in another plane.
  ChunkPlaneRoom(PersistedEntity, PersistedEntity, PersistedEntity),
  /// A conditional passage.
  Conditional {
    /// The condition.
    condition: PassageCondition,
    /// The target if the condition is true.
    true_target: Box<PassageTarget>,
    /// The target if the condition is false.
    false_target: Box<PassageTarget>,
  },
  Trapped {
    // I'll do this... soon.
    // effect: TrapEffect,
    target: Box<PassageTarget>, // Where the player ends up after the trap is triggered.
  },
  Dynamic {
    /// The resolver.
    resolver: PassageResolver,
  },
  TriggersEvent {
    /// The event.
    event: String,
    /// The target.
    target: Box<PassageTarget>,
  },
}
