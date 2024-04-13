use super::condition::PassageCondition;
use crate::prelude::Region;
use crate::prelude::Room;
use serde::{Deserialize, Serialize};

/// A `PassageKind` determines if and how a passage can be traversed.
///
/// This allows us fine control over how the player can move between rooms.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
pub enum PassageKind {
  /// Normal and essentially transparent to the player.
  /// - Room: The destination room.
  Default(Room),
  /// This is not actually a passage, but prints a message to the player.
  /// - String: The message to display.
  NoExit(String),
  /// A passage that traverses a corridor.
  /// - Region: The region of the corridor.
  Corridor(Region),
  /// A specific condition must be met to traverse the passage.
  /// - Room: The destination room.
  /// - PassageCondition: The condition that must be met.
  /// - String: The message to display if the condition is not met.
  Conditional(Room, PassageCondition, String),
}

impl From<Room> for PassageKind {
  fn from(room: Room) -> Self {
    PassageKind::Default(room)
  }
}
