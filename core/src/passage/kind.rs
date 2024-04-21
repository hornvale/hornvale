use crate::prelude::*;
use serde::{Deserialize, Serialize};
use strum::Display;

/// Trait implementations.
pub mod traits;

/// A `PassageKind` determines if and how a passage can be traversed.
///
/// This allows us fine control over how the player can move between rooms.
#[derive(Clone, Debug, Display, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
pub enum PassageKind {
  /// Normal and essentially transparent to the player.
  /// - Room: The destination room.
  Default(Room),
  /// This is not actually a passage, but prints a message to the player.
  /// - String: The message to display.
  NoExit(String),
  /// A passage that traverses a corridor.
  /// - CorridorKind: The corridor kind (including region).
  Corridor(CorridorKind),
  /// A specific condition must be met to traverse the passage.
  /// - Room: The destination room.
  /// - PassageCondition: The condition that must be met.
  /// - String: The message to display if the condition is not met.
  Conditional(Room, PassageCondition, String),
}
