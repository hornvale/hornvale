use derive_more::{Add, Neg, Sub};
use serde::{Deserialize, Serialize};

/// Rooms are the basic building blocks of the world.
///
/// They exist within a 3D grid associated with a region and are used to manage
/// the loading and unloading of rooms as the player moves around the world.
///
/// Because there may be multiple rooms at the same coordinates in different
/// regions, we use the region to disambiguate them, and we must be careful to
/// ensure that the region is always set when we query for entities.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Eq, Hash, Neg, Ord, PartialEq, PartialOrd, Serialize, Sub)]
pub struct Room {
  /// The x-coordinate of the room.
  pub x: i64,
  /// The y-coordinate of the room.
  pub y: i64,
  /// The z-coordinate of the room.
  pub z: i64,
}
