use derive_more::{Add, Neg, Sub};
use serde::{Deserialize, Serialize};

/// The entity is a room.
pub mod is_a_room;
/// Trait implementations.
pub mod traits;

/// Rooms are the basic building blocks of the world.
///
/// They exist within a 3D grid associated with a region and are used to manage
/// the loading and unloading of rooms as the player moves around the world.
///
/// In addition, there is a fourth dimension, `w`, which represents the
/// transition from the outside world to the inside world. This is used to
/// manage rooms, structures, vehicles, etc that are nested within other rooms.
///
/// Because there may be multiple rooms at the same coordinates in different
/// regions, we use the region to disambiguate them, and we must be careful to
/// ensure that the region is always set when we query for entities.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Eq, Hash, Neg, Ord, PartialEq, PartialOrd, Serialize, Sub)]
pub struct Room {
  /// The w-coordinate of the room (outside <-> inside).
  pub w: i64,
  /// The x-coordinate of the room (west <-> east).
  pub x: i64,
  /// The y-coordinate of the room (south <-> north).
  pub y: i64,
  /// The z-coordinate of the room (down <-> up).
  pub z: i64,
}
