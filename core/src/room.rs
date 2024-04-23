use crate::prelude::PassageDirection;
use derive_more::{Add, Neg, Sub};
use serde::{Deserialize, Serialize};

/// The entity is a room.
pub mod is_a_room;

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

impl Room {
  /// Get the room in the given direction.
  pub fn get(&self, direction: PassageDirection) -> Room {
    *self + direction.into()
  }
}

impl From<PassageDirection> for Room {
  fn from(direction: PassageDirection) -> Self {
    use PassageDirection::*;
    match direction {
      North => Room { w: 0, x: 0, y: 1, z: 0 },
      Northeast => Room { w: 0, x: 1, y: 1, z: 0 },
      East => Room { w: 0, x: 1, y: 0, z: 0 },
      Southeast => Room {
        w: 0,
        x: 1,
        y: -1,
        z: 0,
      },
      South => Room {
        w: 0,
        x: 0,
        y: -1,
        z: 0,
      },
      Southwest => Room {
        w: 0,
        x: -1,
        y: -1,
        z: 0,
      },
      West => Room {
        w: 0,
        x: -1,
        y: 0,
        z: 0,
      },
      Northwest => Room {
        w: 0,
        x: -1,
        y: 1,
        z: 0,
      },
      Up => Room { w: 0, x: 0, y: 0, z: 1 },
      Down => Room {
        w: 0,
        x: 0,
        y: 0,
        z: -1,
      },
      In => Room { w: 1, x: 0, y: 0, z: 0 },
      Out => Room {
        w: -1,
        x: 0,
        y: 0,
        z: 0,
      },
    }
  }
}

impl From<(i64, i64, i64, i64)> for Room {
  fn from((w, x, y, z): (i64, i64, i64, i64)) -> Self {
    Room { w, x, y, z }
  }
}

impl From<(i64, i64, i64)> for Room {
  fn from((x, y, z): (i64, i64, i64)) -> Self {
    Room { w: 0, x, y, z }
  }
}

impl From<Room> for (i64, i64, i64, i64) {
  fn from(room: Room) -> Self {
    (room.w, room.x, room.y, room.z)
  }
}

impl From<Room> for (i64, i64, i64) {
  fn from(room: Room) -> Self {
    (room.x, room.y, room.z)
  }
}
