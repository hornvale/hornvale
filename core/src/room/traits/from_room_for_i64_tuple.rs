use crate::prelude::*;

impl From<Room> for (i64, i64, i64) {
  fn from(room: Room) -> Self {
    (room.x, room.y, room.z)
  }
}

impl From<Room> for (i64, i64, i64, i64) {
  fn from(room: Room) -> Self {
    (room.w, room.x, room.y, room.z)
  }
}
