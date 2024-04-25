use crate::world::prelude::*;

impl From<Room> for Vector4D {
  fn from(room: Room) -> Self {
    let Room { w, x, y, z } = room;
    Vector4D { w, x, y, z }
  }
}
