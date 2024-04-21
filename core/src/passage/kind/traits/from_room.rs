use crate::prelude::*;

impl From<Room> for PassageKind {
  fn from(room: Room) -> Self {
    PassageKind::Default(room)
  }
}
