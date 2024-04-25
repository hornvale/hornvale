use crate::world::prelude::*;

impl From<Direction> for PassageDirection {
  fn from(direction: Direction) -> Self {
    Self(direction)
  }
}
