use crate::core::prelude::*;

impl From<Direction> for CorridorDirection {
  fn from(direction: Direction) -> Self {
    Self(direction)
  }
}
