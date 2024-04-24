use crate::prelude::*;

impl From<Direction> for CorridorDirection {
  fn from(direction: Direction) -> Self {
    Self(direction)
  }
}
