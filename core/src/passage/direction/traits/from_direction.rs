use crate::prelude::*;

impl From<Direction> for PassageDirection {
  fn from(direction: Direction) -> Self {
    PassageDirection(direction)
  }
}
