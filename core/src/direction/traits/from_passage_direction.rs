use crate::prelude::*;

impl From<PassageDirection> for Direction {
  fn from(direction: PassageDirection) -> Self {
    direction.0
  }
}
