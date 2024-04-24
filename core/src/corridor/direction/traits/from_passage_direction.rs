use crate::prelude::*;

impl From<PassageDirection> for CorridorDirection {
  fn from(direction: PassageDirection) -> Self {
    Self(direction.0)
  }
}
