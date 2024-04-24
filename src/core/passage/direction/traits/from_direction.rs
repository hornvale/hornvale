use crate::core::prelude::*;

impl From<CorridorDirection> for PassageDirection {
  fn from(direction: CorridorDirection) -> Self {
    Self(direction.0)
  }
}
