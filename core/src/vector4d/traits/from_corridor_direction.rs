use crate::prelude::*;

impl From<CorridorDirection> for Vector4D {
  fn from(direction: CorridorDirection) -> Self {
    direction.0.into()
  }
}
