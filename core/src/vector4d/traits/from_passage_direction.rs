use crate::prelude::*;

impl From<PassageDirection> for Vector4D {
  fn from(direction: PassageDirection) -> Self {
    direction.0.into()
  }
}
