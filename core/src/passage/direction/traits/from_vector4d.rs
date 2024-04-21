use crate::prelude::*;

impl From<Vector4D> for PassageDirection {
  fn from(vector: Vector4D) -> Self {
    PassageDirection(Direction::from(vector))
  }
}
