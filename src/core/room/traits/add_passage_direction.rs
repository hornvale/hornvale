use crate::core::prelude::*;
use std::ops::Add;

impl Add<PassageDirection> for Room {
  type Output = Room;

  fn add(self, direction: PassageDirection) -> Self::Output {
    self + Vector4D::from(direction.0)
  }
}
