use crate::core::prelude::*;
use std::ops::Add;

impl Add<CorridorDirection> for Region {
  type Output = Region;

  fn add(self, direction: CorridorDirection) -> Self::Output {
    self + Vector4D::from(direction.0)
  }
}
