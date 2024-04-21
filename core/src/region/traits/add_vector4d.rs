use crate::prelude::*;
use std::ops::Add;

impl Add<Vector4D> for Region {
  type Output = Region;

  fn add(self, rhs: Vector4D) -> Self::Output {
    Region {
      w: self.w + rhs.w,
      x: self.x + rhs.x,
      y: self.y + rhs.y,
      z: self.z + rhs.z,
    }
  }
}
