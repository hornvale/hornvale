use crate::geometry::*;
use std::ops::Sub;

/// Sub<Vector4D> for Point4D
impl Sub<Vector4D> for Point4D {
  type Output = Self;

  fn sub(self, vector: Vector4D) -> Self {
    Self {
      w: self.w - vector.w,
      x: self.x - vector.x,
      y: self.y - vector.y,
      z: self.z - vector.z,
    }
  }
}
