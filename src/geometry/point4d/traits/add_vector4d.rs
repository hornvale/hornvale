use crate::geometry::*;
use std::ops::Add;

/// Add<Vector4D> for Point4D
impl Add<Vector4D> for Point4D {
  type Output = Self;

  fn add(self, vector: Vector4D) -> Self {
    Self {
      w: self.w + vector.w,
      x: self.x + vector.x,
      y: self.y + vector.y,
      z: self.z + vector.z,
    }
  }
}
