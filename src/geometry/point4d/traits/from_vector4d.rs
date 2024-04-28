use crate::geometry::*;

impl From<Vector4D> for Point4D {
  fn from(vector: Vector4D) -> Self {
    Self {
      w: vector.w,
      x: vector.x,
      y: vector.y,
      z: vector.z,
    }
  }
}
