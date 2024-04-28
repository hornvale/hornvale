use crate::geometry::*;

impl From<Point4D> for Vector4D {
  fn from(point: Point4D) -> Self {
    Self {
      w: point.w,
      x: point.x,
      y: point.y,
      z: point.z,
    }
  }
}
