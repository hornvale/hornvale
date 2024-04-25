use crate::world::prelude::*;

impl From<Vector4D> for Region {
  fn from(vector: Vector4D) -> Self {
    let Vector4D { w, x, y, z } = vector;
    Region { w, x, y, z }
  }
}
