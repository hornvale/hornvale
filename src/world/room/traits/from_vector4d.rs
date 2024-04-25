use crate::world::prelude::*;

impl From<Vector4D> for Room {
  fn from(vector: Vector4D) -> Self {
    let Vector4D { w, x, y, z } = vector;
    Room { w, x, y, z }
  }
}
