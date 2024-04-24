use crate::prelude::*;
use std::ops::Add;

impl Add<Vector4D> for Region {
  type Output = Region;

  fn add(self, vector: Vector4D) -> Self::Output {
    let Region { w, x, y, z } = self;
    let Vector4D {
      w: w2,
      x: x2,
      y: y2,
      z: z2,
    } = vector;
    Region {
      w: w + w2,
      x: x + x2,
      y: y + y2,
      z: z + z2,
    }
  }
}
