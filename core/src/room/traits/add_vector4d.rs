use crate::prelude::*;
use std::ops::Add;

impl Add<Vector4D> for Room {
  type Output = Room;

  fn add(self, vector: Vector4D) -> Self::Output {
    let Room { w, x, y, z } = self;
    let Vector4D {
      w: w2,
      x: x2,
      y: y2,
      z: z2,
    } = vector;
    Room {
      w: w + w2,
      x: x + x2,
      y: y + y2,
      z: z + z2,
    }
  }
}
