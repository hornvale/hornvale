use crate::prelude::*;
use std::ops::Neg;

impl Neg for Vector4D {
  type Output = Self;

  fn neg(self) -> Self::Output {
    Vector4D {
      w: -self.w,
      x: -self.x,
      y: -self.y,
      z: -self.z,
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_neg() {
    init();
    let vector = Vector4D::from((1, 2, 3, 4));
    assert_eq!(-vector, Vector4D::from((-1, -2, -3, -4)));
  }
}
