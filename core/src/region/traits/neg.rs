use crate::prelude::*;
use std::ops::Neg;

impl Neg for Region {
  type Output = Region;

  fn neg(self) -> Self::Output {
    Region {
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
    let region = Region { x: 1, y: 2, z: 3 };
    assert_eq!(-region, Region { x: -1, y: -2, z: -3 });
  }
}
