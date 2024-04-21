use crate::prelude::*;
use std::ops::Neg;

impl Neg for Room {
  type Output = Room;

  fn neg(self) -> Self::Output {
    Room {
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
    let room = Room { w: 1, x: 2, y: 3, z: 4 };
    assert_eq!(
      -room,
      Room {
        w: -1,
        x: -2,
        y: -3,
        z: -4
      }
    );
  }
}
