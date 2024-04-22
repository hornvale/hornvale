use crate::prelude::*;
use std::ops::Neg;

impl Neg for Direction {
  type Output = Self;

  fn neg(self) -> Self::Output {
    self.opposite()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_neg() {
    let direction = Direction::North;
    let result = -direction;
    assert_eq!(Direction::South, result);
  }
}
