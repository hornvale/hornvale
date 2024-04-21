use crate::prelude::*;
use std::ops::Neg;

impl Neg for CorridorDirection {
  type Output = CorridorDirection;

  fn neg(self) -> Self::Output {
    CorridorDirection(self.0.neg())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_neg() {
    let direction = Direction::North;
    let corridor_direction = CorridorDirection::from(direction);
    let result = -corridor_direction;
    assert_eq!(Direction::South, result.0);
  }
}
