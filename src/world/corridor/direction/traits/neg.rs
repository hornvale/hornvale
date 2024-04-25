use crate::world::prelude::*;
use std::ops::Neg;

impl Neg for CorridorDirection {
  type Output = Self;

  fn neg(self) -> Self::Output {
    Self(-self.0)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_neg() {
    init();
    assert_eq!(
      -CorridorDirection(Direction::North),
      CorridorDirection(Direction::South)
    );
  }
}
