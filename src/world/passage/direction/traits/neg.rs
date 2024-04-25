use crate::world::prelude::*;
use std::ops::Neg;

impl Neg for PassageDirection {
  type Output = PassageDirection;

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
    assert_eq!(-PassageDirection(Direction::North), PassageDirection(Direction::South));
  }
}
