use crate::geometry::prelude_internal::*;
use std::ops::Neg;

impl Neg for Direction {
  type Output = Self;

  fn neg(self) -> Self::Output {
    use Direction::*;
    match self {
      North => South,
      Northeast => Southwest,
      East => West,
      Southeast => Northwest,
      South => North,
      Southwest => Northeast,
      West => East,
      Northwest => Southeast,
      Up => Down,
      Down => Up,
      In => Out,
      Out => In,
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_neg() {
    init();
    assert_eq!(-Direction::North, Direction::South);
  }
}
