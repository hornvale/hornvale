use crate::prelude::*;
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

  #[test]
  fn test_neg() {
    let direction = Direction::North;
    let result = -direction;
    assert_eq!(Direction::South, result);
  }
}
