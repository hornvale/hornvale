use crate::prelude::*;

impl Add<Direction> for Region {
  type Output = Region;

  fn add(self, rhs: Direction) -> Self::Output {
    let vector = Vector4D::from(rhs);
    self + vector
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_add() {
    let region = Region::new(1, 2, 3, 4);
    let direction = Direction::North;
    let result = region + direction;
    assert_eq!(Region::new(1, 2, 4, 4), result);
  }
}
