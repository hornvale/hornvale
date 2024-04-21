use crate::prelude::*;
use std::ops::Add;

impl Add<Direction> for Room {
  type Output = Room;

  fn add(self, rhs: Direction) -> Self::Output {
    self + Vector4D::from(rhs)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_add() {
    let room = Room::from((1, 2, 4, 3));
    let direction = Direction::North;
    let result = room + direction;
    assert_eq!(Room::from((1, 2, 5, 3)), result);
  }
}
