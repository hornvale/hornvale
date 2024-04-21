use crate::prelude::*;
use std::ops::Add;

impl Add<Vector4D> for Room {
  type Output = Room;

  fn add(self, rhs: Vector4D) -> Self::Output {
    Room::from((self.w + rhs.w, self.x + rhs.x, self.y + rhs.y, self.z + rhs.z))
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_add() {
    let room = Room::from((1, 2, 4, 3));
    let vector = Vector4D::from((1, 2, 1, 0));
    let result = room + vector;
    assert_eq!(Room::from((2, 4, 5, 3)), result);
  }
}
