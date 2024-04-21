use crate::prelude::*;
use std::ops::Add;

impl Add<CorridorDirection> for Region {
  type Output = Region;

  fn add(self, direction: CorridorDirection) -> Self::Output {
    self + Vector4D::from(direction)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_add_corridor_direction() {
    init();
    let region = Region { w: 0, x: 0, y: 0, z: 0 };
    let result = region + CorridorDirection(Direction::North);
    assert_eq!(result, Region { w: 0, x: 0, y: 1, z: 0 });
  }
}
