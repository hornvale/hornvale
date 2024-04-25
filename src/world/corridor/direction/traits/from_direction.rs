use crate::world::prelude::*;

impl From<Direction> for CorridorDirection {
  fn from(direction: Direction) -> Self {
    Self(direction)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_from() {
    init();
    assert_eq!(
      CorridorDirection::from(Direction::North),
      CorridorDirection(Direction::North)
    );
  }
}
