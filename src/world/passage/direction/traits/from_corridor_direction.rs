use crate::world::prelude::*;

impl From<Direction> for PassageDirection {
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
    assert_eq!(PassageDirection::from(Direction::Up), PassageDirection(Direction::Up));
  }
}
