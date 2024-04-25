use crate::world::prelude::*;

impl From<PassageDirection> for CorridorDirection {
  fn from(direction: PassageDirection) -> Self {
    Self(direction.0)
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
      CorridorDirection::from(PassageDirection(Direction::North)),
      CorridorDirection(Direction::North)
    );
  }
}
