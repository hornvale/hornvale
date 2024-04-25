use crate::world::prelude::*;

impl From<CorridorDirection> for PassageDirection {
  fn from(direction: CorridorDirection) -> Self {
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
      PassageDirection::from(CorridorDirection(Direction::Up)),
      PassageDirection(Direction::Up)
    );
  }
}
