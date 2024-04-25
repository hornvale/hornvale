use crate::world::prelude::*;

/// Various descriptors for Direction.
pub trait DirectionDescriptors {
  /// Returns the opposite direction.
  fn opposite(&self) -> Self;
  /// Is this direction cardinal?
  fn is_cardinal(&self) -> bool;
  /// Is this direction diagonal?
  fn is_diagonal(&self) -> bool;
  /// Is this direction vertical?
  fn is_vertical(&self) -> bool;
  /// Is this direction horizontal?
  fn is_horizontal(&self) -> bool;
  /// Is this direction in-out?
  fn is_in_out(&self) -> bool;
}

impl DirectionDescriptors for Direction {
  fn opposite(&self) -> Self {
    -*self
  }

  fn is_cardinal(&self) -> bool {
    matches!(
      self,
      Direction::North | Direction::East | Direction::South | Direction::West
    )
  }

  fn is_diagonal(&self) -> bool {
    matches!(
      self,
      Direction::Northeast | Direction::Southeast | Direction::Southwest | Direction::Northwest
    )
  }

  fn is_vertical(&self) -> bool {
    matches!(self, Direction::Up | Direction::Down)
  }

  fn is_horizontal(&self) -> bool {
    !self.is_vertical()
  }

  fn is_in_out(&self) -> bool {
    matches!(self, Direction::In | Direction::Out)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_opposite() {
    init();
    assert_eq!(Direction::North.opposite(), Direction::South);
  }

  #[test]
  fn test_is_cardinal() {
    init();
    assert!(Direction::North.is_cardinal());
    assert!(!Direction::Northeast.is_cardinal());
  }

  #[test]
  fn test_is_diagonal() {
    init();
    assert!(Direction::Northeast.is_diagonal());
    assert!(!Direction::North.is_diagonal());
  }

  #[test]
  fn test_is_vertical() {
    init();
    assert!(Direction::Up.is_vertical());
    assert!(!Direction::North.is_vertical());
  }

  #[test]
  fn test_is_horizontal() {
    init();
    assert!(Direction::North.is_horizontal());
    assert!(!Direction::Up.is_horizontal());
  }

  #[test]
  fn test_is_in_out() {
    init();
    assert!(Direction::In.is_in_out());
    assert!(!Direction::North.is_in_out());
  }
}
