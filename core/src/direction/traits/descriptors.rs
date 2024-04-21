use crate::prelude::*;

/// Descriptors for directions.
pub trait DirectionDescriptors {
  /// Is this direction a cardinal direction?
  fn is_cardinal(&self) -> bool;
  /// Is this direction a diagonal direction?
  fn is_diagonal(&self) -> bool;
  /// Is this direction a vertical direction?
  fn is_vertical(&self) -> bool;
  /// Is this direction a horizontal direction?
  fn is_horizontal(&self) -> bool;
  /// Is this direction a positive direction?
  fn is_positive(&self) -> bool;
  /// Is this direction a negative direction?
  fn is_negative(&self) -> bool;
  /// Is this direction an in-out direction?
  fn is_in_out(&self) -> bool;
}

impl DirectionDescriptors for Direction {
  /// Is this direction a cardinal direction?
  fn is_cardinal(&self) -> bool {
    matches!(
      self,
      Direction::North | Direction::East | Direction::South | Direction::West
    )
  }

  /// Is this direction a diagonal direction?
  fn is_diagonal(&self) -> bool {
    matches!(
      self,
      Direction::Northeast | Direction::Southeast | Direction::Southwest | Direction::Northwest
    )
  }

  /// Is this direction a vertical direction?
  fn is_vertical(&self) -> bool {
    matches!(self, Direction::Up | Direction::Down)
  }

  /// Is this direction a horizontal direction?
  fn is_horizontal(&self) -> bool {
    matches!(
      self,
      Direction::North | Direction::East | Direction::South | Direction::West | Direction::In | Direction::Out
    )
  }

  /// Is this direction a positive direction?
  fn is_positive(&self) -> bool {
    matches!(
      self,
      Direction::North | Direction::Northeast | Direction::East | Direction::Southeast | Direction::Up | Direction::In
    )
  }

  /// Is this direction a negative direction?
  fn is_negative(&self) -> bool {
    matches!(
      self,
      Direction::South
        | Direction::Southwest
        | Direction::West
        | Direction::Northwest
        | Direction::Down
        | Direction::Out
    )
  }

  /// Is this direction an in-out direction?
  fn is_in_out(&self) -> bool {
    matches!(self, Direction::In | Direction::Out)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_is_cardinal() {
    assert_eq!(Direction::North.is_cardinal(), true);
    assert_eq!(Direction::Northeast.is_cardinal(), false);
    assert_eq!(Direction::Up.is_cardinal(), false);
    assert_eq!(Direction::Out.is_cardinal(), false);
  }

  #[test]
  fn test_is_diagonal() {
    assert_eq!(Direction::North.is_diagonal(), false);
    assert_eq!(Direction::Northeast.is_diagonal(), true);
    assert_eq!(Direction::Up.is_diagonal(), false);
    assert_eq!(Direction::In.is_diagonal(), false);
  }

  #[test]
  fn test_is_vertical() {
    assert_eq!(Direction::North.is_vertical(), false);
    assert_eq!(Direction::Up.is_vertical(), true);
    assert_eq!(Direction::In.is_vertical(), false);
  }

  #[test]
  fn test_is_horizontal() {
    assert_eq!(Direction::North.is_horizontal(), true);
    assert_eq!(Direction::Up.is_horizontal(), false);
    assert_eq!(Direction::In.is_horizontal(), true);
  }

  #[test]
  fn test_is_positive() {
    assert_eq!(Direction::North.is_positive(), true);
    assert_eq!(Direction::South.is_positive(), false);
    assert_eq!(Direction::Up.is_positive(), true);
    assert_eq!(Direction::Down.is_positive(), false);
    assert_eq!(Direction::In.is_positive(), true);
  }

  #[test]
  fn test_is_negative() {
    assert_eq!(Direction::North.is_negative(), false);
    assert_eq!(Direction::South.is_negative(), true);
    assert_eq!(Direction::Up.is_negative(), false);
    assert_eq!(Direction::Down.is_negative(), true);
    assert_eq!(Direction::In.is_negative(), false);
  }

  #[test]
  fn test_is_in_out() {
    assert_eq!(Direction::North.is_in_out(), false);
    assert_eq!(Direction::In.is_in_out(), true);
    assert_eq!(Direction::Out.is_in_out(), true);
    assert_eq!(Direction::Up.is_in_out(), false);
    assert_eq!(Direction::Down.is_in_out(), false);
  }
}
