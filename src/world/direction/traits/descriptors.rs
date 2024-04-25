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
