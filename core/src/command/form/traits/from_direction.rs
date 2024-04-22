use crate::prelude::*;

impl From<Direction> for CommandForm {
  fn from(_direction: Direction) -> Self {
    CommandForm::Direction
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_from_direction() {
    init();
    assert_eq!(CommandForm::from(Direction::North), CommandForm::Direction);
  }
}
