use crate::prelude::*;

impl From<Direction> for CorridorDirection {
  fn from(direction: Direction) -> Self {
    CorridorDirection(direction)
  }
}

impl From<CorridorDirection> for Direction {
  fn from(corridor_direction: CorridorDirection) -> Self {
    corridor_direction.0
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_from_direction() {
    let direction = Direction::North;
    let corridor_direction = CorridorDirection::from(direction);
    let result = Direction::from(corridor_direction);
    assert_eq!(direction, result);
  }

  #[test]
  fn test_from_corridor_direction() {
    let corridor_direction = CorridorDirection(Direction::North);
    let result = Direction::from(corridor_direction);
    assert_eq!(Direction::North, result);
  }
}
