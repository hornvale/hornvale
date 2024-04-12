use crate::prelude::CorridorDirection;
use derive_more::{Add, Div, Mul, Neg, Sub};
use serde::{Deserialize, Serialize};
use std::i64;

/// A three-dimensional point in the world.
#[derive(
  Add, Sub, Clone, Copy, Debug, Default, Deserialize, Div, Eq, Hash, Mul, Neg, Ord, PartialEq, PartialOrd, Serialize,
)]
pub struct Point {
  /// The x-coordinate of the point (west <-> east).
  pub x: i64,
  /// The y-coordinate of the point (south <-> north).
  pub y: i64,
  /// The z-coordinate of the point (down <-> up).
  pub z: i64,
}

impl From<CorridorDirection> for Point {
  fn from(direction: CorridorDirection) -> Self {
    match direction {
      CorridorDirection::North => Point { x: 0, y: 1, z: 0 },
      CorridorDirection::South => Point { x: 0, y: -1, z: 0 },
      CorridorDirection::East => Point { x: 1, y: 0, z: 0 },
      CorridorDirection::West => Point { x: -1, y: 0, z: 0 },
      CorridorDirection::Up => Point { x: 0, y: 0, z: 1 },
      CorridorDirection::Down => Point { x: 0, y: 0, z: -1 },
    }
  }
}

impl From<(i64, i64, i64)> for Point {
  fn from((x, y, z): (i64, i64, i64)) -> Self {
    Point { x, y, z }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;
  use strum::IntoEnumIterator;

  #[test]
  fn test_from_corridor_direction() {
    init();
    for direction in CorridorDirection::iter() {
      let point = Point::from(direction);
      match direction {
        CorridorDirection::North => assert_eq!(point, Point { x: 0, y: 1, z: 0 }),
        CorridorDirection::South => assert_eq!(point, Point { x: 0, y: -1, z: 0 }),
        CorridorDirection::East => assert_eq!(point, Point { x: 1, y: 0, z: 0 }),
        CorridorDirection::West => assert_eq!(point, Point { x: -1, y: 0, z: 0 }),
        CorridorDirection::Up => assert_eq!(point, Point { x: 0, y: 0, z: 1 }),
        CorridorDirection::Down => assert_eq!(point, Point { x: 0, y: 0, z: -1 }),
      }
    }
  }

  #[test]
  fn test_add() {
    init();
    let point = Point { x: 1, y: 2, z: 3 };
    let direction = CorridorDirection::North;
    let new_point = point + direction.into();
    assert_eq!(new_point, Point { x: 1, y: 3, z: 3 });
  }
}
