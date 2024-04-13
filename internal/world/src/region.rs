use crate::prelude::CorridorDirection;
use derive_more::{Add, Neg, Sub};
use serde::{Deserialize, Serialize};
use std::i64;

/// Regions are 3-dimensional grids of rooms.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Eq, Hash, Neg, Ord, PartialEq, PartialOrd, Serialize, Sub)]
pub struct Region {
  /// The x-coordinate of the region.
  pub x: i64,
  /// The y-coordinate of the region.
  pub y: i64,
  /// The z-coordinate of the region.
  pub z: i64,
}

impl From<CorridorDirection> for Region {
  fn from(direction: CorridorDirection) -> Self {
    match direction {
      CorridorDirection::North => Region { x: 0, y: 1, z: 0 },
      CorridorDirection::South => Region { x: 0, y: -1, z: 0 },
      CorridorDirection::East => Region { x: 1, y: 0, z: 0 },
      CorridorDirection::West => Region { x: -1, y: 0, z: 0 },
      CorridorDirection::Up => Region { x: 0, y: 0, z: 1 },
      CorridorDirection::Down => Region { x: 0, y: 0, z: -1 },
    }
  }
}

impl From<(i64, i64, i64)> for Region {
  fn from((x, y, z): (i64, i64, i64)) -> Self {
    Region { x, y, z }
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
      let region = Region::from(direction);
      match direction {
        CorridorDirection::North => assert_eq!(region, Region { x: 0, y: 1, z: 0 }),
        CorridorDirection::South => assert_eq!(region, Region { x: 0, y: -1, z: 0 }),
        CorridorDirection::East => assert_eq!(region, Region { x: 1, y: 0, z: 0 }),
        CorridorDirection::West => assert_eq!(region, Region { x: -1, y: 0, z: 0 }),
        CorridorDirection::Up => assert_eq!(region, Region { x: 0, y: 0, z: 1 }),
        CorridorDirection::Down => assert_eq!(region, Region { x: 0, y: 0, z: -1 }),
      }
    }
  }

  #[test]
  fn test_add() {
    init();
    let region = Region { x: 1, y: 2, z: 3 };
    let direction = CorridorDirection::North;
    let new_region = region + direction.into();
    assert_eq!(new_region, Region { x: 1, y: 3, z: 3 });
  }

  #[test]
  fn test_sub() {
    init();
    let region = Region { x: 1, y: 2, z: 3 };
    let direction = CorridorDirection::North;
    let new_region = region - direction.into();
    assert_eq!(new_region, Region { x: 1, y: 1, z: 3 });
  }

  #[test]
  fn test_neg() {
    init();
    let region = Region { x: 1, y: 2, z: 3 };
    let new_region = -region;
    assert_eq!(new_region, Region { x: -1, y: -2, z: -3 });
  }
}
