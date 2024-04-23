use crate::prelude::CorridorDirection;
use crate::prelude::CorridorKind;
use derive_more::{Add, Neg, Sub};
use serde::{Deserialize, Serialize};
use strum::IntoEnumIterator;

/// Region generators are responsible for creating regions.
pub mod generator;
/// The collection of region generators.
pub mod generators;
/// The entity is a region.
pub mod is_a_region;

/// Regions are 3-dimensional grids of rooms.
///
/// They exist on a separate 3D grid from rooms and are used to manage the
/// loading and unloading of regions as the player moves around the world.
#[derive(Add, Clone, Copy, Debug, Default, Deserialize, Eq, Hash, Neg, Ord, PartialEq, PartialOrd, Serialize, Sub)]
pub struct Region {
  /// The x-coordinate of the region.
  pub x: i64,
  /// The y-coordinate of the region.
  pub y: i64,
  /// The z-coordinate of the region.
  pub z: i64,
}

impl Region {
  /// Get all corridors from the given region.
  pub fn get_corridors(&self) -> Vec<(CorridorDirection, CorridorKind)> {
    CorridorDirection::iter()
      .map(|direction| (direction, direction.get_corridor(*self)))
      .filter(|(_, corridor)| corridor.is_some())
      .map(|(direction, corridor)| (direction, corridor.unwrap()))
      .collect()
  }
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

impl From<CorridorKind> for Region {
  fn from(kind: CorridorKind) -> Self {
    match kind {
      CorridorKind::Default(region) => region,
      CorridorKind::Ascend(region) => region,
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;
  use strum::IntoEnumIterator;

  #[test]
  fn test_get_corridors() {
    init();
    let region = Region { x: 0, y: 0, z: 0 };
    let corridors = region.get_corridors();
    assert_eq!(corridors.len(), 6);
    assert_eq!(
      corridors[0],
      (
        CorridorDirection::North,
        CorridorKind::Default(Region { x: 0, y: 1, z: 0 })
      )
    );
    assert_eq!(
      corridors[1],
      (
        CorridorDirection::South,
        CorridorKind::Default(Region { x: 0, y: -1, z: 0 })
      )
    );
    assert_eq!(
      corridors[2],
      (
        CorridorDirection::East,
        CorridorKind::Default(Region { x: 1, y: 0, z: 0 })
      )
    );
    assert_eq!(
      corridors[3],
      (
        CorridorDirection::West,
        CorridorKind::Default(Region { x: -1, y: 0, z: 0 })
      )
    );
    assert_eq!(
      corridors[5],
      (
        CorridorDirection::Down,
        CorridorKind::Default(Region { x: 0, y: 0, z: -1 })
      )
    );
    assert_eq!(
      corridors[4],
      (CorridorDirection::Up, CorridorKind::Ascend(Region { x: 0, y: 0, z: 1 }))
    );
  }

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
