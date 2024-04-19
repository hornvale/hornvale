use super::kind::CorridorKind;
use crate::prelude::PassageDirection;
use crate::prelude::Region;
use hornvale_command::prelude::*;
use serde::{Deserialize, Serialize};
use std::ops::Neg;
use strum::{Display, EnumIter};

/// A `CorridorDirection` is a direction in which a corridor can be built.
#[derive(Clone, Copy, Debug, Display, EnumIter, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
pub enum CorridorDirection {
  /// The `North` direction (y+1).
  North,
  /// The `South` direction (y-1).
  South,
  /// The `East` direction (x+1).
  East,
  /// The `West` direction (x-1).
  West,
  /// The `Up` direction (z+1).
  Up,
  /// The `Down` direction (z-1).
  Down,
}

impl CorridorDirection {
  /// Transform the given direction into a region.
  pub fn to_region(&self) -> Region {
    (*self).into()
  }

  /// Get the opposite direction of the given direction.
  pub fn opposite(&self) -> Self {
    match self {
      CorridorDirection::North => CorridorDirection::South,
      CorridorDirection::South => CorridorDirection::North,
      CorridorDirection::East => CorridorDirection::West,
      CorridorDirection::West => CorridorDirection::East,
      CorridorDirection::Up => CorridorDirection::Down,
      CorridorDirection::Down => CorridorDirection::Up,
    }
  }

  /// Build a corridor in the given direction from the given region.
  pub fn get_corridor(&self, region: Region) -> Option<CorridorKind> {
    use CorridorDirection::*;
    use CorridorKind::*;
    let destination = self.to_region() + region;
    match self {
      Up if region.z >= 0 => Some(Ascend(destination)),
      _ => Some(Default(destination)),
    }
  }
}

impl Neg for CorridorDirection {
  type Output = Self;

  fn neg(self) -> Self::Output {
    self.opposite()
  }
}

impl TryFrom<Region> for CorridorDirection {
  type Error = ();

  fn try_from(region: Region) -> Result<Self, Self::Error> {
    match (region.x, region.y, region.z) {
      (0, y, 0) if y > 0 => Ok(CorridorDirection::North),
      (0, y, 0) if y < 0 => Ok(CorridorDirection::South),
      (x, 0, 0) if x > 0 => Ok(CorridorDirection::East),
      (x, 0, 0) if x < 0 => Ok(CorridorDirection::West),
      (0, 0, z) if z > 0 => Ok(CorridorDirection::Up),
      (0, 0, z) if z < 0 => Ok(CorridorDirection::Down),
      _ => Err(()),
    }
  }
}

impl TryFrom<(Region, Region)> for CorridorDirection {
  type Error = ();

  fn try_from((from, to): (Region, Region)) -> Result<Self, Self::Error> {
    let region: Region = (to.x - from.x, to.y - from.y, to.z - from.z).into();
    CorridorDirection::try_from(region)
  }
}

impl TryFrom<PassageDirection> for CorridorDirection {
  type Error = ();

  fn try_from(direction: PassageDirection) -> Result<Self, Self::Error> {
    match direction {
      PassageDirection::North => Ok(CorridorDirection::North),
      PassageDirection::South => Ok(CorridorDirection::South),
      PassageDirection::East => Ok(CorridorDirection::East),
      PassageDirection::West => Ok(CorridorDirection::West),
      PassageDirection::Up => Ok(CorridorDirection::Up),
      PassageDirection::Down => Ok(CorridorDirection::Down),
      _ => Err(()),
    }
  }
}

impl TryFrom<CommandDirection> for CorridorDirection {
  type Error = ();

  fn try_from(direction: CommandDirection) -> Result<Self, Self::Error> {
    match direction {
      CommandDirection::North => Ok(CorridorDirection::North),
      CommandDirection::South => Ok(CorridorDirection::South),
      CommandDirection::East => Ok(CorridorDirection::East),
      CommandDirection::West => Ok(CorridorDirection::West),
      CommandDirection::Up => Ok(CorridorDirection::Up),
      CommandDirection::Down => Ok(CorridorDirection::Down),
      _ => Err(()),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_to_region() {
    init();
    assert_eq!(CorridorDirection::North.to_region(), Region { x: 0, y: 1, z: 0 });
    assert_eq!(CorridorDirection::South.to_region(), Region { x: 0, y: -1, z: 0 });
    assert_eq!(CorridorDirection::East.to_region(), Region { x: 1, y: 0, z: 0 });
    assert_eq!(CorridorDirection::West.to_region(), Region { x: -1, y: 0, z: 0 });
    assert_eq!(CorridorDirection::Up.to_region(), Region { x: 0, y: 0, z: 1 });
    assert_eq!(CorridorDirection::Down.to_region(), Region { x: 0, y: 0, z: -1 });
  }

  #[test]
  fn test_opposite() {
    init();
    assert_eq!(CorridorDirection::North.opposite(), CorridorDirection::South);
    assert_eq!(CorridorDirection::South.opposite(), CorridorDirection::North);
    assert_eq!(CorridorDirection::East.opposite(), CorridorDirection::West);
    assert_eq!(CorridorDirection::West.opposite(), CorridorDirection::East);
    assert_eq!(CorridorDirection::Up.opposite(), CorridorDirection::Down);
    assert_eq!(CorridorDirection::Down.opposite(), CorridorDirection::Up);
  }

  #[test]
  fn test_get_corridor() {
    init();
    assert_eq!(
      CorridorDirection::North.get_corridor(Region { x: 0, y: 0, z: 0 }),
      Some(CorridorKind::Default(Region { x: 0, y: 1, z: 0 }))
    );
    assert_eq!(
      CorridorDirection::South.get_corridor(Region { x: 0, y: 0, z: 0 }),
      Some(CorridorKind::Default(Region { x: 0, y: -1, z: 0 }))
    );
    assert_eq!(
      CorridorDirection::East.get_corridor(Region { x: 0, y: 0, z: 0 }),
      Some(CorridorKind::Default(Region { x: 1, y: 0, z: 0 }))
    );
    assert_eq!(
      CorridorDirection::West.get_corridor(Region { x: 0, y: 0, z: 0 }),
      Some(CorridorKind::Default(Region { x: -1, y: 0, z: 0 }))
    );
    assert_eq!(
      CorridorDirection::Up.get_corridor(Region { x: 0, y: 0, z: 0 }),
      Some(CorridorKind::Ascend(Region { x: 0, y: 0, z: 1 }))
    );
    assert_eq!(
      CorridorDirection::Down.get_corridor(Region { x: 0, y: 0, z: 0 }),
      Some(CorridorKind::Default(Region { x: 0, y: 0, z: -1 }))
    );
  }

  #[test]
  fn test_try_from_region() {
    init();
    assert_eq!(
      CorridorDirection::try_from(Region { x: 0, y: 1, z: 0 }),
      Ok(CorridorDirection::North)
    );
    assert_eq!(
      CorridorDirection::try_from(Region { x: 0, y: -1, z: 0 }),
      Ok(CorridorDirection::South)
    );
    assert_eq!(
      CorridorDirection::try_from(Region { x: 1, y: 0, z: 0 }),
      Ok(CorridorDirection::East)
    );
    assert_eq!(
      CorridorDirection::try_from(Region { x: -1, y: 0, z: 0 }),
      Ok(CorridorDirection::West)
    );
    assert_eq!(
      CorridorDirection::try_from(Region { x: 0, y: 0, z: 1 }),
      Ok(CorridorDirection::Up)
    );
    assert_eq!(
      CorridorDirection::try_from(Region { x: 0, y: 0, z: -1 }),
      Ok(CorridorDirection::Down)
    );
    assert_eq!(CorridorDirection::try_from(Region { x: 1, y: 1, z: 1 }), Err(()));
  }

  #[test]
  fn test_try_from_regions() {
    init();
    assert_eq!(
      CorridorDirection::try_from((Region { x: 0, y: 0, z: 0 }, Region { x: 0, y: 1, z: 0 })),
      Ok(CorridorDirection::North)
    );
    assert_eq!(
      CorridorDirection::try_from((Region { x: 0, y: 0, z: 0 }, Region { x: 0, y: -1, z: 0 })),
      Ok(CorridorDirection::South)
    );
    assert_eq!(
      CorridorDirection::try_from((Region { x: 0, y: 0, z: 0 }, Region { x: 1, y: 0, z: 0 })),
      Ok(CorridorDirection::East)
    );
    assert_eq!(
      CorridorDirection::try_from((Region { x: 0, y: 0, z: 0 }, Region { x: -1, y: 0, z: 0 })),
      Ok(CorridorDirection::West)
    );
    assert_eq!(
      CorridorDirection::try_from((Region { x: 0, y: 0, z: 0 }, Region { x: 0, y: 0, z: 1 })),
      Ok(CorridorDirection::Up)
    );
    assert_eq!(
      CorridorDirection::try_from((Region { x: 0, y: 0, z: 0 }, Region { x: 0, y: 0, z: -1 })),
      Ok(CorridorDirection::Down)
    );
    assert_eq!(
      CorridorDirection::try_from((Region { x: 1, y: 1, z: 1 }, Region { x: 0, y: 0, z: 0 })),
      Err(())
    );
  }
}
