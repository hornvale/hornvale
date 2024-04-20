use crate::prelude::Room;
use hornvale_core::prelude::*;
use serde::{Deserialize, Serialize};
use std::ops::Neg;
use strum::{Display, EnumIter};

/// A `PassageDirection` is a direction in which a passage can be built.
///
/// Note that this is a perfect match for the `Direction` enum in the `core` crate.
#[derive(Clone, Copy, Debug, Display, EnumIter, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
pub enum PassageDirection {
  /// The `North` direction (y+1).
  North,
  /// The `Northeast` direction (x+1, y+1).
  Northeast,
  /// The `East` direction (x+1).
  East,
  /// The `Southeast` direction (x+1, y-1).
  Southeast,
  /// The `South` direction (y-1).
  South,
  /// The `Southwest` direction (x-1, y-1).
  Southwest,
  /// The `West` direction (x-1).
  West,
  /// The `Northwest` direction (x-1, y+1).
  Northwest,
  /// The `Up` direction (z+1).
  Up,
  /// The `Down` direction (z-1).
  Down,
  /// The `In` direction (w+1).
  In,
  /// The `Out` direction (w-1).
  Out,
}

impl PassageDirection {
  /// Get the opposite direction of the given direction.
  pub fn opposite(&self) -> Self {
    use PassageDirection::*;
    match self {
      North => South,
      Northeast => Southwest,
      East => West,
      Southeast => Northwest,
      South => North,
      Southwest => Northeast,
      West => East,
      Northwest => Southeast,
      Up => Down,
      Down => Up,
      In => Out,
      Out => In,
    }
  }
}

impl Neg for PassageDirection {
  type Output = Self;

  fn neg(self) -> Self::Output {
    self.opposite()
  }
}

impl TryFrom<Room> for PassageDirection {
  type Error = ();
  fn try_from(room: Room) -> Result<Self, Self::Error> {
    match (room.w, room.x, room.y, room.z) {
      (0, 0, y, 0) if y > 0 => Ok(PassageDirection::North),
      (0, x, y, 0) if x > 0 && y > 0 => Ok(PassageDirection::Northeast),
      (0, x, 0, 0) if x > 0 => Ok(PassageDirection::East),
      (0, x, y, 0) if x > 0 && y < 0 => Ok(PassageDirection::Southeast),
      (0, 0, y, 0) if y < 0 => Ok(PassageDirection::South),
      (0, x, y, 0) if x < 0 && y < 0 => Ok(PassageDirection::Southwest),
      (0, x, 0, 0) if x < 0 => Ok(PassageDirection::West),
      (0, x, y, 0) if x < 0 && y > 0 => Ok(PassageDirection::Northwest),
      (0, 0, 0, z) if z > 0 => Ok(PassageDirection::Up),
      (0, 0, 0, z) if z < 0 => Ok(PassageDirection::Down),
      (w, 0, 0, 0) if w > 0 => Ok(PassageDirection::In),
      (w, 0, 0, 0) if w < 0 => Ok(PassageDirection::Out),
      _ => Err(()),
    }
  }
}

impl TryFrom<(Room, Room)> for PassageDirection {
  type Error = ();

  fn try_from((from, to): (Room, Room)) -> Result<Self, Self::Error> {
    let room: Room = (to.w - from.w, to.x - from.x, to.y - from.y, to.z - from.z).into();
    PassageDirection::try_from(room)
  }
}

impl From<Direction> for PassageDirection {
  fn from(direction: Direction) -> Self {
    use Direction::*;
    match direction {
      North => PassageDirection::North,
      Northeast => PassageDirection::Northeast,
      East => PassageDirection::East,
      Southeast => PassageDirection::Southeast,
      South => PassageDirection::South,
      Southwest => PassageDirection::Southwest,
      West => PassageDirection::West,
      Northwest => PassageDirection::Northwest,
      Up => PassageDirection::Up,
      Down => PassageDirection::Down,
      In => PassageDirection::In,
      Out => PassageDirection::Out,
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;
  use PassageDirection::*;

  #[test]
  fn test_opposite() {
    init();
    assert_eq!(North.opposite(), South);
    assert_eq!(Northeast.opposite(), Southwest);
    assert_eq!(East.opposite(), West);
    assert_eq!(Southeast.opposite(), Northwest);
    assert_eq!(South.opposite(), North);
    assert_eq!(Southwest.opposite(), Northeast);
    assert_eq!(West.opposite(), East);
    assert_eq!(Northwest.opposite(), Southeast);
    assert_eq!(Up.opposite(), Down);
    assert_eq!(Down.opposite(), Up);
    assert_eq!(In.opposite(), Out);
    assert_eq!(Out.opposite(), In);
  }

  #[test]
  fn test_try_from_room() {
    init();
    assert_eq!(PassageDirection::try_from(Room { w: 0, x: 0, y: 1, z: 0 }), Ok(North));
    assert_eq!(
      PassageDirection::try_from(Room { w: 0, x: 1, y: 1, z: 0 }),
      Ok(Northeast)
    );
    assert_eq!(PassageDirection::try_from(Room { w: 0, x: 1, y: 0, z: 0 }), Ok(East));
    assert_eq!(
      PassageDirection::try_from(Room {
        w: 0,
        x: 1,
        y: -1,
        z: 0
      }),
      Ok(Southeast)
    );
    assert_eq!(
      PassageDirection::try_from(Room {
        w: 0,
        x: 0,
        y: -1,
        z: 0
      }),
      Ok(South)
    );
    assert_eq!(
      PassageDirection::try_from(Room {
        w: 0,
        x: -1,
        y: -1,
        z: 0
      }),
      Ok(Southwest)
    );
    assert_eq!(
      PassageDirection::try_from(Room {
        w: 0,
        x: -1,
        y: 0,
        z: 0
      }),
      Ok(West)
    );
    assert_eq!(
      PassageDirection::try_from(Room {
        w: 0,
        x: -1,
        y: 1,
        z: 0
      }),
      Ok(Northwest)
    );
    assert_eq!(PassageDirection::try_from(Room { w: 0, x: 0, y: 0, z: 1 }), Ok(Up));
    assert_eq!(
      PassageDirection::try_from(Room {
        w: 0,
        x: 0,
        y: 0,
        z: -1
      }),
      Ok(Down)
    );
    assert_eq!(PassageDirection::try_from(Room { w: 1, x: 0, y: 0, z: 0 }), Ok(In));
    assert_eq!(
      PassageDirection::try_from(Room {
        w: -1,
        x: 0,
        y: 0,
        z: 0
      }),
      Ok(Out)
    );
  }
}
