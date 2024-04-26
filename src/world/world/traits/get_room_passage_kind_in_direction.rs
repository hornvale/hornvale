use crate::world::prelude::*;
use anyhow::Error as AnyError;
use hecs::{With, World};

/// A trait for getting the room passage in a specific direction.
pub trait GetRoomPassageKindInDirection {
  /// Get the room passage entity in a specific direction.
  fn get_room_passage_kind_in_direction(
    &self,
    region: &Region,
    room: &Room,
    direction: &PassageDirection,
  ) -> Result<PassageKind, AnyError>;
}

impl GetRoomPassageKindInDirection for World {
  fn get_room_passage_kind_in_direction(
    &self,
    region: &Region,
    room: &Room,
    direction: &PassageDirection,
  ) -> Result<PassageKind, AnyError> {
    let result = self
      .query::<With<(&Region, &Room, &PassageKind, &PassageDirection), &IsAPassage>>()
      .into_iter()
      .filter(|(_, (rgn, rm, _, dir))| *rgn == region && *rm == room && *dir == direction)
      .map(|(_, (_, _, kind, _))| kind.clone())
      .collect::<Vec<_>>()
      .pop();
    match result {
      Some(kind) => Ok(kind),
      None => Err(anyhow::anyhow!("The room passage entity does not exist.")),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::database::Database;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_get_room_passage_kind_in_direction() {
    init();
    let mut database = Database::default();
    let region = Region { w: 0, x: 1, y: 2, z: 3 };
    let room = Room { w: 0, x: 4, y: 2, z: 1 };
    let passage_kind = PassageKind::Default(room);
    let passage_direction = PassageDirection(Direction::North);
    database.world.spawn((Region { w: 0, x: 1, y: 2, z: 3 }, IsARegion));
    database.world.spawn((region, Room { w: 0, x: 4, y: 2, z: 1 }, IsARoom));
    let _passage_entity = database
      .world
      .spawn((region, room, passage_kind.clone(), passage_direction, IsAPassage));
    let result = database
      .world
      .get_room_passage_kind_in_direction(&region, &room, &passage_direction);
    assert_eq!(result.unwrap(), passage_kind);
  }
}
