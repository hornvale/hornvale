use crate::core::prelude::*;
use anyhow::Error as AnyError;
use hecs::{Entity, With, World};

/// A trait for getting the room passage in a specific direction.
pub trait GetRoomPassageEntityInDirection {
  /// Get the room passage entity in a specific direction.
  fn get_room_passage_entity_in_direction(
    &self,
    region: &Region,
    room: &Room,
    direction: &PassageDirection,
  ) -> Result<Entity, AnyError>;
}

impl GetRoomPassageEntityInDirection for World {
  fn get_room_passage_entity_in_direction(
    &self,
    region: &Region,
    room: &Room,
    direction: &PassageDirection,
  ) -> Result<Entity, AnyError> {
    let result = self
      .query::<With<(&Region, &Room, &PassageKind, &PassageDirection), &IsAPassage>>()
      .into_iter()
      .filter(|(_, (rgn, rm, _, dir))| *rgn == region && *rm == room && *dir == direction)
      .map(|(entity, (_, _, _, _))| entity)
      .collect::<Vec<_>>()
      .pop();
    match result {
      Some(entity) => Ok(entity),
      None => Err(anyhow::anyhow!("The room passage entity does not exist.")),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_get_room_passage_entity_in_direction() {
    init();
    let mut world = World::new();
    let region = Region { w: 0, x: 1, y: 2, z: 3 };
    let room = Room { w: 0, x: 4, y: 2, z: 1 };
    let passage_kind = PassageKind::Default(room);
    let passage_direction = PassageDirection(Direction::North);
    world.spawn((Region { w: 0, x: 1, y: 2, z: 3 }, IsARegion));
    world.spawn((region, Room { w: 0, x: 4, y: 2, z: 1 }, IsARoom));
    let passage_entity = world.spawn((region, room, passage_kind, passage_direction, IsAPassage));
    let result = world.get_room_passage_entity_in_direction(&region, &room, &passage_direction);
    assert_eq!(result.unwrap(), passage_entity);
  }
}
