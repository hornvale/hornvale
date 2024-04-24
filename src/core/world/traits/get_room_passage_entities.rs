use crate::core::prelude::*;
use anyhow::Error as AnyError;
use hecs::{Entity, With, World};

/// Get the room passage entities.
pub trait GetRoomPassageEntities {
  /// Get the room passage entities.
  fn get_room_passage_entities(&self, region: &Region, room: &Room) -> Result<Vec<Entity>, AnyError>;
}

/// Implement the `GetRoomPassageEntities` trait for the `World` type.
impl GetRoomPassageEntities for World {
  fn get_room_passage_entities(&self, region: &Region, room: &Room) -> Result<Vec<Entity>, AnyError> {
    let result = self
      .query::<With<(&Region, &Room, &PassageKind, &PassageDirection), &IsAPassage>>()
      .into_iter()
      .filter(|(_, (rgn, rm, _, _))| *rgn == region && *rm == room)
      .map(|(entity, _)| entity)
      .collect::<Vec<_>>();
    Ok(result)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_get_room_passage_entities() {
    init();
    let mut world = World::new();
    let region = Region { w: 0, x: 1, y: 2, z: 3 };
    let room = Room { w: 0, x: 4, y: 2, z: 1 };
    let passage_kind = PassageKind::Default(room);
    let passage_direction = PassageDirection(Direction::North);
    world.spawn((Region { w: 0, x: 1, y: 2, z: 3 }, IsARegion));
    world.spawn((region, Room { w: 0, x: 4, y: 2, z: 1 }, IsARoom));
    let passage_entity = world.spawn((region, room, passage_kind, passage_direction, IsAPassage));
    let passage_entities = world.get_room_passage_entities(&region, &room).unwrap();
    assert_eq!(passage_entities, vec![passage_entity]);
  }
}
