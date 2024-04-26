use crate::world::prelude::*;
use anyhow::Error as AnyError;
use hecs::{Entity, World};

/// Get the room entity containing an entity.
pub trait GetRoomEntityContainingEntity {
  /// Get the room entity containing the entity.
  fn get_room_entity_containing_entity(&self, entity: Entity) -> Result<Entity, AnyError>;
}

/// Implement the `GetRoomContainingEntity` trait for the `World` type.
impl GetRoomEntityContainingEntity for World {
  fn get_room_entity_containing_entity(&self, entity: Entity) -> Result<Entity, AnyError> {
    self
      .get_region_and_room_containing_entity(entity)
      .map(|(region, room)| self.get_room_entity(&region, &room).unwrap())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::database::Database;
  use crate::test_utilities::prelude::*;
  #[test]
  fn test_get_room_entity_containing_entity() {
    init();
    let mut database = Database::default();
    let region = Region { w: 0, x: 1, y: 2, z: 3 };
    let room = Room { w: 0, x: 4, y: 2, z: 1 };
    database.world.spawn((Region { w: 0, x: 1, y: 2, z: 3 }, IsARegion));
    let room_entity = database.world.spawn((region, Room { w: 0, x: 4, y: 2, z: 1 }, IsARoom));
    let entity = database.world.spawn((region, room));
    let room_entity_containing_entity = database.world.get_room_entity_containing_entity(entity).unwrap();
    assert_eq!(room_entity_containing_entity, room_entity);
  }
}
