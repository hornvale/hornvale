use crate::prelude::*;
use anyhow::Error as AnyError;
use hecs::{Entity, With, World};

/// Get the room entity.
pub trait GetRoomEntity {
  /// Get the room entity.
  fn get_room_entity(&self, region: &Region, room: &Room) -> Result<Entity, AnyError>;
}

/// Implement the `GetRoomEntity` trait for the `World` type.
impl GetRoomEntity for World {
  fn get_room_entity(&self, region: &Region, room: &Room) -> Result<Entity, AnyError> {
    self
      .query::<With<(&Region, &Room), &IsARoom>>()
      .into_iter()
      .find(|(_, (reg, rm))| *reg == region && *rm == room)
      .map(|(entity, _)| entity)
      .ok_or_else(|| anyhow::anyhow!("The room entity does not exist."))
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_get_room_entity() {
    init();
    let mut world = World::new();
    let region = Region { w: 0, x: 1, y: 2, z: 3 };
    let room = Room { w: 0, x: 4, y: 2, z: 1 };
    world.spawn((Region { w: 0, x: 1, y: 2, z: 3 }, IsARegion));
    world.spawn((region, Room { w: 0, x: 4, y: 2, z: 1 }, IsARoom));
    world.spawn((region, room));
    let room_entity = world.get_room_entity(&region, &room).unwrap();
    assert_eq!(room_entity, room_entity);
  }
}
