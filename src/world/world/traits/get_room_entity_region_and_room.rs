use crate::world::prelude::*;
use anyhow::Error as AnyError;
use hecs::{Entity, World};

/// Get the room entity's region and room.
pub trait GetRoomEntityRegionAndRoom {
  /// Get the room's region and room.
  fn get_room_entity_region_and_room(&self, entity: Entity) -> Result<(Region, Room), AnyError>;
}

/// Implement the `GetRoomEntityRegionAndRoom` trait for the `World` type.
impl GetRoomEntityRegionAndRoom for World {
  fn get_room_entity_region_and_room(&self, entity: Entity) -> Result<(Region, Room), AnyError> {
    self
      .query_one::<(&Region, &Room)>(entity)
      .unwrap()
      .get()
      .map(|(region, room)| (*region, *room))
      .ok_or_else(|| anyhow::anyhow!("The entity does not have a region and room component."))
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_get_room_entity_region_and_room() {
    init();
    let mut world = World::new();
    let region = Region { w: 0, x: 1, y: 2, z: 3 };
    let room = Room { w: 0, x: 4, y: 2, z: 1 };
    world.spawn((Region { w: 0, x: 1, y: 2, z: 3 }, IsARegion));
    world.spawn((region, Room { w: 0, x: 4, y: 2, z: 1 }, IsARoom));
    let entity = world.spawn((region, room));
    let (region, room) = world.get_room_entity_region_and_room(entity).unwrap();
    assert_eq!(region, Region { w: 0, x: 1, y: 2, z: 3 });
    assert_eq!(room, Room { w: 0, x: 4, y: 2, z: 1 });
  }
}
