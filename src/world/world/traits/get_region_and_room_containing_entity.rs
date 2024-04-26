use crate::world::prelude::*;
use anyhow::Error as AnyError;
use hecs::{Entity, World};

/// Get the region and room containing an entity.
pub trait GetRegionAndRoomContainingEntity {
  /// Get the region and room containing the entity.
  fn get_region_and_room_containing_entity(&self, entity: Entity) -> Result<(Region, Room), AnyError>;
}

/// Implement the `GetRegionAndRoomContainingEntity` trait for the `World` type.
impl GetRegionAndRoomContainingEntity for World {
  fn get_region_and_room_containing_entity(&self, entity: Entity) -> Result<(Region, Room), AnyError> {
    let mut query = self.query_one::<(&Region, &Room)>(entity).unwrap();
    if let Some((region, room)) = query.get() {
      Ok((*region, *room))
    } else {
      anyhow::bail!("The entity does not have a region and room component.");
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::database::Database;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_get_region_and_room_containing_entity() {
    init();
    let mut database = Database::default();
    let region = Region { w: 0, x: 1, y: 2, z: 3 };
    let room = Room { w: 0, x: 4, y: 2, z: 1 };
    database.world.spawn((Region { w: 0, x: 1, y: 2, z: 3 }, IsARegion));
    database.world.spawn((region, Room { w: 0, x: 4, y: 2, z: 1 }, IsARoom));
    let entity = database.world.spawn((region, room));
    let (region, room) = database.world.get_region_and_room_containing_entity(entity).unwrap();
    assert_eq!(region, Region { w: 0, x: 1, y: 2, z: 3 });
    assert_eq!(room, Room { w: 0, x: 4, y: 2, z: 1 });
  }
}
