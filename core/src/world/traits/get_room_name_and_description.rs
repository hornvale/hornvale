use crate::prelude::*;
use anyhow::Error as AnyError;
use hecs::World;

/// Get a room's name and description.
pub trait GetRoomNameAndDescription {
  /// Get a room's name and description.
  fn get_room_name_and_description(&self, region: &Region, room: &Room) -> Result<(Name, Description), AnyError>;
}

impl GetRoomNameAndDescription for World {
  fn get_room_name_and_description(&self, region: &Region, room: &Room) -> Result<(Name, Description), AnyError> {
    self
      .query::<(&Region, &Room, &Name, &Description)>()
      .with::<&IsARoom>()
      .iter()
      .find(|(_, (&rgn, &rm, _, _))| rgn == *region && rm == *room)
      .map(|(_, (_, _, name, description))| (name.clone(), description.clone()))
      .ok_or_else(|| anyhow::anyhow!("The room does not have a name and description component."))
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_get_room_name_and_description() {
    init();
    let mut world = World::new();
    let region = Region { w: 0, x: 1, y: 2, z: 3 };
    let room = Room { w: 0, x: 4, y: 2, z: 1 };
    world.spawn((Region { w: 0, x: 1, y: 2, z: 3 }, IsARegion));
    world.spawn((
      region,
      room,
      Name("Room".to_string()),
      Description("A room.".to_string()),
      IsARoom,
    ));
    let (name, description) = world.get_room_name_and_description(&region, &room).unwrap();
    assert_eq!(name, Name("Room".to_string()));
    assert_eq!(description, Description("A room.".to_string()));
  }
}
