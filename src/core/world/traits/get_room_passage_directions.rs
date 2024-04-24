use crate::core::prelude::*;
use anyhow::Error as AnyError;
use hecs::{With, World};

/// A trait for getting the room passage directions.
pub trait GetRoomPassageDirections {
  /// Get the room passage directions.
  fn get_room_passage_directions(&self, region: &Region, room: &Room) -> Result<Vec<PassageDirection>, AnyError>;
}

impl GetRoomPassageDirections for World {
  fn get_room_passage_directions(&self, region: &Region, room: &Room) -> Result<Vec<PassageDirection>, AnyError> {
    let result = self
      .query::<With<(&Region, &Room, &PassageDirection), &IsAPassage>>()
      .into_iter()
      .filter(|(_, (rgn, rm, _))| *rgn == region && *rm == room)
      .map(|(_, (_, _, dir))| *dir)
      .collect::<Vec<_>>();
    Ok(result)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_get_room_passage_directions() {
    init();
    let mut world = World::new();
    let region = Region { w: 0, x: 1, y: 2, z: 3 };
    let room = Room { w: 0, x: 4, y: 2, z: 1 };
    let passage_direction = PassageDirection(Direction::North);
    world.spawn((Region { w: 0, x: 1, y: 2, z: 3 }, IsARegion));
    world.spawn((region, Room { w: 0, x: 4, y: 2, z: 1 }, IsARoom));
    world.spawn((region, room, passage_direction, IsAPassage));
    let passage_directions = world.get_room_passage_directions(&region, &room).unwrap();
    assert_eq!(passage_directions, vec![passage_direction]);
  }
}
