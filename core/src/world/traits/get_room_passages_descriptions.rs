use crate::prelude::*;
use anyhow::Error as AnyError;
use hecs::{Entity, With, World};

/// Get a room's passages' descriptions as a string.
pub trait GetRoomPassagesDescriptions {
  /// Get the room's passages' descriptions.
  fn get_room_passages_descriptions(&self, entity: Entity) -> Result<String, AnyError>;
}

/// Implement the `GetRoomPassagesDescriptions` trait for the `World` type.
impl GetRoomPassagesDescriptions for World {
  fn get_room_passages_descriptions(&self, entity: Entity) -> Result<String, AnyError> {
    let (region, room) = self.get_room_region_and_room(entity)?;
    // Soon this will get a description for the passage if it exists.
    let directions = self
      .query::<With<(&Region, &Room, &PassageDirection), &IsAPassage>>()
      .into_iter()
      .filter(|(_, (&rgn, &rm, _))| rgn == region && rm == room)
      .map(|(_, (_, _, dir))| dir.to_string().to_lowercase())
      .collect::<Vec<_>>();
    let result = if !directions.is_empty() {
      match directions.len() {
        1 => format!("There is an exit to the {}.", directions[0]),
        2 => format!("There are exits to the {} and {}.", directions[0], directions[1]),
        _ => format!(
          "There are exits to the {}, and {}.",
          directions[..directions.len() - 1].join(", "),
          directions[directions.len() - 1]
        ),
      }
    } else {
      "There are no exits.".to_string()
    };
    Ok(result)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_get_room_passages_descriptions() {
    init();
    let mut world = World::new();
    let region = Region { w: 0, x: 1, y: 2, z: 3 };
    let room = Room { w: 0, x: 4, y: 2, z: 1 };
    let passage_direction = PassageDirection(Direction::North);
    world.spawn((Region { w: 0, x: 1, y: 2, z: 3 }, IsARegion));
    world.spawn((region, Room { w: 0, x: 4, y: 2, z: 1 }, IsARoom));
    world.spawn((region, room, passage_direction, IsAPassage));
    let room_entity = world.get_room_entity(&region, &room).unwrap();
    let result = world.get_room_passages_descriptions(room_entity).unwrap();
    assert_eq!(result, "There is an exit to the north.");
  }
}
