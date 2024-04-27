use crate::database::prelude::*;
use crate::world::prelude::*;
use crate::world::region::Region;
use serde::{Deserialize, Serialize};
use strum::IntoEnumIterator;

/// The Compass Rose Region Generator generates a simple compass rose.
///
/// This generator generates a compass rose with rooms at the various navigable
/// directions. It is useful for testing and debugging purposes.
#[derive(Clone, Copy, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct CompassRoseRegionGenerator;

impl RegionGenerator for CompassRoseRegionGenerator {
  fn generate(&self, region: Region, database: &mut Database) -> Result<(), WorldError> {
    let center_room = Room::default();
    database
      .world
      .spawn_room(region, center_room, "The Center Room", "This is the center room.")?;
    Direction::iter().for_each(|direction| {
      let passage_direction = PassageDirection(direction);
      let room = center_room + passage_direction;
      database
        .world
        .spawn_room(
          region,
          room,
          &format!("The {} Room", direction),
          &format!("This is the {} room.", direction.to_string().to_lowercase()),
        )
        .unwrap();
      database
        .world
        .spawn_passage(region, center_room, room, PassageDirection(direction))
        .unwrap();
      if direction.is_cardinal() || direction.is_vertical() {
        let corridor_direction = CorridorDirection(direction);
        // Insert a passage from the room into a corridor to the next region.
        let next_region = region + corridor_direction;
        database
          .world
          .spawn_corridor(region, next_region, room, PassageDirection(direction))
          .unwrap();
      }
    });
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn test_generate() {
    init();
    let mut database = Database::default();
    let generator = CompassRoseRegionGenerator;
    assert!(generator.generate(Region::default(), &mut database).is_ok());
    // List passages from the center room.
    let center_room = Room::default();
    let passages = database
      .world
      .query::<(&Room, &PassageDirection, &PassageKind)>()
      .iter()
      .filter_map(|(_, (&room, &direction, &ref kind))| {
        if room == center_room {
          Some((direction, kind.clone()))
        } else {
          None
        }
      })
      .collect::<Vec<_>>();
    println!("{:#?}", passages);
  }
}
