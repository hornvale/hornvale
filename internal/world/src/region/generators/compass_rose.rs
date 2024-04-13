use crate::prelude::PassageDirection;
use crate::prelude::PassageKind;
use crate::prelude::Region;
use crate::prelude::RegionGenerator;
use crate::prelude::Room;
use crate::prelude::WorldError;
use hecs::World;
use serde::{Deserialize, Serialize};
use strum::IntoEnumIterator;

/// The Compass Rose Region Generator generates a simple compass rose.
///
/// This generator generates a compass rose with rooms at the various navigable
/// directions. It is useful for testing and debugging purposes.
#[derive(Clone, Copy, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct CompassRoseRegionGenerator;

impl RegionGenerator for CompassRoseRegionGenerator {
  fn generate(&self, region: Region, world: &mut World) -> Result<(), WorldError> {
    let corridors = region.get_corridors();
    corridors.iter().for_each(|&corridor| {
      world.spawn((region, corridor));
    });
    let center_room = Room::default();
    PassageDirection::iter().for_each(|direction| {
      let room = center_room.get(direction);
      let diff = room - center_room;
      // Insert a passage from the center room to the room in the direction.
      world.spawn((region, center_room, direction, PassageKind::from(diff)));
      // Insert a passage from the room in the direction to the center room.
      world.spawn((region, room, direction, PassageKind::from(-diff)));
    });
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use hornvale_test_utilities::prelude::*;

  #[test]
  fn test_generate() {
    init();
    let mut world = World::new();
    let generator = CompassRoseRegionGenerator;
    assert!(generator.generate(Region::default(), &mut world).is_ok());
    // List passages from the center room.
    let center_room = Room::default();
    let passages = world
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
