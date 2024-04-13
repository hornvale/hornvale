use crate::prelude::CorridorDirection;
use crate::prelude::CorridorOrigin;
use crate::prelude::CorridorTerminus;
use crate::prelude::Description;
use crate::prelude::Name;
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
    let center_room = Room::default();
    let name = Name(format!("The Center Room ({:?})", region));
    let description = Description("This is the center room.".to_string());
    world.spawn((region, center_room, name, description));
    PassageDirection::iter().for_each(|direction| {
      let room = center_room.get(direction);
      let name = Name(format!("The {} Room ({:?})", direction, region));
      let description = Description(format!("This is the {} room.", direction.to_string().to_lowercase()));
      world.spawn((region, room, name, description));

      // Insert a passage from the center room to the room in the direction.
      world.spawn((region, center_room, direction, PassageKind::from(room)));
      // Insert a passage from the room in the opposite direction to the center room.
      world.spawn((region, room, -direction, PassageKind::from(center_room)));

      let corridor_direction = CorridorDirection::try_from(direction);
      if corridor_direction.is_err() {
        return;
      }
      // Insert a passage from the room into a corridor to the next region.
      let next_region = region + corridor_direction.unwrap().into();
      world.spawn((
        region,
        room,
        direction,
        PassageKind::Corridor(next_region),
        CorridorOrigin,
      ));
      // Insert a corridor terminus from the next region into the room.
      world.spawn((
        region,
        room,
        CorridorDirection::try_from(direction).expect("Invalid corridor direction"),
        CorridorTerminus,
      ));
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
