use crate::prelude::CorridorDirection;
use crate::prelude::CorridorOrigin;
use crate::prelude::CorridorTerminus;
use crate::prelude::Description;
use crate::prelude::IsARoom;
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

macro_rules! spawn_room {
  ($world:expr, $region:expr, $room:expr, $name:expr, $description:expr) => {
    $world.spawn(($region, $room, $name, $description, IsARoom));
  };
}

macro_rules! spawn_passage {
  ($world:expr, $region:expr, $from_room:expr, $to_room:expr, $direction:expr) => {
    $world.spawn(($region, $from_room, $direction, PassageKind::from($to_room)));
    $world.spawn(($region, $to_room, -$direction, PassageKind::from($from_room)));
  };
}

macro_rules! spawn_corridor {
  ($world:expr, $region:expr, $room:expr, $direction:expr, $next_region:expr) => {
    $world.spawn((
      $region,
      $room,
      $direction,
      PassageKind::Corridor($next_region),
      CorridorOrigin,
    ));
    $world.spawn((
      $region,
      $room,
      CorridorDirection::try_from($direction).expect("Invalid corridor direction"),
      CorridorTerminus,
    ));
  };
}

impl RegionGenerator for CompassRoseRegionGenerator {
  fn generate(&self, region: Region, world: &mut World) -> Result<(), WorldError> {
    let center_room = Room::default();
    let name = Name("The Center Room".to_string());
    let description = Description("This is the center room.".to_string());
    spawn_room!(world, region, center_room, name, description);
    PassageDirection::iter().for_each(|direction| {
      let room = center_room.get(direction);
      let name = Name(format!("The {} Room", direction));
      let description = Description(format!("This is the {} room.", direction.to_string().to_lowercase()));
      spawn_room!(world, region, room, name, description);
      spawn_passage!(world, region, center_room, room, direction);
      let corridor_direction = CorridorDirection::try_from(direction);
      if corridor_direction.is_ok() {
        // Insert a passage from the room into a corridor to the next region.
        let next_region = region + corridor_direction.unwrap().into();
        spawn_corridor!(world, region, room, direction, next_region);
      }
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
