use crate::prelude::CorridorDirection;
use crate::prelude::CorridorOrigin;
use crate::prelude::CorridorTerminus;
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
    PassageDirection::iter().for_each(|direction| {
      let room = center_room.get(direction);
      let diff = room - center_room;
      // Insert a passage from the center room to the room in the direction.
      world.spawn((region, center_room, direction, PassageKind::from(diff)));
      // Insert a passage from the room in the direction to the center room.
      world.spawn((region, room, direction, PassageKind::from(-diff)));
      let corridor_direction = CorridorDirection::try_from(direction);
      if corridor_direction.is_err() {
        return;
      }
      // Insert a passage from the room into a corridor to the next region.
      world.spawn((
        region,
        room,
        direction,
        PassageKind::from(CorridorDirection::try_from(direction).expect("Invalid corridor direction")),
        CorridorOrigin,
      ));
      // Insert a corridor terminus from the next region into the room.
      world.spawn((
        region,
        room,
        CorridorDirection::try_from(-direction).expect("Invalid corridor direction"),
        CorridorTerminus,
      ));
    });
    // Create corridor origins within the region.
    //
    // We do this by finding a room (e.g. the south room, which should have a
    // passage to the south that enters a corridor) and creating the corridor
    // origin.
    let south_room = world
      .query::<&Room>()
      .iter()
      .find(|(_, &room)| room == center_room.get(PassageDirection::South))
      .map(|(_, &room)| room)
      .unwrap();
    world.spawn((
      region,
      south_room,
      PassageDirection::South,
      PassageKind::Corridor(CorridorDirection::South.into()),
      CorridorOrigin,
    ));
    let north_room = world
      .query::<&Room>()
      .iter()
      .find(|(_, &room)| room == center_room.get(PassageDirection::North))
      .map(|(_, &room)| room)
      .unwrap();
    world.spawn((
      region,
      north_room,
      PassageDirection::North,
      PassageKind::Corridor(CorridorDirection::North.into()),
      CorridorOrigin,
    ));
    let east_room = world
      .query::<&Room>()
      .iter()
      .find(|(_, &room)| room == center_room.get(PassageDirection::East))
      .map(|(_, &room)| room)
      .unwrap();
    world.spawn((
      region,
      east_room,
      PassageDirection::East,
      PassageKind::Corridor(CorridorDirection::East.into()),
      CorridorOrigin,
    ));
    let west_room = world
      .query::<&Room>()
      .iter()
      .find(|(_, &room)| room == center_room.get(PassageDirection::West))
      .map(|(_, &room)| room)
      .unwrap();
    world.spawn((
      region,
      west_room,
      PassageDirection::West,
      PassageKind::Corridor(CorridorDirection::West.into()),
      CorridorOrigin,
    ));
    let up_room = world
      .query::<&Room>()
      .iter()
      .find(|(_, &room)| room == center_room.get(PassageDirection::Up))
      .map(|(_, &room)| room)
      .unwrap();
    world.spawn((
      region,
      up_room,
      PassageDirection::Up,
      PassageKind::Corridor(CorridorDirection::Up.into()),
      CorridorOrigin,
    ));
    let down_room = world
      .query::<&Room>()
      .iter()
      .find(|(_, &room)| room == center_room.get(PassageDirection::Down))
      .map(|(_, &room)| room)
      .unwrap();
    world.spawn((
      region,
      down_room,
      PassageDirection::Down,
      PassageKind::Corridor(CorridorDirection::Down.into()),
      CorridorOrigin,
    ));

    // Create corridor termini within the region.
    //
    // We do this by finding a room (e.g. the south room, which should have a
    // passage from the south that enters from a corridor) and creating the
    // corridor terminus.
    world.spawn((region, south_room, CorridorDirection::South, CorridorTerminus));
    world.spawn((region, north_room, CorridorDirection::North, CorridorTerminus));
    world.spawn((region, east_room, CorridorDirection::East, CorridorTerminus));
    world.spawn((region, west_room, CorridorDirection::West, CorridorTerminus));
    world.spawn((region, up_room, CorridorDirection::Up, CorridorTerminus));
    world.spawn((region, down_room, CorridorDirection::Down, CorridorTerminus));
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
