use crate::prelude::*;
use hecs::{Entity, World};
use hornvale_dictionary::prelude::*;

/// Get the entity's region and room.
pub fn get_entity_region_and_room(world: &World, entity: Entity) -> Option<(Region, Room)> {
  let mut query = world.query_one::<(&Region, &Room)>(entity).unwrap();
  if let Some((region, room)) = query.get() {
    Some((*region, *room))
  } else {
    None
  }
}

/// Get a room's name and description.
pub fn get_room_name_and_description(world: &World, region: &Region, room: &Room) -> Option<(Name, Description)> {
  world
    .query::<(&Region, &Room, &Name, &Description)>()
    .with::<&IsARoom>()
    .iter()
    .find(|(_, (&rgn, &rm, _, _))| rgn == *region && rm == *room)
    .map(|(_, (_, _, name, description))| (name.clone(), description.clone()))
}

/// Get a room's passage in a given direction.
pub fn get_room_passage_in_direction(
  world: &World,
  region: &Region,
  room: &Room,
  direction: PassageDirection,
) -> Option<PassageKind> {
  world
    .query::<(&Region, &Room, &PassageDirection, &PassageKind)>()
    .iter()
    .find(|(_, (&rgn, &rm, &dir, _))| rgn == *region && rm == *room && dir == direction)
    .map(|(_, (_, _, _, kind))| kind.clone())
}

/// Get a room's passages.
pub fn get_room_passages(world: &World, region: &Region, room: &Room) -> Vec<(PassageDirection, PassageKind)> {
  world
    .query::<(&Region, &Room, &PassageDirection, &PassageKind)>()
    .iter()
    .filter(|(_, (&rgn, &rm, _, _))| rgn == *region && rm == *room)
    .map(|(_, (_, _, dir, kind))| (*dir, kind.clone()))
    .collect::<Vec<_>>()
}

/// Get a description of a room's passages.
pub fn describe_room_passages(world: &World, entity: Entity) -> String {
  let (region, room) = get_entity_region_and_room(world, entity).unwrap();
  let passages = get_room_passages(world, &region, &room);
  if !passages.is_empty() {
    let passages = passages
      .iter()
      .map(|(dir, _)| dir.to_string().to_lowercase())
      .collect::<Vec<_>>();
    match passages.len() {
      1 => format!("There is an exit to the {}.", passages[0]),
      2 => format!("There are exits to the {} and {}.", passages[0], passages[1]),
      _ => format!(
        "There are exits to the {}, and {}.",
        passages[..passages.len() - 1].join(", "),
        passages[passages.len() - 1]
      ),
    }
  } else {
    "There are no exits.".to_string()
  }
}
