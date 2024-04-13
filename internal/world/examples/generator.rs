//! Test of the world.
use hecs::World;
use hornvale_world::prelude::*;
use std::io::{self, BufRead};

/// A player.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Player;

/// Generate a world.
pub fn main() {
  let mut world = World::new();
  let generator = CompassRoseRegionGenerator;
  generator.generate(Region::default(), &mut world).unwrap();
  let player = world.spawn((Region::default(), Room::default(), Player));
  let mut input_source = io::stdin().lock();
  loop {
    // Find the room the player is in.
    let (region, room) = {
      let (region, room) = world.query_one_mut::<(&Region, &Room)>(player).unwrap();
      (region.clone(), room.clone())
    };
    println!("{:#?}", region);
    println!("{:#?}", room);
    {
      let (room_name, room_description) = world
        .query::<(&Region, &Room, &Name, &Description)>()
        .iter()
        .find(|(_, (&rgn, &rm, _, _))| rgn == region && rm == room)
        .map(|(_, (_, _, &ref name, &ref description))| (name.clone(), description.clone()))
        .unwrap();
      println!("{:?}", room_name.0);
      println!("{:?}", room_description.0);
    }
    print!("> ");
    let mut input = String::new();
    input_source.read_line(&mut input).unwrap();
    let direction = match input.trim() {
      "n" => PassageDirection::North,
      "ne" => PassageDirection::Northeast,
      "e" => PassageDirection::East,
      "se" => PassageDirection::Southeast,
      "s" => PassageDirection::South,
      "sw" => PassageDirection::Southwest,
      "w" => PassageDirection::West,
      "nw" => PassageDirection::Northwest,
      "u" => PassageDirection::Up,
      "d" => PassageDirection::Down,
      _ => continue,
    };
    let player_info = world.query_one_mut::<(&mut Region, &mut Room)>(player).unwrap();
    let room = player_info.1.get(direction);
    world.insert(player, (room,)).unwrap();
  }
}
