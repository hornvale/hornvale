//! Test of the world.
use hecs::World;
use hornvale_command::prelude::*;
use hornvale_world::prelude::*;
use std::io::{self, BufRead, Write};

/// A player.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Player;

/// Generate a world.
pub fn main() {
  let mut world = World::new();
  {
    let generator = CompassRoseRegionGenerator;
    generator.generate(Region::default(), &mut world).unwrap();
  }
  let player = world.spawn((Region::default(), Room::default(), Player));
  let mut input_source = io::stdin().lock();
  let mut debug_mode = false;
  loop {
    // Find the room the player is in.
    let (region, room) = {
      let mut query = world.query_one::<(&Region, &Room)>(player).unwrap();
      let (region, room) = query.get().unwrap();
      (region.clone(), room.clone())
    };
    {
      let (room_name, room_description) = world
        .query::<(&Region, &Room, &Name, &Description)>()
        .with::<&IsARoom>()
        .iter()
        .find(|(_, (&rgn, &rm, _, _))| rgn == region && rm == room)
        .map(|(_, (_, _, &ref name, &ref description))| (name.clone(), description.clone()))
        .unwrap();
      println!("{}", room_name.0);
      println!("{}", room_description.0);
      let exits = world
        .query::<(&Region, &Room, &PassageDirection, &PassageKind)>()
        .iter()
        .filter(|(_, (&rgn, &rm, _, _))| rgn == region && rm == room)
        .map(|(_, (_, _, &ref dir, &ref kind))| (dir.clone(), kind.clone()))
        .collect::<Vec<_>>();
      if !exits.is_empty() {
        let exits = exits
          .iter()
          .map(|(dir, _)| dir.to_string().to_lowercase())
          .collect::<Vec<_>>();
        match exits.len() {
          1 => println!("There is an exit to the {}.", exits[0]),
          2 => println!("There are exits to the {} and {}.", exits[0], exits[1]),
          _ => println!(
            "There are exits to the {}, and {}.",
            exits[..exits.len() - 1].join(", "),
            exits[exits.len() - 1]
          ),
        }
      }
    }
    if debug_mode {
      println!("Region: {:?}", region);
      println!("Room: {:?}", room);
      println!("");
    }
    print!("> ");
    io::stdout().flush().unwrap();
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
      "?" => {
        println!("Region: {:?}", region);
        println!("Room: {:?}", room);
        println!("");
        continue;
      },
      "!" => {
        debug_mode = !debug_mode;
        println!("Debug mode: {}", debug_mode);
        println!("");
        continue;
      },
      "q" => {
        println!("Goodbye!");
        break;
      },
      _ => {
        println!("I don't understand that command.");
        println!("");
        continue;
      },
    };
    // Check if the player can move in the given direction.
    let passage = {
      let info = world
        .query::<(&Region, &Room, &PassageDirection, &PassageKind)>()
        .iter()
        .find(|(_, (&rgn, &rm, &dir, _))| rgn == region && rm == room && dir == direction)
        .map(|(_, (_, _, _, &ref kind))| kind.clone());
      if let Some(kind) = info {
        kind.clone()
      } else {
        PassageKind::NoExit("You can't go that way.".to_string())
      }
    };
    match passage {
      PassageKind::Corridor(next_region) => {
        let next_room = {
          let corridor_direction = CorridorDirection::try_from(-direction).unwrap();
          let next_room_result = world
            .query::<(&Region, &Room, &CorridorDirection, &CorridorTerminus)>()
            .iter()
            .find(|(_, (&rgn, _, &dir, _))| rgn == next_region && dir == corridor_direction)
            .map(|(_, (_, &rm, _, _))| rm.clone());
          if let None = next_room_result {
            let generator = CompassRoseRegionGenerator;
            generator.generate(next_region, &mut world).unwrap();
            let next_room_result = world
              .query::<(&Region, &Room, &CorridorDirection, &CorridorTerminus)>()
              .iter()
              .find(|(_, (&rgn, _, &dir, _))| rgn == next_region && dir == corridor_direction)
              .map(|(_, (_, &rm, _, _))| rm.clone())
              .unwrap();
            next_room_result
          } else {
            next_room_result.unwrap()
          }
        };
        world.insert(player, (next_region, next_room)).unwrap();
      },
      PassageKind::Default(next_room) => {
        world.insert(player, (next_room,)).unwrap();
      },
      PassageKind::Conditional(next_room, condition, message) => {
        if condition.is_met(&world) {
          world.insert(player, (next_room,)).unwrap();
        } else {
          println!("{}", message);
        }
      },
      PassageKind::NoExit(message) => {
        println!("{}", message);
      },
    }
    println!();
  }
  world.clear();
}
