//! Test of the world.
use hecs::World;
use hornvale_command::prelude::*;
use hornvale_dictionary::prelude::*;
use hornvale_input::prelude::*;
use hornvale_parser::prelude::*;
use hornvale_world::prelude::*;
use std::io::{self, Write};

/// A player.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Player;

/// Generate a world.
#[allow(unreachable_code)]
pub fn main() {
  let mut world = World::new();
  {
    let generator = CompassRoseRegionGenerator;
    generator.generate(Region::default(), &mut world).unwrap();
  }
  let mut command_registry = CommandRegistry::default();
  command_registry.register::<LookDirectionCommand>();
  command_registry.register::<LookHereCommand>();
  command_registry.register::<GoDirectionCommand>();
  command_registry.register::<QuitCommand>();
  world.spawn((command_registry,));
  let player = world.spawn((Region::default(), Room::default(), Player));
  let mut input_source = StdinSource::default();
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
    print!("> ");
    io::stdout().flush().unwrap();
    let input = input_source.fetch_input().unwrap();
    let mut scanner = Scanner::new(&input);
    let mut tokens = scanner.scan_tokens().unwrap();
    let classifier = Classifier::new();
    classifier.classify_tokens(&mut *tokens).unwrap();
    let mut parser = Parser::new(&mut *tokens, player, &mut world);
    if let Ok((command_function, command_context)) = parser.parse() {
      command_function(&mut world, &command_context).unwrap();
    } else {
      println!("I don't understand {:#?}", input);
    }
    // Check for QuitFlag.
    {
      let mut query = world.query::<&QuitFlag>();
      let query_result = query.iter().find(|(_, _)| true).map(|(_, _)| ());
      if query_result.is_some() {
        println!("Goodbye!");
        break;
      }
    }
    println!();
  }
  world.clear();
}
