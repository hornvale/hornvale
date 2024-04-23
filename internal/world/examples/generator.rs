//! Test of the world.
use hecs::World;
use hornvale_command::prelude::*;
use hornvale_input::prelude::*;
use hornvale_world::prelude::*;
use hornvale_world_commands::prelude::*;
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
  LookHereCommand::execute(&mut world, player, None, None).unwrap();
  loop {
    print!("> ");
    io::stdout().flush().unwrap();
    let input = input_source.fetch_input().unwrap();
    let mut scanner = Scanner::new(&input);
    let mut tokens = scanner.scan_tokens().unwrap();
    let classifier = Classifier::new();
    if classifier.classify_tokens(&mut *tokens).is_err() {
      match input.to_lowercase().as_str().trim().len() {
        0 => println!("Eh?"),
        _ => println!("I don't understand {:#?}.", input),
      }
      continue;
    }
    let mut parser = Parser::new(&mut *tokens, player, &mut world);
    if let Ok((command_function, (actor, direct_object, indirect_object))) = parser.parse() {
      command_function(&mut world, actor, direct_object, indirect_object).unwrap();
    } else {
      println!("I don't understand {:#?}", input);
    }
    if world.is_quit_flag_set() {
      println!("Goodbye!");
      break;
    }
    println!();
  }
  world.clear();
}
