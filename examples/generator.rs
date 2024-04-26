//! Test of the world.
use hornvale::command::prelude::*;
use hornvale::database::prelude::*;
use hornvale::input::prelude::*;
use hornvale::world::prelude::*;
use std::io::{self, Write};

/// A player.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Player;

/// Generate a world.
#[allow(unreachable_code)]
pub fn main() {
  let mut database = Database::default();
  {
    let generator = CompassRoseRegionGenerator;
    generator.generate(Region::default(), &mut database).unwrap();
  }
  let mut command_registry = CommandRegistry::default();
  command_registry.register::<LookDirectionCommand>();
  command_registry.register::<LookHereCommand>();
  command_registry.register::<GoDirectionCommand>();
  command_registry.register::<QuitCommand>();
  database.world.spawn((command_registry,));
  let player = database.world.spawn((Region::default(), Room::default(), Player));
  let mut input_source = StdinSource::default();
  LookHereCommand::execute(&mut database, player, None, None).unwrap();
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
    let mut parser = Parser::new(&mut *tokens, player, &mut database);
    match parser.parse() {
      Ok((command_function, (actor, direct_object, indirect_object))) => {
        command_function(&mut database, actor, direct_object, indirect_object).unwrap();
      },
      Err(error) => println!("I don't understand {:#?}: {:#?}", input, error),
    }
    if database.world.is_quit_flag_set() {
      println!("Goodbye!");
      break;
    }
    println!();
  }
  database.world.clear();
}
