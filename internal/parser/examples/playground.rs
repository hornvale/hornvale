//! # Parser Playground
//!
//! This playground demonstrates the parser module.

use hecs::World;
use hornvale_parser::prelude::*;

fn main() {
  // Create a new world.
  let mut world = World::new();

  // Create a new parser manager.
  let mut manager = ParserManager::new();

  // Register the no-op parser.
  manager.register(Box::new(NoOpParser));

  // Register the fail parser.
  manager.register(Box::new(FailParser));

  // Parse some input.
  let result = manager.parse("no-op", &mut world);
  match result {
    Ok((command, context)) => {
      println!("Command: {:?}", command);
      println!("Context: {:?}", context);
    },
    Err(error) => eprintln!("Error: {:?}", error),
  }
}
