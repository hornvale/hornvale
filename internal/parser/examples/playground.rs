//! # Parser Playground
//!
//! This playground demonstrates the parser module.

use hornvale_parser::prelude::*;

fn main() {
  // Create a new parser manager.
  let mut manager = ParserManager::new();

  // Register the no-op parser.
  manager.register(Box::new(NoOpParser));

  // Register the fail parser.
  manager.register(Box::new(FailParser));

  // Parse some input.
  let result = manager.parse("no-op");
  match result {
    Ok(command) => println!("{}", command.usage()),
    Err(error) => eprintln!("Error: {:?}", error),
  }
}
