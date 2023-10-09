use anyhow::Error as AnyError;
use specs::prelude::*;

use crate::resource::insert_resources;

/// The `Game` struct.
///
/// This is basically a wrapper around the run loop.
///
/// General flow:
///   1. Initialization: print welcome message, set up game state, etc.
///   2. Run loop: read input, parse input, execute command, process events,
///      display output, repeat.
///   3. Termination: print goodbye message, clean up game state, etc.
///
/// The run loop is the heart of the game.
///
/// For more detail on the run loop, see the documentation for the `run` method
/// and the `ARCHITECTURE.md` file in the root of the project.
#[derive(Debug, Default)]
pub struct Game {}

impl Game {
  /// Creates a new `Game`.
  pub fn new() -> Self {
    Self {}
  }

  /// Runs the `Game`.
  pub fn run(&mut self, seed_string: &str) -> Result<(), AnyError> {
    // Create the ECS.
    let mut ecs = World::new();

    // Initializing the game.
    debug!("Initializing game.");
    insert_resources(&mut ecs, seed_string)?;

    // Kicking off the game.
    debug!("Running game.");

    // Initialization.
    println!("Welcome to Hornvale!");

    let mut counter: u32 = 0;
    // Game loop.
    loop {
      counter += 1;
      if counter > 10 {
        break;
      }
    }

    // Termination message.
    println!("Thanks for playing!");

    // Exit, finally.
    Ok(())
  }
}
