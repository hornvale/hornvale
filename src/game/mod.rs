use anyhow::Error as AnyError;
use specs::prelude::*;
use specs::shrev::EventChannel;
use std::io::{stdin, stdout, Write};

use crate::dispatcher::get_initial_dispatcher;
use crate::dispatcher::get_simulation_dispatcher;
use crate::event::InputEvent;
use crate::resource::InputReadyFlagResource;
use crate::resource::QuitFlagResource;
use crate::resource::SeedStringResource;

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

  /// Read the input from the user.
  pub fn read_input(&self) -> String {
    debug!("Reading input.");
    print!("> ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    input.trim().to_string()
  }

  /// Runs the `Game`.
  pub fn run(&mut self, seed_string: &str) -> Result<(), AnyError> {
    // Create the ECS.
    let mut ecs = World::new();

    // Initializing the game.
    debug!("Initializing game.");
    let mut initial_dispatcher = get_initial_dispatcher(&mut ecs);
    ecs.fetch_mut::<SeedStringResource>().0 = seed_string.to_string();
    debug!("Seed String: {}", ecs.fetch::<SeedStringResource>().0);
    initial_dispatcher.dispatch(&ecs);

    // Kicking off the game.
    debug!("Running game.");
    let mut simulation_dispatcher = get_simulation_dispatcher(&mut ecs);

    // Initialization.
    println!("Welcome to Hornvale!");
    ecs.fetch_mut::<InputReadyFlagResource>().0 = true;

    // Game loop.
    loop {
      // Long-running diegetic actions will set the input_ready flag to false.
      // We don't want to read input until they resolve.
      if ecs.fetch::<InputReadyFlagResource>().0 {
        let input = self.read_input();
        ecs.fetch_mut::<EventChannel<InputEvent>>().single_write(InputEvent {
          input: input.to_owned(),
        });
      }

      // Run the general simulation dispatcher.
      simulation_dispatcher.dispatch(&ecs);

      // Maintain after every tick.  This enables the use of the lazy systems,
      // which should make it easier to have simple, concise systems.
      ecs.maintain();

      // Check for the quit flag.
      if ecs.fetch::<QuitFlagResource>().0 {
        break;
      }
    }

    // Termination message.
    println!("Thanks for playing!");

    // Exit, finally.
    Ok(())
  }
}
