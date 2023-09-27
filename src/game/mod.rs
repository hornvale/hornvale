use anyhow::Error as AnyError;

use crate::game_state::GameState;
use crate::game_state::InputReadyFlagTrait;
use crate::game_state::QuitFlagTrait;
use crate::system::CommandSystem;
use crate::system::EventSystem;
use crate::system::InputSystem;
use crate::system::OutputSystem;
use crate::system::ParserSystem;
use crate::system::SystemTrait;

/// The `Game` struct.
///
/// This is basically a wrapper around the run loop.
#[derive(Debug, Default)]
pub struct Game {}

impl Game {
  /// Creates a new `Game`.
  pub fn new() -> Self {
    Self {}
  }

  /// Runs the `Game`.
  pub fn run(&mut self) -> Result<(), AnyError> {
    debug!("Running game.");

    // Initialization
    println!("Welcome to Hornvale!");

    let mut game_state = GameState::default();
    let command_system = CommandSystem::default();
    let event_system = EventSystem::default();
    let input_system = InputSystem::default();
    let output_system = OutputSystem::default();
    let parser_system = ParserSystem::default();
    game_state.set_input_ready_flag(true);
    loop {
      // Long-running diegetic actions will set the input_ready flag to false.
      // We don't want to read input until they resolve.
      if game_state.get_input_ready_flag() {
        // Read input from the user.
        input_system.run(&mut game_state);
      }
      // Parse player input into a command or commands.
      parser_system.run(&mut game_state);
      // Execute the command or commands entered by the player, which will
      // create events that invoke actions.
      command_system.run(&mut game_state);
      // Process event queue, which will execute actions and apply effects
      // and cancel other events and so forth and so on.
      event_system.run(&mut game_state);
      // Display accumulated output to the user.
      output_system.run(&mut game_state);
      // We've been told to quit.
      if game_state.get_quit_flag() {
        break;
      }
    }

    // Termination
    println!("Thanks for playing!");

    Ok(())
  }
}
