use anyhow::Error as AnyError;

use crate::game_state::GameState;
use crate::game_state::InputReadyFlagTrait;
use crate::game_state::QuitFlagTrait;
use crate::system::ActionSystem;
use crate::system::CommandSystem;
use crate::system::EffectSystem;
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
    let action_system = ActionSystem::default();
    let command_system = CommandSystem::default();
    let effect_system = EffectSystem::default();
    let input_system = InputSystem::default();
    let output_system = OutputSystem::default();
    let parser_system = ParserSystem::default();
    game_state.set_input_ready_flag(true);
    loop {
      if game_state.get_input_ready_flag() {
        // Read input from the user.
        input_system.run(&mut game_state);
      }
      // Parse input into a command or commands.
      parser_system.run(&mut game_state);
      // Execute the command or commands.
      command_system.run(&mut game_state);
      // Execute any actions that have been queued.
      action_system.run(&mut game_state);
      // Apply any effects that have been queued.
      effect_system.run(&mut game_state);
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
