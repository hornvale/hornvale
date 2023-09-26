use anyhow::Error as AnyError;

use crate::game_state::GameState;
use crate::game_state::GameStateTrait;
use crate::system::InputSystem;
use crate::system::OutputSystem;
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

    let mut game_state = GameState::new();
    let input_system = InputSystem::new();
    let output_system = OutputSystem::new();

    loop {
      input_system.run(&mut game_state);
      output_system.run(&mut game_state);
      if game_state.get_quit_flag() {
        break;
      }
    }

    // Termination
    println!("Thanks for playing!");

    Ok(())
  }
}
