use anyhow::Error as AnyError;

use crate::command::CommandTrait;
use crate::game_state::GameState;
use crate::game_state::QuitFlagTrait;

/// The `Quit` struct.
#[derive(Debug, Default)]
pub struct Quit {}

impl Quit {
  /// Creates a new `Quit` command.
  pub fn new() -> Self {
    Self {}
  }
}

impl CommandTrait<GameState> for Quit {
  /// Runs the `Quit` command.
  fn run(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Running quit command.");
    game_state.set_quit_flag(true);
    Ok(())
  }
}
