use anyhow::Error as AnyError;

use crate::action::ActionTrait;
use crate::game_state::GameState;
use crate::game_state::QuitFlagTrait;

/// The `Quit` action.
#[derive(Debug, Default)]
pub struct Quit {}

impl Quit {
  /// Creates a new `Quit` action.
  pub fn new() -> Self {
    Self {}
  }
}

impl ActionTrait<GameState> for Quit {
  /// Runs the `Quit` action.
  fn execute(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Running quit action.");
    game_state.set_quit_flag(true);
    Ok(())
  }
}
