use anyhow::Error as AnyError;

use crate::action::QuitAction;
use crate::command::CommandTrait;
use crate::game_state::ActionQueueTrait;
use crate::game_state::GameState;

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
  /// Is this diegetic?
  fn is_diegetic(&self) -> bool {
    false
  }
  /// Runs the `Quit` command.
  fn run(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Running quit command.");
    game_state.enqueue_action(Box::new(QuitAction::new()));
    Ok(())
  }
}
