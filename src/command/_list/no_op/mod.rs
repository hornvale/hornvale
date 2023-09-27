use anyhow::Error as AnyError;

use crate::action::NoOpAction;
use crate::command::CommandTrait;
use crate::game_state::ActionQueueTrait;
use crate::game_state::GameState;

/// The `NoOp` struct.
#[derive(Debug, Default)]
pub struct NoOp {}

impl NoOp {
  /// Creates a new `NoOp`.
  pub fn new() -> Self {
    Self {}
  }
}

impl CommandTrait<GameState> for NoOp {
  /// Is this diegetic?
  fn is_diegetic(&self) -> bool {
    false
  }
  /// Runs the `NoOp` command.
  fn run(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Running no-op command.");
    game_state.enqueue_action(Box::new(NoOpAction::new()));
    Ok(())
  }
}
