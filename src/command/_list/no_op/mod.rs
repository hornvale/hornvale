use anyhow::Error as AnyError;

use crate::command::CommandTrait;
use crate::event::NoOpEvent;
use crate::game_state::EventQueueTrait;
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
  /// Get the name of this command.
  fn get_name(&self) -> &'static str {
    "NoOp"
  }

  /// Is this diegetic?
  fn is_diegetic(&self) -> bool {
    false
  }

  /// Runs the `NoOp` command.
  fn execute(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Running no-op command.");
    game_state.enqueue_event(Box::new(NoOpEvent::new()));
    Ok(())
  }
}
