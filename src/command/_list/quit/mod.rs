use anyhow::Error as AnyError;

use crate::command::CommandTrait;
use crate::event::QuitEvent;
use crate::game_state::EventQueueTrait;
use crate::game_state::GameState;

/// The `Quit` command.
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
  fn execute(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Running quit command.");
    game_state.enqueue_event(Box::new(QuitEvent::new()));
    Ok(())
  }
}
