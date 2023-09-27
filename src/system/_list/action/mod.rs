use crate::game_state::ActionQueueTrait;
use crate::game_state::GameState;
use crate::system::SystemTrait;

/// The `Action` system.
///
/// This system dequeues actions from the `GameState` and runs them.
#[derive(Debug, Default)]
pub struct Action {}

impl Action {
  /// Creates a new `Action` system.
  pub fn new() -> Self {
    Self {}
  }
}

impl SystemTrait<GameState> for Action {
  /// Runs the `Action`s.
  fn run(&self, game_state: &mut GameState) {
    debug!("Running action system.");
    while let Some(action) = game_state.dequeue_action() {
      action
        .execute(game_state)
        .map_err(|error| error!("Error running action: {}", error))
        .ok();
    }
  }
}
