use crate::game_state::EventQueueTrait;
use crate::game_state::GameState;
use crate::system::SystemTrait;

/// The `Event` system.
///
/// This system dequeues events from the `GameState` and runs them.
#[derive(Debug, Default)]
pub struct Event {}

impl Event {
  /// Creates a new `Event` system.
  pub fn new() -> Self {
    Self {}
  }
}

impl SystemTrait<GameState> for Event {
  /// Runs the `Event`s.
  fn run(&self, game_state: &mut GameState) {
    debug!("Running event system.");
    while let Some(event) = game_state.dequeue_event() {
      event
        .process(game_state)
        .map_err(|error| error!("Error running event: {}", error))
        .ok();
    }
  }
}
