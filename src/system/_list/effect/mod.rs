use crate::game_state::EffectQueueTrait;
use crate::game_state::GameState;
use crate::system::SystemTrait;

/// The `Effect` system.
///
/// This system dequeues effects from the `GameState` and applies them.
#[derive(Debug, Default)]
pub struct Effect {}

impl Effect {
  /// Creates a new `Effect` system.
  pub fn new() -> Self {
    Self {}
  }
}

impl SystemTrait<GameState> for Effect {
  /// Runs the `Effect`s.
  fn run(&self, game_state: &mut GameState) {
    debug!("Running effect system.");
    while let Some(effect) = game_state.dequeue_effect() {
      effect
        .apply(game_state)
        .map_err(|error| error!("Error applying effect: {}", error))
        .ok();
    }
  }
}
