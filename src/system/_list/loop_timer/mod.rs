use crate::game_state::GameState;
use crate::game_state::LoopTimerTrait;
use crate::system::SystemTrait;

/// The `LoopTimer` system.
///
/// This system measures how long the last tick took to process.
#[derive(Debug, Default)]
pub struct LoopTimer {}

impl LoopTimer {
  /// Creates a new `LoopTimer`.
  pub fn new() -> Self {
    Self {}
  }
}

impl SystemTrait<GameState> for LoopTimer {
  /// Runs the `LoopTimer` system.
  fn run(&self, game_state: &mut GameState) {
    debug!("Running loop-timer system.");
    let duration = game_state.get_loop_timer();
    debug!("Loop timer: {:?}", duration);
  }
}
