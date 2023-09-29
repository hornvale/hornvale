use std::time::Duration;
use std::time::Instant;

use crate::game_state::GameState;
use crate::game_state::LoopTimerTrait;

impl LoopTimerTrait for GameState {
  /// Reset the loop timer.
  fn reset_loop_timer(&mut self) {
    self.loop_timer = Instant::now();
  }
  /// Get the time since the last reset.
  fn get_loop_timer(&self) -> Duration {
    self.loop_timer.elapsed()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;

  #[test]
  fn test_reset_loop_timer() {
    init();
    let mut game_state = GameState::new();
    game_state.reset_loop_timer();
  }

  #[test]
  fn test_get_loop_timer() {
    init();
    let mut game_state = GameState::new();
    game_state.reset_loop_timer();
    let loop_timer = game_state.get_loop_timer();
    assert_eq!(loop_timer.as_secs(), 0);
  }
}
