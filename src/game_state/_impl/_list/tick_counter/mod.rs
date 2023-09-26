use crate::game_state::GameState;
use crate::game_state::TickCounter;
use crate::game_state::TickCounterTrait;

impl TickCounterTrait for GameState {
  /// Get tick counter.
  fn get_tick_counter(&self) -> TickCounter {
    self.tick_counter
  }

  /// Set tick counter.
  fn set_tick_counter(&mut self, value: TickCounter) {
    self.tick_counter = value;
  }

  /// Increment tick counter.
  fn increment_tick_counter(&mut self) {
    self.tick_counter = self.tick_counter.wrapping_add(1);
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;

  #[test]
  fn test_get_tick_counter() {
    init();
    let game_state = GameState::new();
    assert_eq!(game_state.get_tick_counter(), 0);
  }

  #[test]
  fn test_set_tick_counter() {
    init();
    let mut game_state = GameState::new();
    game_state.set_tick_counter(1);
    assert_eq!(game_state.get_tick_counter(), 1);
  }

  #[test]
  fn test_increment_tick_counter() {
    init();
    let mut game_state = GameState::new();
    game_state.increment_tick_counter();
    assert_eq!(game_state.get_tick_counter(), 1);
  }
}
