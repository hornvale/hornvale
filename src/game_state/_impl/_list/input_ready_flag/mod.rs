use crate::game_state::GameState;
use crate::game_state::InputReadyFlagTrait;

impl InputReadyFlagTrait for GameState {
  /// Get input-ready flag.
  fn get_input_ready_flag(&self) -> bool {
    self.input_ready_flag
  }

  /// Set input-ready flag.
  fn set_input_ready_flag(&mut self, value: bool) {
    self.input_ready_flag = value;
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_get_input_ready_flag() {
    let game_state = GameState::new();
    assert_eq!(game_state.get_input_ready_flag(), false);
  }

  #[test]
  fn test_set_input_ready_flag_flag() {
    let mut game_state = GameState::new();
    game_state.set_input_ready_flag(true);
    assert_eq!(game_state.get_input_ready_flag(), true);
  }
}
