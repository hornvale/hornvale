use crate::game_state::GameState;
use crate::game_state::QuitFlagTrait;

impl QuitFlagTrait for GameState {
  /// Get quit-game flag.
  fn get_quit_flag(&self) -> bool {
    self.quit_flag
  }

  /// Set quit-game flag.
  fn set_quit_flag(&mut self, value: bool) {
    self.quit_flag = value;
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;

  #[test]
  fn test_get_quit_flag() {
    init();
    let game_state = GameState::new();
    assert_eq!(game_state.get_quit_flag(), false);
  }

  #[test]
  fn test_set_quit_flag() {
    init();
    let mut game_state = GameState::new();
    game_state.set_quit_flag(true);
    assert_eq!(game_state.get_quit_flag(), true);
  }
}
