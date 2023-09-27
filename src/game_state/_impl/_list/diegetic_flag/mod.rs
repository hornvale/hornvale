use crate::game_state::DiegeticFlagTrait;
use crate::game_state::GameState;

impl DiegeticFlagTrait for GameState {
  /// Get diegetic flag.
  fn get_diegetic_flag(&self) -> bool {
    self.diegetic_flag
  }

  /// Set diegetic flag.
  fn set_diegetic_flag(&mut self, value: bool) {
    self.diegetic_flag = value;
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;

  #[test]
  fn test_get_diegetic_flag() {
    init();
    let game_state = GameState::new();
    assert_eq!(game_state.get_diegetic_flag(), false);
  }

  #[test]
  fn test_set_diegetic_flag() {
    init();
    let mut game_state = GameState::new();
    game_state.set_diegetic_flag(true);
    assert_eq!(game_state.get_diegetic_flag(), true);
  }
}
