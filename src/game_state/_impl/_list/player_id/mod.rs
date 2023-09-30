use crate::entity_id::PlayerId;
use crate::game_state::GameState;
use crate::game_state::PlayerIdTrait;

impl PlayerIdTrait for GameState {
  /// Get the player ID.
  fn get_player_id(&self) -> &PlayerId {
    &self.player_id
  }
  /// Set the player ID.
  fn set_player_id(&mut self, player_id: &PlayerId) {
    self.player_id = player_id.clone();
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;

  #[test]
  fn test_set_player_id() {
    init();
    let mut game_state = GameState::new();
    let player_id = PlayerId::default();
    game_state.set_player_id(&player_id);
    let player_id2 = game_state.get_player_id();
    assert_eq!(player_id, player_id2.clone());
  }
}
