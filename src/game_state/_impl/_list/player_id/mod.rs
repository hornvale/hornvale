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
