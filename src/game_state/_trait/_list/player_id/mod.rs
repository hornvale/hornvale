use crate::entity_id::PlayerId as PlayerIdObject;

/// The `PlayerId` trait.
pub trait PlayerId {
  /// Returns the player ID.
  fn get_player_id(&self) -> &PlayerIdObject;
  /// Sets the player ID.
  fn set_player_id(&mut self, player_id: &PlayerIdObject);
}
