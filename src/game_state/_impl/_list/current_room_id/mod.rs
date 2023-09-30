use crate::entity_id::RoomId;
use crate::game_state::CurrentRoomIdTrait;
use crate::game_state::GameState;

impl CurrentRoomIdTrait for GameState {
  /// Get the current room ID.
  fn get_current_room_id(&self) -> &RoomId {
    &self.current_room_id
  }
  /// Set the current room ID.
  fn set_current_room_id(&mut self, room_id: &RoomId) {
    self.current_room_id = room_id.clone();
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::test::init;

  #[test]
  fn test_set_current_room_id() {
    init();
    let mut game_state = GameState::new();
    let room_id = RoomId::default();
    game_state.set_current_room_id(&room_id);
    let current_room_id = game_state.get_current_room_id();
    assert_eq!(current_room_id, &room_id);
  }
}
