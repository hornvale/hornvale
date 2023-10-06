use crate::entity_id::RoomId;
use crate::game_state::CurrentRoomIdTrait;
use crate::game_state::GameState;

impl CurrentRoomIdTrait for GameState {
  /// Get the current room ID.
  fn get_current_room_id(&self) -> Option<RoomId> {
    self.current_room_id.clone()
  }
  /// Set the current room ID.
  fn set_current_room_id(&mut self, room_id: Option<RoomId>) {
    self.current_room_id = room_id;
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
    game_state.set_current_room_id(Some(room_id.clone()));
    let current_room_id = game_state.get_current_room_id();
    assert_eq!(current_room_id, Some(room_id));
  }
}
