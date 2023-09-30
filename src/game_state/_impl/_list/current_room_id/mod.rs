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
