use crate::entity_id::RoomId;

/// The `CurrentRoomId` trait.
pub trait CurrentRoomId {
  /// Returns the current room ID.
  fn get_current_room_id(&self) -> &RoomId;
  /// Sets the current room ID.
  fn set_current_room_id(&mut self, room_id: &RoomId);
}
