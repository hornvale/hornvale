use crate::entity_id::RoomId;
use crate::game_state::GameState;
use crate::game_state::RoomsTrait;
use crate::room::Room;

impl RoomsTrait for GameState {
  /// Get a room by its id.
  fn get_room(&self, room_id: &RoomId) -> Option<&Room> {
    self.rooms.get(room_id)
  }
  /// Get a mutable room by its id.
  fn get_room_mut(&mut self, room_id: &RoomId) -> Option<&mut Room> {
    self.rooms.get_mut(room_id)
  }
  /// Insert a room.
  fn insert_room(&mut self, room: Room) {
    self.rooms.insert(room.id.clone(), room);
  }
  /// Remove a room by its id.
  fn remove_room(&mut self, room_id: &RoomId) -> Option<Room> {
    self.rooms.remove(room_id)
  }
}
