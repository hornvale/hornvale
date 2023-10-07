use crate::chunk::Chunk;
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
    if !self.rooms.contains_key(&room.id) {
      self.rooms.insert(room.id.clone(), room);
    }
  }
  /// Remove a room by its id.
  fn remove_room(&mut self, room_id: &RoomId) -> Option<Room> {
    self.rooms.remove(room_id)
  }
  /// Insert rooms from a Chunk.
  fn insert_rooms_from_chunk(&mut self, chunk: &Chunk) {
    for room in chunk.rooms.values() {
      self.insert_room(room.clone());
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::room::RoomBuilder;
  use crate::test::init;

  #[test]
  fn test_get_room() {
    init();
    let mut game_state = GameState::new();
    let room_id = RoomId::default();
    let room = RoomBuilder::new().id(&room_id).build();
    game_state.insert_room(room);
    let room = game_state.get_room(&room_id);
    assert!(room.is_some());
  }

  #[test]
  fn test_get_room_mut() {
    init();
    let mut game_state = GameState::new();
    let room_id = RoomId::default();
    let room = RoomBuilder::new().id(&room_id).build();
    game_state.insert_room(room);
    let room = game_state.get_room_mut(&room_id);
    assert!(room.is_some());
  }

  #[test]
  fn test_insert_room() {
    init();
    let mut game_state = GameState::new();
    let room_id = RoomId::default();
    let room = RoomBuilder::new().id(&room_id).build();
    game_state.insert_room(room);
    let room = game_state.get_room(&room_id);
    assert!(room.is_some());
  }

  #[test]
  fn test_remove_room() {
    init();
    let mut game_state = GameState::new();
    let room_id = RoomId::default();
    let room = RoomBuilder::new().id(&room_id).build();
    game_state.insert_room(room);
    let room = game_state.remove_room(&room_id);
    assert!(room.is_some());
  }
}
