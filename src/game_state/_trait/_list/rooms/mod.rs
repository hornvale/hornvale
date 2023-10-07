use crate::chunk::Chunk;
use crate::entity_id::RoomId;
use crate::room::Room;

/// The `Rooms` trait.
pub trait Rooms {
  /// Get a room by its id.
  fn get_room(&self, room_id: &RoomId) -> Option<&Room>;
  /// Get a mutable room by its id.
  fn get_room_mut(&mut self, room_id: &RoomId) -> Option<&mut Room>;
  /// Insert a room.
  fn insert_room(&mut self, room: Room);
  /// Remove a room by its id.
  fn remove_room(&mut self, room_id: &RoomId) -> Option<Room>;
  /// Insert rooms from a Chunk.
  fn insert_rooms_from_chunk(&mut self, chunk: &Chunk);
}
