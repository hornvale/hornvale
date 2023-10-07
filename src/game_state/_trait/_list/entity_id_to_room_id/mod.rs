use crate::entity_id::EntityId;
use crate::entity_id::RoomId;

/// The `EntityIdToRoomId` trait.
pub trait EntityIdToRoomId {
  /// Gets the room of an entity.
  fn get_room_of_entity(&self, entity_id: EntityId) -> Option<RoomId>;

  /// Sets the room of an entity.
  fn set_room_of_entity(&mut self, entity_id: EntityId, room_id: RoomId);

  /// Remove an entity from a room.
  fn remove_entity_from_room(&mut self, entity_id: EntityId);
}
