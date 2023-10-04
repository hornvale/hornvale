use crate::entity_id::EntityId;
use crate::entity_id::RoomId;

/// The `RoomIdToEntityIds` trait.
pub trait RoomIdToEntityIds {
  /// Get the entities in a room.
  fn get_entities_in_room(&self, room_id: RoomId) -> Option<Vec<EntityId>>;

  /// Insert an entity into a room.
  fn insert_entity_into_room(&mut self, entity_id: EntityId, room_id: RoomId);

  /// Remove an entity from a room.
  fn remove_entity_from_room(&mut self, entity_id: EntityId, room_id: RoomId);

  /// Remove all entities from a room.
  fn remove_all_entities_from_room(&mut self, room_id: RoomId);

  /// Remove a room.
  fn remove_room(&mut self, room_id: RoomId);
}
