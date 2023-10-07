use crate::entity_id::EntityId;
use crate::entity_id::RoomId;
use crate::game_state::GameState;
use crate::game_state::RoomIdToEntityIdsTrait;

/// Implementation of `RoomIdToEntityIdsTrait`.
impl RoomIdToEntityIdsTrait for GameState {
  /// Get the entities in a room.
  fn get_entities_in_room(&self, room_id: RoomId) -> Option<Vec<EntityId>> {
    self.room_id_to_entity_ids.get(&room_id).cloned()
  }

  /// Insert an entity into a room.
  fn insert_entity_into_room(&mut self, entity_id: EntityId, room_id: RoomId) {
    self.room_id_to_entity_ids.entry(room_id).or_default().push(entity_id);
  }

  /// Remove an entity from a room.
  fn remove_entity_from_room(&mut self, entity_id: EntityId, room_id: RoomId) {
    if let Some(entity_ids) = self.room_id_to_entity_ids.get_mut(&room_id) {
      entity_ids.retain(|id| *id != entity_id);
    }
  }

  /// Remove all entities from a room.
  fn remove_all_entities_from_room(&mut self, room_id: RoomId) {
    self.room_id_to_entity_ids.remove(&room_id);
  }

  /// Remove a room.
  fn remove_room(&mut self, room_id: RoomId) {
    self.room_id_to_entity_ids.remove(&room_id);
  }
}
