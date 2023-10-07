use crate::entity_id::EntityId;
use crate::entity_id::RoomId;
use crate::game_state::EntityIdToRoomIdTrait;
use crate::game_state::GameState;

/// The `EntityIdToRoomId` implementation.
impl EntityIdToRoomIdTrait for GameState {
  /// Gets the room of an entity.
  fn get_room_of_entity(&self, entity_id: EntityId) -> Option<RoomId> {
    self.entity_id_to_room_id.get(&entity_id).cloned()
  }

  /// Sets the room of an entity.
  fn set_room_of_entity(&mut self, entity_id: EntityId, room_id: RoomId) {
    self.entity_id_to_room_id.insert(entity_id, room_id);
  }

  /// Remove an entity from a room.
  fn remove_entity_from_room(&mut self, entity_id: EntityId) {
    self.entity_id_to_room_id.remove(&entity_id);
  }
}
