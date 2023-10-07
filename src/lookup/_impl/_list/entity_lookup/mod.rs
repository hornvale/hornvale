use crate::entity_id::ActorId;
use crate::entity_id::EntityId;
use crate::entity_id::ObjectId;
use crate::entity_id::PlayerId;
use crate::entity_id::RoomId;
use crate::lookup::EntityLookupTrait;
use crate::lookup::Lookup;

/// Implementation of the `EntityLookup` trait.
impl EntityLookupTrait for Lookup {
  /// Get entities in room.
  fn get_entities_in_room(&self, room_id: &RoomId) -> Vec<EntityId> {
    self
      .room2entities
      .get(room_id)
      .cloned()
      .unwrap_or_default()
      .into_iter()
      .collect()
  }

  /// Get entities in room with entity.
  fn get_entities_in_room_with_actor(&self, actor_id: &ActorId) -> Vec<EntityId> {
    self
      .actor2room
      .get(actor_id)
      .map(|room_id| self.get_entities_in_room(room_id))
      .unwrap_or_default()
  }

  /// Get entities in room with entity.
  fn get_entities_in_room_with_entity(&self, entity_id: &EntityId) -> Vec<EntityId> {
    self
      .entity2room
      .get(entity_id)
      .map(|room_id| self.get_entities_in_room(room_id))
      .unwrap_or_default()
  }

  /// Get entities in room with object.
  fn get_entities_in_room_with_object(&self, object_id: &ObjectId) -> Vec<EntityId> {
    self
      .object2room
      .get(object_id)
      .map(|room_id| self.get_entities_in_room(room_id))
      .unwrap_or_default()
  }

  /// Get entities in room with player.
  fn get_entities_in_room_with_player(&self, player_id: &PlayerId) -> Vec<EntityId> {
    self
      .player2room
      .get(player_id)
      .map(|room_id| self.get_entities_in_room(room_id))
      .unwrap_or_default()
  }
}
