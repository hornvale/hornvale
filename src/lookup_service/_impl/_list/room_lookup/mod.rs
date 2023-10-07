use crate::entity_id::ActorId;
use crate::entity_id::ChunkId;
use crate::entity_id::EntityId;
use crate::entity_id::ObjectId;
use crate::entity_id::PlayerId;
use crate::entity_id::RoomId;
use crate::lookup_service::Lookup;
use crate::lookup_service::RoomLookupTrait;

/// Implementation of the `RoomLookup` trait.
impl RoomLookupTrait for Lookup {
  /// List rooms in chunk.
  fn get_rooms_in_chunk(&self, chunk_id: &ChunkId) -> Vec<RoomId> {
    self
      .chunk2rooms
      .get(chunk_id)
      .cloned()
      .unwrap_or_default()
      .into_iter()
      .collect()
  }

  /// Get the room of an actor.
  fn get_room_of_actor(&self, actor_id: &ActorId) -> Option<RoomId> {
    self.actor2room.get(actor_id).cloned()
  }

  /// Get the room of an entity.
  fn get_room_of_entity(&self, entity_id: &EntityId) -> Option<RoomId> {
    self.entity2room.get(entity_id).cloned()
  }

  /// Get the room of an object.
  fn get_room_of_object(&self, object_id: &ObjectId) -> Option<RoomId> {
    self.object2room.get(object_id).cloned()
  }

  /// Get room of player.
  fn get_room_of_player(&self, player_id: &PlayerId) -> Option<RoomId> {
    self.player2room.get(player_id).cloned()
  }
}
