use crate::entity_id::ActorId;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::entity_id::EntityId;
use crate::entity_id::ObjectId;
use crate::entity_id::PlayerId;
use crate::entity_id::RoomId;
use crate::lookup::ChunkLookupTrait;
use crate::lookup::Lookup;

/// Implementation of the `ChunkLookupTrait` trait.
impl ChunkLookupTrait for Lookup {
  /// List chunks in chunk plane.
  fn get_chunks_in_chunk_plane(&self, chunk_plane_id: &ChunkPlaneId) -> Vec<ChunkId> {
    self
      .chunk_plane2chunks
      .get(chunk_plane_id)
      .cloned()
      .unwrap_or_default()
      .into_iter()
      .collect()
  }

  /// Get chunk of actor.
  fn get_chunk_of_actor(&self, actor_id: &ActorId) -> Option<ChunkId> {
    self
      .actor2room
      .get(actor_id)
      .and_then(|room_id| self.room2chunk.get(room_id))
      .cloned()
  }

  /// Get chunk of entity.
  fn get_chunk_of_entity(&self, entity_id: &EntityId) -> Option<ChunkId> {
    self
      .entity2room
      .get(entity_id)
      .and_then(|room_id| self.room2chunk.get(room_id))
      .cloned()
  }

  /// Get chunk of object.
  fn get_chunk_of_object(&self, object_id: &ObjectId) -> Option<ChunkId> {
    self
      .object2room
      .get(object_id)
      .and_then(|room_id| self.room2chunk.get(room_id))
      .cloned()
  }

  /// Get chunk of player.
  fn get_chunk_of_player(&self, player_id: &PlayerId) -> Option<ChunkId> {
    self
      .player2room
      .get(player_id)
      .and_then(|room_id| self.room2chunk.get(room_id))
      .cloned()
  }

  /// Get chunk of room.
  fn get_chunk_of_room(&self, room_id: &RoomId) -> Option<ChunkId> {
    self.room2chunk.get(room_id).cloned()
  }
}
