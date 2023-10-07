use crate::entity_id::ActorId;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::entity_id::EntityId;
use crate::entity_id::ObjectId;
use crate::entity_id::PlayerId;
use crate::entity_id::RoomId;
use crate::lookup_service::ChunkPlaneLookupTrait;
use crate::lookup_service::Lookup;

/// The `ChunkPlaneLookup` trait.
///
/// This helps chunk-plane-related lookups.
impl ChunkPlaneLookupTrait for Lookup {
  /// Get chunk plane of actor.
  fn get_chunk_plane_of_actor(&self, actor_id: &ActorId) -> Option<ChunkPlaneId> {
    self
      .actor2room
      .get(actor_id)
      .and_then(|room_id| self.get_chunk_plane_of_room(room_id))
  }

  /// Get chunk plane of chunk.
  fn get_chunk_plane_of_chunk(&self, chunk_id: &ChunkId) -> Option<ChunkPlaneId> {
    self.chunk2chunk_plane.get(chunk_id).cloned()
  }

  /// Get chunk plane of entity.
  fn get_chunk_plane_of_entity(&self, entity_id: &EntityId) -> Option<ChunkPlaneId> {
    self
      .entity2room
      .get(entity_id)
      .and_then(|room_id| self.get_chunk_plane_of_room(room_id))
  }

  /// Get chunk plane of object.
  fn get_chunk_plane_of_object(&self, object_id: &ObjectId) -> Option<ChunkPlaneId> {
    self
      .object2room
      .get(object_id)
      .and_then(|room_id| self.get_chunk_plane_of_room(room_id))
  }

  /// Get chunk plane of player.
  fn get_chunk_plane_of_player(&self, player_id: &PlayerId) -> Option<ChunkPlaneId> {
    self
      .player2room
      .get(player_id)
      .and_then(|room_id| self.get_chunk_plane_of_room(room_id))
  }

  /// Get chunk plane of room.
  fn get_chunk_plane_of_room(&self, room_id: &RoomId) -> Option<ChunkPlaneId> {
    self
      .room2chunk
      .get(room_id)
      .and_then(|chunk_id| self.chunk2chunk_plane.get(chunk_id))
      .cloned()
  }
}
