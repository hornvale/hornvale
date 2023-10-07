use crate::entity_id::ActorId;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::entity_id::EntityId;
use crate::entity_id::ObjectId;
use crate::entity_id::PlayerId;
use crate::entity_id::RoomId;

/// The `ChunkLookup` trait.
///
/// This helps chunk-related lookups.
pub trait ChunkLookup {
  /// List chunks in chunk plane.
  fn get_chunks_in_chunk_plane(&self, chunk_plane_id: &ChunkPlaneId) -> Vec<ChunkId>;

  /// Get chunk of actor.
  fn get_chunk_of_actor(&self, actor_id: &ActorId) -> Option<ChunkId>;

  /// Get chunk of entity.
  fn get_chunk_of_entity(&self, entity_id: &EntityId) -> Option<ChunkId>;

  /// Get chunk of object.
  fn get_chunk_of_object(&self, object_id: &ObjectId) -> Option<ChunkId>;

  /// Get chunk of player.
  fn get_chunk_of_player(&self, player_id: &PlayerId) -> Option<ChunkId>;

  /// Get chunk of room.
  fn get_chunk_of_room(&self, room_id: &RoomId) -> Option<ChunkId>;
}
