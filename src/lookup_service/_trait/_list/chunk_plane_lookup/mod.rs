use crate::entity_id::ActorId;
use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::entity_id::EntityId;
use crate::entity_id::ObjectId;
use crate::entity_id::PlayerId;
use crate::entity_id::RoomId;

/// The `ChunkPlaneLookup` trait.
///
/// This helps chunk-plane-related lookups.
pub trait ChunkPlaneLookup {
  /// Get chunk plane of actor.
  fn get_chunk_plane_of_actor(&self, actor_id: &ActorId) -> Option<ChunkPlaneId>;

  /// Get chunk plane of chunk.
  fn get_chunk_plane_of_chunk(&self, chunk_id: &ChunkId) -> Option<ChunkPlaneId>;

  /// Get chunk plane of entity.
  fn get_chunk_plane_of_entity(&self, entity_id: &EntityId) -> Option<ChunkPlaneId>;

  /// Get chunk plane of object.
  fn get_chunk_plane_of_object(&self, object_id: &ObjectId) -> Option<ChunkPlaneId>;

  /// Get chunk plane of player.
  fn get_chunk_plane_of_player(&self, player_id: &PlayerId) -> Option<ChunkPlaneId>;

  /// Get chunk plane of room.
  fn get_chunk_plane_of_room(&self, room_id: &RoomId) -> Option<ChunkPlaneId>;
}
