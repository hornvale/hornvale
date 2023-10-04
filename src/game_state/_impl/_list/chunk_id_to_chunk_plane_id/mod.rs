use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::game_state::ChunkIdToChunkPlaneIdTrait;
use crate::game_state::GameState;

/// Implementation of the `ChunkIdToChunkPlaneId` trait.
impl ChunkIdToChunkPlaneIdTrait for GameState {
  /// Gets the plane of a chunk.
  fn get_plane_of_chunk(&self, chunk_id: ChunkId) -> Option<ChunkPlaneId> {
    self.chunk_id_to_chunk_plane_id.get(&chunk_id).cloned()
  }

  /// Sets the plane of a chunk.
  fn set_plane_of_chunk(&mut self, chunk_id: ChunkId, chunk_plane_id: ChunkPlaneId) {
    self.chunk_id_to_chunk_plane_id.insert(chunk_id, chunk_plane_id);
  }

  /// Remove a chunk from a plane.
  fn remove_chunk_from_plane(&mut self, chunk_id: ChunkId) {
    self.chunk_id_to_chunk_plane_id.remove(&chunk_id);
  }
}
