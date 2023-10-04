use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::game_state::ChunkPlaneIdToChunkIdsTrait;
use crate::game_state::GameState;

/// Implementation of the `ChunkPlaneIdToChunkIds` trait.
impl ChunkPlaneIdToChunkIdsTrait for GameState {
  /// Get the chunks in a plane.
  fn get_chunks_in_plane(&self, chunk_plane_id: ChunkPlaneId) -> Option<Vec<ChunkId>> {
    self.chunk_plane_id_to_chunk_ids.get(&chunk_plane_id).cloned()
  }

  /// Insert an entity into a room.
  fn insert_chunk_into_chunk_plane(&mut self, chunk_id: ChunkId, chunk_plane_id: ChunkPlaneId) {
    self
      .chunk_plane_id_to_chunk_ids
      .entry(chunk_plane_id)
      .or_default()
      .push(chunk_id);
  }

  /// Remove a chunk from a chunk plane.
  fn remove_chunk_from_chunk_plane(&mut self, chunk_id: ChunkId, chunk_plane_id: ChunkPlaneId) {
    if let Some(chunk_ids) = self.chunk_plane_id_to_chunk_ids.get_mut(&chunk_plane_id) {
      chunk_ids.retain(|id| *id != chunk_id);
    }
  }

  /// Remove all chunks from a chunk plane.
  fn remove_all_chunks_from_chunk_plane(&mut self, chunk_plane_id: ChunkPlaneId) {
    self.chunk_plane_id_to_chunk_ids.remove(&chunk_plane_id);
  }

  /// Remove a chunk plane.
  fn remove_chunk_plane(&mut self, chunk_plane_id: ChunkPlaneId) {
    self.chunk_plane_id_to_chunk_ids.remove(&chunk_plane_id);
  }
}
