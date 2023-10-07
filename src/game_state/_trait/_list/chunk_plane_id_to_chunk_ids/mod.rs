use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;

/// The `ChunkPlaneIdToChunkIds` trait.
pub trait ChunkPlaneIdToChunkIds {
  /// Get the chunks in a plane.
  fn get_chunks_in_plane(&self, chunk_plane_id: ChunkPlaneId) -> Option<Vec<ChunkId>>;

  /// Insert an entity into a room.
  fn insert_chunk_into_chunk_plane(&mut self, chunk_id: ChunkId, chunk_plane_id: ChunkPlaneId);

  /// Remove a chunk from a chunk plane.
  fn remove_chunk_from_chunk_plane(&mut self, chunk_id: ChunkId, chunk_plane_id: ChunkPlaneId);

  /// Remove all chunks from a chunk plane.
  fn remove_all_chunks_from_chunk_plane(&mut self, chunk_plane_id: ChunkPlaneId);

  /// Remove a chunk plane.
  fn remove_chunk_plane(&mut self, chunk_plane_id: ChunkPlaneId);
}
