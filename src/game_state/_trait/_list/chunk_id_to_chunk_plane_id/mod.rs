use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;

/// The `ChunkIdToChunkPlaneId` trait.
pub trait ChunkIdToChunkPlaneId {
  /// Gets the plane of a chunk.
  fn get_plane_of_chunk(&self, chunk_id: ChunkId) -> Option<ChunkPlaneId>;

  /// Sets the plane of a chunk.
  fn set_plane_of_chunk(&mut self, chunk_id: ChunkId, chunk_plane_id: ChunkPlaneId);

  /// Remove a chunk from a plane.
  fn remove_chunk_from_plane(&mut self, chunk_id: ChunkId);
}
