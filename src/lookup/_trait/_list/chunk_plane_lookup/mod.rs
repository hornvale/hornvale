use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;

/// The `ChunkPlaneLookup` trait.
///
/// This helps chunk-plane-related lookups.
pub trait ChunkPlaneLookup {
  /// Get chunk plane of chunk.
  fn get_chunk_plane_of_chunk(&self, chunk_id: ChunkId) -> Option<ChunkPlaneId>;
}
