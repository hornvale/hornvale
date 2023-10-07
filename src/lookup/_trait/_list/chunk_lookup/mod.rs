use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;

/// The `ChunkLookup` trait.
///
/// This helps chunk-related lookups.
pub trait ChunkLookup {
  /// List chunks in chunk plane.
  fn get_chunks_in_chunk_plane(&self, chunk_plane_id: ChunkPlaneId) -> Vec<ChunkId>;
}
