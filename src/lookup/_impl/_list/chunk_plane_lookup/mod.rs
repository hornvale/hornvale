use crate::entity_id::ChunkId;
use crate::entity_id::ChunkPlaneId;
use crate::lookup::ChunkPlaneLookupTrait;
use crate::lookup::Lookup;

/// The `ChunkPlaneLookup` trait.
///
/// This helps chunk-plane-related lookups.
impl ChunkPlaneLookupTrait for Lookup {
  /// Get chunk plane of chunk.
  fn get_chunk_plane_of_chunk(&self, chunk_id: ChunkId) -> Option<ChunkPlaneId> {
    self.chunk2chunk_plane.get(&chunk_id).cloned()
  }
}
